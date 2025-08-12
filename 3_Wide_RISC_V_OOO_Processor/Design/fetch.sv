// ============================================================================
// Frontend with Branch Predictor Integration
// - Instantiates fetch_unit, predecoder, and branch_predictor_unit
// - Predecoder extracts is_call_instr, is_ret_instr, is_branch_instr for each slot
// ============================================================================

`include "fetch_pkg.sv"
`include "branch_predictor.sv"
`include "fetch_unit.sv"

import fetch_types_pkg::*;

module frontend_with_branch_predictor #(
    parameter ISSUE_WIDTH = 3,
    parameter IQ_DEPTH = 16,
    parameter NO_INSTR = 33,
    parameter INSTRUCTION_MEMORY_SIZE = 64,
    parameter XLEN = 32,
    parameter BTB_SETS = 64,
    parameter BTB_WAYS = 4,
    parameter GSHARE_TABLE_BITS = 10,
    parameter RAS_DEPTH = 16,
    parameter BPQ_DEPTH = 16
) (
    input  logic clk,
    input  logic rst,
    input  logic stall_in,
    output logic fetch_stall,
    // ---------------- Fetch-Decode Interface (to/from Decoder) ----------------
    input  logic [ISSUE_WIDTH-1:0] deq_ready, // From decoder: signals decoder is ready to accept instructions
    output fetch_entry_t fec_dec_latch_out [ISSUE_WIDTH-1:0], // To decoder: fetch-to-decode latch output (instructions, PCs, valid)
    // -------------------------------------------------------------------------
    // ---------------- Startup/Initial PC Input ----------------
    input  logic [31:0] pc_in, // Initial PC after reset
    // ----------------------------------------------------------------
    // ---------------- Retirement/Commit Stage Inputs ----------------
    input  logic update_en,              // From retirement: branch resolved
    input  logic [XLEN-1:0] update_pc,   // From retirement: resolved branch PC
    input  logic update_taken,           // From retirement: resolved branch taken?
    input  logic [XLEN-1:0] update_target, // From retirement: resolved branch target
    input  logic flush,                  // From retirement: pipeline flush (mispredict)
    // ----------------------------------------------------------------
    // ---------------- Fetch/PC Control Outputs ----------------
    output logic [XLEN-1:0] predicted_pc,           // To fetch: next PC to fetch (from branch predictor)
    // ----------------------------------------------------------------
    // ---------------- Retirement/Commit Stage Outputs ----------------
    output logic [ISSUE_WIDTH-1:0] predict_valid,   // To retirement: valid predictions per slot
    output logic [$clog2(RAS_DEPTH):0] ras_sp_out,  // To retirement: RAS stack pointer
    output logic [$clog2(BPQ_DEPTH):0] bpq_count_out // To retirement: BPQ count
);
    // Fetch outputs
    logic [ISSUE_WIDTH-1:0] out_deq_valid;
    fetch_entry_t out_deq_data [ISSUE_WIDTH-1:0];
    fetch_entry_t fetch_reg_out [ISSUE_WIDTH-1:0];

    // Mux for fetch PC: use pc_in on reset, predicted_pc otherwise
    // program stall, fetch stall
    logic [31:0] fetch_pc_mux;
    logic [31:0] next_sequential_pc;
    logic        any_predict_valid;
    logic [31:0] fetch_pc_mux_d;
    logic init_done_pulse;
    logic program_stall_0;
    logic program_stall_1;
    logic program_stall_2;
    logic init;
  
    // Program end reached / fetch stall
    assign fetch_stall = stall_in || program_stall_2;

    // Compute next sequential
    assign next_sequential_pc = fetch_pc_mux_d + (4 * ISSUE_WIDTH);

    // Check if any prediction is valid
    assign any_predict_valid = |predict_valid;

    // 2:1 MUX for Select PC: predicted_pc if prediction valid, else next sequential
    always_comb begin
        if (any_predict_valid)
            fetch_pc_mux = predicted_pc;
        else
            fetch_pc_mux = next_sequential_pc;
    end
  
   always_ff @(posedge clk or posedge rst) begin
    if (rst) begin
        init <= 0;
        fetch_pc_mux_d <= pc_in;
        program_stall_0 <= 0;
        program_stall_1 <= 0;
        program_stall_2 <= 0;
    end else if (!init && init_done_pulse) begin
        init <= 1;
        fetch_pc_mux_d <= fetch_pc_mux_d + (ISSUE_WIDTH * 4);
    end else if ((fetch_pc_mux_d < (NO_INSTR * 4)) && init) begin
        fetch_pc_mux_d <= fetch_pc_mux;
    end else if (init && (fetch_pc_mux_d >= (NO_INSTR * 4)))begin
        program_stall_0 <= 1;
        program_stall_1 <= program_stall_0;
        program_stall_2 <= program_stall_1;
    end
   end

    // Instantiate fetch unit
    fetch_unit #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .IQ_DEPTH(IQ_DEPTH),
        .NO_INSTR(NO_INSTR),
        .INSTRUCTION_MEMORY_SIZE(INSTRUCTION_MEMORY_SIZE)
    ) fetch_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .stall_in(fetch_stall),
        .pc_in(fetch_pc_mux_d), // Use muxed PC
        .deq_ready(deq_ready),
        .out_deq_valid(out_deq_valid),
        .out_deq_data(out_deq_data),
        .fec_dec_latch_out(fec_dec_latch_out),
        .imem_fetch_reg_out(fetch_reg_out),
        .debug_pc(),
        .debug_cycle_count(),
        .init_done(),
        .init_done_pulse(init_done_pulse)
    );

    // Predecoder logic for each slot (now using fetch_reg_out)
    logic is_call_instr [ISSUE_WIDTH]; 
    logic is_ret_instr [ISSUE_WIDTH];
    logic is_branch_instr [ISSUE_WIDTH];
    logic [XLEN-1:0] fetch_pc [ISSUE_WIDTH];
    logic [31:0] instr;
    logic [6:0] opcode;
    logic [4:0] rd;
    logic [2:0] funct3;
    logic [4:0] rs1;
    always_comb begin
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            fetch_pc[i] = fetch_reg_out[i].pc;
            instr = fetch_reg_out[i].instruction;
            opcode = instr[6:0];
            rd     = instr[11:7];
            funct3 = instr[14:12];
            rs1    = instr[19:15];
            // Default
            is_call_instr[i] = 0;
            is_ret_instr[i] = 0;
            is_branch_instr[i] = 0;
            if (fetch_reg_out[i].valid) begin
                // JAL
                if (opcode == 7'b1101111) begin
                    is_call_instr[i] = 1;
                end
                // JALR
                else if (opcode == 7'b1100111) begin
                    if (rd != 0) is_call_instr[i] = 1;
                    else if (rd == 0 && rs1 == 1) is_ret_instr[i] = 1;
                end
                // Branches
                else if (opcode == 7'b1100011) begin
                    is_branch_instr[i] = 1;
                end
            end
        end
    end

    // Instantiate branch predictor unit
    branch_predictor_unit #(
        .XLEN(XLEN),
        .BTB_SETS(BTB_SETS),
        .BTB_WAYS(BTB_WAYS),
        .GSHARE_TABLE_BITS(GSHARE_TABLE_BITS),
        .RAS_DEPTH(RAS_DEPTH),
        .BPQ_DEPTH(BPQ_DEPTH),
        .FETCH_WIDTH(ISSUE_WIDTH)
    ) bpu (
        .clk(clk),
        .rst(rst),
        .fetch_pc(fetch_pc),
        .is_call_instr(is_call_instr),
        .is_ret_instr(is_ret_instr),
        .is_branch_instr(is_branch_instr),
        .predicted_pc(predicted_pc),
        .predict_valid(predict_valid),
        .update_en(update_en),
        .update_pc(update_pc),
        .update_taken(update_taken),
        .update_target(update_target),
        .flush(flush),
        .ras_sp_out(ras_sp_out),
        .bpq_count_out(bpq_count_out),
        .tb_btb_hit(),
        .tb_btb_target(),
        .tb_gshare_pred(),
        .tb_ras_pop_ready(),
        .tb_ras_pop_addr()
    );

endmodule
