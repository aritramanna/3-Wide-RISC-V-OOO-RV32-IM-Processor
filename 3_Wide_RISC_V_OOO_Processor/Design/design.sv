`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Create Date: 06/14/2025 02:20:00 PM
// Module Name: Top Module - Complete Tomasulo Processor Integration
// Description: This module integrates the complete Tomasulo processor pipeline:
//              Frontend → Decoder → Dispatch → Execute → Memory → Commit
//              Features: Parameterized architecture, branch prediction, 
//                       out-of-order execution, memory speculation
// Version : 3.00 - Production Quality with Parameter Validation
// Dependencies: fetch_pkg.sv, decoder_pkg.sv, tomasulo_pkg.sv
//////////////////////////////////////////////////////////////////////////////////

// ============================================================================
// INCLUDES AND IMPORTS
// ============================================================================
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"
`include "fetch.sv"
`include "decoder.sv"
`include "alloc_reorder_retire.sv"
`include "func_unit.sv"
`include "completion_buffer.sv"
`include "top_memory_subsystem.sv"

import fetch_types_pkg::*;
import decoder_pkg::*;
import tomasulo_pkg::*;

// ============================================================================
// MODULE DEFINITION
// ============================================================================
module top_module #(
    // ============================================================================
    // ARCHITECTURE PARAMETERS (Overridable from testbench)
    // ============================================================================
    parameter int ISSUE_WIDTH = 3,                    // Instructions per cycle
    parameter int CDB_WIDTH = 3,                      // Common Data Bus width (results per cycle)
    parameter int no_ARF = 32,                        // Architectural register file size
    parameter int no_RAT = 32,                        // Register alias table size
    parameter int no_instr = 33,                      // Total instructions to execute
    parameter int no_RS_addsublog = 12,                // ALU reservation stations
    parameter int no_RS_muldiv = 4,                   // Mul/Div reservation stations
    parameter int no_LoadStoreBuffer = 16,            // Load/Store buffer entries
    parameter int no_ALU_units = 3,                   // ALU execution units
    parameter int no_MulDiv_units = 2,                // Mul/Div execution units
    parameter int no_ROB = 48,                        // Reorder buffer entries
    parameter int NUM_LD_PORTS = 2,                   // Memory load ports
    parameter int NUM_ST_PORTS = 1,                   // Memory store ports
    
    // ============================================================================
    // FRONTEND PARAMETERS
    // ============================================================================
    parameter int IQ_DEPTH = 16,                      // Instruction queue depth
    parameter int INSTRUCTION_MEMORY_SIZE = 64,       // Instruction memory size
    parameter int BTB_SETS = 64,                      // BTB number of sets
    parameter int BTB_WAYS = 4,                       // BTB number of ways
    parameter int GSHARE_TABLE_BITS = 10,             // GShare table bits
    parameter int RAS_DEPTH = 16,                     // RAS depth
    parameter int BPQ_DEPTH = 16,                     // Branch prediction queue depth
    
    // ============================================================================
    // MEMORY SUBSYSTEM PARAMETERS
    // ============================================================================
    parameter int LQ_DEPTH = 8,                       // Load queue depth
    parameter int SQ_DEPTH = 8,                       // Store queue depth
    parameter int MEM_DEPTH = 1024,                   // Data memory depth
    parameter int DATA_WIDTH = 32,                    // Data memory width
    parameter int ADDR_WIDTH = 32,                    // Address width for memory operations
    parameter int SAFETY_MARGIN = 5                   // Completion buffer safety margin
) (
    // ============================================================================
    // CLOCK AND CONTROL SIGNALS
    // ============================================================================
    input  logic clk,                                 // Clock signal
    input  logic rst,                                 // Synchronous reset
    input  logic flush,                               // Pipeline flush signal
    
    // ============================================================================
    // FETCH INTERFACE
    // ============================================================================
    input  logic [ADDR_WIDTH-1:0] pc_in,              // Program counter input
    output logic fetch_stall,                         // Fetch stall signal
    
    // ============================================================================
    // BRANCH PREDICTION INTERFACE
    // ============================================================================
    input  logic update_en,                           // Branch prediction update enable
    input  logic [ADDR_WIDTH-1:0] update_pc,          // Branch prediction update PC
    input  logic update_taken,                        // Branch prediction update taken
    input  logic [ADDR_WIDTH-1:0] update_target,      // Branch prediction update target
    output logic [ADDR_WIDTH-1:0] predicted_pc,       // Predicted PC
    output logic [ISSUE_WIDTH-1:0] predict_valid,     // Prediction valid signals
    output logic [$clog2(RAS_DEPTH):0] ras_sp_out,    // RAS stack pointer
    output logic [$clog2(BPQ_DEPTH):0] bpq_count_out, // BPQ count
    
    // ============================================================================
    // PIPELINE CONTROL AND STATUS
    // ============================================================================
    output logic alloc_stall,                         // Allocation stall
    output logic fetch_stage_stall,                   // Fetch stage stall
    output logic decode_stage_stall,                  // Decode stage stall
    output logic dispatch_stage_stall,                // Dispatch stage stall
    output logic instruction_queue_full,              // Instruction queue full
    output logic instruction_queue_empty,             // Instruction queue empty
    output logic [4:0] instruction_queue_count,       // Instruction queue count
    
    // ============================================================================
    // PIPELINE DATA OUTPUTS
    // ============================================================================
    output decode_entry_t [ISSUE_WIDTH-1:0] decode_reg,           // Decoded instructions
    output logic [ISSUE_WIDTH-1:0] dec_rdy,                       // Decoder ready signals
    output logic [ISSUE_WIDTH-1:0] decoder_error,                 // Decoder error signals
    
    // ============================================================================
    // ESSENTIAL DEBUG AND MONITORING OUTPUTS
    // ============================================================================
    output logic [ADDR_WIDTH-1:0] debug_pc,                       // Debug: current PC
    output logic [DATA_WIDTH-1:0] debug_cycle_count,              // Debug: cycle counter
    output logic init_done,                                       // Initialization complete
    output logic init_done_pulse,                                 // One-cycle pulse after init
    output logic [ISSUE_WIDTH-1:0] commit_valid,                  // Commit valid signals
    output logic [$clog2(no_ROB)-1:0] commit_rob_id [ISSUE_WIDTH-1:0], // Commit ROB IDs
    output logic [$clog2(no_ROB)-1:0] commit_id_0,                // Commit ID for first instruction
    output logic [$clog2(no_ROB)-1:0] commit_id_1,                // Commit ID for second instruction
    output logic [$clog2(no_ROB)-1:0] commit_id_2,                // Commit ID for third instruction 
    output rob_entry_t [no_ROB-1:0] rob_out,                      // ROB contents for monitoring
    output logic violation_detected,                              // Memory violation detection
    output logic [$clog2(no_ROB)-1:0] violation_rob_index,        // Violation ROB index
    output logic [ADDR_WIDTH-1:0] violation_pc                    // Violation PC
);

    // ============================================================================
    // INTERNAL SIGNALS
    // ============================================================================
    
    // Frontend to decoder interface
    fetch_entry_t fec_dec_latch_out [ISSUE_WIDTH-1:0];
    
    // Stall and control signals
    logic internal_fetch_stall;                                   // No internal fetch stall
    logic internal_alloc_stall;                                   // From Tomasulo core
    logic program_stall;                                          // From frontend
    
    // Reset behavior for control signals
    always_ff @(posedge clk or posedge rst) begin
        if (rst) begin
            internal_fetch_stall <= 1'b0;
        end else begin
            internal_fetch_stall <= 1'b0;  // Always 0 for now
        end
    end
    
    // Allocation ready signals
    logic [ISSUE_WIDTH-1:0] alloc_rdy_from_core;                 // From Tomasulo core
    
    // Tomasulo core internal signals
    AddSub_RS_Entry_t [no_RS_addsublog-1:0] alu_rs_out_internal;
    MulDiv_RS_Entry_t [no_RS_muldiv-1:0] muldiv_rs_out_internal;
    LS_Buffer_Entry_t [no_LoadStoreBuffer-1:0] mem_rs_out_internal;
    
    // Functional unit signals
    ALU_Result_t [no_ALU_units + 2*no_MulDiv_units-1:0] func_unit_out;
    logic [no_RS_addsublog-1:0] alu_rs_clear;
    logic [no_RS_muldiv-1:0] muldiv_rs_clear;
    
    // Completion buffer signals
    ALU_Result_t [ISSUE_WIDTH-1:0] cdb_results_int;
    logic [ISSUE_WIDTH-1:0] cdb_valid_int;
    logic [ISSUE_WIDTH-1:0] cdb_rdy = {ISSUE_WIDTH{1'b1}};       // Always ready
    logic completion_buffer_almost_full;
    
    // Memory subsystem signals
    ALU_Result_t [NUM_LD_PORTS-1:0] load_cdb_result_mem;
    logic [no_LoadStoreBuffer-1:0] lsb_entry_consumed;
    
    // MMU interface signals (vectorized for superscalar commit)
    logic [$clog2(no_ROB)-1:0] mmu_rob_head [ISSUE_WIDTH-1:0];
    logic [ISSUE_WIDTH-1:0] store_commit_valid_wire;
    logic [$clog2(no_ROB)-1:0] store_commit_rob_index_wire [ISSUE_WIDTH-1:0];

    // Combined functional results
    ALU_Result_t [no_ALU_units + 2*no_MulDiv_units + NUM_LD_PORTS - 1:0] all_func_results;
    assign all_func_results = {load_cdb_result_mem, func_unit_out};



    // ============================================================================
    // DIRECT SIGNAL ASSIGNMENTS (No redundant intermediate signals)
    // ============================================================================
    
    // Stall signal assignments
    assign fetch_stall = internal_fetch_stall || program_stall;
    assign alloc_stall = internal_alloc_stall;
    assign fetch_stage_stall = program_stall;
    assign decode_stage_stall = internal_fetch_stall;
    assign dispatch_stage_stall = internal_alloc_stall;
    
    // Frontend status assignments
    assign instruction_queue_full = frontend_inst.fetch_inst.iq_inst.full;
    assign instruction_queue_empty = frontend_inst.fetch_inst.iq_inst.empty;
    assign instruction_queue_count = frontend_inst.fetch_inst.iq_inst.count;
    
    // Debug signal assignments
    assign debug_pc = frontend_inst.fetch_inst.debug_pc;
    assign debug_cycle_count = frontend_inst.fetch_inst.debug_cycle_count;
    assign init_done = frontend_inst.fetch_inst.init_done;
    assign init_done_pulse = frontend_inst.fetch_inst.init_done_pulse;
    
    // Create arrays for decoder inputs from frontend outputs
    logic [ISSUE_WIDTH-1:0][DATA_WIDTH-1:0] fetch_instruction_array;
    logic [ISSUE_WIDTH-1:0][ADDR_WIDTH-1:0] fetch_pc_array;
    logic [ISSUE_WIDTH-1:0] fetch_valid_array;
    
    always_comb begin
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            fetch_instruction_array[i] = fec_dec_latch_out[i].instruction;
            fetch_pc_array[i] = fec_dec_latch_out[i].pc;
            fetch_valid_array[i] = fec_dec_latch_out[i].valid;
        end
    end

    // ============================================================================
    // MODULE INSTANTIATIONS
    // ============================================================================
    
    // ----------------------------------------------------------------------------
    // FRONTEND WITH BRANCH PREDICTOR
    // ----------------------------------------------------------------------------
    frontend_with_branch_predictor #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .IQ_DEPTH(IQ_DEPTH),
        .NO_INSTR(no_instr),
        .INSTRUCTION_MEMORY_SIZE(INSTRUCTION_MEMORY_SIZE),
        .XLEN(ADDR_WIDTH),
        .BTB_SETS(BTB_SETS),
        .BTB_WAYS(BTB_WAYS),
        .GSHARE_TABLE_BITS(GSHARE_TABLE_BITS),
        .RAS_DEPTH(RAS_DEPTH),
        .BPQ_DEPTH(BPQ_DEPTH)
    ) frontend_inst (
        .clk(clk),
        .rst(rst),
        .stall_in(internal_fetch_stall),
        .fetch_stall(program_stall),
        .deq_ready(dec_rdy),
        .fec_dec_latch_out(fec_dec_latch_out),
        .pc_in(pc_in),
        .update_en(update_en),
        .update_pc(update_pc),
        .update_taken(update_taken),
        .update_target(update_target),
        .flush(flush),
        .predicted_pc(predicted_pc),
        .predict_valid(predict_valid),
        .ras_sp_out(ras_sp_out),
        .bpq_count_out(bpq_count_out)
    );
    
    // ----------------------------------------------------------------------------
    // DECODER
    // ----------------------------------------------------------------------------
    tomasulo_decoder #(
        .ISSUE_WIDTH(ISSUE_WIDTH)
    ) decoder_inst (
        .clk(clk),
        .reset(rst),
        .flush(flush),
        .fetch_stall(internal_fetch_stall),
        .alloc_rdy(alloc_rdy_from_core),
        .fetch_instruction(fetch_instruction_array),
        .fetch_pc(fetch_pc_array),
        .fetch_valid(fetch_valid_array),
        .decode_reg(decode_reg),
        .dec_rdy(dec_rdy),
        .decoder_error(decoder_error)
    );
    
    // ----------------------------------------------------------------------------
    // TOMASULO CORE (Dispatch, Execute, Commit)
    // ----------------------------------------------------------------------------
    alloc_reorder_retire #(
        .DATA_WIDTH(DATA_WIDTH),
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .CDB_WIDTH(CDB_WIDTH),
        .no_ARF(no_ARF),
        .no_RAT(no_RAT),
        .no_instr(no_instr),
        .no_RS_addsublog(no_RS_addsublog),
        .no_RS_muldiv(no_RS_muldiv),
        .no_LoadStoreBuffer(no_LoadStoreBuffer),
        .no_ALU_units(no_ALU_units),
        .no_MulDiv_units(no_MulDiv_units),
        .no_ROB(no_ROB)
    ) tomasulo_core (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        
        // Completion signal
        .done(),  // Not used in top module
        
        // Control and status signals
        .alloc_stall(internal_alloc_stall),
        .pipeline_flush(),  // Not used in top module
        
        // Decode interface
        .decode_reg(decode_reg),
        .alloc_rdy(alloc_rdy_from_core),
        
        // ALU functional unit interface
        .alu_rs_out(alu_rs_out_internal),
        .alu_rs_clear_in(alu_rs_clear),
        
        // Mul/Div functional unit interface
        .muldiv_rs_out(muldiv_rs_out_internal),
        .muldiv_rs_clear_in(muldiv_rs_clear),
        
        // Load/Store buffer interface
        .mem_rs_out(mem_rs_out_internal),
        .lsb_entry_consumed(lsb_entry_consumed),
        
        // Common Data Bus (CDB) interface
        .cdb_results_in(cdb_results_int),

        // Store commit handshake
        .store_commit_valid(store_commit_valid_wire),
        .store_commit_rob_index(store_commit_rob_index_wire),
        
        // Commit stage interface
        .commit_valid(commit_valid),
        .commit_rob_id(commit_rob_id),
        .rob_out(rob_out),
        
        // Memory Management Unit interface
        .mmu_rob_head(mmu_rob_head)
    );
    
    // ----------------------------------------------------------------------------
    // FUNCTIONAL UNITS
    // ----------------------------------------------------------------------------
    func_unit func_unit_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .stall(completion_buffer_almost_full),
        .alu_rs_in(alu_rs_out_internal),
        .muldiv_rs_in(muldiv_rs_out_internal),
        .func_out(func_unit_out),
        .alu_rs_clear(alu_rs_clear),
        .muldiv_rs_clear(muldiv_rs_clear)
    );
    
    // ----------------------------------------------------------------------------
    // COMPLETION BUFFER
    // ----------------------------------------------------------------------------
    completion_buffer #(
        .BUFFER_DEPTH(no_ROB),
        .CDB_WIDTH(ISSUE_WIDTH),
        .NUM_INPUTS(no_ALU_units + 2*no_MulDiv_units + NUM_LD_PORTS),
        .SAFETY_MARGIN(SAFETY_MARGIN)
    ) completion_buffer_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .func_results(all_func_results),
        .cdb_results(cdb_results_int),
        .cdb_valid(cdb_valid_int),
        .cdb_rdy(cdb_rdy),
        .buffer_almost_full(completion_buffer_almost_full)
    );
    
    // ----------------------------------------------------------------------------
    // MEMORY SUBSYSTEM
    // ----------------------------------------------------------------------------
    top_memory_subsystem #(
        .LSB_SIZE(no_LoadStoreBuffer),
        .ROB_SIZE(no_ROB),
        .MEM_DEPTH(MEM_DEPTH),
        .DATA_WIDTH(DATA_WIDTH),
        .NUM_LD_PORTS(NUM_LD_PORTS),
        .NUM_ST_PORTS(NUM_ST_PORTS),
        .LQ_DEPTH(LQ_DEPTH),
        .SQ_DEPTH(SQ_DEPTH),
        .ISSUE_WIDTH(ISSUE_WIDTH)
    ) mem_subsystem (
        .clk(clk),
        .rst(rst),
        .lsb_entries(mem_rs_out_internal),
        .rob_head(mmu_rob_head),
        .flush(flush),
        .stall(completion_buffer_almost_full),
        .load_cdb_result(load_cdb_result_mem),
        .mem_read_en(),
        .mem_write_en(),
        .lsb_entry_consumed(lsb_entry_consumed),
        .store_commit_valid(store_commit_valid_wire),
        .store_commit_rob_index(store_commit_rob_index_wire)
    );

    assign commit_id_0 = commit_rob_id[0];
    assign commit_id_1 = commit_rob_id[1];
    assign commit_id_2 = commit_rob_id[2];

endmodule 
