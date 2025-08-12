`timescale 1ns/1ps

module tb_fec_dec;
    import fetch_types_pkg::*;
    import decoder_pkg::*;
    import tomasulo_pkg::*;

    // Parameters
    localparam int ISSUE_WIDTH = 3;
    localparam int IQ_DEPTH = 16;
    localparam int NO_INSTR = 33;
    localparam int INSTRUCTION_MEMORY_SIZE = 64;

    // DUT signals
    logic clk, rst, flush;
    logic [31:0] pc_in;
    logic fetch_stall;
    logic alloc_stall;
    decode_entry_t [ISSUE_WIDTH-1:0] decode_reg;
    logic [ISSUE_WIDTH-1:0] dec_rdy;
    logic [ISSUE_WIDTH-1:0] decoder_error;
    logic [31:0] debug_pc;
    logic [31:0] debug_cycle_count;
    logic init_done;
    logic init_done_pulse;
    
    // Stall outputs from different stages
    logic fetch_stage_stall;
    logic decode_stage_stall;
    logic dispatch_stage_stall;
    logic instruction_queue_full;
    logic instruction_queue_empty;
    logic [4:0] instruction_queue_count;
    
    // Branch prediction interface signals
    logic update_en;
    logic [31:0] update_pc;
    logic update_taken;
    logic [31:0] update_target;
    logic [31:0] predicted_pc;
    logic [ISSUE_WIDTH-1:0] predict_valid;
    logic [4:0] ras_sp_out;
    logic [4:0] bpq_count_out;
    
    // Testbench internal signals
    logic [2:0] valid_count;

    // ROB monitoring signals
    rob_entry_t [47:0] rob_out;
    logic violation_detected;
    logic [5:0] violation_rob_index;
    logic [31:0] violation_pc;

    // Commit Signlas
    logic [ISSUE_WIDTH-1:0] commit_valid;                    
    logic [$clog2(no_ROB)-1:0] commit_rob_id [ISSUE_WIDTH-1:0];

    // DUT instantiation
    top_module #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .CDB_WIDTH(3),
        .no_ARF(32),
        .no_RAT(32),
        .no_instr(NO_INSTR),
        .no_RS_addsublog(12),
        .no_RS_muldiv(4),
        .no_LoadStoreBuffer(16),
        .no_ALU_units(3),
        .no_MulDiv_units(2),
        .no_ROB(48),
        .NUM_LD_PORTS(2),
        .NUM_ST_PORTS(1),
        .IQ_DEPTH(IQ_DEPTH),
        .INSTRUCTION_MEMORY_SIZE(INSTRUCTION_MEMORY_SIZE),
        .BTB_SETS(64),
        .BTB_WAYS(4),
        .GSHARE_TABLE_BITS(10),
        .RAS_DEPTH(16),
        .BPQ_DEPTH(16),
        .LQ_DEPTH(8),
        .SQ_DEPTH(8),
        .MEM_DEPTH(1024),
        .DATA_WIDTH(32),
        .ADDR_WIDTH(32),
        .SAFETY_MARGIN(5)
    ) dut (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .pc_in(pc_in),
        .fetch_stall(fetch_stall),
        .alloc_stall(alloc_stall),
        .decode_reg(decode_reg),
        .dec_rdy(dec_rdy),
        .decoder_error(decoder_error),
        .update_en(update_en),
        .update_pc(update_pc),
        .update_taken(update_taken),
        .update_target(update_target),
        .predicted_pc(predicted_pc),
        .predict_valid(predict_valid),
        .ras_sp_out(ras_sp_out),
        .bpq_count_out(bpq_count_out),
        .debug_pc(debug_pc),
        .debug_cycle_count(debug_cycle_count),
        .init_done(init_done),
        .init_done_pulse(init_done_pulse),
        .fetch_stage_stall(fetch_stage_stall),
        .decode_stage_stall(decode_stage_stall),
        .dispatch_stage_stall(dispatch_stage_stall),
        .instruction_queue_full(instruction_queue_full),
        .instruction_queue_empty(instruction_queue_empty),
        .instruction_queue_count(instruction_queue_count),
        .commit_valid(commit_valid),
        .commit_rob_id(commit_rob_id),
        .rob_out(rob_out),
        .violation_detected(violation_detected),
        .violation_rob_index(violation_rob_index),
        .violation_pc(violation_pc)
    );

    // Clock generation
    initial clk = 0;
    always #5 clk = ~clk;

    // Test stimulus
    initial begin
        $dumpfile("tb_fec_dec.vcd");
        $dumpvars(0, tb_fec_dec);
        $dumpvars(0, tb_fec_dec.clk);
        $dumpvars(0, tb_fec_dec.rst);
        $dumpvars(0, tb_fec_dec.dut.commit_valid);
        $dumpvars(0, tb_fec_dec.dut.commit_rob_id);
        $dumpvars(0, tb_fec_dec.dut.rob_out);
        $dumpvars(0, tb_fec_dec.dut.violation_detected);
        
        $display("=== Starting Testbench ===");
        
        // Initialize debug control signals using hierarchical names
        dut.tomasulo_core.debug_print = 1'b1;  // Disable debug printing by default
        dut.mem_subsystem.mmu.mmu_debug = 1'b1;  // Enable MMU debug output by default
        dut.tomasulo_core.result_checker = 1'b1;  // Enable result checking by default
        
        // Initialize signals
        rst = 1;
        flush = 0;
        pc_in = 32'h00000000;  // Start at PC = 0
        
        // Initialize branch prediction signals
        update_en = 0;
        update_pc = 0;
        update_taken = 0;
        update_target = 0;
        
        $display("[Time %0t] Initializing with reset=1", $time);
        #20;
        rst = 0;
        $display("[Time %0t] Releasing reset", $time);
        
        @(posedge clk);
        @(posedge clk);

        $display("[Time %0t] Starting main test loop", $time);
        $display("[Time %0t] Test completed successfully", $time);
    end
  
    initial begin
      #355000ps;
      $finish;
    end

endmodule 