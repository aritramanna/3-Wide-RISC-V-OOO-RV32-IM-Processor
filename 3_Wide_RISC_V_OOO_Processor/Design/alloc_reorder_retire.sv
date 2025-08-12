 `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Module Name: alloc_reorder_retire
// Description: RISC-V Tomasulo Triple-Issue Out-of-Order Processor Core
//
// Core allocation, renaming, reordering, and retirement logic for Tomasulo-based
// out-of-order RISC-V processor with triple-issue dispatch.
//
// Key Features:
// - Triple-issue dispatch (up to 3 instructions per cycle)
// - Out-of-order execution with in-order retirement
// - Register renaming via RAT with robust dependency resolution
// - Separate reservation stations for ALU, Mul/Div, and memory ops
// - Load/Store buffer for memory operations
// - Common Data Bus (CDB) for result forwarding
//
// Version: 4.00 - RISC-V OOO Core working for non-branch instructions
// Dependencies: tomasulo_pkg.sv, decoder_pkg.sv, fetch_pkg.sv
//////////////////////////////////////

`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"
import tomasulo_pkg::*;
import decoder_pkg::*;

module alloc_reorder_retire #(
    parameter DATA_WIDTH = 32,           // Data width for registers and operands
    parameter ISSUE_WIDTH = 3,           // Maximum instructions issued per cycle
    parameter CDB_WIDTH = 3,             // Common Data Bus width (results per cycle)
    parameter no_ARF = 32,               // Number of architectural registers (RISC-V: 32)
    parameter no_RAT = 32,               // Register Alias Table size
    parameter no_instr = 33,             // Total instructions to execute
    parameter no_RS_addsublog = 12,       // ALU reservation stations
    parameter no_RS_muldiv = 4,          // Mul/Div reservation stations  
    parameter no_LoadStoreBuffer = 16,   // Load/Store buffer entries
    parameter no_ALU_units = 3,          // Number of ALU functional units
    parameter no_MulDiv_units = 2,       // Number of Mul/Div functional units
    parameter no_ROB = 48                // Reorder Buffer size
)(
    // Clock and reset
    input clk,                           // System clock
    input rst,                           // Active-high reset
    input flush,                         // Pipeline flush signal
    
    // Completion signal
    output logic done,                   // All instructions completed
    
    // Control and status signals
    output logic alloc_stall,            // Allocation stall (backpressure)
    output logic pipeline_flush,         // Pipeline flush status
    
    // Decode stage interface
    input decode_entry_t [ISSUE_WIDTH-1:0] decode_reg,  // Decoded instructions
    output logic [ISSUE_WIDTH-1:0] alloc_rdy,           // Ready signals for decode
    
    // ALU functional unit interface
    output AddSub_RS_Entry_t [no_RS_addsublog-1:0] alu_rs_out,      // ALU reservation stations
    input logic [no_RS_addsublog-1:0] alu_rs_clear_in,              // ALU RS clear signals
    
    // Mul/Div functional unit interface  
    output MulDiv_RS_Entry_t [no_RS_muldiv-1:0] muldiv_rs_out,      // Mul/Div reservation stations
    input logic [no_RS_muldiv-1:0] muldiv_rs_clear_in,              // Mul/Div RS clear signals
    
    // Load/Store buffer interface
    output LS_Buffer_Entry_t [no_LoadStoreBuffer-1:0] mem_rs_out,   // Load/Store buffer entries
    input logic [no_LoadStoreBuffer-1:0] lsb_entry_consumed,        // LSB entry consumed signals
    
    // Common Data Bus (CDB) interface
    input ALU_Result_t [ISSUE_WIDTH-1:0] cdb_results_in,            // Results from functional units
    
    // Commit stage interface
    output logic [ISSUE_WIDTH-1:0] commit_valid,                    // Valid commit signals
    output logic [$clog2(no_ROB)-1:0] commit_rob_id [ISSUE_WIDTH-1:0], // Committed ROB indices
    output rob_entry_t [no_ROB-1:0] rob_out,                        // Complete ROB state
    
    // Memory Management Unit interface
    output logic [$clog2(no_ROB)-1:0] mmu_rob_head [ISSUE_WIDTH-1:0],                 // ROB head for MMU
    input logic [ISSUE_WIDTH-1:0] store_commit_valid,
    input logic [$clog2(no_ROB)-1:0] store_commit_rob_index [ISSUE_WIDTH-1:0]
);
    logic debug_print;
    logic result_checker;

    // ============================================================================
    // INTERNAL SIGNALS AND MEMORY STRUCTURES
    // ============================================================================
    
    // Pipeline registers - hold instructions between dispatch and allocation
    dispatch_entry_t [ISSUE_WIDTH-1:0] dispatch_reg;
    dispatch_entry_t [ISSUE_WIDTH-1:0] dispatch_reg_next;
    
    // Instruction tracking
    logic [$clog2(no_instr)-1:0] instr_ID;  // Global instruction ID counter
    
    // ============================================================================
    // REGISTER FILES AND RENAMING STRUCTURES
    // ============================================================================
    
    // Architectural Register File (ARF) - visible to software
    logic [DATA_WIDTH-1:0] ARF[0:no_ARF-1];           // Current ARF state
    logic [DATA_WIDTH-1:0] ARF_next[0:no_ARF-1];      // Next-cycle ARF state
    
    // Register Alias Table (RAT) - maps logical to physical registers
    logic [$clog2(no_ROB)-1:0] RAT_tag [0:no_RAT-1];      // ROB tag for each register
    logic RAT_valid [0:no_RAT-1];                         // Valid bit for each register
    
    // ============================================================================
    // REORDER BUFFER AND RESERVATION STATIONS
    // ============================================================================
    
    // Reorder Buffer (ROB) - ensures in-order retirement
    rob_entry_t ROB [no_ROB-1:0];
    
    // Reservation Stations - track operands and dependencies
    AddSub_RS_Entry_t AS_RS [no_RS_addsublog-1:0];    // ALU reservation stations
    MulDiv_RS_Entry_t MD_RS [no_RS_muldiv-1:0];       // Mul/Div reservation stations
    LS_Buffer_Entry_t LS_buffer [no_LoadStoreBuffer-1:0]; // Load/Store buffer
    
    // ============================================================================
    // COMMON DATA BUS (CDB) STRUCTURES
    // ============================================================================
    
    // CDB buffer - for stages that need buffered values (RS allocation, LSB updates)
    
    // ============================================================================
    // ROB POINTERS AND CONTROL
    // ============================================================================
    
    // ROB circular buffer pointers
    logic [$clog2(no_ROB)-1:0] ROB_head;  // Next ROB entry to allocate
    logic [$clog2(no_ROB)-1:0] ROB_tail;  // Next ROB entry to commit
    
    // ============================================================================
    // PERFORMANCE MONITORING
    // ============================================================================
    
    logic [DATA_WIDTH-1:0] cycle_count;                    // Total cycles executed
    logic [DATA_WIDTH-1:0] total_instructions_committed;   // Instructions retired
    logic all_instructions_completed;                      // Completion flag
    
    // IPC calculation variables - track execution time from first commit
    logic [DATA_WIDTH-1:0] execution_start_cycle;          // Cycle when first instruction commits
    logic [DATA_WIDTH-1:0] execution_cycles;               // Cycles from first commit to completion
    logic first_commit_detected;                           // Flag to detect first commit
    logic [DATA_WIDTH-1:0] instructions_executed;          // Instructions executed during measured period

    // ============================================================================
    // RESOURCE AVAILABILITY TRACKING
    // ============================================================================
    
    // Required resources for current instruction group
    logic [$clog2(no_ROB+1)-1:0] required_rob_entries;    // ROB entries needed
    logic [$clog2(no_RS_addsublog+1)-1:0] required_alu_rs;         // ALU RS needed
    logic [$clog2(no_RS_muldiv+1)-1:0] required_muldiv_rs;      // Mul/Div RS needed
    logic [$clog2(no_LoadStoreBuffer+1)-1:0] required_mem_rs;         // Memory RS needed
    
    // Available resources in the system
    logic [$clog2(no_ROB+1)-1:0] available_rob_entries;   // Free ROB entries
    logic [$clog2(no_RS_addsublog+1)-1:0] available_alu_rs;        // Free ALU RS
    logic [$clog2(no_RS_muldiv+1)-1:0] available_muldiv_rs;     // Free Mul/Div RS
    logic [$clog2(no_LoadStoreBuffer+1)-1:0] available_mem_rs; // Free memory RS
    logic alloc_resources_available;     // All resources available flag

    // Delayed version of alloc_resources_available
    logic alloc_resources_available_d1;

    // Delayed version of alu_rs_clear_in, muldiv_rs_clear_in, and lsb_entry_consumed
    logic [no_RS_addsublog-1:0] alu_rs_clear_in_d1;
    logic [no_RS_muldiv-1:0] muldiv_rs_clear_in_d1;
    logic [no_LoadStoreBuffer-1:0] lsb_entry_consumed_d1;

    // Combinational retire info for up to ISSUE_WIDTH retirements
    ALU_Result_t retire_info_current [ISSUE_WIDTH-1:0];
    // Add this to your declarations section (near other ALU_Result_t variables):
    ALU_Result_t retire_info_reg [ISSUE_WIDTH-1:0];
    // Add 1-cycle and 2-cycle delayed retirement info for extended wakeup
    ALU_Result_t retire_info_reg_1 [ISSUE_WIDTH-1:0];
    ALU_Result_t retire_info_reg_2 [ISSUE_WIDTH-1:0];

    // ============================================================================
    // LOOP VARIABLES (DECLARED OUTSIDE PROCEDURAL BLOCKS)
    // ============================================================================
    
    // Module-level found flags for robust wakeup logic
    integer alu_found_src1 [0:no_RS_addsublog-1];
    integer alu_found_src2 [0:no_RS_addsublog-1];
    integer muldiv_found_src1 [0:no_RS_muldiv-1];
    integer muldiv_found_src2 [0:no_RS_muldiv-1];
    integer lsb_found_src [0:no_LoadStoreBuffer-1];
    integer lsb_found_store_data [0:no_LoadStoreBuffer-1];
    
    // ============================================================================
    // INSTRUMENTATION LOGIC DECLARATIONS
    // ============================================================================
    
    // Logic flags for conditional instrumentation output
    logic decode_valid_found, dispatch_valid_found, rob_valid_found, alu_rs_busy_found, md_rs_busy_found;
    logic lsb_busy_found, cdb_valid_found,forwarding_found, committed_found, next_rat_valid_found; 
    logic current_rat_valid_found, arf_nonzero_found;
    
    // ============================================================================
    // DEPENDENCY RESOLUTION VARIABLES
    // ============================================================================
    
    // Temporary variables for operand dependency resolution
    logic [$clog2(no_ROB)-1:0] current_src2_tag, current_alu_src1_tag, current_alu_src2_tag;
    logic current_src2_ready, current_alu_src1_ready, current_alu_src2_ready;
    logic [DATA_WIDTH-1:0] current_src2_value, current_alu_src1_value, current_alu_src2_value;
    // Retirement bypass hit flags for src1 and src2
    logic retire_bypass_hit_src1, retire_bypass_hit_src2;
    
    // 1-cycle and 2-cycle registered CDB for extended wakeup
    ALU_Result_t [CDB_WIDTH-1:0] cdb_results_reg_1;
    ALU_Result_t [CDB_WIDTH-1:0] cdb_results_reg_2;

    // Temporary RAT for intra-bundle dependency resolution
    logic [$clog2(no_ROB)-1:0] temp_RAT_tag [0:no_RAT-1];
    logic temp_RAT_valid [0:no_RAT-1];
    
    // ============================================================================
    // RESERVATION STATION ALLOCATION TRACKING
    // ============================================================================
    
    // Per-cycle allocation tracking to prevent double-allocation
    logic [no_RS_addsublog-1:0] alu_rs_allocated_this_cycle;
    logic [no_RS_muldiv-1:0] muldiv_rs_allocated_this_cycle;
    logic [no_LoadStoreBuffer-1:0] lsb_allocated_this_cycle;
    
    // Next-state busy arrays for resource allocation
    logic [no_RS_addsublog-1:0] alu_rs_busy_next;
    logic [no_RS_muldiv-1:0] muldiv_rs_busy_next;
    logic [no_LoadStoreBuffer-1:0] lsb_busy_next;
    
    // Post-dispatch busy state for availability calculation
    logic [no_RS_addsublog-1:0] alu_rs_busy_post_dispatch;
    logic [no_RS_muldiv-1:0] muldiv_rs_busy_post_dispatch;
    logic [no_LoadStoreBuffer-1:0] lsb_busy_post_dispatch;

    // Found flags for robust wakeup logic
    bit found_src1, found_src2, found_src, found_store;

    // Add module-level loop variable declarations for all loops and flags
    integer calc_idx, alloc_idx, dispatch_idx, rs_idx, muldiv_idx, ls_idx, store_commit_idx;
    integer calc_idx_alu, calc_idx_muldiv, calc_idx_lsb, prev;
    integer rs_found_flag, ls_found_flag, md_found_flag;
    integer alu_idx, as_idx, md_idx, arf_idx, rat_idx;

    // ============================================================================
    // COMMIT STAGE CONTROL SIGNALS
    // ============================================================================
    
    logic any_valid_instruction;         // Any valid instruction in decode
    integer next_rob_tail;               // Next ROB tail pointer
    integer next_total_instructions_committed; // Next commit counter
    logic disable_commit_loop;           // Stop commit loop flag
    logic commit_happened;               // Commit occurred this cycle
    logic [ISSUE_WIDTH-1:0] commit_valid_next;  // Next-cycle commit valid signals
    logic [$clog2(no_ROB)-1:0] commit_rob_id_next [ISSUE_WIDTH-1:0]; // Next-cycle commit ROB IDs

    integer commit_idx, committed_count;
    integer retiring_rob_idx_dispatch;
    
    // ============================================================================
    // RESOURCE AVAILABILITY CALCULATION
    // ============================================================================
    // This block calculates the number of available resources in the system
    // to determine if new instructions can be dispatched
    
    always_comb begin
        // Count available ROB entries
        available_rob_entries = 0;
        for(calc_idx = 0; calc_idx < no_ROB; calc_idx++) begin
            if(!ROB[calc_idx].valid) begin
                available_rob_entries = available_rob_entries + 1;
            end
        end
        // Count available ALU reservation stations
        available_alu_rs = 0;
        for(calc_idx = 0; calc_idx < no_RS_addsublog; calc_idx++) begin
            if(!alu_rs_busy_post_dispatch[calc_idx]) begin
                available_alu_rs = available_alu_rs + 1;
            end
        end
        // Count available Mul/Div reservation stations
        available_muldiv_rs = 0;
        for(calc_idx = 0; calc_idx < no_RS_muldiv; calc_idx++) begin
            if(!muldiv_rs_busy_post_dispatch[calc_idx]) begin
                available_muldiv_rs = available_muldiv_rs + 1;
            end
        end
        // Count available Load/Store buffer entries
        available_mem_rs = 0;
        for(calc_idx = 0; calc_idx < no_LoadStoreBuffer; calc_idx++) begin
            if(!lsb_busy_post_dispatch[calc_idx]) begin
                available_mem_rs = available_mem_rs + 1;
            end
        end
    end

    always_ff @(posedge clk) begin
        if (rst || flush) begin
            alu_rs_clear_in_d1 <= '0;
            muldiv_rs_clear_in_d1 <= '0;
            lsb_entry_consumed_d1 <= '0;
        end else begin
            alu_rs_clear_in_d1 <= alu_rs_clear_in;
            muldiv_rs_clear_in_d1 <= muldiv_rs_clear_in;
            lsb_entry_consumed_d1 <= lsb_entry_consumed;
        end
    end
    
    // ============================================================================
    // VALID INSTRUCTION DETECTION
    // ============================================================================
    // Check if any valid instructions are present in the decode stage
    
    always_comb begin
        any_valid_instruction = 1'b0;
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            if (decode_reg[i].valid) begin
                any_valid_instruction = 1'b1;
            end
        end
    end

     always_ff @(posedge clk) begin
        if (rst || flush) begin
            alloc_resources_available_d1 <= 1'b0;
        end else begin
            alloc_resources_available_d1 <= alloc_resources_available;
        end
    end
    
    // ============================================================================
    // RESOURCE AVAILABILITY CHECKING AND STALL LOGIC
    // ============================================================================
    // This block determines if resources are available for dispatch and
    // generates stall signals to prevent resource conflicts
    
    always_comb begin
        // Initialize resource requirements
        alloc_resources_available = 1'b1;
        required_rob_entries = 0;
        required_alu_rs = 0;
        required_muldiv_rs = 0;
        required_mem_rs = 0;
        
        // Calculate required resources for current instruction group
        for(alloc_idx = 0; alloc_idx < ISSUE_WIDTH; alloc_idx++) begin
            if(decode_reg[alloc_idx].valid) begin
                required_rob_entries = required_rob_entries + 1;
                case(decode_reg[alloc_idx].instr_type)
                    2'b00: required_alu_rs = required_alu_rs + 1;      // ALU instruction
                    2'b01: required_mem_rs = required_mem_rs + 1;      // Load/Store instruction
                    2'b10: required_muldiv_rs = required_muldiv_rs + 1; // Mul/Div instruction
                    2'b11: ; // Branch/Jump instruction (no resource needed)
                    default: alloc_resources_available = 1'b0;         // Invalid instruction type
                endcase
            end
        end
        
        // Check if all required resources are available
        if(required_rob_entries > available_rob_entries) alloc_resources_available = 1'b0;
        if(required_alu_rs > available_alu_rs) alloc_resources_available = 1'b0;
        if(required_muldiv_rs > available_muldiv_rs) alloc_resources_available = 1'b0;
        if(required_mem_rs > available_mem_rs) alloc_resources_available = 1'b0;
        
        // Generate stall signal based on resource availability
        if (any_valid_instruction) begin
            alloc_stall = !alloc_resources_available;  // Stall if resources unavailable
        end else begin
            // No valid instructions - stall if any resource is exhausted
            alloc_stall = !(available_rob_entries > 0 && available_alu_rs > 0 && 
                           available_muldiv_rs > 0 && available_mem_rs > 0);
        end
        
        // Generate ready signals for decode stage
        for(alloc_idx = 0; alloc_idx < ISSUE_WIDTH; alloc_idx++) begin
            alloc_rdy[alloc_idx] = !alloc_stall;
        end

        // Block allocation if instr_ID >= no_instr
        if (instr_ID >= (no_instr-1)) alloc_resources_available = 1'b0;

    end
    
    // ============================================================================
    // DISPATCH STAGE - ROB ALLOCATION AND DEPENDENCY RESOLUTION
    // ============================================================================
    // This stage allocates ROB entries, resolves operand dependencies,
    // and prepares instructions for reservation station allocation
    
    always @(posedge clk) begin
        if(rst || flush) begin
            // Reset or flush dispatch stage - clear all dispatch registers
            for(dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                dispatch_reg_next[dispatch_idx] = '0;
            end
            for(dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                dispatch_reg[dispatch_idx] <= dispatch_reg_next[dispatch_idx];
            end
            // Reset post-dispatch busy signals
            for (rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                alu_rs_busy_post_dispatch[rs_idx] = 1'b0;
                alu_rs_allocated_this_cycle[rs_idx] = 1'b0;
            end
            for (muldiv_idx = 0; muldiv_idx < no_RS_muldiv; muldiv_idx++) begin
                muldiv_rs_busy_post_dispatch[muldiv_idx] = 1'b0;
                muldiv_rs_allocated_this_cycle[muldiv_idx] = 1'b0;
            end
            for (ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                lsb_busy_post_dispatch[ls_idx] = 1'b0;
                lsb_allocated_this_cycle[ls_idx] = 1'b0;
            end

            for (int i = 0; i < no_RAT; i++) begin
                temp_RAT_tag[i] = '0;
                temp_RAT_valid[i] = 1'b0;
            end

            ROB_head <= '0;
            instr_ID <= '0;
            
        end else begin
            // ============================================================================
            // INSTRUCTION DISPATCH - RESOURCE ALLOCATION
            // ============================================================================

            // Update busy arrays using registered clear signals
            
            for (calc_idx_alu = 0; calc_idx_alu < no_RS_addsublog; calc_idx_alu++) begin
                if (alu_rs_clear_in_d1[calc_idx_alu]) begin
                    alu_rs_busy_post_dispatch[calc_idx_alu] = 1'b0;
                end
            end

            for (calc_idx_muldiv = 0; calc_idx_muldiv < no_RS_muldiv; calc_idx_muldiv++) begin
                if (muldiv_rs_clear_in_d1[calc_idx_muldiv]) begin
                    muldiv_rs_busy_post_dispatch[calc_idx_muldiv] = 1'b0;
                end
            end
            
            for (calc_idx_lsb = 0; calc_idx_lsb < no_LoadStoreBuffer; calc_idx_lsb++) begin
                if (lsb_entry_consumed_d1[calc_idx_lsb]) begin
                    lsb_busy_post_dispatch[calc_idx_lsb] = 1'b0;
                end
            end

            // Apply retirements to temp_RAT (clear mappings for registers being retired this cycle)
            for (commit_idx = 0; commit_idx < ISSUE_WIDTH; commit_idx++) begin
                retiring_rob_idx_dispatch = (ROB_tail + commit_idx) % no_ROB;
                if (commit_valid_next[commit_idx]) begin
                    // Clear ALL RAT mappings that point to this retiring ROB entry
                    for (int rat_reg = 0; rat_reg < no_RAT; rat_reg++) begin
                        if (RAT_valid[rat_reg] && RAT_tag[rat_reg] == retiring_rob_idx_dispatch) begin

                            temp_RAT_tag[rat_reg] = '0;
                            temp_RAT_valid[rat_reg] = 1'b0;
                        end
                    end
                end
            end
            
            // Only proceed if all required resources are available
            if(alloc_resources_available) begin
                // Initialize next-state busy arrays for resource tracking
                for (rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    alu_rs_busy_next[rs_idx] = alu_rs_busy_post_dispatch[rs_idx];
                end
                for (muldiv_idx = 0; muldiv_idx < no_RS_muldiv; muldiv_idx++) begin
                    muldiv_rs_busy_next[muldiv_idx] = muldiv_rs_busy_post_dispatch[muldiv_idx];
                end
                for (ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                    lsb_busy_next[ls_idx] = lsb_busy_post_dispatch[ls_idx];
                end

                // Clear per-cycle allocation trackers to prevent double-allocation
                alu_rs_allocated_this_cycle = '0;
                muldiv_rs_allocated_this_cycle = '0;
                lsb_allocated_this_cycle = '0;

               

                // For each instruction in the bundle, resolve sources, then update temp RAT for dest
                for (dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                    if(decode_reg[dispatch_idx].valid) begin
                        // Copy instruction fields from decode to dispatch register
                        dispatch_reg_next[dispatch_idx].valid = 1'b1;
                        dispatch_reg_next[dispatch_idx].instruction = decode_reg[dispatch_idx].instruction;
                        dispatch_reg_next[dispatch_idx].pc = decode_reg[dispatch_idx].pc;
                        dispatch_reg_next[dispatch_idx].opcode = decode_reg[dispatch_idx].opcode;
                        dispatch_reg_next[dispatch_idx].func3 = decode_reg[dispatch_idx].func3;
                        dispatch_reg_next[dispatch_idx].func7 = decode_reg[dispatch_idx].func7;
                        dispatch_reg_next[dispatch_idx].src1 = decode_reg[dispatch_idx].src1;
                        dispatch_reg_next[dispatch_idx].src2 = decode_reg[dispatch_idx].src2;
                        dispatch_reg_next[dispatch_idx].dst = decode_reg[dispatch_idx].dst;
                        dispatch_reg_next[dispatch_idx].immediate = decode_reg[dispatch_idx].immediate;
                        dispatch_reg_next[dispatch_idx].offset = decode_reg[dispatch_idx].offset;
                        dispatch_reg_next[dispatch_idx].operation = decode_reg[dispatch_idx].operation;
                        dispatch_reg_next[dispatch_idx].instr_type = decode_reg[dispatch_idx].instr_type;

                        // SOURCE OPERAND 1 DEPENDENCY RESOLUTION (with robust zero-cycle wakeup)
                        found_src1 = 0;
                        // 1. Check for intra-bundle dependency (most recent instruction in this bundle writes src1)
                        for (prev = dispatch_idx - 1; prev >= 0; prev--) begin
                            if (decode_reg[prev].valid && decode_reg[prev].dst != 0 && decode_reg[prev].dst == decode_reg[dispatch_idx].src1 && !found_src1) begin
                                dispatch_reg_next[dispatch_idx].src1_tag = (ROB_head + prev) % no_ROB;
                                dispatch_reg_next[dispatch_idx].src1_value = '0;
                                dispatch_reg_next[dispatch_idx].src1_ready = 1'b0;
                                found_src1 = 1;
                            end
                        end
                        // 2. If not intra-bundle, check temp RAT for in-flight mapping
                        if (!found_src1) begin
                            if (temp_RAT_valid[decode_reg[dispatch_idx].src1]) begin
                                dispatch_reg_next[dispatch_idx].src1_tag = temp_RAT_tag[decode_reg[dispatch_idx].src1];
                                dispatch_reg_next[dispatch_idx].src1_value = '0;
                                dispatch_reg_next[dispatch_idx].src1_ready = 1'b0;
                                // Zero-cycle CDB bypass for src1
                                for (int cdb_idx = 0; cdb_idx < CDB_WIDTH; cdb_idx++) begin
                                    // Debug: Print CDB check for specific tag
                                    if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == dispatch_reg_next[dispatch_idx].src1_tag) begin
                                        dispatch_reg_next[dispatch_idx].src1_value = cdb_results_in[cdb_idx].result;
                                        dispatch_reg_next[dispatch_idx].src1_ready = 1'b1;
                                        // Late CDB bypass for src1
                                    end else if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == dispatch_reg_next[dispatch_idx].src1_tag) begin
                                        dispatch_reg_next[dispatch_idx].src1_value = cdb_results_reg_1[cdb_idx].result;
                                        dispatch_reg_next[dispatch_idx].src1_ready = 1'b1;
                                    end 
                                end
                                // Retirement bypass for src1
                                for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                                    if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == dispatch_reg_next[dispatch_idx].src1_tag) begin
                                        dispatch_reg_next[dispatch_idx].src1_value = retire_info_current[retire_idx].result;
                                        dispatch_reg_next[dispatch_idx].src1_ready = 1'b1;
                                    end
                                end
                            end else if (decode_reg[dispatch_idx].src1 == 0) begin
                                dispatch_reg_next[dispatch_idx].src1_tag = 0;
                                dispatch_reg_next[dispatch_idx].src1_value = '0;
                                dispatch_reg_next[dispatch_idx].src1_ready = 1'b1;
                            end else begin
                                // No RAT mapping, read directly from ARF (no retirement bypass)
                                dispatch_reg_next[dispatch_idx].src1_value = ARF_next[decode_reg[dispatch_idx].src1];
                                dispatch_reg_next[dispatch_idx].src1_ready = 1'b1;
                            end
                        end
                        // SOURCE OPERAND 2 DEPENDENCY RESOLUTION (with robust zero-cycle wakeup)
                        found_src2 = 0;
                        // 1. Check for intra-bundle dependency (most recent instruction in this bundle writes src2)
                        for (prev = dispatch_idx - 1; prev >= 0; prev--) begin
                            if (decode_reg[prev].valid && decode_reg[prev].dst != 0 && decode_reg[prev].dst == decode_reg[dispatch_idx].src2 && !found_src2) begin
                                dispatch_reg_next[dispatch_idx].src2_tag = (ROB_head + prev) % no_ROB;
                                dispatch_reg_next[dispatch_idx].src2_value = '0;
                                dispatch_reg_next[dispatch_idx].src2_ready = 1'b0;
                                found_src2 = 1;
                            end
                        end
                        // 2. If not intra-bundle, check temp RAT for in-flight mapping
                        if (!found_src2) begin
                            if (temp_RAT_valid[decode_reg[dispatch_idx].src2]) begin
                                dispatch_reg_next[dispatch_idx].src2_tag = temp_RAT_tag[decode_reg[dispatch_idx].src2];
                                dispatch_reg_next[dispatch_idx].src2_value = '0;
                                dispatch_reg_next[dispatch_idx].src2_ready = 1'b0;
                                // Zero-cycle CDB bypass for src2
                                for (int cdb_idx = 0; cdb_idx < CDB_WIDTH; cdb_idx++) begin
                                    if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == dispatch_reg_next[dispatch_idx].src2_tag) begin
                                        dispatch_reg_next[dispatch_idx].src2_value = cdb_results_in[cdb_idx].result;
                                        dispatch_reg_next[dispatch_idx].src2_ready = 1'b1;
                                        // Late CDB bypass for src2
                                    end else if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == dispatch_reg_next[dispatch_idx].src2_tag) begin
                                        dispatch_reg_next[dispatch_idx].src2_value = cdb_results_reg_1[cdb_idx].result;
                                        dispatch_reg_next[dispatch_idx].src2_ready = 1'b1;
                                    end 
                                end
                                // Retirement bypass for src2
                                for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                                    if (retire_info_current[retire_idx].result_ready &&
                                        retire_info_current[retire_idx].ROB_index == dispatch_reg_next[dispatch_idx].src2_tag) begin
                                        dispatch_reg_next[dispatch_idx].src2_value = retire_info_current[retire_idx].result;
                                        dispatch_reg_next[dispatch_idx].src2_ready = 1'b1;
                                    end
                                end
                            end else if (decode_reg[dispatch_idx].src2 == 0) begin
                                dispatch_reg_next[dispatch_idx].src2_tag = 0;
                                dispatch_reg_next[dispatch_idx].src2_value = '0;
                                dispatch_reg_next[dispatch_idx].src2_ready = 1'b1;
                            end else begin
                                // No RAT mapping, read directly from ARF (no retirement bypass)
                                dispatch_reg_next[dispatch_idx].src2_value = ARF_next[decode_reg[dispatch_idx].src2];
                                dispatch_reg_next[dispatch_idx].src2_ready = 1'b1;
                            end
                        end
                        dispatch_reg_next[dispatch_idx].rob_index = (ROB_head + dispatch_idx) % no_ROB;
                        dispatch_reg_next[dispatch_idx].rob_allocated = 1'b1;
                        case(decode_reg[dispatch_idx].instr_type)
                            2'b00: begin // ALU instruction
                                rs_found_flag = 0;
                                for(rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                                    if(!alu_rs_busy_next[rs_idx] && !alu_rs_allocated_this_cycle[rs_idx] && !rs_found_flag) begin
                                        dispatch_reg_next[dispatch_idx].rs_index = rs_idx;
                                        dispatch_reg_next[dispatch_idx].rs_allocated = 1'b1;
                                        alu_rs_allocated_this_cycle[rs_idx] = 1'b1;
                                        alu_rs_busy_next[rs_idx] = 1'b1; // Mark as allocated for this cycle
                                        rs_found_flag = 1;
                                    end
                                end
                                if (!rs_found_flag) dispatch_reg_next[dispatch_idx].rs_allocated = 1'b0;
                            end
                            2'b01: begin // Load/Store instruction
                                ls_found_flag = 0;
                                for(ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                                    if(!lsb_busy_next[ls_idx] && !lsb_allocated_this_cycle[ls_idx] && !ls_found_flag) begin
                                        dispatch_reg_next[dispatch_idx].rs_index = ls_idx;
                                        dispatch_reg_next[dispatch_idx].rs_allocated = 1'b1;
                                        lsb_allocated_this_cycle[ls_idx] = 1'b1;
                                        lsb_busy_next[ls_idx] = 1'b1; // Mark as allocated for this cycle
                                        ls_found_flag = 1;
                                    end
                                end
                                if (!ls_found_flag) dispatch_reg_next[dispatch_idx].rs_allocated = 1'b0;
                            end
                            2'b10: begin // Mul/Div instruction
                                md_found_flag = 0;
                                for(muldiv_idx = 0; muldiv_idx < no_RS_muldiv; muldiv_idx++) begin
                                    if(!muldiv_rs_busy_next[muldiv_idx] && !muldiv_rs_allocated_this_cycle[muldiv_idx] && !md_found_flag) begin
                                        dispatch_reg_next[dispatch_idx].rs_index = muldiv_idx;
                                        dispatch_reg_next[dispatch_idx].rs_allocated = 1'b1;
                                        muldiv_rs_allocated_this_cycle[muldiv_idx] = 1'b1;
                                        muldiv_rs_busy_next[muldiv_idx] = 1'b1; // Mark as allocated for this cycle
                                        md_found_flag = 1;
                                    end
                                end
                                if (!md_found_flag) dispatch_reg_next[dispatch_idx].rs_allocated = 1'b0;
                            end
                        endcase
                    end else begin
                        dispatch_reg_next[dispatch_idx] = '0;
                    end
                    // Now update temp RAT for this instruction's destination
                    if (decode_reg[dispatch_idx].valid && decode_reg[dispatch_idx].dst != 0) begin
                        temp_RAT_tag[decode_reg[dispatch_idx].dst] = (ROB_head + dispatch_idx) % no_ROB;
                        temp_RAT_valid[decode_reg[dispatch_idx].dst] = 1'b1;
                    end
                end // End of dispatch loop

                // PHASE 2: Update RAT for all destinations in the bundle (Resolving intra bundle dependencies)
                for (dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                    if (dispatch_reg_next[dispatch_idx].valid && dispatch_reg_next[dispatch_idx].dst != 0) begin
                        temp_RAT_tag[dispatch_reg_next[dispatch_idx].dst] = dispatch_reg_next[dispatch_idx].rob_index;
                        temp_RAT_valid[dispatch_reg_next[dispatch_idx].dst] = 1'b1;
                    end
                end
                for(dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                    dispatch_reg[dispatch_idx] <= dispatch_reg_next[dispatch_idx];
                end
                // Update ROB head pointer for all allocated instructions
                ROB_head <= (ROB_head + required_rob_entries) % no_ROB;
                instr_ID <= instr_ID + required_rob_entries;
                // --- Handoff next-state busy arrays for use in next cycle ---
                for (rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    alu_rs_busy_post_dispatch[rs_idx] = alu_rs_busy_next[rs_idx];
                end
                for (muldiv_idx = 0; muldiv_idx < no_RS_muldiv; muldiv_idx++) begin
                    muldiv_rs_busy_post_dispatch[muldiv_idx] = muldiv_rs_busy_next[muldiv_idx];
                end
                for (ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                    lsb_busy_post_dispatch[ls_idx] = lsb_busy_next[ls_idx];
                end
            
            end else begin
                // During stall: preserve dispatch buffer contents - do nothing
                // This prevents the dispatch buffer from being cleared during stall
            end
        end
    end
    
    // ============================================================================
    // RESERVATION STATION ALLOCATION LOGIC
    // ============================================================================
    // This stage allocates instructions to reservation stations and updates
    // the ROB with instruction information
    
    // Register the CDB input for wakeup logic
    always @(posedge clk) begin
        if (rst) begin
            for (int i = 0; i < CDB_WIDTH; i++) begin
                cdb_results_reg_1[i] <= '0;
                cdb_results_reg_2[i] <= '0;
            end
        end else begin
            for (int i = 0; i < CDB_WIDTH; i++) begin
                // Register all CDB results for wakeup logic (no filtering)
                cdb_results_reg_2[i] <= cdb_results_reg_1[i];
                cdb_results_reg_1[i] <= cdb_results_in[i];
            end
        end
    end
    
    always @(posedge clk) begin
        if(rst || flush) begin
            // Reset or flush - clear all reservation stations
            for(alu_idx = 0; alu_idx < no_RS_addsublog; alu_idx++) begin
                AS_RS[alu_idx].busy <= 1'b0;
                AS_RS[alu_idx].ready <= 1'b0;
                AS_RS[alu_idx].instr_ID <= '0;
                AS_RS[alu_idx].pc <= '0;
                AS_RS[alu_idx].operation <= '0;
                AS_RS[alu_idx].src1_valid <= 1'b0;
                AS_RS[alu_idx].src1_tag <= '0;
                AS_RS[alu_idx].src1_value <= '0;
                AS_RS[alu_idx].src2_valid <= 1'b0;
                AS_RS[alu_idx].src2_tag <= '0;
                AS_RS[alu_idx].src2_value <= '0;
                AS_RS[alu_idx].ROB_index <= '0;
                AS_RS[alu_idx].immediate <= '0;
            end
            for(muldiv_idx = 0; muldiv_idx < no_RS_muldiv; muldiv_idx++) begin
                MD_RS[muldiv_idx].busy <= 1'b0;
                MD_RS[muldiv_idx].ready <= 1'b0;
                MD_RS[muldiv_idx].instr_ID <= '0;
                MD_RS[muldiv_idx].pc <= '0;
                MD_RS[muldiv_idx].operation <= '0;
                MD_RS[muldiv_idx].src1_valid <= 1'b0;
                MD_RS[muldiv_idx].src1_tag <= '0;
                MD_RS[muldiv_idx].src1_value <= '0;
                MD_RS[muldiv_idx].src2_valid <= 1'b0;
                MD_RS[muldiv_idx].src2_tag <= '0;
                MD_RS[muldiv_idx].src2_value <= '0;
                MD_RS[muldiv_idx].ROB_index <= '0;
            end
            for(ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                LS_buffer[ls_idx].busy <= 1'b0;
                LS_buffer[ls_idx].ready <= 1'b0;
                LS_buffer[ls_idx].instr_ID <= '0;
                LS_buffer[ls_idx].pc <= '0;
                LS_buffer[ls_idx].op <= '0;
                LS_buffer[ls_idx].src_valid <= 1'b0;
                LS_buffer[ls_idx].src_tag <= '0;
                LS_buffer[ls_idx].src_value <= '0;
                LS_buffer[ls_idx].A <= '0;
                LS_buffer[ls_idx].ROB_index <= '0;
            end
        end else begin
            
            // Un-busy ALU reservation stations when alu_rs_clear_in is high
            for (int i = 0; i < no_RS_addsublog; i++) begin
                if (alu_rs_clear_in[i]) begin
                    AS_RS[i].busy <= 1'b0;
                end
            end
            // Un-busy MulDiv reservation stations when muldiv_rs_clear_in is high
            for (int i = 0; i < no_RS_muldiv; i++) begin
                if (muldiv_rs_clear_in[i]) begin
                    MD_RS[i].busy <= 1'b0;
                end
            end
            // Un-busy LSB entries when lsb_entry_consumed is high
            for (int i = 0; i < no_LoadStoreBuffer; i++) begin
                if (lsb_entry_consumed[i]) begin
                    LS_buffer[i].busy <= 1'b0;
                end
            end

            if (alloc_resources_available_d1) begin
            // Allocate instructions to reservation stations when dispatch is complete
            for(dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                if(dispatch_reg[dispatch_idx].valid && dispatch_reg[dispatch_idx].rs_allocated && dispatch_reg[dispatch_idx].rob_allocated) begin
                    // Allocate to ROB
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].valid = 1'b1;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].ready = 1'b0;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].dest = dispatch_reg[dispatch_idx].dst;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].value = '0;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].instr_ID = instr_ID - required_rob_entries + dispatch_idx;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].pc = dispatch_reg[dispatch_idx].pc;
                    ROB_next[dispatch_reg[dispatch_idx].rob_index].opcode = {dispatch_reg[dispatch_idx].func7, dispatch_reg[dispatch_idx].func3, dispatch_reg[dispatch_idx].opcode}; 
                    // Allocate to appropriate reservation station
                    case(dispatch_reg[dispatch_idx].instr_type)
                        2'b00: begin // ALU
                            if (!AS_RS[dispatch_reg[dispatch_idx].rs_index].busy) begin
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].busy <= 1'b1;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].ROB_index <= dispatch_reg[dispatch_idx].rob_index;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].operation <= dispatch_reg[dispatch_idx].operation;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src1_tag <= dispatch_reg[dispatch_idx].src1_tag;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src1_value <= dispatch_reg[dispatch_idx].src1_value;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src1_valid <= dispatch_reg[dispatch_idx].src1_ready;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src2_tag <= dispatch_reg[dispatch_idx].src2_tag;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src2_value <= dispatch_reg[dispatch_idx].src2_value;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].src2_valid <= dispatch_reg[dispatch_idx].src2_ready;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].immediate <= dispatch_reg[dispatch_idx].immediate;
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].ready <= (dispatch_reg[dispatch_idx].src1_ready && dispatch_reg[dispatch_idx].src2_ready);
                                AS_RS[dispatch_reg[dispatch_idx].rs_index].pc <= dispatch_reg[dispatch_idx].pc;
                            end
                        end
                        2'b01: begin // Load/Store
                            if (!LS_buffer[dispatch_reg[dispatch_idx].rs_index].busy) begin
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].busy <= 1'b1;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].ROB_index <= dispatch_reg[dispatch_idx].rob_index;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].op <= {dispatch_reg[dispatch_idx].func7, dispatch_reg[dispatch_idx].func3, dispatch_reg[dispatch_idx].opcode};
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].src_tag <= dispatch_reg[dispatch_idx].src1_tag;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].src_value <= dispatch_reg[dispatch_idx].src1_value;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].A <= dispatch_reg[dispatch_idx].offset;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].src_valid <= dispatch_reg[dispatch_idx].src1_ready;
                                LS_buffer[dispatch_reg[dispatch_idx].rs_index].pc <= dispatch_reg[dispatch_idx].pc;
                                // Set size field based on func3 (RISC-V: 000=byte, 001=half, 010=word)
                                case (dispatch_reg[dispatch_idx].func3)
                                    3'b000: LS_buffer[dispatch_reg[dispatch_idx].rs_index].size <= 2'b00; // byte
                                    3'b001: LS_buffer[dispatch_reg[dispatch_idx].rs_index].size <= 2'b01; // half
                                    3'b010: LS_buffer[dispatch_reg[dispatch_idx].rs_index].size <= 2'b10; // word
                                    default: LS_buffer[dispatch_reg[dispatch_idx].rs_index].size <= 2'b10; // default to word
                                endcase
                                if (is_store(dispatch_reg[dispatch_idx].operation)) begin
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data_tag <= dispatch_reg[dispatch_idx].src2_tag;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data_valid <= dispatch_reg[dispatch_idx].src2_ready;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data <= dispatch_reg[dispatch_idx].src2_value;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].ready <= dispatch_reg[dispatch_idx].src1_ready && dispatch_reg[dispatch_idx].src2_ready;
                                end else begin
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data_tag <= '0;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data <= '0;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data_valid <= 1'b0;
                                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].ready <= dispatch_reg[dispatch_idx].src1_ready;
                                end
                            end
                        end
                        2'b10: begin // Mul/Div
                            if (!MD_RS[dispatch_reg[dispatch_idx].rs_index].busy) begin
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].busy <= 1'b1;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].ROB_index <= dispatch_reg[dispatch_idx].rob_index;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].operation <= dispatch_reg[dispatch_idx].operation;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src1_tag <= dispatch_reg[dispatch_idx].src1_tag;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src1_value <= dispatch_reg[dispatch_idx].src1_value;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src1_valid <= dispatch_reg[dispatch_idx].src1_ready;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src2_tag <= dispatch_reg[dispatch_idx].src2_tag;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src2_value <= dispatch_reg[dispatch_idx].src2_value;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].src2_valid <= dispatch_reg[dispatch_idx].src2_ready;
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].ready <= (dispatch_reg[dispatch_idx].src1_ready && dispatch_reg[dispatch_idx].src2_ready);
                                MD_RS[dispatch_reg[dispatch_idx].rs_index].pc <= dispatch_reg[dispatch_idx].pc;
                            end
                        end
                    endcase
                end

        // ============================================================================
        //   Retirement during allocation
        // ============================================================================

        // For ALU RS allocation (src1)
        if (!dispatch_reg[dispatch_idx].src1_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src1_tag) begin
                    AS_RS[dispatch_reg[dispatch_idx].rs_index].src1_value <= retire_info_reg_1[retire_idx].result;
                    AS_RS[dispatch_reg[dispatch_idx].rs_index].src1_valid <= 1'b1;
                end
            end
        end

        // For ALU RS allocation (src2)
        if (!dispatch_reg[dispatch_idx].src2_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src2_tag) begin
                    AS_RS[dispatch_reg[dispatch_idx].rs_index].src2_value <= retire_info_reg_1[retire_idx].result;
                    AS_RS[dispatch_reg[dispatch_idx].rs_index].src2_valid <= 1'b1;
                end
            end
        end

        // For MulDiv RS allocation (src1)
        if (!dispatch_reg[dispatch_idx].src1_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src1_tag) begin
                    MD_RS[dispatch_reg[dispatch_idx].rs_index].src1_value <= retire_info_reg_1[retire_idx].result;
                    MD_RS[dispatch_reg[dispatch_idx].rs_index].src1_valid <= 1'b1;
                end
            end
        end

        // For MulDiv RS allocation (src2)
        if (!dispatch_reg[dispatch_idx].src2_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src2_tag) begin
                    MD_RS[dispatch_reg[dispatch_idx].rs_index].src2_value <= retire_info_reg_1[retire_idx].result;
                    MD_RS[dispatch_reg[dispatch_idx].rs_index].src2_valid <= 1'b1;
                end
            end
        end

        // For LSB allocation (src)
        if (!dispatch_reg[dispatch_idx].src1_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src1_tag) begin
                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].src_value <= retire_info_reg_1[retire_idx].result;
                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].src_valid <= 1'b1;
                end
            end
        end

        // For LSB allocation (store_data)
        if (is_store(dispatch_reg[dispatch_idx].operation) && !dispatch_reg[dispatch_idx].src2_ready) begin
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == dispatch_reg[dispatch_idx].src2_tag) begin
                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data <= retire_info_reg_1[retire_idx].result;
                    LS_buffer[dispatch_reg[dispatch_idx].rs_index].store_data_valid <= 1'b1;
                end
            end
        end
    end
end


        
            // ============================================================================
            // CDB BROADCAST - WAKEUP LOGIC FOR WAITING INSTRUCTIONS
            // ============================================================================
            // Robust wakeup logic for reservation stations and load/store buffer entries:
            //
            // For each busy entry in the ALU, MulDiv, and Load/Store reservation stations:
            //   - For each source operand (src1, src2, src_valid, store_data):
            //     1. If the source is not valid, check the following in order:
            //        a) Current cycle CDB (Common Data Bus) results: If a matching ROB tag is broadcast and valid, capture the value and mark the source as valid.
            //        b) 1-cycle delayed CDB results: If a matching ROB tag is broadcast and valid from the previous cycle, capture the value and mark the source as valid.
            //        c) Retiring instructions (retire_info_current): If a matching ROB tag is being retired in this cycle, capture the value and mark the source as valid.
            //     2. Short-circuit after the first match to avoid redundant updates.
            //   - This ensures zero-cycle wakeup for all sources, including those being written back or retired in the same cycle, and prevents missed wakeups or unnecessary stalls.
            //   - The 'ready' bit for each entry is updated separately when all required sources are valid.
            //
            // Robust wakeup for ALU reservation stations
            for (as_idx = 0; as_idx < no_RS_addsublog; as_idx++) begin

                ////////// Wake-up logic for src1 //////////

                if (AS_RS[as_idx].busy && !AS_RS[as_idx].src1_valid && AS_RS[as_idx].src1_tag != 0) begin
                    found_src1 = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src1; cdb_idx++) begin
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == AS_RS[as_idx].src1_tag) begin
                            AS_RS[as_idx].src1_value <= cdb_results_in[cdb_idx].result;
                            AS_RS[as_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src1; cdb_idx++) begin
                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == AS_RS[as_idx].src1_tag) begin
                            AS_RS[as_idx].src1_value <= cdb_results_reg_1[cdb_idx].result;
                            AS_RS[as_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src1; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == AS_RS[as_idx].src1_tag) begin
                            AS_RS[as_idx].src1_value <= retire_info_current[retire_idx].result;
                            AS_RS[as_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src1; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == AS_RS[as_idx].src1_tag) begin
                            AS_RS[as_idx].src1_value <= retire_info_reg_1[retire_idx].result;
                            AS_RS[as_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                end

                ////////// Wake-up logic for src2 //////////

                if (AS_RS[as_idx].busy && !AS_RS[as_idx].src2_valid && AS_RS[as_idx].src2_tag != 0) begin
                    found_src2 = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src2; cdb_idx++) begin
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == AS_RS[as_idx].src2_tag) begin
                            AS_RS[as_idx].src2_value <= cdb_results_in[cdb_idx].result;
                            AS_RS[as_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src2; cdb_idx++) begin
                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == AS_RS[as_idx].src2_tag) begin
                            AS_RS[as_idx].src2_value <= cdb_results_reg_1[cdb_idx].result;
                            AS_RS[as_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src2; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == AS_RS[as_idx].src2_tag) begin
                            AS_RS[as_idx].src2_value <= retire_info_current[retire_idx].result;
                            AS_RS[as_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src2; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == AS_RS[as_idx].src2_tag) begin
                            AS_RS[as_idx].src2_value <= retire_info_reg_1[retire_idx].result;
                            AS_RS[as_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                end
            end


            // Robust wakeup for MulDiv reservation stations
            for (md_idx = 0; md_idx < no_RS_muldiv; md_idx++) begin
                
                ////////// Wake-up logic for src1 //////////

                if (MD_RS[md_idx].busy && !MD_RS[md_idx].src1_valid && MD_RS[md_idx].src1_tag != 0) begin
                    found_src1 = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src1; cdb_idx++) begin
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == MD_RS[md_idx].src1_tag) begin
                            MD_RS[md_idx].src1_value <= cdb_results_in[cdb_idx].result;
                            MD_RS[md_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src1; cdb_idx++) begin
                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == MD_RS[md_idx].src1_tag) begin
                            MD_RS[md_idx].src1_value <= cdb_results_reg_1[cdb_idx].result;
                            MD_RS[md_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src1; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == MD_RS[md_idx].src1_tag) begin
                            MD_RS[md_idx].src1_value <= retire_info_current[retire_idx].result;
                            MD_RS[md_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src1; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == MD_RS[md_idx].src1_tag) begin
                            MD_RS[md_idx].src1_value <= retire_info_reg_1[retire_idx].result;
                            MD_RS[md_idx].src1_valid <= 1'b1;
                            found_src1 = 1;
                        end
                    end
                end
                
                ////////// Wake-up logic for src2 //////////

                if (MD_RS[md_idx].busy && !MD_RS[md_idx].src2_valid && MD_RS[md_idx].src2_tag != 0) begin
                    found_src2 = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src2; cdb_idx++) begin
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == MD_RS[md_idx].src2_tag) begin
                            MD_RS[md_idx].src2_value <= cdb_results_in[cdb_idx].result;
                            MD_RS[md_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src2; cdb_idx++) begin
                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == MD_RS[md_idx].src2_tag) begin
                            MD_RS[md_idx].src2_value <= cdb_results_reg_1[cdb_idx].result;
                            MD_RS[md_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src2; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == MD_RS[md_idx].src2_tag) begin
                            MD_RS[md_idx].src2_value <= retire_info_current[retire_idx].result;
                            MD_RS[md_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src2; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == MD_RS[md_idx].src2_tag) begin
                            MD_RS[md_idx].src2_value <= retire_info_reg_1[retire_idx].result;
                            MD_RS[md_idx].src2_valid <= 1'b1;
                            found_src2 = 1;
                        end
                    end
                end
            end


            // Robust wakeup for Load/Store Buffer
            for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
        
                ////////// Wake-up logic for src_valid (address source) //////////

                if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid) begin
                    found_src = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src; cdb_idx++) begin
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == LS_buffer[lsb_idx].src_tag) begin
                            LS_buffer[lsb_idx].src_value <= cdb_results_in[cdb_idx].result;
                            LS_buffer[lsb_idx].src_valid <= 1'b1;
                            found_src = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_src; cdb_idx++) begin
                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == LS_buffer[lsb_idx].src_tag) begin
                            LS_buffer[lsb_idx].src_value <= cdb_results_reg_1[cdb_idx].result;
                            LS_buffer[lsb_idx].src_valid <= 1'b1;
                            found_src = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == LS_buffer[lsb_idx].src_tag) begin
                            LS_buffer[lsb_idx].src_value <= retire_info_current[retire_idx].result;
                            LS_buffer[lsb_idx].src_valid <= 1'b1;
                            found_src = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_src; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == LS_buffer[lsb_idx].src_tag) begin
                            LS_buffer[lsb_idx].src_value <= retire_info_reg_1[retire_idx].result;
                            LS_buffer[lsb_idx].src_valid <= 1'b1;
                            found_src = 1;
                        end
                    end
                end

                ////////// Wake-up logic for store_data_valid (store data) //////////

                if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid) begin
                    found_store = 0;
                    // 1. Current CDB
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_store; cdb_idx++) begin
    
                        if (cdb_results_in[cdb_idx].result_ready && cdb_results_in[cdb_idx].ROB_index == LS_buffer[lsb_idx].store_data_tag) begin

                            LS_buffer[lsb_idx].store_data <= cdb_results_in[cdb_idx].result;
                            LS_buffer[lsb_idx].store_data_valid <= 1'b1;
                            found_store = 1;
                        end
                    end
                    // 2. Late CDB (1 cycle ago)
                    for (int cdb_idx = 0; cdb_idx < CDB_WIDTH && !found_store; cdb_idx++) begin

                        if (cdb_results_reg_1[cdb_idx].result_ready && cdb_results_reg_1[cdb_idx].ROB_index == LS_buffer[lsb_idx].store_data_tag) begin

                            LS_buffer[lsb_idx].store_data <= cdb_results_reg_1[cdb_idx].result;
                            LS_buffer[lsb_idx].store_data_valid <= 1'b1;
                            found_store = 1;
                        end
                    end
                    // 3. Retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_store; retire_idx++) begin
                        if (retire_info_current[retire_idx].result_ready && retire_info_current[retire_idx].ROB_index == LS_buffer[lsb_idx].store_data_tag) begin
                           
                            LS_buffer[lsb_idx].store_data <= retire_info_current[retire_idx].result;
                            LS_buffer[lsb_idx].store_data_valid <= 1'b1;
                            found_store = 1;
                        end
                    end
                    // 4. 1-cycle delayed retire info
                    for (int retire_idx = 0; retire_idx < ISSUE_WIDTH && !found_store; retire_idx++) begin
                        if (retire_info_reg_1[retire_idx].result_ready && retire_info_reg_1[retire_idx].ROB_index == LS_buffer[lsb_idx].store_data_tag) begin
                            LS_buffer[lsb_idx].store_data <= retire_info_reg_1[retire_idx].result;
                            LS_buffer[lsb_idx].store_data_valid <= 1'b1;
                            found_store = 1;
                        end
                    end
                end
            end

            // Update ready signals
            for (ls_idx = 0; ls_idx < no_LoadStoreBuffer; ls_idx++) begin
                if (LS_buffer[ls_idx].busy && !LS_buffer[ls_idx].ready) begin
                     if (is_store(LS_buffer[ls_idx].op)) begin
                        LS_buffer[ls_idx].ready <= LS_buffer[ls_idx].src_valid && LS_buffer[ls_idx].store_data_valid;
                    end else begin
                        LS_buffer[ls_idx].ready <= LS_buffer[ls_idx].src_valid;
                    end
                end
            end
                    
            for (as_idx = 0; as_idx < no_RS_addsublog; as_idx++) begin
                if (AS_RS[as_idx].busy && !AS_RS[as_idx].ready) begin
                    AS_RS[as_idx].ready <= AS_RS[as_idx].src1_valid && AS_RS[as_idx].src2_valid;
                end
            end
                    
            for (md_idx = 0; md_idx < no_RS_muldiv; md_idx++) begin
                if (MD_RS[md_idx].busy && !MD_RS[md_idx].ready) begin
                    MD_RS[md_idx].ready <= MD_RS[md_idx].src1_valid && MD_RS[md_idx].src2_valid;
                end
            end
        end
    end
        
    
    // ============================================================================
    // RESERVATION STATION OUTPUT ASSIGNMENTS
    // ============================================================================
    // Directly assign the full reservation station arrays to the outputs
    // for functional units to read
    
    always_comb begin
        for (int ii = 0; ii < no_RS_addsublog; ii++) begin
            alu_rs_out[ii] = AS_RS[ii];
        end
        for (int jj = 0; jj < no_RS_muldiv; jj++) begin
            muldiv_rs_out[jj] = MD_RS[jj];
        end
        for (int kk = 0; kk < no_LoadStoreBuffer; kk++) begin
            mem_rs_out[kk] = LS_buffer[kk];
        end
    end
            
    // ============================================================================
    // WRITEBACK AND COMMIT LOGIC
    // ============================================================================
    // This stage processes results from functional units, updates the ROB,
    // and commits completed instructions in program order
    
    // Add next-state variable for ROB
    rob_entry_t ROB_next [no_ROB-1:0];
    int rob_idx, committed_this_cycle;
    // Generate retirement_clears and dispatch_updates signals
    logic retirement_clears [0:no_RAT-1];
    logic dispatch_updates [0:no_RAT-1];
    integer retiring_rob_idx;
    
    // In the always_comb block before the always_ff for ROB
    always_comb begin

        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            commit_valid_next[i] = 1'b0;
            commit_rob_id_next[i] = '0;
            retire_info_current[i] = '0;
        end
        // --- Now copy current state as needed ---
        for (int i = 0; i < no_ROB; i++) begin
            ROB_next[i] = ROB[i];
        end
        for (int i = 0; i < no_ARF; i++) begin
            ARF_next[i] = ARF[i];
        end
        // Writeback: update ROB_next with CDB results
        for (int cdb_idx = 0; cdb_idx < CDB_WIDTH; cdb_idx++) begin
            if (cdb_results_in[cdb_idx].result_ready) begin
                ROB_next[cdb_results_in[cdb_idx].ROB_index].value = cdb_results_in[cdb_idx].result;
                ROB_next[cdb_results_in[cdb_idx].ROB_index].ready = 1'b1;
            end
        end
        // Commit: retire up to ISSUE_WIDTH instructions
        next_rob_tail = ROB_tail;
        committed_this_cycle = 0;
        disable_commit_loop = 0;
        for (int commit_idx = 0; commit_idx < ISSUE_WIDTH; commit_idx++) begin
            rob_idx = (next_rob_tail + commit_idx) % no_ROB;
            if (!disable_commit_loop && ROB_next[rob_idx].valid && ROB_next[rob_idx].ready) begin
                // Update ARF if not x0
                if (ROB_next[rob_idx].dest != 0) begin

                    ARF_next[ROB_next[rob_idx].dest] = ROB_next[rob_idx].value;
                end
                commit_valid_next[commit_idx] = 1'b1;
                commit_rob_id_next[commit_idx] = rob_idx;
                retire_info_current[commit_idx].ROB_index = rob_idx;
                retire_info_current[commit_idx].result = ROB_next[rob_idx].value;
                retire_info_current[commit_idx].result_ready = 1'b1;
                ROB_next[rob_idx].valid = 1'b0;
                ROB_next[rob_idx].ready = 1'b0;
                ROB_next[rob_idx].dest = '0;
                ROB_next[rob_idx].value = '0;
                ROB_next[rob_idx].instr_ID = '0;
                ROB_next[rob_idx].pc = '0;
                ROB_next[rob_idx].opcode = '0;
                committed_this_cycle++;
            end else begin
                disable_commit_loop = 1;
                
            end
        end
        next_rob_tail = (ROB_tail + committed_this_cycle) % no_ROB;
    end

    integer rob_idx_ff;
    
    // In the always_ff block for ROB and commit bookkeeping
    always_ff @(posedge clk) begin
        if (rst) begin
            for(rob_idx_ff = 0; rob_idx_ff < no_ROB; rob_idx_ff++) begin
                ROB[rob_idx_ff] <= '0;
            end
            ROB_tail <= '0;
            cycle_count <= 0;
            total_instructions_committed <= 0;
            all_instructions_completed <= 0;
            // Initialize IPC tracking variables
            execution_start_cycle <= 0;
            execution_cycles <= 0;
            first_commit_detected <= 0;
            instructions_executed <= 0;
            // Initialize ARF
            ARF[0] <= 0;   // x0 (zero) - always 0
            for(arf_idx = 1; arf_idx < no_ARF; arf_idx++) begin
                ARF[arf_idx] <= '0;
            end

        end else if(flush) begin

            for(rob_idx_ff = 0; rob_idx_ff < no_ROB; rob_idx_ff++) begin
                ROB[rob_idx_ff].valid <= 1'b0;
                ROB[rob_idx_ff].ready <= 1'b0;
                ROB[rob_idx_ff].dest <= '0;
                ROB[rob_idx_ff].value <= '0;
                ROB[rob_idx_ff].pc <= '0;
                ROB[rob_idx_ff].opcode <= '0; 
            end

            ROB_tail <= '0;
            
        end else begin
            
            ROB <= ROB_next;
            ARF <= ARF_next;
            ROB_tail <= next_rob_tail;

            // Update ROB with ready signals from store_commit_valid
            for (store_commit_idx = 0; store_commit_idx < ISSUE_WIDTH; store_commit_idx++) begin
                if (store_commit_valid[store_commit_idx]) begin
                    ROB[store_commit_rob_index[store_commit_idx]].ready <= 1'b1;
                end
            end

            // Commit ARF, and other next-state variables as needed
            
            commit_valid <= commit_valid_next;
            commit_rob_id <= commit_rob_id_next;
            retire_info_reg <= retire_info_current;
            // Update 1-cycle and 2-cycle delayed retirement info
            retire_info_reg_1 <= retire_info_reg;
            retire_info_reg_2 <= retire_info_reg_1;
            cycle_count <= cycle_count + 1;
            total_instructions_committed <= total_instructions_committed + (next_rob_tail - ROB_tail + no_ROB) % no_ROB;

            // Track first commit for accurate IPC calculation
            if (!first_commit_detected && (next_rob_tail - ROB_tail + no_ROB) % no_ROB > 0) begin
                first_commit_detected <= 1'b1;
                execution_start_cycle <= cycle_count;
                instructions_executed <= (next_rob_tail - ROB_tail + no_ROB) % no_ROB;
            end else if (first_commit_detected) begin
                instructions_executed <= instructions_executed + (next_rob_tail - ROB_tail + no_ROB) % no_ROB;
            end

            if (total_instructions_committed + (next_rob_tail - ROB_tail + no_ROB) % no_ROB >= no_instr) begin
                all_instructions_completed <= 1'b1;
                done <= 1'b1;
                execution_cycles <= cycle_count - execution_start_cycle + 1;
            end
        end
    end
    
    // ============================================================================
    // RAT UPDATE AND PRIORITY MERGE
    // ============================================================================
    // This stage combines intent from dispatch and retirement using dispatch/retirement flags
    // and updates the actual RAT with priority: flush > dispatch > retirement > hold
    
    // Combinational logic for RAT update enables
    always_comb begin
        // Default: no clears or updates
        for (int i = 0; i < no_RAT; i++) begin
            retirement_clears[i] = 1'b0;
            dispatch_updates[i] = 1'b0;
        end
        // Retirement clears
        // Check if the destination register is being retired and the RAT is valid and the tag matches
        for (int i = 0; i < no_RAT; i++) begin
            for (int commit_idx = 0; commit_idx < ISSUE_WIDTH; commit_idx++) begin
                retiring_rob_idx = (next_rob_tail + commit_idx) % no_ROB;
            end
        end
        // Dispatch updates
                  // Check if the destination register is being dispatched with a non-zero destination register
          for (int dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
              if (dispatch_reg[dispatch_idx].valid && dispatch_reg[dispatch_idx].dst != 0) begin
                  dispatch_updates[dispatch_reg[dispatch_idx].dst] = 1'b1;
              end
          end
    end   

    // Register update for RAT with priority: flush > dispatch > retirement > hold
    always_ff @(posedge clk) begin
        if(rst || flush) begin
            // Initialize RAT
            for(rat_idx = 0; rat_idx < no_RAT; rat_idx++) begin
                RAT_tag[rat_idx] <= 0;
                RAT_valid[rat_idx] <= 1'b0;
            end
        end else begin
            RAT_tag <= temp_RAT_tag;
            RAT_valid <= temp_RAT_valid;
        end
    end

    // ============================================================================
    // OUTPUT ASSIGNMENTS
    // ============================================================================
    
    // Control signal assignments
    assign pipeline_flush = flush;
    
    // Memory Management Unit interface assignments (vectorized for superscalar commit)
    always_comb begin
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            mmu_rob_head[i] = (ROB_tail + i) % no_ROB;
        end
    end

    // Complete ROB state output
    always_comb begin
        for (int i = 0; i < no_ROB; i++) begin
            rob_out[i] = ROB[i];
        end
    end

    // Instrumentation
    `include "instrumentation.sv"

    // ARF Checker
    `include "checker.sv"

endmodule