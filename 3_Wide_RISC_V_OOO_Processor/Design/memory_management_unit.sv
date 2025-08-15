// ============================================================================
// RISC-V Tomasulo Memory Management Unit (MMU)
// ----------------------------------------------------------------------------
// Handles memory disambiguation, store-to-load forwarding, coalescing, and
// Integrates with data_mem.sv and communicates with the ROB/CDB.
// ============================================================================

`timescale 1ns / 1ps

import tomasulo_pkg::*;

module memory_management_unit #(
    parameter ISSUE_WIDTH       = 3,
    parameter LSB_SIZE          = 16,
    parameter ROB_SIZE          = 48,
    parameter MEM_DEPTH         = 1024,
    parameter DATA_WIDTH        = 32,
    parameter NUM_LD_PORTS      = 2,
    parameter NUM_ST_PORTS      = 1,
    parameter ENABLE_FORWARDING = 1,
    parameter ENABLE_COALESCING = 1,
    parameter LQ_DEPTH = 8,
    parameter SQ_DEPTH = 8
) (
    // ------------------------------------------------------------------------
    // Clock and reset
    // ------------------------------------------------------------------------
    input  logic clk,
    input  logic rst,
    input  logic stall,

    // ------------------------------------------------------------------------
    // LSB interface: in-flight memory ops from dispatch
    // ------------------------------------------------------------------------
    input  LS_Buffer_Entry_t [LSB_SIZE-1:0] lsb_entries,

    // ------------------------------------------------------------------------
    // ROB interface: for in-order commit and status (simplified)
    // ------------------------------------------------------------------------
    input  logic [$clog2(ROB_SIZE)-1:0] rob_head [ISSUE_WIDTH-1:0],

    // ------------------------------------------------------------------------
    // Memory interface: to/from data memory
    // ------------------------------------------------------------------------
    output logic [NUM_ST_PORTS-1:0] mem_write_en,
    output logic [31:0] mem_write_addr [NUM_ST_PORTS-1:0],
    output logic [DATA_WIDTH-1:0] mem_write_data [NUM_ST_PORTS-1:0],
    output logic [1:0] mem_write_size [NUM_ST_PORTS-1:0],
    output logic [NUM_LD_PORTS-1:0] mem_read_en,
    output logic [31:0] mem_read_addr [NUM_LD_PORTS-1:0],
    output logic [1:0] mem_read_size [NUM_LD_PORTS-1:0],
    output logic [NUM_LD_PORTS-1:0] mem_read_unsigned,
    input  logic [DATA_WIDTH-1:0] mem_read_data [NUM_LD_PORTS-1:0],

    // ------------------------------------------------------------------------
    // Result interface: load results to CDB/ROB
    // ------------------------------------------------------------------------
    output ALU_Result_t [NUM_LD_PORTS-1:0] load_cdb_result,

    // ------------------------------------------------------------------------
    // Control and status
    // ------------------------------------------------------------------------
    input  logic flush,
    output logic lq_full,
    output logic sq_full,
    output lq_entry_t lq_debug [LQ_DEPTH],
    output sq_entry_t sq_debug [SQ_DEPTH],
    output logic [LSB_SIZE-1:0] lsb_entry_consumed,
    output logic [ISSUE_WIDTH-1:0] store_commit_valid,
    output logic [$clog2(ROB_SIZE)-1:0] store_commit_rob_index [ISSUE_WIDTH-1:0]
);

    // ------------------------------------------------------------------------
    // Internal Queues: LQ and SQ
    // ------------------------------------------------------------------------
    lq_entry_t lq [LQ_DEPTH];
    sq_entry_t sq [SQ_DEPTH];
    // Temporary next-state variables
    lq_entry_t lq_next [LQ_DEPTH];
    sq_entry_t sq_next [SQ_DEPTH];
    logic [$clog2(LQ_DEPTH)-1:0] lq_head, lq_tail;
    logic [$clog2(SQ_DEPTH)-1:0] sq_head, sq_tail;

    // ------------------------------------------------------------------------
    // Temporary variables for queue management
    // ------------------------------------------------------------------------
    integer lq_head_next;
    integer sq_head_next;
    integer lq_free;
    integer sq_free;
    integer lq_enq_count;
    integer sq_enq_count;
    integer lq_idx; // Global declaration for use in loops
    integer sq_idx;
    integer lq_deq_idx; // For LQ dequeue loop
    // Dequeue counters
    integer lq_deq_count;
    integer sq_deq_count;
    // Dequeue enable flags
    logic lq_deq_enable;
    logic sq_deq_enable;
    // Issue logic flags
    logic safe;
    logic youngest;
    integer completed_younger;
    bit fwd_f; // forwarding flag for forwarding loop
    bit coal_f; // coalescing flag for coalescing loop
    bit forwarded; // forwarding flag for main issue loop
    bit safe_to_evict; // For SQ eviction logic
    // Track which LQ/SQ entry is issued on each port
    integer issued_lq_idx [NUM_LD_PORTS-1:0];
    integer issued_sq_idx [NUM_ST_PORTS-1:0];
    // Loop variables (split per block to avoid multiple drivers)
    integer ld_ff_i, st_ff_i, sq_ff_i, sq_ff_j, lq_ff_i, lq_ff_j, lq_deq_ff_i, sq_deq_ff_i, lq_dispatch_ff_i;
    integer ld_comb_i, st_comb_i, sq_comb_i, sq_comb_j, lq_comb_i, lq_comb_j, lq_deq_comb_i, sq_deq_comb_i, lq_dispatch_comb_i;
    integer l_comb, s_comb, port_comb;
    integer ld_issued, st_issued, issued_idx, issued_count;
    // Variables for always_ff block
    integer store_start_ff, store_end_ff, load_start_ff, load_end_ff;
    // Variables for always_comb block  
    integer store_start_comb, store_end_comb, load_start_comb, load_end_comb;
    integer youngest_store_idx, youngest_store_tag, commit_slot, issue_sq;
    logic mmu_debug;
    logic safe_to_issue_store;
    integer check_lq;
    // ------------------------------------------------------------------------
    // Reset and Dispatch Logic
    // ------------------------------------------------------------------------
    always_ff @(posedge clk or negedge rst) begin
        // Print all valid LSB entries at the start of the cycle
        for (int lsb_i = 0; lsb_i < LSB_SIZE; lsb_i++) begin
            if (lsb_entries[lsb_i].busy) begin
                string op_type;
                if (lsb_entries[lsb_i].op[6:0] == 7'b0000011) begin
                    op_type = "LOAD";
                end else if (lsb_entries[lsb_i].op[6:0] == 7'b0100011) begin
                    op_type = "STORE";
                end else begin
                    op_type = "OTHER";
                end
                // Print statement removed
            end
        end
        // Next-state copies
        lq_next = lq;
        sq_next = sq;
        if (rst || flush) begin
            lq_head <= 0;
            lq_tail <= 0;
            sq_head <= 0;
            sq_tail <= 0;
            for (lq_deq_ff_i = 0; lq_deq_ff_i < LQ_DEPTH; lq_deq_ff_i++) begin
                lq_next[lq_deq_ff_i].valid = 0;
                lq_next[lq_deq_ff_i].pc = 32'h0;
                lq_next[lq_deq_ff_i].completed = 1'b0;
                lq_next[lq_deq_ff_i].issued = 1'b0;
            end
            for (sq_deq_ff_i = 0; sq_deq_ff_i < SQ_DEPTH; sq_deq_ff_i++) begin
                sq_next[sq_deq_ff_i].valid = 0;
                sq_next[sq_deq_ff_i].pc = 32'h0;
                sq_next[sq_deq_ff_i].completed = 1'b0;
                sq_next[sq_deq_ff_i].issued = 1'b0;
            end
        end else begin
            // Dequeue up to NUM_LD_PORTS completed loads from LQ head (superscalar commit, synthesizable)
            lq_deq_count = 0;
            lq_deq_enable = 1;
            for (ld_ff_i = 0; ld_ff_i < NUM_LD_PORTS; ld_ff_i++) begin
                lq_deq_idx = (lq_head + lq_deq_count) % LQ_DEPTH;
                if (lq_deq_enable && lq[lq_deq_idx].valid && lq[lq_deq_idx].completed) begin
                    lq_next[lq_deq_idx].valid = 1'b0;
                    lq_next[lq_deq_idx].completed = 1'b1;
                    lq_next[lq_deq_idx].issued = 1'b0;
                    lq_deq_count++;
                end else begin
                    lq_deq_enable = 0;
                end
            end
            lq_head_next = (lq_head + lq_deq_count) % LQ_DEPTH;

            // Only dequeue the head entry in SQ, if it is valid, completed, and all older loads have been issued/completed
            safe_to_evict = 1;
            if (sq[sq_head].valid && sq[sq_head].completed) begin
                for (lq_ff_i = 0; lq_ff_i < LQ_DEPTH; lq_ff_i++) begin
                    if (lq[lq_ff_i].valid && !lq[lq_ff_i].issued && lq[lq_ff_i].rob_tag < sq[sq_head].rob_tag) begin
                        safe_to_evict = 0;
                    end
                end
                if (safe_to_evict) begin
                    sq_next[sq_head].valid = 1'b0;
                    sq_next[sq_head].completed = 1'b0;
                    sq_next[sq_head].issued = 1'b0;
                    sq_head_next = (sq_head + 1) % SQ_DEPTH;
                end else begin
                    sq_head_next = sq_head;
                end
            end else begin
                sq_head_next = sq_head;
            end

            lq_free = (lq_head_next + LQ_DEPTH - lq_tail - 1) % LQ_DEPTH;
            sq_free = (sq_head_next + SQ_DEPTH - sq_tail - 1) % SQ_DEPTH;
            lq_enq_count = 0;
            sq_enq_count = 0;
            commit_slot  = 0;
            issued_count = 0;
            for (ld_ff_i = 0; ld_ff_i < LSB_SIZE; ld_ff_i++) lsb_entry_consumed[ld_ff_i] = 1'b0;
            // Clear store commit handshake outputs at the start of each cycle
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                store_commit_valid[i] = 1'b0;
                store_commit_rob_index[i] = '0;
            end

            if (!stall) begin
            // Enqueue new loads and stores from LSB into LQ and SQ
            for (lq_dispatch_ff_i = 0; lq_dispatch_ff_i < LSB_SIZE; lq_dispatch_ff_i++) begin
                    // Enqueue new loads from LSB to SQ
                    if (lsb_entries[lq_dispatch_ff_i].busy && lsb_entries[lq_dispatch_ff_i].op[6:0] == 7'b0000011 && lsb_entries[lq_dispatch_ff_i].ready) begin
                        if (lq_enq_count < lq_free) begin
                            lq_idx = (lq_tail + lq_enq_count) % LQ_DEPTH;
                            lq_next[lq_idx].valid         = 1'b1;
                            lq_next[lq_idx].addr          = lsb_entries[lq_dispatch_ff_i].src_value + lsb_entries[lq_dispatch_ff_i].A;
                            lq_next[lq_idx].data          = 32'h0;
                            lq_next[lq_idx].size          = lsb_entries[lq_dispatch_ff_i].size;
                            lq_next[lq_idx].rob_tag       = lsb_entries[lq_dispatch_ff_i].ROB_index;
                            lq_next[lq_idx].issued        = 1'b0;
                            lq_next[lq_idx].completed     = 1'b0;
                            lq_next[lq_idx].unsigned_load = (lsb_entries[lq_dispatch_ff_i].op[2:0] == 3'b100 || lsb_entries[lq_dispatch_ff_i].op[2:0] == 3'b101);
                            lq_next[lq_idx].pc            = lsb_entries[lq_dispatch_ff_i].pc;
                            lq_enq_count++;
                            // Mark LSB Load Entry as consumed
                            lsb_entry_consumed[lq_dispatch_ff_i] = 1'b1;
                            if (mmu_debug) begin
                                $display("[%0t] [MMU] LOAD ENQUEUED: LSB[%0d] ROB_ID=%0d Op=0x%05x Addr=0x%08x Size=%0d", $time, lq_dispatch_ff_i, lsb_entries[lq_dispatch_ff_i].ROB_index, lsb_entries[lq_dispatch_ff_i].op, lq_next[lq_idx].addr, lq_next[lq_idx].size);
                            end
                        end
                    end else if (lsb_entries[lq_dispatch_ff_i].busy && lsb_entries[lq_dispatch_ff_i].op[6:0] == 7'b0100011 && lsb_entries[lq_dispatch_ff_i].src_valid && lsb_entries[lq_dispatch_ff_i].store_data_valid)begin
                        // Check if a store with this ROB tag is already in the SQ to prevent re-issuing
                        logic rob_tag_exists = 0;
                        for (int k = 0; k < SQ_DEPTH; k++) begin
                            if (sq_next[k].valid && sq_next[k].rob_tag == lsb_entries[lq_dispatch_ff_i].ROB_index) begin
                                rob_tag_exists = 1;
                            end
                        end
                        // Enqueue new stores from LSB to SQ upto ISSUE_WIDTH and mark them as retired
                        if(!rob_tag_exists && (sq_enq_count < sq_free) && (sq_enq_count < ISSUE_WIDTH)) begin
                            // Mark SQ entry as valid/dispatched
                            sq_idx = (sq_tail + sq_enq_count) % SQ_DEPTH;
                            sq_next[sq_idx].valid     = 1'b1;
                            sq_next[sq_idx].addr      = lsb_entries[lq_dispatch_ff_i].src_value + lsb_entries[lq_dispatch_ff_i].A;
                            sq_next[sq_idx].data      = lsb_entries[lq_dispatch_ff_i].store_data;
                            sq_next[sq_idx].size      = lsb_entries[lq_dispatch_ff_i].size;
                            sq_next[sq_idx].rob_tag   = lsb_entries[lq_dispatch_ff_i].ROB_index;
                            sq_next[sq_idx].issued    = 1'b0;
                            sq_next[sq_idx].completed = 1'b0;
                            sq_next[sq_idx].pc        = lsb_entries[lq_dispatch_ff_i].pc;
                            sq_enq_count++;
                            // Mark LSB Load Entry as consumed
                            lsb_entry_consumed[lq_dispatch_ff_i] = 1'b1;
                            // Commit stores entries for ROB.ready indication
                            store_commit_valid[commit_slot] = 1'b1;
                            store_commit_rob_index[commit_slot] = lsb_entries[lq_dispatch_ff_i].ROB_index;
                            commit_slot++;
                        end
                    end
                end
            end
            lq_tail <= (lq_tail + lq_enq_count) % LQ_DEPTH;
            sq_tail <= (sq_tail + sq_enq_count) % SQ_DEPTH;
            lq_head <= lq_head_next;
            sq_head <= sq_head_next;
        end
        for (ld_ff_i = 0; ld_ff_i < NUM_LD_PORTS; ld_ff_i++) begin
            // Mark processed loads as completed and issued
            if (mem_read_en[ld_ff_i]) begin
                for (lq_ff_i = 0; lq_ff_i < LQ_DEPTH; lq_ff_i++) begin
                    if (lq[lq_ff_i].valid && !lq[lq_ff_i].completed && lq[lq_ff_i].addr == mem_read_addr[ld_ff_i] && lq[lq_ff_i].size == mem_read_size[ld_ff_i]) begin
                        lq_next[lq_ff_i].completed = 1'b1;
                        lq_next[lq_ff_i].issued = 1'b1;
                    end
                end
            end
            // Marks a load as completed/issued in the LQ when its result is produced by forwarding
            if (!mem_read_en[ld_ff_i] && load_cdb_result[ld_ff_i].result_ready) begin
                for (lq_ff_i = 0; lq_ff_i < LQ_DEPTH; lq_ff_i++) begin
                    if (lq[lq_ff_i].valid && !lq[lq_ff_i].completed && lq[lq_ff_i].rob_tag == load_cdb_result[ld_ff_i].ROB_index) begin
                        lq_next[lq_ff_i].completed = 1'b1;
                        lq_next[lq_ff_i].issued = 1'b1;
                    end
                end
            end
        end
        // Coalescing squashes older stores to the same address with same data
        if (ENABLE_COALESCING) begin
            for (sq_ff_i = 0; sq_ff_i < SQ_DEPTH; sq_ff_i++) begin
                if (sq_next[sq_ff_i].valid && !sq_next[sq_ff_i].issued && !sq_next[sq_ff_i].completed) begin
                    for (sq_ff_j = 0; sq_ff_j < SQ_DEPTH; sq_ff_j++) begin
                        if (sq_ff_i != sq_ff_j && sq_next[sq_ff_j].valid && !sq_next[sq_ff_j].issued && !sq_next[sq_ff_j].completed && 
                            sq_next[sq_ff_j].addr == sq_next[sq_ff_i].addr && sq_next[sq_ff_j].size == sq_next[sq_ff_i].size && 
                            sq_next[sq_ff_j].data == sq_next[sq_ff_i].data && sq_next[sq_ff_j].rob_tag > sq_next[sq_ff_i].rob_tag) begin
                            sq_next[sq_ff_i].completed = 1'b1;
                        end
                    end
                end
            end
        end
        // Mark stores as issued if they are at the head of the ROB, for memory interface to drive the stores
        for(issued_idx = 0; issued_idx < ISSUE_WIDTH; issued_idx++)begin
            for (issue_sq = 0; issue_sq < SQ_DEPTH; issue_sq++) begin
                if (sq_next[issue_sq].valid && !sq_next[issue_sq].issued && !sq_next[issue_sq].completed && sq_next[issue_sq].rob_tag == rob_head[issued_idx] && issued_count <= ISSUE_WIDTH) begin
                    // Check that all older loads with address overlap have been completed before issuing this store
                    safe_to_issue_store = 1;
                                         for (check_lq = 0; check_lq < LQ_DEPTH; check_lq++) begin
                         if (lq[check_lq].valid && lq[check_lq].rob_tag < sq_next[issue_sq].rob_tag && !lq[check_lq].completed) begin
                             // Check for address overlap (exact match or partial overlap)
                             store_start_ff = sq_next[issue_sq].addr;
                             store_end_ff   = sq_next[issue_sq].addr + (sq_next[issue_sq].size == 2'b00 ? 0 : (sq_next[issue_sq].size == 2'b01 ? 1 : 3));
                             load_start_ff  = lq[check_lq].addr;
                             load_end_ff    = lq[check_lq].addr + (lq[check_lq].size == 2'b00 ? 0 : (lq[check_lq].size == 2'b01 ? 1 : 3));
                             
                             // Check if addresses overlap
                             if (!(store_end_ff < load_start_ff || load_end_ff < store_start_ff)) begin
                                 safe_to_issue_store = 0;
                             end
                         end
                    end
                    
                    if (safe_to_issue_store) begin
                        sq_next[issue_sq].issued = 1'b1;
                        issued_count++;
                    end
                end
            end
        end
        // Mark stores as completed after they are sent to memory
        for (st_ff_i = 0; st_ff_i < NUM_ST_PORTS; st_ff_i++) begin
            if (mem_write_en[st_ff_i] && issued_sq_idx[st_ff_i] != -1) begin
                sq_next[issued_sq_idx[st_ff_i]].completed = 1'b1;
            end
        end
        // Commit next-state
        lq <= lq_next;
        sq <= sq_next;
    end

    // ------------------------------------------------------------------------
    // Base Issue Logic: Issue loads and stores to memory 
    // ------------------------------------------------------------------------
    always_comb begin
        // Default assignments to outputs
        for (ld_comb_i = 0; ld_comb_i < NUM_LD_PORTS; ld_comb_i++) begin
            mem_read_en[ld_comb_i]        = 0;
            mem_read_addr[ld_comb_i]      = 0;
            mem_read_size[ld_comb_i]      = 0;
            mem_read_unsigned[ld_comb_i]  = 0;
            load_cdb_result[ld_comb_i]    = '{result: 0, result_ready: 0, ROB_index: 0};
            issued_lq_idx[ld_comb_i]      = -1;
        end
        for (st_comb_i = 0; st_comb_i < NUM_ST_PORTS; st_comb_i++) begin
            mem_write_en[st_comb_i]   = 0;
            mem_write_addr[st_comb_i] = 0;
            mem_write_data[st_comb_i] = 0;
            mem_write_size[st_comb_i] = 0;
            issued_sq_idx[st_comb_i]  = -1;
        end
        // Base issue logic
        ld_issued = 0;
        st_issued = 0;
        // Forwarding logic
        if (!stall) begin
        for (lq_comb_i = 0; lq_comb_i < LQ_DEPTH && ld_issued < NUM_LD_PORTS; lq_comb_i++) begin
            if (lq[lq_comb_i].valid && !lq[lq_comb_i].issued && !lq[lq_comb_i].completed) begin
                forwarded = 0;
                if (ENABLE_FORWARDING) begin
                    // Find the most recent store that matches this load (lowest ROB_ID)
                    youngest_store_idx = -1;
                    youngest_store_tag = -1;
                                         for (sq_comb_j = 0; sq_comb_j < SQ_DEPTH; sq_comb_j++) begin
                         if (sq[sq_comb_j].valid && sq[sq_comb_j].rob_tag < lq[lq_comb_i].rob_tag) begin
                             // Check for address overlap (exact match or partial overlap)
                             store_start_comb = sq[sq_comb_j].addr;
                             store_end_comb   = sq[sq_comb_j].addr + (sq[sq_comb_j].size == 2'b00 ? 0 : (sq[sq_comb_j].size == 2'b01 ? 1 : 3));
                             load_start_comb  = lq[lq_comb_i].addr;
                             load_end_comb    = lq[lq_comb_i].addr + (lq[lq_comb_i].size == 2'b00 ? 0 : (lq[lq_comb_i].size == 2'b01 ? 1 : 3));
                             
                             // Check if addresses overlap
                             if (!(store_end_comb < load_start_comb || load_end_comb < store_start_comb)) begin
                                 // Check if store is ready (has data)
                                 if (sq[sq_comb_j].completed) begin
                                     if (youngest_store_idx == -1 || sq[sq_comb_j].rob_tag < youngest_store_tag) begin
                                         youngest_store_tag = sq[sq_comb_j].rob_tag;
                                         youngest_store_idx = sq_comb_j;
                                         forwarded = 1;
                                     end
                                 end
                             end
                         end
                    end
                end
                if (forwarded) begin
                    // Forward the actual store data from the youngest matching store
                    load_cdb_result[ld_issued] = '{result: sq[youngest_store_idx].data, result_ready: 1, ROB_index: lq[lq_comb_i].rob_tag};
                    if (mmu_debug) begin
                        $display("[%0t] [MMU] LOAD FORWARDED: LQ[%0d] ROB_ID=%0d Addr=0x%08x Size=%0d Data=0x%08x from Store ROB_ID=%0d", $time, lq_comb_i, lq[lq_comb_i].rob_tag, lq[lq_comb_i].addr, lq[lq_comb_i].size, sq[youngest_store_idx].data, sq[youngest_store_idx].rob_tag);
                    end
                end else begin
                    mem_read_en[ld_issued]        = 1;
                    mem_read_addr[ld_issued]      = lq[lq_comb_i].addr;
                    mem_read_size[ld_issued]      = lq[lq_comb_i].size;
                    mem_read_unsigned[ld_issued]  = lq[lq_comb_i].unsigned_load;
                    load_cdb_result[ld_issued]    = '{result: 0, result_ready: 0, ROB_index: lq[lq_comb_i].rob_tag};
                    if (mmu_debug) begin
                        $display("[%0t] [MMU] LOAD ISSUED: LQ[%0d] ROB_ID=%0d Addr=0x%08x Size=%0d", $time, lq_comb_i, lq[lq_comb_i].rob_tag, lq[lq_comb_i].addr, lq[lq_comb_i].size);
                    end
                end
                issued_lq_idx[ld_issued]      = lq_comb_i;
                ld_issued++;
            end
        end
        // Memory store
        for (int i = 0; i < SQ_DEPTH; i++) begin
            if (st_issued < NUM_ST_PORTS) begin
                sq_comb_i = (sq_head + i) % SQ_DEPTH;
                if (sq[sq_comb_i].valid && sq[sq_comb_i].issued && !sq[sq_comb_i].completed) begin
                    mem_write_en[st_issued]   = 1;
                    mem_write_addr[st_issued] = sq[sq_comb_i].addr;
                    mem_write_data[st_issued] = sq[sq_comb_i].data;
                    mem_write_size[st_issued] = sq[sq_comb_i].size;
                    issued_sq_idx[st_issued]  = sq_comb_i;
                    if (mmu_debug) begin
                        $display("[%0t] [MMU] STORE ISSUED: SQ[%0d] ROB_ID=%0d Addr=0x%08x Data=0x%08x Size=%0d", $time, sq_comb_i, sq[sq_comb_i].rob_tag, sq[sq_comb_i].addr, sq[sq_comb_i].data, sq[sq_comb_i].size);
                    end
                    st_issued++;
                end
            end
        end
        end
        for (ld_comb_i = 0; ld_comb_i < NUM_LD_PORTS; ld_comb_i++) begin
            if (mem_read_en[ld_comb_i] && load_cdb_result[ld_comb_i].result_ready == 0) begin
                if (issued_lq_idx[ld_comb_i] != -1) begin
                    load_cdb_result[ld_comb_i] = '{result: mem_read_data[ld_comb_i], result_ready: 1, ROB_index: lq[issued_lq_idx[ld_comb_i]].rob_tag};
                end
            end
        end
    end

    // ------------------------------------------------------------------------
    // Status and Backpressure
    // ------------------------------------------------------------------------
    always_comb begin
        lq_full = lq[lq_tail].valid;
        sq_full = sq[sq_tail].valid;
    end

    assign lq_debug = lq;
    assign sq_debug = sq;

endmodule
 
