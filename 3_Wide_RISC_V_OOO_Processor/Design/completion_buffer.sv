`timescale 1ns / 1ps

`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"

import tomasulo_pkg::*;

// =============================================================================
// Completion Buffer Module (Simplified)
// =============================================================================
// 
// Purpose: Stores results from functional units and dispatches to 3-wide CDB
//          with all-or-nothing handshaking protocol.
//
// Features:
// - Circular buffer with FIFO ordering
// - Multi-input support (up to 10 functional units)
// - Direct bypass for zero-latency operation
// - All-or-nothing CDB handshaking
// - Simple and robust design based on instruction queue pattern
//
// Architecture:
// - Head pointer: Next entry to read (dispatch)
// - Tail pointer: Next entry to write (store)
// - Circular buffer with modulo wraparound
// - Bypass mode when buffer empty and CDB ready
// =============================================================================

module completion_buffer #(
    parameter int BUFFER_DEPTH = 48,  // Number of entries in completion buffer
    parameter int CDB_WIDTH = 3,      // Number of results dispatched per cycle
    parameter int NUM_INPUTS = 10,    // Number of functional unit inputs
    parameter int SAFETY_MARGIN = 5   // Extra entries for almost_full safety margin
) (
    // Clock and control
    input  logic clk,
    input  logic rst,
    input  logic flush,
    
    // Input from functional units (parameterized)
    input  ALU_Result_t [NUM_INPUTS-1:0] func_results,
    
    // Output to CDB (3 results per cycle)
    output ALU_Result_t [CDB_WIDTH-1:0] cdb_results,
    output logic [CDB_WIDTH-1:0] cdb_valid,
    input  logic [CDB_WIDTH-1:0] cdb_rdy,  // CDB ready bits for each result
    
    // Status signals
    output logic buffer_full,
    output logic buffer_almost_full,  // Early warning for allocation stall
    output logic buffer_empty,
    output logic [$clog2(BUFFER_DEPTH+1)-1:0] buffer_count,
    output logic [$clog2(BUFFER_DEPTH)-1:0] debug_head_ptr,
    output logic [$clog2(BUFFER_DEPTH)-1:0] debug_tail_ptr,
    output logic [$clog2(BUFFER_DEPTH)-1:0] debug_next_head_ptr
);

    // =============================================================================
    // Internal Storage and Variables
    // =============================================================================
    
    // Circular buffer storage
    ALU_Result_t buffer [0:BUFFER_DEPTH-1];
    logic [BUFFER_DEPTH-1:0] buffer_valid;
    
    // FIFO pointers and count
    logic [$clog2(BUFFER_DEPTH)-1:0] head_ptr;  // Next entry to read (dispatch)
    logic [$clog2(BUFFER_DEPTH)-1:0] tail_ptr;  // Next entry to write (store)
    logic [$clog2(BUFFER_DEPTH):0] count;       // Number of valid entries
    
    // Bypass logic variables
    logic bypass_mode;
    logic [$clog2(NUM_INPUTS+1)-1:0] bypass_count;
    int bypass_index [0 : (CDB_WIDTH-1)];
    logic bypass_index_valid [0 : (CDB_WIDTH-1)];
    int out_idx;
    
    // Loop variables - unique for each block
    integer ii, jj, seq_loop_i, comb_loop_i, comb_loop_j;
    
    // Debug signal assignments
    assign debug_head_ptr = head_ptr;
    assign debug_tail_ptr = tail_ptr;
    assign debug_next_head_ptr = head_ptr;  // Simple assignment for now
    assign buffer_count = count;
    
    // =============================================================================
    // Sequential Logic - Process Dequeues First, Then Enqueues
    // =============================================================================
    
    always_ff @(posedge clk or posedge rst or posedge flush) begin
        if (rst || flush) begin
            // Reset: Clear all buffer entries and reset pointers
            for (seq_loop_i = 0; seq_loop_i < BUFFER_DEPTH; seq_loop_i++) begin
                buffer_valid[seq_loop_i] <= 1'b0;
            end
            head_ptr <= 0;
            tail_ptr <= 0;
            count <= 0;
        end else begin
            // Compute next state
            logic [$clog2(BUFFER_DEPTH)-1:0] next_head, next_tail;
            logic [$clog2(BUFFER_DEPTH):0] next_count;
            next_head = head_ptr;
            next_tail = tail_ptr;
            next_count = count;
            
            // 1. Process all dequeues first (CDB dispatch)
            // Dispatch results whenever there are valid results in the buffer
            if (!bypass_mode && next_count > 0) begin
                for (seq_loop_i = 0; seq_loop_i < CDB_WIDTH; seq_loop_i++) begin
                    if (next_count > 0) begin
                        // Clear the entry being dispatched
                        buffer_valid[next_head] <= 1'b0;
                        next_head = (next_head + 1) % BUFFER_DEPTH;
                        next_count = next_count - 1;
                    end
                end
            end
            
            // 2. Then process all enqueues (store new results)
            // Only enqueue if NOT in bypass mode
            if (!bypass_mode) begin
                for (seq_loop_i = 0; seq_loop_i < NUM_INPUTS; seq_loop_i++) begin
                    if (func_results[seq_loop_i].result_ready && (next_count < BUFFER_DEPTH)) begin
                        buffer[next_tail] <= func_results[seq_loop_i];
                        buffer_valid[next_tail] <= 1'b1;
                        next_tail = (next_tail + 1) % BUFFER_DEPTH;
                        next_count = next_count + 1;
                    end
                end
            end
            
            // Update pointers and count
            head_ptr <= next_head;
            tail_ptr <= next_tail;
            count <= next_count;
        end
    end
    
    // =============================================================================
    // Combinational Logic for CDB Outputs
    // =============================================================================
    
    always_comb begin
        // Count ready inputs for bypass logic
        bypass_count = 0;
        for (comb_loop_i = 0; comb_loop_i < NUM_INPUTS; comb_loop_i++) begin
            if (func_results[comb_loop_i].result_ready) begin
                bypass_count = bypass_count + 1;
            end
        end

        // Enable bypass when buffer is empty, inputs <= CDB_WIDTH, and all CDB outputs are ready
        bypass_mode = (count == 0) && (bypass_count <= CDB_WIDTH) && (&cdb_rdy);

        // CDB Output Multiplexer: Buffer outputs vs Direct functional unit outputs
        if (bypass_mode) begin
            // Bypass mode: Direct wire routing from functional units (zero latency)
            out_idx = 0;
            for (ii = 0; ii < NUM_INPUTS; ii++) begin
                if (func_results[ii].result_ready && out_idx < CDB_WIDTH) begin
                    cdb_valid[out_idx] = 1'b1;
                    cdb_results[out_idx] = func_results[ii];
                    out_idx++;
                end
            end
            // Fill remaining CDB outputs with invalid/default
            for (; out_idx < CDB_WIDTH; out_idx++) begin
                cdb_valid[out_idx] = 1'b0;
                cdb_results[out_idx] = '{default: '0};
            end
        end else begin
            // Normal mode: Buffer outputs in FIFO order
            for (comb_loop_j = 0; comb_loop_j < CDB_WIDTH; comb_loop_j++) begin
                if (count > comb_loop_j) begin
                    cdb_valid[comb_loop_j] = 1'b1;
                    cdb_results[comb_loop_j] = buffer[(head_ptr + comb_loop_j) % BUFFER_DEPTH];
                end else begin
                    cdb_valid[comb_loop_j] = 1'b0;
                    cdb_results[comb_loop_j] = '{default: '0};
                end
            end
        end
    end
    
    // =============================================================================
    // Combinational Logic for Status Signals
    // =============================================================================
    
    always_comb begin
        // Status signals
        buffer_full = (count == BUFFER_DEPTH);
        buffer_almost_full = (count >= (BUFFER_DEPTH - NUM_INPUTS - SAFETY_MARGIN));
        buffer_empty = (count == 0);
    end
    
endmodule 