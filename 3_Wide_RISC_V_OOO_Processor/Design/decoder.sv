`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Create Date: 06/14/2025 02:20:00 PM
// Module Name: Tomasulo 3-Wide Parallel Decoder
// Description: This module implements a 3-wide parallel decoder for the Tomasulo
//              processor with support for flush, stall signals, and error detection
// Version : 1.00
//////////////////////////////////////////////////////////////////////////////////

// Include the decoder package
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"

import fetch_types_pkg::*;
import decoder_pkg::*;
import tomasulo_pkg::get_instr_type;
import tomasulo_pkg::OPERATION_WIDTH;
import tomasulo_pkg::is_store;
import tomasulo_pkg::is_branch;

// ============================================================================
// MODULE DEFINITION
// ============================================================================

module tomasulo_decoder #(
    parameter ISSUE_WIDTH = 3  // Number of instructions decoded per cycle
)(
    // ============================================================================
    // CLOCK AND CONTROL SIGNALS
    // ============================================================================
    input  logic clk,                    // Clock signal
    input  logic reset,                  // Synchronous reset
    input  logic flush,                  // Pipeline flush signal
    
    // ============================================================================
    // FLOW CONTROL SIGNALS
    // ============================================================================
    input  logic fetch_stall,            // (Not used) Stall signal from fetch stage
    input  logic [ISSUE_WIDTH-1:0] alloc_rdy,  // Allocation ready signals from allocation stage
    
    // ============================================================================
    // FETCH STAGE INPUTS
    // ============================================================================
    input  logic [ISSUE_WIDTH-1:0][31:0] fetch_instruction,  // Raw instructions from fetch
    input  logic [ISSUE_WIDTH-1:0][31:0] fetch_pc,           // Program counters
    input  logic [ISSUE_WIDTH-1:0]       fetch_valid,        // Valid instruction flags
    
    // ============================================================================
    // DECODE STAGE OUTPUTS
    // ============================================================================
    output decode_entry_t [ISSUE_WIDTH-1:0] decode_reg,      // Decoded instruction registers
    output logic [ISSUE_WIDTH-1:0] dec_rdy,                  // Decoder ready signals
    output logic [ISSUE_WIDTH-1:0] decoder_error             // Decoder error signals
);

    // ============================================================================
    // INTERNAL SIGNALS
    // ============================================================================
    
    // Decoder error detection signals
    logic [ISSUE_WIDTH-1:0] decode_error_detect;  // Current cycle error detection
    logic [ISSUE_WIDTH-1:0] decode_error_prev;    // Previous cycle error detection (for edge detection)
    logic [ISSUE_WIDTH-1:0] decode_error_pulse;   // One-cycle pulse for new errors

    // ============================================================================
    // COMBINATIONAL LOGIC
    // ============================================================================
    
    // ----------------------------------------------------------------------------
    // Decoder Error Detection Logic
    // ----------------------------------------------------------------------------
    always_comb begin
        for (int k = 0; k < ISSUE_WIDTH; k = k + 1) begin
            // Detect unsupported instructions in each lane
            decode_error_detect[k] = fetch_valid[k] && !is_valid_instruction(fetch_instruction[k]);
            
            // Generate one-cycle pulse for new errors (rising edge detection)
            // This ensures error is asserted for exactly 1 cycle
            decode_error_pulse[k] = decode_error_detect[k] && !decode_error_prev[k];
        end
    end

    // ----------------------------------------------------------------------------
    // Decoder Ready Signal Generation
    // ----------------------------------------------------------------------------
    always_comb begin
        for (int k = 0; k < ISSUE_WIDTH; k = k + 1) begin
            // Decoder is ready if backend can accept instructions
            // Use alloc_rdy from allocation stage for proper flow control
            dec_rdy[k] = alloc_rdy[k];
        end
    end

    // ============================================================================
    // SEQUENTIAL LOGIC
    // ============================================================================
    
    integer i;  // Loop variable for synthesis
    
    always_ff @(posedge clk or posedge reset or posedge flush) begin
        if (reset || flush) begin
            // ====================================================================
            // RESET/FLUSH LOGIC
            // ====================================================================
            for (i = 0; i < ISSUE_WIDTH; i = i + 1) begin
                // Clear all decode registers
                decode_reg[i] <= '0;
                
                // Clear error signals
                decoder_error[i] <= 1'b0;
                decode_error_prev[i] <= 1'b0;
            end
            
        end else begin
            // ====================================================================
            // NORMAL OPERATION
            // ====================================================================
            
            // --------------------------------------------------------------------
            // Instruction Decoding
            // --------------------------------------------------------------------
            for (i = 0; i < ISSUE_WIDTH; i = i + 1) begin
                logic [31:0] temp_imm; // Temporary variable for immediate decoding
                logic [OPERATION_WIDTH-1:0] op; // <-- Add this line
                logic [4:0] temp_dst;
                if (fetch_valid[i] && dec_rdy[i]) begin
                    // Decode instruction fields using package functions
                    decode_reg[i].instruction <= fetch_instruction[i];
                    decode_reg[i].pc          <= fetch_pc[i];
                    decode_reg[i].opcode      <= fetch_instruction[i][6:0];
                    decode_reg[i].func3       <= fetch_instruction[i][14:12];
                    decode_reg[i].func7       <= fetch_instruction[i][31:25];
                    decode_reg[i].src1        <= fetch_instruction[i][19:15];
                    decode_reg[i].src2        <= get_src2(fetch_instruction[i]);
                    op = create_operation(fetch_instruction[i]); 
                    if (is_store(op) || is_branch(op)) begin
                        temp_dst = 0;
                    end else begin
                        temp_dst = fetch_instruction[i][11:7];
                    end
                    decode_reg[i].dst <= temp_dst;
                    decode_reg[i].operation   <= op; 
                    decode_reg[i].instr_type  <= get_instr_type(op);
                    
                    // Correctly decode immediate based on opcode and assign to a temporary variable
                    case (fetch_instruction[i][6:0])
                        7'b0000011: // I-type (loads)
                            temp_imm = fetch_instruction[i][31:20];
                        7'b0100011: // S-type (stores)
                            temp_imm = {{20{fetch_instruction[i][31]}}, fetch_instruction[i][31:25], fetch_instruction[i][11:7]};
                        7'b0010011: // I-type (addi, slti, etc.)
                            temp_imm = fetch_instruction[i][31:20];
                        7'b1100111: // I-type (jalr)
                            temp_imm = fetch_instruction[i][31:20];
                        default:
                            temp_imm = decode_immediate(fetch_instruction[i], fetch_instruction[i][6:0]);
                    endcase

                    // Assign the correctly decoded immediate to both fields
                    decode_reg[i].immediate <= temp_imm;
                    decode_reg[i].offset    <= temp_imm;
                    // Allow ALU, MEM, MULDIV, and BRANCH types
                    if (decode_reg[i].instr_type == 2'b11 || decode_reg[i].instr_type == 2'b00 || decode_reg[i].instr_type == 2'b01 || decode_reg[i].instr_type == 2'b10) begin
                        decode_reg[i].valid <= 1'b1;
                    end else begin
                        decode_reg[i].valid <= 1'b0;
                    end

                    // Print statement removed

                end else if (!dec_rdy[i]) begin
                    // During stall: preserve existing decode register contents
                    // Explicitly preserve the valid bit and other fields
                    decode_reg[i].valid <= decode_reg[i].valid;  // Keep current valid state
                    // Note: Other fields are not assigned, so they retain their current values
                end else begin
                    // No valid instruction from fetch and not stalled - clear valid bit
                    decode_reg[i].valid       <= 1'b0;
                end
            end
            
            // --------------------------------------------------------------------
            // Error Detection State Update
            // --------------------------------------------------------------------
            
            // Store current error detection state for next cycle's edge detection
            for (i = 0; i < ISSUE_WIDTH; i = i + 1) begin
                decode_error_prev[i] <= decode_error_detect[i];
            end
            
            // Assert decoder error for exactly 1 cycle when new error is detected
            for (i = 0; i < ISSUE_WIDTH; i = i + 1) begin
                decoder_error[i] <= decode_error_pulse[i];
            end
        end
    end

endmodule