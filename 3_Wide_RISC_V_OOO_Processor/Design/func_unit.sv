`timescale 1ns / 1ps

// -----------------------------------------------------------------------------
// Unified Functional Unit Module
// Simple instantiation of ALU and MulDiv top modules with output routing
// -----------------------------------------------------------------------------

`include "alu_top.sv"
`include "mul_div_top.sv"
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"

import tomasulo_pkg::*;

module func_unit (
    input  logic clk,    // Clock
    input  logic rst,    // Reset (active high)
    input  logic flush,  // Pipeline flush
    input  logic stall,  // Stall from completion buffer
    
    // ALU Reservation Station inputs
    input  AddSub_RS_Entry_t [no_RS_addsublog-1:0] alu_rs_in,
    
    // MulDiv Reservation Station inputs
    input  MulDiv_RS_Entry_t [no_RS_muldiv-1:0] muldiv_rs_in,
    
    // Unified output interface
    output ALU_Result_t [no_ALU_units + 2*no_MulDiv_units-1:0] func_out,
    
    // RS deallocation signals
    output logic [no_RS_addsublog-1:0] alu_rs_clear,
    output logic [no_RS_muldiv-1:0] muldiv_rs_clear
);

    // ALU top module outputs
    ALU_Result_t [no_ALU_units-1:0] alu_out;
    
    // MulDiv top module outputs
    ALU_Result_t [2*no_MulDiv_units-1:0] muldiv_out;
    
    // Loop variables for output routing
    int alu_out_idx;
    int muldiv_out_idx;

    // -------------------------------------------------------------------------
    // ALU Top Module Instantiation
    // -------------------------------------------------------------------------
    alu_top alu_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .stall(stall),
        .alu_rs_in(alu_rs_in),
        .alu_out(alu_out),
        .rs_clear(alu_rs_clear)
    );

    // -------------------------------------------------------------------------
    // MulDiv Top Module Instantiation
    // -------------------------------------------------------------------------
    muldiv_top muldiv_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .stall(stall),
        .muldiv_rs_in(muldiv_rs_in),
        .muldiv_out(muldiv_out),
        .rs_clear(muldiv_rs_clear)
    );

    // -------------------------------------------------------------------------
    // Unified Output Assignment - Combinational routing
    // ALU outputs come first, followed by MulDiv outputs
    // -------------------------------------------------------------------------
    always_comb begin
        // Assign ALU outputs
        for (alu_out_idx = 0; alu_out_idx < no_ALU_units; alu_out_idx++) begin
            func_out[alu_out_idx] = alu_out[alu_out_idx];
        end
        // Assign MulDiv outputs
        for (muldiv_out_idx = 0; muldiv_out_idx < 2*no_MulDiv_units; muldiv_out_idx++) begin
            func_out[no_ALU_units + muldiv_out_idx] = muldiv_out[muldiv_out_idx];
        end
    end

endmodule 