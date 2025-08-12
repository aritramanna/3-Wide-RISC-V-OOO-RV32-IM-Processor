
`timescale 1ns / 1ps

// ============================================================================
// Top-Level Memory Subsystem: MMU + Data Memory
// ----------------------------------------------------------------------------
// Instantiates memory_management_unit and data_mem
// Wires all memory interface signals between them
// Exposes only necessary external ports
// ============================================================================

`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"
`include "memory_management_unit.sv"
`include "data_mem.sv"

import tomasulo_pkg::*;

module top_memory_subsystem #(
    parameter LSB_SIZE = 8,
    parameter ROB_SIZE = 16,
    parameter MEM_DEPTH = 1024,
    parameter DATA_WIDTH = 32,
    parameter NUM_LD_PORTS = 2,
    parameter NUM_ST_PORTS = 1,
    parameter LQ_DEPTH = 8,
    parameter SQ_DEPTH = 8
) (
    input  logic        clk,
    input  logic        rst,
    input  logic        stall,
    // LSB interface
    input  LS_Buffer_Entry_t [LSB_SIZE-1:0] lsb_entries,
    // ROB interface (simplified - MMU only needs head and head ready status)
    input  logic [$clog2(ROB_SIZE)-1:0] rob_head [ISSUE_WIDTH-1:0],
    // Control
    input  logic flush,
    // Result interface (to ROB/CDB)
    output ALU_Result_t [NUM_LD_PORTS-1:0] load_cdb_result,
    output logic [NUM_LD_PORTS-1:0] mem_read_en,
    output logic [NUM_ST_PORTS-1:0] mem_write_en,
    output logic [LSB_SIZE-1:0] lsb_entry_consumed,
    output logic [ISSUE_WIDTH-1:0] store_commit_valid,
    output logic [$clog2(ROB_SIZE)-1:0] store_commit_rob_index [ISSUE_WIDTH-1:0]
);
    // Internal wires for memory interface
    logic [NUM_ST_PORTS-1:0] mem_write_en_wire;
    logic [31:0] mem_write_addr [NUM_ST_PORTS-1:0];
    logic [DATA_WIDTH-1:0] mem_write_data [NUM_ST_PORTS-1:0];
    logic [1:0] mem_write_size [NUM_ST_PORTS-1:0];
    logic [NUM_LD_PORTS-1:0] mem_read_en_wire;
    logic [31:0] mem_read_addr [NUM_LD_PORTS-1:0];
    logic [1:0] mem_read_size [NUM_LD_PORTS-1:0];
    logic [NUM_LD_PORTS-1:0] mem_read_unsigned;
    logic [DATA_WIDTH-1:0] mem_read_data [NUM_LD_PORTS-1:0];
    logic [LSB_SIZE-1:0] lsb_entry_consumed_wire;

    // Update MMU instantiation to use vectorized rob_head, rob_head_ready, rob_head_opcode
    memory_management_unit #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .LSB_SIZE(LSB_SIZE),
        .ROB_SIZE(ROB_SIZE),
        .MEM_DEPTH(MEM_DEPTH),
        .DATA_WIDTH(DATA_WIDTH),
        .NUM_LD_PORTS(NUM_LD_PORTS),
        .NUM_ST_PORTS(NUM_ST_PORTS),
        .LQ_DEPTH(LQ_DEPTH),
        .SQ_DEPTH(SQ_DEPTH)
    ) mmu (
        .clk(clk),
        .rst(rst),
        .lsb_entries(lsb_entries),
        .rob_head(rob_head),
        .mem_write_en(mem_write_en_wire),
        .mem_write_addr(mem_write_addr),
        .mem_write_data(mem_write_data),
        .mem_write_size(mem_write_size),
        .mem_read_en(mem_read_en_wire),
        .mem_read_addr(mem_read_addr),
        .mem_read_size(mem_read_size),
        .mem_read_unsigned(mem_read_unsigned),
        .mem_read_data(mem_read_data),
        .load_cdb_result(load_cdb_result),
        .flush(flush),
        .stall(stall),
        .lsb_entry_consumed(lsb_entry_consumed_wire),
        .store_commit_valid(store_commit_valid),
        .store_commit_rob_index(store_commit_rob_index)
    );

    data_mem #(
        .MEM_DEPTH(MEM_DEPTH),
        .DATA_WIDTH(DATA_WIDTH),
        .NUM_LD_PORTS(NUM_LD_PORTS),
        .NUM_ST_PORTS(NUM_ST_PORTS)
    ) dmem (
        .clk(clk),
        .rst(rst),
        .write_en(mem_write_en_wire),
        .write_addr(mem_write_addr),
        .write_data(mem_write_data),
        .write_size(mem_write_size),
        .read_en(mem_read_en_wire),
        .read_addr(mem_read_addr),
        .read_size(mem_read_size),
        .read_unsigned(mem_read_unsigned),
        .read_data(mem_read_data)
    );

    assign mem_write_en = mem_write_en_wire;
    assign mem_read_en  = mem_read_en_wire;
    assign lsb_entry_consumed = lsb_entry_consumed_wire;
endmodule 