`timescale 1ns / 1ps
`include "alu_unit.sv"
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"
import tomasulo_pkg::*;

module alu_top (
    input  logic clk,
    input  logic rst,
    input  logic flush,
    input  logic stall,
    input  AddSub_RS_Entry_t [no_RS_addsublog-1:0] alu_rs_in,
    output ALU_Result_t [no_ALU_units-1:0] alu_out,
    output logic [no_RS_addsublog-1:0] rs_clear  // RS deallocation signals
);
    // rsv to alu routing
    AddSub_RS_Entry_t alu_in[no_ALU_units];
    // Functional unit execution valid
    logic [no_ALU_units-1:0] ex_valid;
    // Loop variables
    int alu, alu_rst, alu_ex, rs_idx;
    // Priority encoder variables
    int min_rob;
    int min_idx;
    int rs;
    logic [no_RS_addsublog-1:0] allocated_this_cycle;

  always_comb begin
        if (rst || flush) begin
            for (alu_rst = 0; alu_rst < no_ALU_units; alu_rst++) begin
                alu_in[alu_rst] = '{default: '0};
            end
            ex_valid = '0;
            rs_clear = '0;  // Clear RS signals on reset/flush
        end else begin
            // Initialize outputs
            for (alu = 0; alu < no_ALU_units; alu++) begin
                alu_in[alu] = '{default: '0};
            end
            ex_valid = '0;
            rs_clear = '0;  // Initialize RS clear signals

            if (!stall) begin
                // Priority encoder-based dispatch
                allocated_this_cycle = '0;
                for (alu = 0; alu < no_ALU_units; alu++) begin
                    min_rob = {32{1'b1}}; // max value
                    min_idx = -1;
                    for (rs = 0; rs < no_RS_addsublog; rs++) begin
                        if (alu_rs_in[rs].busy && alu_rs_in[rs].ready && !allocated_this_cycle[rs]) begin
                            if (alu_rs_in[rs].ROB_index < min_rob) begin
                                min_rob = alu_rs_in[rs].ROB_index;
                                min_idx = rs;
                            end
                        end
                    end
                    if (min_idx != -1) begin
                        alu_in[alu] = alu_rs_in[min_idx];
                        ex_valid[alu] = 1;
                        allocated_this_cycle[min_idx] = 1;
                        rs_clear[min_idx] = 1'b1;  // Signal to clear this RS entry
                    end else begin
                        ex_valid[alu] = 0;
                    end
                end
            end
        end
    end

    // REMOVED: Combinational assignment for rsv_allocated_to_func

    genvar alu_idx;
    generate
        for (alu_idx = 0; alu_idx < no_ALU_units; alu_idx++) begin : alu_units
            alu_unit alu_inst (
                .rs_in(alu_in[alu_idx]),
                .alu_out(alu_out[alu_idx]),
                .ex_valid(ex_valid[alu_idx])
            );
        end
    endgenerate
    
endmodule

