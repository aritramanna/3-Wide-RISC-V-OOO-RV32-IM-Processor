`timescale 1ns / 1ps
// Include required packages
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"

import tomasulo_pkg::*;

module alu_unit #(
    parameter OP_WIDTH = 17,
    parameter ROB_IDX_WIDTH = $clog2(no_ROB)
) (
    input  AddSub_RS_Entry_t rs_in,
    input  logic             ex_valid,
    output ALU_Result_t      alu_out
);

    always_comb begin
        alu_out = '0;

        if (ex_valid) begin
            alu_out.ROB_index = rs_in.ROB_index;
            alu_out.result_ready = 1'b1;

            case (rs_in.operation)
                add:    alu_out.result = rs_in.src1_value + rs_in.src2_value;
                sub:    alu_out.result = rs_in.src1_value - rs_in.src2_value;
                addi:   begin
                    alu_out.result = rs_in.src1_value + rs_in.immediate;
                end
                and_op: alu_out.result = rs_in.src1_value & rs_in.src2_value;
                or_op:  alu_out.result = rs_in.src1_value | rs_in.src2_value;
                xor_op: alu_out.result = rs_in.src1_value ^ rs_in.src2_value;
                sll:    alu_out.result = rs_in.src1_value << rs_in.src2_value[4:0];
                srl:    alu_out.result = rs_in.src1_value >> rs_in.src2_value[4:0];
                sra:    alu_out.result = $signed(rs_in.src1_value) >>> rs_in.src2_value[4:0];
                slt:    alu_out.result = ($signed(rs_in.src1_value) < $signed(rs_in.src2_value)) ? 32'd1 : 32'd0;
                sltu:   alu_out.result = (rs_in.src1_value < rs_in.src2_value) ? 32'd1 : 32'd0;
                default: begin
                    alu_out.result = 32'hDEADBEEF;
                    alu_out.result_ready = 1'b0;
                end
            endcase
            
            // ALU Result Print
            /*$display("[%0t] ALU_UNIT: ROB_ID=%0d Op=0x%05x Src1=0x%08x Src2=0x%08x Result=0x%08x", 
                $realtime, rs_in.ROB_index, rs_in.operation, rs_in.src1_value, rs_in.src2_value, alu_out.result);*/
        end else begin
            alu_out.result_ready = 1'b0;
        end
    end

endmodule