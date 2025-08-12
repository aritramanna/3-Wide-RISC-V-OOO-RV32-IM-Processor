`timescale 1ns / 1ps
// Include required packages
`include "fetch_pkg.sv"
`include "decoder_pkg.sv"
`include "tomasulo_pkg.sv"
`include "radix_4_multiplier.sv"
`include "radix_8_dividor.sv"

import tomasulo_pkg::*;

module muldiv_unit #(
    parameter OP_WIDTH = 17,
    parameter ROB_IDX_WIDTH = $clog2(no_ROB)
) (
    input  logic clk,
    input  logic rst,
    input  logic flush,
    // Up to 2 arbitrary ops per cycle
    input  MulDiv_RS_Entry_t rs_in [2],
    input  logic ex_valid [2],
    // Outputs in same order as inputs
    output ALU_Result_t out [2]
);
    // Remove unused internal registers
    // Only keep signals for multiplier/divider control and output

    // Multiplier signals
    logic                        mul_valid_in;
    logic signed [31:0]          mul_multiplicand, mul_multiplier;
    logic [ROB_IDX_WIDTH-1:0]    mul_tag_in;
    logic                        mul_valid_out;
    logic signed [63:0]          mul_product;
    logic [ROB_IDX_WIDTH-1:0]    mul_tag_out;

    // Divider signals
    logic                        div_valid_in;
    logic signed [31:0]          div_dividend, div_divisor;
    logic                        div_signed;
    logic [ROB_IDX_WIDTH-1:0]    div_tag_in;
    logic                        div_valid_out;
    logic signed [31:0]          div_quotient, div_remainder;
    logic [ROB_IDX_WIDTH-1:0]    div_tag_out;

    // Operation pipeline registers for each tag (parameterized by no_ROB)
    logic [OP_WIDTH-1:0] mul_op_pipeline [0:no_ROB-1];
    logic [OP_WIDTH-1:0] div_op_pipeline [0:no_ROB-1];
    
    // Store operation for each tag on issue and clear when operation completes
    always_ff @(posedge clk) begin
        if (rst || flush) begin
            for (int i = 0; i < no_ROB; i++) begin
                mul_op_pipeline[i] <= 17'h1FFFF; // Initialize to invalid
                div_op_pipeline[i] <= 17'h1FFFF; // Initialize to invalid
            end
        end else begin
            // Update operation pipeline entries
            for (int i = 0; i < no_ROB; i++) begin
                // Priority: clear completed operations first, then set new operations
                if (mul_valid_out && mul_tag_out == i) begin
                    mul_op_pipeline[i] <= 17'h1FFFF; // Clear completed multiply
                end else if (div_valid_out && div_tag_out == i) begin
                    div_op_pipeline[i] <= 17'h1FFFF; // Clear completed divide
                end else if (mul_valid_in && mul_tag_in == i) begin
                    mul_op_pipeline[i] <= rs_in[0].operation; // Set new multiply
                end else if (div_valid_in && div_tag_in == i) begin
                    div_op_pipeline[i] <= rs_in[1].operation; // Set new divide
                end
                // Otherwise keep current value (implicit)
            end
        end
    end

    // Instantiate radix-4 Booth multiplier (3-stage pipeline)
    radix4_booth_multiplier #(.WIDTH(32), .TAG_WIDTH(ROB_IDX_WIDTH)) u_radix4_booth_multiplier (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .valid_in(mul_valid_in),
        .multiplicand(mul_multiplicand),
        .multiplier(mul_multiplier),
        .tag_in(mul_tag_in),
        .valid_out(mul_valid_out),
        .product(mul_product),
        .tag_out(mul_tag_out)
    );

    // Instantiate radix-8 divider (pipelined)
    radix8_divider #(.WIDTH(32), .TAG_WIDTH(ROB_IDX_WIDTH)) u_radix8_divider (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .valid_in(div_valid_in),
        .dividend(div_dividend),
        .divisor(div_divisor),
        .tag_in(div_tag_in),
        .valid_out(div_valid_out),
        .quotient(div_quotient),
        .remainder(div_remainder),
        .tag_out(div_tag_out)
    );

    // Fixed mapping: slot 0 = multiplier, slot 1 = divider
    // Multiplier path
    assign mul_valid_in      = ex_valid[0] && (rs_in[0].operation == mul || rs_in[0].operation == mulh);
    assign mul_multiplicand  = $signed(rs_in[0].src1_value);
    assign mul_multiplier    = $signed(rs_in[0].src2_value);
    assign mul_tag_in        = rs_in[0].ROB_index;

    // Divider path
    assign div_valid_in      = ex_valid[1] && (rs_in[1].operation == div || rs_in[1].operation == rem);
    assign div_signed        = 1'b1; // Always signed, since only div/rem are supported
    assign div_dividend      = $signed(rs_in[1].src1_value);
    assign div_divisor       = $signed(rs_in[1].src2_value);
    assign div_tag_in        = rs_in[1].ROB_index;
    


    // Use combinational logic to avoid the race entirely
    always_comb begin
        // Default outputs: invalid
        out[0].result = 32'h0;
        out[0].result_ready = 1'b0;
        out[0].ROB_index = '0;
        out[1].result = 32'h0;
        out[1].result_ready = 1'b0;
        out[1].ROB_index = '0;
        
        // Multiplier result to out[0]
        if (mul_valid_out && mul_op_pipeline[mul_tag_out] != 17'h1FFFF) begin
            case (mul_op_pipeline[mul_tag_out])
                mul:   out[0].result = mul_product[31:0];
                mulh:  out[0].result = mul_product[63:32];
                default: out[0].result = 32'h0;
            endcase
            out[0].ROB_index = mul_tag_out;
            out[0].result_ready = 1'b1;
        end
        
        // Divider result to out[1]
        if (div_valid_out && div_op_pipeline[div_tag_out] != 17'h1FFFF) begin
            case (div_op_pipeline[div_tag_out])
                div: out[1].result = $signed(div_quotient);
                rem: out[1].result = $signed(div_remainder);
                default: out[1].result = 32'h0;
            endcase
            out[1].ROB_index = div_tag_out;
            out[1].result_ready = 1'b1;
        end
    end
endmodule 