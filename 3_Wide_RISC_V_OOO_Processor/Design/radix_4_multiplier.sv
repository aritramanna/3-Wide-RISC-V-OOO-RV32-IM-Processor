//-----------------------------------------------------------------------------
// Radix-4 Booth Multiplier with CSA Tree and 3-Stage Pipelining
//-----------------------------------------------------------------------------
// Description:
// - Accepts two signed inputs and computes a signed product using radix-4
//   Booth encoding and a CSA tree-based reduction.
// - Uses 3 pipeline stages: Booth encoding, CSA reduction, CPA & output
// - Tag and valid signals are pipelined along with data
//-----------------------------------------------------------------------------

module radix4_booth_multiplier #(
    parameter int WIDTH = 32,
    parameter int TAG_WIDTH = 5
)(
    input  logic                         clk,
    input  logic                         rst,
    input  logic                         flush,
    input  logic                         valid_in,
    input  logic signed [WIDTH-1:0]      multiplicand,
    input  logic signed [WIDTH-1:0]      multiplier,
    input  logic        [TAG_WIDTH-1:0]  tag_in,

    output logic                         valid_out,
    output logic signed [2*WIDTH-1:0]    product,
    output logic        [TAG_WIDTH-1:0]  tag_out
);

    //--------------------------------------------------------------------------
    // Local Parameters & Derived Constants
    //--------------------------------------------------------------------------
    localparam int GROUPS     = WIDTH / 2;
    localparam int PP_WIDTH   = 2*WIDTH + 1;
    localparam int REDUCE_CNT = (GROUPS + 2) / 3;

    //--------------------------------------------------------------------------
    // Stage 1: Booth Encoding and Partial Product Generation
    //--------------------------------------------------------------------------

    // Temporary variables for booth encoding
    logic signed [WIDTH:0]      pp_values_temp   [0:GROUPS-1];
    logic [2:0]                 booth_bits_temp  [0:GROUPS-1];

    // Registered outputs
    logic signed [PP_WIDTH-1:0] pp_stage1        [0:GROUPS-1];
    logic                       valid_stage1;
    logic        [TAG_WIDTH-1:0]          tag_stage1;

    integer s1, g;
    always_ff @(posedge clk or posedge rst) begin
        if (rst || flush) begin
            valid_stage1 <= 1'b0;
            tag_stage1   <= '0;
            for (s1 = 0; s1 < GROUPS; s1++) begin
                pp_stage1[s1] <= '0;
            end
        end else begin
            valid_stage1 <= valid_in;
            tag_stage1   <= tag_in;
            
            // Compute booth encoding and partial products in the same cycle
            for (g = 0; g < GROUPS; g++) begin
                booth_bits_temp[g] = {
                    multiplier[2*g+1],
                    multiplier[2*g],
                    (g == 0) ? 1'b0 : multiplier[2*g-1]
                };

                case (booth_bits_temp[g])
                    3'b000, 3'b111: pp_values_temp[g] = 0;
                    3'b001, 3'b010: pp_values_temp[g] = multiplicand;
                    3'b011:         pp_values_temp[g] = multiplicand <<< 1;
                    3'b100:         pp_values_temp[g] = -(multiplicand <<< 1);
                    3'b101, 3'b110: pp_values_temp[g] = -multiplicand;
                    default:        pp_values_temp[g] = 0;
                endcase

                pp_stage1[g] <= $signed({{(PP_WIDTH - WIDTH - 1){pp_values_temp[g][WIDTH]}}, pp_values_temp[g]}) <<< (2*g);
            end
        end
    end

    //--------------------------------------------------------------------------
    // Stage 2: CSA Tree Reduction
    //--------------------------------------------------------------------------

    logic signed [PP_WIDTH-1:0] sum_stage2   [0:REDUCE_CNT-1];
    logic signed [PP_WIDTH-1:0] carry_stage2 [0:REDUCE_CNT-1];
    logic                       valid_stage2;
    logic        [TAG_WIDTH-1:0]          tag_stage2;

    // Variables declared outside always block
    integer s2, idx0, idx1, idx2;
    logic signed [PP_WIDTH-1:0] a_csa, b_csa, c_csa;

    always_ff @(posedge clk or posedge rst) begin
        if (rst || flush) begin
            valid_stage2 <= 1'b0;
            tag_stage2   <= '0;
            for (s2 = 0; s2 < REDUCE_CNT; s2++) begin
                sum_stage2[s2]   <= '0;
                carry_stage2[s2] <= '0;
            end
        end else begin
            valid_stage2 <= valid_stage1;
            tag_stage2   <= tag_stage1;

            for (s2 = 0; s2 < REDUCE_CNT; s2++) begin
                idx0 = s2 * 3;
                idx1 = idx0 + 1;
                idx2 = idx0 + 2;

                a_csa = (idx0 < GROUPS) ? pp_stage1[idx0] : '0;
                b_csa = (idx1 < GROUPS) ? pp_stage1[idx1] : '0;
                c_csa = (idx2 < GROUPS) ? pp_stage1[idx2] : '0;

                sum_stage2[s2]   <= a_csa ^ b_csa ^ c_csa;
                carry_stage2[s2] <= (a_csa & b_csa) | (b_csa & c_csa) | (a_csa & c_csa);
            end
        end
    end

    //--------------------------------------------------------------------------
    // Stage 3: Final CPA (Carry Propagate Adder) and Output Registering
    //--------------------------------------------------------------------------

    logic signed [PP_WIDTH-1:0] sum_final;
    logic signed [PP_WIDTH-1:0] carry_final;
    logic signed [PP_WIDTH-1:0] result_cpa;
    logic                       valid_stage3;
    logic        [TAG_WIDTH-1:0]          tag_stage3;

    integer s3;
    always_ff @(posedge clk or posedge rst) begin
        if (rst || flush) begin
            valid_stage3 <= 1'b0;
            tag_stage3   <= '0;
            sum_final    <= '0;
            carry_final  <= '0;
            result_cpa   <= '0;
        end else begin
            valid_stage3 <= valid_stage2;
            tag_stage3   <= tag_stage2;

            sum_final   = '0;
            carry_final = '0;
            for (s3 = 0; s3 < REDUCE_CNT; s3++) begin
                sum_final   = sum_final   + sum_stage2[s3];
                carry_final = carry_final + (carry_stage2[s3] << 1);
            end

            result_cpa <= sum_final + carry_final;
        end
    end

    //--------------------------------------------------------------------------
    // Final Output
    //--------------------------------------------------------------------------

    assign product   = valid_stage3 ? result_cpa[2*WIDTH-1:0] : '0;
    assign tag_out   = valid_stage3 ? tag_stage3 : '0;
    assign valid_out = valid_stage3;
    
    // Print final output
    /*always @(posedge clk) begin
        if (valid_stage3) begin
            $display("[%0t] MUL_OUTPUT: Tag=%0d Product=%0d (0x%0x) Valid=%0b", 
                     $realtime, tag_stage3, product, product, valid_out);
        end
    end*/

endmodule
