// Pipelined, synthesizable radix-8 divider (SystemVerilog)
// Each pipeline stage processes 3 bits of the dividend per clock cycle
// Outputs are valid after STAGES cycles

module radix8_divider #(
    parameter WIDTH = 32,
    parameter TAG_WIDTH = 5
) (
    input  logic                     clk,
    input  logic                     rst,
    input  logic                     flush,
    input  logic                     valid_in,
    input  logic signed [WIDTH-1:0]  dividend,
    input  logic signed [WIDTH-1:0]  divisor,
    input  logic [TAG_WIDTH-1:0]     tag_in,

    output logic                     valid_out,
    output logic signed [WIDTH-1:0]  quotient,
    output logic signed [WIDTH-1:0]  remainder,
    output logic [TAG_WIDTH-1:0]     tag_out
);
    // Number of radix-8 pipeline stages needed to process WIDTH bits
    localparam STAGES = (WIDTH + 2) / 3;

    // Legacy variables (not used in pipeline, but kept for compatibility)
    logic signed [WIDTH-1:0] dividend_abs, divisor_abs;
    logic sign_quotient, sign_remainder;
    logic [WIDTH-1:0] quotient_reg;
    logic signed [2*WIDTH+2:0] partial_remainder;
    logic [WIDTH-1:0] dividend_reg;
    logic [4:0] tag_reg;
    logic busy;
    integer i, j;
    // Declare all local variables at the top for tool compatibility
    int bit_pos;
    logic [2:0] next_bits;
    logic [2:0] q_digit;
    logic [2:0] best_q_digit;
    logic found;

    // Struct for all per-stage pipeline variables
    typedef struct packed {
        logic signed [2*WIDTH+2:0] partial_remainder; // Current partial remainder for this stage
        logic [WIDTH-1:0] quotient;                  // Accumulated quotient for this stage
        logic [WIDTH-1:0] dividend;                  // Absolute value of dividend (propagated)
        logic [WIDTH-1:0] divisor;                   // Absolute value of divisor (propagated)
        logic sign_quotient;                         // Sign of quotient (propagated)
        logic sign_remainder;                        // Sign of remainder (propagated)
        logic [TAG_WIDTH-1:0] tag;                   // Tag for result tracking (propagated)
        logic valid;                                 // Valid bit for pipeline handshake
    } stage_t;
    stage_t pipeline [0:STAGES]; // The pipeline register array

    always_ff @(posedge clk or posedge rst) begin
        // Temporary arrays for next state (to avoid multiple assignments per clock)
        stage_t next_pipeline [0:STAGES];
        int i, j;
        int bit_pos;
        logic [2:0] next_bits;
        logic [2:0] q_digit;
        logic [2:0] best_q_digit;
        logic found;

        if (rst || flush) begin
            // Reset all pipeline registers
            for (i = 0; i <= STAGES; i++) begin
                pipeline[i].partial_remainder <= 0;
                pipeline[i].quotient <= 0;
                pipeline[i].dividend <= 0;
                pipeline[i].divisor <= 0;
                pipeline[i].sign_quotient <= 0;
                pipeline[i].sign_remainder <= 0;
                pipeline[i].tag <= 0;
                pipeline[i].valid <= 0;
            end
        end else begin
            // Stage 0: latch new input and initialize pipeline
            if (valid_in) begin
                next_pipeline[0].dividend = (dividend[WIDTH-1]) ? -dividend : dividend;
                next_pipeline[0].divisor = (divisor[WIDTH-1]) ? -divisor : divisor;
                next_pipeline[0].sign_quotient = dividend[WIDTH-1] ^ divisor[WIDTH-1];
                next_pipeline[0].sign_remainder = dividend[WIDTH-1];
                next_pipeline[0].tag = tag_in;
                next_pipeline[0].valid = 1'b1;
                next_pipeline[0].partial_remainder = 0;
                next_pipeline[0].quotient = 0;
                
                // Print divider input values
                /*$display("[%0t] DIV_INPUT: Tag=%0d Dividend=%0d Divisor=%0d", 
                         $realtime, tag_in, dividend, divisor);*/
            end else begin
                // If not valid, clear stage 0
                next_pipeline[0].dividend = 0;
                next_pipeline[0].divisor = 0;
                next_pipeline[0].sign_quotient = 0;
                next_pipeline[0].sign_remainder = 0;
                next_pipeline[0].tag = 0;
                next_pipeline[0].valid = 0;
                next_pipeline[0].partial_remainder = 0;
                next_pipeline[0].quotient = 0;
            end

            // Pipeline stages: propagate state and perform one radix-8 division step per stage
            for (i = 1; i <= STAGES; i++) begin
                // Propagate pipeline state from previous stage
                next_pipeline[i].dividend = pipeline[i-1].dividend;
                next_pipeline[i].divisor = pipeline[i-1].divisor;
                next_pipeline[i].sign_quotient = pipeline[i-1].sign_quotient;
                next_pipeline[i].sign_remainder = pipeline[i-1].sign_remainder;
                next_pipeline[i].tag = pipeline[i-1].tag;
                next_pipeline[i].valid = pipeline[i-1].valid;
                next_pipeline[i].quotient = pipeline[i-1].quotient;
                next_pipeline[i].partial_remainder = pipeline[i-1].partial_remainder;

                // Radix-8 step: extract next 3 bits from dividend (from MSB to LSB)
                bit_pos = (STAGES - i) * 3;  // Start from MSB
                if (bit_pos < WIDTH) begin
                    if (bit_pos + 2 < WIDTH)
                        next_bits = (pipeline[i-1].dividend >> bit_pos) & 3'b111;
                    else
                        next_bits = (pipeline[i-1].dividend >> bit_pos) & ((1 << (WIDTH - bit_pos)) - 1);
                end else begin
                    next_bits = 0;
                end
                // Shift partial remainder left by 3 and append next bits
                next_pipeline[i].partial_remainder = (pipeline[i-1].partial_remainder << 3) | next_bits;
                // Find the largest q_digit such that trial_rem >= 0 (no break, synthesizable)
                best_q_digit = 0;
                found = 0;
                for (j = 7; j >= 0; j = j - 1) begin
                    if (!found && (next_pipeline[i].partial_remainder >= pipeline[i-1].divisor * j)) begin
                        best_q_digit = j[2:0];
                        found = 1;
                    end
                end
                q_digit = best_q_digit;
                if (found)
                    next_pipeline[i].partial_remainder = next_pipeline[i].partial_remainder - pipeline[i-1].divisor * q_digit;
                // Accumulate quotient for this stage
                next_pipeline[i].quotient = (pipeline[i-1].quotient << 3) | q_digit;
                

            end

            // Write all next state values to the actual pipeline registers
            for (i = 0; i <= STAGES; i++) begin
                pipeline[i] <= next_pipeline[i];
            end
        end
    end

    // Output from the last pipeline stage, with sign restoration and masking
    assign quotient  = pipeline[STAGES].valid ? (pipeline[STAGES].sign_quotient ? -pipeline[STAGES].quotient[WIDTH-1:0] : pipeline[STAGES].quotient[WIDTH-1:0]) : '0;
    assign remainder = pipeline[STAGES].valid ? (pipeline[STAGES].sign_remainder ? -pipeline[STAGES].partial_remainder[WIDTH-1:0] : pipeline[STAGES].partial_remainder[WIDTH-1:0]) : '0;
    assign valid_out = pipeline[STAGES].valid;
    assign tag_out   = pipeline[STAGES].valid ? pipeline[STAGES].tag : '0;
    
    // Print final output
    /*always @(posedge clk) begin
        if (pipeline[STAGES].valid) begin
            $display("[%0t] DIV_OUTPUT: Tag=%0d Quotient=%0d Remainder=%0d Valid=%0b", 
                     $realtime, pipeline[STAGES].tag, quotient, remainder, valid_out);
        end
    end*/
    
endmodule