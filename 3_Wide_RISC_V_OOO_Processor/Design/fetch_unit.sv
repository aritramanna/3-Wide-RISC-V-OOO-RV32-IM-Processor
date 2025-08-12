//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Create Date: 06/14/2025 02:20:00 PM
// Module Name: Fetch Unit Wrapper for RISC-V Tomasulo Core
// Description: Instantiates instruction_memory and passes through all signals.
//////////////////////////////////////////////////////////////////////////////////

import fetch_types_pkg::*;

// -----------------------------------------------------------------------------
// Parameters:
//   ISSUE_WIDTH: Number of instructions fetched/enqueued/dequeued per cycle
//   IQ_DEPTH   : Depth of the instruction queue
// -----------------------------------------------------------------------------
module fetch_unit #(
    parameter int ISSUE_WIDTH = 3,
    parameter int IQ_DEPTH    = 16,
    parameter int NO_INSTR    = 33,
    parameter int INSTRUCTION_MEMORY_SIZE = 64
) (
    input  logic clk,
    input  logic rst,
    input  logic stall_in,
    input  logic [31:0] pc_in,
    input  logic [ISSUE_WIDTH-1:0] deq_ready,
    input  logic flush,
    output logic [31:0] debug_pc,
    output logic [31:0] debug_cycle_count,
    output logic init_done,
    output logic init_done_pulse,
    output logic [ISSUE_WIDTH-1:0] out_deq_valid,
    output fetch_entry_t out_deq_data [ISSUE_WIDTH-1:0],
    output fetch_entry_t fec_dec_latch_out [ISSUE_WIDTH-1:0],
    output fetch_entry_t imem_fetch_reg_out [ISSUE_WIDTH-1:0]
);

    // Internal connection between instruction_memory and instruction_queue
    fetch_entry_t fetch_reg_out [ISSUE_WIDTH-1:0];

    // Enqueue/dequeue signals
    logic [ISSUE_WIDTH-1:0] enq_valid;
    fetch_entry_t enq_data [ISSUE_WIDTH-1:0];
    logic [ISSUE_WIDTH-1:0] enq_ready;
    logic [ISSUE_WIDTH-1:0] deq_valid;
    fetch_entry_t deq_data [ISSUE_WIDTH-1:0];
    logic iq_full, iq_empty;
    logic imem_stall;

    // Fetch-to-decode latch
    fetch_entry_t fec_dec_latch [ISSUE_WIDTH-1:0];

    // Sequential logic for the fetch-to-decode latch
    always_ff @(posedge clk or posedge rst or posedge flush) begin
        if (rst || flush) begin
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                fec_dec_latch[i] <= '{default: 0};
            end
        end else begin
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                if (deq_ready[i] && deq_valid[i]) begin
                    fec_dec_latch[i] <= deq_data[i];
                end
            end
        end
    end

    // Expose the fec_dec latch as an output
    assign fec_dec_latch_out = fec_dec_latch;

    // Expose instruction memory output directly for predecode (no procedural assignment)
    assign imem_fetch_reg_out = fetch_reg_out;

    // Enqueue only if fetch is valid and queue is ready
    always_comb begin
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
          enq_valid[i] = fetch_reg_out[i].valid && enq_ready[i] && !stall_in;
            enq_data[i]  = fetch_reg_out[i];
        end
    end

    // Expose dequeue interface as outputs (for now, just as example)
    assign out_deq_valid = deq_valid;
    assign out_deq_data = deq_data;

    // Instantiate instruction_queue
    instruction_queue #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .IQ_DEPTH(IQ_DEPTH)
    ) iq_inst (
        .clk(clk),
        .rst(rst),
        .flush(flush),
        .enq_valid(enq_valid),
        .enq_data(enq_data),
        .enq_ready(enq_ready),
        .deq_ready(deq_ready),
        .deq_valid(deq_valid),
        .deq_data(deq_data),
        .full(iq_full),
        .empty(iq_empty)
    );

    // Assign stall for instruction memory: stall if global stall or IQ is full
    assign imem_stall = stall_in || iq_full;

    // Instantiate instruction_memory
    instruction_memory #(
        .ISSUE_WIDTH(ISSUE_WIDTH),
        .NO_INSTR(NO_INSTR),
        .INSTRUCTION_MEMORY_SIZE(INSTRUCTION_MEMORY_SIZE)
    ) imem_inst (
        .clk              (clk),
        .rst              (rst),
        .flush            (flush),
        .stall_in         (imem_stall),
        .pc_in            (pc_in),
        .fetch_reg_out    (fetch_reg_out),
        .debug_pc         (debug_pc),
        .debug_cycle_count(debug_cycle_count),
        .init_done        (init_done),
        .init_done_pulse  (init_done_pulse)
    );

endmodule 

// ============================================================================
// Instruction Queue (Flexible Multi-Ported FIFO) for Superscalar Core
// Each entry is a fetch_entry_t struct
// Supports partial enqueue and dequeue
// Robust: Processes all dequeues first, then all enqueues
// ============================================================================

import fetch_types_pkg::*;

module instruction_queue #(
    parameter int ISSUE_WIDTH = 3,           // Number of instructions enqueued/dequeued per cycle
    parameter int IQ_DEPTH    = 16           // Total number of entries in the queue
) (
    input  logic                          clk,
    input  logic                          rst,
    input  logic                          flush,
  
    // Enqueue interface
    input  logic [ISSUE_WIDTH-1:0]        enq_valid,
    input  fetch_entry_t                  enq_data [ISSUE_WIDTH-1:0],
    output logic [ISSUE_WIDTH-1:0]        enq_ready,

    // Dequeue interface
    input  logic [ISSUE_WIDTH-1:0]        deq_ready,
    output logic [ISSUE_WIDTH-1:0]        deq_valid,
    output fetch_entry_t                  deq_data [ISSUE_WIDTH-1:0],

    // Status
    output logic                          full,
    output logic                          empty
);

    // Internal storage
    fetch_entry_t queue [IQ_DEPTH-1:0];
    logic [$clog2(IQ_DEPTH):0] head, tail, count;

    // Sequential logic
    always_ff @(posedge clk or posedge rst or posedge flush) begin
        if (rst || flush) begin
            head  <= 0;
            tail  <= 0;
            count <= 0;
        end else begin
            // Compute next state
            logic [$clog2(IQ_DEPTH):0] next_head, next_tail, next_count;
            next_head  = head;
            next_tail  = tail;
            next_count = count;

            // 1. Process all dequeues first
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                if (deq_ready[i] && (next_count > 0)) begin
                    next_head  = (next_head + 1) % IQ_DEPTH;
                    next_count = next_count - 1;
                end
            end

            // 2. Then process all enqueues
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
               if (enq_valid[i] && (next_count < IQ_DEPTH)) begin
                    queue[next_tail] <= enq_data[i];
                    next_tail        = (next_tail + 1) % IQ_DEPTH;
                    next_count       = next_count + 1;
                end
            end

            head  <= next_head;
            tail  <= next_tail;
            count <= next_count;
        end
    end

    // Combinational logic for outputs
    always_comb begin
        // Default outputs
        full  = (count == IQ_DEPTH);
        empty = (count == 0);

        for (int j = 0; j < ISSUE_WIDTH; j++) begin
            // Dequeue outputs
            if (count > j) begin
                deq_valid[j] = 1'b1;
                deq_data[j]  = queue[(head + j) % IQ_DEPTH];
            end else begin
                deq_valid[j] = 1'b0;
                deq_data[j]  = '{default: 0};
            end

            // Enqueue readiness
            enq_ready[j] = (count + j < IQ_DEPTH);
        end
    end

endmodule

//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Create Date: 06/14/2025 02:20:00 PM
// Module Name: Instruction Memory for RISC-V Tomasulo Core
// Description: Instruction memory with 4-Byte-aligned(Word-aligned) addressing and init_done pulse.
// Takes in pc value from branch predictor unit and outputs the instruction at that pc value
//////////////////////////////////////////////////////////////////////////////////

module instruction_memory #(
    parameter int ISSUE_WIDTH = 3,
    parameter int NO_INSTR = 32,
    parameter int INSTRUCTION_MEMORY_SIZE = 64
) (
    input  logic clk,                             // Clock signal
    input  logic rst,                             // Reset signal (active high)
    input  logic flush,                           // Flush signal (active high)
    input  logic stall_in,                        // Stall signal input
    input  logic [31:0] pc_in,                    // PC input from branch predictor unit
    output fetch_entry_t fetch_reg_out [ISSUE_WIDTH-1:0], // Output: fetched entries
    output logic [31:0] debug_pc,                 // Debug: current program counter
    output logic [31:0] debug_cycle_count,        // Debug: number of elapsed cycles
    output logic init_done,                       // Flag indicating initialization done
    output logic init_done_pulse                  // One-cycle pulse after init_done
);

    // Internal registers
    fetch_entry_t fetch_reg [ISSUE_WIDTH-1:0];   // Internal fetch registers
    logic [7:0] instruction_memory [(INSTRUCTION_MEMORY_SIZE*4)-1:0]; // Byte-addressed instruction memory
    logic [31:0] cycle_count;                     // Cycle counter
    logic fetch_stall;                            // Stall flag
    logic init_done_reg, init_done_q;             // Registers for init_done and its delayed version
    logic [31:0] temp_instr_mem [INSTRUCTION_MEMORY_SIZE-1:0]; // Temporary word-addressed memory
    logic [$clog2(ISSUE_WIDTH):0] valid_instructions_fetched;  // Count of valid instructions fetched
    logic [31:0] current_pc;                      // Temporary PC calculation

    // Assign output signals
    assign fetch_stall = stall_in || (pc_in >= (NO_INSTR * 4));
    assign fetch_reg_out = fetch_reg;
    assign debug_pc = pc_in;
    assign debug_cycle_count = cycle_count;
    assign init_done = init_done_reg;
    assign init_done_pulse = init_done_reg & ~init_done_q; // Pulse when init_done becomes high

    // Sequential logic
    always_ff @(posedge clk or posedge rst or posedge flush) begin
      if (rst || flush) begin
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                fetch_reg[i].valid <= 0;
                fetch_reg[i].instruction <= 32'h0;
                fetch_reg[i].pc <= 32'h0;
            end
            // Initialize temporary instruction memory with hardcoded values
            // Initialize instruction memory contents
            // RISC-V instructions (hex) with brief comments:
            
            temp_instr_mem[0]  = 32'h00100093; // addi x1, x0, 1
            temp_instr_mem[1]  = 32'h00200113; // addi x2, x0, 2
            temp_instr_mem[2]  = 32'h00300193; // addi x3, x0, 3
            temp_instr_mem[3]  = 32'h00102223; // sw x1, 4(x0)
            temp_instr_mem[4]  = 32'h00202423; // sw x2, 8(x0)
            temp_instr_mem[5]  = 32'h00302623; // sw x3, 12(x0)
            temp_instr_mem[6]  = 32'h00302823; // sw x3, 16(x0)
            temp_instr_mem[7]  = 32'h022081B3; // mul x3, x1, x2    (TYPE=2) - MUL!
            temp_instr_mem[8]  = 32'h00310233; // add x4, x2, x3
            temp_instr_mem[9]  = 32'h001183B3; // add x7, x3, x1
            temp_instr_mem[10] = 32'h00940433; // add x8, x8, x9
            temp_instr_mem[11] = 32'h00400083; // lb x1, 4(x0)
            temp_instr_mem[12] = 32'h00804483; // lbu x9, 8(x0) 
            temp_instr_mem[13] = 32'h00C02483; // lw x9, 12(x0) 
            temp_instr_mem[14] = 32'h00A4C7B3; // xor x15, x9, x10
            temp_instr_mem[15] = 32'h00B50533; // add x10, x10, x11
            temp_instr_mem[16] = 32'h009545B3; // add x11, x10, x9
            temp_instr_mem[17] = 32'h00149633; // sll x12, x9, x1
            temp_instr_mem[18] = 32'h0014D6B3; // srl x13, x9, x1
            temp_instr_mem[19] = 32'h40B60533; // sub x10, x12, x11
            temp_instr_mem[20] = 32'h013976B3; // and x13, x18, x19
            temp_instr_mem[21] = 32'h00c52733; // slt x14, x10, x12
            temp_instr_mem[22] = 32'h00A485B3; // add x11, x9, x10
            temp_instr_mem[23] = 32'h02114333; // div x6, x2, x1     (TYPE=2) - DIV!
            temp_instr_mem[24] = 32'h01000903; // lb x18, 16(x0)  // x18 gets mem x16 which is last updated with 3
            temp_instr_mem[25] = 32'h01400983; // lb x19, 20(x0)  // x19 gets mem x20 which is pre-initialized to 5
            temp_instr_mem[26] = 32'h0199AA23; // sw x25, 20(x19) // to Addr 0x19 / 32'd25
            temp_instr_mem[27] = 32'h00A480B3; // add x1, x9, x10
            temp_instr_mem[28] = 32'h00B48133; // add x2, x9, x11
            temp_instr_mem[29] = 32'h002081B3; // add x3, x1, x2
            temp_instr_mem[30] = 32'h02308433; // mul x8, x1, x3     (TYPE=2) - MUL!
            temp_instr_mem[31] = 32'h0020E3B3; // or x7, x1, x2
            temp_instr_mem[32] = 32'h00138EB3; // add x29, x7, x1
            
            // Convert word memory to byte-addressed instruction memory
            for (int j = 0; j < INSTRUCTION_MEMORY_SIZE; j++) begin
                instruction_memory[j*4+0] <= temp_instr_mem[j][7:0];
                instruction_memory[j*4+1] <= temp_instr_mem[j][15:8];
                instruction_memory[j*4+2] <= temp_instr_mem[j][23:16];
                instruction_memory[j*4+3] <= temp_instr_mem[j][31:24];
            end

            // Reset control registers
            cycle_count <= 0;
            init_done_reg <= 0;
            init_done_q <= 0;
        end else begin
            // Register old init_done state for pulse generation
            init_done_q <= init_done_reg;

            // Raise init_done once after reset
            if (!init_done_reg) begin
                init_done_reg <= 1;
            end

            // Main fetch logic (active after init_done)
            if (!fetch_stall && init_done_reg) begin
                valid_instructions_fetched = 0;
                for (int i = 0; i < ISSUE_WIDTH; i++) begin
                    current_pc = pc_in + (i * 4);
                    if (current_pc < (NO_INSTR * 4)) begin
                        fetch_reg[i].instruction <= {instruction_memory[current_pc+3], instruction_memory[current_pc+2], instruction_memory[current_pc+1], instruction_memory[current_pc]};
                        fetch_reg[i].pc <= current_pc;
                        fetch_reg[i].valid <= 1'b1;
                        valid_instructions_fetched++;
                    end else begin
                        fetch_reg[i].instruction <= 32'h0;
                        fetch_reg[i].pc <= 32'h0;
                        fetch_reg[i].valid <= 1'b0;
                    end
                end
                // Do not increment pc here, as pc_in is now externally provided
                cycle_count <= cycle_count + 1;
            end else begin
                cycle_count <= cycle_count + 1; // Increment cycle even if stalled
            end
        end
    end

endmodule

