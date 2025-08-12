// ============================================================================
// - All design modules included below for 3-wide branch prediction unit
// ============================================================================

// -------------------- BTB Predictor --------------------
// ============================================================================
// Branch Target Buffer (BTB) Predictor Module
// - 4-way set-associative, 3-ported BTB for 3-wide fetch pipelines
// - Implements tree-based Pseudo-LRU (PLRU) replacement policy per set
// - Designed for high-performance, out-of-order, superscalar processors
// ============================================================================

module btb_predictor #(
    parameter XLEN = 32,            // Width of program counter and target addresses
    parameter BTB_SETS = 64,        // Number of sets in the BTB
    parameter BTB_WAYS = 4,         // Number of ways (associativity) per set
    parameter FETCH_WIDTH = 3       // Number of parallel fetch lookups (e.g., 3 for 3-wide fetch)
)(
    input  logic clk,               // Clock signal
    input  logic rst,               // Asynchronous reset

    // Fetch interface: parallel lookup for each fetch slot
    input  logic [XLEN-1:0] fetch_pc [FETCH_WIDTH],   // Array of PCs to lookup in the BTB
    output logic [XLEN-1:0] btb_target [FETCH_WIDTH], // Predicted target address for each fetch slot
    output logic            btb_hit [FETCH_WIDTH],    // Indicates if BTB hit for each fetch slot

    // Update interface: single update per cycle
    input  logic             update_en,              // Enable signal for BTB update
    input  logic [XLEN-1:0]  update_pc,              // PC to update in the BTB
    input  logic [XLEN-1:0]  update_target           // Target address to update in the BTB
);

    localparam SET_IDX_BITS = $clog2(BTB_SETS);      // Number of bits for set index
    localparam TAG_BITS = XLEN - SET_IDX_BITS - 2;   // Number of bits for tag (excluding set index and alignment bits)

    typedef struct packed {
        logic                valid;                  // Valid bit for entry
        logic [TAG_BITS-1:0] tag;                    // Tag bits for address matching
        logic [XLEN-1:0]     target;                 // Predicted target address
    } btb_entry_t;

    btb_entry_t btb_table [BTB_SETS][BTB_WAYS];      // Main BTB storage: sets x ways
    logic [2:0] plru [BTB_SETS];                     // 3-bit PLRU state per set (for 4-way associativity)

    function [2:0] plru_update(input [2:0] old, input logic [1:0] way);
        plru_update = old;
        case (way)
            2'd0: plru_update = {old[2], old[1], 1'b0}; // left-left
            2'd1: plru_update = {old[2], old[1], 1'b1}; // left-right
            2'd2: plru_update = {old[2], 1'b0, old[0]}; // right-left
            2'd3: plru_update = {old[2], 1'b1, old[0]}; // right-right
        endcase
    endfunction

    function logic [1:0] plru_victim(input [2:0] state);
        if (!state[2]) begin // left subtree
            if (!state[1])
                plru_victim = 2'd0; // left-left
            else
                plru_victim = 2'd1; // left-right
        end else begin // right subtree
            if (!state[0])
                plru_victim = 2'd2; // right-left
            else
                plru_victim = 2'd3; // right-right
        end
    endfunction

    genvar i;
    generate
        for (i = 0; i < FETCH_WIDTH; i++) begin : BTB_LOOKUP
            logic [SET_IDX_BITS-1:0] set_idx;
            logic [TAG_BITS-1:0]     tag;
            logic hit;
            logic [XLEN-1:0] target;
            integer way;
    always_comb begin
                set_idx = fetch_pc[i][SET_IDX_BITS+1:2];
                tag     = fetch_pc[i][XLEN-1:SET_IDX_BITS+2];
                hit = 0;
                target = '0;
                for (way = 0; way < BTB_WAYS; way = way + 1) begin
                    if (btb_table[set_idx][way].valid && btb_table[set_idx][way].tag == tag) begin
                        hit = 1;
                        target = btb_table[set_idx][way].target;
                    end
                end
                btb_hit[i] = hit;
                btb_target[i] = target;
    end
        end
    endgenerate

    always_ff @(posedge clk or posedge rst) begin
        integer s, w, i;
        if (rst) begin
            for (s = 0; s < BTB_SETS; s = s + 1) begin
                for (w = 0; w < BTB_WAYS; w = w + 1) begin
                    btb_table[s][w].valid <= 0;
                    btb_table[s][w].tag   <= '0;
                    btb_table[s][w].target<= '0;
                end
                plru[s] <= 3'b0;
            end
        end else begin
            for (i = 0; i < FETCH_WIDTH; i = i + 1) begin
                logic [SET_IDX_BITS-1:0] set_idx;
                logic [TAG_BITS-1:0]     tag;
                set_idx = fetch_pc[i][SET_IDX_BITS+1:2];
                tag     = fetch_pc[i][XLEN-1:SET_IDX_BITS+2];
                for (w = 0; w < BTB_WAYS; w = w + 1) begin
                    if (btb_table[set_idx][w].valid && btb_table[set_idx][w].tag == tag) begin
                        plru[set_idx] <= plru_update(plru[set_idx], w[1:0]);
                    end
                end
            end
            if (update_en) begin
                logic [SET_IDX_BITS-1:0] set_idx;
                logic [TAG_BITS-1:0]     tag;
                logic [1:0] way_to_use;
                logic [1:0] free_way;
                set_idx = update_pc[SET_IDX_BITS+1:2];
                tag     = update_pc[XLEN-1:SET_IDX_BITS+2];
                free_way = 2'b11;
                for (w = 0; w < BTB_WAYS; w = w + 1) begin
                    if (!btb_table[set_idx][w].valid && free_way == 2'b11)
                        free_way = w[1:0];
                end
                if (free_way != 2'b11) begin
                    way_to_use = free_way;
                end else begin
                    way_to_use = plru_victim(plru[set_idx]);
                end
                btb_table[set_idx][way_to_use].valid  <= 1;
                btb_table[set_idx][way_to_use].tag    <= tag;
                btb_table[set_idx][way_to_use].target <= update_target;
                plru[set_idx] <= plru_update(plru[set_idx], way_to_use);
            end
        end
    end

endmodule

// -------------------- Gshare Predictor --------------------
// ============================================================================
// Gshare Branch Predictor
// - 1024-entry 2-bit counter table
// - 10-bit Global History Register (GHR)
// - Index = PC[11:2] XOR GHR
// - Modified for 3-wide parallel prediction
// ============================================================================

module gshare_predictor #(
    parameter TABLE_BITS = 10,  // log2(# of entries), e.g., 1024 entries
    parameter FETCH_WIDTH = 3   // Number of parallel predictions (3-wide)
) (
    input  logic        clk,
    input  logic        rst,
    input  logic [31:0] fetch_pc [FETCH_WIDTH],
    output logic        fetch_pred [FETCH_WIDTH],
    input  logic        update_en,
    input  logic [31:0] update_pc,
    input  logic        update_taken
);

    localparam TABLE_SIZE = 1 << TABLE_BITS;
    localparam GHR_BITS   = TABLE_BITS;

    reg  [1:0] bpred_table [0:TABLE_SIZE-1];
    logic [GHR_BITS-1:0] ghr;
    logic [TABLE_BITS-1:0] fetch_idx [FETCH_WIDTH];

    function logic [TABLE_BITS-1:0] get_index(input logic [31:0] pc, input logic [GHR_BITS-1:0] hist);
        get_index = pc[11:2] ^ hist;
    endfunction

    function logic [1:0] update_counter(input logic [1:0] val, input logic taken);
        case ({val, taken})
            3'b00_0: update_counter = 2'b00;
            3'b00_1: update_counter = 2'b01;
            3'b01_0: update_counter = 2'b00;
            3'b01_1: update_counter = 2'b10;
            3'b10_0: update_counter = 2'b01;
            3'b10_1: update_counter = 2'b11;
            3'b11_0: update_counter = 2'b10;
            3'b11_1: update_counter = 2'b11;
            default: update_counter = val;
        endcase
    endfunction

    genvar i;
    generate
        for (i = 0; i < FETCH_WIDTH; i++) begin : GSHARE_PREDICT
    always_comb begin
                fetch_idx[i]  = get_index(fetch_pc[i], ghr);
                fetch_pred[i] = bpred_table[fetch_idx[i]][1];
            end
    end
    endgenerate

    always_ff @(posedge clk or posedge rst) begin
        integer i;
        if (rst) begin
            ghr <= '0;
            for (i = 0; i < TABLE_SIZE; i = i + 1) begin
                bpred_table[i] <= 2'b01;
            end
        end else if (update_en) begin
            logic [TABLE_BITS-1:0] upd_idx;
            upd_idx = get_index(update_pc, ghr);
            bpred_table[upd_idx] <= update_counter(bpred_table[upd_idx], update_taken);
            ghr <= {ghr[GHR_BITS-2:0], update_taken};
        end
    end

endmodule

// -------------------- Return Address Stack --------------------
// ============================================================================
// Multi-Ported Return Address Stack (RAS)
// - Supports parallel pushes and provides parallel pop predictions
// - Designed for 3-wide superscalar, out-of-order processors
// - Parameterizable for stack depth and fetch width
// ============================================================================

module return_address_stack #(
    parameter int RAS_DEPTH = 16,
    parameter int XLEN = 32,
    parameter int FETCH_WIDTH = 3
)(
    input  logic clk,
    input  logic rst,
    input  logic push_valid [FETCH_WIDTH],
    input  logic [XLEN-1:0] push_addr [FETCH_WIDTH],
    input  logic pop_valid [FETCH_WIDTH],
    output logic [XLEN-1:0] pop_addr [FETCH_WIDTH],
    output logic pop_ready [FETCH_WIDTH],
    input  logic flush,
    output logic [$clog2(RAS_DEPTH):0] ras_sp_out
);

    logic [XLEN-1:0] stack [RAS_DEPTH-1:0];
    logic [$clog2(RAS_DEPTH):0] sp;
    logic [$clog2(RAS_DEPTH):0] sp_after_pops;

    logic wr_en [FETCH_WIDTH];
    logic [$clog2(RAS_DEPTH):0] wr_ptr [FETCH_WIDTH];
    int num_pops;
    int next_ptr;

    // POP: port 0 gets top, then 1, 2...
    genvar i;
    generate
        for (i = 0; i < FETCH_WIDTH; i++) begin : GEN_POP
            assign pop_ready[i] = (sp > i);
            assign pop_addr[i]  = (sp > i) ? stack[sp - 1 - i] : '0;
        end
    endgenerate

    always_ff @(posedge clk or posedge rst) begin
        if (rst || flush) begin
            sp <= 0;
            for (int idx = 0; idx < RAS_DEPTH; idx++) begin
                stack[idx] <= '0;
            end
        end else begin
            // Count pops
            num_pops = 0;
            for (int i = 0; i < FETCH_WIDTH; i++) begin
                if (pop_valid[i]) num_pops++;
            end

            // Compute SP after pops
            sp_after_pops = (num_pops > sp) ? 0 : (sp - num_pops);

            // Assign write pointers in port index order
            next_ptr = sp_after_pops;
            for (int i = 0; i < FETCH_WIDTH; i++) begin
                if (push_valid[i] && next_ptr < RAS_DEPTH) begin
                    wr_en[i] = 1;
                    wr_ptr[i] = next_ptr;
                    next_ptr++;
                end else begin
                    wr_en[i] = 0;
                    wr_ptr[i] = 'x;
                end
            end

            // Perform writes
            for (int i = 0; i < FETCH_WIDTH; i++) begin
                if (wr_en[i]) begin
                    stack[wr_ptr[i]] <= push_addr[i];
                end
            end

            // Update SP
            sp <= next_ptr;
        end
    end

    assign ras_sp_out = sp;

endmodule

// -------------------- Branch Prediction Queue --------------------
// ============================================================================
// Branch Prediction Queue (BPQ) for 3-wide OOO Core
// - Tracks branch predictions as they move through the pipeline
// - Supports up to 3 enqueues per cycle (from fetch)
// - Supports 1 dequeue per cycle (at retire/commit)
// - Parameterizable for queue depth and fetch width
// ============================================================================

module branch_prediction_queue #(
    parameter XLEN = 32,
    parameter FETCH_WIDTH = 3,
    parameter BPQ_DEPTH = 16
)(
    input  logic clk,
    input  logic rst,
    input  logic enq_valid [FETCH_WIDTH],
    input  logic [XLEN-1:0]        enq_pc    [FETCH_WIDTH],
    input  logic [XLEN-1:0]        enq_target[FETCH_WIDTH],
    input  logic enq_taken [FETCH_WIDTH],
    output logic                   deq_valid,
    output logic [XLEN-1:0]        deq_pc,
    output logic [XLEN-1:0]        deq_target,
    output logic                   deq_taken,
    input  logic                   deq_ready,
    input  logic                   flush,
    output logic [$clog2(BPQ_DEPTH):0] bpq_count_out
);

    typedef struct packed {
        logic        valid;
        logic [XLEN-1:0] pc;
        logic [XLEN-1:0] target;
        logic        taken;
    } bpq_entry_t;

    bpq_entry_t queue [BPQ_DEPTH];
    logic [$clog2(BPQ_DEPTH):0] head, tail;
    logic [$clog2(BPQ_DEPTH):0] count;
    logic [$clog2(BPQ_DEPTH):0] bpq_count_out_wire;

    integer i;
    always_ff @(posedge clk or posedge rst) begin
        if (rst || flush) begin
            head <= 0;
            tail <= 0;
            count <= 0;
            for (i = 0; i < BPQ_DEPTH; i = i + 1) begin
                queue[i].valid <= 0;
                queue[i].pc    <= '0;
                queue[i].target<= '0;
                queue[i].taken <= 0;
            end
        end else begin
            for (i = 0; i < FETCH_WIDTH; i = i + 1) begin
                if (enq_valid[i] && count < BPQ_DEPTH) begin
                    queue[tail].valid  <= 1;
                    queue[tail].pc     <= enq_pc[i];
                    queue[tail].target <= enq_target[i];
                    queue[tail].taken  <= enq_taken[i];
                    tail <= (tail + 1) % BPQ_DEPTH;
                    count <= count + 1;
                end
            end
            if (deq_ready && count > 0) begin
                queue[head].valid <= 0;
                head <= (head + 1) % BPQ_DEPTH;
                count <= count - 1;
            end
        end
    end

    assign deq_valid  = (count > 0) && queue[head].valid;
    assign deq_pc     = queue[head].pc;
    assign deq_target = queue[head].target;
    assign deq_taken  = queue[head].taken;
    assign bpq_count_out_wire = count;
    assign bpq_count_out = bpq_count_out_wire;

endmodule 

// -------------------- Branch Predictor Unit (Top Level) --------------------
// ============================================================================
// 3-Wide Superscalar Branch Prediction Unit
// - Integrates a 3-ported BTB, Gshare, and RAS with a Branch Prediction Queue
// - Predicts the next fetch PC based on up to 3 parallel predictions
// - Tracks in-flight predictions for OOO recovery
// ============================================================================
module branch_predictor_unit #(
    parameter XLEN = 32,
    parameter BTB_SETS = 64,
    parameter BTB_WAYS = 4,
    parameter GSHARE_TABLE_BITS = 10,
    parameter RAS_DEPTH = 16,
    parameter BPQ_DEPTH = 16,
    parameter FETCH_WIDTH = 3
)(
    input  logic clk,
    input  logic rst,
    input  logic [XLEN-1:0]    fetch_pc [FETCH_WIDTH],
    input  logic               is_call_instr [FETCH_WIDTH],
    input  logic               is_ret_instr [FETCH_WIDTH],
    input  logic               is_branch_instr [FETCH_WIDTH],
    output logic [XLEN-1:0]    predicted_pc,
    output logic [FETCH_WIDTH-1:0] predict_valid,
    input  logic               update_en,
    input  logic [XLEN-1:0]    update_pc,
    input  logic               update_taken,
    input  logic [XLEN-1:0]    update_target,
    input  logic               flush,
    output logic [$clog2(RAS_DEPTH):0] ras_sp_out,
    output logic [$clog2(BPQ_DEPTH):0] bpq_count_out,
    // Testbench-only outputs for predictor state
    output logic [FETCH_WIDTH-1:0] tb_btb_hit,
    output logic [XLEN-1:0] tb_btb_target [FETCH_WIDTH],
    output logic [FETCH_WIDTH-1:0] tb_gshare_pred,
    output logic [FETCH_WIDTH-1:0] tb_ras_pop_ready,
    output logic [XLEN-1:0] tb_ras_pop_addr [FETCH_WIDTH]
);

    logic [XLEN-1:0] btb_target [FETCH_WIDTH];
    logic            btb_hit [FETCH_WIDTH];
    logic            gshare_pred [FETCH_WIDTH];
    logic [XLEN-1:0] ras_pop_addr [FETCH_WIDTH];
    logic            ras_pop_ready [FETCH_WIDTH];
    logic bpq_enq_valid [FETCH_WIDTH];
    logic [XLEN-1:0]        bpq_enq_pc [FETCH_WIDTH];
    logic [XLEN-1:0]        bpq_enq_target [FETCH_WIDTH];
    logic bpq_enq_taken [FETCH_WIDTH];
    logic                   bpq_deq_valid;
    logic [XLEN-1:0]        bpq_deq_pc;
    logic [XLEN-1:0]        bpq_deq_target;
    logic                   bpq_deq_taken;
    logic [XLEN-1:0]        ras_push_addr [FETCH_WIDTH];
    logic [$clog2(RAS_DEPTH):0] ras_sp_out_wire;
    logic [$clog2(BPQ_DEPTH):0] bpq_count_out_wire;

    // Change internal signals to packed arrays for testbench outputs
    logic [FETCH_WIDTH-1:0] btb_hit_packed;
    logic [FETCH_WIDTH-1:0] gshare_pred_packed;
    logic [FETCH_WIDTH-1:0] ras_pop_ready_packed;

    // Flush-related signals for assertions
    logic ras_sp_prev;
    logic bpq_count_prev;
    logic flush_prev;

    btb_predictor #(
        .XLEN(XLEN), .BTB_SETS(BTB_SETS), .BTB_WAYS(BTB_WAYS), .FETCH_WIDTH(FETCH_WIDTH)
    ) btb (
        .clk(clk), .rst(rst),
        .fetch_pc(fetch_pc),
        .btb_target(btb_target),
        .btb_hit(btb_hit),
        .update_en(update_en), .update_pc(update_pc), .update_target(update_target)
    );

    gshare_predictor #(
        .TABLE_BITS(GSHARE_TABLE_BITS), .FETCH_WIDTH(FETCH_WIDTH)
    ) gshare (
        .clk(clk), .rst(rst),
        .fetch_pc(fetch_pc),
        .fetch_pred(gshare_pred),
        .update_en(update_en), .update_pc(update_pc), .update_taken(update_taken)
    );

    genvar k;
    generate
        for(k=0; k<FETCH_WIDTH; k=k+1) begin
            assign ras_push_addr[k] = fetch_pc[k] + 4;
        end
    endgenerate

    return_address_stack #(
        .RAS_DEPTH(RAS_DEPTH), .XLEN(XLEN), .FETCH_WIDTH(FETCH_WIDTH)
    ) ras (
        .clk(clk), .rst(rst), .flush(flush),
        .push_valid(is_call_instr),
        .push_addr(ras_push_addr),
        .pop_valid(is_ret_instr),
        .pop_addr(ras_pop_addr),
        .pop_ready(ras_pop_ready),
        .ras_sp_out(ras_sp_out_wire)
    );

    branch_prediction_queue #(
        .XLEN(XLEN), .FETCH_WIDTH(FETCH_WIDTH), .BPQ_DEPTH(BPQ_DEPTH)
    ) bpq (
        .clk(clk), .rst(rst), .flush(flush),
        .enq_valid(bpq_enq_valid),
        .enq_pc(bpq_enq_pc),
        .enq_target(bpq_enq_target),
        .enq_taken(bpq_enq_taken),
        .deq_valid(bpq_deq_valid),
        .deq_pc(bpq_deq_pc),
        .deq_target(bpq_deq_target),
        .deq_taken(bpq_deq_taken),
        .deq_ready(update_en),
        .bpq_count_out(bpq_count_out_wire)
    );

    always_comb begin
        logic slot_taken [FETCH_WIDTH];
        logic [XLEN-1:0] slot_target [FETCH_WIDTH];
        for (integer i = 0; i < FETCH_WIDTH; i++) begin
            if (is_ret_instr[i] && ras_pop_ready[i]) begin
                slot_taken[i] = 1;
                slot_target[i] = ras_pop_addr[i];
            end else if (is_call_instr[i] && btb_hit[i]) begin
                slot_taken[i] = 1;
                slot_target[i] = btb_target[i];
            end else if (is_branch_instr[i] && btb_hit[i]) begin
                slot_taken[i] = gshare_pred[i];
                slot_target[i] = btb_target[i];
            end else begin
                slot_taken[i] = 0;
                slot_target[i] = fetch_pc[i] + 4;
            end
        end
        predict_valid = 0;
        if (slot_taken[0]) begin
            predicted_pc = slot_target[0];
        end else if (slot_taken[1]) begin
            predicted_pc = slot_target[1];
        end else if (slot_taken[2]) begin
            predicted_pc = slot_target[2];
        end else begin
            predicted_pc = fetch_pc[0] + (FETCH_WIDTH * 4);
        end
        // Set predict_valid for each lane: calls with BTB hit, branches with BTB hit, and returns
        for (integer i = 0; i < FETCH_WIDTH; i++) begin
            if ((is_call_instr[i] && btb_hit[i]) || (is_branch_instr[i] && btb_hit[i]) || is_ret_instr[i]) begin
                predict_valid[i] = 1;
                bpq_enq_valid[i]  = 1;
                bpq_enq_pc[i]     = fetch_pc[i];
                bpq_enq_target[i] = slot_target[i];
                bpq_enq_taken[i]  = slot_taken[i];
            end else begin
                predict_valid[i] = 0;
                bpq_enq_valid[i]  = 0;
                bpq_enq_pc[i]     = '0;
                bpq_enq_target[i] = '0;
                bpq_enq_taken[i]  = 0;
            end
        end
        // Pack the arrays for testbench outputs
        for (int i = 0; i < FETCH_WIDTH; i++) begin
            btb_hit_packed[i] = btb_hit[i];
            gshare_pred_packed[i] = gshare_pred[i];
            ras_pop_ready_packed[i] = ras_pop_ready[i];
        end
    end

    assign ras_sp_out = ras_sp_out_wire;
    assign bpq_count_out = bpq_count_out_wire;
    // Testbench-only assignments
    assign tb_btb_hit = btb_hit_packed;
    assign tb_btb_target = btb_target;
    assign tb_gshare_pred = gshare_pred_packed;
    assign tb_ras_pop_ready = ras_pop_ready_packed;
    assign tb_ras_pop_addr = ras_pop_addr;

endmodule 