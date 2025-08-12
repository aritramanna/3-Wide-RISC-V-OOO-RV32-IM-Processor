// ============================================================================
// RISC-V RV32I-Compliant Data Memory
// - Fully synthesizable
// - Supports configurable load and store ports for 3-wide Tomasulo
// - Supports byte, halfword, and word loads/stores
// - Handles sign/zero extension for loads
// - Byte-enable logic for stores
// ============================================================================

module data_mem #(
    parameter MEM_DEPTH = 1024,
    parameter DATA_WIDTH = 32,
    parameter NUM_LD_PORTS = 2,
    parameter NUM_ST_PORTS = 1
) (
    input  logic        clk,
    input  logic        rst,
    // Write ports (stores) - configurable number of ports
    input  logic [NUM_ST_PORTS-1:0] write_en,
    input  logic [31:0] write_addr [NUM_ST_PORTS-1:0],
    input  logic [DATA_WIDTH-1:0] write_data [NUM_ST_PORTS-1:0],
    input  logic [1:0]  write_size [NUM_ST_PORTS-1:0],   // 2'b00=byte, 2'b01=half, 2'b10=word
    // Read ports (loads) - configurable number of ports
    input  logic [NUM_LD_PORTS-1:0] read_en,      // configurable load ports
    input  logic [31:0] read_addr [NUM_LD_PORTS-1:0],
    input  logic [1:0]  read_size [NUM_LD_PORTS-1:0],    // 2'b00=byte, 2'b01=half, 2'b10=word
    input  logic [NUM_LD_PORTS-1:0] read_unsigned, // 1=unsigned (LBU/LHU), 0=signed (LB/LH)
    output logic [DATA_WIDTH-1:0] read_data [NUM_LD_PORTS-1:0]
);
    // Memory array (configurable data width)
    logic [DATA_WIDTH-1:0] mem [0:MEM_DEPTH-1];
    
    // Internal signals for write logic (configurable ports)
    logic [$clog2(MEM_DEPTH)-1:0] write_word_idx [NUM_ST_PORTS-1:0];
    logic [1:0] write_byte_off [NUM_ST_PORTS-1:0];
    logic [DATA_WIDTH-1:0] write_word_data [NUM_ST_PORTS-1:0];
    logic [DATA_WIDTH-1:0] write_modified_data [NUM_ST_PORTS-1:0];
    
    // Internal signals for read logic (configurable ports)
    logic [$clog2(MEM_DEPTH)-1:0] read_word_idx [NUM_LD_PORTS-1:0];
    logic [1:0] read_byte_off [NUM_LD_PORTS-1:0];
    logic [DATA_WIDTH-1:0] read_word_data [NUM_LD_PORTS-1:0];
    logic [7:0] read_byte_data [NUM_LD_PORTS-1:0];
    logic [15:0] read_half_data [NUM_LD_PORTS-1:0];
    
    // Loop variables
    integer i, j, k;

    // Combinational logic for write address calculation and data manipulation
    always_comb begin
        for (j = 0; j < NUM_ST_PORTS; j++) begin
            write_word_idx[j] = write_addr[j][31:2] % MEM_DEPTH;
            write_byte_off[j] = write_addr[j][1:0];
            write_word_data[j] = mem[write_word_idx[j]];
            write_modified_data[j] = write_word_data[j];
            
            if (write_en[j]) begin
                case (write_size[j])
                    2'b00: begin // SB
                        case (write_byte_off[j])
                            2'b00: write_modified_data[j][7:0]   = write_data[j][7:0];
                            2'b01: write_modified_data[j][15:8]  = write_data[j][7:0];
                            2'b10: write_modified_data[j][23:16] = write_data[j][7:0];
                            2'b11: write_modified_data[j][31:24] = write_data[j][7:0];
                        endcase
                    end
                    2'b01: begin // SH
                        // Only allow aligned halfword stores (write_byte_off[j][0] == 0)
                        if (write_byte_off[j][0] == 1'b0) begin
                            if (write_byte_off[j][1] == 1'b0)
                                write_modified_data[j][15:0] = write_data[j][15:0];
                            else
                                write_modified_data[j][31:16] = write_data[j][15:0];
                        end
                    end
                    2'b10: begin // SW
                        // Only allow aligned word stores (write_byte_off[j] == 2'b00)
                        if (write_byte_off[j] == 2'b00)
                            write_modified_data[j] = write_data[j];
                    end
                    default: ;
                endcase
            end
        end
    end

    // Sequential logic for memory write and reset
    always_ff @(posedge clk) begin
        if (rst) begin
            for (integer m = 0; m < MEM_DEPTH; m++) begin
                //mem[m] <= {DATA_WIDTH{1'b0}};
                mem[m] <= m;
            end
        end else begin
            for (k = 0; k < NUM_ST_PORTS; k++) begin
                if (write_en[k]) begin
                    mem[write_word_idx[k]] <= write_modified_data[k];
                    //$display("[DATA_MEM][WRITE] port=%0d addr=0x%08x data=0x%08x size=%0d", k, write_addr[k], write_data[k], write_size[k]);
                end
            end
        end
    end

    // Combinational logic for read address calculation and data manipulation
    always_comb begin
        for (i = 0; i < NUM_LD_PORTS; i++) begin
            read_word_idx[i] = read_addr[i][31:2] % MEM_DEPTH;
            read_byte_off[i] = read_addr[i][1:0];
            read_word_data[i] = mem[read_word_idx[i]];
            
            if (read_en[i]) begin
                //$display("[DATA_MEM][READ]  port=%0d addr=0x%08x size=%0d", i, read_addr[i], read_size[i]);
                case (read_size[i])
                    2'b00: begin // LB/LBU
                        case (read_byte_off[i])
                            2'b00: read_byte_data[i] = read_word_data[i][7:0];
                            2'b01: read_byte_data[i] = read_word_data[i][15:8];
                            2'b10: read_byte_data[i] = read_word_data[i][23:16];
                            2'b11: read_byte_data[i] = read_word_data[i][31:24];
                        endcase
                        read_data[i] = read_unsigned[i] ? {{(DATA_WIDTH-8){1'b0}}, read_byte_data[i]} : {{(DATA_WIDTH-8){read_byte_data[i][7]}}, read_byte_data[i]};
                    end
                    2'b01: begin // LH/LHU
                        // Only allow aligned halfword loads (read_byte_off[i][0] == 0)
                        if (read_byte_off[i][0] == 1'b0) begin
                            if (read_byte_off[i][1] == 1'b0)
                                read_half_data[i] = read_word_data[i][15:0];
                            else
                                read_half_data[i] = read_word_data[i][31:16];
                            read_data[i] = read_unsigned[i] ? {{(DATA_WIDTH-16){1'b0}}, read_half_data[i]} : {{(DATA_WIDTH-16){read_half_data[i][15]}}, read_half_data[i]};
                        end else begin
                            read_data[i] = {DATA_WIDTH{1'b1}}; // Invalid/unaligned
                        end
                    end
                    2'b10: begin // LW
                        // Only allow aligned word loads (read_byte_off[i] == 2'b00)
                        if (read_byte_off[i] == 2'b00)
                            read_data[i] = read_word_data[i];
                        else
                            read_data[i] = {DATA_WIDTH{1'b1}}; // Invalid/unaligned
                    end
                    default: read_data[i] = {DATA_WIDTH{1'b1}};
                endcase
            end else begin
                read_data[i] = {DATA_WIDTH{1'b1}};
            end
        end
    end
endmodule