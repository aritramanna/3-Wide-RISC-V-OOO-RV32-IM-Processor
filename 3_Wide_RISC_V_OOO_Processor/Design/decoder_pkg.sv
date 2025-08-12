`ifndef DECODER_PKG_SV
`define DECODER_PKG_SV

`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Engineer: Aritra Manna
// Create Date: 06/14/2025 02:20:00 PM
// Package Name: Decoder Package
// Description: Package containing decoder structures, functions, and constants
// Version : 1.00 - Initial Package
//////////////////////////////////////////////////////////////////////////////////

package decoder_pkg;

    // ============================================================================
    // STRUCTURE DEFINITIONS
    // ============================================================================
    
    // Structure for a single entry in the decode pipeline register.
    // Contains all decoded fields needed for instruction execution and dependency tracking.
    typedef struct packed {
        logic [31:0] instruction; // Raw instruction word
        logic [31:0] pc;          // Program counter of this instruction
        logic [6:0]  opcode;      // Decoded opcode field
        logic [2:0]  func3;       // Decoded func3 field
        logic [6:0]  func7;       // Decoded func7 field
        logic [4:0]  src1;        // Source register 1 index
        logic [4:0]  src2;        // Source register 2 index
        logic [4:0]  dst;         // Destination register index
        logic [31:0] immediate;   // Decoded immediate value (I/S-type)
        logic [31:0] offset;      // Address offset for loads/stores
        logic [16:0] operation;   // Unique operation identifier (func7, func3, opcode)
        logic        valid;       // 1 if this entry holds a valid decoded instruction
        logic [1:0]  instr_type;  // Instruction type: 00=ALU, 01=Load/Store, 10=Mul/Div, 11=Other
    } decode_entry_t;

    // ============================================================================
    // CONSTANTS
    // ============================================================================
    
    // Instruction type constants
    localparam INSTR_TYPE_ALU     = 2'b00;
    localparam INSTR_TYPE_MEM     = 2'b01;
    localparam INSTR_TYPE_MULDIV  = 2'b10;
    localparam INSTR_TYPE_OTHER   = 2'b11;
    
    // Opcode constants
    localparam OPCODE_LOAD        = 7'b0000011;
    localparam OPCODE_STORE       = 7'b0100011;
    localparam OPCODE_ALU_IMM     = 7'b0010011;
    localparam OPCODE_ALU_REG     = 7'b0110011;
    
    // Function3 constants for ALU operations
    localparam FUNC3_ADD          = 3'b000;
    localparam FUNC3_SLL          = 3'b001;
    localparam FUNC3_SLT          = 3'b010;
    localparam FUNC3_SLTU         = 3'b011;
    localparam FUNC3_XOR          = 3'b100;
    localparam FUNC3_SRL          = 3'b101;
    localparam FUNC3_OR           = 3'b110;
    localparam FUNC3_AND          = 3'b111;
    
    // Function7 constants
    localparam FUNC7_ADD          = 7'b0000000;
    localparam FUNC7_SUB          = 7'b0100000;
    localparam FUNC7_SRA          = 7'b0100000;
    localparam FUNC7_MUL          = 7'b0000001;
    localparam FUNC7_DIV          = 7'b0000001;
    
    // 17-bit operation constants for pipeline matching
    localparam OP_ADD   = 17'b0000000_000_0110011; // add
    localparam OP_SUB   = 17'b0100000_000_0110011; // subtract
    localparam OP_AND   = 17'b0000000_111_0110011; // and
    localparam OP_OR    = 17'b0000000_110_0110011; // or
    localparam OP_XOR   = 17'b0000000_100_0110011; // xor
    localparam OP_SLL   = 17'b0000000_001_0110011; // shift left logical
    localparam OP_SRL   = 17'b0000000_101_0110011; // shift right logical
    localparam OP_SRA   = 17'b0100000_101_0110011; // shift right arithmetic
    localparam OP_SLT   = 17'b0000000_010_0110011; // set less than
    localparam OP_SLTU  = 17'b0000000_011_0110011; // set less than unsigned
    localparam OP_MUL   = 17'b0000001_000_0110011; // multiply
    localparam OP_DIV   = 17'b0000001_100_0110011; // divide
    localparam OP_ADDI  = 17'b0000000_000_0010011; // add immediate
    localparam OP_LB    = 17'b0000000_000_0000011; // load byte
    localparam OP_LH    = 17'b0000000_001_0000011; // load halfword
    localparam OP_LW    = 17'b0000000_010_0000011; // load word
    localparam OP_LBU   = 17'b0000000_100_0000011; // load byte unsigned
    localparam OP_LHU   = 17'b0000000_101_0000011; // load halfword unsigned
    localparam OP_SB    = 17'b0000000_000_0100011; // store byte
    localparam OP_SH    = 17'b0000000_001_0100011; // store halfword
    localparam OP_SW    = 17'b0000000_010_0100011; // store word
    localparam OP_JAL   = 17'b0000000_000_1101111; // jump and link
    localparam OP_JALR  = 17'b0000000_000_1100111; // jump and link register
    localparam OP_BEQ   = 17'b0000000_000_1100011; // branch equal
    localparam OP_BNE   = 17'b0000000_001_1100011; // branch not equal
    localparam OP_BLT   = 17'b0000000_100_1100011; // branch less than
    localparam OP_BGE   = 17'b0000000_101_1100011; // branch greater or equal
    localparam OP_BLTU  = 17'b0000000_110_1100011; // branch less than unsigned
    localparam OP_BGEU  = 17'b0000000_111_1100011; // branch greater or equal unsigned

    // ============================================================================
    // FUNCTION DEFINITIONS
    // ============================================================================
    
    // Function for instruction type determination
    function automatic [1:0] get_instr_type;
        input [6:0] opcode;
        input [2:0] func3;
        input [6:0] func7;
        begin
            case(opcode)
                OPCODE_LOAD, OPCODE_STORE: get_instr_type = INSTR_TYPE_MEM;
                OPCODE_ALU_REG: begin
                    case({func7, func3})
                        10'b0000001_000, 10'b0000001_100: get_instr_type = INSTR_TYPE_MULDIV; // MUL, DIV
                        default: get_instr_type = INSTR_TYPE_ALU;
                    endcase
                end
                OPCODE_ALU_IMM: get_instr_type = INSTR_TYPE_ALU;
                default: get_instr_type = INSTR_TYPE_OTHER;
            endcase
        end
    endfunction

    // Function for immediate value decoding
    function automatic [31:0] decode_immediate;
        input [31:0] instr;
        input [6:0] opcode;
        begin
            case(opcode)
                OPCODE_LOAD, OPCODE_ALU_IMM: // I-type (load, addi)
                    decode_immediate = {{20{instr[31]}}, instr[31:20]};
                OPCODE_STORE: // S-type (store)
                    decode_immediate = {{20{instr[31]}}, instr[31:25], instr[11:7]};
                default:
                    decode_immediate = 32'h0;
            endcase
        end
    endfunction

    // Function to check if instruction is valid/decodable
    function automatic logic is_valid_instruction;
        input [31:0] instruction;
        begin
            logic [6:0] opcode;
            logic [2:0] func3;
            logic [6:0] func7;
            
            opcode = instruction[6:0];
            func3 = instruction[14:12];
            func7 = instruction[31:25];
            
            // Check for supported instruction types
            case(opcode)
                OPCODE_LOAD, OPCODE_STORE: is_valid_instruction = 1'b1;
                OPCODE_ALU_REG: begin
                    case({func7, func3})
                        OP_ADD, OP_SUB, OP_AND, OP_OR, OP_XOR, OP_SLL, OP_SRL, OP_SRA, OP_SLT, OP_SLTU, OP_MUL, OP_DIV, OP_ADDI, OP_LB, OP_LH, OP_LW, OP_LBU, OP_LHU, OP_SB, OP_SH, OP_SW, OP_JAL, OP_JALR, OP_BEQ, OP_BNE, OP_BLT, OP_BGE, OP_BLTU, OP_BGEU: 
                            is_valid_instruction = 1'b1;
                        default: is_valid_instruction = 1'b0;
                    endcase
                end
                OPCODE_ALU_IMM: is_valid_instruction = 1'b1;
                default: is_valid_instruction = 1'b0;
            endcase
        end
    endfunction

    // Function to get source register 2 based on instruction type
    function automatic [4:0] get_src2;
        input [31:0] instruction;
        begin
            if (instruction[6:0] == OPCODE_ALU_IMM) begin
                get_src2 = 5'b00000; // Not used for I-type
            end else begin
                get_src2 = instruction[24:20]; // R-type
            end
        end
    endfunction

    // Function to create operation identifier
    function automatic [16:0] create_operation;
        input [31:0] instruction;
        begin
            create_operation = {instruction[31:25], instruction[14:12], instruction[6:0]};
        end
    endfunction

endpackage

`endif // DECODER_PKG_SV 