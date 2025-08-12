`ifndef TOMASULO_PKG_SV
`define TOMASULO_PKG_SV

package tomasulo_pkg;

    `include "fetch_pkg.sv"
    `include "decoder_pkg.sv"
    // ============================================================================
    // IMPORT EXTERNAL PACKAGES
    // ============================================================================
    import fetch_types_pkg::*;
    import decoder_pkg::*;

    // ============================================================================
    // ARCHITECTURE PARAMETERS
    // ============================================================================
    parameter DATA_WIDTH = 32;                    // Data width for registers and ALU operations
    parameter ADDR_WIDTH = 32;                    // Address width for memory operations
    parameter CDB_WIDTH = 3;                      // Common Data Bus width (number of lanes)
    parameter ISSUE_WIDTH = 3;                    // Triple-issue: can issue up to 3 instructions per cycle
    parameter no_ARF = 32;                        // Full RISC-V register file (x0 to x31)
    parameter no_RAT = 32;                        // RAT size matches ARF size for full register support
    parameter no_instr = 33;                      // Number of instructions (33 for perfect 3-wide alignment)
    parameter no_RS_addsublog = 12;                // Single-cycle ALU reservation stations (ADD, SUB, AND, OR, XOR, shifts, comparisons)
    parameter no_RS_muldiv = 4;                   // Multi-cycle ALU reservation stations (MUL, DIV)
    parameter no_LoadStoreBuffer = 16;            // Load/Store reservation stations
    
    // ============================================================================
    // RISC-V INSTRUCTION FIELD WIDTHS
    // ============================================================================
    parameter OPCODE_WIDTH = 7;                   // RISC-V opcode field width
    parameter FUNC3_WIDTH = 3;                    // RISC-V func3 field width
    parameter FUNC7_WIDTH = 7;                    // RISC-V func7 field width
    parameter REG_ADDR_WIDTH = $clog2(no_ARF);    // RISC-V register address width (derived from no_ARF)
    parameter OPERATION_WIDTH = 17;               // Operation identifier width (func7+func3+opcode)
    parameter INSTR_TYPE_WIDTH = 2;               // Instruction type field width
    parameter RS_INDEX_WIDTH = $clog2(no_LoadStoreBuffer > no_RS_addsublog ? no_LoadStoreBuffer : (no_RS_addsublog > no_RS_muldiv ? no_RS_addsublog : no_RS_muldiv)); // Max RS index width
    parameter MEM_SIZE_WIDTH = 2;                 // Memory operation size width (0:byte, 1:half, 2:word)
    
    // ============================================================================
    // EXECUTION UNITS
    // ============================================================================
    parameter no_ALU_units = 3;                   // 3 ALU execution units (Single Cycle)
    parameter no_MulDiv_units = 2;                // 2 Mul/Div execution units (Multi Cycle)
    parameter NUM_LD_PORTS = 2;                   // Number of load ports (for memory subsystem)
    parameter NUM_ST_PORTS = 1;                   // Number of store ports (for memory subsystem)
    // -------------------------------------------------------------------------
    parameter EXEC_CYCLES_WIDTH = 4; // Number of bits for execution cycle counter
    parameter LATENCY_WIDTH = 4;     // Number of bits for operation latency
    
    // ============================================================================
    // BUFFER SIZES
    // ============================================================================
    parameter no_ROB = 48;                        // Reorder Buffer size (Instruction Window)
    parameter no_main_memory = 1024;              // Main memory size (bytes)
    parameter main_memory_length = 256;           // Main memory length (words)
    
    // ============================================================================
    // PIPELINE STAGE DEFINITIONS
    // ============================================================================
    parameter FETCH_STAGE = 0;
    parameter DECODE_STAGE = 1;
    parameter DISPATCH_STAGE = 2;
    parameter EXECUTE_STAGE = 3;
    parameter WRITEBACK_STAGE = 4;
    parameter COMMIT_STAGE = 5;
    
    // ============================================================================
    // INSTRUCTION OPCODES (concatenated func7,func3,opcode)
    // ============================================================================
    
    // Load instructions (I-type)
    parameter lb  = 17'b0000000_000_0000011; // load byte
    parameter lh  = 17'b0000000_001_0000011; // load halfword
    parameter lw  = 17'b0000000_010_0000011; // load word
    parameter lbu = 17'b0000000_100_0000011; // load byte unsigned
    parameter lhu = 17'b0000000_101_0000011; // load halfword unsigned

    // R-type ALU instructions
    parameter add  = 17'b0000000_000_0110011;      // addition
    parameter sub  = 17'b0100000_000_0110011;      // subtraction
    parameter and_op = 17'b0000000_111_0110011;    // AND
    parameter or_op  = 17'b0000000_110_0110011;    // OR
    parameter xor_op = 17'b0000000_100_0110011;    // XOR
    parameter sll = 17'b0000000_001_0110011;       // Shift Left Logical
    parameter srl = 17'b0000000_101_0110011;       // Shift Right Logical
    parameter sra = 17'b0100000_101_0110011;       // Shift Right Arithmetic
    parameter slt = 17'b0000000_010_0110011;       // Set Less Than (signed)
    parameter sltu = 17'b0000000_011_0110011;      // Set Less Than Unsigned

    // I-type ALU instructions (all func7=0000000)
    parameter addi  = 17'b0000000_000_0010011; // add immediate
    parameter andi  = 17'b0000000_111_0010011; // and immediate
    parameter ori   = 17'b0000000_110_0010011; // or immediate
    parameter xori  = 17'b0000000_100_0010011; // xor immediate
    parameter slti  = 17'b0000000_010_0010011; // set less than immediate
    parameter sltiu = 17'b0000000_011_0010011; // set less than immediate unsigned
    parameter slli  = 17'b0000000_001_0010011; // shift left logical immediate
    parameter srli  = 17'b0000000_101_0010011; // shift right logical immediate
    parameter srai  = 17'b0100000_101_0010011; // shift right arithmetic immediate

    // S-type instructions (stores)
    parameter sb = 17'b0000000_000_0100011; // store byte
    parameter sh = 17'b0000000_001_0100011; // store halfword
    parameter sw = 17'b0000000_010_0100011; // store word

    // Multiply/Divide (RV32M) instructions
    parameter mul   = 17'b0000001_000_0110011;      // multiplication
    parameter mulh  = 17'b0000001_001_0110011;      // multiplication high signed
    parameter mulhu = 17'b0000001_011_0110011;      // multiplication high unsigned
    parameter div   = 17'b0000001_100_0110011;      // division
    parameter divu  = 17'b0000001_101_0110011;      // division unsigned
    parameter rem   = 17'b0000001_110_0110011;      // remainder
    parameter remu  = 17'b0000001_111_0110011;      // remainder unsigned
    
    // Jumps (func7=0000000, func3=000)
    parameter jal   = 17'b0000000_000_1101111; // JAL (unconditional jump/call)
    parameter jalr  = 17'b0000000_000_1100111; // JALR (unconditional jump/ret)

    // Branches (B-type, func7=0000000)
    parameter beq   = 17'b0000000_000_1100011; // BEQ (branch equal)
    parameter bne   = 17'b0000000_001_1100011; // BNE (branch not equal)
    parameter blt   = 17'b0000000_100_1100011; // BLT (branch less than)
    parameter bge   = 17'b0000000_101_1100011; // BGE (branch greater or equal)
    parameter bltu  = 17'b0000000_110_1100011; // BLTU (branch less than unsigned)
    parameter bgeu  = 17'b0000000_111_1100011; // BGEU (branch greater or equal unsigned)
    
    // ============================================================================
    // PIPELINE REGISTER STRUCTURES
    // ============================================================================
    
    // Note: fetch_entry_t and decode_entry_t are imported from fetch_types_pkg and decoder_pkg respectively
    
    // Structure for a single entry in the dispatch pipeline register.
    // Contains all fields needed for dependency resolution, resource allocation, and execution.
    typedef struct packed {
        logic [DATA_WIDTH-1:0]      instruction;  // Raw instruction word
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter of this instruction
        logic [OPCODE_WIDTH-1:0]    opcode;       // Decoded opcode field
        logic [FUNC3_WIDTH-1:0]     func3;        // Decoded func3 field
        logic [FUNC7_WIDTH-1:0]     func7;        // Decoded func7 field
        logic [REG_ADDR_WIDTH-1:0]  src1;         // Source register 1 index
        logic [REG_ADDR_WIDTH-1:0]  src2;         // Source register 2 index
        logic [REG_ADDR_WIDTH-1:0]  dst;          // Destination register index
        logic [DATA_WIDTH-1:0]      immediate;    // Decoded immediate value
        logic [DATA_WIDTH-1:0]      offset;       // Address offset for loads/stores
        logic [OPERATION_WIDTH-1:0] operation;    // Unique operation identifier
        logic [INSTR_TYPE_WIDTH-1:0] instr_type;  // Instruction type
        logic                       valid;        // 1 if this entry holds a valid instruction
        // Dependency resolution results
        logic [$clog2(no_ROB)-1:0]  src1_tag;     // ROB tag for src1 (if waiting)
        logic [$clog2(no_ROB)-1:0]  src2_tag;     // ROB tag for src2 (if waiting)
        logic [DATA_WIDTH-1:0]      src1_value;   // Value of src1 (if ready)
        logic [DATA_WIDTH-1:0]      src2_value;   // Value of src2 (if ready)
        logic                       src1_ready;   // 1 if src1 is ready
        logic                       src2_ready;   // 1 if src2 is ready
        // Resource allocation results
        logic [RS_INDEX_WIDTH-1:0]  rs_index;     // Allocated reservation station index
        logic [$clog2(no_ROB)-1:0]  rob_index;    // Allocated ROB index
        logic                       rs_allocated; // 1 if RS allocated
        logic                       rob_allocated; // 1 if ROB allocated
    } dispatch_entry_t;
    
    // ============================================================================
    // DATA STRUCTURES
    // ============================================================================
    
    // Reorder Buffer (ROB) entry structure
    typedef struct packed {
        logic                       valid;        // Valid bit
        logic                       ready;        // Ready bit (result available)
        logic [REG_ADDR_WIDTH-1:0]  dest;         // Destination register
        logic [DATA_WIDTH-1:0]      value;        // Result value
        logic [REG_ADDR_WIDTH-1:0]  instr_ID;     // Instruction ID
        logic [ADDR_WIDTH-1:0]      pc;           // Program Counter for this instruction
        logic [OPCODE_WIDTH-1:0]    opcode;       // Decoded opcode field
    } rob_entry_t;
    
    // Reservation Station (RS) entry structure for Add/Subtract
    typedef struct packed {
        logic [$clog2(no_instr)-1:0] instr_ID;    // Instruction ID
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter
        logic [OPERATION_WIDTH-1:0] operation;    // Operation type
        logic                       busy;         // Entry is busy
        logic                       src1_valid;   // Source 1 ready
        logic [$clog2(no_ROB)-1:0]  src1_tag;     // ROB tag for src1 (if waiting)
        logic [DATA_WIDTH-1:0]      src1_value;   // Source 1 value
        logic                       src2_valid;   // Source 2 ready
        logic [$clog2(no_ROB)-1:0]  src2_tag;     // ROB tag for src2 (if waiting)
        logic [DATA_WIDTH-1:0]      src2_value;   // Source 2 value
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // Allocated ROB index
        logic                       ready;        // Ready to execute
        logic [DATA_WIDTH-1:0]      immediate;    // Immediate value
    } AddSub_RS_Entry_t;
    
    // Reservation Station (RS) entry structure for Multiply/Divide
    typedef struct packed {
        logic [$clog2(no_instr)-1:0] instr_ID;    // Instruction ID
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter
        logic [OPERATION_WIDTH-1:0] operation;    // Operation type
        logic                       busy;         // Entry is busy
        logic                       src1_valid;   // Source 1 ready
        logic [$clog2(no_ROB)-1:0]  src1_tag;     // ROB tag for src1 (if waiting)
        logic [DATA_WIDTH-1:0]      src1_value;   // Source 1 value
        logic                       src2_valid;   // Source 2 ready
        logic [$clog2(no_ROB)-1:0]  src2_tag;     // ROB tag for src2 (if waiting)
        logic [DATA_WIDTH-1:0]      src2_value;   // Source 2 value
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // Allocated ROB index
        logic                       ready;        // Ready to execute
    } MulDiv_RS_Entry_t;
    
    // Load Store Buffer (LSB) entry structure
    typedef struct packed {
        logic [$clog2(no_instr)-1:0] instr_ID;    // Instruction ID
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter
        logic                       busy;         // Entry is busy
        logic                       src_valid;    // Address ready
        logic [$clog2(no_ROB)-1:0]  src_tag;      // ROB tag for address (if waiting)
        logic [DATA_WIDTH-1:0]      src_value;    // Base register value (rs1)
        logic [FUNC3_WIDTH+OPCODE_WIDTH-1:0] op;  // Operation type ({func3, opcode}, 10 bits)
        logic [DATA_WIDTH-1:0]      A;            // Address offset
        logic [MEM_SIZE_WIDTH-1:0]  size;         // Memory operation size (0:byte, 1:half, 2:word)
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // Allocated ROB index
        logic                       ready;        // Ready to execute
        logic                       store_data_valid; // Store data ready
        logic [$clog2(no_ROB)-1:0]  store_data_tag;   // ROB tag for store data (if waiting)
        logic [DATA_WIDTH-1:0]      store_data;       // Store value (from rs2)
    } LS_Buffer_Entry_t;
    
    // Load Queue (LQ) entry structure
    typedef struct packed {
        logic                       valid;        // Entry valid
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter
        logic [ADDR_WIDTH-1:0]      addr;         // Load address
        logic [DATA_WIDTH-1:0]      data;         // Forwarded data (not used for loads)
        logic [MEM_SIZE_WIDTH-1:0]  size;         // 0:byte, 1:half, 2:word
        logic [$clog2(no_ROB)-1:0]  rob_tag;      // Allocated ROB tag
        logic                       issued;       // Issued to memory
        logic                       completed;    // Completed
        logic                       unsigned_load; // Unsigned load
    } lq_entry_t;

    // Store Queue (SQ) entry structure
    typedef struct packed {
        logic                       valid;        // Entry valid
        logic [ADDR_WIDTH-1:0]      pc;           // Program counter
        logic [ADDR_WIDTH-1:0]      addr;         // Store address
        logic [DATA_WIDTH-1:0]      data;         // Store data
        logic [MEM_SIZE_WIDTH-1:0]  size;         // 0:byte, 1:half, 2:word
        logic [$clog2(no_ROB)-1:0]  rob_tag;      // Allocated ROB tag
        logic                       issued;       // Issued to memory
        logic                       completed;    // Completed
    } sq_entry_t;

    // Helper function: check for address overlap (byte granularity)
    function automatic logic addr_overlap(
        input logic [ADDR_WIDTH-1:0] a1, 
        input logic [MEM_SIZE_WIDTH-1:0] s1,
        input logic [ADDR_WIDTH-1:0] a2, 
        input logic [MEM_SIZE_WIDTH-1:0] s2
    );
        logic [ADDR_WIDTH-1:0] a1_hi, a2_hi;
        a1_hi = a1 + (s1 == 2'b00 ? 0 : (s1 == 2'b01 ? 1 : 3));
        a2_hi = a2 + (s2 == 2'b00 ? 0 : (s2 == 2'b01 ? 1 : 3));
        return (a1 <= a2_hi) && (a2 <= a1_hi);
    endfunction

    // Function for operation to string
    function automatic string op_to_str(input logic [OPERATION_WIDTH-1:0] op);
        case (op)
            add:    op_to_str = "add";
            sub:    op_to_str = "sub";
            addi:   op_to_str = "addi";
            and_op: op_to_str = "and";
            or_op:  op_to_str = "or";
            xor_op: op_to_str = "xor";
            sll:    op_to_str = "sll";
            srl:    op_to_str = "srl";
            sra:    op_to_str = "sra";
            slt:    op_to_str = "slt";
            sltu:   op_to_str = "sltu";
            default: op_to_str = $sformatf("0x%05x", op);
        endcase
    endfunction
    
    // ============================================================================
    // FUNCTIONAL UNITS - Handle their own latency and execution
    // ============================================================================
    
    // ALU Functional Unit structure
    typedef struct packed {
        logic                       busy;         // Unit is executing an instruction
        logic [EXEC_CYCLES_WIDTH-1:0] exec_cycles;  // Current execution cycle count
        logic [LATENCY_WIDTH-1:0]     required_latency; // Required latency for current operation
        logic [DATA_WIDTH-1:0]      src1_value;   // Source operand 1
        logic [DATA_WIDTH-1:0]      src2_value;   // Source operand 2
        logic [DATA_WIDTH-1:0]      immediate;    // Immediate value
        logic [OPERATION_WIDTH-1:0] operation;    // Operation to perform
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // ROB entry for result
        logic [DATA_WIDTH-1:0]      result;       // Execution result
        logic                       result_ready; // Result is ready
    } ALU_FU_t;
    
    // Mul/Div Functional Unit structure
    typedef struct packed {
        logic                       busy;         // Unit is executing an instruction
        logic [EXEC_CYCLES_WIDTH-1:0] exec_cycles;  // Current execution cycle count
        logic [LATENCY_WIDTH-1:0]     required_latency; // Required latency for current operation
        logic [DATA_WIDTH-1:0]      src1_value;   // Source operand 1
        logic [DATA_WIDTH-1:0]      src2_value;   // Source operand 2
        logic [OPERATION_WIDTH-1:0] operation;    // Operation to perform
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // ROB entry for result
        logic [DATA_WIDTH-1:0]      result;       // Execution result
        logic                       result_ready; // Result is ready
    } MulDiv_FU_t;
    
    // Memory Functional Unit structure
    typedef struct packed {
        logic                       busy;         // Unit is executing an instruction
        logic [EXEC_CYCLES_WIDTH-1:0] exec_cycles;  // Current execution cycle count
        logic [LATENCY_WIDTH-1:0]     required_latency; // Required latency for current operation
        logic [ADDR_WIDTH-1:0]      address;      // Memory address
        logic [DATA_WIDTH-1:0]      data_in;      // Data to store
        logic [OPCODE_WIDTH-1:0]    operation;    // Load or Store
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // ROB entry for result
        logic [DATA_WIDTH-1:0]      result;       // Load result
        logic                       result_ready; // Result is ready
    } Memory_FU_t;

    // Functional Unit Output Structure
    typedef struct packed {
        logic [DATA_WIDTH-1:0]      result;       // Execution result
        logic                       result_ready; // Result is ready
        logic [$clog2(no_ROB)-1:0]  ROB_index;    // ROB entry for result
        //logic [$clog2(no_ARF)-1:0]  dest;         // Destination architectural register
    } ALU_Result_t;
    
    // ============================================================================
    // INSTRUCTION TYPE DEFINITIONS
    // ============================================================================
    parameter INSTR_TYPE_ALU = 2'b00;      // ALU instructions (ADD, SUB, AND, OR, etc.)
    parameter INSTR_TYPE_MEM = 2'b01;      // Memory instructions (LOAD, STORE)
    parameter INSTR_TYPE_MULDIV = 2'b10;   // Multiply/Divide instructions
    parameter INSTR_TYPE_OTHER = 2'b11;    // Other instructions (branches, jumps)
    parameter INSTR_TYPE_BRANCH = 2'b11; // Branch/jump/call instructions
    
    // ============================================================================
    // FUNCTION HELPER FUNCTIONS
    // ============================================================================
    
    // Function to determine instruction type from operation
    function automatic logic [INSTR_TYPE_WIDTH-1:0] get_instr_type(logic [OPERATION_WIDTH-1:0] operation);
        case(operation[16:0])
            add[16:0], sub[16:0], addi[16:0], and_op[16:0], andi[16:0], or_op[16:0], ori[16:0], xor_op[16:0], xori[16:0], sll[16:0], slli[16:0], srl[16:0], srli[16:0], sra[16:0], srai[16:0], slt[16:0], slti[16:0], sltu[16:0], sltiu[16:0]:
                get_instr_type = INSTR_TYPE_ALU;
            lb[16:0], lh[16:0], lw[16:0], lbu[16:0], lhu[16:0], sb[16:0], sh[16:0], sw[16:0]:
                get_instr_type = INSTR_TYPE_MEM;
            mul[16:0], mulh[16:0], mulhu[16:0], div[16:0], divu[16:0], rem[16:0], remu[16:0]:
                get_instr_type = INSTR_TYPE_MULDIV;
            beq[16:0], bne[16:0], blt[16:0], bge[16:0], bltu[16:0], bgeu[16:0], jal[16:0], jalr[16:0]:
                get_instr_type = INSTR_TYPE_BRANCH;
            default: 
                get_instr_type = INSTR_TYPE_OTHER;
        endcase
    endfunction
    
    // Function to check if operation is a load
    function automatic logic is_load(logic [OPERATION_WIDTH-1:0] operation);
        is_load = (operation == lb)  || (operation == lh)  || (operation == lw) || (operation == lbu) || (operation == lhu);
    endfunction
    
    // Function to check if operation is a store
    function automatic logic is_store(logic [OPERATION_WIDTH-1:0] operation);
        is_store = (operation == sb) || (operation == sh) || (operation == sw);
    endfunction
    
    // Function to check if operation is a memory operation
    function automatic logic is_memory(logic [OPERATION_WIDTH-1:0] operation);
        is_memory = is_load(operation) || is_store(operation);
    endfunction
    
    // Function to check if operation is a multiply/divide
    function automatic logic is_muldiv(logic [OPERATION_WIDTH-1:0] operation);
        is_muldiv = (operation == mul) || (operation == mulh) || (operation == mulhu) ||
                    (operation == div) || (operation == divu) || (operation == rem) || (operation == remu);
    endfunction
    
    // Function to check if operation is an ALU operation
    function automatic logic is_alu(logic [OPERATION_WIDTH-1:0] operation);
        is_alu = (operation == add) || (operation == sub) || (operation == addi) ||
                 (operation == and_op) || (operation == andi) || (operation == or_op) || (operation == ori) || (operation == xor_op) || (operation == xori) ||
                 (operation == sll) || (operation == slli) || (operation == srl) || (operation == srli) || (operation == sra) || (operation == srai) ||
                 (operation == slt) || (operation == slti) || (operation == sltu) || (operation == sltiu);
    endfunction
    
    // Function to check if operation is a branch or jump
    function automatic logic is_branch(logic [OPERATION_WIDTH-1:0] operation);
        is_branch = (operation == beq)  || (operation == bne)  ||
                    (operation == blt)  || (operation == bge)  ||
                    (operation == bltu) || (operation == bgeu) ||
                    (operation == jal)  || (operation == jalr);
    endfunction

    function automatic [16:0] create_operation;
        input [31:0] instruction;
        begin
            create_operation = {instruction[31:25], instruction[14:12], instruction[6:0]};
        end
    endfunction
    
endpackage

`endif // TOMASULO_PKG_SV 