// ============================================================================
// ARF VALUE CHECKERS - PROGRAM CORRECTNESS VERIFICATION
// ============================================================================
// These checkers verify that the ARF values match expected results from the
// program execution based on the instruction sequence in fetch_unit.sv
//
// This module is designed to be included in alloc_reorder_retire.sv
// ============================================================================

// Parameter to enable/disable ARF checker output (can be overridden by including module)
parameter ARF_CHECKER_ENABLE = 1'b1;

// Structure for storing expected values and results
typedef struct {
    logic [31:0] commit_pc;        // PC when instruction commits
    logic [31:0] dest_reg;         // Destination register (saved during init)
    logic [31:0] expected_value;   // Expected ARF value
    logic [31:0] actual_value;     // Actual ARF value
    logic check_passed;            // Pass/fail flag
    string instruction_str;        // Instruction string for display
    logic [31:0] rob_id;           // ROB ID for tracking
} checker_result_t;

// Checker counters
logic [31:0] checker_passed_count;
logic [31:0] checker_failed_count;
logic [31:0] checker_total_checks;
logic summary_printed;

// ROB ID
logic [$clog2(no_ROB)-1:0] rob_id;

// Loop variables for always_ff blocks
integer arf_checker_commit_idx;
integer arf_checker_result_idx;

// Expected values associative array keyed by ROB ID
checker_result_t expected_values [int];

// Saved destination register and expected value
logic [31:0] saved_dest_reg;
logic [31:0] expected_val;

checker_result_t result;
string status;

// Per-instruction commit checker
always_ff @(posedge clk) begin
    if(result_checker) begin
      if (rst || flush) begin
        checker_passed_count = 32'd0;
        checker_failed_count = 32'd0;
        checker_total_checks = 32'd0;
        summary_printed = 1'b0;

        // Instruction 0: addi x1, x0, 1 (PC=0)
        expected_values[0].commit_pc = 32'd0;
        expected_values[0].dest_reg = 32'd1;
        expected_values[0].expected_value = 32'd1;
        expected_values[0].check_passed = 1'b0;  // Initialize to fail
        expected_values[0].instruction_str = "addi x1, x0, 1";
        
        // Instruction 1: addi x2, x0, 2 (PC=4)
        expected_values[1].commit_pc = 32'd4;
        expected_values[1].dest_reg = 32'd2;
        expected_values[1].expected_value = 32'd2;
        expected_values[1].check_passed = 1'b0;  // Initialize to fail
        expected_values[1].instruction_str = "addi x2, x0, 2";
        
        // Instruction 2: addi x3, x0, 3 (PC=8)
        expected_values[2].commit_pc = 32'd8;
        expected_values[2].dest_reg = 32'd3;
        expected_values[2].expected_value = 32'd3;
        expected_values[2].check_passed = 1'b0;  // Initialize to fail
        expected_values[2].instruction_str = "addi x3, x0, 3";
        
        // Instruction 3: sw x1, 4(x0) (PC=12) - No destination register
        expected_values[3].commit_pc = 32'd12;
        expected_values[3].dest_reg = 32'd0;
        expected_values[3].expected_value = 32'd0;
        expected_values[3].check_passed = 1'b0;  // Initialize to fail
        expected_values[3].instruction_str = "sw x1, 4(x0)";
        
        // Instruction 4: sw x2, 8(x0) (PC=16) - No destination register
        expected_values[4].commit_pc = 32'd16;
        expected_values[4].dest_reg = 32'd0;
        expected_values[4].expected_value = 32'd0;
        expected_values[4].check_passed = 1'b0;  // Initialize to fail
        expected_values[4].instruction_str = "sw x2, 8(x0)";
        
        // Instruction 5: sw x3, 12(x0) (PC=20) - No destination register
        expected_values[5].commit_pc = 32'd20;
        expected_values[5].dest_reg = 32'd0;
        expected_values[5].expected_value = 32'd0;
        expected_values[5].check_passed = 1'b0;  // Initialize to fail
        expected_values[5].instruction_str = "sw x3, 12(x0)";
        
        // Instruction 6: sw x3, 16(x0) (PC=24) - No destination register
        expected_values[6].commit_pc = 32'd24;
        expected_values[6].dest_reg = 32'd0;
        expected_values[6].expected_value = 32'd0;
        expected_values[6].check_passed = 1'b0;  // Initialize to fail
        expected_values[6].instruction_str = "sw x3, 16(x0)";
        
        // Instruction 7: mul x3, x1, x2 (PC=28)
        expected_values[7].commit_pc = 32'd28;
        expected_values[7].dest_reg = 32'd3;
        expected_values[7].expected_value = 32'd2;  // 1 * 2 = 2
        expected_values[7].check_passed = 1'b0;  // Initialize to fail
        expected_values[7].instruction_str = "mul x3, x1, x2";
        
        // Instruction 8: add x4, x2, x3 (PC=32)
        expected_values[8].commit_pc = 32'd32;
        expected_values[8].dest_reg = 32'd4;
        expected_values[8].expected_value = 32'd4;  // 2 + 2 = 4
        expected_values[8].check_passed = 1'b0;  // Initialize to fail
        expected_values[8].instruction_str = "add x4, x2, x3";
        
        // Instruction 9: add x7, x3, x1 (PC=36)
        expected_values[9].commit_pc = 32'd36;
        expected_values[9].dest_reg = 32'd7;
        expected_values[9].expected_value = 32'd3;  // 2 + 1 = 3
        expected_values[9].check_passed = 1'b0;  // Initialize to fail
        expected_values[9].instruction_str = "add x7, x3, x1";
        
        // Instruction 10: add x8, x8, x9 (PC=40)
        expected_values[10].commit_pc = 32'd40;
        expected_values[10].dest_reg = 32'd8;
        expected_values[10].expected_value = 32'd0;  // x8 and x9 are uninitialized, assume 0
        expected_values[10].check_passed = 1'b0;  // Initialize to fail
        expected_values[10].instruction_str = "add x8, x8, x9";
        
        // Instruction 11: lb x1, 4(x0) (PC=44)
        expected_values[11].commit_pc = 32'd44;
        expected_values[11].dest_reg = 32'd1;
        expected_values[11].expected_value = 32'd1;  // Loads from memory[4] = 1 (stored by sw x1, 4(x0))
        expected_values[11].check_passed = 1'b0;  // Initialize to fail
        expected_values[11].instruction_str = "lb x1, 4(x0)";
        
        // Instruction 12: lbu x9, 8(x0) (PC=48)
        expected_values[12].commit_pc = 32'd48;
        expected_values[12].dest_reg = 32'd9;
        expected_values[12].expected_value = 32'd2;  // Loads from memory[8] = 2 (stored by sw x2, 8(x0))
        expected_values[12].check_passed = 1'b0;  // Initialize to fail
        expected_values[12].instruction_str = "lbu x9, 8(x0)";
        
        // Instruction 13: lw x9, 12(x0) (PC=52)
        expected_values[13].commit_pc = 32'd52;
        expected_values[13].dest_reg = 32'd9;
        expected_values[13].expected_value = 32'd3;  // Loads from memory[12] = 3 (stored by sw x3, 12(x0))
        expected_values[13].check_passed = 1'b0;  // Initialize to fail
        expected_values[13].instruction_str = "lw x9, 12(x0)";
        
        // Instruction 14: xor x15, x9, x10 (PC=56)
        expected_values[14].commit_pc = 32'd56;
        expected_values[14].dest_reg = 32'd15;
        expected_values[14].expected_value = 32'd3;  // x9=3, x10=0, 3 ^ 0 = 3
        expected_values[14].check_passed = 1'b0;  // Initialize to fail
        expected_values[14].instruction_str = "xor x15, x9, x10";
        
        // Instruction 15: add x10, x10, x11 (PC=60)
        expected_values[15].commit_pc = 32'd60;
        expected_values[15].dest_reg = 32'd10;
        expected_values[15].expected_value = 32'd0;  // x10=0, x11=0 (uninitialized), 0 + 0 = 0
        expected_values[15].check_passed = 1'b0;  // Initialize to fail
        expected_values[15].instruction_str = "add x10, x10, x11";
        
        // Instruction 16: add x11, x10, x9 (PC=64)
        expected_values[16].commit_pc = 32'd64;
        expected_values[16].dest_reg = 32'd11;
        expected_values[16].expected_value = 32'd3;  // x10=0, x9=3, 0 + 3 = 3
        expected_values[16].check_passed = 1'b0;  // Initialize to fail
        expected_values[16].instruction_str = "add x11, x10, x9";
        
        // Instruction 17: sll x12, x9, x1 (PC=68)
        expected_values[17].commit_pc = 32'd68;
        expected_values[17].dest_reg = 32'd12;
        expected_values[17].expected_value = 32'd6;  // x9=3, x1=1, 3 << 1 = 6
        expected_values[17].check_passed = 1'b0;  // Initialize to fail
        expected_values[17].instruction_str = "sll x12, x9, x1";
        
        // Instruction 18: srl x13, x9, x1 (PC=72)
        expected_values[18].commit_pc = 32'd72;
        expected_values[18].dest_reg = 32'd13;
        expected_values[18].expected_value = 32'd1;  // x9=3, x1=1, 3 >> 1 = 1
        expected_values[18].check_passed = 1'b0;  // Initialize to fail
        expected_values[18].instruction_str = "srl x13, x9, x1";
        
        // Instruction 19: sub x10, x12, x11 (PC=76)
        expected_values[19].commit_pc = 32'd76;
        expected_values[19].dest_reg = 32'd10;
        expected_values[19].expected_value = 32'd3;  // x12=6, x11=3, 6 - 3 = 3
        expected_values[19].check_passed = 1'b0;  // Initialize to fail
        expected_values[19].instruction_str = "sub x10, x12, x11";
        
        // Instruction 20: and x13, x18, x19 (PC=80)
        expected_values[20].commit_pc = 32'd80;
        expected_values[20].dest_reg = 32'd13;
        expected_values[20].expected_value = 32'd0;  // x18=0 (uninitialized), x19=0 (uninitialized), 0 & 0 = 0
        expected_values[20].check_passed = 1'b0;  // Initialize to fail
        expected_values[20].instruction_str = "and x13, x18, x19";
        
        // Instruction 21: slt x14, x10, x12 (PC=84)
        expected_values[21].commit_pc = 32'd84;
        expected_values[21].dest_reg = 32'd14;
        expected_values[21].expected_value = 32'd1;  // x10=3, x12=6, 3 < 6 = 1 (true)
        expected_values[21].check_passed = 1'b0;  // Initialize to fail
        expected_values[21].instruction_str = "slt x14, x10, x12";
        
        // Instruction 22: add x11, x9, x10 (PC=88)
        expected_values[22].commit_pc = 32'd88;
        expected_values[22].dest_reg = 32'd11;
        expected_values[22].expected_value = 32'd6;  // x9=3, x10=3, 3 + 3 = 6
        expected_values[22].check_passed = 1'b0;  // Initialize to fail
        expected_values[22].instruction_str = "add x11, x9, x10";
        
        // Instruction 23: div x6, x1, x2 (PC=92)
        expected_values[23].commit_pc = 32'd92;
        expected_values[23].dest_reg = 32'd6;
        expected_values[23].expected_value = 32'd2;  // x1=2, x2=1, 2 / 1 = 2 (integer division)
        expected_values[23].check_passed = 1'b0;  // Initialize to fail
        expected_values[23].instruction_str = "div x6, x1, x2";
        
        // Instruction 24: lb x18, 16(x0) (PC=96)
        expected_values[24].commit_pc = 32'd96;
        expected_values[24].dest_reg = 32'd18;
        expected_values[24].expected_value = 32'd3;  // Loads from memory[16] = 3 (stored by sw x3, 16(x0))
        expected_values[24].check_passed = 1'b0;  // Initialize to fail
        expected_values[24].instruction_str = "lb x18, 16(x0)";
        
        // Instruction 25: lb x19, 20(x0) (PC=100)
        expected_values[25].commit_pc = 32'd100;
        expected_values[25].dest_reg = 32'd19;
        expected_values[25].expected_value = 32'd5;  // Loads from memory[20] = 5 (MMU assigned)
        expected_values[25].check_passed = 1'b0;  // Initialize to fail
        expected_values[25].instruction_str = "lb x19, 20(x0)";
        
        // Instruction 26: sw x25, 20(x19) (PC=104) - No destination register
        expected_values[26].commit_pc = 32'd104;
        expected_values[26].dest_reg = 32'd0;
        expected_values[26].expected_value = 32'd0;
        expected_values[26].check_passed = 1'b0;  // Initialize to fail
        expected_values[26].instruction_str = "sw x25, 20(x19)";
        
        // Instruction 27: add x1, x9, x10 (PC=108)
        expected_values[27].commit_pc = 32'd108;
        expected_values[27].dest_reg = 32'd1;
        expected_values[27].expected_value = 32'd6;  // x9=3, x10=3, 3 + 3 = 6
        expected_values[27].check_passed = 1'b0;  // Initialize to fail
        expected_values[27].instruction_str = "add x1, x9, x10";
        
        // Instruction 28: add x2, x9, x11 (PC=112)
        expected_values[28].commit_pc = 32'd112;
        expected_values[28].dest_reg = 32'd2;
        expected_values[28].expected_value = 32'd9;  // x9=3, x11=6, 3 + 6 = 9
        expected_values[28].check_passed = 1'b0;  // Initialize to fail
        expected_values[28].instruction_str = "add x2, x9, x11";
        
        // Instruction 29: add x3, x1, x2 (PC=116)
        expected_values[29].commit_pc = 32'd116;
        expected_values[29].dest_reg = 32'd3;
        expected_values[29].expected_value = 32'd15;  // x1=6, x2=9, 6 + 9 = 15
        expected_values[29].check_passed = 1'b0;  // Initialize to fail
        expected_values[29].instruction_str = "add x3, x1, x2";
        
        // Instruction 30: mul x8, x1, x3 (PC=120)
        expected_values[30].commit_pc = 32'd120;
        expected_values[30].dest_reg = 32'd8;
        expected_values[30].expected_value = 32'd90;  // x1=6, x3=15, 6 * 15 = 90
        expected_values[30].check_passed = 1'b0;  // Initialize to fail
        expected_values[30].instruction_str = "mul x8, x1, x3";
        
        // Instruction 31: or x7, x1, x2 (PC=124)
        expected_values[31].commit_pc = 32'd124;
        expected_values[31].dest_reg = 32'd7;
        expected_values[31].expected_value = 32'd15;  // x1=6, x2=9, 6 | 9 = 15
        expected_values[31].check_passed = 1'b0;  // Initialize to fail
        expected_values[31].instruction_str = "or x7, x1, x2";
        
        // Instruction 32: add x29, x7, x1 (PC=128)
        expected_values[32].commit_pc = 32'd128;
        expected_values[32].dest_reg = 32'd29;
        expected_values[32].expected_value = 32'd21;  // x7=15, x1=6, 15 + 6 = 21
        expected_values[32].check_passed = 1'b0;  // Initialize to fail
        expected_values[32].instruction_str = "add x29, x7, x1";

    end else begin
        // Only check after system is initialized and we have valid commits
        if (total_instructions_committed > 0) begin
            // Check ARF value when instructions commit (3-wide commit)
            for (arf_checker_commit_idx = 0; arf_checker_commit_idx < ISSUE_WIDTH; arf_checker_commit_idx++) begin
                if (commit_valid[arf_checker_commit_idx]) begin
                    // Get the ROB ID for this commit
                    rob_id = commit_rob_id[arf_checker_commit_idx];
                    
                    // Check if this ROB ID has an expected value
                    if (expected_values.exists(rob_id)) begin
                        checker_total_checks = checker_total_checks + 1;
                        
                        // Get the saved destination register and expected value
                        saved_dest_reg = expected_values[rob_id].dest_reg;
                        expected_val = expected_values[rob_id].expected_value;
                        
                        // Check if value matches using retire_info_reg value and saved destination register
                        if (retire_info_reg[arf_checker_commit_idx].result == expected_val) begin
                            checker_passed_count = checker_passed_count + 1;
                            expected_values[rob_id].check_passed = 1'b1;
                            expected_values[rob_id].actual_value = retire_info_reg[arf_checker_commit_idx].result;
                            expected_values[rob_id].rob_id = rob_id;
                            if (ARF_CHECKER_ENABLE) begin
                                $display("[%0t] ARF Checker: PASS - ROB[%0d] %s -> x%0d = %0d", 
                                         $realtime, rob_id, expected_values[rob_id].instruction_str, saved_dest_reg, retire_info_reg[arf_checker_commit_idx].result);
                            end
                        end else begin
                            checker_failed_count = checker_failed_count + 1;
                            expected_values[rob_id].check_passed = 1'b0;
                            expected_values[rob_id].actual_value = retire_info_reg[arf_checker_commit_idx].result;
                            expected_values[rob_id].rob_id = rob_id;
                            if (ARF_CHECKER_ENABLE) begin
                                $display("[%0t] ARF Checker: FAIL - ROB[%0d] %s -> x%0d = %0d (expected: %0d)", 
                                         $realtime, rob_id, expected_values[rob_id].instruction_str, saved_dest_reg, retire_info_reg[arf_checker_commit_idx].result, expected_val);
                            end
                        end
                    end
                end
            end
        end
        
                // Final summary when all instructions are committed
        if (total_instructions_committed >= 33 && checker_total_checks > 0 && !summary_printed) begin
            if (ARF_CHECKER_ENABLE) begin
                $display("\n=== CHECKER COMPREHENSIVE SUMMARY ===");
                $display("Total checks performed: %0d", checker_total_checks);
                $display("Passed: %0d, Failed: %0d", checker_passed_count, checker_failed_count);
                
                if (checker_failed_count == 0) begin
                    $display("=== ALL CHECKS PASSED ===");
                end else begin
                    $display("=== SOME CHECKS FAILED ===");
                end
                
                $display("\nDetailed Results:");
                $display("ROB_ID | PC    | Instruction           | Dest | Actual | Expected | Status");
                $display("-------|-------|-----------------------|------|--------|----------|--------");
                
                // Print all results in order - iterate through all possible ROB IDs
                for (arf_checker_result_idx = 0; arf_checker_result_idx < no_ROB; arf_checker_result_idx++) begin
                    if (expected_values.exists(arf_checker_result_idx)) begin
                        result = expected_values[arf_checker_result_idx];
                        status = result.check_passed ? "PASS" : "FAIL";
                        if (result.dest_reg != 0) begin  // Instructions with destination registers
                            $display("%6d | %5d | %-20s | x%2d   | %6d | %8d | %s", 
                                     result.rob_id, result.commit_pc, result.instruction_str, 
                                     result.dest_reg, result.actual_value, result.expected_value, status);
                        end else begin  // Store instructions (no destination register)
                            $display("%6d | %5d | %-20s | --    | --     | --       | %s", 
                                     result.rob_id, result.commit_pc, result.instruction_str, status);
                        end
                    end
                end
                
                $display("================================");
                summary_printed = 1'b1;
            end
        end
      end
    end
end
