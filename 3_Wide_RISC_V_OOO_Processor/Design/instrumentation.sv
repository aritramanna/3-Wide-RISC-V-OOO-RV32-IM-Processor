// ============================================================================
// INSTRUMENTATION BLOCK FOR TOMASULO PROCESSOR
// ============================================================================
// This file contains the main instrumentation unit for pipeline monitoring
// It monitors and prints activity of all pipeline stages with well-formatted output
// 
// CONTROL: This instrumentation is gated by the debug_print signal
// - When debug_print = 1'b0: No debug output is generated (silent operation)
// - When debug_print = 1'b1: Full debug output is generated
// - Control from testbench: dut.tomasulo_core.debug_print

// Include this file in alloc_reorder_retire.sv using: `include "instrumentation.sv"

    // ============================================================================
    // IPC MONITORING VARIABLES
    // ============================================================================
    // Variables to track IPC from first commit to last commit
    logic ipc_monitor_active;
    logic [31:0] ipc_start_cycle;
    logic [31:0] ipc_end_cycle;
    logic [31:0] ipc_cycles_counted;
    logic [31:0] ipc_instructions_counted;
    real ipc_final_result;

// ============================================================================
// MAIN INSTRUMENTATION UNIT - PIPELINE MONITORING
// ============================================================================
// This block monitors and prints activity of all pipeline stages
// Only prints for valid entries with well-formatted output and timestamps
// Gated by debug_print signal

always_ff @(posedge clk) begin
    // ============================================================================
    // IPC MONITORING LOGIC (Always Active)
    // ============================================================================
    // Initialize IPC monitoring variables on reset
    if (rst) begin
        ipc_monitor_active = 1'b0;
        ipc_start_cycle = 32'd0;
        ipc_end_cycle = 32'd0;
        ipc_cycles_counted = 32'd0;
        ipc_instructions_counted = 32'd0;
        ipc_final_result = 0.0;
    end else begin
        // Check for instructions committed this cycle
        committed_found = 0; committed_count = 0;
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            if (commit_valid[i]) committed_found = 1;
        end
    
    if (committed_found) begin
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            if (commit_valid[i]) begin
                committed_count++;
            end
        end
        
                // Start IPC monitoring on first commit
    if (!ipc_monitor_active) begin
        ipc_monitor_active = 1'b1;
        ipc_start_cycle = cycle_count;
        ipc_end_cycle = cycle_count;
        ipc_instructions_counted = 0;
        if (debug_print) begin
            $display("[%0t] IPC MONITOR: Starting IPC tracking at cycle %0d (first commit)", $realtime, cycle_count);
        end
    end
        
    // Count instructions committed and update end cycle
    if (ipc_monitor_active) begin
        ipc_instructions_counted = ipc_instructions_counted + committed_count;
        ipc_end_cycle = cycle_count;  // Update end cycle whenever we see commits
    end
    end
    
    // Calculate IPC cycle count from start to end
    if (ipc_monitor_active) begin
        ipc_cycles_counted = ipc_end_cycle - ipc_start_cycle + 1;
    end
    end // end of else block for non-reset condition
    
    // ============================================================================
    // DEBUG PRINTING (Gated by debug_print)
    // ============================================================================
    if (!rst && debug_print) begin
        
        //$display("\n[%0t] ========== CYCLE %0d BEGIN ==========", $realtime, cycle_count);
        
        // ============================================================================
        // DECODE/DISPATCH STAGE MONITORING
        // ============================================================================
        decode_valid_found = 0; dispatch_valid_found = 0;
        for (int i = 0; i < ISSUE_WIDTH; i++) begin
            if (decode_reg[i].valid) decode_valid_found = 1;
            if (dispatch_reg[i].valid) dispatch_valid_found = 1;
        end
        if (decode_valid_found || dispatch_valid_found) begin
            $display("\n[%0t] ========== DECODE/DISPATCH STAGE ==========", $realtime);
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                if (decode_reg[i].valid) begin
                    $display("[%0t] DECODE[%0d]: PC=0x%08x Instr=0x%08x Type=%0d Dst=%0d Src1=%0d Src2=%0d Opcode=0x%02x Func3=0x%01x Func7=0x%02x Operation=0x%05x Imm=0x%08x", 
                        $realtime, i, decode_reg[i].pc, decode_reg[i].instruction, decode_reg[i].instr_type, 
                        decode_reg[i].dst, decode_reg[i].src1, decode_reg[i].src2, decode_reg[i].opcode, 
                        decode_reg[i].func3, decode_reg[i].func7, decode_reg[i].operation, decode_reg[i].immediate);
                end
            end
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                if (dispatch_reg[i].valid) begin
                    $display("[%0t] DISPATCH[%0d]: ROB_ID=%0d RS_ID=%0d RS_Alloc=%0b Src1_Ready=%0b Src2_Ready=%0b Dst=%0d Src1=%0d Src2=%0d Src1_Tag=%0d Src2_Tag=%0d Src1_Value=0x%08x Src2_Value=0x%08x Operation=0x%05x Type=%0d PC=0x%08x Instr=0x%08x", 
                        $realtime, i, dispatch_reg[i].rob_index, dispatch_reg[i].rs_index, 
                        dispatch_reg[i].rs_allocated, dispatch_reg[i].src1_ready, dispatch_reg[i].src2_ready,
                        dispatch_reg[i].dst, dispatch_reg[i].src1, dispatch_reg[i].src2,
                        dispatch_reg[i].src1_tag, dispatch_reg[i].src2_tag, dispatch_reg[i].src1_value, dispatch_reg[i].src2_value,
                        dispatch_reg[i].operation, dispatch_reg[i].instr_type, dispatch_reg[i].pc, dispatch_reg[i].instruction);
                end
            end
        end
        
        // ============================================================================
        // REORDER BUFFER (ROB) MONITORING
        // ============================================================================
        rob_valid_found = 0;
        for (int i = 0; i < no_ROB; i++) begin
            if (ROB[i].valid) rob_valid_found = 1;
        end
        if (rob_valid_found) begin
            $display("\n[%0t] ========== REORDER BUFFER (ROB) ==========", $realtime);
            $display("[%0t] ROB Head: %0d, ROB Tail: %0d", $realtime, ROB_head, ROB_tail);
            for (int i = 0; i < no_ROB; i++) begin
                if (ROB[i].valid) begin
                    $display("[%0t] ROB[%0d]: Valid=%0b Ready=%0b Dest=%0d Value=0x%08x Instr_ID=%0d PC=0x%08x", 
                        $realtime, i, ROB[i].valid, ROB[i].ready, ROB[i].dest, ROB[i].value, 
                        ROB[i].instr_ID, ROB[i].pc);
                end
            end
        end
        
        // ============================================================================
        // RESERVATION STATIONS (RS) MONITORING
        // ============================================================================
        alu_rs_busy_found = 0; md_rs_busy_found = 0; lsb_busy_found = 0;
        for (int i = 0; i < no_RS_addsublog; i++) begin
            if (AS_RS[i].busy) alu_rs_busy_found = 1;
        end
        for (int i = 0; i < no_RS_muldiv; i++) begin
            if (MD_RS[i].busy) md_rs_busy_found = 1;
        end
        for (int i = 0; i < no_LoadStoreBuffer; i++) begin
            if (LS_buffer[i].busy) lsb_busy_found = 1;
        end
        if (alu_rs_busy_found || md_rs_busy_found || lsb_busy_found) begin
            $display("\n[%0t] ========== RESERVATION STATIONS ==========", $realtime);
            if (alu_rs_busy_found) begin
                $display("[%0t] --- ALU Reservation Stations ---", $realtime);
                for (int i = 0; i < no_RS_addsublog; i++) begin
                    if (AS_RS[i].busy) begin
                        $display("[%0t] ALU_RS[%0d]: Busy=%0b Ready=%0b ROB_ID=%0d Op=0x%05x Src1_Valid=%0b Src1_Tag=%0d Src1_Value=0x%08x Src2_Valid=%0b Src2_Tag=%0d Src2_Value=0x%08x", 
                            $realtime, i, AS_RS[i].busy, AS_RS[i].ready, AS_RS[i].ROB_index, AS_RS[i].operation,
                            AS_RS[i].src1_valid, AS_RS[i].src1_tag, AS_RS[i].src1_value, AS_RS[i].src2_valid, AS_RS[i].src2_tag, AS_RS[i].src2_value);
                    end
                end
            end
            if (md_rs_busy_found) begin
                $display("[%0t] --- Mul/Div Reservation Stations ---", $realtime);
                for (int i = 0; i < no_RS_muldiv; i++) begin
                    if (MD_RS[i].busy) begin
                        $display("[%0t] MD_RS[%0d]: Busy=%0b Ready=%0b ROB_ID=%0d Op=0x%05x Src1_Valid=%0b Src1_Tag=%0d Src1_Value=0x%08x Src2_Valid=%0b Src2_Tag=%0d Src2_Value=0x%08x", 
                            $realtime, i, MD_RS[i].busy, MD_RS[i].ready, MD_RS[i].ROB_index, MD_RS[i].operation,
                            MD_RS[i].src1_valid, MD_RS[i].src1_tag, MD_RS[i].src1_value, MD_RS[i].src2_valid, MD_RS[i].src2_tag, MD_RS[i].src2_value);
                    end
                end
            end
            if (lsb_busy_found) begin
                $display("[%0t] --- Load/Store Buffer ---", $realtime);
                for (int i = 0; i < no_LoadStoreBuffer; i++) begin
                    if (LS_buffer[i].busy) begin
                        $display("[%0t] LSB[%0d]: Busy=%0b Ready=%0b ROB_ID=%0d Op=0x%05x Src_Valid=%0b Src_Tag=%0d Src_Value=0x%08x Store_Data_Valid=%0b Store_Data_Tag=%0d Store_Data=0x%08x", 
                            $realtime, i, LS_buffer[i].busy, LS_buffer[i].ready, LS_buffer[i].ROB_index, LS_buffer[i].op,
                            LS_buffer[i].src_valid, LS_buffer[i].src_tag, LS_buffer[i].src_value, LS_buffer[i].store_data_valid, LS_buffer[i].store_data_tag, LS_buffer[i].store_data);
                    end
                end
            end
        end
        
        // ============================================================================
        // EXECUTION STAGE MONITORING (CDB INPUT)
        // ============================================================================
        cdb_valid_found = 0;
        for (int i = 0; i < CDB_WIDTH; i++) begin
            if (cdb_results_in[i].result_ready) cdb_valid_found = 1;
        end
        if (cdb_valid_found) begin
            $display("\n[%0t] ========== EXECUTION STAGE (CDB INPUT) ==========", $realtime);
            for (int i = 0; i < CDB_WIDTH; i++) begin
                if (cdb_results_in[i].result_ready) begin
                    $display("[%0t] CDB[%0d]: ROB_ID=%0d Result=0x%08x Ready=%0b", 
                        $realtime, i, cdb_results_in[i].ROB_index, cdb_results_in[i].result, cdb_results_in[i].result_ready);
                end
            end
        end

        // ============================================================================
        // FORWARDING MESSAGES MONITORING
        // ============================================================================
        forwarding_found = 0;
        // Check for actual forwarding events, not just available results
        for (int i = 0; i < CDB_WIDTH; i++) begin
            if (cdb_results_in[i].result_ready) begin
                // Check if any RS needs this result
                for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                end
                for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                end
                for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                    if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                    if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == cdb_results_in[i].ROB_index) forwarding_found = 1;
                end
            end
            if (cdb_results_reg_1[i].result_ready) begin
                // Check if any RS needs this delayed result
                for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                end
                for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                end
                for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                    if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                    if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == cdb_results_reg_1[i].ROB_index) forwarding_found = 1;
                end
            end
        end
        // Check retirement bypass forwarding
        for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
            if (retire_info_current[retire_idx].result_ready) begin
                for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                end
                for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                end
                for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                    if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                    if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == retire_info_current[retire_idx].ROB_index) forwarding_found = 1;
                end
            end
            if (retire_info_reg_1[retire_idx].result_ready) begin
                for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                    if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                end
                for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                    if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                end
                for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                    if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                    if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == retire_info_reg_1[retire_idx].ROB_index) forwarding_found = 1;
                end
            end
        end
        // Check intra-bundle forwarding
        for (int dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
            if (dispatch_reg[dispatch_idx].valid) begin
                for (int prev = dispatch_idx - 1; prev >= 0; prev--) begin
                    if (dispatch_reg[prev].valid && dispatch_reg[prev].dst != 0 && 
                        (dispatch_reg[prev].dst == dispatch_reg[dispatch_idx].src1 || 
                         dispatch_reg[prev].dst == dispatch_reg[dispatch_idx].src2)) begin
                        forwarding_found = 1;
                        break; // Found the most recent, stop searching
                    end
                end
            end
        end
        if (forwarding_found) begin
            $display("\n[%0t] ========== FORWARDING MESSAGES ==========", $realtime);
            
            // Monitor CDB forwarding to reservation stations (Current Cycle)
            for (int i = 0; i < CDB_WIDTH; i++) begin
                if (cdb_results_in[i].result_ready) begin
                    // Check ALU RS forwarding
                    for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->ALU_SRC1]: CDB[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, cdb_results_in[i].result);
                        end
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->ALU_SRC2]: CDB[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, cdb_results_in[i].result);
                        end
                    end
                    
                    // Check Mul/Div RS forwarding
                    for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->MD_SRC1]: CDB[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, cdb_results_in[i].result);
                        end
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->MD_SRC2]: CDB[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, cdb_results_in[i].result);
                        end
                    end
                    
                    // Check Load/Store Buffer forwarding
                    for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                        if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->LSB_SRC]: CDB[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, cdb_results_in[i].result);
                        end
                        if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == cdb_results_in[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_CURR->LSB_STORE]: CDB[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Store_Data=0x%08x", 
                                $realtime, i, cdb_results_in[i].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, cdb_results_in[i].result);
                        end
                    end
                end
            end
            
            // Monitor Delayed CDB forwarding to reservation stations (1-cycle delayed)
            for (int i = 0; i < CDB_WIDTH; i++) begin
                if (cdb_results_reg_1[i].result_ready) begin
                    // Check ALU RS forwarding
                    for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->ALU_SRC1]: CDB_DELAYED[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->ALU_SRC2]: CDB_DELAYED[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                    end
                    
                    // Check Mul/Div RS forwarding
                    for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->MD_SRC1]: CDB_DELAYED[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->MD_SRC2]: CDB_DELAYED[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                    end
                    
                    // Check Load/Store Buffer forwarding
                    for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                        if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->LSB_SRC]: CDB_DELAYED[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                        if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == cdb_results_reg_1[i].ROB_index) begin
                            $display("[%0t] FORWARD[CDB_DELAYED->LSB_STORE]: CDB_DELAYED[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Store_Data=0x%08x", 
                                $realtime, i, cdb_results_reg_1[i].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, cdb_results_reg_1[i].result);
                        end
                    end
                end
            end
            
            // Monitor retirement bypass forwarding (Current Cycle)
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_current[retire_idx].result_ready) begin
                    // Check ALU RS retirement bypass
                    for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->ALU_SRC1]: RETIRE_CURR[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->ALU_SRC2]: RETIRE_CURR[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                    end
                    
                    // Check Mul/Div RS retirement bypass
                    for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->MD_SRC1]: RETIRE_CURR[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->MD_SRC2]: RETIRE_CURR[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                    end
                    
                    // Check Load/Store Buffer retirement bypass
                    for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                        if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->LSB_SRC]: RETIRE_CURR[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                        if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == retire_info_current[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_CURR->LSB_STORE]: RETIRE_CURR[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Store_Data=0x%08x", 
                                $realtime, retire_idx, retire_info_current[retire_idx].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, retire_info_current[retire_idx].result);
                        end
                    end
                end
            end
            
            // Monitor Delayed retirement bypass forwarding (1-cycle delayed)
            for (int retire_idx = 0; retire_idx < ISSUE_WIDTH; retire_idx++) begin
                if (retire_info_reg_1[retire_idx].result_ready) begin
                    // Check ALU RS retirement bypass
                    for (int rs_idx = 0; rs_idx < no_RS_addsublog; rs_idx++) begin
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src1_valid && AS_RS[rs_idx].src1_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->ALU_SRC1]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                        if (AS_RS[rs_idx].busy && !AS_RS[rs_idx].src2_valid && AS_RS[rs_idx].src2_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->ALU_SRC2]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> ALU_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, rs_idx, AS_RS[rs_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                    end
                    
                    // Check Mul/Div RS retirement bypass
                    for (int rs_idx = 0; rs_idx < no_RS_muldiv; rs_idx++) begin
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src1_valid && MD_RS[rs_idx].src1_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->MD_SRC1]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                        if (MD_RS[rs_idx].busy && !MD_RS[rs_idx].src2_valid && MD_RS[rs_idx].src2_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->MD_SRC2]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> MD_RS[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, rs_idx, MD_RS[rs_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                    end
                    
                    // Check Load/Store Buffer retirement bypass
                    for (int lsb_idx = 0; lsb_idx < no_LoadStoreBuffer; lsb_idx++) begin
                        if (LS_buffer[lsb_idx].busy && !LS_buffer[lsb_idx].src_valid && LS_buffer[lsb_idx].src_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->LSB_SRC]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Value=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                        if (LS_buffer[lsb_idx].busy && is_store(LS_buffer[lsb_idx].op) && !LS_buffer[lsb_idx].store_data_valid && LS_buffer[lsb_idx].store_data_tag == retire_info_reg_1[retire_idx].ROB_index) begin
                            $display("[%0t] FORWARD[RETIRE_DELAYED->LSB_STORE]: RETIRE_DELAYED[%0d] ROB_ID=%0d -> LSB[%0d] ROB_ID=%0d Store_Data=0x%08x", 
                                $realtime, retire_idx, retire_info_reg_1[retire_idx].ROB_index, lsb_idx, LS_buffer[lsb_idx].ROB_index, retire_info_reg_1[retire_idx].result);
                        end
                    end
                end
            end
            
            // Monitor intra-bundle forwarding (dispatch stage)
            for (int dispatch_idx = 0; dispatch_idx < ISSUE_WIDTH; dispatch_idx++) begin
                if (dispatch_reg[dispatch_idx].valid) begin
                    for (int prev = 0; prev < dispatch_idx; prev++) begin
                        if (dispatch_reg[prev].valid && dispatch_reg[prev].dst != 0 && dispatch_reg[prev].dst == dispatch_reg[dispatch_idx].src1) begin
                            $display("[%0t] FORWARD[INTRA_SRC1]: DISPATCH[%0d] ROB_ID=%0d -> DISPATCH[%0d] ROB_ID=%0d Reg=%0d", 
                                $realtime, prev, dispatch_reg[prev].rob_index, dispatch_idx, dispatch_reg[dispatch_idx].rob_index, dispatch_reg[prev].dst);
                        end
                        if (dispatch_reg[prev].valid && dispatch_reg[prev].dst != 0 && dispatch_reg[prev].dst == dispatch_reg[dispatch_idx].src2) begin
                            $display("[%0t] FORWARD[INTRA_SRC2]: DISPATCH[%0d] ROB_ID=%0d -> DISPATCH[%0d] ROB_ID=%0d Reg=%0d", 
                                $realtime, prev, dispatch_reg[prev].rob_index, dispatch_idx, dispatch_reg[dispatch_idx].rob_index, dispatch_reg[prev].dst);
                        end
                    end
                end
            end
        end

        // ============================================================================
        // INSTRUCTIONS COMMITTED THIS CYCLE
        // ============================================================================
        if (committed_found) begin
            $display("\n[%0t] ========== INSTRUCTIONS COMMITTED THIS CYCLE ==========", $realtime);
            
            for (int i = 0; i < ISSUE_WIDTH; i++) begin
                if (commit_valid[i]) begin
                    $display("[%0t] COMMITTED[%0d]: ROB_ID=%0d", 
                        $realtime, i, commit_rob_id[i]);
                end
            end
            $display("[%0t] Total Instructions Committed This Cycle: %0d", $realtime, committed_count);
        end

        /*
        // ============================================================================
        // NEXT RAT AND VALID MONITORING
        // ============================================================================
        next_rat_valid_found = 0;
        for (int i = 0; i < no_RAT; i++) begin
            if (temp_RAT_valid[i]) next_rat_valid_found = 1;
        end
        if (next_rat_valid_found) begin
            $display("\n[%0t] ========== NEXT RAT AND VALID ==========", $realtime);
            for (int i = 0; i < no_RAT; i++) begin
                if (temp_RAT_valid[i]) begin
                    $display("[%0t] NEXT_RAT[%0d]: Tag=%0d Valid=%0b", 
                        $realtime, i, temp_RAT_tag[i], temp_RAT_valid[i]);
                end
            end
        end
        */
        
        // ============================================================================
        // CURRENT RAT AND VALID MONITORING
        // ============================================================================
        current_rat_valid_found = 0;
        for (int i = 0; i < no_RAT; i++) begin
            if (RAT_valid[i]) current_rat_valid_found = 1;
        end
        if (current_rat_valid_found) begin
            $display("\n[%0t] ========== CURRENT RAT AND VALID ==========", $realtime);
            for (int i = 0; i < no_RAT; i++) begin
                if (RAT_valid[i]) begin
                    $display("[%0t] RAT[%0d]: Tag=%0d Valid=%0b", 
                        $realtime, i, RAT_tag[i], RAT_valid[i]);
                end
            end
        end
        
        // ============================================================================
        // ARCHITECTURAL REGISTER FILE (ARF) MONITORING
        // ============================================================================
        arf_nonzero_found = 0;
        for (int i = 0; i < no_ARF; i++) begin
            if (ARF[i] != 0) arf_nonzero_found = 1;
        end
        if (arf_nonzero_found) begin
            $display("\n[%0t] ========== ARCHITECTURAL REGISTER FILE (ARF) ==========", $realtime);
            for (int i = 0; i < no_ARF; i++) begin
                if (ARF[i] != 0) begin  // Only show non-zero registers
                    $display("[%0t] ARF[%0d]: Value=0x%08x", 
                        $realtime, i, ARF[i]);
                end
            end
        end

        // ============================================================================
        // PERFORMANCE METRICS
        // ============================================================================

        if( all_instructions_completed) begin
            $display("\n[%0t] ========== PERFORMANCE METRICS ==========", $realtime);
            $display("[%0t] Cycle Count: %0d", $realtime, cycle_count);
            $display("[%0t] Total Instructions Committed: %0d", $realtime, total_instructions_committed);
            $display("[%0t] All Instructions Completed: %0b", $realtime, all_instructions_completed);
            
            // ============================================================================
            // IPC CALCULATION AND DISPLAY
            // ============================================================================
            if (ipc_monitor_active && ipc_cycles_counted > 0) begin
                ipc_final_result = real'(ipc_instructions_counted) / real'(ipc_cycles_counted);
                $display("\n[%0t] ========== IPC ANALYSIS ==========", $realtime);
                $display("[%0t] IPC Measurement Period: Cycles %0d to %0d", $realtime, ipc_start_cycle, ipc_end_cycle);
                $display("[%0t] Total Cycles Measured: %0d", $realtime, ipc_cycles_counted);
                $display("[%0t] Instructions Committed: %0d", $realtime, ipc_instructions_counted);
                $display("[%0t] FINAL IPC: %0.4f", $realtime, ipc_final_result);
                $display("[%0t] =================================", $realtime);
            end else begin
                $display("\n[%0t] IPC MONITOR: No valid IPC measurement period detected", $realtime);
            end
        end

        
        // ============================================================================
        // RESOURCE AVAILABILITY
        // ============================================================================
        /*$display("\n[%0t] ========== RESOURCE AVAILABILITY ==========", $realtime);
        $display("[%0t] Available ROB Entries: %0d", $realtime, available_rob_entries);
        $display("[%0t] Available ALU RS: %0d", $realtime, available_alu_rs);
        $display("[%0t] Available Mul/Div RS: %0d", $realtime, available_muldiv_rs);
        $display("[%0t] Available Memory RS: %0d", $realtime, available_mem_rs);
        $display("[%0t] Allocation Stall: %0b", $realtime, alloc_stall);*/
        
        
        //$display("\n[%0t] ========== CYCLE %0d END ==========\n", $realtime, cycle_count);
    end
end
