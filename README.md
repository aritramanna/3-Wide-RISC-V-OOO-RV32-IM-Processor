# 3-Wide-RISC-V-OOO-RV32-IM-Processor
3-wide superscalar, out-of-order RISC-V processor (RV32IM subset) in System Verilog, demonstrating key Instruction-Level Parallelism

**Introduction:**

Work-in-progress 3-wide superscalar, out-of-order RISC-V processor (RV32IM subset) in System Verilog, demonstrating key Instruction-Level Parallelism concepts. Features a high-performance Front-End with Gshare branch prediction, 4-way BTB, RAS, and BPQ for deep speculation and rapid recovery, and a decoupled fetch/decode/execute pipeline to hide backend stalls. Implements Tomasulo-based register renaming, all-or-nothing dispatch, and a 48-entry ROB for precise in-order retirement. Includes low-latency ALUs, fully pipelined multiply/divide units, and advanced load/store disambiguation with load-to-store forwarding and store coalescing for sustained high throughput.

Note: The checkpoint and recovery logic, along with its integration with the branch prediction and execution unit, is still under development. Therefore, the discussion here focuses solely on the parallelism mechanisms for non-branch instructions.

EDA Playground Link: https://www.edaplayground.com/x/MrPh

<img width="540" height="511" alt="image" src="https://github.com/user-attachments/assets/394dc993-0197-4c27-8535-eb751497dccc" />


**Specification:**

1. Front-End Cluster
•	3-wide fetch from instruction memory with PC redirection from branch predictor or next-PC logic.
•	Pre-decoder detects control instructions (branch/call/return) and trains branch predictor for speculative fetch.
•	Branch Prediction Unit (BPU): Gshare predictor (10-bit GHR, 1024-entry 2-bit counters).
•	BTB: 256-entry, 4-way set associative with pseudo-LRU replacement.
•	RAS: 16-entry return address stack for accurate return prediction.
•	BPQ: 16-entry queue for tracking speculative predictions and aiding recovery.
•	Prediction priority: RAS > BTB (calls) > Gshare+BTB (conditional branches).
•	Instruction Queue: 16-entry, 3-wide FIFO decouples fetch from backend.
•	Decoder: 3-wide parallel decoding.

2. Reorder Cluster (Tomasulo-based OOO)
•	Dispatch Buffer: Checks intra- and inter-bundle dependencies using RAT/ARF, does register renaming.
•	Allocation: All-or-nothing dispatch to reservation stations (RS) or load/store buffer (LSB); ROB entry assigned to each instruction for in-order retirement.
•	Resources: 6 ALU RS, 8 MUL/DIV RS, 16-entry LSB, 48-entry ROB.

3. Execution Cluster
•	Functional Units: 3 ALUs (1-cycle), 2 pipelined MUL/DIV units.
o	Multiplier: Radix-4 Booth, 3-cycle latency, 1/cycle throughput.
o	Divider: Radix-8, 11-cycle latency, fully pipelined.
•	Scheduler: Oldest-ready-first issue from RS to FU.
•	Completion Buffer: For result forwarding to CDB.

4. Memory Cluster
•	Address Generation Unit (AGU) issues to load/store queues (8 entries each).
•	Data Memory: 1 KB, 2 load pipes + 1 store pipe.
•	Memory Disambiguation: Load-to-store forwarding, store coalescing.

**Results Waveform:**

<img width="540" height="119" alt="image" src="https://github.com/user-attachments/assets/a1d40fbf-1e87-4289-817a-d50e59002b09" />

The waveform shows commit lane: commit_valid[0], commit_valid[1], and commit_valid[2] retiring instructions strictly in program order, beginning with ROB_ID = 0 and concluding with ROB_ID = 32. Instructions are also issued in program order to the execution units, but may complete out of order depending on operand availability and functional unit latency. The Reorder Buffer (ROB) then enforces in-order retirement to maintain precise architectural state. This approach allows out-of-order execution to hide latencies by overlapping independent instruction execution, thereby sustaining high instruction throughput.
I used a dependency heavy workload with long – latency mul /div Operations. We have 33 instructions completing in 24 cycles. Therefore IPC = 1.3750, (Note: For pure alu independent workload, the IPC will be close to 3.0)

**Instructions used for testing:**

Data Hazards occur when instructions exhibit dependencies on each other’s data, potentially causing incorrect results or stalls in a pipeline. The three primary types are:
1.	Read-After-Write (RAW): Also called a true dependency, occurs when an instruction needs to read a register that a prior instruction is still writing to. This enforces the correct program order to get the right data.
2.	Write-After-Write (WAW): Also called an output dependency, arises when two instructions write to the same register in sequence. The processor must ensure the final write is preserved in program order.
3.	Write-After-Read (WAR): Also called an anti-dependency, happens when an instruction writes to a register that a prior instruction reads. Without proper handling, this can cause the write to overwrite a value before the read completes
The following mix of instructions ensures the processor’s register renaming logic handles all three dependency types correctly, while the scheduling and forwarding paths are stressed with interleaved arithmetic, memory, and multiply/divide instructions of varying latencies. 

<img width="540" height="397" alt="image" src="https://github.com/user-attachments/assets/491dfc23-a16f-4903-80f1-c54779395a41" />
<img width="540" height="475" alt="image" src="https://github.com/user-attachments/assets/c9a4670e-c248-41fd-9d78-c5e4d511a77d" />

**CPU Top-Level Description:**

The design.sv file implements the complete top-level integration of a RISC-V Tomasulo out-of-order processor. It serves as the main system-on-chip (SoC) wrapper that connects all major processor components into a unified, parameterized architecture.

 
1. Frontend with Branch Prediction
Module: frontend_with_branch_predictor
Key Features:
•	Instruction Fetch: 3-wide instruction fetch with branch prediction
•	Branch Predictor: GShare predictor with BTB (64 sets, 4 ways)
•	Instruction Queue: 16-entry queue for fetch-decode buffering
•	RAS Support: 16-entry Return Address Stack for call/return prediction
•	BPQ Integration: Branch Prediction Queue for misprediction recovery
2. Instruction Decoder
Module: tomasulo_decoder
Key Features:
•	3-wide Decoding: Decodes up to 3 instructions per cycle
•	RISC-V RV32I Support: Full instruction set decoding
•	Error Detection: Comprehensive decoder error reporting
•	Stall Handling: Integrates with allocation backpressure
3. Tomasulo Core (Backend)
Module: alloc_reorder_retire
Key Features:
•	Register Renaming: 32-entry RAT with ROB-based renaming
•	Resource Allocation: 48-entry ROB, 12 ALU RS, 4 Mul/Div RS, 8 Branch RS
•	Out-of-Order Execution: Full Tomasulo algorithm implementation
•	Checkpoint Management: 16 checkpoints for branch misprediction recovery
•	In-Order Retirement: Ensures program order commitment
4. Functional Units
Module: func_unit
Key Features:
•	3 ALU Units: Arithmetic, logical, and comparison operations
•	2 Mul/Div Units: Multiplication and division with pipelined execution
•	Priority Dispatch: ROB-based priority for oldest instruction first
•	Result Generation: ALU_Result_t format for CDB integration

5. Branch Execution Unit
Module: branch_exec_unit
Key Features:
•	Branch Resolution: All conditional and unconditional branches
•	Misprediction Detection: Compares actual vs predicted outcomes
•	BPQ Coordination: Verifies predictions against BPQ entries
•	Recovery Signaling: Provides correct PC for misprediction recovery
6. Completion Buffer
Module: completion_buffer
Key Features:
•	Result Collection: Aggregates results from all functional units
•	CDB Distribution: 3-wide result broadcasting to reservation stations
•	Bypass Mode: Zero-latency forwarding when buffer empty
•	FIFO Ordering: Ensures proper result sequencing

7. Memory Subsystem
Module: top_memory_subsystem
Key Features:
•	Load/Store Buffer: 16-entry LSB for memory operation management
•	Memory Disambiguation: Store-to-load forwarding and violation detection
•	Multi-Port Memory: 2 load ports, 1 store port
•	Speculative Execution: Load speculation with recovery support
 
Pipeline Architecture
Pipeline Stages:
Fetch → Decode → Dispatch → Execute → Memory → Commit
Key Pipeline Features:
•	3-wide Superscalar: Processes up to 3 instructions per cycle
•	Out-of-Order Execution: Tomasulo algorithm for maximum parallelism
•	Speculative Execution: Branch prediction with recovery mechanisms
•	In-Order Retirement: Maintains program correctness

**Detailed Functionality & Working Algorithm of the Branch Predictor:**

Branch Predictor Overview (3-Stage Hybrid)
•	BTB (Branch Target Buffer): 4-way set associative, 64 sets (256 entries total), stores branch target addresses.
•	Gshare Predictor: 1024-entry, 2-bit saturating counters with a 10-bit global history register for branch direction.
•	RAS (Return Address Stack): 16-entry LIFO stack for CALL/RET predictions.
•	BPQ (Branch Prediction Queue): Tracks in-flight predictions for later verification.

<img width="569" height="412" alt="image" src="https://github.com/user-attachments/assets/027fa5a3-3d5f-4de0-ba95-2dc09aedb84d" />

**Checker Output :**

<img width="355" height="494" alt="image" src="https://github.com/user-attachments/assets/f33dde5c-a1eb-4ca2-9650-fcdcdd8a0c0f" />




