package tmpljit

// FillSumProgram builds the canonical workload as bytecode.
//
// Pseudocode:
//
//	i := 0
//	sum := 0
//	loop:
//	  t := i*2 + 3
//	  sum += t
//	  i++
//	  if i < n { goto loop }
//	return sum
//
// Register layout (must match emit_arm64.go's r2x mapping):
//
//	r0 = n      (input arg, never written)
//	r1 = i
//	r2 = sum
//	r3 = scratch (i*2 then i*2+3)
//	r4 = small constants
//	r5 = (i < n) ? 1 : 0
func FillSumProgram() Program {
	// Instruction indices in the program; used to compute the
	// Jnz signed offset (relative to the instruction *after* Jnz).
	const (
		iMovI = iota
		iMovSum
		iLoopMov2 // start of loop
		iMul
		iMov3
		iAdd3
		iAddSum
		iMov1
		iAddI
		iLt
		iJnz
		iRet
	)
	const jnzOff = iLoopMov2 - (iJnz + 1) // negative: jump backward to loop head
	return Program{
		{Op: OpMovImm, Dst: 1, Imm: 0},        // i = 0
		{Op: OpMovImm, Dst: 2, Imm: 0},        // sum = 0
		{Op: OpMovImm, Dst: 4, Imm: 2},        // r4 = 2  (loop head)
		{Op: OpMul, Dst: 3, A: 1, B: 4},       // r3 = i * 2
		{Op: OpMovImm, Dst: 4, Imm: 3},        // r4 = 3
		{Op: OpAdd, Dst: 3, A: 3, B: 4},       // r3 = r3 + 3
		{Op: OpAdd, Dst: 2, A: 2, B: 3},       // sum += r3
		{Op: OpMovImm, Dst: 4, Imm: 1},        // r4 = 1
		{Op: OpAdd, Dst: 1, A: 1, B: 4},       // i++
		{Op: OpLt, Dst: 5, A: 1, B: 0},        // r5 = (i < n)
		{Op: OpJnz, A: 5, Imm: int32(jnzOff)}, // if r5 != 0, goto loop
		{Op: OpRet, A: 2},                     // return sum
	}
}

// FloorGo is the hand-written Go reference. Same arithmetic as
// FillSumProgram. Acts as the theoretical floor for the
// benchmarks and the oracle for correctness tests.
func FloorGo(n int64) int64 {
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += i*2 + 3
	}
	return sum
}
