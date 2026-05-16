package tracejit

import "mochi/runtime/jit/tmpljit"

// Trace is a recorded loop body. It is a contiguous slice of the
// program's bytecode beginning at the loop header (the back-edge
// target) and ending with the loop's back-edge instruction. The
// recorder rewrites the back-edge into an explicit GuardLoopCont
// at compile time; the original Jnz is kept in Body for diagnostics
// but never emitted.
//
// Field semantics:
//
//	HeaderPC  bytecode PC of the loop header (back-edge target).
//	ExitPC    bytecode PC where the interpreter resumes when the
//	          loop's continuation guard fails. For a do-while loop
//	          like FillSumProgram, this is the instruction
//	          immediately after the Jnz, typically the OpRet.
//	Body      the recorded instructions, header..jnz inclusive.
//	GuardReg  the VM register holding the loop-continuation flag
//	          (the destination of the OpLt that feeds the Jnz).
type Trace struct {
	HeaderPC int
	ExitPC   int
	Body     []tmpljit.Instr
	GuardReg uint8
}

// TraceThreshold is the back-edge hit count after which the recorder
// fires. Kept low (vs. MEP-30's "compile-on-first-call" model)
// because tracing has measurable warmup cost and we want amortization
// to kick in inside the smallest reasonable benchmark.
const TraceThreshold = 8
