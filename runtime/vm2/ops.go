package vm2

// Op is the vm2 opcode. The set is intentionally small for step 3;
// subsequent steps add typed float/string/list/map families per the
// MEP-21 v2 opcode table.
type Op uint8

const (
	OpHalt        Op = iota // never reached; sentinel for verifier
	OpLoadConstI            // A = Consts[B]
	OpMove                  // A = B
	OpAddI64                // A = B + C (signed 48-bit; overflow is implementation-defined)
	OpSubI64                // A = B - C
	OpMulI64                // A = B * C
	OpDivI64                // A = B / C (truncated; division by zero traps)
	OpModI64                // A = B % C (truncated, sign of dividend; mod by zero traps)
	OpLessI64               // A = B < C  (bool)
	OpLessEqI64             // A = B <= C (bool)
	OpEqualI64              // A = B == C (bool)
	OpJump                  // IP = A
	OpJumpIfFalse           // if !A then IP = B
	OpCall                  // A = call Funcs[B], args [C..C+D); RetReg=A
	OpTailCall              // tail-call Funcs[A] with args [B..B+C); reuses frame
	OpReturn                // return A to caller's RetReg
)
