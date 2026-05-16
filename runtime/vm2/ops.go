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
	// Fused i64 compare + conditional branch: jump to C when the
	// predicate holds, fall through otherwise. Saves one dispatch +
	// one bool register round-trip per loop test vs the unfused
	// (OpLessI64 → OpJumpIfFalse) pair. Both signs are provided so emit
	// can pick the form whose fallthrough matches the next block in
	// layout order and skip the trailing OpJump too.
	OpJumpIfLessI64      // if A < B  then IP = C
	OpJumpIfLessEqI64    // if A <= B then IP = C
	OpJumpIfGreaterI64   // if A > B  then IP = C
	OpJumpIfGreaterEqI64 // if A >= B then IP = C
	OpJumpIfEqualI64     // if A == B then IP = C
	OpJumpIfNotEqualI64  // if A != B then IP = C
	OpCall                  // A = call Funcs[B], args [C..C+D); RetReg=A
	OpTailCall              // tail-call Funcs[A] with args [B..B+C); reuses frame
	OpReturn                // return A to caller's RetReg
)
