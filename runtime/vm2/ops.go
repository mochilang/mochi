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
	// A = B + sign-extend(C). Fuses an OpLoadConstI of a 32-bit-fitting
	// integer immediately followed by an OpAddI64 that consumes it as
	// its only use. Saves one dispatch + one const-pool load per loop
	// step counter (e.g. `i = i + 1` lowers to one of these instead of
	// load-const-then-add).
	OpAddI64K
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
	// Same-function tail call. Params already live in regs[0..np) thanks
	// to parallel moves the emitter inserted into the param slots, so
	// the dispatch handler just rewinds IP. No frame swap, no memmove,
	// no callee lookup. Hot self-recursion loops (every `for` lowered
	// to a tail-recursive helper) go through this path.
	OpTailCallSelf
	OpReturn // return A to caller's RetReg

	// String subsystem (MEP-24 §2). Strings are immutable; every
	// allocating op produces a fresh *vmString registered in
	// vm.Objects. Cell encoding is tagPtr whose payload is the
	// Objects index.
	OpLoadStrK  // A = newString(Fn.StrConsts[B])
	OpConcatStr // A = newString(strAt(B).bytes ++ strAt(C).bytes)
	OpLenStr    // A = i64(len(strAt(B).bytes))
	OpIndexStr  // A = newString(one-byte slice strAt(B)[regs[C].Int()]); traps on OOB
	OpEqualStr  // A = bool(strEqual(strAt(B), strAt(C)))
	OpHashStr   // A = i64(strHash(strAt(B)))  // exposed mostly for map-key prep + tests
)
