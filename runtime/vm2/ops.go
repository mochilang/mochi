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
	// allocating op produces a fresh *vmString reached through
	// Cell.Obj. Inline literals (<=5 bytes) pack into a tagSStr Cell
	// and skip allocation entirely.
	OpLoadStrK  // A = newString(Fn.StrConsts[B])
	OpConcatStr // A = newString(strAt(B).bytes ++ strAt(C).bytes)
	OpLenStr    // A = i64(len(strAt(B).bytes))
	OpIndexStr  // A = newString(one-byte slice strAt(B)[regs[C].Int()]); traps on OOB
	OpEqualStr  // A = bool(strEqual(strAt(B), strAt(C)))
	OpHashStr   // A = i64(strHash(strAt(B)))  // exposed mostly for map-key prep + tests

	// List subsystem (MEP-24 §3). Lists are growable []Cell buffers
	// reached through Cell.Obj; a list-valued Cell is tagPtr.
	OpNewList  // A = newList(capHint=B)
	OpListLen  // A = i64(len(listAt(B).data))
	OpListGet  // A = listAt(B).data[regs[C].Int()]; traps on OOB
	OpListSet  // listAt(regs[A]).data[regs[B].Int()] = regs[C]; traps on OOB
	OpListPush // listAt(regs[A]).data = append(..., regs[B]); amortized O(1)
	// Functional list append (MEP-36 §3.5). A = clone(B).push(C). Default
	// allocates a fresh *vmList; with Flags&InstrFlagBLastUse, the runtime
	// elides the clone, mutates B in place, and stores B's pointer into A.
	// This is the spec's anchor for `xs.append(1).append(2)` collapsing to
	// one backing array when the intermediate is dead.
	OpListAppend

	// Map subsystem (MEP-24 §4). Maps are mutable; Cell.Obj points to
	// a *vmMap whose entries is a Go map[any]Cell. Key normalization
	// (string bytes for str keys, scalar value for int/bool/null,
	// identity for other ptr) lives in mapKeyOf.
	OpNewMap // A = newMap()
	OpMapLen // A = i64(len(mapAt(regs[B]).entries))
	OpMapGet // A = mapAt(regs[B]).entries[mapKeyOf(regs[C])]; missing -> CNull()
	OpMapHas // A = bool(present in mapAt(regs[B]), key=regs[C])
	OpMapSet // mapAt(regs[A]).entries[mapKeyOf(regs[B])] = regs[C]
	OpMapDel // delete(mapAt(regs[A]).entries, mapKeyOf(regs[B]))

	// Float arithmetic subsystem (MEP-37 §3.2). Operands and result are
	// float64 Cells decoded via Cell.Float / encoded via CFloat. None of
	// these trap: divide-by-zero produces ±Inf or NaN per IEEE-754, and
	// the dispatch handler stays branch-free.
	OpLoadConstF // A = Consts[B] (float64 const, cell already produced by CFloat)
	OpAddF64     // A = B + C
	OpSubF64     // A = B - C
	OpMulF64     // A = B * C
	OpDivF64     // A = B / C
	OpNegF64     // A = -B
	OpAbsF64     // A = math.Abs(B)
	OpSqrtF64    // A = math.Sqrt(B)
	OpLessF64    // A = B < C  (bool)
	OpLessEqF64  // A = B <= C (bool)
	OpEqualF64   // A = B == C (bool) — IEEE equality, NaN compares unequal
	// Fused multiply-add. A = (B * C) + D. emit picks this over
	// OpMulF64+OpAddF64 when the multiply has exactly one use and that
	// use is the add (MEP-37 §3.2). The runtime uses math.FMA so the
	// rounding is single-rounded, matching Go's `math.FMA`.
	OpFmaF64
	// Numeric conversions. The BG kernels mix int loop counters with
	// float arithmetic (matrix index → coefficient, body count → step),
	// so the i64↔f64 round-trip lives next to the float opcodes rather
	// than in a separate conversion family.
	OpI64ToF64 // A = float64(regs[B].Int())
	OpF64ToI64 // A = int64(regs[B].Float())  (truncation toward zero)

	// Typed array subsystem (MEP-37 §3.3). Element type is fixed at
	// allocation; storage is a flat Go slice carried through Cell.Obj.
	// Get/Set ops trap on OOB. Length operand is taken as a register
	// holding an int Cell so dynamic sizing works without immediate
	// staging.
	OpNewF64Array // A = newF64Array(regs[B].Int())
	OpF64ArrLen   // A = i64(len(f64ArrAt(regs[B]).data))
	OpF64ArrGet   // A = CFloat(f64ArrAt(regs[B]).data[regs[C].Int()])
	OpF64ArrSet   // f64ArrAt(regs[A]).data[regs[B].Int()] = regs[C].Float()

	OpNewI64Array // A = newI64Array(regs[B].Int())
	OpI64ArrLen   // A = i64(len(i64ArrAt(regs[B]).data))
	OpI64ArrGet   // A = CInt(i64ArrAt(regs[B]).data[regs[C].Int()])
	OpI64ArrSet   // i64ArrAt(regs[A]).data[regs[B].Int()] = regs[C].Int()

	OpNewU8Array // A = newU8Array(regs[B].Int())
	OpU8ArrLen   // A = i64(len(u8ArrAt(regs[B]).data))
	OpU8ArrGet   // A = CInt(int64(u8ArrAt(regs[B]).data[regs[C].Int()]))
	OpU8ArrSet   // u8ArrAt(regs[A]).data[regs[B].Int()] = byte(regs[C].Int())
)
