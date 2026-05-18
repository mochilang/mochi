//go:build arm64

package vm3jit

import (
	"fmt"

	"mochi/runtime/vm3"
)

// Register pinning:
//
//	regsI64[r] -> x(9 + r)        for r in [0, 7)   (caller-saved)
//	regsI64[r] -> x(19 + r - 7)   for r in [7, 17)  (callee-saved)
//
// x9..x15 are AArch64 caller-saved temporaries; the JIT can clobber
// them freely. x19..x28 are callee-saved; functions that touch any of
// those slots push them as STP/LDP pairs in the prologue/epilogue.
// x0 carries the regsI64 base pointer (set by the trampoline).
// x1 carries the optional *int64 status word pointer (set by
// trampoline.CallStatus). The JIT writes a non-zero status code via
// [x1] on a deopt path (divide-by-zero etc.) before returning. Phase
// 6.1c+ codegen assumes x1 is always live and never clobbers it; the
// happy path leaves *status at its caller-pre-zeroed value.
// x16/x17 are intra-procedure scratches available to any encoder.
//
// vm3 i64 registers are stored as raw int64 (no NaN-box, no tag). A
// single LDR/STR moves a value between the regsI64 stack window and
// its pinned host register; arithmetic runs directly on the host
// register with no tag arithmetic. This is the key simplification
// over vm2jit and the reason a small instruction count per opcode is
// achievable for arithmetic kernels.

func r2x(r uint16) uint32 {
	if r < 7 {
		return uint32(r) + 9
	}
	return uint32(r) - 7 + 19
}

// r2d maps a vm3 f64 register slot to the AArch64 SIMD register
// number (V0..V7 / D0..D7). Caller pre-checks r < maxF64RegsARM64.
// The same register number is used for both V (128-bit) and D
// (lower 64-bit) views; the instruction encoding chooses the view.
func r2d(r uint16) uint32 { return uint32(r) }

// computeCallSpills runs a backward liveness pass over fn.Code and
// returns, for each bytecode index, the bitset of caller-saved i64
// registers (bank 0..6, mapped to x9..x15) that must be spilled to the
// caller window across the call. For non-call opcodes the entry is 0.
// Self-recursive OpCallI64 sites use this to skip spilling dead regs.
// Shared helpers liveSuccUnion / defUseI64 live in lower_common.go.
func computeCallSpills(fn *vm3.Function) []uint32 {
	n := len(fn.Code)
	spills := make([]uint32, n)
	liveIn := make([]uint32, n+1) // liveIn[n] = 0 (post-exit)
	liveOut := make([]uint32, n)
	for changed := true; changed; {
		changed = false
		for i := n - 1; i >= 0; i-- {
			op := fn.Code[i]
			out := liveSuccUnion(fn, i, liveIn)
			d, u := defUseI64(fn, op)
			in := u | (out &^ d)
			if out != liveOut[i] || in != liveIn[i] {
				liveOut[i] = out
				liveIn[i] = in
				changed = true
			}
		}
	}
	for i, op := range fn.Code {
		if op.Code != vm3.OpCallI64 {
			continue
		}
		mask := uint32(0x7F) // caller-saved (regs 0..6 → x9..x15)
		dstBit := uint32(1) << op.A
		spills[i] = (liveOut[i] &^ dstBit) & mask
	}
	return spills
}

// numCalleeSavedPairs returns the number of x19..x28 STP/LDP pairs the
// frame for fn must push. Each pair covers two register slots. The
// final pair is pushed even if only one of its two regs is live, since
// AArch64 STP is a single 16-byte op and partial pairs would waste
// alignment without saving anything.
func numCalleeSavedPairs(fn *vm3.Function) int {
	n := int(fn.NumRegsI64)
	if n > maxI64Regs {
		n = maxI64Regs
	}
	if n <= 7 {
		return 0
	}
	return (n - 7 + 1) / 2
}

// hasRegRegDivMod reports whether fn contains any reg-reg OpDivI64 or
// OpModI64 (the K-form variants reject /0 at Compile time and need no
// runtime guard, so they do not contribute to the deopt block).
func hasRegRegDivMod(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpDivI64 || op.Code == vm3.OpModI64 {
			return true
		}
	}
	return false
}

// isNonLeaf reports whether fn issues any native BL (currently only
// self-recursive OpCallI64 is lowered to BL; everything else routes
// through the interpreter and so does not need x30 saved). Non-leaf
// fns must push x29:x30 as an extra outermost STP pair so the JIT'd
// callee can use BL/RET without clobbering the caller's return
// address.
func isNonLeaf(fn *vm3.Function) bool {
	for _, op := range fn.Code {
		if op.Code == vm3.OpCallI64 {
			return true
		}
	}
	return false
}

// numLRPair returns 1 if fn needs the x29:x30 outermost STP/LDP pair,
// 0 otherwise. The pair is independent of numCalleeSavedPairs and lives
// in addition to those (x19..x28 register saves).
func numLRPair(fn *vm3.Function) int {
	if isNonLeaf(fn) {
		return 1
	}
	return 0
}

// deoptBlockWordsARM64 returns the number of instruction words the
// per-function shared deopt block occupies, or 0 if no opcode in fn can
// take a deopt path. The block lives at the very end of the JIT stream;
// every CBZ-guarded check branches to its start.
//
// Block layout (when emitted):
//
//	MOV  x16, #status_code
//	STR  x16, [x1]                  ; write *status
//	<pop callee-saved pairs>        ; numCalleeSavedPairs LDPs
//	RET
func deoptBlockWordsARM64(fn *vm3.Function) int {
	if !hasRegRegDivMod(fn) {
		return 0
	}
	return 2 + numCalleeSavedPairs(fn) + numLRPair(fn) + 1
}

// emitDeoptBlockARM64 emits the shared deopt block for fn at the end
// of the instruction stream. statusCode is the int64 value the JIT
// writes to *(x1) before unwinding. Callers compute the branch offset
// from each guard site to this block via pcMap[len(fn.Code)] (the word
// index where the block starts).
func emitDeoptBlockARM64(fn *vm3.Function, statusCode int64) []uint32 {
	ws := movImm64(16, statusCode)
	ws = append(ws, str64(16, 1, 0))
	ws = emitFrameEpilogueARM64(ws, numCalleeSavedPairs(fn), numLRPair(fn))
	ws = append(ws, ret())
	return ws
}

// lowerARM64 returns the AArch64 instruction-word stream for fn. Two
// passes: pass 1 computes pcMap[i] = word index where the lowering of
// bytecode i starts; pass 2 emits real instructions, resolving branch
// destinations through pcMap. opts.SelfIdx, when >= 0, enables self-
// recursive OpCallI64 to lower to a native BL inside this page.
func lowerARM64(fn *vm3.Function, opts Options) ([]uint32, error) {
	if fn.NumRegsF64 > 0 && isNonLeaf(fn) {
		return nil, fmt.Errorf("%w: %s has both f64 regs and OpCallI64 (Phase 6.2b leaves f64+self-recursion to a later sub-phase)",
			ErrNotImplemented, fn.Name)
	}
	pairs := numCalleeSavedPairs(fn)
	lrPair := numLRPair(fn)
	prologueWords := lrPair + pairs + int(fn.NumRegsI64) + int(fn.NumRegsF64)
	spillSets := computeCallSpills(fn)

	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = prologueWords
	for i, op := range fn.Code {
		n, err := wordCountARM64(fn, op, opts, spillSets, i)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d: %w", fn.Name, i, err)
		}
		pcMap[i+1] = pcMap[i] + n
	}

	deoptStart := pcMap[len(fn.Code)]
	total := deoptStart + deoptBlockWordsARM64(fn)
	ws := make([]uint32, 0, total)

	// Prologue: push x29:x30 (outermost) if non-leaf, then push
	// callee-saved pairs, then load each live reg from [x0, #r*8] into
	// its pinned host register, then load each live f64 reg from
	// [x2, #r*8] into V0..V7.
	if lrPair == 1 {
		ws = append(ws, stpPreIdx64(29, 30, 31, -16))
	}
	for k := 0; k < pairs; k++ {
		ws = append(ws, stpPreIdx64(uint32(2*k+19), uint32(2*k+20), 31, -16))
	}
	for r := uint32(0); r < uint32(fn.NumRegsI64); r++ {
		ws = append(ws, ldr64(r2x(uint16(r)), 0, r))
	}
	for r := uint32(0); r < uint32(fn.NumRegsF64); r++ {
		ws = append(ws, ldrD(r2d(uint16(r)), 2, r))
	}

	for i, op := range fn.Code {
		emit, err := emitInstrARM64(fn, op, i, pcMap, deoptStart, opts, spillSets)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: %w", fn.Name, i, op.Code, err)
		}
		if got, want := len(emit), pcMap[i+1]-pcMap[i]; got != want {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: emitted %d words, predicted %d",
				fn.Name, i, op.Code, got, want)
		}
		ws = append(ws, emit...)
	}
	if len(ws) != deoptStart {
		return nil, fmt.Errorf("vm3jit/%s: code stream %d words, predicted %d",
			fn.Name, len(ws), deoptStart)
	}
	if hasRegRegDivMod(fn) {
		ws = append(ws, emitDeoptBlockARM64(fn, StatusDivByZero)...)
	}
	if len(ws) != total {
		return nil, fmt.Errorf("vm3jit/%s: final stream %d words, predicted %d",
			fn.Name, len(ws), total)
	}
	return ws, nil
}

// emitFrameEpilogueARM64 appends LDP instructions to ws in REVERSE
// order of the prologue's pushes so each pop matches the topmost STP
// frame and SP returns to its entry value. pairs is the number of
// x19..x28 STP pairs, lrPair is 1 if x29:x30 was pushed as the
// outermost pair (non-leaf fns).
func emitFrameEpilogueARM64(ws []uint32, pairs, lrPair int) []uint32 {
	for k := pairs - 1; k >= 0; k-- {
		ws = append(ws, ldpPostIdx64(uint32(2*k+19), uint32(2*k+20), 31, 16))
	}
	if lrPair == 1 {
		ws = append(ws, ldpPostIdx64(29, 30, 31, 16))
	}
	return ws
}

// wordCountARM64 returns the exact number of 32-bit words emitInstrARM64
// will produce for op. Used by pass 1 to lay out pcMap. spillSets and
// idx are only consulted for OpCallI64; for other opcodes the spill
// set is irrelevant.
func wordCountARM64(fn *vm3.Function, op vm3.Op, opts Options, spillSets []uint32, idx int) (int, error) {
	switch op.Code {
	case vm3.OpConstI64K:
		// movImm64 emits 1..4 movz/movk words depending on the sign-extended
		// 16-bit constant. For C in [0, 65535] one movz; for negative values
		// a movn or up to 4 movz/movk pairs.
		return movImm64WordCount(int64(op.C)), nil
	case vm3.OpConstI64KW:
		idx := int(uint16(op.C))
		if idx >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstI64KW idx %d out of range", ErrNotImplemented, idx)
		}
		return movImm64WordCount(fn.Consts[idx].Int()), nil
	case vm3.OpMovI64:
		return 1, nil
	case vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64:
		return 1, nil
	case vm3.OpDivI64:
		// CBZ xC, deopt ; SDIV xA, xB, xC.
		return 2, nil
	case vm3.OpModI64:
		// CBZ xC, deopt ; SDIV x17, xB, xC ; MSUB xA, x17, xC, xB.
		return 3, nil
	case vm3.OpNegI64:
		return 1, nil
	case vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K:
		// MOV imm into x16; <op> xd, xn, x16.
		return movImm64WordCount(int64(op.C)) + 1, nil
	case vm3.OpDivI64K, vm3.OpModI64K:
		if op.C == 0 {
			return 0, fmt.Errorf("%w: opcode %d divide-by-zero immediate", ErrNotImplemented, op.Code)
		}
		if op.Code == vm3.OpDivI64K {
			return movImm64WordCount(int64(op.C)) + 1, nil
		}
		// ModI64K: MOV imm into x16; SDIV x17, xb, x16; MSUB xa, x17, x16, xb.
		return movImm64WordCount(int64(op.C)) + 2, nil
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		// CMP xA, xB; B.cond <target>.
		return 2, nil
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		// MOV imm into x16; CMP xA, x16; B.cond <target>.
		return movImm64WordCount(int64(int16(op.B))) + 2, nil
	case vm3.OpJump:
		return 1, nil
	case vm3.OpReturnI64:
		// MOV x0, xA; <pop callee-saved frame>; <pop x29:x30 if non-leaf>; RET.
		return 2 + numCalleeSavedPairs(fn) + numLRPair(fn), nil
	case vm3.OpReturnConstK:
		// movImm64 into x0; <pop callee-saved frame>; <pop x29:x30>; RET.
		return movImm64WordCount(int64(op.C)) + numCalleeSavedPairs(fn) + numLRPair(fn) + 1, nil
	case vm3.OpReturnF64:
		// FMOV x0, D<retSlot>; <pop callee-saved frame>; <pop x29:x30>; RET.
		// Bit-cast the f64 in d<A> to a uint64 in x0 so the trampoline
		// return channel carries either bank uniformly.
		return 2 + numCalleeSavedPairs(fn) + numLRPair(fn), nil

	case vm3.OpConstF64K:
		// MOV imm into x16 with the IEEE 754 bit-pattern, then
		// FMOV D<A>, x16 to bit-cast into the SIMD register. No
		// constant pool lookup at runtime; the bits are baked into
		// the JIT'd stream.
		idx := int(uint16(op.C))
		if idx >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: ConstF64K idx %d out of range", ErrNotImplemented, idx)
		}
		bits := int64(uint64(fn.Consts[idx]))
		return movImm64WordCount(bits) + 1, nil

	case vm3.OpMovF64:
		return 1, nil
	case vm3.OpAddF64, vm3.OpSubF64, vm3.OpMulF64, vm3.OpDivF64:
		return 1, nil
	case vm3.OpNegF64:
		return 1, nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		// FCMP Dn, Dm ; B.cond <target>.
		return 2, nil

	case vm3.OpI64ToF64:
		return 1, nil
	case vm3.OpF64ToI64:
		return 1, nil
	case vm3.OpCallI64:
		// Self-recursion only. Anything else routes back through the
		// interpreter via ErrNotImplemented.
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return 0, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		nSpill := popcount32(spillSets[idx])
		nArgs := fn.NumI64Params()
		// nSpill STRs + nArgs STRs + STP x0 + ADD x0 + BL + MOV x16,x0 +
		// LDP x0 + nSpill LDRs + MOV pinned[dst],x16.
		return 2*nSpill + nArgs + 6, nil
	default:
		return 0, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// emitInstrARM64 emits the AArch64 instructions for op at bytecode
// index idx. pcMap[i] is the word offset where instruction i's
// lowering begins (and pcMap[len(Code)] is the word offset of the
// fall-through past the last instruction; that is also the deopt
// block start when one is emitted). deoptStart is supplied separately
// so guard-checked opcodes (Div/Mod) can compute CBZ offsets without
// re-deriving it.
func emitInstrARM64(fn *vm3.Function, op vm3.Op, idx int, pcMap []int, deoptStart int, opts Options, spillSets []uint32) ([]uint32, error) {
	xA := r2x(op.A)
	xB := r2x(op.B)

	switch op.Code {
	case vm3.OpConstI64K:
		return movImm64(xA, int64(op.C)), nil

	case vm3.OpConstI64KW:
		v := fn.Consts[int(uint16(op.C))].Int()
		return movImm64(xA, v), nil

	case vm3.OpMovI64:
		return []uint32{movReg(xA, xB)}, nil

	case vm3.OpAddI64:
		xC := r2x(uint16(op.C))
		return []uint32{addReg(xA, xB, xC)}, nil
	case vm3.OpSubI64:
		xC := r2x(uint16(op.C))
		return []uint32{subReg(xA, xB, xC)}, nil
	case vm3.OpMulI64:
		xC := r2x(uint16(op.C))
		return []uint32{mulReg(xA, xB, xC)}, nil
	case vm3.OpDivI64:
		xC := r2x(uint16(op.C))
		off, err := branchOff(pcMap[idx], deoptStart, 19)
		if err != nil {
			return nil, fmt.Errorf("DivI64 deopt branch: %w", err)
		}
		return []uint32{cbz64(xC, off), sdivReg(xA, xB, xC)}, nil
	case vm3.OpModI64:
		xC := r2x(uint16(op.C))
		off, err := branchOff(pcMap[idx], deoptStart, 19)
		if err != nil {
			return nil, fmt.Errorf("ModI64 deopt branch: %w", err)
		}
		return []uint32{
			cbz64(xC, off),
			sdivReg(17, xB, xC),
			msubReg(xA, 17, xC, xB),
		}, nil
	case vm3.OpNegI64:
		return []uint32{negReg(xA, xB)}, nil

	case vm3.OpAddI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, addReg(xA, xB, 16)), nil
	case vm3.OpSubI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, subReg(xA, xB, 16)), nil
	case vm3.OpMulI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, mulReg(xA, xB, 16)), nil
	case vm3.OpDivI64K:
		ws := movImm64(16, int64(op.C))
		return append(ws, sdivReg(xA, xB, 16)), nil
	case vm3.OpModI64K:
		// x17 = xB / x16 ; xA = xB - x17 * x16.
		ws := movImm64(16, int64(op.C))
		ws = append(ws, sdivReg(17, xB, 16))
		ws = append(ws, msubReg(xA, 17, 16, xB))
		return ws, nil

	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		cond := condForCmpReg(op.Code)
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx] + 1
		off, err := branchOff(srcWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		return []uint32{cmpReg(xA, xB), bCond(cond, off)}, nil

	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		cond := condForCmpKImm(op.Code)
		imm := int64(int16(op.B))
		ws := movImm64(16, imm)
		cmpWord := pcMap[idx] + len(ws)
		bWord := cmpWord + 1
		dstWord := pcMap[int(uint16(op.C))]
		off, err := branchOff(bWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		ws = append(ws, cmpReg(xA, 16), bCond(cond, off))
		return ws, nil

	case vm3.OpJump:
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx]
		off, err := branchOff(srcWord, dstWord, 26)
		if err != nil {
			return nil, err
		}
		return []uint32{bImm(off)}, nil

	case vm3.OpReturnI64:
		// MOV x0, xA BEFORE the LDPs: if xA is one of x19..x28 the
		// epilogue would clobber it.
		ws := []uint32{movReg(0, xA)}
		ws = emitFrameEpilogueARM64(ws, numCalleeSavedPairs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpReturnConstK:
		ws := movImm64(0, int64(op.C))
		ws = emitFrameEpilogueARM64(ws, numCalleeSavedPairs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpReturnF64:
		// FMOV x0, D<A>: bit-cast the f64 result to a uint64 in x0
		// so the trampoline returns the bit pattern. Caller does
		// math.Float64frombits to recover the f64. Then run the
		// epilogue exactly like OpReturnI64.
		ws := []uint32{fmovXD(0, r2d(op.A))}
		ws = emitFrameEpilogueARM64(ws, numCalleeSavedPairs(fn), numLRPair(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpConstF64K:
		dA := r2d(op.A)
		bits := int64(uint64(fn.Consts[int(uint16(op.C))]))
		ws := movImm64(16, bits)
		ws = append(ws, fmovDX(dA, 16))
		return ws, nil

	case vm3.OpMovF64:
		return []uint32{fmovDD(r2d(op.A), r2d(op.B))}, nil
	case vm3.OpAddF64:
		return []uint32{faddD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpSubF64:
		return []uint32{fsubD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpMulF64:
		return []uint32{fmulD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpDivF64:
		return []uint32{fdivD(r2d(op.A), r2d(op.B), r2d(uint16(op.C)))}, nil
	case vm3.OpNegF64:
		return []uint32{fnegD(r2d(op.A), r2d(op.B))}, nil

	case vm3.OpCmpEqF64Br, vm3.OpCmpNeF64Br,
		vm3.OpCmpLtF64Br, vm3.OpCmpLeF64Br,
		vm3.OpCmpGtF64Br, vm3.OpCmpGeF64Br:
		cond := condForCmpF64(op.Code)
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx] + 1
		off, err := branchOff(srcWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		return []uint32{fcmpD(r2d(op.A), r2d(op.B)), bCond(cond, off)}, nil

	case vm3.OpI64ToF64:
		return []uint32{scvtfDX(r2d(op.A), r2x(op.B))}, nil
	case vm3.OpF64ToI64:
		return []uint32{fcvtzsXD(r2x(op.A), r2d(op.B))}, nil

	case vm3.OpCallI64:
		if opts.SelfIdx < 0 || int(uint16(op.C)) != opts.SelfIdx {
			return nil, fmt.Errorf("%w: CallI64 to non-self idx %d (SelfIdx=%d)",
				ErrNotImplemented, uint16(op.C), opts.SelfIdx)
		}
		spillMask := spillSets[idx]
		nSpill := popcount32(spillMask)
		nArgs := fn.NumI64Params()
		nRegsI64 := int(fn.NumRegsI64)
		ws := make([]uint32, 0, 2*nSpill+nArgs+6)
		// Spill only the caller-saved pinned regs that are live across
		// this call, computed by backward liveness in computeCallSpills.
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, str64(uint32(9+r), 0, uint32(r)))
			}
		}
		// Write args into callee's window slots at offset NumRegsI64*8.
		for k := 0; k < nArgs; k++ {
			src := r2x(op.B + uint16(k))
			ws = append(ws, str64(src, 0, uint32(nRegsI64+k)))
		}
		// Save caller's x0 (regs base), bump to callee window, BL.
		ws = append(ws, stpPreIdx64(0, 31, 31, -16))
		ws = append(ws, addImm64(0, 0, uint32(nRegsI64*8)))
		entryWord := 0
		callSiteWord := pcMap[idx] + len(ws)
		off, err := branchOff(callSiteWord, entryWord, 26)
		if err != nil {
			return nil, fmt.Errorf("CallI64 BL: %w", err)
		}
		ws = append(ws, bl(off))
		// Capture result into x16, restore caller's x0.
		ws = append(ws, movReg(16, 0))
		ws = append(ws, ldpPostIdx64(0, 31, 31, 16))
		// Reload only the regs we spilled.
		for r := uint16(0); r < 7; r++ {
			if spillMask&(1<<r) != 0 {
				ws = append(ws, ldr64(uint32(9+r), 0, uint32(r)))
			}
		}
		// Move result into caller's pinned dst register.
		ws = append(ws, movReg(xA, 16))
		return ws, nil

	default:
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// condForCmpReg returns the AArch64 B.cond condition code for a vm3
// reg-reg compare-and-branch opcode.
func condForCmpReg(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqI64Br:
		return 0x0 // EQ
	case vm3.OpCmpNeI64Br:
		return 0x1 // NE
	case vm3.OpCmpLtI64Br:
		return 0xB // LT
	case vm3.OpCmpLeI64Br:
		return 0xD // LE
	case vm3.OpCmpGtI64Br:
		return 0xC // GT
	case vm3.OpCmpGeI64Br:
		return 0xA // GE
	}
	return 0
}

// condForCmpKImm returns the AArch64 B.cond condition code for a vm3
// reg-imm compare-and-branch opcode.
func condForCmpKImm(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqI64KBr:
		return 0x0
	case vm3.OpCmpNeI64KBr:
		return 0x1
	case vm3.OpCmpLtI64KBr:
		return 0xB
	case vm3.OpCmpLeI64KBr:
		return 0xD
	case vm3.OpCmpGtI64KBr:
		return 0xC
	case vm3.OpCmpGeI64KBr:
		return 0xA
	}
	return 0
}

// branchOff computes the signed instruction-word displacement from
// srcWord (the word that contains the branch) to dstWord, and checks
// that it fits in the given bit-width.
func branchOff(srcWord, dstWord, bits int) (int32, error) {
	diff := dstWord - srcWord
	lim := 1 << (bits - 1)
	if diff >= lim || diff < -lim {
		return 0, fmt.Errorf("branch offset %d out of %d-bit range", diff, bits)
	}
	return int32(diff), nil
}

// --- AArch64 instruction encoders ---
//
// Mechanically these mirror vm2jit's encoders; vm3jit uses only the
// subset needed for i64 arithmetic + control flow (no NaN-box dance,
// no float, no list/map fast paths).

func movz(xd, imm16, hw uint32) uint32 {
	return 0xD2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func movk(xd, imm16, hw uint32) uint32 {
	return 0xF2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func movn(xd, imm16, hw uint32) uint32 {
	return 0x92800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

func addReg(xd, xn, xm uint32) uint32 {
	return 0x8B000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func subReg(xd, xn, xm uint32) uint32 {
	return 0xCB000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func negReg(xd, xm uint32) uint32 {
	// NEG xd, xm == SUB xd, xzr, xm (Rn = 31).
	return 0xCB0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}

func mulReg(xd, xn, xm uint32) uint32 {
	// MUL xd, xn, xm == MADD xd, xn, xm, xzr (Ra = 31).
	return 0x9B007C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func sdivReg(xd, xn, xm uint32) uint32 {
	return 0x9AC00C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func msubReg(xd, xn, xm, xa uint32) uint32 {
	// MSUB xd, xn, xm, xa: bit 15 (o0) = 1.
	return 0x9B008000 | ((xm & 0x1F) << 16) | ((xa & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}

func movReg(xd, xm uint32) uint32 {
	return 0xAA0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}

func ldr64(xt, xn, imm12 uint32) uint32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// str64 encodes STR Xt, [Xn, #imm12*8] (unsigned-offset 64-bit). Used
// by the deopt block to write the status code through *(x1).
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// cbz64 encodes CBZ Xt, <pc-rel> (64-bit, branch if Xt is zero).
// off19 is a signed instruction-word offset; caller must pre-validate
// that it fits in 19 bits via branchOff(_, _, 19).
func cbz64(xt uint32, off19 int32) uint32 {
	return 0xB4000000 | (uint32(off19&0x7FFFF) << 5) | (xt & 0x1F)
}

func bImm(off26 int32) uint32 {
	return 0x14000000 | uint32(off26&0x3FFFFFF)
}

// bl encodes BL <pc-rel>: same imm26 field as B, top opcode bit set so
// the CPU writes x30 with the return address. Used by self-recursive
// OpCallI64 to call back into the same JIT page.
func bl(off26 int32) uint32 {
	return 0x94000000 | uint32(off26&0x3FFFFFF)
}

// addImm64 encodes ADD Xd, Xn, #imm12 (64-bit, shift=0). Used by the
// OpCallI64 lowering to bump x0 from caller's regs base to callee's
// window (offset NumRegsI64*8 bytes).
func addImm64(xd, xn, imm12 uint32) uint32 {
	return 0x91000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

func bCond(cond uint32, off19 int32) uint32 {
	return 0x54000000 | (uint32(off19&0x7FFFF) << 5) | (cond & 0xF)
}

func ret() uint32 { return 0xD65F03C0 }

// stpPreIdx64 encodes STP Xt1, Xt2, [Xn, #imm]! (64-bit pre-index,
// writeback). imm is a 7-bit signed multiple of 8 bytes, so the
// instruction-level immediate is imm/8. Used to push callee-saved
// pairs in 16-byte chunks (imm = -16).
func stpPreIdx64(xt1, xt2, xn uint32, imm int32) uint32 {
	imm7 := uint32(imm/8) & 0x7F
	return 0xA9800000 | (imm7 << 15) | ((xt2 & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xt1 & 0x1F)
}

// ldpPostIdx64 encodes LDP Xt1, Xt2, [Xn], #imm (64-bit post-index,
// writeback). imm is a 7-bit signed multiple of 8. Used to pop the
// callee-saved pairs in 16-byte chunks (imm = +16).
func ldpPostIdx64(xt1, xt2, xn uint32, imm int32) uint32 {
	imm7 := uint32(imm/8) & 0x7F
	return 0xA8C00000 | (imm7 << 15) | ((xt2 & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xt1 & 0x1F)
}

// movImm64 emits the shortest movz/movn/movk sequence that loads v
// into xd. Negative values prefer movn (one-instruction load for
// -65536..-1 etc.); positive values use movz then movk for each
// non-zero 16-bit lane. Result length is movImm64WordCount(v).
func movImm64(xd uint32, v int64) []uint32 {
	if v == 0 {
		return []uint32{movz(xd, 0, 0)}
	}
	if v > 0 && v <= 0xFFFF {
		return []uint32{movz(xd, uint32(v), 0)}
	}
	if v < 0 && v >= -0x10000 {
		return []uint32{movn(xd, uint32(^v), 0)}
	}
	// General 4-lane build: emit movz for the first non-zero lane,
	// then movk for every subsequent non-zero lane.
	lanes := [4]uint32{
		uint32(v) & 0xFFFF,
		uint32(v>>16) & 0xFFFF,
		uint32(v>>32) & 0xFFFF,
		uint32(v>>48) & 0xFFFF,
	}
	ws := make([]uint32, 0, 4)
	first := -1
	for i, l := range lanes {
		if l != 0 {
			first = i
			break
		}
	}
	if first < 0 {
		return []uint32{movz(xd, 0, 0)}
	}
	ws = append(ws, movz(xd, lanes[first], uint32(first)))
	for i := first + 1; i < 4; i++ {
		if lanes[i] != 0 {
			ws = append(ws, movk(xd, lanes[i], uint32(i)))
		}
	}
	return ws
}

// movImm64WordCount predicts the length of movImm64(xd, v) without
// allocating. Must agree with movImm64 byte-for-byte.
func movImm64WordCount(v int64) int {
	if v == 0 {
		return 1
	}
	if v > 0 && v <= 0xFFFF {
		return 1
	}
	if v < 0 && v >= -0x10000 {
		return 1
	}
	lanes := [4]uint32{
		uint32(v) & 0xFFFF,
		uint32(v>>16) & 0xFFFF,
		uint32(v>>32) & 0xFFFF,
		uint32(v>>48) & 0xFFFF,
	}
	n := 0
	for _, l := range lanes {
		if l != 0 {
			n++
		}
	}
	if n == 0 {
		return 1
	}
	return n
}

// --- AArch64 SIMD/FP encoders (scalar double, V0..V31) ---
//
// All encoders take and return raw register numbers (0..31) in the
// "D" (lower 64-bit lane) view of the SIMD bank. The full-vector "V"
// view shares the same register number.

// ldrD encodes LDR Dt, [Xn, #imm12*8] (load 64-bit FP register from
// memory at base+imm12*8). Used by the prologue to fill the pinned
// f64 slots from regsF64 (x2).
func ldrD(dt, xn, imm12 uint32) uint32 {
	return 0xFD400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (dt & 0x1F)
}

// fmovDX encodes FMOV Dd, Xn (general -> SIMD bit-cast).
// Used by OpConstF64K after loading the IEEE 754 bit-pattern into x16.
func fmovDX(dd, xn uint32) uint32 {
	return 0x9E670000 | ((xn & 0x1F) << 5) | (dd & 0x1F)
}

// fmovXD encodes FMOV Xd, Dn (SIMD -> general bit-cast).
// Used by OpReturnF64 to copy d<retSlot> into x0 so the trampoline
// can return the raw IEEE 754 bit pattern.
func fmovXD(xd, dn uint32) uint32 {
	return 0x9E660000 | ((dn & 0x1F) << 5) | (xd & 0x1F)
}

// fmovDD encodes FMOV Dd, Dn (SIMD reg-reg copy, double).
func fmovDD(dd, dn uint32) uint32 {
	return 0x1E604000 | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// faddD encodes FADD Dd, Dn, Dm (scalar double).
func faddD(dd, dn, dm uint32) uint32 {
	return 0x1E602800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fsubD encodes FSUB Dd, Dn, Dm.
func fsubD(dd, dn, dm uint32) uint32 {
	return 0x1E603800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fmulD encodes FMUL Dd, Dn, Dm.
func fmulD(dd, dn, dm uint32) uint32 {
	return 0x1E600800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fdivD encodes FDIV Dd, Dn, Dm.
func fdivD(dd, dn, dm uint32) uint32 {
	return 0x1E601800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fnegD encodes FNEG Dd, Dn.
func fnegD(dd, dn uint32) uint32 {
	return 0x1E614000 | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fcmpD encodes FCMP Dn, Dm (scalar double compare). Sets NZCV from
// the IEEE 754 unordered-aware comparison: ordered Eq/Ne/Lt/Le/Gt/Ge
// branch under the standard cond codes; NaN sets V (and C) so ordered
// predicates fail naturally without an extra unordered check.
func fcmpD(dn, dm uint32) uint32 {
	return 0x1E602000 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5)
}

// scvtfDX encodes SCVTF Dd, Xn (signed 64-bit int -> double, round to
// nearest even). Used by OpI64ToF64.
func scvtfDX(dd, xn uint32) uint32 {
	return 0x9E620000 | ((xn & 0x1F) << 5) | (dd & 0x1F)
}

// fcvtzsXD encodes FCVTZS Xd, Dn (double -> signed 64-bit int,
// truncating toward zero). Used by OpF64ToI64.
func fcvtzsXD(xd, dn uint32) uint32 {
	return 0x9E780000 | ((dn & 0x1F) << 5) | (xd & 0x1F)
}

// condForCmpF64 returns the AArch64 B.cond condition code for a vm3
// f64 reg-reg compare-and-branch opcode. Ordered predicates use the
// standard unsigned condition codes (HI/HS/MI/LS) because FCMP sets
// the flags so unordered (NaN) operands fail Lt/Le/Gt/Ge naturally.
func condForCmpF64(code vm3.OpCode) uint32 {
	switch code {
	case vm3.OpCmpEqF64Br:
		return 0x0 // EQ
	case vm3.OpCmpNeF64Br:
		return 0x1 // NE
	case vm3.OpCmpLtF64Br:
		return 0x4 // MI (FCMP sets N=1 iff Dn < Dm ordered; unordered keeps N=0)
	case vm3.OpCmpLeF64Br:
		return 0x9 // LS (C clear OR Z set; works because FCMP sets C=0 if Dn < Dm)
	case vm3.OpCmpGtF64Br:
		return 0xC // GT (Z clear AND N==V)
	case vm3.OpCmpGeF64Br:
		return 0xA // GE (N==V)
	}
	return 0
}
