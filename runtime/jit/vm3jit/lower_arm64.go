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

// lowerARM64 returns the AArch64 instruction-word stream for fn. Two
// passes: pass 1 computes pcMap[i] = word index where the lowering of
// bytecode i starts; pass 2 emits real instructions, resolving branch
// destinations through pcMap.
func lowerARM64(fn *vm3.Function) ([]uint32, error) {
	pairs := numCalleeSavedPairs(fn)
	prologueWords := pairs + int(fn.NumRegsI64)

	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = prologueWords
	for i, op := range fn.Code {
		n, err := wordCountARM64(fn, op)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d: %w", fn.Name, i, err)
		}
		pcMap[i+1] = pcMap[i] + n
	}

	total := pcMap[len(fn.Code)]
	ws := make([]uint32, 0, total)

	// Prologue: push callee-saved pairs, then load each live reg from
	// [x0, #r*8] into its pinned host register.
	for k := 0; k < pairs; k++ {
		ws = append(ws, stpPreIdx64(uint32(2*k+19), uint32(2*k+20), 31, -16))
	}
	for r := uint32(0); r < uint32(fn.NumRegsI64); r++ {
		ws = append(ws, ldr64(r2x(uint16(r)), 0, r))
	}

	for i, op := range fn.Code {
		emit, err := emitInstrARM64(fn, op, i, pcMap)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: %w", fn.Name, i, op.Code, err)
		}
		if got, want := len(emit), pcMap[i+1]-pcMap[i]; got != want {
			return nil, fmt.Errorf("vm3jit/%s: pc %d op=%d: emitted %d words, predicted %d",
				fn.Name, i, op.Code, got, want)
		}
		ws = append(ws, emit...)
	}
	if len(ws) != total {
		return nil, fmt.Errorf("vm3jit/%s: final stream %d words, predicted %d",
			fn.Name, len(ws), total)
	}
	return ws, nil
}

// emitCalleeSavedEpilogueARM64 appends pairs LDP instructions to ws in
// REVERSE order of the prologue's pushes so each pop matches the
// topmost STP frame and SP returns to its entry value.
func emitCalleeSavedEpilogueARM64(ws []uint32, pairs int) []uint32 {
	for k := pairs - 1; k >= 0; k-- {
		ws = append(ws, ldpPostIdx64(uint32(2*k+19), uint32(2*k+20), 31, 16))
	}
	return ws
}

// wordCountARM64 returns the exact number of 32-bit words emitInstrARM64
// will produce for op. Used by pass 1 to lay out pcMap.
func wordCountARM64(fn *vm3.Function, op vm3.Op) (int, error) {
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
		// MOV x0, xA; <pop callee-saved frame>; RET.
		return 2 + numCalleeSavedPairs(fn), nil
	case vm3.OpReturnConstK:
		// movImm64 into x0; <pop callee-saved frame>; RET.
		return movImm64WordCount(int64(op.C)) + numCalleeSavedPairs(fn) + 1, nil
	default:
		return 0, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
}

// emitInstrARM64 emits the AArch64 instructions for op at bytecode
// index idx. pcMap[i] is the word offset where instruction i's
// lowering begins (and pcMap[len(Code)] is the word offset of the
// fall-through past the last instruction).
func emitInstrARM64(fn *vm3.Function, op vm3.Op, idx int, pcMap []int) ([]uint32, error) {
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
		ws = emitCalleeSavedEpilogueARM64(ws, numCalleeSavedPairs(fn))
		ws = append(ws, ret())
		return ws, nil

	case vm3.OpReturnConstK:
		ws := movImm64(0, int64(op.C))
		ws = emitCalleeSavedEpilogueARM64(ws, numCalleeSavedPairs(fn))
		ws = append(ws, ret())
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

func bImm(off26 int32) uint32 {
	return 0x14000000 | uint32(off26&0x3FFFFFF)
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
