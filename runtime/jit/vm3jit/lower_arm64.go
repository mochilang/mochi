//go:build arm64

package vm3jit

import (
	"fmt"

	"mochi/runtime/vm3"
)

// Register pinning for Phase 6.0:
//
//	regsI64[r] -> x(9 + r)  for r in [0, maxI64Regs)
//
// x9..x15 are AArch64 caller-saved temporaries; the JIT can clobber
// them freely without a callee-saved frame. x0 carries the regsI64
// base pointer (set by the trampoline). x16/x17 are intra-procedure
// scratches available to any encoder.
//
// vm3 i64 registers are stored as raw int64 (no NaN-box, no tag). A
// single LDR/STR moves a value between the regsI64 stack window and
// its pinned host register; arithmetic runs directly on the host
// register with no tag arithmetic. This is the key simplification
// over vm2jit and the reason a small instruction count per opcode is
// achievable for arithmetic kernels.
func r2x(r uint16) uint32 { return uint32(r) + 9 }

// lowerARM64 returns the AArch64 instruction-word stream for fn. Two
// passes: pass 1 computes pcMap[i] = word index where the lowering of
// bytecode i starts; pass 2 emits real instructions, resolving branch
// destinations through pcMap.
func lowerARM64(fn *vm3.Function) ([]uint32, error) {
	prologueWords := int(fn.NumRegsI64)

	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = prologueWords
	for i, op := range fn.Code {
		n, err := wordCountARM64(op)
		if err != nil {
			return nil, fmt.Errorf("vm3jit/%s: pc %d: %w", fn.Name, i, err)
		}
		pcMap[i+1] = pcMap[i] + n
	}

	total := pcMap[len(fn.Code)]
	ws := make([]uint32, 0, total)

	for r := uint32(0); r < uint32(fn.NumRegsI64); r++ {
		ws = append(ws, ldr64(r+9, 0, r))
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

// wordCountARM64 returns the exact number of 32-bit words emitInstrARM64
// will produce for op. Used by pass 1 to lay out pcMap.
func wordCountARM64(op vm3.Op) (int, error) {
	switch op.Code {
	case vm3.OpConstI64K:
		// movImm64 emits 1..4 movz/movk words depending on the sign-extended
		// 16-bit constant. For C in [0, 65535] one movz; for negative values
		// a movn or up to 4 movz/movk pairs.
		return movImm64WordCount(int64(op.C)), nil
	case vm3.OpAddI64:
		return 1, nil
	case vm3.OpAddI64K:
		// Always lower as: MOV imm into x16; ADD xd, xn, x16. Total cost
		// is movImm64 words + 1 ADD. Const range -32768..32767 (int16).
		return movImm64WordCount(int64(op.C)) + 1, nil
	case vm3.OpCmpGeI64Br:
		return 2, nil
	case vm3.OpJump:
		return 1, nil
	case vm3.OpReturnI64:
		// MOV x0, xA; RET.
		return 2, nil
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

	case vm3.OpAddI64:
		// op.C is int16 storage but holds a uint16 register index.
		xC := r2x(uint16(op.C))
		return []uint32{addReg(xA, xB, xC)}, nil

	case vm3.OpAddI64K:
		ws := movImm64(16, int64(op.C))
		ws = append(ws, addReg(xA, xB, 16))
		return ws, nil

	case vm3.OpCmpGeI64Br:
		dstWord := pcMap[int(uint16(op.C))]
		// CMP at pcMap[idx]; B.GE at pcMap[idx]+1.
		srcWord := pcMap[idx] + 1
		off, err := branchOff(srcWord, dstWord, 19)
		if err != nil {
			return nil, err
		}
		return []uint32{cmpReg(xA, xB), bCond(0xA /*GE*/, off)}, nil

	case vm3.OpJump:
		dstWord := pcMap[int(uint16(op.C))]
		srcWord := pcMap[idx]
		off, err := branchOff(srcWord, dstWord, 26)
		if err != nil {
			return nil, err
		}
		return []uint32{bImm(off)}, nil

	case vm3.OpReturnI64:
		return []uint32{movReg(0, xA), ret()}, nil

	default:
		_ = fn
		return nil, fmt.Errorf("%w: opcode %d", ErrNotImplemented, op.Code)
	}
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
