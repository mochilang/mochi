//go:build arm64

package vm2jit

import (
	"fmt"

	"mochi/runtime/vm2"
)

// AArch64 GPR mapping: vm2 register r[i] -> x(9+i), i.e. x9-x15 (7 slots).
// These are temporary (caller-saved) in AAPCS64, so the JIT'd function
// does not need to save/restore them. x0 (the regs pointer) is also
// caller-saved and is preserved through the body since no arithmetic
// opcode touches it.
func r2x(r int32) uint32 { return uint32(r) + 9 }

// --- AArch64 instruction encoders ---

func movz(xd, imm16, hw uint32) uint32 {
	return 0xD2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}
func movk(xd, imm16, hw uint32) uint32 {
	return 0xF2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}
func addReg(xd, xn, xm uint32) uint32 {
	return 0x8B000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func subReg(xd, xn, xm uint32) uint32 {
	return 0xCB000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func mulReg(xd, xn, xm uint32) uint32 {
	return 0x9B000000 | ((xm & 0x1F) << 16) | (31 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func sdivReg(xd, xn, xm uint32) uint32 {
	return 0x9AC00C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func msubReg(xd, xn, xm, xa uint32) uint32 {
	return 0x9B008000 | ((xm & 0x1F) << 16) | ((xa & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}
func orrReg(xd, xn, xm uint32) uint32 {
	return 0xAA000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func movReg(xd, xm uint32) uint32 { return 0xAA0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F) }
func ldr64(xt, xn, imm12 uint32) uint32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func bImm(off26 int32) uint32  { return 0x14000000 | uint32(off26&0x3FFFFFF) }
func bCond(cond uint32, off19 int32) uint32 {
	return 0x54000000 | (uint32(off19&0x7FFFF) << 5) | (cond & 0xF)
}
func tbz(xt, b uint32, off14 int32) uint32 {
	b5 := (b >> 5) & 1
	b40 := b & 0x1F
	return 0x36000000 | (b5 << 31) | (b40 << 19) | (uint32(off14&0x3FFF) << 5) | (xt & 0x1F)
}
func ret() uint32 { return 0xD65F03C0 }

// CSET encodings. CSET Xd, CC = CSINC Xd, XZR, XZR, NOT(CC).
// Condition codes: EQ=0 NE=1 GE=A LT=B GT=C LE=D.
func csetLT(xd uint32) uint32 { return 0x9A9F07E0 | (0xA << 12) | (xd & 0x1F) } // NOT(LT)=GE
func csetLE(xd uint32) uint32 { return 0x9A9F07E0 | (0xC << 12) | (xd & 0x1F) } // NOT(LE)=GT
func csetGE(xd uint32) uint32 { return 0x9A9F07E0 | (0xB << 12) | (xd & 0x1F) } // NOT(GE)=LT
func csetGT(xd uint32) uint32 { return 0x9A9F07E0 | (0xD << 12) | (xd & 0x1F) } // NOT(GT)=LE
func csetEQ(xd uint32) uint32 { return 0x9A9F07E0 | (0x1 << 12) | (xd & 0x1F) } // NOT(EQ)=NE
func csetNE(xd uint32) uint32 { return 0x9A9F07E0 | (0x0 << 12) | (xd & 0x1F) } // NOT(NE)=EQ

// sbfx48 sign-extends bits 0-47 of xn into xd (extracts vm2 int48 payload).
// Implements Cell.Int(): int64(c<<16)>>16 = SBFM with N=1, immr=0, imms=47.
// Base 0x93400000: sf=1, opc=00, class=Bitfield(100110), N=1.
func sbfx48(xd, xn uint32) uint32 {
	return 0x93400000 | (47 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// andMask48 computes xd = xn & 0x0000FFFFFFFFFFFF (clears the tag bits).
// AND immediate with bitmask N=1, immr=0, imms=47.
// Base 0x92400000: sf=1, opc=00, class=Logical(100100), N=1.
func andMask48(xd, xn uint32) uint32 {
	return 0x92400000 | (47 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// movzTagInt emits MOVZ x16, #0xFFFC, LSL#48, loading tagInt into x16.
func movzTagInt() uint32 { return movz(16, 0xFFFC, 3) }

// movzTagBool emits MOVZ x17, #0xFFFD, LSL#48, loading tagBool into x17.
func movzTagBool() uint32 { return movz(17, 0xFFFD, 3) }

// movImm64 emits exactly 4 instructions (movz + 3×movk) to load any
// 64-bit value into xd. Using a fixed 4-word encoding ensures instrWordCountARM64
// returns the same value regardless of the constant's magnitude, which is
// critical for the two-pass pcMap calculation.
func movImm64(xd uint32, v int64) []uint32 {
	u := uint64(v)
	return []uint32{
		movz(xd, uint32(u)&0xFFFF, 0),
		movk(xd, uint32(u>>16)&0xFFFF, 1),
		movk(xd, uint32(u>>32)&0xFFFF, 2),
		movk(xd, uint32(u>>48)&0xFFFF, 3),
	}
}

// emitBinopI64 emits the 6-instruction NaN-box-aware sequence for a binary
// I64 arithmetic op: unbox(B), unbox(C), op, mask, loadTag, rebox(A).
func emitBinopI64(xA, xB, xC uint32, op func(xd, xn, xm uint32) uint32) []uint32 {
	return []uint32{
		sbfx48(8, xB),     // x8  = int(B)
		sbfx48(17, xC),    // x17 = int(C)
		op(8, 8, 17),      // x8  = op(B, C)
		andMask48(8, 8),   // x8 &= payloadMask
		movzTagInt(),      // x16 = tagInt
		orrReg(xA, 8, 16), // xA  = x8 | tagInt
	}
}

// emitCmpBoolI64 emits the 6-instruction sequence for a comparison op that
// produces a bool Cell: unbox(B), unbox(C), cmp, cset, loadBoolTag, rebox(A).
func emitCmpBoolI64(xA, xB, xC uint32, cset func(uint32) uint32) []uint32 {
	return []uint32{
		sbfx48(8, xB),
		sbfx48(17, xC),
		cmpReg(8, 17),
		cset(8),
		movzTagBool(),      // x17 = tagBool
		orrReg(xA, 8, 17), // xA  = x8 | tagBool
	}
}

// instrWordCountARM64 returns the exact number of 32-bit words that
// lowerInstrARM64 will emit for ins. Must be pure and consistent.
func instrWordCountARM64(fn *vm2.Function, ins vm2.Instr) (int, error) {
	switch ins.Op {
	case vm2.OpLoadConstI:
		if int(ins.B) >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: const index %d out of range", ErrNotImplemented, ins.B)
		}
		return 4, nil // movz + 3×movk
	case vm2.OpMove:
		return 1, nil
	case vm2.OpAddI64, vm2.OpSubI64, vm2.OpMulI64, vm2.OpDivI64:
		return 6, nil
	case vm2.OpModI64:
		return 7, nil // sbfx + sbfx + sdiv + msub + andMask48 + movzTag + orr
	case vm2.OpAddI64K:
		return 10, nil // sbfx + movImm64(4) + add + andMask48 + movzTag + orr
	case vm2.OpLessI64, vm2.OpLessEqI64, vm2.OpEqualI64:
		return 6, nil
	case vm2.OpJump:
		return 1, nil
	case vm2.OpJumpIfFalse:
		return 1, nil
	case vm2.OpJumpIfLessI64, vm2.OpJumpIfLessEqI64, vm2.OpJumpIfGreaterI64,
		vm2.OpJumpIfGreaterEqI64, vm2.OpJumpIfEqualI64, vm2.OpJumpIfNotEqualI64:
		return 4, nil // sbfx + sbfx + cmp + b.cond
	case vm2.OpReturn:
		n := fn.NumRegs
		if n > maxRegs {
			n = maxRegs
		}
		return n + 2, nil // N×str + mov x0 + ret
	default:
		return 0, fmt.Errorf("%w: %v", ErrNotImplemented, ins.Op)
	}
}

// compileFnARM64 performs the two-pass compilation:
// Pass 1 builds pcMap (absolute word index for each bytecode instruction).
// Pass 2 emits all words with correct branch offsets from pcMap.
func compileFnARM64(fn *vm2.Function) ([]uint32, error) {
	// Pass 1: prologue + word-count per instruction → pcMap.
	prologue := prologueARM64(fn)
	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = len(prologue)
	for i, ins := range fn.Code {
		cnt, err := instrWordCountARM64(fn, ins)
		if err != nil {
			return nil, fmt.Errorf("instr %d (%v): %w", i, ins.Op, err)
		}
		pcMap[i+1] = pcMap[i] + cnt
	}

	// Pass 2: emit words.
	total := pcMap[len(fn.Code)]
	words := make([]uint32, 0, total)
	words = append(words, prologue...)
	for i, ins := range fn.Code {
		ws, err := lowerInstrARM64(fn, i, ins, pcMap)
		if err != nil {
			return nil, fmt.Errorf("instr %d (%v): %w", i, ins.Op, err)
		}
		words = append(words, ws...)
	}
	return words, nil
}

// prologueARM64 emits the N ldr64 instructions that load vm2 registers
// from the regs pointer in x0 into x9..x(9+N-1).
func prologueARM64(fn *vm2.Function) []uint32 {
	n := fn.NumRegs
	if n > maxRegs {
		n = maxRegs
	}
	ws := make([]uint32, n)
	for r := 0; r < n; r++ {
		ws[r] = ldr64(uint32(9+r), 0, uint32(r))
	}
	return ws
}

// lowerInstrARM64 emits the word sequence for one vm2 instruction.
// pcMap[i] is the absolute word index for bytecode instruction i.
func lowerInstrARM64(fn *vm2.Function, idx int, ins vm2.Instr, pcMap []int) ([]uint32, error) {
	xA := r2x(int32(ins.A))
	xB := r2x(int32(ins.B))
	xC := r2x(int32(ins.C))

	// branchOff computes a signed branch offset in instruction units.
	// instrOff is the word offset of the branch instruction within this
	// instruction's output (0 for single-instruction branches).
	branchOff := func(instrOff, targetBC int, maxBits int) (int32, error) {
		src := pcMap[idx] + instrOff
		dst := pcMap[targetBC]
		off := dst - src
		half := 1 << (maxBits - 1)
		if off < -half || off >= half {
			return 0, fmt.Errorf("branch from instr %d to %d: offset %d out of %d-bit range",
				idx, targetBC, off, maxBits)
		}
		return int32(off), nil
	}

	switch ins.Op {
	case vm2.OpLoadConstI:
		return movImm64(xA, int64(fn.Consts[ins.B])), nil

	case vm2.OpMove:
		return []uint32{movReg(xA, xB)}, nil

	case vm2.OpAddI64:
		return emitBinopI64(xA, xB, xC, addReg), nil
	case vm2.OpSubI64:
		return emitBinopI64(xA, xB, xC, subReg), nil
	case vm2.OpMulI64:
		return emitBinopI64(xA, xB, xC, mulReg), nil
	case vm2.OpDivI64:
		return emitBinopI64(xA, xB, xC, sdivReg), nil

	case vm2.OpModI64:
		// x8 = int(B); x17 = int(C); x16 = x8/x17; x8 = x8 - x16*x17
		return []uint32{
			sbfx48(8, xB),
			sbfx48(17, xC),
			sdivReg(16, 8, 17),    // x16 = int(B)/int(C)
			msubReg(8, 16, 17, 8), // x8  = x8 - x16*x17  (= B mod C)
			andMask48(8, 8),
			movzTagInt(),
			orrReg(xA, 8, 16),
		}, nil

	case vm2.OpAddI64K:
		imm := movImm64(17, int64(ins.C)) // x17 = int32(C) sign-extended
		ws := make([]uint32, 0, 10)
		ws = append(ws, sbfx48(8, xB))
		ws = append(ws, imm...)
		ws = append(ws, addReg(8, 8, 17))
		ws = append(ws, andMask48(8, 8))
		ws = append(ws, movzTagInt())
		ws = append(ws, orrReg(xA, 8, 16))
		return ws, nil

	case vm2.OpLessI64:
		return emitCmpBoolI64(xA, xB, xC, csetLT), nil
	case vm2.OpLessEqI64:
		return emitCmpBoolI64(xA, xB, xC, csetLE), nil
	case vm2.OpEqualI64:
		return emitCmpBoolI64(xA, xB, xC, csetEQ), nil

	case vm2.OpJump:
		off, err := branchOff(0, int(ins.A), 26)
		if err != nil {
			return nil, err
		}
		return []uint32{bImm(off)}, nil

	case vm2.OpJumpIfFalse:
		// TBZ xA, #0, target: jump if bit 0 of xA == 0 (Cell is false).
		off, err := branchOff(0, int(ins.B), 14)
		if err != nil {
			return nil, err
		}
		return []uint32{tbz(xA, 0, off)}, nil

	case vm2.OpJumpIfLessI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xB /*LT*/, idx, pcMap, branchOff)
	case vm2.OpJumpIfLessEqI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xD /*LE*/, idx, pcMap, branchOff)
	case vm2.OpJumpIfGreaterI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xC /*GT*/, idx, pcMap, branchOff)
	case vm2.OpJumpIfGreaterEqI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xA /*GE*/, idx, pcMap, branchOff)
	case vm2.OpJumpIfEqualI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0x0 /*EQ*/, idx, pcMap, branchOff)
	case vm2.OpJumpIfNotEqualI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0x1 /*NE*/, idx, pcMap, branchOff)

	case vm2.OpReturn:
		n := fn.NumRegs
		if n > maxRegs {
			n = maxRegs
		}
		ws := make([]uint32, 0, n+2)
		for r := 0; r < n; r++ {
			ws = append(ws, str64(uint32(9+r), 0, uint32(r)))
		}
		ws = append(ws, movReg(0, uint32(9+int(ins.A)))) // x0 = result Cell
		ws = append(ws, ret())
		return ws, nil

	default:
		return nil, fmt.Errorf("%w: %v", ErrNotImplemented, ins.Op)
	}
}

// emitCondBranchARM64 emits: sbfx(x8,xA) + sbfx(x17,xB) + cmp(x8,x17) + b.cond(target).
// The b.cond is at word offset 3 within this instruction's output.
func emitCondBranchARM64(xA, xB uint32, targetBC int, cond uint32, idx int, pcMap []int,
	branchOff func(int, int, int) (int32, error)) ([]uint32, error) {
	off, err := branchOff(3, targetBC, 19) // b.cond imm19
	if err != nil {
		return nil, err
	}
	return []uint32{
		sbfx48(8, xA),
		sbfx48(17, xB),
		cmpReg(8, 17),
		bCond(cond, off),
	}, nil
}

// maxRegs is the maximum number of vm2 registers the JIT can handle (x9-x15).
const maxRegs = 7

// lowerFnARM64 is the full-function entry point called by lower.go on arm64.
func lowerFnARM64(fn *vm2.Function) ([]uint32, error) {
	return compileFnARM64(fn)
}
