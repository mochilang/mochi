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
func addRegLSL(xd, xn, xm, shift uint32) uint32 {
	return 0x8B000000 | ((xm & 0x1F) << 16) | ((shift & 0x3F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
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
func ldur64(xt, xn uint32, imm9 int32) uint32 {
	return 0xF8400000 | ((uint32(imm9) & 0x1FF) << 12) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func bImm(off26 int32) uint32 { return 0x14000000 | uint32(off26&0x3FFFFFF) }
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

// lsrImm emits LSR Xd, Xn, #shift (alias for UBFM Xd, Xn, #shift, #63).
// shift must be in 0..63. Used for tag-extraction in Phase 2 fast paths.
func lsrImm(xd, xn, shift uint32) uint32 {
	return 0xD340FC00 | ((shift & 0x3F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// --- AArch64 NEON / FP encoders (MEP-38 §3.2.1 float JIT lowering) ---
//
// Float Cells store the float64 bits directly in Cell.Bits (the
// IsFloat predicate is `Bits & 0xFFFF0000_00000000 < 0x7FFB0000_00000000`,
// matched by every normal/subnormal double plus the canonical qNaN
// 0x7FF8). The JIT moves bits between GPR (xN) and FPR (dN) via FMOV,
// performs the FP arithmetic in dN, and stores back. No tag bits are
// applied; the bit pattern *is* the Cell. NaN bit patterns are not
// canonicalized by the JIT (matches Go's native FP behavior and is
// consistent with vm2's runtime which only canonicalizes at CFloat
// construction; arithmetic on already-tagged cells preserves bits).

// faddD, fsubD, fmulD, fdivD: FP data-processing (2 source), ftype=01 (double).
// Encoding: 0 00 11110 01 1 Rm op2 10 Rn Rd, op2 in {0010,0011,0000,0001}.
func faddD(dd, dn, dm uint32) uint32 {
	return 0x1E602800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}
func fsubD(dd, dn, dm uint32) uint32 {
	return 0x1E603800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}
func fmulD(dd, dn, dm uint32) uint32 {
	return 0x1E600800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}
func fdivD(dd, dn, dm uint32) uint32 {
	return 0x1E601800 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// fabsD, fnegD, fsqrtD: FP data-processing (1 source), ftype=01 (double).
// Encoding: 0 00 11110 01 1 opc 10000 Rn Rd, opc in {000001, 000010, 000011}.
func fabsD(dd, dn uint32) uint32  { return 0x1E60C000 | ((dn & 0x1F) << 5) | (dd & 0x1F) }
func fnegD(dd, dn uint32) uint32  { return 0x1E614000 | ((dn & 0x1F) << 5) | (dd & 0x1F) }
func fsqrtD(dd, dn uint32) uint32 { return 0x1E61C000 | ((dn & 0x1F) << 5) | (dd & 0x1F) }

// fcmpD: FCMP Dn, Dm, ftype=01. Sets PSTATE.NZCV; no Rd field.
func fcmpD(dn, dm uint32) uint32 {
	return 0x1E602000 | ((dm & 0x1F) << 16) | ((dn & 0x1F) << 5)
}

// fmovDX: FMOV Xd, Dn (bit-pattern copy double → 64-bit GPR, no convert).
// fmovXD: FMOV Dd, Xn (bit-pattern copy 64-bit GPR → double, no convert).
// sf=1, ftype=01, rmode=00, opcode=110 (DtoX) / 111 (XtoD).
func fmovDX(xd, dn uint32) uint32 { return 0x9E660000 | ((dn & 0x1F) << 5) | (xd & 0x1F) }
func fmovXD(dd, xn uint32) uint32 { return 0x9E670000 | ((xn & 0x1F) << 5) | (dd & 0x1F) }

// scvtfD: SCVTF Dd, Xn, signed 64-bit int to double.
// fcvtzsD: FCVTZS Xd, Dn, double to signed 64-bit int (round toward zero).
func scvtfD(dd, xn uint32) uint32  { return 0x9E620000 | ((xn & 0x1F) << 5) | (dd & 0x1F) }
func fcvtzsD(xd, dn uint32) uint32 { return 0x9E780000 | ((dn & 0x1F) << 5) | (xd & 0x1F) }

// fmaddD: FMADD Dd, Dn, Dm, Da → Dd = Da + Dn*Dm. ftype=01, o2=0, o0=0.
// Maps OpFmaF64(A = B*C + D) with Dd=A, Dn=B, Dm=C, Da=D.
func fmaddD(dd, dn, dm, da uint32) uint32 {
	return 0x1F400000 | ((dm & 0x1F) << 16) | ((da & 0x1F) << 10) | ((dn & 0x1F) << 5) | (dd & 0x1F)
}

// csetMI / csetLS: CSET aliases for ordered float comparisons.
// CSET Xd, MI  = CSINC Xd, XZR, XZR, NOT(MI)=PL.  cond field 0x5.
// CSET Xd, LS  = CSINC Xd, XZR, XZR, NOT(LS)=HI.  cond field 0x8.
// MI is true after FCMP iff Vn < Vm and ordered (NaN sets V=1, clears N).
// LS is true after FCMP iff Vn <= Vm and ordered (NaN sets C=1, Z=0; LS = !C || Z, so unordered → false).
func csetMI(xd uint32) uint32 { return 0x9A9F07E0 | (0x5 << 12) | (xd & 0x1F) }
func csetLS(xd uint32) uint32 { return 0x9A9F07E0 | (0x8 << 12) | (xd & 0x1F) }

// --- FP emit helpers ---

// emitBinopF64 emits the 4-instruction sequence for a binary FP op:
// fmov d0, xB; fmov d1, xC; fop d2, d0, d1; fmov xA, d2.
func emitBinopF64(xA, xB, xC uint32, op func(dd, dn, dm uint32) uint32) []uint32 {
	return []uint32{
		fmovXD(0, xB), // d0 = bits(B)
		fmovXD(1, xC), // d1 = bits(C)
		op(2, 0, 1),   // d2 = op(d0, d1)
		fmovDX(xA, 2), // xA = bits(d2)
	}
}

// emitUnopF64 emits: fmov d0, xB; fop d1, d0; fmov xA, d1.
func emitUnopF64(xA, xB uint32, op func(dd, dn uint32) uint32) []uint32 {
	return []uint32{
		fmovXD(0, xB),
		op(1, 0),
		fmovDX(xA, 1),
	}
}

// emitCmpBoolF64 emits the 6-instruction sequence for a float comparison
// producing a bool Cell: fmov + fmov + fcmp + cset + movzTagBool + orr.
func emitCmpBoolF64(xA, xB, xC uint32, cset func(uint32) uint32) []uint32 {
	return []uint32{
		fmovXD(0, xB),
		fmovXD(1, xC),
		fcmpD(0, 1),
		cset(8),
		movzTagBool(),     // x17 = tagBool
		orrReg(xA, 8, 17), // xA  = x8 | tagBool
	}
}

// movzTagInt emits MOVZ x16, #0xFFFC, LSL#48, loading tagInt into x16.
func movzTagInt() uint32 { return movz(16, 0xFFFC, 3) }

// movzTagBool emits MOVZ x17, #0xFFFD, LSL#48, loading tagBool into x17.
func movzTagBool() uint32 { return movz(17, 0xFFFD, 3) }

// movImm64 emits exactly 4 instructions (movz + 3×movk) to load any
// 64-bit value into xd. Using a fixed 4-word encoding ensures
// instrWordCountARM64 returns the same value regardless of the constant's
// magnitude, which is critical for the two-pass pcMap calculation.
func movImm64(xd uint32, v int64) []uint32 {
	u := uint64(v)
	return []uint32{
		movz(xd, uint32(u&0xFFFF), 0),
		movk(xd, uint32((u>>16)&0xFFFF), 1),
		movk(xd, uint32((u>>32)&0xFFFF), 2),
		movk(xd, uint32((u>>48)&0xFFFF), 3),
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
		movzTagBool(),     // x17 = tagBool
		orrReg(xA, 8, 17), // xA  = x8 | tagBool
	}
}

// listLenWords is the fixed instruction count of an OpListLen lowering:
// 4 (tag check) + 5 (fast body) + 1 (b past_deopt) + deoptStubWords.
func listLenWords(fn *vm2.Function) int { return 10 + deoptStubWords(fn) }

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

	// --- MEP-38 §3.2.1 float lowering ---
	case vm2.OpLoadConstF:
		if int(ins.B) >= len(fn.Consts) {
			return 0, fmt.Errorf("%w: float const index %d out of range", ErrNotImplemented, ins.B)
		}
		return 4, nil // movz + 3×movk (the float bits)
	case vm2.OpAddF64, vm2.OpSubF64, vm2.OpMulF64, vm2.OpDivF64:
		return 4, nil // fmov + fmov + fop + fmov
	case vm2.OpNegF64, vm2.OpAbsF64, vm2.OpSqrtF64:
		return 3, nil // fmov + fop + fmov
	case vm2.OpLessF64, vm2.OpLessEqF64, vm2.OpEqualF64:
		return 6, nil // fmov + fmov + fcmp + cset + movzTagBool + orr
	case vm2.OpFmaF64:
		return 5, nil // 3×fmov(in) + fmadd + fmov(out)
	case vm2.OpI64ToF64:
		return 3, nil // sbfx + scvtf + fmov
	case vm2.OpF64ToI64:
		return 5, nil // fmov + fcvtzs + andMask + movzTagInt + orr

	case vm2.OpReturn:
		n := fn.NumRegs
		if n > maxRegs {
			n = maxRegs
		}
		return n + 2, nil // N×str + mov x0 + ret
	case vm2.OpListLen:
		return listLenWords(fn), nil
	default:
		// Anything else compiles to a deopt stub. See deoptStubWords.
		if isDeoptableOp(ins.Op) {
			return deoptStubWords(fn), nil
		}
		return 0, fmt.Errorf("%w: %v", ErrNotImplemented, ins.Op)
	}
}

// isDeoptableOp reports whether ins.Op is a vm2 opcode the JIT lowers as
// a deopt stub. Any opcode that touches the allocator, the Objects table,
// or vm.Frames (i.e., everything outside arithmetic / comparison /
// control-flow / Move / Return / Phase 2 fast paths) lands here.
//
// Phase 2 fast paths (read-only, no allocation, no growth): OpListLen.
// More to come as the read-only opcode set lands.
func isDeoptableOp(op vm2.Op) bool {
	switch op {
	case vm2.OpLoadStrK, vm2.OpConcatStr, vm2.OpLenStr, vm2.OpIndexStr,
		vm2.OpEqualStr, vm2.OpHashStr,
		vm2.OpNewList, vm2.OpListGet, vm2.OpListSet, vm2.OpListPush, vm2.OpListAppend,
		vm2.OpNewMap, vm2.OpMapLen, vm2.OpMapGet, vm2.OpMapHas, vm2.OpMapSet, vm2.OpMapDel,
		vm2.OpCall, vm2.OpTailCall, vm2.OpTailCallSelf,
		// MEP-38 Phase 2 (§3.2.1) lowers OpLoadConstF, OpAddF64, OpSubF64,
		// OpMulF64, OpDivF64, OpNegF64, OpAbsF64, OpSqrtF64, OpLessF64,
		// OpLessEqF64, OpEqualF64, OpFmaF64, OpI64ToF64, OpF64ToI64 to
		// NEON FP arithmetic. They are no longer in the deopt set.
		vm2.OpNewF64Array, vm2.OpF64ArrLen, vm2.OpF64ArrGet, vm2.OpF64ArrSet,
		vm2.OpNewI64Array, vm2.OpI64ArrLen, vm2.OpI64ArrGet, vm2.OpI64ArrSet,
		vm2.OpNewU8Array, vm2.OpU8ArrLen, vm2.OpU8ArrGet, vm2.OpU8ArrSet,
		// MEP-37 Phase 2 pair ops deopt to the interpreter for the same
		// reason as Phase 1: the JIT does not emit pair construction or
		// projection code yet.
		vm2.OpNewPair, vm2.OpPairFst, vm2.OpPairSnd,
		// MEP-38 Phase 1 byte-view ops + minimal I/O. Deopt for now; a
		// follow-up phase lowers OpBytesGet/OpBytesSet directly so the
		// reverse_complement inner loop stays in JIT-compiled code.
		vm2.OpBytesNew, vm2.OpBytesLen, vm2.OpBytesGet, vm2.OpBytesSet,
		vm2.OpBytesSlice, vm2.OpBytesEqual, vm2.OpBytesHash,
		vm2.OpBytesFromU8Array, vm2.OpBytesFromStr,
		vm2.OpStdoutWriteBytes, vm2.OpStdinReadAll,
		vm2.OpHalt:
		return true
	}
	return false
}

// deoptStubWords returns the fixed word count of a deopt stub for a
// function with this many live JIT registers. Sequence:
//
//	N × str64(x(9+r), x0, r)   spill live regs back to regs[]
//	4 × movImm64(x0, sentinel)  load sentinel-tagged PC
//	1 × ret                     return to wrapper
//
// PC is the deopting bytecode index, embedded as a 64-bit immediate so
// the stub is position-independent and the per-opcode size stays
// constant (which the two-pass pcMap requires).
func deoptStubWords(fn *vm2.Function) int {
	n := fn.NumRegs
	if n > maxRegs {
		n = maxRegs
	}
	return n + 4 + 1
}

// compileFnARM64 performs the two-pass compilation:
// Pass 1 builds pcMap (absolute word index for each bytecode instruction).
// Pass 2 emits all words with correct branch offsets from pcMap.
func compileFnARM64(fn *vm2.Function) ([]uint32, error) {
	prologue := prologueARM64(fn)
	pcMap := make([]int, len(fn.Code)+1)
	pcMap[0] = len(prologue)
	for i, ins := range fn.Code {
		cnt, err := instrWordCountARM64(fn, ins)
		if err != nil {
			return nil, err
		}
		pcMap[i+1] = pcMap[i] + cnt
	}

	total := pcMap[len(fn.Code)]
	words := make([]uint32, 0, total)
	words = append(words, prologue...)
	for i, ins := range fn.Code {
		ws, err := lowerInstrARM64(fn, i, ins, pcMap)
		if err != nil {
			return nil, err
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
		// Cell is 16 bytes (Bits at offset 0, Obj at offset 8). The
		// ldr64 imm12 is scaled by 8, so r*2 yields byte offset r*16,
		// loading regs[r].Bits into x(9+r).
		ws[r] = ldr64(uint32(9+r), 0, uint32(r)*2)
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
			return 0, fmt.Errorf("%w: branch out of range (off=%d, bits=%d)",
				ErrNotImplemented, off, maxBits)
		}
		return int32(off), nil
	}

	switch ins.Op {
	case vm2.OpLoadConstI:
		return movImm64(xA, int64(fn.Consts[ins.B].Bits)), nil

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

	// --- MEP-38 §3.2.1 float lowering ---
	case vm2.OpLoadConstF:
		return movImm64(xA, int64(fn.Consts[ins.B].Bits)), nil
	case vm2.OpAddF64:
		return emitBinopF64(xA, xB, xC, faddD), nil
	case vm2.OpSubF64:
		return emitBinopF64(xA, xB, xC, fsubD), nil
	case vm2.OpMulF64:
		return emitBinopF64(xA, xB, xC, fmulD), nil
	case vm2.OpDivF64:
		return emitBinopF64(xA, xB, xC, fdivD), nil
	case vm2.OpNegF64:
		return emitUnopF64(xA, xB, fnegD), nil
	case vm2.OpAbsF64:
		return emitUnopF64(xA, xB, fabsD), nil
	case vm2.OpSqrtF64:
		return emitUnopF64(xA, xB, fsqrtD), nil
	case vm2.OpLessF64:
		return emitCmpBoolF64(xA, xB, xC, csetMI), nil
	case vm2.OpLessEqF64:
		return emitCmpBoolF64(xA, xB, xC, csetLS), nil
	case vm2.OpEqualF64:
		return emitCmpBoolF64(xA, xB, xC, csetEQ), nil
	case vm2.OpFmaF64:
		// A = B*C + D. FMADD Dd, Dn, Dm, Da = Da + Dn*Dm; map d0=B, d1=C, d2=D, d3=A.
		xD := r2x(int32(ins.D))
		return []uint32{
			fmovXD(0, xB),
			fmovXD(1, xC),
			fmovXD(2, xD),
			fmaddD(3, 0, 1, 2),
			fmovDX(xA, 3),
		}, nil
	case vm2.OpI64ToF64:
		// xB carries a tagInt Cell. Unbox to int64 (sbfx48), convert, store.
		return []uint32{
			sbfx48(8, xB),
			scvtfD(0, 8),
			fmovDX(xA, 0),
		}, nil
	case vm2.OpF64ToI64:
		// xB carries float bits. Convert, mask, retag.
		return []uint32{
			fmovXD(0, xB),
			fcvtzsD(8, 0),
			andMask48(8, 8),
			movzTagInt(),
			orrReg(xA, 8, 16),
		}, nil

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
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xB /*LT*/, branchOff)
	case vm2.OpJumpIfLessEqI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xD /*LE*/, branchOff)
	case vm2.OpJumpIfGreaterI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xC /*GT*/, branchOff)
	case vm2.OpJumpIfGreaterEqI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0xA /*GE*/, branchOff)
	case vm2.OpJumpIfEqualI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0x0 /*EQ*/, branchOff)
	case vm2.OpJumpIfNotEqualI64:
		return emitCondBranchARM64(xA, xB, int(ins.C), 0x1 /*NE*/, branchOff)

	case vm2.OpReturn:
		n := fn.NumRegs
		if n > maxRegs {
			n = maxRegs
		}
		ws := make([]uint32, 0, n+2)
		for r := 0; r < n; r++ {
			// 16-byte Cell stride: imm12 scaled by 8, so r*2 = byte
			// offset r*16. Stores x(9+r) into regs[r].Bits.
			ws = append(ws, str64(uint32(9+r), 0, uint32(r)*2))
		}
		ws = append(ws, movReg(0, uint32(9+int(ins.A)))) // x0 = result Cell
		ws = append(ws, ret())
		return ws, nil

	case vm2.OpListLen:
		return emitListLenFastARM64(fn, idx, xA, xB), nil

	default:
		if isDeoptableOp(ins.Op) {
			return emitDeoptStubARM64(fn, idx), nil
		}
		return nil, fmt.Errorf("%w: %v", ErrNotImplemented, ins.Op)
	}
}

// emitDeoptStubARM64 emits the Phase 1.5 deopt sequence for the
// instruction at bytecode index idx. Each live JIT register is spilled
// back to its slot in regs[]; x0 is loaded with vm2.EncodeDeopt(idx);
// the function returns. The wrapper in init.go inspects x0, detects the
// sentinel via vm2.DecodeDeopt, promotes the JIT frame to a real vm2
// frame at IP=idx, and resumes the interpreter for that frame.
func emitDeoptStubARM64(fn *vm2.Function, idx int) []uint32 {
	n := fn.NumRegs
	if n > maxRegs {
		n = maxRegs
	}
	sentinel := vm2.EncodeDeopt(idx).Bits
	ws := make([]uint32, 0, n+4+1)
	for r := 0; r < n; r++ {
		// 16-byte Cell stride: imm12*8 = byte offset, so r*2 spills
		// x(9+r) back to regs[r].Bits.
		ws = append(ws, str64(uint32(9+r), 0, uint32(r)*2))
	}
	ws = append(ws, movImm64(0, int64(sentinel))...)
	ws = append(ws, ret())
	return ws
}

// emitListLenFastARM64 emits the Phase 2 OpListLen lowering. Sequence:
//
// Tag check (4 words):
//
//	lsr  x8, xB, #48           ; x8 = top 16 bits of regs[B]
//	movz x16, #0xFFFF           ; x16 = tagPtr's high 16
//	cmp  x8, x16                ; eq if regs[B] is tagPtr
//	b.ne <deopt>
//
// Fast body (5 words): read the typed *vmList directly from regs[B].Obj
// (offset B*16+8 from x0; imm12 scaled by 8 = B*2+1) and pack the int48
// length into regs[A]:
//
//	ldr  x16, [x0, #(B*16+8)]  ; x16 = regs[B].Obj = *vmList
//	ldr  x16, [x16, #8]         ; x16 = slice.len  (vmList.data header +8)
//	andMask48 x8, x16           ; x8 = len & payloadMask
//	movz x16, #0xFFFC, lsl#48  ; x16 = tagInt
//	orr  xA, x8, x16            ; regs[A] = CInt(len)
//
// Tail (1 word): jump past the local deopt stub so the happy path
// flows into the next instruction.
//
//	b <past_deopt>
//
// Followed by a standard deopt stub (deoptStubWords). Tag-miss control
// transfers to that stub, which spills live regs and returns the
// sentinel-tagged PC for this instruction; the wrapper finishes the
// op on the interpreter exactly as Phase 1.5 does.
func emitListLenFastARM64(fn *vm2.Function, idx int, xA, xB uint32) []uint32 {
	n := listLenWords(fn)
	ws := make([]uint32, 0, n)

	// Tag check.
	ws = append(ws,
		lsrImm(8, xB, 48),
		movz(16, 0xFFFF, 0),
		cmpReg(8, 16),
	)
	bneIdx := len(ws)
	ws = append(ws, 0) // placeholder for b.ne

	// Fast body. B is the source register index; regs[B].Obj is at
	// byte offset B*16+8 from x0, so imm12 (scaled by 8) = B*2+1.
	regBObjImm := uint32(xB-9)*2 + 1
	ws = append(ws,
		ldr64(16, 0, regBObjImm),
		ldr64(16, 16, 1), // +8 (slice header len)
		andMask48(8, 16),
		movzTagInt(),
		orrReg(xA, 8, 16),
	)
	bIdx := len(ws)
	ws = append(ws, 0) // placeholder for b past_deopt

	// Local deopt stub.
	deoptStart := len(ws)
	ws = append(ws, emitDeoptStubARM64(fn, idx)...)
	pastDeopt := len(ws)

	// Fix branch offsets (in instruction units).
	ws[bneIdx] = bCond(0x1 /*NE*/, int32(deoptStart-bneIdx))
	ws[bIdx] = bImm(int32(pastDeopt - bIdx))
	return ws
}

// emitCondBranchARM64 emits: sbfx(x8,xA) + sbfx(x17,xB) + cmp(x8,x17) + b.cond(target).
// The b.cond is at word offset 3 within this instruction's output.
func emitCondBranchARM64(xA, xB uint32, targetBC int, cond uint32,
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
