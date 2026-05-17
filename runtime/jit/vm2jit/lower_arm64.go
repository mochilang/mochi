//go:build arm64

package vm2jit

import (
	"fmt"

	"mochi/runtime/vm2"
)

// AArch64 GPR mapping: vm2 register r[i] -> x(9+i).
// Mirrors tmpljit's r2x mapping; range is x9-x15 (7 callee-saved GPRs).
func r2x(r int32) uint32 { return uint32(r) + 9 }

// --- AArch64 instruction encoders (subset, extended from tmpljit) ---

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
func mulReg(xd, xn, xm uint32) uint32 {
	return 0x9B000000 | ((xm & 0x1F) << 16) | (31 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func sdivReg(xd, xn, xm uint32) uint32 {
	return 0x9AC00C00 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func msubReg(xd, xn, xm, xa uint32) uint32 { // xd = xa - xn*xm
	return 0x9B008000 | ((xm & 0x1F) << 16) | ((xa & 0x1F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}
func csetGT(xd uint32) uint32 { return 0x9A9F07E0 | (0xC << 12) | (xd & 0x1F) } // GT = cond 0xC
func csetGE(xd uint32) uint32 { return 0x9A9F07E0 | (0xA << 12) | (xd & 0x1F) } // GE = cond 0xA
func csetEQ(xd uint32) uint32 { return 0x9A9F07E0 | (0x0 << 12) | (xd & 0x1F) } // EQ = cond 0x0
func csetLT(xd uint32) uint32 { return 0x9A9F07E0 | (0xB << 12) | (xd & 0x1F) } // LT = cond 0xB
func csetLE(xd uint32) uint32 { return 0x9A9F07E0 | (0xD << 12) | (xd & 0x1F) } // LE = cond 0xD
func csetNE(xd uint32) uint32 { return 0x9A9F07E0 | (0x1 << 12) | (xd & 0x1F) } // NE = cond 0x1
func addImm(xd, xn, imm12 uint32) uint32 {
	return 0x91000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func ldr64(xt, xn, imm12 uint32) uint32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}
func bImm(off26 int32) uint32    { return 0x14000000 | uint32(off26&0x3FFFFFF) }
func blImm(off26 int32) uint32   { return 0x94000000 | uint32(off26&0x3FFFFFF) }
func bCond(cond uint32, off19 int32) uint32 {
	return 0x54000000 | (uint32(off19&0x7FFFF) << 5) | (cond & 0xF)
}
func cbz(xn uint32, off19 int32) uint32  { return 0xB4000000 | (uint32(off19&0x7FFFF) << 5) | (xn & 0x1F) }
func cbnz(xn uint32, off19 int32) uint32 { return 0xB5000000 | (uint32(off19&0x7FFFF) << 5) | (xn & 0x1F) }
func ret() uint32                         { return 0xD65F03C0 }
func nop() uint32                         { return 0xD503201F }

// movImm64 emits 2-4 instructions to load any 64-bit immediate into xd.
func movImm64(xd uint32, v int64) []uint32 {
	if v >= 0 {
		lo := uint32(v) & 0xFFFF
		h1 := uint32(v>>16) & 0xFFFF
		h2 := uint32(v>>32) & 0xFFFF
		h3 := uint32(v>>48) & 0xFFFF
		ws := []uint32{movz(xd, lo, 0)}
		if h1 != 0 {
			ws = append(ws, movk(xd, h1, 1))
		}
		if h2 != 0 {
			ws = append(ws, movk(xd, h2, 2))
		}
		if h3 != 0 {
			ws = append(ws, movk(xd, h3, 3))
		}
		return ws
	}
	// Negative: use movn for the first word.
	inv := ^v
	lo := uint32(inv) & 0xFFFF
	h1 := uint32(inv>>16) & 0xFFFF
	ws := []uint32{movn(xd, lo, 0)}
	if h1 != 0 {
		ws = append(ws, movk(xd, uint32(v>>16)&0xFFFF, 1))
	}
	return ws
}

// lowerARM64 lowers one vm2 Instr to AArch64 words.
// TODO(Phase 1): emits real encodings for arith + control + list + call.
// TODO(Phase 2): strings + maps.
// TODO(Phase 3): sets + structs.
func lowerARM64(_ *vm2.Function, _ int, ins vm2.Instr) ([]uint32, error) {
	a, b, c := r2x(ins.A), r2x(ins.B), r2x(ins.C)
	switch ins.Op {
	// --- Arithmetic (I64) ---
	case vm2.OpAddI64:
		return []uint32{addReg(a, b, c)}, nil
	case vm2.OpAddI64K:
		if ins.C >= 0 && ins.C <= 4095 {
			return []uint32{addImm(a, b, uint32(ins.C))}, nil
		}
		// Fall back: load immediate into scratch x8 then add.
		ws := movImm64(8, int64(ins.C))
		ws = append(ws, addReg(a, b, 8))
		return ws, nil
	case vm2.OpSubI64:
		return []uint32{subReg(a, b, c)}, nil
	case vm2.OpMulI64:
		return []uint32{mulReg(a, b, c)}, nil
	case vm2.OpDivI64:
		return []uint32{sdivReg(a, b, c)}, nil
	case vm2.OpModI64:
		// xd = xn - (xn/xm)*xm  via  sdiv + msub
		return []uint32{
			sdivReg(8, b, c),        // x8 = b/c
			msubReg(a, 8, c, b),     // a  = b - x8*c
		}, nil
	case vm2.OpLessI64:
		return []uint32{cmpReg(b, c), csetLT(a)}, nil
	case vm2.OpLessEqI64:
		return []uint32{cmpReg(b, c), csetLE(a)}, nil
	case vm2.OpEqualI64:
		return []uint32{cmpReg(b, c), csetEQ(a)}, nil
	case vm2.OpMove:
		// ORR xd, xzr, xn  (canonical register move)
		return []uint32{0xAA0003E0 | ((b & 0x1F) << 16) | (a & 0x1F)}, nil
	case vm2.OpLoadConstI:
		// Operand B is the index into fn.Consts; at compile time we don't
		// have the Cell value here. Phase 1 will embed the constant pool
		// base address in the prologue (x8 = &fn.Consts[0]) and emit:
		//   ldr a, [x8, B*8]
		// For the scaffold, return not-implemented so Compile short-circuits.
		return nil, fmt.Errorf("%w: OpLoadConstI (needs constant-pool prologue)", ErrNotImplemented)

	// --- Control flow ---
	// Branch targets need two-pass relocation; the scaffold emits nops as
	// placeholders so the compiler driver can compute word offsets and patch.
	case vm2.OpJump:
		return []uint32{nop()}, nil // placeholder; Phase 1 patches
	case vm2.OpJumpIfFalse:
		return []uint32{cbz(a, 0), nop()}, nil // placeholder
	case vm2.OpJumpIfLessI64:
		// cmp a, b; b.lt target  (fused compare-and-branch, 2 instrs)
		return []uint32{cmpReg(a, b), bCond(0xB /* LT */, 0)}, nil
	case vm2.OpJumpIfLessEqI64:
		return []uint32{cmpReg(a, b), bCond(0xD /* LE */, 0)}, nil
	case vm2.OpJumpIfGreaterI64:
		return []uint32{cmpReg(a, b), bCond(0xC /* GT */, 0)}, nil
	case vm2.OpJumpIfGreaterEqI64:
		return []uint32{cmpReg(a, b), bCond(0xA /* GE */, 0)}, nil
	case vm2.OpJumpIfEqualI64:
		return []uint32{cmpReg(a, b), bCond(0x0 /* EQ */, 0)}, nil
	case vm2.OpJumpIfNotEqualI64:
		return []uint32{cmpReg(a, b), bCond(0x1 /* NE */, 0)}, nil

	// --- Return ---
	case vm2.OpReturn:
		// Phase 1: epilogue stores registers back, then ret.
		// Scaffold emits a single ret; prologue/epilogue are added by the
		// compiler driver in Phase 1.
		return []uint32{ret()}, nil

	// --- List / string / map / set / struct: slow-path callouts ---
	// These are all handled via slow-path shims in Phase 1/2/3.
	// The scaffold returns ErrNotImplemented so Compile declines gracefully.
	case vm2.OpNewList, vm2.OpListLen, vm2.OpListGet, vm2.OpListSet, vm2.OpListPush,
		vm2.OpLoadStrK, vm2.OpConcatStr, vm2.OpLenStr, vm2.OpIndexStr, vm2.OpEqualStr, vm2.OpHashStr,
		vm2.OpNewMap, vm2.OpMapLen, vm2.OpMapGet, vm2.OpMapHas, vm2.OpMapSet, vm2.OpMapDel:
		return nil, fmt.Errorf("%w: %v", ErrNotImplemented, ins.Op)

	// --- Calls ---
	case vm2.OpCall, vm2.OpTailCall, vm2.OpTailCallSelf:
		return nil, fmt.Errorf("%w: %v (needs frame protocol)", ErrNotImplemented, ins.Op)

	case vm2.OpHalt:
		return nil, fmt.Errorf("%w: OpHalt", ErrNotImplemented)

	default:
		return nil, fmt.Errorf("vm2jit: unknown opcode %d", ins.Op)
	}
}
