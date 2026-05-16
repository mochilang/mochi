//go:build darwin && arm64

package tmpljit

import (
	"encoding/binary"
	"fmt"
	"syscall"
	"unsafe"
)

// r2x maps VM register index (0..NumRegs-1) to AArch64 GPR index.
// We use x9..x15: seven caller-saved registers, no prologue save
// required.
func r2x(r uint8) uint32 {
	if r >= NumRegs {
		panic(fmt.Sprintf("tmpljit: register %d out of range", r))
	}
	return uint32(r) + 9
}

// AArch64 encoders. All instructions are 32-bit little-endian.

// movz xd, #imm16, lsl #(16*hw)
func movz(xd, imm16, hw uint32) uint32 {
	return 0xD2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

// movk xd, #imm16, lsl #(16*hw)
func movk(xd, imm16, hw uint32) uint32 {
	return 0xF2800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

// movn xd, #imm16, lsl #(16*hw)
func movn(xd, imm16, hw uint32) uint32 {
	return 0x92800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (xd & 0x1F)
}

// add xd, xn, xm (sf=1)
func addReg(xd, xn, xm uint32) uint32 {
	return 0x8B000000 | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// mul xd, xn, xm  =  madd xd, xn, xm, xzr  (xzr=31)
func mulReg(xd, xn, xm uint32) uint32 {
	return 0x9B000000 | ((xm & 0x1F) << 16) | (31 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// mov xd, xm  =  orr xd, xzr, xm
func movReg(xd, xm uint32) uint32 {
	return 0xAA0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}

// subs xzr, xn, xm  (cmp xn, xm)
func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}

// cset xd, cond  =  csinc xd, xzr, xzr, invert(cond)
// We need: dst = 1 if cond holds else 0.
// csinc xd, xn, xm, cond  => if cond then xn else xm+1
// cset xd, LT => csinc xd, xzr(31), xzr(31), GE(0b1010)
const condGE = 0xA // 0b1010
func cset_lt(xd uint32) uint32 {
	// 0x9A9F07E0: csinc xd, xzr, xzr, GE
	return 0x9A9F07E0 | (condGE << 12) | (xd & 0x1F)
}

// cbnz xd, off (branch if non-zero). off is 19-bit signed in instr units.
func cbnz(xd uint32, off19 int32) uint32 {
	return 0xB5000000 | uint32(off19&0x7FFFF)<<5 | (xd & 0x1F)
}

// ret (ret x30)
func ret() uint32 { return 0xD65F03C0 }

// codeLen returns the fixed instruction count (each = 4 bytes) for
// the given opcode. Used by the first pass to compute byte
// offsets so backward branches can be patched.
func codeLen(op Op) int {
	switch op {
	case OpMovImm:
		// Up to 4 movz/movk pairs to cover an int32. The bytecode
		// stores int32 immediates so 2 halfwords always suffice;
		// emit exactly 2 instructions and either zero-extend
		// (positive) or use movn + movk for negative.
		return 2
	case OpAdd, OpMul:
		return 1
	case OpRet:
		return 2 // mov x0, src; ret
	case OpLt:
		return 2 // cmp + cset
	case OpJnz:
		return 1 // cbnz
	}
	panic("tmpljit: bad opcode")
}

// Compile lowers Program p into ARM64 machine code, allocates an
// executable page, and returns a callable function pointer plus a
// release closure.
//
// The compile is a two-pass copy-and-patch:
//
//   - Pass 1 records byte offsets for every instruction so backward
//     branches know where to land.
//   - Pass 2 emits machine code, including the relative branch
//     offset for OpJnz computed from the pass-1 table.
//
// No optimizations are performed. Each VM opcode lowers to a
// fixed instruction sequence. This is the "tier 1 baseline" of
// MEP-30: short, predictable, easy to reason about.
func Compile(p Program) (*CompiledFunc, error) {
	// Pass 1: compute byte offset of each bytecode instruction
	// in the output stream. Add 4 bytes for the function prologue
	// (mov x9, x0) and the return is the last instruction's own
	// 4 bytes.
	const prologueWords = 1
	offsets := make([]int, len(p)+1)
	pos := prologueWords
	for i, ins := range p {
		offsets[i] = pos
		pos += codeLen(ins.Op)
	}
	offsets[len(p)] = pos // one past last

	// Pass 2: emit.
	words := make([]uint32, 0, pos)
	// Prologue: move arg (x0) into VM r0 (x9). The rest of the
	// VM register file is implicitly garbage; the bytecode is
	// expected to initialize anything it reads.
	words = append(words, movReg(r2x(0), 0))
	for i, ins := range p {
		switch ins.Op {
		case OpMovImm:
			xd := r2x(ins.Dst)
			imm := ins.Imm
			if imm >= 0 {
				lo := uint32(imm) & 0xFFFF
				hi := uint32(imm>>16) & 0xFFFF
				words = append(words, movz(xd, lo, 0))
				words = append(words, movk(xd, hi, 1))
			} else {
				// movn covers the inverted low halfword;
				// movk patches in the high halfword if non-trivial.
				// For -1..-65536 the high halfword of ~imm is zero.
				inv := uint32(^int64(imm)) // bitwise inverse, 64-bit
				lo := inv & 0xFFFF
				hi := (uint32(imm>>16) & 0xFFFF)
				words = append(words, movn(xd, lo, 0))
				words = append(words, movk(xd, hi, 1))
			}
		case OpAdd:
			words = append(words, addReg(r2x(ins.Dst), r2x(ins.A), r2x(ins.B)))
		case OpMul:
			words = append(words, mulReg(r2x(ins.Dst), r2x(ins.A), r2x(ins.B)))
		case OpLt:
			words = append(words, cmpReg(r2x(ins.A), r2x(ins.B)))
			words = append(words, cset_lt(r2x(ins.Dst)))
		case OpJnz:
			// Source PC for the jump is the address of *this* instruction.
			// Target instruction index is i+1 (next bytecode instr) + Imm.
			here := len(words)
			tgtBC := i + 1 + int(ins.Imm)
			if tgtBC < 0 || tgtBC > len(p) {
				return nil, fmt.Errorf("tmpljit: Jnz target %d out of range", tgtBC)
			}
			tgtWord := offsets[tgtBC]
			off := int32(tgtWord - here) // in instruction units
			if off < -(1<<18) || off >= (1<<18) {
				return nil, fmt.Errorf("tmpljit: Jnz offset %d out of 19-bit range", off)
			}
			words = append(words, cbnz(r2x(ins.A), off))
		case OpRet:
			// Move VM result to x0, then ret.
			// Two instructions, but codeLen reports 1 since we
			// fold the mov into the prologueWords budget? No,
			// we declared codeLen(OpRet)=1. Fix: emit only the
			// final ret here, plus a mov immediately before
			// that we account for via prologue accounting.
			// Simpler: change to 2 by emitting mov+ret.
			words = append(words, movReg(0, r2x(ins.A)))
			words = append(words, ret())
		}
	}

	// Convert to bytes.
	buf := make([]byte, len(words)*4)
	for i, w := range words {
		binary.LittleEndian.PutUint32(buf[i*4:], w)
	}

	// Allocate an executable page and copy.
	page, err := syscall.Mmap(-1, 0, pageRound(len(buf)),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE|mapJit)
	if err != nil {
		return nil, fmt.Errorf("tmpljit: mmap: %w", err)
	}
	pthread_jit_write_protect_np(false)
	copy(page, buf)
	pthread_jit_write_protect_np(true)
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		_ = syscall.Munmap(page)
		return nil, fmt.Errorf("tmpljit: mprotect: %w", err)
	}
	// Flush the instruction cache on the freshly written region.
	sys_icache_invalidate(unsafe.Pointer(&page[0]), uintptr(len(page)))

	return &CompiledFunc{
		entry: unsafe.Pointer(&page[0]),
		page:  page,
	}, nil
}

// CompiledFunc owns a JIT page and exposes Call.
type CompiledFunc struct {
	entry unsafe.Pointer
	page  []byte
}

// Free releases the JIT page. After Free, the func must not be called.
func (c *CompiledFunc) Free() error {
	if c.page == nil {
		return nil
	}
	err := syscall.Munmap(c.page)
	c.page = nil
	c.entry = nil
	return err
}

// CodeLen returns the size of the JIT'd machine code in bytes.
func (c *CompiledFunc) CodeLen() int { return len(c.page) }

func pageRound(n int) int {
	const ps = 16384 // darwin/arm64 page size
	return (n + ps - 1) &^ (ps - 1)
}
