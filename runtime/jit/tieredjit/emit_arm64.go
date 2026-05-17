//go:build darwin && arm64

package tieredjit

import (
	"encoding/binary"
	"fmt"
	"syscall"
	"unsafe"

	"mochi/runtime/jit/tmpljit"
)

// Same VM register pinning as tmpljit and tracejit (x9..x15).
func r2x(r uint8) uint32 {
	if r >= tmpljit.NumRegs {
		panic(fmt.Sprintf("tieredjit: register %d out of range", r))
	}
	return uint32(r) + 9
}

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
func mulReg(xd, xn, xm uint32) uint32 {
	return 0x9B000000 | ((xm & 0x1F) << 16) | (31 << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}
func movReg(xd, xm uint32) uint32 {
	return 0xAA0003E0 | ((xm & 0x1F) << 16) | (xd & 0x1F)
}
func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}

const condGE = 0xA

func csetLT(xd uint32) uint32 {
	return 0x9A9F07E0 | (condGE << 12) | (xd & 0x1F)
}
func cbnz(xd uint32, off19 int32) uint32 {
	return 0xB5000000 | (uint32(off19&0x7FFFF) << 5) | (xd & 0x1F)
}
func ret() uint32 { return 0xD65F03C0 }

// add (immediate): xd = xn + imm12  (sf=1, sh=0). imm12 must be in [0, 4095].
func addImm(xd, xn, imm12 uint32) uint32 {
	return 0x91000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// lsl (immediate) #shift, 64-bit. Alias of UBFM xd, xn, #(-shift mod 64), #(63-shift).
// shift must be in [0, 63]. Encoding: 0xD3400000 | immr<<16 | imms<<10 | xn<<5 | xd
// where immr = (64 - shift) & 63, imms = 63 - shift.
func lslImm(xd, xn, shift uint32) uint32 {
	immr := (64 - shift) & 0x3F
	imms := 63 - shift
	return 0xD3400000 | (immr << 16) | ((imms & 0x3F) << 10) | ((xn & 0x1F) << 5) | (xd & 0x1F)
}

// codeLen counts arm64 instructions per optProgram opcode.
func codeLen(op opTier2) int {
	switch op {
	case t2MovImm:
		return 2
	case t2Add, t2Mul, t2AddImm, t2ShlImm, t2MulImm:
		return 1
	case t2Lt:
		return 2
	case t2Jnz:
		return 1
	case t2Ret:
		return 2
	}
	panic("tieredjit: bad opcode")
}

// Compile turns a tmpljit.Program into native code by first running
// the tier-2 optimizer, then lowering the resulting optProgram.
//
// The two-pass copy-and-patch shape is identical to MEP-30:
//
//   - Pass 1 records byte offsets so backward branches can be
//     patched with a 19-bit signed cbnz offset.
//   - Pass 2 emits machine code.
//
// The prologue is one instruction (mov x9, x0) and the epilogue is
// two instructions per Ret (mov x0, src; ret).
func Compile(p tmpljit.Program) (*CompiledFunc, error) {
	opt := optimize(p)

	const prologueWords = 1
	offsets := make([]int, len(opt)+1)
	pos := prologueWords
	for i, ins := range opt {
		offsets[i] = pos
		pos += codeLen(ins.op)
	}
	offsets[len(opt)] = pos

	words := make([]uint32, 0, pos)
	words = append(words, movReg(r2x(0), 0))

	for i, ins := range opt {
		switch ins.op {
		case t2MovImm:
			xd := r2x(ins.dst)
			imm := ins.imm
			if imm >= 0 {
				lo := uint32(imm) & 0xFFFF
				hi := uint32(imm>>16) & 0xFFFF
				words = append(words, movz(xd, lo, 0))
				words = append(words, movk(xd, hi, 1))
			} else {
				inv := uint32(^int64(imm))
				lo := inv & 0xFFFF
				hi := uint32(imm>>16) & 0xFFFF
				words = append(words, movn(xd, lo, 0))
				words = append(words, movk(xd, hi, 1))
			}
		case t2Add:
			words = append(words, addReg(r2x(ins.dst), r2x(ins.a), r2x(ins.b)))
		case t2Mul:
			words = append(words, mulReg(r2x(ins.dst), r2x(ins.a), r2x(ins.b)))
		case t2Lt:
			words = append(words, cmpReg(r2x(ins.a), r2x(ins.b)))
			words = append(words, csetLT(r2x(ins.dst)))
		case t2AddImm:
			words = append(words, addImm(r2x(ins.dst), r2x(ins.a), uint32(ins.imm)))
		case t2ShlImm:
			words = append(words, lslImm(r2x(ins.dst), r2x(ins.a), uint32(ins.imm)))
		case t2MulImm:
			// Materialise the immediate into x16 (scratch), then mul.
			// Only used for non-power-of-two immediates that the
			// optimizer chose to fold; FillSumProgram doesn't hit
			// this path, kept for completeness.
			words = append(words, movz(16, uint32(ins.imm)&0xFFFF, 0))
			words = append(words, mulReg(r2x(ins.dst), r2x(ins.a), 16))
		case t2Jnz:
			here := len(words)
			tgt := i + 1 + int(ins.imm)
			if tgt < 0 || tgt > len(opt) {
				return nil, fmt.Errorf("tieredjit: Jnz target %d out of range", tgt)
			}
			tgtWord := offsets[tgt]
			off := int32(tgtWord - here)
			if off < -(1<<18) || off >= (1<<18) {
				return nil, fmt.Errorf("tieredjit: Jnz offset %d out of 19-bit range", off)
			}
			words = append(words, cbnz(r2x(ins.a), off))
		case t2Ret:
			words = append(words, movReg(0, r2x(ins.a)))
			words = append(words, ret())
		}
	}

	buf := make([]byte, len(words)*4)
	for i, w := range words {
		binary.LittleEndian.PutUint32(buf[i*4:], w)
	}

	page, err := syscall.Mmap(-1, 0, pageRound(len(buf)),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE|mapJit)
	if err != nil {
		return nil, fmt.Errorf("tieredjit: mmap: %w", err)
	}
	pthread_jit_write_protect_np(false)
	copy(page, buf)
	pthread_jit_write_protect_np(true)
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		_ = syscall.Munmap(page)
		return nil, fmt.Errorf("tieredjit: mprotect: %w", err)
	}
	sys_icache_invalidate(unsafe.Pointer(&page[0]), uintptr(len(page)))

	return &CompiledFunc{
		entry: unsafe.Pointer(&page[0]),
		page:  page,
		opt:   opt,
	}, nil
}

// CompiledFunc owns a JIT page.
type CompiledFunc struct {
	entry unsafe.Pointer
	page  []byte
	opt   optProgram
}

// CodeLen returns the executable code size in bytes.
func (c *CompiledFunc) CodeLen() int { return len(c.page) }

// Free releases the JIT page.
func (c *CompiledFunc) Free() error {
	if c.page == nil {
		return nil
	}
	err := syscall.Munmap(c.page)
	c.page = nil
	c.entry = nil
	return err
}

const mapJit = 0x0800

func pageRound(n int) int {
	const ps = 16384
	return (n + ps - 1) &^ (ps - 1)
}
