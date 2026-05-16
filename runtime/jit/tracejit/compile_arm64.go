//go:build darwin && arm64

package tracejit

import (
	"encoding/binary"
	"fmt"
	"syscall"
	"unsafe"

	"mochi/runtime/jit/tmpljit"
)

// Architecture mapping. Identical to tmpljit's r2x (x9..x15) so the
// instruction sequences for OpAdd/OpMul/OpLt/OpMovImm can be
// generated word-for-word. The point of the duplication is to make
// it obvious that the difference between MEP-30 and MEP-31 is not
// the per-op codegen but the surrounding control flow: MEP-30
// compiles a function entered from outside; MEP-31 compiles a loop
// that owns its own back-edge and side-exits.
func r2x(r uint8) uint32 {
	if r >= tmpljit.NumRegs {
		panic(fmt.Sprintf("tracejit: register %d out of range", r))
	}
	return uint32(r) + 9
}

// AArch64 encoders (subset; mirrors tmpljit/emit_arm64.go).
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
func cmpReg(xn, xm uint32) uint32 {
	return 0xEB00001F | ((xm & 0x1F) << 16) | ((xn & 0x1F) << 5)
}

const condGE = 0xA

func csetLT(xd uint32) uint32 {
	return 0x9A9F07E0 | (condGE << 12) | (xd & 0x1F)
}

// cbz / cbnz with a 19-bit signed offset in instruction units.
func cbz(xd uint32, off19 int32) uint32 {
	return 0xB4000000 | (uint32(off19&0x7FFFF) << 5) | (xd & 0x1F)
}
func cbnz(xd uint32, off19 int32) uint32 {
	return 0xB5000000 | (uint32(off19&0x7FFFF) << 5) | (xd & 0x1F)
}

// ldr xt, [xn, #imm12*8]
func ldr64(xt, xn, imm12 uint32) uint32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

// str xt, [xn, #imm12*8]
func str64(xt, xn, imm12 uint32) uint32 {
	return 0xF9000000 | ((imm12 & 0xFFF) << 10) | ((xn & 0x1F) << 5) | (xt & 0x1F)
}

func ret() uint32 { return 0xD65F03C0 }

// codeLen returns the number of arm64 instructions the body opcode
// lowers to. The Jnz of the original bytecode is rewritten by the
// compiler; recording captured the rest verbatim.
func codeLen(op tmpljit.Op) int {
	switch op {
	case tmpljit.OpMovImm:
		return 2
	case tmpljit.OpAdd, tmpljit.OpMul:
		return 1
	case tmpljit.OpLt:
		return 2
	case tmpljit.OpJnz:
		// Replaced at compile time by a 1-instruction cbnz that
		// loops back to the start of the body. The "exit" path is
		// the fall-through: the epilogue follows the back-edge
		// instruction in the byte stream.
		return 1
	}
	panic("tracejit: bad opcode in trace body")
}

// emitBodyOp emits one trace body instruction. Returns the words
// appended. Identical to tmpljit's per-op lowering.
func emitBodyOp(words []uint32, ins tmpljit.Instr) []uint32 {
	switch ins.Op {
	case tmpljit.OpMovImm:
		xd := r2x(ins.Dst)
		imm := ins.Imm
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
	case tmpljit.OpAdd:
		words = append(words, addReg(r2x(ins.Dst), r2x(ins.A), r2x(ins.B)))
	case tmpljit.OpMul:
		words = append(words, mulReg(r2x(ins.Dst), r2x(ins.A), r2x(ins.B)))
	case tmpljit.OpLt:
		words = append(words, cmpReg(r2x(ins.A), r2x(ins.B)))
		words = append(words, csetLT(r2x(ins.Dst)))
	default:
		panic(fmt.Sprintf("tracejit: opcode %d not allowed in body", ins.Op))
	}
	return words
}

// Compile lowers a recorded Trace into ARM64 machine code with the
// calling convention:
//
//	func(regs *[NumRegs]int64)
//
// Native control flow:
//
//	prologue: load x9..x15 from regs[0..6]
//	body_top: lower body[0..N-2] verbatim          (everything before
//	                                                the OpJnz)
//	          cbnz guard_reg, body_top             (the rewritten
//	                                                back-edge: keep
//	                                                looping while
//	                                                guard != 0)
//	epilogue: store x9..x15 back to regs
//	          ret
//
// The "side-exit" in MEP-31 terminology is the fall-through of the
// cbnz: when the guard register is zero, control proceeds to the
// epilogue and the trace returns to the interpreter. The interpreter
// resumes at trace.ExitPC with the register file freshly written.
func Compile(t *Trace) (*CompiledTrace, error) {
	if len(t.Body) < 2 || t.Body[len(t.Body)-1].Op != tmpljit.OpJnz {
		return nil, fmt.Errorf("tracejit: trace body must end in OpJnz")
	}

	const numRegs = tmpljit.NumRegs
	var words []uint32

	// Prologue: load every VM register from regs[*]. x0 = regs base.
	for r := uint32(0); r < numRegs; r++ {
		words = append(words, ldr64(r2x(uint8(r)), 0, r))
	}

	bodyTop := len(words)

	// Emit every body instruction except the trailing OpJnz.
	for _, ins := range t.Body[:len(t.Body)-1] {
		words = emitBodyOp(words, ins)
	}

	// Rewrite the back-edge: cbnz guard_reg, bodyTop.
	here := len(words)
	off := int32(bodyTop - here)
	if off < -(1<<18) || off >= (1<<18) {
		return nil, fmt.Errorf("tracejit: back-edge offset %d out of 19-bit range", off)
	}
	words = append(words, cbnz(r2x(t.GuardReg), off))

	// Epilogue: store every VM register back, then ret.
	for r := uint32(0); r < numRegs; r++ {
		words = append(words, str64(r2x(uint8(r)), 0, r))
	}
	words = append(words, ret())

	buf := make([]byte, len(words)*4)
	for i, w := range words {
		binary.LittleEndian.PutUint32(buf[i*4:], w)
	}

	page, err := syscall.Mmap(-1, 0, pageRound(len(buf)),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE|mapJit)
	if err != nil {
		return nil, fmt.Errorf("tracejit: mmap: %w", err)
	}
	pthread_jit_write_protect_np(false)
	copy(page, buf)
	pthread_jit_write_protect_np(true)
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		_ = syscall.Munmap(page)
		return nil, fmt.Errorf("tracejit: mprotect: %w", err)
	}
	sys_icache_invalidate(unsafe.Pointer(&page[0]), uintptr(len(page)))

	return &CompiledTrace{
		entry:  unsafe.Pointer(&page[0]),
		page:   page,
		trace:  t,
	}, nil
}

// CompiledTrace owns a JIT page and a back-pointer to the source Trace.
type CompiledTrace struct {
	entry unsafe.Pointer
	page  []byte
	trace *Trace
}

// Trace returns the source Trace.
func (c *CompiledTrace) Trace() *Trace { return c.trace }

// CodeLen returns the size of the JIT'd code in bytes.
func (c *CompiledTrace) CodeLen() int { return len(c.page) }

// Free releases the JIT page.
func (c *CompiledTrace) Free() error {
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
