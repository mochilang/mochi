package jit

import (
	"encoding/binary"
	"syscall"
	"unsafe"
)

// Assembler accumulates machine code fragments using copy-and-patch.
type Assembler struct {
	code []byte
}

// New creates a new Assembler.
func New() *Assembler {
	return &Assembler{code: make([]byte, 0, 1024)}
}

// Code returns the assembled machine code.
func (a *Assembler) Code() []byte { return a.code }

// EmitRaw appends raw bytes to the code buffer.
func (a *Assembler) EmitRaw(b []byte) { a.code = append(a.code, b...) }

// --- Stencil based instructions ---

var (
	movRaxImm  = []byte{0x48, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0}
	pushRax    = []byte{0x50}
	popRbx     = []byte{0x5B}
	addRaxRbx  = []byte{0x48, 0x01, 0xD8}
	subRaxRbx  = []byte{0x48, 0x29, 0xD8}
	imulRaxRbx = []byte{0x48, 0x0F, 0xAF, 0xC3}
	retInsn    = []byte{0xC3}
)

// MovRaxImm emits `mov rax, imm64`.
func (a *Assembler) MovRaxImm(v int64) {
	start := len(a.code)
	a.EmitRaw(movRaxImm)
	binary.LittleEndian.PutUint64(a.code[start+2:], uint64(v))
}

// PushRax emits `push rax`.
func (a *Assembler) PushRax() { a.EmitRaw(pushRax) }

// PopRbx emits `pop rbx`.
func (a *Assembler) PopRbx() { a.EmitRaw(popRbx) }

// AddRaxRbx emits `add rax, rbx`.
func (a *Assembler) AddRaxRbx() { a.EmitRaw(addRaxRbx) }

// SubRaxRbx emits `sub rax, rbx`.
func (a *Assembler) SubRaxRbx() { a.EmitRaw(subRaxRbx) }

// IMulRaxRbx emits `imul rax, rbx`.
func (a *Assembler) IMulRaxRbx() { a.EmitRaw(imulRaxRbx) }

// Ret emits `ret`.
func (a *Assembler) Ret() { a.EmitRaw(retInsn) }

// Finalize allocates executable memory and returns it as a function.
func (a *Assembler) Finalize() (func() int64, error) {
	buf := make([]byte, len(a.code))
	copy(buf, a.code)
	mem, err := syscall.Mmap(-1, 0, len(buf), syscall.PROT_READ|syscall.PROT_WRITE|syscall.PROT_EXEC, syscall.MAP_ANON|syscall.MAP_PRIVATE)
	if err != nil {
		return nil, err
	}
	copy(mem, buf)
	m := mem
	return func() int64 { return exec(unsafe.Pointer(&m[0])) }, nil
}

// --- Expression AST ---

type Expr interface {
	compile(a *Assembler)
}

type IntLit struct{ Val int64 }

func (i IntLit) compile(a *Assembler) { a.MovRaxImm(i.Val) }

type BinOp struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b BinOp) compile(a *Assembler) {
	b.Left.compile(a)
	a.PushRax()
	b.Right.compile(a)
	a.PopRbx()
	switch b.Op {
	case "+":
		a.AddRaxRbx()
	case "-":
		a.SubRaxRbx()
	case "*":
		a.IMulRaxRbx()
	}
}

// Compile converts an expression to executable code using copy-and-patch.
func Compile(e Expr) (func() int64, error) {
	asm := New()
	e.compile(asm)
	asm.Ret()
	return asm.Finalize()
}
