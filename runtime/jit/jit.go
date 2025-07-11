package jit

import (
	"encoding/binary"
	"math"
	"strings"
	"syscall"
	"unicode/utf8"
	"unsafe"
)

var funcAddrs map[string]uintptr

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
	movRaxImm      = []byte{0x48, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0}
	movRbxImm      = []byte{0x48, 0xBB, 0, 0, 0, 0, 0, 0, 0, 0}
	movRaxRbx      = []byte{0x48, 0x89, 0xD8}
	movRbxRax      = []byte{0x48, 0x89, 0xC3}
	pushRax        = []byte{0x50}
	pushRbx        = []byte{0x53}
	popRbx         = []byte{0x5B}
	addRaxRbx      = []byte{0x48, 0x01, 0xD8}
	subRaxRbx      = []byte{0x48, 0x29, 0xD8}
	imulRaxRbx     = []byte{0x48, 0x0F, 0xAF, 0xC3}
	andRaxRbx      = []byte{0x48, 0x21, 0xD8}
	orRaxRbx       = []byte{0x48, 0x09, 0xD8}
	xorRaxRbx      = []byte{0x48, 0x31, 0xD8}
	movqXmm0Rax    = []byte{0x66, 0x48, 0x0F, 0x6E, 0xC0}
	movqXmm0Rbx    = []byte{0x66, 0x48, 0x0F, 0x6E, 0xC3}
	movqXmm1Rax    = []byte{0x66, 0x48, 0x0F, 0x6E, 0xC8}
	movqXmm1Rbx    = []byte{0x66, 0x48, 0x0F, 0x6E, 0xCB}
	movqRaxXmm0    = []byte{0x66, 0x48, 0x0F, 0x7E, 0xC0}
	movqRbxXmm1    = []byte{0x66, 0x48, 0x0F, 0x7E, 0xCB}
	addsdX0X1      = []byte{0xF2, 0x0F, 0x58, 0xC1}
	subsdX0X1      = []byte{0xF2, 0x0F, 0x5C, 0xC1}
	mulsdX0X1      = []byte{0xF2, 0x0F, 0x59, 0xC1}
	divsdX0X1      = []byte{0xF2, 0x0F, 0x5E, 0xC1}
	comisdX0X1     = []byte{0x66, 0x0F, 0x2F, 0xC1}
	cvtsi2sdX0Rax  = []byte{0xF2, 0x48, 0x0F, 0x2A, 0xC0}
	cvtsi2sdX0Rbx  = []byte{0xF2, 0x48, 0x0F, 0x2A, 0xC3}
	cvtsi2sdX1Rax  = []byte{0xF2, 0x48, 0x0F, 0x2A, 0xC8}
	cvtsi2sdX1Rbx  = []byte{0xF2, 0x48, 0x0F, 0x2A, 0xCB}
	cvttsd2siRaxX0 = []byte{0xF2, 0x48, 0x0F, 0x2C, 0xC0}
	cvttsd2siRaxX1 = []byte{0xF2, 0x48, 0x0F, 0x2C, 0xC1}
	cqoInsn        = []byte{0x48, 0x99}
	idivRbx        = []byte{0x48, 0xF7, 0xFB}
	movRaxRdx      = []byte{0x48, 0x89, 0xD0}
	cmpRaxRbx      = []byte{0x48, 0x39, 0xD8}
	testRaxRax     = []byte{0x48, 0x85, 0xC0}
	jzRel32        = []byte{0x0F, 0x84, 0, 0, 0, 0}
	jnzRel32       = []byte{0x0F, 0x85, 0, 0, 0, 0}
	jmpRel32       = []byte{0xE9, 0, 0, 0, 0}
	movRdiImm      = []byte{0x48, 0xBF, 0, 0, 0, 0, 0, 0, 0, 0}
	movRsiImm      = []byte{0x48, 0xBE, 0, 0, 0, 0, 0, 0, 0, 0}
	movRdiRax      = []byte{0x48, 0x89, 0xC7}
	movRsiRax      = []byte{0x48, 0x89, 0xC6}
	movRaxRdi      = []byte{0x48, 0x89, 0xF8}
	movRaxRsi      = []byte{0x48, 0x89, 0xF0}
	callRax        = []byte{0xFF, 0xD0}
	retInsn        = []byte{0xC3}
)

// MovRaxImm emits `mov rax, imm64`.
func (a *Assembler) MovRaxImm(v int64) {
	start := len(a.code)
	a.EmitRaw(movRaxImm)
	binary.LittleEndian.PutUint64(a.code[start+2:], uint64(v))
}

// MovRbxImm emits `mov rbx, imm64`.
func (a *Assembler) MovRbxImm(v int64) {
	start := len(a.code)
	a.EmitRaw(movRbxImm)
	binary.LittleEndian.PutUint64(a.code[start+2:], uint64(v))
}

// MovRdiImm emits `mov rdi, imm64`.
func (a *Assembler) MovRdiImm(v int64) {
	start := len(a.code)
	a.EmitRaw(movRdiImm)
	binary.LittleEndian.PutUint64(a.code[start+2:], uint64(v))
}

// MovRsiImm emits `mov rsi, imm64`.
func (a *Assembler) MovRsiImm(v int64) {
	start := len(a.code)
	a.EmitRaw(movRsiImm)
	binary.LittleEndian.PutUint64(a.code[start+2:], uint64(v))
}

// MovRdiRax emits `mov rdi, rax`.
func (a *Assembler) MovRdiRax() { a.EmitRaw(movRdiRax) }

// MovRsiRax emits `mov rsi, rax`.
func (a *Assembler) MovRsiRax() { a.EmitRaw(movRsiRax) }

// MovRaxRdi emits `mov rax, rdi`.
func (a *Assembler) MovRaxRdi() { a.EmitRaw(movRaxRdi) }

// MovRaxRsi emits `mov rax, rsi`.
func (a *Assembler) MovRaxRsi() { a.EmitRaw(movRaxRsi) }

// CallRax emits `call rax`.
func (a *Assembler) CallRax() { a.EmitRaw(callRax) }

// PushRax emits `push rax`.
func (a *Assembler) PushRax() { a.EmitRaw(pushRax) }

// PushRbx emits `push rbx`.
func (a *Assembler) PushRbx() { a.EmitRaw(pushRbx) }

// PopRbx emits `pop rbx`.
func (a *Assembler) PopRbx() { a.EmitRaw(popRbx) }

// MovRaxRbx emits `mov rax, rbx`.
func (a *Assembler) MovRaxRbx() { a.EmitRaw(movRaxRbx) }

// MovRbxRax emits `mov rbx, rax`.
func (a *Assembler) MovRbxRax() { a.EmitRaw(movRbxRax) }

// AddRaxRbx emits `add rax, rbx`.
func (a *Assembler) AddRaxRbx() { a.EmitRaw(addRaxRbx) }

// SubRaxRbx emits `sub rax, rbx`.
func (a *Assembler) SubRaxRbx() { a.EmitRaw(subRaxRbx) }

// IMulRaxRbx emits `imul rax, rbx`.
func (a *Assembler) IMulRaxRbx() { a.EmitRaw(imulRaxRbx) }

// AndRaxRbx emits `and rax, rbx`.
func (a *Assembler) AndRaxRbx() { a.EmitRaw(andRaxRbx) }

// OrRaxRbx emits `or rax, rbx`.
func (a *Assembler) OrRaxRbx() { a.EmitRaw(orRaxRbx) }

// XorRaxRbx emits `xor rax, rbx`.
func (a *Assembler) XorRaxRbx() { a.EmitRaw(xorRaxRbx) }

// MovqXmm0Rax moves RAX into XMM0.
func (a *Assembler) MovqXmm0Rax() { a.EmitRaw(movqXmm0Rax) }

// MovqXmm0Rbx moves RBX into XMM0.
func (a *Assembler) MovqXmm0Rbx() { a.EmitRaw(movqXmm0Rbx) }

// MovqXmm1Rax moves RAX into XMM1.
func (a *Assembler) MovqXmm1Rax() { a.EmitRaw(movqXmm1Rax) }

// MovqXmm1Rbx moves RBX into XMM1.
func (a *Assembler) MovqXmm1Rbx() { a.EmitRaw(movqXmm1Rbx) }

// MovqRaxXmm0 moves XMM0 into RAX.
func (a *Assembler) MovqRaxXmm0() { a.EmitRaw(movqRaxXmm0) }

// MovqRbxXmm1 moves XMM1 into RBX.
func (a *Assembler) MovqRbxXmm1() { a.EmitRaw(movqRbxXmm1) }

// AddSdXmm0Xmm1 emits `addsd xmm0, xmm1`.
func (a *Assembler) AddSdXmm0Xmm1() { a.EmitRaw(addsdX0X1) }

// SubSdXmm0Xmm1 emits `subsd xmm0, xmm1`.
func (a *Assembler) SubSdXmm0Xmm1() { a.EmitRaw(subsdX0X1) }

// MulSdXmm0Xmm1 emits `mulsd xmm0, xmm1`.
func (a *Assembler) MulSdXmm0Xmm1() { a.EmitRaw(mulsdX0X1) }

// DivSdXmm0Xmm1 emits `divsd xmm0, xmm1`.
func (a *Assembler) DivSdXmm0Xmm1() { a.EmitRaw(divsdX0X1) }

// ComisdXmm0Xmm1 emits `comisd xmm1, xmm0`.
func (a *Assembler) ComisdXmm0Xmm1() { a.EmitRaw(comisdX0X1) }

// Cvtsi2sdXmm0Rax converts int64 in RAX to double in XMM0.
func (a *Assembler) Cvtsi2sdXmm0Rax() { a.EmitRaw(cvtsi2sdX0Rax) }

// Cvtsi2sdXmm0Rbx converts int64 in RBX to double in XMM0.
func (a *Assembler) Cvtsi2sdXmm0Rbx() { a.EmitRaw(cvtsi2sdX0Rbx) }

// Cvtsi2sdXmm1Rax converts int64 in RAX to double in XMM1.
func (a *Assembler) Cvtsi2sdXmm1Rax() { a.EmitRaw(cvtsi2sdX1Rax) }

// Cvtsi2sdXmm1Rbx converts int64 in RBX to double in XMM1.
func (a *Assembler) Cvtsi2sdXmm1Rbx() { a.EmitRaw(cvtsi2sdX1Rbx) }

// Cvttsd2siRaxXmm0 converts double in XMM0 to int64 in RAX.
func (a *Assembler) Cvttsd2siRaxXmm0() { a.EmitRaw(cvttsd2siRaxX0) }

// Cvttsd2siRaxXmm1 converts double in XMM1 to int64 in RAX.
func (a *Assembler) Cvttsd2siRaxXmm1() { a.EmitRaw(cvttsd2siRaxX1) }

// IDivRbx emits `cqo; idiv rbx` storing the quotient in rax.
func (a *Assembler) IDivRbx() {
	a.EmitRaw(cqoInsn)
	a.EmitRaw(idivRbx)
}

// IModRaxRbx emits `cqo; idiv rbx` and moves the remainder to rax.
func (a *Assembler) IModRaxRbx() {
	a.EmitRaw(cqoInsn)
	a.EmitRaw(idivRbx)
	a.EmitRaw(movRaxRdx)
}

// CmpRaxRbx emits `cmp rax, rbx`.
func (a *Assembler) CmpRaxRbx() { a.EmitRaw(cmpRaxRbx) }

// TestRaxRax emits `test rax, rax`.
func (a *Assembler) TestRaxRax() { a.EmitRaw(testRaxRax) }

// JzPlaceholder emits a "jz" instruction with a dummy 32-bit offset and returns
// the patch position of the offset.
func (a *Assembler) JzPlaceholder() int {
	start := len(a.code)
	a.EmitRaw(jzRel32)
	return start + 2
}

// JnzPlaceholder emits a "jnz" instruction with a dummy 32-bit offset and returns
// the patch position of the offset.
func (a *Assembler) JnzPlaceholder() int {
	start := len(a.code)
	a.EmitRaw(jnzRel32)
	return start + 2
}

// JmpPlaceholder emits an unconditional jump with a dummy offset and returns
// the patch position of the offset.
func (a *Assembler) JmpPlaceholder() int {
	start := len(a.code)
	a.EmitRaw(jmpRel32)
	return start + 1
}

// PatchRelative patches a 32-bit relative offset at pos to target. The offset
// is calculated relative to the end of the offset field.
func (a *Assembler) PatchRelative(pos int, target int) {
	rel := int32(target - (pos + 4))
	binary.LittleEndian.PutUint32(a.code[pos:], uint32(rel))
}

// SetCC emits a conditional set on AL followed by movzx into RAX.
func (a *Assembler) SetCC(op string) {
	var code byte
	switch op {
	case "==":
		code = 0x94 // sete
	case "!=":
		code = 0x95 // setne
	case "<":
		code = 0x9C // setl
	case "<=":
		code = 0x9E // setle
	case ">":
		code = 0x9F // setg
	case ">=":
		code = 0x9D // setge
	default:
		return
	}
	a.EmitRaw([]byte{0x0F, code, 0xC0})       // setcc al
	a.EmitRaw([]byte{0x48, 0x0F, 0xB6, 0xC0}) // movzx rax, al
}

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

// FinalizePtr allocates executable memory and returns a pointer to it.
func (a *Assembler) FinalizePtr() (uintptr, error) {
	buf := make([]byte, len(a.code))
	copy(buf, a.code)
	mem, err := syscall.Mmap(-1, 0, len(buf), syscall.PROT_READ|syscall.PROT_WRITE|syscall.PROT_EXEC, syscall.MAP_ANON|syscall.MAP_PRIVATE)
	if err != nil {
		return 0, err
	}
	copy(mem, buf)
	return uintptr(unsafe.Pointer(&mem[0])), nil
}

// --- Expression AST ---

type Expr interface {
	compile(a *Assembler)
	isFloat() bool
}

type IntLit struct{ Val int64 }

func (i IntLit) compile(a *Assembler) { a.MovRaxImm(i.Val) }

func (IntLit) isFloat() bool { return false }

type BoolLit struct{ Val bool }

func (b BoolLit) compile(a *Assembler) {
	if b.Val {
		a.MovRaxImm(1)
	} else {
		a.MovRaxImm(0)
	}
}

func (BoolLit) isFloat() bool { return false }

type FloatLit struct{ Val float64 }

func (f FloatLit) compile(a *Assembler) {
	bits := int64(math.Float64bits(f.Val))
	a.MovRaxImm(bits)
}

func (FloatLit) isFloat() bool { return true }

// StrLit represents a literal string.
type StrLit struct{ Val string }

func (StrLit) compile(a *Assembler) {}

func (StrLit) isFloat() bool { return false }

// ListLit represents a literal list of integers.
type ListLit struct {
	Elems []int64
}

func (ListLit) compile(a *Assembler) {}

func (ListLit) isFloat() bool { return false }

// Var references a function parameter by index.
type Var struct{ Index int }

func (v Var) compile(a *Assembler) {
	switch v.Index {
	case 0:
		a.MovRaxRdi()
	case 1:
		a.MovRaxRsi()
	default:
		a.MovRaxImm(0)
	}
}

func (Var) isFloat() bool { return false }

// Call represents a call to a named function.
type Call struct {
	Name string
	Args []Expr
}

func (c Call) compile(a *Assembler) {
	for i, arg := range c.Args {
		arg.compile(a)
		switch i {
		case 0:
			a.MovRdiRax()
		case 1:
			a.MovRsiRax()
		}
	}
	if addr, ok := funcAddrs[c.Name]; ok {
		a.MovRaxImm(int64(addr))
		a.CallRax()
	} else {
		a.MovRaxImm(0)
	}
}

func (Call) isFloat() bool { return false }

type UnOp struct {
	Op   string
	Expr Expr
}

func (u UnOp) compile(a *Assembler) {
	u.Expr.compile(a)
	switch u.Op {
	case "-":
		a.EmitRaw([]byte{0x48, 0xF7, 0xD8}) // neg rax
	case "!":
		a.EmitRaw([]byte{0x48, 0x83, 0xF8, 0x00}) // cmp rax, 0
		a.EmitRaw([]byte{0x0F, 0x94, 0xC0})       // sete al
		a.EmitRaw([]byte{0x48, 0x0F, 0xB6, 0xC0}) // movzx rax, al
	}
}

func (UnOp) isFloat() bool { return false }

// FUnOp represents unary operations on floating point values.
type FUnOp struct {
	Op   string
	Expr Expr
}

func (u FUnOp) compile(a *Assembler) {
	u.Expr.compile(a)
	switch u.Op {
	case "-":
		a.MovRbxImm(math.MinInt64)
		a.XorRaxRbx()
	}
}

func (FUnOp) isFloat() bool { return true }

type BinOp struct {
	Op    string
	Left  Expr
	Right Expr
}

func (BinOp) isFloat() bool { return false }

// FBinOp represents a floating point binary operation.
type FBinOp struct {
	Op    string
	Left  Expr
	Right Expr
}

func (FBinOp) isFloat() bool { return true }

// Cast converts between supported primitive types.
type Cast struct {
	Expr Expr
	To   string
}

func (c Cast) isFloat() bool { return c.To == "float" }

// LenExpr returns the length of a literal integer list.
type LenExpr struct {
	Expr Expr
}

func (LenExpr) isFloat() bool { return false }

// IfExpr represents a simple if-else expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (i IfExpr) isFloat() bool {
	if i.Then.isFloat() {
		return true
	}
	if i.Else != nil {
		return i.Else.isFloat()
	}
	return false
}

func (b BinOp) compile(a *Assembler) {
	if b.Op == "==" || b.Op == "!=" {
		if l, lok := b.Left.(ListLit); lok {
			if r, rok := b.Right.(ListLit); rok {
				eq := len(l.Elems) == len(r.Elems)
				if eq {
					for i := range l.Elems {
						if l.Elems[i] != r.Elems[i] {
							eq = false
							break
						}
					}
				}
				if b.Op == "!=" {
					eq = !eq
				}
				if eq {
					a.MovRaxImm(1)
				} else {
					a.MovRaxImm(0)
				}
				return
			}
		}
		l, lok := b.Left.(StrLit)
		r, rok := b.Right.(StrLit)
		if lok && rok {
			res := l.Val == r.Val
			if b.Op == "!=" {
				res = !res
			}
			if res {
				a.MovRaxImm(1)
			} else {
				a.MovRaxImm(0)
			}
			return
		}
	}
	switch b.Op {
	case "&&":
		b.Left.compile(a)
		a.TestRaxRax()
		skip := a.JzPlaceholder()
		b.Right.compile(a)
		a.PatchRelative(skip, len(a.Code()))
	case "||":
		b.Left.compile(a)
		a.TestRaxRax()
		skip := a.JnzPlaceholder()
		b.Right.compile(a)
		a.PatchRelative(skip, len(a.Code()))
	case "in":
		if list, ok := b.Right.(ListLit); ok {
			b.Left.compile(a)
			patches := []int{}
			for _, v := range list.Elems {
				a.MovRbxImm(v)
				a.CmpRaxRbx()
				patches = append(patches, a.JzPlaceholder())
			}
			a.MovRaxImm(0)
			end := a.JmpPlaceholder()
			target := len(a.Code())
			for _, p := range patches {
				a.PatchRelative(p, target)
			}
			a.MovRaxImm(1)
			a.PatchRelative(end, len(a.Code()))
			return
		}
		if str, ok := b.Right.(StrLit); ok {
			sub, ok := b.Left.(StrLit)
			if !ok {
				a.MovRaxImm(0)
				return
			}
			if strings.Contains(str.Val, sub.Val) {
				a.MovRaxImm(1)
			} else {
				a.MovRaxImm(0)
			}
			return
		}
		a.MovRaxImm(0)
		return
	default:
		b.Right.compile(a)
		a.PushRax()
		b.Left.compile(a)
		a.PopRbx() // rbx = right, rax = left
		switch b.Op {
		case "+":
			a.AddRaxRbx()
		case "-":
			a.SubRaxRbx()
		case "*":
			a.IMulRaxRbx()
		case "/":
			a.IDivRbx()
		case "%":
			a.IModRaxRbx()
		case "==", "!=", "<", "<=", ">", ">=":
			a.CmpRaxRbx()
			a.SetCC(b.Op)
		}
	}
}

func (b FBinOp) compile(a *Assembler) {
	rightFloat := false
	if f, ok := b.Right.(interface{ isFloat() bool }); ok && f.isFloat() {
		rightFloat = true
	}
	leftFloat := false
	if f, ok := b.Left.(interface{ isFloat() bool }); ok && f.isFloat() {
		leftFloat = true
	}

	b.Right.compile(a)
	a.PushRax()
	b.Left.compile(a)
	if leftFloat {
		a.MovqXmm0Rax()
	} else {
		a.Cvtsi2sdXmm0Rax()
	}
	a.PopRbx() // rbx = right bits
	if rightFloat {
		a.MovqXmm1Rbx()
	} else {
		a.Cvtsi2sdXmm1Rbx()
	}
	switch b.Op {
	case "+":
		a.AddSdXmm0Xmm1()
	case "-":
		a.SubSdXmm0Xmm1()
	case "*":
		a.MulSdXmm0Xmm1()
	case "/":
		a.DivSdXmm0Xmm1()
	case "==", "!=", "<", "<=", ">", ">=":
		a.ComisdXmm0Xmm1()
		a.SetCC(b.Op)
		return
	}
	a.MovqRaxXmm0()
}

func (c Cast) compile(a *Assembler) {
	c.Expr.compile(a)
	switch c.To {
	case "float":
		a.Cvtsi2sdXmm0Rax()
		a.MovqRaxXmm0()
	case "int":
		a.MovqXmm0Rax()
		a.Cvttsd2siRaxXmm0()
	}
}

func (l LenExpr) compile(a *Assembler) {
	switch v := l.Expr.(type) {
	case ListLit:
		a.MovRaxImm(int64(len(v.Elems)))
		return
	case StrLit:
		a.MovRaxImm(int64(utf8.RuneCountInString(v.Val)))
		return
	}
	// unsupported operand, default to 0
	a.MovRaxImm(0)
}

func (i IfExpr) compile(a *Assembler) {
	i.Cond.compile(a)
	a.TestRaxRax()
	elsePatch := a.JzPlaceholder()
	i.Then.compile(a)
	endPatch := a.JmpPlaceholder()
	a.PatchRelative(elsePatch, len(a.code))
	if i.Else != nil {
		i.Else.compile(a)
	} else {
		a.MovRaxImm(0)
	}
	a.PatchRelative(endPatch, len(a.code))
}

// FunctionDef defines a simple function.
type FunctionDef struct {
	Name   string
	Params int
	Body   Expr
}

// Program contains a set of functions and a main expression.
type Program struct {
	Funcs []FunctionDef
	Main  Expr
}

// Compile converts an expression to executable code using copy-and-patch.
func Compile(e Expr) (func() int64, error) {
	asm := New()
	asm.PushRbx()
	e.compile(asm)
	asm.PopRbx()
	asm.Ret()
	return asm.Finalize()
}

// CompileProgram compiles a program with function definitions.
func CompileProgram(p Program) (func() int64, error) {
	funcAddrs = map[string]uintptr{}
	for _, fn := range p.Funcs {
		asm := New()
		asm.PushRbx()
		fn.Body.compile(asm)
		asm.PopRbx()
		asm.Ret()
		addr, err := asm.FinalizePtr()
		if err != nil {
			return nil, err
		}
		funcAddrs[fn.Name] = addr
	}
	asm := New()
	asm.PushRbx()
	p.Main.compile(asm)
	asm.PopRbx()
	asm.Ret()
	return asm.Finalize()
}
