package vm

import (
	"fmt"
	"io"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Value reuses the interpreter runtime representation.
type Value = interpreter.Value

// Op defines a VM instruction opcode.
type Op uint8

const (
	OpConst Op = iota
	OpMove
	OpAdd
	OpSub
	OpEqual
	OpLess
	OpJump
	OpJumpIfFalse
	OpLen
	OpIndex
	OpMakeList
	OpPrint
	OpCall2
	OpReturn
)

func (op Op) String() string {
	switch op {
	case OpConst:
		return "Const"
	case OpMove:
		return "Move"
	case OpAdd:
		return "Add"
	case OpSub:
		return "Sub"
	case OpEqual:
		return "Equal"
	case OpLess:
		return "Less"
	case OpJump:
		return "Jump"
	case OpJumpIfFalse:
		return "JumpIfFalse"
	case OpLen:
		return "Len"
	case OpIndex:
		return "Index"
	case OpMakeList:
		return "MakeList"
	case OpPrint:
		return "Print"
	case OpCall2:
		return "Call2"
	case OpReturn:
		return "Return"
	default:
		return "?"
	}
}

type Instr struct {
	Op   Op
	A    int
	B    int
	C    int
	D    int
	Val  Value // inline constant for OpConst
	Line int   // source line for disassembly
}

type Function struct {
	Code    []Instr
	NumRegs int
	Name    string
}

type Program struct {
	Funcs []Function
}

// Disassemble returns a human-readable listing of the program instructions.
// If src is provided, source lines are included as comments.
func (p *Program) Disassemble(src string) string {
	lines := strings.Split(src, "\n")
	var b strings.Builder
	for idx, fn := range p.Funcs {
		name := fn.Name
		if name == "" {
			if idx == 0 {
				name = "main"
			} else {
				name = fmt.Sprintf("fn%d", idx)
			}
		}
		fmt.Fprintf(&b, "func %s (regs=%d)\n", name, fn.NumRegs)
		lastLine := 0
		for _, ins := range fn.Code {
			if ins.Line != lastLine && ins.Line > 0 && ins.Line <= len(lines) {
				fmt.Fprintf(&b, "  // %s\n", strings.TrimSpace(lines[ins.Line-1]))
				lastLine = ins.Line
			}
			fmt.Fprintf(&b, "  %-12s ", ins.Op)
			switch ins.Op {
			case OpConst:
				fmt.Fprintf(&b, "%s %s", formatReg(ins.A), valueToString(ins.Val))
			case OpMove:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpAdd, OpSub, OpEqual, OpLess:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpJump:
				fmt.Fprintf(&b, "%d", ins.A)
			case OpJumpIfFalse:
				fmt.Fprintf(&b, "%s, %d", formatReg(ins.A), ins.B)
			case OpLen:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpIndex:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpMakeList:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpPrint:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpCall2:
				fmt.Fprintf(&b, "%s, %d, %s, %s", formatReg(ins.A), ins.B, formatReg(ins.C), formatReg(ins.D))
			case OpReturn:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			default:
				fmt.Fprintf(&b, "%d,%d,%d,%d", ins.A, ins.B, ins.C, ins.D)
			}
			b.WriteByte('\n')
		}
		b.WriteByte('\n')
	}
	return b.String()
}

type VM struct {
	prog   *Program
	writer io.Writer
}

func New(prog *Program, w io.Writer) *VM {
	return &VM{prog: prog, writer: w}
}

func (m *VM) Run() error {
	_, err := m.call(0, nil)
	return err
}

type frame struct {
	fn   *Function
	regs []Value
	ip   int
}

func (m *VM) call(fnIndex int, args []Value) (Value, error) {
	fn := &m.prog.Funcs[fnIndex]
	f := &frame{fn: fn, regs: make([]Value, fn.NumRegs)}
	for i := 0; i < len(args) && i < len(f.regs); i++ {
		f.regs[i] = args[i]
	}
	stack := []*frame{f}
	for len(stack) > 0 {
		fr := stack[len(stack)-1]
		if fr.ip >= len(fr.fn.Code) {
			stack = stack[:len(stack)-1]
			if len(stack) == 0 {
				return Value{}, nil
			}
			continue
		}
		ins := fr.fn.Code[fr.ip]
		fr.ip++
		switch ins.Op {
		case OpConst:
			fr.regs[ins.A] = ins.Val
		case OpMove:
			fr.regs[ins.A] = fr.regs[ins.B]
		case OpAdd:
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: fr.regs[ins.B].Int + fr.regs[ins.C].Int}
		case OpSub:
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: fr.regs[ins.B].Int - fr.regs[ins.C].Int}
		case OpEqual:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: fr.regs[ins.B].Int == fr.regs[ins.C].Int}
		case OpLess:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: fr.regs[ins.B].Int < fr.regs[ins.C].Int}
		case OpJump:
			fr.ip = ins.A
		case OpJumpIfFalse:
			if !fr.regs[ins.A].Truthy() {
				fr.ip = ins.B
			}
		case OpLen:
			v := fr.regs[ins.B]
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len(v.List)}
		case OpIndex:
			list := fr.regs[ins.B].List
			idx := fr.regs[ins.C].Int
			if idx < 0 || idx >= len(list) {
				return Value{}, fmt.Errorf("index out of range")
			}
			fr.regs[ins.A] = list[idx]
		case OpMakeList:
			n := ins.B
			start := ins.C
			list := make([]Value, n)
			copy(list, fr.regs[start:start+n])
			fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: list}
		case OpPrint:
			fmt.Fprintln(m.writer, valueToAny(fr.regs[ins.A]))
		case OpCall2:
			a := fr.regs[ins.C]
			b := fr.regs[ins.D]
			res, err := m.call(ins.B, []Value{a, b})
			if err != nil {
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpReturn:
			ret := fr.regs[ins.A]
			stack = stack[:len(stack)-1]
			if len(stack) == 0 {
				return ret, nil
			}
			stack[len(stack)-1].regs[0] = ret
		default:
			return Value{}, fmt.Errorf("unknown op")
		}
	}
	return Value{}, nil
}

// --- Compiler ---

type compiler struct {
	prog    *parser.Program
	env     *types.Env
	funcs   []Function
	fnIndex map[string]int
}

type funcCompiler struct {
	fn   Function
	idx  int
	comp *compiler
	vars map[string]int
}

// Compile turns an AST into a Program supporting a limited subset of Mochi.
func Compile(p *parser.Program, env *types.Env) (*Program, error) {
	c := &compiler{prog: p, env: env, fnIndex: map[string]int{}}
	c.funcs = append(c.funcs, Function{})
	for _, st := range p.Statements {
		if st.Fun != nil {
			idx := len(c.funcs)
			c.fnIndex[st.Fun.Name] = idx
			fn := c.compileFun(st.Fun)
			c.funcs = append(c.funcs, fn)
		}
	}
	main := c.compileMain(p)
	c.funcs[0] = main
	return &Program{Funcs: c.funcs}, nil
}

func (c *compiler) compileFun(fn *parser.FunStmt) Function {
	fc := &funcCompiler{comp: c, vars: map[string]int{}}
	fc.fn.Name = fn.Name
	for i, p := range fn.Params {
		fc.vars[p.Name] = i
		if i >= fc.fn.NumRegs {
			fc.fn.NumRegs = i + 1
		}
	}
	fc.idx = len(fn.Params)
	for _, st := range fn.Body {
		fc.compileStmt(st)
	}
	fc.emit(fn.Pos, Instr{Op: OpReturn, A: 0})
	return fc.fn
}

func (c *compiler) compileMain(p *parser.Program) Function {
	fc := &funcCompiler{comp: c, vars: map[string]int{}}
	fc.fn.Name = "main"
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		fc.compileStmt(st)
	}
	fc.emit(p.Pos, Instr{Op: OpReturn, A: 0})
	return fc.fn
}

func (fc *funcCompiler) emit(pos lexer.Position, i Instr) {
	i.Line = pos.Line
	fc.fn.Code = append(fc.fn.Code, i)
}

func (fc *funcCompiler) newReg() int {
	r := fc.idx
	fc.idx++
	if fc.idx > fc.fn.NumRegs {
		fc.fn.NumRegs = fc.idx
	}
	return r
}

func (fc *funcCompiler) compileStmt(s *parser.Statement) {
	switch {
	case s.Let != nil:
		r := fc.compileExpr(s.Let.Value)
		reg := fc.newReg()
		fc.vars[s.Let.Name] = reg
		fc.emit(s.Let.Pos, Instr{Op: OpMove, A: reg, B: r})
	case s.Return != nil:
		r := fc.compileExpr(s.Return.Value)
		fc.emit(s.Return.Pos, Instr{Op: OpReturn, A: r})
	case s.Expr != nil:
		fc.compileExpr(s.Expr.Expr)
	case s.If != nil:
		fc.compileIf(s.If)
	case s.For != nil:
		fc.compileFor(s.For)
	}
}

func (fc *funcCompiler) compileExpr(e *parser.Expr) int {
	if e == nil {
		return fc.newReg()
	}
	return fc.compileBinary(e.Binary)
}

func (fc *funcCompiler) compileBinary(b *parser.BinaryExpr) int {
	left := fc.compileUnary(b.Left)
	for _, op := range b.Right {
		right := fc.compilePostfix(op.Right)
		switch op.Op {
		case "+":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpAdd, A: dst, B: left, C: right})
			left = dst
		case "-":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpSub, A: dst, B: left, C: right})
			left = dst
		case "==":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpEqual, A: dst, B: left, C: right})
			left = dst
		}
	}
	return left
}

func (fc *funcCompiler) compileUnary(u *parser.Unary) int {
	r := fc.compilePostfix(u.Value)
	for _, op := range u.Ops {
		switch op {
		case "-":
			zr := fc.newReg()
			fc.emit(u.Pos, Instr{Op: OpConst, A: zr, Val: Value{Tag: interpreter.TagInt, Int: 0}})
			dst := fc.newReg()
			fc.emit(u.Pos, Instr{Op: OpSub, A: dst, B: zr, C: r})
			r = dst
		case "!":
			// not supported
		}
	}
	return r
}

func (fc *funcCompiler) compilePostfix(p *parser.PostfixExpr) int {
	r := fc.compilePrimary(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil {
			idx := fc.compileExpr(op.Index.Start)
			dst := fc.newReg()
			fc.emit(op.Index.Pos, Instr{Op: OpIndex, A: dst, B: r, C: idx})
			r = dst
		} else if op.Call != nil {
			// Calls as postfix (e.g. foo(args)) are not used in two-sum
		}
	}
	return r
}

func (fc *funcCompiler) compilePrimary(p *parser.Primary) int {
	if p.Lit != nil && p.Lit.Int != nil {
		dst := fc.newReg()
		v := Value{Tag: interpreter.TagInt, Int: *p.Lit.Int}
		fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
		return dst
	}

	if p.List != nil {
		if v, ok := constList(p.List); ok {
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		}
		regs := make([]int, len(p.List.Elems))
		for i, e := range p.List.Elems {
			r := fc.compileExpr(e)
			reg := fc.newReg()
			fc.emit(e.Pos, Instr{Op: OpMove, A: reg, B: r})
			regs[i] = reg
		}
		dst := fc.newReg()
		fc.emit(p.Pos, Instr{Op: OpMakeList, A: dst, B: len(regs), C: regs[0]})
		return dst
	}

	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if r, ok := fc.vars[p.Selector.Root]; ok {
			return r
		}
	}

	if p.Call != nil {
		switch p.Call.Func {
		case "len":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpLen, A: dst, B: arg})
			return dst
		case "print":
			arg := fc.compileExpr(p.Call.Args[0])
			fc.emit(p.Pos, Instr{Op: OpPrint, A: arg})
			return arg
		default:
			if fnIdx, ok := fc.comp.fnIndex[p.Call.Func]; ok {
				a0 := fc.compileExpr(p.Call.Args[0])
				a1 := fc.compileExpr(p.Call.Args[1])
				dst := fc.newReg()
				fc.emit(p.Pos, Instr{Op: OpCall2, A: dst, B: fnIdx, C: a0, D: a1})
				return dst
			}
		}
	}

	return fc.newReg()
}

func (fc *funcCompiler) compileFor(f *parser.ForStmt) {
	start := fc.compileExpr(f.Source)
	end := fc.compileExpr(f.RangeEnd)
	idx, ok := fc.vars[f.Name]
	if !ok {
		idx = fc.newReg()
		fc.vars[f.Name] = idx
	}
	fc.emit(f.Pos, Instr{Op: OpMove, A: idx, B: start})
	loopStart := len(fc.fn.Code)
	cond := fc.newReg()
	fc.emit(f.Pos, Instr{Op: OpLess, A: cond, B: idx, C: end})
	jmp := len(fc.fn.Code)
	fc.emit(f.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	for _, st := range f.Body {
		fc.compileStmt(st)
	}
	one := fc.newReg()
	fc.emit(f.Pos, Instr{Op: OpConst, A: one, Val: Value{Tag: interpreter.TagInt, Int: 1}})
	tmp := fc.newReg()
	fc.emit(f.Pos, Instr{Op: OpAdd, A: tmp, B: idx, C: one})
	fc.emit(f.Pos, Instr{Op: OpMove, A: idx, B: tmp})
	fc.emit(f.Pos, Instr{Op: OpJump, A: loopStart})
	fc.fn.Code[jmp].B = len(fc.fn.Code)
}

func (fc *funcCompiler) compileIf(s *parser.IfStmt) {
	cond := fc.compileExpr(s.Cond)
	jmpFalse := len(fc.fn.Code)
	fc.emit(s.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	for _, st := range s.Then {
		fc.compileStmt(st)
	}
	endJump := -1
	if len(s.Else) > 0 {
		endJump = len(fc.fn.Code)
		fc.emit(s.Pos, Instr{Op: OpJump})
	}
	fc.fn.Code[jmpFalse].B = len(fc.fn.Code)
	if len(s.Else) > 0 {
		for _, st := range s.Else {
			fc.compileStmt(st)
		}
		fc.fn.Code[endJump].A = len(fc.fn.Code)
	}
}

func constList(l *parser.ListLiteral) (Value, bool) {
	vals := make([]Value, len(l.Elems))
	for i, e := range l.Elems {
		v, ok := constExpr(e)
		if !ok {
			return Value{}, false
		}
		vals[i] = v
	}
	return Value{Tag: interpreter.TagList, List: vals}, true
}

func constExpr(e *parser.Expr) (Value, bool) {
	return constBinary(e.Binary)
}

func constBinary(b *parser.BinaryExpr) (Value, bool) {
	if len(b.Right) != 0 {
		return Value{}, false
	}
	return constUnary(b.Left)
}

func constUnary(u *parser.Unary) (Value, bool) {
	if len(u.Ops) == 1 && u.Ops[0] == "-" {
		v, ok := constPostfix(u.Value)
		if ok && v.Tag == interpreter.TagInt {
			v.Int = -v.Int
			return v, true
		}
		return Value{}, false
	}
	if len(u.Ops) > 0 {
		return Value{}, false
	}
	return constPostfix(u.Value)
}

func constPostfix(p *parser.PostfixExpr) (Value, bool) {
	if len(p.Ops) != 0 {
		return Value{}, false
	}
	return constPrimary(p.Target)
}

func constPrimary(p *parser.Primary) (Value, bool) {
	if p.Lit != nil {
		if p.Lit.Int != nil {
			return Value{Tag: interpreter.TagInt, Int: *p.Lit.Int}, true
		}
		if p.Lit.Bool != nil {
			return Value{Tag: interpreter.TagBool, Bool: bool(*p.Lit.Bool)}, true
		}
		if p.Lit.Str != nil {
			return Value{Tag: interpreter.TagStr, Str: *p.Lit.Str}, true
		}
	}
	if p.List != nil {
		return constList(p.List)
	}
	return Value{}, false
}

func valueToAny(v Value) any {
	switch v.Tag {
	case interpreter.TagInt:
		return v.Int
	case interpreter.TagBool:
		return v.Bool
	case interpreter.TagStr:
		return v.Str
	case interpreter.TagList:
		out := make([]any, len(v.List))
		for i, x := range v.List {
			out[i] = valueToAny(x)
		}
		return out
	default:
		return nil
	}
}

func valueToString(v Value) string {
	switch v.Tag {
	case interpreter.TagInt:
		return fmt.Sprintf("%d", v.Int)
	case interpreter.TagBool:
		return fmt.Sprintf("%v", v.Bool)
	case interpreter.TagStr:
		return fmt.Sprintf("%q", v.Str)
	case interpreter.TagList:
		parts := make([]string, len(v.List))
		for i, x := range v.List {
			parts[i] = valueToString(x)
		}
		return "[" + strings.Join(parts, ", ") + "]"
	default:
		return "nil"
	}
}

func formatReg(n int) string {
	return fmt.Sprintf("r%d", n)
}
