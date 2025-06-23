package vm

import (
	"encoding/json"
	"fmt"
	"io"
	"math"
	"sort"
	"strings"
	"time"

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
	OpMul
	OpDiv
	OpMod
	OpEqual
	OpNotEqual
	OpLess
	OpLessEq
	OpIn
	OpJump
	OpJumpIfFalse
	OpLen
	OpIndex
	OpSetIndex
	OpMakeList
	OpPrint
	OpPrint2
	OpCall2
	OpCall
	OpCallV
	OpReturn
	OpNot
	OpJumpIfTrue
	OpNow
	OpJSON
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
	case OpMul:
		return "Mul"
	case OpDiv:
		return "Div"
	case OpMod:
		return "Mod"
	case OpEqual:
		return "Equal"
	case OpNotEqual:
		return "NotEqual"
	case OpLess:
		return "Less"
	case OpLessEq:
		return "LessEq"
	case OpIn:
		return "In"
	case OpJump:
		return "Jump"
	case OpJumpIfFalse:
		return "JumpIfFalse"
	case OpLen:
		return "Len"
	case OpIndex:
		return "Index"
	case OpSetIndex:
		return "SetIndex"
	case OpMakeList:
		return "MakeList"
	case OpPrint:
		return "Print"
	case OpPrint2:
		return "Print2"
	case OpCall2:
		return "Call2"
	case OpCall:
		return "Call"
	case OpCallV:
		return "CallV"
	case OpReturn:
		return "Return"
	case OpNot:
		return "Not"
	case OpJumpIfTrue:
		return "JumpIfTrue"
	case OpNow:
		return "Now"
	case OpJSON:
		return "JSON"
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
	Line    int // source line of function definition
}

type Program struct {
	Funcs []Function
}

func (p *Program) funcName(idx int) string {
	if idx < 0 || idx >= len(p.Funcs) {
		return fmt.Sprintf("%d", idx)
	}
	name := p.Funcs[idx].Name
	if name == "" {
		if idx == 0 {
			return "main"
		}
		return fmt.Sprintf("fn%d", idx)
	}
	return name
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

		// build label map for jump targets
		labels := map[int]string{}
		nextLabel := 0
		for i, ins := range fn.Code {
			switch ins.Op {
			case OpJump:
				if _, ok := labels[ins.A]; !ok {
					labels[ins.A] = fmt.Sprintf("L%d", nextLabel)
					nextLabel++
				}
			case OpJumpIfFalse, OpJumpIfTrue:
				if _, ok := labels[ins.B]; !ok {
					labels[ins.B] = fmt.Sprintf("L%d", nextLabel)
					nextLabel++
				}
			}
			_ = i
		}

		if fn.Line > 0 && fn.Line <= len(lines) {
			fmt.Fprintf(&b, "  // %s\n", strings.TrimSpace(lines[fn.Line-1]))
		}
		fmt.Fprintf(&b, "func %s (regs=%d)\n", name, fn.NumRegs)
		lastLine := 0
		for pc, ins := range fn.Code {
			if lbl, ok := labels[pc]; ok {
				fmt.Fprintf(&b, "%s:\n", lbl)
			}
			if ins.Line != lastLine && ins.Line > 0 && ins.Line <= len(lines) {
				fmt.Fprintf(&b, "  // %s\n", strings.TrimSpace(lines[ins.Line-1]))
				lastLine = ins.Line
			}
			fmt.Fprintf(&b, "  %-12s ", ins.Op)
			switch ins.Op {
			case OpConst:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), valueToString(ins.Val))
			case OpMove:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpEqual, OpNotEqual, OpLess, OpLessEq, OpIn:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpSetIndex:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpJump:
				if lbl, ok := labels[ins.A]; ok {
					fmt.Fprintf(&b, "%s", lbl)
				} else {
					fmt.Fprintf(&b, "%d", ins.A)
				}
			case OpJumpIfFalse, OpJumpIfTrue:
				if lbl, ok := labels[ins.B]; ok {
					fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), lbl)
				} else {
					fmt.Fprintf(&b, "%s, %d", formatReg(ins.A), ins.B)
				}
			case OpLen:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpIndex:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpMakeList:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpPrint:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpPrint2:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpNow:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpJSON:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpCall2:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpCall:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatRegs(ins.D, ins.C))
			case OpCallV:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), ins.C, formatReg(ins.D))
			case OpReturn:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpNot:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
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
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) + toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int + c.Int}
			}
		case OpSub:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) - toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int - c.Int}
			}
		case OpMul:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) * toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int * c.Int}
			}
		case OpDiv:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if (c.Tag == interpreter.TagInt && c.Int == 0) || (c.Tag == interpreter.TagFloat && c.Float == 0) {
				return Value{}, fmt.Errorf("division by zero")
			}
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) / toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int / c.Int}
			}
		case OpMod:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if (c.Tag == interpreter.TagInt && c.Int == 0) || (c.Tag == interpreter.TagFloat && c.Float == 0) {
				return Value{}, fmt.Errorf("division by zero")
			}
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int % c.Int}
			}
		case OpEqual:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpNotEqual:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: !valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpLess:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) < toFloat(fr.regs[ins.C])}
		case OpLessEq:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) <= toFloat(fr.regs[ins.C])}
		case OpIn:
			item := fr.regs[ins.B]
			list := fr.regs[ins.C].List
			found := false
			for _, v := range list {
				if valuesEqual(v, item) {
					found = true
					break
				}
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: found}
		case OpJump:
			fr.ip = ins.A
		case OpJumpIfFalse:
			if !fr.regs[ins.A].Truthy() {
				fr.ip = ins.B
			}
		case OpJumpIfTrue:
			if fr.regs[ins.A].Truthy() {
				fr.ip = ins.B
			}
		case OpLen:
			v := fr.regs[ins.B]
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len(v.List)}
		case OpIndex:
			src := fr.regs[ins.B]
			idxVal := fr.regs[ins.C]
			switch src.Tag {
			case interpreter.TagList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(src.List)
				}
				if idx < 0 || idx >= len(src.List) {
					return Value{}, fmt.Errorf("index out of range")
				}
				fr.regs[ins.A] = src.List[idx]
			case interpreter.TagMap:
				var key string
				switch idxVal.Tag {
				case interpreter.TagStr:
					key = idxVal.Str
				case interpreter.TagInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					return Value{}, fmt.Errorf("invalid map key")
				}
				fr.regs[ins.A] = src.Map[key]
			default:
				return Value{}, fmt.Errorf("invalid index target")
			}
		case OpSetIndex:
			dst := &fr.regs[ins.A]
			idxVal := fr.regs[ins.B]
			val := fr.regs[ins.C]
			switch dst.Tag {
			case interpreter.TagList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(dst.List)
				}
				if idx < 0 || idx >= len(dst.List) {
					return Value{}, fmt.Errorf("index out of range")
				}
				dst.List[idx] = val
			case interpreter.TagMap:
				var key string
				switch idxVal.Tag {
				case interpreter.TagStr:
					key = idxVal.Str
				case interpreter.TagInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					return Value{}, fmt.Errorf("invalid map key")
				}
				if dst.Map == nil {
					dst.Map = map[string]Value{}
				}
				dst.Map[key] = val
			default:
				return Value{}, fmt.Errorf("invalid index target")
			}
		case OpMakeList:
			n := ins.B
			start := ins.C
			list := make([]Value, n)
			copy(list, fr.regs[start:start+n])
			fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: list}
		case OpPrint:
			fmt.Fprintln(m.writer, valueToAny(fr.regs[ins.A]))
		case OpPrint2:
			fmt.Fprintln(m.writer, valueToAny(fr.regs[ins.A]), valueToAny(fr.regs[ins.B]))
		case OpNow:
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: int(time.Now().UnixNano())}
		case OpJSON:
			b, _ := json.Marshal(valueToAny(fr.regs[ins.A]))
			fmt.Fprintln(m.writer, string(b))
		case OpCall2:
			a := fr.regs[ins.C]
			b := fr.regs[ins.D]
			res, err := m.call(ins.B, []Value{a, b})
			if err != nil {
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCall:
			args := make([]Value, ins.C)
			copy(args, fr.regs[ins.D:ins.D+ins.C])
			res, err := m.call(ins.B, args)
			if err != nil {
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCallV:
			fnIdx := fr.regs[ins.B].Int
			args := make([]Value, ins.C)
			copy(args, fr.regs[ins.D:ins.D+ins.C])
			res, err := m.call(fnIdx, args)
			if err != nil {
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpNot:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: !fr.regs[ins.B].Truthy()}
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
	fn    Function
	idx   int
	comp  *compiler
	vars  map[string]int
	loops []*loopContext
}

type loopContext struct {
	breakJumps     []int
	continueJumps  []int
	continueTarget int
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
	fc.fn.Line = fn.Pos.Line
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
	fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	return fc.fn
}

func (c *compiler) compileFunExpr(fn *parser.FunExpr) int {
	fc := &funcCompiler{comp: c, vars: map[string]int{}}
	fc.fn.Line = fn.Pos.Line
	for i, p := range fn.Params {
		fc.vars[p.Name] = i
		if i >= fc.fn.NumRegs {
			fc.fn.NumRegs = i + 1
		}
	}
	fc.idx = len(fn.Params)
	if fn.ExprBody != nil {
		r := fc.compileExpr(fn.ExprBody)
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: r})
	} else {
		for _, st := range fn.BlockBody {
			fc.compileStmt(st)
		}
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	}
	idx := len(c.funcs)
	c.funcs = append(c.funcs, fc.fn)
	return idx
}

func (c *compiler) compileMain(p *parser.Program) Function {
	fc := &funcCompiler{comp: c, vars: map[string]int{}}
	fc.fn.Name = "main"
	fc.fn.Line = 0
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		fc.compileStmt(st)
	}
	fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
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
	case s.Var != nil:
		r := fc.compileExpr(s.Var.Value)
		reg := fc.newReg()
		fc.vars[s.Var.Name] = reg
		fc.emit(s.Var.Pos, Instr{Op: OpMove, A: reg, B: r})
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 {
			r := fc.compileExpr(s.Assign.Value)
			reg, ok := fc.vars[s.Assign.Name]
			if !ok {
				reg = fc.newReg()
				fc.vars[s.Assign.Name] = reg
			}
			fc.emit(s.Assign.Pos, Instr{Op: OpMove, A: reg, B: r})
		} else if len(s.Assign.Index) == 1 {
			listReg, ok := fc.vars[s.Assign.Name]
			if !ok {
				listReg = fc.newReg()
				fc.vars[s.Assign.Name] = listReg
			}
			idx := fc.compileExpr(s.Assign.Index[0].Start)
			val := fc.compileExpr(s.Assign.Value)
			fc.emit(s.Assign.Pos, Instr{Op: OpSetIndex, A: listReg, B: idx, C: val})
		}
	case s.Return != nil:
		r := fc.compileExpr(s.Return.Value)
		fc.emit(s.Return.Pos, Instr{Op: OpReturn, A: r})
	case s.Expr != nil:
		fc.compileExpr(s.Expr.Expr)
	case s.If != nil:
		fc.compileIf(s.If)
	case s.While != nil:
		fc.compileWhile(s.While)
	case s.For != nil:
		fc.compileFor(s.For)
	case s.Break != nil:
		if l := len(fc.loops); l > 0 {
			idx := len(fc.fn.Code)
			fc.emit(s.Break.Pos, Instr{Op: OpJump})
			fc.loops[l-1].breakJumps = append(fc.loops[l-1].breakJumps, idx)
		}
	case s.Continue != nil:
		if l := len(fc.loops); l > 0 {
			idx := len(fc.fn.Code)
			fc.emit(s.Continue.Pos, Instr{Op: OpJump})
			fc.loops[l-1].continueJumps = append(fc.loops[l-1].continueJumps, idx)
		}
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
		switch op.Op {
		case "&&":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpMove, A: dst, B: left})
			jmp := len(fc.fn.Code)
			fc.emit(op.Pos, Instr{Op: OpJumpIfFalse, A: dst})
			right := fc.compilePostfix(op.Right)
			fc.emit(op.Pos, Instr{Op: OpMove, A: dst, B: right})
			fc.fn.Code[jmp].B = len(fc.fn.Code)
			left = dst
			continue
		case "||":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpMove, A: dst, B: left})
			jmp := len(fc.fn.Code)
			fc.emit(op.Pos, Instr{Op: OpJumpIfTrue, A: dst})
			right := fc.compilePostfix(op.Right)
			fc.emit(op.Pos, Instr{Op: OpMove, A: dst, B: right})
			fc.fn.Code[jmp].B = len(fc.fn.Code)
			left = dst
			continue
		}
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
		case "*":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpMul, A: dst, B: left, C: right})
			left = dst
		case "/":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpDiv, A: dst, B: left, C: right})
			left = dst
		case "%":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpMod, A: dst, B: left, C: right})
			left = dst
		case "==":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpEqual, A: dst, B: left, C: right})
			left = dst
		case "!=":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpNotEqual, A: dst, B: left, C: right})
			left = dst
		case "<":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpLess, A: dst, B: left, C: right})
			left = dst
		case ">":
			dst := fc.newReg()
			// a > b  ==>  b < a
			fc.emit(op.Pos, Instr{Op: OpLess, A: dst, B: right, C: left})
			left = dst
		case "<=":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpLessEq, A: dst, B: left, C: right})
			left = dst
		case "in":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpIn, A: dst, B: left, C: right})
			left = dst
		case ">=":
			dst := fc.newReg()
			// a >= b  ==>  b <= a
			fc.emit(op.Pos, Instr{Op: OpLessEq, A: dst, B: right, C: left})
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
			dst := fc.newReg()
			fc.emit(u.Pos, Instr{Op: OpNot, A: dst, B: r})
			r = dst
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
			regs := make([]int, len(op.Call.Args))
			for i := range op.Call.Args {
				regs[i] = fc.newReg()
			}
			for i, a := range op.Call.Args {
				ar := fc.compileExpr(a)
				fc.emit(a.Pos, Instr{Op: OpMove, A: regs[i], B: ar})
			}
			dst := fc.newReg()
			start := 0
			if len(regs) > 0 {
				start = regs[0]
			}
			fc.emit(op.Call.Pos, Instr{Op: OpCallV, A: dst, B: r, C: len(regs), D: start})
			r = dst
		}
	}
	return r
}

func (fc *funcCompiler) compilePrimary(p *parser.Primary) int {
	if p.Lit != nil {
		switch {
		case p.Lit.Int != nil:
			dst := fc.newReg()
			v := Value{Tag: interpreter.TagInt, Int: *p.Lit.Int}
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		case p.Lit.Float != nil:
			dst := fc.newReg()
			v := Value{Tag: interpreter.TagFloat, Float: *p.Lit.Float}
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		case p.Lit.Str != nil:
			dst := fc.newReg()
			v := Value{Tag: interpreter.TagStr, Str: *p.Lit.Str}
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		case p.Lit.Bool != nil:
			dst := fc.newReg()
			v := Value{Tag: interpreter.TagBool, Bool: bool(*p.Lit.Bool)}
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		}
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

	if p.Group != nil {
		return fc.compileExpr(p.Group)
	}

	if p.Map != nil {
		if v, ok := constMap(p.Map); ok {
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		}
	}

	if p.FunExpr != nil {
		idx := fc.comp.compileFunExpr(p.FunExpr)
		dst := fc.newReg()
		fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: Value{Tag: interpreter.TagInt, Int: idx}})
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
		case "now":
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpNow, A: dst})
			return dst
		case "json":
			arg := fc.compileExpr(p.Call.Args[0])
			fc.emit(p.Pos, Instr{Op: OpJSON, A: arg})
			return arg
		case "print":
			if len(p.Call.Args) == 1 {
				arg := fc.compileExpr(p.Call.Args[0])
				fc.emit(p.Pos, Instr{Op: OpPrint, A: arg})
				return arg
			}
			if len(p.Call.Args) == 2 {
				a0 := fc.compileExpr(p.Call.Args[0])
				a1 := fc.compileExpr(p.Call.Args[1])
				fc.emit(p.Pos, Instr{Op: OpPrint2, A: a0, B: a1})
				return a0
			}
		default:
			if fnIdx, ok := fc.comp.fnIndex[p.Call.Func]; ok {
				regs := make([]int, len(p.Call.Args))
				for i := range p.Call.Args {
					regs[i] = fc.newReg()
				}
				for i, a := range p.Call.Args {
					ar := fc.compileExpr(a)
					fc.emit(a.Pos, Instr{Op: OpMove, A: regs[i], B: ar})
				}
				dst := fc.newReg()
				if len(regs) == 2 {
					fc.emit(p.Pos, Instr{Op: OpCall2, A: dst, B: fnIdx, C: regs[0], D: regs[1]})
				} else {
					start := 0
					if len(regs) > 0 {
						start = regs[0]
					}
					fc.emit(p.Pos, Instr{Op: OpCall, A: dst, B: fnIdx, C: len(regs), D: start})
				}
				return dst
			}
			if reg, ok := fc.vars[p.Call.Func]; ok {
				regs := make([]int, len(p.Call.Args))
				for i := range p.Call.Args {
					regs[i] = fc.newReg()
				}
				for i, a := range p.Call.Args {
					ar := fc.compileExpr(a)
					fc.emit(a.Pos, Instr{Op: OpMove, A: regs[i], B: ar})
				}
				dst := fc.newReg()
				start := 0
				if len(regs) > 0 {
					start = regs[0]
				}
				fc.emit(p.Pos, Instr{Op: OpCallV, A: dst, B: reg, C: len(regs), D: start})
				return dst
			}
		}
	}

	return fc.newReg()
}

func (fc *funcCompiler) compileFor(f *parser.ForStmt) {
	if f.RangeEnd != nil {
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
		loop := &loopContext{}
		fc.loops = append(fc.loops, loop)
		for _, st := range f.Body {
			fc.compileStmt(st)
		}
		contTarget := len(fc.fn.Code)
		for _, idx := range loop.continueJumps {
			fc.fn.Code[idx].A = contTarget
		}
		one := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpConst, A: one, Val: Value{Tag: interpreter.TagInt, Int: 1}})
		tmp := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpAdd, A: tmp, B: idx, C: one})
		fc.emit(f.Pos, Instr{Op: OpMove, A: idx, B: tmp})
		fc.emit(f.Pos, Instr{Op: OpJump, A: loopStart})
		endPC := len(fc.fn.Code)
		fc.fn.Code[jmp].B = endPC
		for _, idx := range loop.breakJumps {
			fc.fn.Code[idx].A = endPC
		}
		fc.loops = fc.loops[:len(fc.loops)-1]
	} else {
		list := fc.compileExpr(f.Source)
		length := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpLen, A: length, B: list})
		idx := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpConst, A: idx, Val: Value{Tag: interpreter.TagInt, Int: 0}})
		loopStart := len(fc.fn.Code)
		cond := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpLess, A: cond, B: idx, C: length})
		jmp := len(fc.fn.Code)
		fc.emit(f.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		loop := &loopContext{}
		fc.loops = append(fc.loops, loop)
		elem := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpIndex, A: elem, B: list, C: idx})
		varReg, ok := fc.vars[f.Name]
		if !ok {
			varReg = fc.newReg()
			fc.vars[f.Name] = varReg
		}
		fc.emit(f.Pos, Instr{Op: OpMove, A: varReg, B: elem})
		for _, st := range f.Body {
			fc.compileStmt(st)
		}
		contTarget := len(fc.fn.Code)
		for _, idx := range loop.continueJumps {
			fc.fn.Code[idx].A = contTarget
		}
		one := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpConst, A: one, Val: Value{Tag: interpreter.TagInt, Int: 1}})
		tmp := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpAdd, A: tmp, B: idx, C: one})
		fc.emit(f.Pos, Instr{Op: OpMove, A: idx, B: tmp})
		fc.emit(f.Pos, Instr{Op: OpJump, A: loopStart})
		endPC := len(fc.fn.Code)
		fc.fn.Code[jmp].B = endPC
		for _, idx := range loop.breakJumps {
			fc.fn.Code[idx].A = endPC
		}
		fc.loops = fc.loops[:len(fc.loops)-1]
	}
}

func (fc *funcCompiler) compileWhile(w *parser.WhileStmt) {
	loopStart := len(fc.fn.Code)
	cond := fc.compileExpr(w.Cond)
	jmp := len(fc.fn.Code)
	fc.emit(w.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	loop := &loopContext{continueTarget: loopStart}
	fc.loops = append(fc.loops, loop)
	for _, st := range w.Body {
		fc.compileStmt(st)
	}
	for _, idx := range loop.continueJumps {
		fc.fn.Code[idx].A = loopStart
	}
	fc.emit(w.Pos, Instr{Op: OpJump, A: loopStart})
	endPC := len(fc.fn.Code)
	fc.fn.Code[jmp].B = endPC
	for _, idx := range loop.breakJumps {
		fc.fn.Code[idx].A = endPC
	}
	fc.loops = fc.loops[:len(fc.loops)-1]
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

func constMap(m *parser.MapLiteral) (Value, bool) {
	vals := make(map[string]Value, len(m.Items))
	for _, it := range m.Items {
		k, ok := constExpr(it.Key)
		if !ok {
			return Value{}, false
		}
		var key string
		switch k.Tag {
		case interpreter.TagStr:
			key = k.Str
		case interpreter.TagInt:
			key = fmt.Sprintf("%d", k.Int)
		default:
			return Value{}, false
		}
		v, ok := constExpr(it.Value)
		if !ok {
			return Value{}, false
		}
		vals[key] = v
	}
	return Value{Tag: interpreter.TagMap, Map: vals}, true
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
		if p.Lit.Float != nil {
			return Value{Tag: interpreter.TagFloat, Float: *p.Lit.Float}, true
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
	if p.Map != nil {
		return constMap(p.Map)
	}
	return Value{}, false
}

func valueToAny(v Value) any {
	switch v.Tag {
	case interpreter.TagInt:
		return v.Int
	case interpreter.TagFloat:
		return v.Float
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
	case interpreter.TagMap:
		m := make(map[string]any, len(v.Map))
		for k, x := range v.Map {
			m[k] = valueToAny(x)
		}
		return m
	default:
		return nil
	}
}

func valueToString(v Value) string {
	switch v.Tag {
	case interpreter.TagInt:
		return fmt.Sprintf("%d", v.Int)
	case interpreter.TagFloat:
		return fmt.Sprintf("%g", v.Float)
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
	case interpreter.TagMap:
		keys := make([]string, 0, len(v.Map))
		for k := range v.Map {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		parts := make([]string, len(keys))
		for i, k := range keys {
			parts[i] = fmt.Sprintf("%q: %s", k, valueToString(v.Map[k]))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	default:
		return "nil"
	}
}

func formatReg(n int) string {
	return fmt.Sprintf("r%d", n)
}

func formatRegs(start, n int) string {
	regs := make([]string, n)
	for i := 0; i < n; i++ {
		regs[i] = formatReg(start + i)
	}
	return strings.Join(regs, ", ")
}

func toFloat(v Value) float64 {
	if v.Tag == interpreter.TagFloat {
		return v.Float
	}
	return float64(v.Int)
}

func valuesEqual(a, b Value) bool {
	if a.Tag == interpreter.TagFloat || b.Tag == interpreter.TagFloat {
		return toFloat(a) == toFloat(b)
	}
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case interpreter.TagInt:
		return a.Int == b.Int
	case interpreter.TagBool:
		return a.Bool == b.Bool
	case interpreter.TagStr:
		return a.Str == b.Str
	case interpreter.TagList:
		if len(a.List) != len(b.List) {
			return false
		}
		for i := range a.List {
			if !valuesEqual(a.List[i], b.List[i]) {
				return false
			}
		}
		return true
	case interpreter.TagMap:
		if len(a.Map) != len(b.Map) {
			return false
		}
		for k, av := range a.Map {
			bv, ok := b.Map[k]
			if !ok || !valuesEqual(av, bv) {
				return false
			}
		}
		return true
	default:
		return false
	}
}
