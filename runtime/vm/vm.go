package vm

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
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

// regTag tracks the known type of a register during compilation.
type regTag uint8

const (
	tagUnknown regTag = iota
	tagInt
	tagFloat
	tagBool
)

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
	OpMakeMap
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
	OpAppend
	OpStr
	OpInput
	OpCount
	OpAvg
	OpIterPrep

	// Specialized numeric ops
	OpAddInt
	OpAddFloat
	OpSubInt
	OpSubFloat
	OpMulInt
	OpMulFloat
	OpDivInt
	OpDivFloat
	OpModInt
	OpModFloat
	OpEqualInt
	OpEqualFloat
	OpLessInt
	OpLessFloat
	OpLessEqInt
	OpLessEqFloat
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
	case OpMakeMap:
		return "MakeMap"
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
	case OpAppend:
		return "Append"
	case OpStr:
		return "Str"
	case OpInput:
		return "Input"
	case OpCount:
		return "Count"
	case OpAvg:
		return "Avg"
	case OpIterPrep:
		return "IterPrep"
	case OpAddInt:
		return "AddInt"
	case OpAddFloat:
		return "AddFloat"
	case OpSubInt:
		return "SubInt"
	case OpSubFloat:
		return "SubFloat"
	case OpMulInt:
		return "MulInt"
	case OpMulFloat:
		return "MulFloat"
	case OpDivInt:
		return "DivInt"
	case OpDivFloat:
		return "DivFloat"
	case OpModInt:
		return "ModInt"
	case OpModFloat:
		return "ModFloat"
	case OpEqualInt:
		return "EqualInt"
	case OpEqualFloat:
		return "EqualFloat"
	case OpLessInt:
		return "LessInt"
	case OpLessFloat:
		return "LessFloat"
	case OpLessEqInt:
		return "LessEqInt"
	case OpLessEqFloat:
		return "LessEqFloat"
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
	Code      []Instr
	NumRegs   int
	NumParams int
	Name      string
	Line      int // source line of function definition
}

type Program struct {
	Funcs []Function
}

type closure struct {
	fn   int
	args []Value
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
			case OpAdd, OpSub, OpMul, OpDiv, OpMod,
				OpAddInt, OpAddFloat, OpSubInt, OpSubFloat,
				OpMulInt, OpMulFloat, OpDivInt, OpDivFloat,
				OpModInt, OpModFloat,
				OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
				OpLess, OpLessEq, OpLessInt, OpLessFloat,
				OpLessEqInt, OpLessEqFloat, OpIn:
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
			case OpMakeMap:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpPrint:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpPrint2:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpNow:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpJSON:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpAppend:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpStr:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpInput:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpIterPrep:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpCount:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpAvg:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
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
	reader *bufio.Reader
}

// StackFrame represents a single call frame in a Mochi stack trace.
type StackFrame struct {
	Func string
	Line int
}

// VMError wraps an error with Mochi stack trace information.
type VMError struct {
	Err   error
	Stack []StackFrame
}

func (e *VMError) Error() string {
	return e.Err.Error()
}

func (e *VMError) callGraph() string {
	names := make([]string, len(e.Stack))
	for i, f := range e.Stack {
		names[i] = f.Func
	}
	return strings.Join(names, " -> ")
}

func (e *VMError) stackTrace() string {
	var b strings.Builder
	for i := len(e.Stack) - 1; i >= 0; i-- {
		f := e.Stack[i]
		fmt.Fprintf(&b, "  %s:%d\n", f.Func, f.Line)
	}
	return b.String()
}

func copyTrace(trace []StackFrame) []StackFrame {
	out := make([]StackFrame, len(trace))
	copy(out, trace)
	return out
}

func (m *VM) newError(err error, trace []StackFrame, line int) *VMError {
	tr := copyTrace(trace)
	if len(tr) > 0 {
		tr[len(tr)-1].Line = line
	}
	if vmErr, ok := err.(*VMError); ok {
		vmErr.Stack = tr
		return vmErr
	}
	return &VMError{Err: err, Stack: tr}
}

func New(prog *Program, w io.Writer) *VM {
	return &VM{prog: prog, writer: w, reader: bufio.NewReader(os.Stdin)}
}

func NewWithIO(prog *Program, r io.Reader, w io.Writer) *VM {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return &VM{prog: prog, writer: w, reader: br}
}

func (m *VM) Run() error {
	_, err := m.call(0, nil, []StackFrame{{Func: m.prog.funcName(0), Line: 0}})
	if err != nil {
		if vmErr, ok := err.(*VMError); ok {
			fmt.Fprintln(m.writer, "call graph:", vmErr.callGraph())
			fmt.Fprintln(m.writer, "stack trace:")
			fmt.Fprint(m.writer, vmErr.stackTrace())
		}
	}
	return err
}

type frame struct {
	fn   *Function
	regs []Value
	ip   int
}

func (m *VM) call(fnIndex int, args []Value, trace []StackFrame) (Value, error) {
	fn := &m.prog.Funcs[fnIndex]
	if len(args) < fn.NumParams {
		cl := &closure{fn: fnIndex, args: append([]Value(nil), args...)}
		return Value{Tag: interpreter.TagFunc, Func: cl}, nil
	}
	if len(args) > fn.NumParams {
		return Value{}, m.newError(fmt.Errorf("too many args"), trace, fn.Line)
	}
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
		case OpAddInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int + c.Int}
		case OpAddFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) + toFloat(c)}
		case OpSub:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) - toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int - c.Int}
			}
		case OpSubInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int - c.Int}
		case OpSubFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) - toFloat(c)}
		case OpMul:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) * toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int * c.Int}
			}
		case OpMulInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int * c.Int}
		case OpMulFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) * toFloat(c)}
		case OpDiv:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if (c.Tag == interpreter.TagInt && c.Int == 0) || (c.Tag == interpreter.TagFloat && c.Float == 0) {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) / toFloat(c)}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int / c.Int}
			}
		case OpDivInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if c.Int == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int / c.Int}
		case OpDivFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: toFloat(b) / toFloat(c)}
		case OpMod:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if (c.Tag == interpreter.TagInt && c.Int == 0) || (c.Tag == interpreter.TagFloat && c.Float == 0) {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			} else {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int % c.Int}
			}
		case OpModInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if c.Int == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: b.Int % c.Int}
		case OpModFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: math.Mod(toFloat(b), toFloat(c))}
		case OpEqual:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpNotEqual:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: !valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpEqualInt:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: fr.regs[ins.B].Int == fr.regs[ins.C].Int}
		case OpEqualFloat:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) == toFloat(fr.regs[ins.C])}
		case OpLess:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) < toFloat(fr.regs[ins.C])}
		case OpLessEq:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) <= toFloat(fr.regs[ins.C])}
		case OpLessInt:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: fr.regs[ins.B].Int < fr.regs[ins.C].Int}
		case OpLessFloat:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) < toFloat(fr.regs[ins.C])}
		case OpLessEqInt:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: fr.regs[ins.B].Int <= fr.regs[ins.C].Int}
		case OpLessEqFloat:
			fr.regs[ins.A] = Value{Tag: interpreter.TagBool, Bool: toFloat(fr.regs[ins.B]) <= toFloat(fr.regs[ins.C])}
		case OpIn:
			item := fr.regs[ins.B]
			container := fr.regs[ins.C]
			found := false
			switch container.Tag {
			case interpreter.TagList:
				for _, v := range container.List {
					if valuesEqual(v, item) {
						found = true
						break
					}
				}
			case interpreter.TagMap:
				key := fmt.Sprint(valueToAny(item))
				_, found = container.Map[key]
			case interpreter.TagStr:
				if item.Tag == interpreter.TagStr {
					found = strings.Contains(container.Str, item.Str)
				} else {
					return Value{}, m.newError(fmt.Errorf("invalid substring type"), trace, ins.Line)
				}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid 'in' operand"), trace, ins.Line)
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
			switch v.Tag {
			case interpreter.TagList:
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len(v.List)}
			case interpreter.TagStr:
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len([]rune(v.Str))}
			case interpreter.TagMap:
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len(v.Map)}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid len operand"), trace, ins.Line)
			}
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
					return Value{}, m.newError(fmt.Errorf("index out of range"), trace, ins.Line)
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
					return Value{}, m.newError(fmt.Errorf("invalid map key"), trace, ins.Line)
				}
				fr.regs[ins.A] = src.Map[key]
			default:
				return Value{}, m.newError(fmt.Errorf("invalid index target"), trace, ins.Line)
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
					return Value{}, m.newError(fmt.Errorf("index out of range"), trace, ins.Line)
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
					return Value{}, m.newError(fmt.Errorf("invalid map key"), trace, ins.Line)
				}
				if dst.Map == nil {
					dst.Map = map[string]Value{}
				}
				dst.Map[key] = val
			default:
				return Value{}, m.newError(fmt.Errorf("invalid index target"), trace, ins.Line)
			}
		case OpMakeList:
			n := ins.B
			start := ins.C
			list := make([]Value, n)
			copy(list, fr.regs[start:start+n])
			fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: list}
		case OpMakeMap:
			n := ins.B
			start := ins.C
			mp := make(map[string]Value, n)
			for i := 0; i < n; i++ {
				key := fr.regs[start+i*2]
				val := fr.regs[start+i*2+1]
				var k string
				switch key.Tag {
				case interpreter.TagStr:
					k = key.Str
				case interpreter.TagInt:
					k = fmt.Sprintf("%d", key.Int)
				default:
					return Value{}, m.newError(fmt.Errorf("invalid map key"), trace, ins.Line)
				}
				mp[k] = val
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagMap, Map: mp}
		case OpPrint:
			fmt.Fprintln(m.writer, valueToAny(fr.regs[ins.A]))
		case OpPrint2:
			fmt.Fprintln(m.writer, valueToAny(fr.regs[ins.A]), valueToAny(fr.regs[ins.B]))
		case OpNow:
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: int(time.Now().UnixNano())}
		case OpJSON:
			b, _ := json.Marshal(valueToAny(fr.regs[ins.A]))
			fmt.Fprintln(m.writer, string(b))
		case OpAppend:
			lst := fr.regs[ins.B]
			if lst.Tag != interpreter.TagList {
				return Value{}, m.newError(fmt.Errorf("append expects list"), trace, ins.Line)
			}
			newList := append(append([]Value(nil), lst.List...), fr.regs[ins.C])
			fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: newList}
		case OpStr:
			fr.regs[ins.A] = Value{Tag: interpreter.TagStr, Str: fmt.Sprint(valueToAny(fr.regs[ins.B]))}
		case OpInput:
			line, err := m.reader.ReadString('\n')
			if err != nil && err != io.EOF {
				return Value{}, err
			}
			line = strings.TrimRight(line, "\r\n")
			fr.regs[ins.A] = Value{Tag: interpreter.TagStr, Str: line}
		case OpIterPrep:
			src := fr.regs[ins.B]
			switch src.Tag {
			case interpreter.TagList:
				fr.regs[ins.A] = src
			case interpreter.TagMap:
				ks := make([]string, 0, len(src.Map))
				for k := range src.Map {
					ks = append(ks, k)
				}
				sort.Strings(ks)
				keys := make([]Value, len(ks))
				for i, k := range ks {
					keys[i] = Value{Tag: interpreter.TagStr, Str: k}
				}
				fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: keys}
			case interpreter.TagStr:
				r := []rune(src.Str)
				lst := make([]Value, len(r))
				for i, ch := range r {
					lst[i] = Value{Tag: interpreter.TagStr, Str: string(ch)}
				}
				fr.regs[ins.A] = Value{Tag: interpreter.TagList, List: lst}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid iterator"), trace, ins.Line)
			}
		case OpCount:
			lst := fr.regs[ins.B]
			if lst.Tag != interpreter.TagList {
				return Value{}, fmt.Errorf("count expects list")
			}
			fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: len(lst.List)}
		case OpAvg:
			lst := fr.regs[ins.B]
			if lst.Tag != interpreter.TagList {
				return Value{}, fmt.Errorf("avg expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: interpreter.TagInt, Int: 0}
			} else {
				var sum float64
				for _, v := range lst.List {
					sum += toFloat(v)
				}
				fr.regs[ins.A] = Value{Tag: interpreter.TagFloat, Float: sum / float64(len(lst.List))}
			}
		case OpCall2:
			a := fr.regs[ins.C]
			b := fr.regs[ins.D]
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					fn := &m.prog.Funcs[ins.B]
					if fn.NumParams == 2 {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						fr.regs[0] = a
						fr.regs[1] = b
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}
						continue
					}
				}
			}
			res, err := m.call(ins.B, []Value{a, b}, append(trace, StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCall:
			args := make([]Value, ins.C)
			copy(args, fr.regs[ins.D:ins.D+ins.C])
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					fn := &m.prog.Funcs[ins.B]
					if fn.NumParams == len(args) {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						copy(fr.regs, args)
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}
						continue
					}
				}
			}
			res, err := m.call(ins.B, args, append(trace, StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCallV:
			fnVal := fr.regs[ins.B]
			args := make([]Value, ins.C)
			copy(args, fr.regs[ins.D:ins.D+ins.C])
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					var fnIdx int
					var all []Value
					if fnVal.Tag == interpreter.TagFunc {
						cl := fnVal.Func.(*closure)
						fnIdx = cl.fn
						all = append(append([]Value{}, cl.args...), args...)
					} else {
						fnIdx = fnVal.Int
						all = args
					}
					fn := &m.prog.Funcs[fnIdx]
					if len(all) == fn.NumParams {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						copy(fr.regs, all)
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(fnIdx), Line: ins.Line}
						continue
					}
				}
			}
			if fnVal.Tag == interpreter.TagFunc {
				cl := fnVal.Func.(*closure)
				all := append(append([]Value{}, cl.args...), args...)
				res, err := m.call(cl.fn, all, append(trace, StackFrame{Func: m.prog.funcName(cl.fn), Line: ins.Line}))
				if err != nil {
					if vmErr, ok := err.(*VMError); ok {
						if len(vmErr.Stack) >= 2 {
							vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
						}
					}
					return Value{}, err
				}
				fr.regs[ins.A] = res
				break
			}
			fnIdx := fnVal.Int
			res, err := m.call(fnIdx, args, append(trace, StackFrame{Func: m.prog.funcName(fnIdx), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
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
			return Value{}, m.newError(fmt.Errorf("unknown op"), trace, ins.Line)
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
	tags  map[int]regTag
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
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}}
	fc.fn.Name = fn.Name
	fc.fn.Line = fn.Pos.Line
	fc.fn.NumParams = len(fn.Params)
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
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}}
	fc.fn.Line = fn.Pos.Line
	fc.fn.NumParams = len(fn.Params)
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
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}}
	fc.fn.Name = "main"
	fc.fn.Line = 0
	fc.fn.NumParams = 0
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
	if fc.tags == nil {
		return
	}
	switch i.Op {
	case OpConst:
		fc.tags[i.A] = valTag(i.Val)
	case OpMove:
		fc.tags[i.A] = fc.tags[i.B]
	case OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt:
		fc.tags[i.A] = tagInt
	case OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat:
		fc.tags[i.A] = tagFloat
	case OpAdd, OpSub, OpMul, OpDiv, OpMod:
		fc.tags[i.A] = tagUnknown
	case OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpNot:
		fc.tags[i.A] = tagBool
	case OpLen, OpNow:
		fc.tags[i.A] = tagInt
	case OpJSON, OpPrint, OpPrint2:
		// no result
	case OpAppend, OpStr, OpInput:
		fc.tags[i.A] = tagUnknown
	case OpCount:
		fc.tags[i.A] = tagInt
	case OpAvg:
		fc.tags[i.A] = tagFloat
	}
}

func (fc *funcCompiler) newReg() int {
	r := fc.idx
	fc.idx++
	if fc.idx > fc.fn.NumRegs {
		fc.fn.NumRegs = fc.idx
	}
	if fc.tags != nil {
		fc.tags[r] = tagUnknown
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
		fc.tags[reg] = fc.tags[r]
	case s.Var != nil:
		r := fc.compileExpr(s.Var.Value)
		reg := fc.newReg()
		fc.vars[s.Var.Name] = reg
		fc.emit(s.Var.Pos, Instr{Op: OpMove, A: reg, B: r})
		fc.tags[reg] = fc.tags[r]
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 {
			r := fc.compileExpr(s.Assign.Value)
			reg, ok := fc.vars[s.Assign.Name]
			if !ok {
				reg = fc.newReg()
				fc.vars[s.Assign.Name] = reg
			}
			fc.emit(s.Assign.Pos, Instr{Op: OpMove, A: reg, B: r})
			fc.tags[reg] = fc.tags[r]
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
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpAddFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpAddInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpAdd, A: dst, B: left, C: right})
			}
			left = dst
		case "-":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpSubFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpSubInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpSub, A: dst, B: left, C: right})
			}
			left = dst
		case "*":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpMulFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpMulInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpMul, A: dst, B: left, C: right})
			}
			left = dst
		case "/":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpDivFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpDivInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpDiv, A: dst, B: left, C: right})
			}
			left = dst
		case "%":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpModFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpModInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpMod, A: dst, B: left, C: right})
			}
			left = dst
		case "==":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpEqualFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpEqualInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpEqual, A: dst, B: left, C: right})
			}
			left = dst
		case "!=":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpNotEqual, A: dst, B: left, C: right})
			left = dst
		case "<":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpLessFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpLessInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpLess, A: dst, B: left, C: right})
			}
			left = dst
		case ">":
			dst := fc.newReg()
			// a > b  ==>  b < a
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpLessFloat, A: dst, B: right, C: left})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpLessInt, A: dst, B: right, C: left})
			} else {
				fc.emit(op.Pos, Instr{Op: OpLess, A: dst, B: right, C: left})
			}
			left = dst
		case "<=":
			dst := fc.newReg()
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpLessEqFloat, A: dst, B: left, C: right})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpLessEqInt, A: dst, B: left, C: right})
			} else {
				fc.emit(op.Pos, Instr{Op: OpLessEq, A: dst, B: left, C: right})
			}
			left = dst
		case "in":
			dst := fc.newReg()
			fc.emit(op.Pos, Instr{Op: OpIn, A: dst, B: left, C: right})
			left = dst
		case ">=":
			dst := fc.newReg()
			// a >= b  ==>  b <= a
			if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
				fc.emit(op.Pos, Instr{Op: OpLessEqFloat, A: dst, B: right, C: left})
			} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
				fc.emit(op.Pos, Instr{Op: OpLessEqInt, A: dst, B: right, C: left})
			} else {
				fc.emit(op.Pos, Instr{Op: OpLessEq, A: dst, B: right, C: left})
			}
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
		tmp := make([]struct{ k, v int }, len(p.Map.Items))
		for i, it := range p.Map.Items {
			tmp[i].k = fc.compileExpr(it.Key)
			tmp[i].v = fc.compileExpr(it.Value)
		}
		regs := make([]int, len(p.Map.Items)*2)
		for i, it := range p.Map.Items {
			kreg := fc.newReg()
			fc.emit(it.Pos, Instr{Op: OpMove, A: kreg, B: tmp[i].k})
			vreg := fc.newReg()
			fc.emit(it.Pos, Instr{Op: OpMove, A: vreg, B: tmp[i].v})
			regs[i*2] = kreg
			regs[i*2+1] = vreg
		}
		dst := fc.newReg()
		start := 0
		if len(regs) > 0 {
			start = regs[0]
		}
		fc.emit(p.Pos, Instr{Op: OpMakeMap, A: dst, B: len(p.Map.Items), C: start})
		return dst
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
		case "append":
			list := fc.compileExpr(p.Call.Args[0])
			elem := fc.compileExpr(p.Call.Args[1])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpAppend, A: dst, B: list, C: elem})
			return dst
		case "input":
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpInput, A: dst})
			return dst
		case "count":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpCount, A: dst, B: arg})
			return dst
		case "avg":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpAvg, A: dst, B: arg})
			return dst
		case "str":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpStr, A: dst, B: arg})
			return dst
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
		src := fc.compileExpr(f.Source)
		list := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpIterPrep, A: list, B: src})
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

func valTag(v Value) regTag {
	switch v.Tag {
	case interpreter.TagInt:
		return tagInt
	case interpreter.TagFloat:
		return tagFloat
	case interpreter.TagBool:
		return tagBool
	default:
		return tagUnknown
	}
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
