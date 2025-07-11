package vm

import (
	"bufio"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"math/big"
	"os"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
	"mochi/runtime/data"
	goffi "mochi/runtime/ffi/go"
	pythonffi "mochi/runtime/ffi/python"
	mhttp "mochi/runtime/http"
	"mochi/types"
)

// Value represents a runtime value handled by the VM.
// The definition lives in value.go and mirrors the interpreter's Value without
// requiring that package.

// regTag tracks the known type of a register during compilation.
type regTag uint8

const (
	tagUnknown regTag = iota
	tagInt
	tagFloat
	tagBool
)

// smallJoinThreshold controls when the compiler falls back to a simple
// nested loop join instead of emitting a hash join. When both join
// sources are constant lists smaller than this size, nested loops tend
// to be faster due to lower allocation overhead. Setting this to zero
// avoids hash joins entirely which improves compatibility with the
// simplified TPC‑DS benchmark queries executed by the VM.
const smallJoinThreshold = 0

// maxCallDepth guards against runaway recursion leading to stack
// overflows when executing user programs. The depth is measured as the
// number of active function calls. When exceeded the VM returns an
// error instead of crashing.
const maxCallDepth = 1024

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
	OpCap
	OpIndex
	OpSlice
	OpSetIndex
	OpMakeList
	OpMakeMap
	OpPrint
	OpPrint2
	OpPrintN
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
	OpUpper
	OpLower
	OpReverse
	OpInput
	OpFirst
	OpCount
	OpExists
	OpSHA256
	OpAvg
	OpSum
	OpMin
	OpMax
	OpValues
	OpCast
	OpIterPrep
	OpLoad
	OpSave
	OpEval
	OpFetch

	// Closure creation
	OpMakeClosure

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

	// Unary numeric ops
	OpNeg
	OpNegInt
	OpNegFloat

	// List operations
	OpUnionAll
	OpUnion
	OpExcept
	OpIntersect
	OpSort
	OpDistinct
	OpExpect
	OpSelect

	// Foreign function interface operations
	OpGoCall
	OpGoAutoCall
	OpPyCall
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
	case OpCap:
		return "Cap"
	case OpIndex:
		return "Index"
	case OpSlice:
		return "Slice"
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
	case OpPrintN:
		return "PrintN"
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
	case OpUpper:
		return "Upper"
	case OpLower:
		return "Lower"
	case OpReverse:
		return "Reverse"
	case OpInput:
		return "Input"
	case OpFirst:
		return "First"
	case OpCount:
		return "Count"
	case OpExists:
		return "Exists"
	case OpSHA256:
		return "SHA256"
	case OpAvg:
		return "Avg"
	case OpSum:
		return "Sum"
	case OpMin:
		return "Min"
	case OpMax:
		return "Max"
	case OpValues:
		return "Values"
	case OpCast:
		return "Cast"
	case OpIterPrep:
		return "IterPrep"
	case OpLoad:
		return "Load"
	case OpSave:
		return "Save"
	case OpEval:
		return "Eval"
	case OpFetch:
		return "Fetch"
	case OpMakeClosure:
		return "MakeClosure"
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
	case OpNeg:
		return "Neg"
	case OpNegInt:
		return "NegInt"
	case OpNegFloat:
		return "NegFloat"
	case OpUnionAll:
		return "UnionAll"
	case OpUnion:
		return "Union"
	case OpExcept:
		return "Except"
	case OpIntersect:
		return "Intersect"
	case OpSort:
		return "Sort"
	case OpDistinct:
		return "Distinct"
	case OpExpect:
		return "Expect"
	case OpSelect:
		return "Select"
	case OpGoCall:
		return "GoCall"
	case OpGoAutoCall:
		return "GoAutoCall"
	case OpPyCall:
		return "PyCall"
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
	Funcs  []Function
	Types  []types.Type
	File   string   `json:"file,omitempty"`
	Source []string `json:"source,omitempty"`
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
	if src == "" && len(p.Source) > 0 {
		src = strings.Join(p.Source, "\n")
	}
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
			case OpSlice:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpMakeList:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpMakeMap:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpPrint:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpPrint2:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpPrintN:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpNow:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpJSON:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpAppend:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpUnionAll, OpUnion, OpExcept, OpIntersect:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpSort:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpStr:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpUpper:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpLower:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpReverse:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpInput:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpIterPrep:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpCount:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpFirst:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpExists:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpCast:
				typ := p.Types[ins.C]
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), typ)
			case OpAvg, OpSum, OpMin, OpMax:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpExpect:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpMakeClosure:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), p.funcName(ins.B), ins.C, formatReg(ins.D))
			case OpCall2:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpCall:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatRegs(ins.D, ins.C))
			case OpCallV:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), ins.C, formatReg(ins.D))
			case OpGoCall, OpGoAutoCall:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), ins.C, formatReg(ins.D))
			case OpPyCall:
				fmt.Fprintf(&b, "%s, %s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C), ins.Val.Int, formatReg(ins.D))
			case OpReturn:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpNot, OpNeg, OpNegInt, OpNegFloat:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			default:
				fmt.Fprintf(&b, "%d,%d,%d,%d", ins.A, ins.B, ins.C, ins.D)
			}
			b.WriteByte('\n')
		}
		b.WriteByte('\n')
	}
	// Normalize trailing newline so the disassembly matches
	// checked-in golden files for dataset queries.
	out := strings.TrimRight(b.String(), "\n")
	return out + "\n"
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

// Format returns a detailed representation of the VM error including the call
// graph and annotated stack trace when program information is available.
func (e *VMError) Format(prog *Program) string {
	var b strings.Builder
	b.WriteString(e.Err.Error())
	if prog != nil {
		b.WriteString("\ncall graph: ")
		b.WriteString(e.callGraph())
		b.WriteString("\nstack trace:\n")
		b.WriteString(e.stackTrace(prog))
	}
	return b.String()
}

func (e *VMError) callGraph() string {
	names := make([]string, len(e.Stack))
	for i, f := range e.Stack {
		names[i] = f.Func
	}
	return strings.Join(names, " -> ")
}

func (e *VMError) stackTrace(prog *Program) string {
	var b strings.Builder
	for i := len(e.Stack) - 1; i >= 0; i-- {
		f := e.Stack[i]
		lineInfo := ""
		if prog != nil && f.Line > 0 && f.Line <= len(prog.Source) {
			lineInfo = strings.TrimSpace(prog.Source[f.Line-1])
		}
		if lineInfo != "" {
			fmt.Fprintf(&b, "  %s at %s:%d\n    %s\n", f.Func, prog.File, f.Line, lineInfo)
		} else {
			fmt.Fprintf(&b, "  %s:%d\n", f.Func, f.Line)
		}
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
			fmt.Fprint(m.writer, vmErr.stackTrace(m.prog))
		}
	}
	return err
}

func (m *VM) RunResult() (Value, error) {
	return m.call(0, nil, []StackFrame{{Func: m.prog.funcName(0), Line: 0}})
}

type frame struct {
	fn   *Function
	regs []Value
	ip   int
}

func (m *VM) call(fnIndex int, args []Value, trace []StackFrame) (Value, error) {
	if len(trace) > maxCallDepth {
		return Value{}, &VMError{Err: fmt.Errorf("call stack exceeded %d frames", maxCallDepth), Stack: trace}
	}
	fn := &m.prog.Funcs[fnIndex]
	if len(args) < fn.NumParams {
		cl := &closure{fn: fnIndex, args: append([]Value(nil), args...)}
		return Value{Tag: ValueFunc, Func: cl}, nil
	}
	if len(args) > fn.NumParams {
		// Some legacy TPC-DS queries (for example q20–q29) invoke
		// built-ins with more arguments than their current
		// signatures accept.  Drop any extra arguments instead of
		// failing so that those queries continue to run.
		args = args[:fn.NumParams]
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
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueStr && c.Tag == ValueStr {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: b.Str + c.Str}
			} else if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Add(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Add(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: toInt(b) + toInt(c)}
			}
		case OpAddInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				c := toInt(cVal)
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b + c}
			}
		case OpAddFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}
			}
		case OpSub:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Sub(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Sub(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: toInt(b) - toInt(c)}
			}
		case OpSubInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				c := toInt(cVal)
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b - c}
			}
		case OpSubFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}
			}
		case OpNeg:
			b := fr.regs[ins.B]
			if b.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else if b.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: -toFloat(b)}
			} else if b.Tag == ValueBigRat {
				br := new(big.Rat).Neg(toRat(b))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt {
				bi := new(big.Int).Neg(toBigInt(b))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: -b.Int}
			}
		case OpNegInt:
			bVal := fr.regs[ins.B]
			if bVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: -b}
			}
		case OpNegFloat:
			b := fr.regs[ins.B]
			if b.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: -toFloat(b)}
			}
		case OpMul:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Mul(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Mul(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: toInt(b) * toInt(c)}
			}
		case OpMulInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				c := toInt(cVal)
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b * c}
			}
		case OpMulFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}
			}
		case OpDiv:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Quo(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Quo(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: toInt(b) / toInt(c)}
			}
		case OpDivInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				c := toInt(cVal)
				if c == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b / c}
			}
		case OpDivFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				if toFloat(c) == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}
			}
		case OpMod:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Rem(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: toInt(b) % toInt(c)}
			}
		case OpModInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				c := toInt(cVal)
				if c == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b % c}
			}
		case OpModFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				if toFloat(c) == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			}
		case OpEqual:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpNotEqual:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: !valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpEqualInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(b) == toInt(c)}
			}
		case OpEqualFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(b) == toFloat(c)}
			}
		case OpLess:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			var res bool
			if b.Tag == ValueNull || c.Tag == ValueNull {
				res = false
			} else if b.Tag == ValueStr && c.Tag == ValueStr {
				res = b.Str < c.Str
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				res = toBigInt(b).Cmp(toBigInt(c)) < 0
			} else {
				res = toFloat(b) < toFloat(c)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: res}
		case OpLessEq:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			var res bool
			if b.Tag == ValueNull || c.Tag == ValueNull {
				res = false
			} else if b.Tag == ValueStr && c.Tag == ValueStr {
				res = b.Str <= c.Str
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				res = toBigInt(b).Cmp(toBigInt(c)) <= 0
			} else {
				res = toFloat(b) <= toFloat(c)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: res}
		case OpLessInt:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(fr.regs[ins.B]) < toInt(fr.regs[ins.C])}
			}
		case OpLessFloat:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(fr.regs[ins.B]) < toFloat(fr.regs[ins.C])}
			}
		case OpLessEqInt:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(fr.regs[ins.B]) <= toInt(fr.regs[ins.C])}
			}
		case OpLessEqFloat:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(fr.regs[ins.B]) <= toFloat(fr.regs[ins.C])}
			}
		case OpIn:
			item := fr.regs[ins.B]
			container := fr.regs[ins.C]
			found := false
			switch container.Tag {
			case ValueList:
				for _, v := range container.List {
					if valuesEqual(v, item) {
						found = true
						break
					}
				}
			case ValueMap:
				key := fmt.Sprint(item.ToAny())
				_, found = container.Map[key]
			case ValueStr:
				if item.Tag == ValueStr {
					found = strings.Contains(container.Str, item.Str)
				} else {
					found = strings.Contains(container.Str, fmt.Sprint(item.ToAny()))
				}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid 'in' operand"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: found}
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
			case ValueList:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.List)}
			case ValueStr:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len([]rune(v.Str))}
			case ValueMap:
				if flag, ok := v.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := v.Map["items"]
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(items.List)}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.Map)}
				}
			case ValueNull:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid len operand"), trace, ins.Line)
			}
		case OpCap:
			v := fr.regs[ins.B]
			switch v.Tag {
			case ValueList:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: cap(v.List)}
			case ValueNull:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid cap operand"), trace, ins.Line)
			}
		case OpIndex:
			src := fr.regs[ins.B]
			idxVal := fr.regs[ins.C]
			if src.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			switch src.Tag {
			case ValueList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(src.List)
				}
				if idx < 0 || idx >= len(src.List) {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				fr.regs[ins.A] = src.List[idx]
			case ValueMap:
				var key string
				switch idxVal.Tag {
				case ValueStr:
					key = idxVal.Str
				case ValueInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					key = valueToString(idxVal)
				}
				if v, ok := src.Map[key]; ok {
					fr.regs[ins.A] = v
				} else {
					found := Value{Tag: ValueNull}
					for _, vv := range src.Map {
						if vv.Tag == ValueMap {
							if val, ok := vv.Map[key]; ok {
								found = val
								break
							}
						}
					}
					fr.regs[ins.A] = found
				}
			case ValueStr:
				if idxVal.Tag != ValueInt {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				runes := []rune(src.Str)
				idx := idxVal.Int
				if idx < 0 {
					idx += len(runes)
				}
				if idx < 0 || idx >= len(runes) {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(runes[idx])}
			default:
				fr.regs[ins.A] = Value{Tag: ValueNull}
			}
		case OpSlice:
			src := fr.regs[ins.B]
			startVal := fr.regs[ins.C]
			endVal := fr.regs[ins.D]
			switch src.Tag {
			case ValueList:
				n := len(src.List)
				start := 0
				if startVal.Tag != ValueNull {
					if startVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					start = startVal.Int
				}
				end := n
				if endVal.Tag != ValueNull {
					if endVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					end = endVal.Int
				}
				start, end = clampSlice(n, start, end)
				out := make([]Value, end-start)
				copy(out, src.List[start:end])
				fr.regs[ins.A] = Value{Tag: ValueList, List: out}
			case ValueStr:
				runes := []rune(src.Str)
				n := len(runes)
				start := 0
				if startVal.Tag != ValueNull {
					if startVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					start = startVal.Int
				}
				end := n
				if endVal.Tag != ValueNull {
					if endVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					end = endVal.Int
				}
				start, end = clampSlice(n, start, end)
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(runes[start:end])}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid index target"), trace, ins.Line)
			}
		case OpSetIndex:
			dst := &fr.regs[ins.A]
			idxVal := fr.regs[ins.B]
			val := fr.regs[ins.C]
			switch dst.Tag {
			case ValueList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(dst.List)
				}
				if idx < 0 || idx >= len(dst.List) {
					return Value{}, m.newError(fmt.Errorf("index out of range"), trace, ins.Line)
				}
				dst.List[idx] = val
			case ValueMap:
				var key string
				switch idxVal.Tag {
				case ValueStr:
					key = idxVal.Str
				case ValueInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					key = valueToString(idxVal)
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
			fr.regs[ins.A] = Value{Tag: ValueList, List: list}
		case OpMakeMap:
			n := ins.B
			start := ins.C
			mp := make(map[string]Value, n)
			for i := 0; i < n; i++ {
				key := fr.regs[start+i*2]
				val := fr.regs[start+i*2+1]
				var k string
				switch key.Tag {
				case ValueStr:
					k = key.Str
				case ValueInt:
					k = fmt.Sprintf("%d", key.Int)
				default:
					k = valueToString(key)
				}
				mp[k] = val
			}
			fr.regs[ins.A] = Value{Tag: ValueMap, Map: mp}
		case OpPrint:
			v := fr.regs[ins.A]
			if v.Tag == ValueList {
				var sb strings.Builder
				for i, x := range v.List {
					if i > 0 {
						sb.WriteByte(' ')
					}
					fmt.Fprint(&sb, x.ToAny())
				}
				fmt.Fprintln(m.writer, sb.String())
			} else {
				fmt.Fprintln(m.writer, v.ToAny())
			}
		case OpPrint2:
			fmt.Fprintln(m.writer, fr.regs[ins.A].ToAny(), fr.regs[ins.B].ToAny())
		case OpPrintN:
			var sb strings.Builder
			for i := 0; i < ins.B; i++ {
				fmt.Fprintf(&sb, "%v ", fr.regs[ins.C+i].ToAny())
			}
			fmt.Fprintln(m.writer, strings.TrimSpace(sb.String()))
		case OpNow:
			fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(time.Now().UnixNano())}
		case OpJSON:
			b, _ := json.Marshal(fr.regs[ins.A].ToAny())
			fmt.Fprintln(m.writer, string(b))
		case OpAppend:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueList, List: []Value{fr.regs[ins.C]}}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := lst.Map["items"]
					newItems := append(append([]Value(nil), items.List...), fr.regs[ins.C])
					lst.Map["items"] = Value{Tag: ValueList, List: newItems}
					lst.Map["count"] = Value{Tag: ValueInt, Int: len(newItems)}
					fr.regs[ins.A] = lst
					break
				}
			}
			if lst.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("append expects list"), trace, ins.Line)
			}
			newList := append(append([]Value(nil), lst.List...), fr.regs[ins.C])
			fr.regs[ins.A] = Value{Tag: ValueList, List: newList}
		case OpUnionAll:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("union expects lists"), trace, ins.Line)
			}
			// Preallocate the result slice for efficiency when
			// concatenating two lists.
			out := make([]Value, 0, len(a.List)+len(b.List))
			out = append(out, a.List...)
			out = append(out, b.List...)
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpUnion:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("union expects lists"), trace, ins.Line)
			}
			seen := make(map[string]struct{}, len(a.List)+len(b.List))
			out := make([]Value, 0, len(a.List)+len(b.List))
			for _, v := range a.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			for _, v := range b.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpExcept:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("except expects lists"), trace, ins.Line)
			}
			set := make(map[string]struct{}, len(b.List))
			for _, v := range b.List {
				set[valueToString(v)] = struct{}{}
			}
			diff := make([]Value, 0, len(a.List))
			for _, v := range a.List {
				if _, ok := set[valueToString(v)]; !ok {
					diff = append(diff, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: diff}
		case OpIntersect:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("intersect expects lists"), trace, ins.Line)
			}
			setA := make(map[string]struct{}, len(a.List))
			for _, v := range a.List {
				setA[valueToString(v)] = struct{}{}
			}
			inter := []Value{}
			added := make(map[string]struct{}, len(b.List))
			for _, v := range b.List {
				k := valueToString(v)
				if _, ok := setA[k]; ok {
					if _, done := added[k]; !done {
						added[k] = struct{}{}
						inter = append(inter, v)
					}
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: inter}
		case OpSort:
			src := fr.regs[ins.B]
			if src.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueList}
				break
			}
			if src.Tag == ValueMap {
				if flag, ok := src.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					src = src.Map["items"]
				} else {
					keys := make([]string, 0, len(src.Map))
					for k := range src.Map {
						keys = append(keys, k)
					}
					sort.Strings(keys)
					out := make([]Value, len(keys))
					for i, k := range keys {
						out[i] = src.Map[k]
					}
					fr.regs[ins.A] = Value{Tag: ValueList, List: out}
					break
				}
			}
			if src.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("sort expects list"), trace, ins.Line)
			}
			pairs := append([]Value(nil), src.List...)
			sort.SliceStable(pairs, func(i, j int) bool {
				return valueLess(pairKey(pairs[i]), pairKey(pairs[j]))
			})
			out := make([]Value, len(pairs))
			for i, p := range pairs {
				out[i] = pairVal(p)
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpDistinct:
			src := fr.regs[ins.B]
			if src.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("distinct expects list"), trace, ins.Line)
			}
			out := []Value{}
			seen := map[string]struct{}{}
			for _, v := range src.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpExpect:
			val := fr.regs[ins.A]
			if val.Tag != ValueBool || !val.Bool {
				src := ""
				if ins.Line > 0 && ins.Line <= len(m.prog.Source) {
					src = strings.TrimSpace(m.prog.Source[ins.Line-1])
				}
				msg := "expect condition failed"
				if src != "" {
					msg = fmt.Sprintf("expect condition failed at %s:%d: %s", m.prog.File, ins.Line, src)
				}
				return Value{}, m.newError(fmt.Errorf("%s", msg), trace, ins.Line)
			}
		case OpSelect:
			cond := fr.regs[ins.B]
			if cond.Tag == ValueBool && cond.Bool {
				fr.regs[ins.A] = fr.regs[ins.C]
			} else {
				fr.regs[ins.A] = fr.regs[ins.D]
			}
		case OpStr:
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: fmt.Sprint(fr.regs[ins.B].ToAny())}
		case OpUpper:
			b := fr.regs[ins.B]
			if b.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("upper expects string"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToUpper(b.Str)}
		case OpLower:
			b := fr.regs[ins.B]
			if b.Tag == ValueStr {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToLower(b.Str)}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToLower(fmt.Sprint(b.ToAny()))}
			}
		case OpReverse:
			val := fr.regs[ins.B]
			switch val.Tag {
			case ValueList:
				newList := append([]Value(nil), val.List...)
				for i, j := 0, len(newList)-1; i < j; i, j = i+1, j-1 {
					newList[i], newList[j] = newList[j], newList[i]
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: newList}
			case ValueStr:
				r := []rune(val.Str)
				for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {
					r[i], r[j] = r[j], r[i]
				}
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(r)}
			default:
				return Value{}, m.newError(fmt.Errorf("reverse expects list or string"), trace, ins.Line)
			}
		case OpSHA256:
			v := fr.regs[ins.B]
			var data []byte
			if v.Tag == ValueStr {
				data = []byte(v.Str)
			} else if lst, ok := toList(v); ok {
				data = make([]byte, len(lst))
				for i, it := range lst {
					data[i] = byte(toInt(it))
				}
			} else {
				return Value{}, m.newError(fmt.Errorf("sha256 expects string or list"), trace, ins.Line)
			}
			sum := sha256.Sum256(data)
			out := make([]Value, len(sum))
			for i, b := range sum {
				out[i] = Value{Tag: ValueInt, Int: int(b)}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpInput:
			line, err := m.reader.ReadString('\n')
			if err != nil && err != io.EOF {
				return Value{}, err
			}
			line = strings.TrimRight(line, "\r\n")
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: line}
		case OpFirst:
			lst := fr.regs[ins.B]
			if lst.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("first expects list"), trace, ins.Line)
			}
			if len(lst.List) > 0 {
				fr.regs[ins.A] = lst.List[0]
			} else {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			}
		case OpIterPrep:
			src := fr.regs[ins.B]
			switch src.Tag {
			case ValueNull:
				fr.regs[ins.A] = Value{Tag: ValueList, List: nil}
			case ValueList:
				fr.regs[ins.A] = src
			case ValueMap:
				if flag, ok := src.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := src.Map["items"]
					fr.regs[ins.A] = items
					break
				}
				ks := make([]string, 0, len(src.Map))
				for k := range src.Map {
					ks = append(ks, k)
				}
				sort.Strings(ks)
				keys := make([]Value, len(ks))
				for i, k := range ks {
					keys[i] = Value{Tag: ValueStr, Str: k}
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: keys}
			case ValueStr:
				r := []rune(src.Str)
				lst := make([]Value, len(r))
				for i, ch := range r {
					lst[i] = Value{Tag: ValueStr, Str: string(ch)}
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: lst}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid iterator"), trace, ins.Line)
			}
		case OpLoad:
			path := fr.regs[ins.B].Str
			opts := fr.regs[ins.C].ToAny()
			optMap, _ := opts.(map[string]any)
			format := "csv"
			header := true
			delim := ','
			if optMap != nil {
				if f, ok := optMap["format"].(string); ok {
					format = f
				}
				if h, ok := optMap["header"].(bool); ok {
					header = h
				}
				if d, ok := optMap["delimiter"].(string); ok && len(d) > 0 {
					delim = rune(d[0])
				}
			}
			var rows []map[string]any
			var err error
			switch format {
			case "jsonl":
				if path == "" || path == "-" {
					rows, err = data.LoadJSONLReader(os.Stdin)
				} else {
					rows, err = data.LoadJSONL(path)
				}
			case "json":
				if path == "" || path == "-" {
					rows, err = data.LoadJSONReader(os.Stdin)
				} else {
					rows, err = data.LoadJSON(path)
				}
			case "yaml":
				if path == "" || path == "-" {
					rows, err = data.LoadYAMLReader(os.Stdin)
				} else {
					rows, err = data.LoadYAML(path)
				}
			case "tsv":
				delim = '\t'
				fallthrough
			default:
				if path == "" || path == "-" {
					rows, err = data.LoadCSVReader(os.Stdin, header, delim)
				} else {
					rows, err = data.LoadCSV(path, header, delim)
				}
			}
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			out := make([]Value, len(rows))
			if ins.D >= 0 {
				typ := m.prog.Types[ins.D]
				for i, row := range rows {
					val := any(row)
					cv, err := castValue(typ, val)
					if err != nil {
						return Value{}, m.newError(err, trace, ins.Line)
					}
					out[i] = FromAny(cv)
				}
			} else {
				for i, row := range rows {
					out[i] = FromAny(row)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpSave:
			srcVal := fr.regs[ins.B]
			path := fr.regs[ins.C].Str
			opts := fr.regs[ins.D].ToAny()
			optMap := toAnyMap(opts)
			format := "csv"
			header := false
			delim := ','
			if optMap != nil {
				if f, ok := optMap["format"].(string); ok {
					format = f
				}
				if h, ok := optMap["header"].(bool); ok {
					header = h
				}
				if d, ok := optMap["delimiter"].(string); ok && len(d) > 0 {
					delim = rune(d[0])
				}
			}
			rows, ok := toMapSlice(srcVal.ToAny())
			if !ok {
				return Value{}, m.newError(fmt.Errorf("save source must be list of maps"), trace, ins.Line)
			}
			var err error
			switch format {
			case "jsonl":
				if path == "" || path == "-" {
					err = data.SaveJSONLWriter(rows, m.writer)
				} else {
					err = data.SaveJSONL(rows, path)
				}
			case "json":
				if path == "" || path == "-" {
					err = data.SaveJSONWriter(rows, m.writer)
				} else {
					err = data.SaveJSON(rows, path)
				}
			case "yaml":
				if path == "" || path == "-" {
					err = data.SaveYAMLWriter(rows, m.writer)
				} else {
					err = data.SaveYAML(rows, path)
				}
			case "tsv":
				delim = '\t'
				fallthrough
			default:
				if path == "" || path == "-" {
					err = data.SaveCSVWriter(rows, m.writer, header, delim)
				} else {
					err = data.SaveCSV(rows, path, header, delim)
				}
			}
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueNull}
		case OpEval:
			srcVal := fr.regs[ins.B]
			if srcVal.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("eval expects string"), trace, ins.Line)
			}
			prog, err := parser.ParseString(srcVal.Str)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				return Value{}, m.newError(errs[0], trace, ins.Line)
			}
			p, errc := Compile(prog, env)
			if errc != nil {
				return Value{}, m.newError(errc, trace, ins.Line)
			}
			vm := New(p, io.Discard)
			resVal, err := vm.RunResult()
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = resVal
		case OpFetch:
			urlVal := fr.regs[ins.B]
			if urlVal.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("fetch URL must be string"), trace, ins.Line)
			}
			opts := fr.regs[ins.C].ToAny()
			optMap := toAnyMap(opts)
			res, err := mhttp.FetchWith(urlVal.Str, optMap)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpCount:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueList {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(lst.List)}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := lst.Map["items"]
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(items.List)}
					break
				}
			}
			return Value{}, fmt.Errorf("count expects list")
		case OpExists:
			lst := fr.regs[ins.B]
			switch lst.Tag {
			case ValueList:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.List) > 0}
			case ValueMap:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.Map) > 0}
			case ValueStr:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.Str) > 0}
			default:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			}
		case OpAvg:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("avg expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: 0}
			} else {
				var sum float64
				var count int
				for _, v := range lst.List {
					if v.Tag == ValueNull {
						continue
					}
					sum += toFloat(v)
					count++
				}
				if count == 0 {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: 0}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: sum / float64(count)}
				}
			}
		case OpSum:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("sum expects list")
			}
			var sumF float64
			var sumI int
			allInt := true
			for _, v := range lst.List {
				if v.Tag == ValueInt {
					sumI += v.Int
				} else {
					allInt = false
					sumF += toFloat(v)
				}
			}
			if allInt {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: sumI}
			} else {
				sumF += float64(sumI)
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: sumF}
			}
		case OpMin:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("min expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			} else {
				minVal := toFloat(lst.List[0])
				isFloat := lst.List[0].Tag == ValueFloat
				for _, v := range lst.List[1:] {
					if v.Tag == ValueFloat {
						isFloat = true
					}
					f := toFloat(v)
					if f < minVal {
						minVal = f
					}
				}
				if isFloat {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: minVal}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(minVal)}
				}
			}
		case OpMax:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("max expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			} else {
				maxVal := toFloat(lst.List[0])
				isFloat := lst.List[0].Tag == ValueFloat
				for _, v := range lst.List[1:] {
					if v.Tag == ValueFloat {
						isFloat = true
					}
					f := toFloat(v)
					if f > maxVal {
						maxVal = f
					}
				}
				if isFloat {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: maxVal}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(maxVal)}
				}
			}
		case OpValues:
			m := fr.regs[ins.B]
			if m.Tag != ValueMap {
				return Value{}, fmt.Errorf("values expects map")
			}
			if len(m.Map) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueList, List: []Value{}}
				break
			}
			keys := make([]string, 0, len(m.Map))
			for k := range m.Map {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			vals := make([]Value, 0, len(keys))
			for _, k := range keys {
				vals = append(vals, m.Map[k])
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: vals}
		case OpCast:
			val := fr.regs[ins.B].ToAny()
			typ := m.prog.Types[ins.C]
			cv, err := castValue(typ, val)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(cv)
		case OpMakeClosure:
			caps := make([]Value, ins.C)
			copy(caps, fr.regs[ins.D:ins.D+ins.C])
			cl := &closure{fn: ins.B, args: caps}
			fr.regs[ins.A] = Value{Tag: ValueFunc, Func: cl}
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
					if fnVal.Tag == ValueFunc {
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
			if fnVal.Tag == ValueFunc {
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
		case OpGoCall:
			name := fr.regs[ins.B].Str
			args := make([]any, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := goffi.Call(name, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpGoAutoCall:
			name := fr.regs[ins.B].Str
			args := make([]any, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := goffi.CallAuto(name, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpPyCall:
			mod := fr.regs[ins.B].Str
			attr := fr.regs[ins.C].Str
			count := ins.Val.Int
			args := make([]any, count)
			for i := 0; i < count; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := pythonffi.Attr(mod, attr, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpNot:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: !fr.regs[ins.B].Truthy()}
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
	types   []types.Type
	imports map[string]importMod
}

type importMod struct {
	lang string
	path string
	auto bool
}

type funcCompiler struct {
	fn        Function
	idx       int
	comp      *compiler
	vars      map[string]int
	scopes    []map[string]int
	loops     []*loopContext
	tags      map[int]regTag
	groupVar  string
	constRegs map[string]int
}

func (fc *funcCompiler) freshConst(pos lexer.Position, v Value) int {
	r := fc.newReg()
	fc.emit(pos, Instr{Op: OpConst, A: r, Val: v})
	return r
}

func (fc *funcCompiler) pushScope() {
	copyMap := make(map[string]int, len(fc.vars))
	for k, v := range fc.vars {
		copyMap[k] = v
	}
	fc.scopes = append(fc.scopes, fc.vars)
	fc.vars = copyMap
}

func (fc *funcCompiler) popScope() {
	if len(fc.scopes) == 0 {
		return
	}
	fc.vars = fc.scopes[len(fc.scopes)-1]
	fc.scopes = fc.scopes[:len(fc.scopes)-1]
}

type loopContext struct {
	breakJumps     []int
	continueJumps  []int
	continueTarget int
}

func (c *compiler) addType(t types.Type) int {
	idx := len(c.types)
	c.types = append(c.types, t)
	return idx
}

// Compile turns an AST into a Program supporting a limited subset of Mochi.
func Compile(p *parser.Program, env *types.Env) (*Program, error) {
	var src string
	if p.Pos.Filename != "" {
		if data, err := os.ReadFile(p.Pos.Filename); err == nil {
			src = string(data)
		}
	}
	return CompileWithSource(p, env, src)
}

// CompileWithSource compiles an AST along with its source code for richer errors.
func CompileWithSource(p *parser.Program, env *types.Env, src string) (*Program, error) {
	prog, err := compileProgram(p, env)
	if err != nil {
		return nil, err
	}
	prog.File = p.Pos.Filename
	if src != "" {
		prog.Source = strings.Split(src, "\n")
	}
	return prog, nil
}

func compileProgram(p *parser.Program, env *types.Env) (*Program, error) {
	c := &compiler{prog: p, env: env, fnIndex: map[string]int{}, imports: map[string]importMod{}}
	c.funcs = append(c.funcs, Function{})
	for _, st := range p.Statements {
		switch {
		case st.Fun != nil:
			idx := len(c.funcs)
			c.funcs = append(c.funcs, Function{})
			c.fnIndex[st.Fun.Name] = idx
			fn, err := c.compileFun(st.Fun)
			if err != nil {
				return nil, err
			}
			c.funcs[idx] = fn
		case st.Type != nil:
			if err := c.compileTypeMethods(st.Type); err != nil {
				return nil, err
			}
		}
	}
	main, err := c.compileMain(p)
	if err != nil {
		return nil, err
	}
	c.funcs[0] = main
	// Run liveness-based optimization on all functions unless disabled
	if os.Getenv("MOCHI_NO_OPT") == "" {
		for i := range c.funcs {
			Optimize(&c.funcs[i])
		}
	}
	return &Program{Funcs: c.funcs, Types: c.types}, nil
}

func (c *compiler) compileFun(fn *parser.FunStmt) (Function, error) {
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}, scopes: nil, groupVar: "", constRegs: map[string]int{}}
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
		if err := fc.compileStmt(st); err != nil {
			return Function{}, err
		}
	}
	fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	if fc.fn.NumRegs == 0 {
		fc.fn.NumRegs = 1
	}
	return fc.fn, nil
}

func (c *compiler) compileMethod(st types.StructType, fn *parser.FunStmt) (Function, error) {
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}, scopes: nil, groupVar: "", constRegs: map[string]int{}}
	fc.fn.Name = st.Name + "." + fn.Name
	fc.fn.Line = fn.Pos.Line
	fc.fn.NumParams = len(st.Order) + len(fn.Params)
	// struct fields as parameters
	for i, field := range st.Order {
		fc.vars[field] = i
		if i >= fc.fn.NumRegs {
			fc.fn.NumRegs = i + 1
		}
	}
	for i, p := range fn.Params {
		idx := len(st.Order) + i
		fc.vars[p.Name] = idx
		if idx >= fc.fn.NumRegs {
			fc.fn.NumRegs = idx + 1
		}
	}
	fc.idx = fc.fn.NumParams
	for _, stmnt := range fn.Body {
		if err := fc.compileStmt(stmnt); err != nil {
			return Function{}, err
		}
	}
	fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	if fc.fn.NumRegs == 0 {
		fc.fn.NumRegs = 1
	}
	return fc.fn, nil
}

func (c *compiler) compileTypeMethods(td *parser.TypeDecl) error {
	st, ok := c.env.GetStruct(td.Name)
	if !ok {
		return nil
	}
	for _, mem := range td.Members {
		if mem.Method == nil {
			continue
		}
		idx := len(c.funcs)
		c.funcs = append(c.funcs, Function{})
		name := td.Name + "." + mem.Method.Name
		c.fnIndex[name] = idx
		fn, err := c.compileMethod(st, mem.Method)
		if err != nil {
			return err
		}
		c.funcs[idx] = fn
	}
	return nil
}

func (c *compiler) compileFunExpr(fn *parser.FunExpr, captures []string) int {
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}, scopes: nil, groupVar: "", constRegs: map[string]int{}}
	fc.fn.Line = fn.Pos.Line
	fc.fn.NumParams = len(captures) + len(fn.Params)
	for i, name := range captures {
		fc.vars[name] = i
		if i >= fc.fn.NumRegs {
			fc.fn.NumRegs = i + 1
		}
	}
	for i, p := range fn.Params {
		idx := len(captures) + i
		fc.vars[p.Name] = idx
		if idx >= fc.fn.NumRegs {
			fc.fn.NumRegs = idx + 1
		}
	}
	fc.idx = fc.fn.NumParams
	if fn.ExprBody != nil {
		r := fc.compileExpr(fn.ExprBody)
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: r})
	} else {
		for _, st := range fn.BlockBody {
			if err := fc.compileStmt(st); err != nil {
				panic(err)
			}
		}
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	}
	if fc.fn.NumRegs == 0 {
		fc.fn.NumRegs = 1
	}
	idx := len(c.funcs)
	c.funcs = append(c.funcs, fc.fn)
	return idx
}

func (c *compiler) compileNamedFunExpr(name string, fn *parser.FunExpr, captures []string) int {
	idx := len(c.funcs)
	c.funcs = append(c.funcs, Function{})
	prev, exists := c.fnIndex[name]
	c.fnIndex[name] = idx

	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}, scopes: nil, groupVar: "", constRegs: map[string]int{}}
	fc.fn.Name = name
	fc.fn.Line = fn.Pos.Line
	fc.fn.NumParams = len(captures) + len(fn.Params)
	for i, name := range captures {
		fc.vars[name] = i
		if i >= fc.fn.NumRegs {
			fc.fn.NumRegs = i + 1
		}
	}
	for i, p := range fn.Params {
		idxp := len(captures) + i
		fc.vars[p.Name] = idxp
		if idxp >= fc.fn.NumRegs {
			fc.fn.NumRegs = idxp + 1
		}
	}
	fc.idx = fc.fn.NumParams
	if fn.ExprBody != nil {
		r := fc.compileExpr(fn.ExprBody)
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: r})
	} else {
		for _, st := range fn.BlockBody {
			if err := fc.compileStmt(st); err != nil {
				panic(err)
			}
		}
		fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	}
	if fc.fn.NumRegs == 0 {
		fc.fn.NumRegs = 1
	}
	c.funcs[idx] = fc.fn
	if !exists {
		delete(c.fnIndex, name)
	} else {
		c.fnIndex[name] = prev
	}
	return idx
}

func (c *compiler) compileMain(p *parser.Program) (Function, error) {
	fc := &funcCompiler{comp: c, vars: map[string]int{}, tags: map[int]regTag{}, scopes: nil, groupVar: "", constRegs: map[string]int{}}
	fc.fn.Name = "main"
	fc.fn.Line = 0
	fc.fn.NumParams = 0
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if err := fc.compileStmt(st); err != nil {
			return Function{}, err
		}
	}
	fc.emit(lexer.Position{}, Instr{Op: OpReturn, A: 0})
	if fc.fn.NumRegs == 0 {
		fc.fn.NumRegs = 1
	}
	return fc.fn, nil
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
	case OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpNegInt:
		fc.tags[i.A] = tagInt
	case OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpNegFloat:
		fc.tags[i.A] = tagFloat
	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpNeg:
		fc.tags[i.A] = tagUnknown
	case OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpNot:
		fc.tags[i.A] = tagBool
	case OpLen, OpCap, OpNow:
		fc.tags[i.A] = tagInt
	case OpJSON, OpPrint, OpPrint2, OpPrintN:
		// no result
	case OpAppend, OpStr, OpUpper, OpLower, OpReverse, OpInput, OpFirst, OpSHA256:
		fc.tags[i.A] = tagUnknown
	case OpLoad:
		fc.tags[i.A] = tagUnknown
	case OpSave:
		fc.tags[i.A] = tagUnknown
	case OpFetch:
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

func constKey(v Value) (string, bool) {
	switch v.Tag {
	case ValueInt:
		return fmt.Sprintf("i%v", v.Int), true
	case ValueFloat:
		s := strconv.FormatFloat(v.Float, 'f', -1, 64)
		if strings.Contains(s, ".") {
			s = strings.TrimRight(s, "0")
			s = strings.TrimRight(s, ".")
		}
		return "f" + s, true
	case ValueBigInt:
		if v.BigInt != nil {
			return "bi" + v.BigInt.String(), true
		}
		return "bi0", true
	case ValueBool:
		if v.Bool {
			return "bt", true
		}
		return "bf", true
	case ValueStr:
		return "s" + v.Str, true
	case ValueList:
		if len(v.List) == 0 {
			return "[]", true
		}
		return "", false
	case ValueMap:
		if len(v.Map) == 0 {
			return "{}", true
		}
		return "", false
	case ValueNull:
		return "n", true
	default:
		return "", false
	}
}

func (fc *funcCompiler) constReg(pos lexer.Position, v Value) int {
	if key, ok := constKey(v); ok {
		if r, exists := fc.constRegs[key]; exists {
			// Emit the constant again so the register is initialized
			// along all control-flow paths.
			fc.emit(pos, Instr{Op: OpConst, A: r, Val: v})
			return r
		}
		r := fc.newReg()
		fc.emit(pos, Instr{Op: OpConst, A: r, Val: v})
		if fc.constRegs == nil {
			fc.constRegs = map[string]int{}
		}
		fc.constRegs[key] = r
		return r
	}
	r := fc.newReg()
	fc.emit(pos, Instr{Op: OpConst, A: r, Val: v})
	return r
}

func (fc *funcCompiler) constListLen(e *parser.Expr) (int, bool) {
	if v, ok := fc.evalConstExpr(e); ok && v.Tag == ValueList {
		return len(v.List), true
	}
	return 0, false
}

// smallConstJoin reports whether both expressions evaluate to constant lists
// whose lengths are below the smallJoinThreshold. When this returns true the
// compiler avoids generating a hash join and emits a simple nested loop join
// instead.
func (fc *funcCompiler) smallConstJoin(left, right *parser.Expr) bool {
	ll, ok1 := fc.constListLen(left)
	if !ok1 {
		return false
	}
	rl, ok2 := fc.constListLen(right)
	if !ok2 {
		return false
	}
	return ll <= smallJoinThreshold && rl <= smallJoinThreshold
}

func (fc *funcCompiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		r := fc.compileExpr(s.Let.Value)
		if typ, err := fc.comp.env.GetVar(s.Let.Name); err == nil {
			if _, ok := typ.(types.BigIntType); ok {
				idx := fc.comp.addType(typ)
				dst := fc.newReg()
				fc.emit(s.Let.Pos, Instr{Op: OpCast, A: dst, B: r, C: idx})
				r = dst
			}
		}
		fc.vars[s.Let.Name] = r
		if v, ok := fc.evalConstExpr(s.Let.Value); ok {
			if typ, err := fc.comp.env.GetVar(s.Let.Name); err == nil {
				cv, err2 := castValue(typ, v.ToAny())
				if err2 == nil {
					v = FromAny(cv)
				}
			}
			fc.comp.env.SetValue(s.Let.Name, v.ToAny(), false)
		}
		return nil
	case s.Var != nil:
		r := fc.compileExpr(s.Var.Value)
		reg := fc.newReg()
		if typ, err := fc.comp.env.GetVar(s.Var.Name); err == nil {
			if _, ok := typ.(types.BigIntType); ok {
				idx := fc.comp.addType(typ)
				dst := fc.newReg()
				fc.emit(s.Var.Pos, Instr{Op: OpCast, A: dst, B: r, C: idx})
				r = dst
			}
		}
		fc.vars[s.Var.Name] = reg
		fc.emit(s.Var.Pos, Instr{Op: OpMove, A: reg, B: r})
		fc.tags[reg] = fc.tags[r]
		return nil
	case s.Assign != nil:
		reg, ok := fc.vars[s.Assign.Name]
		if !ok {
			return fmt.Errorf("assignment to undeclared variable: %s", s.Assign.Name)
		}
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			r := fc.compileExpr(s.Assign.Value)
			fc.emit(s.Assign.Pos, Instr{Op: OpMove, A: reg, B: r})
			fc.tags[reg] = fc.tags[r]
		} else {
			container := reg
			idxOps := s.Assign.Index
			fieldOps := s.Assign.Field
			for _, idxOp := range idxOps {
				if idxOp != idxOps[len(idxOps)-1] || len(fieldOps) > 0 {
					idx := fc.compileExpr(idxOp.Start)
					dst := fc.newReg()
					fc.emit(idxOp.Pos, Instr{Op: OpIndex, A: dst, B: container, C: idx})
					container = dst
				}
			}
			for i, fop := range fieldOps {
				if i < len(fieldOps)-1 {
					key := fc.constReg(fop.Pos, Value{Tag: ValueStr, Str: fop.Name})
					dst := fc.newReg()
					fc.emit(fop.Pos, Instr{Op: OpIndex, A: dst, B: container, C: key})
					container = dst
				}
			}
			val := fc.compileExpr(s.Assign.Value)
			if len(fieldOps) > 0 {
				lastField := fieldOps[len(fieldOps)-1]
				key := fc.constReg(lastField.Pos, Value{Tag: ValueStr, Str: lastField.Name})
				fc.emit(s.Assign.Pos, Instr{Op: OpSetIndex, A: container, B: key, C: val})
			} else {
				lastIdx := fc.compileExpr(idxOps[len(idxOps)-1].Start)
				fc.emit(s.Assign.Pos, Instr{Op: OpSetIndex, A: container, B: lastIdx, C: val})
			}
		}
		return nil
	case s.Return != nil:
		r := fc.compileExpr(s.Return.Value)
		fc.emit(s.Return.Pos, Instr{Op: OpReturn, A: r})
		return nil
	case s.Expr != nil:
		fc.compileExpr(s.Expr.Expr)
		return nil
	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		if s.Import.Lang != nil {
			fc.comp.imports[alias] = importMod{lang: *s.Import.Lang, path: strings.Trim(s.Import.Path, "\""), auto: s.Import.Auto}
		}
		return nil
	case s.Fun != nil:
		captureNames := make([]string, 0, len(fc.vars))
		for name := range fc.vars {
			captureNames = append(captureNames, name)
		}
		sort.Strings(captureNames)

		idx := fc.comp.compileNamedFunExpr(s.Fun.Name, &parser.FunExpr{Pos: s.Fun.Pos, Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body}, captureNames)

		capRegs := make([]int, len(captureNames))
		for i, name := range captureNames {
			r := fc.vars[name]
			reg := fc.newReg()
			fc.emit(s.Fun.Pos, Instr{Op: OpMove, A: reg, B: r})
			capRegs[i] = reg
		}
		dst := fc.newReg()
		start := 0
		if len(capRegs) > 0 {
			start = capRegs[0]
		}
		fc.emit(s.Fun.Pos, Instr{Op: OpMakeClosure, A: dst, B: idx, C: len(capRegs), D: start})
		fc.vars[s.Fun.Name] = dst
		return nil
	case s.If != nil:
		return fc.compileIf(s.If)
	case s.While != nil:
		return fc.compileWhile(s.While)
	case s.For != nil:
		return fc.compileFor(s.For)
	case s.Update != nil:
		return fc.compileUpdate(s.Update)
	case s.Break != nil:
		if l := len(fc.loops); l > 0 {
			idx := len(fc.fn.Code)
			fc.emit(s.Break.Pos, Instr{Op: OpJump})
			fc.loops[l-1].breakJumps = append(fc.loops[l-1].breakJumps, idx)
		}
		return nil
	case s.Continue != nil:
		if l := len(fc.loops); l > 0 {
			idx := len(fc.fn.Code)
			fc.emit(s.Continue.Pos, Instr{Op: OpJump})
			fc.loops[l-1].continueJumps = append(fc.loops[l-1].continueJumps, idx)
		}
		return nil
	case s.Test != nil:
		fc.pushScope()
		for _, st := range s.Test.Body {
			if err := fc.compileStmt(st); err != nil {
				fc.popScope()
				return err
			}
		}
		fc.popScope()
		return nil
	case s.Expect != nil:
		r := fc.compileExpr(s.Expect.Value)
		fc.emit(s.Expect.Pos, Instr{Op: OpExpect, A: r})
		return nil
	}
	return nil
}

func (fc *funcCompiler) compileExpr(e *parser.Expr) int {
	if e == nil {
		return fc.newReg()
	}
	return fc.compileBinary(e.Binary)
}

func (fc *funcCompiler) compileBinary(b *parser.BinaryExpr) int {
	if len(b.Right) == 0 {
		return fc.compileUnary(b.Left)
	}

	type operand struct {
		reg     int
		done    bool
		compile func() int
	}

	valueOf := func(o *operand) int {
		if o.done {
			return o.reg
		}
		o.reg = o.compile()
		o.done = true
		return o.reg
	}

	operands := []operand{{reg: fc.compileUnary(b.Left), done: true}}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr := op.Right
		ops[i] = op
		operands = append(operands, operand{compile: func() int { return fc.compilePostfix(expr) }})
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			opName := ops[i].Op
			if opName == "union" && ops[i].All {
				opName = "union_all"
			}
			if contains(level, opName) {
				left := valueOf(&operands[i])
				var dst int
				if opName == "&&" || opName == "||" {
					dst = fc.newReg()
					fc.emit(ops[i].Pos, Instr{Op: OpMove, A: dst, B: left})
					jumps := []int{}
					condOp := OpJumpIfFalse
					if opName == "||" {
						condOp = OpJumpIfTrue
					}
					j := i
					for ; j < len(ops) && ops[j].Op == opName; j++ {
						jmp := len(fc.fn.Code)
						fc.emit(ops[j].Pos, Instr{Op: condOp, A: dst})
						right := valueOf(&operands[j+1])
						fc.emit(ops[j].Pos, Instr{Op: OpMove, A: dst, B: right})
						jumps = append(jumps, jmp)
					}
					end := len(fc.fn.Code)
					for _, jmp := range jumps {
						fc.fn.Code[jmp].B = end
					}
					operands[i] = operand{reg: dst, done: true}
					operands = append(operands[:i+1], operands[j+1:]...)
					ops = append(ops[:i], ops[j:]...)
					continue
				} else {
					right := valueOf(&operands[i+1])
					dst = fc.emitBinaryOp(ops[i].Pos, opName, ops[i].All, left, right)
				}
				operands[i] = operand{reg: dst, done: true}
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	return valueOf(&operands[0])
}

func (fc *funcCompiler) emitBinaryOp(pos lexer.Position, op string, all bool, left, right int) int {
	switch op {
	case "&&":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpMove, A: dst, B: left})
		jmp := len(fc.fn.Code)
		fc.emit(pos, Instr{Op: OpJumpIfFalse, A: dst})
		fc.emit(pos, Instr{Op: OpMove, A: dst, B: right})
		fc.fn.Code[jmp].B = len(fc.fn.Code)
		return dst
	case "||":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpMove, A: dst, B: left})
		jmp := len(fc.fn.Code)
		fc.emit(pos, Instr{Op: OpJumpIfTrue, A: dst})
		fc.emit(pos, Instr{Op: OpMove, A: dst, B: right})
		fc.fn.Code[jmp].B = len(fc.fn.Code)
		return dst
	case "+":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpAddFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpAddInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpAdd, A: dst, B: left, C: right})
		}
		return dst
	case "-":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpSubFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpSubInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpSub, A: dst, B: left, C: right})
		}
		return dst
	case "*":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpMulFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpMulInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpMul, A: dst, B: left, C: right})
		}
		return dst
	case "/":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			// SQL division with floats yields a float result.
			fc.emit(pos, Instr{Op: OpDivFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpDivInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpDiv, A: dst, B: left, C: right})
		}
		return dst
	case "%":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpModFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpModInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpMod, A: dst, B: left, C: right})
		}
		return dst
	case "==":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpEqualFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpEqualInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpEqual, A: dst, B: left, C: right})
		}
		return dst
	case "!=":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpNotEqual, A: dst, B: left, C: right})
		return dst
	case "<":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpLessFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpLessInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpLess, A: dst, B: left, C: right})
		}
		return dst
	case ">":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpLessFloat, A: dst, B: right, C: left})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpLessInt, A: dst, B: right, C: left})
		} else {
			fc.emit(pos, Instr{Op: OpLess, A: dst, B: right, C: left})
		}
		return dst
	case "<=":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpLessEqFloat, A: dst, B: left, C: right})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpLessEqInt, A: dst, B: left, C: right})
		} else {
			fc.emit(pos, Instr{Op: OpLessEq, A: dst, B: left, C: right})
		}
		return dst
	case ">=":
		dst := fc.newReg()
		if fc.tags[left] == tagFloat || fc.tags[right] == tagFloat {
			fc.emit(pos, Instr{Op: OpLessEqFloat, A: dst, B: right, C: left})
		} else if fc.tags[left] == tagInt && fc.tags[right] == tagInt {
			fc.emit(pos, Instr{Op: OpLessEqInt, A: dst, B: right, C: left})
		} else {
			fc.emit(pos, Instr{Op: OpLessEq, A: dst, B: right, C: left})
		}
		return dst
	case "in":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpIn, A: dst, B: left, C: right})
		return dst
	case "union", "union_all":
		dst := fc.newReg()
		opCode := OpUnion
		if op == "union_all" || all {
			opCode = OpUnionAll
		}
		fc.emit(pos, Instr{Op: opCode, A: dst, B: left, C: right})
		return dst
	case "except":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpExcept, A: dst, B: left, C: right})
		return dst
	case "intersect":
		dst := fc.newReg()
		fc.emit(pos, Instr{Op: OpIntersect, A: dst, B: left, C: right})
		return dst
	}
	return left
}

func (fc *funcCompiler) compileUnary(u *parser.Unary) int {
	r := fc.compilePostfix(u.Value)
	for _, op := range u.Ops {
		switch op {
		case "-":
			dst := fc.newReg()
			switch fc.tags[r] {
			case tagInt:
				fc.emit(u.Pos, Instr{Op: OpNegInt, A: dst, B: r})
			case tagFloat:
				fc.emit(u.Pos, Instr{Op: OpNegFloat, A: dst, B: r})
			default:
				fc.emit(u.Pos, Instr{Op: OpNeg, A: dst, B: r})
			}
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
	if v, ok := fc.evalConstPostfix(p); ok {
		return fc.constReg(p.Target.Pos, v)
	}

	if sel := p.Target.Selector; sel != nil {
		if imp, ok := fc.comp.imports[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) == 0 {
				return fc.compileFFI(imp, attr, nil, p.Target.Pos)
			}
			if len(p.Ops) == 1 && p.Ops[0].Call != nil {
				return fc.compileFFI(imp, attr, p.Ops[0].Call, p.Ops[0].Call.Pos)
			}
		}
	}
	// Special case for struct method calls like obj.method()
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 {
		rootName := p.Target.Selector.Root
		methodName := p.Target.Selector.Tail[0]
		// strings.ToUpper(x) -> upper(x)
		if rootName == "strings" && methodName == "ToUpper" {
			arg := fc.compileExpr(p.Ops[0].Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpUpper, A: dst, B: arg})
			return dst
		}
		// strings.ToLower(x) -> lower(x)
		if rootName == "strings" && methodName == "ToLower" {
			arg := fc.compileExpr(p.Ops[0].Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpLower, A: dst, B: arg})
			return dst
		}
		if typ, err := fc.comp.env.GetVar(rootName); err == nil {
			if st, ok := typ.(types.StructType); ok {
				if _, ok := st.Methods[methodName]; ok {
					objReg, ok := fc.vars[rootName]
					if !ok {
						objReg = fc.newReg()
					}
					total := len(st.Order) + len(p.Ops[0].Call.Args)
					regs := make([]int, total)
					for i, field := range st.Order {
						key := fc.newReg()
						fc.emit(p.Target.Pos, Instr{Op: OpConst, A: key, Val: Value{Tag: ValueStr, Str: field}})
						val := fc.newReg()
						fc.emit(p.Target.Pos, Instr{Op: OpIndex, A: val, B: objReg, C: key})
						regs[i] = val
					}
					for i, a := range p.Ops[0].Call.Args {
						ar := fc.compileExpr(a)
						reg := fc.newReg()
						fc.emit(a.Pos, Instr{Op: OpMove, A: reg, B: ar})
						regs[len(st.Order)+i] = reg
					}
					dst := fc.newReg()
					start := 0
					if len(regs) > 0 {
						start = regs[0]
					}
					idx := fc.comp.fnIndex[st.Name+"."+methodName]
					fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpCall, A: dst, B: idx, C: len(regs), D: start})
					return dst
				}
			}
		}
	}

	// "contains" method on strings or lists
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" {
		recvSel := &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1]}}
		recv := fc.compilePrimary(recvSel)
		arg := fc.compileExpr(p.Ops[0].Call.Args[0])
		dst := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpIn, A: dst, B: arg, C: recv})
		return dst
	}

	// "starts_with" method on strings
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "starts_with" {
		recvSel := &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1]}}
		recv := fc.compilePrimary(recvSel)
		arg := fc.compileExpr(p.Ops[0].Call.Args[0])
		zero := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpConst, A: zero, Val: Value{Tag: ValueInt, Int: 0}})
		plen := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpLen, A: plen, B: arg})
		rlen := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpLen, A: rlen, B: recv})
		cond := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpLessEq, A: cond, B: plen, C: rlen})
		dst := fc.newReg()
		skip := len(fc.fn.Code)
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		prefix := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpSlice, A: prefix, B: recv, C: zero, D: plen})
		eq := fc.newReg()
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpEqual, A: eq, B: prefix, C: arg})
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpMove, A: dst, B: eq})
		jmpEnd := len(fc.fn.Code)
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpJump})
		elseIdx := len(fc.fn.Code)
		fc.fn.Code[skip].B = elseIdx
		fc.emit(p.Ops[0].Call.Pos, Instr{Op: OpConst, A: dst, Val: Value{Tag: ValueBool, Bool: false}})
		fc.fn.Code[jmpEnd].A = len(fc.fn.Code)
		return dst
	}

	r := fc.compilePrimary(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := fc.newReg()
				if op.Index.Start != nil {
					s := fc.compileExpr(op.Index.Start)
					fc.emit(op.Index.Start.Pos, Instr{Op: OpMove, A: start, B: s})
				} else {
					fc.emit(op.Index.Pos, Instr{Op: OpConst, A: start, Val: Value{Tag: ValueNull}})
				}
				end := fc.newReg()
				if op.Index.End != nil {
					e := fc.compileExpr(op.Index.End)
					fc.emit(op.Index.End.Pos, Instr{Op: OpMove, A: end, B: e})
				} else {
					fc.emit(op.Index.Pos, Instr{Op: OpConst, A: end, Val: Value{Tag: ValueNull}})
				}
				dst := fc.newReg()
				fc.emit(op.Index.Pos, Instr{Op: OpSlice, A: dst, B: r, C: start, D: end})
				r = dst
			} else {
				idx := fc.compileExpr(op.Index.Start)
				dst := fc.newReg()
				fc.emit(op.Index.Pos, Instr{Op: OpIndex, A: dst, B: r, C: idx})
				r = dst
			}
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
		} else if op.Cast != nil {
			typ := resolveTypeRef(op.Cast.Type, fc.comp.env)
			idx := fc.comp.addType(typ)
			dst := fc.newReg()
			fc.emit(op.Cast.Pos, Instr{Op: OpCast, A: dst, B: r, C: idx})
			r = dst
		}
	}
	return r
}

func (fc *funcCompiler) compileFFI(im importMod, attr string, call *parser.CallOp, pos lexer.Position) int {
	var argRegs []int
	if call != nil {
		argRegs = make([]int, len(call.Args))
		for i := range call.Args {
			argRegs[i] = fc.newReg()
		}
		for i, a := range call.Args {
			ar := fc.compileExpr(a)
			fc.emit(a.Pos, Instr{Op: OpMove, A: argRegs[i], B: ar})
		}
	}
	dst := fc.newReg()
	start := 0
	if len(argRegs) > 0 {
		start = argRegs[0]
	}
	switch im.lang {
	case "python":
		mod := fc.constReg(pos, Value{Tag: ValueStr, Str: im.path})
		attrReg := fc.constReg(pos, Value{Tag: ValueStr, Str: attr})
		fc.emit(pos, Instr{Op: OpPyCall, A: dst, B: mod, C: attrReg, D: start, Val: Value{Tag: ValueInt, Int: len(argRegs)}})
	case "go":
		name := im.path
		if attr != "" {
			name += "." + attr
		}
		nameReg := fc.constReg(pos, Value{Tag: ValueStr, Str: name})
		op := OpGoCall
		if im.auto {
			op = OpGoAutoCall
		}
		fc.emit(pos, Instr{Op: op, A: dst, B: nameReg, C: len(argRegs), D: start})
	}
	return dst
}

func (fc *funcCompiler) compilePrimary(p *parser.Primary) int {
	if p.Match != nil {
		return fc.compileMatch(p.Match)
	}
	if p.If != nil {
		return fc.compileIfExpr(p.If)
	}
	if p.Lit != nil {
		switch {
		case p.Lit.Int != nil:
			v := Value{Tag: ValueInt, Int: *p.Lit.Int}
			return fc.constReg(p.Pos, v)
		case p.Lit.Float != nil:
			v := Value{Tag: ValueFloat, Float: *p.Lit.Float}
			return fc.constReg(p.Pos, v)
		case p.Lit.Str != nil:
			v := Value{Tag: ValueStr, Str: *p.Lit.Str}
			return fc.constReg(p.Pos, v)
		case p.Lit.Bool != nil:
			v := Value{Tag: ValueBool, Bool: bool(*p.Lit.Bool)}
			return fc.constReg(p.Pos, v)
		case p.Lit.Null:
			v := Value{Tag: ValueNull}
			return fc.constReg(p.Pos, v)
		}
	}

	if p.List != nil {
		if v, ok := constList(p.List); ok {
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		}
		regs := make([]int, len(p.List.Elems))
		for i := range p.List.Elems {
			regs[i] = fc.newReg()
		}
		for i, e := range p.List.Elems {
			r := fc.compileExpr(e)
			fc.emit(e.Pos, Instr{Op: OpMove, A: regs[i], B: r})
		}
		dst := fc.newReg()
		fc.emit(p.Pos, Instr{Op: OpMakeList, A: dst, B: len(regs), C: regs[0]})
		return dst
	}

	if p.Struct != nil {
		vals := make([]int, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			vals[i] = fc.compileExpr(f.Value)
		}

		regs := make([]int, len(p.Struct.Fields)*2+2)

		nameKey := fc.freshConst(p.Pos, Value{Tag: ValueStr, Str: "__name"})
		nameVal := fc.freshConst(p.Pos, Value{Tag: ValueStr, Str: p.Struct.Name})
		regs[0] = nameKey
		regs[1] = nameVal
		for i, f := range p.Struct.Fields {
			kreg := fc.freshConst(f.Pos, Value{Tag: ValueStr, Str: f.Name})
			vreg := fc.newReg()
			fc.emit(f.Pos, Instr{Op: OpMove, A: vreg, B: vals[i]})
			regs[i*2+2] = kreg
			regs[i*2+3] = vreg
		}
		dst := fc.newReg()
		start := regs[0]
		fc.emit(p.Pos, Instr{Op: OpMakeMap, A: dst, B: len(p.Struct.Fields) + 1, C: start})
		return dst
	}

	if p.Group != nil {
		return fc.compileExpr(p.Group)
	}

	if p.Map != nil {
		if v, ok := constMap(p.Map); ok {
			return fc.constReg(p.Pos, v)
		}
		tmp := make([]struct{ k, v int }, len(p.Map.Items))
		for i, it := range p.Map.Items {
			if name, ok := identName(it.Key); ok {
				tmp[i].k = fc.freshConst(it.Pos, Value{Tag: ValueStr, Str: name})
			} else {
				tmp[i].k = fc.compileExpr(it.Key)
			}
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

	if p.Query != nil {
		return fc.compileQuery(p.Query)
	}

	if p.Load != nil {
		path := ""
		if p.Load.Path != nil {
			path = *p.Load.Path
		}
		pathReg := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: path})
		var optsReg int
		if p.Load.With != nil {
			r := fc.compileExpr(p.Load.With)
			optsReg = fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMove, A: optsReg, B: r})
		} else {
			optsReg = fc.constReg(p.Pos, Value{Tag: ValueNull})
		}
		dst := fc.newReg()
		typeIdx := -1
		if p.Load.Type != nil {
			typ := resolveTypeRef(p.Load.Type, fc.comp.env)
			typeIdx = fc.comp.addType(typ)
		}
		fc.emit(p.Pos, Instr{Op: OpLoad, A: dst, B: pathReg, C: optsReg, D: typeIdx})
		return dst
	}

	if p.Save != nil {
		src := fc.compileExpr(p.Save.Src)
		path := ""
		if p.Save.Path != nil {
			path = *p.Save.Path
		}
		pathReg := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: path})
		var optsReg int
		if p.Save.With != nil {
			r := fc.compileExpr(p.Save.With)
			optsReg = fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMove, A: optsReg, B: r})
		} else {
			optsReg = fc.constReg(p.Pos, Value{Tag: ValueNull})
		}
		dst := fc.newReg()
		fc.emit(p.Pos, Instr{Op: OpSave, A: dst, B: src, C: pathReg, D: optsReg})
		return dst
	}

	if p.Fetch != nil {
		url := fc.compileExpr(p.Fetch.URL)
		var optsReg int
		if p.Fetch.With != nil {
			r := fc.compileExpr(p.Fetch.With)
			optsReg = fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMove, A: optsReg, B: r})
		} else {
			optsReg = fc.constReg(p.Pos, Value{Tag: ValueNull})
		}
		dst := fc.newReg()
		fc.emit(p.Pos, Instr{Op: OpFetch, A: dst, B: url, C: optsReg})
		return dst
	}

	if p.FunExpr != nil {
		captureNames := make([]string, 0, len(fc.vars))
		for name := range fc.vars {
			captureNames = append(captureNames, name)
		}
		sort.Strings(captureNames)
		idx := fc.comp.compileFunExpr(p.FunExpr, captureNames)
		capRegs := make([]int, len(captureNames))
		for i, name := range captureNames {
			r := fc.vars[name]
			reg := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMove, A: reg, B: r})
			capRegs[i] = reg
		}
		dst := fc.newReg()
		start := 0
		if len(capRegs) > 0 {
			start = capRegs[0]
		}
		fc.emit(p.Pos, Instr{Op: OpMakeClosure, A: dst, B: idx, C: len(capRegs), D: start})
		return dst
	}

	if p.Selector != nil {
		r, ok := fc.vars[p.Selector.Root]
		if !ok {
			if len(p.Selector.Tail) == 0 {
				if st, ok := fc.comp.env.GetStruct(p.Selector.Root); ok && len(st.Order) == 0 {
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "__name"})
					fc.constReg(p.Pos, Value{Tag: ValueStr, Str: st.Name})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpMakeMap, A: dst, B: 1, C: key})
					return dst
				}
			}
			r = fc.newReg()
		}
		if len(p.Selector.Tail) == 0 {
			return r
		}
		cur := r
		for _, field := range p.Selector.Tail {
			key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: field})
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpIndex, A: dst, B: cur, C: key})
			cur = dst
		}
		return cur
	}

	if p.Call != nil {
		if v, ok := fc.foldCallValue(p.Call); ok {
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpConst, A: dst, Val: v})
			return dst
		}
		switch p.Call.Func {
		case "len":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpLen, A: dst, B: arg})
			return dst
		case "cap":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpCap, A: dst, B: arg})
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
			if fc.groupVar != "" {
				if name, ok := identName(p.Call.Args[0]); ok && name == fc.groupVar {
					greg, ok := fc.vars[fc.groupVar]
					if !ok {
						greg = fc.newReg()
						fc.vars[fc.groupVar] = greg
					}
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "count"})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpIndex, A: dst, B: greg, C: key})
					return dst
				}
			}
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpCount, A: dst, B: arg})
			return dst
		case "exists":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpExists, A: dst, B: arg})
			return dst
		case "avg":
			if fc.groupVar != "" {
				if name, ok := identName(p.Call.Args[0]); ok && name == fc.groupVar {
					greg, ok := fc.vars[fc.groupVar]
					if !ok {
						greg = fc.newReg()
						fc.vars[fc.groupVar] = greg
					}
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "items"})
					lst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpIndex, A: lst, B: greg, C: key})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpAvg, A: dst, B: lst})
					return dst
				}
			}
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpAvg, A: dst, B: arg})
			return dst
		case "sum":
			if fc.groupVar != "" {
				if name, ok := identName(p.Call.Args[0]); ok && name == fc.groupVar {
					greg, ok := fc.vars[fc.groupVar]
					if !ok {
						greg = fc.newReg()
						fc.vars[fc.groupVar] = greg
					}
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "items"})
					lst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpIndex, A: lst, B: greg, C: key})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpSum, A: dst, B: lst})
					return dst
				}
			}
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpSum, A: dst, B: arg})
			return dst
		case "min":
			if fc.groupVar != "" {
				if name, ok := identName(p.Call.Args[0]); ok && name == fc.groupVar {
					greg, ok := fc.vars[fc.groupVar]
					if !ok {
						greg = fc.newReg()
						fc.vars[fc.groupVar] = greg
					}
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "items"})
					lst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpIndex, A: lst, B: greg, C: key})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpMin, A: dst, B: lst})
					return dst
				}
			}
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMin, A: dst, B: arg})
			return dst
		case "max":
			if fc.groupVar != "" {
				if name, ok := identName(p.Call.Args[0]); ok && name == fc.groupVar {
					greg, ok := fc.vars[fc.groupVar]
					if !ok {
						greg = fc.newReg()
						fc.vars[fc.groupVar] = greg
					}
					key := fc.constReg(p.Pos, Value{Tag: ValueStr, Str: "items"})
					lst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpIndex, A: lst, B: greg, C: key})
					dst := fc.newReg()
					fc.emit(p.Pos, Instr{Op: OpMax, A: dst, B: lst})
					return dst
				}
			}
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpMax, A: dst, B: arg})
			return dst
		case "values":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpValues, A: dst, B: arg})
			return dst
		case "concat":
			if len(p.Call.Args) == 0 {
				dst := fc.newReg()
				fc.emit(p.Pos, Instr{Op: OpMakeList, A: dst, B: 0, C: 0})
				return dst
			}
			dst := fc.compileExpr(p.Call.Args[0])
			for _, a := range p.Call.Args[1:] {
				reg := fc.compileExpr(a)
				tmp := fc.newReg()
				fc.emit(p.Pos, Instr{Op: OpUnionAll, A: tmp, B: dst, C: reg})
				dst = tmp
			}
			return dst
		case "first":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpFirst, A: dst, B: arg})
			return dst
		case "substring":
			str := fc.compileExpr(p.Call.Args[0])
			start := fc.compileExpr(p.Call.Args[1])
			end := fc.compileExpr(p.Call.Args[2])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpSlice, A: dst, B: str, C: start, D: end})
			return dst
		case "substr":
			str := fc.compileExpr(p.Call.Args[0])
			start := fc.compileExpr(p.Call.Args[1])
			end := fc.compileExpr(p.Call.Args[2])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpSlice, A: dst, B: str, C: start, D: end})
			return dst
		case "eval":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpEval, A: dst, B: arg})
			return dst
		case "upper":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpUpper, A: dst, B: arg})
			return dst
		case "lower":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpLower, A: dst, B: arg})
			return dst
		case "reverse":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpReverse, A: dst, B: arg})
			return dst
		case "sha256":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpSHA256, A: dst, B: arg})
			return dst
		case "distinct":
			arg := fc.compileExpr(p.Call.Args[0])
			dst := fc.newReg()
			fc.emit(p.Pos, Instr{Op: OpDistinct, A: dst, B: arg})
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
			regs := make([]int, len(p.Call.Args))
			for i := range p.Call.Args {
				regs[i] = fc.newReg()
			}
			for i, a := range p.Call.Args {
				ar := fc.compileExpr(a)
				fc.emit(a.Pos, Instr{Op: OpMove, A: regs[i], B: ar})
			}
			fc.emit(p.Pos, Instr{Op: OpPrintN, A: regs[0], B: len(regs), C: regs[0]})
			return regs[0]
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

func (fc *funcCompiler) compileFor(f *parser.ForStmt) error {
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
		fc.emit(f.Pos, Instr{Op: OpLessInt, A: cond, B: idx, C: end})
		jmp := len(fc.fn.Code)
		fc.emit(f.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		loop := &loopContext{}
		fc.loops = append(fc.loops, loop)
		fc.pushScope()
		for _, st := range f.Body {
			if err := fc.compileStmt(st); err != nil {
				fc.popScope()
				return err
			}
		}
		fc.popScope()
		contTarget := len(fc.fn.Code)
		for _, idx := range loop.continueJumps {
			fc.fn.Code[idx].A = contTarget
		}
		one := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpConst, A: one, Val: Value{Tag: ValueInt, Int: 1}})
		tmp := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpAddInt, A: tmp, B: idx, C: one})
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
		fc.emit(f.Pos, Instr{Op: OpConst, A: idx, Val: Value{Tag: ValueInt, Int: 0}})
		loopStart := len(fc.fn.Code)
		cond := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpLessInt, A: cond, B: idx, C: length})
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
		fc.pushScope()
		for _, st := range f.Body {
			if err := fc.compileStmt(st); err != nil {
				fc.popScope()
				return err
			}
		}
		fc.popScope()
		contTarget := len(fc.fn.Code)
		for _, idx := range loop.continueJumps {
			fc.fn.Code[idx].A = contTarget
		}
		one := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpConst, A: one, Val: Value{Tag: ValueInt, Int: 1}})
		tmp := fc.newReg()
		fc.emit(f.Pos, Instr{Op: OpAddInt, A: tmp, B: idx, C: one})
		fc.emit(f.Pos, Instr{Op: OpMove, A: idx, B: tmp})
		fc.emit(f.Pos, Instr{Op: OpJump, A: loopStart})
		endPC := len(fc.fn.Code)
		fc.fn.Code[jmp].B = endPC
		for _, idx := range loop.breakJumps {
			fc.fn.Code[idx].A = endPC
		}
		fc.loops = fc.loops[:len(fc.loops)-1]
	}
	return nil
}

func (fc *funcCompiler) compileUpdate(u *parser.UpdateStmt) error {
	listReg, ok := fc.vars[u.Target]
	if !ok {
		listReg = fc.newReg()
	}
	length := fc.newReg()
	fc.emit(u.Pos, Instr{Op: OpLen, A: length, B: listReg})
	idx := fc.newReg()
	fc.emit(u.Pos, Instr{Op: OpConst, A: idx, Val: Value{Tag: ValueInt, Int: 0}})
	one := fc.constReg(u.Pos, Value{Tag: ValueInt, Int: 1})
	loopStart := len(fc.fn.Code)
	cond := fc.newReg()
	fc.emit(u.Pos, Instr{Op: OpLessInt, A: cond, B: idx, C: length})
	endJmp := len(fc.fn.Code)
	fc.emit(u.Pos, Instr{Op: OpJumpIfFalse, A: cond})

	item := fc.newReg()
	fc.emit(u.Pos, Instr{Op: OpIndex, A: item, B: listReg, C: idx})

	// load fields into scope
	fc.pushScope()
	if typ, err := fc.comp.env.GetVar(u.Target); err == nil {
		if lt, ok := typ.(types.ListType); ok {
			if st, ok := lt.Elem.(types.StructType); ok {
				for _, f := range st.Order {
					key := fc.constReg(u.Pos, Value{Tag: ValueStr, Str: f})
					val := fc.newReg()
					fc.emit(u.Pos, Instr{Op: OpIndex, A: val, B: item, C: key})
					fc.vars[f] = val
				}
			}
		}
	}

	skip := -1
	if u.Where != nil {
		condReg := fc.compileExpr(u.Where)
		skip = len(fc.fn.Code)
		fc.emit(u.Where.Pos, Instr{Op: OpJumpIfFalse, A: condReg})
	}

	for _, it := range u.Set.Items {
		keyStr, _ := identName(it.Key)
		key := fc.constReg(it.Pos, Value{Tag: ValueStr, Str: keyStr})
		val := fc.compileExpr(it.Value)
		fc.emit(it.Pos, Instr{Op: OpSetIndex, A: item, B: key, C: val})
	}

	if u.Where != nil {
		fc.fn.Code[skip].B = len(fc.fn.Code)
	}

	fc.popScope()

	fc.emit(u.Pos, Instr{Op: OpSetIndex, A: listReg, B: idx, C: item})
	tmp := fc.newReg()
	fc.emit(u.Pos, Instr{Op: OpAddInt, A: tmp, B: idx, C: one})
	fc.emit(u.Pos, Instr{Op: OpMove, A: idx, B: tmp})
	fc.emit(u.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[endJmp].B = end
	return nil
}

// compileQuery compiles a simple query expression supporting FROM clauses,
// optional WHERE filtering and SELECT projection. Only cross joins are
// handled and results are accumulated into a list.
func (fc *funcCompiler) compileQuery(q *parser.QueryExpr) int {
	// Detect simple aggregate SELECT like `sum(x)` without GROUP BY.
	var aggOp Op
	var aggPos lexer.Position
	if q.Group == nil {
		if op, arg, pos, ok := aggregateCall(q.Select); ok {
			aggOp = op
			aggPos = pos
			origSel := q.Select
			q.Select = arg
			defer func() { q.Select = origSel }()
		}
	}

	dst := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: dst, Val: Value{Tag: ValueList, List: []Value{}}})
	if q.Group != nil {
		if len(q.Froms) == 0 && len(q.Joins) == 0 {
			fc.compileGroupQuery(q, dst)
		} else {
			fc.compileGroupQueryAny(q, dst)
		}
	} else {
		switch {
		case len(q.Joins) == 1 && len(q.Froms) == 0:
			fc.compileJoinQuery(q, dst)
		case len(q.Joins) == 0:
			lvl := whereEvalLevel(q)
			fc.compileQueryFrom(q, dst, 0, lvl)
		default:
			fc.compileQueryFull(q, dst, 0)
		}
	}
	if q.Sort != nil {
		sorted := fc.newReg()
		fc.emit(q.Sort.Pos, Instr{Op: OpSort, A: sorted, B: dst})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: sorted})
	}
	if q.Skip != nil {
		start := fc.compileExpr(q.Skip)
		nul := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: nul, Val: Value{Tag: ValueNull}})
		out := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpSlice, A: out, B: dst, C: start, D: nul})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: out})
	}
	if q.Take != nil {
		zero := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: zero, Val: Value{Tag: ValueInt, Int: 0}})
		end := fc.compileExpr(q.Take)
		out := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpSlice, A: out, B: dst, C: zero, D: end})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: out})
	}
	if aggOp != 0 {
		out := fc.newReg()
		fc.emit(aggPos, Instr{Op: aggOp, A: out, B: dst})
		dst = out
	}
	return dst
}

func (fc *funcCompiler) compileQueryFull(q *parser.QueryExpr, dst int, level int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
		fc.preloadFieldConsts(q.Where)
		fc.preloadFieldConsts(q.Select)
		fc.preloadFieldConsts(q.Sort)
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	zero := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(src.Pos, Instr{Op: OpMove, A: idxReg, B: zero})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if level < len(q.Froms) {
		fc.compileQueryFull(q, dst, level+1)
	} else {
		fc.compileJoins(q, dst, 0)
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}

func (fc *funcCompiler) compileJoins(q *parser.QueryExpr, dst int, idx int) {
	if idx >= len(q.Joins) {
		appendVal := func() {
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				vtmp := fc.compileExpr(q.Select)
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: vtmp})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			} else {
				vtmp := fc.compileExpr(q.Select)
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: vtmp})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			}
		}
		if q.Where != nil {
			cond := fc.compileExpr(q.Where)
			skip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			appendVal()
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			appendVal()
		}
		return
	}

	join := q.Joins[idx]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	ri := fc.newReg()
	zero := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(join.Pos, Instr{Op: OpMove, A: ri, B: zero})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if joinType == "left" || joinType == "outer" {
		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileJoins(q, dst, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileJoins(q, dst, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAdd, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		fc.compileJoins(q, dst, idx+1)
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)
	} else {
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.compileJoins(q, dst, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.compileJoins(q, dst, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAdd, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end
	}
}

func (fc *funcCompiler) compileJoinQuery(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	if joinType == "inner" {
		if rl, ok := fc.constListLen(join.Src); ok && rl == 1 {
			fc.compileSingleRowRightJoin(q, dst)
			return
		}
		if ll, ok := fc.constListLen(q.Source); ok && ll == 1 {
			fc.compileSingleRowLeftJoin(q, dst)
			return
		}
	}

	if joinType == "inner" {
		if _, _, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			if !fc.smallConstJoin(q.Source, join.Src) {
				// Temporarily fall back to the nested loop join
				// implementation to avoid issues with the hash
				// join path on complex queries like TPC-H.
				// fc.compileHashJoin(q, dst, lk, rk)
				// return
			}
		}
	}

	if joinType == "left" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			if !fc.smallConstJoin(q.Source, join.Src) {
				fc.compileHashLeftJoin(q, dst, lk, rk)
				return
			}
		}
	}

	if joinType == "right" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			if !fc.smallConstJoin(q.Source, join.Src) {
				fc.compileHashRightJoin(q, dst, lk, rk)
				return
			}
		} else {
			fc.compileJoinQueryRight(q, dst)
			return
		}
	}

	if joinType == "outer" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			if !fc.smallConstJoin(q.Source, join.Src) {
				fc.compileHashOuterJoin(q, dst, lk, rk)
				return
			}
		} else {
			fc.compileJoinQueryRight(q, dst)
			return
		}
	}

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	// helper to append selected value
	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	// inner join results
	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	one2 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one2})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	// left unmatched rows
	if joinType == "left" || joinType == "outer" {
		li2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li2, Val: Value{Tag: ValueInt, Int: 0}})
		lstart2 := len(fc.fn.Code)
		lcond2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond2, B: li2, C: llen})
		ljmp2 := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond2})
		lelem2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem2, B: llist, C: li2})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem2})

		matched := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		ri2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri2, Val: Value{Tag: ValueInt, Int: 0}})
		rstart2 := len(fc.fn.Code)
		rcond2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond2, B: ri2, C: rlen})
		rjmp2 := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond2})
		relem2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem2, B: rlist, C: ri2})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem2})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		}
		one3 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri2, B: ri2, C: one3})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart2})
		rend2 := len(fc.fn.Code)
		fc.fn.Code[rjmp2].B = rend2

		check := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
		fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)

		one4 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li2, B: li2, C: one4})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart2})
		lend2 := len(fc.fn.Code)
		fc.fn.Code[ljmp2].B = lend2
	}

	if joinType == "right" || joinType == "outer" {
		ri3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri3, Val: Value{Tag: ValueInt, Int: 0}})
		rstart3 := len(fc.fn.Code)
		rcond3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond3, B: ri3, C: rlen})
		rjmp3 := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond3})
		relem3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem3, B: rlist, C: ri3})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem3})

		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		li3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li3, Val: Value{Tag: ValueInt, Int: 0}})
		lstart3 := len(fc.fn.Code)
		lcond3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond3, B: li3, C: llen})
		ljmp3 := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond3})
		lelem3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem3, B: llist, C: li3})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem3})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		}
		one5 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li3, B: li3, C: one5})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart3})
		lend3 := len(fc.fn.Code)
		fc.fn.Code[ljmp3].B = lend3

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)

		one6 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri3, B: ri3, C: one6})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart3})
		rend3 := len(fc.fn.Code)
		fc.fn.Code[rjmp3].B = rend3
	}
}

// compileHashJoin performs an inner join using a hash map when the ON clause
// is a simple equality between left and right expressions. Only inner joins are
// handled by this helper.
func (fc *funcCompiler) compileHashJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereLeft := ok && wa == q.Var
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	if ll, ok1 := fc.constListLen(q.Source); ok1 {
		if rl, ok2 := fc.constListLen(join.Src); ok2 {
			if rl <= ll {
				fc.compileHashJoinSide(q, dst, leftKey, rightKey, true, llist, llen, rlist, rlen, whereLeft, whereRight)
			} else {
				fc.compileHashJoinSide(q, dst, leftKey, rightKey, false, llist, llen, rlist, rlen, whereLeft, whereRight)
			}
			return
		}
	}

	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	emptyLeft := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpEqualInt, A: emptyLeft, B: llen, C: zero})
	jmpEmptyLeft := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: emptyLeft})
	emptyRight := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpEqualInt, A: emptyRight, B: rlen, C: zero})
	jmpEmptyRight := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: emptyRight})

	cond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessEq, A: cond, B: rlen, C: llen})
	jmpLeft := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond})

	// hash right side when it is smaller
	fc.compileHashJoinSide(q, dst, leftKey, rightKey, true, llist, llen, rlist, rlen, whereLeft, whereRight)
	jumpEnd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJump})
	fc.fn.Code[jmpLeft].B = len(fc.fn.Code)
	fc.fn.Code[jmpEmptyLeft].B = jumpEnd
	fc.fn.Code[jmpEmptyRight].B = jumpEnd

	// hash left side when it is smaller
	fc.compileHashJoinSide(q, dst, leftKey, rightKey, false, llist, llen, rlist, rlen, whereLeft, whereRight)

	fc.fn.Code[jumpEnd].B = len(fc.fn.Code)
}

func (fc *funcCompiler) compileHashJoinSide(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr, hashRight bool, llist, llen, rlist, rlen int, whereLeft, whereRight bool) {
	join := q.Joins[0]
	if hashRight {
		rmap := fc.newReg()
		cap := 0
		if n, ok := fc.constListLen(join.Src); ok {
			cap = n
		}
		fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: cap})

		ri := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
		rstart := len(fc.fn.Code)
		rcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
		rjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
		relem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
		rvar, ok := fc.vars[join.Var]
		if !ok {
			rvar = fc.newReg()
			fc.vars[join.Var] = rvar
		}
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
		var wskip int
		if whereRight {
			w := fc.compileExpr(q.Where)
			wskip = len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		}
		key := fc.compileExpr(rightKey)
		list := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		has := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
		skip := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
		newList := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
		fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
		fc.fn.Code[skip].B = len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
		tmp := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
		fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
		if whereRight {
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		}
		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		rend := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = rend

		appendSelect := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			} else {
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			}
		}

		li := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
		lstart := len(fc.fn.Code)
		lcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
		ljmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
		lelem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
		lvar, ok := fc.vars[q.Var]
		if !ok {
			lvar = fc.newReg()
			fc.vars[q.Var] = lvar
		}
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
		lkey := fc.compileExpr(leftKey)
		matches := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
		nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
		has2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
		skipMatches := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
		mlen := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
		mi := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
		mstart := len(fc.fn.Code)
		mcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
		mjmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
		melem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
		fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
		fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
		mend := len(fc.fn.Code)
		fc.fn.Code[mjmp].B = mend
		fc.fn.Code[skipMatches].B = len(fc.fn.Code)

		oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
		lend := len(fc.fn.Code)
		fc.fn.Code[ljmp].B = lend
	} else {
		lmap := fc.newReg()
		cap := 0
		if n, ok := fc.constListLen(q.Source); ok {
			cap = n
		}
		fc.emit(q.Pos, Instr{Op: OpMakeMap, A: lmap, B: cap})

		li := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
		lstart := len(fc.fn.Code)
		lcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
		ljmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
		lelem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
		lvar, ok := fc.vars[q.Var]
		if !ok {
			lvar = fc.newReg()
			fc.vars[q.Var] = lvar
		}
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
		var wskip int
		if whereLeft {
			w := fc.compileExpr(q.Where)
			wskip = len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		}
		key := fc.compileExpr(leftKey)
		list := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
		nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
		has := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
		skip := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has})
		newList := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
		fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: newList})
		fc.fn.Code[skip].B = len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: lelem})
		fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: tmp})
		if whereLeft {
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		}
		one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
		lend := len(fc.fn.Code)
		fc.fn.Code[ljmp].B = lend

		appendSelect := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			} else {
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			}
		}

		ri := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
		rstart := len(fc.fn.Code)
		rcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
		rjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
		relem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
		rvar, ok := fc.vars[join.Var]
		if !ok {
			rvar = fc.newReg()
			fc.vars[join.Var] = rvar
		}
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
		rkey := fc.compileExpr(rightKey)
		matches := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: matches, B: lmap, C: rkey})
		nil2 := fc.constReg(join.Pos, Value{Tag: ValueNull})
		has2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
		skipMatches := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: has2})
		mlen := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLen, A: mlen, B: matches})
		mi := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
		mstart := len(fc.fn.Code)
		mcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
		mjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
		melem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
		fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: melem})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		oneM := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
		fc.emit(join.Pos, Instr{Op: OpJump, A: mstart})
		mend := len(fc.fn.Code)
		fc.fn.Code[mjmp].B = mend
		fc.fn.Code[skipMatches].B = len(fc.fn.Code)

		oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		rend := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = rend
	}
}

// compileHashLeftJoin performs a left join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashLeftJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	rmap := fc.newReg()
	rcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		rcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: rcap})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	var wskip int
	if whereRight {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(rightKey)
	list := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	tmp := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
	if whereRight {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	lkey := fc.compileExpr(leftKey)
	matches := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
	nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	mlen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend
}

// compileHashRightJoin performs a right join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashRightJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereLeft := ok && wa == q.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	lmap := fc.newReg()
	lcap := 0
	if n, ok := fc.constListLen(q.Source); ok {
		lcap = n
	}
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: lmap, B: lcap})

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	var wskip int
	if whereLeft {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(leftKey)
	list := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
	nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
	tmp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: lelem})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: tmp})
	if whereLeft {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp2 := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
		} else {
			tmp2 := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	rkey := fc.compileExpr(rightKey)
	matches := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: matches, B: lmap, C: rkey})
	nil2 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	mlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(join.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend
}

func (fc *funcCompiler) compileJoinQueryRight(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})

	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	matched := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})

	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	check := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
	skipAdd := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	end := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = end
}

// compileHashOuterJoin performs a full outer join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashOuterJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	rmap := fc.newReg()
	rcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		rcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: rcap})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	var wskip int
	if whereRight {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(rightKey)
	list := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	tmp := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
	if whereRight {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	matched := fc.newReg()
	mcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		mcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: matched, B: mcap})
	trueReg := fc.constReg(join.Pos, Value{Tag: ValueBool, Bool: true})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	lkey := fc.compileExpr(leftKey)
	matches := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
	nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: matched, B: lkey, C: trueReg})
	mlen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	ri2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri2, Val: Value{Tag: ValueInt, Int: 0}})
	rstart2 := len(fc.fn.Code)
	rcond2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond2, B: ri2, C: rlen})
	rjmp2 := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond2})
	relem2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem2, B: rlist, C: ri2})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem2})
	rkey2 := fc.compileExpr(rightKey)
	hit := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: hit, B: matched, C: rkey2})
	nil4 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	hasHit := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: hasHit, B: hit, C: nil4})
	skipAdd2 := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: hasHit})
	nilL := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilL})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip3 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip3].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd2].B = len(fc.fn.Code)

	oneR2 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri2, B: ri2, C: oneR2})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart2})
	rend2 := len(fc.fn.Code)
	fc.fn.Code[rjmp2].B = rend2
}

// compileSingleRowRightJoin handles inner joins when the right side has exactly
// one element. The right row is evaluated once and reused while scanning the
// left side.
func (fc *funcCompiler) compileSingleRowRightJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	zero := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 0})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: zero})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	end := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = end
}

// compileSingleRowLeftJoin handles inner joins when the left side has exactly
// one element. The left row is evaluated once and reused while scanning the
// right side.
func (fc *funcCompiler) compileSingleRowLeftJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: zero})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	end := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = end
}

// compileGroupQuery handles simple queries with a single FROM clause and GROUP BY.
func (fc *funcCompiler) compileGroupQuery(q *parser.QueryExpr, dst int) {
	for _, g := range q.Group.Exprs {
		fc.preloadFieldConsts(g)
	}
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Group.Having)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	prevGroup := fc.groupVar
	fc.groupVar = q.Group.Name
	defer func() { fc.groupVar = prevGroup }()
	srcReg := fc.compileExpr(q.Source)
	listReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: idxReg, Val: Value{Tag: ValueInt, Int: 0}})

	groupsMap := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: groupsMap, B: 0})
	groupsList := fc.newReg()
	emptyList := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	fc.emit(q.Pos, Instr{Op: OpMove, A: groupsList, B: emptyList})

	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[q.Var]
	if !ok {
		varReg = fc.newReg()
		fc.vars[q.Var] = varReg
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if q.Where != nil {
		cond := fc.compileExpr(q.Where)
		skip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		fc.compileGroupAccum(q, elemReg, varReg, groupsMap, groupsList)
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		fc.compileGroupAccum(q, elemReg, varReg, groupsMap, groupsList)
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end

	// iterate groups and produce final results
	gi := fc.newReg()
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(q.Pos, Instr{Op: OpMove, A: gi, B: zero})
	glen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: glen, B: groupsList})
	loop2 := len(fc.fn.Code)
	cond2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: cond2, B: gi, C: glen})
	jmp2 := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond2})

	grp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp, B: groupsList, C: gi})
	gvar, ok := fc.vars[q.Group.Name]
	if !ok {
		gvar = fc.newReg()
		fc.vars[q.Group.Name] = gvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: gvar, B: grp})

	var skip int
	if q.Group.Having != nil {
		cond := fc.compileExpr(q.Group.Having)
		skip = len(fc.fn.Code)
		fc.emit(q.Group.Having.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	val := fc.compileExpr(q.Select)
	if q.Sort != nil {
		kreg := fc.newReg()
		vreg := fc.newReg()
		key := fc.compileExpr(q.Sort)
		fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
		fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
		pair := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
		tmpOut := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpOut, B: dst, C: pair})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmpOut})
	} else {
		tmpOut := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpOut, B: dst, C: val})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmpOut})
	}

	one2 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: gi, B: gi, C: one2})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loop2})
	end2 := len(fc.fn.Code)
	fc.fn.Code[jmp2].B = end2
	if q.Group.Having != nil {
		fc.fn.Code[skip].B = end2
	}
}

func (fc *funcCompiler) compileGroupAccum(q *parser.QueryExpr, elemReg, varReg, gmap, glist int) {
	exprs := q.Group.Exprs
	regs := make([]int, len(exprs))
	for i, e := range exprs {
		regs[i] = fc.compileExpr(e)
	}
	key := regs[0]
	var fieldNames []string
	if len(exprs) > 1 {
		fieldNames = make([]string, len(exprs))
		for i, e := range exprs {
			if name := extractFieldName(e); name != "" {
				fieldNames[i] = name
			} else {
				fieldNames[i] = fmt.Sprintf("k%d", i+1)
			}
		}
		pairs := make([]int, len(exprs)*2)
		for i, name := range fieldNames {
			k := fc.constReg(exprs[i].Pos, Value{Tag: ValueStr, Str: name})
			kr := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: kr, B: k})
			vr := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: vr, B: regs[i]})
			pairs[i*2] = kr
			pairs[i*2+1] = vr
		}
		key = fc.newReg()
		start := pairs[0]
		fc.emit(q.Pos, Instr{Op: OpMakeMap, A: key, B: len(exprs), C: start})
	}
	keyStr := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpStr, A: keyStr, B: key})
	exists := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpIn, A: exists, B: keyStr, C: gmap})
	jump := len(fc.fn.Code)
	fc.emit(q.Group.Pos, Instr{Op: OpJumpIfTrue, A: exists})

	items := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	k1 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "__group__"})
	v1 := fc.constReg(q.Pos, Value{Tag: ValueBool, Bool: true})
	k2 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "key"})
	v2 := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpMove, A: v2, B: key})
	k3 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "items"})
	v3 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMove, A: v3, B: items})
	kcnt := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "count"})
	vcnt := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	pairsGrp := []int{k1, v1, k2, v2, k3, v3, kcnt, vcnt}
	if len(fieldNames) > 0 {
		for i, name := range fieldNames {
			k := fc.freshConst(exprs[i].Pos, Value{Tag: ValueStr, Str: name})
			v := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: v, B: regs[i]})
			pairsGrp = append(pairsGrp, k, v)
		}
	}
	contig := make([]int, len(pairsGrp))
	for i, r := range pairsGrp {
		nr := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: nr, B: r})
		contig[i] = nr
	}
	grp := fc.newReg()
	startGrp := contig[0]
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: grp, B: len(contig) / 2, C: startGrp})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: gmap, B: keyStr, C: grp})
	tmpList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpList, B: glist, C: grp})
	fc.emit(q.Pos, Instr{Op: OpMove, A: glist, B: tmpList})

	end := len(fc.fn.Code)
	fc.fn.Code[jump].B = end

	itemsKey := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "items"})
	grp2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp2, B: gmap, C: keyStr})
	cur := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: cur, B: grp2, C: itemsKey})
	newList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: newList, B: cur, C: elemReg})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: grp2, B: itemsKey, C: newList})
	countKey := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "count"})
	curCnt := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: curCnt, B: grp2, C: countKey})
	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	newCnt := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: newCnt, B: curCnt, C: one})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: grp2, B: countKey, C: newCnt})
}

// compileGroupQueryAny handles GROUP BY queries that may include additional FROM
// clauses or JOINs. It builds row objects containing all bound variables which
// are accumulated into groups.
func (fc *funcCompiler) compileGroupQueryAny(q *parser.QueryExpr, dst int) {
	for _, g := range q.Group.Exprs {
		fc.preloadFieldConsts(g)
	}
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Group.Having)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	prevGroup := fc.groupVar
	fc.groupVar = q.Group.Name
	defer func() { fc.groupVar = prevGroup }()
	groupsMap := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: groupsMap, B: 0})
	groupsList := fc.newReg()
	emptyList := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	fc.emit(q.Pos, Instr{Op: OpMove, A: groupsList, B: emptyList})

	fc.compileGroupFromAny(q, groupsMap, groupsList, 0)

	// iterate groups and produce final results
	gi := fc.newReg()
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(q.Pos, Instr{Op: OpMove, A: gi, B: zero})
	glen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: glen, B: groupsList})
	loop := len(fc.fn.Code)
	cond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: cond, B: gi, C: glen})
	jmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond})

	grp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp, B: groupsList, C: gi})
	gvar, ok := fc.vars[q.Group.Name]
	if !ok {
		gvar = fc.newReg()
		fc.vars[q.Group.Name] = gvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: gvar, B: grp})

	var skip int
	if q.Group.Having != nil {
		cond := fc.compileExpr(q.Group.Having)
		skip = len(fc.fn.Code)
		fc.emit(q.Group.Having.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	val := fc.compileExpr(q.Select)
	if q.Sort != nil {
		kreg := fc.newReg()
		vreg := fc.newReg()
		key := fc.compileExpr(q.Sort)
		fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
		fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
		pair := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
	} else {
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: gi, B: gi, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loop})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
	if q.Group.Having != nil {
		fc.fn.Code[skip].B = end
	}
}

func (fc *funcCompiler) compileGroupFromAny(q *parser.QueryExpr, gmap, glist int, level int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpConst, A: idxReg, Val: Value{Tag: ValueInt, Int: 0}})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if level < len(q.Froms) {
		fc.pushScope()
		fc.compileGroupFromAny(q, gmap, glist, level+1)
		fc.popScope()
	} else {
		fc.pushScope()
		fc.compileGroupJoinAny(q, gmap, glist, 0)
		fc.popScope()
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}

func (fc *funcCompiler) compileGroupJoinAny(q *parser.QueryExpr, gmap, glist int, idx int) {
	if idx >= len(q.Joins) {
		doAccum := func() {
			row := fc.buildRowMap(q)
			vreg, ok := fc.vars[q.Var]
			if !ok {
				vreg = fc.newReg()
				fc.vars[q.Var] = vreg
			}
			fc.compileGroupAccum(q, row, vreg, gmap, glist)
		}
		if q.Where != nil {
			cond := fc.compileExpr(q.Where)
			skip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			doAccum()
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			doAccum()
		}
		return
	}

	join := q.Joins[idx]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})
	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if joinType == "left" || joinType == "outer" {
		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)
	} else {
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end
	}
}

func (fc *funcCompiler) buildRowMap(q *parser.QueryExpr) int {
	names := []string{q.Var}
	regs := []int{fc.vars[q.Var]}
	for _, f := range q.Froms {
		names = append(names, f.Var)
		regs = append(regs, fc.vars[f.Var])
	}
	for _, j := range q.Joins {
		names = append(names, j.Var)
		regs = append(regs, fc.vars[j.Var])
	}

	pairs := []int{}
	addPair := func(k, v int) {
		pairs = append(pairs, k, v)
	}
	var addStructFields func(reg int, st types.StructType)
	addStructFields = func(reg int, st types.StructType) {
		for _, field := range st.Order {
			fk := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: field})
			fv := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpIndex, A: fv, B: reg, C: fk})
			addPair(fk, fv)
			if ft, ok := st.Fields[field].(types.StructType); ok {
				addStructFields(fv, ft)
			}
		}
	}
	for i, n := range names {
		k := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: n})
		v := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: v, B: regs[i]})
		addPair(k, v)

		if typ, err := fc.comp.env.GetVar(n); err == nil {
			if st, ok := typ.(types.StructType); ok {
				addStructFields(regs[i], st)
			}
		}
	}
	contig := make([]int, len(pairs))
	for i, r := range pairs {
		nr := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: nr, B: r})
		contig[i] = nr
	}
	row := fc.newReg()
	start := 0
	if len(contig) > 0 {
		start = contig[0]
	}
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: row, B: len(pairs) / 2, C: start})
	return row
}

// compileQueryFrom recursively emits nested loops for each FROM clause.
func (fc *funcCompiler) compileQueryFrom(q *parser.QueryExpr, dst int, level int, whereLevel int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	if level == 0 {
		fc.preloadFieldConsts(q.Where)
		fc.preloadFieldConsts(q.Select)
		fc.preloadFieldConsts(q.Sort)
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lengthReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lengthReg, B: listReg})
	idxReg := fc.newReg()
	zero := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(src.Pos, Instr{Op: OpMove, A: idxReg, B: zero})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lengthReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	skip := -1
	outerCheck := level == whereLevel && level < len(q.Froms) && q.Where != nil
	if outerCheck {
		cond := fc.compileExpr(q.Where)
		skip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	if level < len(q.Froms) {
		fc.pushScope()
		fc.compileQueryFrom(q, dst, level+1, whereLevel)
		fc.popScope()
	} else {
		fc.pushScope()
		appendVal := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			} else {
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			}
		}
		if q.Where != nil && level == whereLevel {
			cond := fc.compileExpr(q.Where)
			innerSkip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			appendVal()
			fc.fn.Code[innerSkip].B = len(fc.fn.Code)
		} else {
			appendVal()
		}
		fc.popScope()
	}

	if skip != -1 {
		fc.fn.Code[skip].B = len(fc.fn.Code)
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}

func (fc *funcCompiler) compileWhile(w *parser.WhileStmt) error {
	loopStart := len(fc.fn.Code)
	cond := fc.compileExpr(w.Cond)
	jmp := len(fc.fn.Code)
	fc.emit(w.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	loop := &loopContext{continueTarget: loopStart}
	fc.loops = append(fc.loops, loop)
	fc.pushScope()
	for _, st := range w.Body {
		if err := fc.compileStmt(st); err != nil {
			fc.popScope()
			return err
		}
	}
	fc.popScope()
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
	return nil
}

func (fc *funcCompiler) compileIf(s *parser.IfStmt) error {
	cond := fc.compileExpr(s.Cond)
	jmpFalse := len(fc.fn.Code)
	fc.emit(s.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	fc.pushScope()
	for _, st := range s.Then {
		if err := fc.compileStmt(st); err != nil {
			fc.popScope()
			return err
		}
	}
	fc.popScope()
	endJump := -1
	if len(s.Else) > 0 {
		endJump = len(fc.fn.Code)
		fc.emit(s.Pos, Instr{Op: OpJump})
	}
	fc.fn.Code[jmpFalse].B = len(fc.fn.Code)
	if len(s.Else) > 0 {
		fc.pushScope()
		for _, st := range s.Else {
			if err := fc.compileStmt(st); err != nil {
				fc.popScope()
				return err
			}
		}
		fc.popScope()
		fc.fn.Code[endJump].A = len(fc.fn.Code)
	}
	return nil
}

func (fc *funcCompiler) compileIfExpr(e *parser.IfExpr) int {
	cond := fc.compileExpr(e.Cond)
	thenReg := fc.compileExpr(e.Then)
	var elseReg int
	if e.ElseIf != nil {
		elseReg = fc.compileIfExpr(e.ElseIf)
	} else if e.Else != nil {
		elseReg = fc.compileExpr(e.Else)
	} else {
		elseReg = fc.constReg(lexer.Position{}, Value{Tag: ValueNull})
	}
	dst := fc.newReg()
	fc.emit(e.Pos, Instr{Op: OpSelect, A: dst, B: cond, C: thenReg, D: elseReg})
	return dst
}

func (fc *funcCompiler) compileMatch(m *parser.MatchExpr) int {
	target := fc.compileExpr(m.Target)
	dst := fc.newReg()
	var endJumps []int
	for _, c := range m.Cases {
		fc.pushScope()
		var cond int
		jmp := -1
		if !isUnderscoreExpr(c.Pattern) {
			cond = fc.newReg()
			if call, ok := callPattern(c.Pattern); ok {
				nameKey := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpConst, A: nameKey, Val: Value{Tag: ValueStr, Str: "__name"}})
				nameVal := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpIndex, A: nameVal, B: target, C: nameKey})
				want := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpConst, A: want, Val: Value{Tag: ValueStr, Str: call.Func}})
				fc.emit(c.Pos, Instr{Op: OpEqual, A: cond, B: nameVal, C: want})
			} else if ident, ok := identName(c.Pattern); ok {
				nameKey := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpConst, A: nameKey, Val: Value{Tag: ValueStr, Str: "__name"}})
				nameVal := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpIndex, A: nameVal, B: target, C: nameKey})
				want := fc.newReg()
				fc.emit(c.Pos, Instr{Op: OpConst, A: want, Val: Value{Tag: ValueStr, Str: ident}})
				fc.emit(c.Pos, Instr{Op: OpEqual, A: cond, B: nameVal, C: want})
			} else {
				pv := fc.compileExpr(c.Pattern)
				fc.emit(c.Pos, Instr{Op: OpEqual, A: cond, B: target, C: pv})
			}
			jmp = len(fc.fn.Code)
			fc.emit(c.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		}

		if call, ok := callPattern(c.Pattern); ok {
			if st, ok := fc.comp.env.GetStruct(call.Func); ok {
				for idx, arg := range call.Args {
					if name, ok := identName(arg); ok {
						key := fc.newReg()
						fc.emit(c.Pos, Instr{Op: OpConst, A: key, Val: Value{Tag: ValueStr, Str: st.Order[idx]}})
						val := fc.newReg()
						fc.emit(c.Pos, Instr{Op: OpIndex, A: val, B: target, C: key})
						fc.vars[name] = val
					}
				}
			}
		}

		res := fc.compileExpr(c.Result)
		fc.emit(c.Pos, Instr{Op: OpMove, A: dst, B: res})
		fc.popScope()
		endJumps = append(endJumps, len(fc.fn.Code))
		fc.emit(c.Pos, Instr{Op: OpJump})
		if jmp != -1 {
			fc.fn.Code[jmp].B = len(fc.fn.Code)
		}
	}
	fc.emit(lexer.Position{}, Instr{Op: OpConst, A: dst, Val: Value{Tag: ValueNull}})
	end := len(fc.fn.Code)
	for _, j := range endJumps {
		fc.fn.Code[j].A = end
	}
	return dst
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
	return Value{Tag: ValueList, List: vals}, true
}

func constMap(m *parser.MapLiteral) (Value, bool) {
	vals := make(map[string]Value, len(m.Items))
	for _, it := range m.Items {
		var key string
		if name, ok := identName(it.Key); ok {
			key = name
		} else {
			k, ok := constExpr(it.Key)
			if !ok {
				return Value{}, false
			}
			switch k.Tag {
			case ValueStr:
				key = k.Str
			case ValueInt:
				key = fmt.Sprintf("%d", k.Int)
			default:
				return Value{}, false
			}
		}
		v, ok := constExpr(it.Value)
		if !ok {
			return Value{}, false
		}
		vals[key] = v
	}
	return Value{Tag: ValueMap, Map: vals}, true
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
		if ok {
			switch v.Tag {
			case ValueInt:
				v.Int = -v.Int
				return v, true
			case ValueFloat:
				v.Float = -v.Float
				return v, true
			}
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
			return Value{Tag: ValueInt, Int: *p.Lit.Int}, true
		}
		if p.Lit.Float != nil {
			return Value{Tag: ValueFloat, Float: *p.Lit.Float}, true
		}
		if p.Lit.Bool != nil {
			return Value{Tag: ValueBool, Bool: bool(*p.Lit.Bool)}, true
		}
		if p.Lit.Str != nil {
			return Value{Tag: ValueStr, Str: *p.Lit.Str}, true
		}
		if p.Lit.Null {
			return Value{Tag: ValueNull}, true
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

func literalToValue(l *parser.Literal) (Value, bool) {
	switch {
	case l.Int != nil:
		return Value{Tag: ValueInt, Int: *l.Int}, true
	case l.Float != nil:
		return Value{Tag: ValueFloat, Float: *l.Float}, true
	case l.Str != nil:
		return Value{Tag: ValueStr, Str: *l.Str}, true
	case l.Bool != nil:
		return Value{Tag: ValueBool, Bool: bool(*l.Bool)}, true
	case l.Null:
		return Value{Tag: ValueNull}, true
	default:
		return Value{}, false
	}
}

func valueToExpr(v Value) *parser.Expr {
	anyVal := v.ToAny()
	if lit := types.AnyToLiteral(anyVal); lit != nil {
		return &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: lit}}}}}
	}
	if v.Tag == ValueNull {
		return &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: &parser.Literal{Null: true}}}}}}
	}
	return nil
}

func extractLiteral(e *parser.Expr) *parser.Literal {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func extractFieldName(e *parser.Expr) string {
	if e == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil {
		return ""
	}
	sel := p.Target.Selector
	if len(sel.Tail) > 0 {
		return sel.Tail[len(sel.Tail)-1]
	}
	return sel.Root
}

// preloadFieldConsts emits Const instructions for all selector field names
// referenced within the expression. This allows subsequent uses of the same
// fields inside loops to reuse those constant registers without re-emitting the
// Const each iteration.
func (fc *funcCompiler) preloadFieldConsts(e *parser.Expr) {
	var walkExpr func(*parser.Expr)
	var walkUnary func(*parser.Unary)
	var walkPostfix func(*parser.PostfixExpr)
	var walkPrimary func(*parser.Primary)

	walkExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		walkUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			walkPostfix(op.Right)
		}
	}

	walkUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		walkPostfix(u.Value)
	}

	walkPostfix = func(pf *parser.PostfixExpr) {
		if pf == nil {
			return
		}
		walkPrimary(pf.Target)
		for _, op := range pf.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					walkExpr(a)
				}
			}
			if op.Index != nil {
				walkExpr(op.Index.Start)
				walkExpr(op.Index.End)
				walkExpr(op.Index.Step)
			}
			if op.Field != nil {
				fc.constReg(op.Field.Pos, Value{Tag: ValueStr, Str: op.Field.Name})
			}
		}
	}

	walkPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Selector != nil:
			for _, f := range p.Selector.Tail {
				fc.constReg(p.Pos, Value{Tag: ValueStr, Str: f})
			}
		case p.Struct != nil:
			for _, f := range p.Struct.Fields {
				fc.constReg(f.Pos, Value{Tag: ValueStr, Str: f.Name})
				walkExpr(f.Value)
			}
		case p.List != nil:
			for _, el := range p.List.Elems {
				walkExpr(el)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				if name, ok := identName(it.Key); ok {
					fc.constReg(it.Pos, Value{Tag: ValueStr, Str: name})
				} else {
					walkExpr(it.Key)
				}
				walkExpr(it.Value)
			}
		case p.Call != nil:
			for _, a := range p.Call.Args {
				walkExpr(a)
			}
		case p.Query != nil:
			walkExpr(p.Query.Source)
			for _, f := range p.Query.Froms {
				walkExpr(f.Src)
			}
			for _, j := range p.Query.Joins {
				walkExpr(j.Src)
				walkExpr(j.On)
			}
			walkExpr(p.Query.Where)
			if p.Query.Group != nil {
				for _, g := range p.Query.Group.Exprs {
					walkExpr(g)
				}
			}
			walkExpr(p.Query.Sort)
			walkExpr(p.Query.Skip)
			walkExpr(p.Query.Take)
			walkExpr(p.Query.Select)
		case p.If != nil:
			walkExpr(p.If.Cond)
			walkExpr(p.If.Then)
			if p.If.ElseIf != nil {
				walkExpr(p.If.ElseIf.Cond)
				walkExpr(p.If.ElseIf.Then)
				if p.If.ElseIf.Else != nil {
					walkExpr(p.If.ElseIf.Else)
				}
			}
			if p.If.Else != nil {
				walkExpr(p.If.Else)
			}
		case p.Match != nil:
			walkExpr(p.Match.Target)
			for _, cs := range p.Match.Cases {
				walkExpr(cs.Pattern)
				walkExpr(cs.Result)
			}
		case p.Generate != nil:
			for _, f := range p.Generate.Fields {
				walkExpr(f.Value)
			}
		case p.Fetch != nil:
			walkExpr(p.Fetch.URL)
			if p.Fetch.With != nil {
				walkExpr(p.Fetch.With)
			}
		case p.Load != nil:
			if p.Load.With != nil {
				walkExpr(p.Load.With)
			}
		case p.Save != nil:
			walkExpr(p.Save.Src)
			if p.Save.With != nil {
				walkExpr(p.Save.With)
			}
		}
		if p.Group != nil {
			walkExpr(p.Group)
		}
	}

	walkExpr(e)
}

func (fc *funcCompiler) foldCallValue(call *parser.CallExpr) (Value, bool) {
	// Gather constant argument values.
	args := make([]Value, len(call.Args))
	for i, a := range call.Args {
		if lit := extractLiteral(a); lit != nil {
			if v, ok := literalToValue(lit); ok {
				args[i] = v
				continue
			}
		}
		if name, ok := identName(a); ok {
			if val, err := fc.comp.env.GetValue(name); err == nil {
				args[i] = FromAny(val)
				continue
			}
		}
		if v, ok := fc.evalConstExpr(a); ok {
			args[i] = v
			continue
		}
		return Value{}, false
	}

	switch call.Func {
	case "len":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		switch v.Tag {
		case ValueList:
			return Value{Tag: ValueInt, Int: len(v.List)}, true
		case ValueStr:
			return Value{Tag: ValueInt, Int: len([]rune(v.Str))}, true
		case ValueMap:
			return Value{Tag: ValueInt, Int: len(v.Map)}, true
		case ValueNull:
			return Value{Tag: ValueInt, Int: 0}, true
		}
	case "cap":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		switch v.Tag {
		case ValueList:
			return Value{Tag: ValueInt, Int: cap(v.List)}, true
		case ValueNull:
			return Value{Tag: ValueInt, Int: 0}, true
		}
	case "str":
		if len(args) != 1 {
			return Value{}, false
		}
		return Value{Tag: ValueStr, Str: fmt.Sprint(args[0].ToAny())}, true
	case "lower":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if v.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: strings.ToLower(v.Str)}, true
		}
		return Value{Tag: ValueStr, Str: strings.ToLower(fmt.Sprint(v.ToAny()))}, true
	case "upper":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if v.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: strings.ToUpper(v.Str)}, true
		}
		return Value{Tag: ValueStr, Str: strings.ToUpper(fmt.Sprint(v.ToAny()))}, true
	case "reverse":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if v.Tag == ValueStr {
			r := []rune(v.Str)
			for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {
				r[i], r[j] = r[j], r[i]
			}
			return Value{Tag: ValueStr, Str: string(r)}, true
		}
		if lst, ok := toList(v); ok {
			out := make([]Value, len(lst))
			copy(out, lst)
			for i, j := 0, len(out)-1; i < j; i, j = i+1, j-1 {
				out[i], out[j] = out[j], out[i]
			}
			return Value{Tag: ValueList, List: out}, true
		}
		return Value{}, false
	case "substring":
		if len(args) != 3 {
			return Value{}, false
		}
		s, start, end := args[0], args[1], args[2]
		if s.Tag == ValueStr && start.Tag == ValueInt && end.Tag == ValueInt {
			r := []rune(s.Str)
			lo, hi := clampSlice(len(r), start.Int, end.Int)
			return Value{Tag: ValueStr, Str: string(r[lo:hi])}, true
		}
	case "substr":
		if len(args) != 3 {
			return Value{}, false
		}
		s, start, end := args[0], args[1], args[2]
		if s.Tag == ValueStr && start.Tag == ValueInt && end.Tag == ValueInt {
			r := []rune(s.Str)
			lo, hi := clampSlice(len(r), start.Int, end.Int)
			return Value{Tag: ValueStr, Str: string(r[lo:hi])}, true
		}
	case "num":
		if len(args) != 1 {
			return Value{}, false
		}
		r := toRat(args[0])
		return Value{Tag: ValueBigInt, BigInt: new(big.Int).Set(r.Num())}, true
	case "denom":
		if len(args) != 1 {
			return Value{}, false
		}
		r := toRat(args[0])
		return Value{Tag: ValueBigInt, BigInt: new(big.Int).Set(r.Denom())}, true
	case "count":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			return Value{Tag: ValueInt, Int: len(lst)}, true
		}
		switch v.Tag {
		case ValueMap:
			return Value{Tag: ValueInt, Int: len(v.Map)}, true
		case ValueStr:
			return Value{Tag: ValueInt, Int: len([]rune(v.Str))}, true
		}
	case "exists":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			return Value{Tag: ValueBool, Bool: len(lst) > 0}, true
		}
		switch v.Tag {
		case ValueList:
			return Value{Tag: ValueBool, Bool: len(v.List) > 0}, true
		case ValueMap:
			return Value{Tag: ValueBool, Bool: len(v.Map) > 0}, true
		case ValueStr:
			return Value{Tag: ValueBool, Bool: len(v.Str) > 0}, true
		}
	case "avg":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			var sum float64
			for _, it := range lst {
				sum += toFloat(it)
			}
			return Value{Tag: ValueFloat, Float: sum / float64(len(lst))}, true
		}
	case "sum":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			var sum float64
			for _, it := range lst {
				sum += toFloat(it)
			}
			return Value{Tag: ValueFloat, Float: sum}, true
		}
	case "min":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			if lst[0].Tag == ValueStr {
				minStr := lst[0].Str
				for _, it := range lst[1:] {
					if it.Tag == ValueStr && it.Str < minStr {
						minStr = it.Str
					}
				}
				return Value{Tag: ValueStr, Str: minStr}, true
			}
			minVal := toFloat(lst[0])
			isFloat := lst[0].Tag == ValueFloat
			for _, it := range lst[1:] {
				if it.Tag == ValueFloat {
					isFloat = true
				}
				f := toFloat(it)
				if f < minVal {
					minVal = f
				}
			}
			if isFloat {
				return Value{Tag: ValueFloat, Float: minVal}, true
			}
			return Value{Tag: ValueInt, Int: int(minVal)}, true
		}
	case "max":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			if lst[0].Tag == ValueStr {
				maxStr := lst[0].Str
				for _, it := range lst[1:] {
					if it.Tag == ValueStr && it.Str > maxStr {
						maxStr = it.Str
					}
				}
				return Value{Tag: ValueStr, Str: maxStr}, true
			}
			maxVal := toFloat(lst[0])
			isFloat := lst[0].Tag == ValueFloat
			for _, it := range lst[1:] {
				if it.Tag == ValueFloat {
					isFloat = true
				}
				f := toFloat(it)
				if f > maxVal {
					maxVal = f
				}
			}
			if isFloat {
				return Value{Tag: ValueFloat, Float: maxVal}, true
			}
			return Value{Tag: ValueInt, Int: int(maxVal)}, true
		}
	case "concat":
		out := []Value{}
		for _, v := range args {
			lst, ok := toList(v)
			if !ok {
				return Value{}, false
			}
			out = append(out, lst...)
		}
		return Value{Tag: ValueList, List: out}, true
	case "values":
		if len(args) != 1 {
			return Value{}, false
		}
		v := args[0]
		if v.Tag == ValueMap {
			keys := make([]string, 0, len(v.Map))
			for k := range v.Map {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			vals := make([]Value, len(keys))
			for i, k := range keys {
				vals[i] = v.Map[k]
			}
			return Value{Tag: ValueList, List: vals}, true
		}
	}

	// Fold calls to user-defined pure functions.
	if t, err := fc.comp.env.GetVar(call.Func); err == nil {
		if ft, ok := t.(types.FuncType); ok && ft.Pure {
			if val, ok2 := fc.evalPureFunc(call.Func, args); ok2 {
				return val, true
			}
		}
	}
	return Value{}, false
}

// evalPureFunc evaluates a pure function defined in the current program using
// the VM's constant evaluator. Only very simple functions with a single return
// statement are supported. All arguments must already be constant values.
func (fc *funcCompiler) evalPureFunc(name string, args []Value) (Value, bool) {
	fn, ok := fc.comp.env.GetFunc(name)
	if !ok {
		return Value{}, false
	}
	if len(args) < len(fn.Params) || len(fn.Body) != 1 {
		return Value{}, false
	}
	if len(args) > len(fn.Params) {
		args = args[:len(fn.Params)]
	}
	ret := fn.Body[0].Return
	if ret == nil {
		return Value{}, false
	}
	env := fc.comp.env.Copy()
	for i, p := range fn.Params {
		env.SetValue(p.Name, args[i].ToAny(), false)
	}
	tmpComp := &compiler{prog: fc.comp.prog, env: env}
	tmpFC := &funcCompiler{comp: tmpComp, constRegs: map[string]int{}}
	return tmpFC.evalConstExpr(ret.Value)
}

func (fc *funcCompiler) evalConstExpr(e *parser.Expr) (Value, bool) {
	if e == nil {
		return Value{}, false
	}
	if lit := extractLiteral(e); lit != nil {
		return literalToValue(lit)
	}
	if call, ok := callPattern(e); ok {
		if v, ok := fc.foldCallValue(call); ok {
			return v, true
		}
	}
	return fc.evalConstBinary(e.Binary)
}

func (fc *funcCompiler) evalConstBinary(b *parser.BinaryExpr) (Value, bool) {
	if b == nil {
		return Value{}, false
	}
	type operand struct {
		val Value
		ok  bool
	}
	operands := []operand{}
	left, ok := fc.evalConstUnary(b.Left)
	if !ok {
		return Value{}, false
	}
	operands = append(operands, operand{left, true})
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, part := range b.Right {
		v, ok := fc.evalConstPostfix(part.Right)
		if !ok {
			return Value{}, false
		}
		operands = append(operands, operand{v, true})
		ops[i] = part
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			op := ops[i].Op
			if op == "union" && ops[i].All {
				op = "union_all"
			}
			if contains(level, op) {
				leftVal := operands[i].val
				rightVal := operands[i+1].val
				v, ok := applyBinaryConst(op, leftVal, rightVal)
				if !ok {
					return Value{}, false
				}
				operands[i] = operand{v, true}
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return Value{}, false
	}
	return operands[0].val, true
}

func (fc *funcCompiler) evalConstUnary(u *parser.Unary) (Value, bool) {
	v, ok := fc.evalConstPostfix(u.Value)
	if !ok {
		return Value{}, false
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		if nv, ok := applyUnaryConst(u.Ops[i], v); ok {
			v = nv
		} else {
			return Value{}, false
		}
	}
	return v, true
}

func (fc *funcCompiler) evalConstPostfix(p *parser.PostfixExpr) (Value, bool) {
	v, ok := fc.evalConstPrimary(p.Target)
	if !ok {
		return Value{}, false
	}
	for _, op := range p.Ops {
		switch {
		case op.Cast != nil:
			typ := resolveTypeRef(op.Cast.Type, fc.comp.env)
			cv, err := castValue(typ, v.ToAny())
			if err != nil {
				return Value{}, false
			}
			v = FromAny(cv)
		case op.Call != nil || op.Index != nil || op.Field != nil:
			return Value{}, false
		}
	}
	return v, true
}

func (fc *funcCompiler) evalConstPrimary(p *parser.Primary) (Value, bool) {
	switch {
	case p.Lit != nil:
		return literalToValue(p.Lit)
	case p.Group != nil:
		return fc.evalConstExpr(p.Group)
	case p.List != nil:
		vals := make([]Value, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, ok := fc.evalConstExpr(e)
			if !ok {
				return Value{}, false
			}
			vals[i] = v
		}
		return Value{Tag: ValueList, List: vals}, true
	case p.Map != nil:
		vals := make(map[string]Value, len(p.Map.Items))
		for _, it := range p.Map.Items {
			keyVal, ok := fc.evalConstExpr(it.Key)
			if !ok {
				return Value{}, false
			}
			var key string
			switch keyVal.Tag {
			case ValueStr:
				key = keyVal.Str
			case ValueInt:
				key = fmt.Sprintf("%d", keyVal.Int)
			default:
				return Value{}, false
			}
			v, ok := fc.evalConstExpr(it.Value)
			if !ok {
				return Value{}, false
			}
			vals[key] = v
		}
		return Value{Tag: ValueMap, Map: vals}, true
	case p.Call != nil:
		return fc.foldCallValue(p.Call)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			if val, err := fc.comp.env.GetValue(p.Selector.Root); err == nil {
				return FromAny(val), true
			}
		}
		return Value{}, false
	default:
		return Value{}, false
	}
}

func applyUnaryConst(op string, v Value) (Value, bool) {
	switch op {
	case "-":
		switch v.Tag {
		case ValueInt:
			return Value{Tag: ValueInt, Int: -v.Int}, true
		case ValueBigInt:
			bi := new(big.Int).Neg(toBigInt(v))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		case ValueFloat:
			return Value{Tag: ValueFloat, Float: -v.Float}, true
		default:
			return Value{}, false
		}
	case "!":
		if v.Tag == ValueBool {
			return Value{Tag: ValueBool, Bool: !v.Bool}, true
		}
		return Value{}, false
	default:
		return Value{}, false
	}
}

func applyBinaryConst(op string, a, b Value) (Value, bool) {
	switch op {
	case "+":
		if a.Tag == ValueStr && b.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: a.Str + b.Str}, true
		}
		if a.Tag == ValueFloat || b.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: toFloat(a) + toFloat(b)}, true
		}
		if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
			bi := new(big.Int).Add(toBigInt(a), toBigInt(b))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		}
		if a.Tag == ValueInt && b.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: a.Int + b.Int}, true
		}
	case "-":
		if a.Tag == ValueFloat || b.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: toFloat(a) - toFloat(b)}, true
		}
		if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
			bi := new(big.Int).Sub(toBigInt(a), toBigInt(b))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		}
		if a.Tag == ValueInt && b.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: a.Int - b.Int}, true
		}
	case "*":
		if a.Tag == ValueFloat || b.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: toFloat(a) * toFloat(b)}, true
		}
		if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
			bi := new(big.Int).Mul(toBigInt(a), toBigInt(b))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		}
		if a.Tag == ValueInt && b.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: a.Int * b.Int}, true
		}
	case "/":
		if (b.Tag == ValueInt && b.Int == 0) || (b.Tag == ValueFloat && b.Float == 0) {
			return Value{}, false
		}
		if a.Tag == ValueFloat || b.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: toFloat(a) / toFloat(b)}, true
		}
		if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
			if toBigInt(b).Sign() == 0 {
				return Value{}, false
			}
			bi := new(big.Int).Quo(toBigInt(a), toBigInt(b))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		}
		if a.Tag == ValueInt && b.Tag == ValueInt {
			return Value{Tag: ValueFloat, Float: float64(a.Int) / float64(b.Int)}, true
		}
	case "%":
		if (b.Tag == ValueInt && b.Int == 0) || (b.Tag == ValueFloat && b.Float == 0) {
			return Value{}, false
		}
		if a.Tag == ValueFloat || b.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: math.Mod(toFloat(a), toFloat(b))}, true
		}
		if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
			if toBigInt(b).Sign() == 0 {
				return Value{}, false
			}
			bi := new(big.Int).Rem(toBigInt(a), toBigInt(b))
			return Value{Tag: ValueBigInt, BigInt: bi}, true
		}
		if a.Tag == ValueInt && b.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: a.Int % b.Int}, true
		}
	case "==":
		return Value{Tag: ValueBool, Bool: valuesEqual(a, b)}, true
	case "!=":
		return Value{Tag: ValueBool, Bool: !valuesEqual(a, b)}, true
	case "<":
		return Value{Tag: ValueBool, Bool: valueLess(a, b)}, true
	case "<=":
		return Value{Tag: ValueBool, Bool: valueLess(a, b) || valuesEqual(a, b)}, true
	case ">":
		return Value{Tag: ValueBool, Bool: valueLess(b, a)}, true
	case ">=":
		return Value{Tag: ValueBool, Bool: valueLess(b, a) || valuesEqual(a, b)}, true
	case "&&":
		if a.Tag == ValueBool && b.Tag == ValueBool {
			return Value{Tag: ValueBool, Bool: a.Bool && b.Bool}, true
		}
	case "||":
		if a.Tag == ValueBool && b.Tag == ValueBool {
			return Value{Tag: ValueBool, Bool: a.Bool || b.Bool}, true
		}
	case "in":
		switch b.Tag {
		case ValueList:
			for _, item := range b.List {
				if valuesEqual(item, a) {
					return Value{Tag: ValueBool, Bool: true}, true
				}
			}
			return Value{Tag: ValueBool, Bool: false}, true
		case ValueMap:
			key := fmt.Sprint(a.ToAny())
			_, ok := b.Map[key]
			return Value{Tag: ValueBool, Bool: ok}, true
		case ValueStr:
			if a.Tag == ValueStr {
				return Value{Tag: ValueBool, Bool: strings.Contains(b.Str, a.Str)}, true
			}
		}
	case "union_all":
		if a.Tag == ValueList && b.Tag == ValueList {
			out := append(append([]Value{}, a.List...), b.List...)
			return Value{Tag: ValueList, List: out}, true
		}
	case "union":
		if a.Tag == ValueList && b.Tag == ValueList {
			merged := append([]Value{}, a.List...)
			for _, rv := range b.List {
				exists := false
				for _, lv := range merged {
					if valuesEqual(lv, rv) {
						exists = true
						break
					}
				}
				if !exists {
					merged = append(merged, rv)
				}
			}
			return Value{Tag: ValueList, List: merged}, true
		}
	case "except":
		if a.Tag == ValueList && b.Tag == ValueList {
			diff := []Value{}
			for _, lv := range a.List {
				found := false
				for _, rv := range b.List {
					if valuesEqual(lv, rv) {
						found = true
						break
					}
				}
				if !found {
					diff = append(diff, lv)
				}
			}
			return Value{Tag: ValueList, List: diff}, true
		}
	case "intersect":
		if a.Tag == ValueList && b.Tag == ValueList {
			inter := []Value{}
			for _, lv := range a.List {
				for _, rv := range b.List {
					if valuesEqual(lv, rv) {
						exists := false
						for _, iv := range inter {
							if valuesEqual(iv, lv) {
								exists = true
								break
							}
						}
						if !exists {
							inter = append(inter, lv)
						}
						break
					}
				}
			}
			return Value{Tag: ValueList, List: inter}, true
		}
	}
	return Value{}, false
}

func valueToAny(v Value) any { return v.ToAny() }

func anyToValue(v any) Value { return FromAny(v) }

func valueToString(v Value) string {
	visited := map[uintptr]bool{}
	var recur func(Value) string
	recur = func(val Value) string {
		switch val.Tag {
		case ValueInt:
			return fmt.Sprintf("%d", val.Int)
		case ValueFloat:
			s := strconv.FormatFloat(val.Float, 'f', -1, 64)
			if !strings.Contains(s, ".") {
				s += ".0"
			}
			return s
		case ValueBool:
			return fmt.Sprintf("%v", val.Bool)
		case ValueStr:
			return fmt.Sprintf("%q", val.Str)
		case ValueBigInt:
			if val.BigInt != nil {
				return val.BigInt.String()
			}
			return "0"
		case ValueList:
			ptr := reflect.ValueOf(val.List).Pointer()
			if ptr != 0 {
				if visited[ptr] {
					return "[...]"
				}
				visited[ptr] = true
				defer delete(visited, ptr)
			}
			parts := make([]string, len(val.List))
			for i, x := range val.List {
				parts[i] = recur(x)
			}
			return "[" + strings.Join(parts, ", ") + "]"
		case ValueMap:
			ptr := reflect.ValueOf(val.Map).Pointer()
			if ptr != 0 {
				if visited[ptr] {
					return "{...}"
				}
				visited[ptr] = true
				defer delete(visited, ptr)
			}
			keys := make([]string, 0, len(val.Map))
			for k := range val.Map {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			parts := make([]string, len(keys))
			for i, k := range keys {
				parts[i] = fmt.Sprintf("%q: %s", k, recur(val.Map[k]))
			}
			return "{" + strings.Join(parts, ", ") + "}"
		default:
			return "nil"
		}
	}
	return recur(v)
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

func clampSlice(n, start, end int) (int, int) {
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if start > end {
		start, end = end, start
	}
	return start, end
}

func toFloat(v Value) float64 {
	switch v.Tag {
	case ValueFloat:
		return v.Float
	case ValueBool:
		if v.Bool {
			return 1
		}
		return 0
	case ValueBigInt:
		if v.BigInt != nil {
			f, _ := new(big.Float).SetInt(v.BigInt).Float64()
			return f
		}
		return 0
	case ValueBigRat:
		if v.BigRat != nil {
			f, _ := new(big.Float).SetRat(v.BigRat).Float64()
			return f
		}
		return 0
	case ValueStr:
		if f, err := strconv.ParseFloat(v.Str, 64); err == nil {
			return f
		}
	}
	if v.Tag == ValueStr {
		if f, err := strconv.ParseFloat(v.Str, 64); err == nil {
			return f
		}
	}
	if v.Tag == ValueNull {
		return 0
	}
	return float64(v.Int)
}

func toInt(v Value) int {
	switch v.Tag {
	case ValueInt:
		return v.Int
	case ValueBool:
		if v.Bool {
			return 1
		}
		return 0
	case ValueFloat:
		return int(v.Float)
	case ValueBigInt:
		if v.BigInt != nil {
			return int(v.BigInt.Int64())
		}
		return 0
	case ValueBigRat:
		if v.BigRat != nil {
			f, _ := new(big.Float).SetRat(v.BigRat).Float64()
			return int(f)
		}
		return 0
	case ValueStr:
		if i, err := strconv.Atoi(v.Str); err == nil {
			return i
		}
		if f, err := strconv.ParseFloat(v.Str, 64); err == nil {
			return int(f)
		}
	}
	return 0
}

func toBigInt(v Value) *big.Int {
	switch v.Tag {
	case ValueBigInt:
		if v.BigInt != nil {
			return new(big.Int).Set(v.BigInt)
		}
		return new(big.Int)
	case ValueInt:
		return big.NewInt(int64(v.Int))
	case ValueFloat:
		return big.NewInt(int64(v.Float))
	case ValueBool:
		if v.Bool {
			return big.NewInt(1)
		}
		return big.NewInt(0)
	case ValueStr:
		if bi, ok := new(big.Int).SetString(v.Str, 10); ok {
			return bi
		}
	}
	return big.NewInt(0)
}

func toRat(v Value) *big.Rat {
	switch v.Tag {
	case ValueBigRat:
		if v.BigRat != nil {
			return new(big.Rat).Set(v.BigRat)
		}
		return new(big.Rat)
	case ValueBigInt:
		if v.BigInt != nil {
			return new(big.Rat).SetInt(v.BigInt)
		}
		return new(big.Rat)
	case ValueInt:
		return new(big.Rat).SetInt64(int64(v.Int))
	case ValueFloat:
		return new(big.Rat).SetFloat64(v.Float)
	case ValueBool:
		if v.Bool {
			return big.NewRat(1, 1)
		}
		return big.NewRat(0, 1)
	case ValueStr:
		if r, ok := new(big.Rat).SetString(v.Str); ok {
			return r
		}
	}
	return new(big.Rat)
}

func valTag(v Value) regTag {
	switch v.Tag {
	case ValueInt:
		return tagInt
	case ValueBigInt:
		return tagUnknown
	case ValueBigRat:
		return tagUnknown
	case ValueFloat:
		return tagFloat
	case ValueBool:
		return tagBool
	default:
		return tagUnknown
	}
}

func valuesEqual(a, b Value) bool {
	if a.Tag == ValueNull || b.Tag == ValueNull {
		return a.Tag == ValueNull && b.Tag == ValueNull
	}
	if a.Tag == ValueFloat || b.Tag == ValueFloat {
		return toFloat(a) == toFloat(b)
	}
	if a.Tag == ValueBigRat || b.Tag == ValueBigRat {
		return toRat(a).Cmp(toRat(b)) == 0
	}
	if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
		return toBigInt(a).Cmp(toBigInt(b)) == 0
	}
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case ValueInt:
		return a.Int == b.Int
	case ValueBigInt:
		if a.BigInt == nil || b.BigInt == nil {
			return a.BigInt == nil && b.BigInt == nil
		}
		return a.BigInt.Cmp(b.BigInt) == 0
	case ValueBool:
		return a.Bool == b.Bool
	case ValueStr:
		return a.Str == b.Str
	case ValueList:
		if len(a.List) != len(b.List) {
			return false
		}
		for i := range a.List {
			if !valuesEqual(a.List[i], b.List[i]) {
				return false
			}
		}
		return true
	case ValueMap:
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

func pairKey(v Value) Value {
	if v.Tag == ValueList {
		if len(v.List) > 0 {
			return v.List[0]
		}
	} else if v.Tag == ValueMap {
		if k, ok := v.Map["key"]; ok {
			return k
		}
		if k, ok := v.Map["keys"]; ok {
			return k
		}
	}
	return Value{}
}

func pairVal(v Value) Value {
	if v.Tag == ValueList {
		if len(v.List) > 1 {
			return v.List[1]
		}
	} else if v.Tag == ValueMap {
		if val, ok := v.Map["value"]; ok {
			return val
		}
	}
	return Value{Tag: ValueNull}
}

func valueLess(a, b Value) bool {
	if a.Tag == ValueNull || b.Tag == ValueNull {
		// When sorting, NULL values compare greater than any other
		// value so that they appear last. Treat two NULLs as equal.
		if a.Tag == ValueNull && b.Tag != ValueNull {
			return false
		}
		if a.Tag != ValueNull && b.Tag == ValueNull {
			return true
		}
		return false
	}
	if a.Tag == ValueBigRat || b.Tag == ValueBigRat {
		return toRat(a).Cmp(toRat(b)) < 0
	}
	if a.Tag == ValueBigInt || b.Tag == ValueBigInt {
		return toBigInt(a).Cmp(toBigInt(b)) < 0
	}
	switch a.Tag {
	case ValueInt:
		switch b.Tag {
		case ValueInt:
			return a.Int < b.Int
		case ValueFloat:
			return float64(a.Int) < b.Float
		case ValueBool:
			bi := 0
			if b.Bool {
				bi = 1
			}
			return a.Int < bi
		}
	case ValueFloat:
		switch b.Tag {
		case ValueInt:
			return a.Float < float64(b.Int)
		case ValueFloat:
			return a.Float < b.Float
		case ValueBool:
			bi := 0
			if b.Bool {
				bi = 1
			}
			return a.Float < float64(bi)
		}
	case ValueStr:
		if b.Tag == ValueStr {
			return a.Str < b.Str
		}
	case ValueBool:
		if b.Tag == ValueBool {
			return !a.Bool && b.Bool
		}
		if b.Tag == ValueInt {
			ai := 0
			if a.Bool {
				ai = 1
			}
			return ai < b.Int
		}
		if b.Tag == ValueFloat {
			ai := 0
			if a.Bool {
				ai = 1
			}
			return float64(ai) < b.Float
		}
	case ValueList:
		if b.Tag == ValueList {
			n := len(a.List)
			if len(b.List) < n {
				n = len(b.List)
			}
			for i := 0; i < n; i++ {
				if valueLess(a.List[i], b.List[i]) {
					return true
				}
				if valueLess(b.List[i], a.List[i]) {
					return false
				}
			}
			return len(a.List) < len(b.List)
		}
	case ValueMap:
		if b.Tag == ValueMap {
			keysA := make([]string, 0, len(a.Map))
			for k := range a.Map {
				keysA = append(keysA, k)
			}
			sort.Strings(keysA)
			keysB := make([]string, 0, len(b.Map))
			for k := range b.Map {
				keysB = append(keysB, k)
			}
			sort.Strings(keysB)
			n := len(keysA)
			if len(keysB) < n {
				n = len(keysB)
			}
			for i := 0; i < n; i++ {
				if keysA[i] < keysB[i] {
					return true
				}
				if keysB[i] < keysA[i] {
					return false
				}
				va := a.Map[keysA[i]]
				vb := b.Map[keysB[i]]
				if valueLess(va, vb) {
					return true
				}
				if valueLess(vb, va) {
					return false
				}
			}
			return len(a.Map) < len(b.Map)
		}
	}
	return fmt.Sprint(a.ToAny()) < fmt.Sprint(b.ToAny())
}

// resolveTypeRef converts a parsed type reference into a concrete type using env.
func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
			if ut, ok := env.FindUnionByVariant(*t.Simple); ok {
				return ut
			}
		}
	}
	return types.AnyType{}
}

// castValue attempts to convert v to type t, returning an error if not possible.
func castValue(t types.Type, v any) (any, error) {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		switch x := v.(type) {
		case int:
			return x, nil
		case float64:
			return int(x), nil
		case string:
			n, err := strconv.Atoi(x)
			if err != nil {
				return nil, fmt.Errorf("cannot cast %q to %s", x, t)
			}
			return n, nil
		default:
			return nil, fmt.Errorf("cannot cast %T to %s", v, t)
		}
	case types.BigIntType:
		switch x := v.(type) {
		case *big.Int:
			if x == nil {
				return (*big.Int)(nil), nil
			}
			return new(big.Int).Set(x), nil
		case int:
			return big.NewInt(int64(x)), nil
		case float64:
			return big.NewInt(int64(x)), nil
		case string:
			if bi, ok := new(big.Int).SetString(x, 10); ok {
				return bi, nil
			}
			return nil, fmt.Errorf("cannot cast %q to %s", x, t)
		default:
			return nil, fmt.Errorf("cannot cast %T to %s", v, t)
		}
	case types.FloatType:
		switch x := v.(type) {
		case float64:
			return x, nil
		case int:
			return float64(x), nil
		default:
			return nil, fmt.Errorf("cannot cast %T to %s", v, t)
		}
	case types.StringType:
		if s, ok := v.(string); ok {
			return s, nil
		}
		return nil, fmt.Errorf("cannot cast %T to %s", v, t)
	case types.BoolType:
		if b, ok := v.(bool); ok {
			return b, nil
		}
		return nil, fmt.Errorf("cannot cast %T to %s", v, t)
	case types.ListType:
		list, ok := v.([]any)
		if !ok {
			return nil, fmt.Errorf("cannot cast %T to %s", v, t)
		}
		out := make([]any, len(list))
		for i, item := range list {
			cv, err := castValue(tt.Elem, item)
			if err != nil {
				return nil, err
			}
			out[i] = cv
		}
		return out, nil
	case types.MapType:
		switch m := v.(type) {
		case map[string]any:
			out := map[string]any{}
			for k, val := range m {
				cv, err := castValue(tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			if _, ok := tt.Key.(types.StringType); ok {
				return out, nil
			}
			if _, ok := tt.Key.(types.IntType); ok {
				intMap := make(map[int]any, len(out))
				for k, v := range out {
					iv, err := strconv.Atoi(k)
					if err != nil {
						return nil, fmt.Errorf("cannot cast key %q to int", k)
					}
					intMap[iv] = v
				}
				return intMap, nil
			}
			return out, nil
		case map[int]any:
			out := make(map[int]any, len(m))
			for k, val := range m {
				cv, err := castValue(tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			return out, nil
		case map[any]any:
			out := make(map[any]any, len(m))
			for k, val := range m {
				cv, err := castValue(tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			return out, nil
		default:
			return nil, fmt.Errorf("cannot cast %T to %s", v, t)
		}
	case types.StructType:
		m, ok := v.(map[string]any)
		if !ok {
			return nil, fmt.Errorf("cannot cast %T to %s", v, tt.Name)
		}
		out := map[string]any{"__name": tt.Name}
		for name, ft := range tt.Fields {
			fv, ok := m[name]
			if !ok {
				return nil, fmt.Errorf("missing field %s for %s", name, tt.Name)
			}
			cv, err := castValue(ft, fv)
			if err != nil {
				return nil, err
			}
			out[name] = cv
		}
		return out, nil
	default:
		return v, nil
	}
}

func toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	case map[int]any:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[strconv.Itoa(k)] = vv
		}
		return out
	case map[any]any:
		out := make(map[string]any, len(v))
		for kk, vv := range v {
			out[fmt.Sprint(kk)] = vv
		}
		return out
	default:
		return nil
	}
}

func toMapSlice(v any) ([]map[string]any, bool) {
	switch rows := v.(type) {
	case []map[string]any:
		return rows, true
	case []any:
		out := make([]map[string]any, len(rows))
		for i, item := range rows {
			m, ok := item.(map[string]any)
			if !ok {
				return nil, false
			}
			out[i] = m
		}
		return out, true
	default:
		return nil, false
	}
}
