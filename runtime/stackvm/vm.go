package stackvm

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sort"
)

// Op defines an opcode for the stack VM.
type Op uint8

const (
	OpPushConst Op = iota
	OpLoad
	OpStore
	OpAdd
	OpSub
	OpNeg
	OpEqual
	OpLess
	OpLessEq
	OpLen
	OpIndex
	OpMakeList
	OpMakeMap
	OpJump
	OpJumpIfFalse
	OpCall
	OpReturn
	OpPop
	OpPrint
	OpExpect
)

// Instr represents a single VM instruction.
type Instr struct {
	Op  Op
	A   int   // generic int argument (var index, jump target, function index)
	B   int   // secondary argument (e.g. number of args)
	Val Value // constant value for OpPushConst
}

type Function struct {
	Code    []Instr
	NumVars int
	Name    string
}

type Program struct {
	Funcs []Function
}

type frame struct {
	fn     *Function
	locals []Value
	ip     int
}

type VM struct {
	prog   *Program
	writer io.Writer
	reader *bufio.Reader
}

func New(p *Program, w io.Writer) *VM {
	return &VM{prog: p, writer: w, reader: bufio.NewReader(os.Stdin)}
}

// Run executes the program starting at function 0 (main).
func (v *VM) Run() error {
	_, err := v.call(0, nil)
	return err
}

func (v *VM) call(fnIndex int, args []Value) (Value, error) {
	fn := &v.prog.Funcs[fnIndex]
	fr := &frame{fn: fn, locals: make([]Value, fn.NumVars)}
	for i := 0; i < len(args) && i < fn.NumVars; i++ {
		fr.locals[i] = args[i]
	}
	stack := []Value{}
	frames := []*frame{fr}
	for len(frames) > 0 {
		fr = frames[len(frames)-1]
		if fr.ip >= len(fr.fn.Code) {
			frames = frames[:len(frames)-1]
			if len(frames) == 0 {
				return Value{}, nil
			}
			continue
		}
		ins := fr.fn.Code[fr.ip]
		fr.ip++
		switch ins.Op {
		case OpPushConst:
			stack = append(stack, ins.Val)
		case OpLoad:
			stack = append(stack, fr.locals[ins.A])
		case OpStore:
			if len(stack) == 0 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			val := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			fr.locals[ins.A] = val
		case OpAdd:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			switch {
			case a.Tag == ValueInt && b.Tag == ValueInt:
				stack = append(stack, Value{Tag: ValueInt, Int: a.Int + b.Int})
			case a.Tag == ValueStr && b.Tag == ValueStr:
				stack = append(stack, Value{Tag: ValueStr, Str: a.Str + b.Str})
			default:
				return Value{}, fmt.Errorf("invalid add")
			}
		case OpSub:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueInt, Int: a.Int - b.Int})
		case OpNeg:
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueInt, Int: -a.Int})
		case OpEqual:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueBool, Bool: valuesEqual(a, b)})
		case OpLess:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueBool, Bool: valueLess(a, b)})
		case OpLessEq:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueBool, Bool: !valueLess(b, a)})
		case OpLen:
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			stack = append(stack, Value{Tag: ValueInt, Int: len(a.List)})
		case OpIndex:
			idxVal := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			list := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			idx := idxVal.Int
			if idx < 0 {
				idx += len(list.List)
			}
			if idx < 0 || idx >= len(list.List) {
				return Value{}, fmt.Errorf("index out of range")
			}
			stack = append(stack, list.List[idx])
		case OpMakeList:
			n := ins.A
			if len(stack) < n {
				return Value{}, fmt.Errorf("stack underflow")
			}
			list := make([]Value, n)
			for i := n - 1; i >= 0; i-- {
				list[i] = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
			}
			stack = append(stack, Value{Tag: ValueList, List: list})
		case OpMakeMap:
			n := ins.A
			if len(stack) < n*2 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			m := make(map[string]Value, n)
			for i := 0; i < n; i++ {
				val := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				keyVal := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				key := fmt.Sprint(valueToAny(keyVal))
				m[key] = val
			}
			stack = append(stack, Value{Tag: ValueMap, Map: m})
		case OpJump:
			fr.ip = ins.A
		case OpJumpIfFalse:
			if len(stack) == 0 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			cond := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !cond.Truthy() {
				fr.ip = ins.A
			}
		case OpCall:
			if len(stack) < ins.B {
				return Value{}, fmt.Errorf("stack underflow")
			}
			callArgs := make([]Value, ins.B)
			for i := ins.B - 1; i >= 0; i-- {
				callArgs[i] = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
			}
			// builtins
			if ins.A < 0 {
				switch -ins.A {
				case 1: // print
					for i := range callArgs {
						fmt.Fprintln(v.writer, formatValue(callArgs[i]))
					}
					stack = append(stack, Value{Tag: ValueNull})
				case 2: // len
					if len(callArgs) != 1 {
						return Value{}, fmt.Errorf("len expects 1 arg")
					}
					stack = append(stack, Value{Tag: ValueInt, Int: len(callArgs[0].List)})
				case 3: // json
					if len(callArgs) != 1 {
						return Value{}, fmt.Errorf("json expects 1 arg")
					}
					b, _ := json.Marshal(valueToAny(callArgs[0]))
					fmt.Fprintln(v.writer, string(b))
					stack = append(stack, Value{Tag: ValueNull})
				case 4: // min
					if len(callArgs) != 1 {
						return Value{}, fmt.Errorf("min expects 1 arg")
					}
					lst := callArgs[0]
					if lst.Tag != ValueList {
						return Value{}, fmt.Errorf("min expects list")
					}
					if len(lst.List) == 0 {
						stack = append(stack, Value{Tag: ValueNull})
						break
					}
					m := lst.List[0]
					for _, v2 := range lst.List[1:] {
						if valueLess(v2, m) {
							m = v2
						}
					}
					stack = append(stack, m)
				default:
					return Value{}, fmt.Errorf("unknown builtin")
				}
			} else {
				ret, err := v.call(ins.A, callArgs)
				if err != nil {
					return Value{}, err
				}
				stack = append(stack, ret)
			}
		case OpReturn:
			var ret Value
			if ins.A == 1 {
				if len(stack) == 0 {
					return Value{}, fmt.Errorf("stack underflow")
				}
				ret = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
			}
			return ret, nil
		case OpPop:
			if len(stack) == 0 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			stack = stack[:len(stack)-1]
		case OpPrint:
			if len(stack) == 0 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			val := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			fmt.Fprintln(v.writer, formatValue(val))
		case OpExpect:
			if len(stack) == 0 {
				return Value{}, fmt.Errorf("stack underflow")
			}
			val := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if val.Tag != ValueBool || !val.Bool {
				return Value{}, fmt.Errorf("expect condition failed")
			}
		default:
			return Value{}, fmt.Errorf("unknown op")
		}
	}
	return Value{}, nil
}

func formatValue(v Value) string {
	switch v.Tag {
	case ValueInt:
		return fmt.Sprintf("%d", v.Int)
	case ValueStr:
		return v.Str
	case ValueBool:
		if v.Bool {
			return "true"
		}
		return "false"
	case ValueList:
		s := "["
		for i, vv := range v.List {
			if i > 0 {
				s += ","
			}
			s += formatValue(vv)
		}
		s += "]"
		return s
	case ValueMap:
		keys := make([]string, 0, len(v.Map))
		for k := range v.Map {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		s := "{"
		for i, k := range keys {
			if i > 0 {
				s += ","
			}
			s += fmt.Sprintf("%s:%s", k, formatValue(v.Map[k]))
		}
		s += "}"
		return s
	case ValueNull:
		return "null"
	default:
		return "?"
	}
}

func (op Op) String() string {
	switch op {
	case OpPushConst:
		return "PushConst"
	case OpLoad:
		return "Load"
	case OpStore:
		return "Store"
	case OpAdd:
		return "Add"
	case OpSub:
		return "Sub"
	case OpNeg:
		return "Neg"
	case OpEqual:
		return "Equal"
	case OpLess:
		return "Less"
	case OpLessEq:
		return "LessEq"
	case OpLen:
		return "Len"
	case OpIndex:
		return "Index"
	case OpMakeList:
		return "MakeList"
	case OpMakeMap:
		return "MakeMap"
	case OpJump:
		return "Jump"
	case OpJumpIfFalse:
		return "JumpIfFalse"
	case OpCall:
		return "Call"
	case OpReturn:
		return "Return"
	case OpPop:
		return "Pop"
	case OpPrint:
		return "Print"
	case OpExpect:
		return "Expect"
	default:
		return "?"
	}
}
