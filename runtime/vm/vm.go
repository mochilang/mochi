package vm

import (
	"fmt"
	"io"
	"strings"
	"text/tabwriter"
)

// OpCode represents a single VM instruction.
type OpCode int

const (
	OpNOP OpCode = iota
	OpLoadConst
	OpMove
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpEq
	OpLT
	OpLen
	OpIndex
	OpArrayConst
	OpArrayPair
	OpJump
	OpJumpIfZero
	OpReturn
	OpPrint
)

func (op OpCode) String() string {
	switch op {
	case OpNOP:
		return "NOP"
	case OpLoadConst:
		return "LOAD_CONST"
	case OpMove:
		return "MOVE"
	case OpAdd:
		return "ADD"
	case OpSub:
		return "SUB"
	case OpMul:
		return "MUL"
	case OpDiv:
		return "DIV"
	case OpEq:
		return "EQ"
	case OpLT:
		return "LT"
	case OpLen:
		return "LEN"
	case OpIndex:
		return "INDEX"
	case OpArrayConst:
		return "ARRAY_CONST"
	case OpArrayPair:
		return "ARRAY_PAIR"
	case OpJump:
		return "JUMP"
	case OpJumpIfZero:
		return "JUMP_IF_ZERO"
	case OpReturn:
		return "RETURN"
	case OpPrint:
		return "PRINT"
	default:
		return fmt.Sprintf("OpCode(%d)", int(op))
	}
}

func (ins Instr) String() string {
	switch ins.Op {
	case OpLoadConst, OpArrayConst:
		return fmt.Sprintf("%s r%d %v", ins.Op, ins.A, ins.Value)
	case OpMove, OpLen:
		return fmt.Sprintf("%s r%d r%d", ins.Op, ins.A, ins.B)
	case OpPrint:
		return fmt.Sprintf("%s r%d", ins.Op, ins.A)
	case OpAdd, OpSub, OpMul, OpDiv, OpEq, OpLT, OpIndex, OpArrayPair:
		return fmt.Sprintf("%s r%d r%d r%d", ins.Op, ins.A, ins.B, ins.C)
	case OpJump:
		return fmt.Sprintf("%s %d", ins.Op, ins.A)
	case OpJumpIfZero:
		return fmt.Sprintf("%s r%d %d", ins.Op, ins.A, ins.B)
	case OpReturn:
		return ins.Op.String()
	default:
		return fmt.Sprintf("%s a=%d b=%d c=%d v=%v", ins.Op, ins.A, ins.B, ins.C, ins.Value)
	}
}

// Format returns a human readable representation of bytecode instructions.
func Format(code []Instr) string {
	var sb strings.Builder
	tw := tabwriter.NewWriter(&sb, 0, 0, 2, ' ', 0)
	for i, ins := range code {
		fmt.Fprintf(tw, "%04d:\t%s\n", i, ins)
	}
	tw.Flush()
	return sb.String()
}

// Instr is a single instruction in the VM.
type Instr struct {
	Op      OpCode
	A, B, C int
	Value   any
}

// VM is a very small register based virtual machine. It is not a full
// implementation of the Mochi runtime but provides a minimal structure for
// executing register SSA code.
type VM struct {
	regs []any
	ip   int
	code []Instr
	out  io.Writer
}

// New returns a new VM instance with the provided code and register count.
func New(code []Instr, regCount int) *VM {
	return &VM{regs: make([]any, regCount), code: code}
}

// SetWriter sets the output writer used by certain instructions.
func (v *VM) SetWriter(w io.Writer) { v.out = w }

// Run executes the loaded code.
func (v *VM) Run() error {
	for v.ip < len(v.code) {
		ins := v.code[v.ip]
		switch ins.Op {
		case OpNOP:
			// do nothing
		case OpLoadConst:
			v.regs[ins.A] = ins.Value
		case OpMove:
			v.regs[ins.A] = v.regs[ins.B]
		case OpAdd:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			v.regs[ins.A] = a + b
		case OpSub:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			v.regs[ins.A] = a - b
		case OpMul:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			v.regs[ins.A] = a * b
		case OpDiv:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			if b == 0 {
				return fmt.Errorf("division by zero")
			}
			v.regs[ins.A] = a / b
		case OpEq:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			if a == b {
				v.regs[ins.A] = 1
			} else {
				v.regs[ins.A] = 0
			}
		case OpLT:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			if a < b {
				v.regs[ins.A] = 1
			} else {
				v.regs[ins.A] = 0
			}
		case OpLen:
			switch v0 := v.regs[ins.B].(type) {
			case []int:
				v.regs[ins.A] = len(v0)
			case []any:
				v.regs[ins.A] = len(v0)
			default:
				return fmt.Errorf("len unsupported %T", v.regs[ins.B])
			}
		case OpIndex:
			idx, _ := v.regs[ins.C].(int)
			switch arr := v.regs[ins.B].(type) {
			case []int:
				v.regs[ins.A] = arr[idx]
			case []any:
				v.regs[ins.A] = arr[idx]
			default:
				return fmt.Errorf("index unsupported %T", v.regs[ins.B])
			}
		case OpArrayConst:
			v.regs[ins.A] = ins.Value
		case OpArrayPair:
			a, _ := v.regs[ins.B].(int)
			b, _ := v.regs[ins.C].(int)
			v.regs[ins.A] = []int{a, b}
		case OpJump:
			v.ip = ins.A - 1
		case OpJumpIfZero:
			v0, _ := v.regs[ins.A].(int)
			if v0 == 0 {
				v.ip = ins.B - 1
			}
		case OpPrint:
			if v.out == nil {
				return fmt.Errorf("no writer set")
			}
			fmt.Fprintln(v.out, v.regs[ins.A])
		case OpReturn:
			return nil
		default:
			return fmt.Errorf("unknown opcode %d", ins.Op)
		}
		v.ip++
	}
	return nil
}

// Reg returns the value of a register.
func (v *VM) Reg(i int) any { return v.regs[i] }
