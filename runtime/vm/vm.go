package vm

import "fmt"

// VM executes a sequence of instructions.
type VM struct {
	Ins   []Instruction
	ip    int
	stack []any
	vars  map[string]any
}

// New creates a new VM for the given program.
func New(ins []Instruction) *VM {
	return &VM{Ins: ins, vars: map[string]any{}}
}

// Var retrieves the value of a variable after execution.
func (v *VM) Var(name string) any { return v.vars[name] }

func (v *VM) push(val any) { v.stack = append(v.stack, val) }

func (v *VM) pop() any {
	if len(v.stack) == 0 {
		return nil
	}
	val := v.stack[len(v.stack)-1]
	v.stack = v.stack[:len(v.stack)-1]
	return val
}

// Run executes the instructions until OpStop or OpReturn.
func (v *VM) Run() (any, error) {
	for v.ip < len(v.Ins) {
		ins := v.Ins[v.ip]
		switch ins.Op {
		case OpConst:
			v.push(ins.Value)
		case OpAdd:
			b := v.pop().(int)
			a := v.pop().(int)
			v.push(a + b)
		case OpSub:
			b := v.pop().(int)
			a := v.pop().(int)
			v.push(a - b)
		case OpMul:
			b := v.pop().(int)
			a := v.pop().(int)
			v.push(a * b)
		case OpDiv:
			b := v.pop().(int)
			a := v.pop().(int)
			v.push(a / b)
		case OpNeg:
			a := v.pop().(int)
			v.push(-a)
		case OpLoadVar:
			v.push(v.vars[ins.Str])
		case OpStoreVar:
			v.vars[ins.Str] = v.pop()
		case OpPrint:
			fmt.Println(v.pop())
		case OpReturn:
			return v.pop(), nil
		case OpStop:
			return nil, nil
		}
		v.ip++
	}
	return nil, nil
}
