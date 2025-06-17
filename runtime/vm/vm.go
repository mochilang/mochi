package vm

import "fmt"

// Func is a compiled function consisting of SSA instructions.
type Func struct {
	Name   string
	Regs   int
	Instrs []Instr
}

// VM executes SSA instructions in a simple register-based interpreter.
type VM struct{}

// New creates a new VM instance.
func New() *VM { return &VM{} }

// Run executes the given function with optional arguments. The value in register
// 0 of the function at OpRet is returned.
func (vm *VM) Run(fn *Func, args ...any) (any, error) {
	if fn.Regs == 0 {
		fn.Regs = len(args)
	}
	regs := make([]any, fn.Regs)
	copy(regs, args)

	pc := 0
	for pc < len(fn.Instrs) {
		ins := fn.Instrs[pc]
		pc++
		switch ins.Op {
		case OpNop:
			// do nothing
		case OpLoadConst:
			if ins.C >= len(regs) {
				regs = grow(regs, ins.C+1)
			}
			regs[ins.C] = ins.Arg
		case OpAdd:
			regs[ins.C] = asNumber(regs[ins.A]) + asNumber(regs[ins.B])
		case OpSub:
			regs[ins.C] = asNumber(regs[ins.A]) - asNumber(regs[ins.B])
		case OpMul:
			regs[ins.C] = asNumber(regs[ins.A]) * asNumber(regs[ins.B])
		case OpDiv:
			regs[ins.C] = asNumber(regs[ins.A]) / asNumber(regs[ins.B])
		case OpJump:
			pc = ins.Arg.(int)
		case OpJumpIf:
			if regs[ins.A].(bool) {
				pc = ins.Arg.(int)
			}
		case OpCall:
			callee, ok := ins.Arg.(*Func)
			if !ok {
				return nil, fmt.Errorf("invalid callee")
			}
			res, err := vm.Run(callee, regs[ins.A:ins.B]...)
			if err != nil {
				return nil, err
			}
			if ins.C >= len(regs) {
				regs = grow(regs, ins.C+1)
			}
			regs[ins.C] = res
		case OpRet:
			if ins.A < len(regs) {
				return regs[ins.A], nil
			}
			return nil, nil
		default:
			return nil, fmt.Errorf("unknown opcode %v", ins.Op)
		}
	}
	return nil, nil
}

func grow(regs []any, n int) []any {
	if n <= len(regs) {
		return regs
	}
	extra := make([]any, n-len(regs))
	return append(regs, extra...)
}

func asNumber(v any) float64 {
	switch n := v.(type) {
	case int:
		return float64(n)
	case float64:
		return n
	default:
		return 0
	}
}
