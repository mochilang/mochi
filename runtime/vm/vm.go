package vm

import "fmt"

type Value any

// OpCode represents a single VM instruction opcode.
type OpCode int

const (
	OpLoadConst OpCode = iota
	OpMove
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpNeg
	OpEqual
	OpLess
	OpGreater
	OpJump
	OpJumpIfFalse
	OpCall
	OpReturn
	OpHalt
)

// Instruction represents a single bytecode instruction.
type Instruction struct {
	Op OpCode
	A  int
	B  int
	C  int
}

// Builtin defines the signature for built-in functions that can be called by the VM.
type Builtin func(args []Value) (Value, error)

// VM implements a simple register based virtual machine.
type VM struct {
	ops      []Instruction
	consts   []Value
	regs     []Value
	ip       int
	builtins []Builtin
}

// New creates a new VM instance for the provided program.
func New(ops []Instruction, consts []Value, numRegs int) *VM {
	return &VM{ops: ops, consts: consts, regs: make([]Value, numRegs)}
}

// Register adds a builtin function and returns its index.
func (vm *VM) Register(fn Builtin) int {
	vm.builtins = append(vm.builtins, fn)
	return len(vm.builtins) - 1
}

// Run executes the program until OpHalt or OpReturn.
func (vm *VM) Run() (Value, error) {
	for vm.ip < len(vm.ops) {
		inst := vm.ops[vm.ip]
		vm.ip++
		switch inst.Op {
		case OpLoadConst:
			vm.regs[inst.A] = vm.consts[inst.B]
		case OpMove:
			vm.regs[inst.A] = vm.regs[inst.B]
		case OpAdd:
			vm.regs[inst.A] = vm.regs[inst.B].(int) + vm.regs[inst.C].(int)
		case OpSub:
			vm.regs[inst.A] = vm.regs[inst.B].(int) - vm.regs[inst.C].(int)
		case OpMul:
			vm.regs[inst.A] = vm.regs[inst.B].(int) * vm.regs[inst.C].(int)
		case OpDiv:
			vm.regs[inst.A] = vm.regs[inst.B].(int) / vm.regs[inst.C].(int)
		case OpNeg:
			vm.regs[inst.A] = -vm.regs[inst.B].(int)
		case OpEqual:
			vm.regs[inst.A] = vm.regs[inst.B] == vm.regs[inst.C]
		case OpLess:
			vm.regs[inst.A] = vm.regs[inst.B].(int) < vm.regs[inst.C].(int)
		case OpGreater:
			vm.regs[inst.A] = vm.regs[inst.B].(int) > vm.regs[inst.C].(int)
		case OpJump:
			vm.ip = inst.A
		case OpJumpIfFalse:
			cond := vm.regs[inst.A]
			if cond == false || cond == nil {
				vm.ip = inst.B
			}
		case OpCall:
			if inst.B < 0 || inst.B >= len(vm.builtins) {
				return nil, fmt.Errorf("invalid builtin index %d", inst.B)
			}
			args := []Value{vm.regs[inst.C]}
			res, err := vm.builtins[inst.B](args)
			if err != nil {
				return nil, err
			}
			vm.regs[inst.A] = res
		case OpReturn:
			return vm.regs[inst.A], nil
		case OpHalt:
			return vm.regs[inst.A], nil
		default:
			return nil, fmt.Errorf("unknown opcode %d", inst.Op)
		}
	}
	return nil, nil
}
