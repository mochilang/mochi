package vm2jit

import (
	"unsafe"

	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/vm2"
)

func init() {
	vm2.JITCallFn = jitCall
}

// jitCall is installed as vm2.JITCallFn. It bridges the vm2 interpreter's
// OpCall handler to a JIT-compiled callee: copies args from vm.Stack into a
// local register file, invokes the JIT'd code via the trampoline, and returns
// the result Cell.
//
// argBase is the absolute index in vm.Stack of the first argument (caller's
// fr.RegsBase + argSrc). n is the argument count. The return register is
// managed by the caller (eval.go stores the result at regs[ins.A]).
func jitCall(v *vm2.VM, fn *vm2.Function, argBase int, n int, _ int32) vm2.Cell {
	numRegs := fn.NumRegs
	if numRegs == 0 {
		numRegs = 1 // need at least one cell so &regs[0] is valid
	}
	regs := make([]vm2.Cell, numRegs)
	if n > fn.NumRegs {
		n = fn.NumRegs
	}
	copy(regs, v.Stack[argBase:argBase+n])
	return vm2.Cell(trampoline.Call(fn.JITCode, unsafe.Pointer(&regs[0])))
}
