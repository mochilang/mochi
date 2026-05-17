//go:build darwin && arm64

package vm2jit

import (
	"unsafe"

	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/vm2"
)

// CallDirect is exported for test/benchmark use only.
// It creates a jitFrame with the given vm pointer and register values,
// then invokes fn's JIT code via the trampoline. vm may be nil for
// arithmetic-only functions that don't execute list opcodes.
func CallDirect(fn *vm2.Function, v *vm2.VM, regs []vm2.Cell) vm2.Cell {
	// Heap-allocate so that stack-growth during slow-path Go calls
	// (e.g. append inside JITListPush) cannot move the jitFrame and
	// invalidate the x19 register that holds &jf.regs[0].
	jf := &jitFrame{}
	jf.vm = v
	n := len(regs)
	if n > maxJITRegs {
		n = maxJITRegs
	}
	copy(jf.regs[:n], regs[:n])
	return vm2.Cell{Bits: trampoline.Call(fn.JITCode, unsafe.Pointer(&jf.regs[0]))}
}
