package vm2jit

import (
	"unsafe"

	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/vm2"
)

func init() {
	vm2.JITCallFn = jitCall
}

// jitFrame is the register file frame passed to JIT'd functions.
// The layout is:
//
//	offset 0: vm *vm2.VM    — VM pointer, read by the interpreter-resume
//	                          wrapper after a deopt return
//	offset 8: regs[0..N-1] — vm2 Cell register file; x0 points here on entry
//
// The jitFrame is heap-allocated so its address is stable across any
// goroutine stack growth that may occur inside the trampoline.
type jitFrame struct {
	vm   *vm2.VM
	regs [maxJITRegs]vm2.Cell
}

// jitCall is installed as vm2.JITCallFn. Bridges the interpreter's OpCall
// handler to a JIT-compiled callee: copies args from vm.Stack into a jitFrame,
// invokes the JIT'd code via the trampoline, and returns the result Cell.
//
// If the JIT'd code exits via a deopt stub (see emitDeoptStubARM64), the
// trampoline return is a sentinel-tagged Cell that decodes to the
// bytecode PC at which the JIT could not continue. jitCall promotes the
// JIT register file into a real vm2 frame, sets IP to that PC, and
// resumes the interpreter via vm.RunTopFrame; the interpreter finishes
// the function and the Cell it returns is the function's result.
func jitCall(v *vm2.VM, fn *vm2.Function, argBase int, n int, _ int32) vm2.Cell {
	numRegs := fn.NumRegs
	if numRegs == 0 {
		numRegs = 1
	}
	if n > fn.NumRegs {
		n = fn.NumRegs
	}
	jf := &jitFrame{}
	jf.vm = v
	copy(jf.regs[:n], v.Stack[argBase:argBase+n])
	result := vm2.Cell(trampoline.Call(fn.JITCode, unsafe.Pointer(&jf.regs[0])))
	if pc, ok := vm2.DecodeDeopt(result); ok {
		return resumeFromDeopt(v, fn, jf, pc)
	}
	return result
}

// resumeFromDeopt promotes a JIT register file into a real vm2 frame,
// seeded with the regs the JIT spilled on its way out, then runs the
// interpreter until that frame's OpReturn fires. RetReg is unused for
// the JIT-call entry contract (the result is passed back through jitCall's
// return value, not the parent frame's register file).
func resumeFromDeopt(v *vm2.VM, fn *vm2.Function, jf *jitFrame, pc int) vm2.Cell {
	_, base := v.PushFrame(fn, 0)
	top := len(v.Frames) - 1
	v.Frames[top].IP = pc
	n := fn.NumRegs
	if n > maxJITRegs {
		n = maxJITRegs
	}
	copy(v.Stack[base:base+n], jf.regs[:n])
	ret, err := v.RunTopFrame()
	if err != nil {
		// The interpreter only errors on programmer-visible runtime
		// faults (div/0, bounds, OpHalt). Propagating via panic matches
		// the JIT contract: the JIT itself has no error-return path.
		panic(err)
	}
	return ret
}
