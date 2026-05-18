package vm3jit

import (
	"errors"
	"unsafe"

	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/vm3"
)

func init() {
	vm3.JITCallFn = jitCall
}

// jitFrame3 is the per-call scratch the vm3 interp -> JIT boundary
// hands to the trampoline. Heap-allocated so the Go GC will not move it
// during the native call; reset (not cleared) between calls within a
// single OpCallI64 site since the JIT writes the slots it uses and
// reads only its declared params. status is pre-zeroed.
type jitFrame3 struct {
	regsI64 [MaxI64Regs]int64
	regsF64 [MaxF64Regs]float64
	status  int64
}

// jitCall is installed as vm3.JITCallFn. It is called by the interp
// dispatch loop at every OpCallI64 site whose callee has a non-nil
// JITCode entry. The contract:
//
//   - argsI64 carries the callee's i64 parameters preloaded.
//   - argsF64 is reserved for a later Phase 6.2c+ extension that wires
//     f64 parameter passing; this implementation accepts only nil or
//     an empty slice and ignores any value (the JIT's existing f64
//     kernels read regsF64[0..7] from x2/r14 but parameters all live
//     in regsI64 for the current corpus, so the slice stays nil).
//   - On clean return the raw uint64 from the trampoline is passed
//     back to the caller. For i64-returning fns the caller stores it
//     as int64 directly; for f64-returning fns the caller decodes via
//     math.Float64frombits.
//   - On deopt (status != 0) the trampoline result is undefined and
//     we return deopt=true so the interpreter restarts the callee
//     from PC=0 under its normal pushFrame path. Since the JIT does
//     not allocate from arenas (Phase 6.2c+), no rollback is needed.
func jitCall(_ *vm3.VM, fn *vm3.Function, argsI64 []int64, _ []float64) (uint64, bool, error) {
	if fn.JITCode == nil {
		return 0, false, errors.New("vm3jit: jitCall on fn without JITCode")
	}
	jf := &jitFrame3{}
	n := min(len(argsI64), MaxI64Regs)
	copy(jf.regsI64[:n], argsI64[:n])

	var bits uint64
	if fn.JITHasF64 {
		bits = trampoline.CallStatusFF(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status),
			unsafe.Pointer(&jf.regsF64[0]))
	} else {
		bits = trampoline.CallStatus(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status))
	}
	if jf.status != 0 {
		// Status-word deopt (StatusDivByZero etc.). Phase 6.2c keeps
		// it simple: signal deopt and let the interpreter restart the
		// callee from PC=0. A later sub-phase can recover JIT-spilled
		// regs and resume at the exact deopt PC.
		return 0, true, nil
	}
	return bits, false, nil
}

// CompileAndCache compiles prog.Funcs[idx] for the host architecture
// and, on success, stores the entry pointer into the Function so vm3's
// OpCallI64 dispatch can route through the JIT. The returned
// CompiledFunc owns the executable page: the caller must keep it
// alive (and call Free() to release it) for the Program's lifetime.
//
// On any compile failure (ErrUnsupported on host without a backend,
// ErrNotImplemented for opcodes beyond Phase 6.2b coverage), the
// function is left in its non-JIT'd state and the error is returned
// for the caller to log. Callers that want a "compile what we can,
// skip the rest" policy can ignore the error.
//
// The fn.JITCompiled flag is set unconditionally on attempt, so a
// subsequent CompileAndCache call on the same Function is a no-op.
// This keeps cold-start cost out of the OpCallI64 hot path.
func CompileAndCache(prog *vm3.Program, idx uint32) (*CompiledFunc, error) {
	if int(idx) >= len(prog.Funcs) {
		return nil, errors.New("vm3jit: CompileAndCache idx out of range")
	}
	fn := prog.Funcs[idx]
	if fn.JITCompiled {
		return nil, nil
	}
	fn.JITCompiled = true
	cf, err := CompileInProgram(prog, idx)
	if err != nil {
		return nil, err
	}
	fn.JITCode = cf.Entry()
	fn.JITHasF64 = fn.NumRegsF64 > 0
	return cf, nil
}

// CompileProgram is the convenience entry point that walks every
// function in prog and tries to JIT-cache it. Unsupported functions
// are skipped silently (consistent with MEP-39 §6.15 vm2runner
// behavior). The returned slice holds the CompiledFunc handles in
// declaration order; nil entries indicate functions the JIT could
// not compile on this host.
func CompileProgram(prog *vm3.Program) []*CompiledFunc {
	out := make([]*CompiledFunc, len(prog.Funcs))
	for i := range prog.Funcs {
		cf, _ := CompileAndCache(prog, uint32(i))
		out[i] = cf
	}
	return out
}
