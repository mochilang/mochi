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

// vmJITFrame fetches the per-VM jitFrame3 scratch buffer, allocating
// it on the first call. The buffer (~32 KB: 4096 i64 slots plus the
// f64/Cell/arena fields) is parked on the VM via vm3.VM.SetJITState
// so it survives across every OpCall*-driven jitCall within the VM,
// replacing the prior sync.Pool path. For the lists_fill_sum kernel
// the win is roughly 20 ns/call (two pool Get/Put pairs at ~10 ns
// each on Apple M4); the buffer cost is paid once per VM lifetime.
func vmJITFrame(vm *vm3.VM) *jitFrame3 {
	if jf, ok := vm.JITState().(*jitFrame3); ok && jf != nil {
		return jf
	}
	jf := new(jitFrame3)
	vm.SetJITState(jf)
	return jf
}

// jitFrame3RegsI64Words is the number of int64 slots reserved for the
// JIT's per-call regs buffer. The JIT's self-recursive call protocol
// (lower_arm64.go) bumps the regs base pointer by NumRegsI64*8 at every
// BL, so the buffer must hold (max_depth + 1) * NumRegsI64 entries. For
// the worst corpus kernel fib_rec(n=25) this is roughly 2*25 levels *
// 3 regs = 150 int64s; allocating 4096 covers depth-1k recursion in any
// 4-reg fn with comfortable headroom. The trampoline is NOSPLIT so the
// buffer must be heap-allocated up front; sizing it generously is
// cheaper than allocating per call.
const jitFrame3RegsI64Words = 4096

// jitFrame3 is the per-call scratch the vm3 interp -> JIT boundary
// hands to the trampoline. Heap-allocated so the Go GC will not move it
// during the native call; the JIT writes the slots it uses and reads
// only its declared params, so we do not clear between calls. status is
// reset to zero by jitCall before each call.
//
// regsCell carries the Cell-bank window for Phase 6.2d.2.a step 2.
// arenaCtx is a snapshot of the arena slab base pointers populated by
// jitCall just before the trampoline dispatch; its address is pinned in
// x4 (AArch64) / R8 (AMD64) by trampoline.CallStatusM, and the JIT's
// prologue dereferences it once to cache listsBase in a callee-saved
// reg. Layout order is deliberate: arenaCtx sits before status so cold
// fields stay grouped at the end of the cache line that holds the
// status word.
type jitFrame3 struct {
	regsI64  [jitFrame3RegsI64Words]int64
	regsF64  [MaxF64Regs]float64
	regsCell [MaxCellRegs]vm3.Cell
	arenaCtx jitArenaCtx
	status   int64
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
func jitCall(vm *vm3.VM, fn *vm3.Function, argsI64 []int64, argsF64 []float64, argsCell []vm3.Cell) (uint64, bool, error) {
	if fn.JITCode == nil {
		return 0, false, errors.New("vm3jit: jitCall on fn without JITCode")
	}
	jf := vmJITFrame(vm)
	// Zero the slots the JIT's prologue will load. Pool reuse leaves
	// prior call's values in scratch slots; the JIT may load them into
	// pinned regs before any write, so we have to match the interp's
	// "fresh frame starts zeroed" semantics for non-param slots. Param
	// slots are immediately overwritten below.
	nI64 := min(int(fn.NumRegsI64), MaxI64Regs)
	clear(jf.regsI64[:nI64])
	nF64 := min(int(fn.NumRegsF64), MaxF64Regs)
	clear(jf.regsF64[:nF64])
	nCell := min(int(fn.NumRegsCell), MaxCellRegs)
	clear(jf.regsCell[:nCell])
	// Lay out params per ParamBanks position-indexed convention: argsX[k]
	// is meaningful iff fn.ParamBanks[k] == BankX. For i64-only callees
	// (OpCallI64 / Phase 6.2c path) argsCell/argsF64 are nil and we use
	// the fast path: copy argsI64 directly into regsI64[0..]. For mixed-
	// bank callees (Phase 6.2d.2.b path, OpCallMixed) walk ParamBanks and
	// drop each arg into its bank's regs<X>[k] slot.
	if fn.NumRegsCell == 0 && len(argsCell) == 0 {
		n := min(len(argsI64), MaxI64Regs)
		copy(jf.regsI64[:n], argsI64[:n])
	} else {
		for k, b := range fn.ParamBanks {
			switch b {
			case vm3.BankI64:
				if k < len(argsI64) && k < MaxI64Regs {
					jf.regsI64[k] = argsI64[k]
				}
			case vm3.BankF64:
				if k < len(argsF64) && k < MaxF64Regs {
					jf.regsF64[k] = argsF64[k]
				}
			case vm3.BankCell:
				if k < len(argsCell) && k < MaxCellRegs {
					jf.regsCell[k] = argsCell[k]
				}
			}
		}
	}
	jf.status = 0

	var bits uint64
	switch {
	case fn.NumRegsCell > 0:
		populateArenaCtx(&jf.arenaCtx, vm.Arenas())
		bits = trampoline.CallStatusM(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status),
			unsafe.Pointer(&jf.regsF64[0]),
			unsafe.Pointer(&jf.regsCell[0]),
			unsafe.Pointer(&jf.arenaCtx))
	case fn.JITHasF64:
		bits = trampoline.CallStatusFF(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status),
			unsafe.Pointer(&jf.regsF64[0]))
	default:
		bits = trampoline.CallStatus(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status))
	}
	if jf.status != 0 {
		// Deopt-resume protocol (Phase 6.2d.2.c). The JIT's per-fn
		// deopt block spills every pinned reg back to jf.regsX before
		// writing *status and returning, so jf carries the JIT's final
		// state. Copy it into vm.deopt* so OpCallI64 / OpCallMixed can
		// populate the new interp frame from the spilled state instead
		// of the original args; the interpreter then restarts at PC=0
		// and the loop kernels (the only shape admitted under the
		// whitelist) pick up exactly where the JIT left off.
		nI64 := int(fn.NumRegsI64)
		nF64 := int(fn.NumRegsF64)
		nCell := int(fn.NumRegsCell)
		copy(vm.DeoptScratchI64(nI64), jf.regsI64[:nI64])
		copy(vm.DeoptScratchF64(nF64), jf.regsF64[:nF64])
		copy(vm.DeoptScratchCell(nCell), jf.regsCell[:nCell])
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
