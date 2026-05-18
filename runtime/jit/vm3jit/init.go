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
	// Phase 6.2d.2.b step 2.E fast path: JITPreAllocList kernels (the
	// lists/maps entry shape) have a predictable param layout (argsI64
	// only, ParamBanks not consulted) and the JIT body writes every
	// scratch slot before reading it -- skip the per-bank clear()s and
	// the ParamBanks switch, and slot the warm-cache scratch list
	// directly into regsCell[A]. Snapshot/restore is also skipped (the
	// scratch slot is the only allocation, and it lives across calls).
	if fn.JITPreAllocList {
		op0 := fn.Code[0]
		dest := int(op0.A)
		if dest < MaxCellRegs {
			jf.regsCell[dest] = vm.EnsureScratchList(int(op0.C))
		}
		n := min(len(argsI64), MaxI64Regs)
		copy(jf.regsI64[:n], argsI64[:n])
		jf.status = 0
		populateArenaCtx(&jf.arenaCtx, vm.Arenas())
		bits := trampoline.CallStatusM(
			fn.JITCode,
			unsafe.Pointer(&jf.regsI64[0]),
			unsafe.Pointer(&jf.status),
			unsafe.Pointer(&jf.regsF64[0]),
			unsafe.Pointer(&jf.regsCell[0]),
			unsafe.Pointer(&jf.arenaCtx))
		if jf.status != 0 {
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
	// General-case path. Zero the slots the JIT's prologue will load;
	// pool reuse leaves prior call's values in scratch slots and the
	// JIT may load them into pinned regs before any write.
	nI64 := min(int(fn.NumRegsI64), MaxI64Regs)
	clear(jf.regsI64[:nI64])
	nF64 := min(int(fn.NumRegsF64), MaxF64Regs)
	clear(jf.regsF64[:nF64])
	nCell := min(int(fn.NumRegsCell), MaxCellRegs)
	clear(jf.regsCell[:nCell])
	// Per-call arena mark/restore: snapshot before the trampoline,
	// restore on clean return. Deopt skips the restore so the spilled
	// vm.deopt* handles stay valid for the interp's resume.
	var marks vm3.CallScopeMarks
	vm.Arenas().SnapshotForJITEntry(&marks)
	// Lay out params per ParamBanks position-indexed convention: argsX[k]
	// is meaningful iff fn.ParamBanks[k] == BankX. For i64-only callees
	// argsCell/argsF64 are nil and we use the fast path: copy argsI64
	// directly into regsI64[0..]. For mixed-bank callees walk ParamBanks
	// and drop each arg into its bank's regs<X>[k] slot.
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
		// Deopt resumes in the interp at PC=0 with the spilled state,
		// including any Cell handles allocated by the JIT before deopt.
		// Leave the arenas at their post-call lengths so those handles
		// remain valid; the resumed interp frame will set its own marks
		// and reclaim as normal on its own Return.
		return 0, true, nil
	}
	// Clean unboxed return: reclaim any arena slot allocated during the
	// JIT'd call. Safe because admitted Cell-bank callees return
	// unboxed scalars; no handle escapes the call.
	vm.Arenas().RestoreUnboxedReturn(&marks)
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
	// Phase 6.2d.2.b step 2: detect the lists_fill_sum main shape so the
	// lowerer can emit zero words for PC=0's OpNewList. Setting this
	// before CompileInProgram lets the lowerer's wordCountARM64 pass and
	// emitInstrARM64 both see the flag at the same time. If the function
	// fails admission for some other reason we still leave the flag set;
	// it is read only by jitCall, which only fires when JITCode is non-
	// nil (i.e. when admission did succeed).
	fn.JITPreAllocList = canPreAllocList(fn)
	cf, err := CompileInProgram(prog, idx)
	if err != nil {
		fn.JITPreAllocList = false
		return nil, err
	}
	fn.JITCode = cf.Entry()
	fn.JITHasF64 = fn.NumRegsF64 > 0
	return cf, nil
}

// canPreAllocList reports whether the JIT should pre-allocate the list
// produced by fn.Code[0] in the Go-side jitCall and skip its lowering.
// Pre-condition for safety:
//
//   - fn.Code[0] is an OpNewList that writes to regsCell[A] with A in
//     range for the Cell-bank window.
//   - No other op in fn writes to regsCell[A] (otherwise the JIT would
//     overwrite the pre-allocated handle without the lowerer seeing
//     the data-flow).
//
// The Cell-bank admissibility check in compile.go further requires
// that fn has no list ops in its body (so x20 stays free for the
// cross-fn arena-ctx stash). That, combined with this pre-alloc check,
// is sufficient for the `lists_fill_sum` main shape: OpNewList then a
// pair of OpCallMixed sites that only read regsCell[A].
func canPreAllocList(fn *vm3.Function) bool {
	if len(fn.Code) == 0 || fn.Code[0].Code != vm3.OpNewList {
		return false
	}
	dest := fn.Code[0].A
	if int(dest) >= int(fn.NumRegsCell) || int(dest) >= MaxCellRegs {
		return false
	}
	for i := 1; i < len(fn.Code); i++ {
		op := fn.Code[i]
		if op.Code == vm3.OpNewList && op.A == dest {
			return false
		}
		if op.Code == vm3.OpNewMap && op.A == dest {
			return false
		}
	}
	return true
}

// CompileProgram is the convenience entry point that walks every
// function in prog and tries to JIT-cache it. Unsupported functions
// are skipped silently (consistent with MEP-39 §6.15 vm2runner
// behavior). The returned slice holds the CompiledFunc handles in
// declaration order; nil entries indicate functions the JIT could
// not compile on this host.
//
// Compile order is two-pass to satisfy the cross-fn OpCallMixed
// admission (Phase 6.2d.2.b step 1): the caller's site needs
// callee.JITCode set so the BLR sequence can encode the absolute
// branch address. Pass 1 compiles every fn that has no cross-fn
// OpCallMixed in its body (leaves and self-recursive callees); pass 2
// compiles the rest. Mutual recursion via OpCallMixed (A calls B,
// B calls A) is not admitted by step 1: pass 1 skips both since each
// has a cross-fn site, and pass 2 finds neither callee with JITCode
// set; the JIT falls back to the interpreter for both. The two-pass
// scheme is sufficient for the lists_fill_sum shape (main calls fill
// then sum, neither callee calls main back), which is the only
// cross-fn pattern admitted in step 1.
func CompileProgram(prog *vm3.Program) []*CompiledFunc {
	out := make([]*CompiledFunc, len(prog.Funcs))
	for i, fn := range prog.Funcs {
		if hasAnyCrossFnCallMixed(fn, i) {
			continue
		}
		cf, _ := CompileAndCache(prog, uint32(i))
		out[i] = cf
	}
	for i, fn := range prog.Funcs {
		if !hasAnyCrossFnCallMixed(fn, i) {
			continue
		}
		cf, _ := CompileAndCache(prog, uint32(i))
		out[i] = cf
	}
	return out
}

// hasAnyCrossFnCallMixed reports whether fn (at index selfIdx in its
// containing Program) issues at least one OpCallMixed to a callee
// other than itself. Used by CompileProgram to split pass 1 (leaves)
// from pass 2 (callers).
func hasAnyCrossFnCallMixed(fn *vm3.Function, selfIdx int) bool {
	for _, op := range fn.Code {
		if op.Code != vm3.OpCallMixed {
			continue
		}
		if int(uint16(op.C)) == selfIdx {
			continue
		}
		return true
	}
	return false
}
