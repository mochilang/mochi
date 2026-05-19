package vm3

import (
	"errors"
	"fmt"
	"math"
)

// VM is a vm3 interpreter instance.
//
// Three typed register stacks (stackI64/F64/Cell) replace per-frame
// register slices. Each activation reserves NumRegs* contiguous slots
// in the relevant stacks; popFrame trims them back. Frames stores
// activations as flat records keyed by stack-base indices, so the call
// path does no per-call heap allocation. This mirrors the stack layout
// vm2 settled on (single Cell stack + slim frame records) but extended
// to three typed banks so static-type dispatch keeps payloads unboxed.
type VM struct {
	arenas Arenas
	prog   *Program

	stackI64  []int64
	stackF64  []float64
	stackCell []Cell
	frames    []Frame

	// Deopt-resume scratch: when a JIT'd callee deopts, jitCall writes
	// the callee's spilled register state here (via DeoptScratchI64 /
	// DeoptScratchF64 / DeoptScratchCell). OpCallI64 / OpCallMixed then
	// populate the freshly pushed frame from these slices instead of the
	// original args, so the interpreter resumes from PC=0 with the JIT's
	// final state (Phase 6.2d.2.c). Slices are grown lazily by the JIT
	// callback; the interpreter reads len(deoptCell) > 0 etc. via the
	// fn.NumRegsX prefix when copying.
	deoptI64  []int64
	deoptF64  []float64
	deoptCell []Cell

	// jitState is an opaque handle to a per-VM JIT scratch frame
	// (vm3jit's *jitFrame3, type-erased to break the import cycle).
	// Lazily populated on first JIT call so VMs that never hit the
	// JIT pay nothing. Reused across every OpCallMixed/OpCallI64 ->
	// JITCallFn dispatch within the VM, replacing the prior
	// sync.Pool path so the hot list/map kernels skip the pool
	// Get/Put cost (~20 ns/call on Apple M4).
	jitState any

	// jitScratchListIdx is the ArenaList slab index of the per-VM
	// "scratch list" slot reused across every JIT entry that needs a
	// PC=0 OpNewList pre-alloc (Phase 6.2d.2.b step 2.E). -1 sentinel
	// means uninitialized. Created once on first call by
	// EnsureScratchList; never returned to freeLists, so the slot
	// stays under every subsequent SnapshotForJITEntry mark and the
	// cells backing array is retained across iterations. This avoids
	// the per-iter slab truncate + cells re-make cycle that step 2.D
	// otherwise paid every iteration.
	jitScratchListIdx int32
}

// JITState returns the per-VM JIT scratch handle, or nil if none has
// been attached yet. Type-erased so the vm3 package does not need to
// reference vm3jit-private types.
func (vm *VM) JITState() any { return vm.jitState }

// SetJITState stores the per-VM JIT scratch handle. vm3jit's jitCall
// lazily allocates one *jitFrame3 on first use and parks it here so
// the rest of the VM's runtime reuses the same 32 KB buffer.
func (vm *VM) SetJITState(s any) { vm.jitState = s }

// DeoptScratchI64 returns the deopt-resume i64 scratch sized to n, growing
// the backing array if needed. The JIT writes spilled i64 register state
// into the returned slice before a deopt return; OpCallI64 / OpCallMixed
// in run() then copy it into the new interp frame.
func (vm *VM) DeoptScratchI64(n int) []int64 {
	if cap(vm.deoptI64) < n {
		vm.deoptI64 = make([]int64, n, max(n, 16))
	} else {
		vm.deoptI64 = vm.deoptI64[:n]
	}
	return vm.deoptI64
}

// DeoptScratchF64 mirrors DeoptScratchI64 for the f64 bank.
func (vm *VM) DeoptScratchF64(n int) []float64 {
	if cap(vm.deoptF64) < n {
		vm.deoptF64 = make([]float64, n, max(n, 8))
	} else {
		vm.deoptF64 = vm.deoptF64[:n]
	}
	return vm.deoptF64
}

// DeoptScratchCell mirrors DeoptScratchI64 for the Cell bank.
func (vm *VM) DeoptScratchCell(n int) []Cell {
	if cap(vm.deoptCell) < n {
		vm.deoptCell = make([]Cell, n, max(n, 4))
	} else {
		vm.deoptCell = vm.deoptCell[:n]
	}
	return vm.deoptCell
}

// New constructs an empty VM.
func New() *VM { return &VM{jitScratchListIdx: -1} }

// NewWithProgram constructs a VM bound to prog. OpCall* opcodes look
// up callees via prog.Funcs.
func NewWithProgram(prog *Program) *VM { return &VM{prog: prog, jitScratchListIdx: -1} }

// EnsureScratchList returns the JIT entry's per-VM "scratch list" Cell
// handle, allocating the underlying slab slot on first call. The slot
// is never returned to freeLists: subsequent calls just reset its len
// to zero and bump gen so the JIT body sees an empty list at a stable
// slab index. The cells backing array is reused (or grown if capHint
// exceeds cap) across iterations, sidestepping the truncate + re-make
// cycle that step 2.D's snapshot/restore otherwise paid. Phase
// 6.2d.2.b step 2.E.
func (vm *VM) EnsureScratchList(capHint int) Cell {
	if vm.jitScratchListIdx < 0 {
		vm.jitScratchListIdx = int32(vm.arenas.allocScratchList(capHint))
	}
	return vm.arenas.resetScratchList(uint32(vm.jitScratchListIdx), capHint)
}

// RegrowScratchList doubles the warm scratch list's cap and returns a
// fresh handle (Phase 6.2d.2.b step 2.F). Called by vm3jit after a
// StatusListGrow deopt in the PreAlloc fast path so the immediate
// retry sees a list large enough to complete its push loop without
// re-deopting. Returns the zero Cell when no scratch slot exists yet.
func (vm *VM) RegrowScratchList() Cell {
	if vm.jitScratchListIdx < 0 {
		return Cell(0)
	}
	return vm.arenas.regrowScratchList(uint32(vm.jitScratchListIdx))
}

// Arenas returns the VM's arena state.
func (vm *VM) Arenas() *Arenas { return &vm.arenas }

// Program returns the Program this VM was constructed with, or nil.
func (vm *VM) Program() *Program { return vm.prog }

// ErrDivByZero is returned by OpDivI64 / OpModI64 when the divisor is 0.
var ErrDivByZero = errors.New("vm3: integer division by zero")

// ErrUnknownOp is returned when the interpreter loop encounters an
// opcode not yet implemented in the current phase.
var ErrUnknownOp = errors.New("vm3: unknown opcode")

// Run executes fn from PC=0 with no parameters and returns the result
// Cell.
func (vm *VM) Run(fn *Function) (Cell, error) {
	return vm.RunWithArgs(fn, nil)
}

// RunWithArgs executes fn with i64 arguments laid into regsI64[0..]
// before dispatch. Used by tests and bench harness.
//
// When fn has been JIT'd (fn.JITCode != nil and the global JITCallFn is
// installed) the entry function itself is routed through the trampoline
// instead of pushing an interpreter frame. This is the Phase 6.2d.2.b
// step 2 path that admits whole-program JIT for kernels whose main
// dispatches into JIT'd callees (e.g. `lists_fill_sum`): the bench
// harness then skips the interp loop entirely for the entry function.
// On a clean return the trampoline's i64 result Cell is returned via
// CInt; for ResultBank=BankF64 the bits decode back through Float64frombits.
// On a deopt the interpreter is re-entered with the JIT's spilled state
// copied into the entry frame's register window via the deopt-resume
// protocol, so the kernel keeps making progress.
func (vm *VM) RunWithArgs(fn *Function, argsI64 []int64) (Cell, error) {
	vm.frames = vm.frames[:0]
	vm.stackI64 = vm.stackI64[:0]
	vm.stackF64 = vm.stackF64[:0]
	vm.stackCell = vm.stackCell[:0]
	if fn.JITCode != nil && JITCallFn != nil {
		bits, deopt, err := JITCallFn(vm, fn, argsI64, nil, nil)
		if err != nil {
			return Cell(0), err
		}
		if !deopt {
			switch fn.ResultBank {
			case BankF64:
				return CFloat(math.Float64frombits(bits)), nil
			case BankCell:
				return Cell(bits), nil
			default:
				return CInt(int64(bits)), nil
			}
		}
		vm.pushFrame(fn, 0, BankI64)
		regsI64 := vm.stackI64[vm.frames[0].baseI64 : vm.frames[0].baseI64+int(fn.NumRegsI64)]
		if n := min(len(vm.deoptI64), len(regsI64)); n > 0 {
			copy(regsI64[:n], vm.deoptI64[:n])
		}
		if fn.NumRegsF64 > 0 {
			regsF64 := vm.stackF64[vm.frames[0].baseF64 : vm.frames[0].baseF64+int(fn.NumRegsF64)]
			if n := min(len(vm.deoptF64), len(regsF64)); n > 0 {
				copy(regsF64[:n], vm.deoptF64[:n])
			}
		}
		if fn.NumRegsCell > 0 {
			regsCell := vm.stackCell[vm.frames[0].baseCell : vm.frames[0].baseCell+int(fn.NumRegsCell)]
			if n := min(len(vm.deoptCell), len(regsCell)); n > 0 {
				copy(regsCell[:n], vm.deoptCell[:n])
			}
		}
		return vm.run()
	}
	vm.pushFrame(fn, 0, BankI64)
	if len(argsI64) > 0 {
		copy(vm.stackI64[vm.frames[0].baseI64:], argsI64)
	}
	return vm.run()
}

// pushFrame reserves register slots for fn on each typed stack and
// records the activation. retReg/retBank are the caller's return slot
// (ignored for the bottom frame).
func (vm *VM) pushFrame(fn *Function, retReg uint16, retBank Bank) {
	bi := len(vm.stackI64)
	bf := len(vm.stackF64)
	bc := len(vm.stackCell)
	vm.stackI64 = growI64(vm.stackI64, bi+int(fn.NumRegsI64))
	vm.stackF64 = growF64(vm.stackF64, bf+int(fn.NumRegsF64))
	vm.stackCell = growCell(vm.stackCell, bc+int(fn.NumRegsCell))
	vm.frames = append(vm.frames, Frame{
		fn:       fn,
		baseI64:  bi,
		baseF64:  bf,
		baseCell: bc,
		retReg:   retReg,
		retBank:  retBank,
	})
	// Layer A: snapshot arena lengths so an unboxed Return can drop
	// every slot allocated in this activation.
	fr := &vm.frames[len(vm.frames)-1]
	vm.arenas.snapshotMarks(&fr.marks, &fr.freeMarks)
}

func growI64(s []int64, need int) []int64 {
	if cap(s) >= need {
		return s[:need]
	}
	newCap := max(2*cap(s), need, 64)
	ns := make([]int64, need, newCap)
	copy(ns, s)
	return ns
}

func growF64(s []float64, need int) []float64 {
	if cap(s) >= need {
		return s[:need]
	}
	newCap := max(2*cap(s), need, 32)
	ns := make([]float64, need, newCap)
	copy(ns, s)
	return ns
}

func growCell(s []Cell, need int) []Cell {
	if cap(s) >= need {
		return s[:need]
	}
	newCap := max(2*cap(s), need, 32)
	ns := make([]Cell, need, newCap)
	copy(ns, s)
	return ns
}

// run is the dispatch loop. To stay fast on tight loops we hoist all
// frame-derived locals (code, pc, regsI64/F64/Cell, consts) above the
// switch and only refresh them at frame-change points (call, tailcall,
// return). Bounds checks on the register banks become cheap because
// the slices have a fixed length per activation.
func (vm *VM) run() (Cell, error) {
	top := len(vm.frames) - 1
	fr := &vm.frames[top]
	fn := fr.fn
	code := fn.Code
	pc := fr.pc
	regsI64 := vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
	regsF64 := vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
	regsCell := vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
	consts := fn.Consts
	arenas := &vm.arenas

	for {
		op := code[pc]
		switch op.Code {
		case OpNop:
			pc++

		case OpMovI64:
			regsI64[op.A] = regsI64[op.B]
			pc++
		case OpMovF64:
			regsF64[op.A] = regsF64[op.B]
			pc++
		case OpConstI64K:
			regsI64[op.A] = int64(op.C)
			pc++
		case OpConstI64KW:
			regsI64[op.A] = consts[uint16(op.C)].Int()
			pc++
		case OpConstF64K:
			regsF64[op.A] = consts[uint16(op.C)].Float()
			pc++

		case OpAddI64:
			regsI64[op.A] = regsI64[op.B] + regsI64[uint16(op.C)]
			pc++
		case OpSubI64:
			regsI64[op.A] = regsI64[op.B] - regsI64[uint16(op.C)]
			pc++
		case OpMulI64:
			regsI64[op.A] = regsI64[op.B] * regsI64[uint16(op.C)]
			pc++
		case OpDivI64:
			d := regsI64[uint16(op.C)]
			if d == 0 {
				fr.pc = pc
				return CNull(), ErrDivByZero
			}
			regsI64[op.A] = regsI64[op.B] / d
			pc++
		case OpModI64:
			d := regsI64[uint16(op.C)]
			if d == 0 {
				fr.pc = pc
				return CNull(), ErrDivByZero
			}
			regsI64[op.A] = regsI64[op.B] % d
			pc++
		case OpNegI64:
			regsI64[op.A] = -regsI64[op.B]
			pc++
		case OpAddI64K:
			regsI64[op.A] = regsI64[op.B] + int64(op.C)
			pc++
		case OpSubI64K:
			regsI64[op.A] = regsI64[op.B] - int64(op.C)
			pc++
		case OpMulI64K:
			regsI64[op.A] = regsI64[op.B] * int64(op.C)
			pc++
		case OpDivI64K:
			if op.C == 0 {
				fr.pc = pc
				return CNull(), ErrDivByZero
			}
			regsI64[op.A] = regsI64[op.B] / int64(op.C)
			pc++
		case OpModI64K:
			if op.C == 0 {
				fr.pc = pc
				return CNull(), ErrDivByZero
			}
			regsI64[op.A] = regsI64[op.B] % int64(op.C)
			pc++

		case OpAddF64:
			regsF64[op.A] = regsF64[op.B] + regsF64[uint16(op.C)]
			pc++
		case OpSubF64:
			regsF64[op.A] = regsF64[op.B] - regsF64[uint16(op.C)]
			pc++
		case OpMulF64:
			regsF64[op.A] = regsF64[op.B] * regsF64[uint16(op.C)]
			pc++
		case OpDivF64:
			regsF64[op.A] = regsF64[op.B] / regsF64[uint16(op.C)]
			pc++
		case OpNegF64:
			regsF64[op.A] = -regsF64[op.B]
			pc++

		case OpCmpEqI64Br:
			if regsI64[op.A] == regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpNeI64Br:
			if regsI64[op.A] != regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLtI64Br:
			if regsI64[op.A] < regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLeI64Br:
			if regsI64[op.A] <= regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGtI64Br:
			if regsI64[op.A] > regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGeI64Br:
			if regsI64[op.A] >= regsI64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}

		case OpCmpEqI64KBr:
			if regsI64[op.A] == int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpNeI64KBr:
			if regsI64[op.A] != int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLtI64KBr:
			if regsI64[op.A] < int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLeI64KBr:
			if regsI64[op.A] <= int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGtI64KBr:
			if regsI64[op.A] > int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGeI64KBr:
			if regsI64[op.A] >= int64(int16(op.B)) {
				pc = int(uint16(op.C))
			} else {
				pc++
			}

		case OpCmpEqF64Br:
			if regsF64[op.A] == regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpNeF64Br:
			if regsF64[op.A] != regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLtF64Br:
			if regsF64[op.A] < regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpLeF64Br:
			if regsF64[op.A] <= regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGtF64Br:
			if regsF64[op.A] > regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}
		case OpCmpGeF64Br:
			if regsF64[op.A] >= regsF64[op.B] {
				pc = int(uint16(op.C))
			} else {
				pc++
			}

		case OpI64ToF64:
			regsF64[op.A] = float64(regsI64[op.B])
			pc++
		case OpF64ToI64:
			regsI64[op.A] = int64(regsF64[op.B])
			pc++

		case OpJump:
			pc = int(uint16(op.C))

		case OpCallI64:
			callee := vm.prog.Funcs[uint16(op.C)]
			n := callee.NumI64Params()
			// regsI64 is a window into stackI64 starting at fr.baseI64.
			// pushFrame may grow stackI64 (move the backing array), so
			// snapshot the args into a small local buffer first.
			var argbuf [8]int64
			var args []int64
			if n <= len(argbuf) {
				args = argbuf[:n]
			} else {
				args = make([]int64, n)
			}
			copy(args, regsI64[op.B:op.B+uint16(n)])
			// Phase 6.2c: if the callee has been JIT'd, route through
			// the trampoline instead of pushing an interpreter frame.
			// The JIT's recursive self-calls live on the JIT's own
			// stack and do not touch vm.frames. On deopt jitCall writes
			// the callee's spilled register state into vm.deopt*; we
			// pick it up below to populate the resume frame so the
			// interpreter starts at PC=0 with the JIT's final state
			// (Phase 6.2d.2.c deopt-resume protocol).
			resumeFromDeopt := false
			if callee.JITCode != nil && JITCallFn != nil {
				bits, deopt, err := JITCallFn(vm, callee, args, nil, nil)
				if err != nil {
					return Cell(0), err
				}
				if !deopt {
					regsI64[op.A] = int64(bits)
					pc++
					continue
				}
				resumeFromDeopt = true
			}
			// Stash caller state so we resume one past the call site.
			fr.pc = pc + 1
			vm.pushFrame(callee, op.A, BankI64)
			top++
			fr = &vm.frames[top]
			fn = callee
			code = fn.Code
			pc = 0
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			if resumeFromDeopt {
				copy(regsI64, vm.deoptI64)
			} else {
				copy(regsI64, args)
			}
		case OpCallF64:
			callee := vm.prog.Funcs[uint16(op.C)]
			n := callee.NumF64Params()
			var argbuf [8]float64
			var args []float64
			if n <= len(argbuf) {
				args = argbuf[:n]
			} else {
				args = make([]float64, n)
			}
			copy(args, regsF64[op.B:op.B+uint16(n)])
			fr.pc = pc + 1
			vm.pushFrame(callee, op.A, BankF64)
			top++
			fr = &vm.frames[top]
			fn = callee
			code = fn.Code
			pc = 0
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			copy(regsF64, args)
		case OpTailCallI64:
			callee := vm.prog.Funcs[uint16(op.C)]
			n := callee.NumI64Params()
			var argbuf [8]int64
			var args []int64
			if n <= len(argbuf) {
				args = argbuf[:n]
			} else {
				args = make([]int64, n)
			}
			copy(args, regsI64[op.B:op.B+uint16(n)])
			fr.fn = callee
			fn = callee
			code = fn.Code
			pc = 0
			// Resize this frame's typed windows; bases stay.
			end := fr.baseI64 + int(callee.NumRegsI64)
			vm.stackI64 = growI64(vm.stackI64, end)
			regsI64 = vm.stackI64[fr.baseI64:end]
			clear(regsI64)
			copy(regsI64, args)
			endF := fr.baseF64 + int(callee.NumRegsF64)
			vm.stackF64 = growF64(vm.stackF64, endF)
			regsF64 = vm.stackF64[fr.baseF64:endF]
			clear(regsF64)
			endC := fr.baseCell + int(callee.NumRegsCell)
			vm.stackCell = growCell(vm.stackCell, endC)
			regsCell = vm.stackCell[fr.baseCell:endC]
			clear(regsCell)
			consts = fn.Consts

		case OpReturnI64:
			ret := regsI64[op.A]
			retReg := fr.retReg
			retBank := fr.retBank
			// Layer A: unboxed return, so any handle allocated in this
			// activation is dead. Truncate arenas back to the marks
			// captured at pushFrame before clearing the cell window.
			arenas.truncateToMarks(&fr.marks, &fr.freeMarks)
			// Pop the current frame; stacks are sliced back to the
			// caller's high-water mark. Don't bother clearing i64/f64
			// (no GC implications); clear cell to release Go-heap refs
			// reachable from this frame.
			if fn.NumRegsCell > 0 {
				clear(regsCell)
			}
			vm.stackI64 = vm.stackI64[:fr.baseI64]
			vm.stackF64 = vm.stackF64[:fr.baseF64]
			vm.stackCell = vm.stackCell[:fr.baseCell]
			vm.frames = vm.frames[:top]
			top--
			if top < 0 {
				return CInt(ret), nil
			}
			fr = &vm.frames[top]
			fn = fr.fn
			code = fn.Code
			pc = fr.pc
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			switch retBank {
			case BankI64:
				regsI64[retReg] = ret
			case BankF64:
				regsF64[retReg] = float64(ret)
			default:
				regsCell[retReg] = CInt(ret)
			}
		case OpReturnF64:
			ret := regsF64[op.A]
			retReg := fr.retReg
			retBank := fr.retBank
			arenas.truncateToMarks(&fr.marks, &fr.freeMarks)
			if fn.NumRegsCell > 0 {
				clear(regsCell)
			}
			vm.stackI64 = vm.stackI64[:fr.baseI64]
			vm.stackF64 = vm.stackF64[:fr.baseF64]
			vm.stackCell = vm.stackCell[:fr.baseCell]
			vm.frames = vm.frames[:top]
			top--
			if top < 0 {
				return CFloat(ret), nil
			}
			fr = &vm.frames[top]
			fn = fr.fn
			code = fn.Code
			pc = fr.pc
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			switch retBank {
			case BankF64:
				regsF64[retReg] = ret
			case BankI64:
				regsI64[retReg] = int64(ret)
			default:
				regsCell[retReg] = CFloat(ret)
			}
		case OpReturnCell:
			ret := regsCell[op.A]
			retReg := fr.retReg
			// Layer B: handle-aware copy-up. The returned Cell may be a
			// handle into the local arena range; relocate it to slot
			// marks[tag] so the rest of the frame's allocations can be
			// truncated away. Unboxed Cell payloads and external handles
			// trigger the same Layer A truncate path used by OpReturnI64.
			ret = arenas.handleCellReturn(ret, &fr.marks, &fr.freeMarks)
			if fn.NumRegsCell > 0 {
				clear(regsCell)
			}
			vm.stackI64 = vm.stackI64[:fr.baseI64]
			vm.stackF64 = vm.stackF64[:fr.baseF64]
			vm.stackCell = vm.stackCell[:fr.baseCell]
			vm.frames = vm.frames[:top]
			top--
			if top < 0 {
				return ret, nil
			}
			fr = &vm.frames[top]
			fn = fr.fn
			code = fn.Code
			pc = fr.pc
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			regsCell[retReg] = ret
		case OpReturnConstK:
			ret := int64(op.C)
			retReg := fr.retReg
			arenas.truncateToMarks(&fr.marks, &fr.freeMarks)
			if fn.NumRegsCell > 0 {
				clear(regsCell)
			}
			vm.stackI64 = vm.stackI64[:fr.baseI64]
			vm.stackF64 = vm.stackF64[:fr.baseF64]
			vm.stackCell = vm.stackCell[:fr.baseCell]
			vm.frames = vm.frames[:top]
			top--
			if top < 0 {
				return CInt(ret), nil
			}
			fr = &vm.frames[top]
			fn = fr.fn
			code = fn.Code
			pc = fr.pc
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			regsI64[retReg] = ret

		case OpDeopt:
			fr.pc = pc
			return EncodeDeopt(int(uint16(op.C))), nil

		case OpNewList:
			// op.C is the cap hint (0 if unknown). A known cap pre-grows
			// the cells slice so a JIT'd push fast-path never deopts on
			// grow for a kernel whose final size is known at allocation.
			regsCell[op.A] = arenas.AllocList(0, int(op.C))
			pc++
		case OpListLenI64:
			regsI64[op.A] = int64(arenas.ListLen(regsCell[op.B]))
			pc++
		case OpListPushI64:
			lst := regsCell[op.A]
			_, _, idx := lst.DecodeHandle()
			l := &arenas.Lists[idx]
			l.cells = append(l.cells, CInt(regsI64[op.B]))
			l.len = uint32(len(l.cells))
			pc++
		case OpListGetI64:
			lst := regsCell[op.B]
			_, _, idx := lst.DecodeHandle()
			regsI64[op.A] = arenas.Lists[idx].cells[regsI64[uint16(op.C)]].Int()
			pc++
		case OpListSetI64:
			lst := regsCell[op.A]
			_, _, idx := lst.DecodeHandle()
			arenas.Lists[idx].cells[regsI64[uint16(op.C)]] = CInt(regsI64[op.B])
			pc++

		case OpNewMap:
			regsCell[op.A] = arenas.AllocMap(int(uint16(op.C)))
			pc++
		case OpMapSetI64I64:
			arenas.MapSetI64(regsCell[op.A], regsI64[op.B], regsI64[uint16(op.C)])
			pc++
		case OpMapGetI64I64:
			regsI64[op.A] = arenas.MapGetI64(regsCell[op.B], regsI64[uint16(op.C)])
			pc++

		case OpConstStrKW:
			regsCell[op.A] = consts[uint16(op.C)]
			pc++
		case OpLenStr:
			c := regsCell[op.B]
			if c.IsSStr() {
				regsI64[op.A] = int64(c.SStrLen())
			} else {
				regsI64[op.A] = int64(len(arenas.StringBytes(c)))
			}
			pc++
		case OpConcatStr:
			lhs := regsCell[op.B]
			rhs := regsCell[uint16(op.C)]
			var lbuf, rbuf [MaxInlineStr]byte
			var lb, rb []byte
			if lhs.IsSStr() {
				lb = lhs.SStrBytes(&lbuf)
			} else {
				lb = arenas.StringBytes(lhs)
			}
			if rhs.IsSStr() {
				rb = rhs.SStrBytes(&rbuf)
			} else {
				rb = arenas.StringBytes(rhs)
			}
			nlen := len(lb) + len(rb)
			if nlen <= MaxInlineStr {
				var merged [MaxInlineStr]byte
				copy(merged[:], lb)
				copy(merged[len(lb):], rb)
				regsCell[op.A] = CSStr(merged[:nlen])
			} else {
				regsCell[op.A] = arenas.AllocStringConcat(lb, rb)
			}
			pc++

		case OpCallMixed:
			callee := vm.prog.Funcs[uint16(op.C)]
			pbs := callee.ParamBanks
			// Snapshot args before pushFrame (which may regrow slabs).
			var argI64 [8]int64
			var argF64 [8]float64
			var argCell [8]Cell
			argBase := op.B
			for k, b := range pbs {
				idx := argBase + uint16(k)
				switch b {
				case BankI64:
					argI64[k] = regsI64[idx]
				case BankF64:
					argF64[k] = regsF64[idx]
				case BankCell:
					argCell[k] = regsCell[idx]
				}
			}
			// Phase 6.2d.2.b: if the callee has been JIT'd, route through
			// the trampoline. The JIT's pinned regs are loaded from the
			// argsI64/argsF64/argsCell slices in jitCall, matching the
			// same param-bank position-indexed convention used by the
			// interp's pushFrame path. On deopt jitCall writes the
			// callee's spilled register state into vm.deopt*; we pick
			// it up below to populate the resume frame so the
			// interpreter starts at PC=0 with the JIT's final state
			// (Phase 6.2d.2.c deopt-resume protocol).
			resumeFromDeopt := false
			if callee.JITCode != nil && JITCallFn != nil {
				retBank := Bank(op.BankFlags & 0x3)
				bits, deopt, err := JITCallFn(vm, callee, argI64[:len(pbs)], argF64[:len(pbs)], argCell[:len(pbs)])
				if err != nil {
					return Cell(0), err
				}
				if !deopt {
					switch retBank {
					case BankI64:
						regsI64[op.A] = int64(bits)
					case BankF64:
						regsF64[op.A] = math.Float64frombits(bits)
					case BankCell:
						regsCell[op.A] = Cell(bits)
					}
					pc++
					continue
				}
				resumeFromDeopt = true
			}
			fr.pc = pc + 1
			retBank := Bank(op.BankFlags & 0x3)
			vm.pushFrame(callee, op.A, retBank)
			top++
			fr = &vm.frames[top]
			fn = callee
			code = fn.Code
			pc = 0
			regsI64 = vm.stackI64[fr.baseI64 : fr.baseI64+int(fn.NumRegsI64)]
			regsF64 = vm.stackF64[fr.baseF64 : fr.baseF64+int(fn.NumRegsF64)]
			regsCell = vm.stackCell[fr.baseCell : fr.baseCell+int(fn.NumRegsCell)]
			consts = fn.Consts
			if resumeFromDeopt {
				copy(regsI64, vm.deoptI64)
				copy(regsF64, vm.deoptF64)
				copy(regsCell, vm.deoptCell)
			} else {
				for k, b := range pbs {
					switch b {
					case BankI64:
						regsI64[k] = argI64[k]
					case BankF64:
						regsF64[k] = argF64[k]
					case BankCell:
						regsCell[k] = argCell[k]
					}
				}
			}

		case OpTailCallMixed:
			callee := vm.prog.Funcs[uint16(op.C)]
			// Self-tail-call with arg-base 0: args already live in the
			// canonical regs<bank>[k] positions matching ParamBanks[k].
			// No copies / clears needed; just rewind PC.
			if callee == fn && op.B == 0 {
				pc = 0
				continue
			}
			pbs := callee.ParamBanks
			var argI64 [8]int64
			var argF64 [8]float64
			var argCell [8]Cell
			argBase := op.B
			for k, b := range pbs {
				idx := argBase + uint16(k)
				switch b {
				case BankI64:
					argI64[k] = regsI64[idx]
				case BankF64:
					argF64[k] = regsF64[idx]
				case BankCell:
					argCell[k] = regsCell[idx]
				}
			}
			fr.fn = callee
			fn = callee
			code = fn.Code
			pc = 0
			endI := fr.baseI64 + int(callee.NumRegsI64)
			vm.stackI64 = growI64(vm.stackI64, endI)
			regsI64 = vm.stackI64[fr.baseI64:endI]
			clear(regsI64)
			endF := fr.baseF64 + int(callee.NumRegsF64)
			vm.stackF64 = growF64(vm.stackF64, endF)
			regsF64 = vm.stackF64[fr.baseF64:endF]
			clear(regsF64)
			endC := fr.baseCell + int(callee.NumRegsCell)
			vm.stackCell = growCell(vm.stackCell, endC)
			regsCell = vm.stackCell[fr.baseCell:endC]
			clear(regsCell)
			consts = fn.Consts
			for k, b := range pbs {
				switch b {
				case BankI64:
					regsI64[k] = argI64[k]
				case BankF64:
					regsF64[k] = argF64[k]
				case BankCell:
					regsCell[k] = argCell[k]
				}
			}

		default:
			fr.pc = pc
			return CNull(), fmt.Errorf("%w: %d at PC=%d in %s",
				ErrUnknownOp, op.Code, pc, fn.Name)
		}
	}
}
