package vm2

import (
	"io"
	"os"
)

// VM holds the program and the activation-record stacks. Container
// payloads (lists, maps, strings, closures) live on the Go heap and
// reach the host GC via typed pointers carried in Cell.Obj — there is
// no longer a side `Objects[]` registry to keep in sync (MEP-36
// Phase 3).
//
// Activation records live on two contiguous, dynamically grown stacks
// (Stack and Frames) rather than per-call sync.Pool allocations. A
// register VM in a managed host pays for every Pool.Get/Put — profiles
// of fib_rec showed 56% of CPU sitting in the pool path before this
// change. With contiguous stacks, call/return are two integer bumps
// and a slice reslice; the only allocation is the one-shot grow when
// the stack runs out of capacity.
type VM struct {
	Program *Program

	// Stack is the register file shared by every active frame. Frame i
	// owns Stack[Frames[i].RegsBase : Frames[i].RegsBase + Fn.NumRegs).
	Stack []Cell
	// Frames is the activation-record stack. The current frame is
	// Frames[len(Frames)-1]; the parent chain is implicit (-1 of the
	// current index). Run() snapshots the top frame's hot fields into
	// locals for dispatch.
	Frames []frame

	// pairChunks is the per-VM arena for vmPair allocations (MEP-37 §3.4).
	// Chunks grow on demand; addresses within a chunk are stable for the
	// chunk's lifetime, so vmPair* pointers carried in Cell.Obj never
	// dangle. pairNext indexes the next free slot across all chunks.
	pairChunks []*pairChunk
	pairNext   int

	// Stdout and Stdin route OpStdoutWriteBytes and OpStdinReadAll (MEP-38
	// §3.1.3). The defaults wire to os.Stdout/Stdin; test harnesses
	// replace both with bytes.Buffer for deterministic comparison
	// against a Go reference.
	Stdout io.Writer
	Stdin  io.Reader
}

// New constructs a VM bound to a program. Stack and Frames are
// pre-allocated to amortize the first-call grow across the typical
// nesting depths we see in the benchmark corpus.
func New(p *Program) *VM {
	return &VM{
		Program: p,
		Stack:   make([]Cell, 0, 64),
		Frames:  make([]frame, 0, 16),
		Stdout:  os.Stdout,
		Stdin:   os.Stdin,
	}
}

// Reset rewinds the VM's mutable state (Stack length, Frames length,
// pair-arena bump cursor) so the same VM can run Program again without
// reallocating its backing arrays. Capacity is preserved: Stack and
// Frames keep whatever they grew to on the last Run, and the pair
// arena keeps its chunk list so the next allocation skips the chunk
// alloc until pairChunks is exhausted again.
//
// This is the AOT-style "compile once, run many" entry point used by
// benchmarks (MEP-38 Appendix A.4). Calling Run twice without Reset
// works only when the program's main has no side effects, so library
// consumers that care about repeatability should always Reset between
// runs.
func (vm *VM) Reset() {
	vm.Stack = vm.Stack[:0]
	vm.Frames = vm.Frames[:0]
	vm.pairNext = 0
}

// frame is one activation record on the Frames stack. Pure value type;
// no pointers into the heap so the stack itself is cheap to grow.
type frame struct {
	Fn       *Function
	IP       int
	RegsBase int   // index into vm.Stack where this frame's regs begin
	RetReg   int32 // parent register that receives the return value
}

// PushFrame is the exported entry point used by the JIT deopt path to
// promote a JIT register file back into a real activation record before
// resuming via RunTopFrame. Identical contract to pushFrame.
func (vm *VM) PushFrame(fn *Function, retReg int32) (int, int) {
	return vm.pushFrame(fn, retReg)
}

// pushFrame grows Stack + Frames to admit a new activation. Returns
// the index of the new frame in vm.Frames and the regs base.
func (vm *VM) pushFrame(fn *Function, retReg int32) (int, int) {
	base := len(vm.Stack)
	need := base + fn.NumRegs
	if cap(vm.Stack) < need {
		// Grow geometrically. Doubling keeps amortized O(1) and means
		// deep but finite recursion only pays one or two grows.
		newCap := 2 * cap(vm.Stack)
		if newCap < need {
			newCap = need
		}
		if newCap < 64 {
			newCap = 64
		}
		ns := make([]Cell, len(vm.Stack), newCap)
		copy(ns, vm.Stack)
		vm.Stack = ns
	}
	vm.Stack = vm.Stack[:need]
	vm.Frames = append(vm.Frames, frame{Fn: fn, RegsBase: base, RetReg: retReg})
	return len(vm.Frames) - 1, base
}

// popFrame retires the top frame. The parent (if any) becomes current.
//
// MEP-36 Phase 2: zero the popped window before reslicing so any typed
// pointers carried in Cell.Obj are dropped and the host GC can reclaim
// dead containers. Without the clear, a *vmList kept alive by a retired
// register would linger until that slot is overwritten by a later frame.
//
// MEP-36 Phase 3: skip the clear when the popped function has no
// container-typed value in its register window. The flag is computed at
// emit time from the IR value-type stream; pure int/bool programs
// (fib_rec, fact_rec, fib_iter, mul_loop) recover their Phase 1 numbers
// while container-touching frames keep the reclamation contract.
func (vm *VM) popFrame() {
	top := len(vm.Frames) - 1
	fr := &vm.Frames[top]
	base := fr.RegsBase
	if fr.Fn.HasContainerSlots {
		clear(vm.Stack[base:])
	}
	vm.Stack = vm.Stack[:base]
	// The *Function pointer is reachable from vm.Program.Funcs, so the
	// GC keeps it alive regardless of what sits in this frame slot. We
	// used to zero it ("drop the *Function pointer") but that fires a
	// pointer write barrier on every return; the slot will be
	// overwritten by the next pushFrame anyway.
	vm.Frames = vm.Frames[:top]
}
