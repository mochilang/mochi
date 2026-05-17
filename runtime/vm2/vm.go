package vm2

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
}

// New constructs a VM bound to a program. Stack and Frames are
// pre-allocated to amortize the first-call grow across the typical
// nesting depths we see in the benchmark corpus.
func New(p *Program) *VM {
	return &VM{
		Program: p,
		Stack:   make([]Cell, 0, 64),
		Frames:  make([]frame, 0, 16),
	}
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
	fr := vm.Frames[top]
	base := fr.RegsBase
	if fr.Fn.HasContainerSlots {
		clear(vm.Stack[base:])
	}
	vm.Stack = vm.Stack[:base]
	vm.Frames[top] = frame{} // drop the *Function pointer
	vm.Frames = vm.Frames[:top]
}
