// Package bench's GC correctness suite for the vm2 register file.
//
// This file pins the reachability contract for vm2's MEP-36 Phase 3
// world: containers (lists, maps, strings) reach the host GC via
// typed pointers carried in Cell.Obj, not via a side Objects[]
// registry. The tests cover per-Run isolation, idempotency of
// multiple Run() calls on the same VM, independence of separate VMs,
// and reclamation of container payloads once their owning VM is
// dropped and the GC runs.
//
// Each test compiles a real Mochi program through compiler2 and
// executes it against vm2 so the assertions cover the on-the-wire
// behavior, not synthetic VM state.
package bench

import (
	"runtime"
	"testing"
	"time"

	"mochi/compiler2/corpus"
	vm2 "mochi/runtime/vm2"
)

// TestMultipleRunsIndependentResults exercises cross-Run isolation:
// running the same program twice on the same VM must yield the same
// result, with no state from the first run leaking into the second.
func TestMultipleRunsIndependentResults(t *testing.T) {
	for _, p := range corpus.All() {
		t.Run(p.Name, func(t *testing.T) {
			n := sizeFor(p.Name)
			prog := compileCorpus(t, p, n)
			want := p.Expect(n)

			vm := vm2.New(prog)
			for i := range 3 {
				got, err := vm.Run()
				if err != nil {
					t.Fatalf("run %d: %v", i, err)
				}
				if got.Int() != want {
					t.Fatalf("run %d: %s(%d) = %d, want %d", i, p.Name, n, got.Int(), want)
				}
			}
		})
	}
}

// TestSeparateVMsIndependent verifies two VMs on the same Program
// produce the same result. This is the invariant for embedding VMs in
// long-running servers.
func TestSeparateVMsIndependent(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))

	a := vm2.New(prog)
	b := vm2.New(prog)

	gotA, err := a.Run()
	if err != nil {
		t.Fatalf("a.Run: %v", err)
	}
	gotB, err := b.Run()
	if err != nil {
		t.Fatalf("b.Run: %v", err)
	}
	if gotA != gotB {
		t.Fatalf("independent VMs produced different results: a=%v b=%v", gotA, gotB)
	}
}

// TestVMDropReclaimsContainers verifies that once a VM goes out of
// scope and the GC runs, the entire VM is reclaimable — which
// transitively reclaims every container payload it referenced through
// its Stack. The finalizer is attached to the *VM itself; without
// MEP-36 Phase 3's reachability fix (typed pointer in Cell.Obj only,
// no side registry pinning), the finalizer would never run.
func TestVMDropReclaimsContainers(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))

	finalized := make(chan struct{}, 1)
	allocateAndDrop := func() {
		vm := vm2.New(prog)
		if _, err := vm.Run(); err != nil {
			t.Fatalf("run: %v", err)
		}
		runtime.SetFinalizer(vm, func(_ *vm2.VM) {
			select {
			case finalized <- struct{}{}:
			default:
			}
		})
	}
	allocateAndDrop()

	for range 5 {
		runtime.GC()
		select {
		case <-finalized:
			return
		case <-time.After(20 * time.Millisecond):
		}
	}
	t.Fatalf("VM was not finalised within budget; reachability leak suspected")
}

// TestNewVMStartsEmpty checks the entry contract of vm2.New: a fresh
// VM has no resident frames or stack slots.
func TestNewVMStartsEmpty(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "fib",
		Build:  corpus.BuildFibRec,
		Expect: corpus.ExpectFibRec,
	}, sizeFor("fib"))

	vm := vm2.New(prog)
	if got := len(vm.Stack); got != 0 {
		t.Fatalf("fresh VM has Stack len=%d, want 0", got)
	}
	if got := len(vm.Frames); got != 0 {
		t.Fatalf("fresh VM has Frames len=%d, want 0", got)
	}
}
