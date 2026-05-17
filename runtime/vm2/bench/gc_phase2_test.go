// Phase 2/3 GC tests for MEP-36.
//
// These tests pin the memory-management properties that Phase 2 and
// Phase 3 of MEP-36 jointly deliver:
//
//  1. Container constructors do not populate a side Objects[] table:
//     containers are reachable from the operand stack via Cell.Obj
//     only. The field has been removed entirely in Phase 3; what the
//     tests here actually prove is the live-heap reclamation contract
//     that motivated the removal.
//  2. popFrame clear()s the popped window when the frame held a
//     container, so a container that was the callee's last reference
//     becomes reclaimable on the next GC. Phase 3 skips the clear for
//     int-only frames; the reclamation contract still holds.
//  3. Repeated Run() calls do not grow live heap: dead containers from
//     a previous Run are collected before the next steady-state sample.
//  4. Dropping the VM lets the GC reclaim everything the VM held.
package bench

import (
	"runtime"
	"testing"

	"mochi/compiler2/corpus"
	vm2 "mochi/runtime/vm2"
)

// TestPhase2_PopFrameReclamation pins property (2): a container
// allocated inside a callee frame becomes reclaimable once the callee
// returns. We exercise this indirectly by running a corpus program
// that builds a list inside a function, returns its length, and
// discards the list; then we drive enough GC cycles to flush the
// popped frame's slots.
func TestPhase2_PopFrameReclamation(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))

	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		t.Fatalf("warmup run: %v", err)
	}
	runtime.GC()
	var before runtime.MemStats
	runtime.ReadMemStats(&before)

	// Run 256 more times. With popFrame clearing the window, each Run's
	// list payloads should be unreachable after Run returns. Live heap
	// should stay flat across these iterations rather than monotonically
	// growing as it did when Objects[] pinned everything.
	for range 256 {
		if _, err := vm.Run(); err != nil {
			t.Fatalf("loop run: %v", err)
		}
	}
	runtime.GC()
	runtime.GC()
	var after runtime.MemStats
	runtime.ReadMemStats(&after)

	// Live heap may grow a little (Go's GC isn't precise about
	// reclaiming on demand, and there's noise from the test runtime),
	// but a regression that pinned the per-Run lists would show
	// hundreds of KB of growth on lists_fill_sum at sizeFor=128.
	growth := int64(after.HeapAlloc) - int64(before.HeapAlloc)
	const budgetBytes = 512 * 1024 // 512 KB
	if growth > budgetBytes {
		t.Fatalf("live heap grew by %d B across 256 runs (budget %d B); "+
			"popFrame clear may be missing", growth, budgetBytes)
	}
}

// TestPhase2_LiveHeapBoundedAcrossManyRuns is the steady-state form of
// the reclamation contract: live heap (HeapAlloc) stays flat across
// 128 runs of every container-heavy corpus program on a reused VM.
// A regression that pins per-Run containers would scale linearly with
// the iteration count.
func TestPhase2_LiveHeapBoundedAcrossManyRuns(t *testing.T) {
	cases := []struct {
		name string
		p    corpus.Program
	}{
		{"lists_fill_sum", corpus.Program{Name: "lists_fill_sum", Build: corpus.BuildListsFillSum, Expect: corpus.ExpectListsFillSum}},
		{"maps_fill_sum", corpus.Program{Name: "maps_fill_sum", Build: corpus.BuildMapsFillSum, Expect: corpus.ExpectMapsFillSum}},
		{"strings_concat_loop", corpus.Program{Name: "strings_concat_loop", Build: corpus.BuildStringsConcatLoop, Expect: corpus.ExpectStringsConcatLoop}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog := compileCorpus(t, tc.p, sizeFor(tc.name))
			vm := vm2.New(prog)
			if _, err := vm.Run(); err != nil {
				t.Fatalf("warmup: %v", err)
			}
			runtime.GC()
			runtime.GC()
			var base runtime.MemStats
			runtime.ReadMemStats(&base)

			for range 128 {
				if _, err := vm.Run(); err != nil {
					t.Fatalf("run: %v", err)
				}
			}
			runtime.GC()
			runtime.GC()
			var after runtime.MemStats
			runtime.ReadMemStats(&after)

			// Allow up to 4x the baseline reading. A true leak (containers
			// pinned across runs) would scale linearly with iteration
			// count; 128 iterations would blow past any small multiple.
			if after.HeapAlloc > base.HeapAlloc*4 {
				t.Fatalf("live heap grew from %d B to %d B (>4x); "+
					"container reclamation is broken on %s",
					base.HeapAlloc, after.HeapAlloc, tc.name)
			}
		})
	}
}

// TestPhase2_FreshVMReclaimableAfterDrop pins the fresh-VM embedding
// pattern (one VM per request): once the closure returns, the entire
// VM (including its Stack with all live Cells) must be reclaimable.
// We attach a finalizer to the *VM itself; without MEP-36's
// reachability fix (containers reachable only through Cell.Obj on the
// Stack, not pinned in a side registry), the finalizer would never run.
func TestPhase2_FreshVMReclaimableAfterDrop(t *testing.T) {
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

	for range 8 {
		runtime.GC()
		select {
		case <-finalized:
			return
		default:
		}
	}
	t.Fatalf("fresh VM was not finalised within budget; reachability leak suspected")
}
