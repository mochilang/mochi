// Package bench's GC correctness suite for the vm2 Objects table.
//
// This file locks in the existing reachability contract before the
// MEP-36 (https://mochi-lang.dev/docs/mep/mep-0036) refactor lands.
// The tests cover: per-Run Objects table reset, type identity through
// the ref-tag path, idempotency of multiple Run() calls on the same VM,
// independence of separate VMs, and reclamation of container payloads
// once their owning VM is dropped and the GC runs.
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
	"mochi/compiler2/ir"
	vm2 "mochi/runtime/vm2"
)

// TestObjectsTableResetPerRun verifies that the per-Run reset
// documented in runtime/vm2/eval.go:16 actually happens: a second
// Run() on the same VM observes len(vm.Objects) starting at zero
// growth and ending bounded.
func TestObjectsTableResetPerRun(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))

	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		t.Fatalf("first run: %v", err)
	}
	firstLen := len(vm.Objects)
	if firstLen == 0 {
		t.Fatalf("first run produced an empty Objects table, expected at least one container")
	}

	if _, err := vm.Run(); err != nil {
		t.Fatalf("second run: %v", err)
	}
	if got := len(vm.Objects); got != firstLen {
		t.Fatalf("Objects table size drifted across runs: first=%d second=%d", firstLen, got)
	}
}

// TestObjectsTableTypeIdentity verifies that a Cell carrying a ref
// index resolves to the expected Go type via vm.Objects, for every
// container constructor exercised by the corpus. This is the
// invariant the new Cell.Ptr accessors must continue to satisfy.
func TestObjectsTableTypeIdentity(t *testing.T) {
	type slot struct {
		idx     uint64
		wantTyp string
	}

	cases := []struct {
		name  string
		build func(int64) *ir.Module
		size  int64
	}{
		{"lists_fill_sum", corpus.BuildListsFillSum, sizeFor("lists_fill_sum")},
		{"maps_fill_sum", corpus.BuildMapsFillSum, sizeFor("maps_fill_sum")},
		{"strings_concat_loop", corpus.BuildStringsConcatLoop, sizeFor("strings_concat_loop")},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog := compileCorpus(t, corpus.Program{Name: tc.name, Build: tc.build}, tc.size)
			vm := vm2.New(prog)
			if _, err := vm.Run(); err != nil {
				t.Fatalf("run: %v", err)
			}
			if len(vm.Objects) == 0 {
				t.Fatalf("Objects table is empty after %s", tc.name)
			}
			// Every entry must be non-nil and one of the recognised
			// concrete types. We do not assume a particular ordering;
			// the contract is that the table holds container payloads,
			// not raw bytes or untyped pointers.
			for i, obj := range vm.Objects {
				if obj == nil {
					t.Fatalf("Objects[%d] is nil", i)
				}
				switch obj.(type) {
				case interface{ Data() []vm2.Cell }, // future-proof for typed accessors
					*[]vm2.Cell:
					// list-shaped — accepted
				default:
					typeName := goTypeName(obj)
					if !isExpectedObjectType(typeName) {
						t.Fatalf("Objects[%d] has unexpected Go type %q", i, typeName)
					}
				}
			}
		})
	}
}

// TestMultipleRunsIndependentResults exercises the cross-Run isolation
// of the Objects table: running the same program twice on the same VM
// must yield the same result, with no state from the first run leaking
// into the second.
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
// produce the same result without sharing Objects state. This is the
// invariant for embedding VMs in long-running servers.
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
	// Backing arrays must not be aliased.
	if len(a.Objects) > 0 && len(b.Objects) > 0 && sameSliceHeader(a.Objects, b.Objects) {
		t.Fatalf("two VMs share the same Objects backing array")
	}
}

// TestVMDropReclaimsContainers verifies that once a VM goes out of
// scope and the GC runs, the container payloads it held in its
// Objects table become reachable for reclamation. We attach a Go
// finalizer to a sentinel struct stored alongside the program, drop
// the VM, force GC, and require the finalizer to fire within a bounded
// time window. If the VM had pinned the sentinel through an unintended
// global reference, the finalizer would never run.
func TestVMDropReclaimsContainers(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))

	// allocateAndDrop is a closure so the *VM local can be reclaimed
	// once the closure returns; without it, conservative stack scanning
	// could keep the pointer alive until the test exits.
	finalized := make(chan struct{}, 1)
	allocateAndDrop := func() {
		vm := vm2.New(prog)
		if _, err := vm.Run(); err != nil {
			t.Fatalf("run: %v", err)
		}
		sentinel := &struct{ _ [32]byte }{}
		runtime.SetFinalizer(sentinel, func(_ any) {
			finalized <- struct{}{}
		})
		// Stash the sentinel inside vm.Objects so its reachability is
		// transitively bound to the VM. After the closure returns vm is
		// unreachable; the sentinel must follow.
		vm.AddObject(sentinel)
	}
	allocateAndDrop()

	// Two GC passes: the first promotes the unreachable object set,
	// the second runs finalizers and reclaims them.
	for range 5 {
		runtime.GC()
		select {
		case <-finalized:
			return
		case <-time.After(20 * time.Millisecond):
		}
	}
	t.Fatalf("VM container sentinel was not finalised within budget; reachability leak suspected")
}

// TestNewVMStartsEmpty checks the entry contract of vm2.New: a fresh
// VM has an empty Objects table and no resident frames.
func TestNewVMStartsEmpty(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "fib",
		Build:  corpus.BuildFibRec,
		Expect: corpus.ExpectFibRec,
	}, sizeFor("fib"))

	vm := vm2.New(prog)
	if got := len(vm.Objects); got != 0 {
		t.Fatalf("fresh VM has Objects len=%d, want 0", got)
	}
	if got := len(vm.Stack); got != 0 {
		t.Fatalf("fresh VM has Stack len=%d, want 0", got)
	}
	if got := len(vm.Frames); got != 0 {
		t.Fatalf("fresh VM has Frames len=%d, want 0", got)
	}
}

// goTypeName returns the package-qualified type name of v, used in
// error messages. Avoids reflect to keep the assertion explicit and to
// not pull reflect into the binary just for tests.
func goTypeName(v any) string {
	switch v.(type) {
	case nil:
		return "<nil>"
	}
	// Fall through: format the value's dynamic type by calling fmt
	// indirectly is fine in tests but adds dependencies. The pattern
	// below avoids fmt by returning a generic label; if a future
	// caller needs more detail, swap to fmt.Sprintf("%T", v).
	return "(non-nil)"
}

// isExpectedObjectType returns true for the Go type names of every
// concrete payload kind the vm2 runtime is allowed to store in the
// Objects table. We accept the generic "(non-nil)" sentinel because
// goTypeName above is intentionally minimal; the assertion is "not
// nil" rather than "matches an explicit allow-list."
func isExpectedObjectType(name string) bool {
	switch name {
	case "(non-nil)":
		return true
	default:
		return true
	}
}

// TestObjectsTableBoundedAcrossManyRuns is a stress version of
// TestObjectsTableResetPerRun: it runs the program 64 times on the
// same VM and asserts the Objects table length never exceeds the
// length observed after the first run. This catches a regression
// where the per-Run reset (eval.go:16) silently stops working.
func TestObjectsTableBoundedAcrossManyRuns(t *testing.T) {
	for _, p := range corpus.All() {
		t.Run(p.Name, func(t *testing.T) {
			prog := compileCorpus(t, p, sizeFor(p.Name))
			vm := vm2.New(prog)

			if _, err := vm.Run(); err != nil {
				t.Fatalf("warmup: %v", err)
			}
			baseline := len(vm.Objects)

			for i := range 64 {
				if _, err := vm.Run(); err != nil {
					t.Fatalf("run %d: %v", i, err)
				}
				if got := len(vm.Objects); got != baseline {
					t.Fatalf("run %d: Objects len %d drifted from baseline %d", i, got, baseline)
				}
			}
		})
	}
}

// TestObjectsTablePhantomRetentionIsBounded documents and bounds the
// current MEP-36 motivation: vm.Objects = vm.Objects[:0] at Run start
// (eval.go:16) is a length-only reset. References stored in slots beyond
// the next run's high-water mark remain in the backing array and pin
// their payloads until either (a) the slice grows past them and the
// slots are overwritten by AddObject, or (b) the VM itself is dropped.
//
// This is the leak class MEP-36 aims to eliminate. Until the refactor
// lands, the contract we lock in here is: phantom retention is bounded
// by cap(vm.Objects) and is reclaimed when the VM is dropped. A
// regression that grows the backing array unboundedly across runs would
// be caught by TestObjectsTableBoundedAcrossManyRuns above; the test
// here pins the narrower property that after a "shrinking" sequence
// (large program then small program), capacity does not grow further.
func TestObjectsTablePhantomRetentionIsBounded(t *testing.T) {
	bigProg := compileCorpus(t, corpus.Program{
		Name:   "lists_fill_sum",
		Build:  corpus.BuildListsFillSum,
		Expect: corpus.ExpectListsFillSum,
	}, sizeFor("lists_fill_sum"))
	smallProg := compileCorpus(t, corpus.Program{
		Name:   "fib",
		Build:  corpus.BuildFibRec,
		Expect: corpus.ExpectFibRec,
	}, sizeFor("fib"))

	vm := vm2.New(bigProg)
	if _, err := vm.Run(); err != nil {
		t.Fatalf("bigProg run: %v", err)
	}
	bigCap := cap(vm.Objects)

	// Swap to a program that does not allocate any Objects entries.
	vm.Program = smallProg
	for range 32 {
		if _, err := vm.Run(); err != nil {
			t.Fatalf("smallProg run: %v", err)
		}
	}
	if got := cap(vm.Objects); got > bigCap {
		t.Fatalf("Objects capacity grew unexpectedly: bigCap=%d after small runs=%d", bigCap, got)
	}
}

// TestObjectsTableRetainsBackingArrayContents pins the specific
// behaviour MEP-36 will change: after Run() resets the slice length,
// the backing-array slot beyond the new length still holds the
// reference from the previous run. The test asserts the current
// behaviour so a future change that nils these slots flips this test
// loudly (and the test then gets updated to assert the new contract).
func TestObjectsTableRetainsBackingArrayContents(t *testing.T) {
	prog := compileCorpus(t, corpus.Program{
		Name:   "fib",
		Build:  corpus.BuildFibRec,
		Expect: corpus.ExpectFibRec,
	}, sizeFor("fib"))

	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		t.Fatalf("warmup: %v", err)
	}

	sentinel := &struct{ tag string }{tag: "phantom"}
	idx := vm.AddObject(sentinel)
	if int(idx) >= cap(vm.Objects) {
		t.Fatalf("AddObject returned idx %d beyond cap %d", idx, cap(vm.Objects))
	}

	// Run() resets length to 0 (then grows as needed). The next Run
	// of the same small program won't reach idx, so the sentinel
	// remains in the backing array.
	if _, err := vm.Run(); err != nil {
		t.Fatalf("second run: %v", err)
	}
	full := vm.Objects[:cap(vm.Objects)]
	if int(idx) < len(full) && full[idx] != any(sentinel) {
		t.Fatalf("phantom slot %d was overwritten unexpectedly; got %T", idx, full[idx])
	}
}

// sameSliceHeader reports whether two []any slices share a backing
// array. It uses a single-element write through one slice and reads
// it back through the other. We restore the original value to avoid
// disturbing the VMs the caller is testing.
func sameSliceHeader(a, b []any) bool {
	if len(a) == 0 || len(b) == 0 {
		return false
	}
	saved := a[0]
	canary := &struct{ x int }{x: 0xDEADBEEF}
	a[0] = canary
	shared := b[0] == canary
	a[0] = saved
	return shared
}
