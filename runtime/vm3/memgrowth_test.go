package vm3

import "testing"

// TestArenaGrowsWithoutReset documents Phase 1's monotonic arena growth:
// re-running a program that allocates one map per call accumulates one
// fresh slot in the Map arena every call, because Phase 6 mark-sweep is
// not yet implemented. The free list stays empty so each AllocMap takes
// a fresh slab slot.
func TestArenaGrowsWithoutReset(t *testing.T) {
	a := &Arenas{}
	const N = 32
	for range N {
		_ = a.AllocMap(0)
	}
	if got := a.TotalSlots(ArenaMap); got != N {
		t.Fatalf("TotalSlots(ArenaMap) = %d, want %d", got, N)
	}
	if got := a.LiveSlots(ArenaMap); got != N {
		t.Fatalf("LiveSlots(ArenaMap) = %d, want %d (no free until Phase 6)", got, N)
	}
}

// TestArenaResetReclaims documents that Arenas.Reset returns every slab
// to its zero state. Intended for benchmarks and tests that want bounded
// memory between iterations without waiting for Phase 6 mark-sweep.
func TestArenaResetReclaims(t *testing.T) {
	a := &Arenas{}
	const N = 16
	for range N {
		_ = a.AllocMap(0)
		_ = a.AllocList(0, 0)
		_ = a.AllocString([]byte("hello"))
	}
	for _, tag := range []ArenaTag{ArenaMap, ArenaList, ArenaString} {
		if got := a.TotalSlots(tag); got != N {
			t.Fatalf("pre-Reset TotalSlots(%d) = %d, want %d", tag, got, N)
		}
	}
	a.Reset()
	for _, tag := range []ArenaTag{ArenaMap, ArenaList, ArenaString} {
		if got := a.TotalSlots(tag); got != 0 {
			t.Errorf("post-Reset TotalSlots(%d) = %d, want 0", tag, got)
		}
		if got := a.LiveSlots(tag); got != 0 {
			t.Errorf("post-Reset LiveSlots(%d) = %d, want 0", tag, got)
		}
	}
}

// TestArenaFreeListReuse documents the in-arena free-list path: Free
// pushes the slot onto its arena's free list and the next AllocMap
// reuses that slot rather than appending a fresh one.
func TestArenaFreeListReuse(t *testing.T) {
	a := &Arenas{}
	c := a.AllocMap(0)
	a.Free(c)
	c2 := a.AllocMap(0)
	if a.TotalSlots(ArenaMap) != 1 {
		t.Errorf("TotalSlots after free+realloc = %d, want 1 (slot reused)",
			a.TotalSlots(ArenaMap))
	}
	_, gen, _ := c2.DecodeHandle()
	if gen == 0 {
		t.Errorf("generation did not bump on reuse: gen=%d", gen)
	}
}

// TestLayerATruncatesUnboxedReturn asserts that running a program which
// allocates a map and returns an unboxed i64 leaves the map slab empty:
// Layer A's frame mark drops every slot allocated above the caller's
// high-water mark on Return*.
func TestLayerATruncatesUnboxedReturn(t *testing.T) {
	// Single-function program: alloc a map into regsCell[0], do nothing
	// with it, return constant 0 (unboxed via OpReturnConstK).
	main := &Function{
		Name:        "allocAndDrop",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ResultBank:  BankI64,
		Code: []Op{
			MakeOp(OpNewMap, 0, 0, 0),
			MakeOp(OpReturnConstK, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{main}, Entry: 0}
	vm := NewWithProgram(prog)
	if _, err := vm.Run(main); err != nil {
		t.Fatalf("Run error: %v", err)
	}
	if got := vm.Arenas().TotalSlots(ArenaMap); got != 0 {
		t.Errorf("TotalSlots(ArenaMap) after unboxed return = %d, want 0 (Layer A truncate)", got)
	}
}

// TestLayerABoundsReusedVM asserts that re-running an allocating
// program against the same VM keeps arena memory flat: Layer A drops
// each call's allocations on return, so 1000 invocations end with the
// same TotalSlots as one invocation.
func TestLayerABoundsReusedVM(t *testing.T) {
	main := &Function{
		Name:        "allocAndDrop",
		NumRegsI64:  1,
		NumRegsCell: 2,
		ResultBank:  BankI64,
		Code: []Op{
			MakeOp(OpNewMap, 0, 0, 0),
			MakeOp(OpNewList, 1, 0, 0),
			MakeOp(OpReturnConstK, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{main}, Entry: 0}
	vm := NewWithProgram(prog)
	const N = 1000
	for range N {
		if _, err := vm.Run(main); err != nil {
			t.Fatalf("Run error: %v", err)
		}
	}
	if got := vm.Arenas().TotalSlots(ArenaMap); got != 0 {
		t.Errorf("TotalSlots(ArenaMap) after %d reused-VM runs = %d, want 0", N, got)
	}
	if got := vm.Arenas().TotalSlots(ArenaList); got != 0 {
		t.Errorf("TotalSlots(ArenaList) after %d reused-VM runs = %d, want 0", N, got)
	}
}
