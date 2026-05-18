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

// TestLayerBCopyUpReturnedList asserts that returning a freshly
// allocated list via OpReturnCell keeps exactly the returned slot in
// the arena and truncates every other allocation:
//
//   - Function allocates a temp map (ArenaMap), then a list, pushes
//     a few i64 values, and returns the list.
//   - After one Run: ArenaMap is empty (Layer A behavior carried over),
//     ArenaList has one live slot at index 0 (Layer B copy-up).
//   - The returned handle still resolves to a 3-element list.
func TestLayerBCopyUpReturnedList(t *testing.T) {
	main := &Function{
		Name:        "buildAndReturn",
		NumRegsI64:  1,
		NumRegsCell: 2,
		ResultBank:  BankCell,
		Code: []Op{
			MakeOp(OpNewMap, 0, 0, 0), // temp map, will be truncated
			MakeOp(OpNewList, 1, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 10),
			MakeOp(OpListPushI64, 1, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 20),
			MakeOp(OpListPushI64, 1, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 30),
			MakeOp(OpListPushI64, 1, 0, 0),
			MakeOp(OpReturnCell, 1, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{main}, Entry: 0}
	vm := NewWithProgram(prog)
	ret, err := vm.Run(main)
	if err != nil {
		t.Fatalf("Run error: %v", err)
	}
	if !ret.IsHandle() {
		t.Fatalf("ret is not a handle: %x", uint64(ret))
	}
	tag, _, idx := ret.DecodeHandle()
	if tag != ArenaList {
		t.Fatalf("ret tag = %d, want ArenaList", tag)
	}
	if idx != 0 {
		t.Errorf("ret idx after Layer B copy-up = %d, want 0", idx)
	}
	if got := vm.Arenas().TotalSlots(ArenaList); got != 1 {
		t.Errorf("TotalSlots(ArenaList) = %d, want 1 (only returned slot)", got)
	}
	if got := vm.Arenas().TotalSlots(ArenaMap); got != 0 {
		t.Errorf("TotalSlots(ArenaMap) = %d, want 0 (temp truncated)", got)
	}
	if n := vm.Arenas().ListLen(ret); n != 3 {
		t.Errorf("returned ListLen = %d, want 3", n)
	}
}

// TestLayerBBoundsTempAllocations asserts that a function which
// allocates multiple temp containers but returns only one keeps every
// arena except the returned-handle arena bounded across many reused-VM
// invocations. ArenaList grows by 1 per call (one returned list per
// run); ArenaMap stays at 0.
func TestLayerBBoundsTempAllocations(t *testing.T) {
	main := &Function{
		Name:        "tempAllocsThenReturnList",
		NumRegsI64:  1,
		NumRegsCell: 4,
		ResultBank:  BankCell,
		Code: []Op{
			MakeOp(OpNewMap, 0, 0, 0),  // temp1
			MakeOp(OpNewMap, 1, 0, 0),  // temp2
			MakeOp(OpNewList, 2, 0, 0), // temp list (will be dropped, idx > mark)
			MakeOp(OpNewList, 3, 0, 0), // returned list
			MakeOp(OpReturnCell, 3, 0, 0),
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
		t.Errorf("TotalSlots(ArenaMap) after %d runs = %d, want 0", N, got)
	}
	// ArenaList grows by 1 per call (returned slot survives). Phase 5
	// mark-sweep will reclaim those across calls; for now we just
	// verify the growth is linear and matches N exactly, proving the
	// other temp list is truncated.
	if got := vm.Arenas().TotalSlots(ArenaList); got != N {
		t.Errorf("TotalSlots(ArenaList) after %d runs = %d, want %d", N, got, N)
	}
}

// TestLayerBAbortsOnLocalCellRef asserts that the conservative branch
// of Layer B fires when the returned slot contains a local-range
// handle. The returned list holds a reference to a freshly allocated
// string (local handle), so handleCellReturn must leave the slabs
// intact to avoid a use-after-free. ArenaList and ArenaString should
// both retain their slots after Run.
func TestLayerBAbortsOnLocalCellRef(t *testing.T) {
	main := &Function{
		Name:        "listWithLocalString",
		NumRegsI64:  1,
		NumRegsCell: 2,
		ResultBank:  BankCell,
		Consts:      []Cell{CSStr([]byte("ok"))},
		Code: []Op{
			MakeOp(OpNewList, 0, 0, 0),
			// Use a string handle from the const pool? CSStr is inline,
			// not a handle. We need a real arena-allocated string. The
			// simplest way to plant a local-range handle into a list is
			// via direct manipulation in the test setup. Instead, just
			// return the list as-is and verify the contains-scan
			// recognizes the embedded slot we plant manually.
			MakeOp(OpReturnCell, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{main}, Entry: 0}
	vm := NewWithProgram(prog)
	// Plant a real arena-allocated string into the VM's arena before
	// the Run, then poke a list-side reference. Easier path: drive the
	// helper directly via a synthetic frame in the next subtest.
	_ = prog
	_ = vm
	a := &Arenas{}
	// Pre-frame allocation (below mark): a string at idx 0.
	external := a.AllocString([]byte("ext"))
	var marks, freeMarks [numArenaTags]uint32
	// Snapshot marks; freeMarks remain zero.
	a.snapshotMarks(&marks, &freeMarks)
	// Locally allocate a list and a string (both above marks).
	localStr := a.AllocString([]byte("local"))
	listCell := a.AllocList(0, 0)
	tag, _, lidx := listCell.DecodeHandle()
	if tag != ArenaList {
		t.Fatalf("list tag = %d", tag)
	}
	a.Lists[lidx].cells = append(a.Lists[lidx].cells, localStr)
	a.Lists[lidx].len = 1
	// Layer B should detect the embedded local string and skip
	// truncation entirely.
	ret := a.handleCellReturn(listCell, &marks, &freeMarks)
	if ret != listCell {
		t.Errorf("handle was rewritten: got %x want %x", uint64(ret), uint64(listCell))
	}
	if a.TotalSlots(ArenaList) != 1 {
		t.Errorf("ArenaList truncated despite local ref: TotalSlots=%d", a.TotalSlots(ArenaList))
	}
	if a.TotalSlots(ArenaString) != 2 {
		t.Errorf("ArenaString truncated despite local ref: TotalSlots=%d", a.TotalSlots(ArenaString))
	}
	_ = external
}
