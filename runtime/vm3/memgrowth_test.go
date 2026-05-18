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
