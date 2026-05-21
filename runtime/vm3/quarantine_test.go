package vm3

import "testing"

// TestQuarantineFastPathBelowThreshold confirms slots whose current
// generation is below WrapWarn take the LIFO fast path and reuse
// immediately. This is the path 99.99% of allocations follow; it
// must remain zero-overhead relative to Phase 2 behavior.
func TestQuarantineFastPathBelowThreshold(t *testing.T) {
	a := &Arenas{}
	c1 := a.AllocList(0, 4)
	tag, _, idx1 := c1.DecodeHandle()
	if tag != ArenaList {
		t.Fatalf("expected ArenaList, got %v", tag)
	}
	if got := a.Lists[idx1].gen; got >= DefaultWrapWarn {
		t.Fatalf("setup: fresh slot gen=%d, expected < %d", got, DefaultWrapWarn)
	}
	a.Free(c1)
	if got := len(a.freeLists); got != 1 {
		t.Fatalf("free list size = %d, want 1 (LIFO fast path)", got)
	}
	if got := a.wrapQ.depth(ArenaList); got != 0 {
		t.Fatalf("quarantine ring depth = %d, want 0 (below WrapWarn)", got)
	}
	c2 := a.AllocList(0, 4)
	_, _, idx2 := c2.DecodeHandle()
	if idx1 != idx2 {
		t.Fatalf("expected LIFO reuse of slot %d, got %d", idx1, idx2)
	}
}

// TestQuarantineHoldsWrapProneSlot confirms slots whose generation
// is at or above WrapWarn take the quarantine slow path: their index
// is held out of the free list, so the next allocation lands on a
// different (or fresh) slot. This is the load-bearing security
// invariant for MEP-41 §6.7 (TCE-style wrap defense).
func TestQuarantineHoldsWrapProneSlot(t *testing.T) {
	a := &Arenas{}
	c1 := a.AllocList(0, 4)
	_, _, idx1 := c1.DecodeHandle()
	// Force the slot's gen to within wrap-warn window. In practice
	// this state is reached after ~4032 free/reuse cycles; the test
	// simulates it directly to keep runtime fast.
	a.Lists[idx1].gen = DefaultWrapWarn
	a.Free(c1)

	if got := len(a.freeLists); got != 0 {
		t.Fatalf("free list size = %d, want 0 (slot should be quarantined)", got)
	}
	if got := a.wrapQ.depth(ArenaList); got != 1 {
		t.Fatalf("quarantine ring depth = %d, want 1", got)
	}

	c2 := a.AllocList(0, 4)
	_, _, idx2 := c2.DecodeHandle()
	if idx1 == idx2 {
		t.Fatalf("wrap-prone slot %d was reused immediately; quarantine bypassed", idx1)
	}
}

// TestQuarantineDrainAfterDepth confirms that when the quarantine
// FIFO fills past WrapQuarantineDepth, the oldest entry drains to
// the regular free list. This bounds the quarantine's memory footprint
// while still delaying wrap-prone reuse by at least WrapQuarantineDepth
// same-arena allocations.
//
// The test allocates all wrap-prone slots up front (so the free list
// stays empty during allocation), then frees them in sequence. After
// (depth+2) frees, the quarantine ring holds the (depth) most recent
// entries and 2 oldest entries have drained to the free list in
// oldest-first order.
func TestQuarantineDrainAfterDepth(t *testing.T) {
	a := &Arenas{}
	// Use small thresholds to keep the test fast.
	const warn uint16 = 8
	const depth uint32 = 4
	a.SetQuarantineConfig(warn, depth)

	const total uint32 = depth + 2
	cells := make([]Cell, 0, total)
	idxs := make([]uint32, 0, total)
	for i := uint32(0); i < total; i++ {
		c := a.AllocList(0, 4)
		_, _, idx := c.DecodeHandle()
		cells = append(cells, c)
		idxs = append(idxs, idx)
	}
	// Force every allocated slot to wrap-prone gen before freeing.
	for _, idx := range idxs {
		a.Lists[idx].gen = warn
	}
	for _, c := range cells {
		a.Free(c)
	}

	if got := uint32(a.wrapQ.depth(ArenaList)); got != depth {
		t.Fatalf("quarantine ring depth = %d, want %d after fill", got, depth)
	}
	if got := uint32(len(a.freeLists)); got != 2 {
		t.Fatalf("free list size = %d, want 2 (oldest drained)", got)
	}
	// Drain is FIFO at the quarantine level (oldest first) but each
	// drained slot is appended to the free list, so freeLists[0] is
	// the oldest drained slot and freeLists[1] the next-oldest.
	if a.freeLists[0] != idxs[0] || a.freeLists[1] != idxs[1] {
		t.Fatalf("free list = %v, want oldest-first drain [%d %d]",
			a.freeLists, idxs[0], idxs[1])
	}
}

// TestSetQuarantineConfigDisablesViaDepthZero confirms the
// quarantine slow path can be disabled by passing depth=0 with a
// non-zero WrapWarn (the explicit "configured, but disabled" form
// used by benchmarks and stress harnesses that want raw LIFO).
func TestSetQuarantineConfigDisablesViaDepthZero(t *testing.T) {
	a := &Arenas{}
	a.SetQuarantineConfig(DefaultWrapWarn, 0)

	c := a.AllocList(0, 4)
	_, _, idx := c.DecodeHandle()
	a.Lists[idx].gen = DefaultWrapWarn // force wrap-prone
	a.Free(c)

	if got := a.wrapQ.depth(ArenaList); got != 0 {
		t.Fatalf("quarantine ring depth = %d, want 0 when disabled", got)
	}
	if got := len(a.freeLists); got != 1 {
		t.Fatalf("free list size = %d, want 1 (LIFO with quarantine off)", got)
	}
}

// TestQuarantineRingIsPerArena confirms quarantining a slot in one
// arena does not affect a sibling arena's reuse behavior. The
// FIFO ring is indexed by ArenaTag; cross-arena interference would
// break the per-arena threat model.
func TestQuarantineRingIsPerArena(t *testing.T) {
	a := &Arenas{}

	// Force a list-arena slot into quarantine.
	cl := a.AllocList(0, 4)
	_, _, lIdx := cl.DecodeHandle()
	a.Lists[lIdx].gen = DefaultWrapWarn
	a.Free(cl)
	if got := a.wrapQ.depth(ArenaList); got != 1 {
		t.Fatalf("list quarantine depth = %d, want 1", got)
	}

	// A string-arena slot should be unaffected: low gen, takes
	// LIFO fast path, reuses immediately.
	cs := a.AllocString([]byte("x"))
	_, _, sIdx1 := cs.DecodeHandle()
	a.Free(cs)
	if got := a.wrapQ.depth(ArenaString); got != 0 {
		t.Fatalf("string quarantine depth = %d, want 0 (cross-arena leak)", got)
	}
	cs2 := a.AllocString([]byte("y"))
	_, _, sIdx2 := cs2.DecodeHandle()
	if sIdx1 != sIdx2 {
		t.Fatalf("string slot LIFO reuse broken: alloc1=%d alloc2=%d", sIdx1, sIdx2)
	}
}

// TestQuarantineDefaultsApply confirms a fresh Arenas (no
// SetQuarantineConfig call) uses the package DefaultWrap* constants,
// not a zero-config that would silently disable the slow path.
func TestQuarantineDefaultsApply(t *testing.T) {
	a := &Arenas{}
	if got := a.quarantineWrapWarn(); got != DefaultWrapWarn {
		t.Fatalf("default WrapWarn = %d, want %d", got, DefaultWrapWarn)
	}
	if got := a.quarantineDepth(); got != DefaultWrapQuarantineDepth {
		t.Fatalf("default WrapDepth = %d, want %d", got, DefaultWrapQuarantineDepth)
	}
}
