package vm3

import "testing"

// TestMapCapForEntries locks the load-factor 0.5 sizing rule used by
// AllocMap(capHint > 0) in Phase 6.2d.2.d step 1: the smallest pow2
// table that holds capHint inserts without crossing 2*(nLive+1) > cap.
func TestMapCapForEntries(t *testing.T) {
	cases := []struct {
		n    int
		want int
	}{
		{0, mapInitCap},
		{1, mapInitCap},
		{3, mapInitCap},
		{4, 16},
		{8, 32},
		{64, 256},
		{127, 256},
		{128, 512},
		{129, 512},
		{255, 512},
		{256, 1024},
	}
	for _, tc := range cases {
		got := mapCapForEntries(tc.n)
		if got != tc.want {
			t.Errorf("mapCapForEntries(%d) = %d, want %d", tc.n, got, tc.want)
		}
		// Invariant: the chosen cap holds tc.n inserts without crossing
		// the grow trigger 2*(nLive+1) > cap on the final insert.
		if tc.n > 0 && uint32(2*tc.n) > uint32(got) {
			t.Errorf("mapCapForEntries(%d) = %d violates 2n <= cap", tc.n, got)
		}
	}
}

// TestAllocMapPreSize asserts AllocMap(n) returns a slot whose table
// is pre-sized to hold n inserts without invoking growMap.
func TestAllocMapPreSize(t *testing.T) {
	var a Arenas
	c := a.AllocMap(128)
	_, _, idx := c.DecodeHandle()
	m := &a.Maps[idx]
	if got := len(m.table); got != 512 {
		t.Fatalf("AllocMap(128) table len = %d, want 512", got)
	}
	if m.nLive != 0 {
		t.Fatalf("AllocMap(128) nLive = %d, want 0", m.nLive)
	}
	// Insert 128 entries; growMap must not be called (table len stays 512).
	for i := range int64(128) {
		a.MapSetI64(c, i, i*10)
	}
	if got := len(m.table); got != 512 {
		t.Fatalf("after 128 inserts: table len = %d, want 512 (grew unexpectedly)", got)
	}
	if m.nLive != 128 {
		t.Fatalf("after 128 inserts: nLive = %d, want 128", m.nLive)
	}
	// Verify retrieval correctness.
	for i := range int64(128) {
		if got := a.MapGetI64(c, i); got != i*10 {
			t.Fatalf("MapGetI64(%d) = %d, want %d", i, got, i*10)
		}
	}
}

// TestAllocMapZeroCapKeepsLazyShape locks the historical AllocMap(0)
// shape: empty table, first insert grows to mapInitCap. Test code and
// fixtures relying on lazy alloc must keep working.
func TestAllocMapZeroCapKeepsLazyShape(t *testing.T) {
	var a Arenas
	c := a.AllocMap(0)
	_, _, idx := c.DecodeHandle()
	if got := len(a.Maps[idx].table); got != 0 {
		t.Fatalf("AllocMap(0) table len = %d, want 0", got)
	}
	a.MapSetI64(c, 1, 100)
	if got := len(a.Maps[idx].table); got != mapInitCap {
		t.Fatalf("first insert after AllocMap(0): table len = %d, want %d",
			got, mapInitCap)
	}
}
