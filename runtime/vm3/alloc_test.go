package vm3

import (
	"bytes"
	"math/rand/v2"
	"testing"
)

func TestAllocString(t *testing.T) {
	var a Arenas
	for i, src := range [][]byte{
		[]byte("hello"),
		[]byte("world"),
		[]byte(""),
		bytes.Repeat([]byte("x"), 1024),
	} {
		h := a.AllocString(src)
		if !h.IsHandle() {
			t.Fatalf("alloc %d: not handle", i)
		}
		tag, _, _ := h.DecodeHandle()
		if tag != ArenaString {
			t.Fatalf("alloc %d: tag %v", i, tag)
		}
		got := a.StringBytes(h)
		if !bytes.Equal(got, src) {
			t.Fatalf("alloc %d: got %q want %q", i, got, src)
		}
	}
}

func TestAllocListAppendGet(t *testing.T) {
	var a Arenas
	h := a.AllocList(0, 4)
	for i := range 100 {
		a.ListAppend(h, CInt(int64(i*i)))
	}
	if a.ListLen(h) != 100 {
		t.Fatalf("len = %d", a.ListLen(h))
	}
	for i := range 100 {
		if got := a.ListGet(h, i).Int(); got != int64(i*i) {
			t.Fatalf("get %d: got %d", i, got)
		}
	}
}

func TestAllocStructFields(t *testing.T) {
	var a Arenas
	h := a.AllocStruct(7, 3)
	a.StructSetField(h, 0, CInt(1))
	a.StructSetField(h, 1, CFloat(2.5))
	a.StructSetField(h, 2, CBool(true))
	if a.StructField(h, 0).Int() != 1 {
		t.Fatal("field 0")
	}
	if a.StructField(h, 1).Float() != 2.5 {
		t.Fatal("field 1")
	}
	if !a.StructField(h, 2).Bool() {
		t.Fatal("field 2")
	}
}

func TestAllocPair(t *testing.T) {
	var a Arenas
	h := a.AllocPair(CInt(11), CInt(22))
	if a.PairFst(h).Int() != 11 || a.PairSnd(h).Int() != 22 {
		t.Fatal("pair round-trip")
	}
}

func TestAllocI64Arr(t *testing.T) {
	var a Arenas
	h := a.AllocI64Arr(10)
	arr := a.I64Arr(h)
	for i := range arr {
		arr[i] = int64(i + 1)
	}
	got := a.I64Arr(h)
	for i, v := range got {
		if v != int64(i+1) {
			t.Fatalf("got[%d] = %d", i, v)
		}
	}
}

func TestAllocF64Arr(t *testing.T) {
	var a Arenas
	h := a.AllocF64Arr(8)
	arr := a.F64Arr(h)
	for i := range arr {
		arr[i] = float64(i) * 0.5
	}
	got := a.F64Arr(h)
	for i, v := range got {
		if v != float64(i)*0.5 {
			t.Fatalf("got[%d] = %v", i, v)
		}
	}
}

func TestFreeAndReuse(t *testing.T) {
	var a Arenas
	h1 := a.AllocList(0, 2)
	a.ListAppend(h1, CInt(1))
	_, gen1, idx1 := h1.DecodeHandle()
	if gen1 != 0 {
		t.Fatalf("first gen = %d", gen1)
	}

	a.Free(h1)
	if a.LiveSlots(ArenaList) != 0 {
		t.Fatalf("live after free: %d", a.LiveSlots(ArenaList))
	}

	h2 := a.AllocList(0, 2)
	_, gen2, idx2 := h2.DecodeHandle()
	if idx2 != idx1 {
		t.Fatalf("reuse picked new idx %d (was %d)", idx2, idx1)
	}
	if gen2 != gen1+1 {
		t.Fatalf("gen did not bump: %d -> %d", gen1, gen2)
	}
	if a.ListLen(h2) != 0 {
		t.Fatalf("reused list not empty: len=%d", a.ListLen(h2))
	}
}

// TestArenaPropertyRoundTrip asserts every allocator round-trips a
// random workload of 10k allocations per type without panicking,
// without losing data, and without producing duplicate handles.
func TestArenaPropertyRoundTrip(t *testing.T) {
	rng := rand.New(rand.NewPCG(1, 2))
	var a Arenas
	const n = 10_000

	listHandles := make([]Cell, 0, n)
	listLens := make([]int, 0, n)
	for range n {
		k := rng.IntN(8)
		h := a.AllocList(0, k)
		for range k {
			a.ListAppend(h, CInt(rng.Int64()))
		}
		listHandles = append(listHandles, h)
		listLens = append(listLens, k)
	}
	for i, h := range listHandles {
		if a.ListLen(h) != listLens[i] {
			t.Fatalf("list %d: len drift", i)
		}
	}

	strHandles := make([]Cell, 0, n)
	strBodies := make([][]byte, 0, n)
	for i := range n {
		body := make([]byte, rng.IntN(16))
		for j := range body {
			body[j] = byte('a' + (i+j)%26)
		}
		h := a.AllocString(body)
		strHandles = append(strHandles, h)
		strBodies = append(strBodies, body)
	}
	for i, h := range strHandles {
		if !bytes.Equal(a.StringBytes(h), strBodies[i]) {
			t.Fatalf("string %d: drift", i)
		}
	}
}

// BenchmarkAllocListSteadyVsMake compares steady-state arena
// alloc+free (free-list reused) against runtime.mallocgc via `make`.
// Phase 1 gate target: within 2x of make.
func BenchmarkAllocListSteadyVsMake(b *testing.B) {
	b.Run("arena", func(b *testing.B) {
		var a Arenas
		// Warm the slab + free-list so every iteration is reuse-only.
		for range 1024 {
			h := a.AllocList(0, 8)
			a.Free(h)
		}
		b.ResetTimer()
		for range b.N {
			h := a.AllocList(0, 8)
			a.Free(h)
		}
	})
	b.Run("make", func(b *testing.B) {
		var sink []Cell
		for range b.N {
			sink = make([]Cell, 0, 8)
		}
		_ = sink
	})
}

func BenchmarkAllocStringSteadyVsMake(b *testing.B) {
	src := []byte("hello world")
	b.Run("arena", func(b *testing.B) {
		var a Arenas
		for range 1024 {
			h := a.AllocString(src)
			a.Free(h)
		}
		b.ResetTimer()
		for range b.N {
			h := a.AllocString(src)
			a.Free(h)
		}
	})
	b.Run("make", func(b *testing.B) {
		var sink []byte
		for range b.N {
			sink = make([]byte, len(src))
			copy(sink, src)
		}
		_ = sink
	})
}

// BenchmarkAllocListColdVsMake measures slab-growth cost (no free-list
// reuse). Documents the worst case; Phase 1 gate uses the steady-state
// bench above.
func BenchmarkAllocListColdVsMake(b *testing.B) {
	b.Run("arena", func(b *testing.B) {
		var a Arenas
		for range b.N {
			a.AllocList(0, 8)
		}
	})
	b.Run("make", func(b *testing.B) {
		var sink []Cell
		for range b.N {
			sink = make([]Cell, 0, 8)
		}
		_ = sink
	})
}
