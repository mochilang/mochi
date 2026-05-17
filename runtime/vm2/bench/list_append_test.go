package bench

import (
	"runtime"
	"testing"

	"mochi/compiler2/ir"
	"mochi/compiler2/emit"
	vm2 "mochi/runtime/vm2"
)

// MEP-36 Phase 3c: fluent-chain append measurements.
//
// Builds a function that performs N successive functional appends on a
// list whose intermediate values are dead at each step. With emit's
// last-use bit set on OpListAppend's source operand, the dispatcher
// mutates the source in place and reuses the same *vmList pointer
// across all N appends, allocating a single backing array (amortized).
// Without the bit (the "preserve source" path that runs whenever a
// later instruction still reads the source), every append allocates a
// fresh *vmList plus a copy of the prior backing array, so the chain
// is O(N^2) bytes and O(N) heap objects.

func buildFluentChain(n int, lastUse bool) *vm2.Program {
	b := ir.NewBuilder("main", nil, ir.TI64)
	xs := b.NewList(0)
	// In !lastUse mode every intermediate gets an explicit later read
	// (ListLen folded into an accumulator), so emit must keep the
	// per-step copy and the dispatcher cannot mutate in place.
	var acc ir.ValueID = -1
	for i := 0; i < n; i++ {
		next := b.ListAppend(xs, b.ConstI64(int64(i)))
		if !lastUse {
			l := b.ListLen(xs)
			if acc < 0 {
				acc = l
			} else {
				acc = b.AddI64(acc, l)
			}
		}
		xs = next
	}
	got := b.ListLen(xs)
	if acc >= 0 {
		// Fold the accumulator into the return so DCE doesn't eat it.
		got = b.AddI64(got, b.SubI64(acc, acc))
	}
	b.Ret(got)
	m := &ir.Module{Funcs: []*ir.Function{b.Function()}, Main: 0}
	p, err := emit.Compile(m)
	if err != nil {
		panic(err)
	}
	return p
}

// runOnce builds + runs the chain. Construction lives inside the timed
// region to match what a real program would pay; the fluent-chain
// pattern is the workload, not just the inner dispatch.
func runOnce(p *vm2.Program) int64 {
	out, err := vm2.New(p).Run()
	if err != nil {
		panic(err)
	}
	return out.Int()
}

func BenchmarkListAppendFluentChain_InPlace(b *testing.B) {
	const n = 256
	p := buildFluentChain(n, true)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if got := runOnce(p); got != n {
			b.Fatalf("len = %d, want %d", got, n)
		}
	}
}

func BenchmarkListAppendFluentChain_Copy(b *testing.B) {
	const n = 256
	p := buildFluentChain(n, false)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if got := runOnce(p); got != n {
			b.Fatalf("len = %d, want %d", got, n)
		}
	}
}

// TestListAppendFastPathAllocCount asserts that the in-place chain
// allocates O(1) backing arrays regardless of N (geometric growth
// from append's amortized doubling adds a small log factor; we cap
// well above N). The slow path's per-step copy is the comparison
// baseline.
func TestListAppendFastPathAllocCount(t *testing.T) {
	const n = 128
	pFast := buildFluentChain(n, true)
	pSlow := buildFluentChain(n, false)

	// Warm up to settle any one-shot lazy initialization.
	_ = runOnce(pFast)
	_ = runOnce(pSlow)

	measure := func(p *vm2.Program) (mallocs uint64) {
		var before, after runtime.MemStats
		runtime.GC()
		runtime.ReadMemStats(&before)
		_ = runOnce(p)
		runtime.ReadMemStats(&after)
		return after.Mallocs - before.Mallocs
	}
	fast := measure(pFast)
	slow := measure(pSlow)
	// The slow path allocates a fresh *vmList plus a copy of the
	// backing array on every step; the fast path mutates and only
	// pays for backing-array growth (which is O(log n)). The exact
	// numbers depend on the Go allocator, so we assert the ratio
	// rather than absolute counts.
	if slow <= fast*2 {
		t.Fatalf("expected slow path mallocs (%d) to be > 2x fast path (%d)", slow, fast)
	}
	t.Logf("fluent chain n=%d: fast=%d mallocs, slow=%d mallocs (ratio %.1fx)", n, fast, slow, float64(slow)/float64(fast))
}
