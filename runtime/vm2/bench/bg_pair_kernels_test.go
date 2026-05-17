package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goPairSwap is the Go reference for BuildPairSwapKernel: each iteration
// allocates a fresh struct{ A, B int64 } with the slots swapped from the
// previous head, and returns the final head. The struct shape mirrors
// MEP-37 §3.4 packed pairs (two-element record reached via pointer).
type pairI64 struct{ A, B int64 }

func goPairSwap(n int64) *pairI64 {
	head := &pairI64{A: 0, B: 1}
	for i := int64(0); i < n; i++ {
		head = &pairI64{A: head.B, B: head.A}
	}
	return head
}

func TestPairSwapKernel(t *testing.T) {
	const N = int64(1000)
	m := corpus.BuildPairSwapKernel(N)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	got, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if got.IsNull() {
		t.Fatalf("expected non-null pair head, got null")
	}
}

// BenchmarkVM2_BG_PairSwapKernelReused measures the steady-state pair
// swap loop. MEP-38 §A.5 calls this the minimum-body pair workload:
// one OpJumpIfLessI64 + OpPairFst + OpPairSnd + OpNewPair + OpAddI64K +
// OpTailCallSelf per iteration. The arena bump dominates the cost since
// the body has no walks, no math beyond the counter, no branches per
// pair. Reuse via vm.Reset() so the arena chunk-backing arrays stay
// allocated; the bench loop pays zero heap allocations.
func BenchmarkVM2_BG_PairSwapKernelReused(b *testing.B) {
	const N = int64(1000)
	m := corpus.BuildPairSwapKernel(N)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		b.Fatalf("compile: %v", err)
	}
	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		b.Fatalf("warm run: %v", err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Reset()
		if _, err := vm.Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

func BenchmarkGo_BG_PairSwapKernel(b *testing.B) {
	const N = int64(1000)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goPairSwap(N)
	}
}

// goPairWalk is the Go reference for BuildPairWalkKernel: build a linear
// pair chain of N nodes, then walk it counting steps. The build phase
// stresses the heap allocator the same way Mochi stresses the arena;
// the walk phase exercises pointer-chase reads against arena-stored
// pair access.
type pairChain struct{ A int64; B *pairChain }

func goPairWalk(n int64) int64 {
	head := (*pairChain)(nil)
	for i := int64(0); i < n; i++ {
		head = &pairChain{A: 0, B: head}
	}
	var acc int64
	for acc < n {
		head = head.B
		acc++
	}
	return acc
}

func TestPairWalkKernel(t *testing.T) {
	const N = int64(1000)
	m := corpus.BuildPairWalkKernel(N)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	got, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if !got.IsInt() || got.Int() != N {
		t.Fatalf("expected %d, got %#v", N, got)
	}
}

// BenchmarkVM2_BG_PairWalkKernelReused covers the combined alloc-then-walk
// shape: N pair allocations into the arena, followed by N projection
// reads chasing pair.snd. Both phases run tail-recursive against the
// same VM, so the arena cursor and call frames stay reused across runs.
func BenchmarkVM2_BG_PairWalkKernelReused(b *testing.B) {
	const N = int64(1000)
	m := corpus.BuildPairWalkKernel(N)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		b.Fatalf("compile: %v", err)
	}
	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		b.Fatalf("warm run: %v", err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		vm.Reset()
		if _, err := vm.Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

func BenchmarkGo_BG_PairWalkKernel(b *testing.B) {
	const N = int64(1000)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goPairWalk(N)
	}
}
