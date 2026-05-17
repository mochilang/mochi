package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goPairChain is the Go reference for BuildPairThroughputKernel: each
// allocation references the previous chain head twice, so every node
// escapes and neither Go nor vm2 can elide the allocation. Returning
// the final head keeps the entire chain reachable so the benchmark
// loop can't dead-code-eliminate it either.
func goPairChain(n int64) *pairTree {
	var head *pairTree = &pairTree{}
	for i := int64(0); i < n; i++ {
		head = &pairTree{L: head, R: head}
	}
	return head
}

func TestPairThroughputKernel(t *testing.T) {
	const N = int64(1000)
	m := corpus.BuildPairThroughputKernel(N)
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
	// Result is the chain head pair (TPair); just check it's non-null.
	if got.IsNull() {
		t.Fatalf("expected non-null pair head, got null")
	}
}

// BenchmarkVM2_BG_PairThroughputKernelReused measures steady-state
// pair-allocation throughput against the per-VM arena. After the warm
// run, vm.Reset() rewinds pairNext to 0 so the chunk-backing array is
// reused; the bench loop pays zero heap allocations per iteration.
// MEP-38 §A.5: this is the workload where the arena's bump-pointer
// model wins cleanly over Go's GC heap.
func BenchmarkVM2_BG_PairThroughputKernelReused(b *testing.B) {
	const N = int64(1000)
	m := corpus.BuildPairThroughputKernel(N)
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

func BenchmarkGo_BG_PairThroughputKernel(b *testing.B) {
	const N = int64(1000)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goPairChain(N)
	}
}
