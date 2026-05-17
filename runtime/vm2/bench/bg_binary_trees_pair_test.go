package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goBinaryTreesKernel mirrors corpus.BuildBinaryTreesKernel using a
// native *tree pointer pair to match Go's idiomatic shape for the
// binary_trees BG workload. The Mochi peer routes the same shape
// through MEP-37 §3.4 packed pairs.
type pairTree struct{ L, R *pairTree }

func goMakeTree(d int64) *pairTree {
	if d == 0 {
		return &pairTree{}
	}
	return &pairTree{L: goMakeTree(d - 1), R: goMakeTree(d - 1)}
}

func goCheckTree(t *pairTree, d int64) int64 {
	if d == 0 {
		return 1
	}
	return 1 + goCheckTree(t.L, d-1) + goCheckTree(t.R, d-1)
}

func goBinaryTreesKernel(d int64) int64 {
	return goCheckTree(goMakeTree(d), d)
}

func TestBGBinaryTreesPairKernel(t *testing.T) {
	const D = int64(8)
	m := corpus.BuildBinaryTreesKernel(D)
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
	if !got.IsInt() {
		t.Fatalf("expected int, got %#v", got)
	}
	want := goBinaryTreesKernel(D)
	if got.Int() != want {
		t.Fatalf("binary_trees kernel = %d, want %d", got.Int(), want)
	}
}

func BenchmarkVM2_BG_BinaryTreesPairKernel(b *testing.B) {
	const D = int64(8)
	m := corpus.BuildBinaryTreesKernel(D)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		b.Fatalf("compile: %v", err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm2.New(prog).Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

// BenchmarkVM2_BG_BinaryTreesPairKernelReused matches the way a real
// AOT consumer would dispatch: compile once, hold one VM, Reset
// between runs so the Stack / Frames / pair arena backing arrays are
// reused. Fair head-to-head against the Go reference (which also pays
// no per-call runtime construction cost).
func BenchmarkVM2_BG_BinaryTreesPairKernelReused(b *testing.B) {
	const D = int64(8)
	m := corpus.BuildBinaryTreesKernel(D)
	opt.LeafInline(m)
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
	// Warm: run once so any one-shot grows (Stack, Frames, pairChunks)
	// land before the timed loop.
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

func BenchmarkGo_BG_BinaryTreesPairKernel(b *testing.B) {
	const D = int64(8)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goBinaryTreesKernel(D)
	}
}
