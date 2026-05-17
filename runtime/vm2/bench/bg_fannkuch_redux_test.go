package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goFannkuchReduxKernel mirrors corpus.BuildFannkuchReduxKernel: count
// the flips needed to bring 1 to the head of [4,2,1,5,7,3,6], where
// each flip reverses the prefix whose length is the current head
// value. Expected: 9.
func goFannkuchReduxKernel() int64 {
	perm := []int64{4, 2, 1, 5, 7, 3, 6}
	var count int64
	for perm[0] != 1 {
		k := perm[0]
		for lo, hi := int64(0), k-1; lo < hi; lo, hi = lo+1, hi-1 {
			perm[lo], perm[hi] = perm[hi], perm[lo]
		}
		count++
	}
	return count
}

func TestBGFannkuchReduxKernel(t *testing.T) {
	m := corpus.BuildFannkuchReduxKernel()
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
	want := goFannkuchReduxKernel()
	if got.Int() != want {
		t.Fatalf("fannkuch_redux kernel = %d, want %d", got.Int(), want)
	}
}

func BenchmarkVM2_BG_FannkuchReduxKernel(b *testing.B) {
	m := corpus.BuildFannkuchReduxKernel()
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

func BenchmarkGo_BG_FannkuchReduxKernel(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goFannkuchReduxKernel()
	}
}
