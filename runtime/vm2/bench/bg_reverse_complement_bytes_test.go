package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goReverseComplementBytesKernel mirrors corpus.BuildReverseComplementBytesKernel:
// fill a 1024-byte buffer with the repeating "ACGT" pattern, then do an
// in-place reverse-and-complement (walk i in [0, n/2), swap+complement
// the (i, n-1-i) pair at each step), then sum the bytes. The sum is the
// same as the two-buffer form because every byte is replaced by its
// complement, just in a different order.
func goReverseComplementBytesKernel() int64 {
	const n = 1024
	buf := make([]byte, n)
	bases := [4]byte{'A', 'C', 'G', 'T'}
	for i := 0; i < n; i++ {
		buf[i] = bases[i%4]
	}
	complement := func(c byte) byte {
		switch c {
		case 'A':
			return 'T'
		case 'T':
			return 'A'
		case 'C':
			return 'G'
		case 'G':
			return 'C'
		}
		return c
	}
	for i := 0; i < n/2; i++ {
		j := n - 1 - i
		l := buf[i]
		r := buf[j]
		buf[j] = complement(l)
		buf[i] = complement(r)
	}
	var sum int64
	for i := 0; i < n; i++ {
		sum += int64(buf[i])
	}
	return sum
}

func TestBGReverseComplementBytesKernel(t *testing.T) {
	m := corpus.BuildReverseComplementBytesKernel()
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
	want := goReverseComplementBytesKernel()
	if got.Int() != want {
		t.Fatalf("reverse_complement_bytes kernel = %d, want %d", got.Int(), want)
	}
}

// TestBGReverseComplementBytesKernelInlined verifies the kernel still
// computes the same answer after MEP-38 §3.3 leaf-inline has been
// applied. Bound to the same Go reference as the non-inlined variant.
func TestBGReverseComplementBytesKernelInlined(t *testing.T) {
	m := corpus.BuildReverseComplementBytesKernel()
	opt.LeafInline(m)
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
	if got.Int() != goReverseComplementBytesKernel() {
		t.Fatalf("inlined kernel = %d, want %d",
			got.Int(), goReverseComplementBytesKernel())
	}
}

func BenchmarkVM2_BG_ReverseComplementBytesKernel(b *testing.B) {
	m := corpus.BuildReverseComplementBytesKernel()
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

// BenchmarkVM2_BG_ReverseComplementBytesKernelInlined runs the same
// kernel with MEP-38 §3.3 leaf-inline applied: complement() and
// baseFor() get copied into the hot loops, so the inner step has no
// OpCall / OpReturn round trip and the interpreter stays inside the
// caller's frame.
func BenchmarkVM2_BG_ReverseComplementBytesKernelInlined(b *testing.B) {
	m := corpus.BuildReverseComplementBytesKernel()
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
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm2.New(prog).Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

func BenchmarkGo_BG_ReverseComplementBytesKernel(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goReverseComplementBytesKernel()
	}
}
