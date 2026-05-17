package bench

import (
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goReverseComplementKernel mirrors corpus.BuildReverseComplementKernel:
// fill a 1024-byte buffer with the repeating "ACGT" pattern, reverse
// and complement into a second buffer (A↔T, C↔G), then sum the output
// bytes. The arithmetic boils down to (256 * (84+71+67+65)) so the
// test has a deterministic expected value.
func goReverseComplementKernel() int64 {
	const n = 1024
	in := make([]byte, n)
	bases := [4]byte{'A', 'C', 'G', 'T'}
	for i := 0; i < n; i++ {
		in[i] = bases[i%4]
	}
	out := make([]byte, n)
	for i := 0; i < n; i++ {
		var c byte
		switch in[i] {
		case 'A':
			c = 'T'
		case 'T':
			c = 'A'
		case 'C':
			c = 'G'
		case 'G':
			c = 'C'
		default:
			c = in[i]
		}
		out[n-1-i] = c
	}
	var sum int64
	for i := 0; i < n; i++ {
		sum += int64(out[i])
	}
	return sum
}

func TestBGReverseComplementKernel(t *testing.T) {
	m := corpus.BuildReverseComplementKernel()
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
	want := goReverseComplementKernel()
	if got.Int() != want {
		t.Fatalf("reverse_complement kernel = %d, want %d", got.Int(), want)
	}
}

func BenchmarkVM2_BG_ReverseComplementKernel(b *testing.B) {
	m := corpus.BuildReverseComplementKernel()
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

func BenchmarkGo_BG_ReverseComplementKernel(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goReverseComplementKernel()
	}
}
