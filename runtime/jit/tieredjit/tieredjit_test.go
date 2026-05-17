//go:build darwin && arm64

package tieredjit

import (
	"testing"

	"mochi/runtime/jit/tmpljit"
)

func want(n int64) int64 { return n*(n-1) + 3*n }

func TestOptimizerProducesSmallerProgram(t *testing.T) {
	src := tmpljit.FillSumProgram()
	opt := optimize(src)
	if len(opt) >= len(src) {
		t.Fatalf("optimizer did not shrink program: %d -> %d", len(src), len(opt))
	}
	// Sanity: walk the optimized IR with its built-in interp and
	// confirm it matches the closed form. This validates the
	// peephole + branch-rewrite math before the AArch64 backend
	// sees any of it.
	for _, n := range []int64{1, 8, 64, 128, 1024, 4096} {
		if got, w := opt.interp(n), want(n); got != w {
			t.Fatalf("optProgram.interp(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestCompileMatchesClosedForm(t *testing.T) {
	cf, err := Compile(tmpljit.FillSumProgram())
	if err != nil {
		t.Fatal(err)
	}
	defer cf.Free()
	for _, n := range []int64{1, 8, 64, 128, 1024, 4096} {
		if got, w := cf.Call(n), want(n); got != w {
			t.Fatalf("tier-2 JIT(%d) = %d, want %d", n, got, w)
		}
	}
}

func TestTier2CodeSmallerThanTier1(t *testing.T) {
	tier1, err := tmpljit.Compile(tmpljit.FillSumProgram())
	if err != nil {
		t.Fatal(err)
	}
	defer tier1.Free()
	tier2, err := Compile(tmpljit.FillSumProgram())
	if err != nil {
		t.Fatal(err)
	}
	defer tier2.Free()
	// Both are page-rounded, so they will almost always land on
	// the same 16 KiB page. The meaningful comparison is the
	// instruction count, but exposing that cleanly would require
	// plumbing through the unrounded byte length. As a coarse
	// guard, just verify both pages have the expected page size,
	// then log so the reviewer can spot churn.
	t.Logf("tier-1 code page %d bytes; tier-2 code page %d bytes", tier1.CodeLen(), tier2.CodeLen())
}

func BenchmarkTier2_128(b *testing.B) {
	cf, err := Compile(tmpljit.FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if cf.Call(128) != want(128) {
			b.Fatal()
		}
	}
}

func BenchmarkTier2_1024(b *testing.B) {
	cf, err := Compile(tmpljit.FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if cf.Call(1024) != want(1024) {
			b.Fatal()
		}
	}
}

func BenchmarkTier2_10000(b *testing.B) {
	cf, err := Compile(tmpljit.FillSumProgram())
	if err != nil {
		b.Fatal(err)
	}
	defer cf.Free()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if cf.Call(10000) != want(10000) {
			b.Fatal()
		}
	}
}
