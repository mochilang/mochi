package bench

import (
	"math"
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goSpectralNormKernel mirrors the IR built by corpus.BuildSpectralNormKernel.
// It is the source of truth the vm2 result is compared against.
func goSpectralNormKernel(n int64) float64 {
	evalA := func(i, j int64) float64 {
		denom := (i+j)*(i+j+1)/2 + i + 1
		return 1.0 / float64(denom)
	}
	u := make([]float64, n)
	v := make([]float64, n)
	for i := range u {
		u[i] = 1
	}
	for i := int64(0); i < n; i++ {
		var s float64
		for j := int64(0); j < n; j++ {
			s += evalA(i, j) * u[j]
		}
		v[i] = s
	}
	var vu, vv float64
	for i := int64(0); i < n; i++ {
		vu += u[i] * v[i]
		vv += v[i] * v[i]
	}
	return math.Sqrt(vu / vv)
}

// TestBGSpectralNormKernel compiles and runs the spectral_norm BG inner
// kernel and verifies the float result matches the Go oracle to within
// 1e-12. This is the smoke test that the MEP-37 Phase 1 stack (float
// arithmetic, typed-arrays, int↔float conversion) all line up end to
// end through the IR, opt passes, emit, and runtime.
func TestBGSpectralNormKernel(t *testing.T) {
	const n int64 = 10
	m := corpus.BuildSpectralNormKernel(n)
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
	if !got.IsFloat() {
		t.Fatalf("expected float, got %#v", got)
	}
	want := goSpectralNormKernel(n)
	if d := math.Abs(got.Float() - want); d > 1e-12 {
		t.Fatalf("spectral_norm(%d) = %v, want %v (delta %v)", n, got.Float(), want, d)
	}
}

func BenchmarkVM2_BG_SpectralNormKernel(b *testing.B) {
	const n int64 = 100
	m := corpus.BuildSpectralNormKernel(n)
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

// BenchmarkGo_BG_SpectralNormKernel is the Go peer for the vm2 spectral
// norm kernel benchmark. The ratio between this and BenchmarkVM2_*
// quantifies the MEP-37 §3.2/§3.3 dispatch overhead and feeds the
// MEP-37 Appendix A measurements.
func BenchmarkGo_BG_SpectralNormKernel(b *testing.B) {
	const n int64 = 100
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goSpectralNormKernel(n)
	}
}
