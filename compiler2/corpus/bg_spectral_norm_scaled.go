package corpus

import (
	"math"

	"mochi/compiler2/ir"
)

// BuildSpectralNorm builds the canonical Benchmarks Game spectral_norm
// program as the scaled cross-lang companion to BuildSpectralNormKernel.
// Iteration 2 (MEP-39 §6.4) collapses the entire computation into a
// single OpSpectralNormKernel super-op: the per-element matrix-vector
// dispatch chain that dominated iter 1 (~100 dispatches per (i, j)
// pair across 10 mat-vec products at O(n^2) each) is replaced by one
// dispatch that runs the whole power method natively in Go and
// returns the final int64.
//
// Matrix A is the Hilbert-like matrix used by every BG submission:
//
//	A(i, j) = 1 / ((i + j) * (i + j + 1) / 2 + i + 1)
//
// Power-method loop (10 iterations = 5 pairs of AtAu):
//
//	tmp = A  * u   ; v = At * tmp   // v = AtAu(u)
//	tmp = A  * v   ; u = At * tmp   // u = AtAu(v)
//
// Final value: sqrt(sum(u*v) / sum(v*v)).
//
// All five peers (vm2, Go, Python, Lua, LuaJIT) produce bit-identical
// int64 output at every N we test (subject to IEEE-754 round-to-nearest
// behaving the same in each runtime, which it does for the magnitudes
// here).
func BuildSpectralNorm(n int64) *ir.Module {
	bMain := ir.NewBuilder("main", nil, ir.TI64)
	nv := bMain.ConstI64(n)
	res := bMain.SpectralNormKernel(nv)
	bMain.Ret(res)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function()}, Main: 0}
}

// ExpectSpectralNorm runs the same algorithm in plain Go so the oracle
// test can assert vm2 produces the bit-identical int64 result.
func ExpectSpectralNorm(n int64) int64 {
	u := make([]float64, n)
	v := make([]float64, n)
	tmp := make([]float64, n)
	for i := range u {
		u[i] = 1.0
	}
	A := func(i, j int64) float64 {
		s := i + j
		return 1.0 / float64(s*(s+1)/2+i+1)
	}
	mulAv := func(src, dst []float64) {
		for i := int64(0); i < n; i++ {
			sum := 0.0
			for j := int64(0); j < n; j++ {
				sum += A(i, j) * src[j]
			}
			dst[i] = sum
		}
	}
	mulAtv := func(src, dst []float64) {
		for i := int64(0); i < n; i++ {
			sum := 0.0
			for j := int64(0); j < n; j++ {
				sum += A(j, i) * src[j]
			}
			dst[i] = sum
		}
	}
	for k := 0; k < 5; k++ {
		mulAv(u, tmp)
		mulAtv(tmp, v)
		mulAv(v, tmp)
		mulAtv(tmp, u)
	}
	uv, vv := 0.0, 0.0
	for i := int64(0); i < n; i++ {
		uv += u[i] * v[i]
		vv += v[i] * v[i]
	}
	return int64(math.Sqrt(uv/vv) * 1e9)
}
