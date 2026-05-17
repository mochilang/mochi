package corpus

import (
	"math"

	"mochi/compiler2/ir"
)

// BuildMandelbrot wraps BuildMandelbrotKernel for the cross-lang harness.
// MEP-39 §6.2 sets the cross-lang shape as side = N, height = N,
// maxIter = 50, so the workload scales with N^2 and the output is the
// sum of escape counts over the N x N grid in [-2, 1] x [-1, 1].
func BuildMandelbrot(n int64) *ir.Module {
	return BuildMandelbrotKernel(n, n, 50)
}

// ExpectMandelbrot reproduces the same i64 sum that BuildMandelbrot's
// main() returns. The escape-count storage path in the IR truncates
// each count to a byte, but maxIter == 50 fits comfortably in a u8 so
// the byte truncation is a no-op and a plain int sum suffices.
func ExpectMandelbrot(n int64) int64 {
	const maxIter = int64(50)
	var sum int64
	w, h := n, n
	for row := int64(0); row < h; row++ {
		for col := int64(0); col < w; col++ {
			cx := float64(col)/float64(w)*3.0 - 2.0
			cy := float64(row)/float64(h)*2.0 - 1.0
			zr := 0.0
			zi := 0.0
			var k int64
			for k = 0; k < maxIter; k++ {
				r2 := zr * zr
				i2 := zi * zi
				if r2+i2 > 4.0 {
					break
				}
				nzi := math.FMA(2.0*zr, zi, cy)
				nzr := (r2 - i2) + cx
				zr, zi = nzr, nzi
			}
			sum += k
		}
	}
	return sum
}
