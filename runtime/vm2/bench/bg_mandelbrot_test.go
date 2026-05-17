package bench

import (
	"math"
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goMandelbrotKernel mirrors corpus.BuildMandelbrotKernel exactly: a
// w x h escape grid over c in [-2, 1] x [-1, 1], iterating
// z_{n+1} = z_n^2 + c with FMA on the imaginary axis, capped at
// maxIter. Return the sum of escape counts (one byte per pixel).
func goMandelbrotKernel(w, h, maxIter int64) int64 {
	out := make([]byte, w*h)
	for row := int64(0); row < h; row++ {
		for col := int64(0); col < w; col++ {
			cx := float64(col)/float64(w)*3.0 - 2.0
			cy := float64(row)/float64(h)*2.0 - 1.0
			zr := 0.0
			zi := 0.0
			var n int64
			for n = 0; n < maxIter; n++ {
				r2 := zr * zr
				i2 := zi * zi
				if r2+i2 > 4.0 {
					break
				}
				nzi := math.FMA(2.0*zr, zi, cy)
				nzr := (r2 - i2) + cx
				zr, zi = nzr, nzi
			}
			out[row*w+col] = byte(n)
		}
	}
	var s int64
	for _, b := range out {
		s += int64(b)
	}
	return s
}

func TestBGMandelbrotKernel(t *testing.T) {
	const W, H, MI = int64(8), int64(8), int64(50)
	m := corpus.BuildMandelbrotKernel(W, H, MI)
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
	want := goMandelbrotKernel(W, H, MI)
	if got.Int() != want {
		t.Fatalf("mandelbrot kernel = %d, want %d", got.Int(), want)
	}
}

func BenchmarkVM2_BG_MandelbrotKernel(b *testing.B) {
	const W, H, MI = int64(8), int64(8), int64(50)
	m := corpus.BuildMandelbrotKernel(W, H, MI)
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

func BenchmarkGo_BG_MandelbrotKernel(b *testing.B) {
	const W, H, MI = int64(8), int64(8), int64(50)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goMandelbrotKernel(W, H, MI)
	}
}
