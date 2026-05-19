package corpus

import (
	"math"
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestSpectralNormMatchesOracle runs the vm3 spectral_norm kernel for
// a range of vector sizes and asserts the result matches
// ExpectSpectralNorm to within 1e-12. The Go oracle evaluates the
// matrix entries in the same order so the difference should sit at
// the IEEE rounding noise floor.
func TestSpectralNormMatchesOracle(t *testing.T) {
	for _, n := range []int64{1, 2, 5, 10, 100, 500} {
		prog := SpectralNorm.Build(n)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		if err != nil {
			t.Fatalf("spectral_norm(%d): %v", n, err)
		}
		want := ExpectSpectralNorm(n)
		if d := math.Abs(got.Float() - want); d > 1e-12 {
			t.Errorf("spectral_norm(%d) = %v, want %v (delta %v)", n, got.Float(), want, d)
		}
	}
}

// BenchmarkSpectralNormInterp measures interp-only throughput on two
// representative vector sizes so the j.5/l.1 interp baseline can be
// recorded in MEP-40.
func BenchmarkSpectralNormInterp(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"spectral_norm_n100", 100},
		{"spectral_norm_n1000", 1000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			prog := SpectralNorm.Build(tc.n)
			vm := vm3.NewWithProgram(prog)
			fn := prog.Funcs[prog.Entry]
			args := []int64{tc.n}
			b.ResetTimer()
			for range b.N {
				_, _ = vm.RunWithArgs(fn, args)
			}
		})
	}
}

var spectralNormGoSink float64

// BenchmarkSpectralNormGo is the matching native-Go reference so the
// vm3-vs-Go ratio is a single bench command away.
func BenchmarkSpectralNormGo(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"spectral_norm_n100", 100},
		{"spectral_norm_n1000", 1000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			var s float64
			for i := 0; i < b.N; i++ {
				s += ExpectSpectralNorm(n + int64(i&1))
			}
			spectralNormGoSink = s
		})
	}
}
