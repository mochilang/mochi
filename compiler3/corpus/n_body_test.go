package corpus

import (
	"math"
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestN_bodyMatchesOracle runs the vm3 N_body kernel for a range of
// step counts and asserts the result matches ExpectN_body to within a
// 1e-10 tolerance. Both implementations evaluate the floating-point
// ops in the same order so under IEEE-754 the answers should be
// bit-equal; the tolerance just absorbs any harmless round-trip
// difference through the Cell-payload encoding.
func TestN_bodyMatchesOracle(t *testing.T) {
	for _, steps := range []int64{0, 1, 2, 5, 10, 100} {
		prog := N_body.Build(steps)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{steps})
		if err != nil {
			t.Fatalf("n_body(%d): %v", steps, err)
		}
		want := ExpectN_body(steps)
		if d := math.Abs(got.Float() - want); d > 1e-10 {
			t.Errorf("n_body(%d) = %v, want %v (delta %v)", steps, got.Float(), want, d)
		}
	}
}

// BenchmarkN_bodyInterp measures interp-only throughput on two
// representative step counts so the j.2 interp baseline can be
// recorded in MEP-40 before JIT lowering lands in j.3.
func BenchmarkN_bodyInterp(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"n_body_n100", 100},
		{"n_body_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			prog := N_body.Build(tc.n)
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

var nbodyGoSink float64

// BenchmarkN_bodyGo is the matching native-Go reference so the
// vm3-vs-Go ratio is a single bench command away.
func BenchmarkN_bodyGo(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"n_body_n100", 100},
		{"n_body_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			var s float64
			for i := 0; i < b.N; i++ {
				s += ExpectN_body(n + int64(i&1))
			}
			nbodyGoSink = s
		})
	}
}
