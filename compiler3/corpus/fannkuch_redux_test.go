package corpus

import (
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestFannkuchReduxMatchesOracle runs the vm3 fannkuch_redux kernel
// for a range of trial counts and asserts the result matches
// ExpectFannkuchRedux. Both implementations process the trials in the
// same order; the result is an integer flip-count sum so the test
// asserts strict equality.
func TestFannkuchReduxMatchesOracle(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 5, 7, 14, 100, 1000} {
		prog := FannkuchRedux.Build(n)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		if err != nil {
			t.Fatalf("fannkuch_redux(%d): %v", n, err)
		}
		want := ExpectFannkuchRedux(n)
		if got.Int() != want {
			t.Errorf("fannkuch_redux(%d) = %d, want %d", n, got.Int(), want)
		}
	}
}

// BenchmarkFannkuchReduxInterp measures interp-only throughput on the
// two BG cross-lang sizes (1000 and 10000 trials).
func BenchmarkFannkuchReduxInterp(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"fannkuch_redux_n1000", 1000},
		{"fannkuch_redux_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			prog := FannkuchRedux.Build(tc.n)
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

var fannkuchGoSink int64

// BenchmarkFannkuchReduxGo is the matching native-Go reference so the
// vm3-vs-Go ratio is a single bench command away.
func BenchmarkFannkuchReduxGo(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"fannkuch_redux_n1000", 1000},
		{"fannkuch_redux_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			var s int64
			for i := 0; i < b.N; i++ {
				s += ExpectFannkuchRedux(n + int64(i&1))
			}
			fannkuchGoSink = s
		})
	}
}
