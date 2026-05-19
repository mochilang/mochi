package corpus

import (
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestReverseComplementMatchesOracle runs the vm3 reverse_complement
// kernel for a range of buffer sizes and asserts the result matches
// ExpectReverseComplement. Both implementations sum out[] as int64 so
// the test asserts strict equality. For n divisible by 4 the sum is
// (n/4) * 287 by construction; non-multiple-of-4 sizes are covered by
// the oracle.
func TestReverseComplementMatchesOracle(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 4, 7, 16, 100, 1000, 8000} {
		prog := ReverseComplement.Build(n)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		if err != nil {
			t.Fatalf("reverse_complement(%d): %v", n, err)
		}
		want := ExpectReverseComplement(n)
		if got.Int() != want {
			t.Errorf("reverse_complement(%d) = %d, want %d", n, got.Int(), want)
		}
	}
}

// BenchmarkReverseComplementInterp measures interp-only throughput on
// the two BG cross-lang sizes.
func BenchmarkReverseComplementInterp(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"reverse_complement_n1000", 1000},
		{"reverse_complement_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			prog := ReverseComplement.Build(tc.n)
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

var reverseComplementGoSink int64

// BenchmarkReverseComplementGo is the matching native-Go reference so
// the vm3-vs-Go ratio is a single bench command away.
func BenchmarkReverseComplementGo(b *testing.B) {
	for _, tc := range []struct {
		name string
		n    int64
	}{
		{"reverse_complement_n1000", 1000},
		{"reverse_complement_n10000", 10000},
	} {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			var s int64
			for i := 0; i < b.N; i++ {
				s += ExpectReverseComplement(n + int64(i&1))
			}
			reverseComplementGoSink = s
		})
	}
}
