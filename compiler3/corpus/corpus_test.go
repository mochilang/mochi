package corpus

import (
	"testing"

	c2corpus "mochi/compiler2/corpus"
	vm3 "mochi/runtime/vm3"
)

// TestMathKernelsMatchVm2 asserts every compiler3 corpus math kernel
// produces a result bit-identical to compiler2/corpus's Expect*
// reference functions. This is the Phase 2 correctness gate.
func TestMathKernelsMatchVm2(t *testing.T) {
	cases := []struct {
		name   string
		prog   *Program
		ns     []int64
		expect func(int64) int64
	}{
		{"fib_iter", FibIter, []int64{0, 1, 2, 5, 10, 15, 20}, c2corpus.ExpectFibIter},
		{"sum_loop", SumLoop, []int64{0, 1, 2, 10, 100, 1000}, c2corpus.ExpectIterSum},
		{"mul_loop", MulLoop, []int64{1, 2, 3, 5, 10}, c2corpus.ExpectMulLoop},
		{"fact_rec", FactRec, []int64{0, 1, 2, 3, 5, 10, 13}, c2corpus.ExpectFactRec},
		{"fib_rec", FibRec, []int64{0, 1, 2, 5, 10, 15, 20}, c2corpus.ExpectFibRec},
		{"prime_count", PrimeCount, []int64{0, 2, 10, 50, 100}, c2corpus.ExpectPrimeCount},
	}
	for _, tc := range cases {
		for _, n := range tc.ns {
			prog := tc.prog.Build(n)
			vm := vm3.NewWithProgram(prog)
			got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
			if err != nil {
				t.Errorf("%s(%d): %v", tc.name, n, err)
				continue
			}
			want := tc.expect(n)
			if got.Int() != want {
				t.Errorf("%s(%d) = %d want %d", tc.name, n, got.Int(), want)
			}
		}
	}
}

func BenchmarkMathKernels(b *testing.B) {
	// Sizes match runtime/vm2/bench/corpus_test.go so vm3 numbers
	// compare apples-to-apples with vm2 on the same workloads.
	cases := []struct {
		name string
		prog *Program
		n    int64
	}{
		{"fib_iter_n30", FibIter, 30},
		{"sum_loop_n10001", SumLoop, 10001},
		{"mul_loop_n16", MulLoop, 16},
		{"fact_rec_n12", FactRec, 12},
		{"fib_rec_n25", FibRec, 25},
		{"prime_count_n100", PrimeCount, 100},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			prog := tc.prog.Build(tc.n)
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

// goSink is package-global so the optimizer can't prove the loop has
// no observable effect and hoist the call out of the loop body.
var goSink int64

// BenchmarkGoKernels runs the compiler2 Expect* reference functions
// directly. Pairing this with BenchmarkMathKernels gives a "vm3 vs
// native Go" ratio per kernel, the headline MEP-40 perf metric.
//
// The input is perturbed by b.N's parity so Go's compiler can't
// constant-fold an identical-input call out of the loop. The sink
// accumulates results into a package global to defeat dead-store
// elimination of the call itself.
func BenchmarkGoKernels(b *testing.B) {
	cases := []struct {
		name string
		fn   func(int64) int64
		n    int64
	}{
		{"fib_iter_n30", c2corpus.ExpectFibIter, 30},
		{"sum_loop_n10001", c2corpus.ExpectIterSum, 10001},
		{"mul_loop_n16", c2corpus.ExpectMulLoop, 16},
		{"fact_rec_n12", c2corpus.ExpectFactRec, 12},
		{"fib_rec_n25", c2corpus.ExpectFibRec, 25},
		{"prime_count_n100", c2corpus.ExpectPrimeCount, 100},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			fn := tc.fn
			var s int64
			for i := 0; i < b.N; i++ {
				s += fn(n + int64(i&1))
			}
			goSink = s
		})
	}
}
