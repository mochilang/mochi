package corpus

import (
	"testing"

	vm3 "mochi/runtime/vm3"
)

// BenchmarkGoKernelsFair is the shape-faithful Go baseline for
// MEP-40's "vm3 vs Go" ratio. The original BenchmarkGoKernels routes
// through compiler2/corpus.Expect* helpers, several of which are
// closed-form (e.g. ExpectIterSum is (n-1)*n/2, ExpectListsFillSum is
// n*(n-1)/2, ExpectStringsConcatLoop is n+1) and so compare a vm3 O(n)
// loop to a Go O(1) constant. The ratio under that harness is
// meaningless.
//
// The kernels here mirror the vm3 corpus shape one-for-one:
//
//   - sum_loop: explicit i++ loop accumulating into s.
//   - mul_loop: explicit i++ loop multiplying into r.
//   - fact_rec: true recursion via goFactRec.
//   - fib_iter: explicit (a, b) := (b, a+b) loop.
//   - fib_rec: true recursion via goFibRec.
//   - prime_count: nested loop with modulo per (k, i) pair.
//   - strings_concat_loop: real string ++ loop building length-n string.
//   - lists_fill_sum: real []int64 append loop then a sum loop.
//   - maps_fill_sum: real map[int64]int64 fill loop then a sum lookup loop.
//
// Each Go kernel is //go:noinline so the bench loop can't fold its
// body. fairSink is package-global so dead-store elimination can't
// hoist the result out of the loop. The input is perturbed by b.N's
// parity so the optimizer can't memoize on a single constant input.
func BenchmarkGoKernelsFair(b *testing.B) {
	cases := []struct {
		name string
		fn   func(int64) int64
		n    int64
	}{
		{"fib_iter_n30", goFibIter, 30},
		{"sum_loop_n10001", goSumLoop, 10001},
		{"mul_loop_n16", goMulLoop, 16},
		{"fact_rec_n12", goFactRec, 12},
		{"fib_rec_n25", goFibRec, 25},
		{"prime_count_n100", goPrimeCount, 100},
		{"strings_concat_loop_n64", goStringsConcatLoop, 64},
		{"lists_fill_sum_n128", goListsFillSum, 128},
		{"maps_fill_sum_n128", goMapsFillSum, 128},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			n := tc.n
			fn := tc.fn
			var s int64
			for i := 0; i < b.N; i++ {
				s += fn(n + int64(i&1))
			}
			fairSink = s
		})
	}
}

// fairSink defeats dead-store elimination across the Fair bench loop.
var fairSink int64

//go:noinline
func goFibIter(n int64) int64 {
	a, b := int64(0), int64(1)
	for i := int64(0); i < n; i++ {
		a, b = b, a+b
	}
	return a
}

//go:noinline
func goSumLoop(n int64) int64 {
	var s int64
	for i := int64(0); i < n; i++ {
		s += i
	}
	return s
}

//go:noinline
func goMulLoop(n int64) int64 {
	r := int64(1)
	for i := int64(1); i < n; i++ {
		r *= i
	}
	return r
}

//go:noinline
func goFactRec(n int64) int64 {
	if n <= 1 {
		return 1
	}
	return n * goFactRec(n-1)
}

//go:noinline
func goFibRec(n int64) int64 {
	if n < 2 {
		return n
	}
	return goFibRec(n-1) + goFibRec(n-2)
}

//go:noinline
func goPrimeCount(n int64) int64 {
	count := int64(0)
	for k := int64(2); k < n; k++ {
		prime := true
		for i := int64(2); i < k-1; i++ {
			if k%i == 0 {
				prime = false
				break
			}
		}
		if prime {
			count++
		}
	}
	return count
}

//go:noinline
func goStringsConcatLoop(n int64) int64 {
	s := "a"
	for i := int64(0); i < n; i++ {
		s = s + "a"
	}
	return int64(len(s))
}

//go:noinline
func goListsFillSum(n int64) int64 {
	xs := make([]int64, 0, n)
	for i := int64(0); i < n; i++ {
		xs = append(xs, i)
	}
	var s int64
	for _, v := range xs {
		s += v
	}
	return s
}

//go:noinline
func goMapsFillSum(n int64) int64 {
	m := make(map[int64]int64, n)
	for i := int64(0); i < n; i++ {
		m[i] = i
	}
	var s int64
	for j := int64(0); j < n; j++ {
		s += m[j]
	}
	return s
}

// TestGoFairMatchesVm3 asserts every shape-faithful Go kernel returns
// the same result as the vm3 corpus for the same N. Without this
// check the bench numbers would not be apples-to-apples even if the
// shape were honest.
func TestGoFairMatchesVm3(t *testing.T) {
	cases := []struct {
		name string
		prog *Program
		fn   func(int64) int64
		ns   []int64
	}{
		{"fib_iter", FibIter, goFibIter, []int64{0, 1, 2, 5, 10, 20, 30}},
		{"sum_loop", SumLoop, goSumLoop, []int64{0, 1, 2, 100, 10001}},
		{"mul_loop", MulLoop, goMulLoop, []int64{1, 2, 5, 16}},
		{"fact_rec", FactRec, goFactRec, []int64{0, 1, 2, 5, 12}},
		{"fib_rec", FibRec, goFibRec, []int64{0, 1, 2, 10, 25}},
		{"prime_count", PrimeCount, goPrimeCount, []int64{0, 2, 10, 50, 100}},
		{"strings_concat_loop", StringsConcatLoop, goStringsConcatLoop, []int64{0, 1, 5, 64}},
		{"lists_fill_sum", ListsFillSum, goListsFillSum, []int64{0, 1, 10, 128}},
		{"maps_fill_sum", MapsFillSum, goMapsFillSum, []int64{0, 1, 10, 128}},
	}
	for _, tc := range cases {
		for _, n := range tc.ns {
			prog := tc.prog.Build(n)
			vm := vm3.NewWithProgram(prog)
			got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
			if err != nil {
				t.Errorf("%s(%d) vm3: %v", tc.name, n, err)
				continue
			}
			want := tc.fn(n)
			if got.Int() != want {
				t.Errorf("%s(%d): vm3=%d go=%d", tc.name, n, got.Int(), want)
			}
		}
	}
}
