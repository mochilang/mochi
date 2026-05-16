package bench

import (
	"runtime"
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// corpusSize is the per-program N used by both correctness tests and
// benchmarks. The values mirror the smallest size in bench/template
// (math sizes 10/20/30) and the runtime/vm/bench defaults, picking a
// size that exercises the program without making the suite slow.
type corpusSize struct {
	name string
	n    int64
}

var corpusSizes = []corpusSize{
	{"fib", 25},               // matches runtime/vm/bench fib.mochi (fib(25))
	{"iter_sum", 10000},       // matches runtime/vm/bench iter_sum.mochi
	{"math_sum_loop", 10001},  // 1..10000 = same as iter_sum oracle
	{"math_mul_loop", 16},     // 15! = 1307674368000, fits in 48-bit Cell payload
	{"math_fact_rec", 12},     // 12! = 479001600
	{"math_fib_iter", 30},     // matches template size 30
	{"math_fib_rec", 25},      // fib(25) = 75025
	{"math_prime_count", 100}, // primes in [2, 100)
	// Strings subsystem (MEP-24 §2). Concat loop exercises the
	// allocating path: N appends of a single-byte literal to a
	// growing buffer.
	{"strings_concat_loop", 64},
}

func sizeFor(name string) int64 {
	for _, s := range corpusSizes {
		if s.name == name {
			return s.n
		}
	}
	panic("vm2/bench: no size for " + name)
}

func compileCorpus(t testing.TB, p corpus.Program, n int64) *vm2.Program {
	t.Helper()
	m := p.Build(n)
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("%s: compile: %v", p.Name, err)
	}
	return prog
}

// TestCorpusMatchesOracle runs every corpus program once on vm2 and
// asserts the result matches the Go oracle in compiler2/corpus.
func TestCorpusMatchesOracle(t *testing.T) {
	for _, p := range corpus.All() {
		p := p
		t.Run(p.Name, func(t *testing.T) {
			n := sizeFor(p.Name)
			prog := compileCorpus(t, p, n)
			got, err := vm2.New(prog).Run()
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			want := p.Expect(n)
			if got.Int() != want {
				t.Fatalf("%s(%d) = %d, want %d", p.Name, n, got.Int(), want)
			}
		})
	}
}

func benchCorpus(b *testing.B, p corpus.Program) {
	n := sizeFor(p.Name)
	prog := compileCorpus(b, p, n)
	b.ReportAllocs()

	var startMem, endMem runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&startMem)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm2.New(prog).Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
	b.StopTimer()

	runtime.ReadMemStats(&endMem)
	b.ReportMetric(float64(endMem.HeapInuse)/1024/1024, "heap-MB")
	b.ReportMetric(float64(endMem.Sys)/1024/1024, "sys-MB")
	b.ReportMetric(float64(endMem.TotalAlloc-startMem.TotalAlloc)/float64(b.N), "total-B/op")
}

func BenchmarkVM2_Fib(b *testing.B)         { benchCorpus(b, corpus.Program{Name: "fib", Build: corpus.BuildFibRec, Expect: corpus.ExpectFibRec}) }
func BenchmarkVM2_IterSum(b *testing.B)     { benchCorpus(b, corpus.Program{Name: "iter_sum", Build: corpus.BuildIterSum, Expect: corpus.ExpectIterSum}) }
func BenchmarkVM2_SumLoop(b *testing.B)     { benchCorpus(b, corpus.Program{Name: "math_sum_loop", Build: corpus.BuildSumLoop, Expect: corpus.ExpectSumLoop}) }
func BenchmarkVM2_MulLoop(b *testing.B)     { benchCorpus(b, corpus.Program{Name: "math_mul_loop", Build: corpus.BuildMulLoop, Expect: corpus.ExpectMulLoop}) }
func BenchmarkVM2_FactRec(b *testing.B)     { benchCorpus(b, corpus.Program{Name: "math_fact_rec", Build: corpus.BuildFactRec, Expect: corpus.ExpectFactRec}) }
func BenchmarkVM2_FibIter(b *testing.B)     { benchCorpus(b, corpus.Program{Name: "math_fib_iter", Build: corpus.BuildFibIter, Expect: corpus.ExpectFibIter}) }
func BenchmarkVM2_FibRecTmpl(b *testing.B)  { benchCorpus(b, corpus.Program{Name: "math_fib_rec", Build: corpus.BuildFibRec, Expect: corpus.ExpectFibRec}) }
func BenchmarkVM2_PrimeCount(b *testing.B)  { benchCorpus(b, corpus.Program{Name: "math_prime_count", Build: corpus.BuildPrimeCount, Expect: corpus.ExpectPrimeCount}) }
func BenchmarkVM2_StringsConcatLoop(b *testing.B) { benchCorpus(b, corpus.Program{Name: "strings_concat_loop", Build: corpus.BuildStringsConcatLoop, Expect: corpus.ExpectStringsConcatLoop}) }
