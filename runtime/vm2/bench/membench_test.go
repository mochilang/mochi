// Memory and GC baseline benchmarks for the vm2 Objects-table refactor
// tracked in MEP-36. Each benchmark runs one corpus program b.N times
// and reports a richer set of metrics than the throughput suite in
// corpus_test.go: GC cycles, total pause time, mallocs, live heap after
// a forced final GC, and allocations per op.
//
// Why this lives next to the corpus benches rather than replacing them:
// the throughput numbers in corpus_test.go are committed to MEP-30 and
// MEP-34 measured-results appendices and should not move under us. The
// MEP-36 baseline gets its own file so the Phase 0 numbers can be
// diffed against Phase 1 / Phase 2 / Phase 3 cleanly.
//
// Run with:
//
//	go test ./runtime/vm2/bench -bench '^BenchmarkMEP36_Mem' -benchtime=3s -benchmem -run x
package bench

import (
	"runtime"
	"testing"

	"mochi/compiler2/corpus"
	vm2 "mochi/runtime/vm2"
)

// benchMem is the MEP-36 baseline harness. It mirrors benchCorpus but
// (a) reports GC metrics that benchCorpus omits and (b) measures live
// heap after a forced final GC so we can see what the VM pins versus
// what is transient garbage.
//
// Reported metrics:
//
//	gc-cycles/op    - full GC cycles per Run() (lower is better)
//	gc-pause-ns/op  - GC pause time per Run() (lower is better)
//	mallocs/op      - heap allocations per Run()
//	live-heap-KB    - HeapAlloc after forced final GC, single sample
//	heap-inuse-KB   - HeapInuse high-water at end of run loop
//	sys-KB          - process address space at end
//	total-B/op      - total bytes allocated per Run()
func benchMem(b *testing.B, p corpus.Program) {
	n := sizeFor(p.Name)
	prog := compileCorpus(b, p, n)
	b.ReportAllocs()

	// Compile a single VM and reuse it across iterations so the
	// metrics reflect steady-state Run() behaviour, not vm2.New cost
	// (which is measured separately below).
	vm := vm2.New(prog)
	if _, err := vm.Run(); err != nil {
		b.Fatalf("warmup: %v", err)
	}

	var startMem, endMem, liveMem runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&startMem)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm.Run(); err != nil {
			b.Fatalf("run %d: %v", i, err)
		}
	}
	b.StopTimer()

	runtime.ReadMemStats(&endMem)
	runtime.GC()
	runtime.ReadMemStats(&liveMem)

	gcCycles := float64(endMem.NumGC - startMem.NumGC)
	pauseNs := float64(endMem.PauseTotalNs - startMem.PauseTotalNs)
	mallocs := float64(endMem.Mallocs - startMem.Mallocs)
	totalAlloc := float64(endMem.TotalAlloc - startMem.TotalAlloc)
	N := float64(b.N)

	b.ReportMetric(gcCycles/N, "gc-cycles/op")
	b.ReportMetric(pauseNs/N, "gc-pause-ns/op")
	b.ReportMetric(mallocs/N, "mallocs/op")
	b.ReportMetric(totalAlloc/N, "total-B/op")
	b.ReportMetric(float64(liveMem.HeapAlloc)/1024, "live-heap-KB")
	b.ReportMetric(float64(endMem.HeapInuse)/1024, "heap-inuse-KB")
	b.ReportMetric(float64(endMem.Sys)/1024, "sys-KB")
}

// benchMemFreshVM measures the cost of vm2.New + Run() per op, which
// is the canonical embedding pattern (one VM per request) and the path
// most sensitive to the Objects-table layout. This is the headline
// number MEP-36 Phase 1 is expected to move.
func benchMemFreshVM(b *testing.B, p corpus.Program) {
	n := sizeFor(p.Name)
	prog := compileCorpus(b, p, n)
	b.ReportAllocs()

	var startMem, endMem, liveMem runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&startMem)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm2.New(prog).Run(); err != nil {
			b.Fatalf("run %d: %v", i, err)
		}
	}
	b.StopTimer()

	runtime.ReadMemStats(&endMem)
	runtime.GC()
	runtime.ReadMemStats(&liveMem)

	N := float64(b.N)
	b.ReportMetric(float64(endMem.NumGC-startMem.NumGC)/N, "gc-cycles/op")
	b.ReportMetric(float64(endMem.PauseTotalNs-startMem.PauseTotalNs)/N, "gc-pause-ns/op")
	b.ReportMetric(float64(endMem.Mallocs-startMem.Mallocs)/N, "mallocs/op")
	b.ReportMetric(float64(endMem.TotalAlloc-startMem.TotalAlloc)/N, "total-B/op")
	b.ReportMetric(float64(liveMem.HeapAlloc)/1024, "live-heap-KB")
}

// One benchmark per corpus program, reused-VM path.
func BenchmarkMEP36_Mem_Fib(b *testing.B) {
	benchMem(b, corpus.Program{Name: "fib", Build: corpus.BuildFibRec, Expect: corpus.ExpectFibRec})
}
func BenchmarkMEP36_Mem_IterSum(b *testing.B) {
	benchMem(b, corpus.Program{Name: "iter_sum", Build: corpus.BuildIterSum, Expect: corpus.ExpectIterSum})
}
func BenchmarkMEP36_Mem_PrimeCount(b *testing.B) {
	benchMem(b, corpus.Program{Name: "math_prime_count", Build: corpus.BuildPrimeCount, Expect: corpus.ExpectPrimeCount})
}
func BenchmarkMEP36_Mem_StringsConcatLoop(b *testing.B) {
	benchMem(b, corpus.Program{Name: "strings_concat_loop", Build: corpus.BuildStringsConcatLoop, Expect: corpus.ExpectStringsConcatLoop})
}
func BenchmarkMEP36_Mem_ListsFillSum(b *testing.B) {
	benchMem(b, corpus.Program{Name: "lists_fill_sum", Build: corpus.BuildListsFillSum, Expect: corpus.ExpectListsFillSum})
}
func BenchmarkMEP36_Mem_MapsFillSum(b *testing.B) {
	benchMem(b, corpus.Program{Name: "maps_fill_sum", Build: corpus.BuildMapsFillSum, Expect: corpus.ExpectMapsFillSum})
}

// Fresh-VM path, headline metric for the refactor.
func BenchmarkMEP36_FreshVM_Fib(b *testing.B) {
	benchMemFreshVM(b, corpus.Program{Name: "fib", Build: corpus.BuildFibRec, Expect: corpus.ExpectFibRec})
}
func BenchmarkMEP36_FreshVM_PrimeCount(b *testing.B) {
	benchMemFreshVM(b, corpus.Program{Name: "math_prime_count", Build: corpus.BuildPrimeCount, Expect: corpus.ExpectPrimeCount})
}
func BenchmarkMEP36_FreshVM_StringsConcatLoop(b *testing.B) {
	benchMemFreshVM(b, corpus.Program{Name: "strings_concat_loop", Build: corpus.BuildStringsConcatLoop, Expect: corpus.ExpectStringsConcatLoop})
}
func BenchmarkMEP36_FreshVM_ListsFillSum(b *testing.B) {
	benchMemFreshVM(b, corpus.Program{Name: "lists_fill_sum", Build: corpus.BuildListsFillSum, Expect: corpus.ExpectListsFillSum})
}
func BenchmarkMEP36_FreshVM_MapsFillSum(b *testing.B) {
	benchMemFreshVM(b, corpus.Program{Name: "maps_fill_sum", Build: corpus.BuildMapsFillSum, Expect: corpus.ExpectMapsFillSum})
}
