// Package bench hosts the MEP-17 VM performance harness. Each
// benchmark loads a single .mochi program from programs/, compiles
// it once, and then drives vm.Run for b.N iterations against the
// compiled program. The compile step is excluded from timing so the
// numbers reflect interpreter cost, not the bytecode compiler.
//
// Programs double as conformance fixtures: TestBenchPrograms runs
// each program once and asserts stdout matches the matching
// expected/<name>.out file. A bench run that produces wrong output
// fails the test before the timing numbers are reported.
//
// The harness is built with -tags=slow so it does not run in the
// default CI sweep. Invoke it with:
//
//	go test -tags=slow -bench=. -benchmem ./runtime/vm/bench/...
//	go test -tags=slow ./runtime/vm/bench/... -run TestBenchPrograms
//
//go:build slow

package bench

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func init() {
	// MEP-17 reproducibility: pin time-derived heuristics so two runs
	// on the same host produce comparable numbers. The seed value is
	// arbitrary but must stay stable across releases.
	os.Setenv("MOCHI_NOW_SEED", "1")
	os.Setenv("MOCHI_BENCH", "1")
}

// benchPrograms lists every fixture in programs/. The list is the
// canonical reference suite from MEP-17 §Specification §Reference
// suite. Names map 1:1 to file basenames and to the headline columns
// in any benchstat report.
var benchPrograms = []string{
	"fib",
	"iter_sum",
	"string_cat",
	"map_get",
	"list_build",
	"struct_field",
	"hof_map",
	"query_select",
	"closure_dispatch",
	"json_emit",
}

// compileProgram parses, type-checks, and bytecode-compiles a single
// program. It is called once per benchmark function before
// b.ResetTimer so compile cost is not folded into the inner-loop
// measurement.
func compileProgram(tb testing.TB, name string) *vm.Program {
	tb.Helper()
	src := filepath.Join("programs", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		tb.Fatalf("parse %s: %v", name, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		tb.Fatalf("type %s: %v", name, errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		tb.Fatalf("compile %s: %v", name, err)
	}
	return p
}

func runOnce(tb testing.TB, p *vm.Program, w io.Writer) {
	tb.Helper()
	m := vm.New(p, w)
	if err := m.Run(); err != nil {
		tb.Fatalf("run: %v", err)
	}
}

func benchOne(b *testing.B, name string) {
	p := compileProgram(b, name)
	b.ReportAllocs()

	var startMem, endMem runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&startMem)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		runOnce(b, p, io.Discard)
	}
	b.StopTimer()

	runtime.ReadMemStats(&endMem)
	// HeapInuse-MB: heap pages in use after the workload finishes,
	// approximating the live-set size the workload pinned. Sys-MB:
	// total bytes obtained from the OS, approximating process RSS.
	// Both are reported per-run (not per-op) so they read directly
	// as peak memory cost of executing the program once.
	b.ReportMetric(float64(endMem.HeapInuse)/1024/1024, "heap-MB")
	b.ReportMetric(float64(endMem.Sys)/1024/1024, "sys-MB")
	b.ReportMetric(float64(endMem.TotalAlloc-startMem.TotalAlloc)/float64(b.N), "total-B/op")
}

func BenchmarkVM_Fib(b *testing.B)             { benchOne(b, "fib") }
func BenchmarkVM_IterSum(b *testing.B)         { benchOne(b, "iter_sum") }
func BenchmarkVM_StringCat(b *testing.B)       { benchOne(b, "string_cat") }
func BenchmarkVM_MapGet(b *testing.B)          { benchOne(b, "map_get") }
func BenchmarkVM_ListBuild(b *testing.B)       { benchOne(b, "list_build") }
func BenchmarkVM_StructField(b *testing.B)     { benchOne(b, "struct_field") }
func BenchmarkVM_HofMap(b *testing.B)          { benchOne(b, "hof_map") }
func BenchmarkVM_QuerySelect(b *testing.B)     { benchOne(b, "query_select") }
func BenchmarkVM_ClosureDispatch(b *testing.B) { benchOne(b, "closure_dispatch") }
func BenchmarkVM_JsonEmit(b *testing.B)        { benchOne(b, "json_emit") }

// TestBenchPrograms double-duties the bench fixtures as a conformance
// sub-suite. Per MEP-17 §Conformance interaction, a bench run that
// produces wrong output is a test failure, not just a timing curiosity.
func TestBenchPrograms(t *testing.T) {
	for _, name := range benchPrograms {
		name := name
		t.Run(name, func(t *testing.T) {
			p := compileProgram(t, name)
			var got bytes.Buffer
			runOnce(t, p, &got)

			expectedPath := filepath.Join("expected", name+".out")
			want, err := os.ReadFile(expectedPath)
			if err != nil {
				t.Fatalf("read expected %s: %v", expectedPath, err)
			}

			gotTrim := bytes.TrimRight(got.Bytes(), "\n")
			wantTrim := bytes.TrimRight(want, "\n")
			if !bytes.Equal(gotTrim, wantTrim) {
				t.Fatalf("output mismatch for %s\n--- got ---\n%s\n--- want ---\n%s\n", name, gotTrim, wantTrim)
			}
		})
	}
}
