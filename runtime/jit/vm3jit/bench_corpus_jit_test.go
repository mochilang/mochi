package vm3jit_test

import (
	"testing"
	"unsafe"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// vm3jitCorpusSink defeats dead-store elimination of the per-kernel
// b.N loop's return value across the bench cases.
var vm3jitCorpusSink int64

// BenchmarkCorpusJITRunner runs every compiler3 corpus kernel end-to-end
// after calling vm3jit.CompileProgram on the program. This is the
// Phase 6.2d mirror of MEP-39 §6.15 vm2runner JIT integration: a single
// bench entry-point that measures the runtime every corpus program sees
// once the JIT is wired into the normal entry path (the entry function
// is dispatched through the JIT trampoline if JITCode != nil, otherwise
// the interp runs it).
//
// Kernels whose function shape the JIT cannot compile (Cell-bank uses,
// over-cap register counts, opcodes outside Phase 6.2b coverage) are
// silently skipped by CompileProgram. The interp then runs them at
// full interp speed; their bench numbers are identical to the existing
// BenchmarkMathKernels sub-benches in compiler3/corpus and serve as the
// "still needs Phase 6.2d+ Cell-bank lowering" floor. Container kernels
// (lists_fill_sum, maps_fill_sum, strings_concat_loop) fall into this
// category until Cell-bank JIT lowering lands.
//
// The bench input is perturbed by b.N's parity so Go's compiler cannot
// hoist the call out of the loop body (mirrors BenchmarkGoKernels).
func BenchmarkCorpusJITRunner(b *testing.B) {
	cases := []struct {
		name string
		prog *corpus.Program
		n    int64
	}{
		{"fib_iter_n30", corpus.FibIter, 30},
		{"sum_loop_n10001", corpus.SumLoop, 10001},
		{"mul_loop_n16", corpus.MulLoop, 16},
		{"fact_rec_n12", corpus.FactRec, 12},
		{"fib_rec_n25", corpus.FibRec, 25},
		{"prime_count_n100", corpus.PrimeCount, 100},
		{"f64_dot_sum_n1000", corpus.F64DotSum, 1000},
		{"f64_threshold_n100", corpus.F64Threshold, 100},
		{"strings_concat_loop_n64", corpus.StringsConcatLoop, 64},
		{"lists_fill_sum_n128", corpus.ListsFillSum, 128},
		{"maps_fill_sum_n128", corpus.MapsFillSum, 128},
		{"nsieve_n1000", corpus.Nsieve, 1000},
		{"nsieve_n10000", corpus.Nsieve, 10000},
		{"fasta_n10000", corpus.Fasta, 10000},
		{"fasta_n100000", corpus.Fasta, 100000},
		{"mandelbrot_n100", corpus.Mandelbrot, 100},
		{"mandelbrot_n300", corpus.Mandelbrot, 300},
		{"k_nucleotide_n10000", corpus.KNucleotide, 10000},
		{"k_nucleotide_n100000", corpus.KNucleotide, 100000},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			prog := tc.prog.Build(tc.n)
			cfs := vm3jit.CompileProgram(prog)
			defer func() {
				for _, cf := range cfs {
					if cf != nil {
						_ = cf.Free()
					}
				}
			}()
			fn := prog.Funcs[prog.Entry]
			b.ResetTimer()
			var s int64
			switch {
			case fn.JITCode != nil && fn.NumRegsCell == 0:
				// i64-only (or i64+f64) entry: dispatch through the
				// trampoline directly, the same path JITCallFn uses
				// internally for callee dispatch. Sized regs buffer
				// matches init.go's jitFrame3 so recursive callees
				// (fact_rec, fib_rec) have room.
				regs := make([]int64, 4096)
				var status int64
				entry := fn.JITCode
				if fn.JITHasF64 {
					regsF := make([]float64, vm3jit.MaxF64Regs)
					for i := 0; i < b.N; i++ {
						regs[0] = tc.n + int64(i&1)
						s += int64(trampoline.CallStatusFF(entry,
							unsafe.Pointer(&regs[0]),
							unsafe.Pointer(&status),
							unsafe.Pointer(&regsF[0])))
						if status != 0 {
							b.Fatalf("%s: unexpected deopt status=%d", tc.name, status)
						}
					}
				} else {
					for i := 0; i < b.N; i++ {
						regs[0] = tc.n + int64(i&1)
						s += int64(trampoline.CallStatus(entry,
							unsafe.Pointer(&regs[0]),
							unsafe.Pointer(&status)))
						if status != 0 {
							b.Fatalf("%s: unexpected deopt status=%d", tc.name, status)
						}
					}
				}
			case fn.JITCode != nil:
				// Cell-bank entry (Phase 6.2d.2.b step 2: lists_fill_sum
				// main): route through vm.RunWithArgs which calls into
				// JITCallFn after populating arenaCtx + regsCell from
				// the per-VM jitFrame3. Re-use one VM across iterations
				// so arena setup amortizes out of the per-call cost.
				vm := vm3.NewWithProgram(prog)
				args := []int64{tc.n}
				for i := 0; i < b.N; i++ {
					args[0] = tc.n + int64(i&1)
					got, err := vm.RunWithArgs(fn, args)
					if err != nil {
						b.Fatalf("RunWithArgs %s: %v", tc.name, err)
					}
					s += got.Int()
				}
			default:
				// Entry not JIT-compiled: run the program through the
				// interpreter. JITCallFn still routes any JIT'd callee
				// through native code.
				vm := vm3.NewWithProgram(prog)
				args := []int64{tc.n}
				for i := 0; i < b.N; i++ {
					args[0] = tc.n + int64(i&1)
					got, err := vm.RunWithArgs(fn, args)
					if err != nil {
						b.Fatalf("RunWithArgs %s: %v", tc.name, err)
					}
					s += got.Int()
				}
			}
			vm3jitCorpusSink = s
		})
	}
}
