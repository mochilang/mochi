package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// buildSumLoopWrapper assembles a 2-function program: outer main(n)
// returns sum_loop(n), where sum_loop is the standard corpus kernel.
// Used by the Phase 6.2c interp -> JIT boundary benches.
func buildSumLoopWrapper() *vm3.Program {
	inner := corpus.SumLoop.Build(0).Funcs[0]
	inner.Name = "sum_loop"
	main := &vm3.Function{
		Name:       "main_calls_sum_loop",
		NumRegsI64: 2,
		ParamBanks: []vm3.Bank{vm3.BankI64},
		ResultBank: vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpCallI64, 1, 0, 1),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	return &vm3.Program{Funcs: []*vm3.Function{main, inner}, Entry: 0}
}

// vm3jitInterpJITSink defeats dead-store elimination on the
// interp+JIT bench loops.
var vm3jitInterpJITSink int64

// BenchmarkInterpToJITSumLoop measures the end-to-end cost of an
// interpreted outer dispatching one OpCallI64 into a JIT'd inner
// (sum_loop) per b.N iteration. The inner runs entirely in native
// code; the outer pays one frame-allocation hop in jitCall plus the
// trampoline crossing. Compared against BenchmarkInterpToJITSumLoopAllInterp
// this isolates the JIT-boundary win on small-to-medium N.
func BenchmarkInterpToJITSumLoop(b *testing.B) {
	prog := buildSumLoopWrapper()
	cf, err := vm3jit.CompileAndCache(prog, 1)
	if err != nil {
		b.Skipf("CompileAndCache: %v (no JIT backend)", err)
	}
	defer cf.Free()
	main := prog.Funcs[0]
	vm := vm3.NewWithProgram(prog)
	const n = int64(1000)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		got, err := vm.RunWithArgs(main, []int64{n + int64(i&1)})
		if err != nil {
			b.Fatalf("RunWithArgs: %v", err)
		}
		s += got.Int()
	}
	vm3jitInterpJITSink = s
}

// BenchmarkInterpToJITSumLoopAllInterp is the same shape but with
// the JIT disabled. The 2-function dispatch (interp main calls interp
// sum_loop) is the baseline that BenchmarkInterpToJITSumLoop must
// beat.
func BenchmarkInterpToJITSumLoopAllInterp(b *testing.B) {
	prog := buildSumLoopWrapper()
	// Do NOT call CompileAndCache: inner.JITCode stays nil so vm3
	// stays in the interp path. JITCompiled is set to true on the
	// caller side so subsequent calls treat the fn as 'tried and
	// declined'; we leave it false here to keep the lookup behavior
	// identical to a fresh Program.
	main := prog.Funcs[0]
	vm := vm3.NewWithProgram(prog)
	const n = int64(1000)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		got, err := vm.RunWithArgs(main, []int64{n + int64(i&1)})
		if err != nil {
			b.Fatalf("RunWithArgs: %v", err)
		}
		s += got.Int()
	}
	vm3jitInterpJITSink = s
}
