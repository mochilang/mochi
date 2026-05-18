//go:build darwin && arm64

package vm3jit_test

import (
	"testing"
	"unsafe"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// goSumLoopBench is a //go:noinline copy of the same kernel the
// "fair" Go baseline uses. Re-declared here so the bench in this
// package does not need to import the compiler3/corpus tests file.
//
//go:noinline
func goSumLoopBench(n int64) int64 {
	var s int64
	for i := int64(0); i < n; i++ {
		s += i
	}
	return s
}

// vm3jitSink defeats dead-store elimination across the bench loops.
var vm3jitSink int64

// BenchmarkSumLoopJIT measures the JIT'd sum_loop at the same N
// the fair-Go bench uses (10001). Compare its ns/op against
// BenchmarkSumLoopGoFair to compute the vm3jit / Go ratio. Phase 6.0
// gate: this ratio must be < 2x on the n=10001 case.
func BenchmarkSumLoopJIT(b *testing.B) {
	prog := corpus.SumLoop.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()

	const n = int64(10001)
	var regs [vm3jit.MaxI64Regs]int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.Call(entry, unsafe.Pointer(&regs[0])))
	}
	vm3jitSink = s
}

// BenchmarkSumLoopGoFair is the apples-to-apples Go baseline that
// BenchmarkSumLoopJIT compares against. Same N, same parity-perturbed
// input, same package-global sink. Mirrors the shape of
// compiler3/corpus.BenchmarkGoKernelsFair/sum_loop_n10001.
func BenchmarkSumLoopGoFair(b *testing.B) {
	const n = int64(10001)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goSumLoopBench(n + int64(i&1))
	}
	vm3jitSink = s
}

// BenchmarkSumLoopInterp measures the vm3 interpreter on the same
// kernel and N so we can quote "interpreter vs JIT" and "JIT vs Go"
// in the same units.
func BenchmarkSumLoopInterp(b *testing.B) {
	prog := corpus.SumLoop.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(10001)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		got, err := vm.RunWithArgs(fn, []int64{n + int64(i&1)})
		if err != nil {
			b.Fatalf("RunWithArgs: %v", err)
		}
		s += got.Int()
	}
	vm3jitSink = s
}
