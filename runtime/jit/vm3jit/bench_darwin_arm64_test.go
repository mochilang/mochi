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

//go:noinline
func goMulLoopBench(n int64) int64 {
	p := int64(1)
	for i := int64(1); i < n; i++ {
		p *= i
	}
	return p
}

//go:noinline
func goFibIterBench(n int64) int64 {
	a, b := int64(0), int64(1)
	for i := int64(0); i < n; i++ {
		a, b = b, a+b
	}
	return a
}

// BenchmarkMulLoopJIT measures the JIT'd mul_loop. n=16 matches
// compiler3/corpus benches so cross-comparison is apples-to-apples.
func BenchmarkMulLoopJIT(b *testing.B) {
	prog := corpus.MulLoop.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()
	const n = int64(16)
	var regs [vm3jit.MaxI64Regs]int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.Call(entry, unsafe.Pointer(&regs[0])))
	}
	vm3jitSink = s
}

func BenchmarkMulLoopGoFair(b *testing.B) {
	const n = int64(16)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goMulLoopBench(n + int64(i&1))
	}
	vm3jitSink = s
}

func BenchmarkMulLoopInterp(b *testing.B) {
	prog := corpus.MulLoop.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(16)
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

// BenchmarkFibIterJIT measures the JIT'd fib_iter at the same N
// compiler3/corpus benches use (30).
func BenchmarkFibIterJIT(b *testing.B) {
	prog := corpus.FibIter.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()
	const n = int64(30)
	var regs [vm3jit.MaxI64Regs]int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.Call(entry, unsafe.Pointer(&regs[0])))
	}
	vm3jitSink = s
}

func BenchmarkFibIterGoFair(b *testing.B) {
	const n = int64(30)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goFibIterBench(n + int64(i&1))
	}
	vm3jitSink = s
}

func BenchmarkFibIterInterp(b *testing.B) {
	prog := corpus.FibIter.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(30)
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

//go:noinline
func goPrimeCountBench(n int64) int64 {
	count := int64(0)
	for i := int64(2); i < n; i++ {
		isPrime := int64(1)
		for j := int64(2); j*j <= i; j++ {
			if i%j == 0 {
				isPrime = 0
				break
			}
		}
		if isPrime != 0 {
			count++
		}
	}
	return count
}

// BenchmarkPrimeCountJIT measures the JIT'd prime_count at N=1000.
// The kernel exercises reg-reg OpMulI64 and OpModI64 plus the CBZ
// /0 guard (which never trips on the happy path, since j starts at 2
// and increments by 1). Phase 6.1c gate: this ratio must clear the
// 2x-of-Go bar.
func BenchmarkPrimeCountJIT(b *testing.B) {
	prog := corpus.PrimeCount.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()
	const n = int64(1000)
	var regs [vm3jit.MaxI64Regs]int64
	var status int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.CallStatus(entry, unsafe.Pointer(&regs[0]), unsafe.Pointer(&status)))
		if status != 0 {
			b.Fatalf("unexpected deopt: status=%d", status)
		}
	}
	vm3jitSink = s
}

func BenchmarkPrimeCountGoFair(b *testing.B) {
	const n = int64(1000)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goPrimeCountBench(n + int64(i&1))
	}
	vm3jitSink = s
}

//go:noinline
func goFactRecBench(n int64) int64 {
	if n <= 1 {
		return 1
	}
	return n * goFactRecBench(n-1)
}

//go:noinline
func goFibRecBench(n int64) int64 {
	if n < 2 {
		return n
	}
	return goFibRecBench(n-1) + goFibRecBench(n-2)
}

// BenchmarkFactRecJIT measures the JIT'd fact_rec with self-recursive
// BL lowering. n=15 keeps the result inside i64 (15! = 1307674368000)
// and gives a deep enough call stack to make the spill/reload sequence
// visible in the timing.
func BenchmarkFactRecJIT(b *testing.B) {
	prog := corpus.FactRec.Build(0)
	cf, err := vm3jit.CompileInProgram(prog, prog.Entry)
	if err != nil {
		b.Fatalf("CompileInProgram: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()
	regs := make([]int64, 8192)
	const n = int64(15)
	var status int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.CallStatus(entry, unsafe.Pointer(&regs[0]), unsafe.Pointer(&status)))
		if status != 0 {
			b.Fatalf("unexpected deopt: status=%d", status)
		}
	}
	vm3jitSink = s
}

func BenchmarkFactRecGoFair(b *testing.B) {
	const n = int64(15)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goFactRecBench(n + int64(i&1))
	}
	vm3jitSink = s
}

func BenchmarkFactRecInterp(b *testing.B) {
	prog := corpus.FactRec.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(15)
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

// BenchmarkFibRecJIT measures the JIT'd fib_rec with two self-recursive
// BL sites per activation. n=25 gives ~242k call activations (a real
// recursion-bound kernel; the BL/spill/reload cost dominates).
func BenchmarkFibRecJIT(b *testing.B) {
	prog := corpus.FibRec.Build(0)
	cf, err := vm3jit.CompileInProgram(prog, prog.Entry)
	if err != nil {
		b.Fatalf("CompileInProgram: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()
	regs := make([]int64, 8192)
	const n = int64(25)
	var status int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regs[0] = n + int64(i&1)
		s += int64(trampoline.CallStatus(entry, unsafe.Pointer(&regs[0]), unsafe.Pointer(&status)))
		if status != 0 {
			b.Fatalf("unexpected deopt: status=%d", status)
		}
	}
	vm3jitSink = s
}

func BenchmarkFibRecGoFair(b *testing.B) {
	const n = int64(25)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goFibRecBench(n + int64(i&1))
	}
	vm3jitSink = s
}

func BenchmarkFibRecInterp(b *testing.B) {
	prog := corpus.FibRec.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(25)
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

func BenchmarkPrimeCountInterp(b *testing.B) {
	prog := corpus.PrimeCount.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(1000)
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
