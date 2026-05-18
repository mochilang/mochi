//go:build linux && amd64

package vm3jit_test

import (
	"math"
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

// BenchmarkSumLoopJIT measures the JIT'd sum_loop at n=10001. Compare
// its ns/op against BenchmarkSumLoopGoFair to compute the vm3jit / Go
// ratio. Phase 6.2a gate: this ratio must be < 2x.
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

func BenchmarkSumLoopGoFair(b *testing.B) {
	const n = int64(10001)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goSumLoopBench(n + int64(i&1))
	}
	vm3jitSink = s
}

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
// The kernel exercises reg-reg OpMulI64 and OpModI64 plus the TEST/JZ
// zero-check guard (which never trips on the happy path, since j
// starts at 2 and increments by 1). Phase 6.2a gate: this ratio must
// clear the 2x-of-Go bar.
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
// CALL lowering. n=15 keeps the result inside i64 (15! = 1307674368000)
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
// CALL sites per activation. n=25 gives ~242k call activations (a real
// recursion-bound kernel; the CALL/spill/reload cost dominates).
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

//go:noinline
func goF64DotSumBench(n int64) float64 {
	var s float64
	for i := int64(0); i < n; i++ {
		s += float64(i) * 0.5
	}
	return s
}

//go:noinline
func goF64ThresholdBench(n int64) int64 {
	for i := int64(1); i <= n; i++ {
		if 1.0/float64(i) < 0.1 {
			return i
		}
	}
	return 0
}

var vm3jitFSink float64

func BenchmarkF64DotSumJIT(b *testing.B) {
	prog := corpus.F64DotSum.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()

	const n = int64(1000)
	var regsI64 [vm3jit.MaxI64Regs]int64
	var regsF64 [vm3jit.MaxF64Regs]float64
	var status int64
	var s float64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regsI64[0] = n + int64(i&1)
		bits := trampoline.CallStatusFF(entry,
			unsafe.Pointer(&regsI64[0]),
			unsafe.Pointer(&status),
			unsafe.Pointer(&regsF64[0]))
		s += math.Float64frombits(bits)
	}
	vm3jitFSink = s
}

func BenchmarkF64DotSumGoFair(b *testing.B) {
	const n = int64(1000)
	var s float64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goF64DotSumBench(n + int64(i&1))
	}
	vm3jitFSink = s
}

func BenchmarkF64DotSumInterp(b *testing.B) {
	prog := corpus.F64DotSum.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(1000)
	var s float64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		got, err := vm.RunWithArgs(fn, []int64{n + int64(i&1)})
		if err != nil {
			b.Fatalf("RunWithArgs: %v", err)
		}
		s += got.Float()
	}
	vm3jitFSink = s
}

func BenchmarkF64ThresholdJIT(b *testing.B) {
	prog := corpus.F64Threshold.Build(0)
	fn := prog.Funcs[prog.Entry]
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		b.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	entry := cf.Entry()

	const n = int64(100)
	var regsI64 [vm3jit.MaxI64Regs]int64
	var regsF64 [vm3jit.MaxF64Regs]float64
	var status int64
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		regsI64[0] = n + int64(i&1)
		s += int64(trampoline.CallStatusFF(entry,
			unsafe.Pointer(&regsI64[0]),
			unsafe.Pointer(&status),
			unsafe.Pointer(&regsF64[0])))
	}
	vm3jitSink = s
}

func BenchmarkF64ThresholdGoFair(b *testing.B) {
	const n = int64(100)
	var s int64
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s += goF64ThresholdBench(n + int64(i&1))
	}
	vm3jitSink = s
}

func BenchmarkF64ThresholdInterp(b *testing.B) {
	prog := corpus.F64Threshold.Build(0)
	fn := prog.Funcs[prog.Entry]
	vm := vm3.NewWithProgram(prog)
	const n = int64(100)
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
