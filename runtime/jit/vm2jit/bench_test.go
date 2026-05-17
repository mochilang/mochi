//go:build darwin && arm64

package vm2jit_test

// Phase 1 benchmark suite for MEP-34 §Benchmark plan.
// Covers: arith/fib_iter, arith/sum_n, per-opcode microbenchmarks.
// List opcodes are excluded from Phase 1 (slow-path infrastructure is Phase 1.5).
//
// Run with:
//   go test -bench=. -benchtime=5s -count=5 ./runtime/jit/vm2jit/
// then pipe through benchstat for noise-corrected numbers.

import (
	"testing"
	"unsafe"

	"mochi/runtime/jit/vm2jit"
	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/vm2"
)

// --- fib_iter helpers ---

// fibIterFn builds the JIT-able Fibonacci iterative function.
//
//	r0 = n (input)
//	r1 = a = 0
//	r2 = b = 1
//	r3 = i = 0
//	r4 = tmp
//
// Code:
//
//	0: if r3 >= r0 goto 6     (OpJumpIfGreaterEqI64)
//	1: r4 = r1 + r2           (OpAddI64)
//	2: r1 = r2                (OpMove)
//	3: r2 = r4                (OpMove)
//	4: r3 = r3 + 1            (OpAddI64K)
//	5: jump 0                 (OpJump)
//	6: return r1              (OpReturn)
func fibIterFn() *vm2.Function {
	return &vm2.Function{
		Name:    "fib_iter",
		NumRegs: 5,
		Code: []vm2.Instr{
			{Op: vm2.OpJumpIfGreaterEqI64, A: 3, B: 0, C: 6}, // 0: if i>=n goto 6
			{Op: vm2.OpAddI64, A: 4, B: 1, C: 2},              // 1: tmp = a+b
			{Op: vm2.OpMove, A: 1, B: 2},                       // 2: a = b
			{Op: vm2.OpMove, A: 2, B: 4},                       // 3: b = tmp
			{Op: vm2.OpAddI64K, A: 3, B: 3, C: 1},             // 4: i++
			{Op: vm2.OpJump, A: 0},                             // 5: goto 0
			{Op: vm2.OpReturn, A: 1},                           // 6: return a
		},
	}
}

// fibInterp builds the equivalent vm2 program for the interpreter benchmark.
// Uses OpLoadConstI to initialise registers since vm.Run() starts fresh.
func fibInterpProg(n int64) *vm2.Program {
	fn := &vm2.Function{
		Name:    "fib_iter_interp",
		NumRegs: 5,
		Consts: []vm2.Cell{
			vm2.CInt(n), // 0: n
			vm2.CInt(0), // 1: a=0
			vm2.CInt(1), // 2: b=1
			vm2.CInt(0), // 3: i=0
		},
		Code: []vm2.Instr{
			// setup (adds 4 extra instrs vs JIT benchmark, which pre-sets regs)
			{Op: vm2.OpLoadConstI, A: 0, B: 0}, // r0 = n
			{Op: vm2.OpLoadConstI, A: 1, B: 1}, // r1 = 0
			{Op: vm2.OpLoadConstI, A: 2, B: 2}, // r2 = 1
			{Op: vm2.OpLoadConstI, A: 3, B: 3}, // r3 = 0
			// loop at instr 4
			{Op: vm2.OpJumpIfGreaterEqI64, A: 3, B: 0, C: 10}, // 4: if i>=n goto 10
			{Op: vm2.OpAddI64, A: 4, B: 1, C: 2},               // 5: tmp = a+b
			{Op: vm2.OpMove, A: 1, B: 2},                        // 6: a = b
			{Op: vm2.OpMove, A: 2, B: 4},                        // 7: b = tmp
			{Op: vm2.OpAddI64K, A: 3, B: 3, C: 1},              // 8: i++
			{Op: vm2.OpJump, A: 4},                              // 9: goto 4
			{Op: vm2.OpReturn, A: 1},                            // 10: return a
		},
	}
	return &vm2.Program{Funcs: []*vm2.Function{fn}, Main: 0}
}

// --- fib_iter correctness ---

func TestFibIter(t *testing.T) {
	fn := fibIterFn()
	compileOrSkip(t, fn)

	want := []struct {
		n, fib int64
	}{
		{0, 0}, {1, 1}, {2, 1}, {3, 2}, {5, 5},
		{10, 55}, {20, 6765}, {30, 832040},
	}
	for _, tc := range want {
		r := regs(5, vm2.CInt(tc.n), vm2.CInt(0), vm2.CInt(1), vm2.CInt(0))
		got := callJIT(fn, r)
		if got.Int() != tc.fib {
			t.Errorf("fib(%d)=%d want %d", tc.n, got.Int(), tc.fib)
		}
	}
}

// callJITRaw calls the JIT function directly via trampoline (used by bench helpers).
func callJITRaw(fn *vm2.Function, r []vm2.Cell) vm2.Cell {
	return vm2.Cell(trampoline.Call(fn.JITCode, unsafe.Pointer(&r[0])))
}

// --- fib_iter benchmarks ---

func BenchmarkJITFibIter20(b *testing.B) {
	fn := fibIterFn()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	r := regs(5, vm2.CInt(20), vm2.CInt(0), vm2.CInt(1), vm2.CInt(0))
	b.ResetTimer()
	for range b.N {
		r[0] = vm2.CInt(20)
		r[1] = vm2.CInt(0)
		r[2] = vm2.CInt(1)
		r[3] = vm2.CInt(0)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpFibIter20(b *testing.B) {
	prog := fibInterpProg(20)
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

func BenchmarkJITFibIter100(b *testing.B) {
	fn := fibIterFn()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()

	r := regs(5)
	b.ResetTimer()
	for range b.N {
		r[0] = vm2.CInt(100)
		r[1] = vm2.CInt(0)
		r[2] = vm2.CInt(1)
		r[3] = vm2.CInt(0)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpFibIter100(b *testing.B) {
	prog := fibInterpProg(100)
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

// --- sum_n at multiple loop counts ---

func makeSumFn(name string) *vm2.Function {
	return &vm2.Function{
		Name:    name,
		NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 0},
			{Op: vm2.OpReturn, A: 0},
		},
	}
}

func makeSumInterpProg(n int64) *vm2.Program {
	fn := &vm2.Function{
		Name:    "sum_n_interp",
		NumRegs: 3,
		Consts: []vm2.Cell{vm2.CInt(0), vm2.CInt(0), vm2.CInt(n)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},
			{Op: vm2.OpLoadConstI, A: 1, B: 1},
			{Op: vm2.OpLoadConstI, A: 2, B: 2},
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 2, C: 3},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	return &vm2.Program{Funcs: []*vm2.Function{fn}, Main: 0}
}

func BenchmarkJITSumN100(b *testing.B) {
	fn := makeSumFn("sum_100")
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("%v", err)
	}
	defer cf.Free()
	r := regs(3)
	b.ResetTimer()
	for range b.N {
		r[0], r[1], r[2] = vm2.CInt(0), vm2.CInt(0), vm2.CInt(100)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpSumN100(b *testing.B) {
	vm := vm2.New(makeSumInterpProg(100))
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

func BenchmarkJITSumN1k(b *testing.B) {
	fn := makeSumFn("sum_1k")
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("%v", err)
	}
	defer cf.Free()
	r := regs(3)
	b.ResetTimer()
	for range b.N {
		r[0], r[1], r[2] = vm2.CInt(0), vm2.CInt(0), vm2.CInt(1000)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpSumN1k(b *testing.B) {
	vm := vm2.New(makeSumInterpProg(1000))
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

func BenchmarkJITSumN10k(b *testing.B) {
	fn := makeSumFn("sum_10k")
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("%v", err)
	}
	defer cf.Free()
	r := regs(3)
	b.ResetTimer()
	for range b.N {
		r[0], r[1], r[2] = vm2.CInt(0), vm2.CInt(0), vm2.CInt(10000)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpSumN10k(b *testing.B) {
	vm := vm2.New(makeSumInterpProg(10000))
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

// --- per-opcode microbenchmarks ---

func benchOp(b *testing.B, fn *vm2.Function, initRegs func([]vm2.Cell)) {
	b.Helper()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()
	r := make([]vm2.Cell, fn.NumRegs)
	b.ResetTimer()
	for range b.N {
		initRegs(r)
		callJITRaw(fn, r)
	}
}

func BenchmarkJITOpAdd(b *testing.B) {
	fn := &vm2.Function{Name: "op_add", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpAddI64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CInt(3); r[1] = vm2.CInt(4) })
}

func BenchmarkJITOpMul(b *testing.B) {
	fn := &vm2.Function{Name: "op_mul", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpMulI64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CInt(6); r[1] = vm2.CInt(7) })
}

func BenchmarkJITOpDiv(b *testing.B) {
	fn := &vm2.Function{Name: "op_div", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpDivI64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CInt(100); r[1] = vm2.CInt(7) })
}

func BenchmarkJITOpMod(b *testing.B) {
	fn := &vm2.Function{Name: "op_mod", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpModI64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CInt(100); r[1] = vm2.CInt(7) })
}

func BenchmarkJITOpLess(b *testing.B) {
	fn := &vm2.Function{Name: "op_less", NumRegs: 3, Code: []vm2.Instr{
		{Op: vm2.OpLessI64, A: 2, B: 0, C: 1}, {Op: vm2.OpReturn, A: 2},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CInt(3); r[1] = vm2.CInt(5) })
}
