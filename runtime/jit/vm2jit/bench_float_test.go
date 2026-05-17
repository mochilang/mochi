//go:build darwin && arm64

package vm2jit_test

// MEP-38 Phase 2 (§3.2.1) float microbenchmarks. Each per-op bench runs
// a one-instruction-then-return function through the JIT (callJITRaw)
// so the timing isolates the lowered op (plus the function prologue,
// epilogue, and trampoline overhead, which are constant across cases).
//
// The hot-loop bench is the real test: a polynomial-evaluation loop
// that does ~6 float ops per iteration. The interpreter baseline runs
// the same shape through vm2.Run().

import (
	"testing"

	"mochi/runtime/jit/vm2jit"
	"mochi/runtime/vm2"
)

// --- per-op float microbenchmarks (JIT only) ---

func BenchmarkJITOpAddF(b *testing.B) {
	fn := &vm2.Function{Name: "op_addf", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpAddF64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CFloat(1.5); r[1] = vm2.CFloat(2.5) })
}

func BenchmarkJITOpMulF(b *testing.B) {
	fn := &vm2.Function{Name: "op_mulf", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpMulF64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CFloat(1.5); r[1] = vm2.CFloat(2.5) })
}

func BenchmarkJITOpDivF(b *testing.B) {
	fn := &vm2.Function{Name: "op_divf", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpDivF64, A: 0, B: 0, C: 1}, {Op: vm2.OpReturn, A: 0},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CFloat(1.5); r[1] = vm2.CFloat(2.5) })
}

func BenchmarkJITOpSqrtF(b *testing.B) {
	fn := &vm2.Function{Name: "op_sqrtf", NumRegs: 2, Code: []vm2.Instr{
		{Op: vm2.OpSqrtF64, A: 1, B: 0}, {Op: vm2.OpReturn, A: 1},
	}}
	benchOp(b, fn, func(r []vm2.Cell) { r[0] = vm2.CFloat(2.0) })
}

func BenchmarkJITOpFmaF(b *testing.B) {
	fn := &vm2.Function{Name: "op_fmaf", NumRegs: 4, Code: []vm2.Instr{
		{Op: vm2.OpFmaF64, A: 3, B: 0, C: 1, D: 2}, {Op: vm2.OpReturn, A: 3},
	}}
	benchOp(b, fn, func(r []vm2.Cell) {
		r[0] = vm2.CFloat(1.5)
		r[1] = vm2.CFloat(2.5)
		r[2] = vm2.CFloat(0.75)
	})
}

// --- per-op interpreter baselines (for ratio reporting) ---

// makeF64BinopInterpProg builds a single-op interpreter Program whose
// main function loads two float constants, applies op, and returns.
func makeF64BinopInterpProg(op vm2.Op, a, c float64) *vm2.Program {
	fn := &vm2.Function{
		Name: "interp_binop", NumRegs: 2,
		Consts: []vm2.Cell{vm2.CFloat(a), vm2.CFloat(c)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstF, A: 0, B: 0},
			{Op: vm2.OpLoadConstF, A: 1, B: 1},
			{Op: op, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	return &vm2.Program{Funcs: []*vm2.Function{fn}, Main: 0}
}

func BenchmarkInterpOpAddF(b *testing.B) {
	prog := makeF64BinopInterpProg(vm2.OpAddF64, 1.5, 2.5)
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

func BenchmarkInterpOpMulF(b *testing.B) {
	prog := makeF64BinopInterpProg(vm2.OpMulF64, 1.5, 2.5)
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}

// --- hot-loop float bench ---
//
// polyHotFn computes the polynomial sum_{i=0..n} (a*i + b)*(c*i + d) /
// (e + f*i) over a loop of n iterations. The body is six float ops
// (mul, add, mul, add, div, add) plus the loop counter. Sized so the
// loop dominates the prologue/epilogue cost.
//
// Registers (within JIT 7-reg budget):
//   r0 = n (loop bound, int)
//   r1 = i (loop counter, int)
//   r2 = acc (accumulator, float)
//   r3 = i_f (i as float, recomputed each iter)
//   r4 = tmp1
//   r5 = tmp2
//   r6 = scratch
//
// Loop body:
//   i_f  = i64_to_f64(i)
//   tmp1 = i_f * 0.5  + 1.0       (replace coefficients with consts)
//   tmp2 = i_f * 0.25 + 2.0
//   tmp1 = tmp1 * tmp2
//   acc  = acc + tmp1
//   i    = i + 1
//   if i < n loop
//
// Six float ops + one int add + one branch per iteration.

func polyHotFn() *vm2.Function {
	half := vm2.CFloat(0.5)
	one := vm2.CFloat(1.0)
	quarter := vm2.CFloat(0.25)
	two := vm2.CFloat(2.0)
	return &vm2.Function{
		Name: "poly_hot", NumRegs: 7,
		Consts: []vm2.Cell{half, one, quarter, two},
		Code: []vm2.Instr{
			// 0-3: load the four constants into r3..r6 (initialized once at entry,
			// but the JIT does not yet treat consts as hoistable (the loads
			// are inside the loop, paying their cost each iteration).
			{Op: vm2.OpI64ToF64, A: 3, B: 1},     // 0: i_f = i64_to_f64(i)
			{Op: vm2.OpLoadConstF, A: 4, B: 0},   // 1: tmp1 = 0.5
			{Op: vm2.OpMulF64, A: 4, B: 4, C: 3}, // 2: tmp1 = tmp1 * i_f
			{Op: vm2.OpLoadConstF, A: 5, B: 1},   // 3: tmp2 = 1.0
			{Op: vm2.OpAddF64, A: 4, B: 4, C: 5}, // 4: tmp1 = tmp1 + 1.0
			{Op: vm2.OpLoadConstF, A: 5, B: 2},   // 5: tmp2 = 0.25
			{Op: vm2.OpMulF64, A: 5, B: 5, C: 3}, // 6: tmp2 = tmp2 * i_f
			{Op: vm2.OpLoadConstF, A: 6, B: 3},   // 7: scratch = 2.0
			{Op: vm2.OpAddF64, A: 5, B: 5, C: 6}, // 8: tmp2 = tmp2 + 2.0
			{Op: vm2.OpMulF64, A: 4, B: 4, C: 5}, // 9: tmp1 = tmp1 * tmp2
			{Op: vm2.OpAddF64, A: 2, B: 2, C: 4}, // 10: acc = acc + tmp1
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1}, // 11: i = i + 1
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 0, C: 0}, // 12: if i<n goto 0
			{Op: vm2.OpReturn, A: 2},                    // 13: return acc
		},
	}
}

func polyHotInterpProg(n int64) *vm2.Program {
	fn := polyHotFn()
	// Wrap with a prelude that initialises r0=n, r1=0, r2=0.0. Use a small
	// trampoline function (Main=0) that loads the constants and falls through.
	main := &vm2.Function{
		Name: "poly_main", NumRegs: 7,
		Consts: append([]vm2.Cell{vm2.CInt(n), vm2.CInt(0), vm2.CFloat(0)}, fn.Consts...),
		Code: append([]vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},
			{Op: vm2.OpLoadConstI, A: 1, B: 1},
			{Op: vm2.OpLoadConstF, A: 2, B: 2},
		}, []vm2.Instr{
			// Loop body, shifted by 3 instructions for the prelude.
			{Op: vm2.OpI64ToF64, A: 3, B: 1},
			{Op: vm2.OpLoadConstF, A: 4, B: 3},
			{Op: vm2.OpMulF64, A: 4, B: 4, C: 3},
			{Op: vm2.OpLoadConstF, A: 5, B: 4},
			{Op: vm2.OpAddF64, A: 4, B: 4, C: 5},
			{Op: vm2.OpLoadConstF, A: 5, B: 5},
			{Op: vm2.OpMulF64, A: 5, B: 5, C: 3},
			{Op: vm2.OpLoadConstF, A: 6, B: 6},
			{Op: vm2.OpAddF64, A: 5, B: 5, C: 6},
			{Op: vm2.OpMulF64, A: 4, B: 4, C: 5},
			{Op: vm2.OpAddF64, A: 2, B: 2, C: 4},
			{Op: vm2.OpAddI64K, A: 1, B: 1, C: 1},
			{Op: vm2.OpJumpIfLessI64, A: 1, B: 0, C: 3},
			{Op: vm2.OpReturn, A: 2},
		}...),
	}
	return &vm2.Program{Funcs: []*vm2.Function{main}, Main: 0}
}

func BenchmarkJITPolyHot1k(b *testing.B) {
	fn := polyHotFn()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		b.Skipf("Compile: %v", err)
	}
	defer cf.Free()
	r := regs(7)
	b.ResetTimer()
	for range b.N {
		r[0] = vm2.CInt(1000)
		r[1] = vm2.CInt(0)
		r[2] = vm2.CFloat(0)
		callJITRaw(fn, r)
	}
}

func BenchmarkInterpPolyHot1k(b *testing.B) {
	prog := polyHotInterpProg(1000)
	vm := vm2.New(prog)
	b.ResetTimer()
	for range b.N {
		_, _ = vm.Run()
	}
}
