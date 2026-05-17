//go:build darwin && arm64

package vm2jit_test

import (
	"math"
	"testing"

	"mochi/runtime/vm2"
)

// MEP-38 Phase 2 (§3.2.1): float JIT lowering correctness tests.
// Each op gets a dedicated function that returns r0 after applying the
// op once. The test compiles the function via vm2jit, runs it with
// callJIT (nil VM, safe for arithmetic-only ops), and compares Cell
// bits against the Go-native float operation.

func cf(f float64) vm2.Cell { return vm2.CFloat(f) }

func TestAddF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "addf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAddF64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want float64 }{
		{1, 2, 3},
		{-0.5, 0.5, 0},
		{1e100, 1e100, 2e100},
	} {
		got := callJIT(fn, regs(2, cf(tc.a), cf(tc.b)))
		if !got.IsFloat() {
			t.Errorf("addf(%v,%v): tag mismatch, bits=%#x", tc.a, tc.b, got.Bits)
			continue
		}
		if got.Float() != tc.want {
			t.Errorf("addf(%v,%v)=%v want %v", tc.a, tc.b, got.Float(), tc.want)
		}
	}
	// Verify bit-identical result for an IEEE-tricky case where 0.1+0.2 != 0.3.
	// Use variables so Go doesn't constant-fold the right-hand side at
	// arbitrary precision (which would mask the inexact-add).
	a, b := 0.1, 0.2
	got := callJIT(fn, regs(2, cf(a), cf(b)))
	if got.Float() != a+b {
		t.Errorf("addf(0.1,0.2)=%v want %v (Go-native)", got.Float(), a+b)
	}
}

func TestSubF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "subf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpSubF64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want float64 }{
		{5, 3, 2},
		{0, 1, -1},
		{1.5, 0.5, 1.0},
	} {
		got := callJIT(fn, regs(2, cf(tc.a), cf(tc.b)))
		if got.Float() != tc.want {
			t.Errorf("subf(%v,%v)=%v want %v", tc.a, tc.b, got.Float(), tc.want)
		}
	}
}

func TestMulF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "mulf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpMulF64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want float64 }{
		{2, 3, 6},
		{-2, 3, -6},
		{0.5, 0.5, 0.25},
	} {
		got := callJIT(fn, regs(2, cf(tc.a), cf(tc.b)))
		if got.Float() != tc.want {
			t.Errorf("mulf(%v,%v)=%v want %v", tc.a, tc.b, got.Float(), tc.want)
		}
	}
}

func TestDivF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "divf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpDivF64, A: 0, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ a, b, want float64 }{
		{6, 2, 3},
		{1, 4, 0.25},
		{-1, 4, -0.25},
	} {
		got := callJIT(fn, regs(2, cf(tc.a), cf(tc.b)))
		if got.Float() != tc.want {
			t.Errorf("divf(%v,%v)=%v want %v", tc.a, tc.b, got.Float(), tc.want)
		}
	}
}

func TestNegF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "negf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpNegF64, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	compileOrSkip(t, fn)
	for _, v := range []float64{3.0, -2.5, 0, 1e100} {
		got := callJIT(fn, regs(2, cf(v)))
		if got.Float() != -v {
			t.Errorf("negf(%v)=%v want %v", v, got.Float(), -v)
		}
	}
}

func TestAbsF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "absf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpAbsF64, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	compileOrSkip(t, fn)
	for _, v := range []float64{3.0, -3.0, 0, math.Copysign(0, -1), 1e100, -1e100} {
		got := callJIT(fn, regs(2, cf(v)))
		if got.Float() != math.Abs(v) {
			t.Errorf("absf(%v)=%v want %v", v, got.Float(), math.Abs(v))
		}
	}
}

func TestSqrtF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "sqrtf", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpSqrtF64, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	compileOrSkip(t, fn)
	for _, v := range []float64{0, 1, 4, 9, 2, 0.25, 1e10} {
		got := callJIT(fn, regs(2, cf(v)))
		if got.Float() != math.Sqrt(v) {
			t.Errorf("sqrtf(%v)=%v want %v", v, got.Float(), math.Sqrt(v))
		}
	}
}

func TestLessF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "lessf", NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpLessF64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	nan := math.NaN()
	for _, tc := range []struct {
		a, b float64
		want bool
	}{
		{1, 2, true},
		{2, 1, false},
		{1, 1, false},
		{-1, 0, true},
		{nan, 1, false},
		{1, nan, false},
		{nan, nan, false},
	} {
		got := callJIT(fn, regs(3, cf(tc.a), cf(tc.b)))
		if !got.IsBool() {
			t.Errorf("lessf(%v,%v): tag mismatch, bits=%#x", tc.a, tc.b, got.Bits)
			continue
		}
		if got.Bool() != tc.want {
			t.Errorf("lessf(%v,%v)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestLessEqF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "lessef", NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpLessEqF64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	nan := math.NaN()
	for _, tc := range []struct {
		a, b float64
		want bool
	}{
		{1, 2, true},
		{2, 1, false},
		{1, 1, true},
		{nan, 1, false},
		{1, nan, false},
	} {
		got := callJIT(fn, regs(3, cf(tc.a), cf(tc.b)))
		if got.Bool() != tc.want {
			t.Errorf("lessef(%v,%v)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestEqualF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "eqf", NumRegs: 3,
		Code: []vm2.Instr{
			{Op: vm2.OpEqualF64, A: 2, B: 0, C: 1},
			{Op: vm2.OpReturn, A: 2},
		},
	}
	compileOrSkip(t, fn)
	nan := math.NaN()
	for _, tc := range []struct {
		a, b float64
		want bool
	}{
		{1, 1, true},
		{1, 2, false},
		{0, math.Copysign(0, -1), true},
		{nan, nan, false},
		{nan, 1, false},
	} {
		got := callJIT(fn, regs(3, cf(tc.a), cf(tc.b)))
		if got.Bool() != tc.want {
			t.Errorf("eqf(%v,%v)=%v want %v", tc.a, tc.b, got.Bool(), tc.want)
		}
	}
}

func TestFmaF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "fmaf", NumRegs: 4,
		Code: []vm2.Instr{
			{Op: vm2.OpFmaF64, A: 3, B: 0, C: 1, D: 2},
			{Op: vm2.OpReturn, A: 3},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ b, c, d, want float64 }{
		{2, 3, 4, 10},                 // 2*3+4 = 10
		{0.5, 0.5, 0.5, 0.75},         // 0.5*0.5+0.5 = 0.75
		{-1, 2, 5, 3},                 // -1*2+5 = 3
		{1, 1, 0, 1},
	} {
		got := callJIT(fn, regs(4, cf(tc.b), cf(tc.c), cf(tc.d)))
		if got.Float() != tc.want {
			t.Errorf("fmaf(%v,%v,%v)=%v want %v", tc.b, tc.c, tc.d, got.Float(), tc.want)
		}
	}
}

func TestI64ToF64(t *testing.T) {
	fn := &vm2.Function{
		Name: "i2f", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpI64ToF64, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	compileOrSkip(t, fn)
	for _, v := range []int64{0, 1, -1, 42, -42, 1 << 30, -(1 << 30)} {
		got := callJIT(fn, regs(2, vm2.CInt(v)))
		if !got.IsFloat() {
			t.Errorf("i2f(%v): tag mismatch, bits=%#x", v, got.Bits)
			continue
		}
		if got.Float() != float64(v) {
			t.Errorf("i2f(%v)=%v want %v", v, got.Float(), float64(v))
		}
	}
}

func TestF64ToI64(t *testing.T) {
	fn := &vm2.Function{
		Name: "f2i", NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpF64ToI64, A: 1, B: 0},
			{Op: vm2.OpReturn, A: 1},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct {
		in   float64
		want int64
	}{
		{0, 0},
		{1.5, 1},
		{-1.5, -1},
		{42.9, 42},
		{-42.9, -42},
	} {
		got := callJIT(fn, regs(2, cf(tc.in)))
		if !got.IsInt() {
			t.Errorf("f2i(%v): tag mismatch, bits=%#x", tc.in, got.Bits)
			continue
		}
		if got.Int() != tc.want {
			t.Errorf("f2i(%v)=%v want %v", tc.in, got.Int(), tc.want)
		}
	}
}

func TestLoadConstF(t *testing.T) {
	want := 3.141592653589793
	fn := &vm2.Function{
		Name: "pi", NumRegs: 1,
		Consts: []vm2.Cell{vm2.CFloat(want)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstF, A: 0, B: 0},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	compileOrSkip(t, fn)
	got := callJIT(fn, regs(1))
	if !got.IsFloat() {
		t.Fatalf("LoadConstF: tag mismatch, bits=%#x", got.Bits)
	}
	if got.Float() != want {
		t.Fatalf("LoadConstF=%v want %v", got.Float(), want)
	}
}

// TestFloatPipelineMandelLike exercises a small composed FP sequence
// of the shape mandelbrot uses on its inner loop:
//   tmp = zr*zr - zi*zi + cr
// without the per-pixel branch. Verifies the JIT-emitted sequence
// returns identical bits to the Go-native computation.
func TestFloatPipelineMandelLike(t *testing.T) {
	// r0 = zr, r1 = zi, r2 = cr; r3 = zr*zr; r4 = zi*zi; r3 = r3 - r4; r3 = r3 + cr.
	fn := &vm2.Function{
		Name: "mandel_step", NumRegs: 5,
		Code: []vm2.Instr{
			{Op: vm2.OpMulF64, A: 3, B: 0, C: 0},
			{Op: vm2.OpMulF64, A: 4, B: 1, C: 1},
			{Op: vm2.OpSubF64, A: 3, B: 3, C: 4},
			{Op: vm2.OpAddF64, A: 3, B: 3, C: 2},
			{Op: vm2.OpReturn, A: 3},
		},
	}
	compileOrSkip(t, fn)
	for _, tc := range []struct{ zr, zi, cr float64 }{
		{0.5, 0.5, -0.7},
		{1.0, 0, 0.25},
		{-0.1, 0.1, -0.1},
	} {
		got := callJIT(fn, regs(5, cf(tc.zr), cf(tc.zi), cf(tc.cr)))
		want := tc.zr*tc.zr - tc.zi*tc.zi + tc.cr
		if got.Float() != want {
			t.Errorf("mandel_step(%v,%v,%v)=%v want %v",
				tc.zr, tc.zi, tc.cr, got.Float(), want)
		}
	}
}
