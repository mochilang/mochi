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

// runJITI64Kernel compiles fn and calls it via the trampoline with a
// single i64 argument in regs[0], returning the i64 result. The regs
// window must live for the duration of the call; the trampoline is
// NOSPLIT so Go's stack cannot grow under it and the &regs[0] pointer
// stays valid.
func runJITI64Kernel(t *testing.T, fn *vm3.Function, arg int64) int64 {
	t.Helper()
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	var regs [vm3jit.MaxI64Regs]int64
	regs[0] = arg
	got := trampoline.Call(cf.Entry(), unsafe.Pointer(&regs[0]))
	return int64(got)
}

// TestCompileSumLoopMatchesInterp runs the JIT'd sum_loop alongside
// the vm3 interpreter across a range of N and confirms bit-identical
// results. This is the Phase 6.0 correctness gate.
func TestCompileSumLoopMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "sum_loop", corpus.SumLoop,
		[]int64{0, 1, 2, 10, 100, 10001})
}

// TestCompileMulLoopMatchesInterp validates the JIT'd mul_loop across
// a range of N. Exercises OpMulI64 + OpAddI64K + OpCmpGeI64Br.
func TestCompileMulLoopMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "mul_loop", corpus.MulLoop,
		[]int64{0, 1, 2, 3, 10, 15})
}

// TestCompileFibIterMatchesInterp validates the JIT'd fib_iter against
// the interpreter across N in the same range corpus_test.go pins
// (0..20). Exercises OpMovI64 + OpAddI64 + OpAddI64K + OpCmpGeI64Br.
// At N >= 90 the interpreter and JIT diverge but the JIT result matches
// the mathematical fib(N); see TestFibIterJITHighN.
func TestCompileFibIterMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "fib_iter", corpus.FibIter,
		[]int64{0, 1, 2, 5, 10, 15, 20})
}

// TestFibIterJITHighN confirms the JIT computes the mathematically
// correct fib(N) for large N where the recurrence still fits in i64
// (i.e. N <= 92). The JIT does not delegate to the interpreter, so any
// discrepancy here is a JIT codegen bug.
func TestFibIterJITHighN(t *testing.T) {
	prog := corpus.FibIter.Build(0)
	fn := prog.Funcs[prog.Entry]
	cases := []struct {
		n, want int64
	}{
		{30, 832040},
		{50, 12586269025},
		{70, 190392490709135},
		{90, 2880067194370816120},
	}
	for _, c := range cases {
		got := runJITI64Kernel(t, fn, c.n)
		if got != c.want {
			t.Errorf("fib_iter(%d): jit=%d want=%d", c.n, got, c.want)
		}
	}
}

func checkKernelAgainstInterp(t *testing.T, name string, prog *corpus.Program, ns []int64) {
	t.Helper()
	p := prog.Build(0)
	fn := p.Funcs[p.Entry]
	for _, n := range ns {
		got := runJITI64Kernel(t, fn, n)
		vm := vm3.NewWithProgram(p)
		want, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("interp %s(%d): %v", name, n, err)
		}
		if got != want.Int() {
			t.Errorf("%s(%d): jit=%d interp=%d", name, n, got, want.Int())
		}
	}
}

// TestWideI64Frame exercises the callee-saved prologue/epilogue by
// compiling a function with NumRegsI64 in the [8, 17] range. Each
// register gets assigned a distinct value, then a chain of moves and
// adds proves all slots are reachable and survive the function-entry
// load + the final epilogue's LDP pops.
func TestWideI64Frame(t *testing.T) {
	// fn(x) := x + 1 + 2 + ... + 11 + 12 = x + 78.
	//   regs[0] = x (param)
	//   regs[1..12] = constants 1..12
	//   regs[0] = regs[0] + regs[k] for k in 1..12
	//   return regs[0]
	const N = 13 // 1 param + 12 consts; all need distinct host regs
	code := []vm3.Op{}
	for k := uint16(1); k <= 12; k++ {
		code = append(code, vm3.MakeOp(vm3.OpConstI64K, k, 0, int16(k)))
	}
	for k := uint16(1); k <= 12; k++ {
		code = append(code, vm3.MakeOp(vm3.OpAddI64, 0, 0, int16(k)))
	}
	code = append(code, vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0))

	fn := &vm3.Function{
		Name:       "wide_chain",
		NumRegsI64: N,
		ParamBanks: []vm3.Bank{vm3.BankI64},
		ResultBank: vm3.BankI64,
		Code:       code,
	}
	for _, x := range []int64{0, 1, 100, -5} {
		got := runJITI64Kernel(t, fn, x)
		want := x + 78
		if got != want {
			t.Errorf("wide_chain(%d) = %d want %d", x, got, want)
		}
	}
}

// TestRejectF64 confirms a function with f64 banks is rejected by the
// 6.0 gate (so callers fall back to the interpreter cleanly).
func TestRejectF64(t *testing.T) {
	fn := &vm3.Function{
		Name:       "f64_reject",
		NumRegsF64: 1,
		ResultBank: vm3.BankF64,
		Code:       []vm3.Op{vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0)},
	}
	if _, err := vm3jit.Compile(fn); err == nil {
		t.Fatal("expected ErrNotImplemented for f64 fn, got nil")
	}
}

// TestRejectTooManyI64 confirms the 6.0 i64 reg cap rejects oversize
// functions so callers fall back to the interpreter.
func TestRejectTooManyI64(t *testing.T) {
	fn := &vm3.Function{
		Name:       "wide_i64",
		NumRegsI64: vm3jit.MaxI64Regs + 1,
		ResultBank: vm3.BankI64,
		Code:       []vm3.Op{vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0)},
	}
	if _, err := vm3jit.Compile(fn); err == nil {
		t.Fatal("expected ErrNotImplemented for too-many-i64 fn, got nil")
	}
}
