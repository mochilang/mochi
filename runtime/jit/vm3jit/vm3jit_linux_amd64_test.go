//go:build linux && amd64

package vm3jit_test

import (
	"testing"
	"unsafe"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// maxI64RegsAMD64 mirrors the internal AMD64 i64 cap. The wide_chain
// test sizes its chain to fit. Slots 0..5 land in RSI/RDI/R8/R9/R10/R11
// (caller-saved); slots 6..8 land in R12/R13/R14 (callee-saved, pushed
// in prologue).
const maxI64RegsAMD64 = 9

// runJITI64Kernel compiles fn and calls it via the status-word
// trampoline with a single i64 argument in regs[0], returning the i64
// result. On a non-zero status the test fails (callers that expect a
// deopt path should use runJITI64KernelStatus instead).
func runJITI64Kernel(t *testing.T, fn *vm3.Function, arg int64) int64 {
	t.Helper()
	got, status := runJITI64KernelStatus(t, fn, arg)
	if status != vm3jit.StatusOK {
		t.Fatalf("unexpected deopt: status=%d", status)
	}
	return got
}

func runJITI64KernelStatus(t *testing.T, fn *vm3.Function, arg int64) (int64, int64) {
	t.Helper()
	cf, err := vm3jit.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	defer cf.Free()
	var regs [vm3jit.MaxI64Regs]int64
	regs[0] = arg
	var status int64
	got := trampoline.CallStatus(cf.Entry(), unsafe.Pointer(&regs[0]), unsafe.Pointer(&status))
	return int64(got), status
}

func TestCompileSumLoopMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "sum_loop", corpus.SumLoop,
		[]int64{0, 1, 2, 10, 100, 10001})
}

func TestCompileMulLoopMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "mul_loop", corpus.MulLoop,
		[]int64{0, 1, 2, 3, 10, 15})
}

func TestCompileFibIterMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "fib_iter", corpus.FibIter,
		[]int64{0, 1, 2, 5, 10, 15, 20})
}

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

// TestWideI64Frame exercises the callee-saved prologue/epilogue on
// AMD64. NumRegsI64 = 9 spans slots 0..8: slots 0..5 land in
// RSI/RDI/R8/R9/R10/R11 (caller-saved); slots 6..8 land in R12/R13/R14
// (callee-saved, pushed in the prologue). The chain proves every slot
// is reachable and survives function entry plus the epilogue POPs.
func TestWideI64Frame(t *testing.T) {
	// fn(x) := x + 1 + 2 + ... + 7 + 8 = x + 36.
	//   regs[0] = x (param)
	//   regs[1..8] = constants 1..8
	//   regs[0] = regs[0] + regs[k] for k in 1..8
	//   return regs[0]
	const N = maxI64RegsAMD64
	code := []vm3.Op{}
	for k := uint16(1); k <= 8; k++ {
		code = append(code, vm3.MakeOp(vm3.OpConstI64K, k, 0, int16(k)))
	}
	for k := uint16(1); k <= 8; k++ {
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
		want := x + 36
		if got != want {
			t.Errorf("wide_chain(%d) = %d want %d", x, got, want)
		}
	}
}

func TestCompileDivModI64(t *testing.T) {
	cases := []struct {
		op   vm3.OpCode
		b, c int64
		want int64
	}{
		{vm3.OpDivI64, 100, 7, 100 / 7},
		{vm3.OpDivI64, -100, 7, -100 / 7},
		{vm3.OpDivI64, 100, -7, 100 / -7},
		{vm3.OpModI64, 100, 7, 100 % 7},
		{vm3.OpModI64, -100, 7, -100 % 7},
		{vm3.OpModI64, 100, -7, 100 % -7},
	}
	for _, c := range cases {
		fn := &vm3.Function{
			Name:       "div_mod",
			NumRegsI64: 3,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64KW, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64KW, 2, 0, 1),
				vm3.MakeOp(c.op, 0, 1, 2),
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
			Consts: []vm3.Cell{vm3.CInt(c.b), vm3.CInt(c.c)},
		}
		got := runJITI64Kernel(t, fn, 0)
		if got != c.want {
			t.Errorf("%d %v %d: jit=%d want=%d", c.b, c.op, c.c, got, c.want)
		}
	}
}

// TestDivByZeroDeopt confirms the zero-check guard on reg-reg OpDivI64
// routes to the shared deopt block, which writes StatusDivByZero through
// R15 before returning. The result value is undefined and the test
// inspects only the status word.
func TestDivByZeroDeopt(t *testing.T) {
	fn := &vm3.Function{
		Name:       "div_by_zero",
		NumRegsI64: 2,
		ParamBanks: []vm3.Bank{vm3.BankI64},
		ResultBank: vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 1),
			vm3.MakeOp(vm3.OpDivI64, 0, 1, 0),
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
		},
	}
	_, status := runJITI64KernelStatus(t, fn, 0)
	if status != vm3jit.StatusDivByZero {
		t.Fatalf("status: got %d want StatusDivByZero=%d", status, vm3jit.StatusDivByZero)
	}
	got, status := runJITI64KernelStatus(t, fn, 7)
	if status != vm3jit.StatusOK {
		t.Fatalf("happy-path status: got %d want OK", status)
	}
	if got != 0 {
		t.Errorf("1/7: got %d want 0", got)
	}
}

func TestCompilePrimeCountMatchesInterp(t *testing.T) {
	checkKernelAgainstInterp(t, "prime_count", corpus.PrimeCount,
		[]int64{2, 10, 100, 1000})
}

func TestCompileFactRecMatchesInterp(t *testing.T) {
	prog := corpus.FactRec.Build(0)
	for _, n := range []int64{0, 1, 2, 3, 5, 10, 12, 15} {
		got := runJITProgRecursive(t, prog, n)
		vm := vm3.NewWithProgram(prog)
		want, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		if err != nil {
			t.Fatalf("interp fact_rec(%d): %v", n, err)
		}
		if got != want.Int() {
			t.Errorf("fact_rec(%d): jit=%d interp=%d", n, got, want.Int())
		}
	}
}

func TestCompileFibRecMatchesInterp(t *testing.T) {
	prog := corpus.FibRec.Build(0)
	for _, n := range []int64{0, 1, 2, 5, 10, 15, 20} {
		got := runJITProgRecursive(t, prog, n)
		vm := vm3.NewWithProgram(prog)
		want, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		if err != nil {
			t.Fatalf("interp fib_rec(%d): %v", n, err)
		}
		if got != want.Int() {
			t.Errorf("fib_rec(%d): jit=%d interp=%d", n, got, want.Int())
		}
	}
}

func TestRejectNonSelfCallI64(t *testing.T) {
	fn := &vm3.Function{
		Name:       "bad_call",
		NumRegsI64: 2,
		ParamBanks: []vm3.Bank{vm3.BankI64},
		ResultBank: vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpCallI64, 1, 0, 7),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	prog := &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	if _, err := vm3jit.CompileInProgram(prog, 0); err == nil {
		t.Fatal("expected ErrNotImplemented for non-self CallI64, got nil")
	}
}

func runJITProgRecursive(t *testing.T, prog *vm3.Program, arg int64) int64 {
	t.Helper()
	cf, err := vm3jit.CompileInProgram(prog, prog.Entry)
	if err != nil {
		t.Fatalf("CompileInProgram: %v", err)
	}
	defer cf.Free()
	regs := make([]int64, 8192)
	regs[0] = arg
	var status int64
	got := trampoline.CallStatus(cf.Entry(), unsafe.Pointer(&regs[0]), unsafe.Pointer(&status))
	if status != vm3jit.StatusOK {
		t.Fatalf("unexpected deopt: status=%d", status)
	}
	return int64(got)
}

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

// TestRejectTooManyI64 confirms the AMD64 i64 reg cap rejects oversize
// functions so callers fall back to the interpreter. The AMD64 cap is
// 9 (vs the AArch64 cap of 17, exported as MaxI64Regs); we exercise
// the cap at maxI64RegsAMD64 + 1.
func TestRejectTooManyI64(t *testing.T) {
	fn := &vm3.Function{
		Name:       "wide_i64",
		NumRegsI64: maxI64RegsAMD64 + 1,
		ResultBank: vm3.BankI64,
		Code:       []vm3.Op{vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0)},
	}
	if _, err := vm3jit.Compile(fn); err == nil {
		t.Fatal("expected ErrNotImplemented for too-many-i64 fn, got nil")
	}
}
