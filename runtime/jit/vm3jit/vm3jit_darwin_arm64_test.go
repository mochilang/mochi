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

// runJITSumLoop compiles sum_loop and calls it via the trampoline,
// returning the i64 result. The regs window must live for the
// duration of the call; the trampoline is NOSPLIT so Go's stack
// cannot grow under it and the &regs[0] pointer stays valid.
func runJITSumLoop(t *testing.T, fn *vm3.Function, arg int64) int64 {
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
	prog := corpus.SumLoop.Build(0)
	fn := prog.Funcs[prog.Entry]
	for _, n := range []int64{0, 1, 2, 10, 100, 10001} {
		got := runJITSumLoop(t, fn, n)
		vm := vm3.NewWithProgram(prog)
		want, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("interp sum_loop(%d): %v", n, err)
		}
		if got != want.Int() {
			t.Errorf("sum_loop(%d): jit=%d interp=%d", n, got, want.Int())
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
