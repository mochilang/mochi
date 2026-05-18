package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// buildInterpJITProgram constructs a 2-function vm3 Program where
//
//	main(n) returns inner(n)
//	inner(n) returns n*3 + 7
//
// The point is to exercise vm3's OpCallI64 dispatch routing through
// the JIT trampoline when inner.JITCode is set. main itself stays
// interpreted (no JITCode), so the test isolates the interp -> JIT
// call boundary added in Phase 6.2c.
func buildInterpJITProgram() *vm3.Program {
	inner := &vm3.Function{
		Name:       "inner",
		NumRegsI64: 2,
		ParamBanks: []vm3.Bank{vm3.BankI64},
		ResultBank: vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpMulI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpAddI64K, 0, 0, 7),
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
		},
	}
	main := &vm3.Function{
		Name:       "main",
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

// TestInterpToJITCallBoundary exercises the Phase 6.2c hook:
// vm3.JITCallFn is registered by vm3jit's init; CompileAndCache(prog,
// 1) populates inner.JITCode; then vm.RunWithArgs runs main which
// OpCallI64's into inner. The interp dispatch sees JITCode != nil and
// routes through the trampoline. We validate by checking the result
// matches the interp-only baseline and by asserting JITCode is set.
func TestInterpToJITCallBoundary(t *testing.T) {
	prog := buildInterpJITProgram()
	inner := prog.Funcs[1]
	main := prog.Funcs[0]

	cf, err := vm3jit.CompileAndCache(prog, 1)
	if err != nil {
		// Skip on hosts without a JIT backend (e.g. windows, linux/arm64).
		t.Skipf("CompileAndCache: %v (no JIT backend on this host)", err)
	}
	defer cf.Free()
	if inner.JITCode == nil {
		t.Fatalf("CompileAndCache succeeded but inner.JITCode is nil")
	}
	if main.JITCode != nil {
		t.Fatalf("main.JITCode unexpectedly set; only inner should be JIT'd")
	}

	vm := vm3.NewWithProgram(prog)
	for _, n := range []int64{0, 1, 2, 10, 100} {
		got, err := vm.RunWithArgs(main, []int64{n})
		if err != nil {
			t.Fatalf("RunWithArgs n=%d: %v", n, err)
		}
		want := n*3 + 7
		if got.Int() != want {
			t.Fatalf("interp+JIT n=%d: got %d want %d", n, got.Int(), want)
		}
	}
}

// TestInterpToJITCallBoundaryDeoptFalls exercises the deopt branch of
// the OpCallI64 hook. If the JIT trampoline reports status != 0 we
// return deopt=true and the interpreter restarts the callee under
// pushFrame. Phase 6.2c uses this on any reg-reg Div/Mod by-zero; we
// cannot trivially trigger that from a 2-function corpus, so the test
// here only validates that the deopt code path compiles and does not
// regress the happy path. A real div-by-zero deopt test lives in the
// vm3jit_*_test.go arch files.
func TestInterpToJITCallBoundaryDeoptFalls(t *testing.T) {
	prog := buildInterpJITProgram()
	cf, err := vm3jit.CompileAndCache(prog, 1)
	if err != nil {
		t.Skipf("CompileAndCache: %v", err)
	}
	defer cf.Free()
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[0], []int64{42})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 42*3+7 {
		t.Fatalf("got %d want %d", got.Int(), 42*3+7)
	}
}
