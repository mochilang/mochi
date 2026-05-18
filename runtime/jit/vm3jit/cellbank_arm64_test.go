//go:build darwin && arm64

package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListsFillSumSumKernelCompiles confirms the Phase 6.2d.2.a step 2
// gate: vm3jit.CompileProgram admits the sum kernel (idx=2) of
// lists_fill_sum on ARM64. main (idx=0) uses OpNewList which is outside
// the sum-shape whitelist and is expected to fall back to interp; fill
// (idx=1) uses OpListPushI64 which is also outside the whitelist.
func TestListsFillSumSumKernelCompiles(t *testing.T) {
	prog := corpus.ListsFillSum.Build(128)
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	if got := prog.Funcs[2].JITCode; got == nil {
		t.Fatalf("sum (idx=2) did not compile: JITCode is nil")
	}
	if got := prog.Funcs[1].JITCode; got != nil {
		t.Fatalf("fill (idx=1) unexpectedly compiled: should fall back to interp pending Phase 6.2d.2.c")
	}
	if got := prog.Funcs[0].JITCode; got != nil {
		t.Fatalf("main (idx=0) unexpectedly compiled: should fall back to interp pending OpNewList lowering")
	}
}

// TestListsFillSumEndToEnd runs the full corpus.ListsFillSum program
// through vm3 with the JIT installed. Main runs in the interpreter; its
// inner OpCallMixed -> sum site routes through the JIT'd code page via
// JITCallFn. The expected sum of [0, n) is (n-1)*n/2.
func TestListsFillSumEndToEnd(t *testing.T) {
	cases := []int64{0, 1, 2, 8, 32, 64, 128}
	for _, n := range cases {
		prog := corpus.ListsFillSum.Build(n)
		cfs := vm3jit.CompileProgram(prog)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
		if err != nil {
			t.Fatalf("RunWithArgs(n=%d): %v", n, err)
		}
		want := (n - 1) * n / 2
		if n <= 0 {
			want = 0
		}
		if got.Int() != want {
			t.Errorf("lists_fill_sum(n=%d): got=%d want=%d", n, got.Int(), want)
		}
	}
}
