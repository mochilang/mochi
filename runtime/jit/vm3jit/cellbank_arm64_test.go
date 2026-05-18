//go:build darwin && arm64

package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListsFillSumKernelsCompile confirms the Phase 6.2d.2.b step 2
// gate: vm3jit.CompileProgram admits all three kernels of
// lists_fill_sum on ARM64: sum (idx=2) and fill (idx=1) at PC=0 by
// the inline Cell-bank lowering, and main (idx=0) via the OpNewList
// pre-alloc skip + cross-fn OpCallMixed deopt-passthrough wedge.
func TestListsFillSumKernelsCompile(t *testing.T) {
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
	if got := prog.Funcs[1].JITCode; got == nil {
		t.Fatalf("fill (idx=1) did not compile: JITCode is nil")
	}
	if got := prog.Funcs[0].JITCode; got == nil {
		t.Fatalf("main (idx=0) did not compile: should be admitted via OpNewList pre-alloc + cross-fn deopt-passthrough")
	}
	if !prog.Funcs[0].JITPreAllocList {
		t.Fatalf("main (idx=0) JITPreAllocList not set: pre-alloc skip path should be active")
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
