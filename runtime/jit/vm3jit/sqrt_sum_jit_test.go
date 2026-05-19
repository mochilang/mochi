//go:build (darwin && arm64) || (linux && amd64)

package vm3jit_test

import (
	"testing"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestCompileF64SqrtSumMatchesInterp validates the Phase 6.3.4.j prep
// OpSqrtF64 lowering. F64SqrtSum is the f64 dual of F64DotSum: it
// drives an i64 counter through OpSqrtF64 + OpAddF64 and confirms the
// JIT'd FSQRT result is bit-identical to math.Sqrt across N in the
// n_body inner-loop range. The lowering covers both ARM64 (FSQRT) and
// AMD64 (SQRTSD), so this test runs platform-agnostic; Phase 6.3.4.h.2
// (2026-05-19) drops the prior darwin&&arm64 build tag.
func TestCompileF64SqrtSumMatchesInterp(t *testing.T) {
	prog := corpus.F64SqrtSum.Build(0)
	fn := prog.Funcs[prog.Entry]
	for _, n := range []int64{0, 1, 2, 10, 100, 1000} {
		got, status := runJITF64Kernel(t, fn, n)
		if status != vm3jit.StatusOK {
			t.Fatalf("unexpected deopt n=%d status=%d", n, status)
		}
		vm := vm3.NewWithProgram(prog)
		want, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("interp n=%d: %v", n, err)
		}
		if got != want.Float() {
			t.Errorf("f64_sqrt_sum(%d): jit=%g interp=%g", n, got, want.Float())
		}
	}
}
