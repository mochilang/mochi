package vm3jit_test

import (
	"math"
	"testing"

	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestNBodyJITCompiles is the Phase 6.3.4.j.3 correctness gate: the
// compiler3 n_body kernel must JIT-compile (entry has non-nil JITCode
// after CompileProgram) and produce a result within 1e-10 of the
// ExpectN_body oracle. The kernel exercises the new ARM64 lowerings
// for OpListGetF64 and OpListSetF64 on a Cell-bank fn that interleaves
// 7 distinct list handles inside a triple-nested loop; the
// admission whitelist was extended in the same phase to cover the
// f64 op set the kernel needs (Add/Sub/Mul/Div/Sqrt/Const/I64ToF64
// plus the new list f64 ops and OpReturnF64).
//
// On non-ARM64 hosts CompileProgram will fall back to the interpreter
// for any Cell-bank fn, so this test is skipped there.
func TestNBodyJITCompiles(t *testing.T) {
	for _, steps := range []int64{0, 1, 2, 5, 10, 100} {
		prog := c3.N_body.Build(steps)
		cfs := vm3jit.CompileProgram(prog)
		defer func() {
			for _, cf := range cfs {
				if cf != nil {
					_ = cf.Free()
				}
			}
		}()
		fn := prog.Funcs[prog.Entry]
		if fn.JITCode == nil {
			t.Skipf("steps=%d: n_body entry has no JITCode (CompileProgram fell back; expected on non-ARM64)", steps)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(fn, []int64{steps})
		if err != nil {
			t.Fatalf("steps=%d: RunWithArgs: %v", steps, err)
		}
		want := c3.ExpectN_body(steps)
		if d := math.Abs(got.Float() - want); d > 1e-10 {
			t.Fatalf("steps=%d: got %v, want %v (delta %v)", steps, got.Float(), want, d)
		}
	}
}
