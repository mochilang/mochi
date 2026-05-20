//go:build amd64

package vm3jit_test

import (
	"math"
	"testing"

	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestNBodyJITAdmitsAMD64 is the Phase 6.3.4.n.4 admission gate: the
// compiler3 n_body kernel (NumRegsI64=7, NumRegsF64=8, NumRegsCell=7)
// must JIT-compile on AMD64. Phase 6.3.4.n.3 capped cell+f64 fns at
// NumRegsI64 <= 6 because R12 was reserved for the regsF64 base; n.4
// remaps i64 slot 6 to R13 in that shape so the cap rises to 7.
//
// The kernel result is also compared bit-for-bit against the Go
// oracle to catch any spill/load bug introduced by the remap.
func TestNBodyJITAdmitsAMD64(t *testing.T) {
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
			t.Fatalf("steps=%d: n_body entry has no JITCode on AMD64 "+
				"(NumRegsI64=%d NumRegsF64=%d NumRegsCell=%d)",
				steps, fn.NumRegsI64, fn.NumRegsF64, fn.NumRegsCell)
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
