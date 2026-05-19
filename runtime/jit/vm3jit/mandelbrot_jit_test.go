package vm3jit_test

import (
	"testing"

	"mochi/compiler2/corpus"
	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestMandelbrotJITCompiles is the Phase 6.3.4.h correctness gate: the
// compiler3 mandelbrot kernel must JIT-compile (entry has non-nil
// JITCode after CompileProgram) and produce a result bit-identical to
// compiler2/corpus.ExpectMandelbrot. The kernel's hot inner loop uses
// OpFmaF64, which lowers on ARM64 to a single FMADD instruction; this
// test guarantees that lowering stays correctness-equivalent to Go's
// math.FMA as the c2 reference function uses it.
func TestMandelbrotJITCompiles(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 5, 10, 50, 100} {
		prog := c3.Mandelbrot.Build(n)
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
			t.Fatalf("n=%d: mandelbrot entry has no JITCode (CompileProgram fell back)", n)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("n=%d: RunWithArgs: %v", n, err)
		}
		want := corpus.ExpectMandelbrot(n)
		if got.Int() != want {
			t.Fatalf("n=%d: got %d, want %d", n, got.Int(), want)
		}
	}
}
