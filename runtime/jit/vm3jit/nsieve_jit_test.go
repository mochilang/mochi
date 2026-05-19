package vm3jit_test

import (
	"testing"

	"mochi/compiler2/corpus"
	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestNsieveJITCompiles is a guard: after Phase 6.3.4.k.2 admitted
// OpListSetI64, the corpus nsieve kernel must JIT-compile and produce
// the same i64 as compiler2's reference function. If either gate slips,
// the corpus bench would silently fall back to the interp and the gap
// to Go would not visibly close.
func TestNsieveJITCompiles(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 10, 50, 100, 1000} {
		prog := c3.Nsieve.Build(n)
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
			t.Fatalf("n=%d: nsieve entry has no JITCode (CompileProgram fell back)", n)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("n=%d: RunWithArgs: %v", n, err)
		}
		want := corpus.ExpectNsieve(n)
		if got.Int() != want {
			t.Fatalf("n=%d: got %d, want %d", n, got.Int(), want)
		}
	}
}
