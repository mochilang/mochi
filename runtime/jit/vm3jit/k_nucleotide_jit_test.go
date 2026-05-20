package vm3jit_test

import (
	"testing"

	"mochi/compiler2/corpus"
	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestKNucleotideJITCompiles is the Phase 6.3.4.n.8.e correctness gate:
// the compiler3 k_nucleotide kernel must JIT-compile (entry has non-nil
// JITCode after CompileProgram) on both ARM64 and AMD64, and produce a
// result bit-identical to compiler2/corpus.ExpectKNucleotide for every
// N in the cross-VM acceptance set. The refactor in n.8.e dropped
// NumRegsI64 from 11 to 7 by folding MOD_LCG, HASH_MOD, and the three
// fastaThr thresholds into OpModI64KW / OpCmpLtI64KWBr immediates that
// look up the Function.Consts pool at runtime. This test guarantees the
// new wide-K lowerings agree with the interp on every sample N.
func TestKNucleotideJITCompiles(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 10, 100, 1000} {
		prog := c3.KNucleotide.Build(n)
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
			t.Fatalf("n=%d: k_nucleotide entry has no JITCode (CompileProgram fell back)", n)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(fn, []int64{n})
		if err != nil {
			t.Fatalf("n=%d: RunWithArgs: %v", n, err)
		}
		want := corpus.ExpectKNucleotide(n)
		if got.Int() != want {
			t.Fatalf("n=%d: got %d, want %d", n, got.Int(), want)
		}
	}
}
