package corpus

import (
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestSwitchLookup8Match asserts that both dispatch shapes (cmp-chain
// and table) produce bit-identical results to the Go reference for a
// range of N values. The LCG and per-key value table are shared, so
// any divergence is a bug in the bytecode encoding, not in the
// kernel.
func TestSwitchLookup8Match(t *testing.T) {
	ns := []int64{0, 1, 2, 8, 32, 1000, 10000}
	for _, n := range ns {
		want := ExpectSwitchLookup8(n)
		for _, p := range []*Program{SwitchLookup8CmpChain, SwitchLookup8Table} {
			prog := p.Build(n)
			vm := vm3.NewWithProgram(prog)
			got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{n})
			if err != nil {
				t.Errorf("%s(%d): %v", p.Name, n, err)
				continue
			}
			if got.Int() != want {
				t.Errorf("%s(%d) = %d, want %d", p.Name, n, got.Int(), want)
			}
		}
	}
}

// BenchmarkSwitchLookup8 measures the dispatch-cost reduction from
// replacing an 8-case cmp-and-branch chain with a single bounds check
// plus indexed table load (OpLookupI64KW). Both variants execute the
// same LCG and accumulate the same per-key values, so any wall-clock
// difference is attributable to dispatch shape alone.
//
// Expected: table form is meaningfully faster than cmp-chain on
// unpredictable inputs. Go CL 756340 reports -62.65% on the closest
// upstream bench (SwitchLookup8Unpredictable).
func BenchmarkSwitchLookup8(b *testing.B) {
	cases := []struct {
		name string
		prog *Program
		n    int64
	}{
		{"cmp_chain_n100", SwitchLookup8CmpChain, 100},
		{"cmp_chain_n10000", SwitchLookup8CmpChain, 10000},
		{"table_n100", SwitchLookup8Table, 100},
		{"table_n10000", SwitchLookup8Table, 10000},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			prog := tc.prog.Build(tc.n)
			vm := vm3.NewWithProgram(prog)
			fn := prog.Funcs[prog.Entry]
			args := []int64{tc.n}
			b.ResetTimer()
			for range b.N {
				_, _ = vm.RunWithArgs(fn, args)
			}
		})
	}
}
