package vm3jit_test

import (
	"testing"
	"unsafe"

	"mochi/compiler3/corpus"
	"mochi/runtime/jit/vm2jit/trampoline"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestSwitchLookupJITCompiles is the Phase 6.4.b correctness gate: both
// switch-dispatch shapes must JIT-compile on ARM64 and produce results
// bit-identical to the Go-native ExpectSwitchLookup8 reference.
//
// SwitchLookup8Table exercises OpLookupI64KW which lowers to
// movImm64(x16, &fn.I64Tables[0][0]) + LDR Xd, [x16, Xidx, LSL #3].
// SwitchLookup8CmpChain uses no new opcodes; its admission is the
// regression gate so we know the previously-working cmp-chain path
// still lowers after Phase 6.4.b lands.
func TestSwitchLookupJITCompiles(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 8, 32, 1000} {
		want := corpus.ExpectSwitchLookup8(n)
		for _, p := range []*corpus.Program{
			corpus.SwitchLookup8CmpChain,
			corpus.SwitchLookup8Table,
		} {
			prog := p.Build(n)
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
				t.Fatalf("%s(n=%d): entry has no JITCode (CompileProgram fell back)", p.Name, n)
			}
			vm := vm3.NewWithProgram(prog)
			got, err := vm.RunWithArgs(fn, []int64{n})
			if err != nil {
				t.Fatalf("%s(n=%d): RunWithArgs: %v", p.Name, n, err)
			}
			if got.Int() != want {
				t.Fatalf("%s(n=%d) = %d, want %d", p.Name, n, got.Int(), want)
			}
		}
	}
}

// switchLookupJITSink defeats dead-store elimination for the bench
// below; the bench reads its return value into this package global.
var switchLookupJITSink int64

// BenchmarkSwitchLookup8JIT measures the dispatch-cost reduction from
// OpLookupI64KW JIT lowering vs the cmp-chain JIT lowering. Both
// kernels are JIT-compiled here (the interp-only bench in
// compiler3/corpus/switch_lookup_test.go BenchmarkSwitchLookup8 is the
// pre-JIT baseline); their ratio isolates the wall-clock impact of
// dispatch shape under native code.
//
// The MEP-40 spec §6.4 promises the JIT cmp_chain vs table ratio
// reaches < 0.50 on darwin/arm64 (mirroring Go CL 756340's -62.65%).
func BenchmarkSwitchLookup8JIT(b *testing.B) {
	cases := []struct {
		name string
		prog *corpus.Program
		n    int64
	}{
		{"cmp_chain_n100", corpus.SwitchLookup8CmpChain, 100},
		{"cmp_chain_n10000", corpus.SwitchLookup8CmpChain, 10000},
		{"table_n100", corpus.SwitchLookup8Table, 100},
		{"table_n10000", corpus.SwitchLookup8Table, 10000},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			prog := tc.prog.Build(tc.n)
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
				b.Fatalf("%s: entry has no JITCode", tc.name)
			}
			regs := make([]int64, 4096)
			var status int64
			entry := fn.JITCode
			b.ResetTimer()
			var s int64
			for i := 0; i < b.N; i++ {
				regs[0] = tc.n + int64(i&1)
				s += int64(trampoline.CallStatus(entry,
					unsafe.Pointer(&regs[0]),
					unsafe.Pointer(&status)))
				if status != 0 {
					b.Fatalf("%s: unexpected deopt status=%d", tc.name, status)
				}
			}
			switchLookupJITSink = s
		})
	}
}
