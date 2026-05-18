//go:build darwin && arm64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestCrossFnCellBankCallMixed exercises the Phase 6.2d.2.b step 1
// cross-fn OpCallMixed lowering. The program has four functions:
//
//	main    (interp, has OpNewList): builds the list and dispatches.
//	wrapper (JIT, cell-bank): forwards (Cell, I64) -> sum.
//	fill    (JIT, deopt-aware): pushes 0..n into the list.
//	sum     (JIT, deopt-free): folds list with running accumulator.
//
// The wrapper is the new admission point: it sits between an interp
// caller (main) and a JIT'd callee (sum), so its OpCallMixed -> sum
// site lowers to the inline BLR sequence (movImm64 + BLR x16) emitted
// by lower_arm64.OpCallMixed. The end-to-end value (n-1)*n/2 confirms
// the BLR sequence preserves caller frame state across the call.
func TestCrossFnCellBankCallMixed(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 8, 32, 128} {
		capHint := int16(0)
		if n > 0 && n <= 0x7FFF {
			capHint = int16(n)
		}
		main := &vm3.Function{
			Name:        "main",
			NumRegsI64:  4,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewList, 0, 0, capHint),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpMovI64, 2, 0, 0),
				// fill(xs=regsCell[0], i=regsI64[1]=0, n=regsI64[2]).
				// fill is at idx=2.
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 3, B: 0, C: 2},
				// wrapper(xs=regsCell[0], n=regsI64[1]).
				// wrapper expects ParamBanks=[Cell, I64], op.B=0 reads
				// regsCell[0] and regsI64[1]. Set regsI64[1] = n
				// (original arg in regsI64[0]).
				vm3.MakeOp(vm3.OpMovI64, 1, 0, 0),
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		// wrapper(xs Cell, n I64) -> I64. Drives the cross-fn OpCallMixed
		// path: it sets up sum's args in its own regs window, then BLRs
		// straight into sum's JIT entry.
		wrapper := &vm3.Function{
			Name:        "wrapper",
			NumRegsI64:  4,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				// sum.ParamBanks = [Cell, I64, I64, I64]. Need
				// regsCell[0]=xs (param), regsI64[1]=0 (j), regsI64[2]=n,
				// regsI64[3]=0 (acc). regsI64[1] currently holds n.
				vm3.MakeOp(vm3.OpMovI64, 2, 1, 0),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				// sum is at idx=3.
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 3},
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		fill := &vm3.Function{
			Name:        "fill",
			NumRegsI64:  3,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 2, 4),
				vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 0),
			},
		}
		sum := &vm3.Function{
			Name:        "sum",
			NumRegsI64:  4,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64, vm3.BankI64, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 2, 5),
				vm3.MakeOp(vm3.OpListGetI64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 3),
				vm3.MakeOp(vm3.OpReturnI64, 3, 0, 0),
			},
		}
		prog := &vm3.Program{Funcs: []*vm3.Function{main, wrapper, fill, sum}, Entry: 0}
		cfs := vm3jit.CompileProgram(prog)
		if prog.Funcs[3].JITCode == nil {
			for _, cf := range cfs {
				if cf != nil {
					_ = cf.Free()
				}
			}
			t.Fatalf("n=%d: sum (idx=3) did not compile", n)
		}
		if prog.Funcs[1].JITCode == nil {
			for _, cf := range cfs {
				if cf != nil {
					_ = cf.Free()
				}
			}
			t.Fatalf("n=%d: wrapper (idx=1) did not compile: cross-fn OpCallMixed admission failed", n)
		}
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[0], []int64{n})
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
		if err != nil {
			t.Fatalf("n=%d: RunWithArgs: %v", n, err)
		}
		want := (n - 1) * n / 2
		if n <= 0 {
			want = 0
		}
		if got.Int() != want {
			t.Errorf("n=%d: got=%d want=%d", n, got.Int(), want)
		}
	}
}
