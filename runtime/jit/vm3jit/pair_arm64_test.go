//go:build darwin && arm64

package vm3jit_test

import (
	"testing"

	c3 "mochi/compiler3/corpus"
	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestPairJITRead exercises the Phase 6.3.4.m.2 ARM64 lowering for
// OpPairFst / OpPairSnd in isolation. The synthetic program builds a
// pair on the interp side (driver runs interp because OpNewPair has no
// JIT lowering yet), then calls a JIT-compiled helper that does
// OpPairFst + OpPairSnd reads against the pair slab. The helper
// returns a constant (the reads' results are dead, but the LDR
// instructions are emitted and must not fault). End-to-end run asserts
// the helper returns the expected constant and no deopt fires.
func TestPairJITRead(t *testing.T) {
	// Helper (Cell-bank, JIT-admissible). Takes a Cell, reads its fst +
	// snd, returns a constant via OpReturnConstK.
	helper := &vm3.Function{
		Name:        "pair_read_helper",
		NumRegsI64:  0,
		NumRegsCell: 3,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpPairFst, 1, 0, 0),     // pc=0: regsCell[1] = PairFst(regsCell[0])
			vm3.MakeOp(vm3.OpPairSnd, 2, 0, 0),     // pc=1: regsCell[2] = PairSnd(regsCell[0])
			vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 42), // pc=2: return 42
		},
	}
	// Driver (interp). Builds pair(CNull, CNull), calls helper, returns
	// helper's result. Uses OpNewPair which has no JIT lowering, so the
	// driver itself is not admitted; it routes through the interp.
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                       // pc=0: regsCell[0] = pair(CNull, CNull)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 1, B: 0, C: 1}, // pc=1: regsI64[1] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),                                     // pc=2: return regsI64[1]
		},
	}
	prog := &vm3.Program{Funcs: []*vm3.Function{driver, helper}, Entry: 0}
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	if helper.JITCode == nil {
		t.Fatalf("helper JITCode is nil (OpPairFst/Snd should compile)")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 42 {
		t.Fatalf("got %d, want 42", got.Int())
	}
}

// TestBinaryTreesEndToEndWithJIT runs the full binary_trees kernel
// through CompileProgram. None of binary_trees's three functions are
// JIT-admissible at this phase (make_tree allocates inline, check_tree
// self-recurses via OpCallMixed which is not yet lowered, main calls
// both), so all three route through the interp. The test asserts the
// program still returns the oracle value after CompileProgram, ensuring
// the Phase 6.3.4.m.2 changes to admission whitelist + slab kind
// dispatch don't regress binary_trees.
func TestBinaryTreesEndToEndWithJIT(t *testing.T) {
	for _, depth := range []int64{0, 1, 2, 3, 4, 5, 8} {
		prog := c3.BinaryTrees.Build(depth)
		cfs := vm3jit.CompileProgram(prog)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{depth})
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
		if err != nil {
			t.Fatalf("depth=%d: RunWithArgs: %v", depth, err)
		}
		want := c3.ExpectBinaryTrees(depth)
		if got.Int() != want {
			t.Fatalf("depth=%d: got %d, want %d", depth, got.Int(), want)
		}
	}
}
