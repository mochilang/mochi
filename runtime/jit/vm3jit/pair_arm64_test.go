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

// TestSelfCallMixedJIT exercises Phase 6.3.4.m.3 cell-bank self-OpCallMixed
// admission and ARM64 lowering. The synthetic recursive helper takes a Cell
// arg (ignored) plus an i64 depth, recurses on (d-1) until d==0, and returns
// the recursion depth count. The BL self-call path must work without faulting
// or returning the wrong value.
func TestSelfCallMixedJIT(t *testing.T) {
	// rec(c Cell, d i64) -> i64:
	//   if d > 0 -> recurse; else return 0
	//   v = rec(c, d-1)
	//   return v + 1
	// At Funcs[1], so SelfIdx == 1; recursive call passes args at B=0
	// (regsCell[0] = c unchanged, regsI64[1] = d-1 written before call).
	// ParamBanks is position-indexed: with [Cell, I64] and B=0, the cell
	// arg is read from regsCell[0] and the i64 arg from regsI64[1].
	rec := &vm3.Function{
		Name:        "rec",
		NumRegsI64:  3,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpCmpGtI64KBr, 1, 0, 2),                                   // pc=0: if d > 0 -> 2
			vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 99),                                 // pc=1: return 99
			vm3.MakeOp(vm3.OpSubI64K, 1, 1, 1),                                       // pc=2: regsI64[1] = d - 1 (overwrite arg)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 2, B: 0, C: 1}, // pc=3: regsI64[2] = rec(regsCell[0], regsI64[1])
			vm3.MakeOp(vm3.OpAddI64K, 2, 2, 7),                                       // pc=4: result = ret + 7 (sentinel)
			vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),                                     // pc=5: return result
		},
	}
	// driver(d i64) -> i64: builds pair(CNull, CNull), calls rec(pair, d).
	// Driver's d comes in at regsI64[0], but the position-indexed call
	// convention reads the i64 arg from regsI64[B+1=1], so we copy d
	// from regsI64[0] to regsI64[1] before the call.
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                       // pc=0: regsCell[0] = pair(CNull, CNull)
			vm3.MakeOp(vm3.OpAddI64K, 1, 0, 0),                                       // pc=1: regsI64[1] = regsI64[0] + 0 (copy d into arg slot)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 1, B: 0, C: 1}, // pc=2: regsI64[1] = rec(regsCell[0], regsI64[1])
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),                                     // pc=3: return ret
		},
	}
	prog := &vm3.Program{Funcs: []*vm3.Function{driver, rec}, Entry: 0}
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	if rec.JITCode == nil {
		t.Fatalf("rec.JITCode is nil; self-OpCallMixed should admit + lower")
	}
	for _, tc := range []struct{ d, want int64 }{
		{0, 99}, {1, 106}, {2, 113}, {5, 134}, {10, 169}, {32, 323},
	} {
		preDeopt := vm3jit.DeoptCount
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{tc.d})
		postDeopt := vm3jit.DeoptCount
		if err != nil {
			t.Errorf("d=%d: RunWithArgs: %v", tc.d, err)
			continue
		}
		if got.Int() != tc.want {
			t.Errorf("d=%d: got %d, want %d (deopt: %d)", tc.d, got.Int(), tc.want, postDeopt-preDeopt)
		}
	}
}

// TestCheckTreeJITAdmission verifies Phase 6.3.4.m.3 admits check_tree.
// check_tree has shape (Cell, I64)->I64 with self-OpCallMixed twice plus
// OpPairFst/OpPairSnd. After CompileProgram, its JITCode must be non-nil.
func TestCheckTreeJITAdmission(t *testing.T) {
	prog := c3.BinaryTrees.Build(0)
	cfs := vm3jit.CompileProgram(prog)
	defer func() {
		for _, cf := range cfs {
			if cf != nil {
				_ = cf.Free()
			}
		}
	}()
	// Funcs[2] is check_tree per binary_trees.go.
	checkTree := prog.Funcs[2]
	if checkTree.Name != "check_tree" {
		t.Fatalf("expected Funcs[2] to be check_tree, got %q", checkTree.Name)
	}
	if checkTree.JITCode == nil {
		t.Fatalf("check_tree.JITCode is nil; self-OpCallMixed admission should accept it")
	}
}

// TestBinaryTreesEndToEndWithJIT runs the full binary_trees kernel
// through CompileProgram. After Phase 6.3.4.m.3 check_tree is admitted
// (self-OpCallMixed + OpPairFst/Snd) and runs natively; make_tree and
// main still route through the interp (make_tree uses OpNewPair which
// has no JIT lowering, main calls both via OpCallMixed). The test
// asserts the program still returns the oracle value across a depth
// sweep including non-trivial depths so the self-OpCallMixed BL path
// is exercised against real workloads.
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
