//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestCellBankScaffoldAMD64 exercises Phase 6.3.4.m.4c.1 + m.4c.2: the
// AMD64 cell-bank prologue (push RBP+R14, load regsCell base from RCX,
// load *jitArenaCtx from R8) and OpReturnCell lowering (mov disp32(%rbp),
// %rax + epilogue). A JIT-admissible helper takes one Cell arg and
// returns it via OpReturnCell. The interp-side driver builds a pair (its
// OpNewPair routes through the interp), calls the JIT'd helper, and
// asserts the returned Cell still decodes to a valid ArenaPair handle.
func TestCellBankScaffoldAMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_echo_cell",
		NumRegsI64:  0,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpReturnCell, 0, 0, 0), // pc=0: return regsCell[0]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  1,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                        // pc=0: regsCell[0] = pair(CNull, CNull)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 1, B: 0, C: 1}, // pc=1: regsCell[1] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0),                                     // pc=2: return regsCell[1]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank scaffold should admit OpReturnCell")
	}
	preDeopt := vm3jit.DeoptCount
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if d := vm3jit.DeoptCount - preDeopt; d != 0 {
		t.Fatalf("unexpected deopt count delta: %d", d)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}

// TestPairReadAMD64 exercises Phase 6.3.4.m.4c.3: AMD64 OpPairFst +
// OpPairSnd lowering. The helper takes a Cell (pair) arg, OpPairSnd
// extracts its snd cell, OpReturnCell returns it. The driver builds
// pair(CNull, fst) on the interp side, calls the JIT helper, asserts
// the returned cell is the original snd field bit-for-bit. Catches
// any drift in the 6-instruction lowering (mov32, imul, mov pairsBase,
// add, mov fst/sndOff, mov store).
func TestPairReadAMD64(t *testing.T) {
	// helper(p Cell) -> Cell: returns PairSnd(p).
	helper := &vm3.Function{
		Name:        "amd64_pair_snd",
		NumRegsI64:  0,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpPairSnd, 1, 0, 0),    // pc=0: regsCell[1] = PairSnd(regsCell[0])
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0), // pc=1: return regsCell[1]
		},
	}
	// driver builds pair(CNull, p_inner) where p_inner is itself a
	// pair (so the returned snd is a valid ArenaPair handle we can
	// decode). Then driver calls helper and returns its result.
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  1,
		NumRegsCell: 3,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                        // pc=0: regsCell[0] = pair(CNull, CNull) (p_inner)
			vm3.MakeOp(vm3.OpNewPair, 1, 1, 0),                                        // pc=1: regsCell[1] = pair(CNull, p_inner) (outer pair, snd=p_inner)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 2, B: 1, C: 1}, // pc=2: regsCell[2] = helper(regsCell[1])
			vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                     // pc=3: return regsCell[2]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank should admit OpPairSnd")
	}
	preDeopt := vm3jit.DeoptCount
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if d := vm3jit.DeoptCount - preDeopt; d != 0 {
		t.Fatalf("unexpected deopt count delta: %d", d)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}

// TestPairFstReadAMD64 is the OpPairFst mirror of TestPairReadAMD64.
// The helper extracts fst (rather than snd) and the driver builds the
// outer pair with the inner pair in the fst slot. Same correctness
// guarantee against the JITPairFstOffset immediate.
func TestPairFstReadAMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_pair_fst",
		NumRegsI64:  0,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpPairFst, 1, 0, 0),    // pc=0: regsCell[1] = PairFst(regsCell[0])
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0), // pc=1: return regsCell[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  1,
		NumRegsCell: 3,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                        // pc=0: regsCell[0] = pair(CNull, CNull) (p_inner)
			vm3.MakeOp(vm3.OpNewPair, 1, 0, 1),                                        // pc=1: regsCell[1] = pair(p_inner, CNull) (outer, fst=p_inner)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 2, B: 1, C: 1}, // pc=2: regsCell[2] = helper(regsCell[1])
			vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                     // pc=3: return regsCell[2]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank should admit OpPairFst")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}

// TestCellBankScaffoldWithI64AMD64 exercises a cell-bank fn that mixes
// i64 work with OpReturnCell. The prologue must push RBX/R15/R14/RBP
// (cell-bank set), load i64 slots 0..1, run the arithmetic, then return
// the cell handle through RAX. Catches any prologue byte-count drift
// between byteCountAMD64 and emitInstrAMD64.
func TestCellBankScaffoldWithI64AMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_cell_with_i64",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpAddI64K, 0, 1, 99),   // pc=0: regsI64[0] = regsI64[1] + 99 (dead-store, exercises i64 path)
			vm3.MakeOp(vm3.OpReturnCell, 0, 0, 0), // pc=1: return regsCell[0]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                        // pc=0: regsCell[0] = pair
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 1, B: 0, C: 1}, // pc=1: regsCell[1] = helper(cell, d)
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0),                                     // pc=2: return regsCell[1]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank scaffold should admit i64+cell helper")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{7})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}

// TestSelfCallMixedI64ReturnAMD64 exercises Phase 6.3.4.m.4c.5: AMD64
// self-recursive OpCallMixed with a Cell+I64 mixed param shape and an
// I64 result, mirroring binary_trees' check_tree. The helper walks a
// 1-deep pair recursively: at d>0 it follows PairFst, recurses with
// d-1, then adds 1; at d==0 it returns 1. The driver builds a 2-level
// pair so a single recursion bottoms out and returns 2 (leaf + parent
// counted by the +1). Asserts admission, zero-deopt, correct return.
func TestSelfCallMixedI64ReturnAMD64(t *testing.T) {
	// helper(t Cell, d i64) -> i64.
	// Cell-bank fn with ParamBanks=[Cell, I64], ResultBank=I64.
	helper := &vm3.Function{
		Name:        "amd64_self_cm_i64_ret",
		NumRegsI64:  4,
		NumRegsCell: 3,
		ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpCmpGtI64KBr, 1, 0, 2),                                   // pc=0: if d > 0 -> 2
			vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 1),                                  // pc=1: leaf -> 1
			vm3.MakeOp(vm3.OpSubI64K, 3, 1, 1),                                       // pc=2: d_minus_1 = d-1
			vm3.MakeOp(vm3.OpPairFst, 2, 0, 0),                                       // pc=3: regsCell[2] = PairFst(t)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 2, B: 2, C: 1}, // pc=4: regsI64[2] = helper(regsCell[2], regsI64[3])
			vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),                                       // pc=5: regsI64[2]++
			vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),                                     // pc=6: return regsI64[2]
		},
	}
	// driver builds pair(inner, CNull) where inner is pair(CNull,CNull),
	// then calls helper(pair, 1) -> should return 2 (one recursion step
	// + a leaf returning 1).
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  3,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                       // pc=0: regsCell[0] = pair(CNull, CNull) (inner)
			vm3.MakeOp(vm3.OpNewPair, 1, 0, 1),                                       // pc=1: regsCell[1] = pair(inner, CNull)
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, 1),                                     // pc=2: regsI64[2] = 1 (depth)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 1, C: 1}, // pc=3: regsI64[0] = helper(regsCell[1], regsI64[2])
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),                                     // pc=4: return regsI64[0]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank should admit self OpCallMixed (m.4c.5)")
	}
	preDeopt := vm3jit.DeoptCount
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if d := vm3jit.DeoptCount - preDeopt; d != 0 {
		t.Fatalf("unexpected deopt count delta: %d", d)
	}
	if int64(got) != 2 {
		t.Fatalf("got %d, want 2", int64(got))
	}
}

// TestSelfCallMixedCellReturnAMD64 exercises Phase 6.3.4.m.4c.5: AMD64
// self-recursive OpCallMixed with an I64-only param shape and a Cell
// result, mirroring binary_trees' make_tree. The helper, at d>0,
// recursively calls itself with d-1 to populate fst and snd, then
// allocates a pair; at d==0, it allocates a leaf pair. Asserts admission,
// zero-deopt (pairsCap accommodates the tree), and that the returned
// cell decodes to a valid ArenaPair handle.
func TestSelfCallMixedCellReturnAMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_self_cm_cell_ret",
		NumRegsI64:  2,
		NumRegsCell: 3,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpCmpGtI64KBr, 0, 0, 3),                                    // pc=0: if d > 0 -> 3
			vm3.MakeOp(vm3.OpNewPair, 2, 0, 0),                                        // pc=1: leaf pair(CNull, CNull)
			vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                     // pc=2: return leaf
			vm3.MakeOp(vm3.OpSubI64K, 1, 0, 1),                                        // pc=3: arg = d-1
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 0, B: 1, C: 1}, // pc=4: regsCell[0] = helper(d-1)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 1, B: 1, C: 1}, // pc=5: regsCell[1] = helper(d-1)
			vm3.MakeOp(vm3.OpNewPair, 2, 0, 1),                                        // pc=6: pair(fst, snd)
			vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                     // pc=7: return pair
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 2),                                      // pc=0: regsI64[1] = 2 (depth)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 0, B: 1, C: 1}, // pc=1: regsCell[0] = helper(2)
			vm3.MakeOp(vm3.OpReturnCell, 0, 0, 0),                                     // pc=2: return
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank should admit self OpCallMixed with Cell result (m.4c.5)")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}

// TestNewPairJITAMD64 exercises Phase 6.3.4.m.4c.4: AMD64 OpNewPair
// inline allocation in cell-bank. The helper receives one Cell arg
// (CNull from the driver), allocates a pair via OpNewPair with both
// fst and snd set to the input Cell, then returns the new pair via
// OpReturnCell. Asserts:
//   - helper JITCode is non-nil (admission accepted OpNewPair).
//   - deopt count is unchanged (StatusPairGrow not raised: pairsCap
//     is sized for many allocs, one call is well under).
//   - returned Cell decodes to a valid ArenaPair handle.
func TestNewPairJITAMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_new_pair",
		NumRegsI64:  0,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 1, 0, 0),    // pc=0: regsCell[1] = pair(regsCell[0], regsCell[0])
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0), // pc=1: return regsCell[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  1,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankCell,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewPair, 0, 1, 1),                                        // pc=0: regsCell[0] = pair(CNull, CNull) (seed cell arg)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 1, B: 0, C: 1}, // pc=1: regsCell[1] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnCell, 1, 0, 0),                                     // pc=2: return regsCell[1]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank should admit OpNewPair (m.4c.4)")
	}
	preDeopt := vm3jit.DeoptCount
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if d := vm3jit.DeoptCount - preDeopt; d != 0 {
		t.Fatalf("unexpected deopt count delta: %d (expected 0; pairsCap should accommodate one inline alloc)", d)
	}
	if !got.IsHandle() {
		t.Fatalf("returned Cell is not a handle: %#x", uint64(got))
	}
	tag, _, _ := got.DecodeHandle()
	if tag != vm3.ArenaPair {
		t.Fatalf("returned Cell tag = %d, want ArenaPair (%d)", tag, vm3.ArenaPair)
	}
}
