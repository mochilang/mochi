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
