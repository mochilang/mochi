//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListGetI64AMD64 exercises Phase 6.3.4.n.2.a: AMD64 OpListGetI64
// lowering on a cell-bank fn. The driver runs in the interpreter (it
// uses OpNewList + OpListPushI64 which n.2.a does not yet admit), so it
// builds a 3-element list [10, 20, 30] and calls the JIT'd helper. The
// helper is cell-bank (NumRegsCell=1) + i64 (NumRegsI64=2) and uses a
// constant index for the OpListGetI64 access, so the only admission
// gate it has to pass is the new OpListGetI64 case in the AMD64
// whitelist. The returned value must equal the list element at the
// constant idx; a drift in the SBFX (shl/sar) pair or in any of the
// scratch-register choices would surface here as a wrong return or a
// segfault on the SIB load.
func TestListGetI64AMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_get_const",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),    // pc=0: regsI64[0] = 1 (idx)
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 0),   // pc=1: regsI64[1] = regsCell[0][regsI64[0]]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),    // pc=2: return regsI64[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 0),                                       // pc=0: regsCell[0] = []
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 10),                                    // pc=1: regsI64[1] = 10
			vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),                                   // pc=2: list.push(10)
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 20),                                    // pc=3: regsI64[1] = 20
			vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),                                   // pc=4: list.push(20)
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 30),                                    // pc=5: regsI64[1] = 30
			vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),                                   // pc=6: list.push(30)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1}, // pc=7: regsI64[0] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),                                     // pc=8: return regsI64[0]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.a should admit OpListGetI64")
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
	if got.Int() != 20 {
		t.Fatalf("got %d, want 20 (list[1])", got.Int())
	}
}

// TestListGetI64AMD64NegativePayload guards the SBFX (shl/sar by 16)
// pair against a sign-bit drop. A list value of -42 must round-trip as
// -42, not 0x0000FFFF_FFFFFFD6 (which is what a missing sign-extend
// would surface). The helper uses a different (dst, idx) register
// pair than TestListGetI64AMD64 to also catch r2xAMD64 mapping bugs.
func TestListGetI64AMD64NegativePayload(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_get_neg",
		NumRegsI64:  3,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),    // pc=0: regsI64[2] = 0 (idx)
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 2),   // pc=1: regsI64[1] = regsCell[0][regsI64[2]]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),    // pc=2: return regsI64[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 0),                                       // pc=0: regsCell[0] = []
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, -42),                                   // pc=1: regsI64[1] = -42
			vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),                                   // pc=2: list.push(-42)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1}, // pc=3: regsI64[0] = helper(regsCell[0])
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.a should admit OpListGetI64")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != -42 {
		t.Fatalf("got %d, want -42 (sign-extend failed?)", got.Int())
	}
}
