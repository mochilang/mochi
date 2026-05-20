//go:build arm64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListGetI64KARM64 exercises Phase 6.3.4.n.2.e: ARM64 OpListGetI64K
// lowering on a cell-bank fn. The driver builds a 3-element list
// [10, 20, 30] then JIT-calls a helper that reads list[1] via the new
// constant-index opcode. A drift in the LDR imm12 of the cells-array
// pointer load, or in the SBFX (signed bitfield extract) sign-extend,
// would surface as a wrong return or a segfault.
func TestListGetI64KARM64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "arm64_list_getk_const",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpListGetI64K, 0, 0, 1), // pc=0: regsI64[0] = regsCell[0][1]
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),   // pc=1: return regsI64[0]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 8),                                       // pc=0: regsCell[0] = make([]Cell, 0, 8)
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
		t.Fatalf("helper JITCode is nil; ARM64 cell-bank n.2.e should admit OpListGetI64K")
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

// TestListGetI64KARM64NegativePayload checks the SBFX sign-extend on
// the constant-index variant by storing -42 and reading it back.
func TestListGetI64KARM64NegativePayload(t *testing.T) {
	helper := &vm3.Function{
		Name:        "arm64_list_getk_neg",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpListGetI64K, 0, 0, 0), // pc=0: regsI64[0] = regsCell[0][0]
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),   // pc=1: return regsI64[0]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 8),                                       // pc=0: regsCell[0] = []
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
		t.Fatalf("helper JITCode is nil; ARM64 cell-bank n.2.e should admit OpListGetI64K")
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
