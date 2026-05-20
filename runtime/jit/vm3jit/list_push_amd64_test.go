//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListPushI64AMD64 exercises Phase 6.3.4.n.2.c: AMD64 OpListPushI64
// lowering on a cell-bank fn. The driver runs in the interpreter and
// builds a list with cap=8 (room for 3 pushes without growing). The
// helper is JIT'd cell-bank: it pushes 11, 22, 33 to the list, then
// reads cells[2] back via OpListGetI64 and returns it. A drift in any
// of the SIB store base/index choices, the tag-overwrite offset, the
// cap-cmp polarity, or the len-bump bytes would surface here as a
// wrong return or a segfault.
func TestListPushI64AMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_push_then_get",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 11), // pc=0: regsI64[0] = 11
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=1: list.push(11)
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 22), // pc=2: regsI64[0] = 22
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=3: list.push(22)
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 33), // pc=4: regsI64[0] = 33
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=5: list.push(33)
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),  // pc=6: regsI64[0] = 2 (idx)
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 0), // pc=7: regsI64[1] = list[2]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),  // pc=8: return regsI64[1]
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
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1}, // pc=1: regsI64[0] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),                                     // pc=2: return regsI64[0]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.c should admit OpListPushI64")
	}
	preDeopt := vm3jit.DeoptCount
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if d := vm3jit.DeoptCount - preDeopt; d != 0 {
		t.Fatalf("unexpected deopt count delta: %d (helper had cap=8 room for 3 pushes)", d)
	}
	if got.Int() != 33 {
		t.Fatalf("got %d, want 33 (list[2] after pushing 11,22,33)", got.Int())
	}
}

// TestListPushI64AMD64NegativePayload guards the SIB-store + tag-overwrite
// pair against a high-bit drop. Storing -7 must read back as -7 via the
// OpListGetI64 sign-extend pair. The push-tag trick relies on the fact
// that bytes 0..5 of the stored 8-byte xVal already hold the signed
// low-48 payload (two's complement of -7 = 0xFFFF_FFFF_FFFF_FFF9, so
// bytes 0..5 are FF FF FF FF FF FF and the tag overwrite at offset 6
// makes bytes 6..7 = FA FF, producing 0xFFFA_FFFF_FFFF_FFF9). The read
// back via OpListGetI64 reverses with shl/sar by 16 and yields -7.
func TestListPushI64AMD64NegativePayload(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_push_neg",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, -7),  // pc=0: regsI64[0] = -7
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=1: list.push(-7)
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 0),   // pc=2: regsI64[0] = 0 (idx)
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 0),  // pc=3: regsI64[1] = list[0]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),   // pc=4: return regsI64[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 4),                                       // pc=0: regsCell[0] = make([]Cell, 0, 4)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1}, // pc=1: regsI64[0] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),                                     // pc=2: return regsI64[0]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.c should admit OpListPushI64")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != -7 {
		t.Fatalf("got %d, want -7 (push/get round-trip of negative payload)", got.Int())
	}
}

// TestListPushI64AMD64Grow forces the StatusListGrow deopt: the helper
// is JIT-compiled but the driver passes a list with cap=2 and the
// helper tries to push 3 items. The third push must observe cap == len,
// branch to the StatusListGrow block, and let jitCall regrow the slab
// and resume in the interpreter. Round-trip sum across all three
// pushes is checked.
func TestListPushI64AMD64Grow(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_push_grow",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 100), // pc=0: regsI64[0] = 100
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=1: list.push(100)
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 200), // pc=2: regsI64[0] = 200
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=3: list.push(200) — cap reached
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 300), // pc=4: regsI64[0] = 300
			vm3.MakeOp(vm3.OpListPushI64, 0, 0, 0), // pc=5: list.push(300) — triggers grow
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),   // pc=6: regsI64[0] = 2 (idx)
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 0),  // pc=7: regsI64[1] = list[2]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),   // pc=8: return regsI64[1]
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewList, 0, 0, 2),                                       // pc=0: regsCell[0] = make([]Cell, 0, 2)
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1}, // pc=1: regsI64[0] = helper(regsCell[0])
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),                                     // pc=2: return regsI64[0]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.c should admit OpListPushI64")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 300 {
		t.Fatalf("got %d, want 300 (list[2] after pushes 100,200,300 with grow-deopt)", got.Int())
	}
}
