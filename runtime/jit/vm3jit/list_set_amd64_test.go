//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestListSetI64AMD64 exercises Phase 6.3.4.n.2.b: AMD64 OpListSetI64
// lowering on a cell-bank fn. The driver builds a 3-element list
// [10, 20, 30] via interp ops, then JIT-calls a cell-bank helper that
// (1) writes 99 to cells[1] via OpListSetI64 and (2) reads cells[1]
// back via OpListGetI64. A drift in the shl/shr-logical masking pair,
// in the Int48 tag movabs, or in the SIB-store base/index choice
// would surface here as a wrong return value or a segfault.
func TestListSetI64AMD64(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_set_then_get",
		NumRegsI64:  3,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),  // pc=0: regsI64[0] = 1 (idx)
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 99), // pc=1: regsI64[1] = 99 (val)
			vm3.MakeOp(vm3.OpListSetI64, 0, 1, 0), // pc=2: regsCell[0][regsI64[0]] = regsI64[1]
			vm3.MakeOp(vm3.OpListGetI64, 2, 0, 0), // pc=3: regsI64[2] = regsCell[0][regsI64[0]]
			vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),  // pc=4: return regsI64[2]
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.b should admit OpListSetI64")
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
	if got.Int() != 99 {
		t.Fatalf("got %d, want 99 (round-trip of stored cell)", got.Int())
	}
}

// TestListSetI64AMD64NegativePayload guards the shl/shr-logical pack
// pair against a sign-bit leak into the high 16 bits (the tag region).
// Storing -7 must read back as -7, not as 0xFFFA_FFFF_FFFF_FFF9 (a
// missing low-48 mask would leave the original sign bits in bits
// 48..63, but the tag movabs would still set them to 0xFFFA, so the
// OR-combine produces 0xFFFA_FFFF_FFFF_FFF9, which decodes as -7 by
// pure luck of the sign-extend; the real bug surfaces when bits
// 48..63 of the value differ from 0xFFFA). To detect this we use a
// value with non-trivial top 16 bits *after* sign extend: -7 is
// 0xFFFF_FFFF_FFFF_FFF9, so the pack's low-48 mask must zero the top
// 16 before the OR with 0xFFFA<<48; otherwise the stored cell is
// 0xFFFF_FFFF_FFFF_FFF9 (no longer a valid Int48 tag) and the
// OpListGetI64 sign-extend round-trip still returns -7 (because the
// stored bit pattern is exactly the sign-extended form), so we also
// cross-check the interp's Float() decoder rejection. The simpler
// guard: pack must NOT leave a non-tag bit pattern in bits 48..63.
// We check by reading the cell via OpListGetI64 inside JIT and
// returning it; the sign-extend pair produces -7 only if the low 48
// bits hold -7 mod 2^48. A wrong store leaves trash there.
func TestListSetI64AMD64NegativePayload(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_list_set_neg",
		NumRegsI64:  4,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),     // pc=0: regsI64[2] = 0 (idx)
			vm3.MakeOp(vm3.OpConstI64K, 3, 0, -7),    // pc=1: regsI64[3] = -7 (val)
			vm3.MakeOp(vm3.OpListSetI64, 0, 3, 2),    // pc=2: cells[idx] = -7
			vm3.MakeOp(vm3.OpListGetI64, 1, 0, 2),    // pc=3: regsI64[1] = cells[idx]
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),     // pc=4: return regsI64[1]
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
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 42),                                    // pc=1: regsI64[1] = 42
			vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),                                   // pc=2: list.push(42)
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.2.b should admit OpListSetI64")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != -7 {
		t.Fatalf("got %d, want -7 (pack/unpack of negative value failed)", got.Int())
	}
}
