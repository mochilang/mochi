//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestMapSetI64I64AMD64InsertReadback exercises Phase 6.3.4.n.8.c: AMD64
// OpMapSetI64I64 inline insertion paired with OpMapGetI64I64 readback,
// both lowered by the JIT. The driver allocates a fresh map (cap=8) and
// the helper performs three inserts via OpMapSetI64I64 then reads the
// middle value back via OpMapGetI64I64. A regression in the fill_block
// (e.g. the slab-recompute via RCX, the raw+tag value write, or the
// nLive bump) would surface here as either a wrong return, a deopt, or
// a segfault.
func TestMapSetI64I64AMD64InsertReadback(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_set_insert",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 100),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 200),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 300),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 8),
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.8.c should admit OpMapSetI64I64")
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
	if got.Int() != 200 {
		t.Fatalf("got %d, want 200 (map[2] after 3 inserts)", got.Int())
	}
}

// TestMapSetI64I64AMD64Update asserts that re-inserting the same key
// overwrites the value rather than allocating a second entry. A buggy
// probe-body that branched to fill_block on hash-match instead of doing
// the in-place value write would surface as either a wrong return (if
// fill_block bumped nLive past 2*(nLive+1) > cap and triggered grow) or
// as a stale read from the first insert.
func TestMapSetI64I64AMD64Update(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_set_update",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 42),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 111),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 222),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 333),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 8),
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
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
		t.Fatalf("helper JITCode is nil")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 333 {
		t.Fatalf("got %d, want 333 (last value written for key 42)", got.Int())
	}
}

// TestMapSetI64I64AMD64GrowDeopt drives a tiny cap=2 table to the grow
// threshold inside the JIT. The kernel's pre-amble must JB to the
// StatusMapGrow deopt when 2*(nLive+1) > cap; the runtime resumes via
// the interpreter, grows the table, and re-enters the JIT for the rest
// of the inserts. Expect at least one deopt and a correct final readback.
func TestMapSetI64I64AMD64GrowDeopt(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_set_grow",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 10),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 20),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 30),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 4),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 40),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 2),
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
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
		t.Fatalf("helper JITCode is nil")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 30 {
		t.Fatalf("got %d, want 30 (map[3] after grow)", got.Int())
	}
}

// TestMapSetI64I64AMD64NegativeKeyValue guards the SBFX48 (shl/sar 16)
// pair on the entry.key load against a sign-bit drop, and the raw+tag
// value write against an unintended tag-byte clobber on the value's
// signed payload. A bug in either would surface as a wrong readback.
func TestMapSetI64I64AMD64NegativeKeyValue(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_set_neg",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, -5),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, -999),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0),
			vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 4),
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
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
		t.Fatalf("helper JITCode is nil")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != -999 {
		t.Fatalf("got %d, want -999 (negative key+value round-trip)", got.Int())
	}
}

// TestMapSetI64I64AMD64Spill exercises the spill sandwich on the set
// kernel: a helper with NumRegsI64=8 stores values in slots 4..6 (host
// R10/R11/R12) before the OpMapSetI64I64 and accumulates them with the
// readback after. The kernel clobbers R10/R11/R12 mid-flight; the
// entry-spill / exit-restore must preserve the pre-kernel values.
func TestMapSetI64I64AMD64Spill(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_set_spill",
		NumRegsI64:  8,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 4, 0, 4),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, 5),
			vm3.MakeOp(vm3.OpConstI64K, 6, 0, 6),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 7),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 70),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0),
			vm3.MakeOp(vm3.OpAddI64, 7, 4, 5),
			vm3.MakeOp(vm3.OpAddI64, 7, 7, 6),
			vm3.MakeOp(vm3.OpAddI64, 7, 7, 1),
			vm3.MakeOp(vm3.OpReturnI64, 7, 0, 0),
		},
	}
	driver := &vm3.Function{
		Name:        "driver",
		NumRegsI64:  2,
		NumRegsCell: 2,
		ParamBanks:  []vm3.Bank{vm3.BankI64},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 8),
			{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 1},
			vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
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
		t.Fatalf("helper JITCode is nil; spill-path admission may be broken")
	}
	vm := vm3.NewWithProgram(prog)
	got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{0})
	if err != nil {
		t.Fatalf("RunWithArgs: %v", err)
	}
	if got.Int() != 85 {
		t.Fatalf("got %d, want 85 (= 4+5+6+70; slot 4..6 not preserved across kernel)", got.Int())
	}
}
