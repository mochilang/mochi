//go:build amd64

package vm3jit_test

import (
	"testing"

	"mochi/runtime/jit/vm3jit"
	"mochi/runtime/vm3"
)

// TestMapGetI64I64AMD64Hit exercises Phase 6.3.4.n.8.b: AMD64
// OpMapGetI64I64 inline lookup on a cell-bank helper. The driver runs
// in the interpreter, allocates a small map via OpNewMap, and inserts
// three (key, value) pairs via OpMapSetI64I64. It then JIT-calls the
// helper which performs OpMapGetI64I64 on a constant key and returns
// the value. A drift in the splitmix64 emit, the probe-loop pos /
// mask arithmetic, or the entry SBFX48 sign-extend would surface
// here as a wrong return or a segfault on the disp8 entry loads.
func TestMapGetI64I64AMD64Hit(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_get_hit",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),   // pc=0: regsI64[0] = 2 (key)
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0), // pc=1: regsI64[1] = map[regsI64[0]]
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
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 8),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 100),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 200),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 3),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 300),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
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
		t.Fatalf("helper JITCode is nil; AMD64 cell-bank n.8.b should admit OpMapGetI64I64")
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
		t.Fatalf("got %d, want 200 (map[2])", got.Int())
	}
}

// TestMapGetI64I64AMD64Miss asserts the JIT lowering returns 0 when the
// key is absent from the table. A bug in the probe-loop terminator
// (e.g. failing to follow the linear probe to the next empty entry, or
// not branching to the miss block on hash==0) would manifest here as a
// wrong return or an infinite loop / segfault.
func TestMapGetI64I64AMD64Miss(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_get_miss",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 999),
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
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 100),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 2),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 200),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
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
	if got.Int() != 0 {
		t.Fatalf("got %d, want 0 (missing-key sentinel)", got.Int())
	}
}

// TestMapGetI64I64AMD64EmptyTable guards the pre-amble's tableLen
// empty-check (`test rcx,rcx; jz miss`) by looking up a key in a map
// that was allocated with cap=0 and never written. The kernel must
// branch to the miss block before dereferencing the (nil) tablePtr.
func TestMapGetI64I64AMD64EmptyTable(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_get_empty",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 42),
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
			vm3.MakeOp(vm3.OpNewMap, 0, 0, 0),
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
	if got.Int() != 0 {
		t.Fatalf("got %d, want 0 (empty-table miss)", got.Int())
	}
}

// TestMapGetI64I64AMD64NegativeValue guards the SBFX48 (shl/sar by 16)
// pair on the entry.value load against a sign-bit drop. Storing -777
// must round-trip as -777, not 0x0000FFFF_FFFFFCF7. The key must also
// round-trip the per-probe cmp against xKey; if the SBFX48 on
// entry.key dropped the sign, the cmp would never match a negative
// key and the kernel would fall through to miss.
func TestMapGetI64I64AMD64NegativeValue(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_get_neg",
		NumRegsI64:  2,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, -3),
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
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, -3),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, -777),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
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
	if got.Int() != -777 {
		t.Fatalf("got %d, want -777 (negative key+value round-trip)", got.Int())
	}
}

// TestMapGetI64I64AMD64Spill exercises the spill sandwich path: a
// helper with NumRegsI64=8 stores values in slots 4..6 (host R10/R11/
// R12) before the OpMapGetI64I64 and reads them back afterwards. The
// kernel clobbers R10/R11/R12 mid-flight as pos / entry_addr / load
// scratch; the entry-spill / exit-restore pair must preserve the
// pre-kernel values. A regression where the restore is omitted, or
// the spill writes the wrong slot, would surface as a wrong final
// sum (we sum {slot4, slot5, slot6, get-result} = 4 + 5 + 6 + value).
func TestMapGetI64I64AMD64Spill(t *testing.T) {
	helper := &vm3.Function{
		Name:        "amd64_map_get_spill",
		NumRegsI64:  8,
		NumRegsCell: 1,
		ParamBanks:  []vm3.Bank{vm3.BankCell},
		ResultBank:  vm3.BankI64,
		Code: []vm3.Op{
			// Seed slots 4..6 with values the kernel must preserve.
			vm3.MakeOp(vm3.OpConstI64K, 4, 0, 4),
			vm3.MakeOp(vm3.OpConstI64K, 5, 0, 5),
			vm3.MakeOp(vm3.OpConstI64K, 6, 0, 6),
			// Look up key=7 -> value=70 (set by driver).
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 7),
			vm3.MakeOp(vm3.OpMapGetI64I64, 1, 0, 0), // op.A=1 (slot 1), op.C=0 (slot 0). Neither in [4..6].
			// Accumulate slot4+slot5+slot6+value into slot 7.
			vm3.MakeOp(vm3.OpAddI64, 7, 4, 5),  // slot7 = 4 + 5 = 9
			vm3.MakeOp(vm3.OpAddI64, 7, 7, 6),  // slot7 = 9 + 6 = 15
			vm3.MakeOp(vm3.OpAddI64, 7, 7, 1),  // slot7 = 15 + 70 = 85
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
			vm3.MakeOp(vm3.OpConstI64K, 0, 0, 7),
			vm3.MakeOp(vm3.OpConstI64K, 1, 0, 70),
			vm3.MakeOp(vm3.OpMapSetI64I64, 0, 0, 1),
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
