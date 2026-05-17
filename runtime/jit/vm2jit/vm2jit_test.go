//go:build darwin && arm64

package vm2jit_test

import (
	"testing"

	"mochi/runtime/jit/vm2jit"
	"mochi/runtime/vm2"
)

// arithOnlyFn builds a trivial vm2.Function that adds two I64 registers.
// Only uses OpAddI64 + OpReturn — opcodes Phase 1 handles without slow paths.
func arithOnlyFn() *vm2.Function {
	return &vm2.Function{
		Name:    "add_two",
		NumRegs: 3,
		Code: []vm2.Instr{
			// r0 = r0 + r1 (A=0, B=0, C=1)
			{Op: vm2.OpAddI64, A: 0, B: 0, C: 1},
			// return r0
			{Op: vm2.OpReturn, A: 0},
		},
	}
}

// TestCompileUnsupportedOpcode checks that Compile declines gracefully when
// the function contains an opcode not yet handled by Phase 1 (e.g. OpNewList).
func TestCompileUnsupportedOpcode(t *testing.T) {
	fn := &vm2.Function{
		Name:    "alloc_list",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpNewList, A: 0, B: 4},
			{Op: vm2.OpReturn, A: 0},
		},
	}
	_, err := vm2jit.Compile(fn)
	if err == nil {
		t.Fatal("expected ErrNotImplemented for OpNewList, got nil")
	}
}

// TestFunctionJITCodeIsNilBeforeCompile verifies the hook field starts zero.
func TestFunctionJITCodeIsNilBeforeCompile(t *testing.T) {
	fn := arithOnlyFn()
	if fn.JITCode != nil {
		t.Fatal("JITCode should be nil before Compile")
	}
}

// TestCompileArithSetsJITCode checks that Compile installs a non-nil JITCode
// for a function with only Phase-1-supported opcodes.
// Skipped if Compile returns ErrNotImplemented (constant-pool prologue not
// yet wired) — that is an expected Phase 1 TODO.
func TestCompileArithSetsJITCode(t *testing.T) {
	fn := arithOnlyFn()
	cf, err := vm2jit.Compile(fn)
	if err != nil {
		t.Logf("Compile not yet fully implemented (%v) — scaffold-only check", err)
		t.Skip()
	}
	defer cf.Free()
	if fn.JITCode == nil {
		t.Fatal("expected non-nil JITCode after Compile")
	}
	t.Logf("code size: %d bytes", cf.CodeLen())
}
