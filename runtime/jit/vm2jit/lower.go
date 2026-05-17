package vm2jit

import (
	"errors"
	"fmt"

	"mochi/runtime/vm2"
)

// ErrNotImplemented is returned when an opcode is not yet handled by the JIT.
var ErrNotImplemented = errors.New("vm2jit: opcode not yet implemented")

// lowerFunction walks fn's bytecode and emits a flat []uint32 of native
// instructions (AArch64 or AMD64 little-endian words). The result is
// suitable for passing directly to pageWrite.
func lowerFunction(fn *vm2.Function, arch Arch) ([]uint32, error) {
	var words []uint32
	for i, ins := range fn.Code {
		ws, err := lowerInstr(fn, i, ins, arch)
		if err != nil {
			return nil, fmt.Errorf("vm2jit: instr %d (%v): %w", i, ins.Op, err)
		}
		words = append(words, ws...)
	}
	return words, nil
}

// lowerInstr emits the native instruction words for one vm2 Instr.
// Two-pass relocation (branch targets) is handled at this level by
// building a pc-map on the first pass and patching on the second; for
// the scaffold, every branch emits a placeholder and is patched after
// the full word slice is built.
func lowerInstr(fn *vm2.Function, idx int, ins vm2.Instr, arch Arch) ([]uint32, error) {
	switch arch {
	case ARM64:
		return lowerARM64(fn, idx, ins)
	case AMD64:
		return lowerAMD64(fn, idx, ins)
	default:
		return nil, ErrUnsupported
	}
}
