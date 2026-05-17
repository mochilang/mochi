package vm2jit

import (
	"errors"
	"fmt"

	"mochi/runtime/vm2"
)

// ErrNotImplemented is returned when an opcode is not yet handled by the JIT.
var ErrNotImplemented = errors.New("vm2jit: opcode not yet implemented")

// lowerFunction compiles fn's bytecode to native words for arch.
// ARM64 uses the two-pass compileFnARM64 path (branch relocation requires it).
// AMD64 uses a single-pass per-instruction loop.
func lowerFunction(fn *vm2.Function, arch Arch) ([]uint32, error) {
	switch arch {
	case ARM64:
		return lowerFnARM64(fn)
	case AMD64:
		var words []uint32
		for i, ins := range fn.Code {
			ws, err := lowerAMD64(fn, i, ins)
			if err != nil {
				return nil, fmt.Errorf("vm2jit: instr %d (%v): %w", i, ins.Op, err)
			}
			words = append(words, ws...)
		}
		return words, nil
	default:
		return nil, ErrUnsupported
	}
}
