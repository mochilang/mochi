package vm3jit

import (
	"errors"
	"fmt"
	"unsafe"

	"mochi/runtime/vm3"
)

// ErrUnsupported is returned by Compile on architectures that have no
// vm3jit backend.
var ErrUnsupported = errors.New("vm3jit: no backend for this architecture")

// ErrNotImplemented is returned by Compile when fn uses an opcode,
// register count, or bank shape outside the current backend's scope.
// Callers fall back to the vm3 interpreter.
var ErrNotImplemented = errors.New("vm3jit: not implemented")

// maxI64Regs is the cap on simultaneously live i64 registers in the
// JIT. Slots 0..6 land in x9..x15 (caller-saved, free); slots 7..16
// land in x19..x28 (callee-saved, requires STP/LDP pairs in the
// prologue/epilogue). Mirrors vm2jit MEP-39 §6.14 Phase B.
const maxI64Regs = 17

// CompiledFunc is a handle to a vm3jit-compiled function. It owns the
// executable page and must be freed via Free when the function is
// unloaded.
type CompiledFunc struct {
	fn   *vm3.Function
	code []byte
}

// CodeLen returns the size of the JIT'd code in bytes.
func (c *CompiledFunc) CodeLen() int { return len(c.code) }

// Entry returns the executable entry pointer to be passed as the
// first argument of trampoline.Call.
func (c *CompiledFunc) Entry() unsafe.Pointer { return pageEntry(c.code) }

// MaxI64Regs is the cap on simultaneously-live i64 registers Phase
// 6.0 supports. Exported so tests and callers can size their reg
// scratch buffers correctly.
const MaxI64Regs = maxI64Regs

// Free releases the executable page.
func (c *CompiledFunc) Free() error {
	if c.code == nil {
		return nil
	}
	err := pageFree(c.code)
	c.code = nil
	return err
}

// Compile lowers fn to native code for the host architecture and
// returns the handle. Phase 6.0 only accepts i64-only functions whose
// opcode set is covered by lower_arm64. Anything else returns
// ErrNotImplemented so callers can fall back to the interpreter
// cleanly.
func Compile(fn *vm3.Function) (*CompiledFunc, error) {
	if hostArch != ArchARM64 {
		return nil, ErrUnsupported
	}
	if fn.NumRegsF64 != 0 || fn.NumRegsCell != 0 {
		return nil, fmt.Errorf("%w: %s has non-i64 bank usage (F64=%d Cell=%d)",
			ErrNotImplemented, fn.Name, fn.NumRegsF64, fn.NumRegsCell)
	}
	if fn.NumRegsI64 > maxI64Regs {
		return nil, fmt.Errorf("%w: %s uses %d i64 regs (max %d in 6.0)",
			ErrNotImplemented, fn.Name, fn.NumRegsI64, maxI64Regs)
	}
	words, err := lowerARM64(fn)
	if err != nil {
		return nil, err
	}
	page, err := pageAlloc(len(words) * 4)
	if err != nil {
		return nil, err
	}
	if err := pageWrite(page, words); err != nil {
		_ = pageFree(page)
		return nil, err
	}
	return &CompiledFunc{fn: fn, code: page}, nil
}
