package vm2jit

import (
	"errors"

	"mochi/runtime/vm2"
)

// ErrUnsupported is returned by Compile on architectures that have no backend.
var ErrUnsupported = errors.New("vm2jit: no JIT backend for this architecture")

// CompiledFunc is a handle to a JIT-compiled vm2.Function.
// It owns the executable page and must be freed when the Function is unloaded.
type CompiledFunc struct {
	fn   *vm2.Function
	code []byte // mmap'd executable page
	arch Arch
}

// CodeLen returns the size of the JIT'd code in bytes.
func (c *CompiledFunc) CodeLen() int { return len(c.code) }

// Free releases the executable page. The Function's JITCode field is
// not cleared automatically; the caller must nil it before dropping the
// reference.
func (c *CompiledFunc) Free() error {
	if c.code == nil {
		return nil
	}
	err := pageFree(c.code)
	c.code = nil
	return err
}

// Compile lowers fn to native code for the host architecture, installs
// the result into fn.JITCode, and returns the handle. The caller is
// responsible for calling Free when fn is unloaded.
//
// If the host architecture has no backend, Compile returns ErrUnsupported
// and leaves fn unchanged. This is the expected outcome on GOARCH values
// other than arm64 and amd64.
//
// Phase 1 supports: arithmetic, control flow, list opcodes, call/return.
// Opcodes outside Phase 1's scope cause Compile to return ErrNotImplemented.
func Compile(fn *vm2.Function) (*CompiledFunc, error) {
	if hostArch < 0 {
		return nil, ErrUnsupported
	}
	words, err := lowerFunction(fn, hostArch)
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
	cf := &CompiledFunc{fn: fn, code: page, arch: hostArch}
	// Install the entry point. The trampoline reads this pointer to call
	// into the JIT'd code; vm2's OpCall hook checks fn.JITCode != nil.
	fn.JITCode = pageEntry(page)
	return cf, nil
}
