package vm3jit

import (
	"encoding/binary"
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
// AArch64 JIT. Slots 0..6 land in x9..x15 (caller-saved, free); slots
// 7..16 land in x19..x28 (callee-saved, requires STP/LDP pairs in the
// prologue/epilogue). Mirrors vm2jit MEP-39 §6.14 Phase B.
const maxI64Regs = 17

// maxI64RegsAMD64 is the cap on the AMD64 backend. Slots 0..5 land in
// RSI, RDI, R8, R9, R10, R11 (caller-saved); slots 6..8 land in R12,
// R13, R14 (callee-saved, pushed/popped in prologue/epilogue). The
// remaining x86_64 GPRs are reserved: RAX is the return register and
// IDIV quotient; RCX is scratch; RDX is IDIV remainder; RBX holds the
// regsI64 base pointer (preserved by the SysV ABI across our internal
// CALL); R15 holds the *int64 status word pointer; RSP/RBP are the
// stack. See lower_amd64.go.
const maxI64RegsAMD64 = 9

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

// MaxI64Regs is the cap on simultaneously-live i64 registers the
// AArch64 backend supports. Exported so tests and callers can size
// their reg scratch buffers correctly. The AMD64 backend caps lower
// (see maxI64RegsAMD64); tests that target both architectures must
// stay within the smaller value.
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

// Options tunes Compile's behavior. Zero value disables features that
// need extra context from the caller.
type Options struct {
	// SelfIdx is fn's index in its containing Program. When set
	// (>= 0), the JIT lowers self-recursive OpCallI64 (op.C ==
	// SelfIdx) to a native BL inside the same code page. -1
	// disables self-recursion: any OpCallI64 returns ErrNotImplemented
	// and the caller falls back to the interpreter.
	SelfIdx int
}

// DefaultOptions returns the conservative defaults for callers that
// build a fn standalone (no Program context).
func DefaultOptions() Options { return Options{SelfIdx: -1} }

// Compile lowers fn to native code for the host architecture and
// returns the handle. Equivalent to CompileWithOptions(fn,
// DefaultOptions()).
func Compile(fn *vm3.Function) (*CompiledFunc, error) {
	return CompileWithOptions(fn, DefaultOptions())
}

// CompileInProgram is the convenience form for Programs: it looks up
// fn = prog.Funcs[idx] and threads idx through as opts.SelfIdx so the
// JIT can lower self-recursive OpCallI64.
func CompileInProgram(prog *vm3.Program, idx uint32) (*CompiledFunc, error) {
	if int(idx) >= len(prog.Funcs) {
		return nil, fmt.Errorf("vm3jit: fn idx %d out of range (Funcs=%d)", idx, len(prog.Funcs))
	}
	return CompileWithOptions(prog.Funcs[idx], Options{SelfIdx: int(idx)})
}

// CompileWithOptions is the explicit-options form. Phase 6.0..6.1d
// only accepts i64-only functions whose opcode set is covered by the
// host backend (lower_arm64 / lower_amd64). Anything else returns
// ErrNotImplemented so callers can fall back to the interpreter
// cleanly.
func CompileWithOptions(fn *vm3.Function, opts Options) (*CompiledFunc, error) {
	if fn.NumRegsF64 != 0 || fn.NumRegsCell != 0 {
		return nil, fmt.Errorf("%w: %s has non-i64 bank usage (F64=%d Cell=%d)",
			ErrNotImplemented, fn.Name, fn.NumRegsF64, fn.NumRegsCell)
	}
	cap, archOK := archMaxI64Regs()
	if !archOK {
		return nil, ErrUnsupported
	}
	if int(fn.NumRegsI64) > cap {
		return nil, fmt.Errorf("%w: %s uses %d i64 regs (max %d on this arch)",
			ErrNotImplemented, fn.Name, fn.NumRegsI64, cap)
	}
	raw, err := lowerHost(fn, opts)
	if err != nil {
		return nil, err
	}
	page, err := pageAlloc(len(raw))
	if err != nil {
		return nil, err
	}
	if err := pageWrite(page, raw); err != nil {
		_ = pageFree(page)
		return nil, err
	}
	return &CompiledFunc{fn: fn, code: page}, nil
}

// lowerHost dispatches to the per-arch lowerer and returns the raw
// little-endian byte stream for the executable page.
func lowerHost(fn *vm3.Function, opts Options) ([]byte, error) {
	switch hostArch {
	case ArchARM64:
		words, err := lowerARM64(fn, opts)
		if err != nil {
			return nil, err
		}
		buf := make([]byte, len(words)*4)
		for i, w := range words {
			binary.LittleEndian.PutUint32(buf[i*4:], w)
		}
		return buf, nil
	case ArchAMD64:
		return lowerAMD64(fn, opts)
	default:
		return nil, ErrUnsupported
	}
}

// archMaxI64Regs returns the host architecture's cap on simultaneously
// live i64 registers, and whether the architecture is supported at
// all. AArch64 supports 17 (x9..x15 caller-saved + x19..x28
// callee-saved). AMD64 supports 12 (R10/R11/RDI/RSI/RDX/RCX/R8/R9
// caller-saved + R12..R15 callee-saved; RBX is reserved for the
// regsI64 base pointer).
func archMaxI64Regs() (int, bool) {
	switch hostArch {
	case ArchARM64:
		return maxI64Regs, true
	case ArchAMD64:
		return maxI64RegsAMD64, true
	default:
		return 0, false
	}
}
