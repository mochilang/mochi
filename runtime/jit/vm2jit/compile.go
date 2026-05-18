package vm2jit

import (
	"errors"
	"fmt"

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
// maxJITRegs is the maximum number of vm2 registers supported by any JIT
// backend. Functions with more registers must be interpreted.
//
// MEP-39 §6.14 (Phase B full) takes the limit from 9 to 17 by pushing
// all five AArch64 callee-saved GPR pairs (x19..x28) in the prologue.
// The cap lift exposes a follow-up regression: outer-loop functions
// that newly JIT but still deopt on every OpCall to non-JIT'd inner
// loops can pay more in prologue + deopt-resume than they save. The
// regression is recorded in §6.14 and addressed by gating with
// jitProfitable (this file) until iter 14 lowers OpCall as a JIT-side
// fast path.
const maxJITRegs = 17

// ErrUnprofitable is returned by Compile when fn is structurally
// unprofitable to JIT under the current backend (see jitProfitable).
// It is wrapped in ErrNotImplemented so existing callers that only
// distinguish "JIT'd vs interp" continue to work.
var ErrUnprofitable = fmt.Errorf("%w: deopt fraction too high", ErrNotImplemented)

func Compile(fn *vm2.Function) (*CompiledFunc, error) {
	if hostArch < 0 {
		return nil, ErrUnsupported
	}
	if fn.NumRegs > maxJITRegs {
		return nil, fmt.Errorf("%w: %s uses %d registers (max %d)",
			ErrNotImplemented, fn.Name, fn.NumRegs, maxJITRegs)
	}
	if !jitProfitable(fn) {
		return nil, fmt.Errorf("%w: %s", ErrUnprofitable, fn.Name)
	}
	return compileNoGate(fn)
}

// CompileProgram attempts to JIT-compile every function in p. It
// returns the number of functions that were successfully compiled
// (and now have fn.JITCode set) and the total number of functions
// considered. Per-function errors are silently absorbed: any failure
// (unsupported arch, NumRegs over cap, unprofitable, unlowerable
// opcode) leaves fn.JITCode nil so the interpreter's OpCall fast
// path naturally falls through. The returned handles are owned by
// the caller and must be Free'd before the program is discarded.
//
// MEP-39 §6.15 wires this into bench/vm2runner so the iter 12-13
// JIT scaffolding actually fires on the bench path.
func CompileProgram(p *vm2.Program) (handles []*CompiledFunc, compiled, total int) {
	if p == nil {
		return nil, 0, 0
	}
	total = len(p.Funcs)
	handles = make([]*CompiledFunc, 0, total)
	for _, fn := range p.Funcs {
		if fn == nil {
			continue
		}
		cf, err := Compile(fn)
		if err != nil {
			continue
		}
		handles = append(handles, cf)
		compiled++
	}
	return handles, compiled, total
}

// compileNoGate lowers fn unconditionally (skipping the cap and
// profitability gate Compile applies). The deopt-stub tests use this
// via export_test.go to exercise machinery that the public gate would
// reject as unprofitable.
func compileNoGate(fn *vm2.Function) (*CompiledFunc, error) {
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
	fn.JITCode = pageEntry(page)
	return cf, nil
}
