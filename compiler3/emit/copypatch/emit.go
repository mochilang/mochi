package copypatch

import (
	"errors"
	"fmt"

	"mochi/compiler3/ir"
)

// ErrUnsupportedArch is returned by Compile when no stencil table is
// available for the host GOARCH. Phase 1 ships an amd64 table only;
// aarch64 lands in 1.5.
var ErrUnsupportedArch = errors.New("copypatch: no stencil table for this GOARCH (phase 1 ships amd64 only)")

// ErrNoStencil is returned by Compile when a Function references an
// IR opcode the stencil table does not cover. The expected response
// is fall-back to the vm3 interpreter; the JIT does not synthesize
// missing stencils. Phase 1.2 widens the table to cover all non-
// allocating ops.
var ErrNoStencil = errors.New("copypatch: stencil table does not cover this opcode")

// Emitter walks a compiler3 IR Function and produces a buffer of
// machine code plus the relocations the runtime patcher applies. It
// is the JIT analog of compiler3/emit/go: same input shape (an
// ir.Function), different output (machine bytes vs Go source).
//
// One Emitter instance compiles one Function. The instance is cheap
// to construct (no syscalls, no allocations beyond a small slice
// preallocation); the per-VM JIT keeps a sync.Pool of them in
// Phase 1.6 once the BG parity gate lands.
type Emitter struct {
	stencils map[ir.OpCode]Stencil

	// out is the accumulating machine-code buffer. The emitter
	// appends one stencil's Bytes per IR op (the "copy" half of
	// copy-and-patch); the "patch" half walks the parallel relocs
	// slice at the end and rewrites offsets in place.
	out []byte

	// relocs accumulates the patch sites for the whole function, with
	// each RelocSite.Offset re-based to the function-buffer offset
	// (not the per-stencil offset). The runtime patcher applies these
	// after the buffer is mmap'd into the code cache.
	relocs []RelocSite

	// fnReturnTerm, when non-zero, names the SSA value the function's
	// TermReturn carries; the emitter loads it into the trampoline's
	// result slot via the OpInvalid (ret) stencil at the end of the
	// last block.
	fnReturnTerm uint32
}

// NewEmitter constructs an Emitter for the host architecture. Returns
// ErrUnsupportedArch on non-amd64 hosts (Phase 1 scope). Callers
// must check Supported() if they want a non-error precheck.
func NewEmitter() (*Emitter, error) {
	tab := archStencils()
	if tab == nil {
		return nil, ErrUnsupportedArch
	}
	return &Emitter{stencils: tab}, nil
}

// Supported reports whether the host architecture has a stencil
// table. False on non-amd64 in Phase 1. The runtime gates
// `mochi run --jit=copypatch` on this and falls back to vm3 when
// false.
func Supported() bool {
	return archStencils() != nil
}

// Compile walks fn and produces the machine-code buffer plus
// relocations. The returned buffer is RW; the caller must hand it to
// a code-cache slab (cache.go) for the W^X flip before jumping to it.
//
// Phase 1 covers a tiny IR shape: a single basic block whose values
// are exactly one OpConst followed by zero or more OpAddI64 values,
// terminated by a TermReturn that names the last value. Anything
// else returns ErrNoStencil and falls back to the interpreter. The
// Phase 1.2 widening lifts this restriction.
func (e *Emitter) Compile(fn *ir.Function) ([]byte, []RelocSite, error) {
	if fn == nil {
		return nil, nil, fmt.Errorf("copypatch.Compile: nil Function")
	}
	if len(fn.Blocks) != 1 {
		return nil, nil, fmt.Errorf("%w: phase 1 requires exactly one block, got %d",
			ErrNoStencil, len(fn.Blocks))
	}
	e.out = e.out[:0]
	e.relocs = e.relocs[:0]
	blk := &fn.Blocks[0]
	for _, vid := range blk.Values {
		v := &fn.Values[vid]
		if v.Op == ir.OpParam {
			continue // params are arena-base-relative loads inserted by 1.2
		}
		s, ok := e.stencils[v.Op]
		if !ok {
			return nil, nil, fmt.Errorf("%w: %s", ErrNoStencil, v.Op)
		}
		e.appendStencil(&s, v)
	}
	if blk.Term.Kind != ir.TermReturn {
		return nil, nil, fmt.Errorf("%w: phase 1 requires TermReturn terminator, got kind %d",
			ErrNoStencil, blk.Term.Kind)
	}
	// Append the ret stencil (registered under OpInvalid in the
	// Phase 1 amd64 table).
	ret, ok := e.stencils[ir.OpInvalid]
	if !ok {
		return nil, nil, fmt.Errorf("%w: ret stencil missing from table", ErrNoStencil)
	}
	e.appendStencil(&ret, nil)
	// Defensive copy so the caller's mutation cannot disturb the
	// emitter's internal buffers (and vice versa).
	outCopy := make([]byte, len(e.out))
	copy(outCopy, e.out)
	relocsCopy := make([]RelocSite, len(e.relocs))
	copy(relocsCopy, e.relocs)
	return outCopy, relocsCopy, nil
}

// appendStencil copies s.Bytes into e.out, rebasing each reloc's
// Offset to the function-buffer coordinate. The caller's Value v is
// passed through so the Addend on an immediate stencil can carry the
// IR's Value.Const (the Phase 1 OpConst path). v may be nil for
// terminator stencils.
//
// Phase 1 trick on OpConst: the reloc names SymOpRetTarget as a
// placeholder symbol; the actual i64 literal flows through Addend.
// At Install time the SymbolTable resolves SymOpRetTarget to zero so
// the patcher writes the Addend value verbatim. Phase 1.1 replaces
// this with a dedicated SymImmI64 once stencilgen drives symbol
// selection from real Clang relocations.
func (e *Emitter) appendStencil(s *Stencil, v *ir.Value) {
	base := uint32(len(e.out))
	e.out = append(e.out, s.Bytes...)
	first := len(e.relocs)
	e.relocs = append(e.relocs, s.Relocs...)
	for i := first; i < len(e.relocs); i++ {
		e.relocs[i].Offset += base
		if v != nil && v.Op == ir.OpConst && e.relocs[i].Kind == RelocImm64 {
			e.relocs[i].Addend = int32(v.Const)
		}
	}
}
