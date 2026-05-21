package copypatch

import (
	"encoding/binary"
	"errors"
	"fmt"

	"mochi/compiler3/ir"
)

// ErrUnsupportedArch is returned by Compile when no stencil table is
// available for the host GOARCH. Phase 1 shipped an amd64 table only;
// Phase 2 widens the table to cover the full non-allocating opcode set
// (still amd64-only); aarch64 lands in Phase 2.1.
var ErrUnsupportedArch = errors.New("copypatch: no stencil table for this GOARCH (phase 2 ships amd64 only)")

// ErrNoStencil is returned by Compile when a Function references an
// IR opcode the stencil table does not cover. The expected response
// is fall-back to the vm3 interpreter; the JIT does not synthesize
// missing stencils. Phase 2.3 introduces the cross-op register
// allocator that unlocks OpPhi support; until then, OpPhi reports
// ErrNoStencil.
var ErrNoStencil = errors.New("copypatch: stencil table does not cover this opcode")

// ErrBranchOutOfRange is returned by Compile when an inter-block
// branch displacement exceeds the int32 range an E9/0F 85 rel32
// instruction can express. The amd64 ISA tops out at ±2 GiB; a Mochi
// function that produces a buffer larger than that is far outside the
// Phase 2 envelope and triggers fall-back to vm3.
var ErrBranchOutOfRange = errors.New("copypatch: inter-block branch displacement exceeds int32 range")

// Emitter walks a compiler3 IR Function and produces a buffer of
// machine code plus the relocations the runtime patcher applies. It
// is the JIT analog of compiler3/emit/go: same input shape (an
// ir.Function), different output (machine bytes vs Go source).
//
// One Emitter instance compiles one Function. Construction is cheap
// (no syscalls, no allocations beyond a small slice preallocation);
// the per-VM JIT keeps a sync.Pool of them in Phase 1.6 once the BG
// parity gate lands.
//
// Phase 2 extends Phase 1's single-block emitter to walk multi-block
// CFGs with TermJump and TermBranch terminators. The walk happens in
// fn.Blocks order (the IR's stable block ID order); branches that
// target a block emitted later in the buffer are recorded as
// blockFixups and patched after the whole function is emitted. Block
// ordering is the IR builder's responsibility; the emitter does not
// reorder.
type Emitter struct {
	stencils map[ir.OpCode]Stencil

	// out is the accumulating machine-code buffer. The emitter appends
	// one stencil's Bytes per IR op (the "copy" half of copy-and-patch);
	// the "patch" half walks the parallel relocs slice plus blockFixups
	// at the end and rewrites offsets in place.
	out []byte

	// relocs accumulates the patch sites for the whole function, with
	// each RelocSite.Offset re-based to the function-buffer offset (not
	// the per-stencil offset). The runtime patcher applies these after
	// the buffer is mmap'd into the code cache.
	relocs []RelocSite

	// blockStarts records the function-buffer offset at which each
	// block's code begins. Indexed by ir.Block.ID (which equals the
	// block's position in fn.Blocks, per the IR's AddBlock contract).
	blockStarts []uint32

	// blockFixups records every inter-block branch instruction that
	// references a yet-unresolved block start. After the whole function
	// is emitted, finalizeBranches walks this slice and writes the
	// resolved rel32 displacement at each site.
	blockFixups []blockFixup
}

// blockFixup names a 4-byte little-endian int32 displacement slot in
// e.out whose value is target-block-start minus the end of the rel32
// field. site is the offset of the displacement word itself; targetID
// is the ir.Block.ID the branch jumps to.
type blockFixup struct {
	site     uint32
	targetID uint32
}

// NewEmitter constructs an Emitter for the host architecture. Returns
// ErrUnsupportedArch on non-amd64 hosts (Phase 2 scope). Callers must
// check Supported() if they want a non-error precheck.
func NewEmitter() (*Emitter, error) {
	tab := archStencils()
	if tab == nil {
		return nil, ErrUnsupportedArch
	}
	return &Emitter{stencils: tab}, nil
}

// Supported reports whether the host architecture has a stencil
// table. False on non-amd64 in Phase 2. The runtime gates
// `mochi run --jit=copypatch` on this and falls back to vm3 when
// false.
func Supported() bool {
	return archStencils() != nil
}

// Compile walks fn and produces the machine-code buffer plus
// relocations. The returned buffer is RW; the caller must hand it to
// a code-cache slab (cache.go) for the W^X flip before jumping to it.
//
// Phase 2 covers the non-allocating opcode set: OpConst, the i64
// arithmetic ops (Add/Sub/Mul/Neg plus the matching Imm variants),
// the six i64 comparisons (Eq/Ne/Lt/Le/Gt/Ge plus Imm variants), and
// the three terminators TermReturn / TermJump / TermBranch across
// multi-block CFGs. OpPhi triggers ErrNoStencil because the cross-op
// register allocator that unlocks join-point liveness handling lands
// in Phase 2.3. OpDivI64 / OpModI64 trigger ErrNoStencil because
// their overflow handling needs a slow-path call (Phase 2.5). f64
// arithmetic is deferred to Phase 2.2. Anything else falls back to
// vm3.
func (e *Emitter) Compile(fn *ir.Function) ([]byte, []RelocSite, error) {
	if fn == nil {
		return nil, nil, fmt.Errorf("copypatch.Compile: nil Function")
	}
	if len(fn.Blocks) == 0 {
		return nil, nil, fmt.Errorf("%w: function has no blocks", ErrNoStencil)
	}
	e.out = e.out[:0]
	e.relocs = e.relocs[:0]
	if cap(e.blockStarts) < len(fn.Blocks) {
		e.blockStarts = make([]uint32, len(fn.Blocks))
	} else {
		e.blockStarts = e.blockStarts[:len(fn.Blocks)]
		for i := range e.blockStarts {
			e.blockStarts[i] = 0
		}
	}
	e.blockFixups = e.blockFixups[:0]

	for bi := range fn.Blocks {
		blk := &fn.Blocks[bi]
		e.blockStarts[bi] = uint32(len(e.out))
		if err := e.emitBlock(fn, blk); err != nil {
			return nil, nil, err
		}
		if err := e.emitTerm(blk); err != nil {
			return nil, nil, err
		}
	}
	if err := e.finalizeBranches(); err != nil {
		return nil, nil, err
	}
	// Defensive copy so the caller's mutation cannot disturb the
	// emitter's internal buffers (and vice versa).
	outCopy := make([]byte, len(e.out))
	copy(outCopy, e.out)
	relocsCopy := make([]RelocSite, len(e.relocs))
	copy(relocsCopy, e.relocs)
	return outCopy, relocsCopy, nil
}

// emitBlock emits the value-producing instructions of blk. OpParam is
// skipped (params are arena-base-relative loads inserted by Phase 2.2's
// register allocator). OpPhi triggers ErrNoStencil (deferred to 2.3).
// Every other opcode must have a Phase 2 stencil; anything missing
// reports ErrNoStencil so the caller can fall back to vm3.
func (e *Emitter) emitBlock(fn *ir.Function, blk *ir.Block) error {
	for _, vid := range blk.Values {
		v := &fn.Values[vid]
		switch v.Op {
		case ir.OpParam:
			continue
		case ir.OpPhi:
			return fmt.Errorf("%w: phi (deferred to phase 2.3 cross-op register allocator)",
				ErrNoStencil)
		}
		s, ok := e.stencils[v.Op]
		if !ok {
			return fmt.Errorf("%w: %s", ErrNoStencil, v.Op)
		}
		e.appendStencil(&s, v)
	}
	return nil
}

// emitTerm emits the block's terminator. TermReturn appends the ret
// stencil (registered under OpInvalid in the amd64 table). TermJump
// emits E9 + rel32 and records a blockFixup. TermBranch emits
// test rax,rax + jne rel32 + jmp rel32, recording two blockFixups.
// Any other kind reports ErrNoStencil.
func (e *Emitter) emitTerm(blk *ir.Block) error {
	switch blk.Term.Kind {
	case ir.TermReturn:
		ret, ok := e.stencils[ir.OpInvalid]
		if !ok {
			return fmt.Errorf("%w: ret stencil missing from table", ErrNoStencil)
		}
		e.appendStencil(&ret, nil)
		return nil

	case ir.TermJump:
		if !archSupportsBranches() {
			return fmt.Errorf("%w: inter-block jump (host GOARCH lacks Phase 2.x terminator encodings)",
				ErrNoStencil)
		}
		// E9 cd  jmp rel32
		base := uint32(len(e.out))
		e.out = append(e.out, 0xE9, 0, 0, 0, 0)
		e.blockFixups = append(e.blockFixups, blockFixup{
			site:     base + 1,
			targetID: blk.Term.Target,
		})
		return nil

	case ir.TermBranch:
		if !archSupportsBranches() {
			return fmt.Errorf("%w: inter-block branch (host GOARCH lacks Phase 2.x terminator encodings)",
				ErrNoStencil)
		}
		// test rax, rax       (48 85 C0)
		// jne IfTrue          (0F 85 cd)
		// jmp IfFalse         (E9 cd)
		base := uint32(len(e.out))
		e.out = append(e.out,
			0x48, 0x85, 0xC0,
			0x0F, 0x85, 0, 0, 0, 0,
			0xE9, 0, 0, 0, 0,
		)
		e.blockFixups = append(e.blockFixups,
			blockFixup{site: base + 5, targetID: blk.Term.IfTrue},
			blockFixup{site: base + 10, targetID: blk.Term.IfFalse},
		)
		return nil

	default:
		return fmt.Errorf("%w: unsupported terminator kind %d", ErrNoStencil, blk.Term.Kind)
	}
}

// finalizeBranches walks e.blockFixups and writes the resolved rel32
// displacement at each site. A displacement is the target block's
// start offset minus the end of the rel32 field (site + 4). The
// resulting int64 must fit in int32; if not, ErrBranchOutOfRange is
// returned and the caller falls back to vm3.
func (e *Emitter) finalizeBranches() error {
	for _, f := range e.blockFixups {
		if int(f.targetID) >= len(e.blockStarts) {
			return fmt.Errorf("%w: branch target block ID %d out of range (have %d blocks)",
				ErrNoStencil, f.targetID, len(e.blockStarts))
		}
		target := int64(e.blockStarts[f.targetID])
		end := int64(f.site) + 4
		disp := target - end
		if disp < -(1<<31) || disp > (1<<31)-1 {
			return fmt.Errorf("%w: displacement %d at site %d", ErrBranchOutOfRange, disp, f.site)
		}
		binary.LittleEndian.PutUint32(e.out[f.site:f.site+4], uint32(int32(disp)))
	}
	return nil
}

// appendStencil copies s.Bytes into e.out, rebasing each reloc's
// Offset to the function-buffer coordinate. The caller's Value v
// flows through so the Addend on an immediate stencil can carry the
// IR's Value.Const (the OpConst path on RelocImm64 and the *Imm
// opcode path on RelocImm32). v may be nil for terminator stencils.
//
// Phase 2 trick on immediates: the reloc names SymOpRetTarget as a
// placeholder symbol; the actual literal flows through Addend. At
// Install time the SymbolTable resolves SymOpRetTarget to zero so the
// patcher writes the Addend value verbatim. Phase 1.1's stencilgen
// pipeline will replace this with a dedicated SymImmI64 once Clang
// drives symbol selection from real relocations.
func (e *Emitter) appendStencil(s *Stencil, v *ir.Value) {
	base := uint32(len(e.out))
	e.out = append(e.out, s.Bytes...)
	first := len(e.relocs)
	e.relocs = append(e.relocs, s.Relocs...)
	for i := first; i < len(e.relocs); i++ {
		e.relocs[i].Offset += base
		if v == nil {
			continue
		}
		switch e.relocs[i].Kind {
		case RelocImm64:
			if v.Op == ir.OpConst {
				e.relocs[i].Addend = int32(v.Const)
			}
		case RelocImm32:
			if isImmediateOp(v.Op) {
				e.relocs[i].Addend = int32(v.Const)
			}
		}
	}
}

// isImmediateOp reports whether op is one of the *Imm IR opcodes
// whose Value.Const carries a 32-bit literal the stencil patches into
// its imm32 field. Used by appendStencil to decide whether to copy
// Value.Const into the reloc's Addend.
func isImmediateOp(op ir.OpCode) bool {
	switch op {
	case ir.OpAddI64Imm, ir.OpSubI64Imm, ir.OpMulI64Imm,
		ir.OpDivI64Imm, ir.OpModI64Imm,
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm,
		ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm,
		ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm:
		return true
	}
	return false
}
