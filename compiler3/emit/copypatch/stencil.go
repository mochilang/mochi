package copypatch

import (
	"fmt"

	"mochi/compiler3/ir"
)

// RelocKind names the patch shape applied at a relocation site. The
// set is intentionally small: copy-and-patch needs to express only
// immediates, pc-relative branches, and runtime-symbol absolute
// references. Anything outside this set is rejected at stencilgen
// time so the runtime patcher does not need to recognize it.
type RelocKind uint8

const (
	// RelocInvalid is the zero value and should never appear in a
	// loaded stencil.
	RelocInvalid RelocKind = iota

	// RelocImm32 writes a 32-bit little-endian immediate at Offset.
	// Used for x86_64 immediates that fit in 32 bits (most arithmetic
	// constants and small literal values).
	RelocImm32

	// RelocImm64 writes a 64-bit little-endian immediate at Offset.
	// Used for `mov rax, imm64` style payloads (large literal i64,
	// pointer-sized address constants).
	RelocImm64

	// RelocPCRel32 writes (target - (siteAddr + 4)) at Offset as a
	// 32-bit little-endian signed displacement. The standard x86_64
	// call/jmp encoding. Used for cross-stencil branches and for
	// runtime-helper calls that fit within ±2 GiB of the code cache.
	RelocPCRel32

	// RelocAbs64 writes the 64-bit target as a little-endian absolute
	// address. Used for runtime-helper calls when ±2 GiB is not
	// guaranteed (e.g., kalloc_type pointer, arena-base table address).
	RelocAbs64
)

// String renders a RelocKind for diagnostics. Used in stencil-table
// validation errors and in cache dump output.
func (r RelocKind) String() string {
	switch r {
	case RelocInvalid:
		return "invalid"
	case RelocImm32:
		return "imm32"
	case RelocImm64:
		return "imm64"
	case RelocPCRel32:
		return "pcrel32"
	case RelocAbs64:
		return "abs64"
	}
	return "?"
}

// SymbolID names the abstract target of a RelocPCRel32 or RelocAbs64
// relocation. The runtime patcher resolves SymbolID to a concrete
// address by consulting the SymbolTable handed to applyRelocs. The
// SymbolID space is closed at compile time: stencilgen only emits
// references to the symbols enumerated here, and the runtime fails
// loudly on any other value.
type SymbolID uint16

const (
	SymInvalid SymbolID = iota

	// SymArenaBase is the address of the typed-arena base table
	// (runtime/vm3.Arenas). Stencils load handle payloads via
	// `[arenaBase + arenaTag*slabStride + idx*slotStride]`.
	SymArenaBase

	// SymFrame is the address of the current vm3 Frame for which the
	// emitted code was JIT-compiled. The emitter resolves this once at
	// emit time so per-op stencils can immediately deref operands
	// without an extra indirection.
	SymFrame

	// SymVMCtx is the address of the per-VM context block (PC stash,
	// deopt sentinel slot, slow-path landing pad table).
	SymVMCtx

	// SymSlowPathDeref is the slow-path callback for a failed handle
	// generation check. Invoked when the per-deref gen check
	// (MEP-41 rule class A) fails inside a stencil; the slow path
	// raises the appropriate Mochi runtime panic.
	SymSlowPathDeref

	// SymSlowPathDeopt is the deopt landing pad. A stencil invokes
	// this when it cannot continue under JIT (verifier sentinel, arena
	// exhaustion, gen-wrap quarantine hit) so the runtime can fall
	// back to vm3 interpretation.
	SymSlowPathDeopt

	// SymOpRetTarget is the address the OpReturn* stencil writes to
	// (the trampoline's result-pointer slot). The trampoline ABI is
	// shared with runtime/jit/vm2jit/trampoline; copypatch reuses it
	// unchanged in Phase 1.
	SymOpRetTarget
)

// String renders a SymbolID for diagnostics. Used in error messages
// and in cache dump output.
func (s SymbolID) String() string {
	switch s {
	case SymInvalid:
		return "invalid"
	case SymArenaBase:
		return "arena_base"
	case SymFrame:
		return "frame"
	case SymVMCtx:
		return "vm_ctx"
	case SymSlowPathDeref:
		return "slow_path_deref"
	case SymSlowPathDeopt:
		return "slow_path_deopt"
	case SymOpRetTarget:
		return "op_ret_target"
	}
	return "?"
}

// RelocSite is one patch location inside a stencil's byte stream. The
// Offset is relative to the stencil's first byte. Addend is the
// signed constant added to the symbol value before encoding (always
// zero for RelocImm32 / RelocImm64; nonzero for RelocPCRel32 when a
// stencil needs to call into the middle of another stencil, which is
// the GHC-graph-style code-shape technique).
type RelocSite struct {
	Offset uint32
	Kind   RelocKind
	Symbol SymbolID
	Addend int32
}

// Stencil is the load-bearing unit of the copy-and-patch backend: a
// blob of machine code plus the list of patch sites the runtime
// applies before the code is executable. Bytes carries the raw
// machine bytes as Clang emitted them (or, for the hand-written
// Phase 1 placeholders, the byte sequence the stencil author crafted).
// Relocs lists the patch sites that must be applied before Bytes is
// jumped to.
//
// A Stencil never names its own ISA; the package-level
// stencils_<goarch>.go file is per-ISA, so the build system selects
// the right table at compile time and the runtime never has to gate
// on runtime.GOARCH.
type Stencil struct {
	Op     ir.OpCode
	Bytes  []byte
	Relocs []RelocSite
}

// validate reports an error if s is structurally malformed. Used by
// stencil_test.go to gate the generated tables and by emit.go before
// patching. The check is cheap: it is O(len(Relocs)).
//
// Errors caught:
//   - Bytes is nil or empty (stencilgen should never emit a zero-byte op).
//   - A RelocSite Offset is out of range for Bytes.
//   - A RelocSite Kind is RelocInvalid.
//   - A RelocSite Symbol is SymInvalid.
//   - Two RelocSites overlap (the patcher writes raw bytes; overlap
//     would leave the final state dependent on Reloc order).
func (s *Stencil) validate() error {
	if len(s.Bytes) == 0 {
		return fmt.Errorf("stencil %s has empty Bytes", s.Op)
	}
	intervals := make([][2]uint32, 0, len(s.Relocs))
	for i, r := range s.Relocs {
		if r.Kind == RelocInvalid {
			return fmt.Errorf("stencil %s reloc #%d has RelocInvalid kind", s.Op, i)
		}
		if r.Symbol == SymInvalid {
			return fmt.Errorf("stencil %s reloc #%d has SymInvalid symbol", s.Op, i)
		}
		width := relocWidth(r.Kind)
		end := uint64(r.Offset) + uint64(width)
		if end > uint64(len(s.Bytes)) {
			return fmt.Errorf("stencil %s reloc #%d (kind=%s) at offset %d extends past Bytes len %d",
				s.Op, i, r.Kind, r.Offset, len(s.Bytes))
		}
		intervals = append(intervals, [2]uint32{r.Offset, uint32(end)})
	}
	for i := 0; i < len(intervals); i++ {
		for j := i + 1; j < len(intervals); j++ {
			a, b := intervals[i], intervals[j]
			if a[0] < b[1] && b[0] < a[1] {
				return fmt.Errorf("stencil %s relocs #%d and #%d overlap (%d..%d vs %d..%d)",
					s.Op, i, j, a[0], a[1], b[0], b[1])
			}
		}
	}
	return nil
}

// relocWidth returns the number of bytes a relocation of the given
// kind writes. Used by validate() and by patch.go's bounds check.
func relocWidth(k RelocKind) uint32 {
	switch k {
	case RelocImm32, RelocPCRel32:
		return 4
	case RelocImm64, RelocAbs64:
		return 8
	}
	return 0
}

// SymbolTable maps each SymbolID to a runtime address. The emitter
// fills it just before calling applyRelocs; SymInvalid is implicitly
// the zero value and is rejected by applyRelocs.
type SymbolTable [16]uintptr

// Set records that SymbolID id resolves to addr at the current emit.
// Out-of-range ids panic: SymbolID is closed at compile time, so an
// out-of-range write is a stencilgen bug, not a runtime error.
func (t *SymbolTable) Set(id SymbolID, addr uintptr) {
	if int(id) >= len(t) {
		panic(fmt.Sprintf("copypatch.SymbolTable: id %d out of range", id))
	}
	t[id] = addr
}

// Get reads the address bound to id. Returns zero if unset (the
// patcher rejects this case).
func (t *SymbolTable) Get(id SymbolID) uintptr {
	if int(id) >= len(t) {
		return 0
	}
	return t[id]
}
