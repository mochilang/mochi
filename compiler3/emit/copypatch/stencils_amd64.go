//go:build amd64

package copypatch

import (
	"mochi/compiler3/ir"
)

// stencilsAMD64 is the Phase 1 placeholder stencil table for x86_64.
// The real Clang-extracted set lands in Phase 1.1 via tools/stencilgen;
// for Phase 1 we hand-craft three representative shapes:
//
//   - OpConst: load an i64 immediate into RAX. Encodes
//     `48 B8 ii ii ii ii ii ii ii ii` (mov rax, imm64). The imm64 is
//     the patch site; SymbolID = SymOpRetTarget is reused as a
//     placeholder symbol slot for the literal value, with Addend
//     carrying the actual i64 constant. This is a deliberate
//     placeholder: Phase 1.1 introduces SymImmI64 once stencilgen
//     drives the symbol selection.
//
//   - OpAddI64: `48 01 F8` (add rax, rdi). No relocs; the calling
//     stencil convention puts the left operand in RAX and the right
//     in RDI. This is the simplest shape: bytes only, no patch.
//
//   - OpReturnI64: `C3` (ret). No relocs. The Phase 1 trampoline
//     reads the return value from RAX after the ret.
//
// These three stencils exercise the load-bearing shapes the patcher
// needs to validate: an imm-shaped reloc, a no-reloc stencil, and a
// minimal one-byte stencil. The emitter and cache tests use them as
// fixtures.
var stencilsAMD64 = map[ir.OpCode]Stencil{
	ir.OpConst: {
		Op: ir.OpConst,
		// 48 B8 + 8 zero bytes (the imm64 patch site).
		Bytes: []byte{
			0x48, 0xB8,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
		Relocs: []RelocSite{
			{Offset: 2, Kind: RelocImm64, Symbol: SymOpRetTarget, Addend: 0},
		},
	},
	ir.OpAddI64: {
		Op:     ir.OpAddI64,
		Bytes:  []byte{0x48, 0x01, 0xF8},
		Relocs: nil,
	},
	// OpReturnI64 is not a distinct IR opcode; the IR's TermReturn
	// terminator drives Return emission. We register the stencil under
	// OpInvalid so the emitter can fetch it via a dedicated path. This
	// is the same trick CPython's JIT uses for control-flow stencils.
	ir.OpInvalid: {
		Op:     ir.OpInvalid,
		Bytes:  []byte{0xC3},
		Relocs: nil,
	},
}

// archStencils returns the stencil table for the host GOARCH. The
// build-tagged file ensures only one definition is linked into the
// final binary; non-amd64 builds get the stencils_other.go fallback.
func archStencils() map[ir.OpCode]Stencil {
	return stencilsAMD64
}
