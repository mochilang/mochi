//go:build arm64

package copypatch

import (
	"mochi/compiler3/ir"
)

// stencilsARM64 is the hand-written aarch64 stencil table covering
// the Phase 1.1 placeholder shape: the same three opcodes the Phase
// 1.0 amd64 table covers (`OpConst`, `OpAddI64`, ret under
// `OpInvalid`), encoded in the AAPCS64 calling convention pinned by
// MEP-42 §2:
//
//	X0      primary value (left operand, result, branch condition)
//	X1      secondary value (right operand for binary ops)
//	X19-X21 reserved for Frame / typed-arena base / per-VM context
//
// Phase 2.1 widens this table to cover the same non-allocating
// arithmetic and comparison surface the amd64 table reached in
// Phase 2.0. Phase 1.1 leaves the wider opcodes off the table so
// `Emitter.Compile` returns `ErrNoStencil` cleanly and the runtime
// falls back to vm3 on every IR that exceeds the placeholder.
//
// # OpConst lowering
//
// aarch64 has no single instruction that loads an arbitrary 64-bit
// immediate. The two production patterns are:
//
//  1. movz + 3 movk (4 instructions, each patching a 16-bit slot);
//     Clang and LLVM emit this for `__int128_t v = literal;`.
//  2. ldr x0, [pc, #N] + a literal pool entry; the assembler emits
//     this when `-mcmodel=large` or when the immediate cannot be
//     synthesized in fewer instructions.
//
// The literal-pool form is chosen for Phase 1.1 because it preserves
// the `RelocImm64` reloc-kind invariant: the 8-byte literal slot
// patches with a single 64-bit write, identical in shape to the
// amd64 `mov rax, imm64`. The movz/movk form would need a new
// `RelocMovkUABS_G{0,1,2,3}` per-slot reloc kind (the LLVM
// `R_AARCH64_MOVW_UABS_G*` family), which is Phase 2.1's
// responsibility to introduce alongside its widened stencil set.
//
//	ldr x0, [pc, #8]   58 00 00 40 LE → 40 00 00 58
//	b   #12            14 00 00 03 LE → 03 00 00 14   (skip the literal)
//	<8 bytes literal>                                  (RelocImm64 patch site)
//
// Total OpConst = 16 bytes. The `b #12` skips past the literal so
// fall-through execution lands on the next stencil. The patcher
// writes the i64 value directly into bytes [8:16] via the standard
// `RelocImm64` site at offset 8.
//
// # OpAddI64 lowering
//
//	add x0, x0, x1     8B 01 00 00 LE → 00 00 01 8B
//
// 4 bytes, no reloc. Result lands in x0.
//
// # ret lowering
//
//	ret                D6 5F 03 C0 LE → C0 03 5F D6
//
// 4 bytes, no reloc. Registered under `OpInvalid` so `TermReturn`
// emission has a fixed lookup key, matching the amd64 convention.
var stencilsARM64 = map[ir.OpCode]Stencil{
	// OpConst: ldr x0, [pc, #8] + b #12 + 8-byte literal pool.
	ir.OpConst: {
		Op: ir.OpConst,
		Bytes: []byte{
			0x40, 0x00, 0x00, 0x58, // ldr x0, [pc, #8]
			0x03, 0x00, 0x00, 0x14, // b #12 (skip the literal)
			0, 0, 0, 0, 0, 0, 0, 0, // literal pool entry (8 bytes)
		},
		Relocs: []RelocSite{
			{Offset: 8, Kind: RelocImm64, Symbol: SymOpRetTarget, Addend: 0},
		},
	},

	// OpAddI64: add x0, x0, x1.
	ir.OpAddI64: {
		Op:    ir.OpAddI64,
		Bytes: []byte{0x00, 0x00, 0x01, 0x8B},
	},

	// ret. Registered under OpInvalid so TermReturn emission has a
	// fixed lookup key. The trampoline reads the result from X0.
	ir.OpInvalid: {
		Op:    ir.OpInvalid,
		Bytes: []byte{0xC0, 0x03, 0x5F, 0xD6},
	},
}

// archStencils returns the stencil table for the host GOARCH on
// aarch64 builds. Phase 1.1 ships a 3-opcode placeholder; Phase 2.1
// widens to the full non-allocating set.
func archStencils() map[ir.OpCode]Stencil {
	return stencilsARM64
}

// archSupportsBranches reports whether the emitter can lower inter-
// block terminators on aarch64. False in Phase 1.1: the rel26 / cbz
// encodings land in Phase 2.1 alongside the widened opcode set. The
// emitter falls back to vm3 via `ErrNoStencil` on any `TermJump` or
// `TermBranch` while this returns false.
func archSupportsBranches() bool {
	return false
}
