//go:build amd64

package copypatch

import (
	"mochi/compiler3/ir"
)

// stencilsAMD64 is the hand-written x86_64 stencil table for the
// non-allocating subset of compiler3 IR opcodes. Phase 1.1's
// stencilgen pipeline will eventually supersede this with a Clang-
// extracted, byte-identical generated file; the hand-written table
// remains as the Phase-1 reference, the fallback when stencilgen
// fails, and the test substrate.
//
// # Calling convention (Mochi copy-and-patch ABI on x86_64)
//
//	R12     pointer to the current vm3 Frame
//	R13     pointer to the typed-arena base table (Arenas)
//	R14     pointer to the per-VM context (PC stash, deopt slot)
//	RAX     primary value (left operand, result, branch condition)
//	RDI     secondary value (right operand for binary ops)
//	RSI     scratch (cmp result staging, shuffle temp)
//	RDX     scratch (mul high-half, div remainder)
//
// Stencils never spill R12-R14; the emitter never emits code that
// clobbers them. RAX/RDI hold the value-stack top and second-from-
// top; the cross-op Phase 2.3 register allocator widens this to a
// proper allocation, but Phase 2 pins the two-register convention to
// keep stencils stateless.
//
// # Phase 2 stencil set
//
// Arithmetic (i64): OpConst, OpAddI64, OpSubI64, OpMulI64, OpNegI64,
// OpAddI64Imm, OpSubI64Imm, OpMulI64Imm.
//
// Comparisons (i64): OpCmpEqI64, OpCmpNeI64, OpCmpLtI64, OpCmpLeI64,
// OpCmpGtI64, OpCmpGeI64, plus the six matching Imm variants.
//
// Control flow: ret (registered under OpInvalid), TermJump (relative
// branch emitted directly by the emitter, not as a stencil), and
// TermBranch (test rax + jcc emitted directly by the emitter).
//
// Not in Phase 2: OpDivI64 / OpModI64 (need slow-path call for
// overflow handling; Phase 2.5), f64 arithmetic (Phase 2.2), aarch64
// (Phase 2.1), the cross-op register allocator (Phase 2.3), and the
// BG kernel performance gate (Phase 2.4).
var stencilsAMD64 = map[ir.OpCode]Stencil{
	// ----- Constants ---------------------------------------------------

	// OpConst: mov rax, imm64. The literal flows through the reloc's
	// Addend; Phase 1.1 introduces SymImmI64 as a dedicated symbol.
	ir.OpConst: {
		Op:    ir.OpConst,
		Bytes: []byte{0x48, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0},
		Relocs: []RelocSite{
			{Offset: 2, Kind: RelocImm64, Symbol: SymOpRetTarget, Addend: 0},
		},
	},

	// ----- i64 arithmetic ---------------------------------------------

	// OpAddI64: add rax, rdi  (48 01 F8)
	ir.OpAddI64: {
		Op:    ir.OpAddI64,
		Bytes: []byte{0x48, 0x01, 0xF8},
	},
	// OpSubI64: sub rax, rdi  (48 29 F8)
	ir.OpSubI64: {
		Op:    ir.OpSubI64,
		Bytes: []byte{0x48, 0x29, 0xF8},
	},
	// OpMulI64: imul rax, rdi  (48 0F AF C7)
	ir.OpMulI64: {
		Op:    ir.OpMulI64,
		Bytes: []byte{0x48, 0x0F, 0xAF, 0xC7},
	},
	// OpNegI64: neg rax  (48 F7 D8)
	ir.OpNegI64: {
		Op:    ir.OpNegI64,
		Bytes: []byte{0x48, 0xF7, 0xD8},
	},

	// OpAddI64Imm: add rax, imm32  (48 05 ii ii ii ii). Sign-extends.
	// Literal flows through the reloc's Addend; SymImmI64 lands in
	// Phase 1.1.
	ir.OpAddI64Imm: {
		Op:    ir.OpAddI64Imm,
		Bytes: []byte{0x48, 0x05, 0, 0, 0, 0},
		Relocs: []RelocSite{
			{Offset: 2, Kind: RelocImm32, Symbol: SymOpRetTarget, Addend: 0},
		},
	},
	// OpSubI64Imm: sub rax, imm32  (48 2D ii ii ii ii). Sign-extends.
	ir.OpSubI64Imm: {
		Op:    ir.OpSubI64Imm,
		Bytes: []byte{0x48, 0x2D, 0, 0, 0, 0},
		Relocs: []RelocSite{
			{Offset: 2, Kind: RelocImm32, Symbol: SymOpRetTarget, Addend: 0},
		},
	},
	// OpMulI64Imm: imul rax, rax, imm32  (48 69 C0 ii ii ii ii). Sign-extends.
	ir.OpMulI64Imm: {
		Op:    ir.OpMulI64Imm,
		Bytes: []byte{0x48, 0x69, 0xC0, 0, 0, 0, 0},
		Relocs: []RelocSite{
			{Offset: 3, Kind: RelocImm32, Symbol: SymOpRetTarget, Addend: 0},
		},
	},

	// ----- i64 comparisons --------------------------------------------
	//
	// All compares lower as:
	//   cmp rax, rdi        (48 39 F8)
	//   setCC al            (0F 9? C0)
	//   movzx rax, al       (48 0F B6 C0)
	//
	// The setCC encoding varies per relation:
	//   sete  / setz   = 94
	//   setne / setnz  = 95
	//   setl           = 9C
	//   setle          = 9E
	//   setg           = 9F
	//   setge          = 9D

	ir.OpCmpEqI64: {Op: ir.OpCmpEqI64, Bytes: cmpStencil(0x94)},
	ir.OpCmpNeI64: {Op: ir.OpCmpNeI64, Bytes: cmpStencil(0x95)},
	ir.OpCmpLtI64: {Op: ir.OpCmpLtI64, Bytes: cmpStencil(0x9C)},
	ir.OpCmpLeI64: {Op: ir.OpCmpLeI64, Bytes: cmpStencil(0x9E)},
	ir.OpCmpGtI64: {Op: ir.OpCmpGtI64, Bytes: cmpStencil(0x9F)},
	ir.OpCmpGeI64: {Op: ir.OpCmpGeI64, Bytes: cmpStencil(0x9D)},

	// Imm comparisons: cmp rax, imm32 (48 3D ii ii ii ii) plus the
	// setCC/movzx tail. The imm32 reloc lives at offset 2; the setCC
	// byte is at offset 7.

	ir.OpCmpEqI64Imm: {Op: ir.OpCmpEqI64Imm, Bytes: cmpImmStencil(0x94), Relocs: cmpImmReloc()},
	ir.OpCmpNeI64Imm: {Op: ir.OpCmpNeI64Imm, Bytes: cmpImmStencil(0x95), Relocs: cmpImmReloc()},
	ir.OpCmpLtI64Imm: {Op: ir.OpCmpLtI64Imm, Bytes: cmpImmStencil(0x9C), Relocs: cmpImmReloc()},
	ir.OpCmpLeI64Imm: {Op: ir.OpCmpLeI64Imm, Bytes: cmpImmStencil(0x9E), Relocs: cmpImmReloc()},
	ir.OpCmpGtI64Imm: {Op: ir.OpCmpGtI64Imm, Bytes: cmpImmStencil(0x9F), Relocs: cmpImmReloc()},
	ir.OpCmpGeI64Imm: {Op: ir.OpCmpGeI64Imm, Bytes: cmpImmStencil(0x9D), Relocs: cmpImmReloc()},

	// ----- Control flow ----------------------------------------------

	// ret. Registered under OpInvalid so TermReturn emission has a
	// fixed lookup key. The trampoline reads the result from RAX.
	ir.OpInvalid: {
		Op:    ir.OpInvalid,
		Bytes: []byte{0xC3},
	},
}

// cmpStencil constructs the 11-byte sequence for an i64 cmp + setCC +
// movzx. The setOpcode parameter selects the setCC variant.
//
//	48 39 F8         cmp rax, rdi
//	0F <setOp> C0    setCC al
//	48 0F B6 C0      movzx rax, al
func cmpStencil(setOpcode byte) []byte {
	return []byte{
		0x48, 0x39, 0xF8,
		0x0F, setOpcode, 0xC0,
		0x48, 0x0F, 0xB6, 0xC0,
	}
}

// cmpImmStencil constructs the 13-byte sequence for an i64 cmp imm +
// setCC + movzx.
//
//	48 3D ii ii ii ii  cmp rax, imm32
//	0F <setOp> C0      setCC al
//	48 0F B6 C0        movzx rax, al
func cmpImmStencil(setOpcode byte) []byte {
	return []byte{
		0x48, 0x3D, 0, 0, 0, 0,
		0x0F, setOpcode, 0xC0,
		0x48, 0x0F, 0xB6, 0xC0,
	}
}

// cmpImmReloc returns the per-cmp-imm-stencil reloc site list. The
// imm32 patch sits at offset 2.
func cmpImmReloc() []RelocSite {
	return []RelocSite{
		{Offset: 2, Kind: RelocImm32, Symbol: SymOpRetTarget, Addend: 0},
	}
}

// archStencils returns the stencil table for the host GOARCH. The
// build-tagged file ensures only one definition is linked into the
// final binary; non-amd64 builds get the stencils_other.go fallback.
func archStencils() map[ir.OpCode]Stencil {
	return stencilsAMD64
}
