//go:build amd64

package vm2jit

import (
	"fmt"

	"mochi/runtime/vm2"
)

// AMD64 GPR mapping: vm2 register r[i] -> the i-th callee-saved register.
// Using r12-r15, rbx, rbp, r10 (7 slots, matching the arm64 x9-x15 count).
// Encoding uses the REX.B / ModRM byte conventions for 64-bit ops.
//
// r[0]=rbx(3)  r[1]=rbp(5)  r[2]=r12(12)  r[3]=r13(13)
// r[4]=r14(14) r[5]=r15(15) r[6]=r10(10)
var amd64RegMap = [7]byte{3, 5, 12, 13, 14, 15, 10}

func amd64Reg(r int32) byte { return amd64RegMap[r] }

// rex64 returns the REX prefix for a 64-bit op with optional R, X, B extension.
func rex64(R, X, B bool) byte {
	v := byte(0x48)
	if R {
		v |= 0x04
	}
	if X {
		v |= 0x02
	}
	if B {
		v |= 0x01
	}
	return v
}

// ModRM encodes a ModRM byte (mod, reg, rm).
func modRM(mod, reg, rm byte) byte { return (mod << 6) | ((reg & 7) << 3) | (rm & 7) }

// lowerAMD64 lowers one vm2 Instr to AMD64 encoding (REX + opcode + ModRM).
// Returns raw bytes packed as little-endian uint32 words (padded to word boundary).
func lowerAMD64(_ *vm2.Function, _ int, ins vm2.Instr) ([]uint32, error) {
	// Phase 1 scope: arithmetic. Everything else is ErrNotImplemented.
	// The AMD64 backend emits bytes; we pack into uint32 words.
	var enc []byte
	rA, rB, rC := amd64Reg(ins.A), amd64Reg(ins.B), amd64Reg(ins.C)
	extA := rA >= 8
	extB := rB >= 8
	extC := rC >= 8

	switch ins.Op {
	case vm2.OpAddI64:
		// ADD rA, rC  (rA += rC). Canonical: MOV rA,rB then ADD rA,rC.
		enc = encADD64(rA, extA, rB, extB)
		enc = append(enc, encADD64op(rA, extA, rC, extC)...)
	case vm2.OpSubI64:
		enc = encMOV64(rA, extA, rB, extB)
		enc = append(enc, encSUB64(rA, extA, rC, extC)...)
	case vm2.OpMulI64:
		// IMUL rA, rB, rC: MOV rA,rB; IMUL rA,rC
		enc = encMOV64(rA, extA, rB, extB)
		enc = append(enc, encIMUL64(rA, extA, rC, extC)...)
	case vm2.OpLessI64:
		enc = encCMP64(rB, extB, rC, extC)
		enc = append(enc, encSETcc(0x9C, rA, extA)...) // SETL
	case vm2.OpLessEqI64:
		enc = encCMP64(rB, extB, rC, extC)
		enc = append(enc, encSETcc(0x9E, rA, extA)...) // SETLE
	case vm2.OpEqualI64:
		enc = encCMP64(rB, extB, rC, extC)
		enc = append(enc, encSETcc(0x94, rA, extA)...) // SETE
	case vm2.OpMove:
		enc = encMOV64(rA, extA, rB, extB)
	case vm2.OpReturn:
		enc = []byte{0xC3} // RET near
	default:
		return nil, fmt.Errorf("%w: %v on amd64", ErrNotImplemented, ins.Op)
	}
	return bytesToWords(enc), nil
}

// bytesToWords packs a byte slice (any length) into []uint32 little-endian,
// padding with zeros to the word boundary.
func bytesToWords(b []byte) []uint32 {
	n := (len(b) + 3) / 4
	ws := make([]uint32, n)
	for i, v := range b {
		ws[i/4] |= uint32(v) << (uint(i%4) * 8)
	}
	return ws
}

// --- AMD64 micro-encoders ---

// MOV r64, r/m64  (REX.W + 8B /r) — move rSrc into rDst.
func encMOV64(rDst byte, extDst bool, rSrc byte, extSrc bool) []byte {
	return []byte{
		rex64(extDst, false, extSrc),
		0x8B,
		modRM(3, rDst&7, rSrc&7),
	}
}

// ADD r64, r/m64  (move then add; this is the first MOV step).
func encADD64(rDst byte, extDst bool, rSrc byte, extSrc bool) []byte {
	return encMOV64(rDst, extDst, rSrc, extSrc)
}

// ADD r64, r/m64 (pure add opcode, REX.W + 03 /r).
func encADD64op(rDst byte, extDst bool, rSrc byte, extSrc bool) []byte {
	return []byte{
		rex64(extDst, false, extSrc),
		0x03,
		modRM(3, rDst&7, rSrc&7),
	}
}

// SUB r64, r/m64  (REX.W + 2B /r).
func encSUB64(rDst byte, extDst bool, rSrc byte, extSrc bool) []byte {
	return []byte{
		rex64(extDst, false, extSrc),
		0x2B,
		modRM(3, rDst&7, rSrc&7),
	}
}

// IMUL r64, r/m64  (REX.W + 0F AF /r).
func encIMUL64(rDst byte, extDst bool, rSrc byte, extSrc bool) []byte {
	return []byte{
		rex64(extDst, false, extSrc),
		0x0F, 0xAF,
		modRM(3, rDst&7, rSrc&7),
	}
}

// CMP r/m64, r64  (REX.W + 3B /r) — sets flags for rL cmp rR.
func encCMP64(rL byte, extL bool, rR byte, extR bool) []byte {
	return []byte{
		rex64(extL, false, extR),
		0x3B,
		modRM(3, rL&7, rR&7),
	}
}

// SETcc r/m8  (0F 9x /0) — sets rDst to 0 or 1 based on flags.
// cc is the opcode byte (e.g. 0x9C = SETL, 0x94 = SETE).
// Requires a REX prefix to access low byte of r8-r15.
func encSETcc(cc byte, rDst byte, extDst bool) []byte {
	var out []byte
	// Zero-extend: XOR rDst32, rDst32 first (avoids partial-reg stall).
	out = append(out, encXOR32(rDst, extDst)...)
	// REX is required to access SIL/DIL/BPL/SPL (register indices 4-7
	// without REX are AH/CH/DH/BH). Always emit REX for simplicity.
	rexB := byte(0x40)
	if extDst {
		rexB |= 0x01
	}
	out = append(out, rexB, 0x0F, cc, modRM(3, 0, rDst&7))
	return out
}

// XOR r32, r32  (33 /r, no REX) — clears rDst and its upper 32 bits.
func encXOR32(rDst byte, extDst bool) []byte {
	rexB := byte(0x40)
	if extDst {
		rexB |= 0x05 // REX.R + REX.B
	}
	return []byte{rexB, 0x33, modRM(3, rDst&7, rDst&7)}
}
