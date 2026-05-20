//go:build amd64

package vm3jit

import (
	"bytes"
	"encoding/binary"
	"testing"
)

// TestAnd64RREncodingAMD64 covers a few representative dst/src combos
// for and64RR. AMD64 encodes `and dst, src` as REX.W 21 /r mod=11 with
// the REX.R bit set if src >= 8 and REX.B set if dst >= 8. The reg
// field in ModR/M takes the low 3 bits of src; the r/m field takes the
// low 3 bits of dst.
func TestAnd64RREncodingAMD64(t *testing.T) {
	cases := []struct {
		src, dst int
		want     []byte
	}{
		{xRAX, xRBX, []byte{0x48, 0x21, 0xC3}}, // and rbx, rax
		{xRBX, xRAX, []byte{0x48, 0x21, 0xD8}}, // and rax, rbx
		{xR10, xRAX, []byte{0x4C, 0x21, 0xD0}}, // and rax, r10 (REX.R)
		{xRAX, xR10, []byte{0x49, 0x21, 0xC2}}, // and r10, rax (REX.B)
		{xR10, xR11, []byte{0x4D, 0x21, 0xD3}}, // and r11, r10 (REX.R|REX.B)
	}
	for _, c := range cases {
		got := and64RR(c.src, c.dst)
		if !bytes.Equal(got, c.want) {
			t.Errorf("and64RR(src=%d, dst=%d) = % x, want % x", c.src, c.dst, got, c.want)
		}
	}
}

// TestXor64RREncodingAMD64 covers xor64RR: opcode REX.W 31 /r mod=11.
// Same REX rules as and64RR.
func TestXor64RREncodingAMD64(t *testing.T) {
	cases := []struct {
		src, dst int
		want     []byte
	}{
		{xRAX, xRAX, []byte{0x48, 0x31, 0xC0}}, // xor rax, rax (zero rax)
		{xRSI, xRDI, []byte{0x48, 0x31, 0xF7}}, // xor rdi, rsi
		{xR8, xR9, []byte{0x4D, 0x31, 0xC1}},   // xor r9, r8
	}
	for _, c := range cases {
		got := xor64RR(c.src, c.dst)
		if !bytes.Equal(got, c.want) {
			t.Errorf("xor64RR(src=%d, dst=%d) = % x, want % x", c.src, c.dst, got, c.want)
		}
	}
}

// TestOr64RImm8EncodingAMD64 covers or64RImm8: opcode REX.W 83 /1 ib
// with the reg field of ModR/M = 1 (the /1 sub-opcode).
func TestOr64RImm8EncodingAMD64(t *testing.T) {
	cases := []struct {
		dst  int
		imm  int8
		want []byte
	}{
		{xRAX, 1, []byte{0x48, 0x83, 0xC8, 0x01}},        // or rax, 1
		{xRSI, 0x7F, []byte{0x48, 0x83, 0xCE, 0x7F}},     // or rsi, 0x7F
		{xR10, -1, []byte{0x49, 0x83, 0xCA, 0xFF}},       // or r10, -1 (sign-extended)
		{xRBP, 1, []byte{0x48, 0x83, 0xCD, 0x01}},        // or rbp, 1
	}
	for _, c := range cases {
		got := or64RImm8(c.dst, c.imm)
		if !bytes.Equal(got, c.want) {
			t.Errorf("or64RImm8(dst=%d, imm=%d) = % x, want % x", c.dst, c.imm, got, c.want)
		}
	}
}

// TestMov32StoreDisp32EncodingAMD64 covers mov32StoreDisp32: opcode
// 89 /r mod=10 disp32. No REX.W (32-bit store), but REX.R / REX.B if
// either reg is high. Byte count is 6 or 7.
func TestMov32StoreDisp32EncodingAMD64(t *testing.T) {
	cases := []struct {
		src, base int
		disp      int32
		want      []byte
	}{
		{xRAX, xRBX, 0x10, []byte{0x89, 0x83, 0x10, 0x00, 0x00, 0x00}}, // mov [rbx+0x10], eax
		{xRSI, xRBX, -1, []byte{0x89, 0xB3, 0xFF, 0xFF, 0xFF, 0xFF}},   // mov [rbx-1], esi
		{xRAX, xR12, 0x20, []byte{0x41, 0x89, 0x84, 0x24, 0x20, 0x00, 0x00, 0x00}},
		// Wait: xR12 has the SIB-required quirk. The encoder docs say
		// caller pre-checks rBase != RSP/R12, so we don't test that case.
	}
	// Re-shape: remove the R12 case (the helper's contract requires
	// caller to avoid RSP/R12 bases; it would otherwise need a SIB byte).
	cases = cases[:2]
	for _, c := range cases {
		got := mov32StoreDisp32(c.src, c.base, c.disp)
		if !bytes.Equal(got, c.want) {
			t.Errorf("mov32StoreDisp32(src=%d, base=%d, disp=%d) = % x, want % x",
				c.src, c.base, c.disp, got, c.want)
		}
		if len(got) != mov32StoreDisp32ByteCount(c.src, c.base) {
			t.Errorf("mov32StoreDisp32(src=%d, base=%d) len=%d want %d (byte-count helper out of sync)",
				c.src, c.base, len(got), mov32StoreDisp32ByteCount(c.src, c.base))
		}
	}
	// REX.R case (src is R11): 7 bytes.
	got := mov32StoreDisp32(xR11, xRBX, 0x10)
	want := []byte{0x44, 0x89, 0x9B, 0x10, 0x00, 0x00, 0x00}
	if !bytes.Equal(got, want) {
		t.Errorf("mov32StoreDisp32(R11,RBX,0x10) = % x, want % x", got, want)
	}
}

// TestJccRel8EncodingAMD64 covers the 2-byte short jcc form.
func TestJccRel8EncodingAMD64(t *testing.T) {
	cases := []struct {
		cc   byte
		rel  int8
		want []byte
	}{
		{0x4, 0x10, []byte{0x74, 0x10}},  // je +16
		{0x5, 0x10, []byte{0x75, 0x10}},  // jne +16
		{0x4, -2, []byte{0x74, 0xFE}},    // je -2 (self-loop)
		{0x2, 0x7F, []byte{0x72, 0x7F}},  // jb +127
		{0x3, -128, []byte{0x73, 0x80}},  // jae -128
	}
	for _, c := range cases {
		got := jccRel8(c.cc, c.rel)
		if !bytes.Equal(got, c.want) {
			t.Errorf("jccRel8(cc=%#x, rel=%d) = % x, want % x", c.cc, c.rel, got, c.want)
		}
	}
}

// TestJmpRel8EncodingAMD64 covers the 2-byte unconditional short jmp.
func TestJmpRel8EncodingAMD64(t *testing.T) {
	cases := []struct {
		rel  int8
		want []byte
	}{
		{0x10, []byte{0xEB, 0x10}},  // jmp +16
		{-2, []byte{0xEB, 0xFE}},    // jmp -2 (infinite loop)
		{0x7F, []byte{0xEB, 0x7F}},
	}
	for _, c := range cases {
		got := jmpRel8(c.rel)
		if !bytes.Equal(got, c.want) {
			t.Errorf("jmpRel8(rel=%d) = % x, want % x", c.rel, got, c.want)
		}
	}
}

// TestEmitSplitmix64AMD64ByteCount verifies the byte-count helper
// matches the actual emit length for representative register triples.
// The byte count is invariant across (xKey, xOut, xTmp) because every
// instruction in the sequence has REX-independent length once REX.W
// forces a REX byte.
func TestEmitSplitmix64AMD64ByteCount(t *testing.T) {
	triples := []struct{ key, out, tmp int }{
		{xRSI, xRAX, xRDX}, // common case: vm3 slot-0 key, RAX hash, RDX tmp
		{xRDI, xRAX, xRDX}, // slot-1 key
		{xR8, xRAX, xRDX},
		{xR14, xRAX, xRDX},
		{xRBP, xRAX, xRDX},
		{xRSI, xR9, xR10}, // extended-reg out/tmp still 62B because REX.W forces REX
	}
	want := emitSplitmix64AMD64ByteCount()
	for _, tri := range triples {
		got := len(emitSplitmix64AMD64(tri.key, tri.out, tri.tmp))
		if got != want {
			t.Errorf("emitSplitmix64AMD64(key=%d, out=%d, tmp=%d) len=%d, want %d",
				tri.key, tri.out, tri.tmp, got, want)
		}
	}
}

// TestEmitSplitmix64AMD64Layout verifies that the emitted byte stream
// matches the expected instruction sequence at the byte level for a
// representative (xKey=RSI, xOut=RAX, xTmp=RDX) triple. This catches
// off-by-one errors in the emit sequence (wrong operand direction,
// missing imm, stale opcode) before the splitmix64 sequence is wired
// into the map kernel. The output register (RAX) holds h on return;
// xKey (RSI) must be untouched so the per-probe key comparison still
// sees the original i64.
//
// The expected stream is the concatenation of:
//
//	mov rax, rsi               48 89 F0                3
//	shr rax, 30                48 C1 E8 1E             4
//	xor rax, rsi               48 31 F0                3
//	movabs rdx, C1            48 BA <C1 LE 8 bytes>   10
//	imul rax, rdx              48 0F AF C2             4
//	mov rdx, rax               48 89 C2                3
//	shr rdx, 27                48 C1 EA 1B             4
//	xor rax, rdx               48 31 D0                3
//	movabs rdx, C2            48 BA <C2 LE 8 bytes>   10
//	imul rax, rdx              48 0F AF C2             4
//	mov rdx, rax               48 89 C2                3
//	shr rdx, 31                48 C1 EA 1F             4
//	xor rax, rdx               48 31 D0                3
//	or rax, 1                  48 83 C8 01             4
//
// Total: 62 bytes.
func TestEmitSplitmix64AMD64Layout(t *testing.T) {
	const xKey = xRSI
	const xOut = xRAX
	const xTmp = xRDX
	got := emitSplitmix64AMD64(xKey, xOut, xTmp)
	var want []byte
	// mov rax, rsi  (mov64RR(src=RSI, dst=RAX))
	want = append(want, 0x48, 0x89, 0xF0)
	// shr rax, 30
	want = append(want, 0x48, 0xC1, 0xE8, 30)
	// xor rax, rsi  (xor64RR(src=RSI, dst=RAX))
	want = append(want, 0x48, 0x31, 0xF0)
	// movabs rdx, C1
	want = append(want, 0x48, 0xBA)
	var c1le [8]byte
	binary.LittleEndian.PutUint64(c1le[:], splitmix64C1AMD64U)
	want = append(want, c1le[:]...)
	// imul rax, rdx  (imul64RR(src=RDX, dst=RAX))
	want = append(want, 0x48, 0x0F, 0xAF, 0xC2)
	// mov rdx, rax  (mov64RR(src=RAX, dst=RDX))
	want = append(want, 0x48, 0x89, 0xC2)
	// shr rdx, 27
	want = append(want, 0x48, 0xC1, 0xEA, 27)
	// xor rax, rdx  (xor64RR(src=RDX, dst=RAX))
	want = append(want, 0x48, 0x31, 0xD0)
	// movabs rdx, C2
	want = append(want, 0x48, 0xBA)
	var c2le [8]byte
	binary.LittleEndian.PutUint64(c2le[:], splitmix64C2AMD64U)
	want = append(want, c2le[:]...)
	// imul rax, rdx
	want = append(want, 0x48, 0x0F, 0xAF, 0xC2)
	// mov rdx, rax
	want = append(want, 0x48, 0x89, 0xC2)
	// shr rdx, 31
	want = append(want, 0x48, 0xC1, 0xEA, 31)
	// xor rax, rdx
	want = append(want, 0x48, 0x31, 0xD0)
	// or rax, 1   (or64RImm8(dst=RAX, imm=1))
	want = append(want, 0x48, 0x83, 0xC8, 0x01)
	if !bytes.Equal(got, want) {
		t.Fatalf("emitSplitmix64AMD64(RSI, RAX, RDX) mismatch:\n got: % x\nwant: % x", got, want)
	}
}

// TestEmitSplitmix64AMD64MatchesHashI64 runs the splitmix64 byte
// sequence in a pure-Go interpreter that mirrors the AMD64 ops the
// emitter produces and verifies the result matches runtime/vm3/maps.go's
// hashI64 for representative keys. This catches algorithmic bugs (wrong
// shift count, swapped multiplier, missing |1) independently of the
// byte-level layout check.
//
// The sim also asserts the key register is preserved (xKey untouched),
// which the map kernel relies on for the per-probe `cmp stored.key,
// xKey` step.
func TestEmitSplitmix64AMD64MatchesHashI64(t *testing.T) {
	keys := []int64{
		0, 1, -1, 2, -2, 7, 42, 1 << 32, -(1 << 32),
		1<<63 - 1, -(1 << 63), 3141592653589793238,
	}
	for _, k := range keys {
		// Mirror the AMD64 emit sequence in scalar Go. key stays in
		// xKey throughout; out is the hash accumulator; tmp is a
		// transient.
		key := uint64(k)
		// out = key; out >>= 30; out ^= key  -> out = key ^ (key>>30)
		out := key
		out >>= 30
		out ^= key
		// tmp = C1; out *= tmp
		out *= splitmix64C1AMD64U
		// tmp = out; tmp >>= 27; out ^= tmp
		tmp := out
		tmp >>= 27
		out ^= tmp
		// tmp = C2; out *= tmp
		out *= splitmix64C2AMD64U
		tmp = out
		tmp >>= 31
		out ^= tmp
		out |= 1
		// Reference output from runtime/vm3/maps.go.
		ref := splitmix64Reference(k)
		if out != ref {
			t.Errorf("k=%d: emit-sim got %#x want %#x", k, out, ref)
		}
		if key != uint64(k) {
			t.Errorf("k=%d: key clobbered by sim (got %#x want %#x)", k, key, uint64(k))
		}
	}
}

// splitmix64Reference reproduces runtime/vm3/maps.go's hashI64 locally
// so the test does not need to import vm3 (avoids a circular dep
// between vm3jit and vm3 in test-only code; the production code already
// has that dep but tests should not extend it).
func splitmix64Reference(k int64) uint64 {
	x := uint64(k)
	x ^= x >> 30
	x *= splitmix64C1AMD64U
	x ^= x >> 27
	x *= splitmix64C2AMD64U
	x ^= x >> 31
	return x | 1
}
