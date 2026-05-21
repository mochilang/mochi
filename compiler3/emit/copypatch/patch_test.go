package copypatch

import (
	"encoding/binary"
	"testing"
)

// TestApplyRelocsImm32 covers the 32-bit immediate path. The symbol
// value is written verbatim at the patch offset; subsequent bytes
// must be left untouched.
func TestApplyRelocsImm32(t *testing.T) {
	dst := make([]byte, 16)
	dst[4] = 0xAA // sentinel that must not be touched
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm32, Symbol: SymArenaBase}}
	var st SymbolTable
	st.Set(SymArenaBase, 0x12345678)
	if err := applyRelocs(dst, 0, relocs, &st); err != nil {
		t.Fatalf("applyRelocs error: %v", err)
	}
	if got := binary.LittleEndian.Uint32(dst[:4]); got != 0x12345678 {
		t.Errorf("imm32 at offset 0 = 0x%x, want 0x12345678", got)
	}
	if dst[4] != 0xAA {
		t.Errorf("sentinel byte at offset 4 = 0x%x, want 0xAA", dst[4])
	}
}

// TestApplyRelocsImm64 covers the 64-bit immediate path.
func TestApplyRelocsImm64(t *testing.T) {
	dst := make([]byte, 16)
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm64, Symbol: SymArenaBase}}
	var st SymbolTable
	st.Set(SymArenaBase, 0x1122334455667788)
	if err := applyRelocs(dst, 0, relocs, &st); err != nil {
		t.Fatalf("applyRelocs error: %v", err)
	}
	if got := binary.LittleEndian.Uint64(dst[:8]); got != 0x1122334455667788 {
		t.Errorf("imm64 = 0x%x, want 0x1122334455667788", got)
	}
}

// TestApplyRelocsImm64Addend covers the OpConst path: the literal
// flows through the Addend field. The patcher computes
// (addr + Addend); when addr is zero the Addend appears verbatim.
func TestApplyRelocsImm64Addend(t *testing.T) {
	dst := make([]byte, 8)
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm64, Symbol: SymOpRetTarget, Addend: 42}}
	var st SymbolTable
	st.Set(SymOpRetTarget, 0) // addr = 0; Addend supplies the literal
	// Bind a sentinel zero so applyRelocs treats the symbol as bound.
	// (Get returns 0 when unset, which the patcher rejects; we need
	// a non-default Set to mark "intentionally zero".)
	// The patcher rejects addr == 0 unconditionally, so the Phase 1.1
	// real path will introduce SymImmI64 and skip the Get check for
	// it. For this test we bind a non-zero sentinel and subtract it
	// out via Addend.
	st.Set(SymOpRetTarget, 1000)
	relocs[0].Addend = -958 // (1000 + -958) = 42
	if err := applyRelocs(dst, 0, relocs, &st); err != nil {
		t.Fatalf("applyRelocs error: %v", err)
	}
	if got := binary.LittleEndian.Uint64(dst); got != 42 {
		t.Errorf("imm64 with addend = 0x%x, want 42", got)
	}
}

// TestApplyRelocsPCRel32 covers the pc-relative call/jmp shape: the
// written value is target - (siteAddr + 4). Tests both a forward and
// a backward branch.
func TestApplyRelocsPCRel32(t *testing.T) {
	cases := []struct {
		name    string
		baseAdd uintptr
		offset  uint32
		target  uintptr
		want    int32
	}{
		{"forward branch", 0x10000, 0, 0x10100, 0x100 - 4},
		{"backward branch", 0x10100, 0, 0x10000, -(0x100 + 4)},
		{"zero displacement", 0x10000, 0, 0x10004, 0},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			dst := make([]byte, 8)
			relocs := []RelocSite{{Offset: c.offset, Kind: RelocPCRel32, Symbol: SymSlowPathDeref}}
			var st SymbolTable
			st.Set(SymSlowPathDeref, c.target)
			if err := applyRelocs(dst, c.baseAdd, relocs, &st); err != nil {
				t.Fatalf("applyRelocs error: %v", err)
			}
			got := int32(binary.LittleEndian.Uint32(dst[c.offset : c.offset+4]))
			if got != c.want {
				t.Errorf("pcrel32 = %d, want %d", got, c.want)
			}
		})
	}
}

// TestApplyRelocsPCRel32OutOfRange checks the ±2 GiB displacement
// guard. The patcher refuses to silently truncate a target that is
// out of range; the emitter must instead route through an Abs64
// indirect call (Phase 1.4).
func TestApplyRelocsPCRel32OutOfRange(t *testing.T) {
	dst := make([]byte, 8)
	relocs := []RelocSite{{Offset: 0, Kind: RelocPCRel32, Symbol: SymSlowPathDeref}}
	var st SymbolTable
	// Target is way out of ±2 GiB from siteAddr = 0.
	st.Set(SymSlowPathDeref, 0x4_0000_0000)
	err := applyRelocs(dst, 0, relocs, &st)
	if err == nil {
		t.Fatalf("expected out-of-range error, got nil")
	}
	if !contains(err.Error(), "out of 32-bit signed range") {
		t.Errorf("error %q does not mention range", err.Error())
	}
}

// TestApplyRelocsAbs64 covers the 64-bit absolute path.
func TestApplyRelocsAbs64(t *testing.T) {
	dst := make([]byte, 16)
	relocs := []RelocSite{{Offset: 0, Kind: RelocAbs64, Symbol: SymVMCtx}}
	var st SymbolTable
	st.Set(SymVMCtx, 0xCAFE_BABE_DEAD_BEEF)
	if err := applyRelocs(dst, 0, relocs, &st); err != nil {
		t.Fatalf("applyRelocs error: %v", err)
	}
	if got := binary.LittleEndian.Uint64(dst[:8]); got != 0xCAFE_BABE_DEAD_BEEF {
		t.Errorf("abs64 = 0x%x, want 0xCAFEBABEDEADBEEF", got)
	}
}

// TestApplyRelocsUnboundSymbol covers the safety guard: a stencil
// that references a symbol the emitter forgot to bind must be
// rejected, not silently patched with zero.
func TestApplyRelocsUnboundSymbol(t *testing.T) {
	dst := make([]byte, 4)
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm32, Symbol: SymArenaBase}}
	var st SymbolTable
	err := applyRelocs(dst, 0, relocs, &st)
	if err == nil {
		t.Fatalf("expected unbound-symbol error, got nil")
	}
	if !contains(err.Error(), "unbound symbol") {
		t.Errorf("error %q does not mention unbound symbol", err.Error())
	}
}

// TestApplyRelocsNilTable covers the nil-table guard.
func TestApplyRelocsNilTable(t *testing.T) {
	dst := make([]byte, 4)
	err := applyRelocs(dst, 0, []RelocSite{{Offset: 0, Kind: RelocImm32, Symbol: SymArenaBase}}, nil)
	if err == nil {
		t.Fatalf("expected nil-SymbolTable error, got nil")
	}
}

// TestApplyRelocsOutOfBounds covers the per-reloc bounds check inside
// writeReloc. validate() catches this at stencil-load time, but the
// patcher must also catch it in case the table was corrupted in
// memory between load and patch.
func TestApplyRelocsOutOfBounds(t *testing.T) {
	dst := make([]byte, 4)
	// imm64 reloc at offset 0 would extend to offset 8; dst is 4.
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm64, Symbol: SymArenaBase}}
	var st SymbolTable
	st.Set(SymArenaBase, 1)
	err := applyRelocs(dst, 0, relocs, &st)
	if err == nil {
		t.Fatalf("expected out-of-bounds error, got nil")
	}
}

// TestApplyRelocsImm32Truncation covers the imm32 "value does not fit"
// guard. A symbol whose value is between the 32-bit signed and
// unsigned ranges is still rejected because the assembler emitter
// cannot tell which sign-extension shape is intended.
func TestApplyRelocsImm32Truncation(t *testing.T) {
	dst := make([]byte, 4)
	relocs := []RelocSite{{Offset: 0, Kind: RelocImm32, Symbol: SymArenaBase}}
	var st SymbolTable
	// 0x1_FFFF_FFFF is > uint32 max and != sign-extended int32.
	st.Set(SymArenaBase, 0x1_FFFF_FFFF)
	err := applyRelocs(dst, 0, relocs, &st)
	if err == nil {
		t.Fatalf("expected imm32 truncation error, got nil")
	}
	if !contains(err.Error(), "does not fit in 32 bits") {
		t.Errorf("error %q does not mention 32-bit fit", err.Error())
	}
}
