package copypatch

import (
	"encoding/binary"
	"fmt"
)

// applyRelocs walks relocs and writes each patch site into dst at the
// given baseAddr. baseAddr is the runtime address dst will be
// executed from (the RX-mapped page); for RelocPCRel32 the patcher
// uses baseAddr + Offset + 4 as the "next-instruction PC" the
// displacement is relative to.
//
// dst must be sized to hold the stencil's Bytes plus enough headroom
// for every reloc to write its full width. validate() on the stencil
// already proves this at table-load time; applyRelocs re-checks at
// patch time so a corrupted in-memory stencil table cannot drive an
// out-of-bounds write.
//
// On any error, dst may have been partially written; the caller must
// discard the buffer (the cache.go path achieves this by allocating
// a fresh slab on each emit).
func applyRelocs(dst []byte, baseAddr uintptr, relocs []RelocSite, syms *SymbolTable) error {
	if syms == nil {
		return fmt.Errorf("copypatch.applyRelocs: nil SymbolTable")
	}
	for i, r := range relocs {
		addr := syms.Get(r.Symbol)
		if addr == 0 {
			return fmt.Errorf("copypatch.applyRelocs: reloc #%d (kind=%s) references unbound symbol %s",
				i, r.Kind, r.Symbol)
		}
		if err := writeReloc(dst, baseAddr, r, addr); err != nil {
			return fmt.Errorf("copypatch.applyRelocs: reloc #%d (kind=%s symbol=%s): %w",
				i, r.Kind, r.Symbol, err)
		}
	}
	return nil
}

// writeReloc applies a single relocation site. Centralized here so the
// per-RelocKind encoding lives in one place and the per-reloc bounds
// check is consistent.
func writeReloc(dst []byte, baseAddr uintptr, r RelocSite, addr uintptr) error {
	width := relocWidth(r.Kind)
	end := uint64(r.Offset) + uint64(width)
	if end > uint64(len(dst)) {
		return fmt.Errorf("offset %d + width %d > dst len %d", r.Offset, width, len(dst))
	}
	slot := dst[r.Offset : r.Offset+width]
	switch r.Kind {
	case RelocImm32:
		// 32-bit little-endian immediate. Addend is folded in; the
		// final value is (addr + Addend) truncated to 32 bits.
		v := uint64(int64(addr) + int64(r.Addend))
		// Defensive: if the symbol value does not fit in 32 bits and
		// the patch shape is Imm32, the caller picked the wrong
		// reloc; refuse to silently truncate.
		if uint64(uint32(v)) != v && int64(int32(v)) != int64(v) {
			return fmt.Errorf("imm32 value 0x%x does not fit in 32 bits", v)
		}
		binary.LittleEndian.PutUint32(slot, uint32(v))
	case RelocImm64:
		v := uint64(int64(addr) + int64(r.Addend))
		binary.LittleEndian.PutUint64(slot, v)
	case RelocPCRel32:
		// pc-rel32: written value = target - (siteAddr + 4).
		// siteAddr = baseAddr + Offset.
		siteAddr := uint64(baseAddr) + uint64(r.Offset)
		target := uint64(int64(addr) + int64(r.Addend))
		disp := int64(target) - int64(siteAddr+4)
		if disp < int64(int32(-1<<31)) || disp > int64(int32(1<<31-1)) {
			return fmt.Errorf("pcrel32 displacement %d out of 32-bit signed range (target=0x%x site=0x%x)",
				disp, target, siteAddr)
		}
		binary.LittleEndian.PutUint32(slot, uint32(int32(disp)))
	case RelocAbs64:
		v := uint64(int64(addr) + int64(r.Addend))
		binary.LittleEndian.PutUint64(slot, v)
	default:
		return fmt.Errorf("unknown reloc kind %s", r.Kind)
	}
	return nil
}
