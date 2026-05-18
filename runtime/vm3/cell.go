package vm3

import "math"

// Cell is the 8-byte tagged value used throughout vm3. It is a strict
// NaN-box: floats occupy the full uint64 in their bit-pattern range;
// non-float values use the qNaN payload space for tag + payload.
//
// Bits layout (high 16 bits = tag, low 48 bits = payload):
//
//	0x0000..0xFFEF -> float64 (normal or subnormal). Decode via math.Float64frombits.
//	0x7FF8         -> canonical qNaN. Any NaN input normalizes here.
//	0xFFF8         -> tagDeopt  (JIT deopt sentinel; pc in low 48 bits).
//	0xFFF9         -> tagSStr   (inline short string; len in bits 40..43, up to 5 bytes in 0..39).
//	0xFFFA         -> tagInt48  (sign-extended 48-bit signed int in low 48 bits).
//	0xFFFB         -> tagBool   (low bit = value).
//	0xFFFC         -> tagNull   (no payload).
//	0xFFFD         -> reserved.
//	0xFFFE         -> reserved.
//	0xFFFF         -> tagHandle (arena handle; see encoding below).
//
// Handle payload (low 48 bits when tag is 0xFFFF):
//
//	bits 47..44 : arena selector (16 arenas max). See ArenaTag enum.
//	bits 43..32 : generation (12 bits, wraps; debug mode checks for stale handles).
//	bits 31..0  : slab index (32 bits, 4B entries per arena cap).
type Cell uint64

const (
	qNaN      uint64 = 0x7FF8_0000_0000_0000
	tagMask   uint64 = 0xFFFF_0000_0000_0000
	tagDeopt  uint64 = 0xFFF8_0000_0000_0000
	tagSStr   uint64 = 0xFFF9_0000_0000_0000
	tagInt48  uint64 = 0xFFFA_0000_0000_0000
	tagBool   uint64 = 0xFFFB_0000_0000_0000
	tagNull   uint64 = 0xFFFC_0000_0000_0000
	tagHandle uint64 = 0xFFFF_0000_0000_0000

	arenaSelShift uint64 = 44
	arenaSelMask  uint64 = uint64(0xF) << arenaSelShift
	genShift      uint64 = 32
	genMask       uint64 = uint64(0xFFF) << genShift
	idxMask       uint64 = 0xFFFF_FFFF

	payloadMask uint64 = 0x0000_FFFF_FFFF_FFFF

	MaxInlineStr           = 5
	sstrLenShift    uint64 = 40
	sstrLenMask     uint64 = 0xF
	sstrByteMask    uint64 = (uint64(1) << sstrLenShift) - 1

	MaxInlineInt int64 = 1<<47 - 1
	MinInlineInt int64 = -(1 << 47)
)

// ArenaTag selects which arena slab a handle Cell points into.
type ArenaTag uint8

const (
	ArenaString  ArenaTag = 0
	ArenaList    ArenaTag = 1
	ArenaMap     ArenaTag = 2
	ArenaSet     ArenaTag = 3
	ArenaStruct  ArenaTag = 4
	ArenaClosure ArenaTag = 5
	ArenaBignum  ArenaTag = 6
	ArenaBytes   ArenaTag = 7
	ArenaPair    ArenaTag = 8
	ArenaF64Arr  ArenaTag = 9
	ArenaI64Arr  ArenaTag = 10
	ArenaU8Arr   ArenaTag = 11
)

func CFloat(f float64) Cell {
	if f != f {
		return Cell(qNaN)
	}
	return Cell(math.Float64bits(f))
}

func CInt(i int64) Cell {
	return Cell(tagInt48 | uint64(i)&payloadMask)
}

func CBool(b bool) Cell {
	if b {
		return Cell(tagBool | 1)
	}
	return Cell(tagBool)
}

func CNull() Cell { return Cell(tagNull) }

func CSStr(b []byte) Cell {
	var packed uint64
	n := min(len(b), MaxInlineStr)
	for i := range n {
		packed |= uint64(b[i]) << (uint(i) * 8)
	}
	return Cell(tagSStr | (uint64(n) << sstrLenShift) | packed)
}

func (c Cell) IsFloat() bool  { return uint64(c)&tagMask < tagSStr }
func (c Cell) IsSStr() bool   { return uint64(c)&tagMask == tagSStr }
func (c Cell) IsInt() bool    { return uint64(c)&tagMask == tagInt48 }
func (c Cell) IsBool() bool   { return uint64(c)&tagMask == tagBool }
func (c Cell) IsNull() bool   { return uint64(c)&tagMask == tagNull }
func (c Cell) IsHandle() bool { return uint64(c)&tagMask == tagHandle }
func (c Cell) IsDeopt() bool  { return uint64(c)&tagMask == tagDeopt }

func (c Cell) Float() float64 { return math.Float64frombits(uint64(c)) }
func (c Cell) Int() int64     { return int64(uint64(c)<<16) >> 16 }
func (c Cell) Bool() bool     { return uint64(c)&1 != 0 }

// SStrLen returns the byte length of an inline string Cell. Caller must
// have verified IsSStr.
func (c Cell) SStrLen() int {
	return int((uint64(c) >> sstrLenShift) & sstrLenMask)
}

// SStrBytes writes the inline string bytes into buf and returns a subslice.
func (c Cell) SStrBytes(buf *[MaxInlineStr]byte) []byte {
	n := c.SStrLen()
	packed := uint64(c) & sstrByteMask
	for i := range n {
		buf[i] = byte(packed >> (uint(i) * 8))
	}
	return buf[:n]
}

// MakeHandle packs (tag, gen, idx) into a handle Cell.
func MakeHandle(tag ArenaTag, gen uint16, idx uint32) Cell {
	return Cell(tagHandle |
		(uint64(tag) << arenaSelShift) |
		(uint64(gen&0xFFF) << genShift) |
		uint64(idx))
}

// DecodeHandle splits a handle Cell into (tag, gen, idx). Caller must
// have verified IsHandle.
func (c Cell) DecodeHandle() (tag ArenaTag, gen uint16, idx uint32) {
	p := uint64(c) & payloadMask
	tag = ArenaTag((p & arenaSelMask) >> arenaSelShift)
	gen = uint16((p & genMask) >> genShift)
	idx = uint32(p & idxMask)
	return
}

// EncodeDeopt produces the sentinel Cell returned by a JIT deopt stub.
func EncodeDeopt(pc int) Cell {
	return Cell(tagDeopt | uint64(pc)&payloadMask)
}

// DecodeDeopt reports whether c is a deopt sentinel and, if so, returns
// the carried bytecode PC.
func DecodeDeopt(c Cell) (pc int, ok bool) {
	if uint64(c)&tagMask != tagDeopt {
		return 0, false
	}
	return int(uint64(c) & payloadMask), true
}

// FitsInline reports whether i can be boxed inline by CInt without loss.
func FitsInline(i int64) bool {
	return i >= MinInlineInt && i <= MaxInlineInt
}
