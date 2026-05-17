package vm2

import "math"

// Cell is the 8-byte tagged value used by vm2 throughout. It is
// NaN-boxed in the style of LuaJIT / JavaScriptCore: a Cell is either a
// raw IEEE 754 float64 (non-canonicalized NaNs are normalized on
// construction), or one of four tagged forms whose high 16 bits live
// inside the qNaN-with-sign-bit range so they cannot collide with a
// real float bit pattern.
//
// Tag layout (high 16 bits):
//
//	0x0000..0xFFEF  -> float64 (normal or subnormal)
//	0x7FF8          -> canonical qNaN, treated as float
//	0xFFFB          -> inline short string (length in payload bits 40..47, up to 5 bytes in 0..39)
//	0xFFFC          -> int (low 48 bits, signed)
//	0xFFFD          -> bool (low bit)
//	0xFFFE          -> null
//	0xFFFF          -> ptr index into VM.Objects (low 48 bits, unsigned)
//
// Int is 48-bit signed (range -2^47 .. 2^47-1). Wider ints are boxed
// into the Objects table by the compiler; see CPtr.
type Cell uint64

const (
	qNaN    uint64 = 0x7FF8000000000000
	tagMask uint64 = 0xFFFF000000000000
	// tagDeopt is the JIT deopt sentinel. The low 48 bits hold the
	// bytecode PC at which the interpreter should resume. Deopt cells
	// only exist for the few nanoseconds between a JIT epilogue and the
	// wrapper's check; they are never stored in regs or seen by user
	// code. The tag value is chosen below tagSStr so it stays inside the
	// NaN-box range without colliding with any user-visible type.
	tagDeopt uint64 = 0xFFFA000000000000
	tagSStr  uint64 = 0xFFFB000000000000
	tagInt   uint64 = 0xFFFC000000000000
	tagBool  uint64 = 0xFFFD000000000000
	tagNull  uint64 = 0xFFFE000000000000
	tagPtr   uint64 = 0xFFFF000000000000

	payloadMask uint64 = 0x0000FFFFFFFFFFFF

	// MaxInlineStr is the maximum byte length packable as tagSStr. With
	// 48 payload bits, 8 bits hold the length and 40 bits hold up to 5
	// raw bytes packed little-endian.
	MaxInlineStr = 5
	sstrLenShift = 40
	sstrLenMask  = 0xFF
	sstrByteMask = uint64(1)<<sstrLenShift - 1

	// MaxInlineInt and MinInlineInt are the inclusive bounds of an int
	// that fits in a Cell without boxing into Objects.
	MaxInlineInt int64 = 1<<47 - 1
	MinInlineInt int64 = -(1 << 47)
)

// CFloat boxes a float64. NaN inputs are canonicalized so they read
// back as IsFloat.
func CFloat(f float64) Cell {
	if f != f { // any NaN -> canonical qNaN
		return Cell(qNaN)
	}
	return Cell(math.Float64bits(f))
}

// CInt boxes a 48-bit signed int. Callers that may overflow must check
// FitsInline first; out-of-range values are silently truncated here.
func CInt(i int64) Cell {
	return Cell(tagInt | uint64(i)&payloadMask)
}

func CBool(b bool) Cell {
	if b {
		return Cell(tagBool | 1)
	}
	return Cell(tagBool)
}

func CNull() Cell { return Cell(tagNull) }

// CPtr boxes a 48-bit unsigned index into the owning VM's Objects table.
func CPtr(idx uint64) Cell {
	return Cell(tagPtr | idx&payloadMask)
}

func (c Cell) IsFloat() bool { return uint64(c)&tagMask < tagSStr }
func (c Cell) IsSStr() bool  { return uint64(c)&tagMask == tagSStr }
func (c Cell) IsInt() bool   { return uint64(c)&tagMask == tagInt }
func (c Cell) IsBool() bool  { return uint64(c)&tagMask == tagBool }
func (c Cell) IsNull() bool  { return uint64(c)&tagMask == tagNull }
func (c Cell) IsPtr() bool   { return uint64(c)&tagMask == tagPtr }

// IsStr reports whether c carries a string value (inline or heap).
func (c Cell) IsStr() bool {
	t := uint64(c) & tagMask
	return t == tagSStr || t == tagPtr
}

// CSStr packs up to MaxInlineStr bytes into an inline string Cell.
// The caller must ensure len(b) <= MaxInlineStr.
func CSStr(b []byte) Cell {
	var packed uint64
	for i, x := range b {
		packed |= uint64(x) << (uint(i) * 8)
	}
	return Cell(tagSStr | uint64(len(b))<<sstrLenShift | packed)
}

// SStrLen returns the byte length of an inline string Cell. Caller
// must have verified IsSStr.
func (c Cell) SStrLen() int {
	return int((uint64(c) >> sstrLenShift) & sstrLenMask)
}

// SStrBytes writes the inline string bytes into buf and returns a
// subslice. Caller must have verified IsSStr; buf must have room for
// MaxInlineStr bytes.
func (c Cell) SStrBytes(buf *[MaxInlineStr]byte) []byte {
	n := c.SStrLen()
	packed := uint64(c) & sstrByteMask
	for i := range n {
		buf[i] = byte(packed >> (uint(i) * 8))
	}
	return buf[:n]
}

func (c Cell) Float() float64 { return math.Float64frombits(uint64(c)) }

// Int decodes the 48-bit signed payload by sign-extending through a
// left-then-arithmetic-right shift.
func (c Cell) Int() int64 { return int64(c<<16) >> 16 }

func (c Cell) Bool() bool { return uint64(c)&1 != 0 }

// Ptr returns the 48-bit index into VM.Objects.
func (c Cell) Ptr() uint64 { return uint64(c) & payloadMask }

// FitsInline reports whether i can be boxed inline by CInt without
// loss. Compilers should box wider ints through the Objects table.
func FitsInline(i int64) bool {
	return i >= MinInlineInt && i <= MaxInlineInt
}

// EncodeDeopt produces the sentinel Cell returned by a JIT deopt stub.
// pc is the bytecode index at which the interpreter should resume; the
// low 48 bits of the Cell carry pc unchanged. Used by the vm2jit ARM64
// code generator when emitting a deopt point.
func EncodeDeopt(pc int) Cell {
	return Cell(tagDeopt | uint64(pc)&payloadMask)
}

// DecodeDeopt reports whether c is a deopt sentinel and, if so, returns
// the carried bytecode PC. Used by the vm2jit wrapper to distinguish a
// JIT deopt return from an OpReturn return value.
func DecodeDeopt(c Cell) (pc int, ok bool) {
	if uint64(c)&tagMask != tagDeopt {
		return 0, false
	}
	return int(uint64(c) & payloadMask), true
}
