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
	tagInt  uint64 = 0xFFFC000000000000
	tagBool uint64 = 0xFFFD000000000000
	tagNull uint64 = 0xFFFE000000000000
	tagPtr  uint64 = 0xFFFF000000000000

	payloadMask uint64 = 0x0000FFFFFFFFFFFF

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

func (c Cell) IsFloat() bool { return uint64(c)&tagMask < tagInt }
func (c Cell) IsInt() bool   { return uint64(c)&tagMask == tagInt }
func (c Cell) IsBool() bool  { return uint64(c)&tagMask == tagBool }
func (c Cell) IsNull() bool  { return uint64(c)&tagMask == tagNull }
func (c Cell) IsPtr() bool   { return uint64(c)&tagMask == tagPtr }

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
