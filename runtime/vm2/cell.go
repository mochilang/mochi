package vm2

import (
	"math"
	"unsafe"
)

// Cell is the 16-byte tagged value used by vm2 throughout. It is the
// MEP-36 Option X layout: the 8-byte NaN-boxed Bits field carries the
// same tag/payload encoding the runtime used in its pre-refactor
// uint64 form, and Ptr carries a Go-typed pointer for pointer-tagged
// cells so the host GC can reach the pointee directly without going
// through vm.Objects.
//
// Bits layout (high 16 bits):
//
//	0x0000..0xFFEF  -> float64 (normal or subnormal)
//	0x7FF8          -> canonical qNaN, treated as float
//	0xFFFA          -> tagDeopt (JIT deopt sentinel; pc in low 48)
//	0xFFFB          -> inline short string (length in payload bits 40..47, up to 5 bytes in 0..39)
//	0xFFFC          -> int (low 48 bits, signed)
//	0xFFFD          -> bool (low bit)
//	0xFFFE          -> null
//	0xFFFF          -> ptr: low 48 bits are an Objects[] index for the
//	                  self-heal fallback; Ptr field carries the typed
//	                  Go pointer when it is populated.
//
// Int is 48-bit signed (range -2^47 .. 2^47-1). Wider ints are boxed
// into the Objects table by the compiler; see CPtr.
type Cell struct {
	Bits uint64
	// Obj carries the Go-typed pointer for pointer-tagged cells, set by
	// container constructors (newList, newMap, newString) so the host
	// GC can reach the pointee directly. Scalar cells leave it nil.
	// The accessor is PtrTo() (field is exported only so the JIT
	// trampoline can compute offsets at compile time via unsafe.Offsetof).
	Obj unsafe.Pointer
}

const (
	qNaN     uint64 = 0x7FF8000000000000
	tagMask  uint64 = 0xFFFF000000000000
	tagDeopt uint64 = 0xFFFA000000000000
	tagSStr  uint64 = 0xFFFB000000000000
	tagInt   uint64 = 0xFFFC000000000000
	tagBool  uint64 = 0xFFFD000000000000
	tagNull  uint64 = 0xFFFE000000000000
	tagPtr   uint64 = 0xFFFF000000000000

	// tagPtrStrFlag is bit 47 of a tagPtr cell's payload: set when the
	// pointee is a heap *vmString, cleared for *vmList / *vmMap / *vmSet /
	// *vmStruct / *vmClosure. mapKeyOf consults this flag in Phase 2 so it
	// can collapse heap strings to byte-content keys without walking the
	// retired Objects[] indirection.
	tagPtrStrFlag uint64 = 1 << 47

	payloadMask uint64 = 0x0000FFFFFFFFFFFF

	MaxInlineStr = 5
	sstrLenShift = 40
	sstrLenMask  = 0xFF
	sstrByteMask = uint64(1)<<sstrLenShift - 1

	MaxInlineInt int64 = 1<<47 - 1
	MinInlineInt int64 = -(1 << 47)
)

// CFloat boxes a float64. NaN inputs are canonicalized so they read
// back as IsFloat.
func CFloat(f float64) Cell {
	if f != f {
		return Cell{Bits: qNaN}
	}
	return Cell{Bits: math.Float64bits(f)}
}

// CInt boxes a 48-bit signed int.
func CInt(i int64) Cell {
	return Cell{Bits: tagInt | uint64(i)&payloadMask}
}

func CBool(b bool) Cell {
	if b {
		return Cell{Bits: tagBool | 1}
	}
	return Cell{Bits: tagBool}
}

func CNull() Cell { return Cell{Bits: tagNull} }

// CPtr boxes a 48-bit unsigned index into the owning VM's Objects table.
// The typed-pointer companion is filled in by the container constructors
// (newList, newMap, newString) that have access to the actual *T at
// construction time; CPtr alone leaves Ptr nil and callers fall through
// the self-heal accessor path.
func CPtr(idx uint64) Cell {
	return Cell{Bits: tagPtr | idx&payloadMask}
}

func (c Cell) IsFloat() bool { return c.Bits&tagMask < tagSStr }
func (c Cell) IsSStr() bool  { return c.Bits&tagMask == tagSStr }
func (c Cell) IsInt() bool   { return c.Bits&tagMask == tagInt }
func (c Cell) IsBool() bool  { return c.Bits&tagMask == tagBool }
func (c Cell) IsNull() bool  { return c.Bits&tagMask == tagNull }
func (c Cell) IsPtr() bool   { return c.Bits&tagMask == tagPtr }

// IsStr reports whether c carries a string value (inline or heap).
func (c Cell) IsStr() bool {
	if c.IsSStr() {
		return true
	}
	return c.IsHeapStr()
}

// IsHeapStr reports whether c is a tagPtr cell whose pointee is a
// *vmString. The flag is set by newString at construction time.
func (c Cell) IsHeapStr() bool {
	return c.Bits&tagMask == tagPtr && c.Bits&tagPtrStrFlag != 0
}

// CSStr packs up to MaxInlineStr bytes into an inline string Cell.
func CSStr(b []byte) Cell {
	var packed uint64
	for i, x := range b {
		packed |= uint64(x) << (uint(i) * 8)
	}
	return Cell{Bits: tagSStr | uint64(len(b))<<sstrLenShift | packed}
}

// SStrLen returns the byte length of an inline string Cell. Caller
// must have verified IsSStr.
func (c Cell) SStrLen() int {
	return int((c.Bits >> sstrLenShift) & sstrLenMask)
}

// SStrBytes writes the inline string bytes into buf and returns a subslice.
func (c Cell) SStrBytes(buf *[MaxInlineStr]byte) []byte {
	n := c.SStrLen()
	packed := c.Bits & sstrByteMask
	for i := range n {
		buf[i] = byte(packed >> (uint(i) * 8))
	}
	return buf[:n]
}

func (c Cell) Float() float64 { return math.Float64frombits(c.Bits) }

// Int decodes the 48-bit signed payload by sign-extending through a
// left-then-arithmetic-right shift.
func (c Cell) Int() int64 { return int64(c.Bits<<16) >> 16 }

func (c Cell) Bool() bool { return c.Bits&1 != 0 }

// Ptr returns the 48-bit index into VM.Objects (the self-heal fallback path).
func (c Cell) Ptr() uint64 { return c.Bits & payloadMask }

// PtrTo returns the Go-typed pointer carried by this Cell, or nil if
// the cell has no populated typed pointer yet. Callers cast through
// unsafe.Pointer to the concrete type (e.g. (*vmList)(c.PtrTo())).
func (c Cell) PtrTo() unsafe.Pointer { return c.Obj }

// FitsInline reports whether i can be boxed inline by CInt without loss.
func FitsInline(i int64) bool {
	return i >= MinInlineInt && i <= MaxInlineInt
}

// EncodeDeopt produces the sentinel Cell returned by a JIT deopt stub.
func EncodeDeopt(pc int) Cell {
	return Cell{Bits: tagDeopt | uint64(pc)&payloadMask}
}

// DecodeDeopt reports whether c is a deopt sentinel and, if so, returns
// the carried bytecode PC.
func DecodeDeopt(c Cell) (pc int, ok bool) {
	if c.Bits&tagMask != tagDeopt {
		return 0, false
	}
	return int(c.Bits & payloadMask), true
}
