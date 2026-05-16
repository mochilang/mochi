// Package baseline mirrors the current MEP-24 §2 string shape:
// strings are always Flat *vmString backed by []byte. Concat
// allocates a fresh buffer and copies both operands. Inline-packing
// for ≤5-byte results is retained because it is already shipped.
//
// This is the reference point that the three rope-aware strategies
// (mig, ic, aot) must beat. With N concats of a single byte to a
// growing left operand, the buffer-copy cost is O(N²) in total
// bytes — the canonical motivation for ropes.
package baseline

type Cell uint64

const (
	tagSStr      uint64 = 0xFFFA_0000_0000_0000
	tagPtr       uint64 = 0xFFFC_0000_0000_0000
	tagMask      uint64 = 0xFFFE_0000_0000_0000
	sstrLenShift uint   = 40
	sstrByteMask uint64 = 0x0000_00FF_FFFF_FFFF
	MaxInline           = 5
)

func CSStr(b []byte) Cell {
	var packed uint64
	for i, x := range b {
		packed |= uint64(x) << (uint(i) * 8)
	}
	return Cell(tagSStr | uint64(len(b))<<sstrLenShift | (packed & sstrByteMask))
}
func CPtr(idx int) Cell      { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int      { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsSStr() bool  { return uint64(c)&tagMask == tagSStr }
func (c Cell) SStrLen() int  { return int(uint64(c) >> sstrLenShift & 0x1F) }
func (c Cell) SStrBytes() []byte {
	n := c.SStrLen()
	out := make([]byte, n)
	v := uint64(c)
	for i := 0; i < n; i++ {
		out[i] = byte(v >> (uint(i) * 8))
	}
	return out
}

type vmString struct{ bytes []byte }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

func (vm *VM) flatten(c Cell) []byte {
	if c.IsSStr() {
		return c.SStrBytes()
	}
	return vm.Objects[c.Ptr()].(*vmString).bytes
}

// concat is the today-shape concat: inline-pack if it fits, else
// allocate Flat. No rope path. Every iteration past the inline limit
// allocates a fresh []byte and copies both operands.
func (vm *VM) concat(a, b Cell) Cell {
	if a.IsSStr() && b.IsSStr() {
		la, lb := a.SStrLen(), b.SStrLen()
		if la+lb <= MaxInline {
			pa := uint64(a) & sstrByteMask
			pb := uint64(b) & sstrByteMask
			return Cell(tagSStr | uint64(la+lb)<<sstrLenShift | (pa | (pb << (uint(la) * 8))))
		}
	}
	ab := vm.flatten(a)
	bb := vm.flatten(b)
	out := make([]byte, len(ab)+len(bb))
	copy(out, ab)
	copy(out[len(ab):], bb)
	vm.Objects = append(vm.Objects, &vmString{bytes: out})
	return CPtr(len(vm.Objects) - 1)
}

func (vm *VM) length(c Cell) int {
	if c.IsSStr() {
		return c.SStrLen()
	}
	return len(vm.Objects[c.Ptr()].(*vmString).bytes)
}

// ConcatLoop builds the string "xxxx...x" (N copies of "x") via N
// concat-with-literal steps and returns its final length. This is
// the workload that MEP-23 measures as strings/concat_loop.
func (vm *VM) ConcatLoop(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	x := CSStr([]byte("x"))
	s := CSStr(nil)
	for i := int64(0); i < n; i++ {
		s = vm.concat(s, x)
	}
	return int64(vm.length(s))
}
