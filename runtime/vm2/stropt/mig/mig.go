// Package mig implements the MEP-26 (Migration / multi-arm switch)
// dispatch on the full Inline / Flat / Rope shape lattice from MEP-25
// §1. Each OpConcatStr handler does a Go type switch on both
// operands' shapes and picks the result shape from a length /
// threshold decision tree.
//
// String values are immutable, so there is no migration of an
// existing object the way lists migrate. The "migration" here is at
// the dispatch layer: every concat re-classifies the operand shapes
// at runtime. Cost: one 3-way type switch per operand per concat.
package mig

const (
	flatConcatThreshold = 64 // <=: produce Flat; > and depth OK: produce Rope
	maxRopeDepth        = 32
	MaxInline           = 5
)

type Cell uint64

const (
	tagSStr      uint64 = 0xFFFA_0000_0000_0000
	tagPtr       uint64 = 0xFFFC_0000_0000_0000
	tagMask      uint64 = 0xFFFE_0000_0000_0000
	sstrLenShift uint   = 40
	sstrByteMask uint64 = 0x0000_00FF_FFFF_FFFF
)

func CSStr(b []byte) Cell {
	var packed uint64
	for i, x := range b {
		packed |= uint64(x) << (uint(i) * 8)
	}
	return Cell(tagSStr | uint64(len(b))<<sstrLenShift | (packed & sstrByteMask))
}
func CPtr(idx int) Cell    { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int    { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsSStr() bool { return uint64(c)&tagMask == tagSStr }
func (c Cell) SStrLen() int { return int(uint64(c) >> sstrLenShift & 0x1F) }
func (c Cell) sstrBytes(buf *[MaxInline]byte) []byte {
	n := c.SStrLen()
	v := uint64(c)
	for i := 0; i < n; i++ {
		buf[i] = byte(v >> (uint(i) * 8))
	}
	return buf[:n]
}

type vmString struct{ bytes []byte }
type vmStringRope struct {
	left, right Cell
	length      int
	depth       uint8
}

type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

func (vm *VM) addObject(o any) Cell {
	vm.Objects = append(vm.Objects, o)
	return CPtr(len(vm.Objects) - 1)
}

// shapeLen returns the byte length of c by switching on its shape.
// This is the workhorse type-check that the IC and AOT variants
// aim to avoid on the hot path.
func (vm *VM) shapeLen(c Cell) int {
	if c.IsSStr() {
		return c.SStrLen()
	}
	switch o := vm.Objects[c.Ptr()].(type) {
	case *vmString:
		return len(o.bytes)
	case *vmStringRope:
		return o.length
	}
	return 0
}

func (vm *VM) shapeDepth(c Cell) uint8 {
	if !c.IsSStr() {
		if r, ok := vm.Objects[c.Ptr()].(*vmStringRope); ok {
			return r.depth
		}
	}
	return 0
}

// flatten linearizes any shape into a fresh []byte. Used by the
// fall-back path of concat and by Length / Equal for ropes whose
// hash hasn't been computed.
func (vm *VM) flatten(c Cell) []byte {
	if c.IsSStr() {
		var buf [MaxInline]byte
		out := make([]byte, c.SStrLen())
		copy(out, c.sstrBytes(&buf))
		return out
	}
	switch o := vm.Objects[c.Ptr()].(type) {
	case *vmString:
		return o.bytes
	case *vmStringRope:
		out := make([]byte, 0, o.length)
		var walk func(c Cell)
		walk = func(c Cell) {
			if c.IsSStr() {
				var buf [MaxInline]byte
				out = append(out, c.sstrBytes(&buf)...)
				return
			}
			switch n := vm.Objects[c.Ptr()].(type) {
			case *vmString:
				out = append(out, n.bytes...)
			case *vmStringRope:
				walk(n.left)
				walk(n.right)
			}
		}
		walk(c)
		return out
	}
	return nil
}

// concat picks the result shape via the MEP-25 §1 decision tree.
// Every call performs the type-switch on both operands; that switch
// is what Mig pays vs IC/AOT.
func (vm *VM) concat(a, b Cell) Cell {
	la := vm.shapeLen(a)
	lb := vm.shapeLen(b)
	// Case 1: both inline, combined fits inline.
	if a.IsSStr() && b.IsSStr() && la+lb <= MaxInline {
		pa := uint64(a) & sstrByteMask
		pb := uint64(b) & sstrByteMask
		return Cell(tagSStr | uint64(la+lb)<<sstrLenShift | (pa | (pb << (uint(la) * 8))))
	}
	total := la + lb
	// Case 2: combined small enough for flat.
	if total <= flatConcatThreshold {
		ab := vm.flatten(a)
		bb := vm.flatten(b)
		out := make([]byte, total)
		copy(out, ab)
		copy(out[la:], bb)
		return vm.addObject(&vmString{bytes: out})
	}
	// Case 3: rope, unless depth would exceed cap.
	da := vm.shapeDepth(a)
	db := vm.shapeDepth(b)
	d := da
	if db > d {
		d = db
	}
	if d+1 > maxRopeDepth {
		// Flatten, then case 2.
		ab := vm.flatten(a)
		bb := vm.flatten(b)
		out := make([]byte, total)
		copy(out, ab)
		copy(out[la:], bb)
		return vm.addObject(&vmString{bytes: out})
	}
	return vm.addObject(&vmStringRope{left: a, right: b, length: total, depth: d + 1})
}

func (vm *VM) length(c Cell) int { return vm.shapeLen(c) }

// ConcatLoop builds "x" repeated N times via N concats and returns
// the final length. Most of the iterations after iter 6 take Case 3
// (rope) and pay no byte-copy cost; iter 1-5 take Case 1 (inline);
// iter 6 takes Case 2 (flat).
func (vm *VM) ConcatLoop(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	x := CSStr([]byte("x"))
	s := CSStr(nil)
	for i := int64(0); i < n; i++ {
		s = vm.concat(s, x)
	}
	return int64(vm.length(s))
}
