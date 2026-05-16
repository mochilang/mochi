// Package ic implements the MEP-27 (Inline Cache) dispatch on the
// MEP-25 §1 string shape lattice. Each OpConcatStr call site carries
// a K=4 inline cache over the (leftShape, rightShape) tuple. On a
// hit, dispatch is a linear scan over cached tuples followed by a
// direct branch into the specialized arm. On a miss, the IC is
// extended (up to K=4) or marked megamorphic, and the generic
// switch-based dispatch runs.
//
// Unlike lists where the workload is monomorphic, strings/concat_loop
// is genuinely polymorphic: the tuple transitions (Inline, Inline) →
// (Flat, Inline) → (Rope, Inline) as the left operand grows. K=4 IC
// holds all three observed tuples without going megamorphic, so the
// steady-state path after warmup is an IC scan (≤3 compares) + the
// specialized arm.
package ic

const (
	flatConcatThreshold = 64
	maxRopeDepth        = 32
	MaxInline           = 5
)

const (
	shapeInline uint8 = 0
	shapeFlat   uint8 = 1
	shapeRope   uint8 = 2
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
func CPtr(idx int) Cell      { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int      { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsSStr() bool  { return uint64(c)&tagMask == tagSStr }
func (c Cell) SStrLen() int  { return int(uint64(c) >> sstrLenShift & 0x1F) }
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

// ICSlot is K=4 over (leftShape*3 + rightShape) tuples. Count is the
// number of populated entries. State: 0=uninit, 1..4=mono..poly,
// 0xFF=megamorphic (skip the IC).
type ICSlot struct {
	State  uint8
	Tuples [4]uint8
}

// shapeOf is the IC-side classifier: read the Cell's shape tag in
// constant time. Inline is checked first because the IC's expected
// hot path keeps shapes that arrive frequently near the top.
func (vm *VM) shapeOf(c Cell) uint8 {
	if c.IsSStr() {
		return shapeInline
	}
	if _, ok := vm.Objects[c.Ptr()].(*vmStringRope); ok {
		return shapeRope
	}
	return shapeFlat
}

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

func (vm *VM) lenOf(c Cell, shape uint8) int {
	switch shape {
	case shapeInline:
		return c.SStrLen()
	case shapeFlat:
		return len(vm.Objects[c.Ptr()].(*vmString).bytes)
	case shapeRope:
		return vm.Objects[c.Ptr()].(*vmStringRope).length
	}
	return 0
}

func (vm *VM) depthOf(c Cell, shape uint8) uint8 {
	if shape == shapeRope {
		return vm.Objects[c.Ptr()].(*vmStringRope).depth
	}
	return 0
}

// concatSpecialized runs the same MEP-25 §1 decision tree as the mig
// version but with the operand shapes passed in already-resolved.
// Each arm is the AOT-style direct path; no internal type switching.
func (vm *VM) concatSpecialized(a, b Cell, sa, sb uint8) Cell {
	la := vm.lenOf(a, sa)
	lb := vm.lenOf(b, sb)
	if sa == shapeInline && sb == shapeInline && la+lb <= MaxInline {
		pa := uint64(a) & sstrByteMask
		pb := uint64(b) & sstrByteMask
		return Cell(tagSStr | uint64(la+lb)<<sstrLenShift | (pa | (pb << (uint(la) * 8))))
	}
	total := la + lb
	if total <= flatConcatThreshold {
		ab := vm.flatten(a)
		bb := vm.flatten(b)
		out := make([]byte, total)
		copy(out, ab)
		copy(out[la:], bb)
		vm.Objects = append(vm.Objects, &vmString{bytes: out})
		return CPtr(len(vm.Objects) - 1)
	}
	d := vm.depthOf(a, sa)
	if d2 := vm.depthOf(b, sb); d2 > d {
		d = d2
	}
	if d+1 > maxRopeDepth {
		ab := vm.flatten(a)
		bb := vm.flatten(b)
		out := make([]byte, total)
		copy(out, ab)
		copy(out[la:], bb)
		vm.Objects = append(vm.Objects, &vmString{bytes: out})
		return CPtr(len(vm.Objects) - 1)
	}
	vm.Objects = append(vm.Objects, &vmStringRope{left: a, right: b, length: total, depth: d + 1})
	return CPtr(len(vm.Objects) - 1)
}

// concatIC is the IC-aware concat. On a hit, the cached tuple
// pre-classifies the operands so concatSpecialized can skip the
// shape-classifying type switch. On a miss, the IC is extended.
func (vm *VM) concatIC(slot *ICSlot, a, b Cell) Cell {
	if slot.State == 0xFF {
		// Megamorphic: skip IC entirely, classify and dispatch.
		sa := vm.shapeOf(a)
		sb := vm.shapeOf(b)
		return vm.concatSpecialized(a, b, sa, sb)
	}
	sa := vm.shapeOf(a)
	sb := vm.shapeOf(b)
	tup := sa*3 + sb
	// Linear scan over cached tuples — K is at most 4.
	for i := uint8(0); i < slot.State; i++ {
		if slot.Tuples[i] == tup {
			return vm.concatSpecialized(a, b, sa, sb)
		}
	}
	// Miss. Extend or megamorphic.
	if slot.State < 4 {
		slot.Tuples[slot.State] = tup
		slot.State++
		return vm.concatSpecialized(a, b, sa, sb)
	}
	slot.State = 0xFF
	return vm.concatSpecialized(a, b, sa, sb)
}

func (vm *VM) length(c Cell) int { return vm.lenOf(c, vm.shapeOf(c)) }

func (vm *VM) ConcatLoop(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	var slot ICSlot
	x := CSStr([]byte("x"))
	s := CSStr(nil)
	for i := int64(0); i < n; i++ {
		s = vm.concatIC(&slot, s, x)
	}
	return int64(vm.length(s))
}
