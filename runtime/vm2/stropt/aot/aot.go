// Package aot implements the MEP-28 (Ahead-of-Time codegen
// specialization) dispatch on the MEP-25 §1 string shape lattice. The
// compiler emits a specialized OpConcatStr_{LeftShape}_{RightShape}
// per call site once the shape pair is proven (by profiling, by type
// inference, or by an enclosing IC that promoted to monomorphic). At
// the bytecode level this looks like a 3×3 function table indexed by
// (leftShape, rightShape); each entry is a leaf with no
// shape-classifying type switches.
//
// Reality check: when the call site is polymorphic — as
// strings/concat_loop is, transitioning Inline→Flat→Rope as the left
// operand grows — AOT either falls back to a generic helper or
// re-classifies at the call site. The fast path is only fast when
// shape stability holds.
package aot

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
func CPtr(idx int) Cell     { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int     { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }
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

// shapeOf returns the resolved shape. AOT uses it only when the
// compiler couldn't prove the shape (fallback) or in the workload's
// outer driver to pick the specialized opcode to dispatch.
func (vm *VM) shapeOf(c Cell) uint8 {
	if c.IsSStr() {
		return shapeInline
	}
	if _, ok := vm.Objects[c.Ptr()].(*vmStringRope); ok {
		return shapeRope
	}
	return shapeFlat
}

func (vm *VM) flatBytes(c Cell, shape uint8) []byte {
	switch shape {
	case shapeInline:
		var buf [MaxInline]byte
		n := c.SStrLen()
		out := make([]byte, n)
		copy(out, c.sstrBytes(&buf))
		return out
	case shapeFlat:
		return vm.Objects[c.Ptr()].(*vmString).bytes
	default:
		return vm.flattenRope(c)
	}
}

func (vm *VM) flattenRope(c Cell) []byte {
	r := vm.Objects[c.Ptr()].(*vmStringRope)
	out := make([]byte, 0, r.length)
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

func (vm *VM) lenAndDepth(c Cell, shape uint8) (int, uint8) {
	switch shape {
	case shapeInline:
		return c.SStrLen(), 0
	case shapeFlat:
		return len(vm.Objects[c.Ptr()].(*vmString).bytes), 0
	default:
		r := vm.Objects[c.Ptr()].(*vmStringRope)
		return r.length, r.depth
	}
}

// makeFlat / makeRope are the common tails. Each specialized arm
// inlines the shape-known prologue and then funnels into one of these.
func (vm *VM) makeFlat(a, b Cell, sa, sb uint8, la, lb int) Cell {
	ab := vm.flatBytes(a, sa)
	bb := vm.flatBytes(b, sb)
	out := make([]byte, la+lb)
	copy(out, ab)
	copy(out[la:], bb)
	vm.Objects = append(vm.Objects, &vmString{bytes: out})
	return CPtr(len(vm.Objects) - 1)
}

func (vm *VM) makeRopeOrFlat(a, b Cell, sa, sb uint8, la, lb int, da, db uint8) Cell {
	d := da
	if db > d {
		d = db
	}
	if d+1 > maxRopeDepth {
		return vm.makeFlat(a, b, sa, sb, la, lb)
	}
	vm.Objects = append(vm.Objects, &vmStringRope{left: a, right: b, length: la + lb, depth: d + 1})
	return CPtr(len(vm.Objects) - 1)
}

// === 9 specialized concat arms ===

func (vm *VM) concatII(a, b Cell) Cell {
	la, lb := a.SStrLen(), b.SStrLen()
	if la+lb <= MaxInline {
		pa := uint64(a) & sstrByteMask
		pb := uint64(b) & sstrByteMask
		return Cell(tagSStr | uint64(la+lb)<<sstrLenShift | (pa | (pb << (uint(la) * 8))))
	}
	// la+lb ≤ 10, always within flat threshold.
	return vm.makeFlat(a, b, shapeInline, shapeInline, la, lb)
}

func (vm *VM) concatIF(a, b Cell) Cell {
	la := a.SStrLen()
	lb := len(vm.Objects[b.Ptr()].(*vmString).bytes)
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeInline, shapeFlat, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeInline, shapeFlat, la, lb, 0, 0)
}

func (vm *VM) concatIR(a, b Cell) Cell {
	la := a.SStrLen()
	rb := vm.Objects[b.Ptr()].(*vmStringRope)
	lb, db := rb.length, rb.depth
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeInline, shapeRope, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeInline, shapeRope, la, lb, 0, db)
}

func (vm *VM) concatFI(a, b Cell) Cell {
	la := len(vm.Objects[a.Ptr()].(*vmString).bytes)
	lb := b.SStrLen()
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeFlat, shapeInline, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeFlat, shapeInline, la, lb, 0, 0)
}

func (vm *VM) concatFF(a, b Cell) Cell {
	la := len(vm.Objects[a.Ptr()].(*vmString).bytes)
	lb := len(vm.Objects[b.Ptr()].(*vmString).bytes)
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeFlat, shapeFlat, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeFlat, shapeFlat, la, lb, 0, 0)
}

func (vm *VM) concatFR(a, b Cell) Cell {
	la := len(vm.Objects[a.Ptr()].(*vmString).bytes)
	rb := vm.Objects[b.Ptr()].(*vmStringRope)
	lb, db := rb.length, rb.depth
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeFlat, shapeRope, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeFlat, shapeRope, la, lb, 0, db)
}

func (vm *VM) concatRI(a, b Cell) Cell {
	ra := vm.Objects[a.Ptr()].(*vmStringRope)
	la, da := ra.length, ra.depth
	lb := b.SStrLen()
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeRope, shapeInline, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeRope, shapeInline, la, lb, da, 0)
}

func (vm *VM) concatRF(a, b Cell) Cell {
	ra := vm.Objects[a.Ptr()].(*vmStringRope)
	la, da := ra.length, ra.depth
	lb := len(vm.Objects[b.Ptr()].(*vmString).bytes)
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeRope, shapeFlat, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeRope, shapeFlat, la, lb, da, 0)
}

func (vm *VM) concatRR(a, b Cell) Cell {
	ra := vm.Objects[a.Ptr()].(*vmStringRope)
	rb := vm.Objects[b.Ptr()].(*vmStringRope)
	la, da := ra.length, ra.depth
	lb, db := rb.length, rb.depth
	if la+lb <= flatConcatThreshold {
		return vm.makeFlat(a, b, shapeRope, shapeRope, la, lb)
	}
	return vm.makeRopeOrFlat(a, b, shapeRope, shapeRope, la, lb, da, db)
}

// concatAny is the AOT call-site that dispatches via the 3×3 table.
// In production this would be elided: the compiler would have emitted
// the specialized opcode directly. Here we simulate the dispatch cost
// so the benchmark is honest about what AOT still pays when the call
// site sees ≥2 shape pairs (the concat_loop case).
func (vm *VM) concatAny(a, b Cell) Cell {
	sa := vm.shapeOf(a)
	sb := vm.shapeOf(b)
	return concatTable[sa][sb](vm, a, b)
}

var concatTable = [3][3]func(*VM, Cell, Cell) Cell{
	{(*VM).concatII, (*VM).concatIF, (*VM).concatIR},
	{(*VM).concatFI, (*VM).concatFF, (*VM).concatFR},
	{(*VM).concatRI, (*VM).concatRF, (*VM).concatRR},
}

func (vm *VM) length(c Cell) int {
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

func (vm *VM) flatten(c Cell) []byte {
	return vm.flatBytes(c, vm.shapeOf(c))
}

// ConcatLoop is the strings/concat_loop workload. After iter 6 the
// left operand transitions from Inline→Flat→Rope, so the call site
// sees three distinct shape pairs even though the right is always
// Inline. The 3×3 table dispatch captures the cost AOT pays for that
// polymorphism without a compile-time guard.
func (vm *VM) ConcatLoop(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	x := CSStr([]byte("x"))
	s := CSStr(nil)
	for i := int64(0); i < n; i++ {
		s = vm.concatAny(s, x)
	}
	return int64(vm.length(s))
}
