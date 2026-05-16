// Package mig is the MEP-26 (Migration) prototype of the vm2 list
// subsystem. A list begins life as *vmListI64 (int-specialized
// backing). The first push of a non-int Cell migrates the list in
// place to *vmList ([]Cell backing); the migration replaces
// Objects[idx] so existing references see the new shape transparently.
// Migration is one-way and amortized O(1) per operation.
//
// Every list-touching opcode handler resolves the backing-store shape
// via a Go type switch on Objects[idx]. The expected steady-state
// cost is one itab compare per access.
package mig

// Cell is the 8-byte NaN-boxed value used by the mini-VM. Layout
// matches runtime/vm2 (tag in the high 16 bits, payload below) but is
// duplicated here so the package is self-contained.
type Cell uint64

const (
	tagInt  uint64 = 0xFFF8_0000_0000_0000
	tagPtr  uint64 = 0xFFFC_0000_0000_0000
	tagMask uint64 = 0xFFFC_0000_0000_0000
)

func CInt(v int64) Cell { return Cell(tagInt | uint64(v)&0x0000_FFFF_FFFF_FFFF) }
func CPtr(idx int) Cell { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Int() int64 {
	v := int64(c) & 0x0000_FFFF_FFFF_FFFF
	if v&0x0000_8000_0000_0000 != 0 {
		v |= ^int64(0x0000_FFFF_FFFF_FFFF)
	}
	return v
}
func (c Cell) Ptr() int    { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsInt() bool { return uint64(c)&tagMask == tagInt }

// vmListI64 is the int-specialized backing store. Used by default for
// new lists; migrated away from on first non-int push/set.
type vmListI64 struct{ data []int64 }

// vmList is the generic backing store. Holds Cell values regardless
// of tag. The shape every list ends up at after a non-int operation.
type vmList struct{ data []Cell }

// VM holds the heap object table. Single-threaded; matches vm2 vm.go.
type VM struct{ Objects []any }

func (vm *VM) addObject(o any) int { vm.Objects = append(vm.Objects, o); return len(vm.Objects) - 1 }

// newList allocates a *vmListI64 by default — the assumption is that
// most lists hold ints (validated by MEP-23 list benchmarks).
func (vm *VM) newList(capHint int) Cell {
	if capHint < 0 {
		capHint = 0
	}
	return CPtr(vm.addObject(&vmListI64{data: make([]int64, 0, capHint)}))
}

// listPush appends v to the list at c. If the list is still I64 and
// v is an int, the int64 path runs (no boxing). Otherwise the list
// migrates to *vmList in place, then v appends as a Cell.
func (vm *VM) listPush(c Cell, v Cell) {
	idx := c.Ptr()
	switch l := vm.Objects[idx].(type) {
	case *vmListI64:
		if v.IsInt() {
			l.data = append(l.data, v.Int())
			return
		}
		// Migrate: build *vmList, copy, swap in place, then push.
		dst := &vmList{data: make([]Cell, len(l.data), cap(l.data))}
		for i, x := range l.data {
			dst.data[i] = CInt(x)
		}
		vm.Objects[idx] = dst
		dst.data = append(dst.data, v)
	case *vmList:
		l.data = append(l.data, v)
	}
}

// listGet returns element i of the list at c. The type switch picks
// the int64 or Cell path; int64 reads pack back through CInt.
func (vm *VM) listGet(c Cell, i int64) Cell {
	switch l := vm.Objects[c.Ptr()].(type) {
	case *vmListI64:
		return CInt(l.data[i])
	case *vmList:
		return l.data[i]
	}
	return Cell(0)
}

// listLen returns the element count of the list at c.
func (vm *VM) listLen(c Cell) int64 {
	switch l := vm.Objects[c.Ptr()].(type) {
	case *vmListI64:
		return int64(len(l.data))
	case *vmList:
		return int64(len(l.data))
	}
	return 0
}

// FillSum runs the canonical list workload: build [0, 1, ..., n-1]
// via repeated push, then sum the elements. Returns the sum and
// leaves the VM ready for re-use (Objects reset by the caller).
//
// This is the workload that MEP-23 measures as `lists/fill_sum`. The
// kernel is hand-rolled in Go (not bytecode) so each strategy's
// dispatch cost is measured directly — the inner loops below are the
// "specialized handler" equivalent of what a JIT would emit.
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newList(int(n))
	for i := int64(0); i < n; i++ {
		vm.listPush(xs, CInt(i))
	}
	var sum int64
	m := vm.listLen(xs)
	for i := int64(0); i < m; i++ {
		sum += vm.listGet(xs, i).Int()
	}
	return sum
}

// NewVM returns a fresh VM with a reasonable initial Objects cap.
func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }
