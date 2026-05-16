// Package ic is the MEP-27 (Inline Cache) prototype of the vm2 list
// subsystem. Each list-touching call site carries a tiny inline cache
// recording the last-seen backing-store shape. On a monomorphic hot
// loop the IC check is one byte-tag compare + a predicted branch; the
// fast path then runs a single-case type assertion (Go's *T cast with
// ok) instead of a multi-arm type switch.
//
// The package shape mirrors mig and aot so the bench harness can pull
// all three through identical entry points.
package ic

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

type vmListI64 struct{ data []int64 }
type vmList struct{ data []Cell }

// ICSlot is the per-call-site cache. State values:
//
//	0 = uninit (first hit populates)
//	1 = monomorphic, *vmListI64
//	2 = monomorphic, *vmList
//	3 = polymorphic (both shapes seen at this site)
//
// vm2 is single-threaded so the slot is a plain byte. A richer K=4
// IC would carry additional shape tags; K=1 is the minimum that lets
// the hot path skip the multi-arm type switch.
type ICSlot struct{ State uint8 }

type VM struct{ Objects []any }

func (vm *VM) addObject(o any) int { vm.Objects = append(vm.Objects, o); return len(vm.Objects) - 1 }

func (vm *VM) newList(capHint int) Cell {
	if capHint < 0 {
		capHint = 0
	}
	return CPtr(vm.addObject(&vmListI64{data: make([]int64, 0, capHint)}))
}

// listPushIC is the IC-aware push. The slot fast-paths the cached
// shape; misses fall through to the type-switch handler that updates
// the IC. On steady state in a monomorphic loop the slot stays at
// state 1 and the `*vmListI64` assertion succeeds first try.
func (vm *VM) listPushIC(slot *ICSlot, c Cell, v Cell) {
	obj := vm.Objects[c.Ptr()]
	switch slot.State {
	case 1: // cached *vmListI64
		if l, ok := obj.(*vmListI64); ok {
			if v.IsInt() {
				l.data = append(l.data, v.Int())
				return
			}
			// Shape unchanged but value forces migration; rare in
			// monomorphic loops. Update IC to state 2 because the
			// object's shape is about to change.
			dst := &vmList{data: make([]Cell, len(l.data), cap(l.data))}
			for i, x := range l.data {
				dst.data[i] = CInt(x)
			}
			vm.Objects[c.Ptr()] = dst
			dst.data = append(dst.data, v)
			slot.State = 2
			return
		}
		// Cache miss: shape changed under us. Recompute and retry.
		slot.State = 0
		vm.listPushIC(slot, c, v)
	case 2: // cached *vmList
		if l, ok := obj.(*vmList); ok {
			l.data = append(l.data, v)
			return
		}
		slot.State = 0
		vm.listPushIC(slot, c, v)
	default: // uninit or polymorphic — fall through, then update IC
		switch l := obj.(type) {
		case *vmListI64:
			slot.State = 1
			if v.IsInt() {
				l.data = append(l.data, v.Int())
				return
			}
			dst := &vmList{data: make([]Cell, len(l.data), cap(l.data))}
			for i, x := range l.data {
				dst.data[i] = CInt(x)
			}
			vm.Objects[c.Ptr()] = dst
			dst.data = append(dst.data, v)
			slot.State = 2
		case *vmList:
			slot.State = 2
			l.data = append(l.data, v)
		}
	}
}

// listGetIC is the IC-aware get. Same structure as push.
func (vm *VM) listGetIC(slot *ICSlot, c Cell, i int64) Cell {
	obj := vm.Objects[c.Ptr()]
	switch slot.State {
	case 1:
		if l, ok := obj.(*vmListI64); ok {
			return CInt(l.data[i])
		}
		slot.State = 0
		return vm.listGetIC(slot, c, i)
	case 2:
		if l, ok := obj.(*vmList); ok {
			return l.data[i]
		}
		slot.State = 0
		return vm.listGetIC(slot, c, i)
	default:
		switch l := obj.(type) {
		case *vmListI64:
			slot.State = 1
			return CInt(l.data[i])
		case *vmList:
			slot.State = 2
			return l.data[i]
		}
	}
	return Cell(0)
}

// listLenIC is the IC-aware len. Same pattern, no value path.
func (vm *VM) listLenIC(slot *ICSlot, c Cell) int64 {
	obj := vm.Objects[c.Ptr()]
	switch slot.State {
	case 1:
		if l, ok := obj.(*vmListI64); ok {
			return int64(len(l.data))
		}
		slot.State = 0
		return vm.listLenIC(slot, c)
	case 2:
		if l, ok := obj.(*vmList); ok {
			return int64(len(l.data))
		}
		slot.State = 0
		return vm.listLenIC(slot, c)
	default:
		switch l := obj.(type) {
		case *vmListI64:
			slot.State = 1
			return int64(len(l.data))
		case *vmList:
			slot.State = 2
			return int64(len(l.data))
		}
	}
	return 0
}

// FillSum runs fill+sum with three independent IC slots — one per
// logical call site, matching how a real bytecode-level IC would lay
// out per-ip cache entries.
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	var pushSlot, lenSlot, getSlot ICSlot
	xs := vm.newList(int(n))
	for i := int64(0); i < n; i++ {
		vm.listPushIC(&pushSlot, xs, CInt(i))
	}
	var sum int64
	m := vm.listLenIC(&lenSlot, xs)
	for i := int64(0); i < m; i++ {
		sum += vm.listGetIC(&getSlot, xs, i).Int()
	}
	return sum
}

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }
