// Package ic implements the MEP-27 (Inline Cache) dispatch on the
// MEP-25 §1 map shape lattice. Each map call site carries an ICSlot
// with a one-byte state field: 0 uninit, 1 monomorphic on I64, 2
// monomorphic on Generic, 3 polymorphic. On a hit the handler does a
// single-case type assertion and the specialized work; on a miss the
// slot transitions and the handler retries on the generic switch path.
//
// The maps/fill_sum workload is monomorphic (always I64), so after one
// warmup call every iteration hits state=1 and pays one byte-tag
// compare plus the I64 type assertion.
package ic

type Cell uint64

const (
	tagInt uint64 = 0xFFF8_0000_0000_0000
	tagPtr uint64 = 0xFFFC_0000_0000_0000
	tagMsk uint64 = 0xFFFE_0000_0000_0000
)

func CInt(v int64) Cell { return Cell(tagInt | uint64(v)&0x0000_FFFF_FFFF_FFFF) }
func CPtr(idx int) Cell { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsInt() bool { return uint64(c)&tagMsk == tagInt }
func (c Cell) Int() int64 {
	v := int64(c) & 0x0000_FFFF_FFFF_FFFF
	if v&0x0000_8000_0000_0000 != 0 {
		v |= ^int64(0x0000_FFFF_FFFF_FFFF)
	}
	return v
}
func (c Cell) Ptr() int { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }

type vmMapI64 struct{ data map[int64]int64 }
type vmMap struct{ data map[Cell]Cell }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

// ICSlot: 0=uninit, 1=mono I64, 2=mono Generic, 3=polymorphic.
type ICSlot struct{ State uint8 }

func (vm *VM) newMap(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmMapI64{data: make(map[int64]int64, hint)})
	return CPtr(idx)
}

func (vm *VM) migrate(idx int) *vmMap {
	old := vm.Objects[idx].(*vmMapI64)
	nu := &vmMap{data: make(map[Cell]Cell, len(old.data))}
	for k, v := range old.data {
		nu.data[CInt(k)] = CInt(v)
	}
	vm.Objects[idx] = nu
	return nu
}

func (vm *VM) mapSetIC(slot *ICSlot, m Cell, k, v Cell) {
	idx := m.Ptr()
	if slot.State == 1 {
		if mi, ok := vm.Objects[idx].(*vmMapI64); ok {
			if k.IsInt() && v.IsInt() {
				mi.data[k.Int()] = v.Int()
				return
			}
			// Mixed key/value triggers shape migration; bump to poly.
			slot.State = 3
			nu := vm.migrate(idx)
			nu.data[k] = v
			return
		}
		slot.State = 3
	} else if slot.State == 2 {
		if mg, ok := vm.Objects[idx].(*vmMap); ok {
			mg.data[k] = v
			return
		}
		slot.State = 3
	}
	// Uninit or polymorphic: classify and update.
	switch o := vm.Objects[idx].(type) {
	case *vmMapI64:
		if k.IsInt() && v.IsInt() {
			if slot.State == 0 {
				slot.State = 1
			}
			o.data[k.Int()] = v.Int()
			return
		}
		nu := vm.migrate(idx)
		slot.State = 2
		nu.data[k] = v
	case *vmMap:
		if slot.State == 0 {
			slot.State = 2
		}
		o.data[k] = v
	}
}

func (vm *VM) mapGetIC(slot *ICSlot, m Cell, k Cell) Cell {
	idx := m.Ptr()
	if slot.State == 1 {
		if mi, ok := vm.Objects[idx].(*vmMapI64); ok {
			if k.IsInt() {
				return CInt(mi.data[k.Int()])
			}
			return 0
		}
		slot.State = 3
	} else if slot.State == 2 {
		if mg, ok := vm.Objects[idx].(*vmMap); ok {
			return mg.data[k]
		}
		slot.State = 3
	}
	switch o := vm.Objects[idx].(type) {
	case *vmMapI64:
		if slot.State == 0 {
			slot.State = 1
		}
		if k.IsInt() {
			return CInt(o.data[k.Int()])
		}
		return 0
	case *vmMap:
		if slot.State == 0 {
			slot.State = 2
		}
		return o.data[k]
	}
	return 0
}

// FillSum threads two ICSlots, one per call site (set and get).
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	var setSlot, getSlot ICSlot
	xs := vm.newMap(int(n))
	for i := int64(0); i < n; i++ {
		vm.mapSetIC(&setSlot, xs, CInt(i), CInt(i))
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += vm.mapGetIC(&getSlot, xs, CInt(i)).Int()
	}
	return sum
}
