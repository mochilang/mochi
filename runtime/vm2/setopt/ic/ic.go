// Package ic implements MEP-27 Inline Cache for the set container.
// K=1 IC slot per call site: state 0=uninit, 1=mono SetI64,
// 2=mono Generic, 3=polymorphic.
package ic

type Cell uint64

const (
	tagInt uint64 = 0xFFF8_0000_0000_0000
	tagPtr uint64 = 0xFFFC_0000_0000_0000
	tagMsk uint64 = 0xFFFE_0000_0000_0000
)

func CInt(v int64) Cell    { return Cell(tagInt | uint64(v)&0x0000_FFFF_FFFF_FFFF) }
func CPtr(idx int) Cell    { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) IsInt() bool { return uint64(c)&tagMsk == tagInt }
func (c Cell) Int() int64 {
	v := int64(c) & 0x0000_FFFF_FFFF_FFFF
	if v&0x0000_8000_0000_0000 != 0 {
		v |= ^int64(0x0000_FFFF_FFFF_FFFF)
	}
	return v
}
func (c Cell) Ptr() int { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }

type vmSetI64 struct{ data map[int64]struct{} }
type vmSet struct{ data map[Cell]struct{} }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

type ICSlot struct{ State uint8 }

func (vm *VM) newSet(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmSetI64{data: make(map[int64]struct{}, hint)})
	return CPtr(idx)
}

func (vm *VM) migrate(idx int) *vmSet {
	old := vm.Objects[idx].(*vmSetI64)
	nu := &vmSet{data: make(map[Cell]struct{}, len(old.data))}
	for k := range old.data {
		nu.data[CInt(k)] = struct{}{}
	}
	vm.Objects[idx] = nu
	return nu
}

func (vm *VM) setAddIC(slot *ICSlot, s Cell, k Cell) {
	idx := s.Ptr()
	if slot.State == 1 {
		if si, ok := vm.Objects[idx].(*vmSetI64); ok {
			if k.IsInt() {
				si.data[k.Int()] = struct{}{}
				return
			}
			slot.State = 3
			nu := vm.migrate(idx)
			nu.data[k] = struct{}{}
			return
		}
		slot.State = 3
	} else if slot.State == 2 {
		if sg, ok := vm.Objects[idx].(*vmSet); ok {
			sg.data[k] = struct{}{}
			return
		}
		slot.State = 3
	}
	switch o := vm.Objects[idx].(type) {
	case *vmSetI64:
		if k.IsInt() {
			if slot.State == 0 {
				slot.State = 1
			}
			o.data[k.Int()] = struct{}{}
			return
		}
		nu := vm.migrate(idx)
		slot.State = 2
		nu.data[k] = struct{}{}
	case *vmSet:
		if slot.State == 0 {
			slot.State = 2
		}
		o.data[k] = struct{}{}
	}
}

func (vm *VM) setHasIC(slot *ICSlot, s Cell, k Cell) bool {
	idx := s.Ptr()
	if slot.State == 1 {
		if si, ok := vm.Objects[idx].(*vmSetI64); ok {
			if k.IsInt() {
				_, hit := si.data[k.Int()]
				return hit
			}
			return false
		}
		slot.State = 3
	} else if slot.State == 2 {
		if sg, ok := vm.Objects[idx].(*vmSet); ok {
			_, hit := sg.data[k]
			return hit
		}
		slot.State = 3
	}
	switch o := vm.Objects[idx].(type) {
	case *vmSetI64:
		if slot.State == 0 {
			slot.State = 1
		}
		if k.IsInt() {
			_, hit := o.data[k.Int()]
			return hit
		}
		return false
	case *vmSet:
		if slot.State == 0 {
			slot.State = 2
		}
		_, hit := o.data[k]
		return hit
	}
	return false
}

func (vm *VM) FillProbe(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	var addSlot, hasSlot ICSlot
	xs := vm.newSet(int(n))
	for i := int64(0); i < n; i++ {
		vm.setAddIC(&addSlot, xs, CInt(i))
	}
	var hits int64
	for i := int64(0); i < n; i++ {
		if vm.setHasIC(&hasSlot, xs, CInt(i)) {
			hits++
		}
	}
	return hits
}
