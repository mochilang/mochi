// Package aot implements MEP-28 AOT specialization for the set
// container. Specialized I64 handlers take raw int64 members directly,
// skipping the Cell pack/unpack.
package aot

type Cell uint64

const (
	tagInt uint64 = 0xFFF8_0000_0000_0000
	tagPtr uint64 = 0xFFFC_0000_0000_0000
)

func CInt(v int64) Cell { return Cell(tagInt | uint64(v)&0x0000_FFFF_FFFF_FFFF) }
func CPtr(idx int) Cell { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }

type vmSetI64 struct{ data map[int64]struct{} }
type vmSet struct{ data map[Cell]struct{} }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

func (vm *VM) newSetI64(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmSetI64{data: make(map[int64]struct{}, hint)})
	return CPtr(idx)
}

func (vm *VM) setAddI64(s Cell, k int64) {
	si := vm.Objects[s.Ptr()].(*vmSetI64)
	si.data[k] = struct{}{}
}

func (vm *VM) setHasI64(s Cell, k int64) bool {
	si := vm.Objects[s.Ptr()].(*vmSetI64)
	_, hit := si.data[k]
	return hit
}

// Generic counterparts kept for shape-unknown sites.
func (vm *VM) newSetGeneric(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmSet{data: make(map[Cell]struct{}, hint)})
	return CPtr(idx)
}
func (vm *VM) setAddGeneric(s Cell, k Cell) {
	sg := vm.Objects[s.Ptr()].(*vmSet)
	sg.data[k] = struct{}{}
}
func (vm *VM) setHasGeneric(s Cell, k Cell) bool {
	sg := vm.Objects[s.Ptr()].(*vmSet)
	_, hit := sg.data[k]
	return hit
}

func (vm *VM) FillProbe(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newSetI64(int(n))
	for i := int64(0); i < n; i++ {
		vm.setAddI64(xs, i)
	}
	var hits int64
	for i := int64(0); i < n; i++ {
		if vm.setHasI64(xs, i) {
			hits++
		}
	}
	return hits
}
