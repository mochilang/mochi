// Package mig implements MEP-26 Migration for the set container.
// Sets start as *vmSetI64 (map[int64]struct{}) and migrate one-way to
// *vmSet (map[Cell]struct{}) the moment a non-int member is added.
// Every op pays a type switch over the two shapes.
package mig

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

func (vm *VM) setAdd(s Cell, k Cell) {
	idx := s.Ptr()
	switch o := vm.Objects[idx].(type) {
	case *vmSetI64:
		if k.IsInt() {
			o.data[k.Int()] = struct{}{}
			return
		}
		nu := vm.migrate(idx)
		nu.data[k] = struct{}{}
	case *vmSet:
		o.data[k] = struct{}{}
	}
}

func (vm *VM) setHas(s Cell, k Cell) bool {
	switch o := vm.Objects[s.Ptr()].(type) {
	case *vmSetI64:
		if k.IsInt() {
			_, ok := o.data[k.Int()]
			return ok
		}
		return false
	case *vmSet:
		_, ok := o.data[k]
		return ok
	}
	return false
}

// FillProbe inserts N ints then probes the same N members and counts
// hits. The shape stays SetI64 throughout, so the type switch hits
// its hot arm every call.
func (vm *VM) FillProbe(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newSet(int(n))
	for i := int64(0); i < n; i++ {
		vm.setAdd(xs, CInt(i))
	}
	var hits int64
	for i := int64(0); i < n; i++ {
		if vm.setHas(xs, CInt(i)) {
			hits++
		}
	}
	return hits
}
