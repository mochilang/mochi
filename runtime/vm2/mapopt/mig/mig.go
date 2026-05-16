// Package mig implements the MEP-26 (Migration) dispatch on the
// MEP-25 §1 map shape lattice. Maps start as *vmMapI64 (int→int
// specialized) on the first OpMapNew and migrate one-way to *vmMap
// (generic Cell→Cell) the moment a non-int key or value is observed.
// Migration is in-place: vm.Objects[idx] is rewritten so existing
// Cells pointing at the map keep working.
//
// Every map op pays a `switch obj.(type)` to pick the specialized arm.
// On `maps/fill_sum` the workload is monomorphic, the map is and
// remains MapI64, so the type-switch hits its hot arm every call.
// The cost is the per-op type switch versus AOT's single-case
// assertion.
package mig

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

func (vm *VM) newMap(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmMapI64{data: make(map[int64]int64, hint)})
	return CPtr(idx)
}

// migrate flips a *vmMapI64 to a *vmMap in place. Anyone holding the
// Cell keeps the same Objects index; the next op's type switch lands
// on the *vmMap arm.
func (vm *VM) migrate(idx int) *vmMap {
	old := vm.Objects[idx].(*vmMapI64)
	nu := &vmMap{data: make(map[Cell]Cell, len(old.data))}
	for k, v := range old.data {
		nu.data[CInt(k)] = CInt(v)
	}
	vm.Objects[idx] = nu
	return nu
}

// mapSet, the workhorse. Switch on shape; if MapI64 and the key/value
// are ints, stay on the fast path; else migrate and retry on Map.
func (vm *VM) mapSet(m Cell, k, v Cell) {
	idx := m.Ptr()
	switch o := vm.Objects[idx].(type) {
	case *vmMapI64:
		if k.IsInt() && v.IsInt() {
			o.data[k.Int()] = v.Int()
			return
		}
		nu := vm.migrate(idx)
		nu.data[k] = v
	case *vmMap:
		o.data[k] = v
	}
}

func (vm *VM) mapGet(m Cell, k Cell) Cell {
	switch o := vm.Objects[m.Ptr()].(type) {
	case *vmMapI64:
		if k.IsInt() {
			return CInt(o.data[k.Int()])
		}
		// A non-int lookup on a MapI64 always misses; semantically
		// returns the zero Cell. No migration needed (read-only).
		return 0
	case *vmMap:
		return o.data[k]
	}
	return 0
}

// FillSum builds a map of N int→int entries and sums the values.
// On this workload the shape never migrates, so the type-switch hits
// the *vmMapI64 arm every iteration.
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newMap(int(n))
	for i := int64(0); i < n; i++ {
		vm.mapSet(xs, CInt(i), CInt(i))
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += vm.mapGet(xs, CInt(i)).Int()
	}
	return sum
}
