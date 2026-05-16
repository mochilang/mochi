// Package aot implements the MEP-28 (AOT codegen specialization)
// dispatch on the MEP-25 §1 map shape lattice. The compiler emits
// specialized opcodes per (shape, keyType) pair once the shape is
// known from IR types or from a hoisted IC guard. Each handler does a
// single-case type assertion on Objects[idx] and the specialized
// work, with no internal shape switch.
//
// On `maps/fill_sum` the IR knows the map is int->int at allocation
// (OpMapNewI64), so every Set/Get goes through OpMapSetI64I64 /
// OpMapGetI64I64 with no runtime shape check beyond the unavoidable
// itab assertion that Objects[idx] is *vmMapI64.
package aot

type Cell uint64

const (
	tagInt uint64 = 0xFFF8_0000_0000_0000
	tagPtr uint64 = 0xFFFC_0000_0000_0000
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
func (c Cell) Ptr() int { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }

type vmMapI64 struct{ data map[int64]int64 }
type vmMap struct{ data map[Cell]Cell }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

// newMapI64 is the AOT counterpart of OpMapNewI64: the IR proved the
// key/value type so we allocate the specialized header directly.
func (vm *VM) newMapI64(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmMapI64{data: make(map[int64]int64, hint)})
	return CPtr(idx)
}

// Specialized I64xI64 ops: one type assertion, no shape switch.
func (vm *VM) mapSetI64I64(m Cell, k, v int64) {
	mi := vm.Objects[m.Ptr()].(*vmMapI64)
	mi.data[k] = v
}

func (vm *VM) mapGetI64I64(m Cell, k int64) int64 {
	mi := vm.Objects[m.Ptr()].(*vmMapI64)
	return mi.data[k]
}

// Generic counterparts kept so the package compiles when the IR
// cannot prove key/value types.
func (vm *VM) newMapGeneric(hint int) Cell {
	idx := len(vm.Objects)
	vm.Objects = append(vm.Objects, &vmMap{data: make(map[Cell]Cell, hint)})
	return CPtr(idx)
}
func (vm *VM) mapSetGeneric(m Cell, k, v Cell) {
	mg := vm.Objects[m.Ptr()].(*vmMap)
	mg.data[k] = v
}
func (vm *VM) mapGetGeneric(m Cell, k Cell) Cell {
	mg := vm.Objects[m.Ptr()].(*vmMap)
	return mg.data[k]
}

// FillSum is the AOT version of the workload. Because the IR proved
// int->int, the loop calls the specialized I64xI64 ops directly and
// passes raw int64s, skipping the Cell pack/unpack.
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newMapI64(int(n))
	for i := int64(0); i < n; i++ {
		vm.mapSetI64I64(xs, i, i)
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += vm.mapGetI64I64(xs, i)
	}
	return sum
}
