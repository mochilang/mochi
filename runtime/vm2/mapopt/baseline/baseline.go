// Package baseline holds two reference points for the mapopt
// comparison:
//
//   - Generic *vmMap (map[Cell]Cell), the current MEP-24 §4 shape with
//     no int-key specialization. The "do nothing" baseline that the
//     three options must beat.
//
//   - Raw map[int64]int64. No Cell, no Objects table, no dispatch.
//     The theoretical floor; what hand-written Go would achieve.
package baseline

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

type vmMap struct{ data map[Cell]Cell }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

// FillSumGeneric is the today-shape baseline: keys and values are
// Cells, every set/get pays a Cell pack/unpack and an itab dispatch
// through Objects.
func (vm *VM) FillSumGeneric(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := Cell(tagPtr | uint64(len(vm.Objects)))
	vm.Objects = append(vm.Objects, &vmMap{data: make(map[Cell]Cell, n)})
	for i := int64(0); i < n; i++ {
		m := vm.Objects[xs.Ptr()].(*vmMap)
		m.data[CInt(i)] = CInt(i)
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		m := vm.Objects[xs.Ptr()].(*vmMap)
		sum += m.data[CInt(i)].Int()
	}
	return sum
}

// FillSumRaw is the theoretical floor: a plain map[int64]int64, no
// Cell, no Objects table, no dispatch.
func FillSumRaw(n int64) int64 {
	xs := make(map[int64]int64, n)
	for i := int64(0); i < n; i++ {
		xs[i] = i
	}
	var sum int64
	for i := int64(0); i < n; i++ {
		sum += xs[i]
	}
	return sum
}
