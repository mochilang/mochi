// Package baseline holds two reference points for the listopt
// comparison:
//
//   - Generic *vmList ([]Cell), the current MEP-24 §3 shape with no
//     int specialization. The "do nothing" baseline that the three
//     options must beat.
//
//   - Raw []int64, no Cell, no Objects table, no dispatch. The
//     theoretical floor; what hand-written Go would achieve. Useful
//     for sanity-checking that AOT approaches the floor.
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

type vmList struct{ data []Cell }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

// FillSumGeneric is the today-shape baseline: every element is a
// Cell, every read pays a CInt pack and an itab dispatch through
// Objects.
func (vm *VM) FillSumGeneric(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := Cell(uint64(tagPtr) | uint64(len(vm.Objects)))
	vm.Objects = append(vm.Objects, &vmList{data: make([]Cell, 0, n)})
	for i := int64(0); i < n; i++ {
		l := vm.Objects[xs.Ptr()].(*vmList)
		l.data = append(l.data, CInt(i))
	}
	var sum int64
	l := vm.Objects[xs.Ptr()].(*vmList)
	m := int64(len(l.data))
	for i := int64(0); i < m; i++ {
		l := vm.Objects[xs.Ptr()].(*vmList)
		sum += l.data[i].Int()
	}
	return sum
}

// FillSumRaw is the theoretical floor: a plain []int64, no Cell, no
// Objects table, no dispatch. The compiler can keep `xs` in a
// register and inline every op.
func FillSumRaw(n int64) int64 {
	xs := make([]int64, 0, n)
	for i := int64(0); i < n; i++ {
		xs = append(xs, i)
	}
	var sum int64
	for _, v := range xs {
		sum += v
	}
	return sum
}
