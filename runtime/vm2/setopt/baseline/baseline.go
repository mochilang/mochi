// Package baseline holds two reference points for the setopt
// comparison: today's generic map[Cell]struct{} shape plus a raw
// map[int64]struct{} floor.
package baseline

type Cell uint64

const (
	tagInt uint64 = 0xFFF8_0000_0000_0000
	tagPtr uint64 = 0xFFFC_0000_0000_0000
)

func CInt(v int64) Cell { return Cell(tagInt | uint64(v)&0x0000_FFFF_FFFF_FFFF) }
func CPtr(idx int) Cell { return Cell(tagPtr | uint64(idx)&0x0000_FFFF_FFFF_FFFF) }
func (c Cell) Ptr() int { return int(uint64(c) & 0x0000_FFFF_FFFF_FFFF) }

type vmSet struct{ data map[Cell]struct{} }
type VM struct{ Objects []any }

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }

func (vm *VM) FillProbeGeneric(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := Cell(tagPtr | uint64(len(vm.Objects)))
	vm.Objects = append(vm.Objects, &vmSet{data: make(map[Cell]struct{}, n)})
	for i := int64(0); i < n; i++ {
		s := vm.Objects[xs.Ptr()].(*vmSet)
		s.data[CInt(i)] = struct{}{}
	}
	var hits int64
	for i := int64(0); i < n; i++ {
		s := vm.Objects[xs.Ptr()].(*vmSet)
		if _, ok := s.data[CInt(i)]; ok {
			hits++
		}
	}
	return hits
}

func FillProbeRaw(n int64) int64 {
	xs := make(map[int64]struct{}, n)
	for i := int64(0); i < n; i++ {
		xs[i] = struct{}{}
	}
	var hits int64
	for i := int64(0); i < n; i++ {
		if _, ok := xs[i]; ok {
			hits++
		}
	}
	return hits
}
