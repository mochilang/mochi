// Package aot is the MEP-28 (AOT codegen specialization) prototype of
// the vm2 list subsystem. The "compiler" (in this prototype, the
// FillSum kernel itself) has resolved every list-touching call site
// to a shape-specialized variant before run time. The handlers carry
// no shape check at all — they assume their operand's concrete type
// and would trap on a mismatch.
//
// On well-typed Mochi programs (the common case) this is the floor
// cost of any list operation. The trade-off is that the handler set
// is larger: one specialized handler per (op, shape) tuple.
//
// The package shape mirrors mig and ic so the bench harness can pull
// all three through identical entry points.
package aot

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

type VM struct{ Objects []any }

func (vm *VM) addObject(o any) int { vm.Objects = append(vm.Objects, o); return len(vm.Objects) - 1 }

// newListI64 is the AOT entry point for `new list<int>`. The
// emitter chooses it based on the static element type.
func (vm *VM) newListI64(capHint int) Cell {
	if capHint < 0 {
		capHint = 0
	}
	return CPtr(vm.addObject(&vmListI64{data: make([]int64, 0, capHint)}))
}

// newListGeneric is the AOT entry point for `new list<any>`. Not
// used by FillSum but provided for completeness — the parallel
// package layout requires both.
func (vm *VM) newListGeneric(capHint int) Cell {
	if capHint < 0 {
		capHint = 0
	}
	return CPtr(vm.addObject(&vmList{data: make([]Cell, 0, capHint)}))
}

// listPushI64 is the specialized push for *vmListI64 with an int
// value. No shape check; no value-tag check. The emitter has proven
// both statically. Would trap on a wrong-shape Cell — this is the
// "well-typed program" contract.
func (vm *VM) listPushI64(c Cell, v int64) {
	l := vm.Objects[c.Ptr()].(*vmListI64)
	l.data = append(l.data, v)
}

// listGetI64 is the specialized get for *vmListI64. Returns the
// element pre-packed as a CInt.
func (vm *VM) listGetI64(c Cell, i int64) int64 {
	return vm.Objects[c.Ptr()].(*vmListI64).data[i]
}

// listLenI64 is the specialized len for *vmListI64.
func (vm *VM) listLenI64(c Cell) int64 {
	return int64(len(vm.Objects[c.Ptr()].(*vmListI64).data))
}

// FillSum runs fill+sum through the specialized opcodes. Compare
// directly to mig.FillSum / ic.FillSum: this version has no shape
// guard on any list op.
func (vm *VM) FillSum(n int64) int64 {
	vm.Objects = vm.Objects[:0]
	xs := vm.newListI64(int(n))
	for i := int64(0); i < n; i++ {
		vm.listPushI64(xs, i)
	}
	var sum int64
	m := vm.listLenI64(xs)
	for i := int64(0); i < m; i++ {
		sum += vm.listGetI64(xs, i)
	}
	return sum
}

func NewVM() *VM { return &VM{Objects: make([]any, 0, 16)} }
