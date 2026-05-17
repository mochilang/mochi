package vm2

// vmList is the heap representation of a list (MEP-24 §3). Element type
// specialization is intentionally deferred: every slot is a Cell so int
// elements pay the tag overhead. A follow-on MEP gated on profile
// evidence can introduce typed list storage (e.g. []int64) without
// changing the opcode contract.
//
// data is the live element storage; cap(data) doubles as the allocated
// capacity for amortized O(1) push.
type vmList struct {
	data []Cell
}

// newList allocates a *vmList with the given capacity and registers it
// in the VM's Objects table. The returned Cell is a tagPtr.
func (vm *VM) newList(capHint int) Cell {
	if capHint < 0 {
		capHint = 0
	}
	return CPtr(vm.AddObject(&vmList{data: make([]Cell, 0, capHint)}))
}

// listAt fetches the *vmList backing a tagPtr cell. Caller must have
// verified the cell carries a list; the typed opcodes guarantee that
// statically.
func (vm *VM) listAt(c Cell) *vmList {
	return vm.Objects[c.Ptr()].(*vmList)
}

// JIT slow-path accessors — called by runtime/jit/vm2jit for list opcodes.
// These wrap the unexported vmList primitives with exported signatures.

// JITNewList allocates a new list with the given capacity hint and returns
// its Cell. Equivalent to OpNewList.
func JITNewList(vm *VM, capHint int64) Cell { return vm.newList(int(capHint)) }

// JITListLen returns CInt(len(list)) for the list at listCell.
func JITListLen(vm *VM, listCell Cell) Cell {
	return CInt(int64(len(vm.listAt(listCell).data)))
}

// JITListGet returns list[index]. Panics on out-of-bounds.
func JITListGet(vm *VM, listCell Cell, indexCell Cell) Cell {
	l := vm.listAt(listCell)
	i := indexCell.Int()
	if i < 0 || i >= int64(len(l.data)) {
		panic("vm2/jit: list index out of range")
	}
	return l.data[i]
}

// JITListSet writes value to list[index]. Panics on out-of-bounds.
func JITListSet(vm *VM, listCell Cell, indexCell Cell, valueCell Cell) {
	l := vm.listAt(listCell)
	i := indexCell.Int()
	if i < 0 || i >= int64(len(l.data)) {
		panic("vm2/jit: list index out of range")
	}
	l.data[i] = valueCell
}

// JITListPush appends value to the list at listCell. Amortised O(1).
func JITListPush(vm *VM, listCell Cell, valueCell Cell) {
	l := vm.listAt(listCell)
	l.data = append(l.data, valueCell)
}
