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
