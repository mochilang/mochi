package vm3

import "unsafe"

// JITListSlabStride returns the byte distance between consecutive
// vmList entries in Arenas.Lists. vm3jit bakes this as an immediate
// when lowering OpListGetI64 so a change to the vmList field layout is
// picked up automatically (Phase 6.2d.2.a step 2).
func JITListSlabStride() uintptr {
	return unsafe.Sizeof(vmList{})
}

// JITListCellsOffset returns the byte offset of the cells slice header
// within vmList. vm3jit uses it to load the slice's data pointer (the
// first 8 bytes of the slice header).
func JITListCellsOffset() uintptr {
	return unsafe.Offsetof(vmList{}.cells)
}

// JITListsBase returns an unsafe.Pointer to the first element of
// arenas.Lists, or nil if the slab is empty. vm3jit snapshots this at
// each JIT entry; reading slab i means *((*vmList)(unsafe.Pointer(base
// + i*stride))). Because Phase 6.2d.2.a only JIT-lowers read-only Cell
// ops (OpListGetI64), the snapshot stays valid for the full call;
// allocation paths (OpListPushI64) are still routed back through the
// interpreter so a slab resize cannot strand a stale base pointer.
func (a *Arenas) JITListsBase() unsafe.Pointer {
	if len(a.Lists) == 0 {
		return nil
	}
	return unsafe.Pointer(&a.Lists[0])
}
