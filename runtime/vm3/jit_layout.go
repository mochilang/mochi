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

// JITMapSlabStride returns the byte distance between consecutive vmMap
// entries in Arenas.Maps. vm3jit bakes this as an immediate when
// lowering OpMapSetI64I64 / OpMapGetI64I64 (Phase 6.2d.2.d step 4).
func JITMapSlabStride() uintptr {
	return unsafe.Sizeof(vmMap{})
}

// JITMapTableOffset returns the byte offset of the table slice header
// within vmMap. The slice header at this offset is laid out as
// {ptr u64, len u64, cap u64} per Go's slice header convention.
func JITMapTableOffset() uintptr {
	return unsafe.Offsetof(vmMap{}.table)
}

// JITMapNLiveOffset returns the byte offset of vmMap.nLive (a u32
// counter of live entries). vm3jit bakes this offset into the inline
// grow-check and the post-fill nLive bump for OpMapSetI64I64.
func JITMapNLiveOffset() uintptr {
	return unsafe.Offsetof(vmMap{}.nLive)
}

// JITMapEntryStride returns the byte distance between consecutive
// mapEntry records in a map's table backing array.
func JITMapEntryStride() uintptr {
	return unsafe.Sizeof(mapEntry{})
}

// JITMapEntryHashOffset returns the byte offset of mapEntry.hash within
// a mapEntry record.
func JITMapEntryHashOffset() uintptr {
	return unsafe.Offsetof(mapEntry{}.hash)
}

// JITMapEntryKeyOffset returns the byte offset of mapEntry.key within
// a mapEntry record. The Cell payload is a NaN-boxed int48 for i64
// keys; the JIT extracts the int48 via SBFX.
func JITMapEntryKeyOffset() uintptr {
	return unsafe.Offsetof(mapEntry{}.key)
}

// JITMapEntryValueOffset returns the byte offset of mapEntry.value
// within a mapEntry record.
func JITMapEntryValueOffset() uintptr {
	return unsafe.Offsetof(mapEntry{}.value)
}

// JITMapsBase returns an unsafe.Pointer to the first element of
// arenas.Maps, or nil if the slab is empty. Mirror of JITListsBase for
// the map slab: vm3jit snapshots this at each JIT entry into the
// jitArenaCtx so the prologue can hoist &Maps[idx] into a pinned reg
// for OpMapSetI64I64/OpMapGetI64I64 lowering.
func (a *Arenas) JITMapsBase() unsafe.Pointer {
	if len(a.Maps) == 0 {
		return nil
	}
	return unsafe.Pointer(&a.Maps[0])
}
