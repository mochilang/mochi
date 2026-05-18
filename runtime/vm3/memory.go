package vm3

// Layer A of the §6.7 memory plan: per-frame arena marks.
//
// snapshotMarks captures len() of every arena slab and free-list into a
// pair of mark arrays at pushFrame. truncateToMarks slices each slab
// back to its mark on unboxed Return, and filters newly-pushed
// free-list entries that point above the mark (dangling after the
// slice). Slot records dropped by the truncation have their backing
// slice fields zeroed first so Go's GC can reclaim them.
//
// The strategy is the Tofte-Talpin (ML Kit, 1997) region model
// restricted to per-call regions. It catches the dominant allocation
// pattern of math kernels and any function that returns an unboxed
// value (i64 / f64 / SStr / bool / null). Functions that return a
// handle into the local arena range are handled by Layer B (Phase 3.5).

// snapshotMarks records every slab and free-list length into the
// caller-supplied arrays. Called from pushFrame.
func (a *Arenas) snapshotMarks(marks, freeMarks *[numArenaTags]uint32) {
	marks[ArenaString] = uint32(len(a.Strings))
	marks[ArenaList] = uint32(len(a.Lists))
	marks[ArenaMap] = uint32(len(a.Maps))
	marks[ArenaSet] = uint32(len(a.Sets))
	marks[ArenaStruct] = uint32(len(a.Structs))
	marks[ArenaClosure] = uint32(len(a.Closures))
	marks[ArenaBignum] = uint32(len(a.Bignums))
	marks[ArenaBytes] = uint32(len(a.Bytes))
	marks[ArenaPair] = uint32(len(a.Pairs))
	marks[ArenaF64Arr] = uint32(len(a.F64Arrs))
	marks[ArenaI64Arr] = uint32(len(a.I64Arrs))
	marks[ArenaU8Arr] = uint32(len(a.U8Arrs))

	freeMarks[ArenaString] = uint32(len(a.freeStrings))
	freeMarks[ArenaList] = uint32(len(a.freeLists))
	freeMarks[ArenaMap] = uint32(len(a.freeMaps))
	freeMarks[ArenaSet] = uint32(len(a.freeSets))
	freeMarks[ArenaStruct] = uint32(len(a.freeStructs))
	freeMarks[ArenaClosure] = uint32(len(a.freeClosures))
	freeMarks[ArenaBignum] = uint32(len(a.freeBignums))
	freeMarks[ArenaBytes] = uint32(len(a.freeBytes))
	freeMarks[ArenaPair] = uint32(len(a.freePairs))
	freeMarks[ArenaF64Arr] = uint32(len(a.freeF64Arrs))
	freeMarks[ArenaI64Arr] = uint32(len(a.freeI64Arrs))
	freeMarks[ArenaU8Arr] = uint32(len(a.freeU8Arrs))
}

// truncateToMarks slices every slab back to the mark recorded by
// snapshotMarks and drops dangling free-list entries. Slot records
// being discarded have their backing slice fields zeroed so Go's GC
// can reclaim them. Caller must guarantee no live handle outside the
// current frame points above any mark (this is the precondition Layer
// A relies on, satisfied automatically by an unboxed Return).
func (a *Arenas) truncateToMarks(marks, freeMarks *[numArenaTags]uint32) {
	if m := marks[ArenaString]; uint32(len(a.Strings)) > m {
		tail := a.Strings[m:]
		for i := range tail {
			tail[i].data = nil
			tail[i].flags = 0
		}
		a.Strings = a.Strings[:m]
		a.freeStrings = filterFreeList(a.freeStrings, freeMarks[ArenaString], m)
	}
	if m := marks[ArenaList]; uint32(len(a.Lists)) > m {
		tail := a.Lists[m:]
		for i := range tail {
			tail[i].cells = nil
			tail[i].flags = 0
		}
		a.Lists = a.Lists[:m]
		a.freeLists = filterFreeList(a.freeLists, freeMarks[ArenaList], m)
	}
	if m := marks[ArenaMap]; uint32(len(a.Maps)) > m {
		tail := a.Maps[m:]
		for i := range tail {
			tail[i].table = nil
			tail[i].flags = 0
		}
		a.Maps = a.Maps[:m]
		a.freeMaps = filterFreeList(a.freeMaps, freeMarks[ArenaMap], m)
	}
	if m := marks[ArenaSet]; uint32(len(a.Sets)) > m {
		tail := a.Sets[m:]
		for i := range tail {
			tail[i].table = nil
			tail[i].flags = 0
		}
		a.Sets = a.Sets[:m]
		a.freeSets = filterFreeList(a.freeSets, freeMarks[ArenaSet], m)
	}
	if m := marks[ArenaStruct]; uint32(len(a.Structs)) > m {
		tail := a.Structs[m:]
		for i := range tail {
			tail[i].fields = nil
			tail[i].flags = 0
		}
		a.Structs = a.Structs[:m]
		a.freeStructs = filterFreeList(a.freeStructs, freeMarks[ArenaStruct], m)
	}
	if m := marks[ArenaClosure]; uint32(len(a.Closures)) > m {
		tail := a.Closures[m:]
		for i := range tail {
			tail[i].upvalues = nil
			tail[i].flags = 0
		}
		a.Closures = a.Closures[:m]
		a.freeClosures = filterFreeList(a.freeClosures, freeMarks[ArenaClosure], m)
	}
	if m := marks[ArenaBignum]; uint32(len(a.Bignums)) > m {
		tail := a.Bignums[m:]
		for i := range tail {
			tail[i].words = nil
			tail[i].flags = 0
		}
		a.Bignums = a.Bignums[:m]
		a.freeBignums = filterFreeList(a.freeBignums, freeMarks[ArenaBignum], m)
	}
	if m := marks[ArenaBytes]; uint32(len(a.Bytes)) > m {
		tail := a.Bytes[m:]
		for i := range tail {
			tail[i].data = nil
			tail[i].flags = 0
		}
		a.Bytes = a.Bytes[:m]
		a.freeBytes = filterFreeList(a.freeBytes, freeMarks[ArenaBytes], m)
	}
	if m := marks[ArenaPair]; uint32(len(a.Pairs)) > m {
		tail := a.Pairs[m:]
		for i := range tail {
			tail[i].flags = 0
		}
		a.Pairs = a.Pairs[:m]
		a.freePairs = filterFreeList(a.freePairs, freeMarks[ArenaPair], m)
	}
	if m := marks[ArenaF64Arr]; uint32(len(a.F64Arrs)) > m {
		tail := a.F64Arrs[m:]
		for i := range tail {
			tail[i].data = nil
			tail[i].flags = 0
		}
		a.F64Arrs = a.F64Arrs[:m]
		a.freeF64Arrs = filterFreeList(a.freeF64Arrs, freeMarks[ArenaF64Arr], m)
	}
	if m := marks[ArenaI64Arr]; uint32(len(a.I64Arrs)) > m {
		tail := a.I64Arrs[m:]
		for i := range tail {
			tail[i].data = nil
			tail[i].flags = 0
		}
		a.I64Arrs = a.I64Arrs[:m]
		a.freeI64Arrs = filterFreeList(a.freeI64Arrs, freeMarks[ArenaI64Arr], m)
	}
	if m := marks[ArenaU8Arr]; uint32(len(a.U8Arrs)) > m {
		tail := a.U8Arrs[m:]
		for i := range tail {
			tail[i].data = nil
			tail[i].flags = 0
		}
		a.U8Arrs = a.U8Arrs[:m]
		a.freeU8Arrs = filterFreeList(a.freeU8Arrs, freeMarks[ArenaU8Arr], m)
	}
}

// filterFreeList drops entries appended after freeMark whose value is
// at or above slabMark (those slots no longer exist after truncation).
// Entries below freeMark are left untouched because they were live
// before pushFrame and still refer to slots below the mark.
func filterFreeList(free []uint32, freeMark, slabMark uint32) []uint32 {
	if uint32(len(free)) <= freeMark {
		return free
	}
	kept := free[:freeMark]
	for _, idx := range free[freeMark:] {
		if idx < slabMark {
			kept = append(kept, idx)
		}
	}
	return kept
}
