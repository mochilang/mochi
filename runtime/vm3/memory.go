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

// CallScopeMarks is an opaque arena-snapshot record. Exported so the
// JIT entry path can mirror the interp's per-frame snapshotMarks /
// truncateToMarks discipline across a trampoline call boundary, where
// the interp's Frame-resident mark arrays are not in scope. Layout
// matches the unexported per-frame fields so the existing helpers can
// fill / consume the same memory without a copy.
type CallScopeMarks struct {
	marks     [numArenaTags]uint32
	freeMarks [numArenaTags]uint32
}

// SnapshotForJITEntry captures every slab and free-list length into m.
// Pairs with RestoreUnboxedReturn on a JIT call's clean-return path:
// any arena slot allocated between the two calls (e.g. by jitCall's
// PC=0 pre-alloc OpNewList or by the JIT'd body) is released back to
// the free-list once the call returns an unboxed value. Skip the
// restore on deopt so the spilled vm.deopt* handles stay valid for the
// interpreter's resume path.
func (a *Arenas) SnapshotForJITEntry(m *CallScopeMarks) {
	a.snapshotMarks(&m.marks, &m.freeMarks)
}

// RestoreUnboxedReturn truncates each arena back to the snapshot held
// in m. Caller must guarantee the JIT'd call returned an unboxed value
// (no handle escapes from the call), mirroring Layer A's precondition
// in §6.7. The lists_fill_sum main shape (Phase 6.2d.2.b step 2) is
// safe by construction: main returns int64.
func (a *Arenas) RestoreUnboxedReturn(m *CallScopeMarks) {
	a.truncateToMarks(&m.marks, &m.freeMarks)
}

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

// Layer B of the §6.7 memory plan: handle-aware copy-up on return.
//
// handleCellReturn is the Cell-return analogue of truncateToMarks. It
// is called by OpReturnCell. The strategy:
//
//	1. If ret is unboxed (CInt / CFloat / CSStr / CBool / CNull), no
//	   handle is escaping, so truncateToMarks fires as in Layer A.
//	2. If ret is a handle pointing to a slot below the frame's mark,
//	   the slot is external (caller's or pre-frame). Truncate as in
//	   Layer A; the handle stays valid because its slot was never
//	   inside the local range.
//	3. If ret is a handle pointing to a local slot (idx >= mark for
//	   its arena), the slot's referenced cells are scanned. If any
//	   contained cell is itself a local-range handle, abort: leave
//	   the slabs intact for Layer D's mark-sweep (Phase 5).
//	4. Otherwise the slot is "leaf-like": no deep references into
//	   local arenas. Copy the slot's struct down to the mark
//	   position, rewrite the returned handle to point at the mark,
//	   and truncate the arena to mark+1.
//
// Returns the (possibly rewritten) handle Cell that the caller should
// place into the caller's retReg.
func (a *Arenas) handleCellReturn(ret Cell, marks, freeMarks *[numArenaTags]uint32) Cell {
	if !ret.IsHandle() {
		a.truncateToMarks(marks, freeMarks)
		return ret
	}
	tag, gen, idx := ret.DecodeHandle()
	mark := marks[tag]
	if idx < mark {
		a.truncateToMarks(marks, freeMarks)
		return ret
	}
	if a.containsLocalHandle(tag, idx, marks) {
		return ret
	}
	if idx != mark {
		a.moveSlot(tag, idx, mark)
	}
	marks[tag] = mark + 1
	a.truncateToMarks(marks, freeMarks)
	marks[tag] = mark
	return MakeHandle(tag, gen, mark)
}

// containsLocalHandle reports whether the slot at (tag, idx) references
// any cell whose handle points into the local range (idx >= mark for
// its own arena). Only embedded Cell fields are scanned; arenas with
// no Cell payload (String, Bytes, Bignum, F64Arr, I64Arr, U8Arr) can
// never reference local handles and return false.
func (a *Arenas) containsLocalHandle(tag ArenaTag, idx uint32, marks *[numArenaTags]uint32) bool {
	switch tag {
	case ArenaList:
		l := &a.Lists[idx]
		cells := l.cells
		if uint32(len(cells)) > l.len {
			cells = cells[:l.len]
		}
		for _, c := range cells {
			if cellIsLocal(c, marks) {
				return true
			}
		}
	case ArenaMap:
		m := &a.Maps[idx]
		for i := range m.table {
			e := &m.table[i]
			if e.hash == 0 {
				continue
			}
			if cellIsLocal(e.key, marks) || cellIsLocal(e.value, marks) {
				return true
			}
		}
	case ArenaSet:
		s := &a.Sets[idx]
		for i := range s.table {
			e := &s.table[i]
			if e.hash == 0 {
				continue
			}
			if cellIsLocal(e.key, marks) {
				return true
			}
		}
	case ArenaStruct:
		s := &a.Structs[idx]
		for _, f := range s.fields {
			if cellIsLocal(f, marks) {
				return true
			}
		}
	case ArenaClosure:
		c := &a.Closures[idx]
		for _, u := range c.upvalues {
			if cellIsLocal(u, marks) {
				return true
			}
		}
	case ArenaPair:
		p := &a.Pairs[idx]
		if cellIsLocal(p.fst, marks) || cellIsLocal(p.snd, marks) {
			return true
		}
	}
	return false
}

// cellIsLocal reports whether c is a handle into the local range of
// its arena (idx >= mark for that arena).
func cellIsLocal(c Cell, marks *[numArenaTags]uint32) bool {
	if !c.IsHandle() {
		return false
	}
	t, _, i := c.DecodeHandle()
	return i >= marks[t]
}

// moveSlot copies the slot record from slab[src] to slab[dst] for the
// given arena tag. After the copy the two slots share their backing
// slice headers, so the source's backing arrays remain reachable via
// dst once src is truncated away. Caller must guarantee dst < src
// (so the truncation drops src, not dst).
func (a *Arenas) moveSlot(tag ArenaTag, src, dst uint32) {
	switch tag {
	case ArenaString:
		a.Strings[dst] = a.Strings[src]
	case ArenaList:
		a.Lists[dst] = a.Lists[src]
	case ArenaMap:
		a.Maps[dst] = a.Maps[src]
	case ArenaSet:
		a.Sets[dst] = a.Sets[src]
	case ArenaStruct:
		a.Structs[dst] = a.Structs[src]
	case ArenaClosure:
		a.Closures[dst] = a.Closures[src]
	case ArenaBignum:
		a.Bignums[dst] = a.Bignums[src]
	case ArenaBytes:
		a.Bytes[dst] = a.Bytes[src]
	case ArenaPair:
		a.Pairs[dst] = a.Pairs[src]
	case ArenaF64Arr:
		a.F64Arrs[dst] = a.F64Arrs[src]
	case ArenaI64Arr:
		a.I64Arrs[dst] = a.I64Arrs[src]
	case ArenaU8Arr:
		a.U8Arrs[dst] = a.U8Arrs[src]
	}
}
