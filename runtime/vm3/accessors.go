package vm3

// Accessors below project arena handles back into typed views for
// callers (interpreter loop, JIT). Every accessor asserts the arena
// tag matches; the type system normally proves this, but assertion
// catches compiler3 bugs early.

// StringBytes returns the raw bytes backing the string handle c.
func (a *Arenas) StringBytes(c Cell) []byte {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaString {
		return nil
	}
	return a.Strings[idx].data
}

// ListLen returns the logical length of the list handle c.
func (a *Arenas) ListLen(c Cell) int {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaList {
		return 0
	}
	return int(a.Lists[idx].len)
}

// ListAppend appends v to the list handle c, growing the backing
// slice as needed. Returns the new logical length.
func (a *Arenas) ListAppend(c Cell, v Cell) int {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaList {
		return 0
	}
	l := &a.Lists[idx]
	l.cells = append(l.cells, v)
	l.len = uint32(len(l.cells))
	return int(l.len)
}

// ListGet returns the i'th element of list handle c.
func (a *Arenas) ListGet(c Cell, i int) Cell {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaList {
		return CNull()
	}
	return a.Lists[idx].cells[i]
}

// ListSet writes v into the i'th element of list handle c.
func (a *Arenas) ListSet(c Cell, i int, v Cell) {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaList {
		return
	}
	a.Lists[idx].cells[i] = v
}

// StructField returns the field at fieldIdx of struct handle c.
func (a *Arenas) StructField(c Cell, fieldIdx int) Cell {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaStruct {
		return CNull()
	}
	return a.Structs[idx].fields[fieldIdx]
}

// StructSetField writes v into the fieldIdx'th field of struct handle c.
func (a *Arenas) StructSetField(c Cell, fieldIdx int, v Cell) {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaStruct {
		return
	}
	a.Structs[idx].fields[fieldIdx] = v
}

// PairFst returns the first cell of pair handle c.
func (a *Arenas) PairFst(c Cell) Cell {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaPair {
		return CNull()
	}
	return a.Pairs[idx].fst
}

// PairSnd returns the second cell of pair handle c.
func (a *Arenas) PairSnd(c Cell) Cell {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaPair {
		return CNull()
	}
	return a.Pairs[idx].snd
}

// I64Arr returns the backing slice of i64-array handle c.
func (a *Arenas) I64Arr(c Cell) []int64 {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaI64Arr {
		return nil
	}
	return a.I64Arrs[idx].data
}

// F64Arr returns the backing slice of f64-array handle c.
func (a *Arenas) F64Arr(c Cell) []float64 {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaF64Arr {
		return nil
	}
	return a.F64Arrs[idx].data
}

// U8Arr returns the backing slice of u8-array handle c.
func (a *Arenas) U8Arr(c Cell) []byte {
	tag, _, idx := c.DecodeHandle()
	if tag != ArenaU8Arr {
		return nil
	}
	return a.U8Arrs[idx].data
}

// Free returns slot idx of arena tag to the free list. Callers must
// guarantee no live handles reference the slot; debug-mode generation
// checks (Phase 6) catch violations.
func (a *Arenas) Free(c Cell) {
	tag, _, idx := c.DecodeHandle()
	switch tag {
	case ArenaString:
		a.Strings[idx].flags &^= flagAlive
		a.Strings[idx].data = nil
		a.freeStrings = append(a.freeStrings, idx)
	case ArenaList:
		a.Lists[idx].flags &^= flagAlive
		a.Lists[idx].cells = nil
		a.freeLists = append(a.freeLists, idx)
	case ArenaMap:
		a.Maps[idx].flags &^= flagAlive
		a.Maps[idx].table = nil
		a.freeMaps = append(a.freeMaps, idx)
	case ArenaSet:
		a.Sets[idx].flags &^= flagAlive
		a.Sets[idx].table = nil
		a.freeSets = append(a.freeSets, idx)
	case ArenaStruct:
		a.Structs[idx].flags &^= flagAlive
		a.Structs[idx].fields = nil
		a.freeStructs = append(a.freeStructs, idx)
	case ArenaClosure:
		a.Closures[idx].flags &^= flagAlive
		a.Closures[idx].upvalues = nil
		a.freeClosures = append(a.freeClosures, idx)
	case ArenaBignum:
		a.Bignums[idx].flags &^= flagAlive
		a.Bignums[idx].words = nil
		a.freeBignums = append(a.freeBignums, idx)
	case ArenaBytes:
		a.Bytes[idx].flags &^= flagAlive
		a.Bytes[idx].data = nil
		a.freeBytes = append(a.freeBytes, idx)
	case ArenaPair:
		a.Pairs[idx].flags &^= flagAlive
		a.freePairs = append(a.freePairs, idx)
	case ArenaF64Arr:
		a.F64Arrs[idx].flags &^= flagAlive
		a.F64Arrs[idx].data = nil
		a.freeF64Arrs = append(a.freeF64Arrs, idx)
	case ArenaI64Arr:
		a.I64Arrs[idx].flags &^= flagAlive
		a.I64Arrs[idx].data = nil
		a.freeI64Arrs = append(a.freeI64Arrs, idx)
	case ArenaU8Arr:
		a.U8Arrs[idx].flags &^= flagAlive
		a.U8Arrs[idx].data = nil
		a.freeU8Arrs = append(a.freeU8Arrs, idx)
	}
}

// TotalSlots reports how many slots (alive + free) arena tag t currently
// holds. Useful for monitoring monotonic arena growth in long-running VMs
// before Phase 6 GC lands.
func (a *Arenas) TotalSlots(t ArenaTag) int {
	switch t {
	case ArenaString:
		return len(a.Strings)
	case ArenaList:
		return len(a.Lists)
	case ArenaMap:
		return len(a.Maps)
	case ArenaSet:
		return len(a.Sets)
	case ArenaStruct:
		return len(a.Structs)
	case ArenaClosure:
		return len(a.Closures)
	case ArenaBignum:
		return len(a.Bignums)
	case ArenaBytes:
		return len(a.Bytes)
	case ArenaPair:
		return len(a.Pairs)
	case ArenaF64Arr:
		return len(a.F64Arrs)
	case ArenaI64Arr:
		return len(a.I64Arrs)
	case ArenaU8Arr:
		return len(a.U8Arrs)
	}
	return 0
}

// Reset returns every slot to the free state and clears backing slices.
// Intended for benchmarks and tests that reuse a VM across many program
// runs and want bounded memory without Phase 6 GC. Production code should
// instead let the Phase 6 mark-sweep collector retire dead slots.
func (a *Arenas) Reset() {
	a.Strings = a.Strings[:0]
	a.Lists = a.Lists[:0]
	a.Maps = a.Maps[:0]
	a.Sets = a.Sets[:0]
	a.Structs = a.Structs[:0]
	a.Closures = a.Closures[:0]
	a.Bignums = a.Bignums[:0]
	a.Bytes = a.Bytes[:0]
	a.Pairs = a.Pairs[:0]
	a.F64Arrs = a.F64Arrs[:0]
	a.I64Arrs = a.I64Arrs[:0]
	a.U8Arrs = a.U8Arrs[:0]
	a.freeStrings = a.freeStrings[:0]
	a.freeLists = a.freeLists[:0]
	a.freeMaps = a.freeMaps[:0]
	a.freeSets = a.freeSets[:0]
	a.freeStructs = a.freeStructs[:0]
	a.freeClosures = a.freeClosures[:0]
	a.freeBignums = a.freeBignums[:0]
	a.freeBytes = a.freeBytes[:0]
	a.freePairs = a.freePairs[:0]
	a.freeF64Arrs = a.freeF64Arrs[:0]
	a.freeI64Arrs = a.freeI64Arrs[:0]
	a.freeU8Arrs = a.freeU8Arrs[:0]
}

// LiveSlots reports how many alive slots arena tag t currently holds.
func (a *Arenas) LiveSlots(t ArenaTag) int {
	switch t {
	case ArenaString:
		return len(a.Strings) - len(a.freeStrings)
	case ArenaList:
		return len(a.Lists) - len(a.freeLists)
	case ArenaMap:
		return len(a.Maps) - len(a.freeMaps)
	case ArenaSet:
		return len(a.Sets) - len(a.freeSets)
	case ArenaStruct:
		return len(a.Structs) - len(a.freeStructs)
	case ArenaClosure:
		return len(a.Closures) - len(a.freeClosures)
	case ArenaBignum:
		return len(a.Bignums) - len(a.freeBignums)
	case ArenaBytes:
		return len(a.Bytes) - len(a.freeBytes)
	case ArenaPair:
		return len(a.Pairs) - len(a.freePairs)
	case ArenaF64Arr:
		return len(a.F64Arrs) - len(a.freeF64Arrs)
	case ArenaI64Arr:
		return len(a.I64Arrs) - len(a.freeI64Arrs)
	case ArenaU8Arr:
		return len(a.U8Arrs) - len(a.freeU8Arrs)
	}
	return 0
}
