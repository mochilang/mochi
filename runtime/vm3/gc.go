package vm3

// Layer D of the §6.7 memory plan: stop-the-world mark-sweep over the
// VM's arenas. Reclaims slots that Layers A, B, and (future) C miss,
// notably historical return values held by past frames whose handles
// have since escaped reachability.
//
// Algorithm: classic Cheney-style mark-sweep restricted to a fixed,
// per-tag slab universe. Mark phase walks every root Cell, sets
// flagMarked on the reached slot, and recurses through embedded Cell
// fields (list cells, map/set table entries, struct fields, closure
// upvalues, pair fst/snd). Sweep phase walks every slot in every
// arena: alive+marked stays alive (mark bit cleared); alive+unmarked
// is freed (gen bump, backing slice nil'd, slot pushed onto the
// arena's free list); dead slots are skipped (already on free list).
//
// Roots:
//
//   1. Every live frame's regsCell window. The union of those windows
//      is exactly vm.stackCell[0 : len(vm.stackCell)]; the interpreter
//      slices the stack back to the high-water mark on every Return,
//      so iterating the slice walks only currently-live frames.
//   2. Every loaded Function.Consts slice. Const pool entries may hold
//      a handle into ArenaString or similar; constant strings allocated
//      at program-load time must survive collection.
//
// Phase 5 v1 ships a manual Collect entry point only. Auto-triggering
// from alloc pressure is a follow-on (Phase 5.1) once a representative
// program demonstrates the policy choice. Manual collection between
// Runs is sufficient for the reused-VM benchmark pattern where every
// Cell from a previous Run has gone out of scope by the time the next
// Run starts.

// Collect runs a stop-the-world mark-sweep over the VM's arenas.
// Every reachable arena slot (reachable from any live frame's regs
// or from a Function.Consts entry) retains its flagAlive. Every
// alive-but-unreached slot is pushed onto its arena's free list with
// its generation counter bumped.
//
// Collect must be called between vm.Run invocations or from a safe
// point where the interpreter holds no transient handles outside
// vm.stackCell. The current shipped path is "called by tests and
// benches between Runs"; auto-triggered collection is Phase 5.1.
func (vm *VM) Collect() {
	a := &vm.arenas
	for _, c := range vm.stackCell {
		a.markCell(c)
	}
	if vm.prog != nil {
		for _, fn := range vm.prog.Funcs {
			for _, c := range fn.Consts {
				a.markCell(c)
			}
		}
	}
	a.sweep()
}

// markCell marks the slot referenced by c (when c is a handle) and
// recursively marks its embedded Cell fields. Already-marked slots
// short-circuit, so cycles in the handle graph terminate.
func (a *Arenas) markCell(c Cell) {
	if !c.IsHandle() {
		return
	}
	tag, _, idx := c.DecodeHandle()
	switch tag {
	case ArenaString:
		s := &a.Strings[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	case ArenaList:
		s := &a.Lists[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		cells := s.cells
		if uint32(len(cells)) > s.len {
			cells = cells[:s.len]
		}
		for _, child := range cells {
			a.markCell(child)
		}
	case ArenaMap:
		s := &a.Maps[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		for i := range s.table {
			e := &s.table[i]
			if e.hash == 0 {
				continue
			}
			a.markCell(e.key)
			a.markCell(e.value)
		}
	case ArenaSet:
		s := &a.Sets[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		for i := range s.table {
			e := &s.table[i]
			if e.hash == 0 {
				continue
			}
			a.markCell(e.key)
		}
	case ArenaStruct:
		s := &a.Structs[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		for _, f := range s.fields {
			a.markCell(f)
		}
	case ArenaClosure:
		s := &a.Closures[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		for _, u := range s.upvalues {
			a.markCell(u)
		}
	case ArenaBignum:
		s := &a.Bignums[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	case ArenaBytes:
		s := &a.Bytes[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	case ArenaPair:
		s := &a.Pairs[idx]
		if s.flags&flagAlive == 0 || s.flags&flagMarked != 0 {
			return
		}
		s.flags |= flagMarked
		a.markCell(s.fst)
		a.markCell(s.snd)
	case ArenaF64Arr:
		s := &a.F64Arrs[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	case ArenaI64Arr:
		s := &a.I64Arrs[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	case ArenaU8Arr:
		s := &a.U8Arrs[idx]
		if s.flags&flagAlive == 0 {
			return
		}
		s.flags |= flagMarked
	}
}

// sweep walks every slot in every arena. Alive+marked slots have the
// mark bit cleared and stay alive; alive+unmarked slots are freed
// (gen bump, backing slice nil'd, pushed onto the arena's free list);
// dead slots are skipped.
func (a *Arenas) sweep() {
	for i := range a.Strings {
		s := &a.Strings[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.data = nil
		s.gen++
		a.freeStrings = append(a.freeStrings, uint32(i))
	}
	for i := range a.Lists {
		s := &a.Lists[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.cells = nil
		s.gen++
		a.freeLists = append(a.freeLists, uint32(i))
	}
	for i := range a.Maps {
		s := &a.Maps[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.table = nil
		s.gen++
		a.freeMaps = append(a.freeMaps, uint32(i))
	}
	for i := range a.Sets {
		s := &a.Sets[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.table = nil
		s.gen++
		a.freeSets = append(a.freeSets, uint32(i))
	}
	for i := range a.Structs {
		s := &a.Structs[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.fields = nil
		s.gen++
		a.freeStructs = append(a.freeStructs, uint32(i))
	}
	for i := range a.Closures {
		s := &a.Closures[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.upvalues = nil
		s.gen++
		a.freeClosures = append(a.freeClosures, uint32(i))
	}
	for i := range a.Bignums {
		s := &a.Bignums[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.words = nil
		s.gen++
		a.freeBignums = append(a.freeBignums, uint32(i))
	}
	for i := range a.Bytes {
		s := &a.Bytes[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.data = nil
		s.gen++
		a.freeBytes = append(a.freeBytes, uint32(i))
	}
	for i := range a.Pairs {
		s := &a.Pairs[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.gen++
		a.freePairs = append(a.freePairs, uint32(i))
	}
	for i := range a.F64Arrs {
		s := &a.F64Arrs[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.data = nil
		s.gen++
		a.freeF64Arrs = append(a.freeF64Arrs, uint32(i))
	}
	for i := range a.I64Arrs {
		s := &a.I64Arrs[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.data = nil
		s.gen++
		a.freeI64Arrs = append(a.freeI64Arrs, uint32(i))
	}
	for i := range a.U8Arrs {
		s := &a.U8Arrs[i]
		if s.flags&flagAlive == 0 {
			continue
		}
		if s.flags&flagMarked != 0 {
			s.flags &^= flagMarked
			continue
		}
		s.flags &^= flagAlive
		s.data = nil
		s.gen++
		a.freeU8Arrs = append(a.freeU8Arrs, uint32(i))
	}
}
