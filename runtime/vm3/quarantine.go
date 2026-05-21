package vm3

// Quarantine widens the ABA reuse window for slots whose generation
// counter is approaching the 12-bit wrap point. Without quarantine, a
// slot freed at gen=4094 gets its gen bumped to 4095 on the next
// reuse and to 0 on the reuse after that; an escaped handle with
// remembered gen=0 then re-aliases the slot exactly when its slab
// re-allocation happens to fall into that window. The quarantine
// queue holds wrap-prone slot indices out of the free list for
// WrapQuarantineDepth additional same-arena allocations, so the
// effective ABA window grows by that factor.
//
// Design choices:
//
//   - Per-arena quarantine. The risk model is per-arena because each
//     arena has its own free list and gen counters; mixing arenas
//     would create false-sharing of slab indices.
//
//   - Threshold-gated. Slots whose gen has not yet reached
//     WrapWarn use the existing free-list LIFO path (zero overhead).
//     Only wrap-prone slots take the quarantine slow path. The
//     hot-loop benchmark suite is structurally unaffected because no
//     benchmark in tree pushes any slot past gen=64.
//
//   - FIFO drain. The quarantine is a FIFO ring; once it fills, the
//     oldest entry drains to the regular free list. This guarantees
//     a wrap-prone slot's reuse is delayed by at least
//     WrapQuarantineDepth same-arena allocations, which is the
//     security property the MEP-41 §6.7 quarantine table names.
//
// Operational note: WrapWarn is set close to the wrap point (4032)
// rather than e.g. 2048 because the practical wrap risk only arises
// after thousands of free/reuse cycles on the same slot, and a more
// aggressive threshold would force test cases that currently exercise
// gen=1..16 into the quarantine path for no security gain.
//
// Configuration: the per-VM defaults below can be overridden via
// (a *Arenas).SetQuarantineConfig(warn, depth). Setting depth = 0
// disables quarantine for benchmarks and stress tests that want raw
// LIFO behavior. Tests do this in their setup; production VMs leave
// the defaults in place.
const (
	// DefaultWrapWarn is the slot-gen threshold at which a Free
	// pushes into the wrap-quarantine instead of the regular free
	// list. The 12-bit gen field wraps at 4096, so 4032 reserves the
	// final 64 generations for wrap-aware reuse only.
	DefaultWrapWarn uint16 = 4032

	// DefaultWrapQuarantineDepth is the FIFO depth of the
	// wrap-quarantine ring. A wrap-prone slot is held out of the
	// free list for at least this many same-arena allocations before
	// reuse.
	DefaultWrapQuarantineDepth uint32 = 64
)

// wrapQuarantine is the per-arena FIFO ring. Index 0 is the head
// (oldest entry); appends go to the tail. Drain pops the head into
// the matching freeXXX free list.
type wrapQuarantine struct {
	ring [numArenaTags][]uint32
}

func (q *wrapQuarantine) push(tag ArenaTag, idx uint32) {
	q.ring[tag] = append(q.ring[tag], idx)
}

func (q *wrapQuarantine) drainHead(tag ArenaTag) (uint32, bool) {
	r := q.ring[tag]
	if len(r) == 0 {
		return 0, false
	}
	head := r[0]
	q.ring[tag] = r[1:]
	return head, true
}

func (q *wrapQuarantine) depth(tag ArenaTag) int {
	return len(q.ring[tag])
}

// quarantineConfig holds the per-VM tunable thresholds. Stored on
// Arenas so a long-running VM can adjust at startup without recompile.
type quarantineConfig struct {
	WrapWarn  uint16
	WrapDepth uint32
}

// SetQuarantineConfig overrides the per-arena wrap thresholds.
// Calling with depth=0 disables the quarantine slow-path
// (benchmarks). Calling without a prior SetQuarantineConfig uses
// the package defaults.
func (a *Arenas) SetQuarantineConfig(warn uint16, depth uint32) {
	a.qcfg = quarantineConfig{WrapWarn: warn, WrapDepth: depth}
}

// quarantineWrapWarn returns the active WrapWarn threshold for this
// Arenas instance, falling back to the package default if not
// explicitly set.
func (a *Arenas) quarantineWrapWarn() uint16 {
	if a.qcfg.WrapWarn == 0 && a.qcfg.WrapDepth == 0 {
		return DefaultWrapWarn
	}
	return a.qcfg.WrapWarn
}

// quarantineDepth returns the active WrapDepth for this Arenas
// instance, falling back to the package default if not set.
func (a *Arenas) quarantineDepth() uint32 {
	if a.qcfg.WrapWarn == 0 && a.qcfg.WrapDepth == 0 {
		return DefaultWrapQuarantineDepth
	}
	return a.qcfg.WrapDepth
}

// slotGenForFree returns the current generation counter of the slot
// at (tag, idx). Used by Free to decide whether to take the
// quarantine slow-path. Returns 0 if tag is unknown (defensive; the
// caller has already validated the tag from DecodeHandle).
func (a *Arenas) slotGenForFree(tag ArenaTag, idx uint32) uint16 {
	switch tag {
	case ArenaString:
		return a.Strings[idx].gen
	case ArenaList:
		return a.Lists[idx].gen
	case ArenaMap:
		return a.Maps[idx].gen
	case ArenaSet:
		return a.Sets[idx].gen
	case ArenaStruct:
		return a.Structs[idx].gen
	case ArenaClosure:
		return a.Closures[idx].gen
	case ArenaBignum:
		return a.Bignums[idx].gen
	case ArenaBytes:
		return a.Bytes[idx].gen
	case ArenaPair:
		return a.Pairs[idx].gen
	case ArenaF64Arr:
		return a.F64Arrs[idx].gen
	case ArenaI64Arr:
		return a.I64Arrs[idx].gen
	case ArenaU8Arr:
		return a.U8Arrs[idx].gen
	}
	return 0
}

// pushToFreeList appends idx to the per-arena free list keyed by
// tag. Used both by the direct-free path and by the quarantine
// drain path. Centralized so the dozen switch arms in Free() don't
// duplicate the append logic.
func (a *Arenas) pushToFreeList(tag ArenaTag, idx uint32) {
	switch tag {
	case ArenaString:
		a.freeStrings = append(a.freeStrings, idx)
	case ArenaList:
		a.freeLists = append(a.freeLists, idx)
	case ArenaMap:
		a.freeMaps = append(a.freeMaps, idx)
	case ArenaSet:
		a.freeSets = append(a.freeSets, idx)
	case ArenaStruct:
		a.freeStructs = append(a.freeStructs, idx)
	case ArenaClosure:
		a.freeClosures = append(a.freeClosures, idx)
	case ArenaBignum:
		a.freeBignums = append(a.freeBignums, idx)
	case ArenaBytes:
		a.freeBytes = append(a.freeBytes, idx)
	case ArenaPair:
		a.freePairs = append(a.freePairs, idx)
	case ArenaF64Arr:
		a.freeF64Arrs = append(a.freeF64Arrs, idx)
	case ArenaI64Arr:
		a.freeI64Arrs = append(a.freeI64Arrs, idx)
	case ArenaU8Arr:
		a.freeU8Arrs = append(a.freeU8Arrs, idx)
	}
}

// FreeWithQuarantine is the quarantine-aware free entry. It is the
// implementation the public (a *Arenas).Free wraps; callers can
// invoke it directly to short-circuit the quarantine check (e.g.
// internal compaction paths where the slot is known to be safe for
// immediate reuse).
//
// Behavior:
//
//   - If the slot's current gen is below the WrapWarn threshold,
//     the slot is appended to the regular free list (LIFO reuse).
//   - If the slot's current gen is at or above WrapWarn, the slot
//     is pushed to the wrap-quarantine FIFO. If the FIFO depth then
//     exceeds WrapQuarantineDepth, the oldest quarantined entry
//     drains to the regular free list, so the ring's working depth
//     is bounded.
//
// The slot's backing slice and flagAlive bit are still cleared at
// the calling site (Free), so the quarantine only affects when the
// slot's index becomes available for reuse, not whether the slot's
// payload is reachable.
func (a *Arenas) routeToFreeOrQuarantine(tag ArenaTag, idx uint32) {
	warn := a.quarantineWrapWarn()
	depth := a.quarantineDepth()
	if depth == 0 || a.slotGenForFree(tag, idx) < warn {
		a.pushToFreeList(tag, idx)
		return
	}
	a.wrapQ.push(tag, idx)
	if uint32(a.wrapQ.depth(tag)) > depth {
		if head, ok := a.wrapQ.drainHead(tag); ok {
			a.pushToFreeList(tag, head)
		}
	}
}
