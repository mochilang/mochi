package vm3

import (
	"testing"
)

// FuzzAllocFree is the MEP-41 Phase 6 fuzz harness for the runtime's
// allocator + free-list + quarantine path. Each iteration interprets
// the input bytes as a sequence of (op, arena, payload) triples and
// applies them to a fresh Arenas instance.
//
// The encoding (1 byte per field) is intentionally lossy: the fuzzer
// produces an arbitrary byte stream and the harness maps each byte
// into the small space of legal ops / arenas / sizes. The harness
// does not need to round-trip; its job is to surface panics or
// gen-monotonicity violations on adversarial sequences.
//
// Triple encoding (3 bytes per step):
//
//	byte 0 (op):     0 = alloc, 1 = free, 2 = SetQuarantineConfig wiggle
//	byte 1 (arena):  ArenaTag, modulo 12 (the 12 named arenas)
//	byte 2 (param):  alloc => size hint (0..255); free => slot index
//	                 (modulo the arena's current slab len);
//	                 qconfig => warn-low bit (toggle depth = 0 to disable
//	                 quarantine, or DefaultWrapWarn / DefaultWrapQuarantineDepth).
//
// Assertions:
//
//   - No panic on any input. The allocator paths are the structural
//     core of memory safety; a panic here would mean the arena state
//     can be driven into a corrupt shape by valid Cell traffic.
//
//   - Gen monotonicity. For each (tag, idx) seen, the slot's gen field
//     after the step must be >= the value observed before, allowing
//     for the 12-bit wrap. Quarantine widens the effective ABA window
//     past the wrap; the assertion still holds modulo the wrap.
//
//   - Allocator self-consistency. After every alloc, the returned
//     Cell decodes to (tag, gen, idx) with idx < TotalSlots(tag) and
//     the slot's flagAlive bit set. After every Free, the slot's
//     flagAlive bit is cleared.
//
// Seed corpus: a handful of small representative byte strings hitting
// each arena. The fuzzer mutates from these.
func FuzzAllocFree(f *testing.F) {
	f.Add([]byte{0, 0, 8, 0, 1, 4, 0, 2, 0, 0, 4, 0})    // alloc string, list, map, struct
	f.Add([]byte{0, 9, 16, 0, 10, 8, 0, 11, 4})          // alloc f64arr, i64arr, u8arr
	f.Add([]byte{0, 1, 8, 1, 1, 0, 0, 1, 4})             // alloc + free + realloc on list
	f.Add([]byte{2, 0, 0, 0, 0, 4, 1, 0, 0, 0, 0, 8})    // toggle quarantine + alloc + free + alloc
	f.Add([]byte{0, 0, 1, 0, 0, 2, 0, 0, 3, 1, 0, 0})    // many string allocs + one free

	f.Fuzz(func(t *testing.T, data []byte) {
		if len(data) < 3 {
			return
		}
		a := &Arenas{}
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("Arenas op-stream panic: %v", r)
			}
		}()
		genSeen := make(map[uint64]uint16)
		for off := 0; off+3 <= len(data); off += 3 {
			op := data[off] % 3
			tag := ArenaTag(data[off+1] % 12)
			param := data[off+2]
			switch op {
			case 0:
				c := fuzzAlloc(a, tag, int(param))
				if !c.IsHandle() {
					continue
				}
				gotTag, gen, idx := c.DecodeHandle()
				if gotTag != tag {
					t.Errorf("alloc(tag=%d): returned tag=%d", tag, gotTag)
					continue
				}
				if idx >= uint32(a.TotalSlots(tag)) {
					t.Errorf("alloc(tag=%d): idx %d out of slab len %d", tag, idx, a.TotalSlots(tag))
				}
				if !fuzzSlotAlive(a, tag, idx) {
					t.Errorf("alloc(tag=%d, idx=%d): flagAlive not set after alloc", tag, idx)
				}
				key := uint64(tag)<<32 | uint64(idx)
				prev, ok := genSeen[key]
				if ok {
					// Gen wraps at 4096 (12-bit field); accept either
					// strict-greater or the wrap-around step.
					if gen < prev && (uint32(prev)+1)&0xFFF != uint32(gen) {
						t.Errorf("alloc(tag=%d, idx=%d): gen %d -> %d non-monotone (no wrap)", tag, idx, prev, gen)
					}
				}
				genSeen[key] = gen
			case 1:
				n := a.TotalSlots(tag)
				if n == 0 {
					continue
				}
				idx := uint32(int(param) % n)
				if !fuzzSlotAlive(a, tag, idx) {
					// Already-free is legal; the harness does not
					// track liveness precisely. The Arenas Free is
					// idempotent at the flag level.
					continue
				}
				c := MakeHandle(tag, fuzzSlotGen(a, tag, idx), idx)
				a.Free(c)
				if fuzzSlotAlive(a, tag, idx) {
					t.Errorf("free(tag=%d, idx=%d): flagAlive still set after Free", tag, idx)
				}
			case 2:
				// Wiggle the quarantine config: even param disables,
				// odd re-enables with the default depth. Either way
				// the allocator must remain consistent.
				if param&1 == 0 {
					a.SetQuarantineConfig(DefaultWrapWarn, 0)
				} else {
					a.SetQuarantineConfig(DefaultWrapWarn, DefaultWrapQuarantineDepth)
				}
			}
		}
	})
}

// fuzzAlloc dispatches an alloc call to the right per-arena entry
// point. The size hint is bounded so a single fuzz step cannot
// exhaust process memory.
func fuzzAlloc(a *Arenas, tag ArenaTag, sizeHint int) Cell {
	const cap = 16
	n := min(sizeHint, cap)
	switch tag {
	case ArenaString:
		return a.AllocString(make([]byte, n))
	case ArenaList:
		return a.AllocList(0, n)
	case ArenaMap:
		return a.AllocMap(n)
	case ArenaSet:
		return a.AllocSet(n)
	case ArenaStruct:
		return a.AllocStruct(0, n)
	case ArenaClosure:
		return a.AllocClosure(0, n)
	case ArenaBignum:
		return a.AllocBignum(0, n)
	case ArenaBytes:
		return a.AllocBytes(make([]byte, n))
	case ArenaPair:
		return a.AllocPair(CNull(), CNull())
	case ArenaF64Arr:
		return a.AllocF64Arr(n)
	case ArenaI64Arr:
		return a.AllocI64Arr(n)
	case ArenaU8Arr:
		return a.AllocU8Arr(n)
	}
	return 0
}

// fuzzSlotAlive reports whether the slot at (tag, idx) currently has
// flagAlive set. Used by the fuzz harness to spot-check the post-
// condition of alloc and Free.
func fuzzSlotAlive(a *Arenas, tag ArenaTag, idx uint32) bool {
	switch tag {
	case ArenaString:
		return a.Strings[idx].flags&flagAlive != 0
	case ArenaList:
		return a.Lists[idx].flags&flagAlive != 0
	case ArenaMap:
		return a.Maps[idx].flags&flagAlive != 0
	case ArenaSet:
		return a.Sets[idx].flags&flagAlive != 0
	case ArenaStruct:
		return a.Structs[idx].flags&flagAlive != 0
	case ArenaClosure:
		return a.Closures[idx].flags&flagAlive != 0
	case ArenaBignum:
		return a.Bignums[idx].flags&flagAlive != 0
	case ArenaBytes:
		return a.Bytes[idx].flags&flagAlive != 0
	case ArenaPair:
		return a.Pairs[idx].flags&flagAlive != 0
	case ArenaF64Arr:
		return a.F64Arrs[idx].flags&flagAlive != 0
	case ArenaI64Arr:
		return a.I64Arrs[idx].flags&flagAlive != 0
	case ArenaU8Arr:
		return a.U8Arrs[idx].flags&flagAlive != 0
	}
	return false
}

// fuzzSlotGen returns the gen field of the slot at (tag, idx).
// Equivalent to slotGenForFree but available outside quarantine.go.
func fuzzSlotGen(a *Arenas, tag ArenaTag, idx uint32) uint16 {
	return a.slotGenForFree(tag, idx)
}

