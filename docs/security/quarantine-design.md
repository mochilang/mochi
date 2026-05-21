# Wrap-detection quarantine design

This document records the design and tuning of the per-arena
wrap-detection quarantine that lands in MEP-41 Phase 3. The
quarantine is a small slot-index FIFO that widens the ABA reuse
window for slots whose 12-bit generation counter is approaching its
wrap point. The accompanying source is `runtime/vm3/quarantine.go`;
the tests are `runtime/vm3/quarantine_test.go`.

Created: 2026-05-21 19:30 (GMT+7) as part of MEP-41 Phase 3.

## 1. What the quarantine defends against

vm3 stores a handle's generation in 12 bits of the Cell (see
`runtime/vm3/cell.go`, `genShift=32`, mask `0xFFF`). At every
dereference, `c.DecodeHandle()` extracts (tag, gen, idx) and the
accessor compares gen against the live slot's `gen` field. A
mismatch yields a trap (Phase 1 verifier rule class A makes the
producer of the gen field unforgeable; rule class C makes the gen
itself non-observable from inside Mochi code).

The 12-bit field wraps at 4096. A slot freed at gen=4094 takes
gen=4095 on the next reuse and gen=0 on the reuse after that. An
escaped handle that remembered (tag, gen=0, idx) from a prior
generation thus aliases the slot on its next gen=0 reuse, even
though the verifier guarantees the held Cell is well-formed.

The wrap-detection quarantine targets this last-mile reuse. It
trades a small amount of arena memory (one slot-index in the FIFO
per wrap-prone slot) for a much wider effective ABA window.

The threat is the same one Apple's Memory Integrity Enforcement (MIE,
September 2025) names as "Type Confusion Engine"-style attack: an
adversary who can observe generation-counter behavior and time a
free/reuse sequence to land their stale handle on a freshly
re-allocated slot. The verifier rule class C (Phase 2) closes the
*observation* path; the quarantine closes the *timing* path.

## 2. Design constraints

The quarantine has to coexist with two existing properties of the
arena allocator:

1. **LIFO fast-path stays zero-overhead.** No benchmark in the BG
   suite pushes any slot past gen=64. Adding a per-Free branch to
   the hot path was unacceptable. Solution: gate the slow path on a
   generation threshold (WrapWarn) that no realistic benchmark
   crosses. The cost of the slow path is paid only by allocations
   that have already churned thousands of times.

2. **Per-arena isolation.** Each arena has its own free list and
   gen counters. A cross-arena quarantine ring would create false
   sharing of slot indices and would also blur the per-arena threat
   model (the verifier rule class D, MEP-41 §6.5, is per-arena).
   Solution: the ring is `[numArenaTags][]uint32` and `push`,
   `drainHead`, and `depth` all index by ArenaTag.

## 3. Algorithm

`(*Arenas).Free` always:
1. Clears `flagAlive` on the slot.
2. Nil's the backing slice (so the Go GC can reclaim the payload).
3. Calls `routeToFreeOrQuarantine(tag, idx)`.

`routeToFreeOrQuarantine`:
- If quarantine is disabled (`depth == 0`) or the slot's current gen
  is below WrapWarn: append idx to the regular free list (LIFO
  fast-path).
- Otherwise: push idx to the wrap-quarantine FIFO. If the ring's
  depth now exceeds WrapQuarantineDepth, pop the oldest entry and
  append it to the regular free list. This bounds memory while
  guaranteeing a wrap-prone slot's reuse is delayed by at least
  WrapQuarantineDepth same-arena allocations.

`takeXxxSlot` is unmodified: it still pops LIFO from `a.freeXxx`.
The quarantine widens the window between `Free` and `takeXxxSlot`
seeing the slot, not how the slot is taken once it lands on the
free list.

## 4. Tuning rationale

| Parameter | Default | Rationale |
|-----------|---------|-----------|
| `DefaultWrapWarn` | 4032 | 12-bit gen wraps at 4096. Reserve the final 64 generations (4096 - 64 = 4032) for wrap-aware reuse only. Any threshold lower than this would force tests that exercise gen=1..16 into the slow path for zero security gain. |
| `DefaultWrapQuarantineDepth` | 64 | The FIFO holds at most 64 wrap-prone slot indices per arena. With 12 arenas, the worst-case quarantine footprint is 12 × 64 × 4 bytes = 3 KiB per VM. A wrap-prone slot is guaranteed at least 64 same-arena allocations before reuse. |

The numbers are deliberately conservative. The hot-loop benchmark
suite (BG and friends) is structurally unaffected: no test in tree
pushes a slot past gen=64. Long-running VMs that do churn slots
millions of times pay the 3 KiB footprint and a single branch per
Free; in exchange the effective ABA window becomes (12-bit gen) ×
WrapQuarantineDepth instead of just (12-bit gen).

## 5. Per-VM tuning hook

`(*Arenas).SetQuarantineConfig(warn, depth)` lets a long-running VM
adjust the thresholds without recompile. Two intended uses:

- **Benchmarks**: `SetQuarantineConfig(DefaultWrapWarn, 0)` disables
  the slow path entirely. Stress tests that simulate millions of
  free/reuse cycles to validate quarantine semantics under load
  use this to get raw LIFO behavior for their baseline runs.

- **High-security deployments**: `SetQuarantineConfig(2048, 256)`
  trades more memory and an earlier threshold crossing for a longer
  effective ABA window. Useful for embedded scenarios where the
  same VM serves many tenants and an escaped handle's worst-case
  lifetime might exceed the default 64-allocation gap.

## 6. Interaction with mark-sweep GC

The Phase 5 mark-sweep collector in `runtime/vm3/gc.go` calls
`routeToFreeOrQuarantine` on each swept slot. This means the GC
respects the quarantine: a swept slot whose gen has crossed
WrapWarn lands in the quarantine ring, exactly as if it had been
`Free`'d explicitly. The bump in `gen` happens inside the sweep
loop before the route call, so the routing decision is based on
the post-sweep generation.

## 7. Tests

`runtime/vm3/quarantine_test.go` covers:

| Test | Property |
|------|----------|
| `TestQuarantineFastPathBelowThreshold` | Slots with low gen take the LIFO fast path; no ring traffic. |
| `TestQuarantineHoldsWrapProneSlot` | A slot forced to gen=WrapWarn is held out of the free list on `Free`. The next allocation lands on a different slot. |
| `TestQuarantineDrainAfterDepth` | After depth+2 wrap-prone Frees, exactly 2 slots have drained to the free list, in oldest-first FIFO order. |
| `TestSetQuarantineConfigDisablesViaDepthZero` | Setting depth=0 with a non-zero warn explicitly disables the slow path; wrap-prone Frees go straight to the free list. |
| `TestQuarantineRingIsPerArena` | Quarantining a list slot does not affect string-arena reuse; per-arena isolation holds. |
| `TestQuarantineDefaultsApply` | A fresh `Arenas` returns the package defaults for both `quarantineWrapWarn()` and `quarantineDepth()`. |

## 8. What's not yet here (Phase 3.1, Phase 3.2)

The Phase 3 closeout in MEP-41 §8 pre-registers two follow-ups that
share the quarantine theme but require larger surface-area changes:

- **Phase 3.1 - Guard slabs.** A `-vm3-guard-slabs` build flag that
  places a non-readable, non-writable page between every pair of
  arena slabs, so a Cell whose `idx` field has been corrupted
  past the slab end faults immediately. Requires platform-specific
  `mmap` plumbing.

- **Phase 3.2 - Bytecode-level OpSeal / OpUnseal.** The Go-level
  `Sealer` in `runtime/vm3/sealing.go` already implements the
  per-VM sealing table. Phase 3.2 wires it into the IR and the
  bytecode dispatcher so Mochi source can mark FFI boundaries
  with `seal`/`unseal` opcodes. Requires IR + `compiler3` changes.

Both are tracked as follow-up issues in the same milestone as the
parent Phase 3 PR.

## 9. Cross-references

- MEP-41 §6.7 (wrap-quarantine mention in the threat model).
- MEP-41 §8 (phase table).
- `docs/security/threat-model.md` §4.7 (the threat the quarantine closes).
- `docs/security/memory-safety.md` §9 (phase status table).
- Apple MIE blog post (September 2025): the motivating threat model
  for both rule class C (gen opacity, Phase 2) and this quarantine.
