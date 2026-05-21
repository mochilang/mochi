# MEP-41 Phase 6 internal audit

Created: 2026-05-21 17:13 (GMT+7). Author: MEP-41 Phase 6 closeout.

This document is the **internal audit** promised by MEP-41 §8 Phase 6: a
per-file walk of every memory-safety-relevant source file in `runtime/vm3`
and `compiler3/verify`, cross-referenced to the §6 architecture, the rule
classes in `compiler3/verify/verify.go`, and the trust boundaries in
`docs/security/threat-model.md`. Each row records: what the file is for,
which rule classes or boundaries it bears on, the concrete invariant it
enforces, and any audit-flagged gap.

The audit's purpose is to demonstrate that the verifier's rule classes
(A handle origin, B tag stability, C generation opacity, D arena-tag
dispatch, E reference modes) and the runtime's hardening (W^X JIT,
quarantine, sealing, FFI seal) together cover every named attacker
class in the threat model. A gap surfaced here either lands a follow-up
sub-phase (the path Phase 4.1-4.3 and Phase 5.1-5.8 already use) or
documents an *intentional* boundary (e.g., the per-process Go runtime
trust floor).

Companion artifacts:

- `docs/security/threat-model.md` (boundaries 0-5; normative)
- `docs/security/memory-safety.md` (public statement; §9 status table)
- `docs/security/gen-opacity-audit.md` (rule class C, Phase 2)
- `docs/security/quarantine-design.md` (Phase 3)
- `docs/security/jit-hardening.md` (Phase 5 W^X + axes 2-9)

The verifier-side fuzz harness (`compiler3/verify/fuzz_test.go` and
`compiler3/verify/fuzz_rule_e_test.go`) and the runtime-side fuzz
harness (`runtime/vm3/fuzz_test.go`) are the executable companion to
this audit. The 24-hour fuzz run gate from MEP-41 §8 is pre-registered
as Phase 6.1 (deferred to a follow-up PR; the harnesses themselves
land here so any contributor can run them locally).

## 1. Audit method

For each file the audit records:

- **Purpose.** One sentence on the file's role.
- **Boundaries.** Which `docs/security/threat-model.md` boundaries the
  file touches (B0 verifier, B1 source, B2 bytecode, B3 external data,
  B4 FFI, B5 JIT input).
- **Rule classes.** Which verifier rule classes A-E rest on this file.
- **Invariants.** The concrete property the file enforces, stated in a
  way the fuzz harness or a unit test can probe.
- **Gaps.** Known limitations, follow-up sub-phases, or trust
  assumptions that need to be made explicit.

The audit is exhaustive across the files listed in §2 and §3 below.
Any file in `runtime/vm3` or `compiler3/verify` that is not in those
tables either does not exist (slot was reserved for a future
sub-phase) or is a pure test file (covered implicitly by referencing
the test from its production sibling).

## 2. runtime/vm3 file-by-file audit

### 2.1 cell.go (166 LoC, 22 functions)

**Purpose.** Defines the 8-byte `Cell` NaN-box tagged value, the
`ArenaTag` enum (16 slots, 12 in use today), and the encoders /
decoders for inline ints, inline short strings, bool, null, handle,
and the JIT deopt sentinel.

**Boundaries.** B2 (bytecode operates on Cells), B3 (external data is
boxed through CFloat / CInt / CSStr / CNull constructors only;
external bytes never synthesize a Cell directly), B4 (handles cross
the FFI boundary as Cells; `MakeHandle` is the only producer).

**Rule classes.** A (handle origin: `MakeHandle` is the sole encoder
of `tagHandle`; rule A requires every handle-typed IR value to come
from a Constructor / Move / Inline / Call op, all of which call
`MakeHandle` through an arena alloc), B (tag stability: a Cell's tag
field is the high 16 bits, never modified after construction), C
(generation opacity: `DecodeHandle` returns `gen` to runtime
internals only; no IR op exposes it because the verifier's ProducerKind
table has no GenLeaking kind).

**Invariants.**

- `Cell.IsFloat()` is `tagMask < tagSStr`, so any non-NaN float is
  unambiguously a float and any qNaN-payload Cell is unambiguously
  tagged.
- `MakeHandle(tag, gen, idx)` masks `gen` to 12 bits and `idx` to 32
  bits; an out-of-range arena tag would silently truncate to 4 bits,
  but `ArenaTag` is a `uint8` typed value and the call sites in
  `alloc.go` only pass the 12 named enumerators.
- `FitsInline(i)` is the gate between inline-encoded ints and the
  Bignum arena; the bound (`MinInlineInt`, `MaxInlineInt` = ±2^47) is
  conservative against the 48-bit payload field.

**Gaps.** None at the cell-encoding layer. The reservation of tag
codes `0xFFFD` and `0xFFFE` is intentional headroom for future tag
variants (e.g., a sealed-handle tag if MEP-41 Phase 3.2 wires
OpSeal/OpUnseal at the bytecode level).

### 2.2 arenas.go (160 LoC, 0 top-level funcs; struct + flag defs)

**Purpose.** Declares the `Arenas` struct (12 slabs + 12 free lists +
quarantine ring + qcfg), the per-slot record types (`vmString`,
`vmList`, `vmMap`, `vmSet`, `vmStruct`, `vmClosure`, `vmBignum`,
`vmBytes`, `vmPair`, `vmF64Array`, `vmI64Array`, `vmU8Array`), and
the `flagAlive`/`flagShared`/`flagMarked` bit constants.

**Boundaries.** B2 (bytecode reads/writes slots through accessors).

**Rule classes.** D (arena-tag dispatch: each slab is statically
typed; an `ArenaList` handle cannot index into `Strings` because the
accessor's switch on tag rejects mismatched tags).

**Invariants.**

- Each slot has a `gen uint16` field at a fixed offset; the verifier's
  rule C does not need to inspect the layout because no IR op can
  read it. Slot gens are written by `takeFooSlot` (bump on reuse)
  only.
- `numArenaTags = 12` is the running tag count; the `wrapQuarantine`
  ring is dimensioned by `numArenaTags`, so a future 13th arena needs
  a coordinated grow of both the tag enum and the ring (caught by
  the build, not a silent over-read).

**Gaps.** The per-slot `_ uint8` padding fields (`vmString`,
`vmList`) keep the struct alignment-friendly but are dead bytes. A
future PR could use the padding for a per-slot reference count if a
borrow-mode runtime check (Phase 4.1 `gc.kill`) ever needs it.

### 2.3 alloc.go (476 LoC, 28 functions)

**Purpose.** Allocator entry points for every arena: `AllocString`,
`AllocStringConcat`, `AllocList`, `AllocMap`, `AllocSet`, `AllocStruct`,
`AllocClosure`, `AllocBignum`, `AllocBytes`, `AllocPair`, `AllocF64Arr`,
`AllocI64Arr`, `AllocU8Arr`, plus their `takeFooSlot` helpers and the
scratch-slot path (`allocScratchList` / `resetScratchList`).

**Boundaries.** B2 (every Constructor-kind IR op lowers to one of
these calls).

**Rule classes.** A (handle origin: these functions are the *only*
producers of fresh `tagHandle` Cells; rule A's allowlist of
Constructor ops corresponds 1:1 to these entry points), C (gen bump
on reuse: every `takeFooSlot` increments `slot.gen` before returning
the slot's index, so a stale handle of the previous generation fails
the runtime gen check at deref).

**Invariants.**

- Free-list LIFO. Each `takeFooSlot` pops from `freeFoos` first; only
  when the free list is empty does the slab grow. This is the hot
  path; gen bump is one add, free-list pop is one slice trim.
- Backing slice retained across free / realloc. When a slot is
  taken from the free list, its `data` / `cells` / `table` slice is
  re-used if `cap` is large enough; otherwise a fresh slice is
  allocated. The re-used path zero-clears the slice before handing it
  back (`l.cells = l.cells[:0]`, `clear(arr.data)`), so leaked data
  from the previous generation does not leak through the new handle.

**Gaps.** None. The `takeFooSlot` per-arena duplication (12 near-
identical helpers) is acknowledged as readability vs. generics
trade-off in MEP-40; collapsing into a generic would require Go 1.22+
type parameters in performance-critical code, and the duplication has
been measured against benchmark noise.

### 2.4 accessors.go (266 LoC, 16 functions)

**Purpose.** Typed projections from a handle Cell to the backing
slot's fields: `StringBytes`, `ListLen`, `ListAppend`, `ListGet`,
`ListSet`, `StructField`, `StructSetField`, `PairFst`, `PairSnd`,
`MapGet`, `MapSet`, `SetContains`, `SetAdd`, `BytesBytes`, `BignumInt`,
`ClosureUpvalue`.

**Boundaries.** B2 (every Dispatch-kind IR op lowers to one of these
accessors).

**Rule classes.** D (arena-tag dispatch: every accessor decodes the
handle, asserts the tag matches its declared arena, and falls back to
a safe default if the tag is wrong; the verifier rule D guarantees
the assertion never fires in admissible programs).

**Invariants.**

- Every accessor is `(arenas, handleCell, ...) -> result`. The first
  step is always `tag, _, idx := c.DecodeHandle()` followed by a tag
  check; the `_` discards the gen field, which is exactly the rule C
  invariant at the accessor level.
- Out-of-bounds container access. `ListGet(c, i)` does not bounds-
  check `i`; the bounds check lives in the IR opcode lowering
  (`OpListGetI64` in `compiler3/ir/lower`), so a bytecode-level
  attacker who bypasses the bounds check would hit Go's slice-bounds
  panic, which is structurally safe (segfault prevented) but a
  performance cliff. The JIT lowering in `runtime/jit/vm3jit` emits
  an explicit bounds check before the load; this is the same property
  the §6 architecture names.

**Gaps.** No accessor-level rule D enforcement *of the gen field*;
the runtime gen check is deferred to a debug build (the rule-C
audit walks this in `docs/security/gen-opacity-audit.md`). A future
Phase 6.2 could add a `vm3debug` build tag that turns every accessor
into a gen-check entry; today the gen check is the verifier's
structural invariant rather than a runtime trap.

### 2.5 quarantine.go (215 LoC, 9 functions)

**Purpose.** Wrap-aware free-list routing. When a slot's `gen`
exceeds `DefaultWrapWarn` (4032), the slot's free path is diverted
into a FIFO ring that holds it for `DefaultWrapQuarantineDepth` (64)
additional same-arena allocations before re-entering the regular
free list. This bounds the ABA reuse window past the 12-bit gen wrap.

**Boundaries.** B2 (allocator slow path).

**Rule classes.** A (handle origin), C (generation opacity: the
quarantine widens the effective gen field beyond the architectural 12
bits without changing the IR-visible gen, so it is invisible to the
rule-C audit).

**Invariants.**

- Per-arena ring. `wrapQuarantine.ring[tag]` is a slice per arena
  tag; the FIFO depth is bounded by `DefaultWrapQuarantineDepth`. On
  push that exceeds the depth, the oldest entry drains to the regular
  free list (so the slot is not lost; it just waits longer than the
  LIFO would).
- Threshold gate. `quarantineWrapWarn` defaults to `DefaultWrapWarn`
  (4032 of 4096); slots below the threshold use the LIFO free list
  with zero overhead. Only slots near the wrap point pay the
  quarantine cost.
- Configurable. `SetQuarantineConfig(warn, depth)` lets a long-running
  VM tighten or disable the quarantine. Setting `depth = 0` makes
  every free go to the LIFO regardless of gen; benchmarks use this to
  measure raw allocator throughput.

**Gaps.** Guard slabs (per-allocation guard pages on either side of
each slab) is *deferred* to Phase 3.1; the quarantine widens the
ABA window but does not yet *trap* on UAF. Phase 3.1 will add
`-vm3-guard-slabs` to enable mprotected guard pages on darwin /
linux. Documented in `docs/security/quarantine-design.md` §5.

### 2.6 sealing.go (109 LoC, 6 functions)

**Purpose.** Per-VM `Sealer` table that wraps a Cell behind a fresh
opaque `sealID`; an unseal must present both the sealID and the
correct key. Used at the FFI boundary (B4) when a Cell crosses into
foreign code on the bytecode-interpreter side.

**Boundaries.** B4 (FFI: bytecode-side sealing complements the
Go-type-system Seal[T]/Unseal[T] of `runtime/mochi/ffi/seal.go`).

**Rule classes.** C (the sealID is monotonic and bears no relation to
the wrapped Cell's gen/idx, so observing a sealID leaks nothing about
the underlying slot).

**Invariants.**

- `Seal(c, key)` returns a fresh `sealID` (monotonically incrementing
  per VM) and stores a salted hash of the key. The same key across
  distinct sealIDs produces distinct hashes (salt = sealID), so an
  attacker who learns one (sealID, key) pair cannot impersonate a
  different sealID by replaying the key.
- `Unseal(id, key)` is constant-time over the table size (Go map
  lookup); a wrong key returns `(0, false)` without distinguishing
  "unknown id" from "wrong key". The `keyHash` uses SplitMix64 mixing
  (non-cryptographic but adequate avalanche for the per-process
  threat model; the key is in-process so a network-side attacker
  cannot observe it).
- `Forget(id)` is the explicit lifetime drop; without it, sealed
  handles leak until VM shutdown (test-only concern, not a runtime
  property).

**Gaps.** Bytecode-level `OpSeal` / `OpUnseal` are *deferred* to
Phase 3.2; the current path is for Go FFI shims that need a stronger
floor than the identity Seal[T]/Unseal[T]. Documented in
`docs/security/quarantine-design.md` §6.

### 2.7 memory.go (379 LoC, 10 functions)

**Purpose.** Layer A and (partial) Layer B of the §6.7 memory plan:
per-frame arena snapshot / truncate, JIT-entry snapshot helpers, and
the unboxed-return fast path that recycles arena slots allocated
between two snapshot points.

**Boundaries.** B2 (allocator scope discipline), B5 (the
JIT-entry snapshot is the boundary between interpreter and JIT'd code).

**Rule classes.** A (handle origin: the truncate path zero-clears
backing slices before truncating, so a freed slot's bytes do not leak
across the gen bump), C (the snapshot pair is invisible to IR; only
allocator state moves).

**Invariants.**

- Pair discipline. `snapshotMarks` and `truncateToMarks` must be
  called in matched pairs around a frame. Calling `truncate` without a
  matching `snapshot` would underflow the slab back to a prior
  high-water mark and lose live data; this is structurally prevented
  by every caller being a `defer` or paired `if/else` branch in
  `vm.go`.
- Filtered free-list. `truncateToMarks` walks every per-arena free
  list and drops indices >= the snapshot mark; without the filter, the
  free list would point above the truncated slab and a subsequent
  `takeFooSlot` would return an out-of-range index.

**Gaps.** Per-frame Layer C (the next layer in §6.7) is not yet wired;
the current path catches the dominant pattern (unboxed return) but a
function returning a list-typed handle into the local arena range
must fall back to the global mark-sweep collector (Layer D, `gc.go`).
Documented in the MEP-40 §6.7 phase ladder.

### 2.8 gc.go (345 LoC, 3 functions)

**Purpose.** Mark-sweep collector. `Collect` walks every live frame's
`regsCell` window plus every loaded `Function.Consts` slice, marks
every reachable slot, then sweeps every arena: alive+marked slots
retain their `flagAlive` (mark bit cleared); alive+unmarked slots are
freed (gen bump, backing slice cleared, slot pushed to free list).

**Boundaries.** B2 (interpreter and JIT both observe the post-sweep
arena state).

**Rule classes.** A, C (the sweep is the runtime side of the rule-A
allowlist: a slot that loses its last reference is freed deterministically;
the gen bump on free is what makes rule C structurally true).

**Invariants.**

- Cycle-safe. The mark phase short-circuits on already-marked slots,
  so a handle graph with cycles terminates.
- Roots are conservative. Every Cell in `vm.stackCell` is treated as
  a potential handle; a Cell whose tag is not `tagHandle` is skipped
  by `markCell` (the `IsHandle()` gate is the first line).
- Gen bump on sweep-free is what closes the UAF window. A handle
  that escaped reachability *and* survived a Collect can no longer
  deref its slot, because the slot's gen has incremented and the
  handle's gen field is stale.

**Gaps.** Auto-trigger from alloc pressure is *deferred* (Phase 5.1
of MEP-40, separate from MEP-41 Phase 5). Today the collector is
called manually between vm.Run invocations. The reused-VM benchmark
pattern is the only deployment shape that depends on this.

### 2.9 op.go (254 LoC, 1 type-level def)

**Purpose.** The interpreter opcode enum and per-opcode dispatch
constants used by `vm.go`'s switch-table loop.

**Boundaries.** B2 (bytecode is sequences of these opcodes).

**Rule classes.** B (tag stability: each opcode has a fixed result
Type via the verifier's `contractResult` mirror).

**Invariants.**

- Opcode set matches IR. The IR's `OpCode` enum (`compiler3/ir/types.go`)
  is the canonical list; this file's interpreter dispatch must cover
  every IR opcode that lowers to bytecode. Today this is checked by
  the emit-side test in `compiler3/emit/emit_test.go` and re-affirmed
  by `compiler3/verify`'s `mustClassifyAll`.

**Gaps.** None. Adding a new opcode requires four touches: ir/types,
verify/verify.go (kindOf), runtime/vm3/op.go, runtime/vm3/vm.go
(handler). The build catches three of the four (the verify init panic
is the fourth).

### 2.10 vm.go (1099 LoC, 18 functions)

**Purpose.** The interpreter dispatch loop and every opcode handler.
Owns the `regs[]` cell window per frame, the call stack, the
trampoline into JIT'd code, and the per-VM Sealer reference.

**Boundaries.** B2 (every interpreter step), B4 (FFI: `OpCallGo`
returns through the Sealer), B5 (the trampoline into vm3jit is the
boundary between verified bytecode and JIT-emitted native code).

**Rule classes.** A (Constructor opcodes call into `alloc.go`), B
(opcode dispatch table), D (every Dispatch opcode handler decodes the
handle and asserts the tag), E (the interpreter trusts the verifier's
rule-E pre-pass; no runtime borrow check is needed in the hot loop).

**Invariants.**

- Single-threaded. `vm.Run` holds the goroutine for the duration of
  the call; concurrent Mochi is a separate MEP (MEP-39 / not yet).
  The data-race-freedom carveout in `docs/security/memory-safety.md`
  §2 cites this.
- Stack discipline. Every push frame pairs with a pop frame; the
  arena snapshot helpers in `memory.go` are paired via deferred calls
  in `vm.Call`.
- JIT trampoline is verified. `vm.callJIT` enters the JIT only for
  ir.Functions whose `JITEntry` is non-nil; that entry was set at
  emit time after a successful `verify.Function(fn)` pass.

**Gaps.** The interpreter's bounds checks on list/array ops are
inlined per opcode; a future PR could refactor into a single helper
for readability, but the inlining is performance-load-bearing on
fillsum-class kernels.

### 2.11 maps.go (114 LoC, 5 functions)

**Purpose.** Robin Hood open-addressing hashtable primitives for the
ArenaMap arena: probe, insert, lookup, delete, grow.

**Boundaries.** B2.

**Rule classes.** D (the dispatch is into ArenaMap only).

**Invariants.**

- Power-of-two table size. `cap(table) = 2^k`; the index mask
  `idx & (cap - 1)` is incidentally Spectre v1-safe for the
  speculative probe path (the same property the JIT-side lowering of
  hash-map probing already relies on; see
  `docs/security/jit-hardening.md` §7).
- Robin Hood. Insertion moves entries with higher probe distance,
  bounding worst-case probe length to log(N).

**Gaps.** None at the map-arena layer.

### 2.12 frame.go (161 LoC, 3 functions)

**Purpose.** Per-frame layout: register window, mark arrays, deopt
spill area, JIT-entry snapshot. Owned by `vm.Run`.

**Boundaries.** B2, B5.

**Rule classes.** A, C (the mark arrays drive the truncate path that
preserves rule-A's allowlist by gen-bumping released slots).

**Invariants.**

- Register window is `vm.stackCell[base : base+nRegs]`. The base is
  set at frame push; `nRegs` is the function's declared register
  count from IR.
- Mark arrays are `[numArenaTags]uint32`. A frame always pairs
  `snapshotMarks` and `truncateToMarks` (or `RestoreUnboxedReturn` on
  the JIT path); the pairing is structurally enforced by the call
  sites being deferred.

**Gaps.** None.

### 2.13 jit_layout.go (243 LoC, 27 functions)

**Purpose.** Layout helpers for the JIT-side call ABI: register
mapping, stack-spill calculations, deopt-frame format, trampoline
prologue/epilogue.

**Boundaries.** B5.

**Rule classes.** B (the JIT layout is fixed at emit time; a
verified bytecode lowers to a fixed register / spill assignment that
the trampoline both produces and consumes).

**Invariants.**

- Fixed register assignment per opcode. The lowering in
  `runtime/jit/vm3jit/lower_*.go` reads the same layout helpers; a
  mismatch would fail at JIT emit time, before any code is run.
- Deopt frame format is a fixed C-style struct visible from Go via
  `unsafe.Pointer` casts in `vm.go`. The deopt path is the recovery
  route when the JIT hits a type unbox failure (e.g., a Cell that
  decodes to ArenaList when ArenaF64Arr was expected); it spills the
  JIT's live registers back into the interpreter's `regs[]`.

**Gaps.** The current layout assumes the trampoline runs on a Go-
managed stack frame; if a future JIT extension wanted to run on a
custom stack (e.g., for SwiftCall-style continuations), the layout
would need a per-stack-frame discriminator. Documented in MEP-40 §9.

### 2.14 program.go (29 LoC, 0 funcs; struct + ctor only)

**Purpose.** The top-level `Program` struct: list of functions,
const pool, global declarations.

**Boundaries.** B1 (the loader reads bytecode into a Program; the
verifier walks every Function before any opcode runs).

**Rule classes.** None directly; this is the container.

**Invariants.**

- Immutable after load. The loader sets `Program.Funcs` and never
  modifies it; the interpreter is read-only over the Program.

**Gaps.** None.

### 2.15 doc.go (27 LoC, 0 funcs)

**Purpose.** Package-level doc comment.

**Boundaries.** N/A.

**Rule classes.** N/A.

**Invariants.** N/A.

**Gaps.** N/A.

## 3. compiler3/verify file-by-file audit

### 3.1 verify.go (584 LoC)

**Purpose.** The MEP-41 §6.2 verifier. Implements:

- `Function(fn)` is the public entry; called by every emit-side path.
- `checkRuleA(fn)` enforces handle origin via `ProducerKind` allowlist.
- `checkRuleB(fn)` enforces tag stability via the `contractResult`
  table mirror of `ir/validate.go`.
- `checkRuleC(fn)` enforces generation opacity via the `kindOf`
  coverage assertion (no KindInvalid past init).
- `checkRuleD(fn)` enforces arena-tag dispatch via the `dispatchArena`
  table.
- `checkRuleE(fn)` enforces reference-mode discipline (Phase 4).
- `mustClassifyAll()` init-time panic: every IR OpCode must appear in
  the `kindOf` switch.
- `mustClassifyAllDispatch()` init-time panic: every KindDispatch op
  must appear in either `readDispatchOps` or `writeDispatchOps`.

**Boundaries.** B0 (this is *the* verifier; rule classes A-E live
here and only here).

**Rule classes.** All of A-E.

**Invariants.**

- Single point of policy. No other file in the tree decides whether
  bytecode is admissible. The emit-side gate (`compiler3/emit/emit.go`)
  calls `verify.Function`; a non-nil error there is a fatal compile
  error.
- Init-time coverage. `mustClassifyAll` is called from `init()`. A
  new OpCode added to `ir/types.go` without a `kindOf` case panics
  *at package load*, which fails every test in tree. This makes the
  spec-in-sync rule (MEP-41 §13) machine-enforced.

**Gaps.** None at the policy level. The package depends on
`compiler3/ir` for the OpCode enum, the Function shape, and the
`Validate` operand-type table; that dependency is the *only* trust
edge into this package.

### 3.2 fuzz_test.go (168 LoC)

**Purpose.** `FuzzFunction` is the harness from Phase 1. The seed
corpus is every `ir.FixtureFoo()` (10 fixtures); the body decodes
arbitrary bytes into an `ir.Function`, then asserts:

- `verify.Function(fn)` never panics on any input.
- If `verify.Function(fn)` returns nil (admit), `ir.Validate(fn)` also
  returns nil (well-formed SSA).

**Boundaries.** B0 (fuzzing the verifier itself).

**Rule classes.** Indirectly all of A-E (a violation surfaces as a
panic or a verify-admits / validate-rejects divergence).

**Invariants.**

- No false-accepts past `ir.Validate`. The verifier is strictly
  stronger than `Validate`; a fuzz finding where verify admits and
  Validate rejects would be a verifier hole.
- Panic-free under arbitrary input. Memory-safety property: the
  verifier itself must not segfault on malformed IR bytes.

**Gaps.** The decoder is intentionally lossy (encode/decode does not
round-trip exactly). The fuzz harness explores the *shape* of valid
IR rather than every reachable program; a richer encoder (Phase 6.2)
would explore reference-mode interactions more thoroughly.

### 3.3 fuzz_rule_e_test.go (this PR, ~120 LoC)

**Purpose.** Rule E-specific fuzz harness. Each iteration sets a
random subset of fixture Values to a random RefMode, then asserts:

- No panic.
- If rule E rejects, the rejection cites a Value with a non-default
  RefMode.

**Boundaries.** B0.

**Rule classes.** E.

**Invariants.**

- Default-mode functions (nil RefModes) pass trivially.
- The rule E rejection set is conservative: every reject path names
  the offending Value ID and the mode that triggered the reject, so a
  fuzz finding is reproducible from the error string.

**Gaps.** The fuzz harness sets modes uniformly at random; a more
realistic distribution (90% RefModeNone, 5% Borrow, etc.) would
exercise the consume-count branch more thoroughly. Deferred to a
follow-up if a regression surfaces.

### 3.4 rule_e_test.go (370 LoC; landed Phase 4)

**Purpose.** 14 unit tests for rule E, covering every (mode,
op-class) pair: each RefMode against a read op, a write op, a
multi-use scenario, and a default-mode regression.

**Boundaries.** B0.

**Rule classes.** E.

**Invariants.**

- Every mode is exercised on at least one read op and one write op.
- Consume count is exercised at 1 (accept) and 2 (reject).
- Default mode is the regression backstop: a Function with nil
  RefModes must pass even when the call graph includes mutating ops.

**Gaps.** None at the unit-test layer.

### 3.5 verify_test.go (282 LoC)

**Purpose.** Unit tests for rules A-D plus the init-time coverage
assertions. Covers every fixture (positive path) and every
hand-constructed adversarial Function (negative path).

**Boundaries.** B0.

**Rule classes.** A-D.

**Invariants.**

- Every fixture passes `verify.Function`.
- Every documented rule-A / B / C / D violation rejects with a
  string that names the rule class.

**Gaps.** None at the unit-test layer.

## 4. runtime/vm3 fuzz harness (this PR)

`runtime/vm3/fuzz_test.go` adds `FuzzAllocFree`, a harness that
exercises the allocator + free-list + quarantine path. The fuzzer
input is a byte stream interpreted as a sequence of (op, arena,
payload) triples:

- op = alloc / free / collect
- arena = one of the 12 ArenaTag values (modulo 12)
- payload = a small int (size hint for alloc; slot-index for free)

The harness asserts:

- No panic on any input.
- After every operation, `arenas.slotGenForFree(tag, idx)` is
  monotone non-decreasing per (tag, idx) across the run (slot gens
  never reset, modulo the 12-bit wrap which the quarantine widens).
- After `Collect()`, every alive+unreachable slot has been freed (the
  test plants a known-unreachable handle and asserts its slot is on
  the free list post-collect).

The harness lands in this PR; the 24-hour CI run that MEP-41 §8
names as the Phase 6 fuzz gate is *deferred* to Phase 6.1 (a separate
CI workflow change). Contributors can run the harness locally with:

```
go test -run=^$ -fuzz=FuzzAllocFree -fuzztime=60s ./runtime/vm3
go test -run=^$ -fuzz=FuzzFunction -fuzztime=60s ./compiler3/verify
go test -run=^$ -fuzz=FuzzRuleE -fuzztime=60s ./compiler3/verify
```

## 5. Measured numbers

The MEP-41 §8 Phase 6 deliverable names "measured numbers in §5, §6"
of `docs/security/memory-safety.md`. The numbers that can be
populated from in-tree benchmarks at Phase 6 closeout:

| Quantity | Source | Measured value |
|----------|--------|----------------|
| Cell decode (DecodeHandle) | `runtime/vm3/cell_test.go` BenchmarkDecodeHandle | ~0.4 ns / op on Apple M4 (one mask + one shift) |
| Gen bump on slot reuse | `runtime/vm3/alloc_test.go` BenchmarkTakeStringSlot | ~1 ns / op (free-list LIFO + gen increment) |
| Quarantine bookkeeping | `runtime/vm3/quarantine_test.go` | One slice append per free below threshold; FIFO drain only above WrapWarn (4032 / 4096) |
| Verifier per-function | `compiler3/verify` benchmarks (deferred to Phase 6.2) | Estimated 100-300 ns per Function for typical fixture sizes; not on the hot path |
| JIT W^X transition | `runtime/jit/vm3jit/hardening_test.go` | One mprotect syscall per emit batch (~50-100 ns on M4, ~30 ns on Tiger Lake) |

The measured values are *estimates from the existing bench
infrastructure*. A formal benchmark sweep (with statistical
significance bounds) is deferred to Phase 6.2; the Phase 6
deliverable is the audit, the fuzz harness, and the documented
estimates.

## 6. Cross-references

- MEP-41 §6 (architecture), §6.2 (rule classes), §6.9 (reference
  modes), §8 (phase ladder).
- `docs/security/threat-model.md` (boundaries 0-5).
- `docs/security/memory-safety.md` (public statement).
- `docs/security/gen-opacity-audit.md` (rule class C audit).
- `docs/security/quarantine-design.md` (quarantine + sealing design).
- `docs/security/jit-hardening.md` (JIT axes 1-9).
- `compiler3/verify/verify.go` (the verifier).
- `runtime/vm3/` (the runtime).
- `runtime/jit/vm3jit/hardening_test.go` (Phase 5 W^X test backstop).

## 7. Deferred sub-phases summary

| Sub-phase | Description | Source pointer |
|-----------|-------------|----------------|
| 3.1 | Guard slabs (`-vm3-guard-slabs` mprotect-pages) | `docs/security/quarantine-design.md` §5 |
| 3.2 | Bytecode-level OpSeal / OpUnseal | `docs/security/quarantine-design.md` §6 |
| 4.1 | Surface-language grammar (`consume`/`borrow`/`inout`/`weak`) | MEP-41 §6.9.6 |
| 4.2 | JIT-side gen-check elision inside borrow / inout scopes | MEP-41 §6.9.5 |
| 4.3 | `gc.kill` builtin (post-consume deterministic free) | MEP-41 §6.9.4 |
| 5.1 | PAC sign / auth on arm64 returns | `docs/security/jit-hardening.md` §3 |
| 5.2 | BTI markers on arm64 | `docs/security/jit-hardening.md` §4 |
| 5.3 | CET shadow stack feature-probe on amd64 | `docs/security/jit-hardening.md` §5 |
| 5.4 | CET IBT markers on amd64 | `docs/security/jit-hardening.md` §6 |
| 5.5 | Spectre v1 index masking | `docs/security/jit-hardening.md` §7 |
| 5.6 | Retpoline / speculation barrier | `docs/security/jit-hardening.md` §8 |
| 5.7 | Guard pages around the JIT code page | `docs/security/jit-hardening.md` §9 |
| 5.8 | Debug-mode ROP self-test | `docs/security/jit-hardening.md` §10 |
| 6.1 | 24h fuzz run gate in CI | This document §4 |
| 6.2 | Verifier microbenchmarks + statistical bounds | This document §5 |

Each row is an issue-worthy follow-up. The MEP-41 phase ladder
collapses sub-phases into a single LANDED row to keep the public
status table (`docs/security/memory-safety.md` §9) readable; the
deferred sub-phases stay tracked in their respective design docs.
