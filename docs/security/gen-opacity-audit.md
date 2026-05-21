# Generation opacity audit (MEP-41 Phase 2)

Created: 2026-05-21 18:00 (GMT+7). Author: MEP-41 Phase 2 closeout.

This document is the per-call-site audit promised by MEP-41 §8 Phase 2 deliverable two ("audit of every existing vm3 opcode for compliance with rule class C"). It is the human-readable companion to `runtime/vm3/genopacity_test.go`, which encodes the same finding as an executable AST check.

## 1. What rule class C says

From MEP-41 §6.2: **rule class C (generation opacity)** prohibits any IR opcode from producing a Value whose payload is the 12-bit generation tag of a handle Cell. The motivation comes from Apple Memory Integrity Enforcement, September 2025: TCE (Tag Confidentiality Enforcement) treats the hardware pointer tag as a secret because a confused-deputy attacker who can read the tag of one handle can synthesize a handle that re-aliases a freed slot, defeating the per-deref generation check. The same threat applies to vm3: if a Mochi program (or attacker-controlled bytecode) could observe a handle's gen, it could forge a handle to a slot that was freed and re-allocated in the same generation, turning a memory-safety violation into a type-confusion gadget.

The structural enforcement is two-layered.

- At the **bytecode layer**, the IR has no opcode that extracts the gen field. There is no `OpHandleGen`, `OpHandleIdx`, or `OpHandleTag` in `compiler3/ir/types.go`. The closest things are the dispatch ops (`OpListGetI64`, `OpMapGetI64I64`, etc.), and those consume a handle and an index but produce an arena-typed element, never a handle internal.
- At the **Go-runtime layer**, the only API that exposes gen is the method `Cell.DecodeHandle` on `runtime/vm3/cell.go`. That method returns `(tag ArenaTag, gen uint16, idx uint32)`. The audit below catalogues every caller and shows that every caller either drops gen via `_` or routes gen directly back into `MakeHandle` (a handle round-trip). No caller surfaces gen to any external interface (Cell payload, VM register, JIT spill slot, FFI return).

## 2. Call-site inventory

`DecodeHandle` is called from 34 sites in non-test code as of 2026-05-21. They split into three classes.

### Class C1: gen discarded at the destructure (no flow)

The caller writes `tag, _, idx := c.DecodeHandle()` or `_, _, idx := c.DecodeHandle()`. The gen value is dropped at the language level; no Go variable holds it after the statement.

| File | Line | Function | Purpose |
|------|------|----------|---------|
| `accessors.go` | 10 | `(*Arenas).StringBytes` | Project string handle to bytes |
| `accessors.go` | 19 | `(*Arenas).ListLen` | Project list handle to length |
| `accessors.go` | 29 | `(*Arenas).ListAppend` | Append to list |
| `accessors.go` | 41 | `(*Arenas).ListGet` | List element read |
| `accessors.go` | 50 | `(*Arenas).ListSet` | List element write |
| `accessors.go` | 59 | `(*Arenas).StructField` | Struct field read |
| `accessors.go` | 68 | `(*Arenas).StructSetField` | Struct field write |
| `accessors.go` | 77 | `(*Arenas).PairFst` | Pair first |
| `accessors.go` | 86 | `(*Arenas).PairSnd` | Pair second |
| `accessors.go` | 95 | `(*Arenas).I64Arr` | i64-array backing slice |
| `accessors.go` | 104 | `(*Arenas).F64Arr` | f64-array backing slice |
| `accessors.go` | 113 | `(*Arenas).U8Arr` | u8-array backing slice |
| `accessors.go` | 124 | `(*Arenas).Free` | Return slot to free list |
| `gc.go` | 66 | `(*Arenas).markCell` | Mark phase of GC |
| `maps.go` | 68 | `(*Arenas).MapGet` | Map lookup |
| `maps.go` | 96 | `(*Arenas).MapSet` | Map insert |
| `memory.go` | 343 | `cellIsLocal` | Stack-frame-local handle check |
| `vm.go` | 818-897 (14 sites) | dispatch loop | Typed-array fast paths |

Total: 31 sites. All structurally opaque.

### Class C2: gen named, flows only into `MakeHandle` (handle round-trip)

The caller binds gen as a real identifier because it needs to round-trip the handle (rewrite idx, keep gen). The gen value flows handle → handle and never escapes to a non-handle Cell.

| File | Line | Function | Sink |
|------|------|----------|------|
| `memory.go` | 256 | `(*Arenas).handleCellReturn` | `MakeHandle(tag, gen, mark)` at line 271 |

Total: 1 site. This is the only legitimate use of the gen name in the package. It corresponds to the "compaction returns a rewritten handle to the caller" path in tail-call/return ABI; the rewritten handle has the same gen as the original (because the slot is the same slot, just at a new index after compaction).

### Class C3: gen named, flows elsewhere (forbidden)

Empty as of 2026-05-21. The `runtime/vm3/genopacity_test.go::TestGenerationOpacity` AST check is wedged against this class: any new entry causes the test to fail with a `gen leak: ...` message naming the file, line, and identifier.

## 3. Why this is the entire surface

The audit covers every `DecodeHandle` call site because that method is the *only* way to obtain the gen field from a handle Cell. The method is the structural choke point:

- The handle Cell is a `uint64` (see `cell.go::Cell`). The `genShift` (32) and `genMask` constants that name the gen field's bit position are package-private. A caller outside `runtime/vm3` cannot synthesize the shift/mask combination without re-deriving the constants from the package source, which would constitute an explicit reach-around that code review must catch.
- Inside `runtime/vm3` the only place those constants are read is `DecodeHandle`'s body. A grep for `genShift` returns one usage in `cell.go` (the method itself) and zero usages in any other file. The `genMask` constant is read by `DecodeHandle` only.
- `MakeHandle` is the inverse and takes gen as a parameter. Round-trip is therefore the only way gen flows handle → handle.

Combining the two: every code path that could read gen is a `DecodeHandle` callsite; the AST test enumerates them; the audit table above pins their classification. The property is structurally closed.

## 4. JIT lowering (forward look)

vm3jit is not in tree as of 2026-05-21 (Phase 5 of MEP-40 / Phase 5 of MEP-41). When it lands, the gen-opacity audit extends to: every JIT-emitted gen-check sequence must use a register that the JIT register allocator marks as a compiler-internal temporary, never spilled to a named slot that user SSA can reference. The Phase 5 audit will extend `runtime/vm3jit/genopacity_test.go` (or its equivalent) to walk the JIT's IR and assert this property at the register-class level.

Until vm3jit exists, the interpreter is the only execution surface and the audit above is complete.

## 5. Permitted additions

Any future PR that adds a legitimate gen consumer (for example, a debug-mode handle dump that prints gen for diagnostic output) must:

1. Land the consumer behind a debug build tag (`//go:build vm3debug`) so the consumer is not in the default binary.
2. Extend `runtime/vm3/genopacity_test.go::allowedGenSinks` to include the new sink, with the entry naming the MEP and reviewer.
3. Document the addition here under §5 below with the same justification.

No entries have been added under §5 as of 2026-05-21.

## 6. Cross-references

- MEP-41 §6.2 rule class C (this audit's normative source).
- MEP-41 §8 Phase 2 (this audit's gate).
- `runtime/vm3/cell.go::DecodeHandle` (the single API surface).
- `runtime/vm3/genopacity_test.go` (this audit's executable form).
- `compiler3/verify/verify.go::checkRuleC` (the bytecode-layer half).
- Apple MIE technical paper, "Memory Integrity Enforcement", 2025-09-09 (the TCE thesis that motivates this rule).
