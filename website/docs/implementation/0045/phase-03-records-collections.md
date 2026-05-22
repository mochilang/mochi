---
title: "Phase 3. Records, lists, maps, sets"
sidebar_position: 5
sidebar_label: "Phase 3. Records + collections"
description: "MEP-45 Phase 3 tracking: record types, list<T>, map<K,V>, set<T>, omap<K,V>, monomorphisation pass."
---

# Phase 3. Records, lists, maps, sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 3](/docs/mep/mep-0045#phase-3-records-lists-maps-sets) |
| Status         | IN PROGRESS |
| Started        | 2026-05-22 22:07 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Records / collections fixture suite (~80 cases) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 3.0 starts. Records + collections unlock realistic data-shaping code; without them no useful Mochi program compiles. Aligns._

## Sub-phases

| #   | Scope                                                                                                                          | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 3.0 | Record types: `struct mochi_R` in source field order; field access; record literals; equality                                  | LANDED      | —      | — |
| 3.1 | `list<T>`: per-T `mochi_list_<T>` (i64 / f64 / bool / str); literals `[e, ...]`; `xs[i]`; `len(xs)`; `append`; `for x in xs`   | LANDED      | —      | — |
| 3.2 | `map<K,V>`: per-(K,V) open-addressing hash table; literals `{k: v, ...}`; `m[k]`; `len`; `keys`; `values`; `k in m`; `for k in m` | IN PROGRESS | —      | — |
| 3.3 | `set<T>`: Swiss table with elided value slot; `+`, `-`, `contains`, `len`                                                      | NOT STARTED | —      | — |
| 3.4 | Monomorphisation pass: `transpiler3/c/lower/mono.go` lowers each concrete instantiation once, deterministic ordering           | NOT STARTED | —      | — |
| 3.5 | `omap<K,V>` (insertion-order map): Swiss table + parallel insertion-order list (needed by Phase 8)                             | NOT STARTED | —      | — |

## Decisions made

### Phase 3.0 (records)

- **Started:** 2026-05-22 22:07 (GMT+7)
- **Goal-alignment audit:** Records are the floor for every later phase that touches data. Without `type R { ... }`, neither Phase 3.1 (`list<R>`), Phase 3.2 (`map<K, R>`), Phase 4 (sum types over records), Phase 6 (string/IO with structured input), nor Phase 8 (query DSL row shape) can land. The user-facing goal moves: a Mochi program that defines a struct, instantiates it, accesses fields, and prints can now be AOT-compiled to a single C TU. Aligns.
- **Flat `Type` enum + parallel `RecordName` strings.** `aotir.Type` stays an `int`-backed enum with one new variant (`TypeRecord`). The record's identity rides on a parallel `RecordName` string on every IR node that can carry a record value (`Param`, `LetStmt`, `VarRef`, `Function.ReturnRecordName`, `CallExpr.ResultRecordName`, `FieldAccess.ResultRecordName`). Rationale: refactoring `Type` into a struct would force a touch on every type-compare site in the verifier, lower, and emit; the parallel-string approach changes ~12 lines per carrier instead.
- **Field-order normalisation in the lowerer, not the verifier.** `lowerStructLit` reorders the user-supplied literal arguments into the record-declaration order. The verifier then strictly enforces decl-order (the emit pass relies on this so designated-init args render in struct-field order, keeping the C source byte-deterministic for byte-equal inputs).
- **Pass 0 collects records before Pass 1 collects fn signatures.** This means a `fun mk(): R` can reference `type R` even when the `type` decl is below the `fun` in source order (Mochi has no forward decls).
- **No nested records in 3.0.** A record field whose type is itself a record is rejected at lower-time. The IR already plumbs `RecordField.RecordName` and `FieldAccess.ResultRecordName` for the eventual unlock; the per-record `mochi_eq_<Name>` helper has the nested-record branch wired but never reached. Lands properly with 3.0.x once a fixture set exists.
- **No `print(record)` in 3.0.** Whole-record printing requires a per-record formatter (deferred to a later sub-phase if needed). `print(r.field)` works for any scalar field; that covers every fixture.
- **No field assignment in 3.0.** `r.f = v` is rejected: records are value-semantics, so the surface would mean "rebuild and rebind `r`", which the syntax does not express. Re-assign the whole binding instead.
- **Equality is structural and field-wise.** `BinEqRec` lowers to `mochi_eq_<Name>(a, b)`, a per-record helper that pairwise-compares each field with the appropriate primitive operator (`==` for scalars, `strcmp` for strings). `BinNeRec` is `!mochi_eq_<Name>(...)`. Cross-record-type comparisons are rejected at verify-time. This also unlocks direct string equality via `BinEqStr` / `BinNeStr` (`==` / `!=` on two string operands lower to `strcmp(a,b) == 0` / `!= 0`).
- **C99 designated init for record literals.** A `Pt { x: 1, y: 2 }` literal emits as `(struct mochi_Pt){.x = INT64_C(1), .y = INT64_C(2)}` (a C99 compound literal). Lifetime: the lowerer always binds the literal to a name (or passes it as an arg), so the storage extends to the surrounding block scope under C99 rules.
- **Records are passed and returned by value.** `static struct mochi_R foo(struct mochi_R p)`. No `*` / heap allocation; the C ABI handles it. This matches Mochi's value-semantics.
- **Field naming clash with Mochi keywords.** Mochi reserves words like `on` (event syntax). Record fields named with such words are rejected by the surface parser, not by the lowerer. Two fixtures originally used `on` and were renamed to `enabled` before the record-bool gate would parse.

### Phase 3.1 (lists)

- **Started:** 2026-05-22 22:38 (GMT+7)
- **Goal-alignment audit:** Lists unlock realistic data-flow code: collecting values across loop iterations, holding parsed lines from input, threading results through `append` in a fold. Without them the surface tops out at scalar arithmetic. Phase 3.2 (`map<K, V>`) reuses the per-T runtime + monomorphisation pattern that 3.1 introduces; Phase 6 (string/IO) needs list-of-string to model line streams; Phase 8 (query DSL) needs list-of-record as the row sequence. So 3.1 is the smallest step that moves the user-facing goal and the largest pre-req for everything after. Aligns.
- **Scalar element types only in 3.1 (`int`, `float`, `bool`, `string`).** Record and list element types are rejected at lower-time with a Phase 3.1 diagnostic. The IR carries `ElemType` on every list-bearing carrier (parallel-field pattern, same as Phase 3.0's `RecordName`); `list<R>` and `list<list<T>>` will unlock by widening the lower-time predicate and adding `mochi_list_<RecordName>` instantiations in Phase 3.4 monomorphisation. The runtime ships only the four scalar instantiations; the i64 path is the reference and the f64 / bool / str helpers are direct copies with the element type substituted.
- **Functional `append` semantics.** `xs = append(xs, v)` returns a new list with a freshly-malloced buffer and leaves the input list's buffer untouched. Two reasons. First, this matches vm3's `OpAppend` oracle: vm3 builds a new list value, the AOT-C path must too, or any program that holds an alias to the pre-append list will diverge byte-for-byte. Second, in-place grow with shared backing storage is a Phase 18 (perf) optimisation that requires escape analysis to make safe; we'd rather pay an O(n) memcpy per append now than ship a divergence-prone aliasing model and clean it up later. Empty lists allocate no buffer (data = NULL, len = cap = 0); the first append mallocs a 1-element buffer.
- **Empty list literal `[]` is rejected.** A bare `[]` has no element-type information for the lowerer to pick a runtime instantiation, and the surface has no annotation form yet. Programs that need an empty list must use `let xs: list<int> = []` once Phase 3.4 lands a typed-empty path, or seed with one element. Rejection message: `"empty list literal: Phase 3.1 requires at least one element so the element type can be inferred"`.
- **`xs[i]` is a runtime bounds check, not a verified static one.** The index expression always lowers to `mochi_list_<T>_index(xs, i)` which compares against `len` and trips `mochi_panic_index()` on out-of-range (exit code 4, `MOCHI_ERR_INDEX`). Negative indices are rejected (vm3 does not have Python-style negative indexing; the AOT path must agree). Slice syntax (`xs[a..b]`) is rejected in 3.1 with a Phase 3.1 diagnostic; it lands with the string/list slice unification in Phase 6.
- **C99 compound literal for list-lit buffer storage.** `[1, 2, 3]` emits as `mochi_list_i64_lit((const int64_t[]){INT64_C(1), INT64_C(2), INT64_C(3)}, INT64_C(3))`. The compound literal lives in automatic storage at the enclosing block, then `mochi_list_i64_lit` memcpys it into a heap-malloced buffer that the list value owns; this keeps the list literal source-deterministic and avoids a per-fixture static initialiser.
- **Per-T runtime now, monomorphisation later.** 3.1 ships four hand-written instantiations of the list helpers (one per scalar element type, ~40 LOC each). This is small enough to maintain by hand and avoids dragging the monomorphisation pass forward. Phase 3.4 collects the concrete `(op, T)` set the program uses and emits exactly the helpers needed; the runtime then drops the unused per-T helpers from libmochi. Until then, every program links against all four instantiations regardless of which it uses; dead-code elimination at the linker handles the bloat.
- **`for x in xs` is a length-bounded numeric loop.** The for-each lowers to a fresh block that captures the list into a `__mochi_list_x` temp, queries `len` once into `__mochi_len_x`, and iterates `0..len` with `__mochi_i_x`, fetching `x = mochi_list_<T>_index(__mochi_list_x, __mochi_i_x)`. The capture + length-cache pattern matches vm3's iterator snapshot semantics (mutation of the underlying list inside the loop body does not change the iteration set). The temp names are mangled with the loop variable to avoid nested-for collisions.
- **Lists are passed and returned by value.** `static mochi_list_i64 foo(mochi_list_i64 xs)`. The struct header (data + len + cap) copies; the buffer is shared until the next `append` allocates a fresh one. This matches Mochi's value-semantics surface and the C ABI handles the by-value struct passing without any heap-alloc hops.
- **Lifetime: leak on program exit in 3.1.** No `free()` calls. The OS reclaims at process tear-down. Phase 7 (error model + panic unwinding) adds a real reclamation path; until then the lists keep their buffers for the full program lifetime. The tradeoff is intentional: a refcounted or arena-backed scheme adds runtime complexity that the fixture suite would not exercise, and shipping the user-facing surface earlier is the goal.

### Phase 3.2 (maps)

- **Started:** 2026-05-22 23:00 (GMT+7)
- **Gate green:** 2026-05-22 23:25 (GMT+7), 22 fixtures passing (`TestPhase3Maps`).
- **Goal-alignment audit:** Maps are the smallest collection that lets a Mochi program model lookup tables (word counts, lookup-by-id, configuration, indices into other collections). Without them every program that needs associative storage has to fall back to two parallel lists + linear scan. Phase 3.5 (`omap<K,V>`) inherits the entire per-(K,V) runtime + parallel-field plumbing this phase introduces; Phase 8 (query DSL) needs `map<K, list<R>>` as the group-by accumulator shape; Phase 9 (streams/agents) needs `map<id, agent>` as the actor-registry shape. So 3.2 is the smallest pre-req that unlocks four downstream phases worth of code. Aligns.
- **Hand-rolled open-addressing, not cwisstable.** The original MEP table named cwisstable as the underlying table. We swapped to a hand-written open-addressing + linear-probe + load-factor-0.5 implementation (~120 LOC under `MOCHI_MAP_DEFINE`) for three reasons. First, no vendored C dep keeps the runtime portable across the eventual cross-target matrix (Phase 11) without picking up `<emmintrin.h>` or `<arm_neon.h>` SIMD intrinsics. Second, the SplitMix64 finalizer for i64 keys matches `runtime/vm3/maps.go::hashI64` byte-for-byte, so fixtures that count hash-table collisions stay deterministic across the vm3 oracle and the AOT path. Third, ~120 LOC of macro-expanded C beats ~3kLOC of vendored SIMD when the fixtures top out at ~100 entries; cwisstable's perf advantage doesn't appear until Phase 18 perf work, where the table swap is a one-symbol-set substitution behind the same `mochi_map_<K>_<V>_*` ABI.
- **Eight (K,V) instantiations in 3.2.** Keys are restricted to `int` and `string`; values to the four scalars (`int`, `float`, `bool`, `string`). The runtime ships all eight under `mochi_map_<K>_<V>_*` symbol names; the lowerer rejects record / list keys or values at lower-time with a phase-named diagnostic. Map-of-record and nested maps land with Phase 3.4 monomorphisation, same as the list-of-record path.
- **`KeyType` + `ValueType` parallel fields on every map-bearing IR carrier.** Same parallel-field pattern as Phase 3.0's `RecordName` and Phase 3.1's `ElemType`. `Param`, `Function.ReturnKeyType` / `ReturnValueType`, `LetStmt`, `VarRef`, `CallExpr.ResultKeyType` / `ResultValueType`, and the six map exprs (`MapLit`, `MapGetExpr`, `MapHasExpr`, `MapLenExpr`, `MapKeysExpr`, `MapValuesExpr`) carry the parallel pair beside `Type==TypeMap`. The alternative (`Type` as a struct, or a registry-of-instantiations) would touch every type-compare site in the verifier; the parallel-field approach changes ~20 lines per carrier instead.
- **Sort-on-iteration via insertion-sort snapshot.** Both `for k in m` and `keys(m)` / `values(m)` snapshot the live entries into a fresh buffer, then insertion-sort by key (`<` for i64, `strcmp` for strings). This matches vm's key-sorted iteration order so byte-equal stdout against the vm3 oracle is achievable. Insertion sort suffices at fixture scale (~100 entries worst case); a merge-sort or heap-sort drop-in is a Phase 18 perf task behind the same snapshot interface.
- **Functional / immutable in 3.2.** No `m[k] = v`, no `delete(m, k)`. Maps are constructed once from a literal (or `let m = mk()`) and queried thereafter. Mutation lands in a later sub-phase that also brings list mutation; until then `m = {...}` rebinds the whole map. This matches the vm3 oracle's value-semantics for map literals.
- **`k in m` is the membership test, no `has()` surface.** The Mochi type-checker recognises `in` as a binary operator that produces `bool` when the right operand is a `map<K, V>`. The lowerer special-cases that pattern in `lowerBinary` to emit a `MapHasExpr` instead of a `BinaryExpr`. We also wire a `has(m, k)` builtin in the lowerer so future fixtures that prefer call syntax still work; both surfaces lower to the same `MapHasExpr` node.
- **`m[k]` panics on missing key in 3.2; no `Option` yet.** A failed lookup trips `mochi_panic_index()` (exit code 4, `MOCHI_ERR_INDEX`); programs that must probe first should use `k in m` and only then index. The Mochi type-checker types `m[k]` as `option<V>` on the surface, so any arithmetic that uses `m[k]` directly fails type-check today; fixtures sum via `values(m)` or index after a successful `in` check. Real option-aware unwrap arrives with Phase 4 (option types).
- **`for k in m` is sugar for `for k in keys(m)`.** The lowerer synthesises a `MapKeysExpr` inline and re-uses the existing `ForEachStmt`. No new IR node, no new emit-time path; the emitter doesn't even know the loop's source was a map. This keeps the for-each shape stable across collection types.
- **Empty map literal `{}` is rejected (mirrors list rule).** A bare `{}` has no `(K, V)` for the lowerer to pick an instantiation, and the surface has no annotated-empty form yet. A future typed-empty path lands with Phase 3.4 once the lowerer threads annotated-binding types into literal-time resolution. Rejection message: `"empty map literal: Phase 3.2 requires at least one entry so the key and value types can be inferred"`.
- **Lifetime: leak on program exit, same as 3.1.** No `free()` for table buffers or the per-call snapshot allocations under `keys()` / `values()`. Phase 7 (panic unwinding) adds a real reclamation path. The per-call snapshot allocations are a known leak proportional to call count, accepted for fixture-scale programs.

## Deferred work

_Concurrent-safe maps: not in v1 (use a stream/agent boundary)._

- Phase 3.0: `print(record)`, field assignment (`r.f = v`), nested-record fields, generic record decls, methods on records. All have unblocked paths in the IR (no struct shape changes needed) and will land as 3.0.x sub-phases or with Phase 4.
- Phase 3.1: `list<R>` / `list<list<T>>` (deferred to 3.4 monomorphisation); `print(list)` (deferred until a per-T list formatter lands); list slice `xs[a..b]` (deferred to Phase 6); list equality `xs == ys` (deferred to 3.4); in-place mutation aliasing model (deferred to Phase 18 perf); empty-list literal with type annotation (deferred to 3.4).
- Phase 3.2: `map<K, R>` / `map<K, list<V>>` / `map<R, V>` (deferred to 3.4 monomorphisation); `m[k] = v` and `delete(m, k)` mutation (deferred along with list mutation); `print(map)` (deferred until a per-(K,V) map formatter lands); map equality `m == n` (deferred to 3.4); option-aware unwrap of `m[k]` for arithmetic (deferred to Phase 4 option types); empty-map literal with type annotation (deferred to 3.4); per-call snapshot allocations under `keys()` / `values()` accepted as a known leak until Phase 7 reclamation.

## Closeout notes

_Fill in after gate green._
