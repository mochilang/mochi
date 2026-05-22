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
| 3.0 | Record types: `struct mochi_R` in source field order; field access; record literals; equality                                  | IN PROGRESS | —      | — |
| 3.1 | `list<T>`: `mochi_list__T` (growable dense vector); `[]`, `len`, `append`, `[i]`, slice                                        | NOT STARTED | —      | — |
| 3.2 | `map<K,V>`: cwisstable Swiss table per `(K,V)` instantiation; `m[k]`, `len`, `keys`, `values`                                  | NOT STARTED | —      | — |
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

## Deferred work

_Concurrent-safe maps: not in v1 (use a stream/agent boundary)._

- Phase 3.0: `print(record)`, field assignment (`r.f = v`), nested-record fields, generic record decls, methods on records. All have unblocked paths in the IR (no struct shape changes needed) and will land as 3.0.x sub-phases or with Phase 4.

## Closeout notes

_Fill in after gate green._
