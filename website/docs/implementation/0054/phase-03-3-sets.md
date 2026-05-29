---
title: "Phase 3.3. set<T> for scalar T"
sidebar_position: 7
sidebar_label: "Phase 3.3. set<T>"
description: "MEP-54 Phase 3.3, set<T> lowering to map[T]struct{} with sorted iteration."
---

# Phase 3.3. `set<T>` for scalar T

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22501](https://github.com/mochilang/mochi/pull/22501) |
| Commit         | 87eed650a6 |

## Gate

10 fixtures cover the scalar set surface: `set_lit`, `set_has`, `set_len`, `set_add`, `set_for_each`, `set_strings`, `set_dedup`, `set_empty`, `set_floats`, `set_count_unique`. Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/set_`.

## Lowering decisions

Mochi `set<T>` lowers to Go's idiomatic set encoding `map[T]struct{}`. Set literals emit an IIFE so duplicate input elements collapse via idempotent `s[k] = struct{}{}` assignments: `func() map[T]struct{} { s := make(map[T]struct{}); s[a] = struct{}{}; s[b] = struct{}{}; return s }()`. Choosing `map[T]struct{}` over `map[T]bool` is the standard Go memory-saving trick: a `struct{}` value is zero-sized, so the underlying hashtable allocates only for keys, not values.

`SetHasExpr` lowers via the same two-value-index IIFE as `MapHasExpr`. `SetLenExpr` wraps `len(s)` with `int64(...)`. `SetAddStmt` lowers to `s[k] = struct{}{}`. `ForEachStmt` over a set iterates via `slices.Sorted(maps.Keys(s))` to preserve Mochi's sorted-iteration semantics; without the sort, Go's runtime randomisation would surface in fixture goldens. The `slices` and `maps` stdlib imports are registered on demand.

`lowerSetType` returns `map[Elem]struct{}` and is threaded into `letTypeText`, `paramTypeText` (Phase 6.0), and `returnTypeText` (Phase 6.0) so set values can be locals, function arguments, and return values uniformly.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerSetType(elem)` -> `map[Elem]struct{}` |
| `transpiler3/go/lower/expr.go` | `SetLit` IIFE; `SetHasExpr`; `SetLenExpr` |
| `transpiler3/go/lower/stmt.go` | `SetAddStmt` -> `s[k] = struct{}{}`; `ForEachStmt` over a set sorts keys |
| `tests/transpiler3/go/fixtures/set_*/` | 10 set fixtures |

## Test set

- `TestPhase1Hello/set_lit`, `set_has`, `set_len`, `set_add`, `set_for_each`, `set_strings`, `set_dedup`, `set_empty`, `set_floats`, `set_count_unique`.

## Closeout notes

The IIFE-on-literal pattern was preferred over a runtime helper because each fixture's literal is small and inlined construction keeps the dependency graph simpler. The set's sorted iteration mirrors the map decision in Phase 3.2; both are essential for the reproducibility pin in Phase 16 to make any sense.
