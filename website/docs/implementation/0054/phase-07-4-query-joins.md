---
title: "Phase 7.4. Query joins (inner / cross / left)"
sidebar_position: 16
sidebar_label: "Phase 7.4. Query joins"
description: "MEP-54 Phase 7.4, cross / inner / left joins via hash-join desugaring (map[K][]E index) end-to-end through the Go pipeline."
---

# Phase 7.4. Query joins

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22564](https://github.com/mochilang/mochi/pull/22564) |
| Commit         | 2ec49d122f |

## Gate

8 fixtures cover the join surface lifted from the JVM `query_join/` baseline: `join_cross_basic`, `join_cross_strings`, `join_cross_where`, `join_inner_int`, `join_inner_sum`, `join_inner_where`, `join_left_basic`, `join_left_filtered`. Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/join_`.

## Lowering decisions

The shared C lowerer's hash-join desugaring emits a `map<K, list<K>>` index keyed by the join key, plus a `ForEach` over the inner side that consults the index and skips when the key is absent. Phase 7.4 teaches the Go lowerer three new things to consume that shape:

1. `lowerMapTypeWithList(K, list, E)` -> `map[K][]E`. The new `ListValueElemType` field on `aotir.LetStmt` and `aotir.MapLit` carries the element type so the lowerer can produce the right Go map type. Without this, the C lowerer's `map<int, list<int>>` would lose the inner element type at the Go boundary.

2. `StrConvertExpr` for `str(x)` projections. Inner-int / float / bool conversions go through Go's `strconv.Itoa(int(x))`, `strconv.FormatFloat(x, 'g', -1, 64)`, and `strconv.FormatBool(x)`. The string -> string case is the identity. `join_cross_strings` exercises the projection path.

3. `blockReferencesVar` scan to drop unused `ForEach` iteration bindings. When the hash-join inner loop binds a row variable that the body never reads (the join collapses by key only), the Go output emits `for range xs { ... }` so Go's "declared and not used" check stays quiet.

The inner-join arm uses the index for O(1) probe per outer row; the left-join arm emits the same probe but with a `if len(matches) == 0 { ... }` branch to surface the unmatched outer row. The cross-join arm is the simple double-loop with no index. The `RawExpr` helper holds the inline type text (`chan T`, `[]T`, etc.) so the new `map[K][]E` shape doesn't need a fresh AST node.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerMapTypeWithList(K, value, listValueElem)` -> `map[K][]E` when `value==TypeList` |
| `transpiler3/go/lower/expr.go` | `StrConvertExpr` (int/float/bool/string sources) |
| `transpiler3/go/lower/stmt.go` | `blockReferencesVar` scan -> drop unused ForEach bindings |
| `tests/transpiler3/go/fixtures/join_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/join_*` subtests covering inner / cross / left joins with int and string keys, with and without `where` post-filters.

## Closeout notes

The `blockReferencesVar` scan was a Go-specific necessity: Go's compile-time "declared and not used" is the C runtime's silent dead store, so the lowerer had to learn to elide the binding. Adding the scan in 7.4 (rather than waiting for a Phase 7.7-time spurious test failure) saved an audit pass. The `RawExpr`-based map-type printing avoided adding a fresh `MapTypeExpr` AST node when the inline string was unambiguous.
