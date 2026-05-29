---
title: "Phase 3.1. list<T> for scalar T"
sidebar_position: 5
sidebar_label: "Phase 3.1. list<T>"
description: "MEP-54 Phase 3.1, list<int|float|string|bool> lowering to native Go slices ([]T)."
---

# Phase 3.1. `list<T>` for scalar T

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking PR    | [#22490](https://github.com/mochilang/mochi/pull/22490) |
| Commit         | 8bb154ea8f |

## Gate

12 fixtures cover scalar list values: `list_lit`, `list_index`, `list_len`, `list_append`, `list_set`, `list_for_each`, `list_strings`, `list_floats`, `list_bools`, `list_nested_loop`, `list_sum_loop`, `list_empty`. All 29 fixtures (Phase 1+2+3.1) pass byte-equal under `go test ./transpiler3/go/build/... -run TestPhase1Hello`.

## Lowering decisions

Mochi `list<int|float|string|bool>` lowers to a native Go slice `[]T`. `aotir.ListLit` emits `[]T{...}`; the lowerer threads the element type through `ElemType` so an empty `[]int{}` is distinguishable from `[]string{}`. `IndexExpr` emits `xs[int(i)]` with a narrow conversion (literal int indices stay bare to keep the printed source readable). `LenExpr` emits `int64(len(xs))` to preserve the int pin established in Phase 2; without the wrap the `len` result would be Go's machine-width `int`, inviting a 32/64-bit divergence on cross-compilation. `AppendExpr` emits `append(xs, v)` directly (Go's variadic append is the canonical idiom).

`ForEachStmt` lowers `for x in xs { ... }` to `for _, x := range xs { ... }`. The bare `_` ignores the index when the Mochi loop does not need it; Phase 7.4 later refines this to drop the binding entirely when unused (`for range xs`). `ListSetStmt` emits `xs[int(i)] = v` for in-place mutation. `LetStmt` is taught to consult `VarType` so a `list<int>` binding gets the right `[]int64` annotation.

The Go slice is value-typed (Go semantics) but list values in Mochi are mutable through aliasing once shared, matching Go's behaviour on a shared underlying array. The Mochi semantics page calls out that `xs = ys` is an alias; the Go lowerer relies on Go's reference-on-the-underlying-array behaviour to deliver that.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerListType(elem)` -> `[]Elem` (scalar elem types only) |
| `transpiler3/go/lower/expr.go` | `ListLit` -> `[]T{...}`; `IndexExpr` -> `xs[int(i)]`; `LenExpr` -> `int64(len(xs))`; `AppendExpr` -> `append(xs, v)` |
| `transpiler3/go/lower/stmt.go` | `ForEachStmt` -> `for _, x := range xs {...}`; `ListSetStmt` -> `xs[int(i)] = v`; `LetStmt` routes through `letTypeText` |
| `tests/transpiler3/go/fixtures/list_*/` | 12 list fixtures |

## Test set

- `TestPhase1Hello/list_lit`, `list_index`, `list_len`, `list_append`, `list_set`, `list_for_each`, `list_strings`, `list_floats`, `list_bools`, `list_nested_loop`, `list_sum_loop`, `list_empty`.

## Closeout notes

Wrapping `len` in `int64(...)` is non-negotiable: Phase 7.5's `ListSumExpr` later relies on the wrapper to keep the accumulator type consistent. The narrow `int(i)` on `IndexExpr` is intentionally wider than necessary (Go's `int` is 64-bit on the supported tuples) because uniformly wrapping makes the printer trivial; gofmt won't shorten `xs[int(0)]` to `xs[0]` but a literal-only optimisation kept the printed source clean for the `list_lit` golden file.
