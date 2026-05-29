---
title: "Phase 7.12. List HOFs (map / filter / reduce)"
sidebar_position: 24
sidebar_label: "Phase 7.12. List HOFs"
description: "MEP-54 Phase 7.12, ListMapExpr / ListFilterExpr / ListFoldlExpr lowered as IIFEs over the input slice."
---

# Phase 7.12. List HOFs

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | [#22612](https://github.com/mochilang/mochi/pull/22612) |
| Commit         | 24b8ac0274 |

## Gate

8 fixtures: `hof_map`, `hof_filter`, `hof_reduce`, `hof_map_str`, `hof_filter_str`, `hof_reduce_float`, `hof_combined`, `hof_map_to_bool`. 261 transpiler3/go fixtures green (was 253).

## Lowering decisions

The three Phase 6.1 higher-order-list nodes dispatch in `lowerExpr`. Each is emitted as an IIFE that walks the input slice and either projects, filters, or folds elements through the function value. The function value is already a Go func of the right signature (the `FunLit` / `VarRef` path in `lowerExpr` produces a callable closure from Phase 6.1), so the call site is a plain `fn(x)` or `fn(acc, x)`.

- `ListMapExpr(xs, fn)` -> `func() []B { out := make([]B, 0, len(xs)); for _, x := range xs { out = append(out, fn(x)) }; return out }()`. The result element type comes from `ListMapExpr.ElemType` (B in `map(xs: list<A>, fn: fun(A): B): list<B>`).
- `ListFilterExpr(xs, pred)` -> same shape but `if pred(x) { out = append(out, x) }`. Element type comes from `ListFilterExpr.ElemType` (preserved input element type).
- `ListFoldlExpr(xs, fn, init)` -> `func() T { acc := init; for _, x := range xs { acc = fn(acc, x) }; return acc }()`. Accumulator type from `ListFoldlExpr.AccType`.

The IIFE pattern keeps each HOF as an expression so it composes inside larger expressions (the `hof_combined` fixture chains filter -> map -> reduce). Using the named element type from the aotir node, rather than inferring from the function value's return type, avoids a Phase 7.12-time type-checker reimplementation.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `ListMapExpr`, `ListFilterExpr`, `ListFoldlExpr` IIFE lowering |
| `tests/transpiler3/go/fixtures/hof_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/hof_*` subtests covering int doubling, even-filter, sum-reduce, string upper-mapping, string-substring filter, float reduce, the canonical filter+map+reduce pipeline, and map-to-bool element-type changes.

## Closeout notes

Phase 7.12 wraps up the Phase 7 query / HOF surface. The next sub-phase plan is Phase 8 (Datalog) and Phase 9.2 (streams); Phase 9.1 (channels) shipped on the staging branch ahead of Phase 8 because channels turned out to be a small, self-contained patch while Datalog's compile-time semi-naive engine is a larger build.
