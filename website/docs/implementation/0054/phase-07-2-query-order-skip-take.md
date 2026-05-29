---
title: "Phase 7.2. Query order_by + skip / take"
sidebar_position: 14
sidebar_label: "Phase 7.2. order_by + skip / take"
description: "MEP-54 Phase 7.2, post-query patterns (ListSortAsc / ListSlice) recognised and lowered to slices.Sort + mochiListSlice."
---

# Phase 7.2. Query `order_by` + skip / take

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22559](https://github.com/mochilang/mochi/pull/22559) |
| Commit         | 5e2e396f95 |

## Gate

8 new fixtures cover `order_by` (ints, strings), `skip`, `take`, `skip+take`, `order+take`, `order+skip+take`, and `order+filter` compositions. All pass under `go test ./transpiler3/go/build/... -run TestPhase1Hello/query_`.

## Lowering decisions

The shared C lowerer emits `order by` as a `__queryN = ListSortAscExpr(__queryN)` self-assignment and `skip n / take m` as a `__queryN = ListSliceExpr(__queryN, s, e)` self-assignment. Both forms always have the result var aliased on both sides so the in-place sort matches Mochi semantics without an extra clone.

- `ListSortAscExpr(xs)` lowers to `slices.Sort(xs)` (a statement) when it appears on the right-hand side of a self-assign; the `slices` import is registered on demand.
- `ListSliceExpr(xs, start, end)` lowers to `mochiListSlice(xs, int(start), int(end))`, a small generic helper that clamps both bounds. The C lowerer encodes `skip n` with `end=INT64_MAX/2` which would panic if fed directly to Go's slice operator, so the helper does the clamp.

`mochiListSlice` is introduced via a new helper-injection path in the lowerer: `addHelper(name)` records that a helper is needed; an `emittedHelpers` pass prepends the helper's `RawDecl` text before `main`. The corresponding `gotree.RawDecl` is a verbatim code block used only by these inline helpers. The pattern keeps the generated file self-contained without an extra runtime module dependency, which matters for the Phase 15 single-file `go-module` target.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/stmt.go` | Recognise `__queryN = ListSortAscExpr(__queryN)` -> `slices.Sort(__queryN)`; recognise `__queryN = ListSliceExpr(...)` -> `__queryN = mochiListSlice(...)` |
| `transpiler3/go/lower/lower.go` | `addHelper(name)` + `emittedHelpers` pass; `mochiListSlice` helper text |
| `transpiler3/go/gotree/` | `RawDecl` node for verbatim helper code |
| `tests/transpiler3/go/fixtures/query_order_*/`, `query_skip_*/`, `query_take_*/`, ... | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/query_*` subtests covering order_by / skip / take / their compositions.

## Closeout notes

Recognising the self-assign pattern specifically (rather than treating `ListSortAscExpr` as an arbitrary expression) lets the Go output stay statement-level: `slices.Sort(xs)` returns nothing, so the C-style "expression that mutates and returns" lowering would have needed an IIFE wrapper. The helper-injection pass is a generic mechanism: every later phase that needs an inline helper (`mochiAbsI64`, `mochiTry`, `mochiLines`, `mochiPanicValue`, `mochiOMap`, ...) registers via the same `addHelper` API.
