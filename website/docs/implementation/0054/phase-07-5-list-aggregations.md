---
title: "Phase 7.5. List aggregations + arena_query"
sidebar_position: 17
sidebar_label: "Phase 7.5. List aggregations"
description: "MEP-54 Phase 7.5, sum/min/max/contains reductions + NumCastExpr (float -> int) + StrUpper/Lower so arena_query fixtures build."
---

# Phase 7.5. List aggregations + arena_query

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22567](https://github.com/mochilang/mochi/pull/22567) |
| Commit         | cb2410a13d |

## Gate

17 fixtures: 4 math (`sum_int`, `list_contains_int`, `list_contains_str`, `list_contains_bool`), 5 type-cast (`list_max_int`, `list_max_string`, `list_min_int`, `list_min_float`, `min_max_combined`), 8 arena (`arena_int_filter`, `arena_bool_filter`, `arena_float_select`, `arena_str_select`, `arena_nested_query`, `arena_order_take`, `arena_large_result`, `arena_join_inner`). All 193 transpiler3/go fixtures green.

## Lowering decisions

The four list reductions lower to inline helpers because Go 1.22's stdlib lacks a generic min/max/sum over slices that preserves the rune-aware behaviour needed for strings:

- `ListSumExpr` -> `mochiListSumI64` or `mochiListSumF64` depending on the element type. The accumulator type pin matches the element type (`int64`, `float64`) so summation is bit-exact relative to a sequential `for _, v := range xs { acc += v }`.
- `ListMinExpr` / `ListMaxExpr` -> `mochiListMin{I64,F64,Str}` / `mochiListMax{I64,F64,Str}`. String variants compare lex-by-byte (matching the C runtime's `strcmp`).
- `ListContainsExpr` -> `slices.Contains` for the `v in xs` predicate; the `slices` import is registered on demand. No custom helper because the stdlib version is type-parameterised and handles all four scalar element types.

`NumCastExpr` lowers `int(x)` for float -> int truncation to `int64(x)`. The truncate-toward-zero semantics match both Mochi and Go.

`StrUpperExpr` / `StrLowerExpr` lower to `strings.ToUpper(s)` / `strings.ToLower(s)`. These are pulled in by `arena_str_select` even though they don't strictly belong to the aggregation surface; bundling them here avoided a separate Phase 7.5.1 just for strings.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `ListSumExpr`, `ListMinExpr`, `ListMaxExpr`, `ListContainsExpr`, `NumCastExpr`, `StrUpperExpr`, `StrLowerExpr` |
| `transpiler3/go/lower/lower.go` | `mochiListSumI64`, `mochiListSumF64`, `mochiListMin{I64,F64,Str}`, `mochiListMax{I64,F64,Str}` helper texts |
| `tests/transpiler3/go/fixtures/sum_int/`, `list_*/`, `arena_*/`, `min_max_*/` | 17 fixtures |

## Test set

- 17 `TestPhase1Hello/sum_int`, `list_contains_*`, `list_max_*`, `list_min_*`, `min_max_combined`, `arena_*` subtests.

## Closeout notes

Phase 7.5 was scoped to "what arena_query needs" rather than "every list aggregation in MEP-54 §X" because the arena_query fixture suite is the regression goalpost from the JVM baseline. Hitting all 8 arena fixtures in one phase was a forcing function: each one transitively requires a different aggregation, and shipping them piecemeal would have hidden inter-aggregation bugs (e.g., min and max sharing a string-comparison subtle).
