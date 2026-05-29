---
title: "Phase 7.7. Math builtins"
sidebar_position: 19
sidebar_label: "Phase 7.7. Math builtins"
description: "MEP-54 Phase 7.7, MathCallExpr dispatch for abs/floor/ceil; abs_i64 via tiny helper, others via stdlib math."
---

# Phase 7.7. Math builtins

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22581](https://github.com/mochilang/mochi/pull/22581) |
| Commit         | 28fe55f223 |

## Gate

5 fixtures: `abs_int`, `abs_float`, `floor_basic`, `ceil_basic`, `math_combined`. 222 transpiler3/go fixtures green.

## Lowering decisions

`MathCallExpr` dispatches on the builtin name. `abs_i64` cannot use Go's `math.Abs` because that operates on `float64`; instead it lowers to a tiny `mochiAbsI64` helper that branches on sign (`if x < 0 { return -x }; return x`). The branch is the canonical Go idiom; Go has no integer math.Abs in the stdlib. `abs_f64`, `floor`, and `ceil` delegate to `math.Abs`, `math.Floor`, `math.Ceil` and register the `math` import on demand.

Floor and ceil return `float64` from the stdlib (matching IEEE semantics); the upstream type-checker has already verified the result is consumed where a `float` is expected, so no additional cast is emitted at the Go side.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `MathCallExpr` arm dispatching `abs_i64`, `abs_f64`, `floor`, `ceil` |
| `transpiler3/go/lower/lower.go` | `mochiAbsI64` helper text |
| `tests/transpiler3/go/fixtures/abs_*/`, `floor_*/`, `ceil_*/`, `math_combined/` | 5 fixtures |

## Test set

- 5 `TestPhase1Hello/abs_int`, `abs_float`, `floor_basic`, `ceil_basic`, `math_combined`.

## Closeout notes

Adding `mochiAbsI64` rather than `math.Abs(float64(x))` -> `int64(...)` avoided a float round-trip that would silently misbehave at the int64 extremes (e.g., `INT64_MIN` doesn't survive a round-trip through float64). The two-line helper is the right tradeoff over a clever oneliner.
