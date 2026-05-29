---
title: "Phase 17. Math (abs, floor, ceil) and map helpers (len, keys, values)"
sidebar_position: 21
sidebar_label: "Phase 17. Math and map helpers"
description: "MEP-56 Phase 17, math and Hash helper builtins lowered to Ruby instance methods."
---

# Phase 17. Math (abs, floor, ceil) and map helpers (len, keys, values)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | d4ab0240e7 |

## Gate

`TestPhase17MathMap` in `transpiler3/ruby/build/phase17_test.go`: four subtests (`math_abs_int`, `math_abs_float`, `math_floor_ceil`, `map_len`). Each subtest compiles a Mochi program (e.g. `abs(-7)` must print `7`, `floor(3.7)` must print `3`, `ceil(3.7)` must print `4`) and runs the emitted `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib`, then diffs stdout. The two `abs` subtests cover both the int (`abs_i64`) and float (`abs_f64`) IR signatures so the math dispatch is exercised on each path.

## Lowering decisions

Math is a single dispatch table on `aotir.MathCallExpr.Func`; map helpers are direct `Hash` method calls (`transpiler3/ruby/lower/lower.go` lines 1022 to 1053):

- `aotir.MathCallExpr` with `Func == "abs_i64"` or `"abs_f64"` to `MethodCall{Method: "abs"}` (lines 1022 to 1029). Ruby `Integer#abs` and `Float#abs` are both C-level and return the same primitive type, so the int / float split survives the lowering without a cast.
- `aotir.MathCallExpr` with `Func == "floor"` to `MethodCall{Method: "floor"}` (lines 1030 to 1031). Ruby `Float#floor` returns `Integer`, matching Mochi's `int` result type.
- `aotir.MathCallExpr` with `Func == "ceil"` to `MethodCall{Method: "ceil"}` (lines 1032 to 1033). Same `Float -> Integer` return as `floor`.
- An unrecognised `Func` returns `nil, fmt.Errorf("ruby lower: unknown math func %q", e.Func)` (lines 1034 to 1035), so future math additions (e.g. `sqrt`) fail loudly instead of silently producing wrong Ruby.
- `aotir.MapLenExpr` to `MethodCall{Method: "size"}` (lines 1036 to 1041); `Hash#size` is the canonical name (`length` is an alias but `size` matches the `set` / `array` choice).
- `aotir.MapKeysExpr` to `MethodCall{Method: "keys"}` (lines 1042 to 1047). `Hash#keys` returns `Array<K>` in insertion order, matching Mochi's `keys(map)` ordering guarantee since Phase 3.5 settled on insertion-ordered `Hash`.
- `aotir.MapValuesExpr` to `MethodCall{Method: "values"}` (lines 1048 to 1053).

The shared `MethodCall` shape lets all six builtins render through the same `rtree.MethodCall.RubyExprString()` formatter, so the emitted Ruby is consistently `receiver.method` with no per-call special casing.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `MathCallExpr` dispatch (lines 1022 to 1035) and three `Map*Expr` cases (lines 1036 to 1053) |
| `transpiler3/ruby/build/phase17_test.go` | `TestPhase17MathMap` with 4 subtests |

## Test set

- `TestPhase17MathMap/math_abs_int`, `math_abs_float`, `math_floor_ceil`, `map_len`.

## Closeout notes

Phase 17 landed on CRuby 4.0 (Homebrew). The `MathCallExpr.Func` dispatch is intentionally a switch with an explicit error default so a future Mochi-side math addition that forgets to update the Ruby lowerer surfaces as a build error, not a silent miscompile. `Hash#size` (not `length`) is the consistent name used across the set / map / omap pages so a reader skimming `phase-16` and `phase-17` together sees one idiom. `keys` and `values` come through the same `MethodCall` node as `len` to keep render shapes uniform; insertion order falls out of Ruby's `Hash` semantics for free.
