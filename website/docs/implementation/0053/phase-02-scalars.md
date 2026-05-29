---
title: "Phase 2. Scalars and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Scalars"
description: "MEP-53 Phase 2, scalars (int, float, bool, string), arithmetic, control flow (if / else / while / for), casts, deep string helpers."
---

# Phase 2. Scalars, control flow, casts, string helpers

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 2b5904fada |

## Gate

`TestPhase2Scalars` walks `tests/transpiler3/rust/fixtures/phase02-scalars/` (20 fixtures) and asserts byte-equal stdout. The fixture set covers integer arithmetic (add, sub, mul, div, mod), float arithmetic (with deterministic formatting: NaN, ±Inf, integer-valued floats render as the integer), boolean ops, string concat, string helpers (`upper`, `lower`, `reverse`, `len`, `contains`, `index`, `substring`), casts (`(int)f`, `(float)i`, `int(s)`, `str(i)`), `if / elif / else`, `while`, and `for i in lo..hi`.

## Lowering decisions

Scalars lower to native Rust primitives: `int` → `i64`, `float` → `f64`, `bool` → `bool`, `string` → `String`. Arithmetic uses the native operators; integer division and modulo route through `mochi_runtime::check::div_i64` / `mod_i64` to raise panic code 5 on zero divisor (consistent with vm3 semantics). Casts route through `mochi_runtime::conv` so the truncate-toward-zero semantics of `float_to_int` match vm3 exactly.

`print(v)` selects the runtime helper by typecheck-inferred type: `i64` → `print_i64`, `f64` → `print_f64`, `bool` → `print_bool`, `String` / `&str` → `print_str`. Float printing uses the runtime's deterministic formatting (NaN → "NaN", ±Inf → "+Inf" / "-Inf", integer-valued doubles in [-2^53, 2^53] render without a decimal point, otherwise the default `{}` Display).

Control flow lowers directly: `if cond { ... } else if cond { ... } else { ... }`, `while c { ... }`, `for i in lo..hi { ... }` (Rust's exclusive `..` range matches Mochi's). `break` and `continue` keep their names.

String helpers route through `mochi_runtime::strings` for char-aware (not byte-aware) semantics. `reverse(s)` collects chars in reverse and re-joins; `substring(s, lo, hi)` iterates chars by index; `len(s)` counts chars not bytes.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/lower.go` | Wire scalar arithmetic, control flow, casts |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `io::print_*`, `conv::*`, `strings::*`, `check::div_i64/mod_i64` |
| `transpiler3/rust/build/phase02_test.go` | 20-fixture gate |
| `tests/transpiler3/rust/fixtures/phase02-scalars/*.mochi` + `.out` | 20 fixtures |

## Test set

- `TestPhase2Scalars/<fixture>` for each `.mochi` in the fixture directory (20 fixtures).

## Closeout notes

Float deterministic formatting was the most subtle piece: vm3 prints `1.0` as `1` (no decimal) for integer-valued floats within the safe-integer range, but `1.5` as `1.5`. The runtime's `print_f64` checks `f.fract() == 0.0 && f >= -2^53 && f <= 2^53` and falls through to `print_i64` when matched. NaN and ±Inf get their own branches because Rust's default Display would print `NaN` and `inf` / `-inf` which don't match vm3.
