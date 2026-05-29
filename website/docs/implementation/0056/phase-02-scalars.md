---
title: "Phase 2. Scalars and arithmetic"
sidebar_position: 4
sidebar_label: "Phase 2. Scalars and arithmetic"
description: "MEP-56 Phase 2, int and float and bool and string literals, arithmetic, comparison, unary."
---

# Phase 2. Scalars and arithmetic

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | d5559fc885 |

## Gate

Two test functions in `transpiler3/ruby/build/phase02_test.go`. `TestPhase2Scalars` runs the `let` subtest against `examples/v0.1/let.mochi` (expected `42\nMochi\n`). `TestPhase2InlineFixtures` runs six inline fixtures: `arith_int`, `arith_float`, `compare_int`, `compare_string`, `unary`, and `string_concat`. Each subtest writes a Mochi source into a temp dir, runs `Driver.Build`, executes the emitted `.rb` under the resolved Ruby with `-I mochi-runtime/lib`, and asserts stdout matches the recorded `want` byte-for-byte.

## Lowering decisions

Scalar literals lower one-to-one: `aotir.StringLit` → `rtree.StringLit`, `aotir.IntLit` → `rtree.IntLit`, `aotir.FloatLit` → `rtree.FloatLit`, `aotir.BoolLit` → `rtree.BoolLit`. Ruby's print output for these matches Mochi: an int prints as bare digits, a float as `10.0` (the trailing `.0` is preserved by routing `mochi_print_f64` through `Mochi::Runtime::IO.putln` rather than relying on bare `puts`), a bool as `true`/`false`, a string verbatim. `mochi_print_i64` lowers to `puts arg` while `mochi_print_f64` lowers to `Mochi::Runtime::IO.putln(arg)` so all float formatting funnels through a single runtime method (`lower.go` lines 502 to 517).

`lowerBinary` maps the `aotir.BinOp` enum to Ruby operators via `rubyBinOp`: `BinAddI64`/`BinAddF64` → `+`, `BinSubI64`/`BinSubF64` → `-`, `BinMulI64`/`BinMulF64` → `*`, `BinDivI64`/`BinDivF64` → `/`, `BinModI64` → `%`, the six `Eq`/`Ne` flavours → `==`/`!=`, the `Lt`/`Le`/`Gt`/`Ge` flavours → `<`/`<=`/`>`/`>=`, `BinAndBool`/`BinOrBool` → `&&`/`||`, and `BinStrCat` → `+`. The lowerer leaves operator precedence to Ruby; the surrounding `rtree.BinaryOp` renderer wraps the whole expression in parens when emitted as an argument so the original Mochi left-to-right grouping is preserved (the `arith_int` fixture relies on explicit parens in the source).

`lowerUnary` covers `UnNegI64`/`UnNegF64` → `-x` and `UnNotBool` → `!x`. `LetStmt` becomes a Ruby `Assign{LHS: rubyIdent(name), RHS: lowered}`; the `rubyIdent` mangler appends `_` to the 36 reserved Ruby keywords (alias, and, begin, ..., yield) so a Mochi `var while = 1` would render as `while_ = 1`. `var` and `let` collapse to the same Ruby form because Ruby has no const/let distinction; the Mochi var-vs-let invariant is a typecheck-time concern that never reaches the emitter.

A known limitation logged in the source: `BinDivI64` becomes Ruby's plain `/`, which floor-divides for negatives. Phase 2 fixtures use only positive operands, so it is correct here, but the comment at `lower.go:1187` flags this for a later phase to round to truncation via `.div`/`.divmod`. The audit pass added `TestPhase29EdgeCases/negative_int_floor_div_known_divergence` to lock the floor-div emission in place so any later change is caught alongside the spec update. Concrete divergence: spec `-7 / 2` should be `-3` (truncate toward zero, matching C/JVM/Swift/.NET), current Ruby output is `-4`. The four-input test asserts the floor values `-4, -4, 3, -4` so the gap is permanently visible.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerExpr` scalar-literal arms, `lowerBinary`/`rubyBinOp`, `lowerUnary`, `lowerLetStmt`, `lowerAssignStmt`, `rubyIdent` keyword mangler |
| `transpiler3/ruby/build/phase02_test.go` | `TestPhase2Scalars` (1 subtest), `TestPhase2InlineFixtures` (6 subtests) |

## Test set

- `TestPhase2Scalars/let`
- `TestPhase2InlineFixtures/arith_int`, `arith_float`, `compare_int`, `compare_string`, `unary`, `string_concat`

## Closeout notes

Phase 2 landed on CRuby 3.4 with all seven subtests green. Float printing via `Mochi::Runtime::IO.putln` rather than bare `puts` gives the lowerer a single chokepoint for later phases that need finer control (NaN, Inf, scientific notation). Key implementation insight: Mochi's no-precedence grammar parses `a + b * c` as `(a + b) * c`, so the inline `arith_int` fixture uses explicit parens to be exact. The `compare_string` arm validates that `BinEqStr` lowers to `==`, which Ruby strings implement as byte-wise equality, matching vm3.
