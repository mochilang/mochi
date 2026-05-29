---
title: "Phase 2.5. Control flow"
sidebar_position: 5
sidebar_label: "Phase 2.5. Control flow"
description: "MEP-56 Phase 2.5, if and elsif and else, while, for-range."
---

# Phase 2.5. Control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 4738082cde |

## Gate

`TestPhase25ControlFlow` in `transpiler3/ruby/build/phase025_test.go`: five inline subtests, `if_else`, `if_elsif_else`, `while_loop`, `for_range`, and `for_range_sum`. Each compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation.

## Lowering decisions

`aotir.IfStmt` lowers via `lowerIfStmt` into an `rtree.IfStmt{Cond, Then, Elsifs, Else}`. The aotir IR for chained `if A {} else if B {} else {}` is a nested `IfStmt` inside the outer `Else` block. The lowerer detects this pattern: it walks `cur := i.Else` while `cur` is a Block containing a single statement that is itself an `IfStmt`, and folds each level into an `Elsifs` arm. Only when the walk exits (because `cur` is multi-statement, the inner statement is not an IfStmt, or `cur` is nil) does any remaining `cur` become the final `Else` body. This produces Ruby `if/elsif/elsif/else/end` instead of deeply nested `if; else; if; else; end`, which is both more readable and what Rubyists expect (`lower.go` lines 348 to 386).

`aotir.WhileStmt` lowers to a `RawStmt` rendering `while Cond` plus the indented body plus `end`. `aotir.ForRangeStmt` lowers similarly: `(start...end).each do |var| ... end` using Ruby's triple-dot half-open range, which matches Mochi's `[start, end)` semantics. `aotir.ForEachStmt` (covered in Phase 3 for lists) renders as `xs.each do |x| ... end`. `aotir.BreakStmt` → `break`, `aotir.ContinueStmt` → `next`, `aotir.ReturnStmt` → `return` (with or without a value), all emitted as `RawStmt`/`Return` nodes.

The control-flow lowerings deliberately render via `RawStmt` rather than dedicated `rtree.WhileStmt`/`rtree.ForStmt` nodes. Reason: Ruby's `while` and `each do |i|` blocks have no structural quirks worth modelling (no labelled breaks, no comma-separated init clauses); a one-shot `strings.Builder` keeps the lowerer compact. If later phases need peephole passes that match on loops, the `RawStmt` form can be promoted then.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerIfStmt` with elsif-fold loop, `lowerWhileStmt`, `lowerForRangeStmt`, `lowerForEachStmt`, `BreakStmt`/`ContinueStmt`/`ReturnStmt` arms in `lowerStmt` |
| `transpiler3/ruby/build/phase025_test.go` | `TestPhase25ControlFlow` with 5 subtests |

## Test set

- `TestPhase25ControlFlow/if_else`, `if_elsif_else`, `while_loop`, `for_range`, `for_range_sum`.

## Closeout notes

Phase 2.5 landed on CRuby 3.4 with all five subtests green. The `for_range_sum` fixture (`1..11`) confirms the triple-dot half-open semantics, summing 1 through 10 inclusive to 55. Key implementation insight: the elsif-fold loop iterates until it hits something that is not a single nested IfStmt, so a Mochi `else` clause that contains multiple statements (or anything other than a sole `if`) terminates the fold and renders as a regular `else`. This matches the C lowerer's IR shape and avoids the "ladder of doom" Ruby that a naive emitter would produce.
