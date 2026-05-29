---
title: "Phase 3. Lists"
sidebar_position: 6
sidebar_label: "Phase 3. Lists"
description: "MEP-56 Phase 3, list literal and index and len and for-each and set and append."
---

# Phase 3. Lists

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

`TestPhase3Lists` in `transpiler3/ruby/build/phase03_test.go`: six inline subtests, `list_lit_index`, `list_len`, `for_each`, `list_set`, `list_append`, and `str_len`. Each subtest compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation.

## Lowering decisions

Mochi lists lower directly to Ruby `Array`. `aotir.ListLit` is rendered by `lowerListLit` as a comma-joined bracket literal `[elem1, elem2, ...]`, packaged as a `rtree.RawExpr` because the result is one whole expression token (lower.go lines 1088 to 1098). `aotir.IndexExpr` becomes `recv[idx]` via `lowerIndexExpr`, also as a `RawExpr`, since Ruby's `[]` is already a method call and `rtree.MethodCall` with a bracket-form method would over-complicate the renderer.

`aotir.LenExpr` lowers to `recv.length`, picked over `.size` to give a uniform name with `aotir.StrLenExpr` (which also lowers to `.length`); both return `Integer` and behave identically on `Array` and `String`. `aotir.ForEachStmt` produces `xs.each do |x| ... end` via the same `RawStmt` style as the for-range loop, with `rubyIdent` mangling on the loop variable.

`aotir.ListSetStmt` is a statement: `xs[1] = 99` becomes `RawStmt{Text: "xs[1] = 99"}`. The C lowerer surfaces this as a separate statement type rather than an `AssignStmt` with an indexed LHS, which the Ruby lowerer accepts as-is. `aotir.AppendExpr` is the canonical Mochi `append(xs, v)` and lowers to `xs + [v]`, a functional append that returns a new array without mutating `xs`. The comment at lower.go:663 notes this is intentional: Mochi's `append` semantics are pure, matching the vm3 oracle.

The `str_len` subtest belongs to Phase 3 because it shares the `len(...)` surface with lists; `lowerExpr`'s `StrLenExpr` arm lowers to the same `.length` call.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerListLit`, `lowerIndexExpr`, `LenExpr`/`StrLenExpr` arms, `lowerForEachStmt`, `ListSetStmt` arm, `AppendExpr` arm |
| `transpiler3/ruby/build/phase03_test.go` | `TestPhase3Lists` with 6 subtests |

## Test set

- `TestPhase3Lists/list_lit_index`, `list_len`, `for_each`, `list_set`, `list_append`, `str_len`.

## Closeout notes

Phase 3 landed on CRuby 3.4 with all six subtests green. The functional append (`xs + [v]`) bumped past one early attempt that used `<<`, which mutates in place and would have broken any fixture that holds a reference to the pre-append list. Key implementation insight: Mochi's `var xs = ...; xs = append(xs, v)` pattern lowers to the same Ruby `xs = xs + [v]`, so the user-facing var-rebinding is preserved without a separate ABI for in-place vs functional append. `len(s)` on strings reuses `.length`, giving a single name for both arrays and strings (matches Ruby community style).
