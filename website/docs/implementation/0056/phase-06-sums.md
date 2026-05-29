---
title: "Phase 6. Sum types and pattern matching"
sidebar_position: 10
sidebar_label: "Phase 6. Sum types and pattern matching"
description: "MEP-56 Phase 6, sum type declarations to Ruby Data subclasses inside a wrapper module, match expressions to case/in."
---

# Phase 6. Sum types and pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | bdc3ca1fc7 |

## Gate

`TestPhase6SumTypes` in `transpiler3/ruby/build/phase06_test.go`: one inline subtest, `sum_variant_match`. The subtest compiles a Mochi source with `type Num = Pos(n: int) | Neg(n: int) | Zero` plus an `abs_val` function that exhaustively matches all three variants, executes the `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout (`5\n3\n0\n`) against the recorded expectation. The subtest additionally asserts that the rendered `.rb` contains eight render-shape substrings: `module Num`, `Pos = Data.define(:n)`, `Neg = Data.define(:n)`, `Zero = Data.define()`, `case x`, `in Num::Pos(n:)`, `in Num::Neg(n:)`, and `in Num::Zero`. Both the union namespace shape and the pattern syntax are locked, not just the runtime output.

## Lowering decisions

Each `aotir.UnionDecl` lowers to an `rtree.ModuleDecl` named after the union, containing one `rtree.DataDecl` per variant (lower.go lines 78 to 88). The variant `Data.define(...)` arguments come from the variant's `Fields` slice; a payload-less variant like `Zero` lowers to `Zero = Data.define()`. Wrapping the variants in `module Num` namespaces them so two unions can share a variant name (for example `Result::Ok` and `Option::Ok`) without colliding at the Ruby level. The union modules are appended to the outer wrapper's `progDecls` after records and agents but before the nested `Main` module, so the file shape is `module Wrapper; ...; module Num; Pos = Data.define(:n); Neg = Data.define(:n); Zero = Data.define(); end; module Main; ...; end; end`.

`aotir.VariantLit` lowers to `UnionName::VariantName.new(field: value, ...)` via a `MethodCall` whose receiver is the qualified identifier `Num::Pos` and whose method is `new`, with each field argument rendered as the raw text `name: value` for keyword-style construction (lower.go lines 714 to 728). The `::` qualifier is essential because the variant lives inside the union module; a bare `Pos.new(...)` would not resolve from `Main.run`. `aotir.VariantFieldAccess` lowers to a no-arg `MethodCall{Receiver: recv, Method: e.FieldName}` (lines 731 to 736), rendering as `value.n`, which works because `Data.define(:n)` auto-defines a `n` reader.

`aotir.MatchStmt` lowers via `lowerMatchStmt` (lower.go lines 1100 to 1141) to an `rtree.CaseInStmt`. For each arm, the pattern string is built with `fmt.Sprintf("%s::%s", m.UnionName, a.VariantName)` and, if the arm has bindings, a parenthesised hash-pattern body listing each field as either `field:` (when the binding name matches the field name) or `field: localvar` (when it differs). For `Pos(n) => n`, the rendered pattern is `Num::Pos(n:)`, which uses Ruby 3.0+ deconstructive pattern matching: it binds the matched value's `n` field to a local `n` in the arm body. For the payload-less `Zero` arm, no parentheses are emitted, so the pattern is just `Num::Zero`, matching by class only. Optional default arms map to the `else` clause of `case/in`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `prog.Unions` loop emits `module UnionName` containing one `DataDecl` per variant; `VariantLit` → `UnionName::Variant.new(field: v)`; `VariantFieldAccess` → `recv.field`; `MatchStmt` → `case/in` via `lowerMatchStmt` |
| `transpiler3/ruby/rtree/` | `CaseInStmt` and `CaseInArm` nodes rendering Ruby 3.0 `case x; in Pat; ...; end` |
| `transpiler3/ruby/build/phase06_test.go` | `TestPhase6SumTypes` with 1 subtest and 8 render-shape assertions |

## Test set

- `TestPhase6SumTypes/sum_variant_match`.

## Closeout notes

Phase 6 landed on CRuby 3.4 with the single subtest green. Locking the `in Num::Pos(n:)` pattern shape in `wantInRb` was deliberate, the Ruby parser accepts several equivalent forms (`in Num::Pos(n: n)`, `in Num::Pos => {n:}`, etc.) and a future refactor producing any of those would still run correctly but would silently change the readability and the binding semantics; pinning the canonical `in Pat(field:)` form keeps the rendered Ruby idiomatic. Key implementation insight: Ruby 3.0 introduced `case/in` pattern matching and 3.2 introduced `Data.define`, which is why MEP-56 §1 puts the floor at Ruby 3.2; on 2.7 the lowerer would have had to emit `case ... when` plus manual class checks plus reader calls, doubling the per-arm code.
