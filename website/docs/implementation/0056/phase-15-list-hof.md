---
title: "Phase 15. List aggregates and HOF (min, max, sum, in, map, filter, reduce)"
sidebar_position: 19
sidebar_label: "Phase 15. List aggregates and HOF"
description: "MEP-56 Phase 15, list aggregates and higher-order operations lowered to Ruby Enumerable."
---

# Phase 15. List aggregates and HOF (min, max, sum, in, map, filter, reduce)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 0732df08e0 |

## Gate

`TestPhase15ListAggHOF` in `transpiler3/ruby/build/phase15_test.go`: five subtests (`list_min_max_sum`, `list_in`, `list_map`, `list_filter`, `list_reduce`). Each subtest compiles a Mochi program that hits one Enumerable family (e.g. `reduce([1,2,3,4], plus, 0)` must print `10`), then runs the emitted `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib` and diffs stdout. The HOF cases (`list_map`, `list_filter`, `list_reduce`) pass a named Mochi function (`dbl`, `is_even`, `plus`) as the callback, so the gate also covers Phase 7's closure-as-lambda machinery flowing through `.call`.

## Lowering decisions

List aggregates lower to Ruby `Enumerable` method calls; HOF builtins lower to a `RawExpr` carrying a Ruby block that invokes the callback via `.call` (`transpiler3/ruby/lower/lower.go` lines 874 to 938):

- `aotir.ListMinExpr` to `MethodCall{Method: "min"}` (lines 874 to 879). Ruby `Enumerable#min` returns `nil` on an empty array; the gate exercises a non-empty list, matching Mochi's documented "undefined on empty" for `min`.
- `aotir.ListMaxExpr` to `MethodCall{Method: "max"}` (lines 880 to 885).
- `aotir.ListContainsExpr` (the desugared form of `x in xs`) to `MethodCall{Method: "include?", UseParens: true}` (lines 886 to 895). Ruby `Array#include?` does linear `==` scan, matching `in` semantics.
- `aotir.ListSumExpr` to `MethodCall{Method: "sum"}` (lines 896 to 901). Ruby `Enumerable#sum` is C-level since 2.4 and works on `Integer` and `Float` lists; the gate's `[3,1,4,1,5,9,2,6]` yields `31`.
- `aotir.ListMapExpr` to `RawExpr` rendering `list.map { |__x| (fn).call(__x) }` (lines 902 to 912). The wrapped `(fn).call(__x)` form is essential: Mochi `fn` is a Ruby `Proc` (lambda) built by Phase 7, so a bare `fn(__x)` would parse as a method invocation, not a Proc call.
- `aotir.ListFilterExpr` to `RawExpr` rendering `list.select { |__x| (fn).call(__x) }` (lines 913 to 923). `select` is preferred over `filter` because `select` is the historical Ruby spelling and is present on every supported runtime; `Array#filter` is only an alias added in 2.6.
- `aotir.ListFoldlExpr` (Mochi's `reduce(xs, fn, init)`) to `RawExpr` rendering `list.inject(init) { |__acc, __x| (fn).call(__acc, __x) }` (lines 924 to 938). `inject` (not `reduce`) is the historical spelling and is identical in Ruby. The seed is passed positionally so it stays distinct from the per-element accumulator.

The `__x` and `__acc` block parameters use the `__`-prefix convention reserved by the lowerer to avoid colliding with any Mochi-level identifier (Phase 0 §lowering convention).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | Six `aotir.List*Expr` cases lower to Enumerable methods or Ruby blocks calling the Mochi Proc via `.call`, lines 874 to 938 |
| `transpiler3/ruby/build/phase15_test.go` | `TestPhase15ListAggHOF` with 5 subtests |

## Test set

- `TestPhase15ListAggHOF/list_min_max_sum`, `list_in`, `list_map`, `list_filter`, `list_reduce`.

## Closeout notes

Phase 15 landed on CRuby 4.0 (Homebrew). The decision to render `(fn).call(__x)` instead of `fn.call(__x)` is defensive: when `fn` is a `RawExpr` carrying e.g. `lambda { ... }` syntax, the outer parens stop Ruby from parsing the `.call` as a method on the literal closing brace. Picking `inject` over `reduce` and `select` over `filter` deliberately maximises compatibility back to older Rubies (mruby 4 is targeted by Phase 27) without giving anything up on modern CRuby.
