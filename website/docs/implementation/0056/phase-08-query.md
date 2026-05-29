---
title: "Phase 8. Query DSL (from / where / select / order / skip / take)"
sidebar_position: 12
sidebar_label: "Phase 8. Query DSL"
description: "MEP-56 Phase 8, query expressions desugared upstream to a result-list accumulator and lowered to Ruby for-each with begin/end scope wrapper."
---

# Phase 8. Query DSL (from / where / select / order / skip / take)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 271fe13799 |

## Gate

`TestPhase8Queries` in `transpiler3/ruby/build/phase08_test.go`: five inline subtests, `query_map`, `query_filter`, `query_filter_map`, `query_order_by`, and `query_skip_take`. Each subtest compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. The set covers a pure `select` projection (`from n in nums select n * 2`), a `where` filter, a combined `where`/`select`, an `order by` ascending sort, and a `skip 1 take 3` window. There are no `wantInRb` render-shape assertions; the gate is purely behavioural, because the C front-end already desugars the query expression and locking the desugared IR shape at the Ruby level would duplicate the desugar tests.

## Gate (continued)

The behavioural coverage is exhaustive across the five Mochi query clauses Phase 8 supports: projection (`select expr`), predicate (`where cond`), composition (`where` then `select`), ordering (`order by key`), and windowing (`skip N take M`). `query_filter` exercises `n % 2 == 0` to confirm the predicate lowers without paren ambiguity around the comparison; `query_skip_take` uses `[10, 20, 30, 40, 50]` to pick the inclusive `[20, 30, 40]` window, which would catch any off-by-one in the desugared skip/take.

## Lowering decisions

Phase 8 piggybacks on the C front-end's query desugaring rather than introducing a query-specific lowerer. The C front-end rewrites `let xs = from n in src where cond order by key skip s take t select expr` into a sequence of `aotir.LetStmt{Name: "xs", Init: ListLit{}}` plus an `aotir.QueryScopeStmt` whose body contains a `ForEachStmt` over the sorted/sliced source list, with the per-iteration body assigning `xs = append(xs, expr)` when the predicate holds. That sequence already maps onto plain Ruby because `ForEachStmt` lowers to `src.each do |n| ... end`, `AssignStmt` to `xs = xs + [expr]`, and `AppendExpr` to functional list concatenation (lower.go lines 653 to 664).

`aotir.QueryScopeStmt` lowers via `lowerQueryScopeStmt` (lower.go lines 1281 to 1299) to a `begin ... end` block wrapping the inner statements. In the C target the QueryScope drives arena allocation; in Ruby it is a no-op scope wrapper since the GC handles the temporary result list. Keeping the `begin/end` shape (rather than inlining the body bare) was deliberate: it preserves the lexical nesting of the IR so a future debugger or source-map step can attribute output lines back to the query expression, and it groups the result-list initialisation and the accumulator loop visually.

The `order by`, `skip`, and `take` clauses are also desugared upstream: the front-end emits a sorted source list (via `aotir.ListSortAscExpr`, lower.go lines 836 to 841, which renders as `src.sort`) and slices it (`aotir.ListSliceExpr`, lines 842 to 859, which renders as `(src[start...end] || [])`) before the foreach. The Ruby side does not see `order` or `skip`/`take` as distinct IR nodes; it just sees the pre-sorted, pre-sliced source list driving the foreach. This is why the test set passes the Enumerable behaviour without the Ruby lowerer ever emitting `.sort_by`, `.drop`, or `.first(n)` calls directly: the equivalent sequence (`sort` then range slice) is produced upstream and lowers through the generic list-op paths.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `QueryScopeStmt` arm dispatches to `lowerQueryScopeStmt`, which wraps the body in `begin/end`; `ListSortAscExpr` → `recv.sort`; `ListSliceExpr` → `(recv[start...end] \|\| [])`; `AppendExpr` → `lhs + [val]`; `ForEachStmt`, `AssignStmt`, `LetStmt` reused from earlier phases |
| `transpiler3/ruby/rtree/` | `RawStmt` used as the carrier for the `begin/end` block; `Foreach` and `Assign` nodes from Phase 2.5 carry the loop body |
| `transpiler3/ruby/build/phase08_test.go` | `TestPhase8Queries` with 5 subtests |

## Test set

- `TestPhase8Queries/query_map`, `query_filter`, `query_filter_map`, `query_order_by`, `query_skip_take`.

## Closeout notes

Phase 8 landed on CRuby 3.4 with all five subtests green. The desugar-upstream decision (let the C front-end emit a `ForEachStmt` plus accumulator rather than building a Ruby-specific `select/map/sort_by/drop/first` chain) was driven by Mochi's "one IR, many backends" principle. A direct chain would have rendered more idiomatic Ruby (`nums.select { |n| n.even? }.map { |n| n * 2 }`), but it would have duplicated the predicate-and-projection composition logic that the C front-end already runs to produce the BEAM and JVM targets. The current shape costs one extra `begin/end` wrapper per query but keeps the Ruby backend free of query-specific code. Future Phase 8.1 (joins, group-by) will continue to rely on upstream desugaring, with the Ruby lowerer only learning new operators if the desugarer outputs an op that has no existing Mochi-side equivalent.
