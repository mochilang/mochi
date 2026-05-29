---
title: "Phase 9. Datalog (compile-time semi-naive eval)"
sidebar_position: 13
sidebar_label: "Phase 9. Datalog"
description: "MEP-56 Phase 9, Datalog facts and rules evaluated at lower time via semi-naive fixpoint, results emitted as a Ruby Array of frozen string literals."
---

# Phase 9. Datalog (compile-time semi-naive eval)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 2ab17282e6 |

## Gate

`TestPhase9Datalog` in `transpiler3/ruby/build/phase09_test.go`: five inline subtests, `dl_parent_basic`, `dl_ancestor`, `dl_empty_result`, `dl_neq_constraint`, and `dl_not_negation`. Each subtest compiles a Mochi source containing `fact` declarations and (for the rule-bearing subtests) `rule` declarations, executes the `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. `dl_parent_basic` covers a single-step query (`query parent("tom", Y)` returns `bob`), `dl_ancestor` covers a recursive rule reaching transitive closure (`ancestor("tom", Y)` returns `bob`, `ann`, `pat`), `dl_empty_result` covers a no-match query that must surface as `len(xs) == 0`, `dl_neq_constraint` covers the `X != Y` body literal exercising the `IsNeq` path, and `dl_not_negation` covers the `not p(X)` body literal exercising the `IsNot` path. There are no `wantInRb` assertions because the gate is purely behavioural: the rendered Ruby is a literal array of strings, and its shape is fully determined by the compile-time fixpoint.

## Lowering decisions

The full Datalog program (facts + rules + the query head) is captured in `aotir.DatalogQueryExpr.Prog` by the C front-end. The Ruby lowerer evaluates that program at lower time and emits the results as a Ruby Array literal of pre-computed strings; no Datalog engine ships with the Ruby runtime. This mirrors the JVM and BEAM strategies, where the IR carries enough information that runtime evaluation would be redundant work.

`aotir.DatalogQueryExpr` lowers via `lowerDatalogQueryExpr` in `transpiler3/ruby/lower/datalog.go` (lines 13 to 31), which calls `datalogEval` (lines 35 to 92) to compute the result list and then wraps each result string in an `rtree.StringLit` joined into an `rtree.RawExpr` of the form `["bob", "ann", "pat"]`. The empty-result case (`dl_empty_result`) produces the Ruby literal `[]`, which is why `len(xs)` returns 0 cleanly through the `ListLenExpr` path lowered elsewhere.

`datalogEval` implements semi-naive bottom-up evaluation. Initial state seeds each relation from `e.Prog.Facts` (lines 42 to 46). The outer loop iterates `e.Prog.Rules` until no new tuples are derived in a full pass (lines 48 to 62), so the recursive `ancestor` rule in `dl_ancestor` converges in three iterations: round 1 derives `(tom, bob), (bob, ann), (ann, pat)` from the base case; round 2 derives `(tom, ann)` and `(bob, pat)` from the recursive case; round 3 derives `(tom, pat)`; round 4 finds no new tuples and terminates. `deriveRule` (lines 94 to 192) walks the rule body, threading a list of variable-binding environments through each literal: positive literals join (lines 137 to 168), negation (`IsNot`) filters out envs that match (lines 109 to 135), and disequality (`IsNeq`) drops envs where two named vars bind to the same value (lines 97 to 107).

Query evaluation (lines 64 to 91) walks the queried relation and matches each tuple against `e.QueryArgs` slot-by-slot. A non-empty argument string is treated as a ground literal (the surrounding quotes are stripped if present, lines 73 to 76); an empty string is treated as a free variable whose binding is appended to the output list. For `ancestor("tom", Y)`, the matcher accepts every tuple with `tom` in slot 0 and emits the slot-1 binding, producing the three-element output. Constant equality uses string compare throughout because the C front-end has already serialised every datalog argument to a string, removing the need to track Mochi types through the engine.

Helpers `resolveArg` (lines 194 to 202), `isVar` (lines 204 to 212), `tupleInRelation` (lines 214 to 231), and `copyEnv` (lines 233 to 239) handle the standard semi-naive plumbing: quoted strings unwrap to their content, bare identifiers are treated as variables, the deduplication check avoids re-adding existing tuples, and the env copy on each match prevents cross-branch pollution.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/datalog.go` | `lowerDatalogQueryExpr` emitting a Ruby Array of pre-computed string literals; `datalogEval` semi-naive fixpoint; `deriveRule` for positive/negation/disequality literals; `resolveArg`, `isVar`, `tupleInRelation`, `copyEnv` helpers |
| `transpiler3/ruby/lower/lower.go` | `aotir.DatalogQueryExpr` arm in `lowerExpr` dispatches to `lowerDatalogQueryExpr` (lines 737 to 738) |
| `transpiler3/ruby/build/phase09_test.go` | `TestPhase9Datalog` with 5 subtests |

## Test set

- `TestPhase9Datalog/dl_parent_basic`, `dl_ancestor`, `dl_empty_result`, `dl_neq_constraint`, `dl_not_negation`.

## Closeout notes

Phase 9 landed on CRuby 3.4 with all five subtests green. The compile-time evaluation strategy was chosen over emitting a runtime Datalog engine for two reasons: every existing Mochi backend (C, JVM, BEAM) already does compile-time eval, so the engine is dead weight at runtime; and the Mochi fact/rule corpus is closed at compile time, so any runtime engine would just re-derive the same fixpoint on every process start. Key implementation insight: the `aotir.RawCStmt` arm at lower.go line 212 is a deliberate no-op for the C target's Datalog fixpoint scaffolding, since the Ruby lowerer evaluates the program at compile time via `lowerDatalogQueryExpr` and the C-side fixpoint setup is dead weight here. Future Phase 9.1 (aggregations like `count`, `sum`) will extend `deriveRule` rather than introducing runtime code.
