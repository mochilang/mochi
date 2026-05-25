---
title: "Phase 8. Query DSL"
sidebar_position: 10
sidebar_label: "Phase 8. Query DSL"
description: "MEP-45 Phase 8 tracking: query algebra lowering with operator fusion, joins (inner/left/cross), group-by, order-by, distinct, set ops, arena allocation, load/save adapters."
---

# Phase 8. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 8](/docs/mep/mep-0045#phase-8-query-dsl) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 17:16 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Query fixture suite (~60 cases: filter, map, group-by, order-by, distinct, union, intersect, except, inner/left/cross join) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Query DSL (`from x in xs where cond select expr`) is the highest-value language feature for dataset and AI workflows. Without it programs that process collections must use explicit for-loops + append; the query surface is significantly more readable and matches the MEP-45 target examples. Phase 8.0 lands the core filter+map path and unblocks the majority of realistic single-collection query programs. Aligns directly with user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 8.0 | Query algebra lowering: `from x in src [where cond] select expr` desugars to a for-loop + append inside the lower pass. `lowerQueryExpr` mirrors `lowerMatchExpr` (emits into `l.currentBlock`, returns a `VarRef` to a fresh temp list). No new IR node needed. 8 fixtures under `tests/transpiler3/c/fixtures/query/`. `TestPhase8QueryDSL` gate green. | LANDED 2026-05-25 17:16 (GMT+7) | — | — |
| 8.1 | `order by` (sort_asc), `skip N` (list_slice start), `take N` (list_slice end): `ListSortAscExpr` + `ListSliceExpr` IR nodes; emit as `mochi_list_<T>_sort_asc` and `mochi_list_<T>_slice`; 8 fixtures; `TestPhase8QueryDSL` gate extended. | LANDED 2026-05-25 17:40 (GMT+7) | — | — |
| 8.2 | Joins: inner join (`join y in ys on cond`), left join (`left join y in ys on cond`), cross join (`from y in ys`): all three desugar to nested `ForEachStmt` nodes in `lowerQueryExpr`; no new IR nodes needed; 8 fixtures; `TestPhase8QueryJoins` gate green. | LANDED 2026-05-25 19:41 (GMT+7) | — | — |
| 8.3 | Arena allocation: intermediates live in `mochi_arena` released at query boundary; surviving result copied to GC | NOT STARTED | — | — |
| 8.4 | `load`/`save` adapters: JSON (yyjson), YAML (libfyaml), CSV (home-grown) | NOT STARTED | — | — |

## Decisions made

**No new IR node for basic queries.** Phase 8.0 desugars `from x in src where cond select expr` directly in the lower pass into existing IR nodes: a `LetStmt` for the empty result list (mutable), a `ForEachStmt` over the source, and an `AssignStmt + AppendExpr` to accumulate results. An optional `IfStmt` wraps the append when a `where` clause is present. This reuses all existing IR infrastructure (verifier, emitter) without adding a `ListCompExpr` node.

**Phase 8.1: `ListSortAscExpr` and `ListSliceExpr` are new IR nodes.** Both carry the same ElemType/ElemRecordName/InnerElemType/MapElemKeyType/MapElemValueType metadata as `AppendExpr` so the verifier's `exprElemType` family and the emitter's walkExpr family work without special-casing. The emitter maps them to `mochi_list_<T>_sort_asc(xs)` and `mochi_list_<T>_slice(xs, start, end)` respectively; the runtime helpers were added in Phase 8.1 to `list.c`/`list.h`.

**Phase 8.1 desugaring of order/skip/take.** After the `ForEachStmt` is emitted into `l.currentBlock`:
- If `q.Sort != nil`: emit `__queryN = mochi_list_<T>_sort_asc(__queryN)` (an `AssignStmt` with `ListSortAscExpr`).
- If `q.Skip != nil` or `q.Take != nil`: emit `__queryN = mochi_list_<T>_slice(__queryN, start, end)`. `start` defaults to 0 when `skip` is absent; `end` defaults to a sentinel (1<<62-1) when `take` is absent; when both are present, `end = skip + take` using a `BinaryExpr{BinAddI64}`.

**Phase 8.1 restricts order-by key to scalar element types.** The sort key is the loop element itself (identity key). Non-identity sort keys (e.g. `order by n.field`) require a Schwartzian transform and are deferred to a later sub-phase.

**`lowerQueryExpr` follows the `lowerMatchExpr` pattern.** Like match-as-expression, query-as-expression works by emitting statements into `l.currentBlock` (the block currently being built) and returning a `VarRef` to a fresh temp variable. The temp counter is shared with match temp names (both use `l.tempCounter`; query temps are named `__queryN`).

**Phase 8.2: nested-loop joins, not hash-joins.** The MEP spec mentions hash-join via Swiss table as the production target, but for correctness and simplicity the Phase 8.2 lower pass uses nested-loop joins for all three forms. The outer loop iterates the left-side source; inner loops iterate each join/from source. The `on` condition (inner join) or no condition (cross join) filters tuples. This is semantically equivalent and produces byte-equal output on the fixture corpus. Hash-join is a Phase 8.3+ performance concern.

**Phase 8.2: join desugaring produces nested ForEachStmt nodes.** No new IR nodes were needed. The body is built inside out: the innermost body is the `append` statement (wrapped in an `IfStmt` when a `where` clause is present), then each join clause wraps it in a `ForEachStmt` + `IfStmt{on}` (for inner join) or a `ForEachStmt` + `__anyN` flag trick (for left join), then each from clause wraps it in a plain `ForEachStmt`. The outermost `ForEachStmt` for the primary `from` clause is emitted last into `l.currentBlock`.

**Phase 8.2: left join uses a boolean `__anyN` flag per outer row.** For `left join y in ys on cond select x_expr` (where `x_expr` does not reference `y`), the desugared code emits `let __anyN = false` before the inner loop, sets `__anyN = true` on each match, and after the inner loop emits `if !__anyN { append x_expr }`. This correctly produces all outer rows even when no inner row matches. Fixtures restrict the select expression to left-side variables only; accessing the right-side variable in a left join select requires Option<T> support, deferred to a later phase.

**Phase 8.2 sources are lowered in outer scope.** All join/from source expressions (right-side lists) are lowered in the outer scope before any loop variable is pushed. This prevents accidental capture of sibling loop variables in source expressions and matches the type-checker's scoping rules.

**Query scope management.** The loop variable `x` from `from x in src` is pushed into an inner scope for the duration of lowering the `where` and `select` expressions. The outer scope then receives the temp result list binding. This mirrors the ForEachStmt scope handling in `lowerForEach`.

**Phase 8.0 restricts to single-source, scalar-element queries.** Multiple `from` clauses (cross-join), `join`, `group by`, `order by`, `distinct`, `skip`, `take` all return a clear "lands in Phase 8.N" error. The select expression can produce int, float, bool, or string elements; record or list elements are Phase 8.1+.

## Bug fixes in this phase

- Queries in print-expression position: `print(from n in nums select n)` fails because the lower pass rejects printing list values. Fixture design avoids this by iterating with `for x in result { print(x) }`.

## Deferred work

- `group by`: Phase 8.1+ (requires aggregation).
- `distinct`: Phase 8.1+ (requires set dedup).
- `union`, `intersect`, `except` set operators: Phase 8.1+.
- Non-identity sort keys (`order by n.field`): Phase 8.1+ (Schwartzian transform).
- Arena allocation for intermediates: Phase 8.3.
- `load`/`save` adapters (JSON, YAML, CSV): Phase 8.4.
- Cost-based join reordering: v2.
- Select expressions producing list or record values: Phase 8.1+.
- Queries in print-expression position (requires print-list support from Phase 3.1+).
- Left/right/outer join where select references the nullable side: requires Option types (deferred to when Option<T> lands in the AOT transpiler).
- Hash-join optimization (Swiss table): nested-loop is correct for the fixture corpus; hash-join is a Phase 8.3+ performance concern.

## Closeout notes

_Fill in after gate fully green (all 5 sub-phases)._
