---
title: "Phase 7. Query DSL"
sidebar_position: 8
sidebar_label: "Phase 7. Query DSL"
description: "MEP-52 Phase 7, Mochi query DSL desugared to for-of loops; hash-join over Map<K, T[]>; left-join sentinel pattern; order by + skip + take via existing list helpers; 25 fixtures green on Node + Deno + Bun."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 7](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 23:00 (GMT+7) |
| Tracking issue | (filled at PR creation) |
| Tracking PR    | (filled at PR creation) |

## Gate

`TestPhase7QueryNode` / `TestPhase7QueryDeno` / `TestPhase7QueryBun`: 25 fixtures green on Node 22, Deno 2, Bun 1.1, byte-equal stdout against vm3. Floor 20, shipped 25.

Secondary gates:

- `TestPhase7EmitShape`: token-level assertions on the emit (for-of loop form, hash-join `Map<K, T[]>` type, left-join sentinel `__any`, `mochi_list_sort_asc` / `mochi_list_slice` calls).
- `TestPhase7QueryScopeIsTransparent`: asserts no `mochi_arena_`, `__qa`, or `_append_arena` tokens leak into the TS emit. The aotir `QueryScopeStmt` is an arena wrapper that the C transpiler needs for stack-bump allocation; in TS the JS GC handles lifetimes and the wrapper collapses to its body.

Browser (Chromium 130) deferred to Phase 17 alongside JSR + Jupyter.

## Goal-alignment audit

Mochi's query DSL is the front-end for almost every data-shaped program: report generation, ETL, leaderboards, analytics. The MEP-52 spec sketched two routes: (a) ES2024 Iterator helpers (`Iterator.from(xs).filter(p).map(f).toArray()`), or (b) `Array.prototype.filter/.map/.flatMap` chains. Phase 7 ships a third option that beats both: **let the aotir query-desugar pass do all the work**, and lower its output (for-of + append + ListSort + ListSlice) using the existing Phase 2 / 3 surface.

Why this is the right call:

1. The aotir pass already runs for the C transpiler and produces a single canonical lowering shared across every target. There is no second source of truth.
2. The desugared shape (mutable accumulator + for-of) composes with Phase 11 async coloring (substituting `for await`), Phase 10 streams (substituting `AsyncIterable`), and Phase 9 agents (the inner loop body is a closure-body, which Phase 6 already handles). Iterator helpers, by contrast, force a separate `.from` / `.fromAsync` dispatch at every async boundary.
3. Phase 16 reproducibility (byte-equal emit across builds) is easier to gate when the emit tokens come from a small fixed set of Phase 2 / 3 lowerers, not from a variable-output strategy picker.

The cost of this approach is that the Phase 7.0 emit looks longer than the Iterator-helper form. That cost is paid in source-file size, not in execution: V8 / SpiderMonkey / JavaScriptCore all open-code for-of over an array.

## As-shipped lowering

The aotir Phase 8.3 query desugar pass turns

```
let evens = from n in nums where n % 2 == 0 select n
```

into

```
LetStmt __query1 (Mutable=true, ElemType=int, Init=ListLit{ElemType: int})
QueryScopeStmt {
  Body: {
    ForEachStmt n in nums {
      IfStmt n % 2 == 0 {
        AssignStmt __query1 = AppendExpr(__query1, n)
      }
    }
  }
}
LetStmt evens = VarRef __query1
```

The TS lowerer:

- `QueryScopeStmt` is a transparent wrapper. `lowerQueryScopeStmt` returns the lowered statements of `s.Body` flat, dropping the arena annotation entirely.
- Every interior node (LetStmt, ForEachStmt, IfStmt, AssignStmt, AppendExpr) routes through its existing Phase 2 / 3 lowerer.

Result:

```typescript
const nums: number[] = [1, 2, 3, 4, 5, 6];
let __query1: number[] = [];
for (const n of nums) {
    if (((n % 2) === 0)) {
        __query1 = [...__query1, n];
    }
}
const evens: number[] = __query1;
```

### Multi-source comprehensions (`from x in xs from y in ys`)

Aotir wraps the inner body in nested ForEachStmt nodes, innermost-last. The TS emit becomes nested for-of loops with the same `__queryN` accumulator. No flatMap, no iterator chain.

### Hash-join (inner equality join)

When the aotir `extractHashJoinKeys` pass identifies an equality join where the inner key depends only on the inner row and the outer key on outer rows, it emits an O(n+m) hash-join over `Map<K, T[]>`:

```
let __hidx_2: Map<number, number[]> = new Map<number, number[]>();
for (const y of ys) {
    let __hk_2: number = y;
    let __hv_2: number[] = [];
    if (__hidx_2.has(__hk_2)) {
        __hv_2 = mochi_map_get(__hidx_2, __hk_2);
    }
    __hidx_2.set(__hk_2, [...__hv_2, y]);
}
for (const x of xs) {
    if (__hidx_2.has(x)) {
        const __hlist_2: number[] = mochi_map_get(__hidx_2, x);
        for (const y of __hlist_2) {
            __query1 = [...__query1, x];
        }
    }
}
```

This required widening `tsTypeForMapSlot` to render `Map<K, T[]>` (Phase 3.2 supported only scalar map values). The widening threads the new `ListValueElemType` field from aotir LetStmt / MapLit through the type renderer; no new map runtime helper is needed (`mochi_map_get` is generic over V, and `.has` / `.set` work uniformly).

When the inner key cannot be statically separated from the outer scope (cross-source predicate), the aotir pass falls back to a nested-loop join: `for x { for y { if (cond) { ... } } }`. Same TS emit, no hash index.

### Left join

The aotir desugar expands `left join y in ys on cond` into a sentinel-flag pattern:

```
let __any2: boolean = false;
for (const y of ys) {
    if ((x === y)) {
        __any2 = true;
        __query1 = [...__query1, x];
    }
}
if (!(__any2)) {
    __query1 = [...__query1, x];
}
```

`__any2` is a mutable boolean LetStmt; `!__any2` is a `UnaryExpr UnNotBool`. Both route through Phase 2 untouched.

### Order by + skip + take

The aotir pass post-processes the result list:

| Mochi clause      | aotir node            | TS emit                              |
|-------------------|-----------------------|--------------------------------------|
| `order by x`      | `ListSortAscExpr`     | `__query1 = mochi_list_sort_asc(__query1);` |
| `skip N`          | `ListSliceExpr`       | `__query1 = mochi_list_slice(__query1, N, ...);` |
| `take M`          | `ListSliceExpr`       | `__query1 = mochi_list_slice(__query1, 0, M);` |
| `skip N take M`   | `ListSliceExpr`       | `__query1 = mochi_list_slice(__query1, N, N+M);` |

All three helpers were already shipped in Phase 3.1. Aotir's `order by` only supports the case where the sort key is the same as the select expression (sort by `x`, not by `f(x)`); the Phase 7 fixture corpus stays within that limitation. Sort-by-arbitrary-key lands when aotir gains a separate sort key slot on QueryScopeStmt.

## Sub-phases (as shipped)

| # | Scope | Status |
|---|-------|--------|
| 7.0 | Simple comprehensions: `from x in xs where p select f(x)` to for-of + push | LANDED |
| 7.1 | Multi-source comprehensions: `from x in xs from y in ys` to nested for-of | LANDED |
| 7.2a | Hash-join (inner equality join via `Map<K, T[]>`) | LANDED |
| 7.2b | Nested-loop join (fallback when hash-key analysis fails) | LANDED (inherits multi-from path) |
| 7.2c | Left join (sentinel-flag pattern) | LANDED |
| 7.2d | Merge join (when both sides are sorted by the join key) | DEFERRED (no aotir provenance tag yet) |
| 7.3 | Group-by | DEFERRED (needs Phase 3.4 list-of-records + record method dispatch for `g.key` / `count(g)`; revisit with Phase 3.4 widening) |
| 7.4 | Top-K | DEFERRED (aotir does not yet desugar `order by ... take K` into a heap; full sort + slice is the current path) |
| 7.5 | Async sources | DEFERRED (Phase 10 streams territory; revisit when `AsyncIterable<T>` ships) |

## Sub-phase 7.0, Simple comprehensions

### Decisions made (7.0)

**For-of loop over `Iterator.from(...).filter(...).map(...).toArray()`**: rejected the iterator-helper form. The for-of + push form is what aotir hands the TS lowerer, and matches the C transpiler emit shape (just translated word-for-word from C `for (i = 0; i < n; i++)` to TS `for (const x of xs)`). Reproducibility, async-coloring, and stream substitution all favour the for-of form.

**Mutable accumulator over functional fold**: `[...xs, v]` allocates per push. For Phase 7 fixtures (corpus sizes 0-8 elements) the cost is negligible. The execution-budget gate (`TestPhase7QueryPerf` on a 1M-row representative join) is deferred until Phase 7.4; if it comes due, the `AssignStmt + AppendExpr` peephole can be lowered in-place to `.push()` here without changing the IR contract.

**Result var as `let __queryN`, not `const`**: the inner loop body mutates the accumulator via AssignStmt. Mochi's `let` is the same surface as TS `const`; the `__queryN` synthetic carries `Mutable: true` on its aotir LetStmt, which the Phase 2 lowerer renders as `let`.

## Sub-phase 7.1, Multi-source comprehensions

### Decisions made (7.1)

**Nested for-of**: the aotir pass wraps `from y in ys` inside `from x in xs` as nested ForEachStmt nodes. The TS emit is two for-of loops, no intermediate tuples. The `[x, y] as const` tuple form sketched in the original spec was rejected because aotir already preserves the binding scope without it.

**Three or more sources**: same pattern, nested N-deep. Phase 7 fixture corpus covers cross-product of three sources.

## Sub-phase 7.2, Joins

### Decisions made (7.2)

**Hash-join over `Map<K, T[]>`**: required widening `tsTypeForMapSlot(key, value, listValueElem)` so it can render `Map<K, T[]>`. The `listValueElem` parameter threads aotir's `ListValueElemType` field through LetStmt and MapLit. No new runtime helper; `mochi_map_get` is generic over V, and `.has` / `.set` work uniformly.

**Left-join sentinel-flag**: `let __anyN = false` plus `if (!__anyN)` fallback. Mochi's left-join semantics emit one row per outer row regardless of inner matches; the sentinel preserves that count.

**Merge join deferred**: aotir does not currently carry a `Sorted` provenance tag on query sources. When it does, the TS lowerer will add a two-pointer walk.

## Sub-phase 7.3, Group-by (deferred)

The aotir `lowerGroupByQueryExpr` desugars `group by k into g select f(g.key, g)` into a `Map<K, list<T>> + keys-iteration + select` pipeline. The select expression typically reads `g.key` (a record field on the group object) and calls `count(g)` / `sum(g.field)` aggregations. Both surfaces need Phase 3.4 (list-of-records) + record method dispatch to type-check in TS. Deferred to a Phase 7.3 sub-PR once aotir's group result type is reified as a record.

## Sub-phase 7.4, Top-K (deferred)

Aotir does not yet recognise `order by f(x) limit K` as a top-K query (it lowers to a full sort + slice). Phase 7 inherits that: `take N` after `order by` is a full sort + slice, not a min-heap. When aotir grows a top-K provenance, the TS lowerer adds a `MinHeap<T>` runtime helper and a corresponding lowering path.

## Sub-phase 7.5, Async sources (deferred)

`AsyncIterable<T>` lowering is Phase 10 (streams) territory. The query desugar already produces ForEachStmt nodes; substituting `for await` is a one-token change once the source type carries an async colour. Deferred to Phase 10.

## Files (as shipped)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase07.go` | `lowerQueryScopeStmt`: transparent wrapper that flattens the arena body |
| `transpiler3/typescript/lower/lower.go` | QueryScopeStmt dispatch in `lowerStmt`; LetStmt threads `ListValueElemType` |
| `transpiler3/typescript/lower/phase03.go` | `tsTypeForMapSlot` widened to render `Map<K, T[]>`; `tsTypeForLetSlotV2` adds listValueElem parameter; `lowerMapLit` threads `ListValueElemType` |
| `transpiler3/typescript/build/phase07_test.go` | `TestPhase7Query{Node,Deno,Bun}`, `TestPhase7EmitShape`, `TestPhase7QueryScopeIsTransparent` |
| `tests/transpiler3/typescript/fixtures/phase07-query/` | 25 fixtures |

## Fixture corpus (25)

Where + select (5): `req_filter_int`, `req_filter_float`, `req_filter_bool`, `req_filter_string`, `req_filter_no_results`.

Map/projection (4): `req_map_int`, `req_map_float`, `req_map_string`, `req_filter_map`.

Order by (3): `req_order_asc_int`, `req_order_asc_string`, `req_order_asc_float`.

Skip / take (3): `req_skip_only`, `req_take_only`, `req_skip_take`.

Multi-from (3): `req_cross_product`, `req_cross_filter`, `req_cross_three`.

Joins (4): `req_inner_join_int`, `req_inner_join_strings`, `req_inner_join_where`, `req_left_join`.

Composition (3): `req_query_in_function`, `req_query_zero_results`, `req_query_chained`.

## Deferred work

- 7.2d Merge join (waiting on aotir `Sorted` provenance tag on query sources).
- 7.3 Group-by (waiting on Phase 3.4 list-of-records + record method dispatch).
- 7.4 Top-K with min-heap (waiting on aotir top-K provenance).
- 7.5 Async sources (Phase 10 streams territory).
- Sort by arbitrary key expression (waiting on aotir to carry a separate sort key slot on QueryScopeStmt).
- Order-by descending (currently `mochi_list_sort_asc` only; aotir does not emit a sort-desc node).
- Browser (Chromium 130) target: Phase 17 alongside JSR + Jupyter.
