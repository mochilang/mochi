---
title: "Phase 7. Query DSL"
sidebar_position: 8
sidebar_label: "Phase 7. Query DSL"
description: "MEP-52 Phase 7, Mochi query DSL lowered to ES2024 Iterator helpers (Iterator.from, .map, .filter, .take) plus AsyncIterable for async sources; hash/merge/nested-loop joins; group-by; top-K via min-heap; 40 fixtures."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 7](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase7Query`: 40 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: tsc strict zero diagnostics; runtime-budget gate (the runtime additions for query and join stay below 1 KB gzipped); execution-budget gate (a representative joins fixture must complete within 2x the vm3 wall-clock baseline on the linux-x64 CI runner).

## Goal-alignment audit

Mochi's query DSL is the front-end for almost every data-shaped program: report generation, ETL, leaderboards, analytics. The TS surface gives us two routes: (a) `Array.prototype.map/filter/...` (eager, allocates intermediate arrays), or (b) TC39 Iterator helpers (`Iterator.from(...).map(...).filter(...)`, lazy, single-pass). The MEP-52 decision is to prefer iterator helpers, falling back to `Array.prototype` only when the IR proves the source is finite and the user wants the intermediate `T[]` reified. Async sources lower to `AsyncIterable<T>` and the `for await` form. Joins, group-by, and top-K are implemented in `@mochi/runtime/query` because the platform does not ship them.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | Simple comprehensions: `[f(x) for x in xs if p(x)]` to `Iterator.from(xs).filter(p).map(f).toArray()` | NOT STARTED | n/a |
| 7.1 | Multi-source comprehensions: `[f(x, y) for x in xs for y in ys]` to flatMap chain | NOT STARTED | n/a |
| 7.2 | Joins: hash join (default), merge join (when both sides are sorted), nested-loop (when one side is small); IR-driven choice | NOT STARTED | n/a |
| 7.3 | Group-by: `Map.groupBy(xs, keyFn)` (ES2024) plus aggregations | NOT STARTED | n/a |
| 7.4 | Top-K: min-heap implementation in `@mochi/runtime/query/heap` | NOT STARTED | n/a |
| 7.5 | Async sources: `AsyncIterable<T>` plumbing through query pipelines via `Iterator.fromAsync` and async generators | NOT STARTED | n/a |

## Sub-phase 7.0, Simple comprehensions

### Decisions made (7.0)

**Mochi**: `[u.name for u in users if u.age > 18]`

**TypeScript**:

```typescript
Iterator.from(users).filter((u) => u.age > 18n).map((u) => u.name).toArray()
```

**Why iterator helpers (ES2024)**: lazy single-pass; no intermediate array for `filter`'s output. `Iterator.from(array)` is constant-time (array is already iterable). The terminal `.toArray()` materialises only when the consumer expects a `T[]`. Node 22 / Deno 2 / Bun 1.1 / Chromium 122+ all ship iterator helpers natively.

**Fallback to `Array.prototype`**: when the IR signals `xs` is an array, the producer is finite, and the consumer is a single `for`/`for-of` loop, the emitter prefers `xs.filter(...).map(...)` (slightly less indirection, similar perf in V8). The IR-pass `query/lowerStrategy.go` picks the form.

## Sub-phase 7.1, Multi-source comprehensions

### Decisions made (7.1)

**Mochi**: `[f(x, y) for x in xs for y in ys if p(x, y)]`

**TypeScript**:

```typescript
Iterator.from(xs)
  .flatMap((x) => Iterator.from(ys).map((y) => [x, y] as const))
  .filter(([x, y]) => p(x, y))
  .map(([x, y]) => f(x, y))
  .toArray()
```

The intermediate `[x, y]` tuple is unavoidable for non-fused chains; TypeScript and V8 do not eliminate it via escape analysis. The emitter falls back to a hand-coded nested loop when the IR proves the tuple escapes (e.g., the comprehension returns objects whose lifetime exceeds the loop).

## Sub-phase 7.2, Joins

### Decisions made (7.2)

**Hash join (default for equi-join)**:

```typescript
// SELECT u.name, o.amount FROM users u JOIN orders o ON u.id = o.user_id
const usersById = new Map<bigint, User>(users.map((u) => [u.id, u]));
const result: Array<{ name: string; amount: bigint }> = [];
for (const o of orders) {
  const u = usersById.get(o.userId);
  if (u !== undefined) result.push({ name: u.name, amount: o.amount });
}
```

**Merge join (when both sides are sorted by the join key)**: two-pointer walk over both arrays; emitter only chooses this when the IR proves both sides are sorted (via a `Sorted` provenance tag).

**Nested-loop join (when one side is small)**: cardinality threshold is 16 (heuristic from `Iterator.from(small).flatMap(...)` benchmarks on V8); below that the hash table allocation cost dominates.

**Choice algorithm**: the IR `query/joinStrategy.go` pass selects per join. The TS emitter consumes the choice and emits the right shape.

## Sub-phase 7.3, Group-by

### Decisions made (7.3)

**Mochi**: `[(k, sum(o.amount for o in g)) for (k, g) in group(orders, by=o.userId)]`

**TypeScript**:

```typescript
const groups: Map<bigint, Order[]> = Map.groupBy(orders, (o) => o.userId);
const result = Iterator.from(groups)
  .map(([k, g]) => [k, g.reduce((acc, o) => acc + o.amount, 0n)] as const)
  .toArray();
```

`Map.groupBy` is ES2024 (TC39 Stage 4, native in Node 22, Deno 2, Bun 1.1, Chromium 117+). `Object.groupBy` is also available but returns a plain object; the emitter prefers `Map.groupBy` for the `Map<K, V[]>` shape (avoids the prototype-pollution surface of object-as-map).

**Aggregations**: `sum`, `count`, `min`, `max`, `avg` lower to inlined reduces. `mochiAvg(xs)` is a runtime helper because `avg` over `bigint`s wants exact rational return type (the Mochi spec defines `avg` as `int -> float` for now; the emitter emits `Number(sum) / xs.length`).

## Sub-phase 7.4, Top-K

### Decisions made (7.4)

**`@mochi/runtime/query/heap`**: hand-rolled min-heap, roughly 80 LOC of TS:

```typescript
export class MinHeap<T> {
  private buf: T[] = [];
  constructor(private readonly cmp: (a: T, b: T) => number) {}
  size(): number { return this.buf.length; }
  peek(): T | undefined { return this.buf[0]; }
  push(v: T): void { /* sift up */ }
  pop(): T | undefined { /* swap, sift down */ }
}
```

**`top-K` lowering**: `[x for x in xs order by f(x) limit K]` lowers to a single pass that maintains a K-size min-heap (over the `(-key, value)` for top-K-largest) and emits the heap contents at the end.

**Choice vs full sort**: emitter uses heap when `K < len(xs) / 4` (heuristic), otherwise falls back to `.toSorted((a, b) => cmp(a, b)).slice(0, K)`.

## Sub-phase 7.5, Async sources

### Decisions made (7.5)

**`AsyncIterable<T>`**: Mochi `stream<T>` lowers to `AsyncIterable<T>` (Phase 10 lands the full stream surface). The query DSL pipes async sources through `Iterator.fromAsync(asyncSource).filter(...).map(...)`.

**For-await fallback**: when the IR signals an async source plus a synchronous consumer that wants the full result, the emitter emits a manual `for await` accumulator. `Iterator.fromAsync` is TC39 Stage 4 in ES2024.

**Backpressure**: `for await` is naturally pull-based; the producer awaits the consumer at each iteration. No queue is interposed (the agent's queue from Phase 9 handles cross-task buffering separately).

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/query.go` | Comprehension and query DSL lowering; iterator-helper vs Array.prototype choice |
| `transpiler3/typescript/lower/joins.go` | Hash / merge / nested-loop join lowering; strategy selection from IR |
| `transpiler3/typescript/lower/groupby.go` | Map.groupBy emission and aggregation inlining |
| `transpiler3/typescript/lower/topk.go` | Top-K min-heap emission and full-sort fallback |
| `runtime3/typescript/src/query/heap.ts` | `MinHeap<T>` for top-K |
| `runtime3/typescript/src/query/aggregations.ts` | `mochiSum`, `mochiCount`, `mochiAvg`, etc. |
| `transpiler3/typescript/build/phase07_test.go` | `TestPhase7Query` |
| `tests/transpiler3/typescript/fixtures/phase07-query/` | 40 fixtures |

## Test set

- `TestPhase7Query`, 40 fixtures four-runtime.
- `TestPhase7QueryBudget`, the runtime's `query/` sub-path stays under 1 KB gzipped.
- `TestPhase7QueryPerf`, a 1M-row representative joins fixture completes within 2x vm3 wall-clock baseline.

## Deferred work

- arquero, danfojs, duckdb-wasm, polars-js. All rejected per [[08-dataset-pipeline]] §12.
- Window functions (`row_number() over (partition by k order by t)`). Phase 7 ships group-by only; window functions are a v1.5 candidate.
- Distributed query (sharded over multiple workers). Not in MEP-52 scope.
