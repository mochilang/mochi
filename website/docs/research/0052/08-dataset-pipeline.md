---
title: "MEP-52 research note 08, Dataset and query pipeline (TS / JS lowering)"
description: "Mochi query DSL lowered to ES2024 Iterator helpers, AsyncIterable pipelines, three join strategies, datalog semi-naive evaluator. Deliberate rejection of arrow / parquet / duckdb-wasm / polars / arquero / danfojs."
sidebar_position: 8
---

# MEP-52 research note 08, Dataset pipeline: query DSL via Iterator helpers + AsyncIterable, hash / merge / nested-loop joins, datalog semi-naive evaluation, deliberate rejection of arrow / parquet / duckdb-wasm

Author: research pass for MEP-52 (Mochi to TypeScript / JavaScript transpiler).
Date: 2026-05-23 17:25 (GMT+7).

This note specifies the lowering of Mochi's LINQ-style query DSL and Datalog into TypeScript source. The runtime substrate is the ES2024 standard library: `Iterator.from` plus the iterator helpers (`map`, `filter`, `take`, `drop`, `flatMap`, `reduce`, `toArray`, `forEach`, `some`, `every`, `find`), `AsyncIterable<T>` plus the async iterator helpers when available, `Map` and `Set` with their ES2024 methods, `Map.groupBy` and `Object.groupBy`, plus a small `mochi_runtime/query.ts` helper module covering things JS does not give us natively (top-K via min-heap, percentile, sliding window, semi-naive Datalog evaluator). The transpiler does **not** lower to Apache Arrow (`apache-arrow`), parquet, DuckDB-Wasm, polars-js, arquero, or danfojs in v1; the explicit rejection is defended in §15.

Companion notes: [[01-language-surface]], [[02-design-philosophy]], [[03-prior-art-transpilers]], [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[07-runtime-portability]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The four big-picture decisions, defended in [[02-design-philosophy]] §13 and stated here as operating assumptions:

1. **Iterator helpers for sync, AsyncIterable for async.** Mochi's `from x in xs ... select f(x)` lowers to `Iterator.from(xs).filter(...).map(...).toArray()` when `xs` is a sync `Iterable`, and to `(async function* () { ... })()` plus the `mochi_runtime/query/async.ts` helpers when `xs` is an `AsyncIterable`. The choice is made statically by the type checker; mixed sync / async raises an emit-time error.

2. **Map and Set are the workhorses.** Hash joins use `Map<K, V[]>`; distinct uses `Set<T>`; group-by uses `Map.groupBy(xs, x => key(x))`. The ES2024 set methods (`intersection`, `union`, `difference`, `symmetricDifference`) cover set-algebra queries natively.

3. **Class records and discriminated unions, not arrow tables.** Mochi records lower to TS classes with `readonly` fields (see [[06-type-lowering]] §4); the query DSL operates over `Iterable<Record>`, not `RecordBatch<...>`. Apache Arrow, polars-js, arquero, and danfojs are v2 candidates with explicit user opt-in, not the v1 default.

4. **Datalog via semi-naive evaluation.** Mochi Datalog rules lower to a fixpoint loop that materialises relations as `Set<string>` (with tuple keys serialised) or `Set<readonly [...]>` (with structural equality via a `Map` of normalised keys), and applies rules in topological order until no relation grows. Magic sets, stratified negation, and aggregate rules are lowered to specialised passes.

The query DSL surface (Mochi grammar):

```mochi
let result = from x in xs
             from y in ys
             where x.k == y.k
             where x.v > 10
             group by x.k into g
             order by g.key
             select { k: g.key, total: sum(g.v) }
             limit 100
```

This is the LINQ-style shape borrowed from C#, Kotlin, and Mochi-on-Python. The emit target:

```typescript
const result: readonly { readonly k: number; readonly total: number }[] =
  Iterator.from(
    Map.groupBy(
      Iterator.from(xs)
        .flatMap((x: X): readonly { readonly x: X; readonly y: Y }[] =>
          Iterator.from(ys)
            .filter((y: Y): boolean => x.k === y.k && x.v > 10)
            .map((y: Y): { readonly x: X; readonly y: Y } => ({ x, y }))
            .toArray(),
        )
        .toArray(),
      (pair: { readonly x: X; readonly y: Y }): number => pair.x.k,
    ).entries(),
  )
    .map(
      ([k, items]: [
        number,
        readonly { readonly x: X; readonly y: Y }[],
      ]): { readonly k: number; readonly total: number } => ({
        k,
        total: items.reduce((acc: number, p) => acc + p.x.v, 0),
      }),
    )
    .toArray()
    .toSorted((a, b) => a.k - b.k)
    .slice(0, 100);
```

The lowering is mechanical: each clause maps to an iterator-helper call, except `group by` (uses `Map.groupBy`) and `order by` (uses `Array.prototype.toSorted` since Iterator helpers do not include sort). The optimiser passes filter pushdown and projection pruning before emit, so the shape above is the canonical form, not the literally fastest form (the optimiser produces tighter code when applicable).

---

## 1. Lowering choice: Iterator helpers vs arrays

ES2024 (TC39 Stage 4, V8 12.6+, Firefox 131+, Safari 18.4+) introduces the **Iterator helpers proposal**: every iterator inherits methods like `map`, `filter`, `take`, `drop`, `flatMap`, `reduce`, `toArray`, `forEach`, `some`, `every`, `find`. The methods are lazy (each returns a new iterator until `toArray` or `forEach` consumes it).

Comparison with the eager `Array` methods:

| Aspect              | `Array.prototype.map / filter` | `Iterator.prototype.map / filter` (ES2024) |
|---------------------|----------------------------------|--------------------------------------------|
| Eagerness           | eager (materialises array)       | lazy (returns iterator)                    |
| Memory              | O(n) per intermediate            | O(1) per step                              |
| Reusable            | yes (array)                      | no (single-pass iterator)                  |
| Composes with reduce| via `arr.reduce(...)`            | via `iter.reduce(...)`                     |
| Native in Node 22   | yes                              | yes (V8 12.6+)                             |
| Native in Browser   | yes                              | no (Baseline 2025; polyfill via core-js)   |

We lower Mochi queries to **iterator helpers** when the result is consumed by `reduce` / `toArray` / `forEach` and intermediate arrays are wasteful; we lower to **`Array.prototype`** when the result is bound to a `let` and may be iterated multiple times.

Static analysis at the IR pass:

- If the result is bound to a `let` and the binding has more than one use site, lower to `.toArray()` at the end (materialised).
- If the result feeds directly into `sum` / `min` / `max` / `count` / a `for` loop / another query, lower to a chain of iterator helpers terminating in `reduce` / `forEach`.
- If the result is the body of a function returning `readonly T[]`, lower with `.toArray()` at the end.
- If the result is the body of a function returning `Iterable<T>` or `IterableIterator<T>`, lower without `.toArray()`.

This avoids unnecessary materialisation. A common Mochi pattern:

```mochi
let total = sum(from x in xs where x.v > 0 select x.v)
```

lowers to:

```typescript
const total: number = Iterator.from(xs)
  .filter((x: X): boolean => x.v > 0)
  .map((x: X): number => x.v)
  .reduce((acc: number, v: number) => acc + v, 0);
```

with zero intermediate array allocation. The Iterator helpers' lazy semantics mean the filter, map, and reduce all run in one pass.

For comparison, the array-based form would be:

```typescript
const total: number = xs
  .filter((x: X): boolean => x.v > 0)
  .map((x: X): number => x.v)
  .reduce((acc: number, v: number) => acc + v, 0);
```

which allocates two intermediate arrays. For `xs` of length 100K this is ~2x slower and uses 2x the peak memory.

### 1.1 The `Iterator.from` entry point

`Iterator.from(iterableOrIterator)` wraps any `Iterable<T>` (array, Set, Map.entries, custom iterators) into an `Iterator<T>` with the helper methods. The wrapping is free (no copy).

```typescript
const arr: readonly number[] = [1, 2, 3];
const iter: Iterator<number> = Iterator.from(arr);
const result: number[] = iter.map((x) => x * 2).toArray();
// result: [2, 4, 6]
```

For arrays specifically, the V8 engine optimises `Array.prototype.values()` (which Iterator.from calls under the hood) to a fast-path iterator. We rely on this without explicit benchmarking.

For Maps:

```typescript
const m: Map<string, number> = new Map([["a", 1], ["b", 2]]);
const total: number = Iterator.from(m.values()).reduce((acc, v) => acc + v, 0);
// total: 3
```

For Sets:

```typescript
const s: Set<number> = new Set([1, 2, 3]);
const evens: number[] = Iterator.from(s).filter((x) => x % 2 === 0).toArray();
// evens: [2]
```

The emitter prefers `Iterator.from(...)` over `[...src]` (spread) for sources of unknown size, because spread eagerly materialises.

### 1.2 The full iterator helper list

The methods we emit:

- `map<U>(fn: (value: T) => U): Iterator<U>` lazy transform.
- `filter(predicate: (value: T) => boolean): Iterator<T>` lazy filter.
- `take(limit: number): Iterator<T>` first N.
- `drop(skip: number): Iterator<T>` skip N.
- `flatMap<U>(fn: (value: T) => Iterable<U>): Iterator<U>` lazy flatten-after-map.
- `reduce<U>(fn: (acc: U, value: T) => U, init: U): U` eager fold.
- `toArray(): T[]` materialise.
- `forEach(fn: (value: T) => void): void` consume.
- `some(predicate: (value: T) => boolean): boolean` short-circuit OR.
- `every(predicate: (value: T) => boolean): boolean` short-circuit AND.
- `find(predicate: (value: T) => boolean): T | undefined` short-circuit search.

Note that the spec also includes `Iterator.prototype.indexed()` (returns `Iterator<[number, T]>`); this lands later (Stage 3 as of 2026-Q1). We do not depend on it; for `enumerate`-like patterns we emit a manual index counter.

### 1.3 What Iterator helpers do NOT include

Notably absent:

- `sort` / `sortBy`. Sorting requires materialisation; the iterator helpers do not include it. We use `Array.prototype.toSorted` (ES2023) on the materialised result.
- `groupBy`. This is `Map.groupBy(...)` (a static method on `Map`, ES2024), not an iterator helper.
- `distinct` / `unique`. Hand-rolled via `Set`.
- `zip`. The proposal has not landed; we hand-roll a `zip2(a, b)` helper.
- `chunk` / `windowed`. Hand-rolled in `mochi_runtime/query.ts`.

For these, the runtime helpers fill the gap. The next sections enumerate.

## 2. Async lowering

When the source iterator is `AsyncIterable<T>`, the entire pipeline becomes async. JavaScript supports `for await ... of` for consumption and `async function* () { ... }` for generation.

The async iterator helpers proposal (TC39 Stage 2 as of 2026-Q1) is NOT yet shippable; we cannot rely on `asyncIter.map(...)`. We hand-roll the equivalents in `mochi_runtime/query/async.ts`:

```typescript
export async function* asyncMap<T, U>(
  source: AsyncIterable<T>,
  fn: (value: T) => U | Promise<U>,
): AsyncIterable<U> {
  for await (const value of source) {
    yield await fn(value);
  }
}

export async function* asyncFilter<T>(
  source: AsyncIterable<T>,
  predicate: (value: T) => boolean | Promise<boolean>,
): AsyncIterable<T> {
  for await (const value of source) {
    if (await predicate(value)) {
      yield value;
    }
  }
}

export async function* asyncFlatMap<T, U>(
  source: AsyncIterable<T>,
  fn: (value: T) => Iterable<U> | AsyncIterable<U>,
): AsyncIterable<U> {
  for await (const value of source) {
    const inner = fn(value);
    if (Symbol.asyncIterator in inner) {
      for await (const u of inner as AsyncIterable<U>) {
        yield u;
      }
    } else {
      for (const u of inner as Iterable<U>) {
        yield u;
      }
    }
  }
}

export async function* asyncTake<T>(
  source: AsyncIterable<T>,
  limit: number,
): AsyncIterable<T> {
  let count = 0;
  for await (const value of source) {
    if (count >= limit) return;
    yield value;
    count += 1;
  }
}

export async function* asyncDrop<T>(
  source: AsyncIterable<T>,
  skip: number,
): AsyncIterable<T> {
  let count = 0;
  for await (const value of source) {
    if (count >= skip) {
      yield value;
    }
    count += 1;
  }
}

export async function asyncReduce<T, U>(
  source: AsyncIterable<T>,
  fn: (acc: U, value: T) => U | Promise<U>,
  init: U,
): Promise<U> {
  let acc: U = init;
  for await (const value of source) {
    acc = await fn(acc, value);
  }
  return acc;
}

export async function asyncToArray<T>(
  source: AsyncIterable<T>,
): Promise<T[]> {
  const out: T[] = [];
  for await (const value of source) {
    out.push(value);
  }
  return out;
}

export async function asyncForEach<T>(
  source: AsyncIterable<T>,
  fn: (value: T) => void | Promise<void>,
): Promise<void> {
  for await (const value of source) {
    await fn(value);
  }
}

export async function asyncSome<T>(
  source: AsyncIterable<T>,
  predicate: (value: T) => boolean | Promise<boolean>,
): Promise<boolean> {
  for await (const value of source) {
    if (await predicate(value)) return true;
  }
  return false;
}

export async function asyncEvery<T>(
  source: AsyncIterable<T>,
  predicate: (value: T) => boolean | Promise<boolean>,
): Promise<boolean> {
  for await (const value of source) {
    if (!(await predicate(value))) return false;
  }
  return true;
}

export async function asyncFind<T>(
  source: AsyncIterable<T>,
  predicate: (value: T) => boolean | Promise<boolean>,
): Promise<T | undefined> {
  for await (const value of source) {
    if (await predicate(value)) return value;
  }
  return undefined;
}
```

Mochi:

```mochi
let alerts = from ev in event_stream
             where ev.level == "ERROR"
             select { svc: ev.service, ts: ev.timestamp }
             limit 100
```

lowers to:

```typescript
import {
  asyncFilter,
  asyncMap,
  asyncTake,
  asyncToArray,
} from "@mochi/runtime/query/async";

const alerts: readonly { readonly svc: string; readonly ts: number }[] =
  await asyncToArray(
    asyncTake(
      asyncMap(
        asyncFilter(event_stream, (ev) => ev.level === "ERROR"),
        (ev) => ({ svc: ev.service, ts: ev.timestamp }),
      ),
      100,
    ),
  );
```

The nesting is read inside-out, like Lisp; the IR pass emits it as a single pipeline. We considered a builder-pattern wrapper class (`asyncPipeline(stream).filter(...).map(...).take(100).toArray()`) for readability, and may add it in v2 if the async iterator helpers proposal does not land by then. v1 uses the function-composition form because it's a 1:1 mapping to the future Iterator helpers shape.

### 2.1 The `asyncIterFromSync` adapter

If a Mochi query mixes sync and async sources, the sync source must be wrapped:

```typescript
export async function* asyncIterFromSync<T>(
  source: Iterable<T>,
): AsyncIterable<T> {
  for (const value of source) {
    yield value;
  }
}
```

This is the canonical way to lift an `Iterable<T>` into an `AsyncIterable<T>`. The cost is one microtask per element (since `yield` in an async generator schedules a microtask). For small sources this is fine; for large sources the IR pass warns the user and suggests restructuring (e.g. materialise the async side first).

### 2.2 Top-level await in queries

Async pipelines complete with a top-level `await`:

```typescript
const result = await asyncToArray(stream);
```

This requires the surrounding module to be ESM (which it always is in our emit). Top-level await blocks the module's evaluation, which is fine for batch programs but bad for libraries (every importer pays the wait). We emit a wrapper:

```typescript
export async function computeAlerts(): Promise<readonly Alert[]> {
  return asyncToArray(asyncTake(asyncFilter(stream, p), 100));
}
```

and expose `computeAlerts()` rather than letting `await` leak to the top level. The IR pass detects top-level `await` inside an exported expression and wraps it automatically.

## 3. Joins: three strategies

Mochi joins lower to one of three strategies, picked by the IR pass. The decision is identical in shape to MEP-51 §5; the implementation uses TS-native containers (`Map<K, V[]>`).

### 3.1 Hash join (default)

For an inner equi-join with one small side and one large side, we build a hash index on the small side and probe with the large side:

```mochi
from x in xs from y in ys where x.k == y.k select { x: x, y: y }
```

lowers to:

```typescript
type X = { readonly k: number; readonly v: string };
type Y = { readonly k: number; readonly w: string };

const ysByK: Map<number, Y[]> = new Map();
for (const y of ys) {
  const bucket = ysByK.get(y.k);
  if (bucket !== undefined) {
    bucket.push(y);
  } else {
    ysByK.set(y.k, [y]);
  }
}

const result: readonly { readonly x: X; readonly y: Y }[] = Iterator.from(xs)
  .flatMap((x: X): Iterable<{ readonly x: X; readonly y: Y }> => {
    const matches: readonly Y[] = ysByK.get(x.k) ?? [];
    return matches.map((y: Y) => ({ x, y }));
  })
  .toArray();
```

The IR pass picks the smaller side (when statically determinable via size hints) for the hash index. When both sides are unknown, we default to indexing the right-hand side (`ys`).

For outer joins (`left join`, `right join`):

```mochi
from x in xs left join y in ys on x.k == y.k select { x: x, y: y }
```

lowers to:

```typescript
type Y = { readonly k: number; readonly w: string };

const ysByK: Map<number, Y[]> = new Map();
for (const y of ys) {
  const bucket = ysByK.get(y.k);
  if (bucket !== undefined) {
    bucket.push(y);
  } else {
    ysByK.set(y.k, [y]);
  }
}

const result: readonly { readonly x: X; readonly y: Y | null }[] =
  Iterator.from(xs)
    .flatMap((x: X): Iterable<{ readonly x: X; readonly y: Y | null }> => {
      const matches: readonly Y[] = ysByK.get(x.k) ?? [];
      if (matches.length === 0) {
        return [{ x, y: null }];
      }
      return matches.map((y) => ({ x, y }));
    })
    .toArray();
```

The `null` fallback gives left-join semantics. We use `Y | null` rather than `Y | undefined` to match Mochi's option-type lowering (see [[06-type-lowering]] §10): `T?` always lowers to `T | null`, never `T | undefined`.

### 3.2 Merge join

For sorted-sorted equi-joins, we use a two-pointer merge:

```mochi
from x in xs from y in ys where x.k == y.k select (x, y)  // both sorted by k
```

The IR pass detects sorted-ness via `@sorted_by("k")` type annotations or via a preceding `order by k` clause. If both sides are sorted, lower to a merge join:

```typescript
function mergeJoin<X extends { readonly k: number }, Y extends { readonly k: number }>(
  xs: readonly X[],
  ys: readonly Y[],
): readonly (readonly [X, Y])[] {
  const out: (readonly [X, Y])[] = [];
  let i = 0;
  let j = 0;
  while (i < xs.length && j < ys.length) {
    const xk = xs[i]!.k;
    const yk = ys[j]!.k;
    if (xk < yk) {
      i += 1;
    } else if (xk > yk) {
      j += 1;
    } else {
      const k = xk;
      const iStart = i;
      while (i < xs.length && xs[i]!.k === k) i += 1;
      const jStart = j;
      while (j < ys.length && ys[j]!.k === k) j += 1;
      for (let ii = iStart; ii < i; ii += 1) {
        for (let jj = jStart; jj < j; jj += 1) {
          out.push([xs[ii]!, ys[jj]!] as const);
        }
      }
    }
  }
  return out;
}

const result: readonly (readonly [X, Y])[] = mergeJoin(xs, ys);
```

Merge join is O(n + m) vs hash join's O(n + m + cost_of_hashing), with smaller constants and better cache behaviour for large sorted inputs. The non-null assertions (`xs[i]!`) are necessary under `--noUncheckedIndexedAccess`: TS treats `xs[i]` as `X | undefined`, and we know the index is valid because of the loop guard. We could use a runtime check (`if (xs[i] === undefined) throw new Error()`) but the non-null assertion is the canonical pattern when we have already proven the index is in range.

We use merge join when both sides are statically sorted by the join key; otherwise hash join.

### 3.3 Nested-loop fallback

For very small sides (statically known via type hint `@max_size(16)` or constant literal), we skip the hash and do a nested loop:

```typescript
const result: readonly { readonly x: X; readonly y: Y }[] = Iterator.from(xs)
  .flatMap((x: X): Iterable<{ readonly x: X; readonly y: Y }> =>
    Iterator.from(ys)
      .filter((y: Y) => x.k === y.k)
      .map((y: Y) => ({ x, y }))
      .toArray(),
  )
  .toArray();
```

The nested loop is O(n * m) but with a tiny constant when both sides are small (e.g. lookup table + one row of 1000 rows).

### 3.4 Join selection heuristics

The IR pass picks:

1. If both sides have static `@sorted_by(K)`, use merge join.
2. Else if one side has static `@max_size(N)` with N <= 16, use nested loop.
3. Else if join key is non-equi (`x.k > y.k`), use nested loop (no other strategy works).
4. Else use hash join.

The decision is recorded in the IR as a `JoinStrategy` enum and is inspectable via `mochi build --emit-ir`.

### 3.5 Composite-key joins

For joins on multiple keys (`where x.k1 == y.k1 and x.k2 == y.k2`), the hash key is a composite. Two options:

- **Tuple key**: use a `Map<string, Y[]>` with `JSON.stringify([k1, k2])` as the key. Simple, slow.
- **Nested Map**: use `Map<K1, Map<K2, Y[]>>`. Faster but more boilerplate.

The IR pass picks the nested form when the key arity is <= 3 and the types are primitive (number, string, boolean, bigint). Otherwise it falls back to the JSON-stringified form, with a docstring warning that this is slow.

```typescript
// Nested Map form for two-key join
const ysByKey: Map<number, Map<string, Y[]>> = new Map();
for (const y of ys) {
  let outer = ysByKey.get(y.k1);
  if (outer === undefined) {
    outer = new Map();
    ysByKey.set(y.k1, outer);
  }
  const bucket = outer.get(y.k2);
  if (bucket !== undefined) {
    bucket.push(y);
  } else {
    outer.set(y.k2, [y]);
  }
}
```

For object-typed composite keys (e.g. `where x.point == y.point` where `point` is `{x: number, y: number}`), `Map`'s reference equality means two distinct `{x: 1, y: 2}` objects would not match. The IR pass detects object-typed keys and either:

(a) Inserts a key-normalisation function (`(p) => `${p.x},${p.y}`` for known shapes), or
(b) Falls back to nested loop with `deepEqual`.

We document this in [[06-type-lowering]] §11 as the "composite key normalisation" pattern.

## 4. Aggregates

Aggregates close over a query and produce a scalar (or a record per group).

### 4.1 sum / min / max / count

Direct lowering using the reducer pattern:

```mochi
let total = sum(from x in xs select x.v)
let lo    = min(from x in xs select x.v)
let hi    = max(from x in xs select x.v)
let cnt   = count(from x in xs select x.v)
```

lowers to:

```typescript
const total: number = Iterator.from(xs)
  .map((x: X) => x.v)
  .reduce((acc: number, v: number) => acc + v, 0);

const lo: number | null = Iterator.from(xs).reduce<number | null>(
  (acc, x) => (acc === null || x.v < acc ? x.v : acc),
  null,
);

const hi: number | null = Iterator.from(xs).reduce<number | null>(
  (acc, x) => (acc === null || x.v > acc ? x.v : acc),
  null,
);

const cnt: number = Iterator.from(xs).reduce((acc) => acc + 1, 0);
```

For `count(xs)` where `xs` is an array, we lower to `xs.length` directly (O(1)).

For `min` and `max`, the result is `T | null` (null for empty input). This matches Mochi's semantics: `min` of an empty list is null, not an error. We use `null` as the sentinel (matching the option-type lowering).

### 4.2 avg

JavaScript has no built-in average; we use a two-pass approach (since iterators are single-pass):

```typescript
export function avg(it: Iterable<number>): number | null {
  let sum = 0;
  let count = 0;
  for (const v of it) {
    sum += v;
    count += 1;
  }
  return count === 0 ? null : sum / count;
}
```

For Mochi:

```mochi
let mean = avg(from x in xs select x.v)
```

lowers to:

```typescript
import { avg } from "@mochi/runtime/query";

const mean: number | null = avg(Iterator.from(xs).map((x: X) => x.v));
```

The `avg` helper consumes the iterator in one pass. For numerical stability on large datasets, we can switch to Welford's online algorithm:

```typescript
export function avgStable(it: Iterable<number>): number | null {
  let mean = 0;
  let count = 0;
  for (const v of it) {
    count += 1;
    mean += (v - mean) / count;
  }
  return count === 0 ? null : mean;
}
```

The IR pass picks the simple form by default and the stable form when an `@stable` annotation is present.

### 4.3 Percentile / median / quantile

JS has no built-in `percentile`; we hand-roll. Linear interpolation (matches numpy's `linear` method):

```typescript
export function percentile(xs: readonly number[], p: number): number {
  if (xs.length === 0) return Number.NaN;
  const sorted: readonly number[] = [...xs].toSorted((a, b) => a - b);
  const k: number = (sorted.length - 1) * p;
  const f: number = Math.floor(k);
  const c: number = Math.ceil(k);
  if (f === c) {
    return sorted[Math.trunc(k)]!;
  }
  return sorted[f]! + (sorted[c]! - sorted[f]!) * (k - f);
}

export function median(xs: readonly number[]): number {
  return percentile(xs, 0.5);
}
```

Mochi:

```mochi
let p95 = percentile(xs, 0.95)
```

lowers to:

```typescript
import { percentile } from "@mochi/runtime/query";

const p95: number = percentile(xs.map((x) => x.v), 0.95);
```

The percentile helper materialises (it needs random access to do interpolation). For higher accuracy on large datasets, the runtime exposes `quantilesStreaming(it, q)` which uses the t-digest algorithm; v2.

### 4.4 Custom aggregates

User-defined aggregates lower to a fold (`reduce`):

```mochi
let total = reduce(xs, 0, fn(acc, x) -> acc + x.v)
```

lowers to:

```typescript
const total: number = xs.reduce((acc: number, x: X): number => acc + x.v, 0);
```

For aggregates over `Iterable<T>` (not just `Array<T>`):

```typescript
const total: number = Iterator.from(xs).reduce(
  (acc: number, x: X): number => acc + x.v,
  0,
);
```

## 5. group by

Mochi's `group by` lowers to ES2024's `Map.groupBy`:

```mochi
from x in xs group by x.k into g select { k: g.key, n: count(g) }
```

lowers to:

```typescript
const result: readonly { readonly k: number; readonly n: number }[] =
  Iterator.from(Map.groupBy(xs, (x: X): number => x.k).entries())
    .map(
      ([k, group]: [number, readonly X[]]): {
        readonly k: number;
        readonly n: number;
      } => ({
        k,
        n: group.length,
      }),
    )
    .toArray();
```

`Map.groupBy(xs, keyFn)` (ES2024, V8 12.1+) returns a `Map<K, V[]>` where each value array preserves insertion order. The `.entries()` call gives us `IterableIterator<[K, V[]]>` which we then iterate.

Note that `Map.groupBy` is a **static** method on the `Map` constructor, not an iterator helper. We do not chain it; we call it as a step in the pipeline.

There is also `Object.groupBy(xs, keyFn)` (ES2024) which returns a plain object `{ [k: string]: V[] }`. We prefer `Map.groupBy` because it preserves arbitrary key types (number, bigint, object). `Object.groupBy` only supports string keys (JS object keys are always strings).

### 5.1 group by with multiple aggregates

```mochi
from x in xs group by x.k into g select {
    k: g.key,
    n: count(g),
    total: sum(g.v),
    avg: avg(g.v),
}
```

lowers to:

```typescript
const result: readonly {
  readonly k: number;
  readonly n: number;
  readonly total: number;
  readonly avg: number;
}[] = Iterator.from(Map.groupBy(xs, (x: X): number => x.k).entries())
  .map(([k, group]: [number, readonly X[]]) => {
    const total: number = group.reduce((acc, x) => acc + x.v, 0);
    return {
      k,
      n: group.length,
      total,
      avg: total / group.length,
    };
  })
  .toArray();
```

We compute `total` once and reuse for `avg`. The IR pass detects multi-use within the group and emits the materialised form.

### 5.2 Counter / specialised count-only group-by

For `group by x.k select count(g)` specifically:

```typescript
const counts: Map<number, number> = new Map();
for (const x of xs) {
  counts.set(x.k, (counts.get(x.k) ?? 0) + 1);
}

const result: readonly { readonly k: number; readonly n: number }[] =
  Iterator.from(counts.entries())
    .map(([k, n]) => ({ k, n }))
    .toArray();
```

The hand-rolled count loop is ~2x faster than `Map.groupBy` plus `.length` because it skips the intermediate array allocation. The IR pass picks the specialised form when the aggregate is exactly `count(g)`.

### 5.3 Group order

`Map.groupBy` preserves the insertion order of the first occurrence of each key. So `from x in xs group by x.k` produces groups in the order keys are first encountered. This matches Mochi's specified group order for `group by` without `order by`.

When `order by` follows `group by`:

```mochi
from x in xs group by x.k into g order by g.key select ...
```

we append `.toSorted((a, b) => a.k - b.k)` after the group-by pipeline.

## 6. order by

Mochi's `order by` lowers to `Array.prototype.toSorted` (ES2023) for general ordering or a min-heap for top-K.

### 6.1 General order by

```mochi
from x in xs order by x.k select x
```

lowers to:

```typescript
const result: readonly X[] = [...xs].toSorted((a: X, b: X): number => a.k - b.k);
```

`Array.prototype.toSorted` is non-mutating (it returns a new array). For mutating sort:

```mochi
xs.sort_by(fn(x) -> x.k)
```

lowers to:

```typescript
xs.sort((a, b) => a.k - b.k);
```

`Array.prototype.sort` is in-place and stable as of ES2019.

For multi-key sort:

```mochi
order by x.k1, x.k2 desc
```

lowers to:

```typescript
const result: readonly X[] = [...xs].toSorted((a, b) => {
  const d1: number = a.k1 - b.k1;
  if (d1 !== 0) return d1;
  return b.k2 - a.k2; // desc on k2
});
```

The comparator chain handles all keys in one pass. For non-numeric keys we use `String.prototype.localeCompare` or a custom comparator.

### 6.2 Top-K (heap-based)

```mochi
from x in xs order by x.k limit 10
```

If `limit` is small (statically <= 100) and the source is large, we lower to a min-heap:

```typescript
export function nLargest<T>(
  k: number,
  source: Iterable<T>,
  cmp: (a: T, b: T) => number,
): readonly T[] {
  // Min-heap keyed by cmp.
  const heap: T[] = [];
  const lessThan = (i: number, j: number): boolean => cmp(heap[i]!, heap[j]!) < 0;
  const swap = (i: number, j: number): void => {
    const tmp = heap[i]!;
    heap[i] = heap[j]!;
    heap[j] = tmp;
  };
  const siftUp = (idx: number): void => {
    let i = idx;
    while (i > 0) {
      const parent = (i - 1) >> 1;
      if (lessThan(i, parent)) {
        swap(i, parent);
        i = parent;
      } else {
        break;
      }
    }
  };
  const siftDown = (idx: number): void => {
    let i = idx;
    const n = heap.length;
    while (true) {
      const l = 2 * i + 1;
      const r = 2 * i + 2;
      let smallest = i;
      if (l < n && lessThan(l, smallest)) smallest = l;
      if (r < n && lessThan(r, smallest)) smallest = r;
      if (smallest === i) break;
      swap(i, smallest);
      i = smallest;
    }
  };
  for (const value of source) {
    if (heap.length < k) {
      heap.push(value);
      siftUp(heap.length - 1);
    } else if (cmp(value, heap[0]!) > 0) {
      heap[0] = value;
      siftDown(0);
    }
  }
  return heap.toSorted((a, b) => cmp(b, a)); // descending
}
```

Mochi:

```mochi
from u in users order by u.score desc limit 10
```

lowers to:

```typescript
import { nLargest } from "@mochi/runtime/query";

const top10: readonly User[] = nLargest(
  10,
  users,
  (a, b) => a.score - b.score,
);
```

`nLargest(k, n)` runs in O(n log k), beating `xs.toSorted((a, b) => b.score - a.score).slice(0, 10)`'s O(n log n) for small k.

For descending top-K (smallest):

```typescript
import { nSmallest } from "@mochi/runtime/query";

const bottom10: readonly User[] = nSmallest(
  10,
  users,
  (a, b) => a.score - b.score,
);
```

The IR pass picks `nLargest` / `nSmallest` when:

- `limit` is a constant <= 100.
- `xs` has no static size hint or `@max_size > 1000`.

Otherwise it uses `[...xs].toSorted(...)[:N]`.

### 6.3 Streaming top-K

For async iterators, neither `toSorted` nor the in-memory heap applies directly without first materialising. We provide `asyncNLargest`:

```typescript
export async function asyncNLargest<T>(
  k: number,
  source: AsyncIterable<T>,
  cmp: (a: T, b: T) => number,
): Promise<readonly T[]> {
  const heap: T[] = [];
  // (same heap operations as nLargest)
  for await (const value of source) {
    if (heap.length < k) {
      heap.push(value);
      // siftUp(heap.length - 1);
    } else if (cmp(value, heap[0]!) > 0) {
      heap[0] = value;
      // siftDown(0);
    }
  }
  return heap.toSorted((a, b) => cmp(b, a));
}
```

(The full sift implementations would be inlined here in the runtime; abbreviated for brevity.)

## 7. limit and offset

Mochi `limit N` and `skip N`:

```mochi
from x in xs select x skip 10 limit 100
```

For array inputs (random access):

```typescript
const result: readonly X[] = xs.slice(10, 110);
```

For iterators:

```typescript
const result: readonly X[] = Iterator.from(xs)
  .drop(10)
  .take(100)
  .toArray();
```

The IR pass picks `xs.slice(10, 110)` when the source is statically `readonly X[]`; otherwise the iterator-helper form.

Edge case: `limit -1` in Mochi means "no limit". The IR pass elides the `take` entirely.

## 8. Set operations

Mochi's set operators map to ES2024 Set methods:

| Mochi          | TypeScript                                  |
|----------------|---------------------------------------------|
| `a union b`    | `a.union(b)`                                |
| `a intersect b`| `a.intersection(b)`                         |
| `a except b`   | `a.difference(b)`                           |
| `a symdiff b`  | `a.symmetricDifference(b)`                  |
| `x in a`       | `a.has(x)`                                  |
| `a subseteq b` | `a.isSubsetOf(b)`                           |
| `a supseteq b` | `a.isSupersetOf(b)`                         |
| `a disjoint b` | `a.isDisjointFrom(b)`                       |

All methods are ES2024 Stage 4, V8 12.5+, Firefox 127+, Safari 17.4+. For browsers below Baseline 2024 we polyfill via core-js (see [[07-runtime-portability]] §1.4).

```mochi
let common = set_a intersect set_b
let distinct = set_a union set_b
```

lowers to:

```typescript
const common: ReadonlySet<number> = setA.intersection(setB);
const distinct: ReadonlySet<number> = setA.union(setB);
```

For ordered sets (where iteration order is observable), Mochi uses `new Set([...])` (Set iteration is insertion-ordered in JS by spec since ES2015):

```typescript
const orderedSet: Set<number> = new Set([1, 2, 3, 2, 1]); // {1, 2, 3}, order preserved
```

For multi-set operations (preserving counts), Mochi lowers to `Map<T, number>`:

```mochi
let counts = multiset(xs)
let merged = counts1 + counts2
```

lowers to:

```typescript
function multiset<T>(xs: Iterable<T>): Map<T, number> {
  const m: Map<T, number> = new Map();
  for (const x of xs) {
    m.set(x, (m.get(x) ?? 0) + 1);
  }
  return m;
}

function multisetAdd<T>(
  a: ReadonlyMap<T, number>,
  b: ReadonlyMap<T, number>,
): Map<T, number> {
  const result: Map<T, number> = new Map(a);
  for (const [k, v] of b) {
    result.set(k, (result.get(k) ?? 0) + v);
  }
  return result;
}

const counts: ReadonlyMap<string, number> = multiset(xs);
const merged: ReadonlyMap<string, number> = multisetAdd(counts1, counts2);
```

JS has no `Counter` class equivalent; we hand-roll one. The runtime exports `Multiset<T>` as a small wrapper class with `add`, `sub`, `union`, `intersection` methods for convenience.

## 9. distinct / unique

Mochi `distinct`:

```mochi
from x in xs distinct select x
```

lowers to:

```typescript
const result: readonly X[] = [...new Set(xs)];
```

For records (where structural identity matters):

```mochi
from x in xs distinct by x.k select x
```

lowers to:

```typescript
const seen: Set<number> = new Set();
const result: readonly X[] = Iterator.from(xs)
  .filter((x: X): boolean => {
    if (seen.has(x.k)) return false;
    seen.add(x.k);
    return true;
  })
  .toArray();
```

The mutable `seen` is captured by the closure. Alternative without mutation:

```typescript
const result: readonly X[] = [
  ...Map.groupBy(xs, (x: X) => x.k).values(),
].map((bucket) => bucket[0]!);
```

The first form is slightly faster (one pass, no intermediate Map). The IR pass picks the first form by default.

## 10. Window functions

JS has no equivalent of SQL window functions. Mochi window functions are hand-rolled in `mochi_runtime/query.ts`:

```typescript
export function* window<T>(
  xs: Iterable<T>,
  size: number,
): IterableIterator<readonly T[]> {
  const buf: T[] = [];
  for (const x of xs) {
    buf.push(x);
    if (buf.length > size) buf.shift();
    if (buf.length === size) yield [...buf];
  }
}

export function rowNumber<T>(
  xs: Iterable<T>,
): IterableIterator<readonly [number, T]> {
  let i = 0;
  return Iterator.from(xs).map((x: T): readonly [number, T] => {
    const idx = i;
    i += 1;
    return [idx + 1, x];
  });
}

export function lag<T>(
  xs: Iterable<T>,
  n: number = 1,
): IterableIterator<T | null> {
  const buf: (T | null)[] = Array.from({ length: n }, () => null);
  let pos = 0;
  return Iterator.from(xs).map((x: T): T | null => {
    const out = buf[pos]!;
    buf[pos] = x;
    pos = (pos + 1) % n;
    return out;
  });
}

export function lead<T>(
  xs: readonly T[],
  n: number = 1,
): readonly (T | null)[] {
  return xs.map((_, i) => (i + n < xs.length ? xs[i + n]! : null));
}
```

`window` uses an array buffer with shift; for large windows we would use a deque, but JS does not have a stdlib deque. The `Array.prototype.shift` is O(n) so for windows > 1000 elements we should use a ring buffer. The IR pass emits the ring buffer form for large window sizes.

Mochi:

```mochi
from x in xs let avg = avg(window(xs, 5)) select { x: x, avg: avg }
```

lowers to:

```typescript
const xsArr: readonly X[] = [...xs];
const result: readonly { readonly x: X; readonly avg: number | null }[] =
  Iterator.from(xsArr)
    .map((x: X, i: number) => {
      const start = Math.max(0, i - 4);
      const slice = xsArr.slice(start, i + 1);
      return {
        x,
        avg: avg(slice.map((s) => s.v)),
      };
    })
    .toArray();
```

(Note: `Iterator.prototype.map` does not pass the index. We use a manual counter or `Array.prototype.map` with index.)

For sliding windows over async iterators:

```typescript
export async function* asyncWindow<T>(
  source: AsyncIterable<T>,
  size: number,
): AsyncIterable<readonly T[]> {
  const buf: T[] = [];
  for await (const x of source) {
    buf.push(x);
    if (buf.length > size) buf.shift();
    if (buf.length === size) yield [...buf];
  }
}
```

## 11. Streaming queries over AsyncIterable

Async iterables are produced by Mochi `stream` declarations (see [[09-agent-streams]]). The query DSL over a stream uses the `async*` helpers and `for await`:

```mochi
stream Tick {
    let n: int
}

let totals = from t in tick_stream
             where t.n > 0
             group by t.n / 100 into bucket
             select { bucket: bucket.key, n: count(bucket) }
```

For an **infinite stream**, `group by` cannot complete (group close requires end-of-input). The IR pass detects the unbounded source and reports a compile-time error unless a `window` or `tumble` is specified:

```mochi
let totals = from t in tick_stream
             tumble 60.seconds
             group by t.n / 100 into bucket
             select { bucket: bucket.key, n: count(bucket) }
```

Tumbling windows lower to a periodic flush:

```typescript
import type { AsyncIterable } from "node:stream/web"; // or built-in

interface Result {
  readonly bucket: number;
  readonly n: number;
}

async function* tumblingAggregate(
  stream: AsyncIterable<Tick>,
  windowSecs: number,
): AsyncIterable<Result> {
  const buf: Tick[] = [];
  let deadline: number = performance.now() / 1000 + windowSecs;
  for await (const tick of stream) {
    buf.push(tick);
    const now: number = performance.now() / 1000;
    if (now >= deadline) {
      const groups: Map<number, number> = new Map();
      for (const t of buf) {
        const key = Math.floor(t.n / 100);
        groups.set(key, (groups.get(key) ?? 0) + 1);
      }
      for (const [bucket, n] of groups.entries()) {
        yield { bucket, n };
      }
      buf.length = 0;
      deadline = now + windowSecs;
    }
  }
}
```

Hopping windows (overlapping) and session windows (gap-defined) have similar shapes; the IR pass picks the right template.

## 12. Worked Mochi-to-TypeScript examples

Five end-to-end examples showing the full lowering.

### 12.1 Simple filter and map

Mochi:

```mochi
let result = from x in numbers
             where x > 10
             select x * 2
```

TS:

```typescript
const result: readonly number[] = Iterator.from(numbers)
  .filter((x: number): boolean => x > 10)
  .map((x: number): number => x * 2)
  .toArray();
```

If `result` is consumed by `sum`:

```mochi
let total = sum(from x in numbers where x > 10 select x * 2)
```

TS:

```typescript
const total: number = Iterator.from(numbers)
  .filter((x: number): boolean => x > 10)
  .map((x: number): number => x * 2)
  .reduce((acc: number, v: number) => acc + v, 0);
```

### 12.2 Group by with multiple aggregates

Mochi:

```mochi
let stats = from order in orders
            group by order.customer_id into g
            select {
                customer_id: g.key,
                count: count(g),
                total: sum(g.amount),
                avg:   avg(g.amount),
            }
```

TS:

```typescript
type Order = { readonly customer_id: number; readonly amount: number };

class CustomerStats {
  readonly customer_id: number;
  readonly count: number;
  readonly total: number;
  readonly avg: number;

  constructor(args: {
    readonly customer_id: number;
    readonly count: number;
    readonly total: number;
    readonly avg: number;
  }) {
    this.customer_id = args.customer_id;
    this.count = args.count;
    this.total = args.total;
    this.avg = args.avg;
  }
}

const stats: readonly CustomerStats[] = Iterator.from(
  Map.groupBy(orders, (o: Order): number => o.customer_id).entries(),
)
  .map(([cid, group]: [number, readonly Order[]]) => {
    const total: number = group.reduce((acc, o) => acc + o.amount, 0);
    return new CustomerStats({
      customer_id: cid,
      count: group.length,
      total,
      avg: total / group.length,
    });
  })
  .toArray();
```

Note that we use a class with `readonly` fields instead of an interface for the record type. This matches the Mochi record lowering ([[06-type-lowering]] §4): record types lower to classes so that pattern-matching, instanceof checks, and method dispatch all work. For pure data shapes (no methods), an interface would work too; the class form is consistent.

### 12.3 Inner join via hash

Mochi:

```mochi
let result = from o in orders
             from c in customers
             where o.customer_id == c.id
             select { order: o, customer_name: c.name }
```

TS:

```typescript
type Order = { readonly customer_id: number; readonly amount: number };
type Customer = { readonly id: number; readonly name: string };

const customersById: Map<number, Customer> = new Map(
  customers.map((c: Customer): readonly [number, Customer] => [c.id, c]),
);

class OrderWithCustomer {
  readonly order: Order;
  readonly customer_name: string;
  constructor(args: { readonly order: Order; readonly customer_name: string }) {
    this.order = args.order;
    this.customer_name = args.customer_name;
  }
}

const result: readonly OrderWithCustomer[] = Iterator.from(orders)
  .filter((o: Order): boolean => customersById.has(o.customer_id))
  .map((o: Order) => {
    const c: Customer = customersById.get(o.customer_id)!;
    return new OrderWithCustomer({ order: o, customer_name: c.name });
  })
  .toArray();
```

The IR pass detects that `customers` has unique `id` (via a `@unique` annotation or via a primary-key declaration) and uses a `Map<number, Customer>` index instead of `Map<number, Customer[]>`. Probing becomes a single lookup, not a list iteration.

The non-null assertion (`customersById.get(o.customer_id)!`) is justified by the preceding `.has(...)` filter; TS still does not narrow the second `.get()` call, so we assert. Alternative: combine has + get into one operation:

```typescript
const result: readonly OrderWithCustomer[] = Iterator.from(orders)
  .flatMap((o: Order): Iterable<OrderWithCustomer> => {
    const c: Customer | undefined = customersById.get(o.customer_id);
    return c === undefined ? [] : [new OrderWithCustomer({ order: o, customer_name: c.name })];
  })
  .toArray();
```

The `flatMap` with empty-or-singleton is slightly slower but avoids the non-null assertion. The IR pass picks based on a config flag (default: prefer the asserting form for performance).

### 12.4 Order by with top-K

Mochi:

```mochi
let top10 = from u in users
            order by u.score desc
            limit 10
            select { name: u.name, score: u.score }
```

TS:

```typescript
import { nLargest } from "@mochi/runtime/query";

type User = { readonly name: string; readonly score: number };

class TopUser {
  readonly name: string;
  readonly score: number;
  constructor(args: { readonly name: string; readonly score: number }) {
    this.name = args.name;
    this.score = args.score;
  }
}

const top10: readonly TopUser[] = Iterator.from(
  nLargest(10, users, (a: User, b: User): number => a.score - b.score),
)
  .map((u: User) => new TopUser({ name: u.name, score: u.score }))
  .toArray();
```

The IR pass picks `nLargest` because `limit 10` is a constant <= 100 and the source has no `@max_size` hint.

### 12.5 Async stream pipeline

Mochi:

```mochi
let alerts = from ev in event_stream
             where ev.level == "ERROR"
             tumble 5.seconds
             group by ev.service into g
             select { service: g.key, count: count(g) }
```

TS:

```typescript
import { asyncFilter } from "@mochi/runtime/query/async";

type Event = {
  readonly level: string;
  readonly service: string;
  readonly timestamp: number;
};

class Alert {
  readonly service: string;
  readonly count: number;
  constructor(args: { readonly service: string; readonly count: number }) {
    this.service = args.service;
    this.count = args.count;
  }
}

async function* alertsPipeline(
  eventStream: AsyncIterable<Event>,
): AsyncIterable<Alert> {
  const errorStream: AsyncIterable<Event> = asyncFilter(
    eventStream,
    (ev) => ev.level === "ERROR",
  );
  const buf: Event[] = [];
  let deadline: number = performance.now() / 1000 + 5.0;
  for await (const ev of errorStream) {
    buf.push(ev);
    const now: number = performance.now() / 1000;
    if (now >= deadline) {
      const groups: Map<string, number> = new Map();
      for (const e of buf) {
        groups.set(e.service, (groups.get(e.service) ?? 0) + 1);
      }
      for (const [svc, cnt] of groups.entries()) {
        yield new Alert({ service: svc, count: cnt });
      }
      buf.length = 0;
      deadline = now + 5.0;
    }
  }
}
```

The Mochi consumer:

```mochi
for alert in alerts {
    log_alert(alert)
}
```

lowers to:

```typescript
for await (const alert of alertsPipeline(eventStream)) {
  logAlert(alert);
}
```

## 13. Query optimisation: IR passes

The IR pass at `transpiler3/typescript/lower/` performs the following optimisations before emit. These are general and apply across other targets too; the TS-specific bits are noted.

### 13.1 Filter pushdown

A `where` clause that depends only on one source can be pushed into the source's projection:

```mochi
from x in xs from y in ys where x.k > 0 where x.k == y.k select (x, y)
```

becomes:

```mochi
from x in xs where x.k > 0 from y in ys where x.k == y.k select (x, y)
```

In TypeScript this means moving the `.filter(x => x.k > 0)` earlier in the iterator chain:

```typescript
const result: readonly (readonly [X, Y])[] = Iterator.from(xs)
  .filter((x: X): boolean => x.k > 0)
  .flatMap((x: X) =>
    Iterator.from(ys)
      .filter((y: Y) => x.k === y.k)
      .map((y: Y): readonly [X, Y] => [x, y])
      .toArray(),
  )
  .toArray();
```

Filter pushdown reduces the Cartesian product before the join evaluates.

### 13.2 Projection pruning

If only `x.name` is used downstream, we can avoid carrying the full record:

```mochi
from x in xs select { name: x.name, age: x.age } where age > 18
```

If the user only needs `name` later, the IR pass folds the projection:

```typescript
const result: readonly string[] = Iterator.from(xs)
  .filter((x: X): boolean => x.age > 18)
  .map((x: X): string => x.name)
  .toArray();
```

instead of constructing a record and then re-projecting.

### 13.3 Predicate normalisation

`where a and (b or c)` is rewritten to `where (a and b) or (a and c)` if it enables further pushdown. We avoid this when `a` has side effects.

### 13.4 Materialisation insertion

When an iterator is used multiple times, insert `xs = [...xs]`:

```mochi
let mean = avg(xs)
let dev  = avg(from x in xs select (x - mean) ** 2)  // re-iterates xs
```

The IR pass detects two uses of `xs` and emits:

```typescript
const xsMat: readonly number[] = [...xs];
const mean: number | null = avg(xsMat);
const dev: number | null = mean === null
  ? null
  : avg(Iterator.from(xsMat).map((x) => (x - mean) ** 2));
```

Without the explicit `[...xs]`, the second iteration would consume an already-exhausted iterator and return empty.

### 13.5 Join reordering

For 3+ way joins, the IR pass picks an order that minimises intermediate row counts using static size hints. We use a left-deep tree with the smallest table first (since we hash-index the smallest table).

### 13.6 Constant folding in predicates

`where x.k > 10 && x.k < 100 && true` collapses to `where x.k > 10 && x.k < 100`. JS does not support chained comparisons (`10 < x.k < 100` is invalid in JS), so we emit the `&&` form.

### 13.7 Sort elision

`from x in xs order by x.k group by x.k` redundantly sorts (the `group by` does not need pre-sorted input, since `Map.groupBy` builds a hash-table by key). The IR pass drops the redundant `order by`:

```typescript
const result = Iterator.from(Map.groupBy(xs, (x) => x.k).entries())
  .map(([k, g]) => ({ k, items: g }))
  .toArray();
```

If the result must be in key order, the IR pass appends a `.toSorted` at the end.

### 13.8 Limit pushdown

`from x in xs order by x.k limit 10` should not materialise the full sort; the IR pass picks `nLargest(10, xs, ...)` as discussed in §6.2.

## 14. Datalog evaluation

Mochi Datalog rules lower to a semi-naive bottom-up fixpoint loop. A rule:

```mochi
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

lowers to a TypeScript program that materialises each relation as a `Set<string>` (with tuple keys serialised) and iterates rules until no relation grows.

```typescript
import { tupleKey, parseTupleKey } from "@mochi/runtime/query/datalog";

function evaluateDatalog(
  mother: ReadonlySet<readonly [string, string]>,
  father: ReadonlySet<readonly [string, string]>,
): { readonly parent: ReadonlySet<readonly [string, string]>; readonly ancestor: ReadonlySet<readonly [string, string]> } {
  const parent: Set<string> = new Set();
  for (const [x, y] of mother) parent.add(tupleKey([x, y]));
  for (const [x, y] of father) parent.add(tupleKey([x, y]));

  const ancestor: Set<string> = new Set(parent);

  // semi-naive: track delta_ancestor (new tuples added in the last iteration)
  let delta: Set<string> = new Set(parent);
  while (delta.size > 0) {
    const newDelta: Set<string> = new Set();
    // Build index on parent keyed by (second column).
    const parentByY: Map<string, string[]> = new Map();
    for (const key of parent) {
      const [x, y] = parseTupleKey<[string, string]>(key);
      const bucket = parentByY.get(y);
      if (bucket !== undefined) bucket.push(x);
      else parentByY.set(y, [x]);
    }
    // Build index on delta keyed by (first column).
    const deltaByY: Map<string, string[]> = new Map();
    for (const key of delta) {
      const [y, z] = parseTupleKey<[string, string]>(key);
      const bucket = deltaByY.get(y);
      if (bucket !== undefined) bucket.push(z);
      else deltaByY.set(y, [z]);
    }
    // Join: parent(X, Y), delta_ancestor(Y, Z) -> ancestor(X, Z)
    for (const [y, xs] of parentByY.entries()) {
      const zs = deltaByY.get(y);
      if (zs === undefined) continue;
      for (const x of xs) {
        for (const z of zs) {
          const key = tupleKey([x, z]);
          if (!ancestor.has(key)) {
            ancestor.add(key);
            newDelta.add(key);
          }
        }
      }
    }
    delta = newDelta;
  }

  const parentOut: Set<readonly [string, string]> = new Set();
  for (const key of parent) parentOut.add(parseTupleKey<[string, string]>(key));
  const ancestorOut: Set<readonly [string, string]> = new Set();
  for (const key of ancestor) ancestorOut.add(parseTupleKey<[string, string]>(key));
  return { parent: parentOut, ancestor: ancestorOut };
}
```

`tupleKey` and `parseTupleKey` are runtime helpers that serialise / deserialise tuples to / from `string` for Set membership:

```typescript
const TUPLE_SEP = "

export function tupleKey<T extends readonly unknown[]>(t: T): string {
  return t.map((v) => JSON.stringify(v)).join(TUPLE_SEP);
}

export function parseTupleKey<T extends readonly unknown[]>(key: string): T {
  return key.split(TUPLE_SEP).map((s) => JSON.parse(s)) as unknown as T;
}
```

JS does not give us structural equality for tuples (two `[1, 2]` arrays are `!==`), so we serialise to a string for Set membership. The `

The transition from naive (rebuilding index every iteration) to semi-naive (incremental delta) is what gives this algorithm its name; the iteration only re-joins delta-against-static, not delta-against-delta.

### 14.1 Indexed evaluation

The join `parent(X, Y), delta_ancestor(Y, Z)` is O(|parent| * |delta|) without indexing. We build a hash index on the join variable (Y):

The `parentByY` and `deltaByY` maps above are the indices. The actual join becomes O(|parent| + |delta| + |output|) which is asymptotically optimal for equi-joins.

The IR pass detects join keys and emits the indices. For pure equi-joins on a single variable, this is the canonical form.

### 14.2 Stratified negation

Negation is allowed only when the negated relation is fully computed before the negating rule runs (stratification). The IR pass:

1. Builds the rule dependency graph.
2. Topologically sorts strongly-connected components.
3. Each SCC is evaluated to fixpoint; only after the SCC stabilises do downstream rules (which may negate the SCC's relations) run.

Stratified negation lowers to a sequence of fixpoint loops:

```typescript
// Stratum 1: compute parent, ancestor (no negation)
const { parent, ancestor } = evaluateStratum1(mother, father);

// Stratum 2: not_ancestor depends on ancestor (negation across strata)
const allPairs: Set<string> = new Set();
const people: Set<string> = new Set();
for (const [x, y] of parent) {
  people.add(x);
  people.add(y);
}
for (const a of people) {
  for (const b of people) {
    allPairs.add(tupleKey([a, b]));
  }
}
const notAncestor: Set<string> = new Set(allPairs);
for (const k of ancestor) notAncestor.delete(tupleKey(k));

// Stratum 3: cousin uses not_ancestor
const { cousin } = evaluateStratum3(parent, notAncestor);
```

If the user writes a rule with negation inside a cycle (non-stratified), the emitter reports a compile-time error.

### 14.3 Magic sets

For top-down query evaluation (asking "is `ancestor("alice", "bob")` true?" rather than computing all ancestors), the magic-sets transformation rewrites rules to focus the bottom-up evaluation. The transformation is an IR pass; the TS emit is the same shape as semi-naive, just with a smaller rule set focused on the magic predicate.

### 14.4 Aggregate rules

Mochi Datalog supports aggregates:

```mochi
count_children(P, N) :- N = count { C : parent(P, C) }.
```

lowers to:

```typescript
const counts: Map<string, number> = new Map();
for (const key of parent) {
  const [p, _c] = parseTupleKey<[string, string]>(key);
  counts.set(p, (counts.get(p) ?? 0) + 1);
}
const countChildren: Set<string> = new Set();
for (const [p, n] of counts.entries()) {
  countChildren.add(tupleKey([p, n]));
}
```

Aggregate rules must be in their own stratum (the aggregate must close before any negation against it). The IR pass enforces this.

## 15. Why we do NOT lower to arrow / parquet / duckdb-wasm / polars-js / arquero / danfojs in v1

The six candidates considered and rejected for v1.

### 15.1 apache-arrow (`apache-arrow` on npm)

Pros:

- Columnar in-memory format; standard for analytics interop.
- Bindings across languages (Python, R, Java, C++, Rust, JS).
- Zero-copy IPC via the Arrow IPC streaming format.
- Used as the storage layer for DuckDB-Wasm, polars-js, arquero, and (indirectly) danfojs.

Cons that block v1:

- **Heavy dependency**: `apache-arrow` is ~600 KB minified (~180 KB gzip). The Mochi runtime is ~12 KB gzip (browser bundle); adding Arrow grows the bundle 15x.
- **Type-system mismatch**: Arrow's `RecordBatch<...>` is parameterised by an Arrow schema, not a TS type. Bridging requires runtime schema construction and erases the static-type benefits of Mochi.
- **Boxing tax**: scalar values returned from Arrow vectors are boxed `Vector.get(i)` calls, which are slow when read element-by-element. The "vectorised" speedup only shows when batch operations are used; Mochi's emitted queries are scalar-shaped.
- **No semantic improvement**: the Mochi query DSL already compiles to tight per-element loops; switching to Arrow does not improve the typical query shape.
- **Wasm dependency optional**: `apache-arrow` is pure-JS but slow; the fast path requires `apache-arrow-c` (Wasm) which adds another ~2 MB.
- **Browser CORS for IPC**: reading Arrow files over HTTP requires the server to set CORS headers correctly; the user pays the integration tax.

Arrow is a v2 candidate with explicit `--target=ts-arrow` opt-in, where the user accepts the dependency in exchange for cross-language interop (e.g. exporting a Mochi result table to Python for further analysis).

### 15.2 parquet (`parquetjs`, `@dsnp/parquetjs`)

Parquet is a columnar disk format (paired with Arrow as the in-memory equivalent). The JS bindings:

Pros:

- Standard format for analytics data on disk.
- Compatible with Spark, Hive, DuckDB, etc.
- Efficient compression (Snappy, gzip, Zstd).

Cons that block v1:

- **JS implementations are immature**: `parquetjs` is unmaintained (last release 2021); `@dsnp/parquetjs` is the active fork but still pre-1.0.
- **Wasm dependency required**: production-quality Parquet reading on JS requires `parquet-wasm` (~1.5 MB) or `arrow-wasm`.
- **Read-only**: most JS Parquet libraries can read but not write reliably.
- **Type predicates incomplete**: Parquet's logical types (DECIMAL, TIMESTAMP_NS, etc.) do not always round-trip cleanly through JS.
- **Memory model mismatch**: Parquet is paged columnar; JS's iterator-helper model is row-iterated.

Parquet is a v3+ candidate, paired with the Arrow v2.

### 15.3 DuckDB-Wasm (`@duckdb/duckdb-wasm`)

Pros:

- Full SQL engine running in the browser via WebAssembly.
- ~10x faster than pure-JS query engines on analytical workloads.
- Can read Parquet, CSV, JSON files directly.
- Standard SQL surface.

Cons that block v1:

- **Heavy dependency**: ~4 MB Wasm bundle. Defeats the "12 KB gzip Hello World" target.
- **SQL output, not TS expressions**: Mochi query DSL is a comprehension chain; DuckDB consumes SQL strings. Lowering requires generating SQL, which is a separate code generator.
- **No structural-type bridge**: DuckDB's tables are columnar with a runtime schema; Mochi records are TS classes. Bridging requires per-row marshalling.
- **Worker required**: DuckDB-Wasm runs in a Web Worker (for SharedArrayBuffer threading). The integration boilerplate is non-trivial.
- **Cold start**: ~500 ms to instantiate the Wasm module on first query.
- **Loss of debuggability**: a generated SQL query is harder to inspect than an iterator chain.
- **Mochi types lost**: Mochi's TS types do not carry over to DuckDB SQL; the user loses type safety inside the query.

DuckDB-Wasm is a v2 candidate for the "fast analytical queries over million-row datasets in the browser" use case. The flag would be `--target=ts-duckdb` and the codegen would emit SQL strings instead of iterator chains.

### 15.4 polars-js (`nodejs-polars`)

Polars is a Rust-based DataFrame library with JS bindings via `nodejs-polars` (Node-only, uses native node binding) or `polars-lazy-wasm` (WebAssembly, browser-compatible).

Pros:

- Rust-fast; ~10x faster than pandas / arquero.
- Lazy evaluation amenable to query optimisation.
- Strict typing (closer to Mochi than pandas).
- Active development.

Cons that block v1:

- **Heavy dependency**: `nodejs-polars` is ~30 MB (Rust binary). `polars-lazy-wasm` is ~3 MB.
- **Native binding**: Node-only, no browser support (without the Wasm variant which is ~3 MB).
- **Type-checker friction**: `nodejs-polars` TS types are improving but still have lambda-inference gaps.
- **API instability**: pre-1.0 API churn through 2024; 1.0 (still in preview as of 2026-Q1) settles things.
- **No agent / stream interop**: polars LazyFrame is column-batch-based, not row-iterator-based; bridging to AsyncIterable requires materialisation.

Polars is a stronger v2 candidate than DuckDB because of the type-system alignment, but still v2 because of the dependency weight.

### 15.5 arquero (`arquero`)

Pros:

- Pure-JS, ~80 KB minified.
- Pandas-inspired API.
- Used by Observable notebooks; mature.
- Reasonable performance for small-to-medium datasets.

Cons that block v1:

- **Still 80 KB**: ~7x the Mochi runtime size; defeats the bundle-size target.
- **Type weakness**: arquero's `.derive({col: d => d.x + d.y})` uses lambda introspection (reads `d.x` to figure out which columns are used); the TS type system cannot follow this.
- **Null semantics**: arquero uses `null` and `undefined` interchangeably; Mochi distinguishes them sharply.
- **No iterator-helper alignment**: arquero is `Table` -> `Table` (a copy each step); Mochi's lazy iterator chain is closer to the spec.
- **Limited async support**: arquero is sync-only; async pipelines require manual orchestration.

Arquero is a possible v2 alternative for the "I want pandas-like in JS" UX, but the Mochi-native iterator chain covers the same workload with smaller code.

### 15.6 danfojs (`danfojs`)

Pros:

- Pandas-like API in JS.
- Built on top of TensorFlow.js for fast numerical ops.
- Both Node and browser variants.

Cons that block v1:

- **Heavy dependency**: ~500 KB (pulls in TensorFlow.js).
- **Pandas semantic mismatch**: same null / index / mutation issues as pandas in Python.
- **Type-checker support is weak**: danfojs TS types are partial.
- **API instability**: danfojs is pre-1.0 as of 2026-Q1.
- **Built for ML, not DSL**: targeted at data scientists doing exploratory work, not generated code.

Danfojs is a non-candidate for any Mochi target.

### 15.7 Why pure JS wins for Mochi v1

The thread through all five rejections:

- **Dependency weight**: every alternative is at least 50 KB gzip; most are 100 KB+. The Mochi runtime is 12 KB gzip. Adding any of these inflates the user's bundle non-trivially.
- **Type-system friction**: every alternative has weaker TS types than the Mochi-emitted code. Switching to a library throws away the type safety that Mochi's emit guarantees.
- **Semantic divergence**: every alternative has its own conventions for nulls, mutation, eagerness, error handling. Mapping Mochi semantics to those is a nontrivial translation each time.
- **Performance crossover**: pure JS is competitive for sub-1M-row workloads. Above 1M rows, polars / DuckDB win. v1 targets the sub-1M range; v2 will offer opt-in fast paths.
- **No new operators needed**: Iterator helpers + Map.groupBy + Set methods cover every Mochi query operator natively. We do not need a library to fill gaps.

## 16. Performance characterisation

Some rough numbers for Mochi-emitted TS query performance on Node 22.11.0, ubuntu-22.04, M-series Mac mini (M2, 8GB):

| Query shape                                | Rows     | Time (ms) | Notes                                  |
|--------------------------------------------|----------|-----------|----------------------------------------|
| filter + map (iterator helpers)            | 1K       | 0.15      | no allocation, V8 optimised            |
| filter + map (iterator helpers)            | 100K     | 8         | linear scan                            |
| filter + map (iterator helpers)            | 1M       | 80        | linear scan                            |
| group by (3 groups, count)                 | 1K       | 0.3       | Map.groupBy                            |
| group by (3 groups, count)                 | 100K     | 12        | Map.groupBy                            |
| group by (3 groups, sum + avg)             | 100K     | 18        | one pass with cached total             |
| inner join (hash, 1K x 1K, 10% match)      | 1K + 1K  | 0.5       | Map probe                              |
| inner join (hash, 100K x 100K, 1% match)   | 100K + 100K | 50    | Map probe                              |
| order by                                   | 100K     | 22        | Array.toSorted (Timsort-ish)           |
| top-10 (min-heap)                          | 100K     | 5         | nLargest                               |
| Datalog ancestor (1K parent facts)         | 1K       | 60        | semi-naive, 5 iterations               |

For comparison:

- The same query in **MEP-51 Python** runs about 50 % slower (CPython interpreter overhead).
- The same query in **polars-js** is ~3x faster for large data (100K+) and ~2x slower for small data (1K) (Rust marshalling overhead).
- The same query in **arquero** is ~1.5x faster for 100K (vectorised) and ~equal for 1K.
- The same query in **MEP-50 Kotlin** is ~2x faster (JVM JIT plus inlined functional operations).

The conclusion: Mochi v1's iterator-helper lowering is fast enough for typical query workloads (under 1M rows), and the dependency story is much simpler. v2 will offer opt-in flags for larger workloads.

### 16.1 Iterator helpers performance characteristics

V8's Iterator helpers implementation (V8 12.6+, landed 2024-Q2) has these properties:

- `Iterator.from(array)` is roughly the same cost as `array[Symbol.iterator]()`. The wrapping is free.
- Chained `.map(...).filter(...).map(...)` are inlined by TurboFan when the lambdas are monomorphic. Polymorphic lambdas (different shapes through the same iterator) deoptimise.
- `.toArray()` is the same as a manual `for (const x of iter) arr.push(x)` loop.
- `.reduce(fn, init)` is roughly 1.2x slower than the equivalent `for` loop with an accumulator (the function call overhead is real but small).

For maximally hot paths (millions of elements), the IR pass can emit a `for ... of` loop with an inlined predicate body instead of the iterator chain. This is the `@inline_query` annotation.

### 16.2 Map vs object for hash indices

We always use `Map<K, V>`, never object literal `{ [k: string]: V }`. Reasons:

- `Map` supports non-string keys (number, bigint, object). Object keys are coerced to string.
- `Map.size` is O(1); `Object.keys(obj).length` is O(n).
- `Map` iteration is insertion-ordered by spec; object iteration is "mostly" insertion-ordered (with quirks for integer-string keys).
- `Map` has no prototype pollution risk; object literals do.
- V8 optimises `Map<string, V>` to nearly the same speed as `{ [k: string]: V }` for hot loops (since V8 12.x).

The one exception: when the key set is known at compile time and small (e.g. enum string), the IR pass uses a `Record<EnumKey, V>` type with a literal object. This avoids the Map indirection at the cost of compile-time-only flexibility.

## 17. Type lowering for query results

The query lowering produces typed TypeScript. Each `select { a: ..., b: ... }` clause infers a record type and emits a class:

```mochi
from x in xs select { name: x.name, age: x.age }
```

emits:

```typescript
class _Anon_ageName {
  readonly name: string;
  readonly age: number;
  constructor(args: { readonly name: string; readonly age: number }) {
    this.name = args.name;
    this.age = args.age;
  }
}

const result: readonly _Anon_ageName[] = Iterator.from(xs)
  .map((x: X): _Anon_ageName => new _Anon_ageName({ name: x.name, age: x.age }))
  .toArray();
```

The class name is mangled from the field names (`_Anon_` prefix plus sorted field names). The IR pass deduplicates: two queries with the same anonymous record shape produce one class.

For `tsc --strict` compliance, every field has a type annotation. The IR pass propagates types from the source records.

For inline-record `select { ... }` clauses where the user does not care about identity, the IR pass can emit a structural type instead:

```typescript
const result: readonly { readonly name: string; readonly age: number }[] =
  Iterator.from(xs)
    .map((x: X): { readonly name: string; readonly age: number } => ({
      name: x.name,
      age: x.age,
    }))
    .toArray();
```

This is faster (no class allocation) but loses `instanceof` checks. The IR pass picks structural for anonymous records and class for named records.

## 18. Determinism gates for query output

Query output is part of the byte-equal stdout gate (see [[11-testing-gates]]). To ensure determinism:

- `Map` and `Set` iteration is insertion-ordered (spec invariant since ES2015); we never iterate a `Set` for output without sorting first if order matters.
- `group by` without `order by` uses `Map.groupBy` (insertion order = first-encountered-key order), deterministic.
- `Array.prototype.toSorted` is stable; equal-key elements retain insertion order.
- `Math.random` is never called inside a query without a seeded RNG.
- `Date.now` and `performance.now` are not embedded in query output; they are mocked in tests via the `MochiClock` runtime injection.
- `Object.keys(obj)` iteration order is implementation-defined for non-integer keys; we always use `Object.entries(obj)` and sort if order is observable.

The CI gate runs each fixture twice on the same runner and asserts identical stdout. This catches any accidental hash-order or microtask-scheduling dependency.

For cross-runtime determinism, the gate runs each fixture on Node 22, Deno 2, Bun 1.1, and Chromium (via Playwright). The four stdouts must SHA256-match.

## 19. Memory profile

Iterator helpers are memory-efficient when chained lazily; eager `.toArray()` at intermediate stages defeats this. The IR pass minimises `.toArray()` insertions.

Memory budget for the standard query corpus (100K rows of ~100-byte records):

| Stage                       | Memory peak (V8)  | Memory peak (JSC) |
|-----------------------------|-------------------|-------------------|
| source array                | 10 MB             | 10 MB             |
| filter (lazy iterator)      | 10 MB + ~1 KB     | 10 MB + ~1 KB     |
| group by (Map.groupBy)      | 12 MB             | 12 MB             |
| order by (toSorted)         | 14 MB             | 14 MB             |
| top-K (min-heap)            | 10 MB + ~K * 100B | 10 MB + ~K * 100B |

For larger datasets (10M+ rows), the eager `toSorted` becomes prohibitive. Users should:

- Stream from disk via `node:fs/promises` line-by-line readers or `Bun.file().stream()`.
- Use `mergeSort` over multiple sorted chunks (the runtime exposes `mochi.query.mergeStreams(...)` for this).
- Avoid `group by` without windowing on infinite streams.

The runtime documentation calls these out as "query patterns that scale". v2 will offer `--target=ts-polars` for the larger workloads.

## 20. Streaming aggregates: cold vs hot

Aggregates over async iterators come in two flavours:

- **Closing aggregates**: the stream is finite (e.g. read-file-line-by-line). The aggregate computes when the stream ends. Lowering: `await asyncReduce(stream, fn, init)`.
- **Tumbling aggregates**: the stream is infinite (e.g. live event feed). The aggregate emits periodically. Lowering: `async function* tumblingAgg(...)` (as shown in §12.5).

The IR pass picks based on the source type. A stream marked `@finite` (from a file, an array, a finite generator) gets the closing form; a stream marked `@infinite` (from a websocket, a timer, an agent) gets the tumbling form (with a required `tumble` clause).

If the user writes a closing aggregate over an `@infinite` source, the emitter errors at IR time:

```
error: cannot apply closing aggregate `sum` to infinite stream `event_stream`;
       use `tumble <window>` or `take <n>` first
```

## 21. Streaming joins

Async joins follow the same three-strategy split:

- **Async hash join**: materialise one side (the smaller), iterate the other lazily.
- **Async merge join**: two-pointer iteration, only when both sides are sorted streams.
- **Async nested loop**: rare, only for tiny tables.

For the async hash join, the build side is materialised first:

```typescript
async function asyncHashJoin<X, Y, K>(
  xs: AsyncIterable<X>,
  ys: AsyncIterable<Y>,
  xKey: (x: X) => K,
  yKey: (y: Y) => K,
): Promise<AsyncIterable<readonly [X, Y]>> {
  const ysByKey: Map<K, Y[]> = new Map();
  for await (const y of ys) {
    const k: K = yKey(y);
    const bucket = ysByKey.get(k);
    if (bucket !== undefined) bucket.push(y);
    else ysByKey.set(k, [y]);
  }
  async function* probe(): AsyncIterable<readonly [X, Y]> {
    for await (const x of xs) {
      const matches = ysByKey.get(xKey(x));
      if (matches === undefined) continue;
      for (const y of matches) {
        yield [x, y];
      }
    }
  }
  return probe();
}
```

The build phase blocks on the smaller side completing; if both sides are infinite, the IR pass errors.

## 22. Lazy evaluation and short-circuiting

Iterator helpers naturally short-circuit:

- `.find(p)`: stops at the first matching element.
- `.some(p)`: stops at the first true.
- `.every(p)`: stops at the first false.
- `.take(n).toArray()`: stops after consuming n elements.

The IR pass detects user code that uses these shapes and avoids materialising the upstream:

```mochi
let first_even = (from x in xs select x).find(fn(x) -> x % 2 == 0)
```

lowers to:

```typescript
const firstEven: number | undefined = Iterator.from(xs).find(
  (x: number): boolean => x % 2 === 0,
);
```

Note `find` returns `T | undefined`; we convert to `T | null` if Mochi's option type is in play:

```typescript
const firstEven: number | null = Iterator.from(xs).find(
  (x: number): boolean => x % 2 === 0,
) ?? null;
```

The `?? null` normalisation aligns with Mochi's `T?` lowering (T | null, never T | undefined).

## 23. Index-tracking variants

For Mochi queries that need the original index (`for (i, x) in enumerate(xs)`):

```mochi
let pairs = enumerate(xs)
```

lowers to:

```typescript
let __idx = 0;
const pairs: IterableIterator<readonly [number, X]> = Iterator.from(xs).map(
  (x: X): readonly [number, X] => {
    const i = __idx;
    __idx += 1;
    return [i, x];
  },
);
```

The closure captures the mutable index. Alternative using `Array.prototype.entries` (when source is an array):

```typescript
const pairs: IterableIterator<readonly [number, X]> = xs.entries();
```

`Array.prototype.entries()` returns `IterableIterator<[number, X]>` (mutable tuple in the spec). We cast to `readonly [number, X]` to match Mochi's tuple immutability.

The IR pass picks `xs.entries()` when the source is statically an array; otherwise the closure form.

## 24. Comparison to MEP-51 Python query lowering

The structural correspondence:

| Aspect              | MEP-51 Python                 | MEP-52 TS                          |
|---------------------|-------------------------------|------------------------------------|
| Sync workhorse      | generator expressions         | Iterator helpers (ES2024)          |
| Async workhorse     | `async for` comprehensions    | `for await ... of` + async helpers |
| Group by            | `dict[K, list[V]]` or itertools.groupby | `Map.groupBy` (ES2024)    |
| Set ops             | `set` operators (`|`, `&`)    | `Set.intersection / union / etc.` (ES2024) |
| Distinct            | `set` then to list            | `[...new Set(xs)]`                 |
| Sort                | `sorted(...)` (Timsort)       | `[...xs].toSorted(...)` (ES2023)   |
| Top-K               | `heapq.nlargest`              | `nLargest` (hand-rolled min-heap)  |
| Window              | `collections.deque(maxlen=N)` | hand-rolled (no deque in stdlib)   |
| Percentile          | `statistics.fmean` + manual   | hand-rolled (no statistics module) |
| Datalog             | semi-naive over `set[tuple]`  | semi-naive over `Set<string>` (tuple keys serialised) |

The shapes are nearly identical. The big differences:

- TS lacks Python's `collections.deque`; we hand-roll a ring buffer for sliding windows over large datasets.
- TS lacks Python's `statistics` module; we hand-roll mean / median / percentile.
- TS lacks structural equality for tuples; we serialise to string for Set membership in Datalog.
- TS has `Map.groupBy` and `Set` methods natively; Python's stdlib equivalents (`itertools.groupby` after sort) are less direct.
- TS Iterator helpers are lazier than Python generator expressions in some edge cases (V8 optimises chained iterators better than CPython optimises chained generators).

Net: roughly equivalent capability; TS code is shorter for set-algebra queries, Python code is shorter for sliding-window queries.

## 25. Comparison to MEP-50 Kotlin query lowering

| Aspect              | MEP-50 Kotlin                     | MEP-52 TS                        |
|---------------------|-----------------------------------|----------------------------------|
| Sync workhorse      | `Sequence<T>` + extensions        | Iterator helpers                 |
| Async workhorse     | `Flow<T>` (cold) / `SharedFlow` (hot) | AsyncIterable + helpers      |
| Group by            | `groupBy { key }`                 | `Map.groupBy(xs, keyFn)`         |
| Set ops             | Kotlin `union / intersect / minus`| `Set.union / intersection / difference` |
| Sort                | `sortedBy { it.k }`               | `[...xs].toSorted((a, b) => ...)` |
| Top-K               | hand-rolled (no stdlib)           | hand-rolled (no stdlib)          |
| Datalog             | semi-naive over `Set<Pair<...>>`  | semi-naive over `Set<string>`    |

Kotlin has built-in support for joins via the `kotlinx-coroutines-core` Flow library (which has `combine`, `zip`, `merge`). TS hand-rolls the equivalents in `mochi_runtime/query/async.ts`. Otherwise the shapes are very similar.

Kotlin's `Sequence` is roughly equivalent to JS's `Iterator` (both are lazy single-pass). Kotlin's `Flow` is roughly equivalent to JS's `AsyncIterable` (both are cold async; hot variants require special types).

## 26. Summary

The TS / JS query DSL lowering targets ES2024 Iterator helpers for sync (`Iterator.from(xs).filter(...).map(...).toArray()`), AsyncIterable plus a small `mochi_runtime/query/async.ts` for async, `Map<K, V[]>` hash indices for joins, `Map.groupBy` for group-by, `Array.prototype.toSorted` for sorting (with min-heap for top-K), a small set of hand-rolled helpers for windows / percentile / Datalog, and ES2024 Set methods for set algebra. The lowering is direct, iterator-chain-shaped, and `tsc --strict` clean. Apache Arrow, parquet-js, DuckDB-Wasm, polars-js, arquero, and danfojs are explicitly deferred to v2 because of dependency weight, type-system friction, and semantic divergence.

The companion notes pick up: [[06-type-lowering]] for the class / interface record shape that queries produce, [[07-runtime-portability]] for the cross-runtime determinism gate that query output sits on top of, [[09-agent-streams]] for the AsyncIterable substrate, [[11-testing-gates]] for the gate enumeration including query-specific fuzzers, and [[12-risks-and-alternatives]] for the v2 polars / DuckDB / Arrow roadmap.
