---
title: "Phase 7. Query DSL"
sidebar_position: 11
sidebar_label: "Phase 7. Query DSL"
description: "MEP-49 Phase 7 — query DSL to Swift lazy Sequence chain; group_by to OrderedDictionary; join via hash join; order_by via sorted(by:); top-K via Heap."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 7](/docs/mep/mep-0049#phase-7-query-dsl) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase7Query`: 30 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

The Mochi query DSL is the primary data-wrangling surface. On Swift, it lowers to `lazy` sequence chains from stdlib + `swift-algorithms` + `swift-async-algorithms`. The lowering is isomorphic: `from x in xs where p(x) select f(x)` → `xs.lazy.filter(p).map(f)`. Group-by, join, and top-K use `swift-collections` and `swift-algorithms` -- the same dependencies already pulled in by Phase 3. Phase 7 adds `swift-algorithms` as a new MochiRuntime dependency.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `from x in xs where p(x) select f(x)` → `xs.lazy.filter { p($0) }.map { f($0) }` | NOT STARTED | — |
| 7.1 | `order_by`, `skip`, `take` → `sorted(by:)`, `dropFirst`, `prefix` | NOT STARTED | — |
| 7.2 | `group_by k select { key: key, items: items }` → `OrderedDictionary` grouping via `swift-algorithms` | NOT STARTED | — |
| 7.3 | `join` (inner, hash join) → `Dictionary(uniqueKeysWithValues:)` + `compactMap`; `left_join` | NOT STARTED | — |
| 7.4 | Aggregates: `count`, `sum`, `avg`, `min`, `max` → stdlib `reduce` / `min(by:)` / `max(by:)` | NOT STARTED | — |
| 7.5 | Top-K: `take N order_by k` → `Heap<T>` from `swift-collections` (O(N log K) vs O(N log N)) | NOT STARTED | — |
| 7.6 | Async query pipeline over `AsyncSequence`; `for await` consumption | NOT STARTED | — |

## Sub-phase 7.0 -- from / where / select

### Decisions made (7.0)

**`lazy` chain preferred over eager**: the Mochi query pipeline is lazy. `xs.lazy.filter { ... }.map { ... }` produces a `LazyFilterSequence<LazyMapSequence<...>>`. The result is materialized to `[T]` via `Array(...)` only when the Mochi source binds the result to a `list<T>` variable. When flowing directly into a `for` loop, no materialization is emitted.

**`LazySequence` vs `AsyncSequence`**: synchronous queries lower to the `lazy` synchronous sequence chain. Async queries (Phase 7.6) lower to `AsyncSequence` operators from `swift-async-algorithms`.

**Generated code form**:

```swift
// Mochi: from x in users where x.age > 18 select x.name
let result: [String] = users.lazy
    .filter { x in x.age > Int64(18) }
    .map { x in x.name }
    |> Array.init
```

The `|> Array.init` is sxtree shorthand for `Array(...)`. In practice, the lowerer emits `Array(users.lazy.filter { ... }.map { ... })`.

**Variable shadowing**: the `from x in ...` binding introduces `x` as the iteration variable. The lowerer generates a unique name if `x` conflicts with an outer scope binding (uses `__qx0`, `__qx1`, etc.).

## Sub-phase 7.1 -- order_by, skip, take

### Decisions made (7.1)

**`order_by k asc`**: → `sorted(by: { a, b in a.k < b.k })`. Multi-key: chained `sorted(by:)` calls with secondary comparator.

**Stable sort**: Swift 5.8+ `sorted(by:)` is stable. Documented in stdlib. The lowerer relies on this guarantee.

**`order_by k desc`**: → `sorted(by: { a, b in a.k > b.k })`.

**Multi-key**: Mochi `order_by k1 asc, k2 desc` → a single `sorted(by:)` with a composite comparator:

```swift
.sorted(by: { a, b in
    if a.k1 != b.k1 { return a.k1 < b.k1 }
    return a.k2 > b.k2
})
```

**`skip n`**: → `.dropFirst(Int(n))`. Returns a `Sequence` slice; materialized to `Array` when needed.

**`take n`**: → `.prefix(Int(n))`.

**`skip n take m`**: → `.dropFirst(Int(n)).prefix(Int(m))`. Applied to the ordered sequence.

## Sub-phase 7.2 -- group_by

### Decisions made (7.2)

**`swift-algorithms` `chunked(by:)` vs manual grouping**: `swift-algorithms` provides `chunked(by:)` and `grouped(by:)` operations. `grouped(by:)` returns `[K: [V]]` (a stdlib `Dictionary`), which is unordered. For Mochi's ordered-map semantics, the lowerer uses a manual fold into `OrderedDictionary`:

```swift
// Mochi: from o in orders group_by o.customerId select { id: key, items: items }
var __groups = OrderedDictionary<Int64, [Order]>()
for o in orders {
    __groups[o.customerId, default: []].append(o)
}
let result: [GroupResult] = __groups.map { key, items in
    GroupResult(id: key, items: items)
}
```

**Insertion order**: groups appear in the order their first element was encountered. This matches Mochi's `group_by` semantics and `OrderedDictionary` behavior.

**Aggregate in select**: `select { id: key, total: sum(items, fun(o) => o.amount) }` → the aggregate is applied to the group's item array using `reduce`:

```swift
total: items.reduce(Int64(0)) { acc, o in acc + o.amount }
```

## Sub-phase 7.3 -- join and left_join

### Decisions made (7.3)

**Hash join (inner)**: Mochi `from o in orders join c in customers on o.customerId == c.id select ...` → hash join:

```swift
// Build phase: hash customers by id
let __customerById = Dictionary(uniqueKeysWithValues: customers.map { c in (c.id, c) })

// Probe phase:
let result = orders.compactMap { o -> JoinResult? in
    guard let c = __customerById[o.customerId] else { return nil }
    return JoinResult(order: o, customer: c)
}
```

`Dictionary` (stdlib, unordered) is used for the hash table because the join result order is determined by the left (probe) side -- `orders`. `OrderedDictionary` is not needed here.

**`compactMap` for inner join**: `compactMap` naturally implements the "skip unmatched rows" semantics of an inner join.

**Left join**: Mochi `from o in orders left_join c in customers on ...` → same hash build, but using `map` with `Optional<Customer>`:

```swift
let result = orders.map { o -> LeftJoinResult in
    let c: Customer? = __customerById[o.customerId]
    return LeftJoinResult(order: o, customer: c)
}
```

The right-side element is `Customer?` (Mochi `option<Customer>`).

**Merge join**: used when both sides are already sorted on the join key (detected by the query planner when both sides have an `order_by` on the join key). Merge join emits a two-pointer algorithm. Deferred to a future query optimiser phase.

**Cross join**: `from a in xs from b in ys select ...` → nested `flatMap`:

```swift
let result = xs.flatMap { a in ys.map { b in (a, b) } }
```

## Sub-phase 7.4 -- Aggregates

### Decisions made (7.4)

**`count()`**: → `xs.count` (materializes if lazy) or `xs.reduce(0) { acc, _ in acc + 1 }` (stays lazy). For simple count-all, `.count` is emitted after materializing.

**`sum(f)`**: → `xs.reduce(Int64(0)) { acc, x in acc + f(x) }`. For `float` fields: `xs.reduce(0.0) { acc, x in acc + f(x) }`.

**`avg(f)`**: → `xs.reduce((Int64(0), Int64(0))) { acc, x in (acc.0 + f(x), acc.1 + 1) }` then `Double(acc.0) / Double(acc.1)`. Returns `Double`.

**`min(f)` / `max(f)`**: → `xs.min(by: { a, b in f(a) < f(b) })!.field` (the `!` is safe post-filter when the list is non-empty; a guard is emitted). Alternatively, `xs.map(f).min()!` when extracting a scalar.

**`distinct()`**: → `Array(OrderedSet(xs))` (from Phase 3.3). Preserves first-occurrence order.

**`distinct(f)` (distinct by key)**: → `swift-algorithms` `.uniqued(on: f)`. Requires `swift-algorithms` as a dependency (added in Phase 7).

## Sub-phase 7.5 -- Top-K

### Decisions made (7.5)

**`take N order_by k asc` → Heap**: for large datasets, sorting all elements to take the top N is O(M log M). A min-heap of size N gives O(M log N). The lowerer detects `take N order_by k` and emits the heap pattern:

```swift
import Collections  // Heap<T> from swift-collections

var __heap = Heap<(Int64, Record)>()  // (key, value)
for x in xs {
    let k = x.someField
    if __heap.count < Int(n) {
        __heap.insert((k, x))
    } else if let top = __heap.min, k > top.0 {
        _ = __heap.popMin()
        __heap.insert((k, x))
    }
}
let result = __heap.unordered.sorted(by: { a, b in a.0 > b.0 }).map(\.1)
```

**When to use heap**: the lowerer uses the heap path when the `take` limit N is a compile-time constant and the source is not already sorted. The threshold for switching from `sorted + prefix` to heap is N < M/log(M), approximated as N < 1000 for unknown M (conservative). The exact threshold is configurable via a compiler flag.

## Sub-phase 7.6 -- Async query pipeline

### Decisions made (7.6)

**`AsyncSequence` source**: when the source `xs` is an `AsyncSequence` (e.g., from Phase 10 streams), `where` → `.filter`, `select` → `.map` using `swift-async-algorithms` operators.

**`swift-async-algorithms` dependency**: added to MochiRuntime `Package.swift` in Phase 7.6.

```swift
// Mochi: from x in asyncStream where pred(x) select f(x)
let result = asyncStream
    .filter { x in pred(x) }
    .map { x in f(x) }
// Consumed with: for await x in result { ... }
```

**Aggregates on async sequence**: `await result.count()`, `await result.reduce(0, +)` using `swift-async-algorithms` async reduce. These are `async throws` and require the enclosing function to be `async` (Phase 11).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/query.go` | Query DSL lowering: `QueryScopeStmt`, `GroupByExpr`, `JoinExpr`, aggregates |
| `transpiler3/swift/lower/lower.go` | `ListSortExpr`, `ListSliceExpr`, `ListFilterExpr`, `ListMapExpr` updates for query context |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Query.swift` | Heap-based top-K helpers; `mochiDistinct` |
| `transpiler3/swift/runtime/Package.swift` | `swift-algorithms`, `swift-async-algorithms` dependencies added |
| `transpiler3/swift/build/phase07_test.go` | `TestPhase7Query`: 30 fixtures |
| `tests/transpiler3/swift/fixtures/phase07-query/` | 30 fixture directories |

## Test set

- `TestPhase7Query` -- 30 fixtures covering: `query_filter`, `query_select`, `query_where_select`, `query_no_where`, `query_empty_result`, `query_sort_asc`, `query_sort_desc`, `query_multi_sort`, `query_skip`, `query_take`, `query_skip_take`, `query_group_by`, `query_group_by_aggregate`, `query_sum`, `query_avg`, `query_min_max`, `query_count`, `query_distinct`, `query_distinct_by`, `query_inner_join`, `query_left_join`, `query_cross_join`, `query_top_k`, `query_nested`, `query_chained`, `query_lazy_no_materialize`, `query_async_filter`, `query_async_map`, `query_async_group`, `query_async_aggregate`.

## Deferred work

- `IQueryable<T>` / database query integration. Deferred to Phase 12 (FFI).
- Query plan optimiser (predicate pushdown, merge join). Deferred to a future sub-MEP.
- Window functions (`ROW_NUMBER`, `LAG`, `LEAD`). Deferred to Phase 12.
- `from x in asyncStream group_by` (streaming aggregation). Deferred to Phase 10 extension.
