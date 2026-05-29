---
title: "08. Dataset pipeline"
sidebar_position: 9
sidebar_label: "08. Dataset pipeline"
description: "Query DSL lowering via straight-line for loops + slices stdlib helpers, hash/merge joins, group-by, top-K via container/heap, Datalog semi-naive evaluation at compile-time."
---

# 08. Dataset pipeline

This note describes how Mochi's query DSL and Datalog rules lower onto Go.

## Query DSL

Mochi's query is `from x in xs where p select e`, optionally with `order_by`, `skip`, `take`, joins, group-by, and aggregations. The clower lowers it to an `aotir.QueryExpr` that the Go lowerer turns into straight-line code.

### Simple filter + map

```mochi
from x in xs where x > 0 select x * 2
```

lowers to:

```go
result := []int64{}
for _, x := range xs {
    if x > 0 {
        result = append(result, x*2)
    }
}
```

No iterator chain, no `filter().map().collect()`. Straight-line code is what `gofmt` makes look clean and what `go vet` reasons about correctly.

### Order_by + skip + take

```mochi
from x in xs order by x.score desc skip 10 take 5
```

lowers to:

```go
result := []Foo{}
for _, x := range xs {
    result = append(result, x)
}
sort.SliceStable(result, func(i, j int) bool {
    return result[i].Score > result[j].Score
})
result = result[10:]
if len(result) > 5 {
    result = result[:5]
}
```

`sort.SliceStable` rather than `slices.SortFunc` because `SliceStable` is stable (matters when `order_by` has ties) and `slices.SortFunc` is `O(n log n)` non-stable. Go 1.21 added `slices.SortStableFunc` but we keep `sort.SliceStable` to match Go 1.21 floor.

### Joins

```mochi
from x in xs join y in ys on x.id == y.id select { x, y }
```

For small `xs` / `ys` the lowerer emits a nested loop (cross product + filter). For large joins it uses a hash join via `mochiruntime/query.HashJoin`:

```go
result := []Result{}
hashTable := map[K][]Y{}
for _, y := range ys {
    hashTable[y.Id] = append(hashTable[y.Id], y)
}
for _, x := range xs {
    for _, y := range hashTable[x.Id] {
        result = append(result, Result{X: x, Y: y})
    }
}
```

The lowerer chooses nested-loop vs hash join based on a fixture-driven heuristic (currently: hash join when at least one side is annotated as a "table" in the source); Phase 7.4 documents the heuristic.

Left joins emit a marker `found` bool inside the inner loop; cross joins skip the `on` predicate.

### Group-by + aggregations

```mochi
from x in xs group by x.category select { category: x.category, total: sum(x.amount) }
```

lowers to:

```go
groups := mochiruntime.OMap[string, []Order]{}
for _, x := range xs {
    groups.Append(x.Category, x)
}
result := []Result{}
for _, cat := range groups.Keys() {
    bucket := groups.Get(cat)
    total := int64(0)
    for _, o := range bucket {
        total += o.Amount
    }
    result = append(result, Result{Category: cat, Total: total})
}
```

The `OMap` (insertion-ordered map) gives deterministic iteration order for the result, matching vm3.

### Top-K

```mochi
from x in xs order by x.score desc take 10
```

When `take` is small relative to `len(xs)`, the lowerer chooses a heap-backed top-K via `mochiruntime/query.TopK`:

```go
result := mochiruntime.TopK(xs, 10, func(a, b Foo) bool {
    return a.Score > b.Score
})
```

`TopK` uses `container/heap` with a min-heap of size 10, scanning xs once. O(N log K) versus O(N log N) for a full sort.

The lowerer chooses the heap path when `take` is known at compile-time and is below a threshold (currently 100, configurable per fixture).

### String ops in queries

`contains`, `len`, `index` inside the `where` or `select` clause lower to the corresponding `mochiruntime/stringz` helpers. Phase 7.3 covers the string-op subset of the query DSL.

### Arena-allocated query results

For queries that produce a known-size result, the lowerer can pre-allocate the slice:

```go
result := make([]Foo, 0, len(xs))   // arena allocation
```

This avoids the slice-grow-on-append cost. The arena lowering kicks in when the query has no `where` clause (so the output size equals the input size). Phase 7.5 wires it.

## Datalog

Mochi's Datalog surface is `fact parent(alice, bob)`, `rule ancestor(X, Z) := parent(X, Z)`, `rule ancestor(X, Z) := parent(X, Y), ancestor(Y, Z)`, `query ancestor(alice, _)`. The Go lowerer evaluates the entire program at compile-time via semi-naive fixpoint in `transpiler3/go/lower/datalog.go`.

```mochi
fact parent("alice", "bob")
fact parent("bob", "carol")
rule ancestor(X, Z) := parent(X, Z)
rule ancestor(X, Z) := parent(X, Y), ancestor(Y, Z)
query ancestor("alice", _)
```

evaluates at compile time to `[("alice", "bob"), ("alice", "carol")]` and lowers to:

```go
ancestorResult := []Tuple{
    {V0: "alice", V1: "bob"},
    {V0: "alice", V1: "carol"},
}
```

Compile-time evaluation has two consequences:

1. The runtime cost of Datalog is zero. The query result is a frozen literal in the emitted Go.
2. Recursive rules with parametric queries (`rule reachable(X) := edge(start, X)` where `start` is a runtime variable) are not supported. The clower rejects these with a "non-ground Datalog rule" error.

Phase 8 lands the compile-time evaluator. If future surface requires runtime evaluation, the `mochiruntime/datalog` package is the home for it.

## Aggregation primitives

`sum(xs)`, `min(xs)`, `max(xs)`, `count(xs)`, `avg(xs)` lower to runtime helpers or direct stdlib calls depending on the element type:

- `sum(xs: list<int>)` → `mochiruntime.SumI64(xs)`
- `min(xs: list<int>)` → `slices.Min(xs)`
- `max(xs: list<int>)` → `slices.Max(xs)`
- `count(xs)` → `int64(len(xs))`
- `avg(xs: list<int>)` → `mochiruntime.AvgI64(xs)` (returns float64)

The `slices` stdlib package shipped in Go 1.21, which is our floor.

## Cross-target consistency

The query lowering shape (straight-line for loop with append, sort.SliceStable for order_by, hash join for big joins) matches the C target (MEP-45) and Rust target (MEP-53) within the constraints of each target's stdlib. This keeps the cross-target test corpus (which compares stdout byte-equality) clean: the same query produces the same result tuples in the same order across every target.
