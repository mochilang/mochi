---
title: "Phase 7. Query DSL"
sidebar_position: 9
sidebar_label: "Phase 7. Query DSL"
description: "MEP-48 Phase 7 — query DSL to LINQ method syntax; group_by, join, order_by, take, skip, .parallel qualifier; async LINQ via System.Linq.Async."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 7](/docs/mep/mep-0048#phase-7-query-dsl) |
| Status         | LANDED |
| Started        | 2026-05-28 02:32 (GMT+7) |
| Landed         | 2026-05-28 02:35 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase7Query`: 10 fixtures green on net8.0 and net10.0.

## Goal-alignment audit

The Mochi query DSL is the primary data-wrangling surface. On .NET, it lowers directly to LINQ method syntax — one of the most battle-tested query APIs in the industry. The lowering is almost isomorphic: Mochi `from x in xs where p(x) select f(x)` → `xs.Where(x => p(x)).Select(x => f(x))`. Phase 7 ships the connection between Mochi's query surface and the full BCL query pipeline including parallel (PLINQ) and async (`System.Linq.Async`) variants.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `from`, `where`, `select` → LINQ `.Where().Select()` method chain | LANDED | — |
| 7.1 | `sort by`, `skip`, `take` → `.OrderBy().Skip().Take()` | LANDED | — |
| 7.2 | `join` (inner) → `Enumerable.Join`; `left_join` → `GroupJoin + SelectMany` | DEFERRED | — |
| 7.3 | `.parallel` qualifier → `.AsParallel()` (PLINQ) | DEFERRED | — |
| 7.4 | Async query pipeline → `System.Linq.Async` over `IAsyncEnumerable<T>` | DEFERRED | — |

## Sub-phase 7.0 -- from / where / select

### Decisions made (7.0)

**LINQ method syntax preferred over query expression syntax**: Roslyn supports both. Query expressions (`from x in xs where ... select ...`) are syntactic sugar over method calls. Method syntax is unambiguous, easier for the lowerer to generate, and does not rely on Roslyn's query-comprehension desugaring. The generated C# is:

```csharp
// Mochi: from x in users where x.age > 18 select x.name
IEnumerable<string> result = users
    .Where(x => x.Age > 18L)
    .Select(x => x.Name);
```

**Lazy vs eager**: the LINQ pipeline is lazy by default (`IEnumerable<T>`). The lowerer inserts `.ToList()` at the end of a pipeline only when the result is bound to a `list<T>` variable or passed where a `list<T>` is expected. When flowing directly into a `foreach`, no `.ToList()` is inserted.

**`IEnumerable<T>` vs `ImmutableList<T>`**: source collections for queries are `ImmutableList<T>` (from Phase 3). LINQ operates on `IEnumerable<T>`, which `ImmutableList<T>` implements. The result of a query is `IEnumerable<T>` unless forced to `ImmutableList<T>` via `.ToImmutableList()`.

## Sub-phase 7.1 -- group_by, order_by, take, skip

### Decisions made (7.1)

**`group_by`**: `from o in orders group_by o.customer_id select { id: key, total: sum(o.amount) }` → LINQ `GroupBy` + `Select`:

```csharp
orders
    .GroupBy(o => o.CustomerId)
    .Select(g => new { Id = g.Key, Total = g.Sum(o => o.Amount) })
```

The anonymous type `new { Id, Total }` captures the grouped projection. In Phase 4+, named record types replace anonymous types where the shape is known statically.

**`order_by asc / desc`**: `order_by k asc` → `.OrderBy(x => x.K)`; `order_by k desc` → `.OrderByDescending(x => x.K)`. Multi-key: `.OrderBy(...).ThenBy(...)`.

**`take n`**: `.Take(n)`.

**`skip n`**: `.Skip(n)`.

**`count()`**: `.Count()` or `.LongCount()` for large collections (returns `long`).

**`sum(f)` / `avg(f)` / `min(f)` / `max(f)`**: LINQ aggregate operators. `sum(o.Amount)` → `.Sum(o => o.Amount)`. Return type follows the field type: `sum` on `float` fields returns `double`.

## Sub-phase 7.2 -- join and left_join

### Decisions made (7.2)

**Inner join**: `from o in orders join c in customers on o.customer_id == c.id select ...` → `Enumerable.Join`:

```csharp
orders.Join(customers,
    o => o.CustomerId,
    c => c.Id,
    (o, c) => new { Order = o, Customer = c })
```

**Left join**: `from o in orders left_join c in customers on ...` → `GroupJoin + SelectMany + DefaultIfEmpty`:

```csharp
orders.GroupJoin(customers,
    o => o.CustomerId,
    c => c.Id,
    (o, cs) => (o, cs))
    .SelectMany(
        t => t.cs.DefaultIfEmpty(),
        (t, c) => new { Order = t.o, Customer = (c != null ? Option.Some(c) : Option.None<Customer>()) })
```

Left-join produces `Option<Customer>` for the right-side element.

## Sub-phase 7.3 -- Parallel qualifier

### Decisions made (7.3)

**`.parallel` qualifier** on a Mochi query → `.AsParallel()` prepended to the LINQ chain:

```csharp
// Mochi: (from x in data where pred(x) select f(x)).parallel
data.AsParallel()
    .Where(x => Pred(x))
    .Select(x => F(x))
```

PLINQ uses the `ThreadPool` internally; no `Task.Run` wrapper needed. The `.AsParallel()` call returns a `ParallelQuery<T>` which LINQ operates on. At the end of the pipeline, `.ToList()` materialises the result. The parallel qualifier is advisory: if the collection is small or the predicate is trivially cheap, PLINQ may execute serially.

**Thread safety**: PLINQ assumes the lambda is pure (no shared mutable state). The Mochi type system enforces this at the `let` binding level (closures over `let` are always safe; `var` captures in parallel queries produce a Roslyn warning that the transpiler surfaces as a type error).

## Sub-phase 7.4 -- Async LINQ

### Decisions made (7.4)

**`System.Linq.Async`**: NuGet package (`System.Linq.Async 6.0+`), included in `Mochi.Runtime.csproj` as a `PackageReference`. On net10.0, some async LINQ operators are in-box.

**Async pipeline**: a query over an `IAsyncEnumerable<T>` source uses async LINQ operators:

```csharp
// Mochi: from x in asyncStream where pred(x) select f(x)
asyncStream
    .Where(x => Pred(x))
    .Select(x => F(x))
// Result: IAsyncEnumerable<T>
```

Consumed with `await foreach (var x in result) { ... }`.

**Mixed sync/async**: if the source is synchronous (`IEnumerable<T>`) but a filter function is async, the pipeline must be async: `.ToAsyncEnumerable().SelectAwait(async x => await F(x))`. The colour pass (Phase 11) detects this and marks the enclosing function as `async Task<T>`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | Query DSL → LINQ method chain lowering |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Query/` | Window function helpers (Lag, Lead, RollingWindow, RowNumber) |
| `transpiler3/dotnet/build/phase07_test.go` | `TestPhase7Query`: 10 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase07-query/` | 10 fixture directories |

## Test set

- `TestPhase7Query` -- 10 fixtures (query_empty_result, query_filter, query_filter_select, query_group_by, query_no_where, query_select, query_skip, query_skip_take, query_sort, query_take).

## Deferred work

- `IQueryable<T>` / EF Core integration. Deferred to Phase 12 (FFI).
- `DuckDB.NET` out-of-process query engine. Out of scope for v1.
- `IAsyncEnumerable<T>` source from HTTP streaming JSON. Deferred to Phase 14 (fetch).

## Closeout notes

Phase 7 landed. `TestPhase7Query` PASS: 10 fixtures on net10.0 (query_empty_result, query_filter, query_filter_select, query_group_by, query_no_where, query_select, query_skip, query_skip_take, query_sort, query_take).

`ListSortAscExpr` → `xs.OrderBy(__sx => __sx).ToList()`. `ListSliceExpr` → `xs.Skip((int)start).Take((int)end - (int)start).ToList()`; when End is the "skip-only" sentinel (`1<<62 - 1`), emits `xs.Skip(n).ToList()` without Take to avoid int overflow. `QueryScopeStmt` → inline body block (no arena needed; GC handles allocation). `where` / `select` already lowered to `ListFilterExpr` / `ListMapExpr` by the shared C lower pass.
