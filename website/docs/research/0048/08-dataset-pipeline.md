# MEP-48 research note 08, Dataset pipeline lowering

Status: research note feeding MEP-48 (Mochi to .NET / CLR transpiler).
Date: 2026-05-23 (GMT+7).
Sibling cross-ref: [[../0047/08-dataset-pipeline]] (JVM target).

This note covers how Mochi's query DSL (`from x in xs where ... group by ... order by ... select ...`) lowers to .NET. The short version is, .NET is the easy target. C# 3.0 shipped LINQ in 2007 with precisely this shape, and the standard library has been the polished reference implementation of integrated query syntax for nearly two decades. The lowering is one of the most natural in all of MEP-48.

## 1. LINQ as the canonical lowering target

Mochi's surface syntax,

```mochi
from x in xs
where p(x)
select f(x)
```

is essentially identical to C#'s query expression syntax. The compiler still desugars C# query syntax to method calls (`Where`, `Select`, etc.) on `IEnumerable<T>`, and Mochi's CLR backend skips the query-syntax step entirely and emits the method-chain form directly:

```csharp
xs.Where(x => p(x)).Select(x => f(x))
```

We prefer the method-chain form for three reasons.

1. Debugger stack traces show real method names (`Enumerable.Where`, `Enumerable.Select`) rather than compiler-synthesised query helpers. Stepping through is materially nicer.
2. Profiler output lines up with documented API names, so the user can search Microsoft Learn directly.
3. The method-chain form composes more cleanly when we splice in non-LINQ helpers like `MochiRuntime.Query.Window.Lag(...)`.

Query expression syntax is still legal C# and still compiles to the same IL, but Mochi codegen will not emit it. This matches the convention used by EF Core's own generated code and by ASP.NET internal logs.

## 2. LINQ to Objects (`System.Linq.Enumerable`)

`System.Linq.Enumerable` is the primary lowering destination. It provides extension methods on `IEnumerable<T>` for the full standard query operator set, all with deferred execution.

The full .NET 10 surface includes (alphabetical, abridged): `Aggregate`, `All`, `Any`, `Append`, `AsEnumerable`, `Average`, `Cast`, `Chunk`, `Concat`, `Contains`, `Count`, `DefaultIfEmpty`, `Distinct`, `DistinctBy`, `ElementAt`, `ElementAtOrDefault`, `Empty`, `Except`, `ExceptBy`, `First`, `FirstOrDefault`, `GroupBy`, `GroupJoin`, `Intersect`, `IntersectBy`, `Join`, `Last`, `LastOrDefault`, `LongCount`, `Max`, `MaxBy`, `Min`, `MinBy`, `OfType`, `Order`, `OrderBy`, `OrderByDescending`, `OrderDescending`, `Prepend`, `Range`, `Repeat`, `Reverse`, `Select`, `SelectMany`, `SequenceEqual`, `Single`, `SingleOrDefault`, `Skip`, `SkipLast`, `SkipWhile`, `Sum`, `Take`, `TakeLast`, `TakeWhile`, `ThenBy`, `ThenByDescending`, `ToArray`, `ToDictionary`, `ToHashSet`, `ToList`, `ToLookup`, `TryGetNonEnumeratedCount`, `Union`, `UnionBy`, `Where`, `Zip`.

Deferred execution semantics. The Mochi lowering forces materialisation at the end of every pipeline (see section 18) because Mochi has eager dataset semantics, but the intermediate stages of the chain remain lazy. That gives us the right balance, intermediate iterators stay fusion-friendly under the JIT while the eventual observable result matches vm3.

## 3. PLINQ (`System.Linq.ParallelEnumerable`)

PLINQ is the parallel implementation of LINQ to Objects. The conversion is a single extension method, `.AsParallel()`, which binds the subsequent operator chain to the `ParallelEnumerable` implementations.

```csharp
var evens = xs.AsParallel()
              .Where(x => x % 2 == 0)
              .Select(x => x * x);
```

Internally PLINQ partitions the source sequence into chunks, dispatches them onto the ThreadPool, and merges results. The partitioning strategy is chosen by the runtime, range partitioner for indexed sources, chunk partitioner for streams, hash partitioner for join keys, etc.

Mochi opts in to PLINQ via the `.parallel` qualifier:

```mochi
from x in xs.parallel
where heavy(x)
select transform(x)
```

lowers to

```csharp
xs.AsParallel().Where(x => heavy(x)).Select(x => transform(x)).ToList()
```

Default is sequential. We do not auto-parallelise, the same way vm3 does not auto-parallelise. The user opts in explicitly because:

1. Parallelism is only a win when the per-element work exceeds the synchronisation overhead. PLINQ is conservative by default but the heuristic is still wrong sometimes.
2. Ordering may not be preserved unless the user adds `.AsOrdered()`. Mochi queries are ordered by default, so we have to emit `.AsOrdered()` after `.AsParallel()` whenever the original sequence is order-sensitive, and only drop it when the user writes an explicit `.unordered` hint.
3. PLINQ can rethrow exceptions wrapped in `AggregateException`, which would surprise users coming from vm3.

For tuning, the lowering exposes `.WithDegreeOfParallelism(n)` and `.WithCancellation(token)` via runtime helpers when the user asks for them through dataset hints (`xs.parallel(threads = 4)`).

## 4. `IAsyncEnumerable<T>` and `System.Linq.AsyncEnumerable`

Async pipelines lower to `IAsyncEnumerable<T>` consumed by `await foreach`.

Important .NET 10 detail. The community `System.Linq.Async` NuGet package is deprecated. As of .NET 10, Microsoft ships `System.Linq.AsyncEnumerable` (10.0.x) as part of the runtime libraries, providing the full LINQ operator surface on `IAsyncEnumerable<T>`. The new class is `System.Linq.AsyncEnumerable` (singular). The `System.Linq.AsyncEnumerable` package also works on older runtimes (down to netstandard2.0), so we can target it uniformly. Mochi's CLR runtime depends on `System.Linq.AsyncEnumerable >= 10.0.0`.

Mochi async query syntax:

```mochi
async from row in fetchRows()
where row.active
select row.id
```

lowers to:

```csharp
fetchRows()
    .Where(row => row.active)
    .Select(row => row.id)
    .ToListAsync(cancellationToken)
```

Where `fetchRows()` returns `IAsyncEnumerable<Row>`. The `Where`/`Select` overloads in `System.Linq.AsyncEnumerable` take synchronous lambdas just like the sync version. For lambdas that need to await, the user writes a body that returns `ValueTask<T>` and the overload resolution picks the async variant. The old `SelectAwait` / `WhereAwait` naming from `System.Linq.Async` 6.x is gone in the new package, the unified `Select` and `Where` accept either lambda shape.

## 5. Join operators

Inner join:

```mochi
from o in orders
join c in customers on o.cid equals c.id
select (o, c)
```

lowers to:

```csharp
orders.Join(customers,
            o => o.cid,
            c => c.id,
            (o, c) => (o, c))
      .ToList()
```

The four arguments to `Enumerable.Join` are outer source, inner key selector for outer, inner key selector for inner, and result selector. Internally LINQ to Objects builds a `Lookup<TKey, TInner>` from the inner sequence (a hash multimap) and probes it as it iterates the outer sequence, that is a hash join.

Left outer join via `GroupJoin` + `SelectMany` + `DefaultIfEmpty`:

```mochi
from o in orders
left join c in customers on o.cid equals c.id
select (o, c)
```

lowers to:

```csharp
orders.GroupJoin(customers,
                 o => o.cid,
                 c => c.id,
                 (o, cs) => new { o, cs })
      .SelectMany(x => x.cs.DefaultIfEmpty(),
                  (x, c) => (x.o, c))
      .ToList()
```

`GroupJoin` is the LINQ primitive that gives you, for each outer element, the (possibly empty) set of inner elements matching its key. Combining it with `SelectMany` + `DefaultIfEmpty` is the canonical left-outer-join pattern documented across the LINQ literature.

Right outer join. Lowered by swapping arguments (right outer with A and B is the same as left outer with B and A and reversed selector). Mochi codegen does this swap during the lowering pass.

Full outer join. Not native to LINQ. Mochi runtime provides `Mochi.Runtime.Query.FullOuterJoin<TLeft, TRight, TKey, TResult>(IEnumerable<TLeft>, IEnumerable<TRight>, ...)` which builds two `Lookup` indexes and walks the key union.

Cross join. Lowers to `xs.SelectMany(_ => ys, (x, y) => (x, y))`. Pure cartesian product, no key.

## 6. GroupBy

```mochi
from o in orders
group by o.cid into g
select (cid: g.key, total: sum(g.amount))
```

lowers to:

```csharp
orders.GroupBy(o => o.cid)
      .Select(g => new { cid = g.Key, total = g.Sum(o => o.amount) })
      .ToList()
```

`Enumerable.GroupBy` returns `IEnumerable<IGrouping<TKey, TElement>>`, where `IGrouping<TKey, TElement>` extends `IEnumerable<TElement>` and exposes a `Key` property. The grouping is buffered (you cannot stream a hash-based group-by, it has to see all inputs before emitting any groups), so this stage forces materialisation under the hood.

Compound key. Mochi `group by (o.year, o.region)` lowers to a value-tuple key:

```csharp
orders.GroupBy(o => (o.year, o.region))
      .Select(g => new { year = g.Key.Item1,
                         region = g.Key.Item2,
                         count = g.Count(),
                         total = g.Sum(o => o.amount) })
```

Value tuples in C# implement `IEquatable<T>` and `GetHashCode` structurally, so they work as `GroupBy` keys with no extra setup.

## 7. OrderBy and ThenBy

```mochi
order by k desc, k2 asc
```

lowers to:

```csharp
xs.OrderByDescending(x => x.k).ThenBy(x => x.k2)
```

OrderBy and ThenBy return `IOrderedEnumerable<T>`, a subtype of `IEnumerable<T>` that supports stacking subordinate orderings. The actual sort is delayed until the first enumeration. Internally `OrderBy` uses an introspective sort (quicksort with heapsort fallback) on a buffered copy of the input, stable across keys.

Descending suffix maps to `OrderByDescending` / `ThenByDescending`. Mochi's `order by` clause is order-preserving on equal keys (stable), which matches LINQ's documented behaviour.

## 8. Window operations

LINQ has no native window functions (no `LAG`, `LEAD`, `ROW_NUMBER`, no cumulative sum). The Mochi runtime fills the gap in `Mochi.Runtime.Query.Window`:

- `Lag<T>(IEnumerable<T> src, int n, T defaultValue)`
- `Lead<T>(IEnumerable<T> src, int n, T defaultValue)`
- `CumulativeSum<T>(IEnumerable<T> src)` where `T : INumber<T>`
- `RowNumber<T>(IEnumerable<T> src)` returning `IEnumerable<(long index, T value)>`
- `RollingWindow<T>(IEnumerable<T> src, int size)` returning sliding `IReadOnlyList<T>` windows

These are implemented as straightforward iterator methods (`yield return`). For `Lag`/`Lead`, the zip-with-shifted-tail pattern is the natural implementation, `src.Zip(src.Skip(n), (a, b) => ...)`. Cumulative sum uses a running accumulator inside an iterator. The lowering for Mochi `cumsum x over xs` calls these helpers directly.

## 9. Hash join optimisation

`Enumerable.Join` internally uses a hash join. The implementation reads the inner sequence, builds a `Lookup<TKey, TInner>` (essentially `Dictionary<TKey, List<TInner>>`), then streams the outer sequence and probes the lookup. That is the textbook hash join, sequential, build side on the inner, probe side on the outer.

PLINQ's `Join` operator runs a partitioned hash join. Each partition gets its own probe-side iterator but shares the build-side lookup once it is constructed.

For joins that LINQ does not provide natively (full outer, anti-join, semi-join with a custom merge), the Mochi runtime exposes `Mochi.Runtime.Query.HashJoin<TLeft, TRight, TKey, TResult>` with parameters for both key selectors, both default-element strategies (none / use-default-for-missing), and a result selector. This is what Mochi's compiler reaches for when it sees join variants that do not map cleanly to `Join` or `GroupJoin`.

```csharp
public static IEnumerable<TResult> HashJoin<TLeft, TRight, TKey, TResult>(
    IEnumerable<TLeft> left,
    IEnumerable<TRight> right,
    Func<TLeft, TKey> leftKey,
    Func<TRight, TKey> rightKey,
    Func<TLeft, TRight, TResult> resultSelector,
    JoinKind kind,
    IEqualityComparer<TKey>? comparer = null);
```

Where `JoinKind` is `Inner | LeftOuter | RightOuter | FullOuter | LeftSemi | LeftAnti`.

## 10. Aggregation collectors

LINQ has built-in specialised aggregators:

- `Sum`, `Average`, `Min`, `Max`, `Count`, `LongCount`
- `MinBy`, `MaxBy` (since .NET 6)
- `Aggregate(seed, accumulator)` and `Aggregate(seed, accumulator, resultSelector)` for arbitrary folds

Mochi's `reduce` operator lowers to `Aggregate`. Mochi `sum`, `count`, `avg`, `min`, `max` lower to the dedicated specialisations because the JIT recognises them and emits tight loops.

Custom collectors. Mochi `collect into MyAcc` lowers to:

```csharp
xs.Aggregate(MyAcc.Empty,
             (acc, x) => acc.Combine(x),
             acc => acc.Result())
```

The runtime ships a small collector library, `Mochi.Runtime.Query.Collectors`, with helpers for histogram, top-k, percentile, and reservoir sampling. None are LINQ primitives, all are `Aggregate` underneath.

## 11. Primitive specialisation

This is where .NET sweeps the floor with the JVM. The CLR has true generics, no type erasure. `List<long>` is genuinely a list of 64-bit values laid out contiguously, no boxing. LINQ over `List<long>` keeps the values unboxed throughout the pipeline.

Compare with the JVM, where `List<Long>` stores `Object` references to boxed `java.lang.Long` heap objects, and every operator chain has to unbox before computing and rebox to pass along.

Practical consequences for Mochi:

1. `from x in xs select x + 1` over a `List<long>` is a tight loop of native int64 arithmetic in IL.
2. `Sum`, `Average`, etc. on numeric primitives go through specialised overloads (`Enumerable.Sum(IEnumerable<long>)` etc.), no virtual dispatch on the accumulator.
3. `long[]` is preferred over `List<long>` for hot loops because `Enumerable` has fast paths for arrays (devirtualised in .NET 10 thanks to runtime changes that let the JIT devirtualise interface methods on array types).

The Mochi codegen prefers `long[]` for read-only datasets known to be immutable and falls back to `List<long>` when mutation or unknown size matters.

## 12. Span-based loops

Where LINQ overhead matters (delegate dispatch, iterator state machine), the lowering can bypass LINQ entirely and emit a `foreach` over `Span<T>` or `ReadOnlySpan<T>`.

```mochi
@hint(loop = "span")
from x in xs
where x > 0
select x * 2
```

lowers to:

```csharp
var result = new List<long>();
foreach (var x in xs.AsSpan())
{
    if (x > 0) result.Add(x * 2);
}
return result;
```

This trades the deferred-execution composability of LINQ for raw loop speed. The compiler decides based on the `@hint(loop = "span")` directive, or automatically when the entire pipeline is simple (filter + map + optional fold) and the source is array-backed.

Span loops require a known concrete array or `List<T>` source. Mochi's type inference threads enough static information through the dataset pipeline to know when this is safe.

## 13. DuckDB.NET integration

For OLAP-scale queries (millions of rows, complex aggregations), the Mochi runtime can route queries to DuckDB via the DuckDB.NET ADO.NET provider. DuckDB.NET is the official .NET binding for the embedded analytical database DuckDB, distributed as `DuckDB.NET.Data.Full` (with bundled native library) and `DuckDB.NET.Data` (managed-only).

DuckDB gives us columnar vectorised execution, predicate pushdown, hash join, and aggregation that beats any in-process LINQ pipeline at scale.

Out of scope for MEP-48 v1. Tracking as a Phase-2 sub-MEP, the routing pass would inspect the dataset pipeline, decide if it is pushdown-eligible (no closures over outer variables, no opaque user functions, only standard query operators), translate to SQL, and execute against an in-memory DuckDB connection. Out of scope means we do not block v1 on it, not that it is impossible.

## 14. Apache Arrow .NET

The `Apache.Arrow` NuGet package provides .NET bindings for Apache Arrow, the columnar in-memory format. Useful for zero-copy interop with Python, R, and DuckDB.

Out of scope for MEP-48 v1. Same reasoning as DuckDB.NET. Filed as a tracking idea.

## 15. CSV reading

`CsvHelper` (current 33.x, MIT-licensed) is the de-facto CSV library on .NET. The Mochi runtime wraps it in `Mochi.Runtime.Csv.ReadCsv<T>(path, options)` which returns `IEnumerable<T>` for streaming reads and `IAsyncEnumerable<T>` for async reads.

```csharp
public static IEnumerable<T> ReadCsv<T>(
    string path,
    CsvOptions? options = null);
```

CsvHelper supports custom mapping (`ClassMap<T>`), type converters, headers, quoted fields, and so on. Mochi's runtime delegates configuration through.

CSV writing similarly through `CsvHelper.Writer`, wrapped as `Mochi.Runtime.Csv.WriteCsv<T>(path, IEnumerable<T> rows, options)`.

## 16. Parquet

`Parquet.Net` (fully managed) and `ParquetSharp` (G-Research's native-backed binding) are the two notable Parquet libraries. ParquetSharp is faster on read-heavy workloads (it wraps the apache-parquet-cpp library directly) and supports Arrow interop. Parquet.Net is pure .NET, lighter dependency, and good enough for most workloads.

Out of scope for MEP-48 v1. Will be added as a Mochi.Runtime.Parquet helper module post-v1 if there is demand. The codegen will not emit Parquet calls.

## 17. EF Core and Dapper

Explicitly NOT used by the Mochi runtime. EF Core (Entity Framework Core) is a heavyweight ORM that translates LINQ to SQL via `IQueryable` expression trees, that is not Mochi's job. Dapper is a micro-ORM that wraps ADO.NET, also not Mochi's job.

Users are free to import these via FFI / direct package references in their Mochi project, but the Mochi runtime has zero hard dependency. The CLR codegen never emits `EntityFrameworkCore` namespaces or `Dapper.Sql*` calls.

The reason matters. EF Core's `IQueryable` path has expression-tree limits, no closures, no captured locals, restricted syntax. If Mochi lowered to `IQueryable` automatically we would have to enforce those restrictions in the type checker, which is not work we want to take on for v1.

## 18. Lazy versus eager evaluation

LINQ to Objects is lazy by default, queries are deferred execution iterators that fire on enumeration. PLINQ is mostly lazy, with eager exceptions for ordering and grouping. `System.Linq.AsyncEnumerable` is lazy.

Mochi queries are eager by default. To bridge the gap, the lowering pass inserts `.ToList()` (or `.ToListAsync(token)` for async) at the end of every query chain that is bound to a value or returned from a function.

```mochi
let result = from x in xs where x > 0 select x * 2
```

lowers to:

```csharp
var result = xs.Where(x => x > 0).Select(x => x * 2).ToList();
```

Exception, if the result flows directly into a `foreach`, we can keep it lazy and rely on the consumer to drive enumeration:

```mochi
for row in (from x in xs where x > 0 select x * 2) {
    print(row)
}
```

lowers to:

```csharp
foreach (var row in xs.Where(x => x > 0).Select(x => x * 2)) {
    Console.WriteLine(row);
}
```

This preserves vm3 semantics (eager when materialised, lazy when streamed) while avoiding unnecessary buffer allocations.

## 19. Distinct versus DistinctBy

C# 6+ supports both `Distinct` (whole-element equality) and `DistinctBy` (key-based, added in .NET 6). Mochi `distinct` lowers to `Distinct()`, Mochi `distinct by k` lowers to `DistinctBy(x => x.k)`.

```mochi
from x in xs distinct by x.email select x
```

lowers to:

```csharp
xs.DistinctBy(x => x.email).ToList()
```

Same pattern for `MaxBy`, `MinBy`, `ExceptBy`, `IntersectBy`, `UnionBy`, all introduced in .NET 6 and stable through .NET 10. The lowering uses them where Mochi syntax matches.

## 20. SelectMany

Mochi nested `from` clauses lower to `SelectMany`:

```mochi
from x in xs
from y in x.items
select (x, y)
```

lowers to:

```csharp
xs.SelectMany(x => x.items, (x, y) => (x, y)).ToList()
```

The two-argument overload of `SelectMany` keeps the outer element in scope for the result selector, which matches Mochi's lexical scoping rule (`x` is still visible after the second `from`).

This is also the lowering for Mochi `flatMap`:

```mochi
xs.flatMap(x -> x.items)
```

becomes

```csharp
xs.SelectMany(x => x.items)
```

In LINQ query expression syntax, every `from` clause after the first compiles to a `SelectMany`. We emit method-chain form, but the semantics are identical.

## 21. Datalog over LINQ

Mochi ships a small Datalog engine in `Mochi.Runtime.Datalog`. The semi-naive evaluator uses LINQ pipelines for rule bodies and `Aggregate` for head materialisation.

A rule like `path(x, z) <- edge(x, y), path(y, z)` lowers conceptually to:

```csharp
var step = edge.Join(path,
                     e => e.to,
                     p => p.from,
                     (e, p) => new Path(e.from, p.to));
delta = step.Except(path);  // new facts only
path = path.Union(delta);
```

The Datalog engine is implemented as plain LINQ over `HashSet<Fact>` and `IEnumerable<Fact>`. Each rule body is a LINQ pipeline, each fixpoint iteration computes the delta via `Except` and unions it back. This stays portable across .NET 10 LTS and any runtime that supports modern LINQ.

The Datalog engine is fully in scope for v1.

## 22. Cross-reference siblings

- `[[01-runtime-bridge]]` for how `Mochi.Runtime.Query.*` is wired into the CLR runtime.
- `[[02-types-and-ir]]` for how Mochi's dataset types lower to `IEnumerable<T>` and value tuples.
- `[[06-strings-numerics]]` for primitive specialisation context.
- `[[07-async-and-await]]` for the async pipeline plumbing that section 4 above relies on.
- `[[../0047/08-dataset-pipeline]]` sibling note for the JVM target (Stream API and parallel streams).

## Code sample matrix

| Mochi | Emitted C# |
|---|---|
| `from x in xs select x + 1` | `xs.Select(x => x + 1).ToList()` |
| `from x in xs where x > 0 select x` | `xs.Where(x => x > 0).ToList()` |
| `from x in xs.parallel where heavy(x) select x` | `xs.AsParallel().Where(x => heavy(x)).ToList()` |
| `from x in xs from y in x.items select (x, y)` | `xs.SelectMany(x => x.items, (x, y) => (x, y)).ToList()` |
| `from o in os join c in cs on o.cid equals c.id select (o, c)` | `os.Join(cs, o => o.cid, c => c.id, (o, c) => (o, c)).ToList()` |
| `from o in os group by o.cid into g select (g.key, sum(g.amt))` | `os.GroupBy(o => o.cid).Select(g => (g.Key, g.Sum(o => o.amt))).ToList()` |
| `from x in xs order by x.k desc, x.k2 asc select x` | `xs.OrderByDescending(x => x.k).ThenBy(x => x.k2).ToList()` |
| `from x in xs distinct by x.email select x` | `xs.DistinctBy(x => x.email).ToList()` |
| `xs.reduce(0, (acc, x) -> acc + x)` | `xs.Aggregate(0L, (acc, x) => acc + x)` |
| `async from r in fetchRows() where r.ok select r` | `fetchRows().Where(r => r.ok).ToListAsync(ct)` |

## Performance section

Expected benchmarks on .NET 10 LTS, comparing to the JVM target and to vm3.

### Numeric pipeline (List of long)

Pipeline: `from x in xs where x > 0 select x * 2`, sum reduction, 10 million elements.

| Runtime | Time | Notes |
|---|---|---|
| .NET 10 LTS, `List<long>` | baseline 1.0x | no boxing, JIT inlines `Where` and `Select` after .NET 10 devirtualisation work |
| .NET 10 LTS, `long[]` | ~0.85x | array fast paths in Enumerable, devirtualised in .NET 10 |
| JVM 21, `List<Long>` stream | ~1.4-1.8x slower | every element boxes through `java.lang.Long`, escape analysis sometimes helps |
| JVM 21, `LongStream` | ~1.0-1.1x | LongStream avoids boxing, comparable to .NET when arithmetic dominates |
| Mochi vm3 | ~0.9-1.1x | hand-rolled interpreter, tight loops, comparable for simple pipelines |
| Mochi vm3 + bytecode peephole | ~0.8x | when the optimiser fires on the whole pipeline |

Projection. On long-heavy numeric pipelines, .NET 10 should be 1.2-2x faster than the JVM with default `Stream<Long>`. With `LongStream` on the JVM, the gap closes substantially, but the JVM specialisation is per-primitive (LongStream, IntStream, DoubleStream) and only exists for these three types, whereas the CLR specialises automatically for every value type. For double, decimal, and user-defined `struct` types the .NET gap widens further.

### Join pipeline

Pipeline: inner join between two 1M-row sequences on a single int key, project result, sum.

| Runtime | Time |
|---|---|
| .NET 10 LTS Join | baseline 1.0x |
| .NET 10 LTS PLINQ Join (4 cores) | ~0.35x |
| JVM 21 Stream merge | ~1.2x slower |
| JVM 21 parallel stream | ~0.4x |
| Mochi vm3 hash join | ~1.0x sequential |

Both PLINQ and parallel streams scale linearly to physical core count for hash join, with similar partition overhead. Sequential, .NET edges JVM modestly because the LINQ `Lookup` building is unboxed for primitive keys.

### Group-by aggregation

Pipeline: group by string key, count + sum per group, 5M rows, 50K distinct keys.

| Runtime | Time |
|---|---|
| .NET 10 LTS GroupBy | baseline 1.0x |
| JVM 21 Collectors.groupingBy | ~1.05-1.15x |
| Mochi vm3 | ~1.0x |
| DuckDB.NET embedded | ~0.3x (out of scope for v1) |

Group-by is dominated by hash table operations. Both runtimes are mature here, the gap is small. DuckDB would be a step change but is out of scope.

### Async pipeline

`IAsyncEnumerable<T>` over a network source with 100 ms per-batch latency, 1000 batches, transform and accumulate.

| Runtime | Time |
|---|---|
| .NET 10 LTS AsyncEnumerable.* | baseline 1.0x |
| JVM 21 Project Reactor Flux | ~1.0-1.05x |
| Mochi vm3 (sync) | ~10x slower (no async pipeline) |

Async pipelines are I/O-bound, both runtimes are equivalent. Mochi vm3 has no first-class async support, so this is the place where the CLR backend opens a clear gap.

### Summary

For long-heavy numeric pipelines, expect .NET 10 LTS to be 1.2-2x faster than the JVM default and comparable to or modestly faster than vm3. For join and group-by, all three are within noise of each other on sequential code, with PLINQ and parallel streams scaling similarly. For async, the CLR is a major win over vm3 and a wash with the JVM. The CLR's true generics are the foundational reason, primitive collections stay primitive throughout the pipeline with no boxing wrapper.

## Sources

- [Enumerable Class (System.Linq), Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable?view=net-10.0)
- [Enumerable.Select Method, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.select?view=net-10.0)
- [Enumerable.Where Method, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.where?view=net-10.0)
- [Enumerable.SelectMany Method, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.selectmany?view=net-10.0)
- [Enumerable.GroupBy Method, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.groupby?view=net-10.0)
- [Introduction to PLINQ, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/introduction-to-plinq)
- [ParallelEnumerable.AsParallel Method, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.parallelenumerable.asparallel?view=net-10.0)
- [How to: Create and Execute a Simple PLINQ Query, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/how-to-create-and-execute-a-simple-plinq-query)
- [Breaking change, System.Linq.AsyncEnumerable in .NET 10](https://learn.microsoft.com/en-us/dotnet/core/compatibility/core-libraries/10.0/asyncenumerable)
- [NuGet Gallery, System.Linq.AsyncEnumerable 10.0.7](https://www.nuget.org/packages/System.Linq.AsyncEnumerable/)
- [NuGet Gallery, System.Linq.Async 7.0.1 (deprecated)](https://www.nuget.org/packages/System.Linq.Async)
- [Grouping Data, C#, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/csharp/linq/standard-query-operators/grouping-data)
- [NuGet Gallery, CsvHelper 33.1.0](https://www.nuget.org/packages/CsvHelper/)
- [CsvHelper documentation](https://joshclose.github.io/CsvHelper/)
- [DuckDB.NET, Getting Started](https://duckdb.net/docs/getting-started.html)
- [ParquetSharp, G-Research GitHub](https://github.com/G-Research/ParquetSharp)
- [DuckDB and Apache Arrow integration](https://arrow.apache.org/blog/2021/12/03/arrow-duckdb/)
- [Performance Improvements in .NET 10, Microsoft DevBlogs](https://devblogs.microsoft.com/dotnet/performance-improvements-in-net-10/)
- [Boxing in C#, NDepend Blog](https://blog.ndepend.com/boxing-in-c-what-it-costs-you-and-how-to-get-rid-of-it/)
- [LinqBenchmarks, NetFabric GitHub](https://github.com/NetFabric/LinqBenchmarks)
- [.NET 9 LINQ Performance Edition, Steven Giesel](https://steven-giesel.com/blogPost/783a404a-e39e-480f-bc99-a514a75d752d)
