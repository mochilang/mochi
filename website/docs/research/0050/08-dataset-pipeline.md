# MEP-50 research note 08, Mochi query DSL and dataset pipeline on Kotlin

Author: research pass for MEP-50.
Date: 2026-05-23 11:35 (GMT+7).

This note covers Mochi's LINQ-style query DSL (`from ... in ... where ... select ... group by ... order by ... limit ...`), join strategies, aggregates, set ops, streaming queries, and datalog evaluation, and how each lowers onto Kotlin 2.1 plus `kotlinx.coroutines.flow`. Companion notes: the shared-decisions anchor, [[05-codegen-design]], [[06-type-lowering]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]].

The Kotlin target leans on the stdlib's `Sequence<T>` for finite, synchronous collections and on `kotlinx.coroutines.flow.Flow<T>` for asynchronous streams. We supplement with a small runtime helper module (`MochiRuntime.Query`) that implements joins, groupings, and aggregations that the stdlib does not. We do **not** depend on Spark, Arrow, or any heavyweight dataset toolkit at the Mochi-runtime layer; user-level integrations are documented but not bundled.

We assume Kotlin 2.1 as the floor with strict concurrency (the `kotlin.experimental.warningsAsErrors=true` Gradle option) and `kotlinx-coroutines-core` 1.10.1+ as the coroutines library.

---

## 1. Mochi query surface recap

Mochi inherits a LINQ-style query DSL ([[01-language-surface]] §3). Examples:

```mochi
let adults     = from x in xs where x.age > 18 select x.name
let by_dept    = from p in people group by p.dept into g select { dept: g.key, n: count(g) }
let joined     = from o in orders join u in users on o.user_id == u.id select { u.name, o.amount }
let top10      = from p in people order by p.score desc limit 10 select p.name
let outer_left = from a in xs left join b in ys on a.id == b.aid select { a, b }
let cross      = from a in xs cross join b in ys select { a, b }
```

Surface clauses supported per the language docs:

- `from x in coll` (single-source iteration).
- `join y in coll2 on x.k == y.k` (inner equi-join; default hash, merge when sorted, nested-loop fallback for non-hashable keys).
- `left join`, `right join`, `cross join`.
- `group by key into g` with optional `having pred`.
- `where pred` (filter, can precede or follow a join).
- `order by k1, k2 desc` (stable sort, ascending by default).
- `limit N`, `offset M`, paging window.
- `select expr` (terminal projection).
- Aggregate functions in the select: `count`, `sum`, `avg`, `min`, `max`.

All clauses lower to Kotlin code. The fundamental decision in the lowering: **Sequence vs Flow**, decided per query by the source type.

## 2. Sequence vs Flow: the two-tier choice

Mochi queries split cleanly along the sync/async axis:

- A query whose source is a `list<T>` (a materialised in-memory collection) lowers to Kotlin `Sequence<T>`. Every intermediate op is synchronous and stays inside the calling function. Materialisation to `List<T>` is via `.toList()`.
- A query whose source is a `stream<T>` (defined in [[09-agent-streams]]) lowers to Kotlin `Flow<T>`. Every intermediate op is `suspend`, the chain is collected via `flow.collect { ... }` or `flow.toList()` (the latter is a suspend function that buffers everything; gate it on bounded streams).

The optimiser picks the variant by tracing the source type through the Mochi type checker. There is no implicit upgrade. A query that joins a `list` with a `stream` is rejected at type-check time; the user must either `.collect` the stream into a buffer first or wrap the list as a one-shot Flow via `flowOf(*list.toTypedArray())`.

`Sequence<T>` shape:

```kotlin
val adults: List<String> = xs.asSequence()
    .filter { it.age > 18 }
    .map { it.name }
    .toList()
```

`Flow<T>` shape:

```kotlin
val adultsFlow: Flow<String> = stream
    .filter { it.age > 18 }
    .map { it.name }

adultsFlow.collect { name -> println(name) }
```

Both protocols are part of the standard stack as of Kotlin 2.1 / coroutines 1.10. `Sequence<T>` is in `kotlin.sequences`, has been stable since Kotlin 1.0, and ships in every Kotlin target including K/Native and K/JS. `Flow<T>` is in `kotlinx.coroutines.flow`, has been stable since coroutines 1.3 (2019), and ships everywhere kotlinx.coroutines does (every Kotlin target we care about).

The Mochi codegen for the **same Mochi source** can lower to either tier depending on the input type. The IR is identical; only the final emit pass switches between `Sequence` operators and `Flow` operators.

## 3. From / select / where on Sequence

Three of the most basic clauses, lowered straightforwardly:

```mochi
let adults = from x in xs where x.age > 18 select x.name
```

```kotlin
val adults: List<String> = xs.asSequence()
    .filter { it.age > 18 }
    .map { it.name }
    .toList()
```

The Mochi codegen always emits `.asSequence()` at the head when the source is a `List<T>`, so intermediate transformations stay lazy. The pipeline materialises only at the `.toList()` terminal.

For a single-pass consumer (e.g. `for x in (from x in xs where ... select ...)`), the codegen emits the chain without the final `.toList()`:

```kotlin
for (name in xs.asSequence().filter { it.age > 18 }.map { it.name }) {
    println(name)
}
```

This avoids allocating an intermediate `List<String>` purely to iterate it once.

For a chain that has *no* lazy stages (just a single `select`), Kotlin's eager `.map` on `List<T>` is slightly faster (no Sequence wrapper allocation). The codegen detects single-stage chains and emits the eager form:

```mochi
let names = from x in xs select x.name
```

```kotlin
val names: List<String> = xs.map { it.name }
```

The same rule applies to `.filter` if there is no downstream transformation.

## 4. From / select / where on Flow

```mochi
let alerts = from e in event_stream where e.severity == "high" select e.message
```

```kotlin
val alerts: Flow<String> = event_stream
    .filter { it.severity == "high" }
    .map { it.message }
```

`Flow.filter` and `Flow.map` are suspend operators that return new `Flow<T>` instances. The chain is lazy: nothing runs until a terminal operator like `.collect`, `.first`, `.toList`, or `.fold`.

Mochi never emits `.toList()` on an unbounded Flow (the type checker tracks boundedness). For bounded Flows (e.g. one produced by `flow { for (i in 1..100) emit(i) }`), `.toList()` is allowed and produces a `List<T>`.

The Flow lowering picks up backpressure for free: each upstream emit suspends until the downstream collector is ready. See [[09-agent-streams]] §13 for backpressure details.

## 5. Order by

`order by` lowers to Kotlin's `sortedBy` / `sortedByDescending`:

```mochi
let top = from p in people order by p.score desc select p.name
```

```kotlin
val top: List<String> = people.asSequence()
    .sortedByDescending { it.score }
    .map { it.name }
    .toList()
```

Multi-key sort lowers to `sortedWith` plus a `Comparator` chain:

```mochi
let sorted = from r in rows order by r.k1 asc, r.k2 desc select r
```

```kotlin
val sorted: List<Row> = rows.asSequence()
    .sortedWith(compareBy<Row> { it.k1 }.thenByDescending { it.k2 })
    .toList()
```

**Stability**: Kotlin's `sortedBy` and `sortedWith` use Java's `Arrays.sort` for `List<T>` and `mergeSort` for `Sequence`. On JVM, `Arrays.sort` for object arrays is **stable** (TimSort, guaranteed by the spec since Java 7). On K/Native, `mergeSort` is stable. On K/JS, Kotlin uses `Array.prototype.sort` which is **stable** since ES2019 (Node.js 12+, all modern browsers). On Kotlin/Wasm, the stdlib uses the same TimSort port as the JVM stdlib (compiled to Wasm). All targets give stable sort, matching Mochi's spec.

**On Flow**: there is no `Flow.sortedBy`. Sorting requires buffering. The codegen emits a `.toList().sortedBy { ... }.asFlow()` chain:

```mochi
let sorted = from e in stream order by e.ts
```

```kotlin
val sorted: Flow<Event> = flow {
    val buf = stream.toList()
    for (e in buf.sortedBy { it.ts }) emit(e)
}
```

This buffers the entire stream, so `order by` on an unbounded stream is rejected by the type checker. The user must add a windowing clause (see §17).

## 6. Limit and offset

```mochi
let page = from x in xs order by x.id limit 10 offset 20 select x
```

```kotlin
val page: List<Item> = xs.asSequence()
    .sortedBy { it.id }
    .drop(20)
    .take(10)
    .toList()
```

`take(N)` and `drop(N)` are stdlib `Sequence` ops. The order is canonical: `drop(offset)` then `take(limit)` matches the Mochi semantic (offset 20 means "skip the first 20", then "take the next 10").

For `Flow`:

```kotlin
val page: Flow<Item> = stream.drop(20).take(10)
```

`Flow.take(N)` cancels the upstream once N items have been emitted; `Flow.drop(N)` silently consumes the first N. Both are in `kotlinx.coroutines.flow`.

For very-large offsets, `drop` is O(N) (it just iterates and discards). Mochi documents this; users with random-access requirements (`offset 1000000`) should use an indexed data structure, not a query.

## 7. Group by

`group by` produces a `Map<K, List<V>>` keyed by the group expression, preserving insertion order.

```mochi
let by_dept = from p in people group by p.dept into g select { dept: g.key, n: count(g) }
```

```kotlin
val by_dept: List<DeptRow> = people.groupBy { it.dept }
    .map { (dept, members) -> DeptRow(dept = dept, n = members.size) }
```

**Insertion order preservation**: Kotlin's `Iterable.groupBy` implementation uses `LinkedHashMap<K, MutableList<V>>` internally (verified in `kotlin.collections.Maps.kt`); the result is a `LinkedHashMap` so iteration order matches first-occurrence-of-key. This matches Mochi's spec.

For aggregate-only group_by (no group list needed), the codegen fuses the aggregate into the map value:

```mochi
let counts = from p in people group by p.dept into g select { dept: g.key, n: count(g) }
```

```kotlin
val counts: List<DeptRow> = buildMap<String, Int> {
    for (p in people) {
        merge(p.dept, 1) { a, b -> a + b }
    }
}.map { (dept, n) -> DeptRow(dept = dept, n = n) }
```

`Map.merge` (since Kotlin 1.6, via java.util.Map.merge interop on JVM, with a stdlib shim on K/Native and K/JS) lets us increment without materialising the per-group list.

For a sum aggregate:

```mochi
let totals = from o in orders group by o.user_id into g
             select { user_id: g.key, total: sum(g |> map(o => o.amount)) }
```

```kotlin
val totals: List<TotalRow> = buildMap<Long, Long> {
    for (o in orders) {
        merge(o.user_id, o.amount) { a, b -> a + b }
    }
}.map { (user_id, total) -> TotalRow(user_id = user_id, total = total) }
```

For a multi-aggregate group_by (count + sum + avg), the codegen builds a small accumulator data class:

```mochi
let stats = from p in people group by p.dept into g select {
    dept: g.key, n: count(g), avg: avg(g |> map(p => p.salary))
}
```

```kotlin
data class DeptAcc(var sum: Long = 0L, var n: Int = 0)

val stats: List<StatsRow> = buildMap<String, DeptAcc> {
    for (p in people) {
        val acc = getOrPut(p.dept) { DeptAcc() }
        acc.sum += p.salary
        acc.n += 1
    }
}.map { (dept, acc) -> StatsRow(dept = dept, n = acc.n, avg = acc.sum.toDouble() / acc.n) }
```

**`having`** post-filters the entries:

```mochi
let popular = from p in people group by p.dept into g having count(g) > 5
              select { dept: g.key, n: count(g) }
```

```kotlin
val popular: List<DeptRow> = people.groupBy { it.dept }
    .filter { (_, members) -> members.size > 5 }
    .map { (dept, members) -> DeptRow(dept = dept, n = members.size) }
```

`Map.filter` preserves order; `Map.filterValues` is also fine for single-key predicates.

## 8. Joins, the build-and-probe pattern

The default equi-join lowering is **hash join**. Build a hash table on the smaller relation, probe with the larger.

```mochi
let joined = from o in orders join u in users on o.user_id == u.id
             select { name: u.name, amount: o.amount }
```

```kotlin
val idx: Map<Long, User> = users.associateBy { it.id }

val joined: List<Row> = orders.mapNotNull { o ->
    idx[o.user_id]?.let { u -> Row(name = u.name, amount = o.amount) }
}
```

`associateBy { it.id }` returns a `LinkedHashMap<Long, User>` keyed by id. For one-to-many joins:

```kotlin
val idx: Map<Long, List<User>> = users.groupBy { it.id }

val joined: List<Row> = orders.flatMap { o ->
    (idx[o.user_id] ?: emptyList()).map { u ->
        Row(name = u.name, amount = o.amount)
    }
}
```

Time complexity O(n + m); space O(min(n, m)) for the build side. The codegen picks the build side based on the declared row-count annotation or, when absent, the right-hand operand of `join`. The unique vs many decision flows from the join key's declared type (`@unique`) or from a side-table from the type checker.

Stdlib coverage: `associateBy` for unique, `groupBy` for many. Both return `LinkedHashMap`. No external dependency.

## 9. Joins, the runtime helper

For joins more complex than the trivial associate-and-mapNotNull (e.g. multi-column join keys, outer joins, or sorted inputs with merge join), Mochi ships `MochiRuntime.Query` with helper functions:

```kotlin
object Query {
    fun <L, R, K, O> hashJoin(
        left: Iterable<L>,
        right: Iterable<R>,
        leftKey: (L) -> K,
        rightKey: (R) -> K,
        combine: (L, R) -> O
    ): List<O> {
        val idx = HashMap<K, MutableList<R>>()
        for (r in right) {
            idx.getOrPut(rightKey(r)) { mutableListOf() }.add(r)
        }
        return left.flatMap { l ->
            (idx[leftKey(l)] ?: emptyList()).map { r -> combine(l, r) }
        }
    }

    fun <L, R, K, O> leftJoin(
        left: Iterable<L>,
        right: Iterable<R>,
        leftKey: (L) -> K,
        rightKey: (R) -> K,
        combine: (L, R?) -> O
    ): List<O> {
        val idx = HashMap<K, MutableList<R>>()
        for (r in right) {
            idx.getOrPut(rightKey(r)) { mutableListOf() }.add(r)
        }
        return left.flatMap { l ->
            val matches = idx[leftKey(l)]
            if (matches.isNullOrEmpty()) listOf(combine(l, null))
            else matches.map { r -> combine(l, r) }
        }
    }

    fun <L, R, K, O> rightJoin(
        left: Iterable<L>,
        right: Iterable<R>,
        leftKey: (L) -> K,
        rightKey: (R) -> K,
        combine: (L?, R) -> O
    ): List<O> = leftJoin(right, left, rightKey, leftKey) { r, l -> combine(l, r) }

    fun <L, R, O> crossJoin(
        left: Iterable<L>,
        right: Iterable<R>,
        combine: (L, R) -> O
    ): List<O> = left.flatMap { l -> right.map { r -> combine(l, r) } }
}
```

Mochi codegen for a left-join lowers to:

```kotlin
val joined = Query.leftJoin(
    left = orders, right = users,
    leftKey = { it.user_id }, rightKey = { it.id }
) { o, u -> Row(name = u?.name, amount = o.amount) }
```

The helper is generic and unboxed for primitive keys (Kotlin's reified generics specialise the call site under whole-module optimisation).

`Sequence` has no built-in `join`. Mochi's helpers operate on `Iterable<T>` which `Sequence<T>` extends; the helpers themselves return `List<O>` because joins require materialisation of at least one side.

For Sequence-friendly streaming joins (probe side as a Sequence), Mochi has a Sequence-returning variant:

```kotlin
fun <L, R, K, O> hashJoinSeq(
    left: Sequence<L>,
    right: Iterable<R>,
    leftKey: (L) -> K,
    rightKey: (R) -> K,
    combine: (L, R) -> O
): Sequence<O> = sequence {
    val idx = HashMap<K, MutableList<R>>()
    for (r in right) idx.getOrPut(rightKey(r)) { mutableListOf() }.add(r)
    for (l in left) {
        val matches = idx[leftKey(l)] ?: continue
        for (r in matches) yield(combine(l, r))
    }
}
```

The build phase materialises the right side; the probe phase is lazy. This is the standard streaming-join pattern.

## 10. Merge-join

When both inputs are sorted on the join key, merge-join is O(n + m) with O(1) space:

```kotlin
fun <L, R, K : Comparable<K>, O> mergeJoin(
    left: List<L>, right: List<R>,
    leftKey: (L) -> K, rightKey: (R) -> K,
    combine: (L, R) -> O
): List<O> {
    val out = mutableListOf<O>()
    var i = 0
    var j = 0
    while (i < left.size && j < right.size) {
        val lk = leftKey(left[i])
        val rk = rightKey(right[j])
        when {
            lk < rk -> i++
            lk > rk -> j++
            else -> {
                var k = j
                while (k < right.size && rightKey(right[k]) == lk) {
                    out.add(combine(left[i], right[k]))
                    k++
                }
                i++
            }
        }
    }
    return out
}
```

Mochi's optimiser picks merge-join only when statistics flag both sides as sorted on the join key. Statistics come from the IR pass that tracks `order by` invariants and from `@sorted(by:)` annotations on dataset loaders.

In practice, hash-join is the default; merge-join is reserved for large pre-sorted datasets (e.g. results of database scans with `ORDER BY`).

## 11. Aggregations

The Mochi aggregates lower to Kotlin stdlib functions where possible:

| Mochi    | Kotlin (Sequence/Iterable)             | Notes                                  |
|----------|-----------------------------------------|----------------------------------------|
| `count(g)` | `g.count()` or `g.size` (List)       | `count()` works on Sequence; `size` on List |
| `sum(g.x)` | `g.sumOf { it.x }`                   | Returns `Long` for `Long`, `Double` for `Double` |
| `min(g)` | `g.min()` (since 1.7) or `g.minOrNull()` | Returns nullable; throws on 1.4-1.6 `min` |
| `max(g)` | `g.max()` or `g.maxOrNull()`          | Same as min                            |
| `avg(g.x)` | custom: `g.map { it.x }.average()` or fold | Returns `Double`; `NaN` on empty   |

`average()` is in the stdlib for `Iterable<Number>` since Kotlin 1.0. It returns `Double` and divides by `count`. For empty input it returns `Double.NaN`; Mochi normalises this to `null` (Mochi's `avg` on empty returns `nil`):

```kotlin
fun mochiAvg(xs: Iterable<Long>): Double? {
    var sum = 0L
    var n = 0
    for (x in xs) { sum += x; n += 1 }
    return if (n == 0) null else sum.toDouble() / n
}
```

In a group_by context the aggregate folds into the per-group accumulator (see §7 example).

For aggregates over `Flow<T>`, the suspending operators are:

| Mochi    | Flow operator                                         |
|----------|--------------------------------------------------------|
| `count`  | `flow.count()` (suspend, terminal)                     |
| `sum`    | `flow.fold(0L) { a, x -> a + x.amount }`               |
| `min`    | custom fold with sentinel                              |
| `max`    | custom fold with sentinel                              |
| `avg`    | custom fold tracking sum + count                       |

`Flow.reduce` exists but throws on empty; Mochi avoids it for nullable-aware aggregates and emits a `fold` chain instead.

## 12. Materialisation, the decision point

The codegen has a single decision point: when is the result of a query a lazy chain vs a `List`? The rule:

- If the query result is bound to a `let xs: list<T>` (the user wrote a type or the inferred type is `list<T>`), materialise to `List` via `.toList()`.
- If the result is consumed exactly once by a subsequent `reduce` / `for` / `first`, keep it lazy via `Sequence` without `.toList()`.
- If the result feeds two or more later expressions, materialise. Re-iterating a `Sequence` is legal but recomputes everything.

The Mochi IR pass `mochi-ir-use-count` tracks use-count and emits the right shape. Materialisation cost is one heap allocation plus N element copies; lazy cost is per-op closure overhead. For chains shorter than ~3 ops and N > ~1000, materialisation often wins on JVM because the JIT specialises the iteration; for K/Native and JS, the gap is smaller.

For Flow results, `.toList()` is suspending and buffers the entire stream into memory. The codegen reserves it for bounded Flows and emits a Mochi-side diagnostic if applied to a stream type without a bound annotation.

## 13. Primitive specialisation

Kotlin distinguishes between boxed and unboxed primitives only for arrays:

- `IntArray`, `LongArray`, `DoubleArray`, `BooleanArray` (unboxed primitive arrays on JVM, equivalent specialised types on K/Native and K/JS).
- `List<Long>` is implemented as `ArrayList<java.lang.Long>` on JVM, so each element is a boxed `Long` object (16 bytes overhead per element). Iteration goes through `Iterator<Long>` which auto-unboxes.

Mochi's default lowering for `list<int>` is `List<Long>`, not `LongArray`. This is because:

- Most Mochi queries consume Lists, not Arrays, and the stdlib's collection operators (`filter`, `map`, `groupBy`) all work on `Iterable<T>` not on `IntArray`.
- The boxing cost is paid once at construction; subsequent iteration is fast.
- Cross-target portability: `IntArray` exists on K/Native and K/JS but has subtly different perf characteristics; `List<Long>` is uniform.

For users who need primitive arrays (perf-critical inner loops, FFI passthrough), Mochi exposes `list<int> @primitive_array` which lowers to `LongArray`. The Mochi linter flags inappropriate uses (e.g. `@primitive_array` on a list that gets passed to a `groupBy`).

The Kotlin compiler under `-Xinline` will inline the lambda body into the iteration, but it does **not** unbox elements when iterating `List<Long>`. To get true unboxed iteration, the user has to either use `LongArray` or wrap the iteration in a `for (i in 0 until xs.size)` indexed loop on a `LongArray`. Mochi emits the indexed-loop form for `@primitive_array`-annotated columns.

## 14. Parallel queries

Mochi's `parallel` annotation on a query lowers to a `runBlocking` + `withContext(Dispatchers.Default)` chunk + `awaitAll` pattern:

```mochi
let total = (from x in xs select x.amount) |> sum @parallel
```

```kotlin
val total: Long = runBlocking {
    xs.chunked(10_000).map { chunk ->
        async(Dispatchers.Default) {
            chunk.sumOf { it.amount }
        }
    }.awaitAll().sum()
}
```

`Iterable.chunked(N)` is stdlib (since 1.2). Each chunk runs on `Dispatchers.Default` (the shared coroutine threadpool, sized to `Runtime.getRuntime().availableProcessors()` on JVM, similar shape elsewhere). `awaitAll()` joins all child results.

Mochi only enables `@parallel` for associative reductions (sum, count, min, max). Non-associative ops (weighted average without splitting) are rejected by the type checker.

For Flow-based parallel queries, `Flow.flatMapMerge` provides concurrency:

```mochi
let results = from x in stream parallel(4) select expensive_op(x)
```

```kotlin
val results: Flow<Output> = stream.flatMapMerge(concurrency = 4) { x ->
    flow { emit(expensive_op(x)) }
}
```

`flatMapMerge` runs up to N concurrent inner flows. Order is **not preserved** (the first-completed emit wins); for order-preserving concurrency the user opts into `flatMapConcat` (sequential) or `parMap` (a Mochi-runtime helper that preserves order).

## 15. Flow transformations

`Flow<T>` operators in `kotlinx.coroutines.flow` cover the standard query DSL surface:

| Mochi      | Flow operator                                  |
|------------|------------------------------------------------|
| `where p`  | `.filter { p }`                                |
| `select e` | `.map { e }`                                   |
| `limit N`  | `.take(N)`                                     |
| `offset N` | `.drop(N)`                                     |
| `flatten`  | `.flatMapConcat { it }`                        |
| `merge`    | `.flatMapMerge { it }` or top-level `merge()` |
| `zip`      | `flow.zip(other) { a, b -> ... }`              |

Cancellation propagates through the chain: cancelling the downstream collector cancels every upstream operator. Errors propagate too: an exception in any operator cancels the chain and rethrows at the collector.

For stream-time-based operations:

| Mochi              | Flow operator                                |
|--------------------|----------------------------------------------|
| `debounce(d)`      | `.debounce(d)`                               |
| `throttle(d)`      | `.sample(d)`                                 |
| `window(d)`        | `.chunked(d)` (kotlinx-coroutines ext)       |
| `combineLatest(b)` | `.combine(b) { a, b -> ... }`                |

`debounce` and `sample` are in `kotlinx.coroutines.flow` since 1.5. `chunked` for time-based windows is in `kotlinx-coroutines` as `Flow.chunked(by:)` since 1.7 (Flow.chunked taking a Duration).

## 16. Streaming joins

Joining a stream with a list is supported as long as the list side fits in memory: the codegen builds a hash index on the list, then probes per stream event.

```mochi
let m = from o in orderStream
        join u in users on o.user_id == u.id
        select { name: u.name, amount: o.amount }
```

```kotlin
val idx: Map<Long, User> = users.associateBy { it.id }

val m: Flow<Row> = orderStream.mapNotNull { o ->
    idx[o.user_id]?.let { u -> Row(name = u.name, amount = o.amount) }
}
```

Joining two streams is more delicate. The supported pattern requires partitioning by the join column to a hash table on the smaller, bounded side; the other side is consumed as a true stream. If both sides are unbounded, the codegen emits an error and asks the user to add a `window` clause to bound the join.

```mochi
let m = from o in stream_a window 60s
        join u in stream_b window 60s on o.k == u.k
        select { o, u }
```

The codegen lowers each `window` to a chunk-by-time Flow, then joins the chunks. Across windows the join is hash-based per window. This is a common pattern in event stream processing (Apache Flink terms: tumbling-window join).

```kotlin
val windowedA = stream_a.chunked(60.seconds)
val windowedB = stream_b.chunked(60.seconds)

val m: Flow<Row> = windowedA.zip(windowedB) { batchA, batchB ->
    val idx = batchB.associateBy { it.k }
    batchA.mapNotNull { a -> idx[a.k]?.let { b -> Row(o = a, u = b) } }
}.flatMapConcat { it.asFlow() }
```

## 17. Window functions

Mochi's `windowed(n)` lowers to `Sequence.windowed`:

```kotlin
val rolling: List<Double> = xs.asSequence()
    .windowed(size = 3, step = 1)
    .map { window -> window.average() }
    .toList()
```

`windowed(size, step)` is stdlib since Kotlin 1.2. It returns a `Sequence<List<T>>` of overlapping windows by default. For non-overlapping, `step = size`:

```kotlin
val batches: List<List<Item>> = xs.windowed(size = 100, step = 100).toList()
```

Equivalent to `xs.chunked(100)` (which is the canonical Kotlin idiom for non-overlapping chunks).

Time windows on a Flow use the kotlinx-coroutines `chunked` extension (added in 1.8):

```mochi
let per_minute = from e in stream window 60s into batch select { count: count(batch) }
```

```kotlin
val per_minute: Flow<MinuteRow> = stream
    .chunked(60.seconds)
    .map { batch -> MinuteRow(count = batch.size) }
```

The codegen picks the static-N variant when the window literal is an integer count, and the time variant when it is a duration literal.

## 18. Top-K

Mochi's `order by ... limit K` with large input and small K lowers to a bounded heap:

```mochi
let top10 = from p in people order by p.score desc limit 10 select p
```

For small K relative to N, sorting the entire list is O(N log N); a bounded min-heap is O(N log K).

Kotlin's stdlib does not include a heap data structure. Mochi ships `MochiRuntime.Collections.MinHeap<T>`:

```kotlin
class MinHeap<T>(private val capacity: Int, private val cmp: Comparator<T>) {
    private val data = ArrayList<T>(capacity)
    fun offer(x: T): T? { /* insert; if size > capacity, evict min */ }
    fun sortedDescending(): List<T> = data.sortedWith(cmp.reversed())
}
```

For the simple case, the codegen falls back to `sortedByDescending().take(K)` which is correct but suboptimal:

```kotlin
val top10: List<Person> = people.sortedByDescending { it.score }.take(10)
```

When the IR pass sees `order by x desc limit K` with `K < N/100` (heuristic), the codegen switches to the heap path. The threshold is tunable per project.

On JVM, `java.util.PriorityQueue` is available; on K/Native and K/JS, Mochi's heap is the only option.

## 19. Distinct

Mochi `distinct` lowers depending on the element's Hashable conformance:

- Hashable and order-irrelevant: `Set` from the stdlib.

  ```kotlin
  val uniq: List<T> = xs.toSet().toList()
  ```

- Hashable and order-preserving: `LinkedHashSet`.

  ```kotlin
  val uniq: List<T> = xs.toMutableSet().toList()  // LinkedHashSet by default
  ```

- Stdlib `distinct`:

  ```kotlin
  val uniq: List<T> = xs.distinct()
  ```

`Iterable.distinct()` returns a `List<T>` preserving first-occurrence order. Internally it uses `LinkedHashSet`. This is the canonical Mochi lowering.

For `distinctBy`:

```mochi
let uniq_by_email = from u in users select u distinct by u.email
```

```kotlin
val uniq_by_email: List<User> = users.distinctBy { it.email }
```

`distinctBy { key }` preserves the **first occurrence** of each key, matching Mochi semantics.

For Flow:

```kotlin
val uniq: Flow<T> = stream.distinctUntilChanged()  // adjacent-duplicate elimination
```

`distinctUntilChanged` is in `kotlinx.coroutines.flow`. For true global distinct on a stream (any duplicate, not just adjacent), the user has to buffer; the codegen rejects unbounded inputs.

## 20. Datalog evaluation

Mochi's datalog facts and rules ([[01-language-surface]] §6) lower to a runtime engine in `MochiRuntime.Datalog`. The engine uses **semi-naive bottom-up evaluation** with a small in-memory term representation.

**Facts and rules** are registered at module init time:

```mochi
fact parent("alice", "bob")
fact parent("bob", "carol")

rule ancestor(X, Y) :- parent(X, Y)
rule ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)
```

lowers to:

```kotlin
object MochiDatalogModule {
    val engine = DatalogEngine()
    init {
        engine.addFact("parent", listOf(MochiDatalogTerm.Str("alice"), MochiDatalogTerm.Str("bob")))
        engine.addFact("parent", listOf(MochiDatalogTerm.Str("bob"), MochiDatalogTerm.Str("carol")))
        engine.addRule(Rule(
            head = Atom("ancestor", listOf(Var("X"), Var("Y"))),
            body = listOf(Atom("parent", listOf(Var("X"), Var("Y"))))
        ))
        engine.addRule(Rule(
            head = Atom("ancestor", listOf(Var("X"), Var("Y"))),
            body = listOf(
                Atom("parent", listOf(Var("X"), Var("Z"))),
                Atom("ancestor", listOf(Var("Z"), Var("Y")))
            )
        ))
    }
}
```

**Term representation**: a sealed interface with variant types:

```kotlin
sealed interface MochiDatalogTerm {
    data class Atom(val name: String) : MochiDatalogTerm
    data class IntLit(val value: Long) : MochiDatalogTerm
    data class Str(val value: String) : MochiDatalogTerm
    data class Compound(val name: String, val args: List<MochiDatalogTerm>) : MochiDatalogTerm
    data class ListTerm(val elements: List<MochiDatalogTerm>) : MochiDatalogTerm
    data class Var(val name: String) : MochiDatalogTerm
}
```

The sealed interface lets the engine pattern-match exhaustively via `when (t) { is Atom -> ... is IntLit -> ... }`. Kotlin 2.1's smart casts for sealed when expressions remove the need for an explicit `else` branch.

**Unification**: a recursive walk with a substitution map:

```kotlin
fun unify(a: MochiDatalogTerm, b: MochiDatalogTerm, subst: MutableMap<String, MochiDatalogTerm>): Boolean {
    val resolvedA = resolve(a, subst)
    val resolvedB = resolve(b, subst)
    return when {
        resolvedA is Var -> { subst[resolvedA.name] = resolvedB; true }
        resolvedB is Var -> { subst[resolvedB.name] = resolvedA; true }
        resolvedA is Atom && resolvedB is Atom -> resolvedA.name == resolvedB.name
        resolvedA is IntLit && resolvedB is IntLit -> resolvedA.value == resolvedB.value
        resolvedA is Str && resolvedB is Str -> resolvedA.value == resolvedB.value
        resolvedA is Compound && resolvedB is Compound ->
            resolvedA.name == resolvedB.name &&
            resolvedA.args.size == resolvedB.args.size &&
            resolvedA.args.zip(resolvedB.args).all { (x, y) -> unify(x, y, subst) }
        else -> false
    }
}

fun resolve(t: MochiDatalogTerm, subst: Map<String, MochiDatalogTerm>): MochiDatalogTerm =
    if (t is Var) subst[t.name]?.let { resolve(it, subst) } ?: t else t
```

**Semi-naive bottom-up evaluation**:

The engine maintains a set of derived facts `I`. Initially `I` contains the EDB (extensional) facts. In each iteration, the engine tries to apply each rule with at least one body atom matching a **new** fact from the last iteration (the `delta` set). Newly derived facts go into `I` and into the next `delta`. The fixed point is reached when `delta` is empty.

```kotlin
class DatalogEngine {
    private val facts = mutableSetOf<Fact>()
    private val rules = mutableListOf<Rule>()

    fun query(atom: Atom): Sequence<Map<String, MochiDatalogTerm>> = sequence {
        evaluate()
        for (fact in facts) {
            if (fact.name == atom.name && fact.args.size == atom.args.size) {
                val subst = mutableMapOf<String, MochiDatalogTerm>()
                if (atom.args.zip(fact.args).all { (a, f) -> unify(a, f, subst) }) {
                    yield(subst.toMap())
                }
            }
        }
    }

    private fun evaluate() {
        var delta = facts.toSet()
        while (delta.isNotEmpty()) {
            val newDelta = mutableSetOf<Fact>()
            for (rule in rules) {
                for (subst in derive(rule, delta)) {
                    val newFact = instantiate(rule.head, subst)
                    if (facts.add(newFact)) newDelta.add(newFact)
                }
            }
            delta = newDelta
        }
    }
}
```

The full engine is ~400 lines. It is single-threaded, single-database, and synchronous. Performance is adequate for fixtures with a few hundred rules and a few thousand facts; production-scale datalog (millions of facts) is out of scope for v1.

**Magic-set transform**: deferred to v2. The magic-set transform reorders rules to push selections down, dramatically improving performance for queries with selective heads. Implementing it requires a non-trivial IR pass; v1 ships without.

**Property-based generated facts**: deferred to v2. Mochi's spec allows `forall x in xs, p(x) => fact ...`; lowering this requires extending the engine to support quantified head rules. v1 supports only ground facts and rules with finite head variables.

## 21. Performance

Targets for v0.1 (vs the vm3 baseline; benchmarks on AMD64 Ubuntu 24.04, OpenJDK 21, Kotlin 2.1, release mode):

- 1M-row `from x in L where x.k > 100 select x.v sum`:
  - Kotlin/JVM ≤ 1.5x slower than the C target (MEP-45), ≤ 3x slower than vm3.
  - Kotlin/Native ≤ 2x slower than C.
  - Kotlin/JS ≤ 5x slower than vm3.
  - Kotlin/Wasm ≤ 4x slower than vm3.
- 100K-row hash join (1:1 cardinality):
  - Kotlin/JVM ≤ 2x slower than C.
  - Kotlin/Native ≤ 2.5x slower than C.
- 1M-row `group by` into 100 keys: < 150ms wall clock on JVM, < 250ms on K/Native.
- CSV load of 1M rows × 10 cols: < 1.5s including parsing (JVM with `kotlinx-serialization-csv` or Apache Commons CSV).
- Flow chain (filter + map + reduce) over 100K events: per-element suspend dominates; ≤ 5x slower than the sync `Sequence` equivalent.

The single largest perf knob in the codegen is keeping the chain monomorphic and unboxed. Boxing every `Long` in a `Sequence<Long>` chain is the most common foot-gun; the Mochi linter warns when an `Any`-typed intermediate appears in a hot path.

## 22. Dataset I/O

Mochi's dataset I/O (CSV, JSON, Parquet) lowers to per-target libraries:

| Format    | JVM / Android                              | K/Native / K/JS / K/Wasm                          |
|-----------|-------------------------------------------|----------------------------------------------------|
| JSON      | `kotlinx-serialization-json` (multiplatform) | `kotlinx-serialization-json` (multiplatform)     |
| CBOR      | `kotlinx-serialization-cbor`              | `kotlinx-serialization-cbor`                       |
| ProtoBuf  | `kotlinx-serialization-protobuf`          | `kotlinx-serialization-protobuf`                   |
| CSV       | Apache Commons CSV (JVM only)             | Hand-rolled `kotlinx-io` based parser              |
| Parquet   | Apache Parquet (JVM only)                 | Not supported                                      |
| Avro      | Apache Avro (JVM only)                    | Not supported                                      |

**JSON / CBOR / ProtoBuf via kotlinx-serialization**: the `kotlinx.serialization` library (1.7+) is multiplatform and ships for every Kotlin target. Mochi codegen for records emits `@Serializable` annotations:

```kotlin
@Serializable
data class Person(val name: String, val age: Int)

val json = Json.encodeToString(person)
val parsed: Person = Json.decodeFromString<Person>(jsonText)
```

`kotlinx.serialization.json.Json` is the JSON codec; `Cbor` for CBOR; `ProtoBuf` for protobuf. All three are multiplatform and have consistent semantics.

**CSV**: there is no multiplatform CSV library in the Kotlin ecosystem that we ship by default. For JVM, Apache Commons CSV is the de facto standard; for K/Native and K/JS, Mochi ships a hand-rolled CSV parser in `MochiRuntime.IO.Csv` that handles RFC 4180 (quoted fields, escaped quotes, CRLF and LF line endings).

**Parquet / Avro / ORC**: these are JVM-only in practice. The Apache Parquet library depends on Apache Hadoop (transitively, ~30 MB of dependencies). Mochi exposes Parquet read/write only on the JVM target via a separate optional module (`mochi-runtime-jvm-parquet`); users who target K/Native or K/JS and need Parquet must export to a JVM bridge.

## 23. Apache Arrow Kotlin bindings

Apache Arrow has Java bindings (mature since 2017) and Kotlin can consume them via Java interop. There is no native Kotlin Arrow library as of mid-2026.

For Mochi-on-Kotlin, Arrow integration is **deferred to v2**. Reasons:

- The Arrow Java library bundles ~10 MB of dependencies and uses `sun.misc.Unsafe`, which is restricted in newer JDKs.
- Multiplatform Arrow does not exist (the Kotlin Arrow Foundation library at arrow-kt.io is a different project, focused on functional types like `Either` and `IO`, not Apache Arrow columnar format).
- Mochi's per-column primitive specialisation (§13) gets us most of Arrow's perf benefits without the dependency cost.

A future MEP-50 v2 may add Arrow integration via the JVM target only, with a Mochi `@arrow` annotation to opt in.

## 24. Spark integration

Apache Spark (the JVM big-data engine) is the canonical dataset framework on the JVM. Mochi could potentially compile to Spark DataFrames, but this is **out of scope for v1**:

- Spark introduces a heavy runtime (~200 MB of JARs, Hadoop dependencies, Java 17 incompatibility on older versions).
- Spark's lazy evaluation model differs from Mochi's eager-Sequence and lazy-Flow semantics; lowering would require a separate IR pass.
- Spark is JVM-only; Mochi's other targets (Native, JS, Wasm) cannot consume Spark.

We document Spark integration as a manual user-driven step: Mochi-on-JVM-Kotlin code can be wrapped in a Spark UDF, called from a separate Spark application. The Mochi runtime provides a thin helper (`MochiRuntime.Spark`) for the JVM target only.

A future MEP-50 v3 might add direct Spark codegen, but it would be a separate sub-target rather than a layer over the existing Kotlin output.

## 25. Sample lowerings

A simple from / where / select:

```mochi
let adults = from p in people where p.age > 18 select p.name
```

```kotlin
val adults: List<String> = people.asSequence()
    .filter { it.age > 18 }
    .map { it.name }
    .toList()
```

A group_by with aggregate:

```mochi
let by_dept = from p in people group by p.dept into g select {
    dept: g.key, n: count(g), avg_sal: avg(g |> map(p => p.salary))
}
```

```kotlin
data class DeptAcc(var sum: Long = 0L, var n: Int = 0)

val by_dept: List<DeptRow> = buildMap<String, DeptAcc> {
    for (p in people) {
        val acc = getOrPut(p.dept) { DeptAcc() }
        acc.sum += p.salary
        acc.n += 1
    }
}.map { (dept, acc) ->
    DeptRow(dept = dept, n = acc.n, avgSal = acc.sum.toDouble() / acc.n)
}
```

A hash join:

```mochi
let joined = from o in orders join u in users on o.user_id == u.id
             select { name: u.name, amount: o.amount }
```

```kotlin
val idx: Map<Long, User> = users.associateBy { it.id }
val joined: List<Row> = orders.mapNotNull { o ->
    idx[o.user_id]?.let { u -> Row(name = u.name, amount = o.amount) }
}
```

A streaming filter on a Flow:

```mochi
let alerts = from e in event_stream where e.severity == "high" select e.message
```

```kotlin
val alerts: Flow<String> = event_stream
    .filter { it.severity == "high" }
    .map { it.message }

alerts.collect { msg -> println(msg) }
```

A datalog query:

```mochi
fact parent("alice", "bob")
rule ancestor(X, Y) :- parent(X, Y)
rule ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)

let descendants_of_alice = query ancestor("alice", Y) select Y
```

```kotlin
val descendants_of_alice: List<String> = MochiDatalogModule.engine
    .query(Atom("ancestor", listOf(Str("alice"), Var("Y"))))
    .map { subst -> (subst["Y"] as Str).value }
    .toList()
```

A top-K query:

```mochi
let top10 = from p in people order by p.score desc limit 10 select p.name
```

```kotlin
val top10: List<String> = people
    .sortedByDescending { it.score }
    .take(10)
    .map { it.name }
```

(For K << N the codegen switches to the heap path described in §18.)

A window query on a stream:

```mochi
let per_minute = from e in stream window 60s into batch select { count: count(batch) }
```

```kotlin
val per_minute: Flow<MinuteRow> = stream
    .chunked(60.seconds)
    .map { batch -> MinuteRow(count = batch.size) }
```

---

## Cross-references

- [[01-language-surface]] §3, the DSL surface.
- [[04-runtime]] §5, the `MochiRuntime.Query` module structure.
- [[05-codegen-design]] §6, MIR pipeline IR shared with MEP-45/46/47/48/49.
- [[06-type-lowering]] §4, `LinkedHashMap` for Mochi maps, `Long` and `Double` primitives.
- [[07-kotlin-target-portability]] §10, Kotlin/Wasm caveats for Flow.
- [[09-agent-streams]], `Flow` and the `kotlinx.coroutines.flow` operator set.
- [[10-build-system]] §3, Gradle dependencies (kotlinx-coroutines, kotlinx-serialization).
- [[11-testing-gates]] §2, vm3 differential oracle.
- [[../0049/08-dataset-pipeline]], the Swift counterpart (Sequence + AsyncSequence).

---

## Sources

1. Kotlin Standard Library, `Sequence` reference. <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.sequences/-sequence/>
2. Kotlin Standard Library, `Iterable.groupBy` reference. <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/group-by.html>
3. kotlinx.coroutines Flow guide. <https://kotlinlang.org/docs/flow.html>
4. kotlinx.coroutines 1.10 changelog. <https://github.com/Kotlin/kotlinx.coroutines/blob/master/CHANGES.md>
5. kotlinx.serialization documentation. <https://kotlinlang.org/docs/serialization.html>
6. Apache Commons CSV. <https://commons.apache.org/proper/commons-csv/>
7. Apache Parquet Java. <https://github.com/apache/parquet-java>
8. RFC 4180 (CSV). <https://www.rfc-editor.org/rfc/rfc4180>
9. Ullman, "Principles of Database and Knowledge-Base Systems Vol. 1." Computer Science Press, 1989 (hash and merge join algorithms).
10. Bancilhon and Ramakrishnan, "An amateur's introduction to recursive query processing strategies." SIGMOD 1986 (semi-naive evaluation).
11. Apache Arrow Java. <https://arrow.apache.org/docs/java/>
12. Apache Spark documentation. <https://spark.apache.org/docs/latest/>
13. ES2019 stable Array.prototype.sort. <https://tc39.es/ecma262/#sec-array.prototype.sort>
14. Kotlin sealed types in 2.1. <https://kotlinlang.org/docs/whatsnew21.html>
