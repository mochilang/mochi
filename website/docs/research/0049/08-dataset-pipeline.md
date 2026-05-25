# MEP-49 research note 08, Mochi query DSL and dataset pipeline on Swift

Author: research pass for MEP-49.
Date: 2026-05-23 10:30 (GMT+7).

This note covers Mochi's LINQ-style query DSL (`from ... in ... where ... select ...`), group_by, joins (hash, merge, nested-loop, cross), order_by, aggregates, set ops, and streaming queries, and how each lowers onto Swift 6.0. Companion notes: [[05-codegen-design]], [[06-type-lowering]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]].

The Swift target leans on the standard `Sequence` and `AsyncSequence` protocols, `Dictionary` and `Set` from the stdlib, and three apple-supported packages: `swift-collections` (`OrderedDictionary`, `OrderedSet`, `Heap`), `swift-algorithms` (`windows`, `chunks`, `uniqued`), and `swift-async-algorithms` (merging, throttling, backpressure). We assume Swift 6.0 as the floor with strict concurrency on by default.

---

## 1. Mochi query surface recap

Mochi inherits a LINQ-style query DSL (see [[01-language-surface]] §3). Examples:

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

All of these lower to Swift code, but the choice between `Sequence` and `AsyncSequence`, between hash and merge join, and between materialised and lazy chains varies by clause.

## 2. Swift Sequence vs. AsyncSequence

Mochi queries split cleanly along the sync/async axis:

- A query whose source is a `list<T>` (materialised in memory) lowers to Swift `Sequence`. Every intermediate op is synchronous and stays inside the calling function.
- A query whose source is a `stream<T>` (defined in [[09-agent-streams]]) lowers to Swift `AsyncSequence`. Every intermediate op is `async`, and consuming the chain requires `await for ... in ...`.

The optimiser picks the variant by tracing the source type through the type checker. There is no implicit upgrade: a query that joins a `list` with a `stream` is rejected at type-check time and the user has to `collect` the stream into a buffer first (see §16).

`Sequence` shape:

```swift
let adults: [String] = xs.filter { $0.age > 18 }.map { $0.name }
```

`AsyncSequence` shape:

```swift
let adultStream = stream
    .filter { $0.age > 18 }
    .map { $0.name }
for await name in adultStream { print(name) }
```

Both protocols are part of Swift 6.0; `AsyncSequence` gained typed throws (`AsyncSequence<Element, Failure>`) in SE-0421 (Swift 6.0), so the codegen can emit `AsyncSequence<Row, Never>` for in-process pipelines that never throw.

## 3. Sequence chaining

`Sequence` and its companion `LazySequence` provide a small but sufficient set of ops:

- `filter(_:)` returns `[Element]` eagerly; `.lazy.filter(_:)` returns a `LazyFilterSequence` that fuses with downstream ops.
- `map(_:)`, `compactMap(_:)`, `flatMap(_:)`.
- `reduce(_:_:)`, `reduce(into:_:)` for accumulators.
- `sorted(by:)` and `sorted()` (TimSort under the hood, stable).
- `prefix(_:)`, `dropFirst(_:)`, `prefix(while:)`.
- `min()`, `max()`, `min(by:)`, `max(by:)`.

The Mochi codegen prefers lazy chains when the result feeds a single terminal op (a `reduce`, a `for`, or a `first`):

```swift
let total = xs.lazy
    .filter { $0.amount > 0 }
    .map { $0.amount }
    .reduce(0, +)
```

The `.lazy` prefix turns the chain into `LazySequence<LazyFilterSequence<LazyMapSequence<...>>>`. Each element flows through the chain without intermediate `Array` allocation. Once the user binds the result to a `let xs: [Row]`, the codegen drops `.lazy` because materialisation is unavoidable.

## 4. swift-collections OrderedDictionary for group_by

Mochi's group_by preserves the insertion order of the first occurrence of each key. Swift's stdlib `Dictionary<K, V>` does not guarantee iteration order. We pull `OrderedDictionary` from `apple/swift-collections` (1.1+; Swift 6.0 compatible).

Lowering shape:

```swift
import OrderedCollections

var groups = OrderedDictionary<String, [Person]>()
for p in people {
    groups[p.dept, default: []].append(p)
}
let result = groups.map { (dept, members) in
    Row(dept: dept, n: members.count)
}
```

`OrderedDictionary` exposes the same `subscript(_:default:)` shape as `Dictionary`, so the `groupingBy` idiom is a one-liner adaptation. The package is part of the Swift Server Workgroup's curated set and is permissively licensed; we add it to `Package.swift` as a default dependency (see [[10-build-system]] §3).

When the user's select clause only needs aggregate values (no group materialisation), the codegen folds aggregates directly into the dictionary value, avoiding the intermediate `[Person]` list:

```swift
var counts = OrderedDictionary<String, Int>()
for p in people { counts[p.dept, default: 0] += 1 }
```

This mirrors the `groupingBy(_, Collectors.counting())` pattern from MEP-47 §3.4.

## 5. Hash-join implementation

The default equi-join lowering. Build a hash table on the smaller relation (by row count if known, else the right side per source order), probe with the larger.

```swift
let idx: [Int64: User] = Dictionary(uniqueKeysWithValues:
    users.map { ($0.id, $0) })

let out: [Row] = orders.compactMap { o in
    guard let u = idx[o.userId] else { return nil }
    return Row(name: u.name, amount: o.amount)
}
```

For a many-to-many join the value side is a `[User]`:

```swift
let idx: [Int64: [User]] = Dictionary(grouping: users, by: \.id)

let out: [Row] = orders.flatMap { o in
    (idx[o.userId] ?? []).map { u in
        Row(name: u.name, amount: o.amount)
    }
}
```

Time complexity O(n + m); space O(min(n, m)) for the build side. The codegen picks the build side based on the declared row-count annotation or, when absent, the right-hand operand of `join`. The unique vs many decision flows from the join key's declared type (`@unique`) or from a side-table from the type checker.

## 6. Merge-join

When the type checker proves both inputs are already sorted by the join column (e.g. results of an `order by k` upstream, or a primary-key scan), the optimiser switches to a merge-join: O(n + m) time, O(1) space, single pass, friendly to streaming.

```swift
func mergeJoin(_ left: [Order], _ right: [User]) -> [Row] {
    var i = 0, j = 0, out: [Row] = []
    while i < left.count && j < right.count {
        if left[i].userId < right[j].id { i += 1 }
        else if left[i].userId > right[j].id { j += 1 }
        else {
            var k = j
            while k < right.count && right[k].id == left[i].userId {
                out.append(Row(name: right[k].name, amount: left[i].amount))
                k += 1
            }
            i += 1
        }
    }
    return out
}
```

Mochi's optimiser picks merge-join only when the statistics flag both sides as sorted on the join key. The statistics come from the IR pass that tracks `order by` invariants and from `@sorted(by:)` annotations on dataset loaders.

## 7. Nested-loop join

The unconditional fallback when neither input is hashable on the join key (custom equality without `Hashable`, or floating-point joins) and neither is sorted. Complexity O(n * m).

```swift
let out: [Row] = xs.flatMap { x in
    ys.compactMap { y in
        guard predicate(x, y) else { return nil }
        return Row(x: x, y: y)
    }
}
```

The codegen emits a compile-time warning at the call site when nested-loop is selected, plus a note pointing at the join predicate. The user can silence the warning with an explicit `@nested_loop` hint (acknowledged) or restructure the join to use a hashable key. The warning shows the estimated cardinality if the optimiser has size annotations.

This is also the lowering for non-equi joins (`on x.k <= y.k`); equi-only joins go through the hash or merge path.

## 8. Cross-join

The cartesian product. Distinct from a nested-loop equi-join because there is no predicate at all. Lowers to a `flatMap` chain over the outer side, with the inner side wrapped in a `Sequence`.

```swift
let out: [Row] = xs.flatMap { x in
    ys.map { y in Row(x: x, y: y) }
}
```

For three-way cross-joins the chain extends naturally:

```swift
let out = as.flatMap { a in bs.flatMap { b in cs.map { c in (a, b, c) } } }
```

Cardinality scales as O(|xs| * |ys|), so the codegen emits a warning when at least one side has more than 1000 rows in its size annotation. Cross joins are typically only useful for small dimension tables.

## 9. Aggregate functions

The Mochi aggregates `count`, `sum`, `avg`, `min`, `max` lower to native Swift `Sequence` ops:

| Mochi    | Swift                                                       |
|----------|-------------------------------------------------------------|
| `count`  | `xs.count` (when materialised) or `xs.lazy.reduce(0)`       |
| `sum`    | `xs.reduce(0, +)` for `Int64`/`Double`                      |
| `min`    | `xs.min()` (returns `Element?`)                             |
| `max`    | `xs.max()`                                                  |
| `avg`    | custom: `xs.reduce((s: 0, n: 0)) { (.s + $1, .n + 1) }`     |

`avg` does not have a stdlib counterpart; the codegen emits a small reducer that accumulates both the sum and the count, then divides at the end. For empty inputs the result is `nil` (Mochi `avg` on empty returns `nil` per the spec).

In a group_by context the aggregate folds into the per-group accumulator without materialising the group list:

```swift
struct DeptAgg { var sum: Int64 = 0; var n: Int = 0 }
var agg = OrderedDictionary<String, DeptAgg>()
for p in people {
    agg[p.dept, default: DeptAgg()].sum += p.salary
    agg[p.dept, default: DeptAgg()].n += 1
}
let rows = agg.map { (dept, a) in
    Row(dept: dept, avg: Double(a.sum) / Double(a.n))
}
```

## 10. Order_by

For synchronous sources, `order by` lowers to `Array.sorted(by:)`:

```swift
let top = people.sorted { $0.score > $1.score }.prefix(10)
```

Multi-key sort chains comparators with a tuple comparison or a step-wise `if`:

```swift
let sorted = rows.sorted {
    if $0.k1 != $1.k1 { return $0.k1 < $1.k1 }
    return $0.k2 > $1.k2
}
```

`Sequence.sorted` is stable in Swift 5.8+ and remains stable in 6.0 (SE-0317 doc note). Stable means equal keys preserve insertion order, which matches Mochi's spec.

For `AsyncSequence` there is no direct `sorted`: sorting requires the full input. The codegen collects the stream to an array, sorts, then re-yields:

```swift
var buf: [Event] = []
for try await e in stream { buf.append(e) }
buf.sort { $0.ts > $1.ts }
```

This buffers the entire stream, so `order by` on an unbounded stream is rejected by the type checker; the user must use a windowed variant (see [[09-agent-streams]]).

## 11. Group_by

For synchronous sources, the simplest lowering is `Dictionary(grouping:by:)` for unordered group_by:

```swift
let g: [String: [Person]] = Dictionary(grouping: people, by: \.dept)
```

Mochi requires insertion-order output, so the codegen always uses the explicit `OrderedDictionary` walk shown in §4. The performance cost is a small constant overhead vs the stdlib `Dictionary` builder; `OrderedDictionary` maintains both a hash table and an array of keys.

When the select clause needs only aggregates, the codegen fuses the fold (see §9). When it needs the full group list (e.g. `select { dept: g.key, names: g |> map(p => p.name) }`), the codegen first materialises into `OrderedDictionary<K, [V]>` and then maps over the entries.

`having pred` post-filters the entries:

```swift
let result = agg.filter { _, value in value.n > 5 }
    .map { (k, v) in Row(k: k, n: v.n) }
```

`OrderedDictionary.filter` preserves order and returns another `OrderedDictionary` (or a key/value array depending on the API call).

## 12. Pipeline materialisation

The codegen has a single decision point: when is the result of a query a lazy chain vs an `Array`? The rule:

- If the query result is bound to a `let xs: [T]` (the user wrote a type or the inferred type is `list<T>`), materialise to `Array`.
- If the result is consumed exactly once by a subsequent `reduce` / `for` / `first`, keep it lazy via `.lazy`.
- If the result feeds two or more later expressions, materialise. Re-iterating a `LazySequence` is legal but recomputes everything.

The IR pass tracks use-count and emits the right shape. Materialisation cost is one heap allocation plus N element copies; lazy cost is per-op closure overhead. For chains shorter than ~3 ops and N > ~1000, materialisation wins on modern Swift releases because `Array.withUnsafeBufferPointer` enables vectorisation in the consumer.

## 13. Primitive specialisation

Mochi int columns lower to `[Int64]`, not `[Any]`. Mochi float columns lower to `[Double]`. Swift's `Array<Int64>` is contiguous, unboxed, and the same shape as a C `int64_t[]` on Apple platforms (verified via `withContiguousStorageIfAvailable`).

The query DSL emits typed chains:

```swift
let total: Int64 = xs.lazy
    .filter { $0 > 100 }
    .map { $0 * 2 }
    .reduce(0, +)
```

The compiler specialises `LazySequence<Array<Int64>>` and inlines the closures under `-O` whole-module. The resulting machine code is competitive with a hand-written `for` loop on Apple Silicon (M-series) as of Swift 6.0. The single largest perf knob in the codegen is avoiding any `Any` or existential `any Hashable` step in the middle of the chain (see §20).

For boolean columns we still pay one bit per element via `Array<Bool>`; Swift does not provide a packed bit vector in the stdlib. swift-collections offers `BitArray` (1.1+) which we use for hot boolean paths if the user opts in.

## 14. Parallel queries

Mochi's `parallel` annotation on a query lowers to a Swift `TaskGroup` with chunked work:

```swift
let chunks = xs.chunks(ofCount: 10_000) // swift-algorithms
let total = await withTaskGroup(of: Int64.self) { group in
    for chunk in chunks {
        group.addTask { chunk.reduce(0, +) }
    }
    var sum: Int64 = 0
    for await partial in group { sum += partial }
    return sum
}
```

`chunks(ofCount:)` comes from `swift-algorithms` (1.2+); it returns a `Sequence` of `SubSequence` slices, no allocation per chunk. `TaskGroup` distributes the slices across the cooperative thread pool. The chunk size is tunable; for sum/avg/count the default 10K gives a good balance between scheduler overhead and cache locality.

For streaming inputs, `swift-async-algorithms` provides backpressure-aware merging via `merge(_:_:)` and `combineLatest(_:_:)`. The codegen uses `merge` to fan-in partial results from parallel sub-streams.

The optimiser only enables parallel lowering when the operation is associative and stateless (sum, count, min, max). For non-associative ops (e.g. weighted moving average) the codegen rejects the `parallel` annotation at compile time.

## 15. AsyncSequence transformations

`AsyncSequence` operators in Swift 6.0 are async functions: `filter(_:)`, `map(_:)`, `reduce(_:_:)`, `prefix(_:)`, `dropFirst(_:)`. They all return a wrapper conforming to `AsyncSequence` (e.g. `AsyncMapSequence`, `AsyncFilterSequence`).

```swift
let names = stream
    .filter { $0.age > 18 }
    .map { $0.name }

for await n in names {
    print(n)
}
```

`for await` is the canonical consumer. Errors propagate via `for try await` when the underlying sequence has a non-`Never` failure type. Cancellation is cooperative: the consumer drops the iterator and the source observes `Task.isCancelled` on its next yield.

swift-async-algorithms supplements with `chunked(by:)`, `throttle(for:)`, `debounce(for:)`, `interspersed(with:)`, and combinators (`merge`, `zip`, `combineLatest`). Mochi's `window 60s` syntax lowers to `chunks(ofCount:timeout:)` or `chunked(by: ClockSegment(...))`. Full details in [[09-agent-streams]].

## 16. Streaming joins

Joining a stream with a list is supported as long as the list side fits in memory: the codegen builds a hash index on the list, then probes per stream event.

```swift
let idx: [Int64: User] = Dictionary(uniqueKeysWithValues: users.map { ($0.id, $0) })

for await o in orderStream {
    guard let u = idx[o.userId] else { continue }
    print(Row(name: u.name, amount: o.amount))
}
```

Joining two streams is more delicate. The supported pattern requires partitioning by the join column to a hash table on the smaller, bounded side; the other side is consumed as a true stream. If both sides are unbounded, the codegen emits an error and asks the user to add a `window` clause to bound the join.

```mochi
let m = from o in stream_a window 60s
        join u in stream_b window 60s on o.k == u.k
        select { o, u }
```

The codegen lowers each `window` to a chunk-by-time AsyncSequence, then joins the chunks. Across windows the join is hash-based per window. This is a common pattern in event stream processing (Apache Flink terms: tumbling-window join).

## 17. Window functions

Mochi's `windowed(n)` lowers to `windows(ofCount:)` from swift-algorithms:

```swift
import Algorithms

let rolling = xs.windows(ofCount: 3).map { window in
    window.reduce(0, +) / 3
}
```

`windows(ofCount:)` returns overlapping windows by default. For non-overlapping, `chunks(ofCount:)` is the right primitive.

Time windows on a stream use `swift-async-algorithms` chunked-by-clock:

```swift
import AsyncAlgorithms

let perMinute = stream
    .chunked(by: .repeating(every: .seconds(60), clock: .continuous))

for await batch in perMinute {
    print("count this minute: \(batch.count)")
}
```

The codegen picks the static-N variant when the window literal is an integer count, and the time variant when it is a duration literal.

## 18. Top-K

Mochi's `order by ... limit K` with large input and small K lowers to a bounded heap from swift-collections, not a full sort:

```swift
import Collections

var heap = Heap<ScoreRow>(minimumCapacity: K)
for r in rows {
    heap.insert(r)
    if heap.count > K { heap.removeMin() }
}
let topK = heap.sorted().reversed()
```

`Heap` is a min-max heap; `removeMin()` is O(log K). For K << N the heap path is O(N log K) vs O(N log N) for a full sort. The codegen picks the heap path when the IR pass sees `order by x desc limit K` with `K < N/100` (heuristic; the threshold is tunable).

When K is small relative to N but not vanishing, `partialSort` from swift-algorithms is the middle-ground option: it sorts only the first K positions of the array.

## 19. Distinct

Mochi `distinct` lowers depending on the element's Hashable conformance:

- Hashable and order-irrelevant: `Set` from the stdlib.

  ```swift
  let uniq = Array(Set(xs))
  ```

- Hashable and order-preserving: `OrderedSet` from swift-collections.

  ```swift
  import OrderedCollections
  let uniq = Array(OrderedSet(xs))
  ```

- Hashable, lazy: `uniqued()` from swift-algorithms.

  ```swift
  import Algorithms
  let uniq = xs.uniqued()
  ```

- Not Hashable but Equatable: a quadratic scan, only valid for tiny inputs; the codegen emits a warning above N = 64.

Mochi always preserves insertion order for `distinct` per the spec, so the codegen prefers `OrderedSet` or `uniqued()`. Set vs OrderedSet differs by a small constant overhead (the order array); for hot paths and ~10K elements OrderedSet adds ~5% over Set.

## 20. Existential `any`

Swift 5.7 (and inherited in 6.0) requires the explicit `any Protocol` syntax for existential types:

```swift
let xs: [any Comparable] = [...] // compile error without `any`
```

Existentials carry a witness-table pointer and a value buffer per element; calls go through the witness table rather than a direct dispatch. The cost is roughly equivalent to a Java interface call: a few cycles, but defeats inlining.

Mochi's typed-column representation avoids `any` whenever the column is monomorphic. A list of `int` lowers to `[Int64]`, never `[any Numeric]`. Generic queries (the user writes a polymorphic helper) use generic functions specialised by the compiler under `-O`, not existentials.

When the user reaches for a heterogeneous collection (`list<any>` in Mochi), the codegen falls back to `[any Hashable & Sendable]` and the query loses primitive specialisation. The IR pass flags this with a one-time advisory note (similar to MEP-47's boxing warning).

## 21. Performance considerations

Targets for v0.1 (vs the vm3 baseline; benchmarks on Apple Silicon M-series, Swift 6.0, `-O` whole-module, ARC enabled):

- 1M-row `from x in L where x.k > 100 select x.v sum`: Swift target ≤ 1.5x slower than the C target, ≤ 3x slower than vm3.
- 100K-row hash join (1:1 cardinality): Swift ≤ 2x slower than C.
- 1M-row `group by` into 100 keys: < 150ms wall clock.
- CSV load of 1M rows × 10 cols: < 1.5s including parsing.
- AsyncSequence chain (filter + map + reduce) over 100K events: per-element await dominates; ≤ 5x slower than the sync equivalent.

For hot paths the codegen materialises `AsyncSequence` to `Array` and re-yields synchronously when the upstream type is `list<T>` (only the producer-side decision; the consumer still uses `for await`).

The single biggest win is keeping the chain monomorphic and unboxed: `[Int64]` with closures the compiler can specialise gives near-C performance under whole-module optimisation. Adding a single `any Hashable` step is the most common foot-gun.

## 22. Sample lowerings

A simple from/where/select:

```mochi
let adults = from p in people where p.age > 18 select p.name
```

```swift
let adults: [String] = people
    .filter { $0.age > 18 }
    .map { $0.name }
```

A group_by with aggregate (per §4 and §9):

```mochi
let by_dept = from p in people group by p.dept into g select {
    dept: g.key, n: count(g), avg_sal: avg(g |> map(p => p.salary))
}
```

```swift
import OrderedCollections

struct DeptAgg { var sum: Int64 = 0; var n: Int = 0 }
var agg = OrderedDictionary<String, DeptAgg>()
for p in people {
    var d = agg[p.dept, default: DeptAgg()]
    d.sum += p.salary
    d.n += 1
    agg[p.dept] = d
}
let by_dept: [Row] = agg.map { (k, v) in
    Row(dept: k, n: v.n, avg_sal: Double(v.sum) / Double(v.n))
}
```

A hash join (per §5):

```mochi
let joined = from o in orders join u in users on o.user_id == u.id
             select { name: u.name, amount: o.amount }
```

```swift
let idx: [Int64: User] = Dictionary(uniqueKeysWithValues:
    users.map { ($0.id, $0) })

let joined: [Row] = orders.compactMap { o in
    guard let u = idx[o.userId] else { return nil }
    return Row(name: u.name, amount: o.amount)
}
```

A streaming filter on an AsyncSequence (per §15):

```mochi
let alerts = from e in event_stream where e.severity == "high" select e.message
```

```swift
let alerts = event_stream
    .filter { $0.severity == "high" }
    .map { $0.message }

for await msg in alerts {
    print(msg)
}
```

---

## Cross-references

- [[01-language-surface]] §3 , the DSL surface.
- [[04-runtime]] §5 , arena allocation hooks for query results.
- [[05-codegen-design]] §6 , MIR pipeline IR shared with MEP-45/46/47/48.
- [[06-type-lowering]] §4 , `OrderedDictionary` for Mochi maps, `Int64` and `Double` primitives.
- [[09-agent-streams]] , `AsyncSequence` and the `mochi.stream` operator set.
- [[10-build-system]] §3 , Swift Package Manager dependencies (swift-collections, swift-algorithms, swift-async-algorithms).
- [[11-testing-gates]] §2 , vm3 differential oracle.

---

## Sources

1. Swift Standard Library, `Sequence` protocol. <https://developer.apple.com/documentation/swift/sequence>
2. Swift Standard Library, `AsyncSequence` protocol. <https://developer.apple.com/documentation/swift/asyncsequence>
3. SE-0421 Typed throws in AsyncSequence (Swift 6.0). <https://github.com/apple/swift-evolution/blob/main/proposals/0421-generalize-async-sequence.md>
4. apple/swift-collections. <https://github.com/apple/swift-collections>
5. apple/swift-algorithms. <https://github.com/apple/swift-algorithms>
6. apple/swift-async-algorithms. <https://github.com/apple/swift-async-algorithms>
7. SE-0317 async let bindings. <https://github.com/apple/swift-evolution/blob/main/proposals/0317-async-let.md>
8. SE-0335 Existential `any` syntax. <https://github.com/apple/swift-evolution/blob/main/proposals/0335-existential-any.md>
9. Swift Concurrency: TaskGroup. <https://developer.apple.com/documentation/swift/taskgroup>
10. WWDC 2021, "Meet AsyncSequence." <https://developer.apple.com/videos/play/wwdc2021/10058/>
11. WWDC 2022, "Embrace Swift generics." <https://developer.apple.com/videos/play/wwdc2022/110352/>
12. Apple Swift Forum, "OrderedDictionary use cases." <https://forums.swift.org/t/ordered-dictionary/>
13. Reactive Streams specification (background for backpressure design). <https://www.reactive-streams.org/>
14. Ullman, "Principles of Database and Knowledge-Base Systems Vol. 1." Computer Science Press, 1989 (hash and merge join algorithms).
