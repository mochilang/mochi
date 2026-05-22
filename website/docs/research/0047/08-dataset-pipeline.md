# MEP-47 research note 08, Mochi query DSL and dataset pipeline on JVM

Author: research pass for MEP-47.
Date: 2026-05-23 01:00 (GMT+7).

This note covers Mochi's LINQ-style query DSL (`from ... in ... where ... select ...`), Datalog facts and rules, group_by, joins (hash and indexed), stream-aware queries, CSV/JSON load and save adapters, and how each lowers onto the JVM. Companion notes: [[05-codegen-design]], [[06-type-lowering]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]].

The JVM target leans on `java.util.stream` (introduced JDK 8, mature on JDK 21 LTS), `java.util.concurrent.Flow` (JEP 266, JDK 9), and the upgraded `Collectors` and pattern-matching surface that JDK 21 makes available. We assume JDK 21 LTS as the floor and JDK 25 LTS as the upper bound for tooling decisions.

---

## 1. Mochi query surface recap

Mochi inherits a LINQ-style query DSL (see [[01-language-surface]] §3). Examples:

```mochi
let adults     = from x in xs where x.age > 18 select x.name
let by_dept    = from p in people group by p.dept into g select { dept: g.key, n: count(g) }
let joined     = from o in orders join u in users on o.user_id == u.id select { u.name, o.amount }
let top10      = from p in people order by p.score desc limit 10 select p.name
let page       = from p in posts order by p.ts desc offset 20 limit 10 select p
let all_tags   = (from p in posts select p.tag) union (from c in comments select c.tag)
```

Surface clauses supported per the language docs:

- `from x in coll` (single-source iteration).
- `join y in coll2 on x.k == y.k` (inner equi-join; default hash, nested-loop fallback when keys are non-hashable).
- `group by key into g` (groups rows; binds `g` to the materialised group).
- `where pred` (filter).
- `order by k1, k2 desc` (sort; default ascending; stable on ties).
- `limit N`, `offset M` (window).
- `select expr` (terminal projection).
- `union`, `intersect`, `except` between two queries.

All of these compile to JVM bytecode, but the choice of `Stream` operator, `Collector`, or hand-rolled fold varies by clause.

## 2. JVM Stream API surface

`java.util.stream.Stream<T>` is a lazy, sequential pipeline of operations terminated by a collector. The relevant surface we lean on:

### 2.1 Sources

- `Collection.stream()` for any `java.util.List`, `Set`, or other Collection.
- `Arrays.stream(T[])` for arrays.
- `Stream.of(T...)` for varargs literals.
- `Stream.generate(Supplier)`, `Stream.iterate(seed, fn)` for infinite generators (not used by Mochi queries; would only appear in streams from [[09-agent-streams]]).
- `Files.lines(Path)` for line-oriented file ingest (used by the CSV load path before parsing).

### 2.2 Intermediate operations

`filter`, `map`, `flatMap`, `sorted`, `distinct`, `peek`, `limit`, `skip`. All are lazy. The pipeline is realised only when a terminal op fires.

### 2.3 Terminal operations

`collect`, `forEach`, `reduce`, `count`, `findFirst`, `findAny`, `anyMatch`, `allMatch`, `noneMatch`, `toList` (JDK 16+, returns an unmodifiable list, GA on JDK 21).

### 2.4 Primitive specialisations

`IntStream`, `LongStream`, `DoubleStream`. Mochi `int` is 64-bit; we lower to `LongStream` to avoid `Long` boxing. Mochi `float` is 64-bit; lower to `DoubleStream`. There is no `BoolStream` or `CharStream`; Mochi `bool` collections stay as `Stream<Boolean>` (one of the few unavoidable boxing sites).

### 2.5 Parallel streams

`.parallel()` switches the pipeline to the common `ForkJoinPool` (`ForkJoinPool.commonPool()`). Heuristically only worth it for input size in the millions and ops that are associative + stateless. Mochi v0.1 does not auto-parallelise; user-visible `par` hint is a phase 2 question (see §4).

## 3. Mochi → Stream lowering recipe, per clause

### 3.1 `from x in xs where pred select expr`

The bread-and-butter case lowers to:

```java
List<R> out = xs.stream()
                .filter(x -> pred(x))
                .map(x -> expr(x))
                .toList();
```

For `list<long>` sources where the predicate and projection only use primitive long arithmetic, the codegen emits `LongStream` instead:

```java
long[] out = xs.stream()
               .mapToLong(Long::longValue)   // unbox once if source is List<Long>
               .filter(x -> x > 18)
               .map(x -> x * 2)
               .toArray();
```

When the source itself is already `long[]`, we start with `Arrays.stream(arr)` to skip the unbox entirely.

### 3.2 `select` only (identity-filter)

If there is no `where`, the codegen skips the `.filter` node. Stream API will fuse `map` alone into a single pass.

### 3.3 `join`

Naive nested-loop:

```java
List<R> out = xs.stream()
    .flatMap(x -> ys.stream()
                    .filter(y -> x.id() == y.fk())
                    .map(y -> project(x, y)))
    .toList();
```

This is O(N × M) and unacceptable for any non-trivial join. The MEP-47 codegen prefers a **hash join**: pre-build a `Map<K, List<V>>` of the inner side, then probe:

```java
Map<Long, List<User>> idx = users.stream()
    .collect(Collectors.groupingBy(User::id));

List<R> out = orders.stream()
    .flatMap(o -> idx.getOrDefault(o.userId(), List.of()).stream()
                     .map(u -> new R(u.name(), o.amount())))
    .toList();
```

For unique join keys we use `Collectors.toMap(User::id, Function.identity())` and `idx.get(o.userId())` directly without `flatMap`. The IR pass distinguishes the unique vs many case from the join key's declared type or annotation.

For ETS-like indexed sources (Datalog fact tables, see §7), the codegen skips the build step and goes straight to lookups against the existing index.

### 3.4 `group by`

```java
Map<String, List<Person>> g = people.stream()
    .collect(Collectors.groupingBy(Person::dept));
```

When the select clause uses `count(g)` or other reducing aggregates without touching the full group, we use a downstream collector to avoid materialising the list:

```java
Map<String, Long> counts = people.stream()
    .collect(Collectors.groupingBy(Person::dept, Collectors.counting()));
```

The MIR pass (see [[05-codegen-design]] §6) classifies aggregates as "fold-only" (count, sum, min, max, avg) and selects the corresponding `Collectors.counting`, `summingLong`, `minBy`, `maxBy`, `averagingDouble`.

### 3.5 `order by`

```java
List<Person> sorted = people.stream()
    .sorted(Comparator.comparingInt(Person::score).reversed())
    .toList();
```

Multi-key order chains comparators with `.thenComparing(...)`. The codegen flips to `Comparator.reverseOrder()` for `desc` on top-level key. Stable sort: Stream API's `sorted` uses TimSort (stable) under the hood.

### 3.6 `limit` / `offset`

```java
List<Person> page = people.stream().skip(20).limit(10).toList();
```

`skip` followed by `limit` is the canonical paging shape; both are short-circuiting when possible. Note: on parallel streams `skip` is **ordered** and effectively serialises a prefix; the codegen rejects `parallel + skip` for query lowering.

### 3.7 `union`, `intersect`, `except`

Set vs bag semantics need to match Mochi's spec. The Mochi language docs treat collection union as set-union by default (deduplicating); MEP-47 follows that:

```java
List<R> u = Stream.concat(a.stream(), b.stream()).distinct().toList();
```

For multiset (bag) union, `union all`:

```java
List<R> u = Stream.concat(a.stream(), b.stream()).toList();
```

`intersect`:

```java
Set<R> rs = Set.copyOf(b);
List<R> out = a.stream().filter(rs::contains).distinct().toList();
```

`except`:

```java
Set<R> rs = Set.copyOf(b);
List<R> out = a.stream().filter(x -> !rs.contains(x)).distinct().toList();
```

The smaller-build heuristic (build the set on the smaller side) applies when both sides are materialised; the planner picks the smaller side at compile time using size annotations, or falls back to the literal source-order build.

## 4. Optimisations

### 4.1 Pipeline fusion

Stream API already fuses `filter + map + filter + map` into a single traversal at the JIT level: each element flows through the chain of `Sink` objects without intermediate buffering. The Mochi codegen does not need to do its own fusion at the IR level for streaming ops. C2 inlines the lambda call sites after a few thousand invocations, and the resulting machine code is competitive with hand-rolled loops on JDK 21 ([JEP 416](https://openjdk.org/jeps/416), [JEP 458](https://openjdk.org/jeps/458) help on the launch path).

### 4.2 Short-circuiting

`findFirst`, `findAny`, `anyMatch`, `allMatch`, `noneMatch`, and `limit` all short-circuit. Mochi `first(...)` lowers to `.findFirst().orElse(null)` (or `.findFirst().get()` when the type system has proved non-empty). Mochi `exists(p in xs where pred)` lowers to `.anyMatch(p -> pred(p))`.

### 4.3 Primitive specialisation

This is the single largest perf knob. The lowering pass tracks the element type from the source down through each op; if the type is `long`, `double`, or `int` and no map step widens to a reference type, the entire pipeline stays in `LongStream` / `DoubleStream`. Boxing on a 10M-element stream costs ~150ms of allocation overhead; primitive specialisation eliminates it.

### 4.4 Parallel

`.parallel()` is correct only when the operation is associative and stateless. Mochi has no `par` query hint in v0.1; the codegen always emits sequential streams. Adding one in v0.2 is plausible:

```mochi
let totals = from p in people parallel group by p.dept into g select { dept: g.key, n: count(g) }
```

Defer to the spec body for the keyword choice. Out of scope for note 08.

### 4.5 Avoid `Stream` when a loop is clearer

For tiny inputs (size known at compile time, < 8) the codegen falls back to a plain `for` loop over an array literal. Stream pipeline setup costs ~50ns; for an 8-element list that is a 100% overhead vs an unrolled loop. The threshold lives in the codegen options.

## 5. Collectors catalogue

The MEP-47 codegen emits one of the following collectors per terminal op:

| Mochi                                  | Collector                                           |
|----------------------------------------|-----------------------------------------------------|
| `select expr` (list result)            | `Collectors.toUnmodifiableList()` (JDK 10+)         |
| `select expr` (mutable list result)    | `Collectors.toList()`                               |
| `select { key: ..., value: ... }` map  | `Collectors.toUnmodifiableMap(k, v)`                |
| `distinct select expr`                 | `Collectors.toUnmodifiableSet()`                    |
| `group by k into g`                    | `Collectors.groupingBy(k, downstream)`              |
| `count`                                | `Collectors.counting()`                             |
| `sum`                                  | `Collectors.summingLong` / `summingDouble`          |
| `avg`                                  | `Collectors.averagingDouble`                        |
| `string join`                          | `Collectors.joining(sep, prefix, suffix)`           |
| boolean partition                      | `Collectors.partitioningBy(p, downstream)`          |

`toList()` (terminal, JDK 16+) is preferred over `Collectors.toUnmodifiableList()` for hot paths: it skips the collector indirection and returns an unmodifiable list directly. The codegen emits `toList()` when JDK 21+ is the floor (which MEP-47 mandates).

## 6. Indexed and materialised inputs

Mochi sources arrive in a few shapes:

- **`list<T>` already in memory**. Direct `.stream()` on the backing `java.util.List`.
- **`map<K, V>` insertion-ordered**. `.entrySet().stream()` yields `Map.Entry<K, V>`. Mochi `from kv in m` exposes `kv.key` and `kv.value`. Mochi's map iteration order is insertion order, which matches `LinkedHashMap`'s contract; we use `LinkedHashMap` as the runtime representation (see [[06-type-lowering]] §4).
- **`load "data.csv"` dataset**. See §10. Lowering produces a `List<T>` materialised in memory, then queries treat it as a list source.
- **Datalog fact relation**. See §7. Each relation is a `Map<Predicate, Set<Tuple>>` (or per-predicate typed `Set<TupleR>`); query lowering uses the relation's index API directly.
- **Mochi `stream<T>`**. See §9. Distinct from Java `Stream<T>`; this is a `Flow.Publisher<T>` at runtime, and queries over streams use the `mochi.flow` operator library, not Stream API.

## 7. Datalog evaluation strategy

Mochi `fact` and `rule` declarations are a separate sub-DSL. Example:

```mochi
fact parent("alice", "bob")
fact parent("bob", "charlie")
rule ancestor(X, Y) :- parent(X, Y)
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)
```

JVM-canonical implementation:

- Each fact relation is a `HashMap<PredicateId, Set<Tuple>>` keyed by ground-tuple structural equality. For predicates with declared schemas (the recommended case), the relation is a `Set<TupleR>` where `TupleR` is a codegen-emitted `record`.
- Rules compile to a Datalog **semi-naive evaluator** (Ullman 1989; Bancilhon & Ramakrishnan 1986). Same algorithm as MEP-45/46.
- The evaluator is the `mochi.dl` module (`mochi.dl.Engine`); rules register at module load and queries call `engine.query(predicateId, bindings)`.
- Intermediate deltas live in per-iteration `HashSet<Tuple>` scratch sets, dropped at fixpoint.

Stratified negation, no recursion through negation; the front end rejects programs that violate stratification (see [[01-language-surface]] §6).

For workloads beyond the in-memory range (>1M facts), MEP-47.1 will explore an embedded JDBC backend (H2, SQLite-JDBC). Direct JDBC in `mochi.query` is out of scope for v0.1; deferred (see §14).

## 8. Tuple representation for Datalog

Two options:

- **Option A**: `record Tuple(Object[] vals)` with structural `equals`/`hashCode`. Generic, no codegen per predicate, but every numeric column boxes.
- **Option B**: per-predicate typed records emitted by codegen, e.g. `record TupleParent(String parent, String child) {}`. No boxing for primitive columns; record `equals`/`hashCode` are JIT-friendly (JEP 395, GA on JDK 16).

MEP-47 picks **Option B** for declared-schema predicates. Option A is the fallback for the dynamic-Datalog escape hatch (`fact pred(...args)` without a declared schema), with a Dialyzer-equivalent warning that the path will box.

The codegen emits one `record` per predicate inside the generated module; equality and hash come for free via the record contract. Tuples are immutable, which simplifies the semi-naive evaluator's worklist sharing.

## 9. Stream API integration with Mochi streams

This is the most subtle part of the note. Two things share the word "stream" and they mean different things:

- **`java.util.stream.Stream<T>`** is a lazy in-memory pipeline of operations. It is consumed once, has no notion of time, and exists for the duration of the terminal op.
- **`mochi stream<T>`** is a runtime construct: a publisher that delivers events over time, possibly forever. It maps onto `java.util.concurrent.Flow.Publisher<T>` (see [[09-agent-streams]]).

A Mochi query over a `list<T>` lowers to Java `Stream<T>`. A Mochi query over a `stream<T>` does not. Instead:

1. The pipeline lowers to a chain of `Flow.Processor` operators in the `mochi.flow` module.
2. The terminal `select` becomes either a list-buffer subscriber (for `into list`), a file sink (for `into file`), or another `stream<T>` (when the result is itself a stream).
3. Blocking operators (`group by`, `order by`, full `distinct`) are illegal on unbounded streams; the type checker rejects them. Windowed equivalents (`window 60s group by ...`) are the supported form, deferred to [[09-agent-streams]].

**Bridging**:

- `from x in list_a join y in stream_b` would need to buffer the stream side; we reject it. The user must explicitly buffer with `let buf = stream_b |> collect()` first.
- `from x in stream_a select x.field` is fine; the projection runs per event.

**Decision**: Mochi v0.1 ships a minimal in-tree `mochi.flow` operator set (`filter`, `map`, `mapAsync`, `window`, `merge`, `collect`). We reject Project Reactor and RxJava as default dependencies. They are excellent libraries, but the dependency surface (Reactor: 10+ artifacts; RxJava: 1 artifact but 1.7MB) is more than MEP-47 wants to take on. Optional integration via FFI is left to user code (see [[10-build-system]] §7).

## 10. Load adapters (CSV, JSON, JSONL, YAML)

The language supports `load "people.json"` with format inferred from extension, or `load "people.csv" as Person`. JVM library choices:

- **JSON**: `com.fasterxml.jackson.databind.ObjectMapper`. Industry standard. Fast (Jackson Afterburner module pushes throughput close to DSL-JSON levels). 4MB jar; acceptable.
- **CSV**: `com.fasterxml.jackson.dataformat.csv.CsvMapper`. Same Jackson umbrella; reuses the dispatch infrastructure.
- **JSONL**: `ObjectMapper.readValues(Reader)` with `MappingIterator`; one object per line.
- **YAML**: `com.fasterxml.jackson.dataformat.yaml.YAMLFactory`. SnakeYAML 2.x is the underlying parser.

Lowering shape:

```java
List<Person> people = MochiLoad.json("people.json", Person.class);
```

`MochiLoad.json` is in the `mochi.io` runtime module; it wraps `ObjectMapper.readValue(File, Class)` with Mochi-style error handling and arena allocation hooks (see [[04-runtime]] §5). Per-record-type wrappers (`MochiLoad.csv(Path, Class<T>)`) are generated by the codegen as static methods of the module class.

For `load PATH with { schema: T, header: true, ... }`, the options struct generates a `CsvSchema.Builder` chain at codegen time.

## 11. Save adapters

`save xs to "out.csv"` is the dual:

```java
MochiSave.csv("out.csv", xs, Person.class);
```

`MochiSave.csv` uses `CsvMapper.writer(schema)` and writes the list. `MochiSave.json` uses `ObjectMapper.writeValue(File, Object)`. `MochiSave.jsonl` writes one `objectMapper.writeValueAsString(...)` per line, separated by `\n`. UTF-8 by default, no BOM.

For large outputs (>100MB), the codegen switches to a streaming writer (`SequenceWriter`) to avoid materialising the full JSON string in memory.

## 12. Performance budget

Targets for v0.1 (vs the vm3 baseline, on a modern laptop, JDK 21 with default GC = G1):

- 1M-row `from x in L where x.k > 100 select x.v sum`: JVM target ≤ 2x slower than the C target, ≤ 4x slower than vm3.
- 100K-row hash join (1:1 cardinality): JVM ≤ 2x slower than C.
- 1M-row `group by` into 100 keys: < 200ms wall clock.
- Datalog `ancestor` over 10K parent facts, 5 levels deep: < 100ms.
- CSV load of 1M rows × 10 cols: < 2s including parsing.

These are aspirational. The gates in [[11-testing-gates]] are differential against vm3 for correctness; performance gates are separate benchmarks that do not block landings but are tracked over time.

## 13. Differential testing

Same shape as MEP-45/46. Every query in the fixture corpus runs on:

1. vm3 (the recording oracle, which produces the canonical stdout).
2. The MEP-47 JVM target.

A test passes when byte-equal stdout matches. The fixture corpus includes:

- `tests/vm/valid/from_*.mochi` (single-source queries).
- `tests/vm/valid/join_*.mochi` (join shapes).
- `tests/vm/valid/group_*.mochi` (group_by).
- `tests/vm/valid/order_limit_*.mochi` (paging).
- `tests/vm/valid/set_ops_*.mochi` (union, intersect, except).
- `tests/vm/valid/datalog_*.mochi` (fact + rule).
- `tests/vm/valid/load_*.mochi` (CSV/JSON adapters; fixture data files alongside).

vm3 is the recording oracle (see [[11-testing-gates]] §2). No JVM-specific gold files; we diff against the vm3 output.

## 14. Explicit rejects

The following are **not** part of MEP-47 v0.1:

- **Apache Spark / Flink / Hadoop / Beam**. Mochi is in-process; distributed query engines are a different product. Users who want Spark can call it via FFI (see [[10-build-system]] §7).
- **jOOQ as the default query backend**. jOOQ is excellent for SQL-flavoured Java, but Mochi's query DSL is its own surface; binding to jOOQ would couple us to its DSL evolution and SQL dialect dispatcher. Optional integration only.
- **Direct JDBC in `mochi.query`**. SQL backends are a separate `mochi.db` module (deferred to MEP-47.2). Users in v0.1 who need a database call JDBC via FFI and feed results into Mochi queries as `list<T>`.
- **Reactive Streams adoption beyond `Flow`**. We use the JDK 9 `Flow` SPI directly. Reactor / RxJava are not default dependencies (see §9).
- **Parallel streams as the default**. Sequential streams are the floor. `.parallel()` is opt-in only and not yet exposed by Mochi syntax in v0.1.
- **Lazy query results**. Mochi queries are eagerly evaluated to `List<T>` unless the result type is `stream<T>`. No LINQ-style deferred enumerators. This matches MEP-45/46.

## 15. Open questions

1. Does `union` default to set-union (deduplicate) or bag-union (preserve duplicates)? The Mochi spec says set-union; confirm in the spec body.
2. Does `group by ... into g` always materialise the group, or only when the select clause demands the full list? Today we always materialise; a streaming-aggregator pass is an obvious optimisation.
3. Should `order by` default to ascending? Yes per LINQ convention, but confirm.
4. Should `limit -n` (negative) error or yield empty? Probably error; the type checker should reject negative literals; runtime checks for computed limits.
5. Should we add a `par` query hint in v0.2 that flips to parallel streams when the input is large enough? Plausible.
6. What is the right threshold for the "Stream is heavier than a loop" tiny-input fallback? Probably 8 elements; benchmark to confirm.
7. Should Datalog support recursion through aggregation (e.g. shortest-path) in v0.1? Probably no; deferred.

## 16. Cross-references

- [[01-language-surface]] §3 , the DSL surface.
- [[04-runtime]] §5 , arena allocation hooks.
- [[05-codegen-design]] §6 , MIR pipeline IR (same shape as MEP-45/46).
- [[06-type-lowering]] §4 , `LinkedHashMap` for Mochi maps, primitive vs boxed long.
- [[09-agent-streams]] `Flow.Publisher` and the `mochi.flow` operator set.
- [[10-build-system]] §7 , FFI surface for Spark / Reactor / jOOQ users.
- [[11-testing-gates]] §2 , vm3 differential oracle.

---

## Sources

1. Java SE 21 `java.util.stream` package documentation. <https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/stream/package-summary.html>
2. Java SE 21 `java.util.concurrent.Flow` documentation. <https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/concurrent/Flow.html>
3. JEP 266: More Concurrency Updates (Flow API). <https://openjdk.org/jeps/266>
4. JEP 395: Records (final). <https://openjdk.org/jeps/395>
5. JEP 416: Reimplement Core Reflection with Method Handles. <https://openjdk.org/jeps/416>
6. JEP 458: Launch Multi-File Source-Code Programs. <https://openjdk.org/jeps/458>
7. Brian Goetz, "Stream Sources." Java Magazine, 2014.
8. Stuart Marks, "Stream Performance." JVM Language Summit 2017.
9. Ullman, "Principles of Database and Knowledge-Base Systems Vol. 1." Computer Science Press, 1989.
10. Bancilhon & Ramakrishnan, "An Amateur's Introduction to Recursive Query Processing Strategies." SIGMOD 1986.
11. Souffle Datalog. <https://souffle-lang.github.io/>
12. Jackson Databind documentation. <https://github.com/FasterXML/jackson-databind>
13. Jackson CSV documentation. <https://github.com/FasterXML/jackson-dataformats-text/tree/master/csv>
14. SnakeYAML 2.x. <https://bitbucket.org/snakeyaml/snakeyaml>
15. Reactive Streams specification. <https://www.reactive-streams.org/>
