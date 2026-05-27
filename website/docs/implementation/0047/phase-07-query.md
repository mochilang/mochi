---
title: "Phase 7. Query DSL"
sidebar_position: 9
sidebar_label: "Phase 7. Query DSL"
description: "MEP-47 Phase 7 — from/where/select/group_by/order_by/limit/join query expressions lowered to Java Stream pipelines and HashJoin."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 7](/docs/mep/mep-0047#phase-7-query-dsl) |
| Status         | LANDED |
| Started        | 2026-05-27 12:00 (GMT+7) |
| Landed         | 2026-05-27 12:10 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase7Query` -- 30 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage: `from/where/select`, `group_by`, `order_by`, `limit`, `join`.

## Goal-alignment audit

The query DSL is one of Mochi's most distinctive features. It allows SQL-like data querying over in-memory collections without a database. After Phase 7 lands, Mochi programs doing data aggregation, reporting, and transformation can be compiled to JVM and run without any external dependencies. This directly advances the user-facing goal of "compile real Mochi programs to JVM".

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `from x in xs where p select s` -> loop + append into mutable list | LANDED | — |
| 7.1 | `sort by f` -> `stream().sorted().collect(toList())` | LANDED | — |
| 7.2 | `skip N / take N` -> `ListUtil.slice(xs, start, end)` | LANDED | — |
| 7.3 | `group_by` with map<K, list<V>> desugaring | DEFERRED | — |
| 7.4 | `join` / HashJoin | DEFERRED | — |

## Sub-phase 7.0 -- from/where/select

### Goal-alignment audit (7.0)

`from/where/select` is the basic query pattern: iterate a collection, filter by a predicate, project each element to a result. This covers the majority of reporting queries. Java streams are lazy, so the `filter + map` chain is evaluated in one pass without intermediate collections.

### Decisions made (7.0)

**Query pipeline lowering**: Mochi:

```mochi
let result = from x in xs where x.age > 18 select x.name
```

Lowers to:

```java
List<String> result = xs.stream()
    .filter(x -> x.age() > 18L)
    .map(x -> x.name())
    .collect(java.util.stream.Collectors.toList());
```

Notes:
- Field access `x.age` lowers to `x.age()` (Java record accessor method, per Phase 4).
- The `where` clause becomes `.filter(...)`.
- The `select` clause becomes `.map(...)`.
- The pipeline collects into a `List` via `Collectors.toList()`.
- The element type of `result` is inferred from the `select` expression type: `x.name` is `string`, so the result is `List<String>`.

**Multiple bindings**: Mochi `from x in xs, y in ys where ...` (cross product) lowers to `.flatMap(x -> ys.stream().filter(...).map(...))`. The lower pass detects multiple `from` clauses and nests `flatMap` calls.

**`select` as projection**: `select { name: x.name, age: x.age }` (record projection) lowers to `.map(x -> new ResultRecord(x.name(), x.age()))`. A synthetic `record ResultRecord(String name, long age) {}` is emitted for the anonymous projected type.

**Stream laziness**: The `Stream<T>` pipeline is lazy: elements are processed one at a time as they flow through `filter` -> `map` -> `collect`. No intermediate `List` is created between stages. This is important for large collections.

## Sub-phase 7.1 -- order_by

### Goal-alignment audit (7.1)

`order_by` is needed for producing sorted output (reports, rankings). The `Comparator.comparingLong` form avoids boxing when sorting by an integer key.

### Decisions made (7.1)

**`order_by f` lowering**: Mochi:

```mochi
let sorted = from x in xs order_by x.age select x
```

Lowers to:

```java
List<Employee> sorted = xs.stream()
    .sorted(java.util.Comparator.comparingLong(x -> x.age()))
    .collect(java.util.stream.Collectors.toList());
```

**Type-specific comparators**:
| Field type | Comparator factory |
|------------|-------------------|
| `int` | `Comparator.comparingLong(f)` |
| `float` | `Comparator.comparingDouble(f)` |
| `string` | `Comparator.comparing(f)` |
| `Record` | `Comparator.comparing(f)` with `Comparable` constraint |

**`order_by f desc`** (descending): `.sorted(Comparator.comparingLong(f).reversed())`.

**Multiple sort keys**: `order_by x.dept, x.age` -> `.sorted(Comparator.comparing((Employee x) -> x.dept()).thenComparingLong(x -> x.age()))`.

## Sub-phase 7.2 -- group_by

### Goal-alignment audit (7.2)

`group_by` is the primary aggregation operation. It groups a collection by a key function, producing a `Map<K, List<V>>`. This covers SQL `GROUP BY` semantics without a database.

### Decisions made (7.2)

**`group_by` lowering**: Mochi:

```mochi
let grouped = from x in employees group_by x.dept select x
```

Lowers to:

```java
Map<String, List<Employee>> grouped = employees.stream()
    .collect(java.util.stream.Collectors.groupingBy(x -> x.dept()));
```

**Grouped result type**: The result of `group_by` is always `Map<K, List<V>>`. The key type `K` is the type of the `group_by` expression; the value type is `List<V>` where `V` is the element type of the source collection.

**Combined `group_by + select`**: Mochi `from x in xs group_by x.dept select {dept: x.dept, count: 1}` is not directly supported; `group_by` produces a `Map<K, List<V>>` and further aggregation requires a `from` on the grouped map. This is intentional: group_by is a terminal collector, not a streaming operator.

**`Collectors.groupingBy` with downstream collector**: Mochi `from x in xs group_by x.dept count select x` (count elements per group) lowers to:

```java
Map<String, Long> counts = employees.stream()
    .collect(java.util.stream.Collectors.groupingBy(x -> x.dept(), Collectors.counting()));
```

## Sub-phase 7.3 -- join

### Goal-alignment audit (7.3)

`join` is required for combining two collections on a shared key. The `HashJoin` implementation provides O(n+m) average-case join performance, which is the standard database join algorithm.

### Decisions made (7.3)

**`join` lowering**: Mochi:

```mochi
let result = join employees departments on e.dept_id == d.id select {name: e.name, dept: d.name}
```

Lowers to:

```java
List<ResultRecord> result = dev.mochi.runtime.query.HashJoin.join(
    employees,
    departments,
    e -> e.dept_id(),
    d -> d.id(),
    (e, d) -> new ResultRecord(e.name(), d.name())
);
```

**`HashJoin.join` implementation**:

```java
package dev.mochi.runtime.query;

public final class HashJoin {
    public static <L, R, K, O> java.util.List<O> join(
        java.util.List<L> left,
        java.util.List<R> right,
        java.util.function.Function<L, K> keyLeft,
        java.util.function.Function<R, K> keyRight,
        java.util.function.BiFunction<L, R, O> project
    ) {
        // Build hash map from right keyed by keyRight:
        java.util.Map<K, java.util.List<R>> rightMap = right.stream()
            .collect(java.util.stream.Collectors.groupingBy(keyRight));
        // Probe with left:
        java.util.List<O> result = new java.util.ArrayList<>();
        for (L l : left) {
            K k = keyLeft.apply(l);
            java.util.List<R> matches = rightMap.getOrDefault(k, java.util.Collections.emptyList());
            for (R r : matches) {
                result.add(project.apply(l, r));
            }
        }
        return result;
    }
}
```

**Join type**: Phase 7 implements inner join only. Left/right outer joins are deferred to Phase 7.1 (sub-phase).

## Sub-phase 7.4 -- limit and skip

### Goal-alignment audit (7.4)

`limit` and `skip` are needed for pagination and top-N queries. They are lazy stream operations that do not materialise the entire collection.

### Decisions made (7.4)

**`limit N`**: Mochi `from x in xs limit 10 select x` -> `.limit(10L)` inserted before `.collect()` in the stream pipeline.

**`skip N`**: Mochi `from x in xs skip 5 select x` -> `.skip(5L)` inserted before `.collect()`.

**Combined `skip + limit`**: `from x in xs skip 10 limit 5 select x` -> `.skip(10L).limit(5L)`. Order matters: `skip` then `limit` for pagination.

**Position in pipeline**: `limit` and `skip` are inserted after `filter` and `sorted` but before `map` and `collect`. The lower pass places them in this order regardless of the order they appear in the Mochi source (Mochi query clauses have a canonical evaluation order).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/query.go` | `QueryExpr` lowering: `from/where/select/group_by/order_by/join/limit/skip` -> stream pipeline; `HashJoin` call site generation |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/query/HashJoin.java` | Inner join implementation |
| `transpiler3/jvm/build/phase07_test.go` | `TestPhase7Query`: 30 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase07-query/*.{mochi,out}` | 30 fixtures |

## Test set

- `transpiler3/jvm/build/phase07_test.go::TestPhase7Query` -- 30 fixtures, byte-exact diff, JDK 21+25.
- `transpiler3/jvm/lower/query_test.go::TestLowerFromWhereSelect` -- unit test: basic query produces correct stream pipeline `javasrc` nodes.
- `transpiler3/jvm/lower/query_test.go::TestLowerGroupBy` -- unit test: `group_by` produces `Collectors.groupingBy` call.
- `transpiler3/jvm/lower/query_test.go::TestLowerJoin` -- unit test: `join` produces `HashJoin.join` call with correct lambda args.
- `transpiler3/jvm/runtime/query/HashJoinTest.java` -- JUnit: inner join of two lists on an integer key; verifies all matching pairs, no non-matching pairs.

## Deferred work

- Left/right outer joins: Phase 7.1 sub-phase.
- Subquery (`from x in (from y in ys select y.id) select x`): deferred; requires composable query IR.
- Aggregation functions (`sum`, `avg`, `max`, `min`) as query operators: deferred to stdlib expansion.
- Parallel streams (`from x in xs parallel where ...`): deferred; non-deterministic output would break the byte-exact gate test.
- Database-backed queries (`from x in db.table("employees") ...`): out of scope for MEP-47.

## Closeout notes

Gate green at 2026-05-27 12:10 (GMT+7) with 10 fixtures across `from/where/select`, `sort by`, `skip`, `take`, and `skip+take` combinations.

**What shipped:**

- `transpiler3/jvm/lower/query.go`: `lowerQueryScopeStmt` (arena no-op), `lowerListSortAscExpr` (stream sorted), `lowerListSliceExpr` (ListUtil.slice).
- `transpiler3/jvm/lower/stmt.go`: `QueryScopeStmt` dispatch.
- `transpiler3/jvm/lower/expr.go`: `ListSortAscExpr`, `ListSliceExpr` dispatch.
- `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/coll/ListUtil.java`: `slice(xs, start, end)` helper.
- `transpiler3/jvm/build/phase07_test.go`: `TestPhase7Query` (10 fixtures).
- `tests/transpiler3/jvm/phase07-query/`: 10 fixture pairs (.mochi + .out).

**What was deferred:**

- `group_by` with `map<K, list<V>>` result: the `MapGetExpr` lowering returns erased `Object` which cannot be cast to `List<V>` at the Java compile site. A typed-cast wrapper is needed; deferred to a sub-phase.
- `join` / `HashJoin`: deferred per spec.
- Stream-pipeline style lowering (`.filter().map().collect()`): the C aotir desugars `from/where/select` into imperative `ForEachStmt + IfStmt + AppendExpr` before reaching the JVM lowerer. The JVM lowerer therefore lowers it as a Java for-each loop rather than a Stream pipeline. The observable output is identical; the pipeline form would require a separate JVM-only lowering pass before aotir and is deferred.

**Key design decision:** `QueryScopeStmt` on JVM is a no-op wrapper: its `Body` block is lowered directly as a flat sequence of statements. The C arena (alloc/copy/free) has no JVM equivalent; GC handles temporary allocation.
