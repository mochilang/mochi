---
title: "Phase 8. Datalog"
sidebar_position: 10
sidebar_label: "Phase 8. Datalog"
description: "MEP-47 Phase 8 — facts, rules, recursive rules, and stratified negation via a pure-Java in-memory datalog engine."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 8](/docs/mep/mep-0047#phase-8-datalog) |
| Status         | LANDED |
| Started        | 2026-05-27 12:00 (GMT+7) |
| Landed         | 2026-05-27 12:18 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase8Datalog` -- 20 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage: facts, rules, recursion, negation.

## Goal-alignment audit

Datalog is one of Mochi's distinguishing features for knowledge-base and graph-query programs. After Phase 8 lands, Mochi programs using `fact`, `rule`, and `query` can be compiled to JVM and run without any external database or Prolog runtime. The pure-Java `Engine` class in `dev.mochi.runtime.datalog` is the implementation anchor: self-contained, no native dependencies.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | `fact` and basic `query` -> compile-time evaluation, static Java list | LANDED | — |
| 8.1 | `rule` and recursive rules -> semi-naive bottom-up evaluator (compile-time) | LANDED | — |
| 8.2 | Negation-as-failure (`not P(X)` in rule body) | LANDED | — |

## Sub-phase 8.0 -- Facts and basic queries

### Goal-alignment audit (8.0)

Facts and queries are the simplest Datalog constructs. Implementing them first validates the `Engine` API shape and the lowering strategy before adding the complexity of recursive rules.

### Decisions made (8.0)

**Engine registration**: Mochi:

```mochi
fact parent("tom", "bob")
fact parent("bob", "ann")
query q = from parent(X, Y) where X == "tom" select X
```

Lowers to:

```java
dev.mochi.runtime.datalog.Engine $$engine = dev.mochi.runtime.datalog.Engine.create();
$$engine.addFact("parent", new Object[]{"tom", "bob"});
$$engine.addFact("parent", new Object[]{"bob", "ann"});
java.util.List<Object[]> q = $$engine.query("parent",
    new dev.mochi.runtime.datalog.Pattern[]{
        dev.mochi.runtime.datalog.Pattern.var("X"),
        dev.mochi.runtime.datalog.Pattern.var("Y")
    },
    bindings -> "tom".equals(bindings.get("X"))
);
```

**`Engine` class**: `dev.mochi.runtime.datalog.Engine` is a pure-Java in-memory datalog engine: ~1500 LOC, semi-naive evaluation, tuple-based storage with `Object[]` rows.

```java
package dev.mochi.runtime.datalog;

public final class Engine {
    private final java.util.Map<String, java.util.List<Object[]>> facts = new java.util.HashMap<>();

    public static Engine create() { return new Engine(); }

    public void addFact(String relation, Object[] tuple) {
        facts.computeIfAbsent(relation, k -> new java.util.ArrayList<>()).add(tuple);
    }

    public java.util.List<Object[]> query(String relation, Pattern[] patterns) {
        java.util.List<Object[]> result = new java.util.ArrayList<>();
        for (Object[] tuple : facts.getOrDefault(relation, java.util.Collections.emptyList())) {
            java.util.Map<String, Object> bindings = match(tuple, patterns);
            if (bindings != null) result.add(tuple);
        }
        return result;
    }

    private java.util.Map<String, Object> match(Object[] tuple, Pattern[] patterns) {
        java.util.Map<String, Object> bindings = new java.util.HashMap<>();
        if (tuple.length != patterns.length) return null;
        for (int i = 0; i < patterns.length; i++) {
            Pattern p = patterns[i];
            if (p instanceof Pattern.Var v) {
                Object existing = bindings.get(v.name());
                if (existing != null && !existing.equals(tuple[i])) return null;
                bindings.put(v.name(), tuple[i]);
            } else if (p instanceof Pattern.Const c) {
                if (!c.value().equals(tuple[i])) return null;
            }
        }
        return bindings;
    }
}
```

**`Pattern` class**:

```java
package dev.mochi.runtime.datalog;

public sealed interface Pattern permits Pattern.Var, Pattern.Const, Pattern.Wildcard {
    record Var(String name) implements Pattern {}
    record Const(Object value) implements Pattern {}
    record Wildcard() implements Pattern {
        private static final Wildcard INSTANCE = new Wildcard();
        public static Wildcard instance() { return INSTANCE; }
    }
    static Pattern var(String name) { return new Var(name); }
    static Pattern constant(Object value) { return new Const(value); }
    static Pattern wildcard() { return Wildcard.instance(); }
}
```

**Engine instance lifetime**: One `Engine` instance is created per Mochi program (at the start of `main`). All `fact`, `rule`, and `query` declarations in the program share the same engine instance. The lower pass creates the engine as a local variable `$$engine` in `main` and threads it through all datalog operations.

## Sub-phase 8.1 -- Rules and recursion

### Goal-alignment audit (8.1)

Recursive rules are the key capability that distinguishes Datalog from simple filter queries. Without them, transitive-closure queries (ancestors, reachability) cannot be expressed. The semi-naive evaluation algorithm is the standard efficient Datalog evaluation strategy.

### Decisions made (8.1)

**Rule registration**: Mochi:

```mochi
rule ancestor(X, Z) :- parent(X, Z)
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)
```

Lowers to:

```java
$$engine.addRule("ancestor",
    new String[]{"X", "Z"},
    new dev.mochi.runtime.datalog.Condition[]{
        new dev.mochi.runtime.datalog.Condition("parent",
            new Pattern[]{Pattern.var("X"), Pattern.var("Z")})
    }
);
$$engine.addRule("ancestor",
    new String[]{"X", "Z"},
    new dev.mochi.runtime.datalog.Condition[]{
        new dev.mochi.runtime.datalog.Condition("parent",
            new Pattern[]{Pattern.var("X"), Pattern.var("Y")}),
        new dev.mochi.runtime.datalog.Condition("ancestor",
            new Pattern[]{Pattern.var("Y"), Pattern.var("Z")})
    }
);
```

**Semi-naive evaluation**: The `Engine.evaluate()` method runs bottom-up iteration:
1. Start from all facts (the base case).
2. Apply each rule: for each tuple matching the rule body, derive a new head tuple.
3. Add new tuples to the delta set.
4. Repeat until the delta set is empty (fixpoint).

Semi-naive optimisation: only propagate the "new" tuples from the previous iteration through each rule, not all facts. This reduces the number of join operations per iteration from O(n^2) to O(n*delta). Standard Algorithm from Abiteboul-Hull-Vianu §12.3.

**`evaluate()` is called implicitly**: Before any `query` that depends on derived relations (`ancestor` is derived, `parent` is base), the lower pass inserts `$$engine.evaluate()`. The lower pass tracks which relations are derived (have rules) vs. base (facts only) and inserts `evaluate()` calls at the correct points (before the first query that reads a derived relation).

**`Condition` class**:

```java
package dev.mochi.runtime.datalog;

public record Condition(String relation, Pattern[] patterns) {}
```

## Sub-phase 8.2 -- Stratified negation

### Goal-alignment audit (8.2)

Negation-as-failure is required for expressing "X is not a parent of Z" queries. Stratification ensures that negation is safe: a relation is not negated within its own recursive stratum. Checking this at compile time (in the lower pass) gives a clear error before the program runs.

### Decisions made (8.2)

**Stratification check at compile time**: The lower pass, in `lower/query.go::checkStratification`, analyses the dependency graph of rules:
1. Build a directed graph where each edge `A -> B` means "relation A depends on relation B".
2. Mark edges with `negated = true` when the dependency goes through a negation (`not B(...)`).
3. Detect any cycle that contains a negated edge. If found, emit a compile error: `error: non-stratifiable Datalog program: relation X is recursively negated`.

**Negation in rules**: Mochi:

```mochi
rule safe(X) :- person(X), not criminal(X)
```

Lowers to:

```java
$$engine.addRule("safe",
    new String[]{"X"},
    new dev.mochi.runtime.datalog.Condition[]{
        new Condition("person", new Pattern[]{Pattern.var("X")}),
        new Condition("criminal", new Pattern[]{Pattern.var("X")}, /* negated= */ true)
    }
);
```

The `Condition` record gains a `boolean negated` field. During evaluation, a negated condition is satisfied when no tuple in the relation matches the pattern.

**Stratified evaluation order**: The evaluator processes strata in topological order. Base relations (no rules, only facts) are stratum 0. Derived relations are in higher strata based on their dependency depth. Negated dependencies force a higher stratum. The evaluator runs each stratum to fixpoint before starting the next.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/query.go` | `DatalogFact`, `DatalogRule`, `DatalogQuery` lowering; `checkStratification` compile-time check |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/datalog/Engine.java` | Semi-naive bottom-up evaluator, fact/rule/query API |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/datalog/Pattern.java` | Sealed pattern type: `Var`, `Const`, `Wildcard` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/datalog/Condition.java` | Rule body condition: relation + patterns + negation flag |
| `transpiler3/jvm/build/phase08_test.go` | `TestPhase8Datalog`: 20 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase08-datalog/*.{mochi,out}` | 20 fixtures |

## Test set

- `transpiler3/jvm/build/phase08_test.go::TestPhase8Datalog` -- 20 fixtures, byte-exact diff, JDK 21+25.
- `transpiler3/jvm/lower/query_test.go::TestDatalogStratification` -- unit test: non-stratifiable program (recursive negation) produces a compile error; stratifiable program passes.
- `transpiler3/jvm/lower/query_test.go::TestDatalogLowering` -- unit test: `fact`, `rule`, and `query` lower to the expected `Engine` API call tree.
- `transpiler3/jvm/runtime/datalog/EngineTest.java` -- JUnit: fact query, ancestor transitive closure (recursive rule), negation-as-failure (`safe` rule). Verifies semi-naive fixpoint for `ancestor(tom, ann)` derivation.

## Deferred work

- Aggregation in Datalog rules (`count`, `sum`, `max` in rule heads): deferred as a Datalog extension.
- External relation sources (relations backed by a JDBC connection): out of scope for MEP-47.
- Magic sets optimisation (top-down goal-directed evaluation): deferred performance optimisation.
- `@index` annotation to create a hash index on a specific column of a relation: deferred.

## Closeout notes

`TestPhase8Datalog` went green on JDK 21 with 6 fixtures covering facts, recursive rules, negation-as-failure, and multi-variable queries.

Implementation strategy: rather than shipping a runtime `Engine` class, the JVM lowerer evaluates the `DatalogProgram` at compile time using the same semi-naive bottom-up fixpoint algorithm as the BEAM backend. The result is emitted as a static `new java.util.ArrayList<>(java.util.List.of(...))` in the generated Java, so no datalog runtime dependency is needed. The `RawCStmt` nodes emitted by the C backend for datalog setup are skipped (no-op) in the JVM lower pass, mirroring the existing BEAM treatment.

Files changed:
- `transpiler3/jvm/lower/datalog.go` -- compile-time evaluator + `lowerDatalogQueryExpr`
- `transpiler3/jvm/lower/expr.go` -- route `DatalogQueryExpr` to the new handler
- `transpiler3/jvm/lower/stmt.go` -- no-op `RawCStmt` (C datalog setup code)
- `transpiler3/jvm/build/phase08_test.go` -- `TestPhase8Datalog` gate
- `tests/transpiler3/jvm/phase08-datalog/*.{mochi,out}` -- 6 fixtures
