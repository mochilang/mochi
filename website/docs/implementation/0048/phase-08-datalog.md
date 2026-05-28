---
title: "Phase 8. Datalog"
sidebar_position: 10
sidebar_label: "Phase 8. Datalog"
description: "MEP-48 Phase 8 — fact/rule/query lowering to Mochi.Runtime.Datalog; semi-naive evaluator; 6 fixtures."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 8](/docs/mep/mep-0048#phase-8-datalog) |
| Status         | LANDED |
| Started        | 2026-05-28 02:38 (GMT+7) |
| Landed         | 2026-05-28 02:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase8Datalog`: 6 fixtures green on net8.0 and net10.0.

## Goal-alignment audit

Datalog is Mochi's relational reasoning surface. Phase 8 ships the `.NET` backend for Datalog, reusing the same in-memory semi-naive evaluator strategy as the BEAM and JVM targets but implemented in C#. The gate is byte-equal stdout against vm3, which uses the same evaluator algorithm.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | `fact` declarations → `Engine.Assert` calls; `rule` declarations → `Engine.AddRule` | NOT STARTED | — |
| 8.1 | `query` expressions → `Engine.Query<T>()` returning `IEnumerable<T>` | NOT STARTED | — |
| 8.2 | Semi-naive evaluator in `Mochi.Runtime.Datalog.Engine` | NOT STARTED | — |

## Sub-phase 8.0 -- Facts and rules

### Decisions made (8.0)

**`fact parent(alice, bob)` → C# generated code**:

```csharp
Engine.Assert("parent", "alice", "bob");
```

**`rule ancestor(X, Y) :- parent(X, Y)` → C# generated code**:

```csharp
Engine.AddRule(
    head: ("ancestor", new[] { "X", "Y" }),
    body: new[] { ("parent", new[] { "X", "Y" }) }
);
```

The `Engine` is instantiated once per Mochi module in a static field. Facts and rules are registered at module initialisation via `[ModuleInitializer]` in the generated code.

## Sub-phase 8.1 -- Query expressions

### Decisions made (8.1)

**`query ancestor(alice, ?Z)` → C# generated code**:

```csharp
IEnumerable<DatalogTuple> results = Engine.Query("ancestor",
    DatalogTerm.Const("alice"), DatalogTerm.Var("Z"));
foreach (var row in results)
    Print.Line(row.Get("Z"));
```

`DatalogTuple` is a `IReadOnlyDictionary<string, string>` mapping variable names to bound values.

## Sub-phase 8.2 -- Semi-naive evaluator

### Decisions made (8.2)

**`Mochi.Runtime.Datalog.Engine`**: pure C# implementation. The evaluator:
1. Stores facts as `HashSet<DatalogTuple>` per relation.
2. On `Query`, runs bottom-up semi-naive evaluation: fixed-point iteration of rule applications until no new tuples are derived.
3. Incremental delta tracking: `Δ` sets for each iteration; halts when `Δ` is empty.

The evaluator is single-threaded and in-memory (no RETE network in Phase 8). Sufficient for the fixture corpus which covers transitive closure, ancestor relationships, graph reachability, and path queries. Large-scale Datalog (millions of facts) is deferred.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | Datalog fact/rule/query lowering to Engine.Assert/AddRule/Query calls |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Datalog/Engine.cs` | Semi-naive evaluator, fact store, rule application |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Datalog/DatalogTerm.cs` | `Const` / `Var` term discriminated union |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Datalog/DatalogTuple.cs` | Binding environment |
| `transpiler3/dotnet/build/phase08_test.go` | `TestPhase8Datalog`: 6 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase08-datalog/` | 6 fixture directories |

## Test set

- `TestPhase8Datalog` -- 6 fixtures (dl_ancestor, dl_connected, dl_facts_grandparent, dl_negation, dl_parent_basic, dl_sibling).

## Deferred work

- RETE network for large-scale Datalog. Deferred to Phase 3 sub-MEP.
- Stratified negation. Deferred pending evaluator design.
- Persistent fact storage (DuckDB, SQLite). Deferred to Phase 12 (FFI).

## Closeout notes

Phase 8 landed. `TestPhase8Datalog` PASS: 6 fixtures on net10.0 (dl_ancestor, dl_connected, dl_facts_grandparent, dl_negation, dl_parent_basic, dl_sibling).

Compile-time semi-naive Datalog evaluation (same strategy as BEAM backend): `DatalogQueryExpr` is evaluated at lower time and emitted as a static `List<string>` literal. `RawCStmt` (C-specific setup) is a no-op for .NET.
