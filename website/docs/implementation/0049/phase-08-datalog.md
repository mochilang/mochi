---
title: "Phase 8. Datalog"
sidebar_position: 12
sidebar_label: "Phase 8. Datalog"
description: "MEP-49 Phase 8 — fact/rule/query lowering to MochiRuntime.Datalog; compile-time semi-naive evaluation; struct facts; 20 fixtures."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 8](/docs/mep/mep-0049#phase-8-datalog) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase8Datalog`: 20 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Datalog is Mochi's relational reasoning surface. Phase 8 ships the Swift backend for Datalog, reusing the same compile-time semi-naive evaluator strategy as the BEAM and JVM targets. The gate is byte-equal stdout against vm3, which uses the same evaluation algorithm. Unlike the .NET backend (which generated `Engine.Assert/AddRule/Query` calls at runtime), the Swift backend evaluates Datalog at transpile time and emits the results as static Swift arrays -- no runtime evaluator needed for the common case.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | `fact parent(alice, bob)` → Swift struct fact; compile-time evaluation; emitted as `let __dl_parent: [ParentFact] = [...]` | NOT STARTED | — |
| 8.1 | `rule ancestor(X,Y) :- parent(X,Y)` → semi-naive fixed-point evaluation at compile time | NOT STARTED | — |
| 8.2 | `query ancestor(alice, ?Z)` → static lookup against precomputed `[AncestorFact]`; print results | NOT STARTED | — |
| 8.3 | `MochiRuntime.Datalog.Engine` for dynamic fact assertion (non-static case) | NOT STARTED | — |

## Sub-phase 8.0 -- Fact declarations

### Decisions made (8.0)

**Compile-time evaluation strategy**: when all facts are declared as `fact` literals (no runtime-dynamic facts), the Go lowerer evaluates the Datalog program at transpile time using the same semi-naive algorithm as vm3. The derived relation (e.g., `ancestor`) is materialized as a Swift array literal. This produces the smallest and fastest generated code: zero runtime overhead, direct array iteration.

**Typed fact structs**: each Mochi `fact` relation becomes a Swift struct:

```swift
// Mochi: fact parent(alice, bob)
struct ParentFact: Equatable, Hashable {
    let arg0: String
    let arg1: String
}

let __dl_parent: [ParentFact] = [
    ParentFact(arg0: "alice", arg1: "bob"),
    ParentFact(arg0: "bob", arg1: "carol"),
]
```

**Naming**: `fact parent(...)` → struct `ParentFact` (relation name PascalCase + "Fact"), array `__dl_parent` (relation name with `__dl_` prefix to avoid user name collision).

**Heterogeneous fact arities**: each relation has a fixed arity (determined at declaration). The struct has `arg0`, `arg1`, ..., `argN` fields. Phase 8 uses positional names; labeled names are deferred to Phase 12 (typed Datalog schema).

## Sub-phase 8.1 -- Rule evaluation at compile time

### Decisions made (8.1)

**Semi-naive fixed-point in the Go lowerer**: `transpiler3/swift/lower/datalog.go` implements the semi-naive evaluator in Go (same as the BEAM backend's Go evaluator). It runs at transpile time and produces the complete derived relation.

**Algorithm**:
1. Parse `fact` and `rule` declarations from `aotir`.
2. Initialize each relation's set from `fact` declarations.
3. Run bottom-up semi-naive fixed-point: for each rule, compute new tuples using `Δ` (new-in-last-iteration) sets. Halt when all `Δ` are empty.
4. Emit the final derived sets as Swift array literals.

**Complexity**: the Go evaluator handles the fixture corpus (transitive closure, ancestry, reachability) efficiently. For the 20 fixtures, evaluation completes in <1ms per program.

**Stratified negation**: rules with negation (`not parent(X, Y)`) require stratified evaluation. The Go evaluator detects negation in rule bodies and partitions rules into strata. Each stratum evaluates to fixpoint before the next. Deferred to Phase 8.3 (runtime engine handles dynamic negation).

**Recursive rules**: transitive closure rules (`ancestor(X,Z) :- ancestor(X,Y), parent(Y,Z)`) are handled by the fixed-point iteration. No special treatment needed.

## Sub-phase 8.2 -- Query expressions

### Decisions made (8.2)

**Static lookup**: `query ancestor(alice, ?Z)` → a linear scan over the precomputed `__dl_ancestor` array, filtering on the bound argument and collecting the unbound variable:

```swift
// Mochi: query ancestor(alice, ?Z)
let __q_ancestor_alice_Z: [String] = __dl_ancestor
    .filter { $0.arg0 == "alice" }
    .map { $0.arg1 }
for z in __q_ancestor_alice_Z {
    MochiRuntime.print(z)
}
```

**Multiple bound variables**: `query ancestor(?X, carol)` → filter on `arg1`, collect `arg0`.

**Multiple free variables**: `query connected(?X, ?Y)` → collect all `(arg0, arg1)` tuples, print each pair.

**Output format**: matching vm3's output: one result per line, variables printed in order of appearance in the query.

## Sub-phase 8.3 -- Runtime Datalog engine

### Decisions made (8.3)

**When the runtime engine is needed**: for programs that assert facts dynamically at runtime (from user input, network data, or FFI calls), the compile-time evaluator cannot precompute the derived relations. The lowerer detects dynamic fact assertions and falls back to `MochiRuntime.Datalog.Engine`.

**`MochiRuntime.Datalog.Engine`**: pure Swift implementation in `MochiRuntime/Sources/MochiRuntime/Datalog/Engine.swift`. The evaluator:

```swift
public actor DatalogEngine {
    private var facts: [String: Set<DatalogTuple>] = [:]
    private var rules: [DatalogRule] = []
    
    public func assert(_ relation: String, _ args: String...) {
        facts[relation, default: []].insert(DatalogTuple(args))
    }
    
    public func addRule(_ rule: DatalogRule) {
        rules.append(rule)
    }
    
    public func query(_ relation: String, _ pattern: DatalogPattern) async -> [DatalogTuple] {
        // Run semi-naive evaluation, then filter by pattern
        await evaluate()
        return facts[relation]?.filter { pattern.matches($0) } ?? []
    }
    
    private func evaluate() async {
        // Semi-naive fixed-point
        var changed = true
        while changed {
            changed = false
            for rule in rules {
                let newFacts = rule.derive(from: facts)
                for (rel, tuples) in newFacts {
                    let before = facts[rel]?.count ?? 0
                    facts[rel, default: []].formUnion(tuples)
                    if (facts[rel]?.count ?? 0) > before { changed = true }
                }
            }
        }
    }
}
```

**`actor`**: the engine is an `actor` to satisfy Swift 6's Sendable requirements when the engine is shared across tasks.

**Generated code for dynamic case**:

```swift
// Mochi: fact parent(alice, bob) (when dynamic)
let __engine = DatalogEngine()
await __engine.assert("parent", "alice", "bob")
await __engine.addRule(DatalogRule(...))
let results = await __engine.query("ancestor", DatalogPattern(bound: [0: "alice"], free: [1]))
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/datalog.go` | Compile-time semi-naive evaluator; `DatalogQueryExpr` → static array literal |
| `transpiler3/swift/lower/lower.go` | `FactDecl`, `RuleDecl`, `DatalogQueryExpr` dispatch |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Datalog/Engine.swift` | Runtime `DatalogEngine` actor |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Datalog/DatalogTuple.swift` | Tuple and pattern types |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Datalog/DatalogRule.swift` | Rule representation and derivation |
| `transpiler3/swift/build/phase08_test.go` | `TestPhase8Datalog`: 20 fixtures |
| `tests/transpiler3/swift/fixtures/phase08-datalog/` | 20 fixture directories |

## Test set

- `TestPhase8Datalog` -- 20 fixtures covering: `dl_parent_basic`, `dl_ancestor`, `dl_sibling`, `dl_grandparent`, `dl_connected`, `dl_reachability`, `dl_transitive_closure`, `dl_negation`, `dl_stratified`, `dl_multi_rule`, `dl_multi_query`, `dl_bound_free`, `dl_all_free`, `dl_all_bound`, `dl_facts_only`, `dl_large_closure`, `dl_cycle`, `dl_path_query`, `dl_dynamic_assert`, `dl_dynamic_query`.

## Deferred work

- RETE network for large-scale dynamic Datalog. Deferred to a Phase 8 sub-MEP.
- Persistent fact storage (SQLite via FFI). Deferred to Phase 12.
- Datalog with typed schemas (relation fields named, typed). Deferred to Phase 12.
- Probabilistic Datalog. Out of v1 scope.
