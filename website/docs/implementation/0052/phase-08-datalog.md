---
title: "Phase 8. Datalog"
sidebar_position: 9
sidebar_label: "Phase 8. Datalog"
description: "MEP-52 Phase 8, Mochi datalog facts, rules, recursion, semi-naive evaluation in @mochi/runtime/datalog (~700 LOC of TS); 20 fixtures."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 8](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase8Datalog`: 20 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: tsc strict zero diagnostics; the datalog engine runtime stays under 8 KB gzipped (`@mochi/runtime/datalog` budget); fixed-point termination test (every fixture's bottom-up evaluation terminates within 1000 iterations on the test corpus).

## Goal-alignment audit

Mochi's datalog sub-language is used for graph reachability, type inference inside Mochi tooling, and rule-based business logic. The TypeScript surface does not have a datalog engine, so MEP-52 ships one under `@mochi/runtime/datalog`. The engine uses semi-naive bottom-up evaluation: each iteration considers only "new" facts (the delta from the previous iteration) when applying rules, dramatically reducing the redundant work of naive bottom-up. This matches the engine MEP-45 ships for C and the engine MEP-51 ships for Python; the algorithm is the same, only the TS rewrite is new.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | Fact storage: `Map<RelationName, Set<Tuple>>` with structural-equality tuple keys | NOT STARTED | n/a |
| 8.1 | Rule representation: `{head: Atom, body: Atom[]}` plus variable binding map | NOT STARTED | n/a |
| 8.2 | Semi-naive evaluation loop with delta-relation tracking | NOT STARTED | n/a |
| 8.3 | Tuple matching and unification: positional + named-binding | NOT STARTED | n/a |
| 8.4 | Negation-as-failure (stratified): topologically sort rules, evaluate strata bottom-up | NOT STARTED | n/a |

## Sub-phase 8.0, Fact storage

### Decisions made (8.0)

**Datalog facts** are typed tuples keyed by relation name. The runtime stores them as a `Map<string, Set<TupleKey>>` where `TupleKey` is a canonical string representation of the tuple:

```typescript
// @mochi/runtime/datalog/db
export type TupleKey = string;
export function tupleKey(values: readonly unknown[]): TupleKey {
  return JSON.stringify(values, (_k, v) =>
    typeof v === "bigint" ? `__bigint__${v.toString()}` : v
  );
}

export class FactDB {
  private readonly relations: Map<string, Set<TupleKey>> = new Map();
  add(rel: string, values: readonly unknown[]): boolean { /* ... */ }
  has(rel: string, values: readonly unknown[]): boolean { /* ... */ }
  scan(rel: string): IterableIterator<readonly unknown[]> { /* ... */ }
}
```

**Why string keys**: `Set<readonly unknown[]>` uses reference equality. Two arrays with identical contents would be different keys. JSON-stringify (with the `bigint` quirk) gives a stable canonical form; collisions are impossible for the value types datalog admits (int, string, bool; no records as tuple positions in Phase 8).

## Sub-phase 8.1, Rule representation

### Decisions made (8.1)

**Rule shape**:

```typescript
export type Term =
  | { readonly kind: "var"; readonly name: string }
  | { readonly kind: "const"; readonly value: unknown };

export type Atom = {
  readonly relation: string;
  readonly terms: readonly Term[];
};

export type Rule = {
  readonly head: Atom;
  readonly body: readonly Atom[];
};
```

The IR pass shared with MEP-45 lowers Mochi datalog source to this shape; the TS emitter writes the rule list as a `const rules: Rule[] = [...]` array.

## Sub-phase 8.2, Semi-naive evaluation

### Decisions made (8.2)

**Algorithm** (semi-naive, classic Ullman):

```typescript
// @mochi/runtime/datalog/eval
export function evaluate(facts: FactDB, rules: readonly Rule[]): FactDB {
  let delta: FactDB = facts.clone();
  let next: FactDB = facts.clone();
  while (delta.size() > 0) {
    const newDelta = new FactDB();
    for (const rule of rules) {
      // For each rule, fire it with at least one body atom matched against delta
      for (const newFact of fireRuleWithDelta(rule, next, delta)) {
        if (!next.has(newFact.relation, newFact.values)) {
          next.add(newFact.relation, newFact.values);
          newDelta.add(newFact.relation, newFact.values);
        }
      }
    }
    delta = newDelta;
  }
  return next;
}
```

**Why semi-naive**: naive evaluation re-derives every fact every iteration. Semi-naive only re-derives facts that need a "new" body atom; iteration `n+1` only fires a rule where at least one body atom matches a fact added in iteration `n`. For transitive-closure problems this is the difference between O(n^3) and O(n^2).

**Termination**: datalog without function symbols terminates (the universe of facts is bounded by the cross-product of the active domain). Negation requires stratification (sub-phase 8.4).

## Sub-phase 8.3, Tuple matching and unification

### Decisions made (8.3)

**Body matching**: for each rule body atom, scan the matching relation and try to unify against the current variable bindings. A binding is a `Map<string, unknown>`. Unification succeeds if every variable maps consistently across the body atoms.

```typescript
function unify(
  atom: Atom,
  tuple: readonly unknown[],
  bindings: Map<string, unknown>
): Map<string, unknown> | null {
  if (atom.terms.length !== tuple.length) return null;
  const out = new Map(bindings);
  for (let i = 0; i < atom.terms.length; i++) {
    const t = atom.terms[i]!;
    const v = tuple[i];
    if (t.kind === "const") {
      if (!mochiDeepEq(t.value, v)) return null;
    } else {
      const existing = out.get(t.name);
      if (existing === undefined) out.set(t.name, v);
      else if (!mochiDeepEq(existing, v)) return null;
    }
  }
  return out;
}
```

**Head emission**: substitute bindings into head terms; if any head variable is unbound the rule is range-restricted and the emitter rejects at lower-time.

## Sub-phase 8.4, Negation-as-failure

### Decisions made (8.4)

**Stratification**: build a dependency graph (rule head's relation depends on each body atom's relation; negation edge is marked). The graph must be acyclic when ignoring positive edges, otherwise the program is rejected at lower-time as non-stratifiable. Strata are SCCs in the positive-only subgraph, topologically ordered.

**Evaluation**: each stratum is evaluated to fixed-point before the next stratum starts. Negation `not p(x)` at a body atom succeeds if `p(x)` is not in the current facts (which is fully determined by the time the negation's stratum is reached, by construction).

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/datalog.go` | Mochi datalog source to Rule/Atom IR; rule-list emission |
| `runtime3/typescript/src/datalog/db.ts` | FactDB with TupleKey canonicalisation |
| `runtime3/typescript/src/datalog/eval.ts` | Semi-naive evaluation loop |
| `runtime3/typescript/src/datalog/unify.ts` | Unification with variable bindings |
| `runtime3/typescript/src/datalog/strata.ts` | Stratification with negation-as-failure |
| `transpiler3/typescript/build/phase08_test.go` | `TestPhase8Datalog` |
| `tests/transpiler3/typescript/fixtures/phase08-datalog/` | 20 fixtures (transitive closure, ancestors, graph reachability, type inference toy, etc.) |

## Test set

- `TestPhase8Datalog`, 20 fixtures four-runtime.
- `TestPhase8DatalogBudget`, `@mochi/runtime/datalog` stays under 8 KB gzipped.
- `TestPhase8Stratifiable`, a non-stratifiable fixture is rejected at lower-time with an explicit error.

## Deferred work

- Aggregations inside rules (`count(...)`, `sum(...)` as datalog atoms). Phase 8 ships range-restricted positive + stratified-negation only. Aggregations are a v1.5 candidate.
- Incremental maintenance (DBSP-style differential dataflow). Out of scope.
- External-database integration (project a relational table as facts). Phase 14 (fetch) and a v2 datalog adapter cover this together.
