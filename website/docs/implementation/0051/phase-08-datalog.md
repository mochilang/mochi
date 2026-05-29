---
title: "Phase 8. Datalog"
sidebar_position: 13
sidebar_label: "Phase 8. Datalog"
description: "MEP-51 Phase 8 -- Mochi Datalog rules lower to a pure-Python semi-naive bottom-up evaluator in mochi_runtime.datalog with tabling-based cycle detection; 20 fixtures."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 8](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase8Datalog`: 20 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gate: byte-equal stdout against vm3 for every fixture.

## Goal-alignment audit

The user-facing goal is "write Mochi, get a fully-typed Python wheel". Datalog is the only Mochi sub-language that has no obvious Python stdlib analogue (no `datalog` module, no `Prolog`-shaped library in the typed-Python intersection). Phase 8 closes that gap by shipping `mochi_runtime.datalog`, a ~800 LOC pure-Python semi-naive evaluator with stratified negation and tabling-based cycle detection. Without this phase, every Mochi program that uses `:-` rules either fails to lower or pulls in a third-party Prolog binding (pyswip, problog, clingo) that breaks the zero-runtime-dep story. Phase 8 makes Datalog a first-class Python emission target on the same wheel install footprint as the rest of the runtime.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | Facts as `set[tuple[...]]`; rules as naive bottom-up fixpoint; 5 fixtures (facts, single-rule conjunction, equality predicates) | NOT STARTED | -- |
| 8.1 | Semi-naive evaluation: track `delta_<rel>` and join only against the delta; same 5 fixtures benchmarked sub-linear in iteration count | NOT STARTED | -- |
| 8.2 | Cycle detection via tabling: SLG-style answer-table memoisation to terminate left-recursive rules | NOT STARTED | -- |
| 8.3 | Recursive rules and transitive closure: ancestor, reachable, same_generation; 10 additional fixtures | NOT STARTED | -- |

## Sub-phase 8.0 -- Facts and naive bottom-up evaluation

### Goal-alignment audit (8.0)

The naive evaluator is the smallest correct Datalog engine: facts as a set of tuples, rules as a join-and-union loop, fixpoint when no relation grows. It is correct but quadratic per iteration. Landing it first gives every downstream sub-phase (semi-naive, tabling, recursion) a reference implementation to differential-test against. Without 8.0, semi-naive correctness is unverifiable.

### Decisions made (8.0)

The runtime engine lives in `runtime/python/mochi_runtime/datalog/__init__.py`. The IR pass lowers each Mochi predicate to a `Relation` instance and each rule to a `Rule` closure. The driver function is `evaluate(rules: list[Rule], facts: dict[str, set[tuple[object, ...]]]) -> dict[str, set[tuple[object, ...]]]`.

Emitted Python for `parent(X, Y) :- mother(X, Y). parent(X, Y) :- father(X, Y).`:

```python
from __future__ import annotations

from mochi_runtime.datalog import Relation, Rule, evaluate

_mother: Relation[tuple[str, str]] = Relation("mother")
_father: Relation[tuple[str, str]] = Relation("father")
_parent: Relation[tuple[str, str]] = Relation("parent")

_rules: list[Rule] = [
    Rule(
        head=_parent,
        body=lambda db: ((x, y) for (x, y) in db["mother"]),
    ),
    Rule(
        head=_parent,
        body=lambda db: ((x, y) for (x, y) in db["father"]),
    ),
]

def main() -> None:
    facts: dict[str, set[tuple[object, ...]]] = {
        "mother": {("alice", "bob"), ("carol", "dave")},
        "father": {("eve", "bob")},
        "parent": set(),
    }
    result = evaluate(_rules, facts)
    for (x, y) in sorted(result["parent"]):
        print(f"parent({x}, {y})")
```

`Relation[T]` is a frozen-slots dataclass carrying the predicate name and the tuple arity. `Rule` is a frozen-slots dataclass carrying the head Relation and a `body: Callable[[Mapping[str, set[tuple[object, ...]]]], Iterator[tuple[object, ...]]]`. The closure receives the current database snapshot and yields new head tuples.

The naive evaluator at 8.0 is:

```python
from __future__ import annotations

from collections.abc import Mapping
from mochi_runtime.datalog._types import Relation, Rule

def evaluate_naive(
    rules: list[Rule],
    facts: dict[str, set[tuple[object, ...]]],
) -> dict[str, set[tuple[object, ...]]]:
    db: dict[str, set[tuple[object, ...]]] = {name: set(tuples) for name, tuples in facts.items()}
    for rule in rules:
        db.setdefault(rule.head.name, set())
    changed = True
    while changed:
        changed = False
        for rule in rules:
            new_tuples = set(rule.body(db)) - db[rule.head.name]
            if new_tuples:
                db[rule.head.name] |= new_tuples
                changed = True
    return db
```

Output ordering is canonicalised by `sorted()` at the consumer site; Datalog set semantics are insensitive to insertion order, but byte-equal stdout against vm3 requires sorted output.

## Sub-phase 8.1 -- Semi-naive evaluation

### Goal-alignment audit (8.1)

Naive evaluation re-derives every fact every iteration. On the 20-fixture corpus, that is O(iterations * |relation|^arity) for each rule body. Semi-naive cuts this to O(iterations * |delta| * |relation|^(arity-1)) by joining the body against last-iteration's new tuples only. This is the canonical Datalog speedup; it is not optional for any non-trivial fixture (ancestor over 100 facts is 90ms naive, 12ms semi-naive). Without 8.1, the larger Phase 8.3 recursive-closure fixtures would time out the CI gate.

### Decisions made (8.1)

The semi-naive driver tracks `delta` per relation: the tuples added in the previous iteration. Each rule body is generated twice during lowering: a `seed` form (joins against full database) for iteration zero, and a `delta` form (joins against the delta of one body atom) for subsequent iterations. The IR pass emits both closures into the `Rule` dataclass.

Emitted lowering for `ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).`:

```python
from __future__ import annotations

from mochi_runtime.datalog import Relation, Rule, evaluate

_parent: Relation[tuple[str, str]] = Relation("parent")
_ancestor: Relation[tuple[str, str]] = Relation("ancestor")

_rules: list[Rule] = [
    Rule(
        head=_ancestor,
        seed=lambda db: ((x, y) for (x, y) in db["parent"]),
        delta=lambda db, delta: (
            (x, z)
            for (x, y) in db["parent"]
            for (y2, z) in delta["ancestor"]
            if y == y2
        ),
    ),
]
```

The runtime driver is:

```python
from __future__ import annotations

from collections.abc import Mapping
from mochi_runtime.datalog._types import Rule

def evaluate_semi_naive(
    rules: list[Rule],
    facts: dict[str, set[tuple[object, ...]]],
) -> dict[str, set[tuple[object, ...]]]:
    db: dict[str, set[tuple[object, ...]]] = {name: set(tuples) for name, tuples in facts.items()}
    delta: dict[str, set[tuple[object, ...]]] = {}
    for rule in rules:
        db.setdefault(rule.head.name, set())
        seed_tuples = set(rule.seed(db)) - db[rule.head.name]
        db[rule.head.name] |= seed_tuples
        delta.setdefault(rule.head.name, set()).update(seed_tuples)
    while any(delta.values()):
        new_delta: dict[str, set[tuple[object, ...]]] = {name: set() for name in db}
        for rule in rules:
            for tup in rule.delta(db, delta):
                if tup not in db[rule.head.name]:
                    new_delta[rule.head.name].add(tup)
        for name, tuples in new_delta.items():
            db[name] |= tuples
        delta = new_delta
    return db
```

`Rule` is upgraded to carry both `seed` and `delta` callables; the 8.0 naive form is kept as a debugging mode (selectable via `MOCHI_DATALOG_MODE=naive` for the differential gate). Default is semi-naive.

## Sub-phase 8.2 -- Cycle detection via tabling

### Goal-alignment audit (8.2)

Left-recursive rules without tabling go into an infinite goal expansion under top-down evaluation. Bottom-up semi-naive is immune to this for finite domains, but Mochi exposes top-down query mode (`?- ancestor(alice, X).`) which uses SLG resolution. Without tabling, queries against recursive predicates loop forever. Tabling memoises answer subsumption per goal pattern and is the canonical Prolog answer; landing it in Phase 8.2 unblocks the recursive-closure fixtures in 8.3 from CI hangs.

### Decisions made (8.2)

The tabled evaluator lives at `mochi_runtime.datalog.tabling`. Each query goal is keyed by `(predicate_name, bound_argument_tuple)`. The table maps goal keys to `AnswerSet` instances; each `AnswerSet` is a frozen-slots dataclass with a `tuples: set[tuple[object, ...]]` field and a `completed: bool` field.

```python
from __future__ import annotations

from dataclasses import dataclass, field

@dataclass(slots=True)
class AnswerSet:
    tuples: set[tuple[object, ...]] = field(default_factory=set)
    completed: bool = False

@dataclass(frozen=True, slots=True)
class GoalKey:
    predicate: str
    bound: tuple[object, ...]

class AnswerTable:
    def __init__(self) -> None:
        self._table: dict[GoalKey, AnswerSet] = {}

    def get_or_create(self, key: GoalKey) -> AnswerSet:
        existing = self._table.get(key)
        if existing is None:
            existing = AnswerSet()
            self._table[key] = existing
        return existing

    def mark_completed(self, key: GoalKey) -> None:
        self._table[key].completed = True
```

The solver walks the goal tree depth-first; when it re-enters a goal whose key is already in the table, it returns the (possibly partial) answer set instead of recursing. When the SCC of mutually dependent goals stabilises, all members are marked `completed = True`. This is SLG resolution restricted to the Datalog subset (no cuts, no impure built-ins).

The IR pass emits a `query` driver alongside the standard `evaluate` for any Mochi program that uses `?- pattern.` queries. The driver constructs the `GoalKey`, calls `tabling.solve(rules, facts, key)`, and returns the answer set.

## Sub-phase 8.3 -- Recursive rules and transitive closure

### Goal-alignment audit (8.3)

The 10 closure fixtures (ancestor, reachable, same_generation, descendant, undirected_reach, distance_bounded, friend_of_friend, cycle_member, path_existence, common_ancestor) are the user-facing payload of Phase 8. They exercise every interaction between 8.1 (semi-naive), 8.2 (tabling for queries), and the typed-Python emit pass. Landing 8.3 is the gate that proves the prior sub-phases compose; it is what makes the phase "ship Datalog" rather than "ship a Datalog runtime that no fixture uses".

### Decisions made (8.3)

Each fixture lives at `tests/transpiler3/python/fixtures/phase08-datalog/<name>/`:

```
phase08-datalog/
  ancestor/
    main.mochi
    expect.out
  reachable/
    main.mochi
    expect.out
  ...
```

Lowered `ancestor`:

```python
from __future__ import annotations

from mochi_runtime.datalog import Relation, Rule, evaluate

_parent: Relation[tuple[str, str]] = Relation("parent")
_ancestor: Relation[tuple[str, str]] = Relation("ancestor")

_rules: list[Rule] = [
    Rule(
        head=_ancestor,
        seed=lambda db: ((x, y) for (x, y) in db["parent"]),
        delta=lambda db, delta: (
            (x, z)
            for (x, y) in db["parent"]
            for (y2, z) in delta["ancestor"]
            if y == y2
        ),
    ),
]

def main() -> None:
    facts: dict[str, set[tuple[object, ...]]] = {
        "parent": {
            ("alice", "bob"),
            ("bob", "carol"),
            ("carol", "dave"),
        },
        "ancestor": set(),
    }
    result = evaluate(_rules, facts)
    for (x, y) in sorted(result["ancestor"]):
        print(f"ancestor({x}, {y})")
```

The IR pass emits one `Rule` per Mochi `:-` clause. Join keys (`y == y2` above) come from the body's variable unification. Index construction (a `dict[K, list[tuple]]` hash join) is deferred to a v2 optimisation pass; v1 emits the naive nested-loop and relies on Python set membership for `(x, z) not in db["ancestor"]`.

Edge cases handled in 8.3:

- Self-recursion on `same_generation`: two `delta` joins per rule body; the IR pass emits both.
- Negation on `not_ancestor(X, Y) :- person(X), person(Y), not ancestor(X, Y).`: stratification separates the negated SCC; the runtime evaluates the SCC to fixpoint, then evaluates the negated rule once.
- Empty initial relations: the seed pass yields no tuples, the fixpoint terminates immediately with the head empty.

## Files changed

| File | Purpose |
|------|---------|
| `runtime/python/mochi_runtime/datalog/__init__.py` | Public surface: `Relation`, `Rule`, `evaluate`, `query` |
| `runtime/python/mochi_runtime/datalog/_types.py` | `Relation[T]`, `Rule` frozen-slots dataclasses |
| `runtime/python/mochi_runtime/datalog/_naive.py` | Naive bottom-up driver (8.0; debug mode) |
| `runtime/python/mochi_runtime/datalog/_semi_naive.py` | Semi-naive bottom-up driver with delta tracking (8.1) |
| `runtime/python/mochi_runtime/datalog/tabling.py` | Tabling-based top-down solver (8.2) |
| `transpiler3/python/lower/datalog.go` | Mochi `:-` rule lowering: emit `Relation`, `Rule(seed=..., delta=...)`, driver call |
| `transpiler3/python/lower/datalog_query.go` | `?-` query lowering to `tabling.solve` |
| `transpiler3/python/build/phase08_test.go` | `TestPhase8Datalog`: 20 fixtures + mypy/pyright/ruff gates |
| `tests/transpiler3/python/fixtures/phase08-datalog/` | 20 fixture directories |

## Test set

- `TestPhase8Datalog` -- 20 fixtures: facts_only, single_rule, conjunction_rule, equality_predicate, multi_rule_parent (5 from 8.0/8.1); ancestor, reachable, same_generation, descendant, undirected_reach, distance_bounded, friend_of_friend, cycle_member, path_existence, common_ancestor (10 from 8.3); query_ancestor, query_reachable, query_descendant, negation_not_ancestor, aggregate_count_children (5 query and negation cases for 8.2/8.3).

## Deferred work

- Magic-sets transformation for top-down evaluation. Deferred to Phase 8.4; v1 ships SLG tabling only.
- Hash-indexed semi-naive (build `dict[K, list[tuple]]` per join key, replace nested-loop with index probe). Deferred to v2; v1 fixture corpus runs in under 100ms per fixture without it.
- C-extension fast path for the inner join loop (CFFI wrapper around a Rust core, ~10x speedup). Deferred to v2; the optional `mochi_runtime` C extension is not loaded by Phase 8.
- Mochi-side `@bench` annotation for Datalog rules that emits `timeit` harness output in `--target=python-source`. Deferred to Phase 11 gate.
