---
title: "Phase 8. Datalog"
sidebar_position: 13
sidebar_label: "Phase 8. Datalog"
description: "MEP-51 Phase 8, Mochi Datalog programs evaluated at compile time on the Go side (porting the BEAM backend's semi-naive evaluator) and emitted as a static Python list[str] literal so the wheel ships no runtime Datalog engine."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 8](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 |
| Landed         | 2026-05-29 18:53 (GMT+7) |
| Tracking issue | TBA |
| Tracking PR    | TBA |

## Gate

`TestPhase8Datalog` (transpiler3/python/build/phase08_test.go) walks every `.mochi` under `tests/transpiler3/python/fixtures/phase08-datalog/` and asserts byte-equal stdout against the recorded `.out` file. 16 fixtures green on CPython 3.13.0:

- 8 base Datalog fixtures from `tests/transpiler3/c/fixtures/datalog`: `dl_parent_basic`, `dl_ancestor`, `dl_chain`, `dl_reachability`, `dl_filter_const`, `dl_empty_result`, `dl_multi_query`, `dl_siblings` (the last covers `X != Y` inequality literals).
- 3 stratified-negation fixtures: `neg_complement`, `neg_indirect`, `neg_orphan` (`not Pred(...)` negation-as-failure with stratum ordering).
- 5 magic-set / left-linear / transitive-closure fixtures from `tests/transpiler3/c/fixtures/magic_datalog`: `ms_ancestor_dag`, `ms_left_linear`, `ms_sibling`, `ms_transitive`, `ms_two_step`.

Cross-OS + 3.12 + mypy / pyright / ruff strict gates roll up at Phase 16.

## Goal-alignment audit

Datalog is the only Mochi sub-language with no Python stdlib analogue. The user-facing goal is "any Mochi program with `fact / rule / query` runs on Python and prints byte-equal output to vm3". The Python wheel must ship zero runtime Datalog dependency (no `pyswip`, `clingo`, or `problog`); Phase 18's PyPI Trusted Publishing target requires the wheel to be pure-stdlib at the top of the dep tree.

The implementation choice is compile-time evaluation on the Go side. Mochi `fact` and `rule` statements cannot be reloaded at runtime (there is no `eval-fact` or `assert` surface), so the entire program is known at compile time. The Python lowerer ports the BEAM backend's `datalogEval` semi-naive evaluator (`transpiler3/beam/lower/lower.go:2885`) verbatim into `transpiler3/python/lower/datalog.go` and emits a static `list[str]` literal at the call site of each `query Name(args)` expression. This is lossless (same fixpoint algorithm as the BEAM and C backends) and adds zero LOC to `mochi_runtime`.

## Sub-phases

Because the implementation is one shared evaluator plus one expression-lowering case, the original sub-phase split (8.0 facts, 8.1 semi-naive, 8.2 tabling, 8.3 recursion) collapses to a single landing. The published gate covers all the shapes that the original split would have separately covered:

| Original scope | Coverage in this landing |
|----------------|--------------------------|
| 8.0 Facts and base evaluation | `dl_parent_basic`, `dl_filter_const`, `dl_empty_result`, `dl_multi_query` |
| 8.1 Semi-naive over recursive rules | `dl_ancestor`, `dl_chain`, `dl_reachability` |
| 8.2 Stratified negation | `neg_complement`, `neg_indirect`, `neg_orphan` |
| 8.3 Recursive transitive closure | `ms_ancestor_dag`, `ms_left_linear`, `ms_sibling`, `ms_transitive`, `ms_two_step` |

## Design decisions

### Compile-time evaluation, not runtime

The original draft of this doc imagined `mochi_runtime.datalog` shipping a `Relation` / `Rule` / `evaluate` API with a ~800 LOC pure-Python semi-naive engine. That design has three drawbacks:

1. The wheel grows by roughly 30 KiB for an engine that runs once per program and discards its state immediately.
2. The runtime engine would need its own typed-Python conformance pass (mypy / pyright on closures-over-tuples is fragile around `tuple[object, ...]` invariance).
3. The Go-side BEAM evaluator already exists and is proven against the C backend's fixture suite. Porting it sidesteps re-deriving the algorithm in two languages.

The compile-time approach inverts all three:

1. Wheel adds zero runtime code; the runtime helper directory stays at Phase 7's footprint.
2. The emitted Python is a literal `list[str]` plus the existing `for x in xs: print(x)` loop, which mypy and pyright infer without help.
3. The evaluator is a Go file mirror-copied from BEAM. Any future correctness improvement on BEAM transfers to Python with a one-line port.

### Why a port rather than a shared package

`transpiler3/beam/lower/datalogEval` is currently a private function in the BEAM lowerer. Extracting it to a shared package (e.g. `transpiler3/datalog/eval`) would be cleaner but requires touching the BEAM module and its tests in the same PR. Phase 8 keeps the BEAM file untouched and adds `transpiler3/python/lower/datalog.go` with the same algorithm under Python-prefixed names (`datalogEval`, `datalogDeriveRule`, etc.). The two implementations are byte-equivalent; a future refactor PR can dedupe.

### `RawCStmt` skip

The C lowerer attaches a `*aotir.RawCStmt` next to every `DatalogQueryExpr`. The raw C statement is the C backend's setup for `__dl<N>_result`, the C-side mochi_list_str variable. Python ignores it (and never emits it). The Python `lowerBlock` skips any `RawCStmt` it sees:

```go
if _, ok := s.(*aotir.RawCStmt); ok {
    continue
}
```

This is the same pattern as `QueryScopeStmt` from Phase 7: C-only IR nodes splice or skip on Python.

### Emitted shape

For `fact parent("alice", "bob") fact parent("alice", "carol") let xs = query parent("alice", Y) for x in xs { print(x) }`:

```python
from __future__ import annotations


def main() -> None:
    xs: list[str] = ["bob", "carol"]
    for x in xs:
        Print.line(x)


if __name__ == "__main__":
    main()
```

The fact / rule statements emit nothing (they accumulate into the C lowerer's `logicFacts` / `logicRules` slices and are only consulted when a `query` expression triggers `lowerDatalogQueryExpr`).

### Multi-query and reused programs

`dl_multi_query` exercises two `query` calls over the same fact set. The C IR snapshots the program state per `DatalogQueryExpr` (`Prog *DatalogProgram` field), so each query evaluates against the snapshot at its point in the program (post-facts, pre-or-post any intervening rules). Python evaluates each `DatalogQueryExpr` independently; no shared state between queries. Performance: even for 5-rule transitive-closure programs the evaluator finishes in microseconds at compile time, so re-evaluating per query is fine.

### Empty results

`dl_empty_result` queries `parent("nobody", Y)`, which produces an empty list. The emit is `xs: list[str] = []` followed by `print(len(xs))` (the existing print path for `len` over an empty list yields `0`).

### Negation-as-failure

`neg_complement`, `neg_indirect`, `neg_orphan` use `not Pred(X)` in rule bodies. The BEAM evaluator's negation pass iterates the negated relation in the state map and keeps environments only when no tuple matches. Stratum ordering (Stratum 0 derived before Stratum 1 uses negation) is implicit in the C lowerer's rule ordering: the C lower at `transpiler3/c/lower/lower.go:7437` computes strata and re-orders rules so that the semi-naive loop derives lower strata to fixpoint first. The Python port inherits this ordering by consuming `e.Prog.Rules` in the order the C lowerer emits.

### Inequality (`X != Y`)

`dl_siblings` and `ms_sibling` use `X != Y` body literals. The C IR represents these as `DatalogRuleBody{IsNeq: true, NeqA: "X", NeqB: "Y"}`. The Python port handles them in the join loop with a single `if a == b: drop env` check.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/datalog.go` | New: compile-time semi-naive evaluator ported from BEAM (`datalogEval`, `datalogDeriveRule`, `datalogTupleIn`, plus arg / variable / unquote helpers) |
| `transpiler3/python/lower/lower.go` | Added `*aotir.DatalogQueryExpr` case in `lowerExpr`; added `*aotir.RawCStmt` skip in `lowerBlock` |
| `transpiler3/python/build/build.go` | Cache marker `mep51-phase07` → `mep51-phase08` |
| `transpiler3/python/build/phase08_test.go` | New: `TestPhase8Datalog` walks all phase08-datalog fixtures |
| `tests/transpiler3/python/fixtures/phase08-datalog/` | 16 fixtures: 11 from `tests/transpiler3/c/fixtures/datalog`, 5 from `tests/transpiler3/c/fixtures/magic_datalog` |

## Test set

- `TestPhase8Datalog`, 16 sub-tests, all green (1.5s wall clock on host; trivial because emitted Python is a literal list).
- Full `go test ./transpiler3/python/...` green (31.6s), no Phase 1-7 regressions.

## Deferred work

- **Runtime Datalog API for dynamically-loaded fact sets.** Mochi v1 has no surface for loading facts at runtime; if a future language change adds `assert(parent("x", "y"))` or `:load_facts(...)`, the compile-time strategy stops working and the runtime engine described in the original draft becomes necessary. Estimated effort: ~600 LOC pure-Python evaluator plus the typed conformance pass. Not in scope for v1.
- **Magic Sets optimisation surface (Phase 15.1 in the C umbrella).** The C lower at `transpiler3/c/lower/lower.go:7495` has the goal-directed magic-set rewrite. The current Python port uses the unrewritten rule list, which is fine for the v1 fixture sizes (all queries finish in microseconds at compile time). If a future fixture stresses the evaluator past 100ms, the port can consume the magic-rewritten rule list (the BEAM backend already does this via the `DatalogProgram` snapshot).
- **Sharing the evaluator with BEAM.** `transpiler3/python/lower/datalog.go` is a mirror copy of `transpiler3/beam/lower/lower.go:2885-3090`. A future cleanup PR can extract both into `transpiler3/datalog/eval`.

## Build / cache

Cache marker bumped to `mep51-phase08`. Existing `~/.cache/mochi/python/*` entries from Phase 7 are invalidated by the marker change.
