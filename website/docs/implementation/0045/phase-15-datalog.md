---
title: "Phase 15. Datalog / logic"
sidebar_position: 17
sidebar_label: "Phase 15. Datalog"
description: "MEP-45 Phase 15 tracking: datalog lowering with semi-naive evaluation and magic-set transform."
---

# Phase 15. Datalog / logic

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 15](/docs/mep/mep-0045#phase-15-datalog--logic) |
| Status         | COMPLETE |
| Started        | 2026-05-26 01:57 (GMT+7) |
| Landed         | 2026-05-26 07:57 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Logic fixture suite (~20 cases: ancestors, reachability, magic-set, stratified negation) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Datalog is a first-class language feature in Mochi. The AOT path must support every language feature that vm3 supports or it becomes a second-class citizen. Phases 15.0 and 15.1 ensure the full Datalog surface (base facts, recursive rules, goal-directed evaluation) compiles and produces identical output to vm3. Aligns with the byte-equal correctness gate.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 15.0 | Fixed-point Datalog evaluation via direct C emission: `RawCStmt`/`RawCExpr` passthrough IR nodes; `lowerLogicQuery` generates fact tables as `const char*[]` arrays + do-while derivation loop + query collection into `mochi_list_str`; `fact`/`rule` statements collected into lowerer; `query` expr returns `list<string>`; 8 fixtures + `TestPhase15Datalog` gate; string-only terms | LANDED 2026-05-26 01:57 (GMT+7) | — | — |
| 15.1 | Magic-set transform for goal-directed evaluation (Bancilhon et al., PODS 1986)                                    | LANDED 2026-05-26 05:31 (GMT+7) | (this PR) | — |
| 15.2 | Stratified negation: `not` keyword added; `LogicCond.Not` AST field; `computeDatalogStrata()` fixpoint; `isNot` in `logicBody`; not-existence check emitted before head insertion; one do-while loop per stratum; 3 fixtures + `TestPhase15StratifiedNeg` gate | LANDED 2026-05-26 07:57 (GMT+7) | — | — |

## Decisions made

### Phase 15.0 (2026-05-26 01:57 GMT+7)

**Fixed-point evaluation via direct C emission.** Rather than adding a new IR node type for Datalog, Phase 15.0 uses `RawCStmt`/`RawCExpr` passthrough nodes. The `lowerLogicQuery` function generates a self-contained C block containing: (a) const arrays for base relations, (b) dynamic arrays for derived relations seeded with base facts, (c) a do-while loop that iterates the rule set to fixed-point, and (d) a result-collection pass. The block is emitted as `RawCStmt` and the result variable is returned as `RawCExpr` so the caller can use it in a let binding.

**String-only terms.** Phase 15.0 restricts fact and rule arguments to string literals and variables. This covers the canonical use cases (name graphs, type hierarchies) without requiring a polymorphic term representation.

**Gate:** `TestPhase15Datalog` (8 fixtures: dl_ancestor, dl_chain, dl_empty_result, dl_filter_const, dl_multi_query, dl_parent_basic, dl_reachability, dl_siblings).

### Phase 15.1 (2026-05-26 05:31 GMT+7)

**Magic-set transform (Bancilhon et al., PODS 1986).** The transform is applied automatically inside `lowerLogicQuery` when the query predicate has at least one bound (constant) argument and at least one rule derives the query predicate. The algorithm:

1. Identify bound positions: positions in the query args where the argument is a string constant.
2. Create `magic_REL` predicate with arity = number of bound positions.
3. Seed `magic_REL` from the query constants (added as a `logicFact`).
4. For each rule whose head is `REL`: prepend `magic_REL(bound-head-vars)` as the first body condition (the "magic guard"). This prevents derivation of tuples whose bound arguments are not reachable from the query goal.
5. For each recursive body call to `REL` within those rules: generate a magic propagation rule `magic_REL(new-bound-args) :- magic_REL(current-bound-args), <non-recursive body conds before the recursive call>`. Trivial propagations where the bound args are identical across the recursion (right-linear case) are omitted to avoid noise.

**Implementation locus.** `applyMagicSet` is a new `lowerer` method added just before `lowerLogicQuery` in `transpiler3/c/lower/lower.go`. It returns a pair of transformed `([]logicFact, []logicRule)` slices. `lowerLogicQuery` was refactored to use local `facts`/`rules` variables (populated by `applyMagicSet`) instead of `l.logicFacts`/`l.logicRules` directly, so the magic-set transform is transparently applied to all downstream C code generation.

**Right-linear vs left-linear recursion.** For right-linear rules like `anc(X,Y):-anc(X,Z),parent(Z,Y)`, the bound variable X is identical in head and recursive body call, so the propagation is trivial and no propagation rule is generated. `magic_anc` stays as a single-tuple fact holding the query constant, and all derivations share the same bound X. For left-linear rules like `fwd(X,Y):-step(X,Z),fwd(Z,Y)`, the recursive call has Z (not X) in the bound position, so a non-trivial propagation rule `magic_fwd(Z):-magic_fwd(X),step(X,Z)` is generated. The magic set grows through the step relation, guiding evaluation to only compute `fwd` tuples reachable from the query start.

**Backward compatibility.** The transform is a semantic-preserving rewrite: it produces the same external output as naive evaluation, just with fewer intermediate tuples. All 8 Phase 15.0 fixtures continue to pass after Phase 15.1 is applied.

**Gate:** `TestPhase15MagicSet` (5 fixtures: ms_sibling, ms_transitive, ms_left_linear, ms_two_step, ms_ancestor_dag).

### Phase 15.2 (2026-05-26 07:57 GMT+7)

**`not` keyword and `LogicCond.Not` AST field.** `"not"` was added to the Mochi keyword list in `parser/parser.go`. `LogicCond` gained a `Not *LogicPredicate` field parsed as `'not' @@`, alongside the existing `Pred` and `Neq` fields. `assertLogicCond` in `parser/invariants.go` updated to accept the third arm. `collectRule` in `lower.go` constructs `logicBody{isNot: true, name: ..., args: ...}` for `not` conditions.

**`computeDatalogStrata` fixpoint algorithm.** Assigns each relation a stratum number via iterative fixpoint: positive body conditions propagate the maximum of their strata to the head; `not bc` propagates `stratum(bc.name) + 1`. Cycles through negation are detected as a post-fixpoint check (error if `stratum(bc.name) >= stratum(head)` for any `isNot` body).

**Stratified multi-loop emission.** `lowerLogicQuery` now emits one `do { ... } while (changed);` loop per stratum (stratum 0 first, stratum N last). Rules in each stratum are only emitted in that stratum's loop, so negated relations are fully evaluated in their lower-stratum loop before the higher-stratum loop reads them.

**Not-existence check.** For each `isNot` body condition inside the innermost positive-body loops, an existence scan over the negated relation is emitted. If any tuple matches all bound arguments, a `__notfound_i_j = 1` flag is set and a `continue` skips the head insertion. This is O(n) per not-condition per candidate tuple, which is acceptable for the relation sizes Mochi Datalog targets.

**Gate:** `TestPhase15StratifiedNeg` (11 fixtures: all 8 Phase 15.0 fixtures + neg_orphan, neg_complement, neg_indirect).

## Deferred work

- Aggregates over recursive rules: v2.
- Semi-naive (incremental) evaluation: future optimisation phase.

## Closeout notes

All sub-phases 15.0, 15.1, and 15.2 are LANDED. The full Datalog surface (base facts, recursive rules, goal-directed magic-set, stratified negation) compiles and runs correctly via the C transpiler path.
