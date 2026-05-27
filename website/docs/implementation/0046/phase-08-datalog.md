---
title: "Phase 8. Datalog"
sidebar_position: 10
sidebar_label: "Phase 8. Datalog"
description: "MEP-46 Phase 8 implementation spec: lowering Mochi Datalog facts and rules to ETS tables with a semi-naive fixed-point evaluator."
---

# Phase 8. Datalog

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 8](/docs/mep/mep-0046#phase-8-datalog) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

---

## Goal-alignment audit

Datalog is a distinguishing feature of Mochi that has no equivalent in standard
Erlang. This phase validates that Mochi's Datalog programs run on BEAM with
correct output (byte-equal vs vm3), establishing that the BEAM target is a
viable deployment path for Mochi programs that use relational logic. It also
produces the `mochi_datalog.erl` and `mochi_datalog_ets.erl` runtime modules
that are shipped as part of the Mochi OTP application.

---

## Sub-phase 8.0: Fact declarations -> ETS tables

### Table creation

`fact parent(string, string)` declares a relation with two string columns. Each
relation gets its own ETS table. Tables are created in the generated
`__datalog_init/0` function:

```go
// lowerFactDecl emits an ETS new + insert for each base fact
// Table creation:
c_call(
    c_atom("ets"), c_atom("new"),
    []cerl.Expr{
        c_atom("mochi_datalog_parent"),
        c_cons(c_atom("set"),
            c_cons(c_atom("named_table"),
                c_cons(c_atom("public"), c_nil()))),
    },
)
```

The table is created with `[set, named_table, public]` options. `named_table`
allows access by atom name from any process. `public` allows any process to
read and write (required for the semi-naive evaluator which runs in the calling
process).

### Fact insertion

`parent("alice", "bob")` -> `ets:insert(mochi_datalog_parent, {alice, bob})`:

```go
c_call(
    c_atom("ets"), c_atom("insert"),
    []cerl.Expr{
        c_atom("mochi_datalog_parent"),
        c_tuple([]cerl.Expr{c_string("alice"), c_string("bob")}),
    },
)
```

### __datalog_init/0

The lowerer collects all fact declarations and emits a single
`__datalog_init/0` function that creates all ETS tables and inserts all base
facts. The generated `main/0` calls `__datalog_init()` as its first statement:

```go
// Generated main/0 structure:
c_fun([]cerl.Var{},
    c_let([c_var("_")],
        c_apply(c_fname("__datalog_init", 0), []cerl.Expr{}),
        lowerExpr(mainBody),
    ),
)
```

---

## Sub-phase 8.1: Rule declarations -> semi-naive evaluator

### Rule representation

`rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)` is a recursive rule.
Each rule is lowered to an Erlang fun that:
1. Queries ETS tables for matching body atoms (using ETS match specs).
2. Inserts new derived facts into the head relation's ETS table.
3. Returns the count of newly inserted facts.

```go
// Each rule becomes a fun() -> integer() (count of new insertions)
c_fun([]cerl.Var{},
    // [ets:insert(ancestor, {X, Z}) || {X, Y} <- ets:tab2list(parent),
    //                                   {Y2, Z} <- ets:tab2list(ancestor),
    //                                   Y =:= Y2]
    // ...emitted as nested foldl
    ...
)
```

### Rule lowering details

Multi-body rules are lowered as nested foldl over ETS lookups:

```go
// rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)
//
// Lowered as:
// fun() ->
//   lists:foldl(fun({X, Y}, Acc0) ->
//     lists:foldl(fun({Y2, Z}, Acc1) ->
//       case Y =:= Y2 of
//         true ->
//           mochi_datalog_ets:delta_insert(ancestor, {X, Z}),
//           Acc1 + 1;
//         false -> Acc1
//       end
//     end, Acc0, ets:tab2list(mochi_datalog_ancestor))
//   end, 0, ets:tab2list(mochi_datalog_parent))
// end
```

The lowerer generates variable names from rule body atom positions to avoid
collisions: `V_r0_0`, `V_r0_1` for the first rule's first body atom's
arguments, etc.

### mochi_datalog_ets:delta_insert/2

`mochi_datalog_ets:delta_insert(RelName, Tuple)` inserts into both the main
table and the delta table if the fact is not already present:

```erlang
delta_insert(RelName, Tuple) ->
    DeltaName = list_to_atom(atom_to_list(RelName) ++ "_delta"),
    case ets:insert_new(RelName, Tuple) of
        true  -> ets:insert(DeltaName, Tuple), true;
        false -> false
    end.
```

`ets:insert_new/2` is atomic and returns `false` if a duplicate exists (for
`set` tables). This guarantees idempotency: the same fact is never inserted
twice.

---

## Sub-phase 8.2: Recursive rules and fixpoint termination

### Fixpoint loop

`mochi_datalog:compute_fixpoint/2` takes a list of rule funs and runs them in
a loop until no new facts are inserted:

```erlang
compute_fixpoint(Rules, _Tables) ->
    case run_rules(Rules) of
        0    -> ok;                          % stable: no new facts
        _N   -> compute_fixpoint(Rules, _Tables)  % iterate again
    end.

run_rules(Rules) ->
    lists:foldl(fun(RuleFun, Acc) -> Acc + RuleFun() end, 0, Rules).
```

### Semi-naive optimization

In the naive evaluator, each iteration joins against the full relation. In the
semi-naive evaluator, recursive body atoms join against the delta table (new
facts from the previous iteration) rather than the full table:

- Each relation `rel` has a shadow `mochi_datalog_rel_delta` ETS table.
- At the start of each iteration, the delta tables are cleared.
- `delta_insert` populates both the main and delta tables during an iteration.
- Recursive body atoms in rules use `ets:tab2list(mochi_datalog_rel_delta)`
  instead of `ets:tab2list(mochi_datalog_rel)`.

The lowerer identifies recursive body atoms (atoms whose relation name matches
the head relation of any rule in the same stratum) and emits delta-table
lookups for them. Non-recursive body atoms use the full table.

### Stratification

Rules are stratified by the lowerer before emitting the fixpoint computation.
Negation-in-rule-body requires stratification (the negated relation must be
fully computed before the current stratum runs). The lowerer performs a
dependency analysis:

1. Build a dependency graph: relation A depends on B if B appears in a rule
   body for A.
2. Find SCCs (strongly connected components) using Tarjan's algorithm.
3. Topologically sort SCCs into strata.
4. Emit one `compute_fixpoint` call per stratum, in topological order.

### Termination guarantee

The Mochi type system ensures all Datalog programs are range-restricted: every
variable that appears in a rule head also appears in at least one positive body
atom. This guarantees that the fixpoint computation terminates (the set of
derivable facts is bounded by the Herbrand base, which is finite for
range-restricted programs). The lowerer validates range restriction at compile
time and emits a clear error if a rule violates it.

---

## Sub-phase 8.3: Query expressions over facts and rules

### ETS match object queries

`query ancestor(X, "alice")` where `X` is a free variable and `"alice"` is a
bound constant lowers to an ETS match object lookup:

```go
// ets:match_object(mochi_datalog_ancestor, {'_', alice})
c_call(
    c_atom("ets"), c_atom("match_object"),
    []cerl.Expr{
        c_atom("mochi_datalog_ancestor"),
        c_tuple([]cerl.Expr{c_atom("_"), c_string("alice")}),
    },
)
```

Free variables in the query pattern become `'_'` in the ETS match spec. Bound
constants become their lowered value. The lowerer determines which variables
are free (not yet bound in the surrounding scope) at the query expression site.

### Integration with query DSL

`query from r in ancestor(_, "alice") select r` combines ETS lookup with Phase
7's query DSL lowering. The `ancestor(_, "alice")` fact query is first lowered
to an `ets:match_object` call, and the result (a list of tuples) is then
treated as the source collection for the `from`/`select` expression.

### mochi_datalog:query/2

`mochi_datalog:query(RelName, Pattern)` is a convenience wrapper:

```erlang
query(RelName, Pattern) ->
    TableName = list_to_atom("mochi_datalog_" ++ atom_to_list(RelName)),
    ets:match_object(TableName, Pattern).
```

---

## Fixtures

20 fixture files under `tests/dataset/slt/beam/phase08/`:

| File | Tests |
|---|---|
| `001_parent_child.mochi` | Base facts, simple query |
| `002_transitive_closure.mochi` | Recursive ancestor rule |
| `003_stratified.mochi` | Two strata, second depends on first |
| `004_magic_set.mochi` | Magic set transformation (manual) |
| `005_cycle_detection.mochi` | Cycle in relation graph |
| `006_range_restriction.mochi` | Compile-time range restriction check |
| `007_multi_rule_head.mochi` | Multiple rules for same relation |
| `008_negation.mochi` | Negation-as-failure in stratified program |
| `009_aggregation_in_rule.mochi` | Aggregate in rule body |
| `010_ets_large_facts.mochi` | Large fact set, ETS performance |
| `011_query_free_var.mochi` | Query with free variable |
| `012_query_bound_const.mochi` | Query with multiple bound constants |
| `013_query_dsl_over_facts.mochi` | `from r in query(...) select ...` |
| `014_fixpoint_count.mochi` | Count iterations to fixpoint |
| `015_mutual_recursion.mochi` | Mutually recursive rules (same SCC) |
| `016_semi_naive_delta.mochi` | Verify semi-naive reduces iterations |
| `017_string_keys.mochi` | String-keyed relations |
| `018_int_keys.mochi` | Integer-keyed relations |
| `019_mixed_types.mochi` | Relation with mixed column types |
| `020_empty_base.mochi` | Rules over empty fact set |

---

## Decisions made

### Why ETS instead of in-process lists for fact tables

ETS tables are off-heap, GC-transparent, and support O(1) lookup by key (for
`set` tables). For recursive Datalog programs that may derive millions of
facts, ETS avoids heap pressure on the calling process. An in-process list or
map accumulator would trigger major GC pauses as the derived set grows.
Additionally, ETS `named_table` allows fact tables to survive across process
restarts and be inspected from any process during debugging.

### Why semi-naive evaluation

Naive evaluation re-evaluates every rule against the full relation each
iteration, costing O(|relation|^2) per iteration for binary recursive rules.
Semi-naive tracks new (delta) facts from the previous iteration and joins
recursive body atoms only against deltas, reducing per-iteration cost from
O(n^2) to O(n * |delta|). For programs where `|delta|` is small relative to
`|relation|` (the common case after the first few iterations), this is a
significant speedup. The MEP-45 Datalog implementation uses the same strategy;
we carry it forward to BEAM.

### Why compile-time range restriction check

An un-range-restricted rule (a variable appears in the rule head but not in any
positive body atom) potentially derives an infinite set of facts, causing the
fixpoint loop to run forever. Rather than detecting infinite loops at runtime
(which would require a timeout heuristic), the lowerer rejects such programs at
compile time with a clear error message identifying the offending variable and
rule. This fail-fast approach is safer for production deployments.

### Why one ETS table per relation (not a single global table with a relation-name key)

Per-relation tables allow ETS `match_object` to use the table's internal index
directly, with the match pattern matching only on the fact's columns. A single
global table would require all match patterns to include the relation name as
the first element, doubling the pattern complexity and halving the effective
index selectivity. Per-relation tables also make `ets:tab2list` more efficient
for full-table scans during fixpoint computation.

## Closeout notes

Phase 8 landed across three commits. Sub-phase 8.0 (Datalog compile-time evaluation on BEAM) landed as `86668b31bb`. Sub-phases 8.1 and 8.2 (Datalog negation and multi-freevar test coverage) landed as `d4da0ed2b0`. The implementation uses a fixed-point evaluator on the BEAM side with ETS-backed relation tables, matching the design in §Sub-phase 8.0. All fixtures produce byte-equal output against vm3.
