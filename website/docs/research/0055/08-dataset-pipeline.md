---
title: "Dataset pipeline: query DSL lowering and Datalog compile-time evaluation"
description: "Phase 7 query DSL lowering (from/where/select/order-by/skip/take onto PHP arrays + usort) and Phase 8 Datalog compile-time semi-naive evaluation emitting static PHP array literals."
sidebar_position: 8
---

# Dataset pipeline: query DSL lowering and Datalog compile-time evaluation

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/lower/lower.go` (lines 430-478 for
`QueryScopeStmt` and `RawCStmt`), `transpiler3/php/lower/datalog.go`,
`transpiler3/php/build/phase07_test.go`,
`transpiler3/php/build/phase08_test.go`,
`tests/transpiler3/php/fixtures/phase07-query/`,
`tests/transpiler3/php/fixtures/phase08-datalog/`.

This note covers the two dataset-related lowering paths in MEP-55:
the Phase 7 query DSL and the Phase 8 Datalog evaluator.

## 1. Phase 7: Query DSL lowering

### 1.1 Desugaring in aotir

The query DSL (`from n in nums where n % 2 == 0 select n * 2`) is
desugared by the shared aotir lowerer (not the PHP-specific lowerer)
into a canonical imperative form:

1. A `LetStmt` binding the result accumulator to `[]`.
2. A `ForEachStmt` iterating over the source collection.
3. An optional `IfStmt` for the `where` predicate.
4. An `ArrayAppendExpr` pushing the selected element.
5. Optional `ListSortAscExpr` for `order by`.
6. Optional `ListSliceExpr` for `skip` / `take`.

The PHP lowerer receives this already-desugared form. The query temp
variable names (`$__query1`, `$__query2`) are minted by the shared aotir
desugaring pass, not by the PHP lowerer.

### 1.2 PHP lowering of the desugared form

The PHP lowerer handles the query DSL through two special cases:

**`QueryScopeStmt`** (lower.go lines 473-475): A C-specific arena-scope
wrapper. On the PHP side this is dropped entirely:
```go
case *aotir.QueryScopeStmt:
    return l.lowerBlock(v.Body)
```
PHP uses reference counting and cycle collection instead of arenas; the
body is inlined directly into the surrounding block.

**`RawCStmt`** (lower.go lines 476-481): Pre-rendered C source for
Datalog setup (see §2 below). The PHP path computes Datalog results at
compile time and emits a static array, so this hint is a no-op:
```go
case *aotir.RawCStmt:
    return nil, nil
```

### 1.3 Emitted PHP shape

A simple filter query:
```mochi
from n in nums where n % 2 == 0 select n
```
emits (from `TestPhase7EmitFragments` `query_filter` fixture):
```php
$__query1 = [];
foreach ($nums as $n) {
    if ((($n % 2) === 0)) {
        $__query1 = [...$__query1, $n];
    }
}
$evens = $__query1;
```

The `ArrayAppendExpr` (`[...$__query1, $n]`) is non-mutating: it creates
a new array each iteration. PHP's copy-on-write makes this relatively
cheap for small-to-medium result sets, though not O(1) per element.

### 1.4 `order by` → `mochi_list_sort_asc`

The `order by n` clause appends a sort step after the gather loop. The
PHP lowerer emits the `mochi_list_sort_asc` inline helper (lower.go
lines 239-254), which uses PHP's spaceship operator for
type-agnostic natural ordering:
```php
usort($xs, fn($a, $b) => $a <=> $b);
return $xs;
```
The helper takes the array by value (PHP copy-on-write), sorts it in
place (mutating the copy), and returns the sorted copy. The caller's
original array is untouched.

From `TestPhase7EmitFragments` `query_order_by` fragment:
```php
$__query1 = mochi_list_sort_asc($__query1);
$sorted = $__query1;
```

The same spaceship-backed helper sorts ints, floats, and strings
uniformly, as confirmed by `query_order_by_strings` fixture.

### 1.5 `skip` / `take` → `array_slice`

Skip and take lower to `array_slice($xs, start, length)`:

- `skip N` (no take): `array_slice($__query1, N, (4611686018427387903 - N))`
  where `4611686018427387903` is `PHP_INT_MAX / 2` (safe large value).
- `take N` (no skip): `array_slice($__query1, 0, ((0 + N) - 0))`
- `skip S take T`: `array_slice($__query1, S, ((S + T) - S))`

The redundant arithmetic for the canonical form is emitted by the shared
aotir desugaring; the PHP lowerer passes it through without simplifying.

From `TestPhase7EmitFragments`:
```
query_skip:  $__query1 = array_slice($__query1, 2, (4611686018427387903 - 2));
query_take:  $__query1 = array_slice($__query1, 0, ((0 + 3) - 0));
```

### 1.6 Nested queries

Two queries in one function body use independent temp counters. The
second query gets `$__query2`. The counter resets between function
bodies because the aotir desugaring uses a per-function counter.
From `query_nested` fixture:
```php
$small = $__query1;
$doubled_small = $__query2;
```

### 1.7 Queries inside user functions

Queries inside user functions emit with the `mochi__` prefix on the
function name:
```php
function mochi__filter_evens(array $nums): array { ... }
function mochi__double_all(array $nums): array { ... }
```
Each function body has its own `$__query1` temp, independent of other
functions.

## 2. Phase 8: Datalog compile-time semi-naive evaluation

### 2.1 Strategy

Mochi's Datalog DSL allows compile-time rules and facts to be evaluated
at transpile time. The PHP target does not ship a runtime Datalog engine.
Instead, `datalogEval` in `transpiler3/php/lower/datalog.go` runs the
evaluation at Go/transpile time and emits the results as a static PHP
array literal.

This is the same strategy used by the BEAM (MEP-46) backend; the C
(MEP-45) backend emits C-level loop code at runtime instead.

### 2.2 `datalogEval` implementation

`datalogEval` (datalog.go lines 9-62) is a bottom-up semi-naive Datalog
evaluator:

1. Seeds the state map from the program's `Facts` (ground atoms).
2. Iterates: for each `Rule`, derives new tuples by `dlDeriveRule`.
3. Stops when no new tuples are added (`changed == false`).
4. Filters tuples from the named query relation against the query args
   (bound args constrain, unbound args project free variables).
5. Returns the list of free-variable values as `[]string`.

The state is `map[string][][]string`: relation name → list of tuples
(each tuple is `[]string` of string-serialised values).

**Rule derivation** (`dlDeriveRule`, datalog.go lines 64-149): handles
three literal types:
- Positive literals: join against the relation state.
- Negation literals (`IsNot`): remove environments where the relation
  would match.
- Inequality literals (`IsNeq`): remove environments where two variables
  are equal.

**Variable notation**: uppercase words are Datalog variables (`dlIsVar`,
datalog.go line 177). Quoted strings (`"alice"`) are constants
(`dlUnquote`, line 178).

### 2.3 Emitted PHP shape

The `lowerDatalogQueryExpr` call site (not shown in the excerpts above,
but present in the `lowerExpr` switch) calls `datalogEval` and emits the
results as a PHP array literal. For a query that finds ancestors:
```mochi
// facts: parent(alice, bob), parent(bob, carol)
// rule:  ancestor(X, Z) :- parent(X, Z).
// rule:  ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
// query: ancestor(alice, Who)
```
The emitted PHP is:
```php
$ancestors = ["bob", "carol"];
```

No PHP code loops or evaluates rules at runtime. The results are fully
computed at transpile time and baked into the emitted source. This means
the PHP output is maximally simple and fast (array literal, no
interpreter overhead) at the cost of being static (the Datalog program
cannot take runtime input).

### 2.4 Why emit a static array (not a runtime engine)

The fixture corpus (`phase08-datalog/`) contains only programs where the
Datalog rules and facts are fully known at compile time. A runtime engine
would be needed only if rules or facts could be provided at runtime (e.g.,
from a database query). That use case is out of scope for MEP-55 Phase 8.

The static-array approach:
- Requires no runtime dependency.
- Is maximally fast at runtime.
- Produces byte-equal output across PHP versions.
- Is easily validated: `TestPhase8EmitFragments` checks that the emitted
  array contains the expected values.

A future MEP could add a runtime Datalog engine (potentially using
`amphp/revolt` for async evaluation, or a dedicated `mochi/datalog`
Composer package) for programs that need runtime-variable rules.

## 3. `RawCStmt` and `QueryScopeStmt` no-ops

The aotir IR carries two statement types that only make sense for the C
backend:

- `RawCStmt`: pre-rendered C source code for the Datalog runtime setup
  (`dl_solver_add_rule`, etc.). The PHP backend evaluates Datalog at
  compile time, so this hint is ignored (lower.go line 481: `return
  nil, nil`).

- `QueryScopeStmt`: a C-specific arena allocation scope wrapping query
  result accumulation. PHP's garbage collector handles memory
  automatically, so the scope wrapper is inlined without the frame
  (lower.go line 475: `return l.lowerBlock(v.Body)`).

Both no-ops are documented in the source with comments explaining why
the C-backend hint is irrelevant for PHP.
