---
title: "Phase 5. Sum types and pattern matching"
sidebar_position: 7
sidebar_label: "Phase 5. Sum types and pattern matching"
description: "MEP-46 Phase 5 implementation spec: lowering Mochi sum types, option, and result to tagged BEAM values and c_case."
---

# Phase 5. Sum types and pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 5](/docs/mep/mep-0046#phase-5-sum-types-and-pattern-matching) |
| Status         | LANDED |
| Started        | 2026-05-26 14:21 (GMT+7) |
| Landed         | 2026-05-26 14:37 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

---

## Goal-alignment audit

This phase directly advances the correctness gate (byte-equal stdout vs vm3)
for programs that use sum types, which is one of the most common Mochi
patterns. Without this phase, any program using `option<T>`, `Result<T,E>`, or
user-defined tagged unions fails to compile to BEAM. Completing this phase
unlocks a large fraction of real Mochi programs.

---

## Sub-phase 5.0: Variant constructors and basic match

### Representation

Sum type `type Shape = Circle(float) | Rectangle(float, float) | Point` lowers
to tagged tuples and bare atoms depending on arity:

| Mochi variant | Core Erlang representation |
|---|---|
| `Point` (unit) | `c_atom("point")` |
| `Circle(3.14)` (single field) | `c_tuple([c_atom("circle"), c_float(3.14)])` |
| `Rectangle(w, h)` (multi-field) | `c_tuple([c_atom("rectangle"), V_w, V_h])` |

The tag atom is always the lowercase variant name. The lowerer derives it with
`strings.ToLower(variant.Name)`.

### Constructor lowering

```
lowerVariantCtor(ctor, args):
  tag = c_atom(strings.ToLower(ctor.Name))
  if len(args) == 0:
    return tag
  cerl_args = [lowerExpr(a) for a in args]
  return c_tuple([tag] + cerl_args)
```

### Match lowering

The MEP-45 match-to-decision-tree pass (Maranget canonicalisation) runs on
aotir **before** the BEAM lowerer sees it. The BEAM lowerer receives a
canonical sorted match tree with unique tags and maps each canonical arm
directly to a `c_clause`.

```
match shape {
  Circle(r)         => expr1
  Rectangle(w, h)   => expr2
  Point             => expr3
}
```

lowers to (via the cerl API):

```go
c_case(V_shape, []cerl.Clause{
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{c_atom("circle"), c_var("V_r")})},
        c_atom("true"),
        lowerExpr(expr1),
    ),
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{c_atom("rectangle"), c_var("V_w"), c_var("V_h")})},
        c_atom("true"),
        lowerExpr(expr2),
    ),
    c_clause(
        []cerl.Expr{c_atom("point")},
        c_atom("true"),
        lowerExpr(expr3),
    ),
})
```

### Non-exhaustive match catch-all

For non-exhaustive matches, the lowerer appends a final catch-all clause:

```go
c_clause(
    []cerl.Expr{c_var("_")},
    c_atom("true"),
    c_call(c_atom("mochi_core"), c_atom("panic_match"), []cerl.Expr{}),
)
```

`mochi_core:panic_match/0` in the runtime raises `erlang:error(mochi_err_match)`.

---

## Sub-phase 5.1: Nested patterns and guard clauses

### Nested patterns

Nested patterns are handled structurally. The Maranget pass from MEP-45 has
already flattened multi-level matches into a decision tree; the BEAM lowerer
emits one `c_clause` per leaf. Each level of nesting becomes a nested pattern
within the same `c_clause` pattern list.

Example: `Some(Circle(r))` over `option<Shape>`:

```go
c_clause(
    []cerl.Expr{
        c_tuple([]cerl.Expr{
            c_atom("some"),
            c_tuple([]cerl.Expr{c_atom("circle"), c_var("V_r")}),
        }),
    },
    c_atom("true"),
    body,
)
```

### Guard clauses

`Circle(r) when r > 0.0 =>` lowers to a `c_clause` with a non-trivial guard
expression. The guard is passed as the second argument to `c_clause`:

```go
guard_expr = c_call(
    c_atom("erlang"), c_atom(">"),
    []cerl.Expr{c_var("V_r"), c_float(0.0)},
)

c_clause(
    []cerl.Expr{c_tuple([]cerl.Expr{c_atom("circle"), c_var("V_r")})},
    guard_expr,
    body,
)
```

BEAM guard constraints: only pure expressions are permitted (no side effects,
no message sends, no ETS writes). The lowerer validates guard expressions for
purity and emits a compile-time error if an impure expression appears in a
guard position. Permitted guard forms: arithmetic, comparison, boolean logic,
type tests (`is_integer/1`, `is_atom/1`, etc.), `size/1`, `element/2`,
`hd/1`, `tl/1`.

### Wildcard patterns

`_ =>` lowers to a fresh uniquely-named variable to avoid collisions when
multiple wildcards appear in the same `case`:

```go
c_clause(
    []cerl.Expr{c_var(fmt.Sprintf("V__wildcard_%d", n))},
    c_atom("true"),
    body,
)
```

where `n` is a per-function monotonically increasing counter.

---

## Sub-phase 5.2: option\<T\> lowering

`option<T>` is a Mochi built-in sum type equivalent to
`type Option<T> = Some(T) | None`. It uses the same tagged representation as
user-defined sum types.

| Mochi | Core Erlang |
|---|---|
| `Some(v)` | `c_tuple([c_atom("some"), lowerExpr(v)])` |
| `None` | `c_atom("none")` |

Match lowering is identical to user-defined sum type match. The `is_some(opt)`
builtin lowers to an inline `c_case`:

```go
c_case(V_opt, []cerl.Clause{
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{c_atom("some"), c_var("_")})},
        c_atom("true"),
        c_atom("true"),
    ),
    c_clause(
        []cerl.Expr{c_var("_")},
        c_atom("true"),
        c_atom("false"),
    ),
})
```

---

## Sub-phase 5.3: Result\<T,E\> lowering

`Result<T,E>` maps to the OTP-native `ok`/`error` tuple idiom so that Mochi
functions returning `Result` are directly composable with OTP library functions.

| Mochi | Core Erlang |
|---|---|
| `Ok(v)` | `c_tuple([c_atom("ok"), lowerExpr(v)])` |
| `Err(e)` | `c_tuple([c_atom("error"), lowerExpr(e)])` |

Match lowering:

```go
// match r { Ok(v) => body1 | Err(e) => body2 }
c_case(V_r, []cerl.Clause{
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{c_atom("ok"), c_var("V_v")})},
        c_atom("true"),
        lowerExpr(body1),
    ),
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{c_atom("error"), c_var("V_e")})},
        c_atom("true"),
        lowerExpr(body2),
    ),
})
```

Mochi `try { ... }` blocks that can fail produce `Result` values. The lowerer
wraps the body in a `c_try` node, maps the success path to
`c_tuple([c_atom("ok"), V_result])`, and maps the catch path to
`c_tuple([c_atom("error"), V_reason])`.

---

## Fixtures

25 fixture files under `tests/dataset/slt/beam/phase05/`:

| File | Tests |
|---|---|
| `001_sum_basic.mochi` | Shape: Circle, Rectangle, Point |
| `002_unit_variants.mochi` | Multiple unit variants, bare-atom representation |
| `003_nested_patterns.mochi` | Nested sum types in match arms |
| `004_option_some_none.mochi` | option\<int\> match, is_some |
| `005_result_ok_err.mochi` | Result\<int,string\> match |
| `006_guard_basic.mochi` | Single guard clause on variant |
| `007_guard_compound.mochi` | AND/OR guards |
| `008_wildcard.mochi` | Wildcard in various positions |
| `009_non_exhaustive.mochi` | Panic-match catch-all clause fires |
| `010_option_chain.mochi` | Chained option operations |
| `011_result_chain.mochi` | Chained result operations |
| `012_nested_option.mochi` | option\<option\<T\>\> |
| `013_sum_in_list.mochi` | List of sum type values |
| `014_sum_in_record.mochi` | Record containing sum type field |
| `015_match_in_fun.mochi` | Match inside anonymous function |
| `016_match_in_loop.mochi` | Match inside loop body |
| `017_multi_guard.mochi` | Multiple guards on same variant |
| `018_result_try.mochi` | try block producing Result |
| `019_option_map.mochi` | Mapping over option values |
| `020_recursive_sum.mochi` | Recursive sum type (binary tree) |
| `021_polymorphic_match.mochi` | Generic sum type match |
| `022_interop_ok_error.mochi` | Interop with OTP {ok,V}/{error,R} returns |
| `023_exhaustive_check.mochi` | All variants covered, no catch-all emitted |
| `024_variant_in_map.mochi` | Map values are sum type |
| `025_complex_nested.mochi` | Deeply nested patterns, 4+ levels |

All fixtures are compiled with `mochi build --target=beam` and stdout is
compared byte-for-byte against vm3 output (correctness gate).

---

## Decisions made

### Why bare atom for unit variants, not zero-tuple

`point` (atom) vs `{point}` (zero-arity tuple): OTP idiom uses bare atoms for
unit variants (`none`, `ok`, `error`). Pattern matching on atoms is O(1) via
atom-table lookup; tuple dispatch requires checking tuple size and extracting
the tag element. This matches how Erlang and Elixir developers expect to see
these types and enables direct interop with OTP library functions that use
`ok`, `error`, `none`, `true`, `false` as unit values.

### Why `mochi_record_tag` for records but atom tag for sums

Records are structural types: all fields are always present, and the tag
exists only for discrimination between record types. Sum variants ARE their
tag; the tag is definitional, not incidental. These are different
representational patterns and both are idiomatic in BEAM. Conflating them
would sacrifice either performance (records paying tuple overhead on every
field access) or clarity (sums using a separate tag field that looks like a
record field).

### Why we let OTP's v3_kernel compile the pattern match decision tree

OTP's `v3_kernel` pass (which runs after Core Erlang in the OTP compiler
pipeline) performs full pattern-match compilation: it converts a flat `case`
with one clause per arm into an optimal decision tree, including heuristics for
minimizing test count and sharing sub-tests. We emit a flat `c_case` in
post-Maranget canonical form (from MEP-45). OTP then compiles this to
optimised BEAM bytecode. Re-implementing the decision tree compiler in the
Mochi BEAM lowerer would duplicate well-tested OTP logic with no performance
benefit since OTP's output is identical regardless of how we structure the
input clauses.

---

## Closeout notes

Implemented as sub-phase 5.0 covering unit variants, field-bearing variants, and basic match lowering including match-as-expression and wildcard arms. Five fixtures (400-404) all pass `TestPhase5SumTypes`.

Key implementation decisions and bugs fixed:

- Unit variants lower to bare atoms; field-bearing variants lower to tagged tuples `{tag, f1, f2, ...}`.
- `LetStmt` with nil `Init` (declaration-only, emitted by the C lowerer for match-as-expression temp vars) is now skipped in the beam lowerer; the binding is established by the wrapping `CLet` in `lowerMatchStmt`.
- Statement-position match (`ResultVar == ""`): cont is threaded into each arm's body and the case expression is returned directly, not wrapped in `CSeq`. The earlier `CSeq(matchExpr, cont)` caused cont to execute twice.
- Expression-position match (`ResultVar != ""`): `lowerMatchArmAsExpr` extracts the value from the final `AssignStmt` in each arm body; the outer `CLet([V_ResultVar], c_case(...), cont)` binds the result.
- Recursive union types are not yet supported (C lowerer phase 4.0 restriction); fixture 403 was simplified from a binary tree to a flat two-variant `Expr` type.
- The `variantToUnion` lookup was added to the C lowerer's `lowerStructLit` so `Green {}` style variant construction is recognized as a variant, not an undeclared record.
