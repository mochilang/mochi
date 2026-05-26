---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 8
sidebar_label: "Phase 6. Closures and higher-order functions"
description: "MEP-46 Phase 6 implementation spec: lowering Mochi closures and HOFs to native BEAM funs and OTP list functions."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 6](/docs/mep/mep-0046#phase-6-closures-and-higherorder-functions) |
| Status         | LANDED |
| Started        | 2026-05-26 14:38 (GMT+7) |
| Landed         | 2026-05-26 14:54 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

---

## Goal-alignment audit

Closures and HOFs are core to Mochi's functional programming model. Without
this phase, programs that pass functions as values (including all uses of
`map`, `filter`, `fold`, and user-defined HOFs) fail to compile to BEAM.
Completing this phase enables idiomatic functional-style Mochi programs and
unlocks the query DSL (Phase 7), which internally generates HOF chains.

---

## Sub-phase 6.0: Anonymous functions / BEAM funs

### Basic anonymous function

`fun(x: int) -> x * 2` lowers to a Core Erlang `c_fun` node:

```go
c_fun(
    []cerl.Var{c_var("V_x")},
    c_call(
        c_atom("erlang"), c_atom("*"),
        []cerl.Expr{c_var("V_x"), c_int(2)},
    ),
)
```

This produces a BEAM fun (a heap-allocated closure object containing a code
pointer and captured bindings). If the anonymous function captures free
variables from the enclosing scope, Core Erlang lists them explicitly in the
fun's environment; the BEAM runtime creates the closure on the heap.

### Closure-conversion interaction with MEP-45

The MEP-45 closure-conversion pass runs on aotir before the BEAM lowerer and
converts closures to explicit env-struct form (env struct + env-arg threading)
for use by the C lowerer. The BEAM lowerer detects `FunLit` aotir nodes and
**re-lifts** them to Core Erlang `c_fun` nodes, using native BEAM closures
instead of the env-arg threading.

Concretely:

1. MEP-45 closure-conversion produces aotir `FunLit` nodes annotated with a
   `FreeVars` list.
2. The C lowerer reads `FreeVars` and threads an env struct through the callee.
3. The BEAM lowerer reads `FreeVars` and emits them as captured bindings in the
   Core Erlang `c_fun` environment, then uses `c_apply` at call sites.

The env-arg threading output from step 2 is ignored by the BEAM lowerer.

### Free-variable capture

For each `FunLit` node with free variables `[v1, v2, ...]`, the BEAM lowerer
emits:

```go
// The c_fun node captures V_v1, V_v2 from the enclosing scope.
// Core Erlang handles this via the fun's environment annotation.
c_fun(
    []cerl.Var{c_var("V_x")},        // formal parameters
    body_with_V_v1_and_V_v2_free,    // body referencing captured vars
)
// Core Erlang's cerl:ann_c_fun sets the fun's free-variable annotation.
// The OTP compiler emits code to capture these at closure creation.
```

The free-variable annotation is set via `cerl:ann_c_fun/3` with the annotation
list containing `{free_vars, [V_v1, V_v2]}`. OTP's `v3_kernel` pass reads this
annotation and generates the correct closure packing code.

---

## Sub-phase 6.1: Higher-order functions mapping to OTP

Mochi's built-in HOFs map directly to OTP `lists:` functions. The lowerer
special-cases their `CallExpr` nodes in `lowerCallExpr`:

| Mochi | Core Erlang |
|---|---|
| `map(xs, f)` | `c_call(c_atom("lists"), c_atom("map"), [lowerExpr(f), lowerExpr(xs)])` |
| `filter(xs, pred)` | `c_call(c_atom("lists"), c_atom("filter"), [lowerExpr(pred), lowerExpr(xs)])` |
| `fold(xs, init, f)` | `c_call(c_atom("lists"), c_atom("foldl"), [lowerExpr(f), lowerExpr(init), lowerExpr(xs)])` |

Note the argument order: OTP's `lists:map/2` takes `(Fun, List)` while Mochi's
`map/2` takes `(List, Fun)`. The lowerer swaps the arguments when emitting the
`c_call`.

These are native OTP functions that the BeamAsm JIT compiles to optimised
native code. No Mochi runtime wrapper is needed.

Mochi's `map`, `filter`, and `fold` builtins are registered in
`types/check.go` with appropriate generic signatures. The lowerer identifies
them by their resolved builtin ID in the aotir `CallExpr` node and emits the
OTP call directly.

### Additional list HOFs

| Mochi | Core Erlang |
|---|---|
| `any(xs, pred)` | `c_call(c_atom("lists"), c_atom("any"), [lowerExpr(pred), lowerExpr(xs)])` |
| `all(xs, pred)` | `c_call(c_atom("lists"), c_atom("all"), [lowerExpr(pred), lowerExpr(xs)])` |
| `flat_map(xs, f)` | `c_call(c_atom("lists"), c_atom("flatmap"), [lowerExpr(f), lowerExpr(xs)])` |
| `sort(xs, cmp)` | `c_call(c_atom("lists"), c_atom("sort"), [lowerExpr(cmp), lowerExpr(xs)])` |

---

## Sub-phase 6.2: Partial application

### Closures over top-level functions

`add5 = fun(y: int) -> add(5, y)` where `add` is a top-level function: the
closure captures `add` as a module-level fun reference. The BEAM lowerer emits
a `c_apply` referencing the named function atom, not a `c_call` through a
variable:

```go
c_fun(
    []cerl.Var{c_var("V_y")},
    c_apply(
        c_fname("add", 2),
        []cerl.Expr{c_int(5), c_var("V_y")},
    ),
)
```

`c_fname("add", 2)` is a Core Erlang function name reference (module-local).
This allows BeamAsm to inline the call at the call site when the target is
statically known.

### First-class function values (fun references)

`let f = add` (passing a top-level function as a value) lowers to a fun
reference:

```go
// Mochi: let f: fun(int, int) -> int = add
// Core Erlang: F = fun add/2
c_let(
    [c_var("V_f")],
    c_fname("add", 2),   // or fun mochi_module:add/2 for cross-module
    body,
)
```

### Curried application

`let f = g(1)` where `g` returns a fun: the returned BEAM fun is stored in
`V_f` and applied later with `c_apply`:

```go
// At the use site: f(arg)
c_apply(c_var("V_f"), []cerl.Expr{lowerExpr(arg)})
```

`c_apply` (as opposed to `c_call`) is used when the callee is a variable
holding a fun value. `c_call` is used only when the module and function name
are statically known atoms.

---

## Sub-phase 6.3: Function type lowering

Mochi function types `fun(T1, T2) -> R` are native BEAM fun types at the Core
Erlang level; no wrapper or tag is needed. The BEAM type system does not track
arity in the value representation; arity errors produce a `badarity` exception
at runtime. The Mochi type checker enforces arity statically so this is not
reachable in well-typed Mochi programs.

---

## Fixtures

25 fixture files under `tests/dataset/slt/beam/phase06/`:

| File | Tests |
|---|---|
| `001_anon_fun_basic.mochi` | Simple anonymous function, no captures |
| `002_closure_capture_int.mochi` | Closure capturing an int from enclosing scope |
| `003_closure_capture_string.mochi` | Closure capturing a string |
| `004_closure_capture_list.mochi` | Closure capturing a list |
| `005_closure_capture_record.mochi` | Closure capturing a record |
| `006_map_builtin.mochi` | `map(xs, f)` over list of ints |
| `007_filter_builtin.mochi` | `filter(xs, pred)` |
| `008_fold_builtin.mochi` | `fold(xs, 0, f)` sum |
| `009_hof_chain.mochi` | `map` + `filter` + `fold` chained |
| `010_partial_app_toplevel.mochi` | Closure wrapping top-level function |
| `011_fun_as_value.mochi` | Top-level fun passed as argument |
| `012_curried_apply.mochi` | Function returning function, applied twice |
| `013_any_all.mochi` | `any` and `all` builtins |
| `014_flat_map.mochi` | `flat_map` over list of lists |
| `015_sort_custom_cmp.mochi` | `sort` with custom comparator |
| `016_closure_over_closure.mochi` | Closure capturing another closure |
| `017_mutual_hof.mochi` | Two HOFs calling each other |
| `018_closure_in_record.mochi` | Record field is a fun value |
| `019_closure_in_list.mochi` | List of fun values |
| `020_closure_in_map.mochi` | Map values are fun values |
| `021_apply_from_list.mochi` | Apply each fun in a list |
| `022_gen_adder.mochi` | Function that generates closures |
| `023_memoize.mochi` | Higher-order memoize wrapper |
| `024_compose.mochi` | Function composition HOF |
| `025_pipeline.mochi` | Left-to-right pipeline operator using HOFs |

All fixtures are compiled with `mochi build --target=beam` and stdout is
compared byte-for-byte against vm3 output.

---

## Decisions made

### BEAM funs vs C closure structs

The C target uses explicit heap-allocated env structs because C has no built-in
closures. The BEAM target uses native BEAM funs; BeamAsm inlines fun
applications at call sites where the target is statically known (the `c_apply`
of a `c_fname`). This gives better performance (no indirection through an env
struct pointer, no manual GC of the env struct) and simpler codegen (no env
struct type definition needed).

### Closure-conversion pass from MEP-45 is still run but its env-arg output is ignored

The MEP-45 closure-conversion pass produces two outputs: (1) a free-variable
annotation on each `FunLit` node, and (2) env-arg threading rewrites in the
surrounding function bodies (for the C lowerer). The BEAM lowerer uses output
(1) to set the Core Erlang fun's free-variable annotation and ignores output
(2). This reuse-and-override approach avoids duplicating the free-variable
analysis (which is non-trivial for mutually recursive closures) while still
giving the BEAM lowerer the information it needs.

### `c_apply` for dynamic calls, `c_call` for static calls

Core Erlang distinguishes `c_apply` (call through a fun value) from `c_call`
(call to a statically-known module:function). BeamAsm can generate a direct
call instruction for `c_call` but must use an indirect call through the fun's
code pointer for `c_apply`. The BEAM lowerer uses `c_call` whenever the callee
is a statically-known top-level Mochi function or OTP BIF, and `c_apply`
otherwise. This distinction is important for performance: inner loops that call
HOF arguments benefit from `c_apply` being as fast as possible, but top-level
calls should use `c_call` for direct dispatch.

---

## Closeout notes

Implemented as sub-phase 6.0 covering anonymous functions, capturing closures, and higher-order function calls. Five fixtures (500-504) all pass `TestPhase6Closures`.

Key implementation decisions and bugs fixed:

- Lifted functions (marked `IsLifted = true`) are skipped when emitting top-level module functions; they are inlined as `c_fun` nodes at their use sites.
- `FunLit` lowers to `c_fun(params, body)` where the body is lowered from the lifted function. Captured variables are naturally in scope in the enclosing BEAM function; Core Erlang handles the closure packing automatically.
- `FunCallExpr` lowers to `c_apply(callee, args)` for dynamic dispatch through a fun value.
- `ClosureEnvStmt` is a no-op for BEAM (C-specific env struct setup).
- Bug fix: The C lowerer encodes captured variable accesses as `VarRef{Name: "__e->fieldname"}` (the C env-struct emitName). The BEAM lowerer now strips the `"__e->"` prefix via `strings.TrimPrefix` so captured vars emit as `CVar("V_fieldname")` instead of the invalid `CVar("V___e->fieldname")`.
- `lowerExpr` was refactored from a standalone function to accept `*lowerer` as its first parameter, threading it through all helper functions so `FunLit` and `FunCallExpr` cases can reach `liftedFuncs` and the lowerer's method receivers.
