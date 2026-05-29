---
title: "Phase 6. Top-level user functions"
sidebar_position: 11
sidebar_label: "Phase 6. Functions"
description: "MEP-54 Phase 6.0, user-defined fun declarations lower to Go funcs (mangled mochi__name)."
---

# Phase 6. Top-level user functions

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22529](https://github.com/mochilang/mochi/pull/22529) |
| Commit         | 9f8c04e82e |

## Gate

19 new fixtures cover the user-function surface. Returns by type: `fun_return_int`, `fun_return_float`, `fun_return_bool`, `fun_return_string`, `fun_return_unit`. Arg types: `fun_arg_string`, `fun_arg_bool`, `fun_arg_record`, `fun_arg_list`. Composition: `fun_no_args`, `fun_two_funs`, `fun_call_twice`, `fun_pass_var`, `fun_record_return`. Call sites: `fun_call_in_if`, `fun_call_in_loop`, `fun_call_in_arith`. Other: `fun_basic` (add), `fun_recursive` (factorial). Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/fun_`.

## Lowering decisions

Each non-`Main` `aotir.Function` lowers to a Go `func mochi__<name>(...) <ret> { ... }` declaration placed above `main`. The mangled `mochi__` prefix is already stamped on by the shared C lowerer, so call sites need no name resolution: a Mochi `add(1, 2)` call becomes `mochi__add(1, 2)` whether it appears in `main` or in another `fun`.

Param and return types thread through new helpers `paramTypeText` and `returnTypeText` that mirror `letTypeText`'s compound-type dispatch. Both handle the full type surface lit by Phases 0-5: record, union, list, map, set, plus the four scalars. `lowerCallExpr` produces a `gotree.CallExpr` for user-function calls used as values; the default branch of `CallStmt` covers user-function calls used as statements (no return capture).

Recursive functions work without extra lifting because each Go func is a top-level declaration and Go allows self-reference at the package level. Mutual recursion would work similarly; the C lowerer emits both functions, the Go lowerer translates each independently, and Go's forward references handle the order.

Deferred to Phase 6.1: lifted closures (`FunLit`, `Captures`, `ClosureEnvStmt`), first-class function values, `TypeFun` in `lowerType`. Phase 6.0 covers the "top-level declarations called by name" pattern; Phase 6.1 then layers lambdas + captures on top.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/lower.go` | `lowerFunction(fn)` -> `gotree.FuncDecl` for each non-Main function |
| `transpiler3/go/lower/types.go` | `paramTypeText`, `returnTypeText` mirror `letTypeText` |
| `transpiler3/go/lower/expr.go` | `lowerCallExpr` -> `gotree.CallExpr` for user-function calls |
| `transpiler3/go/lower/stmt.go` | `CallStmt` default branch handles user-function call as a statement |
| `tests/transpiler3/go/fixtures/fun_*/` | 19 fixtures |

## Test set

- 19 `TestPhase1Hello/fun_*` subtests covering return types, arg types, composition, call sites, and recursion.

## Closeout notes

The `mochi__` prefix means generated Go code never clashes with stdlib names or the runtime helper names (`mochiLines`, `mochiPanic`, etc.) which use a `mochi` prefix without the double underscore. Splitting Phase 6 into 6.0 (top-level functions) and 6.1 (closures) was the right call: Phase 7.1 query DSL only needs top-level functions, so 6.0 unblocked the query work without waiting for the closure infrastructure.
