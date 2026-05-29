---
title: "Phase 6.1. Closures (FunLit + Captures + ClosureEnv)"
sidebar_position: 12
sidebar_label: "Phase 6.1. Closures"
description: "MEP-54 Phase 6.1, first-class function values with captured envs lowered via typed __mochi_env *EnvType first param."
---

# Phase 6.1. Closures

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22546](https://github.com/mochilang/mochi/pull/22546) |
| Commit         | dcb6d58867 |

## Gate

18 fixtures cover non-capturing lambdas of every scalar shape, lambdas as args and returns, single / multi-capture closures, closure factories (`makeAdder`), and shimming free functions into `fun`-typed params. Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/closure_`.

## Lowering decisions

Lifted lambdas (emitted by the C lowerer as top-level `aotir.Function` declarations with a `Captures` list) take a typed `__mochi_env *EnvType` first param. The env struct is a Go `type closureEnv_<id> struct { Field1 T1; Field2 T2; ... }` whose fields are the captured variables. `FunLit` allocates the env struct inline via IIFE and returns a wrapper that forwards captures to the lifted function:

```go
func() func(int64) int64 {
    env := &closureEnv_3{Field1: x}
    return func(y int64) int64 { return mochi__lambda_3(env, y) }
}()
```

This shape lets capturing lambdas work both as let bindings and as return values; the canonical `makeAdder` pattern (`fun makeAdder(n) -> fun(x) -> n + x`) compiles to a closure factory whose returned function value carries the env by reference.

Captured variable references emitted by the C lowerer as `__e->field` are translated to `__mochi_env.Field` selectors via a name-rewriting pass in `lowerExpr`. The Go struct's exported fields use the capitalised form (matching the Phase 3.4 record convention).

Free functions passed where a `fun` type is expected get a shim wrapper that adapts the no-env Go func signature to the `func(env, ...args)` shape the call site expects. `ClosureEnvStmt` is elided in the Go output because env allocation is co-located with the `FunLit`; the C runtime needed a separate statement for the env, the Go runtime does not.

`TypeFun` is added to `lowerType` so `fun` values can be locals (`var f func(int64) int64 = ...`), function args, and return values uniformly. `lowerFunType(sig)` produces `func(T1, T2, ...) R` from an `aotir.FunSig`, restricting the param and return types to scalars and unit (matching the aotir `FunSig` restriction).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerFunType(sig)` -> `func(T1, ...) R` |
| `transpiler3/go/lower/lower.go` | Lifted closures take `__mochi_env *EnvType` first param; env struct declarations emitted alongside the lifted function |
| `transpiler3/go/lower/expr.go` | `FunLit` -> IIFE wrapper; `VarRef` rewriting `__e->field` -> `__mochi_env.Field`; free-function shim wrapping |
| `transpiler3/go/lower/stmt.go` | `ClosureEnvStmt` elided |
| `tests/transpiler3/go/fixtures/closure_*/` | 18 fixtures |

## Test set

- 18 `TestPhase1Hello/closure_*` subtests covering non-capturing lambdas, captures, factories, and free-function shims.

## Closeout notes

Lifting closures into a typed env struct (rather than Go's idiomatic native closure) was forced by the aotir layer: the shared C lowerer has already done the env-capture analysis and emits lifted functions with explicit env parameters, so the Go lowerer either honours that shape or re-derives the env analysis. Honouring it kept the Go lowerer thin. The IIFE wrapper around `FunLit` is the price of supporting closures-as-return-values: a bare lifted-function reference would not carry the env. Eliding `ClosureEnvStmt` was a Go-specific optimisation; the C path needed the explicit env materialisation but Go's GC handles the env struct lifetime as long as the closure holds it.
