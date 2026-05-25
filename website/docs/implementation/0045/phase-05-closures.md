---
title: "Phase 5. Closures and higher-order functions"
sidebar_position: 7
sidebar_label: "Phase 5. Closures"
description: "MEP-45 Phase 5 tracking: non-capturing closure lifting, C function-pointer representation, typed fun-parameter/return support."
---

# Phase 5. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 5](/docs/mep/mep-0045#phase-5-closures-and-higher-order-functions) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 16:30 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

8-fixture suite under `tests/transpiler3/c/fixtures/closures/`: simple one-arg closure, two-argument closure, bool-returning closure, string-returning closure with if/return arms, float closure, closure defined inside a named function (non-capturing), multiple closures of same and different fun types in one program, block-body closure. All 8 fixtures compile + run byte-equal vs expected output. `TestPhase5Closures` gate is green.

## Goal-alignment audit

Higher-order combinators are how Mochi expresses data transformation; the query DSL (Phase 8) and agent pipelines (Phase 9) lean heavily on first-class functions. Phase 5.0 establishes the C function-pointer infrastructure (fun-typed IR nodes, typedef generation, lifted function emission) that all subsequent closure work builds on. Restricting to non-capturing closures in 5.0 gives a clean, verifiable foundation with zero GC complexity. Aligns directly with the user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 5.0 | Non-capturing closure support: `FunLit` IR node (lifted to top-level aotir.Function); `FunCallExpr` for calling fun-typed variables; `FunSig` type + `FunTypeName()` C typedef names; `TypeFun` type enum entry; `collectFunSigs` + `emitFunTypedefs` emit passes; verifier updated to carry `FunSig` on TypeFun bindings; lower pass: `lowerFunExpr` lifts anonymous functions, `lowerFunVarCall` for indirect calls; `TestPhase5Closures` gate (8 fixtures). Scalar primitive param/return types only (int, float, bool, string). Unit return supported. | LANDED 2026-05-25 16:30 (GMT+7) | — | — |
| 5.1 | Capturing closures (free-variable capture by value): env struct heap-allocated, fat pointer `{fn, env}` struct representation. All closures (capturing and non-capturing) use `mochi_closure_*` struct typedef; non-capturing ones set `env=NULL`. Free-variable scanner (`scanFreeVarNames`) pre-scans parser AST; `ClosureEnvStmt` IR node for env alloc; `emitName` field on `lbinding` rewrites captured VarRef to `__e->fieldname`; lifted functions add `void *__mochi_env` first param. `TestPhase5CapturingClosures` gate (8 fixtures). | LANDED 2026-05-25 18:55 (GMT+7) | — | — |
| 5.2 | Free function as closure shim: `env == NULL` path for top-level functions passed as fun-typed args | NOT STARTED | — | — |
| 5.3 | Method as closure shim: `env == self` path for method references | NOT STARTED | — | — |

## Decisions made

**C representation for non-capturing closures.** Each `fun(x: T): R => body` literal is lifted to a top-level C function with a generated name (`__anon_N`). The lifting happens in `lowerFunExpr`, which creates a new `aotir.Function` with a fresh scope (no parent chain) and appends it to `prog.Functions`. The `FunLit` IR node records the lifted function name; the emitter renders it as a bare function pointer (no cast needed since C function names decay to pointers).

**C typedef names.** Each unique `FunSig` gets a `typedef` of the form:
```c
typedef int64_t (*mochi_fnptr_i64_to_i64)(int64_t);
typedef bool (*mochi_fnptr_i64_to_bool)(int64_t);
typedef void (*mochi_fnptr_i64_to_void)(int64_t);
```
The collector (`collectFunSigs`) walks all program nodes to find unique signatures and deduplicates by typedef name. Typedefs are emitted before any struct or function declarations so all usage sites see the type.

**Non-capturing enforced by fresh scope.** `lowerFunExpr` creates the inner lowerer with `newLScope(nil)` (no parent), so any reference to an outer-scope variable produces an "undeclared variable" error. This cleanly prevents Phase 5.0 from silently capturing variables; capturing closures are deferred to Phase 5.1.

**Fun-typed let bindings.** `LetStmt.FunSig` carries the signature when `VarType==TypeFun`. The emitter handles TypeFun specially: it emits `mochi_fnptr_<sig> <name> = <funlit-name>;` rather than going through the generic `cTypeFull` path (which does not handle TypeFun).

**Fun-typed parameters.** Functions can accept `fun(T): R` parameters. The lower pass stores the `FunSig` on `Param.FunSig` and the verifier propagates it into the scope binding for the parameter name. Call-site arguments are type-checked to be `TypeFun` expressions.

## Phase 5.1 decisions

**Fat-pointer struct for all closures.** Phase 5.1 changes the C representation of all closure values from a bare function pointer (`mochi_fnptr_*`) to a struct (`mochi_closure_*`):
```c
typedef struct { int64_t (*fn)(void *, int64_t); void *env; } mochi_closure_i64_to_i64;
```
This uniform representation means the calling convention is identical for capturing and non-capturing closures. Non-capturing closures pass `env=NULL`; capturing closures pass a pointer to a malloc'ed struct.

**Env struct typedef naming.** Each capturing lifted function `__anon_N` gets a `typedef struct { ... } __anon_N_env_t;` emitted immediately before the function definition. The struct fields are the captured variables in sorted order (deterministic across runs). The variable holding the env pointer is named `__anon_N_env` in the caller.

**Free-variable scanner.** `scanFreeVarNames(fe *parser.FunExpr, paramNames map[string]bool) []string` walks the closure body (both ExprBody and BlockBody forms) and collects all `SelectorExpr.Root` identifiers not in the parameter set and not declared locally (by a let/var/for statement in the body). The result is sorted for determinism. The scanner does NOT recurse into nested `FunExpr` nodes (nested closures form their own capture chain).

**ClosureEnvStmt IR node.** When a capturing FunLit is lowered, `lowerBinding` emits a `ClosureEnvStmt` immediately before the `LetStmt` that binds the closure. The emitter renders it as:
```c
__anon_2_env_t *__anon_2_env = (__anon_2_env_t *)malloc(sizeof(__anon_2_env_t));
__anon_2_env->x = x;
__anon_2_env->base = base;
```

**`emitName` on `lbinding`.** Captured variables in the inner closure scope are seeded with `emitName: "__e->fieldname"`. When `lowerPrimary` constructs a `VarRef` for such a binding, it uses `emitName` as the `VarRef.Name`. The C emitter then generates `__e->x` directly without needing a special IR node.

**`void *__mochi_env` first parameter.** All lifted functions (IsLifted=true) receive `void *__mochi_env` as their first C parameter. Non-capturing lifted functions ignore it. Capturing lifted functions cast it to their env type: `__anon_2_env_t *__e = (__anon_2_env_t *)__mochi_env;`.

**Verifier scope seeding.** When verifying a capturing lifted function, the verifier scope is pre-seeded with `"__e->fieldname" -> type` bindings so VarRef nodes with env-relative names pass the unresolved-variable check.

**`ResultFunSig` on `CallExpr`.** Phase 5.1 needed to propagate the function signature through user function calls that return `TypeFun`. Added `ResultFunSig *FunSig` to `CallExpr`, `returnFunSig *aotir.FunSig` to `funcSig`, and `ReturnFunSig *FunSig` to `aotir.Function`. This enables `let f = make_adder(5); f(3)` where `make_adder` returns a closure.

## Bug fixes in this phase

- `verifyLetStmt` was missing `funSig: s.FunSig` when registering a TypeFun binding in scope. Without it, verifier lookups for fun-typed variables would lose the FunSig, causing indirect calls to fail signature checking.
- Variable named `double` (C keyword) in initial fixture collided with the C type name. Fixture renamed to use `double_it`.
- Closure inside named function incorrectly captured outer function parameter `n` (a capturing closure). Fixed fixture to use only the closure's own parameter.
- String concat (`+` on string operands) is not supported in the lower pass for closures in Phase 5.0. Fixture replaced with if/return arms over string literals.

## Deferred work

- Capturing closures (Phase 5.1): free variables need an env struct heap-allocated and a fat pointer representation.
- Top-level named functions passed as `fun`-typed arguments: the vm3 runtime does not handle this correctly (returns nil), so this pattern is deferred until the vm3 oracle is fixed or an alternative comparison method is available.
- Closures over complex types (record/union/list/map) as parameters or return: deferred to Phase 5.x after fat-pointer support lands.
- Escape analysis for stack-allocated env: deferred to v2.

## Closeout notes

_Fill in after gate fully green (all 4 sub-phases)._
