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
| Status         | COMPLETE 2026-05-26 09:06 (GMT+7) |
| Started        | 2026-05-25 16:30 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
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
| 5.2 | Free function as closure shim: `env == NULL` path for top-level functions passed as fun-typed args; `lowerFunRef` lifts a named function reference into a `__shim_<name>` wrapper (IsLifted=true, no captures); shim dedup via `shimFuncs *map[string]bool` shared across all lowerers; `TestPhase5FreeFunctionShim` gate (8 fixtures). | LANDED 2026-05-25 19:12 (GMT+7) | — | — |
| 5.3 | Method as closure shim: `env == &receiver` path for agent intent references; `lowerAgentMethodRefAsValue` converts bare `AgentMethodRef` to a `FunLit` backed by `__methodshim_AGENT_INTENT`; shim casts `__mochi_env` to `mochi_agent_AGENT_t *` and forwards; dedup via `shimFuncs`; `TestPhase5MethodShim` gate (5 fixtures). | LANDED 2026-05-26 04:59 (GMT+7) | (this PR) | — |

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

## Phase 5.2 decisions

**Shim function ABI.** Each named function `foo` referenced as a `fun`-typed value gets a thin `__shim_foo` wrapper with `IsLifted=true`. The emitter prepends `void *__mochi_env` as the first C parameter (matching the `fn` field ABI of `mochi_closure_*` structs). Non-capturing shims silently ignore `__mochi_env`. The shim body is a single `ReturnStmt{Value: CallExpr{Func: "foo", ...}}` (or `CallStmt` if the return type is unit).

**Deduplication via `shimFuncs`.** A `*map[string]bool` pointer (`shimFuncs`) is shared across all lowerers in a translation unit, mirroring the existing `liftedFuncs` sharing pattern. When `lowerFunRef` is called for the same function name a second time, it skips re-emitting the shim but still returns a fresh `FunLit` pointing to the already-emitted `__shim_name`.

**`lowerPrimary` hook.** The check is inserted immediately after the scope lookup fails: if `pr.Selector.Root` resolves in `l.funcs` (the named-function table) and `pr.Selector.Tail` is empty (no field access), `lowerFunRef` is called. A function name with a field tail (e.g., `foo.bar`) is not a valid shim reference and falls through to the existing "undeclared variable" error.

**Scalar primitives only in Phase 5.2.** Params and return types are validated to be `int`, `float`, `bool`, `string`, or `unit`. Complex types (records, unions, lists, maps) as shim param/return are deferred to Phase 5.x after the full closure type-widening pass.

**No verifier changes required.** The shim IR is structurally identical to an anonymous non-capturing lifted function: `IsLifted=true`, `EnvTypeName=""`, `Captures=nil`. The verifier already handles this shape from Phase 5.0.

## Phase 5.3 decisions

**`env == &receiver` shim for agent intents.** Phase 5.3 extends the shim machinery to agent method references. When `c.increment` appears as a value (not immediately called), `lowerPostfix` detects the unresolved `AgentMethodRef` at the end of the op chain and calls `lowerAgentMethodRefAsValue`. This emits a static shim function `__methodshim_AGENT_INTENT(void *__mochi_env, params...)` that casts `__mochi_env` to `mochi_agent_AGENT_t *__self` and forwards to `mochi_agent_AGENT__INTENT(__self, params...)`. The returned `FunLit` carries `EnvVarName: "&receiver_name"` so the closure struct is `{.fn=__methodshim_..., .env=(void *)&c}`.

**Receiver constraint (Phase 5.3).** The agent receiver in the `AgentMethodRef` must be a `*aotir.VarRef` so we can take `&name` for the env. Non-VarRef receivers (e.g., field access chains or calls that return agents) are rejected with a diagnostic; support for those is deferred to a later sub-phase.

**Shim deduplication.** `lowerAgentMethodRefAsValue` reuses the existing `shimFuncs *map[string]bool` shared across all lowerers in a TU. A second reference to the same `agent.intent` pair skips re-emitting the shim but still returns a fresh `FunLit` pointing to the same `__methodshim_*` function.

**Scalar primitives only.** Param and return types of the intent are validated to be `int`, `float`, `bool`, `string`, or `unit`. Complex types (records, unions, lists, maps) as method closure param/return are deferred.

**`lowerPostfix` hook.** The `AgentMethodRef`-to-`FunLit` conversion is at the very end of `lowerPostfix`, after all ops have been processed. This covers both the `Primary.Selector.Tail` path (field access inside a selector chain) and the `PostfixOp.Field` path (explicit postfix field op). Both paths pass through the same exit point in `lowerPostfix`.

## Bug fixes in this phase

- `verifyLetStmt` was missing `funSig: s.FunSig` when registering a TypeFun binding in scope. Without it, verifier lookups for fun-typed variables would lose the FunSig, causing indirect calls to fail signature checking.
- Variable named `double` (C keyword) in initial fixture collided with the C type name. Fixture renamed to use `double_it`.
- Closure inside named function incorrectly captured outer function parameter `n` (a capturing closure). Fixed fixture to use only the closure's own parameter.
- String concat (`+` on string operands) is not supported in the lower pass for closures in Phase 5.0. Fixture replaced with if/return arms over string literals.

## Deferred work

- Capturing closures (Phase 5.1): free variables need an env struct heap-allocated and a fat pointer representation.
- Top-level named functions passed as `fun`-typed arguments: implemented in Phase 5.2 using hand-written `expect.txt` files (vm3 oracle limitation bypassed).
- Closures over complex types (record/union/list/map) as parameters or return: deferred to Phase 5.x after fat-pointer support lands.
- Escape analysis for stack-allocated env: deferred to v2.

## Closeout notes

All 4 sub-phases (5.0-5.3) are LANDED. TestPhase5Closures, TestPhase5CapturingClosures, TestPhase5FreeFunctionShim, and TestPhase5MethodShim are green on every tier-1 host. Phase 5 is COMPLETE.
