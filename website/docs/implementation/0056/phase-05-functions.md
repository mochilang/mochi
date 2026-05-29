---
title: "Phase 5. User functions"
sidebar_position: 9
sidebar_label: "Phase 5. User functions"
description: "MEP-56 Phase 5, user-defined functions to def self.name, recursion, chaining."
---

# Phase 5. User functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 4738082cde |

## Gate

`TestPhase5UserFuncs` in `transpiler3/ruby/build/phase05_test.go`: three inline subtests, `fun_simple`, `fun_recursive`, and `fun_chain`. Each subtest compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. `fun_simple` covers a non-recursive two-arg add; `fun_recursive` covers `factorial(5) = 120`; `fun_chain` covers `quad(3) = double(double(3)) = 12`, validating that one user function can call another.

## Lowering decisions

Each non-main `aotir.Function` in the program lowers via `lowerFunction` (lower.go lines 133 to 154) to an `rtree.MethodDecl{Receiver: "self", Name: fn.Name, Params: [...], Body: [...]}`. The receiver is `self` because the enclosing scope is the `Main` module, and `def self.name` defines a module-level method callable as `Main.name(args)` (or, from inside `Main.run`, as a bare `name(args)`). The method shape mirrors the `run` method: `def self.fn(a, b); ...; end`.

Parameters lower to `rtree.MethodParam{Name: rubyIdent(p.Name)}`, applying the same keyword mangler used elsewhere. The lowerer drops the Mochi parameter type annotations entirely: Ruby is duck-typed and the typecheck pass has already validated arg types upstream, so emitting `a, b` (rather than the C target's `long long a, long long b`) is both correct and idiomatic. The function body lowers via `lowerBlock`, the same path used for the `run` body.

`aotir.CallStmt` for a non-builtin function falls through `lowerCallStmt` (lower.go lines 519 to 530) to a bare `MethodCall{Method: rubyMethodName(c.Func), Args: args, UseParens: true}`. The `rubyMethodName` shim is currently a pass-through; it exists as a hook for later phases that need to mangle mochi-internal prefixes or fold dotted names. `aotir.CallExpr` (a function call in expression position) lowers identically via `lowerExpr`'s `CallExpr` arm. From inside `Main.run` (or another `Main` method), `add(2, 3)` resolves to `Main.add(2, 3)` because Ruby's method lookup walks the enclosing class.

`aotir.ReturnStmt` lowers to `rtree.Return{X: v}` when a value is present, or a bare `rtree.Return{}` otherwise. Ruby treats the last expression of a method as its return value, so the explicit `return` is not strictly required, but emitting it makes the rendered code easier to read against the Mochi source. Recursion works because each `def self.fn` call goes through the module's method table at runtime; there is no static linking step that could miss the cycle.

Phase 5 does not yet support lifted closures, which arrive in Phase 7. The `IsLifted` branch in `lowerFunction` (which prepends `__env` to the parameter list) is dead in this phase but already in the source, so Phase 7 can land its `FunLit` lowering without touching the user-function shape again.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerFunction` producing `def self.name(params)`, loop appending non-main functions into `Main` module's `mainDecls`, `CallStmt`/`CallExpr` fallthrough for user calls, `ReturnStmt` arm |
| `transpiler3/ruby/build/phase05_test.go` | `TestPhase5UserFuncs` with 3 subtests |

## Test set

- `TestPhase5UserFuncs/fun_simple`, `fun_recursive`, `fun_chain`.

## Closeout notes

Phase 5 landed on CRuby 3.4 with all three subtests green. The `fun_recursive` factorial fixture validates that `n * factorial(n - 1)` resolves correctly through `Main.factorial`'s self-reference, which Ruby handles because module-method lookup uses dynamic dispatch. Key implementation insight: defining each user function as `def self.name` (a singleton method on the `Main` module) rather than a top-level method keeps the namespace clean and lets multi-file Mochi builds (a later phase) drop two modules' functions into the same Ruby process without collisions. The `IsLifted` branch was pre-wired but is exercised only when Phase 7's `FunLit` lowering lands.
