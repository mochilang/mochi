---
title: "Phase 7. Closures and higher-order functions"
sidebar_position: 11
sidebar_label: "Phase 7. Closures and higher-order functions"
description: "MEP-56 Phase 7, fun(...): T => expr lowered to Ruby lambdas over lifted module methods, capturing environments via a hash."
---

# Phase 7. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | a25a09e775 |

## Gate

`TestPhase7Closures` in `transpiler3/ruby/build/phase07_test.go`: six inline subtests, `closure_simple`, `closure_two_arg`, `closure_in_function`, `capture_int`, `capture_multi`, and `capture_string`. Each subtest compiles a Mochi source via `Driver.Build`, executes the `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. `closure_simple` additionally asserts the rendered source contains `def self.__anon_` (the lifted anonymous method) and `double_it.call(21)` (the Proc invocation), and `capture_int` asserts `:x =>` appears (the env-hash entry keyed by captured field name). The set covers non-capturing one-arg, non-capturing two-arg, closures defined inside functions, single-int capture, multi-int capture, and string capture, which together exercise both the lifting machinery and the env-hash plumbing.

## Lowering decisions

Mochi's C front-end lifts every `fun(...): T => expr` to a top-level `aotir.Function` with `IsLifted = true` and an auto-generated `__anon_N` name; the closure expression itself becomes an `aotir.FunLit{FuncName, EnvVarName, Sig}` reference. The Ruby lowerer threads that lifted function through `lowerFunction` (lower.go lines 133 to 154), which prepends a `__env` parameter slot to the param list for lifted functions, so the rendered Ruby method is `def self.__anon_0(__env, __a0); ...; end`. Non-lifted user functions still get the `__env` slot in callers but pass `nil`, keeping the ABI uniform across lifted and non-lifted callees.

`aotir.FunLit` lowers via `lowerFunLit` (lower.go lines 1341 to 1367) to a Ruby lambda literal of the form `->(__a0, __a1) { __anon_0(env_or_nil, __a0, __a1) }`. The lambda is a first-class Proc, so the caller can bind it to a local with `let f = fun(...) => ...` and pass it around; the body just forwards to the lifted module method. For non-capturing closures `EnvVarName` is empty and the env argument is the literal `nil`; for capturing ones it is the identifier produced by `lowerClosureEnvStmt`.

`aotir.ClosureEnvStmt` (lower.go lines 1327 to 1339) precedes the `LetStmt` binding the capturing FunLit. It lowers to an `rtree.Assign` whose RHS is a Ruby hash literal of the form `{:x => x, :y => y}`, keying each capture by the source-side `FieldName` (which is also the Ruby symbol the lifted method uses to read it). Using a hash, not an Array or a Struct, was a deliberate trade: the lifted method sees `__env[:x]` rather than positional `__env[0]`, so a future refactor that reorders captures cannot silently swap values, and the `:x =>` substring asserted in `capture_int` locks that contract.

`aotir.FunCallExpr` lowers via `lowerFunCallExpr` (lower.go lines 1369 to 1399). When the callee is a direct `FunLit`, the lowerer skips the Proc indirection and emits a direct call to the lifted method with the env hash prepended, so `(fun(x) => x * 2)(21)` renders as `__anon_0(nil, 21)`. When the callee is a `VarRef` to a closure-typed local, it falls through to `MethodCall{Receiver: callee, Method: "call", Args: args, UseParens: true}`, rendering as `double_it.call(21)`. The render-shape assertion in `closure_simple` (`double_it.call(21)`) locks the Proc-invocation form.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerFunction` prepends `__env` param when `fn.IsLifted`; `FunLit` arm dispatches to `lowerFunLit`; `FunCallExpr` arm dispatches to `lowerFunCallExpr`; `ClosureEnvStmt` arm builds the env hash via `lowerClosureEnvStmt` |
| `transpiler3/ruby/build/phase07_test.go` | `TestPhase7Closures` with 6 subtests and 3 render-shape assertions |

## Test set

- `TestPhase7Closures/closure_simple`, `closure_two_arg`, `closure_in_function`, `capture_int`, `capture_multi`, `capture_string`.

## Closeout notes

Phase 7 landed on CRuby 3.4 with all six subtests green. The lift-then-wrap-in-lambda strategy was chosen over emitting an inline `lambda { |x| ... }` because lifting keeps the closure body in the same shape as a regular user function, which lets the existing `lowerFunction` path handle it without a separate inline-body lowerer; the lambda is just a thin Proc-shaped adapter. Key implementation insight: Ruby's `->(args) { body }` syntax (the stabby lambda) produces a strict-arity Proc; using `lambda { |args| body }` would have been equivalent here, but the stabby form makes the rendered code one character shorter per closure and visually separates these from `Proc.new` blocks elsewhere in the runtime. The `__env[:field]` reads happen inside the lifted method body, which is generated by the same `VarRef` lowering used everywhere else once the C front-end has rewritten captured names into env-field accesses.
