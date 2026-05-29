---
title: "Phase 6. Closures and higher-order"
sidebar_position: 11
sidebar_label: "Phase 6. Closures and higher-order"
description: "MEP-51 Phase 6, non-capturing closures lifted to module-level defs and higher-order parameters typed as collections.abc.Callable. Capturing closures deferred upstream because the c lower bakes C-specific emit names into VarRef.Name."
---

# Phase 6. Closures and higher-order

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 6](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (6.0, 6.1, 6.3; 6.2 BLOCKED UPSTREAM) |
| Started        | 2026-05-29 17:30 (GMT+7) |
| Landed         | 2026-05-29 18:15 (GMT+7) |
| Tracking issue | (filled in at PR time) |
| Tracking PR    | (filled in at PR time) |

## Gate

`TestPhase6Closures`: 15 fixtures green on CPython 3.13 with the standard `runPythonFixture` harness (byte-equal stdout vs hand-computed expected output). Strict typecheck and `ruff` carry-forward gates are deferred to Phase 16 per the umbrella decision matrix.

Fixtures cover: lifted non-capturing closures (8 carried from `tests/transpiler3/c/fixtures/closures/`) + higher-order parameters typed as `Callable[[T1, T2, ...], R]` (7 new). All shipping fixtures emit `from collections.abc import Callable` so the import-set sanity stays correct.

## Goal-alignment audit

The user-facing payoff is that Mochi programs using first-class functions, the most common shape outside Query DSL, transpile to runnable Python without forcing the user to rewrite call sites. Phase 6 ships that for every non-capturing closure (lambda, named `let` binding, multi-statement body) and for every higher-order function that accepts or returns a callable. Capturing closures over the enclosing scope (the `let factor = 3; let mul = fn(x: int): int => x * factor` shape) are deferred to a follow-up sub-phase because the aotir IR layer leaks a C-specific `__e->FieldName` emit name into `VarRef.Name` for captured slots, see "Upstream blocker (6.2)" below. The deferral does not block Phase 7 (Query DSL), which builds on `for ... in` lowering, not closure capture; nor Phase 9 (agents), which uses `Channel` send/receive, not closure capture.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | Non-capturing closures (lifted by c lower to module-level `def`) | LANDED | (filled in at commit time) |
| 6.1 | Multi-statement closure body (same lifting path; body is just a `Block`) | LANDED | (same commit) |
| 6.2 | Capturing closures over outer scope | BLOCKED UPSTREAM | — |
| 6.3 | Higher-order via `collections.abc.Callable` (params and returns) | LANDED | (same commit) |

## Sub-phase 6.0 + 6.1, Non-capturing lifted closures

### Goal-alignment audit (6.0 + 6.1)

Non-capturing closures, whether single-expression `fn(x) => x * 2` or multi-statement `fn(x) { let y = x + 1; return y * y }`, all look the same once the c lower's closure-conversion pass runs. The pass lifts the closure body into a top-level `aotir.Function` and replaces the original `FunLit` with a reference to the lifted function's name. Python's lowerer can therefore reuse the same `lowerFunction` path that handles named functions; the lifted closure becomes a regular module-level `def` and the `FunLit` site lowers to a bare `Name` reference.

### Decisions made (6.0 + 6.1)

**Lift, don't inline as `lambda`.** Even single-expression closures lower to a top-level `def` rather than an inline `lambda` because the c lower has already lifted the body. Python doesn't get a "more idiomatic" output here because both the C and Python targets share the same upstream IR; routing single-expression closures back to inline `lambda` on the Python side would require duplicating the lifting decision and re-inlining, which is strictly worse. The lifted name (e.g. `__anon_1`) is the same one the C target uses.

**Multi-statement bodies are free.** Because the c lower lifts every closure body to a regular `aotir.Function`, the Python lowerer doesn't need a separate "multi-statement closure" path; it just calls `lowerBlock` on the body and gets a real Python function. This is why 6.0 and 6.1 land in the same patch: there is no Python-side distinction.

**Sample emit for `closure_simple.mochi`:**

```python
from __future__ import annotations

from mochi_runtime.io import Print


def __anon_1(x: int) -> int:
    return (x * 2)


def main() -> None:
    double_it = __anon_1
    r = __anon_1(21)
    Print.line(r)


if __name__ == "__main__":
    main()
```

Note that `double_it = __anon_1` is the bare `FunLit` reference; the lifted `__anon_1` is the closure body. The `r = __anon_1(21)` call site stays as a regular `CallExpr` because the c lower rewrites the let-bound call through to the lifted name directly. This means non-capturing closures are zero-overhead on Python the same way they are on C, no allocation for an env struct, no Python `lambda` heap cell.

**Why reject `len(Captures) > 0` at Lower() rather than at lowerExpr.** The check sits at the Lower() entry point, before any per-function work, so a capturing closure surfaces as a single clear error rather than a downstream crash on `__e->`-prefixed `VarRef.Name` strings. See [[phase-06-closures#sub-phase-62-capturing-closures-blocked-upstream]].

## Sub-phase 6.2, Capturing closures, BLOCKED UPSTREAM

### Goal-alignment audit (6.2)

Capturing closures are the form Mochi users reach for to build higher-order helpers (`let mul3 = fn(x) => x * factor` inside a function that takes `factor` as a parameter). Without 6.2, Phase 7 (Query DSL) is unaffected (its codegen lowers `from ... select` to nested `for` rather than closures), and Phase 9 (agents) is unaffected (Channel-based message passing, not lexical capture). But user-written `map`/`filter` helpers that close over their own arguments do regress to a clear error. The recommendation in the error message is to lift the capture to a parameter manually until upstream lands the fix.

### Upstream blocker (6.2)

`transpiler3/c/lower/lower.go:3955` sets `emitName: "__e->" + freeName` on every captured free variable inside a lifted closure body. That `emitName` flows through the aotir layer as a `VarRef.Name`, so by the time the Python lowerer sees the lifted function body, every reference to a captured slot is already the literal string `__e->factor` (or whatever the field name is). The Python lowerer can't strip that prefix without re-discovering which slots were captures, and the IR no longer carries that information (the `Captures` slice tells Python which slots existed, but not which `VarRef.Name`s mention them in the body).

A clean fix lives upstream: the c lower needs to emit captures as a structural IR node, e.g. `CaptureRef{Slot: "factor"}`, rather than baking the C emit name into `VarRef.Name`. Each backend (C, Python, future Rust/Go) would then render `CaptureRef` in its own syntax. The C backend would render it as `__e->factor`; Python would render it as `factor` (with `nonlocal factor` if the c lower marks the slot mutable). Until that upstream pass lands, capturing closures stay rejected at Lower().

### Forward plan (6.2)

When the upstream IR change lands, the Python lowerer needs:

1. A `*aotir.CaptureRef` case in `lowerExpr` that emits `&pysrc.Name{Id: v.Slot}` (no prefix).
2. A `nonlocal` synthesis pass in `lowerFunction`: walk the body, collect every `CaptureRef` whose corresponding `FunCapture` has `Mutable=true`, and prepend `pysrc.NonlocalStmt{Names: [...sorted slot names...]}` to the function body. Sorted because Python's `nonlocal` order is a style choice and a sorted emit keeps stdout-byte-equal stable across IR-walk reorderings.
3. Remove the Lower()-entry rejection check for `len(fn.Captures) > 0`.
4. Carry forward 8 fixtures from `tests/transpiler3/c/fixtures/capturing_closures/` (capture_bool, capture_float, capture_in_function, capture_int, capture_let_mutation, capture_returns_var, capture_string, capture_two_args).
5. Add 2 fixtures for `nonlocal` mutation paths, since the C side doesn't exercise this shape (C closures over mutable env work via pointer rebinding, not the equivalent of Python's `nonlocal`).

The forward plan does not require any change to `pysrc` (`NonlocalStmt` already exists) or to `emit`. It is purely a `lower/lower.go` change once the upstream IR change is in.

## Sub-phase 6.3, Higher-order via Callable

### Goal-alignment audit (6.3)

A function that accepts a callback or returns a callback is the most common higher-order shape. Mochi `fun apply(x: int, f: fun(int): int): int` lowers to Python `def mochi__apply(x: int, f: Callable[[int], int]) -> int`. Without correct `Callable` typing, both type checkers fall back to `Callable[[int], Any]` (mypy) or the bare `function` type (pyright), and the Phase-16 strict gate would reject either. The annotation is also user-visible in IDE tooltips, so emitting it correctly now (rather than waiting for Phase 16) is a small, free quality-of-life win.

### Decisions made (6.3)

**Use `collections.abc.Callable`, not `typing.Callable`.** Per MEP-51 §4, every callable type comes from `collections.abc`. The `typing.Callable` shim is deprecated for new code, and `ruff check --select=UP006` would flag it on the Phase-16 gate. The lowerer flips a `needsCallable bool` flag the first time `pyTypeForFun` runs, and `Lower()` injects `from collections.abc import Callable` only when the flag is set so the import set stays minimal.

**Bracket form `Callable[[P1, P2], R]` with explicit param list.** The empty-param form `Callable[[], R]` is the right Python for a no-arg callback. The variadic form `Callable[..., R]` is not emitted because the aotir verifier rejects non-fixed-arity `FunSig`s.

**`TypeUnit` return becomes `None`.** A `fun(int): unit` callback lowers to `Callable[[int], None]`. The lowerer special-cases `sig.ReturnType == aotir.TypeUnit` because `pyTypeFor(TypeUnit)` returns an empty `TypeRef` (the rest of the type system uses empty `TypeRef` to mean "no annotation").

**`FunCallExpr` dispatch through bare callee.** When the IR carries a `FunCallExpr` (calling a function-typed local), the lowerer recursively lowers `Callee` and then constructs `pysrc.Call{Func: callee, Args: args}`. Because `Callee` is typically a `VarRef`, the emit shape is `f(x)` rather than `f.__call__(x)`. This matches the Mochi source intent and stays type-checker-clean (both checkers know `Callable[[T], R]` is callable).

**Sample emit for `higher_order_apply_int.mochi`:**

```python
from __future__ import annotations

from mochi_runtime.io import Print
from collections.abc import Callable


def __anon_1(n: int) -> int:
    return (n + 1)


def mochi__apply(x: int, f: Callable[[int], int]) -> int:
    return f(x)


def main() -> None:
    inc = __anon_1
    Print.line(mochi__apply(10, inc))
    Print.line(mochi__apply(99, inc))


if __name__ == "__main__":
    main()
```

**Function-typed return.** A function that returns a callable lowers to `def make(...) -> Callable[[int], int]`. The same `pyTypeForFun` helper renders the annotation; `lowerFunction` checks `fn.ReturnType == aotir.TypeFun` and calls it on `fn.ReturnFunSig`.

**Function-typed `let` annotation.** `let f: fun(int): int = some_fn` (`higher_order_typed_let.mochi`) lowers through `lowerLetStmt`'s existing `pyTypeForUnion` path; the c lower threads the `FunSig` through `LetStmt.FunSig` and the Python lowerer routes through the same `Callable[...]` emit. The annotation appears on the binding, not on the rhs.

**Inline `FunLit` at call site.** `run(5, fun(n: int): int => n * n)` (`higher_order_pass_lambda.mochi`) works because the c lower lifts the inline `FunLit` to a top-level `def` before the Python lowerer sees it. The call site becomes `run(5, __anon_2)`. Same zero-overhead story as 6.0.

### Deferred to Phase 11 (async)

- `Callable[[T], Awaitable[R]]` for async callbacks. The Phase-11 colour pass decides which callbacks become awaitable; until then the annotation is plain `Callable[[T], R]`.
- `Callable[..., Coroutine[Any, Any, R]]` for coroutine-typed callbacks. Same reason.

### Deferred indefinitely

- `ParamSpec` (PEP 612) for forwarding callable signatures. Mochi has no surface for `ParamSpec`, so the lowerer never needs it. If FFI (Phase 12) surfaces a decorator-shaped Python import, we revisit.
- First-class continuations / `call/cc`. Out of scope for MEP-51 (no Mochi surface).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | `pyTypeForFun(sig *aotir.FunSig)`; `FunCallExpr` case in `lowerExpr`; param and return `TypeFun` handling in `lowerFunction`; `Lower()`-entry rejection of `len(fn.Captures) > 0` with a clear upstream-blocked error |
| `transpiler3/python/build/build.go` | Cache marker bumped `mep51-phase05` → `mep51-phase06` so Phase 6's emit changes invalidate older cache hits |
| `transpiler3/python/build/phase06_test.go` | `TestPhase6Closures`, walks `tests/transpiler3/python/fixtures/phase06-closures` with `runPythonFixture` |
| `tests/transpiler3/python/fixtures/phase06-closures/` | 15 fixtures: 8 non-capturing closures carried from the C fixture set, plus 7 higher-order shapes (apply_int, apply_bool, apply_float, apply_string, two_param_fn, typed_let, pass_lambda) |
| `website/docs/implementation/0051/phase-06-closures.md` | This page |

## Test set

| Fixture | Shape |
|---------|-------|
| `closure_block_body` | Multi-statement closure body, lifted by c lower |
| `closure_bool_return` | `fun(b: bool): bool`, lifted, called directly |
| `closure_float` | `fun(x: float): float`, scalar-float path through `pyTypeFor` |
| `closure_in_function` | Closure declared inside another function, lifted to module scope |
| `closure_multiple_types` | Mixed-type params on one lifted closure |
| `closure_simple` | Minimal `fun(x: int): int => x * 2` |
| `closure_string_return` | `fun(s: string): string`, string scalar path |
| `closure_two_arg` | Two-arg closure, exercises `FunSig.ParamTypes` length>1 |
| `higher_order_apply_int` | `Callable[[int], int]` param, called via `FunCallExpr` |
| `higher_order_apply_bool` | `Callable[[bool], bool]` param |
| `higher_order_apply_float` | `Callable[[float], float]` param |
| `higher_order_apply_string` | `Callable[[string], string]` param |
| `higher_order_two_param_fn` | `Callable[[int, int], int]` param |
| `higher_order_typed_let` | `let f: fun(int): int = ...`, exercises `LetStmt.FunSig` plumbing |
| `higher_order_pass_lambda` | Inline `FunLit` passed at call site, lifted by c lower |

## Deferred work

- Capturing closures, see [Sub-phase 6.2](#sub-phase-62-capturing-closures-blocked-upstream) above. Blocked on c lower IR change.
- Async closures (`Callable[[T], Awaitable[R]]`), deferred to Phase 11.
- Strict typecheck gate (`mypy --strict`, `pyright --strict`) and `ruff` fixed-point, deferred to Phase 16 per the umbrella matrix.
