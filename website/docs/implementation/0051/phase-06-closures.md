---
title: "Phase 6. Closures and higher-order"
sidebar_position: 11
sidebar_label: "Phase 6. Closures and higher-order"
description: "MEP-51 Phase 6, Mochi closures lowered to lambda (single-expression) or nested def (multi-statement) with nonlocal capture mutation and higher-order via collections.abc.Callable."
---

# Phase 6. Closures and higher-order

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 6](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase6Closures`: 30 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: lambda single-expression closures, nested `def` multi-statement closures, capture via Python's closure semantic, `nonlocal` for capture mutation, higher-order parameters typed as `Callable[[T], R]` from `collections.abc`.

## Goal-alignment audit

Closures and higher-order functions are the Mochi functional core; without them, `map`, `filter`, `sorted(key=...)`, and the Query DSL's `select` projection cannot compose. Lambda lowers cleanly when the body fits a single expression; multi-statement bodies need a nested `def` (Python lambdas are expression-only). The capture distinction (read-only vs read-write via `nonlocal`) is settled here so Phase 7 (Query DSL) and Phase 9 (agents) inherit a working capture model.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | Lambda for single-expression closures (`lambda x: x * 2`) | NOT STARTED | — |
| 6.1 | Nested `def` for multi-statement closures (Python lambdas are expression-only) | NOT STARTED | — |
| 6.2 | Captures and `nonlocal` for capture mutation | NOT STARTED | — |
| 6.3 | Higher-order parameters typed as `Callable[[T], R]` from `collections.abc` | NOT STARTED | — |

## Sub-phase 6.0, Lambda

### Goal-alignment audit (6.0)

The simplest closure shape (single expression, no captures or read-only captures) is the most common in Mochi programs (almost every `map` call, every `sorted(key=...)`). Lambda handles this with zero ceremony.

### Decisions made (6.0)

**Emitted source for `let double = fn(x: int) -> int => x * 2`**:

```python
from __future__ import annotations

from collections.abc import Callable


def main() -> None:
    double: Callable[[int], int] = lambda x: x * 2
    result: int = double(21)
```

**Why annotate the binding as `Callable[[int], int]`**: lambdas have no inline parameter or return type annotation in Python. Without the binding-level annotation, both type checkers fall back to `Callable[[int], Any]` (mypy) or the bare lambda type (pyright), and the strict gate rejects either. The binding-level annotation gives both checkers a concrete callable type.

**`Callable` from `collections.abc`**: per MEP-51 §4, `collections.abc.Callable` is used instead of `typing.Callable`. The former is PEP 585 stdlib (since 3.9, fully supported in 3.12); the latter is deprecated for new code. `ruff check` with `UP006` would flag `typing.Callable` if emitted.

**No-parameter lambda**: `fn() -> int => 42` lowers to `lambda: 42` with annotation `Callable[[], int]`. Both checkers accept the empty parameter list.

**Multi-parameter lambda**: `fn(x: int, y: int) -> int => x + y` lowers to `lambda x, y: x + y` with annotation `Callable[[int, int], int]`.

## Sub-phase 6.1, Nested def

### Goal-alignment audit (6.1)

Python lambdas accept only one expression body. Mochi closures with multiple statements (early returns, mutation, logging) lower to nested `def`. The decision rule is purely syntactic at the IR level: if the body fits one expression, use lambda; otherwise nested `def`.

### Decisions made (6.1)

**Emitted source for a multi-statement closure**:

```python
from __future__ import annotations

from collections.abc import Callable


def main() -> None:
    def normalize(s: str) -> str:
        trimmed: str = s.strip()
        if trimmed == "":
            return "<empty>"
        return trimmed.lower()

    name: str = normalize("  Ana  ")
```

**Nested `def` keeps annotations**: unlike lambda, nested `def` carries full parameter and return-type annotations. Both checkers infer the resulting `Callable[[str], str]` directly from the `def` signature.

**Why not always nested `def`**: lambdas are more readable for single-expression bodies (the Mochi-side intent is a one-liner; the Python output should mirror that). The lowerer keeps lambda for the simple case and falls through to `def` only when the IR carries multiple statements.

**Decision rule at the lowerer**: in `transpiler3/python/lower/closure.go`, `LowerClosure(c *aotir.Closure)` inspects `len(c.Body.Stmts)`. If exactly one statement and it is a `ReturnStmt` with a simple expression (no nested `if`, no `match`), emit `lambda`. Otherwise emit nested `def`.

**Nested `def` inside a list comprehension**: rare but legal in Mochi (a `select` clause with a multi-statement body). The lowerer extracts the closure to a named nested `def` above the comprehension and references it by name inside the comprehension, rather than inlining a lambda.

## Sub-phase 6.2, Captures and nonlocal

### Goal-alignment audit (6.2)

Mochi closures over `let` bindings (read-only) are straightforward (Python closures over outer scope). Closures over `var` bindings that mutate the captured variable need `nonlocal`. Without `nonlocal`, the inner assignment shadows the outer binding (creates a local) and the mutation is invisible to the caller.

### Decisions made (6.2)

**Read-only capture**:

```python
from __future__ import annotations

from collections.abc import Callable


def main() -> None:
    factor: int = 3
    multiplier: Callable[[int], int] = lambda x: x * factor
    result: int = multiplier(10)
```

No `nonlocal` needed; Python closures see `factor` by reference. Both checkers infer the lambda's `int` parameter and return type.

**Read-write capture via `nonlocal`**:

```python
from __future__ import annotations

from collections.abc import Callable


def make_counter() -> Callable[[], int]:
    count: int = 0

    def step() -> int:
        nonlocal count
        count += 1
        return count

    return step
```

**Lowerer rule**: when the Mochi IR marks a captured `var` as mutated inside a closure body, the lowerer prepends `nonlocal <names>` to the nested `def`. Lambdas cannot use `nonlocal` (single-expression bodies cannot assign); the IR pass forces these into nested `def` automatically.

**`nonlocal` vs `global`**: Mochi has module-level `var` bindings that closures can mutate; the lowerer emits `global` instead of `nonlocal` for these. Module-level `let` bindings are immutable by Mochi semantic; the lowerer never emits `global` for those.

**Captured variable type stability**: Python's closure cell holds the binding, not a snapshot. If the outer scope rebinds the captured variable, the closure sees the new value. Mochi semantics agree.

**`for`-loop variable capture footgun**: a classic Python pitfall is `[lambda: i for i in range(3)]` returning `[2, 2, 2]` instead of `[0, 1, 2]`. Mochi closures inside comprehensions are rare, but the IR's closure-conversion pass binds the loop variable at capture time (via default-argument trick: `lambda i=i: i`) when needed. See [[05-codegen-design]] §12 for the late-binding workaround.

## Sub-phase 6.3, Higher-order via Callable

### Goal-alignment audit (6.3)

A function that accepts a callback is the canonical higher-order shape. Mochi `fun map<T, R>(xs: list<T>, f: fn(T) -> R) -> list<R>` lowers to a Python function with a `Callable` parameter. Without correct `Callable` typing, every higher-order call site fails `mypy --strict`.

### Decisions made (6.3)

**Emitted source**:

```python
from __future__ import annotations

from collections.abc import Callable


def apply_twice[T](x: T, f: Callable[[T], T]) -> T:
    return f(f(x))


def main() -> None:
    result: int = apply_twice(3, lambda n: n * 2)
```

**PEP 695 type parameter on the higher-order function**: `def apply_twice[T]` keeps the generic shape. The `Callable[[T], T]` parameter is correctly bound to the function's `T`.

**`Awaitable` for async callbacks**: a function that accepts an async callback lowers to `Callable[[T], Awaitable[R]]` (from `collections.abc`). Phase 11 (async coloring) makes the colour pass decide which form to emit based on whether the callback is invoked via `await`.

**Variadic callable**: Mochi has no first-class variadic callback in v1; `Callable[..., R]` is not emitted. Future v2 might add this.

**`functools.partial`** is not emitted. The Mochi IR's monomorphisation and closure-conversion passes resolve partial application at compile time into a fully-applied function, so `partial` never surfaces in the output.

**Method references** (`obj.method` as a callable): Mochi `obj.method` used as a value lowers to `obj.method` (Python bound method object). Both checkers infer the correct `Callable` type from the method signature.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Closure lowering dispatch (lambda vs nested def); higher-order parameter typing |
| `transpiler3/python/lower/closure.go` | `LowerClosure`: single-expression to lambda, multi-statement to nested def; `nonlocal` insertion for mutated captures; loop-variable late-binding workaround |
| `transpiler3/python/build/phase06_test.go` | `TestPhase6Closures`: 30 fixtures |
| `tests/transpiler3/python/fixtures/phase06-closures/` | 30 fixture directories: lam_id, lam_double, lam_two_params, lam_no_params, lam_in_let, lam_in_call, def_multi_stmt, def_early_return, def_with_if, def_with_let, capture_readonly_int, capture_readonly_str, capture_readonly_list, capture_mut_counter, capture_mut_via_nonlocal, capture_mut_recursive, capture_global_var, ho_simple, ho_callable_annot, ho_apply_twice, ho_callable_no_args, ho_callable_multi_args, ho_pep695_generic, ho_method_ref, ho_predicate_filter, ho_proj_map, ho_compose, ho_curry_via_inner_def, ho_id, ho_nested_closure |

## Test set

- `TestPhase6Closures`, walks all 30 fixtures with the standard gate stack.

## Deferred work

- Async closures (closures invoking `await`), deferred to Phase 11 (async coloring) where the colour pass decides between `Callable[[T], Awaitable[R]]` and `Callable[[T], Coroutine[Any, Any, R]]`.
- `functools.partial` emission for partial application, deferred indefinitely (the IR resolves partial application via monomorphisation).
- `ParamSpec` (PEP 612) for forwarding callable signatures, deferred to Phase 12 (FFI), where decorator-shaped Mochi exports need it.
- First-class continuations / call/cc, out of scope for MEP-51 (no Mochi surface).
