---
title: "Phase 5. Sum types"
sidebar_position: 10
sidebar_label: "Phase 5. Sum types"
description: "MEP-51 Phase 5, Mochi sum types lowered to PEP 695 type aliases over frozen-slots dataclass variants with PEP 634 exhaustive match enforced by mypy strict and pyright strict (no case _ fallback)."
---

# Phase 5. Sum types

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 5](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase5Sums`: 40 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

A critical sub-gate is that `mypy --strict` and `pyright --strict` both flag a missing match arm as an error on every fixture variation, without the lowerer emitting a `case _:` catch-all. This is the only test that `MochiResult`, Option, and user sum types reliably exhaust at the type-checker layer.

Fixtures cover: basic sum types, generic sum types with PEP 695 type parameters (`type Option[T] = Some[T] | None_`), nested sum types, and recursive sum types (Tree node).

## Goal-alignment audit

Sum types are Mochi's primary algebraic abstraction. `Option<T>`, `Result<T, E>`, and every user-defined ADT lower through this pipeline. Phase 5 enforces exhaustiveness at the type-checker layer rather than at runtime: no `case _:` is emitted, so adding a new variant to an ADT forces every existing match site to update under `mypy --strict` and `pyright --strict`. This is stronger than runtime `_ => throw` because the failure surfaces at build time, never at runtime, matching vm3's compile-time exhaustiveness check.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | Basic sum type lowering: PEP 695 `type T = A | B` over frozen-slots dataclass variants | NOT STARTED | — |
| 5.1 | PEP 634 `match` plus exhaustiveness enforced by mypy and pyright (no `case _:` catch-all emitted) | NOT STARTED | — |
| 5.2 | Generic sum types: `type Option[T] = Some[T] | None_` with PEP 695 type parameter on the variant | NOT STARTED | — |
| 5.3 | Nested and recursive sum types: Tree node, JSON-shaped variants | NOT STARTED | — |

## Sub-phase 5.0, Basic sum type emission

### Goal-alignment audit (5.0)

A working basic sum type is the foundation; every later sub-phase adds richness on top. Without correct emission of the PEP 695 type alias and the per-variant dataclasses in the right order, `mypy --strict` rejects the file outright.

### Decisions made (5.0)

**Emitted source for `type Shape = Circle{ r: float } | Rect{ w: float, h: float }`**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Circle:
    r: float


@dataclass(frozen=True, slots=True)
class Rect:
    w: float
    h: float


type Shape = Circle | Rect
```

**PEP 695 `type` statement**: the canonical 3.12+ form. The `type` alias is lazily evaluated, so the variants can reference each other in nested sum types without forward-reference quoting. Both mypy 1.13+ and pyright 1.1.380+ accept PEP 695 type aliases under `--strict`.

**Variant dataclass shape**: every variant is `@dataclass(frozen=True, slots=True)`. Nullary variants (no fields) declare an empty body:

```python
@dataclass(frozen=True, slots=True)
class None_:
    pass
```

The trailing underscore on `None_` avoids collision with Python's `None` singleton (per MEP-51 §3 reserved-word mangling). The same rule applies to `True_`, `False_`, `Type_`, `Class_`.

**Variant declaration order**: variants are emitted in declaration order (matching the Mochi source). The `type T = A | B` alias follows the variants. Python's lazy alias evaluation means the order does not affect runtime semantics, but it keeps the emitted source readable.

**Equality across variants**: `Circle(r=1.0) == Rect(w=1.0, h=1.0)` is `False` because they are different dataclass classes. `__eq__` checks `type(self) == type(other)` first.

## Sub-phase 5.1, Exhaustive match without case _

### Goal-alignment audit (5.1)

The exhaustiveness gate is the most important property of Mochi sum types. If a `case _:` catch-all is emitted, adding a new variant silently misses every match site and only surfaces at runtime. By omitting `case _:`, the type checkers enforce exhaustiveness at build time, matching vm3 semantics.

### Decisions made (5.1)

**Emitted source for `match s { Circle{r} => 3.14 * r * r, Rect{w, h} => w * h }`**:

```python
from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Circle:
    r: float


@dataclass(frozen=True, slots=True)
class Rect:
    w: float
    h: float


type Shape = Circle | Rect


def area(s: Shape) -> float:
    match s:
        case Circle(r=r):
            return math.pi * r * r
        case Rect(w=w, h=h):
            return w * h
```

**Why no `case _:` catch-all**: under `mypy --strict` and `pyright --strict`, a `match` over a sealed union without a catch-all arm produces a "non-exhaustive match" error if any variant is missing. The presence of `case _:` would mask that error. By omitting the catch-all, the type checkers serve as the exhaustiveness gate. This decision is recorded in MEP-51 §7.

**Why keyword pattern (`Circle(r=r)`) not positional (`Circle(r)`)**: dataclass auto-generates `__match_args__`, so positional matching works. But keyword matching is robust to field reordering, more readable, and lets both checkers infer the bound variable type more reliably under `--strict`. The lowerer always emits keyword patterns.

**Guard clauses**: Mochi `match s { Circle{r} if r > 0 => ... }` lowers to `case Circle(r=r) if r > 0: ...`. Both checkers accept PEP 634 `if` guards.

**Type narrowing inside the arm**: inside `case Circle(r=r):`, the bound `r` is typed as `float`. Both mypy and pyright narrow correctly.

**Return-value match**: when `match` is used as an expression position (Mochi blocks return their last expression), the lowerer wraps in an inner helper `def __match_n(s: Shape) -> R:` and invokes it. PEP 634 `match` is statement-only; the helper is required.

## Sub-phase 5.2, Generic sum types

### Goal-alignment audit (5.2)

`Option<T>` and `Result<T, E>` are the canonical generic sum types and are used pervasively (every error-returning function, every nullable field). PEP 695 makes generic sum types one-liners; without it, the emit would be considerably noisier.

### Decisions made (5.2)

**Emitted source for `type Option<T> = Some<T> | None`** (the Mochi `None` variant lowers to `None_` per reserved-word mangling):

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Some[T]:
    value: T


@dataclass(frozen=True, slots=True)
class None_:
    pass


type Option[T] = Some[T] | None_
```

**PEP 695 type parameter on variant dataclass**: `class Some[T]:` (PEP 695 syntax) replaces `Generic[T]` and `TypeVar("T")`. Both mypy and pyright accept the PEP 695 form under `--strict`. Mochi-emitted code never uses `typing.TypeVar` or `typing.Generic`.

**PEP 695 type parameter on the alias**: `type Option[T] = Some[T] | None_` parameterises the union over `T`. Consumers write `Option[int]`, `Option[str]`, etc.

**`MochiResult[T, E]`** in `mochi_runtime.result`:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Ok[T]:
    value: T


@dataclass(frozen=True, slots=True)
class Err[E]:
    error: E


type MochiResult[T, E] = Ok[T] | Err[E]
```

This is the only runtime-supplied sum type. User-defined sum types are emitted into the user's package, never into `mochi_runtime`.

**Generic variant matching**:

```python
def unwrap_or[T](opt: Option[T], default: T) -> T:
    match opt:
        case Some(value=v):
            return v
        case None_():
            return default
```

Both checkers infer `v: T` inside the `Some` arm. The function itself uses PEP 695 type parameter syntax (`def unwrap_or[T]`).

## Sub-phase 5.3, Nested and recursive sum types

### Goal-alignment audit (5.3)

Recursive sum types (Tree, JSON, AST nodes) are the prototypical use case. Without recursive support, half the fixture corpus would defer to Phase 12 or later. PEP 695 plus `from __future__ import annotations` makes recursive aliases straightforward.

### Decisions made (5.3)

**Emitted source for `type Tree<T> = Leaf<T> | Branch<T>` where `Branch{ left: Tree<T>, right: Tree<T> }`**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Leaf[T]:
    value: T


@dataclass(frozen=True, slots=True)
class Branch[T]:
    left: Tree[T]
    right: Tree[T]


type Tree[T] = Leaf[T] | Branch[T]
```

**Forward references inside variant field annotations**: under `from __future__ import annotations`, the field annotation `Tree[T]` is a string at class-construction time and is resolved lazily by the type checker. Both mypy and pyright accept this. Without the future import, the emitter would need explicit string quoting (`"Tree[T]"`); the future import is mandatory in every emitted module precisely to avoid this.

**Nested sum types**: `type JSONValue = JNull | JBool{ b: bool } | JNumber{ n: float } | JString{ s: str } | JArray{ items: list<JSONValue> } | JObject{ fields: map<str, JSONValue> }` lowers cleanly:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class JNull:
    pass


@dataclass(frozen=True, slots=True)
class JBool:
    b: bool


@dataclass(frozen=True, slots=True)
class JNumber:
    n: float


@dataclass(frozen=True, slots=True)
class JString:
    s: str


@dataclass(frozen=True, slots=True)
class JArray:
    items: list[JSONValue]


@dataclass(frozen=True, slots=True)
class JObject:
    fields: dict[str, JSONValue]


type JSONValue = JNull | JBool | JNumber | JString | JArray | JObject
```

The `JArray.items` and `JObject.fields` annotations refer to `JSONValue` before it is declared; the future import makes this legal. Both checkers resolve the recursive alias under `--strict`.

**Tree traversal example**:

```python
def sum_tree(t: Tree[int]) -> int:
    match t:
        case Leaf(value=v):
            return v
        case Branch(left=l, right=r):
            return sum_tree(l) + sum_tree(r)
```

Recursion through the sum alias is type-checker friendly. The match remains exhaustive.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | `UnionDecl` to PEP 695 `type` alias + per-variant frozen-slots dataclass; `MatchStmt` to PEP 634 `match` with keyword patterns; no `case _:` catch-all |
| `transpiler3/python/lower/match.go` | Match-expression-position wrapping: emit inner `def __match_n(...) -> R:` helper so PEP 634 statement-only match can be used in expression position |
| `runtime/python/mochi_runtime/result.py` | `Ok[T]`, `Err[E]`, `type MochiResult[T, E] = Ok[T] | Err[E]` |
| `runtime/python/mochi_runtime/option.py` | `Some[T]`, `None_`, `type Option[T] = Some[T] | None_` (for explicit `Option[T]` users; the language can also use `T | None` per MEP-51 §4) |
| `transpiler3/python/build/phase05_test.go` | `TestPhase5Sums`: 40 fixtures |
| `tests/transpiler3/python/fixtures/phase05-sums/` | 40 fixture directories covering nullary variants, single-field variants, multi-field variants, generic Option, generic Result, nested unions, recursive Tree, JSON-shaped sum types, match in expression position, match with guards, exhaustiveness negative tests (one-variant-missing fixtures recorded as expected mypy/pyright errors), match return type |

## Test set

- `TestPhase5Sums`, walks all 40 fixtures with the standard gate stack.
- A separate `TestPhase5Exhaustiveness` runs the missing-variant fixtures and asserts that `mypy --strict` and `pyright --strict` both produce a non-exhaustive-match diagnostic (no Python execution; the gate is the diagnostic itself).

## Deferred work

- `match` over Python primitive types (int, str literal patterns), deferred to Phase 6 (closures introduce the surface where such matches arise).
- `MochiResult` adapter for boundary FFI (auto-wrap raised exception into `Err`), deferred to Phase 11 (async coloring + MochiResult).
- Visitor-pattern code generation for huge sum types, deferred indefinitely (PEP 634 match is the canonical visitor; no extra surface needed).
- `dataclass(slots=True)` plus `Generic[T]` interaction (Python 3.12 fixed an earlier bug here), no further action needed; PEP 695 sidesteps the legacy form.
