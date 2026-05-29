---
title: "Phase 3.3. Sets"
sidebar_position: 7
sidebar_label: "Phase 3.3. Sets"
description: "MEP-51 Phase 3.3, Mochi set lowered to mochi_runtime.collections.OrderedSet wrapping dict[T, None] to preserve insertion order semantics not offered by Python's builtin set."
---

# Phase 3.3. Sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 3.3](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase33Sets`: 15 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: `OrderedSet` construction, add / has / len, set operators (`|`, `&`, `-`), iteration in insertion order, set comprehensions.

## Goal-alignment audit

Python's builtin `set` does not preserve insertion order. Mochi `set<T>` does (per the Mochi reference, set iteration follows insertion order). If the lowerer mapped Mochi set to Python `set`, every set fixture would diverge from vm3 on iteration-order-sensitive output. Phase 3.3 ships `mochi_runtime.collections.OrderedSet`, a thin wrapper around `dict[T, None]`, which inherits Python dict's insertion-order guarantee and gives vm3-byte-equal iteration.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.3.0 | `OrderedSet` wrapper in `mochi_runtime.collections` over `dict[T, None]`, with PEP 695 generic `OrderedSet[T]` | NOT STARTED | — |
| 3.3.1 | `add`, `has` (`__contains__`), `len`, iteration in insertion order | NOT STARTED | — |
| 3.3.2 | Set operators: union `\|`, intersection `&`, difference `-` | NOT STARTED | — |

## Sub-phase 3.3.0, OrderedSet runtime wrapper

### Goal-alignment audit (3.3.0)

The wrapper is the foundation; all subsequent operators are methods on it. Without the wrapper, Phase 3.3.1 and 3.3.2 have nowhere to dispatch.

### Decisions made (3.3.0)

**`mochi_runtime.collections.OrderedSet`** at `runtime/python/mochi_runtime/collections.py`:

```python
from __future__ import annotations

from collections.abc import Iterable, Iterator
from typing import Self


class OrderedSet[T]:
    __slots__ = ("_data",)

    _data: dict[T, None]

    def __init__(self, items: Iterable[T] = ()) -> None:
        self._data = dict.fromkeys(items)

    def add(self, item: T) -> None:
        self._data[item] = None

    def __contains__(self, item: object) -> bool:
        return item in self._data

    def __len__(self) -> int:
        return len(self._data)

    def __iter__(self) -> Iterator[T]:
        return iter(self._data)

    def __or__(self, other: OrderedSet[T]) -> Self:
        result: Self = type(self)(self._data.keys())
        for item in other:
            result.add(item)
        return result

    def __and__(self, other: OrderedSet[T]) -> Self:
        return type(self)(item for item in self if item in other)

    def __sub__(self, other: OrderedSet[T]) -> Self:
        return type(self)(item for item in self if item not in other)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, OrderedSet):
            return NotImplemented
        return list(self._data.keys()) == list(other._data.keys())

    def __hash__(self) -> int:
        return hash(tuple(self._data.keys()))
```

**PEP 695 generic class syntax** (`class OrderedSet[T]:`) is the v1 form. `typing.TypeVar` is never emitted. Both mypy 1.13+ and pyright 1.1.380+ accept PEP 695 generic classes.

**`__slots__`**: gives memory locality and prevents accidental attribute creation. Aligns with the frozen-slots dataclass discipline used for records (Phase 4).

**`Self` return type**: PEP 673 `Self` is used on `__or__`, `__and__`, `__sub__` so subclasses (none in v1 but future-friendly) propagate.

**`dict.fromkeys`**: zero-cost initialiser using the insertion-order guarantee of Python dict. Avoids manually inserting one key at a time.

**`__hash__` over tuple of keys**: makes `OrderedSet` hashable for use as a dict key or another set element. Mochi `set<set<int>>` is rare but legal.

## Sub-phase 3.3.1, add, has, len, iter

### Goal-alignment audit (3.3.1)

These are the four primitive set operations. They cover every fixture that does not need a binary operator.

### Decisions made (3.3.1)

**Emitted source for `let s = {1, 2, 3}; s.add(4); print(s has 1); print(len(s))`**:

```python
from __future__ import annotations

from mochi_runtime.collections import OrderedSet
from mochi_runtime.io import Print


def main() -> None:
    s: OrderedSet[int] = OrderedSet([1, 2, 3])
    s.add(4)
    Print.line(1 in s)
    Print.line(len(s))
```

**Set literal syntax**: Mochi `{1, 2, 3}` (set literal, distinguished from map by element shape) lowers to `OrderedSet([1, 2, 3])`. Python set literal `{1, 2, 3}` is never emitted (it produces a Python `set`, the wrong type).

**`s has x` membership**: lowers to `x in s`. The wrapper's `__contains__` makes this O(1) average.

**`len(s)`**: lowers to `len(s)`. The wrapper's `__len__` returns `int`.

**Iteration**: `for x in s { print(x) }` lowers to `for x in s:`. The wrapper's `__iter__` yields keys in insertion order.

## Sub-phase 3.3.2, Set operators

### Goal-alignment audit (3.3.2)

Union, intersection, and difference are the canonical set algebra. The wrapper implements them via `__or__`, `__and__`, `__sub__` so Mochi `s | t` lowers to `s | t` directly.

### Decisions made (3.3.2)

**Emitted source**:

```python
from __future__ import annotations

from mochi_runtime.collections import OrderedSet


def main() -> None:
    a: OrderedSet[int] = OrderedSet([1, 2, 3])
    b: OrderedSet[int] = OrderedSet([3, 4, 5])
    union: OrderedSet[int] = a | b
    intersect: OrderedSet[int] = a & b
    diff: OrderedSet[int] = a - b
```

**Ordering of results**: union preserves left-then-right insertion order (matches vm3). Intersection preserves left-order. Difference preserves left-order. All three match the natural order of `for item in self` iteration.

**No symmetric difference**: Mochi has no `^` operator on sets in the v1 surface. The wrapper does not implement `__xor__`.

**Equality**: `a == b` compares ordered keys (not just set membership). This is stronger than Python builtin `set` equality (which is order-insensitive) but matches Mochi semantics: two sets are equal if they hold the same elements in the same insertion order.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Set literal, `.add`, `has`, `len`, iteration, `\|` / `&` / `-` operators |
| `runtime/python/mochi_runtime/collections.py` | `OrderedSet[T]` wrapper over `dict[T, None]` |
| `transpiler3/python/build/phase03_3_test.go` | `TestPhase33Sets`: 15 fixtures |
| `tests/transpiler3/python/fixtures/phase03-3-sets/` | 15 fixture directories: set_lit, set_add, set_has, set_len, set_iter_order, set_union, set_intersect, set_diff, set_empty, set_str, set_int, set_bool_membership, set_nested, set_eq, set_print |

## Test set

- `TestPhase33Sets`, walks all 15 fixtures with the standard gate stack.

## Deferred work

- Symmetric difference (`^`), deferred indefinitely (no Mochi surface).
- `frozenset`-equivalent immutable set, deferred indefinitely (Mochi `let` binding plus type checker is the immutability gate).
- Set-of-record fixtures, deferred to Phase 4 (records introduce hashable dataclasses).
- Optimised native-extension `OrderedSet` (C extension under `mochi_runtime._collections_native`), deferred to Phase 16 (reproducibility) or later if profiling warrants.
