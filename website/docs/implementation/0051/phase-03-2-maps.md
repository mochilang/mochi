---
title: "Phase 3.2. Maps"
sidebar_position: 6
sidebar_label: "Phase 3.2. Maps"
description: "MEP-51 Phase 3.2, Mochi map literal / index / len / keys / values / has / for-each lowered to Python dict with insertion-order semantics."
---

# Phase 3.2. Maps

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 3.2](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase32Maps`: 25 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: map literal construction with key / value type inference, index, `len`, `.keys()`, `.values()`, `in` membership, `for-each` over items, nested maps, and Mochi map comprehensions.

## Goal-alignment audit

Maps are Mochi's primary associative collection and the substrate for record-like access patterns before Phase 4 introduces dataclasses. Python `dict[K, V]` matches Mochi `map<K, V>` exactly on insertion-order semantics (since Python 3.7). The lowering must preserve the type-checker friendliness established in Phase 3.1 lists, and the membership and iteration syntax must read idiomatically.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.2.0 | Map literal `{"a": 1}`, index `m["a"]`, `len(m)`, typed as `dict[K, V]` | NOT STARTED | — |
| 3.2.1 | `m.keys()`, `m.values()`, `k in m` membership | NOT STARTED | — |
| 3.2.2 | `for (k, v) in m { ... }` over items | NOT STARTED | — |
| 3.2.3 | Nested dict (`dict[str, dict[str, int]]`) + Mochi map comprehensions | NOT STARTED | — |

## Sub-phase 3.2.0, Map literal, index, len

### Goal-alignment audit (3.2.0)

The first three primitives (`{...}`, `[]`, `len`) are the minimal map surface and the foundation for everything later. Without them, no fixture in this phase compiles.

### Decisions made (3.2.0)

**Emitted source for `let m = {"a": 1, "b": 2}`**:

```python
from __future__ import annotations


def main() -> None:
    m: dict[str, int] = {"a": 1, "b": 2}
    a: int = m["a"]
    n: int = len(m)
```

**Key and value type annotation**: the Mochi type checker resolves `{"a": 1}` as `map<str, int>`. The lowerer emits `dict[str, int]` (PEP 585 built-in subscripted generic, never `typing.Dict[str, int]`).

**Empty map**: `let m: map<str, int> = {}` lowers to `m: dict[str, int] = {}`. Annotation is required (otherwise `mypy --strict` flags as `dict[Never, Never]`).

**Insertion order**: Python `dict` preserves insertion order since 3.7. Mochi maps also preserve insertion order. The two agree without a wrapper.

**`m["missing"]` lookup**: lowers to `m["missing"]`, raises `KeyError` on miss. Mochi-level `panic` semantics align; Phase 11 will wrap explicit `try` blocks into `MochiResult.Err`.

## Sub-phase 3.2.1, keys, values, membership

### Goal-alignment audit (3.2.1)

`.keys()` and `.values()` are the most common iteration entry points. `k in m` is the canonical existence check. Each maps directly to a Python idiom that both type checkers accept.

### Decisions made (3.2.1)

**Emitted source**:

```python
from __future__ import annotations


def main() -> None:
    m: dict[str, int] = {"a": 1, "b": 2}
    ks: list[str] = list(m.keys())
    vs: list[int] = list(m.values())
    has_a: bool = "a" in m
```

**Why `list(m.keys())`**: Python's `m.keys()` returns a view (`dict_keys[K]`), not a `list`. Both type checkers infer `dict_keys[str]` rather than `list[str]`. To match the Mochi-level `list<K>` return type of `.keys()`, the lowerer wraps with `list(...)`. The `list()` call is O(n) but unavoidable; the Mochi semantic is "a list of keys", not "a view".

**Iteration without materialisation**: when `m.keys()` is the iteration source of a `for-each` (Phase 3.2.2) rather than a let binding, the lowerer omits the `list()` wrap. The view iterates the same way.

**Membership**: Mochi `m has "a"` lowers to Python `"a" in m`. Python's `in` on `dict` is O(1) average. Both type checkers accept `str in dict[str, int]` as `bool`.

## Sub-phase 3.2.2, for-each over items

### Goal-alignment audit (3.2.2)

Item iteration (key + value together) is the canonical reduce loop. Python's `m.items()` returns `(K, V)` tuples and supports destructuring in the `for` head.

### Decisions made (3.2.2)

**Emitted source for `for (k, v) in m { print(k); print(v) }`**:

```python
from __future__ import annotations

from mochi_runtime.io import Print


def main() -> None:
    m: dict[str, int] = {"a": 1, "b": 2}
    for k, v in m.items():
        Print.line(k)
        Print.line(v)
```

**Tuple destructuring in `for` head**: Python supports `for k, v in m.items():`. Both type checkers infer `k: str`, `v: int`. No mangling needed.

**Iteration order**: matches insertion order (Python 3.7+ contract). vm3 also iterates in insertion order. No re-sort needed.

**Async iteration over a dict**: not a Mochi surface (dicts are sync collections); no `async for` emission.

## Sub-phase 3.2.3, Nested dict and comprehensions

### Goal-alignment audit (3.2.3)

Nested maps appear in real Mochi programs (e.g., a per-tenant settings map). The annotation must compose. Mochi map comprehensions (`from k in keys select (k, e(k))`) lower to Python dict comprehensions.

### Decisions made (3.2.3)

**Emitted source for nested dict**:

```python
from __future__ import annotations


def main() -> None:
    config: dict[str, dict[str, int]] = {
        "alpha": {"x": 1, "y": 2},
        "beta": {"x": 3, "y": 4},
    }
    alpha_x: int = config["alpha"]["x"]
```

**Emitted source for map comprehension**:

```python
from __future__ import annotations


def main() -> None:
    xs: list[int] = [1, 2, 3]
    squared: dict[int, int] = {x: x * x for x in xs}
```

**Mochi map comprehension surface**: `from x in xs into_map { x: x * x }` (provisional surface name; the lowerer keys off the IR node, not the syntax). Lowers to a dict comprehension when the Mochi type checker resolves the result as `map<K, V>`.

**No `defaultdict`**: Mochi has no default-value map surface in v1. If a Mochi program needs default-on-miss semantics, it lowers to an explicit `m.get(k, default)` call.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Map literal, index, `len`, `.keys()`, `.values()`, `in` membership, `for-each` over items, nested dict |
| `transpiler3/python/lower/comprehension.go` | Mochi map comprehension to Python dict comprehension |
| `transpiler3/python/build/phase03_2_test.go` | `TestPhase32Maps`: 25 fixtures |
| `tests/transpiler3/python/fixtures/phase03-2-maps/` | 25 fixture directories: map_lit, map_index, map_len, map_keys, map_values, map_has, map_for_each_items, map_for_each_keys, map_for_each_values, map_empty, map_nested, map_str_int, map_str_str, map_str_bool, map_str_float, map_compr, map_index_missing, map_get_default, map_iter_order, map_in_func, map_in_list, map_concat (via `**`), map_assign, map_overwrite, map_str_to_list |

## Test set

- `TestPhase32Maps`, walks all 25 fixtures with the standard gate stack.

## Deferred work

- `defaultdict` / `m.get_or_insert(k, factory)` semantics, deferred to Phase 7 (Query DSL group-by emits a default-init aggregator).
- `TypedDict` for heterogeneous-value maps, deferred to Phase 13 (LLM provider-config maps).
- `frozendict` / immutable map type, deferred indefinitely (Mochi `let` plus type-checker prevents rebinding; in-place mutation prevention via runtime wrapping rejected on cost grounds).
- Map-of-record fixtures, picked up by Phase 4 (records) and Phase 3.4 (list of records) together.
