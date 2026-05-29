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
| Status         | LANDED 2026-05-29 17:35 (GMT+7) |
| Started        | 2026-05-29 17:15 (GMT+7) |
| Landed         | 2026-05-29 17:35 (GMT+7) |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

`TestPhase32Maps`: 14 fixtures green on the local CPython 3.13 toolchain (the multi-toolchain + tier-1 OS matrix is the Phase 17/18 umbrella's responsibility; this sub-phase ships green against the local toolchain only). Carry-forward gates (`mypy --strict`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix`) remain deferred to Phase 16, consistent with the previous sub-phases.

Fixtures cover: map literal construction with `dict[K, V]` PEP 585 annotation (K ∈ string / int; V ∈ int / float / bool / string), `m[k]` read, `m[k] = v` write, `has(m, k)` membership, `len(m)`, `keys(m)`, `values(m)` (both with stable ascending-key order to match vm3), iteration over `keys(m)` with subsequent value lookup, multi-write update semantics, empty map literal, map-typed function param + return, and the int-keyed instantiation.

## Goal-alignment audit

Maps are Mochi's primary associative collection and the substrate for record-like access patterns before Phase 4 introduces dataclasses. Python `dict[K, V]` matches Mochi `map<K, V>` exactly on insertion-order semantics (since Python 3.7). The lowering must preserve the type-checker friendliness established in Phase 3.1 lists, and the membership and iteration syntax must read idiomatically.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.2.0 | Map literal `{"a": 1}`, index `m["a"]`, `len(m)`, typed as `dict[K, V]` | LANDED 2026-05-29 | TBD |
| 3.2.1 | `keys(m)`, `values(m)` (key-sorted via runtime helper), `has(m, k)` membership | LANDED 2026-05-29 | TBD |
| 3.2.2 | `for (k, v) in m { ... }` over items | DEFERRED — Mochi surface lowers via `keys(m)` + lookup; no aotir node for paired key/value iteration |
| 3.2.3 | Nested map (`dict[str, dict[str, int]]`) + Mochi map comprehensions | DEFERRED to Phase 7.0 (comprehensions) / Phase 3.4 (nested compounds) |

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

## Decisions made during landing (2026-05-29)

**`keys(m)` / `values(m)` route through `mochi_runtime.mapping.keys_sorted` / `values_sorted`, not `m.keys()` / `m.values()`**: the aotir spec for `MapKeysExpr` mandates ascending-key order to stay byte-equal with vm3. Python `dict.keys()` returns insertion order, which only happens to match the sorted order in the inherited dotnet fixtures because the literals were already written in alphabetical order. The runtime helpers wrap `sorted(m.keys())` / `[m[k] for k in sorted(m.keys())]` so the lowering is robust against future fixtures that insert keys out of order. The Phase 3.2 `map_keys_sorted.mochi` fixture pins this down: it literal-constructs `{"c": 3, "a": 1, "b": 2}` and expects `a / b / c` in iteration.

**`has(m, k)` emits `k in m`**: the idiomatic Python form; O(1) average for dicts and reads as English at the call site. `dict.__contains__` is what vm3's runtime helper resolves to internally.

**`m[k] = v` emits an `IndexAssignStmt`, not a method call**: Python `dict.__setitem__` syntax matches Mochi `m[k] = v` one to one. No reassignment of the surrounding binding needed; the lowerer's `MapPutStmt` arm renders `<Name>[key] = value` and lets Python mutate in place. This is the only place in the Phase 3.2 surface where mutation is observable, matching Mochi's reference semantics for maps.

**`m[k]` (read) emits `m[k]`, not `m.get(k)`**: aotir's `MapGetExpr` panics on missing keys (matching vm3). Python's `dict[k]` raises `KeyError`, which propagates to `__main__` and surfaces as a traceback, the same shape as the Phase 3.1 list out-of-range case. No `MochiResult.Err` adapter until Phase 11.

**Empty map literal `{}` is parsed and lowered**: `let m: map<string, int> = {}` produces `m: dict[str, int] = {}` and `len(m) == 0`. Annotation is required on the Mochi side because the literal alone is untyped; the lowerer propagates the annotation through `pyTypeForCompound`.

**`pyTypeForCompound(t, elem, k, v)` replaces `pyTypeForFull(t, elem)`**: a single resolver now handles list (`elem`) and map (`k`, `v`) annotations. The four-argument form keeps the call sites stable as later phases add `set[T]` and `tuple[T, ...]`.

**Sub-phase 3.2.2 (`for (k, v) in m`) deferred without code**: aotir has no paired-key/value iteration node today; the Mochi-side surface is `for k in keys(m) { print(m[k]) }`, which already works. Wiring a `dict.items()` lowering before there is an IR node would be premature; this is the audit-rule call from [[feedback_goal_alignment_audit]].

**Cache phase marker bumped to `mep51-phase03-2`**: same rationale as Phase 3.1, so old Phase 3.1 wheels do not shadow Phase 3.2 emit changes.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/pysrc/nodes.go` | New `DictLit`, `IndexAssignStmt` AST nodes |
| `transpiler3/python/lower/lower.go` | `aotir.MapLit` / `MapGetExpr` / `MapHasExpr` / `MapLenExpr` / `MapKeysExpr` / `MapValuesExpr` / `MapPutStmt`; `pyTypeForCompound` renames + extends `pyTypeForFull` to thread `KeyType` / `ValueType` into `dict[K, V]` annotations |
| `runtime/python/mochi_runtime/mapping.py` | `keys_sorted(m)`, `values_sorted(m)` |
| `transpiler3/python/build/build.go` | `cacheKey` marker bumped to `mep51-phase03-2` |
| `transpiler3/python/build/phase03_2_test.go` | `TestPhase32Maps` walks all fixtures |
| `tests/transpiler3/python/fixtures/phase03-2-maps/` | 14 fixtures: map_bool_values, map_empty, map_float_values, map_fn_return, map_has, map_int_keys, map_iterate_print, map_keys, map_keys_sorted, map_len, map_overwrite, map_put_get, map_update, map_values |

## Test set

- `TestPhase32Maps`, walks all 14 fixtures and diffs stdout byte-for-byte against the `.out` file.

## Deferred work

- `for (k, v) in m { ... }` (sub-phase 3.2.2) deferred: no aotir node for paired key/value iteration; the equivalent surface `for k in keys(m) { print(m[k]) }` already works (see `map_iterate_print.mochi`).
- Nested maps (sub-phase 3.2.3 first half) deferred to Phase 3.4.
- Mochi map comprehensions (sub-phase 3.2.3 second half) deferred to Phase 7.0 (lowers via `QueryExpr`).
- `m[k]` returning `option<V>` from the Mochi type checker side rules out arithmetic patterns like `m[w] = m[w] + 1` until Phase 5 lands option destructuring. The Phase 3.2 fixtures avoid this by either assigning a literal RHS or reading inside `print(...)` which accepts option.
- `defaultdict` / `m.get_or_insert(k, factory)` semantics, deferred to Phase 7 (Query DSL group-by emits a default-init aggregator).
- `TypedDict` for heterogeneous-value maps, deferred to Phase 13 (LLM provider-config maps).
- `frozendict` / immutable map type, deferred indefinitely.
- Map-of-record fixtures, picked up by Phase 4 (records) and Phase 3.4 (list of records) together.
- `mypy --strict`, `pyright --strict`, `ruff format` fixed-point, multi-Python matrix — all carry-forward to Phase 16.
