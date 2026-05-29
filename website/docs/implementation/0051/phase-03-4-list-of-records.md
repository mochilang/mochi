---
title: "Phase 3.4. List of records"
sidebar_position: 8
sidebar_label: "Phase 3.4. List of records"
description: "MEP-51 Phase 3.4, Mochi list of dataclass instances lowered to list[Record] with field access in comprehensions, sorting, and filtering, before full record semantics ship in Phase 4."
---

# Phase 3.4. List of records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 3.4](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase34ListOfRecords`: 20 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: `list[Record]` typing, comprehensions over records with field access (`[r.name for r in users]`), sorting by field, filtering by field predicate.

## Goal-alignment audit

Phase 3.4 is the first place where lists meet a user-defined nominal type. The Query DSL (Phase 7) builds on `list[Record]` for nearly every fixture. Lowering field access in a comprehension correctly (`[r.name for r in users]`) is the load-bearing step that lets Phase 7's `from r in users select r.name` work without further lowering effort. Phase 3.4 lands a minimal subset of Phase 4 record support (declaration plus field read) just enough to populate and iterate `list[Record]`; full record semantics (equality, `with` update, `replace`) ship in Phase 4.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.4.0 | `list[Record]` typing: declare a minimal `@dataclass(frozen=True, slots=True)` record and a `list[Record]` variable | NOT STARTED | — |
| 3.4.1 | Comprehensions over records: `[r.name for r in users]` and predicate filtering | NOT STARTED | — |
| 3.4.2 | Sorting (`sorted(xs, key=...)`) and filtering (`[r for r in xs if p(r)]`) | NOT STARTED | — |

## Sub-phase 3.4.0, list of dataclass instances

### Goal-alignment audit (3.4.0)

A list with no user-defined element type only carries Phase 2 / 3.1 surface. Phase 3.4.0 introduces the minimal record so subsequent sub-phases have something to read fields from. Full record handling (equality, `with`, multi-field, defaults) is Phase 4; 3.4.0 only emits enough for `list[User]` to compile and iterate.

### Decisions made (3.4.0)

**Emitted source for `type User { id: int, name: str }; let users = [User{id: 1, name: "Ana"}, User{id: 2, name: "Bo"}]`**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class User:
    id: int
    name: str


def main() -> None:
    users: list[User] = [
        User(id=1, name="Ana"),
        User(id=2, name="Bo"),
    ]
```

**`@dataclass(frozen=True, slots=True)`**: the canonical Mochi record shape per MEP-51 §6. `frozen=True` gives free `__hash__`, immutability, and equality. `slots=True` drops `__dict__` and gives memory locality. Both gates required for all later phases.

**Keyword-only construction**: the lowerer emits `User(id=1, name="Ana")` rather than positional `User(1, "Ana")` to guard against field-order drift if the Mochi source reorders fields between revisions. Per MEP-51 §6, `kw_only=True` is added when a record has more than three fields; 3.4.0 stays with positional-acceptable construction but emits kwargs at the call site for readability.

**Module-scope declaration**: the record class is emitted at module top level (above `def main`), not nested inside `main`. Nested dataclass declarations break `mypy --strict` (closure over a nested class fails generic instantiation under PEP 695).

**Cross-module record import**: when a Mochi program declares the record in module `models.user` and uses it in module `pipelines.compute`, the lowerer emits `from .models.user import User` in `compute.py`. Cross-module record imports are exercised here so Phase 4 inherits a working import flow.

## Sub-phase 3.4.1, Comprehensions over records

### Goal-alignment audit (3.4.1)

Field access inside a comprehension is the prototypical Mochi pattern `from x in xs select x.field`. It is also the simplest projection in Phase 7's Query DSL. Phase 3.4.1 lands it once.

### Decisions made (3.4.1)

**Emitted source for `let names = from u in users select u.name`**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class User:
    id: int
    name: str


def main() -> None:
    users: list[User] = [
        User(id=1, name="Ana"),
        User(id=2, name="Bo"),
    ]
    names: list[str] = [u.name for u in users]
```

**Type checker propagation**: both mypy and pyright infer `u.name` as `str` from the `list[User]` annotation. No explicit type annotation on the comprehension is needed; the `names: list[str]` annotation is the user-facing target type.

**Multi-field projection** (`from u in users select (u.id, u.name)`) lowers to a tuple comprehension `[(u.id, u.name) for u in users]` with annotation `list[tuple[int, str]]`. Tuples are the Mochi-side return type when the projection has no nominal record target.

**Anonymous record projection** is not emitted in 3.4.1 (the Mochi surface requires an explicit target type for record results). Phase 4 covers user-named record projections.

## Sub-phase 3.4.2, Sorting and filtering

### Goal-alignment audit (3.4.2)

Sorting and filtering are the two most common list-of-records operations. They underpin Phase 7's `order_by` and `where` clauses. Lowering them here gives Phase 7 a working primitive to compose.

### Decisions made (3.4.2)

**Emitted source for `let adults = [u for u in users if u.age >= 18]`** (Mochi `from u in users where u.age >= 18 select u`):

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class User:
    id: int
    name: str
    age: int


def main() -> None:
    users: list[User] = [
        User(id=1, name="Ana", age=21),
        User(id=2, name="Bo", age=15),
    ]
    adults: list[User] = [u for u in users if u.age >= 18]
```

**Emitted source for sort by field** (Mochi `from u in users order_by u.age select u`):

```python
sorted_users: list[User] = sorted(users, key=lambda u: u.age)
```

**Why `lambda`** rather than `operator.attrgetter`: `lambda u: u.age` reads more cleanly in generated code and both type checkers infer `Callable[[User], int]` from the surrounding context. `operator.attrgetter` adds a runtime helper import and obscures the field name in tracebacks.

**Multi-key sort**: `order_by u.age desc, u.name asc` lowers to a two-step sort:

```python
sorted_users: list[User] = sorted(
    sorted(users, key=lambda u: u.name),
    key=lambda u: u.age,
    reverse=True,
)
```

The inner sort handles the secondary key first; Python's sort is stable, so the outer primary sort preserves the secondary ordering. Phase 7.2 refines this into `sorted(users, key=lambda u: (-u.age, u.name))` when the keys are scalar; the two-step form is the universal fallback.

**`filter()` vs comprehension**: comprehensions are emitted by default (more idiomatic, type-checker friendly). `filter()` is only emitted for higher-order callers (Phase 6).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Minimal `RecordDecl` lowering to frozen-slots dataclass; field access inside comprehensions; sorted+filter primitives |
| `transpiler3/python/lower/dataclass.go` | `@dataclass(frozen=True, slots=True)` emission with field-by-field annotation |
| `transpiler3/python/build/phase03_4_test.go` | `TestPhase34ListOfRecords`: 20 fixtures |
| `tests/transpiler3/python/fixtures/phase03-4-list-of-records/` | 20 fixture directories: lor_basic, lor_field_read, lor_compr_proj, lor_compr_tuple_proj, lor_compr_where, lor_sort_by_int, lor_sort_by_str, lor_sort_desc, lor_sort_multi_key, lor_filter_predicate, lor_filter_and_sort, lor_count_filtered, lor_nested_compr, lor_for_each, lor_index_first, lor_empty_filter, lor_record_eq_in_filter, lor_record_two_fields, lor_record_three_fields, lor_cross_module_import |

## Test set

- `TestPhase34ListOfRecords`, walks all 20 fixtures with the standard gate stack.

## Deferred work

- Record `with` update inside comprehensions, deferred to Phase 4.1.
- `dataclasses.replace` for partial copy, deferred to Phase 4.1.
- Nested records (record-in-record), deferred to Phase 4.2.
- `frozen=True` equality across deep structures, deferred to Phase 4 (full record semantics).
