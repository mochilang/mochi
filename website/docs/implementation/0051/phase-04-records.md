---
title: "Phase 4. Records"
sidebar_position: 9
sidebar_label: "Phase 4. Records"
description: "MEP-51 Phase 4, Mochi record types lowered to @dataclass(frozen=True, slots=True), with dataclasses.replace for the with update expression, cross-module imports, and field defaults via field(default_factory=...)."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 4](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase4Records`: 35 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: basic dataclass declaration plus construction plus equality, `dataclasses.replace` for Mochi `with`, nested records and cross-module imports, field defaults including `default_factory` for mutable containers.

## Goal-alignment audit

Records are the first nominal type with structure. Sum types (Phase 5), agent messages (Phase 9), stream items (Phase 10), and Datalog facts (Phase 8) all build on the frozen-slots dataclass shape. Phase 3.4 introduced a minimum subset (declaration + field read for use in lists); Phase 4 lands full semantics: equality, hashability, immutability via `frozen=True`, memory locality via `slots=True`, the `with` update via `dataclasses.replace`, and nested record types across module boundaries. If `__eq__` and `__hash__` are off here, sum types in Phase 5 inherit broken equality and Datalog tabling in Phase 8 silently dedupes nothing.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | `@dataclass(frozen=True, slots=True)` basic record + auto `__eq__`, `__hash__`, `__repr__` | NOT STARTED | — |
| 4.1 | `dataclasses.replace` for Mochi `with` update expression | NOT STARTED | — |
| 4.2 | Nested records (record-in-record) + cross-module imports | NOT STARTED | — |
| 4.3 | Default fields via `field(default=...)` for scalars and `field(default_factory=...)` for mutable containers | NOT STARTED | — |

## Sub-phase 4.0, Basic dataclass

### Goal-alignment audit (4.0)

Phase 3.4 emitted a minimum dataclass to make `list[Record]` compile. Phase 4.0 finishes the job: every emitted record has equality, hashability, immutability, and a `__repr__` that survives `print(record)` round-tripping.

### Decisions made (4.0)

**Emitted source for `type Point { x: int, y: int }`**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int
```

**Auto-derived methods**: `@dataclass(frozen=True, slots=True)` gives:

- `__init__(self, x: int, y: int)` (positional and keyword).
- `__eq__(self, other)` comparing all fields.
- `__hash__(self)` over the tuple of field values (only when `frozen=True`; mutable dataclasses get `__hash__ = None`).
- `__repr__(self)` returning `"Point(x=1, y=2)"`.
- `__match_args__ = ("x", "y")` for PEP 634 positional matching in Phase 5.

**Field naming**: Mochi snake_case field names are preserved verbatim (`user_id` stays `user_id`). Python convention agrees with Mochi here; no PascalCase conversion (which would be needed for .NET or JVM targets).

**`kw_only=True` threshold**: when a record has more than three fields, the lowerer emits `@dataclass(frozen=True, slots=True, kw_only=True)` so call sites must use keyword arguments and field-order drift becomes a compile error at every call site. Records with three or fewer fields stay with positional construction allowed.

**Equality semantics**: `Point(x=1, y=2) == Point(x=1, y=2)` is `True`. Both type checkers accept the dataclass-generated `__eq__`. Mochi-level `==` on records lowers directly to Python `==`.

**Hashability**: `hash(Point(x=1, y=2))` is well-defined and consistent with `__eq__`. Records can be used as dict keys, set elements, and Datalog table keys (Phase 8). `slots=True` does not affect hashability but reduces memory.

**`__repr__` and `print(p)`**: `print(Point(x=1, y=2))` produces `"Point(x=1, y=2)\n"`. vm3 also produces this representation. The Mochi-level `print(record)` lowers to `Print.line(repr(record))` only when the record has no custom display surface; the default dataclass `__repr__` matches vm3.

## Sub-phase 4.1, with update via dataclasses.replace

### Goal-alignment audit (4.1)

Mochi `{ r with x: 3 }` is the canonical immutable-update expression. Python has no native `with` expression on dataclasses, but `dataclasses.replace(r, x=3)` is the stdlib equivalent and is type-checker friendly under both mypy and pyright.

### Decisions made (4.1)

**Emitted source for `let p2 = { p with x: 3 }`**:

```python
from __future__ import annotations

from dataclasses import dataclass, replace


@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int


def main() -> None:
    p: Point = Point(x=1, y=2)
    p2: Point = replace(p, x=3)
```

**Why `replace` not a custom `with_x` helper**: `dataclasses.replace` is stdlib (no runtime helper to ship), type-aware (both checkers infer the return type as the same dataclass), and matches the canonical Python idiom. A custom helper per field would balloon the emitted module size and obscure the semantic.

**Multi-field update**: `{ p with x: 3, y: 4 }` lowers to `replace(p, x=3, y=4)`. `replace` accepts arbitrary keyword arguments and the type checker confirms each key is a valid field.

**Type checker quirk**: pyright 1.1.380+ infers `replace(p, x=3)` as `Point` correctly. mypy 1.13+ needs the `--strict` flag to be combined with the dataclass plugin (enabled by default since 1.0). No additional config required in `pyproject.toml`.

**`Mochi.with` chained**: `{ p with x: 3 } with y: 4` lowers to `replace(replace(p, x=3), y=4)`. The two-step form is slightly chatty but type-checker clean. A future v2 optimisation could collapse the two `replace` calls into one when the IR sees them adjacent.

## Sub-phase 4.2, Nested records and cross-module imports

### Goal-alignment audit (4.2)

Real Mochi programs declare records across modules. A `pipelines.compute` module imports `models.user.User` and produces results typed as `models.result.Result`. The lowerer must emit clean `from .models.user import User` imports and `ruff check --fix --select=I` must sort them deterministically.

### Decisions made (4.2)

**Emitted source for nested record**:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Address:
    street: str
    city: str


@dataclass(frozen=True, slots=True)
class User:
    id: int
    name: str
    address: Address


def main() -> None:
    u: User = User(
        id=1,
        name="Ana",
        address=Address(street="1 Main", city="Hanoi"),
    )
    home_city: str = u.address.city
```

**Cross-module import**: Mochi source `pipelines/compute.mochi` referencing `models.user.User` emits a Python module `pipelines/compute.py` with `from ..models.user import User` (relative import within the same package). Absolute imports (`from mochi_user.models.user import User`) are emitted only at the top level of the package per `ruff` convention.

**Forward references**: under `from __future__ import annotations`, all annotations are lazily evaluated. A record that references another record declared later in the same module compiles without issue (no forward-reference quoting needed). Mutually recursive records (User holds list of Friend, Friend holds User) compile cleanly under the future-import.

**Equality on nested records**: `__eq__` recurses through fields. `User(id=1, address=Address("1 Main", "Hanoi")) == User(id=1, address=Address("1 Main", "Hanoi"))` is `True` because `Address.__eq__` is also auto-derived and field-by-field.

**Hashing on nested records**: `hash(user)` hashes the tuple `(id, name, address)`, which in turn hashes the `Address` tuple. Both records must be frozen for hashing to work; the lowerer always emits `frozen=True`.

## Sub-phase 4.3, Field defaults

### Goal-alignment audit (4.3)

Mochi `type Config { retries: int = 3, timeout: float = 1.0, tags: list<str> = [] }` is a common pattern. Python dataclass defaults must use `field(default_factory=...)` for mutable defaults (lists, dicts, sets) to avoid the well-known "shared mutable default" bug.

### Decisions made (4.3)

**Emitted source for `type Config { retries: int = 3, tags: list<str> = [] }`**:

```python
from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True, slots=True)
class Config:
    retries: int = 3
    tags: list[str] = field(default_factory=list)
```

**Scalar default**: lowers to a literal default (`retries: int = 3`). Both type checkers accept this on a `frozen=True` dataclass.

**Mutable default**: lowers to `field(default_factory=list)` (or `dict`, or `set`, or a no-arg constructor for any user record). Python forbids `field(default=[])` on dataclasses (raises at class creation time); the lowerer must use the factory form.

**`default_factory` for nested record**: `tags: TagSet = TagSet()` lowers to `tags: TagSet = field(default_factory=TagSet)`. The factory is the bare class name (no-arg constructor); for parameterised defaults, the lowerer emits a `lambda: TagSet(initial_capacity=8)` factory.

**Defaults must follow non-defaults**: Python forbids `def foo(x=1, y)`. Dataclasses inherit this restriction. The Mochi type checker enforces the same ordering at the record-declaration level (records with defaults follow records without defaults in field order); the lowerer trusts this and does not re-validate.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | `RecordDecl` to `@dataclass(frozen=True, slots=True)`; `with` expression to `dataclasses.replace`; nested record support |
| `transpiler3/python/lower/dataclass.go` | Phase 3.4 minimal lowering extended with defaults (`field(default=...)` and `field(default_factory=...)`); `kw_only=True` threshold; cross-module import emission |
| `transpiler3/python/build/phase04_test.go` | `TestPhase4Records`: 35 fixtures |
| `tests/transpiler3/python/fixtures/phase04-records/` | 35 fixture directories: rec_basic, rec_int_field, rec_str_field, rec_bool_field, rec_float_field, rec_two_fields, rec_three_fields, rec_four_fields_kw, rec_eq_true, rec_eq_false, rec_hash_in_set, rec_hash_in_dict_key, rec_with_one_field, rec_with_two_fields, rec_with_chained, rec_nested_one_level, rec_nested_two_levels, rec_mutual_recursive, rec_default_int, rec_default_str, rec_default_bool, rec_default_float, rec_default_factory_list, rec_default_factory_dict, rec_default_factory_set, rec_default_factory_record, rec_cross_module_simple, rec_cross_module_nested, rec_cross_module_with_update, rec_repr, rec_print, rec_field_access_chain, rec_fn_arg, rec_fn_return, rec_in_list |

## Test set

- `TestPhase4Records`, walks all 35 fixtures with the standard gate stack.

## Deferred work

- `__match_args__` positional vs keyword-only matching strategy under PEP 634, deferred to Phase 5.1 (sum-type match emission picks the strategy).
- JSON serialisation via `dataclasses.asdict` plus `json.dumps`, deferred to Phase 12 (FFI surfaces JSON helpers in `mochi_runtime.json`).
- `pydantic.BaseModel` adapter for FastAPI consumers, deferred to v1.5 per MEP-51 §Open questions Q1.
- Mutable record fields (Mochi `var` field), deferred indefinitely (Mochi records are immutable by spec).
