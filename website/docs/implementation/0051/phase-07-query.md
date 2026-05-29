---
title: "Phase 7. Query DSL"
sidebar_position: 12
sidebar_label: "Phase 7. Query DSL"
description: "MEP-51 Phase 7, Mochi from/where/select/group_by/order_by/joins lowered to Python generator expressions plus itertools.groupby plus sorted, with async queries over AsyncIterator via mochi_runtime.stream helpers."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 7](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase7Query`: 40 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: `from / where / select` chains, `group_by` with aggregates, `order_by asc / desc` with multi-key sort, inner and left joins, async queries over streams.

## Goal-alignment audit

The Query DSL is Mochi's primary data-wrangling surface and the most common construct in the fixture corpus. Phase 7 lowers Mochi `from / where / select` to Python generator expressions and list comprehensions, `group_by` to `sorted` + `itertools.groupby`, `order_by` to `sorted(key=..., reverse=...)`, and joins to nested comprehensions or `itertools.product`. Async queries over streams reuse Phase 10's `AsyncIterator` shape via runtime helpers (`aiter`, `anext`) re-exported from `mochi_runtime.stream`. If the comprehension lowering is correct, the whole data pipeline (Phase 8 Datalog, Phase 10 streams, Phase 14 fetch JSON-streaming) inherits a working compositional substrate.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `from`, `where`, `select` to Python generator expressions (consumed via list comprehension or `for-each`) | NOT STARTED | — |
| 7.1 | `group_by` to `itertools.groupby` after `sorted` | NOT STARTED | — |
| 7.2 | `order_by k1 asc, k2 desc` to `sorted(xs, key=lambda r: (r.k1, neg(r.k2)))` with multi-key composition | NOT STARTED | — |
| 7.3 | Inner and left joins to nested comprehensions or `itertools.product` plus filter | NOT STARTED | — |
| 7.4 | Async queries over streams: `async for` plus `aiter` / `anext` helpers from `mochi_runtime.stream` | NOT STARTED | — |

## Sub-phase 7.0, from / where / select

### Goal-alignment audit (7.0)

The three-clause base case (`from`, `where`, `select`) is the most common shape. If the lowerer emits a clean generator expression, both type checkers infer the result type without help. Performance is good (lazy evaluation), and downstream operators (`group_by`, `order_by`) compose without materialising intermediates.

### Decisions made (7.0)

**Emitted source for `from u in users where u.age >= 18 select u.name`**:

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
    adult_names: list[str] = [u.name for u in users if u.age >= 18]
```

**Generator expression vs list comprehension**: when the result is bound to a `list<T>` variable or returned from a function with `list<T>` return type, the lowerer emits a list comprehension (`[expr for x in xs if p]`). When the result flows into a `for-each` directly, the lowerer emits a generator expression (`(expr for x in xs if p)`) to avoid materialising the intermediate list.

**Where-clause placement**: Mochi `where p(x)` after `from` lowers to `if p(x)` at the comprehension tail. Multiple `where` clauses concatenate (`if p(x) if q(x)`) which Python evaluates left-to-right with short-circuit semantics.

**Select-clause projection**: scalar projection (`select u.name`) lowers to `u.name`; tuple projection (`select (u.id, u.name)`) lowers to a tuple literal `(u.id, u.name)`; record projection (`select User{id: u.id, name: u.name}`) lowers to `User(id=u.id, name=u.name)`.

**Multi-source `from`** (`from u in users from o in orders`) lowers to nested comprehension clauses: `[(u, o) for u in users for o in orders]`. Cartesian product is explicit at the IR level; the lowerer does not need `itertools.product` for this shape.

## Sub-phase 7.1, group_by

### Goal-alignment audit (7.1)

`group_by` is the canonical aggregation primitive. Python has no built-in group-by expression; `itertools.groupby` requires sorted input. The lowerer pairs them (sort first, then group) and emits the per-group aggregate inline.

### Decisions made (7.1)

**Emitted source for `from o in orders group_by o.customer_id into g select { id: g.key, total: g.sum(o.amount) }`**:

```python
from __future__ import annotations

from dataclasses import dataclass
from itertools import groupby
from operator import attrgetter


@dataclass(frozen=True, slots=True)
class Order:
    id: int
    customer_id: int
    amount: float


@dataclass(frozen=True, slots=True)
class CustomerTotal:
    id: int
    total: float


def main() -> None:
    orders: list[Order] = []  # populated elsewhere
    by_customer: list[CustomerTotal] = [
        CustomerTotal(
            id=key,
            total=sum(o.amount for o in group),
        )
        for key, group in groupby(
            sorted(orders, key=attrgetter("customer_id")),
            key=attrgetter("customer_id"),
        )
    ]
```

**Why `sorted` + `itertools.groupby` rather than a dict accumulator**: `itertools.groupby` matches the Mochi semantics one-to-one (groups are contiguous after sort, aggregates roll over each group). A dict-based accumulator would also work but loses the streaming property and forces the lowerer to special-case sum/min/max/avg per aggregate function. The sort + groupby pattern composes uniformly for any per-group aggregate expression.

**`attrgetter` vs lambda**: `attrgetter("customer_id")` is the canonical Python idiom for keyed sort/group. It is faster than `lambda o: o.customer_id` (one less Python-level call) and reads more clearly. The lowerer emits `attrgetter` when the key is a single attribute access; falls back to `lambda` for arithmetic or multi-field keys.

**`group` reuse**: `itertools.groupby` yields an iterator that is exhausted after one pass. The lowerer materialises `group` only inside the per-aggregate scope: `sum(o.amount for o in group)` is the canonical single-pass aggregate.

**Multi-aggregate per group**: when the `select` clause has more than one aggregate, the lowerer materialises the group once: `group = list(group)` then `sum(...)`, `min(...)`, `max(...)` over the materialised list. This avoids exhausting the iterator after the first aggregate.

## Sub-phase 7.2, order_by

### Goal-alignment audit (7.2)

`order_by` is the second-most-common Query DSL clause. Python's `sorted(xs, key=..., reverse=...)` handles single-key ascending/descending directly. Multi-key sort needs a tuple key (with negation for descending numeric keys), with a stable-sort fallback for non-negatable types (strings).

### Decisions made (7.2)

**Single-key ascending**:

```python
from __future__ import annotations

from operator import attrgetter


sorted_users: list[User] = sorted(users, key=attrgetter("age"))
```

**Single-key descending**:

```python
sorted_users: list[User] = sorted(users, key=attrgetter("age"), reverse=True)
```

**Multi-key ascending**:

```python
sorted_users: list[User] = sorted(
    users,
    key=attrgetter("age", "name"),
)
```

`attrgetter` with multiple names returns a tuple (`(o.age, o.name)`) which Python compares lexicographically. Matches Mochi semantics.

**Multi-key mixed (asc / desc) with numeric desc**: for descending on a numeric key, the lowerer emits a negated key inside a `lambda`:

```python
sorted_users: list[User] = sorted(
    users,
    key=lambda u: (-u.age, u.name),
)
```

**Multi-key mixed with non-numeric desc**: strings cannot be negated. The lowerer falls back to two-pass stable sort:

```python
# order_by name desc, age asc:
sorted_users: list[User] = sorted(
    sorted(users, key=attrgetter("age")),
    key=attrgetter("name"),
    reverse=True,
)
```

Python's sort is stable since 2.3 (Timsort), so the inner sort's order is preserved for ties on the outer key.

**`take n` after `order_by`**: lowers to `[:n]` slice on the sorted result. `sorted(users, key=...)[:n]` is the canonical limit pattern.

## Sub-phase 7.3, Joins

### Goal-alignment audit (7.3)

Joins are the canonical multi-source query. Inner joins lower to nested comprehensions with an equality filter. Left joins need a `default` for missing right-side matches.

### Decisions made (7.3)

**Inner join**:

```python
from __future__ import annotations


def main() -> None:
    orders: list[Order] = []
    customers: list[Customer] = []
    joined: list[tuple[Order, Customer]] = [
        (o, c) for o in orders for c in customers if o.customer_id == c.id
    ]
```

**Indexed inner join (build hash map for the smaller side)**: when the Mochi IR carries an inner-join hint, the lowerer emits a pre-built dict:

```python
customer_by_id: dict[int, Customer] = {c.id: c for c in customers}
joined: list[tuple[Order, Customer]] = [
    (o, customer_by_id[o.customer_id])
    for o in orders
    if o.customer_id in customer_by_id
]
```

This is O(n + m) instead of O(n * m). The IR pass picks the hashed form when both sides have a unique join key.

**Left join**:

```python
joined: list[tuple[Order, Customer | None]] = [
    (
        o,
        next((c for c in customers if c.id == o.customer_id), None),
    )
    for o in orders
]
```

`next(iterator, default)` returns the first match or the default. For an indexed left join (build dict for the right side), the lowerer emits `customer_by_id.get(o.customer_id, None)`.

**`itertools.product`** is not emitted for joins (the comprehension form is more readable). It is emitted only for explicit Mochi Cartesian-product surface (rare).

## Sub-phase 7.4, Async queries over streams

### Goal-alignment audit (7.4)

Streams (Phase 10) expose `AsyncIterator[T]`. A Mochi query over a stream must use `async for` rather than `for`. The lowerer detects async stream sources from the IR's colour pass and emits `async for` with `aiter` / `anext` helpers re-exported from `mochi_runtime.stream`.

### Decisions made (7.4)

**Emitted source for `from x in async_stream where p(x) select f(x)`**:

```python
from __future__ import annotations

from collections.abc import AsyncIterator

from mochi_runtime.stream import aiter as mochi_aiter


async def collect_filtered(source: AsyncIterator[int]) -> list[int]:
    results: list[int] = []
    async for x in mochi_aiter(source):
        if x > 0:
            results.append(x * 2)
    return results
```

**Why `mochi_aiter` rather than the builtin `aiter`**: Python's `aiter` (3.10+) is a builtin that calls `__aiter__`. The runtime helper `mochi_runtime.stream.aiter` does the same thing but is also typed under `--strict` for both checkers across all platforms (pyright on Windows occasionally has narrower typing on the builtin). Keeping the runtime indirection avoids edge cases and provides a hook for Phase 10's stream bounded-buffer behaviour.

**`async for` in a list comprehension**: PEP 530 added `[x async for x in source]` (async comprehension). The lowerer emits this form when the enclosing function is `async def`:

```python
results: list[int] = [x * 2 async for x in mochi_aiter(source) if x > 0]
```

Both checkers accept async comprehensions under `--strict`.

**Mixed sync source + async filter**: a Mochi query with a sync source list but an async filter callback lowers via `mochi_runtime.stream.to_async`:

```python
from mochi_runtime.stream import to_async

results: list[int] = [
    x async for x in to_async(xs) if await is_valid(x)
]
```

**`async for` over `AsyncIterator` with backpressure**: bounded mailbox / bounded stream surface lands in Phase 10. Phase 7.4 only handles the query-side composition.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Query DSL lowering dispatcher: `from / where / select` to generator expressions; `group_by` to `sorted` + `groupby`; `order_by` to `sorted`; joins to nested comprehensions or hashed dict lookups |
| `transpiler3/python/lower/query.go` | Per-clause lowering helpers (`lowerFromWhere`, `lowerGroupBy`, `lowerOrderBy`, `lowerJoin`) |
| `transpiler3/python/lower/query_async.go` | Async query lowering: `async for`, `mochi_runtime.stream.aiter`, async comprehension |
| `runtime/python/mochi_runtime/stream.py` | `aiter`, `anext`, `to_async`, `aiter_bounded` helpers re-exported for Query DSL use (full stream surface in Phase 10) |
| `transpiler3/python/build/phase07_test.go` | `TestPhase7Query`: 40 fixtures |
| `tests/transpiler3/python/fixtures/phase07-query/` | 40 fixture directories: query_select_scalar, query_select_tuple, query_select_record, query_where, query_where_chain, query_from_two_sources, query_group_by_sum, query_group_by_count, query_group_by_avg, query_group_by_multi_agg, query_order_by_asc, query_order_by_desc, query_order_by_multi_key, query_order_by_mixed_asc_desc, query_order_by_then_take, query_take_n, query_skip_n, query_skip_then_take, query_inner_join, query_inner_join_indexed, query_left_join, query_left_join_indexed, query_count, query_sum, query_min, query_max, query_avg, query_group_then_order, query_filter_then_group, query_record_proj, query_tuple_proj, query_nested_where, query_async_select, query_async_where, query_async_compr, query_async_to_sync_filter, query_empty_source, query_single_element, query_let_in_select, query_filter_string_eq, query_filter_float_lt |

## Test set

- `TestPhase7Query`, walks all 40 fixtures with the standard gate stack.

## Deferred work

- Window functions (`row_number`, `rank`, `lag`, `lead`), deferred to Phase 8 or to a v1.5 query extension (no Mochi surface in v1).
- SQL-style `having` clause after `group_by`, deferred (no Mochi surface; the lowerer can be extended later by chaining a `where` after the group projection).
- `IQueryable`-equivalent deferred-execution surface (build-then-translate), out of scope; the Mochi pipeline executes eagerly via comprehensions.
- DuckDB / Arrow integration for very large datasets, deferred to v1.5 or v2 (Mochi `from x in dataset` over external sources is a Phase 13 / 14 concern).
