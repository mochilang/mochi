---
title: "Phase 7. Query DSL"
sidebar_position: 12
sidebar_label: "Phase 7. Query DSL"
description: "MEP-51 Phase 7, Mochi from / where / select / order_by asc / cross joins / inner joins / left joins lowered through the C arena-query desugaring, with sort_asc / sum_i64 / sum_f64 / str_from runtime helpers to avoid Python builtin shadowing."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 7](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (7.0, 7.2, 7.3); 7.1 group_by BLOCKED UPSTREAM; 7.4 async DEFERRED to Phase 10 |
| Started        | 2026-05-29 |
| Landed         | 2026-05-29 18:43 (GMT+7) |
| Tracking issue | TBA |
| Tracking PR    | TBA |

## Gate

`TestPhase7Query` (transpiler3/python/build/phase07_test.go) walks every `.mochi` under `tests/transpiler3/python/fixtures/phase07-query/` and asserts byte-equal stdout against vm3 (`mochi run`). 32 fixtures green on CPython 3.13.0 (host platform). Cross-OS + 3.12 + mypy / pyright / ruff strict gates roll up at Phase 16.

Fixture corpus mirrors the C transpiler's `tests/transpiler3/c/fixtures/query`, `query_join`, `arena_query`:

- 16 from / where / select / order_by asc / skip / take fixtures.
- 8 join fixtures (cross, inner, left).
- 8 arena_query fixtures (the same query shapes executed inside Mochi's arena/scoped allocation surface, which compiles down to the same IR on Python).

## Goal-alignment audit

The Query DSL is Mochi's primary data-wrangling surface. Phase 6 (closures + higher-order) and Phase 7 together unlock all functional-flavour code in the corpus. Phase 7's user-facing goal is "any Mochi `from / where / select` program runs on Python with byte-equal stdout". The lowerer reaches that goal by reusing the C-side desugaring (which already turns Mochi query expressions into for-each loops with conditional `append`) and bolting on a handful of Python-specific runtime helpers for the operations where Python's builtin namespace would otherwise collide with user `let` bindings.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `from`, `where`, `select` lowered via `QueryScopeStmt` inline splice + existing `ForEachStmt` / `IfStmt` / `AppendExpr` / `AssignStmt` paths | LANDED | this PR |
| 7.1 | `group_by k into g select { k: g.key, c: count(g) }` | BLOCKED UPSTREAM (C lowerer treats unquoted record literal `{k: v}` as a map literal, fails on undeclared variable `k`; see "Deferred work") | — |
| 7.2 | `order_by k asc` via `mochi_runtime.query.sort_asc`; `order_by k desc` already desugared by C lower into `sort_asc` over a reverse-comparable surrogate | LANDED | this PR |
| 7.3 | Cross / inner / left joins via multi-`Froms` and `Joins` inside `QueryScopeStmt` | LANDED | this PR |
| 7.4 | `async for` over `AsyncIterator` sources | DEFERRED to Phase 10 (no async surface in the v1 query corpus) | — |

## Sub-phase 7.0, from / where / select

### Goal-alignment audit (7.0)

The three-clause base case (`from`, `where`, `select`) covers more than half of the Query DSL corpus. The C lowerer already desugars it into the canonical form:

```text
from x in src where cond select expr
=>
let __queryN: list<T> = []
for x in src { if cond { __queryN = append(__queryN, expr) } }
__queryN
```

The C lowerer wraps that whole sequence in a `QueryScopeStmt` so that the arena-allocation pass can scope-allocate the temporary list. Python uses garbage collection, so the Python lowerer splices the `QueryScopeStmt.Body` inline.

### Decisions made (7.0)

**Inline splice in `lowerBlock`** (`transpiler3/python/lower/lower.go`):

```go
if q, ok := s.(*aotir.QueryScopeStmt); ok {
    body, err := l.lowerBlock(q.Body)
    if err != nil {
        return nil, fmt.Errorf("QueryScopeStmt: %w", err)
    }
    out = append(out, body...)
    continue
}
```

The `ResultVar`, `ArenaVar`, and `ElemType` fields are ignored; the body block contains a `LetStmt` for the result and the for/if/assign chain that populates it. Python doesn't need a scoped arena, so splicing is sound: the temp name (`__queryN`) is unique per query expression and the let binding goes through the normal `LetStmt` path.

**Why splice rather than emit a Python comprehension**: the C IR is already in statement form. Re-recognising "this `ForEachStmt` plus `IfStmt` plus `AssignStmt` is actually a comprehension" would require pattern-matching across multiple `aotir` nodes, would not handle multi-source `from x in xs from y in ys` cleanly, and would gain little (CPython compiles list comprehensions to roughly the same bytecode as the explicit for / append loop). Re-using the IR's statement form keeps the lowerer one-to-one with the C path.

**Where-clause placement**: the C lower already emits `IfStmt{Cond: cond, Then: appendBlock}` inside the for-each body. Python lowering of `IfStmt` (Phase 2) handles it. Multiple `where` clauses concatenate into nested `IfStmt`s on the C side; Python emits one `if ...:` per layer, which is byte-equal to a single conjoined `if a and b:` after CPython's peephole pass.

**Select-clause projection**: scalar projection lowers to the projected expression directly. Tuple projection lowers to a Python tuple literal via the existing `TupleLitExpr` path. Record projection lowers to a `Record(...)` constructor call via the existing `RecordLitExpr` path (Phase 4).

**Multi-source `from`** (cross product) lowers via `Froms[]` in the C IR. The C lower nests `ForEachStmt` accordingly; Python's existing for-each lowering handles the nesting without changes.

## Sub-phase 7.2, order_by

### Goal-alignment audit (7.2)

`order_by` is the second-most-common Query DSL clause. The C lowerer already canonicalises both `asc` and `desc` to a single `aotir.ListSortAscExpr` node by negating the key for descending. Python only needs to emit a call to a runtime helper that calls `sorted()`.

### Decisions made (7.2)

**Why a runtime helper rather than emitting `sorted(...)` directly**: Mochi's parser accepts `let sorted = from n in nums order by n select n`. That `let` rebinds `sorted` to the user's result. If the lowerer emits a bare `sorted(...)` call later in the same function, CPython treats `sorted` as a local variable name (because the function has a later assignment to it). The reference fails with `UnboundLocalError: cannot access local variable 'sorted' where it is not associated with a value`.

The fix is to route the call through `mochi_runtime.query.sort_asc`:

```python
from mochi_runtime.query import sort_asc

def main() -> None:
    nums: list[int] = [3, 1, 2]
    sorted_: list[int] = sort_asc(nums)  # name mangled if it collides
```

This is the canonical Mochi-on-Python pattern for any builtin that might be shadowed: define a thin runtime wrapper, emit the qualified call.

**Emitted helper** (`runtime/python/mochi_runtime/query.py`):

```python
from typing import TypeVar

T = TypeVar("T")


def sort_asc(xs: list[T]) -> list[T]:
    return sorted(xs)
```

The helper preserves Python's stable-sort semantics. Mochi's `order_by k1 asc, k2 asc` reaches a single `sort_asc` call over a list of tuple keys, so the helper does not need a separate multi-key variant.

**Why no `sort_desc` helper**: the C lowerer already desugars `order_by k desc` by negating the key (numeric) or by emitting a `ListSortAscExpr` over a `__neg_k` surrogate (non-numeric uses the same key with the result list reversed). Python sees only `ListSortAscExpr` and routes everything through `sort_asc`.

## Sub-phase 7.3, Joins

### Goal-alignment audit (7.3)

The C IR represents joins as additional `Froms[]` entries (cross join) or `Joins[]` entries (inner / left). The lowerer doesn't need a new IR shape; the join condition becomes another `IfStmt` inside the nested for-each loops.

### Decisions made (7.3)

**Cross join** (`from x in xs from y in ys select (x, y)`): the C lower emits two nested `ForEachStmt`s with the select expression inside the inner body. Python emits two nested `for ...:` blocks. The result tuple is built via the existing `TupleLitExpr` path.

**Inner join** (`from x in xs join y in ys on x.k == y.k select (x, y)`): equivalent to a cross join with an extra `IfStmt` on the join condition. The C lower normalises `on` clauses into `IfStmt` inside the inner body; Python lowering is unchanged from cross join.

**Left join** (`from x in xs left join y in ys on x.k == y.k select (x, y)`): the C lower emits a `__found = false` accumulator plus an inner for-each that toggles `__found` on match, plus a fallback `if !__found { append (x, nil) }`. Mochi `nil` lowers to Python `None`. The fixtures in `tests/transpiler3/c/fixtures/query_join/join_left_*` exercise this shape and reach byte-equal stdout on the Python path without further changes.

**Why no `itertools.product`**: the C IR already explicates the nested loops. Emitting `itertools.product` would require recognising the for-each-of-for-each shape and rewriting it into a single comprehension, which is gratuitous (CPython's nested `for` is no slower than `product`).

## Runtime helpers added in Phase 7

| File | Helper | Purpose |
|------|--------|---------|
| `runtime/python/mochi_runtime/query.py` | `sort_asc(xs)` | Routes `ListSortAscExpr` through a qualified `sorted` reference to survive user `let sorted = ...` shadowing |
| `runtime/python/mochi_runtime/query.py` | `sum_i64(xs)` | Routes `ListSumExpr` over `list[int]` through a qualified `sum` reference; integer-typed return |
| `runtime/python/mochi_runtime/query.py` | `sum_f64(xs)` | Routes `ListSumExpr` over `list[float]` through `sum(xs, 0.0)` so the float zero seed produces a float result even for empty lists |
| `runtime/python/mochi_runtime/fmt.py` | `str_from(value)` | Routes `StrConvertExpr` so that `str(true)` lowers to `"true"` (not Python's `"True"`) and `str(1.5e10)` matches `float_str` byte-for-byte |

Each helper is one obvious line of Python. The motivation is uniform: any Mochi builtin that the parser can rebind via `let` (or that has a Mochi-vs-Python semantic gap) goes through a qualified runtime call so the emit stays correct under shadowing.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | `QueryScopeStmt` inline splice; `ListSortAscExpr` → `sort_asc`; `ListSumExpr` → `sum_i64` / `sum_f64`; `StrConvertExpr` → `str_from`; `StrUpperExpr` → `.upper()`; `StrLowerExpr` → `.lower()`; `needsQuery` / `needsStrFrom` / `needsSumI64` / `needsSumF64` import flags |
| `transpiler3/python/build/build.go` | Cache marker bumped `mep51-phase06` → `mep51-phase07` |
| `transpiler3/python/build/phase07_test.go` | `TestPhase7Query`: walks all phase07-query fixtures, byte-equal vs vm3 |
| `runtime/python/mochi_runtime/query.py` | New: `sort_asc`, `sum_i64`, `sum_f64` helpers |
| `runtime/python/mochi_runtime/fmt.py` | Added `str_from(value)` for bool / float / object string conversion |
| `tests/transpiler3/python/fixtures/phase07-query/` | 32 fixtures: 16 from `tests/transpiler3/c/fixtures/query`, 8 from `query_join`, 8 from `arena_query` |

## Test set

- `TestPhase7Query`, 32 sub-tests, all green (17.7s wall clock on host).
- Full `go test ./transpiler3/python/...` green (106s) after this change, no Phase 1-6 regressions.

## Deferred work

- **Sub-phase 7.1 (`group_by`) BLOCKED UPSTREAM.** The C lowerer at `transpiler3/c/lower/lower.go:6314` calls `lowerExpr(q.Select)` on the group-by select expression. When the select is the canonical `{k: g.key, c: count(g)}`, the C lowerer parses `{k: ..., c: ...}` as a Mochi map literal (because there is no record-type annotation) and then fails with `map literal key 0: undeclared variable k`. The same input runs cleanly on vm3 (`mochi run tests/compiler/c/group_by.mochi` prints `1 2 / 2 1`). This is a C-lower / Mochi parser-typing gap, not a Python lowerer issue. Filing as part of the umbrella Phase 7.1 work to be picked up after the C-side fix lands. Once the upstream gap closes, the Python path needs only fixture coverage (it already lowers `MapPutStmt`, `MapKeysExpr`, `MapGetExpr`, and `QueryScopeStmt`).
- **Sub-phase 7.4 (async queries) DEFERRED to Phase 10.** The fixture corpus has no async query sources; Phase 10's stream surface lands the `AsyncIterator` shape first, then Phase 7.4 fixtures get added.
- **`itertools.groupby` / `attrgetter` / async comprehension** code paths described in earlier drafts of this doc are dropped. The actual lowering reuses the C IR's statement form rather than emitting Python-side comprehensions or itertools combinators. Re-introducing them is possible later as a pure post-lowering optimisation if profiling shows the explicit-loop form is too slow on large queries; not required for Phase 7's byte-equal gate.
- **Window functions, SQL `HAVING`, IQueryable-style deferred execution, DuckDB / Arrow integration**: out of scope for v1, same rationale as previous drafts.

## Build / cache

Cache marker bumped to `mep51-phase07`. Existing `~/.cache/mochi/python/*` entries from Phase 6 are invalidated by the marker change. No on-disk migration is needed; the next build refills the cache.
