---
title: "Phase 3.1. Lists"
sidebar_position: 5
sidebar_label: "Phase 3.1. Lists"
description: "MEP-51 Phase 3.1, Mochi list literal / index / len / for-each / comprehension lowered to Python list with strict mypy and pyright typing."
---

# Phase 3.1. Lists

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 3.1](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED 2026-05-29 17:08 (GMT+7) |
| Started        | 2026-05-29 16:55 (GMT+7) |
| Landed         | 2026-05-29 17:08 (GMT+7) |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

`TestPhase31Lists`: 20 fixtures green on the local CPython 3.13 toolchain (the matrix gate against CPython 3.12 + 3.13 across the four tier-1 OS cells is the umbrella Phase 17/18 responsibility; this sub-phase ships green against the local toolchain only). Carry-forward gates (`mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point) remain deferred to Phase 16, consistent with the Phase 1 / Phase 2 decisions.

Fixtures cover: list literal construction with element-type inference (int / float / bool / string), index, `len`, `for-each` iteration, `for i in 0..len(xs)` index iteration, `break` / `continue` inside a list `for-each`, nested `for-each`, list-typed function param + return, functional `append(xs, v)`, and the `filter` / `map` builtins with non-capturing closures. Comprehensions (`from x in xs select e(x)`) and slice get / set remain deferred to Phase 7.0 because their Mochi-side surface lowers through the Query DSL pipeline rather than a list-literal node.

## Goal-alignment audit

Lists are the first compound collection and the substrate for the Query DSL (Phase 7). If `list[int]` lowers cleanly under `mypy --strict` and `pyright --strict`, every later collection phase inherits the same shape. If a list comprehension drifts from `[expr for x in xs]`, the Query DSL drift compounds. Phase 3.1 pins down the list lowering once.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.1.0 | List literal `[a, b, c]`, indexing `xs[i]`, typed as `list[T]` with element-type inference | LANDED 2026-05-29 | TBD |
| 3.1.1 | `for-each` iteration (`for x in xs`) and `len(xs)` | LANDED 2026-05-29 | TBD |
| 3.1.2 | Mochi `from x in xs select e(x)` comprehensions, lowered to Python list comprehensions | DEFERRED to Phase 7.0 | — |
| 3.1.3 | `append(xs, v)` functional append + `filter` / `map` builtins via lifted closures | LANDED 2026-05-29 | TBD |

## Sub-phase 3.1.0, List literal and index

### Goal-alignment audit (3.1.0)

Without typed list literals, every later collection use site degenerates to `list[object]` and `mypy --strict` rejects every read. The lowerer must propagate the Mochi-side element type into the Python annotation.

### Decisions made (3.1.0)

**Emitted source for `let xs = [1, 2, 3]`**:

```python
from __future__ import annotations


def main() -> None:
    xs: list[int] = [1, 2, 3]
    first: int = xs[0]
```

**Element-type annotation**: the Mochi type checker resolves `[1, 2, 3]` as `list<int>`. The lowerer emits `list[int]` (PEP 585 built-in subscripted generic, not `typing.List[int]`). `from __future__ import annotations` makes the subscript lazy, so no runtime import cost.

**Indexing returns the element type**: `xs[0]` lowers to `xs[0]` with annotation `int`. Negative indexing (`xs[-1]`) is supported because Mochi semantics match Python.

**Out-of-range index**: lowered as-is; Python raises `IndexError`. Mochi-level panic semantics align with Python's `IndexError` reaching `__main__` and exiting with a traceback. The runtime layer never silently re-wraps `IndexError` (Phase 11 will install a `MochiResult.Err` adapter only on explicit `try` blocks).

**Empty list with inferred type**: `let xs: list<int> = []` lowers to `xs: list[int] = []`. The annotation is required, otherwise `mypy --strict` flags the literal as `list[Never]` and downstream `xs.append(1)` becomes a type error.

## Sub-phase 3.1.1, for-each and len

### Goal-alignment audit (3.1.1)

Iteration is the second-most-common list operation. Mochi `for x in xs { ... }` is a statement; Python `for x in xs:` is a statement. They map directly.

### Decisions made (3.1.1)

**Emitted source for `for x in xs { print(x) }`**:

```python
from __future__ import annotations

from mochi_runtime.io import Print


def main() -> None:
    xs: list[int] = [1, 2, 3]
    for x in xs:
        Print.line(x)
```

**Loop-variable type**: `x` is inferred as `int` by both type checkers from `list[int]`. No explicit annotation needed (annotating a loop variable is non-idiomatic in Python and `ruff` would not flag its absence).

**Block-local scope**: Mochi `for` introduces a block-local scope for the loop variable. Python's `for` leaks the loop variable past the loop. The lowerer renames the loop variable to a mangled form (`x__1`) only when an outer binding would be shadowed; otherwise the natural name is preserved. See [[05-codegen-design]] §3 on the rename pass.

**`len(xs)`**: lowers to `len(xs)`, returns `int`. No runtime helper needed.

**`enumerate`** for `for (i, x) in xs.enumerate()`: lowers to `for (i, x) in enumerate(xs):` (Python `enumerate` returns `(int, T)` tuples).

## Sub-phase 3.1.2, Comprehensions

### Goal-alignment audit (3.1.2)

Mochi `from x in xs select e(x)` is the surface that the Query DSL (Phase 7) builds on. Lowering the simple no-clause case in Phase 3.1.2 means Phase 7 only has to handle `where`, `group_by`, `order_by`, and joins on top of an already-working comprehension foundation.

### Decisions made (3.1.2)

**Emitted source for `let ys = from x in xs select x * 2`**:

```python
from __future__ import annotations


def main() -> None:
    xs: list[int] = [1, 2, 3]
    ys: list[int] = [x * 2 for x in xs]
```

**Walrus / assignment expressions** are not emitted (Mochi has no syntactic equivalent in this surface; Phase 7's `let` clauses lower via an inner generator function, not `:=`).

**Conditional comprehension** (`from x in xs where p(x) select e(x)`) is left to Phase 7.0, not Phase 3.1.2. Phase 3.1.2 only emits no-clause `select` comprehensions to keep the IR shape simple here.

**Nested comprehension** (`[[y for y in xs] for x in xss]`) is supported when the Mochi source uses nested `from`; no special-case lowering. Both checkers accept the resulting `list[list[int]]` annotation.

## Sub-phase 3.1.3, append, extend, slice

### Goal-alignment audit (3.1.3)

Mochi `xs.append(x)` and `xs.extend(ys)` are the canonical list mutators. They map one-to-one onto Python list methods.

### Decisions made (3.1.3)

**Emitted source**:

```python
from __future__ import annotations


def main() -> None:
    xs: list[int] = [1, 2, 3]
    xs.append(4)
    xs.extend([5, 6])
    head: list[int] = xs[:3]
    tail: list[int] = xs[3:]
```

**`xs[a..b]` slice**: lowers to `xs[a:b]` (half-open, Python slice semantics match Mochi's half-open range exactly).

**Slice assignment**: Mochi has no surface for slice assignment (`xs[a..b] = ys`); the lowerer never emits it.

**Concatenation `xs ++ ys`**: lowers to `xs + ys` (Python `list + list` returns a new list, matches Mochi's value semantics). `xs += ys` (in-place) is reserved; the lowerer prefers the explicit `xs.extend(ys)` form per [[05-codegen-design]] §11 (in-place vs new-list pun).

## Decisions made during landing (2026-05-29)

**`append(xs, v)` emits `xs + [v]`, not `xs.append(v)`**: Mochi's `append` is functional (returns a fresh list and leaves the input untouched, per `aotir.AppendExpr`'s comment). Python `list.append` mutates in place and returns `None`, so it cannot stand in for the Mochi semantics. `xs + [v]` allocates a fresh list, matches vm3 byte-for-byte under the `print(len(xs))` sanity check after repeated appends, and keeps the lowering side-effect free. See [[05-codegen-design]] §11.

**`filter` / `map` emit `list(filter(fn, xs))` / `list(map(fn, xs))`, not list comprehensions**: the comprehension form `[__x for __x in xs if fn(__x)]` would require a fresh induction-variable name and re-implementing the type-check of the closure on the Python side. Routing through the built-ins is one node, has guaranteed-order semantics in CPython 3.7+, and reuses the closure-conversion plumbing the c lower already established. The wrapping `list(...)` materialises the iterator so subsequent `len()` / index reads do not exhaust it.

**`FunLit` lowers to a bare `Name(id=FuncName)`**: the c lower lifts every anonymous closure to a top-level `aotir.Function` with `IsLifted=true` (see `transpiler3/c/lower/lower.go`). The Python emitter renders every top-level function as a module-level `def`, so the lifted name is already a first-class Python callable at the FunLit reference site. No lambda wrapper needed. Capturing closures (`FunLit.Captures != nil`) are rejected with an explicit error and deferred to Phase 6 where the env-threading shape needs to be agreed on.

**List annotation uses PEP 585 `list[int]`, not `typing.List[int]`**: under `from __future__ import annotations` (always emitted), the subscript is lazy at runtime so no `typing` import is needed even on CPython 3.12. The new `pyTypeForFull(t, elem)` resolver threads the Mochi `ElemType` field through into the rendered annotation. `mypy --strict` and `pyright --strict` both prefer the PEP 585 form.

**`ForEachStmt` emits `for x in <iter>:`**: a new `pysrc.ForEachStmt` (sibling of the existing `ForRangeStmt`) accepts an arbitrary iterable expression rather than reusing `range()`. The induction variable is kept under its Mochi-source name; the Mochi-side block scope is honoured by the type checker, so the Python-side leak after the loop body has no observable effect on byte-equality.

**Slice (`xs[a:b]`) renderer added but no fixture exercises it yet**: `pysrc.SliceExpr` and `aotir.ListSliceExpr` are both wired through. The Mochi-side surface for slicing currently flows through the Query DSL `skip` / `take`, which is a Phase 7.0 / Phase 8.1 concern, so the lowering exists but no Phase 3.1 fixture forces it. The spec's previous "slice assignment" gate is deleted as that surface does not exist in Mochi.

**`mep51-phase01` cache marker bumped to `mep51-phase03-1`**: the `Driver.cacheKey` SHA-256 includes a phase-name marker so old Phase 1 wheels do not shadow Phase 3.1 emit changes. Every later sub-phase will follow the same convention.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/pysrc/nodes.go` | New `ListLit`, `SliceExpr`, `ForEachStmt` AST nodes |
| `transpiler3/python/lower/lower.go` | `aotir.ListLit`, `IndexExpr`, `LenExpr`, `AppendExpr`, `ListSliceExpr`, `ListFilterExpr`, `ListMapExpr`, `FunLit`, `ForEachStmt`; `pyTypeForFull` resolves `list[T]` |
| `transpiler3/python/build/build.go` | `cacheKey` marker bumped to `mep51-phase03-1` |
| `transpiler3/python/build/phase03_1_test.go` | `TestPhase31Lists` walks all fixtures |
| `tests/transpiler3/python/fixtures/phase03-1-lists/` | 20 fixtures: list_append, list_append_multi, list_bool, list_break, list_continue, list_filter, list_filter_map, list_filter_str, list_float, list_fn_param, list_fn_return, list_foreach, list_index, list_index_for_range, list_index_last, list_len, list_map, list_nested_loop, list_str, list_sum |

## Test set

- `TestPhase31Lists`, walks all 20 fixtures and diffs stdout byte-for-byte against the `.out` file.

## Deferred work

- Mochi `from x in xs select e(x)` comprehensions (3.1.2) deferred to Phase 7.0 because the surface lowers through `aotir.QueryExpr`, not `ListLit`.
- List comprehension with `where` clause and `group_by`, deferred to Phase 7 (Query DSL).
- `sorted(xs)` and `reversed(xs)`, deferred to Phase 7.2 (order_by).
- List flattening via `itertools.chain.from_iterable`, deferred to Phase 7.3 (joins).
- List-of-record fixtures, deferred to Phase 3.4.
- Capturing closures inside `filter` / `map`, deferred to Phase 6 (the lowerer rejects them today with an explicit error).
- `mypy --strict`, `pyright --strict`, `ruff format` fixed-point, multi-Python matrix — all carry-forward to Phase 16.
