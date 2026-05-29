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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase31Lists`: 25 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: list literal construction with element-type inference, index, `len`, `for-each` iteration, Mochi `from x in xs select e(x)` style comprehensions, and the canonical list mutation surface (`append`, `extend`, slice assignment).

## Goal-alignment audit

Lists are the first compound collection and the substrate for the Query DSL (Phase 7). If `list[int]` lowers cleanly under `mypy --strict` and `pyright --strict`, every later collection phase inherits the same shape. If a list comprehension drifts from `[expr for x in xs]`, the Query DSL drift compounds. Phase 3.1 pins down the list lowering once.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.1.0 | List literal `[a, b, c]`, indexing `xs[i]`, typed as `list[T]` with element-type inference | NOT STARTED | — |
| 3.1.1 | `for-each` iteration (`for x in xs`) and `len(xs)` | NOT STARTED | — |
| 3.1.2 | Mochi `from x in xs select e(x)` comprehensions, lowered to Python list comprehensions | NOT STARTED | — |
| 3.1.3 | List operations: `append`, `extend`, slice get / set, `xs[a:b]` | NOT STARTED | — |

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

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | List literal, index, slice, `for-each`, `append`, `extend`, `len` lowering |
| `transpiler3/python/lower/comprehension.go` | Mochi `from / select` to Python list comprehension |
| `transpiler3/python/build/phase03_1_test.go` | `TestPhase31Lists`: 25 fixtures |
| `tests/transpiler3/python/fixtures/phase03-1-lists/` | 25 fixture directories: list_lit, list_index, list_index_neg, list_len, list_for_each, list_for_enum, list_compr, list_compr_nested, list_append, list_extend, list_slice, list_concat, list_str, list_bool, list_float, list_index_out_of_range, list_empty, list_empty_annot, list_compr_with_arith, list_for_each_print, list_method_chain, list_nested_lit, list_nested_for, list_assign_idx, list_select_one |

## Test set

- `TestPhase31Lists`, walks all 25 fixtures with the standard gate stack.

## Deferred work

- List comprehension with `where` clause and `group_by`, deferred to Phase 7 (Query DSL).
- `sorted(xs)` and `reversed(xs)`, deferred to Phase 7.2 (order_by).
- List flattening via `itertools.chain.from_iterable`, deferred to Phase 7.3 (joins).
- List-of-record fixtures, deferred to Phase 3.4.
