---
title: "Phase 3.3. Sets"
sidebar_position: 7
sidebar_label: "Phase 3.3. Sets"
description: "MEP-51 Phase 3.3, Mochi set lowered to Python builtin set with deterministic sorted iteration via the c-lower SetToListExpr rewrite, producing vm3-byte-equal stdout without a runtime wrapper."
---

# Phase 3.3. Sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 3.3](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 17:10 (GMT+7) |
| Landed         | 2026-05-29 17:23 (GMT+7) |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

`TestPhase33Sets`: 11 fixtures green on CPython 3.12.0 in the worktree at `/tmp/mep51-p1`. Carry-forward gates (`mypy --strict`, `pyright --strict`, `ruff format` fixed-point, 3.12 + 3.13 matrix) deferred to Phase 16. Primary correctness gate is byte-equal stdout vs the AOT IR semantics encoded in `transpiler3/c/lower`.

Fixtures cover: set construction, dedup on construction, `add`, `has`, `len`, iteration in deterministic (sorted) order, derived sets via `for` + `add`, set as collector across a list, mixed-type element fixtures (int / string / bool / float).

## Goal-alignment audit

Mochi `set<T>` semantics require deterministic iteration order across implementations so vm3 and Python produce byte-equal stdout. Python's builtin `set` is hash-randomised across processes (PYTHONHASHSEED), which would diverge from vm3 on every iteration-sensitive fixture.

The chosen approach: **defer determinism to the c lower**, not to a Python runtime wrapper. The c lower in `transpiler3/c/lower/lower.go:2205` already rewrites `for x in <setExpr>` to `for x in SetToListExpr(<setExpr>)`. Python's `SetToListExpr` handler emits `sorted(s)`. Net effect: every set iteration that reaches the Python emitter is already wrapped in a sort, so we can ship the builtin `set` without a wrapper.

This trades a Python-side abstraction (an `OrderedSet` class) for a one-line rewrite in the c lower that benefits every backend. The Mochi reference specifies sorted ascending iteration for `keys(m)` / `values(m)` / set foreach, so the c-lower wrap is the canonical place for the rewrite.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.3.0 | Set literal lowering: `SetLiteralExpr` → `{e1, e2, ...}` (empty → `set()`) | LANDED | this PR |
| 3.3.1 | `add`, `has`, `len`, `to_list` runtime ops + `for x in s` deterministic iteration | LANDED | this PR |

There is no sub-phase 3.3.2 for set operators (`\|`, `&`, `-`). Mochi v1 surface exposes `add(s, x)` only; binary set algebra is not currently in the language. If MEP-XX later adds it, a follow-up sub-phase will wire `__or__` / `__and__` / `__sub__` calls.

## Sub-phase 3.3.0, Set literal lowering

### Goal-alignment audit (3.3.0)

Every set fixture depends on being able to construct a set literal. Without this sub-phase, nothing else runs.

### Decisions made (3.3.0)

**Emitted source for `var s: set<int> = set{1, 2, 3}; print(len(s))`**:

```python
from __future__ import annotations


def main() -> None:
    s: set[int] = {1, 2, 3}
    print(len(s))
```

**Empty set literal**: Mochi `set{}` lowers to `set()`, **not** `{}`. Python `{}` is the empty dict, not the empty set. The lowerer detects `len(SetLiteralExpr.Elems) == 0` and emits a `Call(Name("set"))`.

**Element ordering inside the literal**: preserved from the AOT IR `SetLiteralExpr.Elems` slice. Python set literal `{1, 2, 3}` does not guarantee internal order, but iteration is always wrapped in `sorted()` (see 3.3.1) so this is invisible to stdout.

**Type annotation**: `set[T]` via PEP 585 generic subscription under `from __future__ import annotations`. No `typing.Set` import. The `pyTypeForCompound` path renders `TypeSet` → `set[<elem>]` via the same fold used for lists and maps.

**Duplicate elements at construction**: `set{1, 2, 1}` lowers to `{1, 2, 1}` which Python dedupes at literal evaluation. `set_len.mochi` and `set_dedup.mochi` lock in this behaviour: a 3-element literal with one duplicate yields `len(s) == 2`.

## Sub-phase 3.3.1, Operations + iteration

### Goal-alignment audit (3.3.1)

These are the four runtime operations Mochi exposes on `set<T>` in v1: `add`, `has`, `len`, foreach. Without them, the set literal is inert.

### Decisions made (3.3.1)

**Emitted source for `var s: set<int> = set{1}; s = add(s, 2); print(has(s, 2)); print(len(s)); for x in s { print(x) }`**:

```python
from __future__ import annotations


def main() -> None:
    s: set[int] = {1}
    s = s | {2}
    print((2 in s))
    print(len(s))
    for x in sorted(s):
        print(x)
```

**`add(s, x)` is functional, not in-place**. Mochi's `add` returns a new set; mutating callers reassign. The lowerer emits `s | {x}` (union with a singleton) rather than `s.add(x)`. This mirrors how list `append(xs, v)` lowers to `xs + [v]` rather than `xs.append(v)` in Phase 3.1, and keeps the IR functional all the way down to Python.

**`has(s, x)` lowers to `x in s`**. The Mochi expression `has(s, x)` is parsed as `SetHasExpr{Receiver: s, Elem: x}` by the c lower; Python emits `Compare(left=x, op=In, right=s)`.

**`len(s)` lowers to `len(s)`**. Python's builtin `len()` reads `set.__len__()` in O(1).

**`for x in s` lowers to `for x in sorted(s)`** transparently, because the c lower rewrites `ForEachStmt{List: <setExpr>}` into `ForEachStmt{List: SetToListExpr(<setExpr>)}` at `transpiler3/c/lower/lower.go:2205`. The Python emitter sees a `SetToListExpr` in the iterator slot and emits `sorted(<setExpr>)`. No special-case in the Python lowerer is required.

**`to_list(s)` (if Mochi user code calls it explicitly) lowers to `sorted(s)`**. Locks in deterministic stdout across all fixtures.

**vm3 byte-equality on bool fixtures**. `set_bools.mochi` prints `len(s)` and two `has(s, x)` results. Python `True`/`False` would lowercase to `true`/`false` via the Phase 2 `Print` shim. Set membership on bools also gates the subtle Python identity `True == 1, False == 0`: the fixture constructs `set{true, false, true}` and asserts `len == 2`, which matches Python set deduping on equality not identity. vm3 keeps bool and int in separate type universes; we never mix the two in a single set, so this corner is safe in Phase 3.3.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/pysrc/nodes.go` | `SetLit` node (renders `{e1, e2, ...}` with deterministic element ordering) |
| `transpiler3/python/lower/lower.go` | Handlers for `SetLiteralExpr` (empty → `set()`), `SetAddExpr` (`s \| {x}`), `SetHasExpr` (`x in s`), `SetLenExpr` (`len(s)`), `SetToListExpr` (`sorted(s)`); `pyTypeForCompound` extended with `TypeSet` → `set[<elem>]` |
| `transpiler3/python/build/build.go` | Cache marker bumped to `mep51-phase03-3` |
| `transpiler3/python/build/phase03_3_test.go` | `TestPhase33Sets`, walks fixture directory |
| `tests/transpiler3/python/fixtures/phase03-3-sets/` | 11 fixtures (see below) |

## Test set

`TestPhase33Sets` walks 11 fixtures:

| Fixture | What it locks in |
|---------|------------------|
| `set_add_has` | `add` returns a new set, `has` after add reports `true`, miss reports `false`, `len` after add is N+1 |
| `set_add_chain` | Repeated `add` calls; redundant `add(s, 2)` does not grow `len`; verifies functional `add` is associative under reassignment |
| `set_basic` | `has` on initial literal: hit, miss |
| `set_bools` | `set<bool>`: `{true, false, true}` dedupes to size 2; `has(true)` and `has(false)` both true |
| `set_count_unique` | Set as collector across a `for` over a list with duplicates; final `len` + sorted iteration |
| `set_dedup` | `set{1, 2, 1, 3, 2, 1}` → `len == 3`, iteration order `1, 2, 3` |
| `set_floats` | `set<float>`: iteration over `{1.5, 2.5, 3.5}` produces sorted ascending output |
| `set_for_each` | Sum reduction over `for x in s` produces the same total regardless of literal element order |
| `set_iterate_sorted` | `set{3, 1, 2}` iterates as `1, 2, 3` — the canonical determinism guarantee |
| `set_len` | `set{1, 2, 1}` → `len == 2`, then `add(3)` → `len == 3` |
| `set_strings` | `set<string>`: `has` hit + miss on string elements |

## Deferred work

- **Set-typed function parameters and return types** (`fun f(s: set<int>): set<int>`). Currently blocked by upstream limitations in MEP-45 `aotir`: `Param` and the `Return*` fields on `Function` have no `SetElemType` slot, so the c lower cannot propagate the set element type across the IR boundary. The verifier rejects `pr.ElemType != TypeInvalid` on non-list params. Two fixtures (`set_fn_param`, `set_fn_return`) were written, hit this gap, and were dropped from Phase 3.3. To be opened against MEP-45 as a tracking issue and revisited in MEP-51 Phase 6 (closures + higher-order) when the same fix unblocks generic collection params.
- **Set algebra operators** `\|`, `&`, `-`, `^`. Not in Mochi v1 surface; revisit if a future MEP adds them.
- **Set comprehensions** (`{f(x) for x in xs if pred(x)}`). Mochi's surface is `for` + `add` (see `set_count_unique`); no comprehension form. Revisit alongside list comprehensions if either lands.
- **`frozenset`-equivalent immutable set**. Mochi `let` binding is the immutability gate; no separate type needed.
- **Set-of-record fixtures**. Deferred to Phase 4 (records introduce hashable frozen dataclasses).
- **mypy / pyright / ruff strict gates and the 3.12 + 3.13 matrix**. Deferred to Phase 16 (reproducible build).
