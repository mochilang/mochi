---
title: "Phase 5. Sum types"
sidebar_position: 10
sidebar_label: "Phase 5. Sum types"
description: "MEP-51 Phase 5, Mochi sum types lowered to PEP 695 type aliases over @dataclass(frozen=True, slots=True) variants with PEP 634 keyword-pattern match. Generic (5.2) and recursive (5.3) sum types blocked at upstream Mochi parser / MEP-45 aotir."
---

# Phase 5. Sum types

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 5](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (5.0 + 5.1; 5.2 / 5.3 blocked upstream) |
| Started        | 2026-05-29 17:50 (GMT+7) |
| Landed         | 2026-05-29 18:00 (GMT+7) |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

`TestPhase5Sums`: 20 fixtures green on CPython 3.12.0 in the worktree at `/tmp/mep51-p1`. Carry-forward gates (`mypy --strict`, `pyright --strict`, `ruff format` fixed-point, 3.12 + 3.13 matrix) deferred to Phase 16. Primary correctness gate is byte-equal stdout vs the AOT IR semantics encoded in `transpiler3/c/lower`.

Fixtures cover: nullary single variant, single-field variants per scalar type (`int` / `float` / `bool` / `string`), multi-field variants (two and three fields), 5-variant nullary enum, two-variant `Result`-shaped union with mixed scalar fields, three-variant union with shared field name (`Pos`/`Neg`/`Zero`), wildcard arm, function returning a union, function consuming a union via match, match-in-return, match-in-let (expression position), match-in-statement (no result var), match nested inside `if`, two consecutive matches over the same value, two coexisting unions in one program, square-case dispatch.

## Goal-alignment audit

Sum types are Mochi's primary algebraic abstraction. `Option<T>`, `Result<T, E>`, and every user-defined ADT lower through this pipeline. Phase 5 enforces exhaustiveness at the type-checker layer rather than at runtime: the lowerer only emits `case _:` when the Mochi source contains an explicit `_ =>` arm. For sealed sum types without a wildcard, missing-variant matches surface under `mypy --strict` / `pyright --strict` (deferred to Phase 16) rather than at runtime, matching vm3's compile-time exhaustiveness check.

The Phase 5 sub-phases that go beyond the basic emission and match (5.2 generic sum types like `Option<T>`, 5.3 nested / recursive sum types) require upstream work in the Mochi parser and MEP-45 AOT IR before MEP-51 can wire them. `aotir.UnionDecl` has no type-parameter slot, `VariantField.UnionName` cannot recursively name the enclosing union (the verifier would reject `Branch{ left: Tree, right: Tree }` because `Tree` is not yet declared when the field is verified), and the Mochi parser does not yet accept `type Option<T> = Some<T> | None`. These constraints are inherited from c lower's Phase 3-4 scope (one monomorphic record/union surface, no generics).

The phase lands the basic emission gate so future regressions in the shared `lowerUnionDecl` / `lowerVariantLit` / `lowerMatchStmt` / `lowerVariantFieldAccess` / `pyTypeForUnion` path surface here, and the deferred-work section captures the upstream issues so they can be promoted as those upstreams advance.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | Basic sum type lowering: PEP 695 `type T = A | B` over frozen+slotted dataclass variants; nullary variants render as `pass` bodies | LANDED | this PR |
| 5.1 | PEP 634 `match` with keyword class patterns (`case Variant(field=bind):`), explicit wildcard arm (`case _:`) when the Mochi source uses `_`, and match-as-expression via a c-lower-introduced mutable result var with a `name: T` PEP 526 declaration | LANDED | this PR |
| 5.2 | Generic sum types: `type Option[T] = Some[T] | None_` with PEP 695 type parameter on the variant | BLOCKED UPSTREAM | Mochi parser does not yet accept `type T<...> = ...`; aotir `UnionDecl` has no type-parameter slot |
| 5.3 | Nested and recursive sum types: Tree node, JSON-shaped variants | BLOCKED UPSTREAM | aotir verifier rejects self-referential `VariantField.UnionName` (Phase 3-4 monomorphic surface) |

## Sub-phase 5.0, Basic sum type emission

### Goal-alignment audit (5.0)

A working basic sum type is the foundation; every later sub-phase adds richness on top. Without correct emission of the per-variant dataclasses plus the PEP 695 type alias, `mypy --strict` rejects the file outright at Phase 16.

### Decisions made (5.0)

**Emitted source for `type Shape = Circle(r: int) | Square(side: int)`**:

```python
from __future__ import annotations

from mochi_runtime.io import Print
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Circle:
    r: int


@dataclass(frozen=True, slots=True)
class Square:
    side: int


type Shape = Circle | Square
```

**PEP 695 `type` statement**: the canonical 3.12+ form. The `type` alias is lazily evaluated, so the variants can be referenced before their declaration (Phase 5.3 forward-reference scenario, still blocked upstream). Both mypy 1.13+ and pyright 1.1.380+ accept PEP 695 type aliases under `--strict`.

**Variant dataclass shape**: every variant is `@dataclass(frozen=True, slots=True)`. Nullary variants (no fields) declare a `pass` body, rendered by `pysrc.ClassDef` when the `Fields` slice is empty:

```python
@dataclass(frozen=True, slots=True)
class Zero:
    pass
```

**Constructor call shape**: variant literals lower as keyword-argument calls so dataclass field reordering cannot silently rebind positional arguments:

```python
Circle(r=5)
P(a=3, b=4)
Zero()
```

Nullary variants render with an empty argument list, matching the `case Zero():` pattern shape used by `match`.

**Variant declaration order**: variants are emitted in declaration order (matching the Mochi source); the `type T = A | B` alias follows the variants. Python's lazy alias evaluation means the order does not affect runtime semantics, but it keeps the emitted source readable.

**Equality across variants**: `Circle(r=1) == Square(side=1)` is `False` because they are different dataclass classes; `__eq__` checks `type(self) == type(other)` first. Same-variant equality follows the auto-generated `__eq__` over all fields.

## Sub-phase 5.1, PEP 634 match

### Goal-alignment audit (5.1)

`match` is the consumption side of sum types. Without it, the variant data is unreachable from the user's perspective. PEP 634 keyword class patterns interact cleanly with frozen-slots dataclasses and let both type checkers (Phase 16) infer the binding types inside each arm.

### Decisions made (5.1)

**Emitted source for `let area = match s { Circle(r) => r * r, Square(side) => side * side }`** (expression-position match):

```python
def main() -> None:
    s: Shape = Circle(r=5)
    __match1: int
    match s:
        case Circle(r=r):
            __match1 = (r * r)
        case Square(side=side):
            __match1 = (side * side)
    area: int = __match1
    Print.line(area)
```

The c lower introduces a fresh `__matchN` mutable temp via a `LetStmt{Init: nil, Mutable: true}` immediately before the `MatchStmt`; every arm body ends with `__matchN = <expr>`. The Python lowerer emits the `Init==nil` declaration as a PEP 526 annotation-only statement (`__match1: int`), then every match arm assigns into it before the parent `let area: int = __match1` reads it. This makes the bind-site unambiguous to both type checkers.

**Why keyword pattern (`Circle(r=r)`) not positional (`Circle(r)`)**: dataclass auto-generates `__match_args__`, so positional matching would also work. Keyword matching is robust to field reordering, more readable, and lets both checkers infer the bound variable type more reliably under `--strict`. The lowerer always emits keyword patterns.

**Statement-position match**: when the Mochi source uses `match` as a statement (no `let x = match …`), the c lower passes empty `ResultVar` and each arm body lowers the result expression as a side-effecting statement. The Python lowerer emits a bare `match` with no surrounding declaration:

```python
def main() -> None:
    r: Result = Ok(value=42)
    match r:
        case Ok(value=v):
            Print.line(v)
        case Err(msg=m):
            Print.line(m)
```

**Wildcard arm**: when the Mochi source contains `_ =>`, the c lower populates `MatchStmt.Default`. The Python lowerer emits a trailing `case _:`. When the source omits `_`, no `case _:` is emitted — under Phase 16's `mypy --strict` and `pyright --strict`, a missing variant arm is then flagged as a non-exhaustive match. This is the mechanism by which Mochi's compile-time exhaustiveness lifts into the Python type checker.

```python
match d:
    case North():
        __match1 = "up"
    case South():
        __match1 = "down"
    case _:
        __match1 = "sideways"
```

**Guard clauses**: a Mochi arm `Pattern when expr =>` populates `MatchArm.Guard`; the Python lowerer emits `case Pattern() if expr:`. Both checkers accept PEP 634 `if` guards.

**Type narrowing inside the arm**: inside `case Circle(r=r):`, the bound `r` is typed as `int` by both mypy and pyright via the dataclass field annotation, with no extra hints from the lowerer.

## Sub-phase 5.2, Generic sum types (BLOCKED UPSTREAM)

### Why blocked

`aotir.UnionDecl` has no type-parameter slot (no `TypeParams []string` field, no `aotir.TypeVar`); the Mochi parser does not accept `type Option<T> = Some<T> | None`; and the c lower's `Param.Type` is monomorphic. The Phase 5.0 surface only supports concrete unions over scalars / records, so a generic `Option[T]` cannot survive the type-check pass and `clower.Lower` would reject it before the Python lowerer ever runs.

### Forward plan (when Mochi parser + aotir lift the gate)

The Python side becomes straightforward. Once aotir surfaces `UnionDecl.TypeParams []string` and `VariantField.TypeVarName`, `lowerUnionDecl` extends to:

```go
func lowerUnionDecl(u *aotir.UnionDecl) []pysrc.Stmt {
    out := make([]pysrc.Stmt, 0, len(u.Variants)+1)
    names := make([]string, 0, len(u.Variants))
    for _, v := range u.Variants {
        cls := &pysrc.ClassDef{
            Name:       v.Name,
            TypeParams: u.TypeParams, // NEW: PEP 695 [T] on the class
            Decorators: []string{"dataclass(frozen=True, slots=True)"},
            Fields:     lowerVariantFields(v.Fields),
        }
        out = append(out, cls)
        names = append(names, applyTypeParams(v.Name, u.TypeParams))
    }
    out = append(out, &pysrc.UnionDef{
        Name:       u.Name,
        TypeParams: u.TypeParams, // NEW
        Variants:   names,
    })
    return out
}
```

Targeted emit shape for `type Option<T> = Some<T> | None`:

```python
@dataclass(frozen=True, slots=True)
class Some[T]:
    value: T


@dataclass(frozen=True, slots=True)
class None_:
    pass


type Option[T] = Some[T] | None_
```

The trailing-underscore mangling on `None_` aligns with MEP-51 §3 reserved-word handling. The PEP 695 `[T]` on both the variant class and the alias replaces `typing.Generic` + `typing.TypeVar`; Mochi-emitted code never uses the legacy `typing.TypeVar` form.

`MochiResult[T, E]` (runtime-supplied, future Phase 11) follows the same shape:

```python
@dataclass(frozen=True, slots=True)
class Ok[T]:
    value: T


@dataclass(frozen=True, slots=True)
class Err[E]:
    error: E


type MochiResult[T, E] = Ok[T] | Err[E]
```

## Sub-phase 5.3, Nested and recursive sum types (BLOCKED UPSTREAM)

### Why blocked

`aotir.VariantField` carries `UnionName` for nested unions, but the verifier rejects self-referential `UnionName` because the union is not yet declared when its field is verified. The Phase 3-4 monomorphic surface in c lower has not been extended to support forward references, and Mochi-parser-level recursive `type Tree<T> = Leaf<T> | Branch{ left: Tree<T>, right: Tree<T> }` is not yet accepted (it requires the generic-sum-type surface from 5.2 plus a forward-reference relaxation in the verifier).

### Forward plan

Once the c lower's verifier accepts forward-reference `UnionName` and `RecordName` slots inside `VariantField`, the Python lowerer needs no additional wiring: `pyTypeForUnion(TypeUnion, ..., unionName, ...)` already returns the bare alias name and `from __future__ import annotations` (always emitted) defers resolution to the type checker. The forward-plan emit for `type Tree<T> = Leaf<T> | Branch<T>` where `Branch{ left: Tree<T>, right: Tree<T> }`:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Leaf[T]:
    value: T


@dataclass(frozen=True, slots=True)
class Branch[T]:
    left: Tree[T]
    right: Tree[T]


type Tree[T] = Leaf[T] | Branch[T]
```

The recursive forward reference `Tree[T]` inside `Branch[T].left` resolves lazily under the future annotations import; both mypy and pyright accept the recursive alias under `--strict`. A nested non-generic JSON-shaped variant is the same pattern without `[T]`.

Recursive consumption (`def sum_tree(t: Tree[int]) -> int:`) reuses the existing `lowerMatchStmt` plus `lowerVariantFieldAccess` paths without changes:

```python
def sum_tree(t: Tree[int]) -> int:
    match t:
        case Leaf(value=v):
            return v
        case Branch(left=l, right=r):
            return sum_tree(l) + sum_tree(r)
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/pysrc/nodes.go` | Added `UnionDef` (PEP 695 type alias), `MatchStmt` + `MatchCase` + `FieldBinding` (PEP 634 match with keyword class patterns), `AnnotateStmt` (declaration-only PEP 526 annotation for `LetStmt{Init: nil}` produced by the c lower's match-expression rewrite). |
| `transpiler3/python/lower/lower.go` | Added `lowerUnionDecl` iterating `prog.Unions` to emit per-variant `@dataclass(frozen=True, slots=True)` + `type Name = V1 | V2`; added `lowerMatchStmt` lowering aotir `MatchStmt` to PEP 634 with explicit `case _:` only when the source has `_`; added `lowerVariantLit` (keyword constructor) and `*aotir.UnionVarRef` / `*aotir.VariantFieldAccess` cases; added `pyTypeForUnion` (extends `pyTypeForRecord` with union-name slots and `list[UnionName]`); threaded `UnionName` / `ReturnUnionName` / `ElemUnionName` through `lowerLetStmt`, `lowerFunction`. `LetStmt{Init: nil}` now lowers to `AnnotateStmt` instead of crashing in `lowerExpr(nil)`. |
| `transpiler3/python/build/build.go` | Cache marker `mep51-phase04` → `mep51-phase05` so prior cached entries do not satisfy the new gate. |
| `transpiler3/python/build/phase05_test.go` | `TestPhase5Sums`: 20 fixtures. |
| `tests/transpiler3/python/fixtures/phase05-sums/` | 20 fixtures: 14 carried forward from `tests/transpiler3/c/fixtures/sum_types/` + 6 new (`sum_multi_field_variant`, `sum_three_field_variant`, `sum_five_nullary_variants`, `sum_match_in_if`, `sum_two_consecutive_matches`, `sum_function_returning_via_match`). |

## Test set

- `TestPhase5Sums` walks all 20 fixtures with the standard `runPythonFixture` gate (byte-equal stdout vs `.out`).
- `mypy --strict` / `pyright --strict` exhaustiveness assertion deferred to Phase 16, alongside the rest of the strict-mode matrix.

## Deferred work

- **Phase 5.2 generic sum types** — blocked at Mochi parser + aotir, see §5.2 forward plan.
- **Phase 5.3 nested + recursive sum types** — blocked at aotir verifier, see §5.3 forward plan.
- **Block-style match arms in expression position** — the c lower's `lowerMatchBodyWithScope` (`transpiler3/c/lower/lower.go:7060`) explicitly does not auto-assign `ResultVar` for `Pattern => { stmts }` arms; this lifts when the c lower threads the last-expression value through the block. Until then, expression-position matches must use `Pattern => expr` form.
- **`match` over Python primitive types** (int literal patterns, string literal patterns) — deferred to a future phase once Mochi syntax extends past variant patterns.
- **`MochiResult` adapter for boundary FFI** — deferred to Phase 11 (async colouring + MochiResult).
- **`case _:` exhaustiveness diagnostic gate** — the gate (no `case _:` emitted → checker flags missing variant) is wired structurally in Phase 5.1, but the actual `mypy --strict` / `pyright --strict` enforcement that drives that gate is part of Phase 16's reproducible-build pass.
