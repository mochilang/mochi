---
title: "Phase 4. Records"
sidebar_position: 9
sidebar_label: "Phase 4. Records"
description: "MEP-51 Phase 4, Mochi record types lowered to @dataclass(frozen=True, slots=True). Phase 4 of MEP-51 consolidates bare-record (non-list) coverage and documents upstream gaps in Mochi parser / MEP-45 aotir blocking with-update, nested records, and field defaults."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 4](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (consolidation only; 4.1 / 4.2 / 4.3 blocked upstream) |
| Started        | 2026-05-29 17:43 (GMT+7) |
| Landed         | 2026-05-29 17:46 (GMT+7) |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

`TestPhase4Records`: 20 fixtures green on CPython 3.12.0 in the worktree at `/tmp/mep51-p1`. Carry-forward gates (`mypy --strict`, `pyright --strict`, `ruff format` fixed-point, 3.12 + 3.13 matrix) deferred to Phase 16. Primary correctness gate is byte-equal stdout vs the AOT IR semantics encoded in `transpiler3/c/lower`.

Fixtures cover: single field per scalar type (`int` / `float` / `bool` / `string`), two-field records, mixed-field records, `let`-typed binding, literal field order independence (`lit_unordered`), field arithmetic, field-read inside `if` / `while`, var reassignment, equality / inequality on identical and divergent values, equality with string fields, record as function argument, record returned from function, two record types in one program.

## Goal-alignment audit

Phase 3.4 (list of records) already shipped every Python-side construct Phase 4.0 calls for: `@dataclass(frozen=True, slots=True)` declarations, `R(field=value)` keyword construction, `r.field` attribute access, `list[R]` annotations, and `==` / `!=` riding the auto-generated `__eq__` / `__hash__`. The 20 bare-record fixtures here pass with zero code changes on top of Phase 3.4.

The Phase 4 sub-phases that go beyond consolidation (4.1 `with`-update, 4.2 nested records + cross-module imports, 4.3 field defaults) require upstream work in the Mochi parser and MEP-45 AOT IR before MEP-51 can wire them. The c lower at `transpiler3/c/lower/lower.go` explicitly rejects nested record fields ("nested record fields are not supported in Phase 3.0") and the Mochi parser does not currently accept the `{ ...r, field: value }` spread syntax that would lower to `dataclasses.replace`. `print(record)` is also rejected at the c lower with "print() does not accept a record value in Phase 3.1 (access scalar fields instead)".

This phase lands the consolidation gate so future regressions in the Phase 3.4 plumbing surface here (separate test, separate failure mode), and the deferred-work section captures the upstream issues so they can be promoted as those upstreams advance.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | Bare-record fixtures: declaration, construction, field read, equality, function arg/return — every Phase 3.4 construct exercised on a non-list-element record | LANDED (rides Phase 3.4 plumbing) | this PR |
| 4.1 | `dataclasses.replace` for Mochi `{ ...r, x: 3 }` spread update | BLOCKED UPSTREAM | Mochi parser does not yet accept the spread syntax |
| 4.2 | Nested records (record-in-record) + cross-module imports | BLOCKED UPSTREAM | c lower rejects nested record fields in aotir Phase 3.0 |
| 4.3 | Field defaults via `field(default=...)` / `field(default_factory=...)` | BLOCKED UPSTREAM | aotir `RecordField` has no Default slot; Mochi parser does not accept `f: T = expr` in `type` blocks |

## Sub-phase 4.0, Bare record consolidation

### Goal-alignment audit (4.0)

Phase 3.4 emitted `@dataclass(frozen=True, slots=True)` classes for every record declared anywhere in the program. Phase 4.0 confirms that bare-record use sites (records not embedded in a list, not iterated, not field-accessed via index) all work: construction, field read, var reassignment, equality, function argument, function return. This is the rigorous "consolidate Phase 3.4 plumbing" gate.

### Decisions made (4.0)

**Emitted source for `type Pt { x: int, y: int }; let p = Pt { x: 1, y: 2 }; print(p.x)`** is identical to what Phase 3.4 emits (the lowerer doesn't distinguish list-element from bare records):

```python
from __future__ import annotations

from mochi_runtime.io import Print
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Pt:
    x: int
    y: int


def main() -> None:
    p: Pt = Pt(x=1, y=2)
    Print.line(p.x)


if __name__ == "__main__":
    main()
```

**Auto-derived methods** (`__init__`, `__eq__`, `__hash__`, `__repr__`, `__match_args__`) all come from `@dataclass(frozen=True, slots=True)` — no manual emission. The Mochi `==` / `!=` operators (`BinEqRec` / `BinNeRec`) wire directly to Python `==` / `!=`, locked in by `record_eq_same_true`, `record_eq_diff_false`, `record_eq_string_field`, `record_ne_true`, `record_ne_false`.

**`record_lit_unordered`** locks in that the c lower reorders Mochi-source field order to declared field order at the IR boundary, so `Pt { y: 2, x: 1 }` and `Pt { x: 1, y: 2 }` both lower to the same Python `Pt(x=1, y=2)` keyword call. Field-order drift in user source code does not affect emitted output.

**`record_let_typed`** locks in `let p: Pt = Pt { x: 1, y: 2 }` — explicit type annotation on the binding. The `pyTypeForRecord(TypeRecord, _, "Pt", "", _, _)` path emits `p: Pt = Pt(x=1, y=2)` with both sides annotated identically.

**`record_passed_to_function` / `record_returned_from_function`** lock in that `Function.Params[].RecordName` and `Function.ReturnRecordName` thread through `lowerFunction` into the emitted `def fn(p: Pt) -> Pt:` annotation.

## Sub-phase 4.1, with-update (BLOCKED UPSTREAM)

### Goal-alignment audit (4.1)

Mochi's intended `{ ...r, x: 3 }` spread update is the canonical immutable-record-update form. The Python target is `dataclasses.replace(r, x=3)`, which the stdlib already provides — no runtime helper needed.

### Blocker

The Mochi parser currently rejects `let p2 = Pt { ...p, x: 3 }`:

```
help:
  Check for a missing `{` or `}` to close the block.
```

The grammar does not include a spread arm inside record literals. Until the parser ships this surface, MEP-51 cannot lower it; the IR has no `RecordUpdate` node to map to `replace`.

### Forward plan

When Mochi grammar adds `{ ...r, field: value }` (or `{ r with field: value }`, whichever wins the surface discussion), MEP-45 aotir adds a `RecordUpdate` node carrying `Receiver Expr` + `Overrides []RecordLitArg`. MEP-51 then adds one handler:

```go
case *aotir.RecordUpdate:
    recv, _ := l.lowerExpr(v.Receiver)
    kwargs := []pysrc.KeywordArg{}
    for _, f := range v.Overrides {
        val, _ := l.lowerExpr(f.Value)
        kwargs = append(kwargs, pysrc.KeywordArg{Name: f.Name, Value: val})
    }
    l.needsReplace = true
    return &pysrc.Call{Func: &pysrc.Name{Id: "replace"}, Args: []pysrc.Expr{recv}, Kwargs: kwargs}, nil
```

Plus `from dataclasses import dataclass, replace` when `needsReplace`.

## Sub-phase 4.2, Nested records and cross-module imports (BLOCKED UPSTREAM)

### Goal-alignment audit (4.2)

Real programs nest records (`type User { id: int, addr: Addr }`) and split records across files. The Python target is straightforward: nested records are just nested type references on the dataclass field, and cross-module imports become `from .module import RecordName`.

### Blocker

The c lower (MEP-45 aotir) explicitly rejects nested record fields:

```
transpiler3/c/lower: type "User": field "addr": nested record fields are not supported in Phase 3.0
```

The aotir IR's verifier (see `transpiler3/c/aotir/verifier.go`) gates `TypeRecord` fields out of `RecordDecl.Fields` until MEP-45's own Phase 3.X advances. Mochi cross-module imports also require parser-level work (module resolution, import path syntax) that has not yet shipped.

### Forward plan

When MEP-45 lifts the nested-record gate, MEP-51's Python lowerer already handles them by accident: `lowerRecordDecl` walks `rec.Fields` and emits each as a `ClassField{Name, Type}` with `pyTypeForRecord` resolving record-typed fields via `f.RecordName`. The `from __future__ import annotations` mode means forward references inside the same module compile without explicit quoting.

Cross-module imports will need the lowerer to track which records were declared in which Mochi modules and emit `from .module import R` at the top of each consuming module. Until Mochi cross-module surface ships, this stays speculative.

## Sub-phase 4.3, Field defaults (BLOCKED UPSTREAM)

### Goal-alignment audit (4.3)

Mochi `type Config { retries: int = 3, tags: list<string> = [] }` is a common pattern. Python dataclass defaults need `field(default_factory=...)` for mutable values to avoid the well-known shared-default bug.

### Blocker

Mochi parser does not accept `f: T = expr` inside `type` blocks. aotir `RecordField` (at `transpiler3/c/aotir/program.go:136-140`) has no `Default Expr` slot.

### Forward plan

When Mochi grammar adds default-value syntax and aotir grows `RecordField.Default Expr` + `RecordField.DefaultIsMutable bool`, MEP-51's `lowerRecordDecl` extends to emit `field(default=<lit>)` for scalar defaults and `field(default_factory=<callable>)` for list/dict/set/record defaults. The Python lowerer also needs a `needsField` flag to add `field` to the `from dataclasses import dataclass, field` import.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/build.go` | Cache marker bumped to `mep51-phase04` |
| `transpiler3/python/build/phase04_test.go` | `TestPhase4Records`, walks fixture directory |
| `tests/transpiler3/python/fixtures/phase04-records/` | 20 fixtures (see Test set) |

No code changes to `lower.go` / `pysrc/nodes.go` / `emit/emit.go`. Phase 3.4 already shipped every Python construct Phase 4.0 needs; this phase is consolidation only.

## Test set

`TestPhase4Records` walks 20 fixtures (lifted from `tests/transpiler3/c/fixtures/records/`, which is the canonical aotir-Phase-3.0 records corpus):

| Fixture | What it locks in |
|---------|------------------|
| `record_bool_field` | Bool field declaration + construction + read + Print.line round-trip |
| `record_int_field` | Int field analog |
| `record_float_field` | Float field analog; touches Print.line(float) → Phase 2.1 fmt path |
| `record_string_field` | String field analog |
| `record_two_int_fields` | Multi-field record; both fields read |
| `record_mixed_fields` | Mixed types in one record (int, string, bool, float) |
| `record_let_typed` | Explicit type annotation on let binding: `let p: Pt = Pt{...}` |
| `record_lit_unordered` | Field-order independence: source order ≠ declared order, output matches declared order |
| `record_field_arith` | Arithmetic on record fields (`r.x + r.y`) |
| `record_field_in_if` | Field read inside an if-condition |
| `record_field_in_while` | Field read inside a while-condition and body |
| `record_var_reassign` | `var p: Pt = ...; p = ...`; record reassignment via fresh literal |
| `record_eq_same_true` | `Pt{x: 1, y: 2} == Pt{x: 1, y: 2}` is true (auto `__eq__`) |
| `record_eq_diff_false` | `Pt{x: 1, y: 2} == Pt{x: 1, y: 3}` is false |
| `record_eq_string_field` | Equality with string fields (locks in `str == str` not `is`) |
| `record_ne_true` | `!=` returns true when fields differ |
| `record_ne_false` | `!=` returns false when fields match |
| `record_passed_to_function` | `fun f(p: Pt): int` reads `p.x`; record param annotation via `RecordName` |
| `record_returned_from_function` | `fun mk(): Pt` returns record literal; `ReturnRecordName` annotation |
| `record_two_types` | Two record declarations in one program; each gets its own `@dataclass` |

## Deferred work

- **`{ ...r, field: value }` spread update** for Mochi-side record-with-update → Phase 4.1, blocked on Mochi parser; tracking issue to be opened against `mochilang/mochi`. Once unblocked, the MEP-51 lowering plan is a 10-line patch (see 4.1 § "Forward plan" above).
- **Nested records (record-in-record)** → Phase 4.2, blocked on MEP-45 aotir lifting the "nested record fields are not supported in Phase 3.0" gate at `transpiler3/c/lower/lower.go`. The Python emitter is already correct for nested fields once the upstream lets them through.
- **Cross-module imports** → Phase 4.2, blocked on Mochi parser module resolution. Speculative until that ships.
- **Field defaults** (`f: T = default`) → Phase 4.3, blocked on Mochi parser + aotir `RecordField.Default` slot.
- **`print(record)` via auto `__repr__`** → blocked on c lower's `print() does not accept a record value in Phase 3.1` gate. The Python emitter already has `__repr__` for free via `@dataclass`; the gate is upstream.
- **Record as map key, set element** → blocked on aotir verifier rejecting `KeyType=TypeRecord` (map) and `ElemType=TypeRecord` on `set` params/lets. Hashing is correct on the Python side (frozen dataclass auto-`__hash__`), so the gate is purely upstream.
- **`__match_args__` PEP 634 positional matching** → Phase 5 (sum-type match emission picks the strategy).
- **JSON serialisation via `dataclasses.asdict` + `json.dumps`** → Phase 12 (FFI surfaces JSON helpers).
- **`pydantic.BaseModel` adapter for FastAPI consumers** → v1.5 per MEP-51 §Open questions Q1.
- **Mutable record fields** (Mochi `var` field) → indefinite; Mochi records are immutable by spec.
- **mypy / pyright / ruff strict gates + 3.12 + 3.13 matrix** → Phase 16.
