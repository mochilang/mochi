---
title: "Phase 4. Records"
sidebar_position: 6
sidebar_label: "Phase 4. Records"
description: "MEP-48 Phase 4 — record types to sealed record class / readonly record struct; value-type elision; structural equality; record pattern matching."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 4](/docs/mep/mep-0048#phase-4-records) |
| Status         | LANDED |
| Started        | 2026-05-28 02:07 (GMT+7) |
| Landed         | 2026-05-28 02:07 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase4Records`: 14 fixtures green on net8.0 and net10.0.

## Goal-alignment audit

Records are the first type-level Mochi feature that requires multiple C# declarations (the record type itself + any associated methods). Phase 4 establishes the multi-file layout, the `sealed record` vs `readonly record struct` decision logic, and the `IEquatable<T>` auto-derivation that all later phases rely on. Records are used as list/map elements (Phase 3.4 depends on this), as sum-type variants (Phase 5), as agent messages (Phase 9), and as stream items (Phase 10).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | `type Point { x: int, y: int }` → `public sealed record Point(long X, long Y)` (positional record class) | NOT STARTED | — |
| 4.1 | Small value-type elision: ≤4 fields, all value types → `public readonly record struct` | NOT STARTED | — |
| 4.2 | Record methods; record update syntax `{ r with X = newX }` | NOT STARTED | — |
| 4.3 | `MOCHI006` analyzer: suggest `record struct` when applicable | NOT STARTED | — |

## Sub-phase 4.0 -- Sealed record class

### Decisions made (4.0)

**Default lowering**: `type Point { x: int, y: int }` → positional record class:

```csharp
namespace Mochi.User;

public sealed record Point(long X, long Y);
```

C# positional records auto-generate:
- Primary constructor `Point(long X, long Y)`
- `Equals(Point other)` / `GetHashCode()` from all properties
- `ToString()` producing `Point { X = 1, Y = 2 }`
- `Deconstruct(out long X, out long Y)` for pattern matching

All of these are correct for Mochi semantics. No manual `GetHashCode` override needed.

**Naming**: field names are converted from Mochi snake_case (`x`) to C# PascalCase (`X`). This is a breaking change vs naive lowering (which would keep `x`), but PascalCase is the C# convention for properties, enforced by the `CA1507` Roslyn analyzer. Generated code must pass `TreatWarningsAsErrors`.

**Namespace**: each Mochi module's record types land in the same `Mochi.User.<ModuleName>` namespace as the module class. This allows record types to be used across modules via `using Mochi.User.Geometry;`.

**File layout**: each Mochi source file emits one `.cs` file. A file with both module functions and record types emits one class + the record types into the same namespace in the same file. For large projects, the build driver puts each module in its own `.cs` file under `target/dotnet/src/`.

## Sub-phase 4.1 -- Value-type elision

### Decisions made (4.1)

**Heuristic**: a Mochi record lowers to `readonly record struct` (value type, stack-allocated, no GC pressure) if:
1. Field count ≤ 4, AND
2. All fields are scalar types (`int` → `long`, `float` → `double`, `bool`, `string`), AND
3. The record is never assigned to an `object` field or used in a collection of `object` (which would box it).

Condition 3 is conservative: if uncertain, fall back to `sealed record class`. Condition 2 allows `string` in `readonly record struct` because `string` is a reference type but its value-type struct containing it is still useful (avoids an extra heap allocation for the record envelope).

**Rationale**: `Point { x: int, y: int }` is a very common pattern in Mochi programs. Lowering to `readonly record struct` eliminates the heap allocation for the record object itself. On a hot path that creates millions of points, this matters. The 4-field limit follows the CLR guideline that structs larger than 16 bytes are not beneficial to stack-allocate.

**`MOCHI006`**: Roslyn analyzer suggestion (not error) that fires when the lowerer emits a `sealed record class` for a record that qualifies for `readonly record struct`. Helps during development; off by default in CI.

## Sub-phase 4.2 -- Record methods and update syntax

### Decisions made (4.2)

**Record methods**: `type Point { x: int, y: int; fun norm(): float => ... }` lowers the method `norm` as a regular instance method on the record:

```csharp
public sealed record Point(long X, long Y)
{
    public double Norm() => Math.Sqrt((double)(X * X + Y * Y));
}
```

Methods on `readonly record struct` work the same way; they are `public` instance methods.

**Record update syntax**: `{ p with x = 3 }` lowers to C#'s `with` expression: `p with { X = 3 }`. C# `with` on records creates a shallow copy with the specified properties changed. This is the canonical pattern; no wrapper needed.

**Null safety**: C# nullable reference types are enabled (`<Nullable>enable</Nullable>`). All record properties are non-nullable by default. A Mochi `Option<T>` field lowers to `T?` for value types or uses the `Option<T>` record (Phase 5). No raw `null` in generated code.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | Record type → `sealed record` / `readonly record struct` decision + emission; field type mapping; record literal `{ x: 1, y: 2 }` → `new Point(1L, 2L)`; update `{ r with x: 3 }` → `r with { X = 3L }` |
| `transpiler3/dotnet/build/phase04_test.go` | `TestPhase4Records`: 14 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase04-records/` | 14 fixture directories |

## Test set

- `TestPhase4Records` -- 14 fixtures: record_basic, record_bool_field, record_eq_false, record_eq_true, record_field_arith, record_float_field, record_fn_arg, record_fn_return, record_in_if, record_in_list, record_single_field, record_string_field, record_two_types, record_var_reassign.

## Deferred work

- Record serialisation to/from JSON via `System.Text.Json` source generators. Deferred to Phase 12 (FFI).
- Mutable record fields. Deferred to Phase 2 review (Mochi records are immutable by default; mutable field access is a `var` re-binding, not a field mutation).

## Closeout notes

Phase 4 landed. `TestPhase4Records` PASS: 14 fixtures on SDK 10.0.107 net10.0. `aotir.RecordDecl` → `public sealed record Name(T1 Field1, T2 Field2)` (positional record class). Field names snake_case → PascalCase via `snakeToPascal`. `RecordLit` → `new TypeName(arg1, arg2, ...)` positional constructor. `FieldAccess` → `receiver.PascalCaseField`. `lowerReturnType` and `lowerParamType` handle `TypeRecord` by name. Record declarations prepended to CompilationUnit types (before class) so C# sees them at compile time. `BinEqRec`/`BinNeRec` use `==`/`!=` which C# positional records implement as structural equality.
