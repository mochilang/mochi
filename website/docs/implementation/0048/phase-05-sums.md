---
title: "Phase 5. Sum types and pattern matching"
sidebar_position: 7
sidebar_label: "Phase 5. Sum types and pattern matching"
description: "MEP-48 Phase 5 — sum types to abstract record + sealed record variants; match to C# 8+ switch expressions; MOCHI001 exhaustiveness; MOCHI002 dead arms."
---

# Phase 5. Sum types and pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 5](/docs/mep/mep-0048#phase-5-sum-types-and-pattern-matching) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase5Sums`: 25 fixtures green on net8.0 and net10.0. Analyzer-clean: `MOCHI001` (non-exhaustive match) and `MOCHI002` (unreachable arm) both active in CI as errors.

## Goal-alignment audit

Sum types are Mochi's primary abstraction over `Option<T>`, `Result<T,E>`, and user-defined variants. The sealed-record hierarchy + switch expression approach gives compile-time exhaustiveness (C# 8 pattern-match completeness analysis) and readable generated code. Phase 5 also ships the first two active `Mochi.Analyzers` rules; from this phase on, a Mochi programmer who adds a new variant to an ADT gets a compile error on all un-updated match sites.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | `type Shape = Circle(r: float) \| Rect(w: float, h: float)` → `abstract record Shape` + sealed variants | NOT STARTED | — |
| 5.1 | `match` → C# 8 switch expression with record-pattern deconstruction | NOT STARTED | — |
| 5.2 | `Option<T>` and `Result<T,E>` built-in sum types in `Mochi.Runtime.Types` | NOT STARTED | — |
| 5.3 | `MOCHI001` (non-exhaustive match) and `MOCHI002` (unreachable arm) analyzers activated | NOT STARTED | — |

## Sub-phase 5.0 -- Sealed record hierarchy

### Decisions made (5.0)

**Lowering**:

```csharp
// Mochi: type Shape = Circle(r: float) | Rect(w: float, h: float)

[MochiUnion]
public abstract record Shape;

public sealed record Circle(double R) : Shape;
public sealed record Rect(double W, double H) : Shape;
```

The `[MochiUnion]` attribute (defined in `Mochi.Runtime.Types`) marks the hierarchy as a Mochi-generated sum type. `Mochi.Analyzers` uses this attribute to identify switch expressions that must be exhaustive.

**`abstract record`**: C# 9+ sealed-abstract record hierarchies. The `abstract record Shape` cannot be instantiated directly. All variants are `sealed record` (no further subclassing). This gives Roslyn's pattern-match completeness analysis enough information to detect missing cases.

**`[MochiUnion]` attribute**: `[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]` applied to the abstract base. The analyzer reads this attribute to locate all permitted subtypes (via Roslyn symbol lookup). Without this attribute, Roslyn's completeness analysis only fires for `sealed` classes where the compiler can enumerate all subtypes — which requires the union to be in a single compilation unit. With `[MochiUnion]`, the analyzer can fire even across assemblies.

**Null-safety**: `abstract record Shape` is a reference type. In nullable-enabled code, a `Shape?` variable can be null; a `Shape` variable cannot. Generated match arms do not need null checks because the type system prevents `null` flow from Mochi code. The `_ => throw new MochiMatchExhaustivityError(value)` catch-all arm is still emitted as a runtime safety net.

## Sub-phase 5.1 -- Match to switch expression

### Decisions made (5.1)

**`match` → C# switch expression**:

```csharp
// Mochi:
// match shape {
//   Circle(r) => 3.14159 * r * r,
//   Rect(w, h) => w * h
// }

double area = shape switch {
    Circle { R: var r } => Math.PI * r * r,
    Rect { W: var w, H: var h } => w * h,
    _ => throw new MochiMatchExhaustivityError(shape)
};
```

Each Mochi pattern arm lowers to a C# switch arm with record-pattern deconstruction (`{ R: var r }`). Variable capture uses `var` binding in the pattern.

**Guard clauses**: `when condition` → `when (condition)` in the switch arm.

**Nested patterns**: `Circle(r) if r > 0.0` → `Circle { R: var r } when (r > 0.0)`.

**List patterns** (C# 11+, net8.0+): `match xs { [] => ..., [h, ..t] => ... }` → `switch (xs) { case [] => ..., case [var h, .. var t] => ... }`. C# 11 list patterns on `ImmutableList<T>` require the list to implement `IReadOnlyList<T>` (which `ImmutableList<T>` does) and the `Count` property.

**`MochiMatchExhaustivityError`**: runtime exception in `Mochi.Runtime.Errors`. Thrown by the catch-all `_ =>` arm. The `[MochiUnion]` + `MOCHI001` analyzer is the primary exhaustiveness gate; the `_ => throw` is the defence in depth for code that bypasses the analyzer (e.g., reflection-based construction).

## Sub-phase 5.2 -- Option&lt;T&gt; and Result&lt;T,E&gt;

### Decisions made (5.2)

**`Option<T>`**:

```csharp
[MochiUnion]
public abstract record Option<T>;
public sealed record Some<T>(T Value) : Option<T>;
public sealed record None<T> : Option<T>;
```

Fast-path for nullable value types: `Option<long>` can alternatively lower to `long?` (`Nullable<long>`) for performance. The lowerer uses `long?` when the option is never matched structurally (only `if opt != null`). When the option is used in a `match`, the full `Option<T>` hierarchy is emitted.

**`Result<T,E>`**:

```csharp
[MochiUnion]
public abstract record Result<T, E>;
public sealed record Ok<T, E>(T Value) : Result<T, E>;
public sealed record Err<T, E>(E Error) : Result<T, E>;
```

`Result.Ok` and `Result.Err` factory methods for ergonomic construction.

## Sub-phase 5.3 -- MOCHI001 and MOCHI002 analyzers

### Decisions made (5.3)

**MOCHI001** fires when a switch expression over a `[MochiUnion]` type is non-exhaustive (missing one or more variants). This is an *error* (not a warning). Generated code from the Mochi transpiler is always exhaustive (the transpiler emits the `_ => throw` catch-all), so MOCHI001 only fires on hand-written C# that extends Mochi types — which is discouraged but not forbidden.

**MOCHI002** fires when a switch arm is unreachable because a prior arm already covers the same pattern. This catches transpiler bugs where the lowerer emits duplicate arms.

Both diagnostics are implemented as Roslyn `DiagnosticAnalyzer` in `Mochi.Analyzers`. They are active in Phase 5 CI as errors (`<TreatWarningsAsErrors>true</TreatWarningsAsErrors>` + `<WarningsAsErrors>MOCHI001;MOCHI002</WarningsAsErrors>`).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/decl.go` | Sum type → abstract record + sealed records; `[MochiUnion]` attribute |
| `transpiler3/dotnet/lower/match.go` | match → switch expression with record-pattern deconstruction |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Types/Option.cs` | `Option<T>`, `Some<T>`, `None<T>` |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Types/Result.cs` | `Result<T,E>`, `Ok<T,E>`, `Err<T,E>` |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Types/MochiUnionAttribute.cs` | `[MochiUnion]` attribute definition |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Errors/MochiMatchExhaustivityError.cs` | Runtime catch-all exception |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Rules/MOCHI001.cs` | Non-exhaustive match diagnostic |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Rules/MOCHI002.cs` | Unreachable arm diagnostic |
| `transpiler3/dotnet/build/phase05_test.go` | `TestPhase5Sums`: 25 fixtures + analyzer-clean gate |
| `tests/transpiler3/dotnet/fixtures/phase05-sums/` | 25 fixture directories |

## Test set

- `TestPhase5Sums` -- 25 fixtures: circle/rect area, option some/none, result ok/err, nested match, guard match, list pattern empty, list pattern head/tail, exhaustive shape match, option chain, result unwrap, recursive sum type (tree), json-like ADT, match on string literal, match on int literal, match with when guard, nested option, option map/flatMap (implemented as match), result map/flatMap, multi-variant match, wildcard arm, sum type in list, sum type as record field, Option<string> fast path (nullable), sum type printed via ToString, match result in loop.
- `TestMOCHI001Fires` -- verify `MOCHI001` fires on a handwritten C# file missing a variant.
- `TestMOCHI002Fires` -- verify `MOCHI002` fires on a duplicate pattern arm.

## Deferred work

- Structural equality for deep sum-type trees. `ImmutableList<Option<T>>` structural equality requires combining `CollectionEq.ListEqual` with `Option<T>` equals. Deferred to Phase 5 implementation review.
- `[ModuleInitializer]` for sum-type variant registration (used by future reflection-based serialisation). Deferred to Phase 12.

## Closeout notes

Phase 5 not yet started.
