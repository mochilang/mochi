---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives and control flow"
description: "MEP-48 Phase 2 — int/float/bool/string arithmetic, comparisons, if/else, while, for-in, let/var, user functions. 20 fixtures, Roslyn-clean."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 2](/docs/mep/mep-0048#phase-2-primitives-and-control-flow) |
| Status         | LANDED |
| Started        | 2026-05-28 01:44 (GMT+7) |
| Landed         | 2026-05-28 01:44 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase2Scalars`: 20 fixtures green on net8.0 and net10.0, all four tier-1 OS cells. Roslyn-clean secondary gate (zero warnings with `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`).

## Goal-alignment audit

Phase 2 closes the gap between "hello world" and "a real program". After Phase 2, every arithmetic expression, comparison, boolean combinator, conditional branch, loop, and user-defined function lowers correctly. This is the foundation all later phases build on: records and sums (Phases 4-5) require correct expression lowering; closures (Phase 6) require correct `let`/`var`; agents (Phase 9) require correct function call conventions.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.0 | `int` / `float` / `bool` arithmetic; comparisons; `let` / `var` | NOT STARTED | — |
| 2.1 | `if` / `else` / `while` / `return` / `break` / `continue` | NOT STARTED | — |
| 2.2 | `for x in xs`; user-defined functions; tail-call rewrite to loop | NOT STARTED | — |
| 2.3 | Integer divide-by-zero: `MOCHI_ERR_DIVZERO` → `MochiDivideByZeroError` | NOT STARTED | — |
| 2.4 | Float `NaN` / `Infinity` formatting match vm3; `int_cast` / `float_cast` | NOT STARTED | — |

## Sub-phase 2.0 -- Arithmetic, comparisons, let/var

### Decisions made (2.0)

**Type lowering rules (normative)**:

| Mochi type | C# type | Notes |
|-----------|---------|-------|
| `int` | `long` | NEVER `int`. Literal `42` emitted as `42L`. |
| `float` | `double` | Literal `3.14` emitted as `3.14`. |
| `bool` | `bool` | Literals `true` / `false`. |
| `string` | `string` | UTF-16 internally; UTF-8 at boundaries. |

**MOCHI003 analyzer**: planned for `Mochi.Analyzers`; would fire if a Mochi `int`-typed expression is lowered to C# `int` instead of `long`. `Mochi.Analyzers` is not yet created; MOCHI003 is deferred. The `long` invariant is enforced by convention and by `TreatWarningsAsErrors=true` on generated code.

**`let` binding**: `let x = expr` lowers to `var x = expr;` (C# implicitly typed). Mochi `let` is single-assignment; C# `var` does not enforce this statically, but Mochi's type checker ensures there is no re-assignment, so the generated code is always single-write.

**`var` binding**: Mochi `var x = expr` allows re-assignment; lowers to `var x = expr;` followed by `x = newExpr;` for re-assignments. C# `var` locals are mutable by default.

**Arithmetic operators**: `+`, `-`, `*`, `/`, `%` map directly. `/` on `long` is integer division (C# truncates toward zero), matching vm3.

**Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=` map directly for scalars. For records/lists, equality comparison routes through generated `IEquatable<T>` (from C# positional records) or `Mochi.Runtime.Eq.Equals(a, b)` helper. Phase 2 only covers scalar comparisons.

**Boolean operators**: `&&`, `||`, `!` map directly. Short-circuit semantics preserved (C# `&&` / `||` are short-circuit by definition).

**String concatenation**: `a + b` where both operands are `string` lowers to `string.Concat(a, b)` (not `a + b`, to avoid Roslyn CS0414 on nullable analysis edge cases). For mixed int-to-string: `print(42 + " apples")` lowers to `string.Concat(42L.ToString(), " apples")`.

## Sub-phase 2.1 -- Control flow

### Decisions made (2.1)

**`if/else`**: `aotir.IfStmt` with an else branch → `csharpsrc.IfStmt`. Without else → `if (cond) { ... }`. C# does not require braces, but the emitter always emits braces to avoid dangling-else parsing ambiguity in generated code.

**`while`**: maps directly.

**`return`**: `aotir.ReturnStmt` → `csharpsrc.ReturnStmt`.

**`break` / `continue`**: map directly to C# `break` / `continue`.

**Block as expression**: Mochi allows `let x = if (c) { ... e1 } else { ... e2 }`. In C#, `if` is a statement; this lowers to an immediately-invoked lambda: `var x = ((Func<long>)(() => { if (c) { ...; return e1; } else { ...; return e2; } }))();`. The wrapping `Func<T>` is determined from the type of the block. This pattern is used only when a Mochi `if` is used in an expression position; statement-position `if` maps to a plain `if` statement.

## Sub-phase 2.2 -- for-in, user functions, tail calls

### Decisions made (2.2)

**`for x in xs`**: `aotir.ForInStmt` → `csharpsrc.ForeachStmt`. For built-in sequences (lists, ranges), the `IEnumerable<T>` interface is already implemented by `ImmutableList<T>` / `Range` / `IAsyncEnumerable<T>`.

**User functions**: `fun add(a: int, b: int): int => a + b` lowers to a `public static long Add(long a, long b)` method on the module class. Top-level `let add = fun ...` is the same. The name is converted from snake_case to PascalCase by `mangle.go`.

**Tail-call optimisation**: Mochi allows tail-recursive functions. C# has no `.tail.` IL prefix in the standard codegen path; Roslyn does not emit it. **Decision**: tail-recursive functions are detected in the IR (single recursive call in tail position) and rewritten to a `while (true)` loop + parameter reassignment. This is the same strategy used by Kotlin/JVM and Scala 3. Non-tail recursion is emitted as normal recursion (stack depth is the user's problem; Phase 2 does not implement trampoline bouncing).

**Tail-call rewrite example**:
```csharp
// Before rewrite (naive emission):
public static long Fact(long n, long acc) {
    if (n <= 1L) return acc;
    return Fact(n - 1L, n * acc);  // tail call — rewrite to loop
}

// After tail-call rewrite:
public static long Fact(long n, long acc) {
    while (true) {
        if (n <= 1L) return acc;
        (n, acc) = (n - 1L, n * acc);
    }
}
```

The rewrite is done in the `lower/` pass, not in `aotir`. The rewriter recognises the pattern: last statement is a `ReturnStmt` whose expression is a `CallExpr` to the same function. The rewrite uses a C# tuple deconstruction assignment `(n, acc) = (newN, newAcc)` to update all parameters atomically (avoiding parameter aliasing bugs).

## Sub-phase 2.3 -- Integer divide-by-zero

### Decisions made (2.3)

vm3 raises a `MOCHI_ERR_DIVZERO` error on integer division by zero. C# throws `System.DivideByZeroException`. The .NET transpiler wraps integer division in a helper:

```csharp
namespace Mochi.Runtime.Errors;

public sealed class MochiDivideByZeroError : Exception
{
    public MochiDivideByZeroError() : base("integer divide by zero") { }
}
```

The lowerer emits division as: `MochiMath.IntDiv(a, b)` where:

```csharp
public static long IntDiv(long a, long b) {
    if (b == 0L) throw new MochiDivideByZeroError();
    return a / b;
}
```

This is consistent with the JVM target (`MochiMath.intDiv`) and the C target's `MOCHI_DIVZERO` signal handler. Float division by zero produces `Infinity` / `NaN` (IEEE 754), matching vm3 — no wrapper needed.

## Sub-phase 2.4 -- Float formatting and casts

### Decisions made (2.4)

**Float NaN / Infinity**: vm3 uses Go's `strconv.FormatFloat(f, 'g', -1, 64)`. For NaN, Go prints `"NaN"`; for +Inf, `"+Inf"`; for -Inf, `"-Inf"`. C#'s `double.ToString("G")` prints `"NaN"`, `"Infinity"`, `"-Infinity"`. The `Print.Line(double v)` method in `Mochi.Runtime.IO` must normalise:
```csharp
public static void Line(double v) {
    string s = double.IsNaN(v) ? "NaN"
             : double.IsPositiveInfinity(v) ? "+Inf"
             : double.IsNegativeInfinity(v) ? "-Inf"
             : v.ToString("G", CultureInfo.InvariantCulture);
    Console.WriteLine(s);
}
```

**`int_cast(f: float): int`**: Mochi truncates toward zero. C#'s `(long)f` truncates toward zero. Direct cast.

**`float_cast(i: int): float`**: `(double)i`. Widening conversion, always exact for integers up to 2^53.

**`string_to_int(s: string): Option<int>`**: `long.TryParse(s, out var n) ? Some(n) : None`. Returns `Option<long>`.

**`int_to_string(i: int): string`**: `i.ToString(CultureInfo.InvariantCulture)`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | All scalar expression lowering (arithmetic, comparison, cast); if/while/for/return/break/continue; user function → static method; tail-call rewriter; snake_case → PascalCase name mangling |
| `transpiler3/dotnet/runtime/Mochi.Runtime/IO/Print.cs` | Float formatting; bool lowercase |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Errors/MochiDivideByZeroError.cs` | Divide-by-zero exception |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Math/MochiMath.cs` | `IntDiv` wrapper; float cast helpers |
| `transpiler3/dotnet/build/phase02_test.go` | `TestPhase2Scalars`: 20 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase02-scalars/` | 20 fixture directories |

## Test set

- `TestPhase2Scalars` -- 20 fixtures covering: int arithmetic, float arithmetic, bool ops, string concat, if/else, while, for-in, user functions, recursive functions (fact, fib), tail-recursive fact, divide-by-zero error, float NaN, float Infinity, int_cast, float_cast, string_to_int, compare_float, comparison chain, nested conditions.

## Deferred work

- `string` indexing and slicing. Deferred to Phase 3 (strings as a collection type).
- Multi-return functions (destructuring). Deferred to Phase 4 (records).
- Numeric promotion rules for mixed int/float expressions. Deferred to Phase 2 implementation review.

## Closeout notes

Phase 2 landed. `TestPhase2Scalars` PASS: 20 fixtures on SDK 10.0.107 net10.0. Key additions: `CastExpr` + `AssignStmt` nodes; `lowerBinaryExpr` routes `BinStrCat` → `string.Concat`; `ForRangeStmt` → `for (long i = start; i < end; i++)`; `NumCastExpr` → `(long)expr`; `StrLenExpr/StrIndexExpr/StrContainsExpr`; `MathCallExpr` → `Math.Abs/Floor/Ceiling`. Runtime: `IO/Print.Line(double)` uses `+Inf`/`-Inf`/`NaN` format; `Errors/MochiDivideByZeroError`, `Math/MochiMath.IntDiv` added.
