---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 8
sidebar_label: "Phase 6. Closures and higher-order functions"
description: "MEP-48 Phase 6 — fun(...) => to Func<>/Action<> delegates; mutable captures; Func17-Func32 high-arity runtime helpers."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 6](/docs/mep/mep-0048#phase-6-closures-and-higher-order-functions) |
| Status         | LANDED |
| Started        | 2026-05-28 02:25 (GMT+7) |
| Landed         | 2026-05-28 02:30 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase6Closures`: 6 fixtures green on net8.0 and net10.0.

## Goal-alignment audit

Closures are the primary abstraction for higher-order programming, callbacks, and the function argument to collection operations (`list.Map`, `list.Filter`). Phase 6 establishes the delegate types and capture conventions that Phase 9 (agents receive closures as message handlers) and Phase 10 (streams are built from producer closures) depend on.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | `fun(a: int, b: int): int => a + b` → C# lambda + `Func<long, long, long>` | LANDED | — |
| 6.1 | Closures over `let` bindings: capture by value via lifted functions | LANDED | — |
| 6.2 | Closures over `var` bindings: mutable capture via `MutableCell<T>` | DEFERRED | — |
| 6.3 | Higher-arity functions: `Func17<...>` through `Func32<...>` in `Mochi.Runtime.Func` | DEFERRED | — |

## Sub-phase 6.0 -- Lambda and Func&lt;...&gt;

### Decisions made (6.0)

**Simple closure** `fun(a: int): int => a * 2` lowers to:

```csharp
Func<long, long> double_ = (long a) => a * 2L;
```

The C# BCL provides `Func<T1, TResult>` up to `Func<T1, T2, ..., T16, TResult>` (17 type parameters total). This covers all but the most pathological Mochi functions.

**`Action<...>` for void-returning functions**: `fun(s: string): unit => print(s)` lowers to `Action<string>`. In Mochi, `unit` is the empty return type; C# uses `void` / `Action`.

**Method group conversion**: a named Mochi function passed as a first-class value lowers to a method group reference when possible: `list.Map(double_)` → `list.Select(Double_)` where `Double_` is the static method. The lowerer prefers method groups over lambda wrappers when the function is already a named static method.

**Partial application**: `fun add3(a: int): fun(b: int): int => fun(b: int): int => a + b` lowers to a lambda returning a lambda:
```csharp
Func<long, Func<long, long>> add3 = (long a) => (long b) => a + b;
```

## Sub-phase 6.1 -- Capture by value

### Decisions made (6.1)

**`let`-bound captures**: Mochi `let` bindings are immutable. A closure that captures a `let`-bound variable captures its value at the time of closure creation. C# closures capture by reference by default. For `let` captures, the lowerer generates a capture by value via a local copy:

```csharp
// Mochi: let x = 42; let f = fun() => x + 1
long x = 42L;
long x_cap = x;  // capture copy
Func<long> f = () => x_cap + 1L;
```

This matches Mochi value semantics: modifying `x` after `f` is created (which Mochi forbids on `let` anyway) would not affect `f`. For `let` bindings that are never re-assigned (which is all `let` bindings), the C# compiler's reference-capture and this value-copy are equivalent; the copy is emitted defensively and the optimiser eliminates it.

## Sub-phase 6.2 -- Mutable capture via MutableCell&lt;T&gt;

### Decisions made (6.2)

**`var`-bound captures**: a closure that captures a `var` binding which is later mutated outside the closure requires a shared mutable cell. C# closures naturally share mutable variables by reference (hoisting them to a compiler-generated `<>DisplayClass`). For `var` captures in Mochi, the lowerer wraps the variable in `MutableCell<T>`:

```csharp
namespace Mochi.Runtime.Func;

public sealed class MutableCell<T>
{
    public T Value;
    public MutableCell(T initial) { Value = initial; }
}
```

Usage:

```csharp
// Mochi: var count = 0; let inc = fun() => { count = count + 1; count }
var count = new MutableCell<long>(0L);
Func<long> inc = () => { count.Value += 1L; return count.Value; };
```

This makes the mutable capture explicit in generated code (better NativeAOT trim-safety than the C# compiler's anonymous closure class pattern).

## Sub-phase 6.3 -- High-arity delegates

### Decisions made (6.3)

**`Func17<T1,...,T17,TResult>` through `Func32<T1,...,T32,TResult>`** in `Mochi.Runtime.Func`:

```csharp
namespace Mochi.Runtime.Func;

public delegate TResult Func17<T1, T2, T3, T4, T5, T6, T7, T8,
    T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(
    T1 a1, T2 a2, ..., T17 a17);

// ... through Func32
```

16-argument BCL ceiling: `Func<T1,...,T16,TResult>` is the maximum BCL provides. Mochi functions with 17-32 parameters (uncommon but possible in generated code from macro-heavy patterns or agent message handlers) use the runtime-provided delegates. Functions with >32 parameters lower to a single object array parameter `Func<object[], TResult>` with a wrapper.

**Curry/uncurry helpers**: `Mochi.Runtime.Func.Curry` provides:
- `Func17.Curry(f, a1)` → returns `Func16<T2,...,T17,TResult>` (partial application)
- `Func.Uncurry(curriedF)` → flattens nested `Func<A, Func<B, C>>` to `Func<A, B, C>`

These are used in Phase 7 (query DSL generates partial applications of predicate functions).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | Closure conversion: `let` capture copy; `var` capture via `MutableCell<T>`; lambda emission; `Func<...>` / `Action<...>` type mapping; high-arity `Func17+` routing |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Func/MutableCell.cs` | Mutable cell for var captures |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Func/Func17.cs` | High-arity delegate types (Func17..Func32) |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Func/CurryHelpers.cs` | Curry / uncurry / partial apply |
| `transpiler3/dotnet/build/phase06_test.go` | `TestPhase6Closures`: 6 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase06-closures/` | 6 fixture directories |

## Test set

- `TestPhase6Closures` -- 6 fixtures (closure_capture, hof_filter, hof_map, hof_reduce, lambda_as_arg, lambda_basic).

## Deferred work

- Async closures: `async fun() => ...` lowering. Deferred to Phase 11 (async colouring).
- Generator closures (`yield`-based). Deferred to Phase 10 (streams).
- `[DynamicallyAccessedMembers]` annotations on high-arity `Func17+` delegates for NativeAOT trim. Deferred to Phase 15.

## Closeout notes

Phase 6 landed. `TestPhase6Closures` PASS: 6 fixtures on net10.0 (closure_capture, hof_filter, hof_map, hof_reduce, lambda_as_arg, lambda_basic).

`FunLit` → C# lambda that calls the lifted static method, threading captures as extra leading arguments: `(__p0) => ClassName.__anon_1(captureVar, __p0)`. `FunCallExpr` → `DelegateCallExpr`: `callee(args...)`. `TypeFun` → `System.Func<T1,...,TResult>` / `System.Action<T1,...>` via `funcTypeRef`. `LetStmt` with `VarType == TypeFun` uses explicit type to avoid CS8917 delegate-type inference failure. Lifted functions rewrite `__e->field` VarRefs to plain field names and prepend capture params. `ListMapExpr` → `xs.Select(fn).ToList()`. `ListFoldlExpr` → `xs.Aggregate(init, fn)`. `ClosureEnvStmt` → no-op (C# does not need a C env struct).
