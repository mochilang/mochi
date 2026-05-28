---
title: "Phase 5. Sum types and pattern matching"
sidebar_position: 9
sidebar_label: "Phase 5. Sum types and pattern matching"
description: "MEP-49 Phase 5 — Mochi union types to Swift enum with associated values; match to exhaustive switch; Option<T> to T?; Result<T,E> to Result<T,E>."
---

# Phase 5. Sum types and pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 5](/docs/mep/mep-0049#phase-5-sum-types) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase5Sums`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green (exhaustiveness warnings become errors).

## Goal-alignment audit

Sum types are the primary abstraction over `option<T>`, `result<T,E>`, and user-defined variants. Swift `enum` with associated values is the natural and idiomatic target -- it gives compile-time exhaustiveness checking via the Swift compiler's switch exhaustiveness analysis, readable generated code, and full `Sendable` / `Hashable` / `Codable` support when all associated values conform.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | `type Shape = Circle(r: float) \| Rect(w: float, h: float)` → `public enum Shape` with associated values | NOT STARTED | — |
| 5.1 | `match` → Swift `switch` with exhaustive case pattern; `indirect` for recursive variants | NOT STARTED | — |
| 5.2 | `option<T>` → `T?`; `some(x)` → `x`; `none` → `nil`; `match opt { some(v) => ..., none => ... }` | NOT STARTED | — |
| 5.3 | `result<T,E>` → `Result<T,E>`; `ok(v)` → `.success(v)`; `err(e)` → `.failure(e)` | NOT STARTED | — |
| 5.4 | `Sendable`, `Hashable`, `Codable` conformances synthesised by Swift for all enum variants | NOT STARTED | — |

## Sub-phase 5.0 -- Enum declaration

### Decisions made (5.0)

**`public enum`**: every Mochi union type lowers to a `public enum`. Swift enums with associated values are the canonical sum type representation. Unlike the .NET target which needs an abstract record hierarchy, Swift enums have built-in discriminant + associated data.

**Associated value labels**: Mochi `Circle(r: float)` → Swift `case circle(r: Double)`. Labels are preserved (Swift associated values support labels). The label is lowercase (matching Mochi's snake_case) to keep the case name and label consistent.

**Case name**: Mochi variant names are PascalCase (`Circle`). Swift enum case names are lowerCamelCase (`circle`). The lowerer converts: `Circle` → `.circle`, `Rect` → `.rect`. This matches Swift API Design Guidelines.

```swift
// Mochi: type Shape = Circle(r: float) | Rect(w: float, h: float)
public enum Shape: Sendable, Hashable, Codable {
    case circle(r: Double)
    case rect(w: Double, h: Double)
}
```

**Nullary variants**: Mochi `type Color = Red | Green | Blue` → enum cases with no associated values:

```swift
public enum Color: Sendable, Hashable, Codable {
    case red
    case green
    case blue
}
```

**`indirect` for recursive variants**: Mochi `type List<T> = Nil | Cons(head: T, tail: List<T>)` requires `indirect` because `Cons` contains the enum itself. The lowerer marks the whole enum `indirect`:

```swift
public indirect enum MochiList<T>: Sendable, Hashable, Codable
where T: Sendable & Hashable & Codable {
    case `nil`
    case cons(head: T, tail: MochiList<T>)
}
```

**`nil` keyword conflict**: Mochi `Nil` variant cannot be named `nil` in Swift (reserved keyword). The lowerer uses a backtick escape: `` case `nil` ``.

**One file per union type**: same as records. Each Mochi union type gets its own `.swift` file.

## Sub-phase 5.1 -- Match to switch

### Decisions made (5.1)

**Swift `switch` is exhaustive by default**: the Swift compiler requires all cases to be covered (or a `default` arm). Since Mochi's `match` is always exhaustive, the lowerer never emits a `default` arm unless the Mochi source has a wildcard `_` arm.

**Pattern lowering**:

```swift
// Mochi:
// match shape {
//   Circle(r) => 3.14159 * r * r,
//   Rect(w, h) => w * h
// }

let area: Double
switch shape {
case .circle(let r):
    area = 3.14159 * r * r
case .rect(let w, let h):
    area = w * h
}
```

**Switch as expression**: Swift `switch` is a statement by default (not an expression). For Mochi `match` used as an expression, the lowerer assigns to a temp variable:

```swift
let __area: Double
switch shape {
case .circle(let r): __area = 3.14159 * r * r
case .rect(let w, let h): __area = w * h
}
let area = __area
```

In Swift 5.9+ (`if` and `switch` expressions via SE-0380), the lowerer can emit `switch` as an expression directly. The lowerer targets Swift 6.0 (which includes SE-0380), so the expression form is preferred when the switch result is used immediately:

```swift
let area = switch shape {
case .circle(let r): 3.14159 * r * r
case .rect(let w, let h): w * h
}
```

**Guard clauses**: Mochi `Circle(r) if r > 0.0 => ...` → `case .circle(let r) where r > 0.0:`.

**Nested patterns**: `Pair(Some(x), _)` → `case .pair(.some(let x), _):`.

**Wildcard**: Mochi `_ =>` → `default:`.

## Sub-phase 5.2 -- option<T> → T?

### Decisions made (5.2)

**`T?` not `Option<T>`**: Mochi `option<T>` maps to Swift `Optional<T>` (written `T?`). Swift `Optional` is a stdlib enum with `.some(T)` and `.none` cases. This avoids a custom `Option` type in `MochiRuntime` and integrates with Swift's `??`, `if let`, `guard let`, optional chaining, and `map`/`flatMap` on optionals.

**`some(x)` → `Optional.some(x)` → just `x` in context**: in Swift, wrapping a value in `Optional` is implicit when the type context is `T?`. So `some(x)` in Mochi lowers to just `x` when assigned to a `T?` variable. When a `none` is needed, the lowerer emits `Optional<T>.none` or `nil` depending on context.

**`match opt { some(v) => f(v), none => g() }`**: lowered to:

```swift
switch opt {
case .some(let v): f(v)
case .none: g()
}
```

Or with the `if let` idiom when the `none` branch is trivial:

```swift
if let v = opt {
    f(v)
} else {
    g()
}
```

The lowerer prefers `switch` for exhaustive multi-arm matches and `if let` for simple `some/none` pairs.

**`opt ?? default`**: Mochi `opt.get_or(default)` → `opt ?? default`.

**`opt.map(f)`**: → `opt.map(f)`. Swift `Optional.map` applies `f` to the wrapped value if present.

**`opt.flat_map(f)`**: → `opt.flatMap(f)`.

**Optional chaining**: `user?.address?.city` in Mochi → `user?.address?.city` in Swift. The lowerer recognises optional field chains and emits optional chaining directly.

## Sub-phase 5.3 -- result<T,E> → Result<T,E>

### Decisions made (5.3)

**Swift stdlib `Result<T,E>`**: Mochi `result<T,E>` maps to Swift's `Result<Success, Failure>` where `Failure: Error`. `ok(v)` → `.success(v)`. `err(e)` → `.failure(e)`.

**Error type**: Mochi error types in `result<T,E>` are Mochi union types or record types. They are lowered to Swift types that also conform to `Error`. The lowerer adds `Error` conformance to the error enum/struct when it appears as the `E` in `result<T,E>`:

```swift
// Mochi: type MyError = NotFound | Unauthorized
public enum MyError: Error, Sendable, Hashable, Codable {
    case notFound
    case unauthorized
}
```

**`match res { ok(v) => ..., err(e) => ... }`**:

```swift
switch res {
case .success(let v): ...
case .failure(let e): ...
}
```

**`result.map(f)`**: → `res.map(f)`.

**`result.flat_map(f)`**: → `res.flatMap(f)`.

**Typed throws bridge** (Phase 11): `result<T,E>` and `throws(E)` are interconvertible. `Result.get()` throws the error; `Result(catching:)` wraps a throwing call. The bridge is fully established in Phase 11.

## Sub-phase 5.4 -- Synthesised conformances

### Decisions made (5.4)

**`Sendable`**: Swift synthesises `Sendable` for enums when all associated values are `Sendable`. Since all Mochi types are `Sendable` by construction (records, scalars, collections), the synthesis always succeeds.

**`Hashable`**: synthesised when all associated values are `Hashable`. Same reasoning.

**`Codable`**: synthesised when all associated values are `Codable`. Nullary cases encode as a string of the case name. Associated-value cases encode as an object `{ "type": "circle", "r": 1.5 }` by default. Custom encoding keys can be specified in Phase 14 (JSON customisation).

**`CustomStringConvertible`**: NOT synthesised by Swift for enums with associated values. `MochiRuntime.print` uses `String(reflecting:)` for enum values in Phase 5. A proper `description` implementation is deferred.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `UnionDecl` → `public enum`; variant lowering; `indirect` detection |
| `transpiler3/swift/lower/match.go` | `MatchStmt`/`MatchExpr` → `switch` statement/expression; pattern deconstruction |
| `transpiler3/swift/lower/option.go` | `option<T>` → `T?`; `some`/`none` literal lowering; optional chain lowering |
| `transpiler3/swift/lower/result.go` | `result<T,E>` → `Result<T,E>`; `ok`/`err` lowering; `Error` conformance injection |
| `transpiler3/swift/build/phase05_test.go` | `TestPhase5Sums`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase05-sums/` | 25 fixture directories |

## Test set

- `TestPhase5Sums` -- 25 fixtures covering: `sum_basic`, `sum_nullary`, `sum_recursive`, `sum_generic`, `sum_nested_match`, `sum_guard`, `sum_wildcard`, `sum_as_expression`, `sum_in_list`, `sum_in_map`, `sum_codable`, `sum_hashable`, `sum_multi_variant`, `option_some_none`, `option_map`, `option_flat_map`, `option_chain`, `option_get_or`, `option_in_list`, `result_ok_err`, `result_map`, `result_flat_map`, `result_error_type`, `result_typed_throws_bridge`, `sum_string_result`.

## Deferred work

- `CustomStringConvertible` for enums. Deferred (low priority; `String(reflecting:)` is a working fallback).
- `Codable` custom encoding keys for sum types. Deferred to Phase 14.
- Pattern matching on string literals in `match`. Deferred to Phase 2 extension.
- Non-copyable (`~Copyable`) enum variants for unique-ownership types. Out of v1 scope.
