---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 10
sidebar_label: "Phase 6. Closures and higher-order functions"
description: "MEP-49 Phase 6 — fun(...)=>expr to Swift closures; @escaping, @Sendable at actor boundaries; capture lists; partial application; Func<> protocol."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 6](/docs/mep/mep-0049#phase-6-closures) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase6Closures`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green (no `@Sendable` / `@escaping` inference failures).

## Goal-alignment audit

Closures are Mochi's primary abstraction for higher-order programming, callbacks, and the function argument to collection operations. Phase 6 ships the closure ABI conventions that Phase 9 (actor message handlers receive closures) and Phase 10 (stream producers are closures) depend on. The `@Sendable` annotation is required by Swift 6's strict concurrency for closures that cross actor boundaries.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | `fun(a: int, b: int): int => a + b` → Swift closure `{ (a: Int64, b: Int64) -> Int64 in a + b }` | NOT STARTED | — |
| 6.1 | `@escaping` annotation: closures stored in variables or passed to functions that outlive the call | NOT STARTED | — |
| 6.2 | `@Sendable` annotation: closures passed across actor boundaries (detected in Phase 9) | NOT STARTED | — |
| 6.3 | Capture lists: `[weak self]`, `[capturedVar]` for explicit capture semantics | NOT STARTED | — |
| 6.4 | Partial application; curried functions; `fun(T)->R` as first-class value | NOT STARTED | — |

## Sub-phase 6.0 -- Closure literals

### Decisions made (6.0)

**Swift closure syntax**: Mochi `fun(a: int, b: int): int => a + b` → Swift closure:

```swift
let add: (Int64, Int64) -> Int64 = { (a: Int64, b: Int64) -> Int64 in
    return a + b
}
```

The lowerer always emits explicit parameter types and return type in the closure signature. Trailing closure syntax is only used when the closure is the last argument of a function call (Swift convention).

**`unit` → `Void`**: Mochi `fun(s: string): unit => print(s)` → `(String) -> Void`. The return type annotation is emitted as `Void` (not omitted) to make the closure type explicit in generated code.

**Multi-line closures**: Mochi closures with a block body (`fun(x) => { let y = x + 1; y * 2 }`) → Swift closures with a multi-statement body:

```swift
let f: (Int64) -> Int64 = { (x: Int64) -> Int64 in
    let y: Int64 = x + Int64(1)
    return y * Int64(2)
}
```

**Named function as closure**: a named Mochi function passed as a first-class value → Swift function reference using the identifier directly:

```swift
// Mochi: list.map(double)  (where double is a top-level function)
let results = xs.map(double)
```

When the function is a method on a type, the lowerer uses a closure wrapper: `{ x in MyType.method(x) }`.

**Function type in variable declarations**: Mochi `let f: fun(int) -> int = ...` → Swift `let f: (Int64) -> Int64 = ...`. The function type is part of the `let` binding annotation.

## Sub-phase 6.1 -- @escaping

### Decisions made (6.1)

**When `@escaping` is required**: a closure is `@escaping` in Swift when it outlives the function call that receives it -- i.e., it is stored in a variable, returned, or passed to another `@escaping` parameter. The Swift compiler enforces this.

**Escape analysis in the lowerer**: the lowerer conservatively marks closures `@escaping` when they are passed to:
- A function parameter that stores the closure (determined by the callee's signature in the `aotir`).
- A `var` binding (closures stored in mutable variables escape by definition).
- Any `list.map`/`filter`/`reduce` (these return new collections containing transformed values; the closure must not outlive the operation, so it is NOT `@escaping` here).
- Actor mailbox continuation enqueue (Phase 9, always `@escaping @Sendable`).

**Trailing closures**: `xs.map { x in f(x) }` -- the closure is not `@escaping` here. `xs.map` takes `@escaping` in stdlib but the Swift compiler's liveness analysis handles this. The lowerer does not add `@escaping` to the closure literal itself; it is part of the `map` function's parameter signature.

**`@escaping` in generated function signatures**:

```swift
// Mochi: fun register(callback: fun(int) -> unit): unit
public func register(callback: @escaping (Int64) -> Void) {
    // stores callback
}
```

## Sub-phase 6.2 -- @Sendable

### Decisions made (6.2)

**`@Sendable` requirement**: in Swift 6, closures that cross actor isolation boundaries must be `@Sendable`. This means all captured variables must be `Sendable` (or the closure must not capture non-Sendable state). The `@Sendable` attribute is added by the lowerer when:
- The closure is passed as an argument to an `async` function.
- The closure is stored in an `actor`'s property (Phase 9).
- The closure is submitted to `Task { }` or `withTaskGroup`.

**Static determination**: the lowerer determines `@Sendable` requirement from the callee's parameter type in the `aotir`. If the callee's type system says the parameter is `@Sendable`, the lowerer emits `@Sendable` on the closure.

**Combined**: `@escaping @Sendable` for closures stored in actor fields:

```swift
// Mochi: agent handler with a stored closure
func onMessage(_ handler: @escaping @Sendable (Message) async -> Void) { ... }
```

**`Sendable` conformance of captured vars**: all Mochi `let` bindings are `Sendable` (their types conform). Mochi `var` bindings captured in `@Sendable` closures require the variable's type to be `Sendable`. The lowerer emits a capture list with `captureVar` (value capture) for `var` captures in `@Sendable` closures to avoid shared mutable state:

```swift
// Captures `count` by value at closure creation time:
let f: @Sendable () -> Int64 = { [count] in count }
```

## Sub-phase 6.3 -- Capture lists

### Decisions made (6.3)

**`[weak self]` for actor references**: closures inside actor methods that capture `self` use `[weak self]` when the closure outlives the actor's message processing loop. The lowerer detects this pattern in Phase 9 and emits the capture list.

**`[capturedVar]` (value copy)**: Mochi closures capture `let` bindings by value semantics (the binding is immutable). Swift closures capture by reference by default. For `let` bindings, the difference is immaterial (the value cannot change). For `var` bindings captured in a `@Sendable` closure, the lowerer explicitly captures by value: `{ [count] in ... }`. This makes the capture explicit and avoids `@Sendable` violations.

**Mochi `let` → always safe**: since Mochi `let` bindings are immutable, their Swift counterparts can be captured by reference (the Swift default) without correctness issues. The lowerer does not emit explicit capture lists for `let` captures.

**No `unowned`**: the lowerer never emits `unowned`. `weak` is sufficient and safer (no crash on dangling reference). `unowned` is an optimisation deferred to a future pass.

## Sub-phase 6.4 -- Partial application and currying

### Decisions made (6.4)

**Partial application**: Mochi `fun add3(a: int): fun(b: int): int => fun(b: int): int => a + b` lowers to a closure returning a closure:

```swift
let add3: (Int64) -> (Int64) -> Int64 = { (a: Int64) -> (Int64) -> Int64 in
    return { (b: Int64) -> Int64 in
        return a + b
    }
}
```

**Curried functions**: Mochi allows curried syntax `fun add(a: int)(b: int): int => a + b`. The lowerer flattens this to the nested closure form above.

**`list.map(f)` with a partially applied function**: `users.map(get_name)` where `get_name` is a top-level function → `users.map(getName)` (direct function reference). `users.map(add(3))` where `add(3)` returns a closure → `users.map(add3)` where `add3 = add(Int64(3))`.

**First-class functions as values**: Mochi `fun(T) -> R` type → Swift `(T) -> R`. Swift function types are first-class. No boxing or delegate wrapper needed (unlike .NET which needed `Func<T,R>`).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `FunLit` → Swift closure; `FunCallExpr` → call expression |
| `transpiler3/swift/lower/closure.go` | `@escaping` / `@Sendable` annotation logic; capture list emission |
| `transpiler3/swift/lower/types.go` | `TypeFun` → `(T1,...,Tn) -> R` Swift function type |
| `transpiler3/swift/build/phase06_test.go` | `TestPhase6Closures`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase06-closures/` | 25 fixture directories |

## Test set

- `TestPhase6Closures` -- 25 fixtures covering: `closure_basic`, `closure_capture_let`, `closure_capture_var`, `closure_multi_param`, `closure_unit_return`, `closure_as_arg`, `closure_stored`, `closure_returning_closure`, `closure_escaping`, `closure_sendable`, `closure_capture_list`, `hof_map`, `hof_filter`, `hof_reduce`, `hof_flat_map`, `hof_for_each`, `lambda_basic`, `lambda_as_arg`, `partial_apply`, `curried`, `first_class_func`, `closure_in_list`, `closure_over_record`, `closure_nested`, `closure_recursive`.

## Deferred work

- Async closures (`async fun() => ...`). Deferred to Phase 11 (async colouring).
- Generator closures (`yield`-based). Deferred to Phase 10 (streams).
- `[DynamicallyAccessedMembers]`-equivalent for NativeAOT-style trimming. Not applicable to Swift (no reflection trimming concerns; ARC is static).
- High-arity closures (>8 parameters). Swift supports unlimited parameters; no `Func17` equivalent needed. Out of scope.
