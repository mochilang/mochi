---
title: "Phase 11. Async colouring and typed throws"
sidebar_position: 15
sidebar_label: "Phase 11. Async / typed throws"
description: "MEP-49 Phase 11 — async colour pass adds async/await throughout the call graph; SE-0413 typed throws fun foo(): T throws E → func foo() throws(E) -> T."
---

# Phase 11. Async colouring and typed throws

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 11](/docs/mep/mep-0049#phase-11-async-colour) |
| Status         | LANDED |
| Started        | 2026-05-28 14:02 (GMT+7) |
| Landed         | 2026-05-28 14:02 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase11Async`: 15 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Swift's strict concurrency model requires every async call to be explicitly `await`ed, and every function that calls an async function must itself be `async`. This "colour" propagation must be done globally over the Mochi program's call graph before code generation. Phase 11 ships the colour pass and typed-throws lowering, which unblocks all async features (agent calls, stream consumption, fetch, LLM) from being usable in arbitrary function positions.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Async colour pass over `aotir.Program`: propagate `async` upward through the call graph | LANDED | mep/0049-phase-11 |
| 11.1 | `async func` emission; `await` at every async call site; `async let` for concurrent bindings | LANDED | mep/0049-phase-11 |
| 11.2 | SE-0413 typed throws: `fun foo(): T throws E` → `func foo() throws(E) -> T` | DEFERRED | — |
| 11.3 | `result<T,E>` / `throws(E)` interconversion bridge: `Result.get()`, `Result(catching:)` | DEFERRED | — |
| 11.4 | `try await` at combined async-throwing call sites | DEFERRED | — |

## Sub-phase 11.0 -- Async colour pass

### Decisions made (11.0)

**Colour analysis**: a function is "red" (async) if it contains any of:
- An `await agent_call` expression.
- A `for await` loop over a stream.
- A `try await` expression (async throws).
- Any call to another red function.
- An `async let` binding.

The colour pass performs a topological traversal of the `aotir` call graph, marking functions red bottom-up. The pass runs after `aotir.Lower` and before `lower.Lower`.

**`ColourMap`**: a `map[FunctionID]bool` stored in the lowerer context. The lowerer checks `ColourMap[fnID]` when emitting a function declaration or call.

**`main` is always red if any top-level code is async**: if `main` contains any red call (including spawning agents), the lowerer emits `@main struct { static func main() async { ... } }`.

**`Task { }` wrapping**: synchronous callers of red functions cannot call them directly in Swift 6. The lowerer detects this pattern (a "blue" function that logically should call a red function) and wraps the call in `Task { await f() }`. For agent casts (fire-and-forget), `Task.detached { await ... }` is used if the context is synchronous.

## Sub-phase 11.1 -- async func emission

### Decisions made (11.1)

**`async func` keyword**: functions marked red in `ColourMap` → `func name(...) async -> T`. The `async` keyword is placed before `->`.

**`await` at call sites**: every call to a red function from another red function → `await f(args)`. The sxtree `FunctionCallExpr` node has an `IsAsync bool` field; the emitter adds `await ` prefix when true.

**`async let` for concurrent bindings**: Mochi `let (x, y) = concurrent { (f(), g()) }` (parallel bindings) → Swift `async let`:

```swift
async let x = f()
async let y = g()
let (xVal, yVal) = await (x, y)
```

`async let` starts both tasks concurrently; the `await` at the tuple collects both results.

**`withTaskGroup` for dynamic concurrency**: Mochi `let results = parallel { xs.map(f) }` (parallel map over a list where `f` is async) → Swift:

```swift
let results: [U] = await withTaskGroup(of: U.self) { group in
    for x in xs {
        group.addTask { await f(x) }
    }
    var out: [U] = []
    for await r in group { out.append(r) }
    return out
}
```

## Sub-phase 11.2 -- Typed throws (SE-0413)

### Decisions made (11.2)

**SE-0413** (Swift 6.0): `func foo() throws(E) -> T` where `E: Error`. Previously, Swift only supported untyped `throws` (which erases the error type to `any Error`). With typed throws, the error type is statically known.

**Mochi `fun foo(): T throws E` mapping**:

```swift
// Mochi: fun parse(s: string): int throws ParseError
public func parse(_ s: String) throws(ParseError) -> Int64 {
    // ...
}
```

**Untyped throws bridge**: when Mochi code calls a Swift FFI function that uses untyped `throws`, the lowerer wraps it in a `do { ... } catch { throw MochiError.wrap(error) }` to convert to a typed throw. `MochiError` is a catch-all error type in MochiRuntime.

**Re-throw**: `fun foo(): T throws E` that calls `bar(): U throws E` can re-throw directly. Functions that call multiple throwing functions with different error types must use `throws` (untyped) or a union error type.

**`rethrows`**: Mochi HOFs like `list.map_throwing(f)` where `f` is a throwing closure → `func mapThrowing<E>(_ f: (T) throws(E) -> U) throws(E) -> [U]`. The Swift compiler handles `rethrows` for this pattern.

## Sub-phase 11.3 -- Result / throws bridge

### Decisions made (11.3)

**`Result.get()` → throws**: `res.get()` in Mochi → `try res.get()` in Swift. Swift's `Result<T,E>.get()` throws the failure value when the result is `.failure`.

**`Result(catching:)`**: Mochi `result_of { f() }` → `Result { try f() }`. Wraps a throwing call in a `Result`.

**Async result**: `async_result_of { await f() }` → `await Result { try await f() }`. Both `async` and `throws` at the same call site.

**`flatMap` bridge**: `res.flat_map(f)` where `f` returns `result<U,E>` → `res.flatMap(f)`. Swift's `Result.flatMap` handles this.

## Sub-phase 11.4 -- try await

### Decisions made (11.4)

**Combined async + throws call sites**: agent calls that can throw (e.g., `call(agent, fetch_data)` where `fetch_data` throws a network error) → `try await agent.fetchData()`.

**Error propagation**: in a `func foo() async throws(NetworkError)`, a `try await agent.fetchData()` that throws `NetworkError` propagates the error automatically. No explicit re-throw needed.

**`for try await`**: a stream that can throw (e.g., a network stream that terminates with an error) → `for try await x in stream { ... }`. The loop is wrapped in `do { ... } catch { }`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/colour/colour.go` | `ColourPass`: propagates async colour upward through call graph |
| `transpiler3/swift/lower/lower.go` | Checks `ColourMap`; emits `async func`, `await`, `async let`, typed throws |
| `transpiler3/swift/lower/throws.go` | Typed throws emission; `Result`/`throws` bridge; `rethrows` analysis |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Error.swift` | `MochiError` catch-all; `withTimeout`; error wrapping |
| `transpiler3/swift/build/phase11_test.go` | `TestPhase11Async`: 15 fixtures |
| `tests/transpiler3/swift/fixtures/phase11-async/` | 15 fixture directories |

## Test set

- `TestPhase11Async` -- 15 fixtures covering: `async_basic`, `async_chain`, `async_agent_call`, `async_stream_for_await`, `async_let_parallel`, `async_task_group`, `throws_basic`, `throws_propagate`, `throws_typed`, `throws_result_bridge`, `try_await_basic`, `try_await_agent`, `async_throws_rethrow`, `async_throws_for_try_await`, `async_colour_propagation`.

## Deferred work

- `async throws` functions with multiple error types (requires error union or untyped throws). Deferred to a Phase 11 extension.
- Swift Concurrency structured task tree visualization. Out of scope.
- Deadlock detection in the colour pass (cycle detection in the async call graph). Deferred.
- `@discardableResult` for fire-and-forget async functions. Deferred to a linting pass.
