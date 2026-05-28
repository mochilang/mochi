---
title: "Phase 11. Async colouring"
sidebar_position: 15
sidebar_label: "Phase 11. Async"
description: "MEP-49 Phase 11 — async colour pass propagates async/await through the call graph; AsyncExpr → Task{...}; AwaitExpr → await fut.value; __await_all__ → mochiAwaitAll."
---

# Phase 11. Async colouring

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 11](/docs/mep/mep-0049#phase-11-async-colour) |
| Status         | LANDED |
| Started        | 2026-05-28 14:02 (GMT+7) |
| Landed         | 2026-05-28 14:02 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase11Async`: 7 fixtures green on Swift 6.0+, macOS 15. Gate builds each fixture and compares stdout to `.expected`.

## Goal-alignment audit

Swift's strict concurrency model requires every async call to be explicitly `await`ed, and every function that calls an async function must itself be `async`. Phase 11 ships the colour pass that propagates async upward through the call graph, plus lowering for `AsyncExpr`, `AwaitExpr`, and the `__await_all__` built-in.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Async colour pass over `aotir.Program`: propagate `async` upward through call graph | LANDED | mep/0049-phase-11 |
| 11.1 | `AsyncExpr` → `Task<T, Never> { body }`; `AwaitExpr` → `await fut.value` | LANDED | mep/0049-phase-11 |
| 11.2 | `__await_all__(list)` → `mochiAwaitAll(list)` | LANDED | mep/0049-phase-11 |
| 11.3 | SE-0413 typed throws; `Result`/`throws` bridge | DEFERRED | — |
| 11.4 | `try await` at combined async-throwing call sites | DEFERRED | — |

## Sub-phase 11.0 -- Async colour pass

### Decisions made (11.0)

**`ColourMap`**: a `map[string]Colour` in `transpiler3/swift/colour/colour.go`. `Colour` is an int (`Blue = 0` = synchronous, `Red = 1` = async).

**Seeding**: a function is initially Red if its body contains any `AsyncExpr` or `AwaitExpr` AST node (detected via recursive walk of `aotir.Stmt`/`aotir.Expr`).

**Propagation**: after seeding, the pass iterates to fixpoint: if function A calls function B and B is Red, A becomes Red. This ensures `async` propagates up the entire call chain.

**`main` is Red if any function is Red**: when the colour pass marks `main` as Red (because it directly or transitively calls an async function), the lowerer emits:

```swift
@main
struct MochiOut {
    static func main() async {
        // ...
    }
}
```

## Sub-phase 11.1 -- AsyncExpr and AwaitExpr lowering

### Decisions made (11.1)

**`AsyncExpr` → `Task<T, Never>`**: a Mochi `async { expr }` becomes a Swift unstructured task:

```swift
Task<Int64, Never> {
    return expr
}
```

The return type is inferred from the `AsyncExpr.Type` field in the aotir IR.

**`AwaitExpr` → `await fut.value`**: a Mochi `await futureVar` becomes:

```swift
await futureVar.value
```

`Task.value` is the async property that suspends until the task completes and returns its result.

**`async func` keyword on FuncDecl**: functions marked Red in `ColourMap` get `IsAsync: true` on their `sxtree.FuncDecl` node. The `FuncDecl.SwiftString` emitter adds `async` before `->` in the signature.

## Sub-phase 11.2 -- mochiAwaitAll

### Decisions made (11.2)

**`__await_all__(list)` built-in**: Mochi parallel-await of a list of tasks is lowered by the call-expression handler: when `e.Func == "__await_all__"`, emit `mochiAwaitAll(arg)`.

**`mochiAwaitAll<T: Sendable>([Task<T, Never>]) async -> [T]`**: defined in `Async.swift`. Iterates the task list sequentially with `await task.value`, collecting results. Sequential rather than concurrent collection is correct because the tasks are already running; this just harvests results in order.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/colour/colour.go` | `Analyse(prog)`: seeds AsyncExpr/AwaitExpr, propagates Red upward to fixpoint |
| `transpiler3/swift/lower/lower.go` | Checks `ColourMap`; emits `async func`; lowers `AsyncExpr`, `AwaitExpr`, `__await_all__` |
| `transpiler3/swift/sxtree/nodes.go` | `IsAsync bool` field on `FuncDecl` |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Async.swift` | `mochiAwaitAll<T: Sendable>([Task<T, Never>]) async -> [T]` |
| `transpiler3/swift/build/phase11_test.go` | `TestPhase11Async`: 7 fixtures |
| `tests/transpiler3/swift/fixtures/phase11-async/` | 7 fixture directories |

## Test set

- `TestPhase11Async` -- 7 fixtures: `async_all`, `async_basic`, `async_bool`, `async_chain`, `async_string`, `async_sum`, `async_two`.

## Deferred work

- SE-0413 typed throws: `fun foo(): T throws E` → `func foo() throws(E) -> T`. Deferred to Phase 11.3.
- `Result`/`throws` bridge: `Result.get()`, `Result(catching:)`. Deferred.
- `try await` at combined async-throwing call sites. Deferred to Phase 11.4.
- `async let` for concurrent bindings. Deferred.
- `withTaskGroup` for dynamic parallel maps. Deferred.
- Deadlock detection in the colour pass (cycle detection). Deferred.
