---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "Swift Concurrency fundamentals (async/await, Task, actors, @Sendable), the DispatchGroup-bounded Task dispatch pattern, @MainActor isolation deadlock risks, the opt-in actor-mode async bridge, cancellation semantics, and contrast with the tokio singleton approach MEP-73 uses."
---

# 08. Async bridge

## Swift Concurrency fundamentals

Swift Concurrency was introduced in Swift 5.5 (SE-0296, SE-0297, WWDC 2021) as a first-class language feature. Key components:

**`async` functions.** A function marked `async` is a coroutine: it can suspend execution without blocking the calling thread, returning control to the Swift Concurrency runtime. The compiler transforms `async` functions into state machines (similar to Rust's `async fn` and Python's `async def`). An `async` function must be called with `await`:

```swift
func fetchUser(id: String) async throws -> User {
    let url = URL(string: "https://api.example.com/users/\(id)")!
    let (data, _) = try await URLSession.shared.data(from: url)
    return try JSONDecoder().decode(User.self, from: data)
}
```

**`Task`.** A `Task` is a unit of asynchronous work that can be scheduled on the Swift Concurrency thread pool. `Task { }` creates a child task inheriting the current actor context. `Task.detached { }` creates an unstructured task with no inherited context.

**Actors.** A type declared `actor` has its stored properties and methods protected by the actor's isolated executor. Calls to an actor's methods from outside the actor require `await`, ensuring serial access. `@MainActor` is a special global actor bound to the main thread's run loop.

**`@Sendable`.** A closure or function type annotated `@Sendable` can be safely passed across task or actor boundaries (it does not capture mutable state without synchronisation). `@_cdecl` functions are inherently `@Sendable`-compatible because they have C calling convention and no captured state.

## The impedance mismatch

Mochi's execution model is single-threaded and synchronous (MEP-53 §5). A Mochi program calls a Swift function expecting a return value; the Swift function may be `async`. From the `@_cdecl` wrapper's perspective, it is called from C, which has no `async` execution context. The wrapper must resolve the async computation to a synchronous value before returning to the C caller.

Three resolution strategies are viable:

## Strategy 1: `DispatchGroup`-bounded `Task` (default, `async-mode = "dispatch-group"`)

The default strategy for every `async` Swift function:

```swift
// Generated wrapper for: public func fetchUser(id: String) async throws -> User
@_cdecl("mochi_Pkg_fetchUser")
public func mochi_Pkg_fetchUser(_ id_ptr: UnsafePointer<CChar>, _ id_len: Int,
                                 _ out_ptr: UnsafeMutablePointer<UnsafeMutableRawPointer?>,
                                 _ out_err: UnsafeMutablePointer<UnsafeMutableRawPointer?>) -> Int32 {
    let id = String(bytes: UnsafeBufferPointer(start: id_ptr, count: id_len), encoding: .utf8)!
    var result: Result<User, Error>?
    let group = DispatchGroup()
    group.enter()
    Task {
        defer { group.leave() }
        do {
            result = .success(try await fetchUser(id: id))
        } catch {
            result = .failure(error)
        }
    }
    group.wait()
    switch result! {
    case .success(let user):
        // marshal user to C ABI...
        return 0
    case .failure(let err):
        let errMsg = err.localizedDescription
        // marshal errMsg...
        return 1
    }
}
```

**Why it works.** The `@_cdecl` entry point is called from the Mochi main thread, which is not a Swift Concurrency thread (it has no Swift executor attached). Calling `DispatchGroup.wait()` on a non-executor thread is safe: the thread blocks on a semaphore, yielding the OS thread to the scheduler. The `Task { }` is enqueued on the cooperative Swift thread pool (which has its own worker threads, separate from the blocked Mochi main thread). When the `Task` completes, `group.leave()` fires, and the blocked `wait()` call returns.

**Why `Task { }` not `Task.detached { }`.** `Task { }` inherits the caller's actor context. Since the caller is on the Mochi main thread (not an actor), the inherited context is "unstructured / generic executor". `Task.detached { }` would also work but would orphan any potential task cancellation propagation. The bridge uses `Task { }` for consistency.

**`@MainActor` risk.** If the wrapped function is `@MainActor`-isolated, the `Task { }` body will attempt to hop to the main actor. The main actor requires the main thread's run loop to be draining. The Mochi main thread is blocking on `group.wait()`, which is a semaphore wait (not a run loop drain). This means the `Task { }` enqueued to hop to `@MainActor` will never execute, and `group.wait()` will never return: **deadlock**. The bridge mitigates this by emitting `SkipReport` for `@MainActor`-isolated functions by default. Users can opt in via `[swift.capabilities] main-actor = true`, which changes the wrapper to use `Task.detached { }` and adds a `MainActor.run { }` wrapper inside the detached task (avoiding the main thread hop by running the function on a detached executor in a `@MainActor`-annotated closure that the cooperative pool can execute without the main run loop).

**Cost.** One `DispatchGroup` allocation + one `Task` allocation per async call. On Apple Silicon M2, this costs approximately 1-5 µs overhead per call. Acceptable for I/O-bound operations (network, disk); disproportionate for hot-loop computation. For computation-bound async functions, the user should prefer synchronous Swift alternatives.

## Strategy 2: Actor-mode async bridge (opt-in, `async-mode = "actor"`)

When `[swift.runtime] async-mode = "actor"` is set, the bridge exposes `async func` items as Mochi `async fun` declarations. The `@_cdecl` wrapper is not emitted; instead, the MEP-53 Swift emit pass generates:

```swift
// Emitted by MEP-53 when lowering a Mochi async call to swift
public func mochiAsync_Pkg_fetchUser(_ id: String) async throws -> MochiUser {
    let swiftUser = try await fetchUser(id: id)
    return MochiUser(from: swiftUser)
}
```

The Mochi-side `async fun fetch_user(id: string): User` is lowered by MEP-53 to a Swift `async` call to `mochiAsync_Pkg_fetchUser`, which is then composed naturally with Swift's structured concurrency. No `DispatchGroup` is used. The async coloring propagates from Swift through the MEP-53 emit pass to Mochi.

**Constraint.** This mode requires the MEP-53 Swift emit pass to understand `async` Swift functions as a calling convention, not just as a synchronous-callable pair. This is a non-trivial MEP-53 change and is why actor mode is deferred to phase 14.

## Cancellation semantics

In `dispatch-group` mode, cancellation is not propagated: the `Task { }` runs to completion regardless of whether the Mochi program exits. Long-running async operations (e.g., a Vapor request handler that takes 30 seconds) will block the Mochi main thread for the duration. Mitigation: the bridge enforces a default timeout of 30 seconds per async call (configurable via `[swift.runtime] async-timeout-seconds = 60`); when the timeout fires, the `DispatchGroup` is released with a timeout error and the orphaned `Task` continues in the background until the process exits.

In actor mode, Swift's structured cancellation propagates normally.

## Concurrency comparison: MEP-69 vs MEP-73

| Aspect | MEP-73 (Rust, tokio) | MEP-69 (Swift, Concurrency) |
|--------|---------------------|---------------------------|
| Async runtime | `tokio::runtime::Runtime` singleton | No dedicated runtime; uses Swift's built-in cooperative pool |
| Dispatch mechanism | `runtime.block_on(async { ... })` | `DispatchGroup` + `Task { ... }` |
| Thread model | tokio uses its own thread pool | Swift Concurrency uses the cooperative thread pool (OS-provided on Apple, swift-corelibs on Linux) |
| Startup cost | Lazy tokio runtime init (~5 ms first call) | Zero startup cost; Swift Concurrency pool is always active |
| `@MainActor` risk | Not applicable (tokio is Rust-agnostic) | Deadlock risk for `@MainActor`-isolated functions |
| Opt-in async propagation | Not available in v1 | Available via `async-mode = "actor"` (phase 14) |

## Linux Swift Concurrency

Swift Concurrency on Linux uses `swift-corelibs-libdispatch` (an open-source reimplementation of Grand Central Dispatch). The `DispatchGroup` + `Task` pattern works identically on Linux and Apple platforms; the cooperative thread pool size defaults to the number of CPU cores.

One Linux-specific consideration: `swift-corelibs-libdispatch` on Linux does not support the global `DispatchQueue.main.sync` pattern (there is no main run loop). The bridge's `@MainActor` detection (`SkipReport` for `@MainActor`-isolated functions) ensures that Linux builds never attempt to use the main queue synchronously.

## Cross-references

- [[02-design-philosophy]] §4 for the rationale for choosing `DispatchGroup`-bounded `Task` as the default.
- [[05-type-mapping]] for how `async throws` functions are represented in the type table.
- [[09-abi-stability]] for the memory safety guarantees around the opaque handles passed across the async dispatch boundary.
- [[12-risks-and-alternatives]] §R8 for the `@MainActor` deadlock risk and the mitigation in depth.
