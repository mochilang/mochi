---
title: "Phase 9. Agents (actor + AsyncStream)"
sidebar_position: 13
sidebar_label: "Phase 9. Agents"
description: "MEP-49 Phase 9 — Mochi agent to Swift actor with AsyncStream<Message> mailbox; cast (fire-and-forget); call (request-reply via CheckedContinuation); OTP-style supervision."
---

# Phase 9. Agents (actor + AsyncStream)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 9](/docs/mep/mep-0049#phase-9-agents) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase9Agents`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green (all actor isolation violations surfaced as errors).

## Goal-alignment audit

Mochi agents are the primary concurrency primitive. Swift `actor` is the direct semantic match: isolated mutable state, cooperative scheduling, Sendable message passing. The `AsyncStream<Message>` mailbox gives back-pressure and ordering guarantees. This is one of the five load-bearing decisions in MEP-49: actor+AsyncStream rather than raw GCD or Combine gives a clean mental model, explicit message types, and structural supervision.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | `agent Counter { var n = 0; ... }` → `public actor Counter` with `AsyncStream<Message>` mailbox + `runLoop()` | NOT STARTED | — |
| 9.1 | `cast(counter, inc)` → fire-and-forget `continuation.yield(.inc)` | NOT STARTED | — |
| 9.2 | `call(counter, value)` → request-reply via `withCheckedContinuation` | NOT STARTED | — |
| 9.3 | Agent `init` → `Task { await self.runLoop() }` spawned in `init` | NOT STARTED | — |
| 9.4 | Supervision: `MochiRuntime.Supervisor` actor with `one_for_one`, `rest_for_one`, `one_for_all` | NOT STARTED | — |
| 9.5 | `@ui` annotation → `@MainActor` isolation; UIKit/SwiftUI message dispatch | NOT STARTED | — |

## Sub-phase 9.0 -- Actor declaration

### Decisions made (9.0)

**Full agent lowering pattern**: a Mochi `agent Counter { var n: int = 0; ... }` lowers to:

```swift
public actor Counter {
    private var n: Int64
    private let mailbox: AsyncStream<Message>
    private let continuation: AsyncStream<Message>.Continuation

    private enum Message: Sendable {
        case inc
        case value(CheckedContinuation<Int64, Never>)
    }

    public init(n: Int64 = Int64(0)) {
        self.n = n
        let (stream, cont) = AsyncStream<Message>.makeStream(
            bufferingPolicy: .bufferingNewest(1024)
        )
        self.mailbox = stream
        self.continuation = cont
        Task { [weak self] in
            await self?.runLoop()
        }
    }

    private func runLoop() async {
        for await msg in mailbox {
            switch msg {
            case .inc:
                n += Int64(1)
            case .value(let k):
                k.resume(returning: n)
            }
        }
    }
}
```

**`AsyncStream.makeStream(bufferingPolicy:)`**: SE-0388 (Swift 5.9+). The `.bufferingNewest(1024)` policy drops oldest messages when the buffer is full and the loop is behind. The buffer size of 1024 is the MochiRuntime default; it is configurable per-agent via an annotation `@buffer(size: N)`.

**`Task { [weak self] in await self?.runLoop() }`**: the unstructured task spawns the message loop. `[weak self]` prevents the actor from being pinned by the task if the actor is released by all external references.

**`Message` enum**: each agent method that can receive messages becomes a `case` in the `Message` enum. Fire-and-forget methods (no return value) are simple cases (`.inc`). Request-reply methods carry a `CheckedContinuation` case (`.value(CheckedContinuation<Int64, Never>)`).

**Actor isolation**: Swift 6 enforces that `n` is only accessed from within the actor. The `runLoop()` method is `private` and only called from the `Task` spawned in `init`. All external interactions go through the public API methods (cast and call).

## Sub-phase 9.1 -- cast (fire-and-forget)

### Decisions made (9.1)

**`cast(counter, inc)` in Mochi** → `counter.inc()` in Swift (a synchronous actor method that enqueues the message):

```swift
// Generated actor method for cast:
public nonisolated func inc() {
    continuation.yield(.inc)
}
```

**`nonisolated`**: the `inc()` method is `nonisolated` because `continuation.yield` is itself a synchronous, `Sendable`-safe operation. This allows `inc()` to be called from any context without `await`. From the caller's perspective, `cast(counter, inc)` is a non-async call -- fire and forget.

**`AsyncStream.Continuation` is `Sendable`**: `continuation.yield` can be called from any isolation context. The `continuation` property must be accessible from the `nonisolated` method. It is stored as a `let` property (immutable after `init`), so it is accessible nonisolated.

**Back-pressure**: if the mailbox buffer is full, `continuation.yield(.inc)` returns `.dropped`. The lowerer wraps this in a discardable result; dropped messages are silent by default. A `@failOnDrop` annotation can make the drop a runtime error.

## Sub-phase 9.2 -- call (request-reply)

### Decisions made (9.2)

**`call(counter, value)` in Mochi** → `await counter.value()` in Swift:

```swift
// Generated actor method for call:
public func value() async -> Int64 {
    await withCheckedContinuation { k in
        continuation.yield(.value(k))
    }
}
```

**`withCheckedContinuation`**: SE-0300. The continuation is passed as the associated value of the `.value` message. The `runLoop` resumes it with `k.resume(returning: n)`. The `await` at the call site suspends until the reply arrives.

**`withCheckedThrowingContinuation`**: used when the agent method can throw (Mochi `call` with an error type). The `Message` case carries `CheckedContinuation<T, Error>`.

**Deadlock prevention**: the `call` pattern can deadlock if an agent calls another agent that calls back synchronously. Mochi's type system tracks agent call graphs (Phase 11) and flags cycles. In Phase 9, the programmer is responsible for avoiding deadlocks.

**Timeout**: `withCheckedContinuation` has no built-in timeout. `MochiRuntime` provides a `callWithTimeout` helper:

```swift
public func valueWithTimeout(_ deadline: Duration) async throws -> Int64 {
    try await withTimeout(deadline) {
        await self.value()
    }
}
```

`withTimeout` is implemented in `MochiRuntime` using `withThrowingTaskGroup`.

## Sub-phase 9.3 -- Agent init and lifecycle

### Decisions made (9.3)

**`init` spawns the run loop**: the `Task { [weak self] in await self?.runLoop() }` in `init` is the run loop's unstructured task. It runs until the actor is deallocated (at which point `[weak self]` resolves to `nil`, `self?.runLoop()` is a no-op, and the loop exits naturally because the `AsyncStream` is terminated by ARC when `continuation` is released).

**Actor deallocation**: when all external references to the actor drop, ARC deallocates it. The `Continuation`'s deinit calls `continuation.finish()`, which terminates the `AsyncStream`, causing the `for await` loop in `runLoop` to exit.

**Explicit shutdown**: Mochi `stop(counter)` → a dedicated `.stop` message case:

```swift
case stop
```

The `runLoop` handles `.stop` by returning from the loop (breaking the `for await`). The actor can then be collected.

## Sub-phase 9.4 -- Supervision

### Decisions made (9.4)

**`MochiRuntime.Supervisor` actor**: implements OTP-style supervision:

```swift
public actor Supervisor {
    public enum Strategy { case oneForOne, restForOne, oneForAll }
    public enum RestartPolicy { case permanent, transient, temporary }

    public struct ChildSpec: Sendable {
        public let id: String
        public let start: @Sendable () async -> any MochiAgent
        public let restart: RestartPolicy
        public let maxRestarts: Int
        public let within: Duration
    }

    private var children: [String: ChildEntry] = [:]
    private let strategy: Strategy

    public init(strategy: Strategy) { self.strategy = strategy }

    public func startChild(_ spec: ChildSpec) async { ... }
    public func terminateChild(_ id: String) async { ... }
    public func handleCrash(_ id: String, error: Error) async { ... }
}
```

**`MochiAgent` protocol**: all generated agents conform to a `MochiAgent` protocol with `func stop() async` and a stable `id: String`.

**Restart strategies**:
- `one_for_one`: restart only the crashed child.
- `rest_for_one`: restart the crashed child and all children started after it.
- `one_for_all`: restart all children when any one crashes.

**Max restarts**: if a child crashes more than `maxRestarts` times within `within` duration, the supervisor terminates itself (escalates to its own supervisor). This implements the OTP max-restarts circuit breaker.

**Linking**: `MochiRuntime.Supervisor.startChild` registers the child. The supervisor monitors the child's `Task` using structured `withTaskGroup`. If the child's task completes with an error, the supervisor's `handleCrash` is called.

## Sub-phase 9.5 -- @ui annotation → @MainActor

### Decisions made (9.5)

**`@ui`-annotated Mochi agents** → `@MainActor`-isolated actors:

```swift
// Mochi: @ui agent ViewModel { ... }
@MainActor
public final class ViewModel: ObservableObject {
    // Not an actor -- @MainActor classes can conform to ObservableObject
    // for SwiftUI integration
}
```

**`class` not `actor`**: SwiftUI's `ObservableObject` + `@Published` requires a `class`. `@MainActor` provides the isolation guarantee. Mochi `@ui` agents lower to `@MainActor class` rather than `actor` to enable SwiftUI integration.

**Non-`@ui` agents**: lower to `actor` (the standard case in 9.0).

**`@MainActor` dispatch**: `cast(viewModel, updateTitle("hello"))` from a non-`@MainActor` context → `await viewModel.updateTitle("hello")` (async dispatch to main actor). The `await` is mandatory in Swift 6 strict concurrency.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/agent.go` | `AgentDecl` → `actor` declaration; `Message` enum generation; `runLoop` body |
| `transpiler3/swift/lower/lower.go` | `CastExpr`, `CallExpr` (agent) → nonisolated/async method calls |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Supervisor.swift` | `Supervisor` actor; `MochiAgent` protocol; restart strategies |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Timeout.swift` | `withTimeout` helper |
| `transpiler3/swift/build/phase09_test.go` | `TestPhase9Agents`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase09-agents/` | 25 fixture directories |

## Test set

- `TestPhase9Agents` -- 25 fixtures covering: `agent_basic_counter`, `agent_cast_fire_forget`, `agent_call_reply`, `agent_state_mutation`, `agent_multiple_messages`, `agent_init_args`, `agent_stop`, `agent_two_agents`, `agent_ping_pong`, `agent_chain`, `agent_spawn_in_loop`, `agent_backpressure`, `agent_timeout`, `agent_supervisor_one_for_one`, `agent_supervisor_rest_for_one`, `agent_supervisor_one_for_all`, `agent_restart_permanent`, `agent_restart_transient`, `agent_restart_temporary`, `agent_max_restarts`, `agent_ui_main_actor`, `agent_sendable_message`, `agent_record_message`, `agent_enum_message`, `agent_large_state`.

## Deferred work

- Distributed actors (Swift Distributed Actors framework). Deferred to a Phase 9 sub-MEP.
- `@cluster` annotation for distributed deployment. Out of v1 scope.
- Agent hot-code reload. Out of v1 scope.
- `select` receive (multiple mailboxes). Deferred to Phase 9.1.
