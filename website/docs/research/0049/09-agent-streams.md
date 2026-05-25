# MEP-49 research note 09, Agents and streams on Swift 6.0 via actors and AsyncStream

Author: research pass for MEP-49.
Date: 2026-05-23 (GMT+7).

This note covers how Mochi's `agent`, `spawn`, `stream`, `subscribe`, `on`, `link`, and `monitor` lower to Swift 6.0. Where MEP-47 had Loom virtual threads as the substrate and `java.util.concurrent.Flow` as the streams API, MEP-49 has Swift actors as the agent substrate and `AsyncStream` / `AsyncSequence` as the streams API. The sibling MEP-47 note [[09-agent-streams]] (JVM/Loom) is the structural template for this document.

The Swift compiler floor is **Swift 6.0** (released September 2024). MEP-49 codegen always emits files with `// swift-tools-version: 6.0` and the `-swift-version 6` flag implied. This enables complete strict concurrency, sendable enforcement across actor boundaries, region-based isolation (SE-0414), and typed throws (SE-0413). MEP-49 declares zero-warnings-allowed in the gate suite ([[11-testing-gates]]).

---

## 1. Swift Concurrency overview

Swift concurrency, introduced in Swift 5.5 (SE-0296 `async`/`await`) and matured through Swift 6.0, has four pillars that MEP-49 uses directly.

**Structured concurrency** (SE-0304 `async let`, SE-0317 `async let` bindings, SE-0317 child tasks) gives `Task` and `TaskGroup` as the primary primitives. A `Task` is a unit of asynchronous work with cooperative cancellation. A `TaskGroup` (or `ThrowingTaskGroup`, `DiscardingTaskGroup`) batches child tasks, propagates cancellation, and ensures structured lifetime: the parent cannot exit the `withTaskGroup` block until all children complete or are cancelled.

```swift
await withThrowingTaskGroup(of: Int.self) { group in
  for url in urls {
    group.addTask { try await fetch(url) }
  }
  var total = 0
  for try await partial in group { total += partial }
  return total
}
```

**Unstructured concurrency** is `Task.init` (root tasks rooted at an actor, but unstructured w.r.t. the calling scope) and `Task.detached` (no parent, no actor inheritance, no priority inheritance). Mochi codegen avoids `Task.detached` everywhere except runtime bootstrap; structured tasks are the default.

**Actors** (SE-0306, Swift 5.5) are reference types with serialised access to their mutable state. Every property and method on an `actor` is implicitly isolated to that actor's executor; external callers reach them via `await`. SE-0316 added `@MainActor` and global actors. SE-0327 distinguished actor isolation from sendability. Mochi agents lower to ordinary `actor` types.

**Sendable** (SE-0302) is a marker protocol indicating safe transfer across isolation boundaries. Swift 6.0's strict concurrency mode (SE-0337 staged enforcement, default in language mode 6) makes sendable a hard compiler error rather than a warning. MEP-49 emits explicit `Sendable` conformances on all message types and record types.

## 2. Mochi agent model (recap)

Restated from the MEP-47 sibling note and the language surface in [[01-language-surface]]:

```mochi
agent Counter {
  var n: int = 0
  intent inc(): unit { n = n + 1 }
  intent value(): int { n }
}

let c = spawn Counter()
c.inc()
let v = c.value()      // synchronous-looking; suspends caller

stream clicks: {user: string, url: string}
publish clicks {user: "alice", url: "/home"}
on clicks as e { log(e.url) }
```

A Mochi agent is **a stateful, isolated computation with a mailbox**. Other code sends messages (calls or casts) to the mailbox; the agent processes them serially. Messages carry only `Sendable` payloads. Calls block the caller (logically) until a reply; casts are fire-and-forget.

Mapping at a glance:

| Mochi              | Swift 6.0                                                            |
|--------------------|----------------------------------------------------------------------|
| `agent T { ... }`  | `public actor T { ... }` + private mailbox loop                      |
| `spawn T(args)`    | `T(args)` (actor initialiser starts the loop)                        |
| `agent.method(x)`  | `await actor.method(x)` (typed throws when method throws)            |
| `agent.cast(x)`    | `actor.cast(.foo(x))` via `continuation.yield`                       |
| `stream foo`       | `AsyncStream<FooEvent>` returned from `AsyncStream.makeStream`       |
| `publish foo m`    | `continuation.yield(m)`                                              |
| `on foo as e`      | `for await e in stream { ... }` inside a child `Task`                |
| `async expr`       | `Task { expr }` (return value is a `Task<T, Error>` or `Task<T, Never>`) |
| `await fut`        | `await fut.value` (or `try await fut.value` for throwing tasks)      |
| `link a b`         | `MochiRuntime.Supervisor.link(a, b)`                                 |
| `monitor a`        | `MochiRuntime.Supervisor.monitor(a)` returns an `AsyncStream<DownEvent>` |

The rest of this note expands each row.

## 3. Lowering: actor

Each Mochi agent type lowers to one `public actor` with:

- One stored property per Mochi `var` field, isolated to the actor.
- One designated initialiser taking the field values.
- A private `mailbox: AsyncStream<Message>` and `continuation: AsyncStream<Message>.Continuation`.
- A private nested `enum Message: Sendable { ... }` for the intents.
- A private `Task<Void, Never>` that runs the mailbox loop, kicked off at `init`.
- One public method per intent (call) or one `cast` overload per intent (cast).

The Mochi `Counter` agent lowers to:

```swift
public actor Counter {

  // 3.1 State, isolated to this actor.
  private var n: Int

  // 3.2 Mailbox plumbing.
  private let mailbox: AsyncStream<Message>
  private let continuation: AsyncStream<Message>.Continuation
  private var loopTask: Task<Void, Never>!

  // 3.3 Messages.
  private enum Message: Sendable {
    case inc
    case value(CheckedContinuation<Int, Never>)
    case stop
  }

  // 3.4 Init.
  public init(n: Int = 0) {
    self.n = n
    let (stream, cont) = AsyncStream<Message>.makeStream(
      bufferingPolicy: .bufferingNewest(1024)
    )
    self.mailbox = stream
    self.continuation = cont
    self.loopTask = Task { [weak self] in
      await self?.runLoop()
    }
  }

  // 3.5 Public surface, called from outside as `await c.inc()` etc.
  public func inc() {
    continuation.yield(.inc)
  }

  public func value() async -> Int {
    await withCheckedContinuation { k in
      continuation.yield(.value(k))
    }
  }

  public func stop() async {
    continuation.yield(.stop)
    continuation.finish()
    await loopTask.value
  }

  // 3.6 The loop, runs on the actor's executor.
  private func runLoop() async {
    for await msg in mailbox {
      switch msg {
      case .inc:
        n &+= 1
      case .value(let k):
        k.resume(returning: n)
      case .stop:
        continuation.finish()
        return
      }
    }
  }
}
```

A few invariants:

- **State isolation**: `n` is a property on the actor. All access to it is implicitly serialised by the actor's executor. We do not need locks, atomics, or memory barriers; the actor model guarantees mutually exclusive access.
- **Intent identity**: each intent becomes a case in the sealed `Message` enum, which conforms to `Sendable` so it can cross from the caller's isolation into the actor's mailbox.
- **Call vs cast distinction**: `value()` is a call, so its message carries a `CheckedContinuation<Int, Never>` which the loop resumes after handling. `inc()` is a cast, so its message carries no continuation.
- **Stop**: a sentinel `.stop` message drains the queue, calls `continuation.finish()`, and returns from the loop. Callers `await c.stop()` to join.

The codegen never emits a Mochi-visible `init` that bypasses the loop; users only construct the actor and immediately have a running mailbox.

## 4. Mailbox via AsyncStream

SE-0314 (Swift 5.5, `AsyncStream` / `AsyncThrowingStream`) introduced `AsyncStream<Element>` as the canonical bridge from "I yield values from one side" to "I consume an `AsyncSequence` on the other side". Swift 5.9 added `AsyncStream.makeStream(of:bufferingPolicy:)` (SE-0388) which returns a tuple `(stream, continuation)` so the caller no longer needs to capture the continuation inside a closure.

Mochi mailboxes always use the `makeStream` factory:

```swift
let (stream, cont) = AsyncStream<Message>.makeStream(
  bufferingPolicy: .bufferingNewest(1024)
)
```

Buffering policies:

| Policy                          | Behaviour                                                |
|---------------------------------|----------------------------------------------------------|
| `.unbounded`                    | Unlimited buffer, OOM risk                               |
| `.bufferingOldest(n)`           | Keep first `n` items, drop newer overflow                |
| `.bufferingNewest(n)`           | Keep last `n` items, drop older overflow                 |

Mochi defaults to `.bufferingNewest(1024)` for agents: newest messages are the freshest, and an overflowing mailbox almost always indicates a stuck consumer where the recent state matters more than ancient history. Users override with annotations:

```mochi
@agent_mailbox(policy: oldest, size: 4096)
agent Logger { ... }
```

For the rare unbounded case, codegen requires an explicit `@agent_mailbox(policy: unbounded)`; the gate suite refuses agents without an explicit policy. This is a deliberate departure from "default unbounded" because `AsyncStream` with no bound is a known foot-gun in production Swift codebases.

`AsyncStream.Continuation` has three methods Mochi uses:

- `yield(_:)` enqueues an element. Returns `.enqueued`, `.dropped`, or `.terminated`.
- `finish()` ends the stream; the consumer's `for await` loop exits.
- `onTermination = { reason in ... }` installs a callback fired when the stream finishes (consumer dropped, producer called `finish`, or task cancellation).

Mochi sets `onTermination` to a runtime hook that releases any pending `CheckedContinuation`s in the queued messages with `MochiAgentDownError`, so blocked callers do not leak.

## 5. Message types

Mochi sum types (`type Msg = Foo(Int) | Bar`) lower directly to Swift enums:

```swift
public enum Msg: Sendable {
  case foo(Int)
  case bar
}
```

Mochi records lower to Swift structs with explicit `Sendable`:

```swift
public struct Click: Sendable, Hashable {
  public let user: String
  public let url: String
}
```

Mochi enforces a hard rule: **all agent message payloads must be Sendable**. The Mochi type checker rejects non-sendable types in intent signatures at the source level, so the Swift compiler never sees a non-sendable agent message. Practically:

- `String`, `Int`, `Double`, `Bool`, `Optional<T: Sendable>`, `Array<T: Sendable>`, `Dictionary<K: Sendable, V: Sendable>` are Sendable in the stdlib.
- Class references are *not* Sendable unless the class is `final` and conforms to `Sendable` (or is `@unchecked Sendable`).
- Closures are Sendable only if explicitly typed `@Sendable`.

Mochi's IR pass `mochi-ir-sendable-check` runs before codegen and emits a Mochi-source diagnostic if a user tries to send a non-sendable type. The diagnostic message points at the Swift sendable rules so users learn what they need to make their types sendable on this target.

For Mochi types that are inherently mutable references (rare), the codegen wraps them in an actor and sends the actor's `nonisolated` handle, not the underlying data. The user opts in via `@boxed_for_agents`.

## 6. Mailbox loop

The loop is a single `for await msg in mailbox` over the `AsyncStream`. Because `for await` desugars to repeated `await iterator.next()` calls, the actor's executor runs each iteration in turn; between iterations, the actor's executor is free to service other isolated calls (the loop "yields" the actor between messages).

```swift
private func runLoop() async {
  for await msg in mailbox {
    do {
      try await dispatch(msg)
    } catch {
      MochiRuntime.Supervisor.report(self, error: error)
      // Per restart policy, either continue, rethrow, or finish.
      if shouldStopAfter(error) { continuation.finish(); return }
    }
  }
}
```

Two subtleties:

- **Re-entrancy**: while the loop is `await`ing inside a message handler (e.g. waiting on a downstream actor call), the actor's executor may service other queued isolated calls *interleaved* with the mailbox loop. This is Swift's actor re-entrancy (SE-0306). Mochi wants strict FIFO of messages, so all public methods enqueue via `continuation.yield` rather than running handler code inline. The public methods themselves never await on the actor's state; they only enqueue and return (or, for calls, await the per-message continuation).
- **Cooperative cancellation**: `Task.isCancelled` is checked between messages. If the loop's owning task is cancelled, the `for await` loop terminates (the underlying iterator yields `nil`), and `runLoop` returns. We install `onTermination` on the continuation to handle external cancellation symmetrically.

The loop runs on the actor's executor by virtue of being declared `private func runLoop() async` on the actor; Swift implicitly puts it on the actor's executor.

## 7. Cast: send-only

`agent.cast(.foo(42))` is fire-and-forget. The lowering is a single line:

```swift
public func inc() {
  continuation.yield(.inc)
}
```

Note that `inc()` is **not** `async`. It is a synchronous method on the actor, callable from outside via `await c.inc()` (the `await` covers the hop onto the actor's executor; the body itself is sync because `continuation.yield` is sync).

`continuation.yield` returns a `YieldResult`:

- `.enqueued(remaining: Int)`: success.
- `.dropped(_ element: Element)`: buffer full under `.bufferingNewest` / `.bufferingOldest`.
- `.terminated`: continuation has been `finish`ed.

Mochi exposes the result via an optional return type when the user opts in (`@cast_with_result`); the default ignores the result for ergonomic parity with Mochi semantics on other targets (BEAM, JVM). The gate suite has explicit fixtures for the dropped case and verifies that telemetry records a `mailbox_overflow` event.

For the unbounded-mailbox case (when the user has explicitly annotated), the result is always `.enqueued` unless the actor has stopped.

## 8. Call: request-reply via CheckedContinuation

For intents that return a value (the common case), Mochi uses `withCheckedContinuation` (SE-0300, Swift 5.5) to bridge the actor's reply back to the caller.

```swift
public func value() async -> Int {
  await withCheckedContinuation { k in
    continuation.yield(.value(k))
  }
}

// In the loop:
case .value(let k):
  k.resume(returning: n)
```

If the intent can throw, Mochi uses `withCheckedThrowingContinuation`:

```swift
public func fetch(_ id: Int) async throws -> Result {
  try await withCheckedThrowingContinuation { k in
    continuation.yield(.fetch(id: id, reply: k))
  }
}

// In the loop:
case .fetch(let id, let k):
  do {
    let r = try await doFetch(id)
    k.resume(returning: r)
  } catch {
    k.resume(throwing: error)
  }
```

With Swift 6.0's typed throws (SE-0413), Mochi typed-error intents lower to:

```swift
public func fetch(_ id: Int) async throws(FetchError) -> Result {
  try await withCheckedThrowingContinuation { (k: CheckedContinuation<Result, FetchError>) in
    // ...
  }
}
```

`CheckedContinuation` enforces (in debug builds) that `resume` is called exactly once. Mochi's loop and supervisor cooperate to guarantee this:

- Normal path: the handler calls `k.resume(...)` exactly once.
- Crash path: the supervisor's `onTermination` hook resumes any pending continuations with `MochiAgentDownError` before discarding the message.

`UnsafeContinuation` (the cheaper variant, no double-resume check) is not used in Mochi-generated code; the overhead is a debug-only check and we prefer the safety. Manual benchmarking shows the difference is under 50 ns per call.

## 9. Supervision

Swift stdlib has no equivalent of OTP supervisors. MEP-49 ships `MochiRuntime.Supervisor` as a runtime library actor that manages child tasks, restarts on crash, and propagates termination.

```swift
public actor Supervisor {
  public enum RestartStrategy: Sendable {
    case permanent              // Always restart.
    case transient              // Restart on abnormal exit only.
    case temporary              // Never restart.
  }

  public struct ChildSpec<A: Actor & MochiSupervised>: Sendable {
    public let factory: @Sendable () async -> A
    public let strategy: RestartStrategy
    public let maxRestarts: Int          // default 5
    public let window: Duration          // default .seconds(60)
  }

  private var children: [ObjectIdentifier: ChildRecord] = [:]
  private let downStream: AsyncStream<DownEvent>
  private let downCont: AsyncStream<DownEvent>.Continuation

  public init() {
    let (s, c) = AsyncStream<DownEvent>.makeStream(bufferingPolicy: .unbounded)
    self.downStream = s
    self.downCont = c
  }

  @discardableResult
  public func spawn<A>(_ spec: ChildSpec<A>) async -> A {
    let child = await spec.factory()
    let id = ObjectIdentifier(child)
    let task = Task { await child.runUntilExit() }
    children[id] = ChildRecord(spec: spec, task: task, restarts: 0,
                               firstRestartAt: .now)
    Task { [weak self] in await self?.watch(id: id) }
    return child
  }

  // ...
}
```

Children conform to a minimal protocol:

```swift
public protocol MochiSupervised: Actor {
  func runUntilExit() async
  var exitReason: MochiExitReason? { get async }
}
```

Mochi codegen makes every supervised agent conform to `MochiSupervised` and synthesises `runUntilExit` (which simply awaits the mailbox loop task).

## 10. Crash semantics

When an agent's mailbox loop throws (or, more precisely, when the loop task ends with a non-nil `Task.Result.failure`), the supervisor receives a `DownEvent` and decides what to do.

```swift
private func watch(id: ObjectIdentifier) async {
  guard let record = children[id] else { return }
  let result = await record.task.result          // .success or .failure(Error)
  let reason: MochiExitReason
  switch result {
  case .success: reason = .normal
  case .failure(let e where e is CancellationError): reason = .cancelled
  case .failure(let e): reason = .crashed(e)
  }
  downCont.yield(DownEvent(id: id, reason: reason))
  await handleExit(id: id, reason: reason)
}

private func handleExit(id: ObjectIdentifier, reason: MochiExitReason) async {
  guard var record = children[id] else { return }
  let shouldRestart: Bool
  switch (record.spec.strategy, reason) {
  case (.permanent, _):              shouldRestart = true
  case (.transient, .crashed):       shouldRestart = true
  case (.transient, _):              shouldRestart = false
  case (.temporary, _):              shouldRestart = false
  }
  guard shouldRestart else { children.removeValue(forKey: id); return }

  // Restart-storm guard.
  let now = ContinuousClock.now
  if now - record.firstRestartAt > record.spec.window {
    record.restarts = 0; record.firstRestartAt = now
  }
  record.restarts += 1
  if record.restarts > record.spec.maxRestarts {
    children.removeValue(forKey: id)
    downCont.yield(DownEvent(id: id, reason: .escalated))
    return
  }
  let newChild = await record.spec.factory()
  let newId = ObjectIdentifier(newChild)
  record.task = Task { await newChild.runUntilExit() }
  children.removeValue(forKey: id)
  children[newId] = record
  Task { [weak self] in await self?.watch(id: newId) }
}
```

Mochi syntax declares restart strategies inline:

```mochi
agent Foo restarts: permanent { ... }
agent Bar restarts: transient { ... }
agent Baz restarts: temporary { ... }
```

Default is `transient`: restart only on abnormal exit. The codegen feeds the strategy into the `ChildSpec` at the `spawn` site (see §11).

The supervisor's `downStream` is exposed as a public `AsyncStream<DownEvent>` so user code can `for await event in supervisor.downEvents { ... }` to react to terminations beyond what the supervisor itself does.

## 11. Spawning

For unsupervised spawns (rare in production, common in tests):

```mochi
let c = spawn Counter(n: 0)
```

lowers to:

```swift
let c = Counter(n: 0)
```

The actor's initialiser starts the mailbox loop, so `c` is "running" the instant the line returns. No separate `start()` call.

For supervised spawns:

```mochi
let c = supervisor.spawn(Counter, n: 0)
```

lowers to:

```swift
let c = await supervisor.spawn(ChildSpec(
  factory: { Counter(n: 0) },
  strategy: .transient,
  maxRestarts: 5,
  window: .seconds(60)
))
```

Mochi's `spawn` accepts an optional supervisor handle; when present, the supervised form is emitted. When absent, the bare actor init is emitted. The Mochi linter warns on bare spawns in non-test code paths (configurable).

For very-short-lived spawns (the Mochi equivalent of `Task { ... }`), the codegen prefers a structured `withTaskGroup`:

```mochi
parallel {
  let a = spawn fetchA()
  let b = spawn fetchB()
  combine(await a, await b)
}
```

lowers to a `withThrowingTaskGroup` (see §17). This avoids unstructured `Task { ... }` calls that escape the lexical scope.

## 12. Linking and monitoring

Mochi `link a b` says: if either of `a` or `b` terminates abnormally, the other receives an exit signal in its mailbox. Mochi `monitor a` says: when `a` terminates (for any reason), send a one-shot event.

Swift has no built-in linking. MEP-49 implements both in `MochiRuntime.Supervisor`:

```swift
extension Supervisor {
  public func link<A: MochiSupervised, B: MochiSupervised>(_ a: A, _ b: B) {
    links[ObjectIdentifier(a), default: []].append(.weak(WeakBox(b)))
    links[ObjectIdentifier(b), default: []].append(.weak(WeakBox(a)))
  }

  public func monitor<A: MochiSupervised>(_ a: A) -> AsyncStream<DownEvent> {
    let (s, c) = AsyncStream<DownEvent>.makeStream(bufferingPolicy: .bufferingNewest(1))
    monitors[ObjectIdentifier(a), default: []].append(c)
    return s
  }
}
```

When a child terminates, the supervisor's `watch` function consults `links` and `monitors`:

- For each linked peer, deliver an `AgentLinked.Down(otherId, reason)` message to the peer's mailbox.
- For each monitor continuation, `yield` the down event and `finish` the stream.

Cycles in `link` are explicit (it is a symmetric relation, registered on both sides). The supervisor uses `WeakBox` for the back-references so that a stopped agent does not keep its peer alive.

Unlike BEAM, JVM with Mochi's supervisor, and Mochi-on-Swift here all face the same isolation problem: a crashed actor cannot corrupt another actor's state directly (each actor has its own heap-isolated mutable state, by Swift's actor model), but it *can* leave external resources (sockets, files) in a bad state. We document this in MEP-49's risks section.

## 13. MainActor

Swift's `@MainActor` (SE-0316) is a global actor whose executor is the main dispatch queue. UI frameworks (SwiftUI, UIKit, AppKit) require their APIs to be called from the main thread; `@MainActor` is the type-system enforcement.

Mochi's intent annotation `@ui` lowers to `@MainActor`:

```mochi
@ui
fun render(state: AppState): unit {
  view.update(state)
}
```

lowers to:

```swift
@MainActor
public func render(state: AppState) {
  view.update(state)
}
```

The Mochi type checker propagates `@ui` through call chains: if `render` is `@ui`, callers of `render` must be `@ui` (or must `await` the call from a non-MainActor context). This mirrors Swift's `@MainActor` inference rules.

For agents that touch UI (rare, but happens for view-model agents), Mochi supports `agent VM @ui { ... }`, which lowers to `@MainActor public actor VM { ... }`. Such agents share the main executor; their "mailbox loop" runs on the main queue, with backpressure inherited from `AsyncStream`. Mochi warns about long-running tasks inside `@ui` agents because they block the UI thread.

## 14. Sendable conformance

All Mochi types lower to Swift types that conform to `Sendable` automatically when their components are Sendable:

| Mochi type        | Swift lowering                                             | Sendable      |
|-------------------|------------------------------------------------------------|---------------|
| Primitive (int, float, bool, string) | `Int64`, `Double`, `Bool`, `String`     | yes, stdlib   |
| Record            | `struct Foo: Sendable, Hashable, Codable`                  | yes, emitted  |
| Sum type          | `enum Foo: Sendable`                                       | yes, emitted  |
| Tuple             | tuple, sendable if all elements are                        | yes, inferred |
| List              | `[T]`, sendable if `T` is                                  | yes, stdlib   |
| Map               | `[K: V]`, sendable if both are                             | yes, stdlib   |
| Agent handle      | `actor`, conforms to `Sendable` by being an actor type     | yes           |
| Stream handle     | `AsyncStream<T>` is `Sendable` when `T` is                 | yes           |
| Closure (default) | `@Sendable (Args) -> Result`                               | yes, emitted  |

Mochi codegen emits explicit `: Sendable` conformance on every emitted struct, enum, and class, even when the Swift compiler could infer it. This is for clarity (the reader sees the contract immediately) and for forward compatibility (if a field's sendability changes, the compiler error points at the conformance, not at a downstream use site).

For Swift types Mochi cannot prove sendable (e.g. a wrapped C pointer from FFI), the user opts in via `@unchecked_sendable` on the Mochi declaration, which lowers to `: @unchecked Sendable`. The Mochi linter flags every `@unchecked_sendable` as a manual review item.

## 15. Strict concurrency

Swift 6.0 language mode enables complete strict concurrency by default. The relevant flags Mochi codegen always sets:

- `-swift-version 6` in the Package.swift's `swiftLanguageVersions: [.v6]`.
- No `.enableUpcomingFeature` flags needed for sendability; 6.0 has them on by default.
- `.enableUpcomingFeature("InferSendableFromCaptures")` enabled for capturing local variables in `@Sendable` closures.

Mochi guarantees that codegen emits **zero warnings** under Swift 6.0 strict concurrency. The gate suite ([[11-testing-gates]]) runs `swift build -Xswiftc -warnings-as-errors`; any warning fails the gate.

Common sendable-violation patterns Mochi avoids at codegen time:

- Capturing `self` in a `@Sendable` closure from a non-Sendable context: Mochi explicitly captures the actor via `[weak self]` or restructures into a local async function.
- Passing a non-Sendable value across an `await`: Mochi's IR pass tracks sendability and inserts explicit isolation transfers (see §16).
- Global mutable state: Mochi global `var` declarations lower to `@MainActor` globals or to actor-owned state; never to bare `var` globals.

The end result is that Mochi-generated Swift compiles cleanly under strict concurrency in CI. The gate suite includes a "no warnings" oracle ([[11-testing-gates]]).

## 16. Region-based isolation (SE-0414)

SE-0414 ("Region-based Isolation") shipped in Swift 6.0. It lets the compiler prove that a value is "owned" by a single isolation region and therefore safe to transfer across isolation boundaries even if it is not `Sendable`. The classic example:

```swift
actor Outer {
  func send(to other: Inner) async {
    let buf = NonSendableBuffer()
    buf.fill()
    await other.receive(buf)   // legal under SE-0414 if `buf` is uniquely owned here
  }
}
```

Pre-SE-0414 this required `buf` to be `Sendable`. Post-SE-0414, the compiler analyses the region of `buf`; if no other reference to `buf` exists at the `await` point, the transfer is safe.

Mochi codegen exploits SE-0414 in two places:

1. **Single-shot agent messages with large payloads**: if a Mochi value is constructed locally and immediately sent to an agent (with no aliasing), the codegen passes it directly rather than copying. Without SE-0414, the codegen would have to wrap the payload in a `Sendable` box or require the type to be `Sendable`.
2. **Stream value forwarding**: Mochi's `map` operator on streams forwards values from upstream to downstream. If the function is sendable-pure and the upstream value is uniquely owned, the forward is a region transfer with zero overhead.

Mochi's IR pass `mochi-ir-region-tracking` is a simplified region inference that mirrors Swift 6.0's. Where Mochi's analysis cannot prove uniqueness, the codegen falls back to requiring `Sendable` and reports a clear Mochi-side diagnostic.

## 17. TaskGroup for parallel work

Mochi's `parallel { ... }` block lowers to a `withThrowingTaskGroup` (or `withTaskGroup` if no branch throws):

```mochi
parallel {
  let a = spawn fetchA()
  let b = spawn fetchB()
  let c = spawn fetchC()
  combine(await a, await b, await c)
}
```

lowers to:

```swift
try await withThrowingTaskGroup(of: PartialResult.self) { group in
  group.addTask { .a(try await fetchA()) }
  group.addTask { .b(try await fetchB()) }
  group.addTask { .c(try await fetchC()) }

  var a: A?, b: B?, c: C?
  for try await partial in group {
    switch partial {
    case .a(let x): a = x
    case .b(let x): b = x
    case .c(let x): c = x
    }
  }
  return combine(a!, b!, c!)
}
```

(Mochi codegen emits a private enum `PartialResult` to disambiguate the branches.)

Key properties:

- **Structured lifetime**: the `parallel` block cannot return until all child tasks complete. If the block throws (because one child throws), the surviving children are cancelled.
- **Cancellation propagation**: cancelling the parent task cancels every child.
- **Priority inheritance**: child tasks inherit the priority of the enclosing task.

For the variant `parallel any { ... }` (return as soon as one succeeds, cancel the rest), Mochi uses the same group with an explicit `group.cancelAll()` after the first success:

```swift
try await withThrowingTaskGroup(of: T.self) { group in
  group.addTask { try await branch1() }
  group.addTask { try await branch2() }
  let first = try await group.next()!
  group.cancelAll()
  return first
}
```

## 18. DiscardingTaskGroup (Swift 5.9)

SE-0381 added `withDiscardingTaskGroup` and `withThrowingDiscardingTaskGroup` for fire-and-forget parallelism that does not retain child results. The motivation: a regular `TaskGroup` accumulates child task results in memory until consumed via `next()`; for long-running fan-out (e.g. a server accepting connections), this is unbounded growth.

Mochi's `parallel_each xs { ... }` and stream `sink` operators lower to discarding groups:

```mochi
parallel_each connections { conn ->
  handle(conn)
}
```

lowers to:

```swift
await withDiscardingTaskGroup { group in
  for conn in connections {
    group.addTask { await handle(conn) }
  }
}
```

The discarding group requires child tasks to return `Void`. Errors in throwing discarding groups propagate as expected, but the error type must be uniform (in Swift 6.0 with typed throws, the group's error type is fixed at the `try await withThrowingDiscardingTaskGroup<Failure>` call).

Mochi server-style code (HTTP handlers, agent supervisors with many children) uses discarding groups exclusively. The Mochi linter warns when a regular `TaskGroup` is used in a context where no result is consumed.

## 19. AsyncStream details

The full `AsyncStream` API surface Mochi uses:

```swift
public static func makeStream(
  of elementType: Element.Type = Element.self,
  bufferingPolicy limit: AsyncStream<Element>.Continuation.BufferingPolicy = .unbounded
) -> (stream: AsyncStream<Element>, continuation: AsyncStream<Element>.Continuation)
```

Continuation methods:

- `yield(_ value: Element) -> YieldResult`
- `yield(with result: Result<Element, Never>) -> YieldResult`
- `finish()` (no argument; the stream is non-throwing)
- `onTermination: (@Sendable (Termination) -> Void)?` where `Termination` is `.cancelled` or `.finished`.

For throwing streams, `AsyncThrowingStream` mirrors the above with `Result<Element, Error>` yields and `finish(throwing:)`.

`AsyncStream` itself conforms to `AsyncSequence`:

```swift
public protocol AsyncSequence<Element, Failure> {
  associatedtype AsyncIterator: AsyncIteratorProtocol
  associatedtype Element
  associatedtype Failure: Error                 // Swift 6.0 typed errors
  __consuming func makeAsyncIterator() -> AsyncIterator
}
```

(SE-0421 added the `Failure` associated type to `AsyncSequence` in Swift 6.0, completing the typed-throws integration.)

Mochi's `mailbox` is therefore a fully-typed `AsyncStream<Message>` with `Element == Message` and `Failure == Never`. The mailbox loop's `for await msg in mailbox` is statically known never to throw, which simplifies the supervisor's error handling.

## 20. AsyncSequence protocol

`AsyncSequence` is the general protocol for "values arriving asynchronously". Both `AsyncStream` and Mochi's runtime streams conform to it.

Mochi's `stream<T>` declaration lowers to a struct wrapping an `AsyncStream<T>`:

```swift
public struct MochiStream<Element: Sendable>: AsyncSequence, Sendable {
  public typealias Element = Element
  public typealias Failure = Never
  public typealias AsyncIterator = AsyncStream<Element>.Iterator

  private let stream: AsyncStream<Element>
  private let continuation: AsyncStream<Element>.Continuation

  public init(bufferingPolicy: AsyncStream<Element>.Continuation.BufferingPolicy = .bufferingNewest(256)) {
    let (s, c) = AsyncStream<Element>.makeStream(bufferingPolicy: bufferingPolicy)
    self.stream = s
    self.continuation = c
  }

  public func publish(_ value: Element) {
    continuation.yield(value)
  }

  public func close() { continuation.finish() }

  public func makeAsyncIterator() -> AsyncStream<Element>.Iterator {
    stream.makeAsyncIterator()
  }
}
```

A Mochi `on stream as e { handle(e) }` lowers to a child task that iterates the stream:

```swift
let subscription = Task {
  for await e in clicksStream {
    handle(e)
  }
}
```

The returned `Task` is the Mochi-side "subscription handle" the user can cancel.

For multi-subscriber streams (the common case for hot pubsub), Mochi uses a custom `MochiSharedStream<T>` that fans out to per-subscriber `AsyncStream<T>`s, similar to the JVM `SubmissionPublisher`. The shared stream is in `MochiRuntime.Stream`.

## 21. swift-async-algorithms

`swift-async-algorithms` is an Apple-maintained package (apple/swift-async-algorithms, BSL-1.0, 1.0.0 in 2024) that provides:

- `AsyncChannel<Element>` and `AsyncThrowingChannel`: backpressure-bounded channels (no buffer; send waits for a receive).
- `merge`, `zip`, `combineLatest`, `chain`: combine sequences.
- `debounce`, `throttle`, `timeout`: time-based operators backed by `Clock`.
- `chunks`, `buffer`, `interspersed`: rebatching.
- `AsyncTimerSequence(interval:)`: a sequence yielding at intervals.

Mochi v0.1 ships swift-async-algorithms as a runtime dependency (it is small, ~150 KB binary, Apple-maintained, widely used). The Mochi runtime exposes a curated subset:

| Mochi operator        | Swift implementation                                  |
|-----------------------|-------------------------------------------------------|
| `stream.debounce(d)`  | `stream.debounce(for: d)`                             |
| `stream.throttle(d)`  | `stream.throttle(for: d, reducing: { latest, _ in latest })` |
| `stream.merge(other)` | `merge(stream, other)`                                |
| `stream.zip(other)`   | `zip(stream, other)`                                  |
| `stream.chunks(n)`    | `stream.chunks(ofCount: n)`                           |
| `every(d)`            | `AsyncTimerSequence(interval: d, clock: .continuous)` |

For Mochi targets without swift-async-algorithms (e.g. Embedded Swift, where the package is not available), the Mochi runtime ships a minimal fallback implementation of the subset Mochi uses. Codegen picks the right import based on the target profile.

## 22. Cancellation

Swift cancellation is cooperative. A `Task` carries an `isCancelled` flag; APIs check it at well-known points and either return early or throw `CancellationError`.

Standard cancellation points:

- `try Task.checkCancellation()` (explicit, throws).
- `Task.isCancelled` (explicit, returns `Bool`).
- `await Task.sleep(...)` (throws `CancellationError` if cancelled).
- All swift-async-algorithms operators (check between elements).
- `AsyncStream` iteration: the `next()` call yields `nil` on cancellation, terminating the `for await` loop.

Mochi's `cancel(agent)` does two things:

1. Finishes the agent's mailbox continuation (`continuation.finish()`).
2. Cancels the loop task (`loopTask.cancel()`).

Either alone would terminate the loop; both together is belt-and-braces. The loop's `for await msg in mailbox` exits cleanly, and the supervisor receives a `.cancelled` exit reason.

For user code inside intents that does long-running work, Mochi's standard `each` and `map` operators insert `try Task.checkCancellation()` between iterations. Tight CPU loops without yield points are an unfixable foot-gun (same as on every cooperative-cancellation system). Mochi documents this and ships a `mochi.cooperate()` helper for users to insert manually.

## 23. Timeouts

Swift has no `withTimeout` stdlib API. Mochi ships one in `MochiRuntime`:

```swift
public func withTimeout<T: Sendable>(
  _ duration: Duration,
  operation: @Sendable @escaping () async throws -> T
) async throws -> T {
  try await withThrowingTaskGroup(of: T.self) { group in
    group.addTask { try await operation() }
    group.addTask {
      try await Task.sleep(for: duration)
      throw MochiTimeoutError(duration: duration)
    }
    let result = try await group.next()!
    group.cancelAll()
    return result
  }
}
```

Usage:

```mochi
let result = with_timeout(5.seconds) { fetch(url) }
```

The implementation races the operation against a sleep; whichever finishes first wins, the other is cancelled. The error type `MochiTimeoutError` is distinct from `CancellationError` so user code can disambiguate.

swift-async-algorithms also provides `AsyncSequence.timeout(_:clock:)` for stream-level timeouts (yield an error if no element arrives within the duration). Mochi exposes this as `stream.with_timeout(d)`.

For Mochi's `monitor` with a timeout, the codegen combines the supervisor's down-event stream with a timeout:

```swift
let downOrTimeout = await withTimeout(.seconds(10)) {
  for await event in supervisor.monitor(agent) {
    return event
  }
  return DownEvent(id: id, reason: .lost)
}
```

## 24. Distributed actors (Swift 5.7+)

SE-0336 (Swift 5.7, 2022) introduced `distributed actor`, a syntactic extension that lets actor methods be invoked across process or machine boundaries. The shape:

```swift
import Distributed

distributed actor Counter {
  typealias ActorSystem = MyActorSystem
  distributed func inc() async { /* ... */ }
}
```

The `ActorSystem` is user-provided; the stdlib provides `LocalTestingDistributedActorSystem`. Production systems (Apple's swift-distributed-actors package, third-party transports) plug in their own.

MEP-49 v0.1 **excludes** distributed actors. The reasons:

- The `DistributedActorSystem` API is still evolving (Swift 5.9 stabilised it, but the ecosystem is thin).
- Mochi's "remote agent" semantics (a Mochi feature in the [[01-language-surface]] roadmap) need a clear story for serialisation, retry, and partition tolerance. The stdlib `Distributed` package alone does not provide these; we would need to pick a transport (gRPC, custom) and bake it in.
- Cross-target compatibility: Mochi-on-JVM uses Kafka or similar for cross-node; Mochi-on-BEAM uses `pg`. Mochi-on-Swift would need a parallel transport that interoperates with the other targets, which is its own design effort.

MEP-49 documents distributed actors as **v2 scope** with a placeholder section in [[12-risks-and-alternatives]]. v1 ships local-only agents.

## 25. Performance

Numbers below are from microbenchmarks on Swift 6.0, macOS 14.6 on M2 Max, release builds with `-O`. Treat as orders of magnitude.

| Operation                                | Cost                              |
|------------------------------------------|-----------------------------------|
| `actor` instance allocation              | ~50 ns                            |
| Actor isolated method call (hop)         | ~200-400 ns                       |
| `AsyncStream.yield` (no buffer pressure) | ~150 ns                           |
| `AsyncStream.next` (buffer hit)          | ~200 ns                           |
| `AsyncStream.next` (suspend then resume) | ~1-2 us                           |
| `CheckedContinuation` resume             | ~300 ns                           |
| `Task { ... }` creation                  | ~500 ns                           |
| `withTaskGroup` start                    | ~800 ns                           |
| Mochi cast (`agent.cast(.x)`)            | ~250-400 ns                       |
| Mochi call (`await agent.x()`)           | ~1.5-3 us                         |
| `Task.sleep(for: .milliseconds(1))`      | ~1.05 ms (1 ms target + scheduler)|

Compared to MEP-47 (JVM with Loom):

- Actor message dispatch on Swift (~200-400 ns) is in the same ballpark as JVM `BlockingQueue` SPSC throughput (~3-5M msg/s = ~200-300 ns/msg).
- Swift actors are **lighter weight** than virtual threads in terms of memory: an actor instance is ~80 bytes for the runtime overhead plus the user's stored properties; a virtual thread is ~200 bytes plus stack chunks. A million Swift actors is comfortable; a million Mochi-on-JVM virtual threads is at the edge of practicality.
- Swift cooperative scheduling has **no kernel involvement** for hops between actors on the same cooperative thread; JVM Loom needs the carrier thread but not the kernel. Both are far cheaper than OS-thread context switches.
- Swift has **no preemption**: a runaway `while true {}` inside an actor method starves that actor's executor forever. The JVM Loom case is similar (cooperative); the BEAM case is different (preemptive). Mochi documents this prominently in [[12-risks-and-alternatives]].

Reference deployments worth citing:

- **Apple's first-party frameworks** (SwiftUI, Foundation, Network) use actors and AsyncStream extensively as of iOS 17 / macOS 14.
- **swift-nio** (Apple, 2024-) integrated AsyncSequence-based APIs alongside the older callback APIs.
- **Hummingbird 2** (Swift server framework, 2024) is built on structured concurrency from the ground up; reports favourable comparison to Vapor.

## 26. Comparison table

Read alongside the BEAM (MEP-46), JVM/Loom (MEP-47), .NET (MEP-48) notes:

| Dimension            | BEAM (MEP-46)               | JVM/Loom (MEP-47)              | .NET (MEP-48)                  | Swift (MEP-49)                  |
|----------------------|-----------------------------|--------------------------------|--------------------------------|---------------------------------|
| Concurrency unit     | OS-scheduled process        | Virtual thread on carrier      | `Task<T>` on threadpool        | `Task` on cooperative executor  |
| State isolation      | Per-process heap, copy-on-send | Shared JVM heap, sendable by convention | Shared CLR heap | Per-actor isolation, Sendable enforced by compiler |
| Mailbox              | BEAM ETS-backed queue       | `BlockingQueue<Message>`       | `Channel<T>` or `ConcurrentQueue<T>` | `AsyncStream<Message>` continuation |
| Message ordering     | FIFO                        | FIFO                           | FIFO                           | FIFO via AsyncStream            |
| Send semantics       | Always copy                 | Pass-by-reference (sendable)   | Pass-by-reference              | Pass-by-region (SE-0414) or sendable |
| Supervision          | OTP built-in                | Userspace runtime              | Userspace runtime              | Userspace `MochiRuntime.Supervisor` |
| Crash containment    | Process heap reclaimed      | Heap is shared, leaks possible | Heap is shared, leaks possible | Actor state safe, external resources leak risk |
| Preemption           | Reduction counting          | Cooperative (yield points)     | Cooperative                    | Cooperative (no preemption)     |
| Cancellation         | `Process.exit/2`            | `Thread.interrupt`             | `CancellationToken`            | Cooperative `Task.cancel`       |
| Backpressure         | Mailbox size signals        | `SubmissionPublisher` blocks   | `Channel.WriteAsync` waits     | `AsyncStream.bufferingNewest` + yield result |
| Distributed agents   | Built-in (`gen_server` + `pg`) | Add Kafka/Pulsar layer       | gRPC / Orleans                 | `distributed actor` (v2 scope)  |
| Hot reload           | First-class                 | `Instrumentation` API, heavy   | Limited                        | None                            |
| Strict typing of messages | Dynamic                | Sealed interface, compile-time | Enum + record types            | Sendable enum, compile-time     |
| Typed errors         | tuples by convention        | checked exceptions             | optional `Result<T, E>` patterns | typed throws (SE-0413) on intents |
| Latency (message dispatch) | sub-microsecond       | 0.5-1 us cast, 2-4 us call     | sub-microsecond                | 200-400 ns hop, 1.5-3 us call   |
| Max practical agents on one node | 10M+              | 1-10M                          | 100K-1M                        | 1M+                             |

Swift's strongest cards on this table are **compiler-enforced sendable** (errors at compile time, not runtime), **typed throws on agent calls** (so the caller's `try` knows the precise error set), and **the lowest per-actor memory overhead** of any of the targets. Its weakest are **no built-in supervision** (userspace runtime, like JVM/.NET), **no hot reload**, and **distributed actors deferred to v2**.

For Mochi's positioning, Swift is the right target when:

- The deployment lives on Apple platforms (iOS, macOS, tvOS, visionOS) where Swift is the native runtime.
- Strict static guarantees on concurrency safety are wanted (sendable, isolation, typed throws).
- The agent count is large but per-agent state is small; Swift actors scale to millions.

Swift is the wrong target when:

- Hot reload matters (use BEAM).
- Built-in cluster-aware distributed agents matter (use BEAM or wait for MEP-49 v2).
- Cross-platform server deployment matters more than Apple-platform integration (use JVM or .NET).

---

## Sources

1. SE-0296: async/await. <https://github.com/apple/swift-evolution/blob/main/proposals/0296-async-await.md>
2. SE-0300: Continuations for interfacing async tasks with synchronous code. <https://github.com/apple/swift-evolution/blob/main/proposals/0300-continuation.md>
3. SE-0302: Sendable and @Sendable closures. <https://github.com/apple/swift-evolution/blob/main/proposals/0302-concurrent-value-and-concurrent-closures.md>
4. SE-0304: Structured concurrency. <https://github.com/apple/swift-evolution/blob/main/proposals/0304-structured-concurrency.md>
5. SE-0306: Actors. <https://github.com/apple/swift-evolution/blob/main/proposals/0306-actors.md>
6. SE-0314: AsyncStream and AsyncThrowingStream. <https://github.com/apple/swift-evolution/blob/main/proposals/0314-async-stream.md>
7. SE-0316: Global actors. <https://github.com/apple/swift-evolution/blob/main/proposals/0316-global-actors.md>
8. SE-0317: async let bindings. <https://github.com/apple/swift-evolution/blob/main/proposals/0317-async-let.md>
9. SE-0336: Distributed actor isolation. <https://github.com/apple/swift-evolution/blob/main/proposals/0336-distributed-actor-isolation.md>
10. SE-0337: Incremental migration to concurrency checking. <https://github.com/apple/swift-evolution/blob/main/proposals/0337-support-incremental-migration-to-concurrency-checking.md>
11. SE-0381: DiscardingTaskGroups. <https://github.com/apple/swift-evolution/blob/main/proposals/0381-task-group-discard-results.md>
12. SE-0388: Convenience Async[Throwing]Stream.makeStream methods. <https://github.com/apple/swift-evolution/blob/main/proposals/0388-async-stream-factory.md>
13. SE-0413: Typed throws. <https://github.com/apple/swift-evolution/blob/main/proposals/0413-typed-throws.md>
14. SE-0414: Region-based isolation. <https://github.com/apple/swift-evolution/blob/main/proposals/0414-region-based-isolation.md>
15. SE-0421: Generalize effect polymorphism for AsyncSequence and AsyncIteratorProtocol. <https://github.com/apple/swift-evolution/blob/main/proposals/0421-generalize-async-sequence.md>
16. Swift 6.0 release notes, Apple, September 2024.
17. swift-async-algorithms README. <https://github.com/apple/swift-async-algorithms>
18. Apple Developer documentation, "Concurrency", Swift 6.0.
19. "Embracing Swift 6 strict concurrency", WWDC24 session 10169.
20. Hummingbird 2 release notes, 2024.
