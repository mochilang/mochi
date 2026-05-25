# MEP-50 research note 09, Agents and streams (Kotlin coroutines)

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).
Sources: kotlinx.coroutines documentation (1.10.1), Kotlin coroutines
guide on kotlinlang.org, Roman Elizarov's "Coroutines: how does it
work" KotlinConf 2017 and 2019 talks, the kotlinx.coroutines roadmap
(github.com/Kotlin/kotlinx.coroutines/blob/master/ROADMAP.md), the
discussion thread on the deprecation of the `actor { }` builder
(github.com/Kotlin/kotlinx.coroutines/issues/87), the Mochi agent and
stream language surface in `docs/features/*.md`, and the sibling
research bundles for MEP-46 (BEAM) and MEP-49 (Swift) whose agent and
stream lowerings inspire ours.

This note specifies the lowering of Mochi `agent`, `stream`, `spawn`,
`!` (cast), and `?` (call) into Kotlin source. The runtime substrate
is `kotlinx.coroutines`: `CoroutineScope`, `Job`, `SupervisorJob`,
`Channel<T>`, `Flow<T>`, `CompletableDeferred<T>`. The transpiler does
**not** use the deprecated `actor { }` coroutine builder; the modern
shape is a custom class with a private `Channel` and a launched
receive loop.

The two big-picture decisions, defended in [[02-design-philosophy]] §12
and stated here as the operating assumptions:

1. **Coroutines, not threads.** Every Mochi agent and stream lowers to
   structured-concurrency coroutine code. The transpiler never emits
   raw `Thread`, `ExecutorService.submit`, or `Runnable`. This is
   universal across all KMP targets (JVM, Android, Native, JS, Wasm),
   so the lowering is identical in `commonMain`.

2. **Custom actor class, not `actor { }` builder.** The deprecated
   `kotlinx.coroutines.channels.actor { }` builder is unsuitable
   because: (a) it returns `SendChannel<E>`, exposing the channel
   directly to callers, breaking encapsulation; (b) it lacks
   request-reply ergonomics; (c) the design is being phased out per
   the kotlinx.coroutines ROADMAP. The modern shape is documented
   throughout this note.

## 1. The custom actor class pattern

The canonical Mochi agent lowers to a Kotlin class with:

- A constructor taking a `CoroutineScope` (the parent scope; cancellation
  flows from parent to agent).
- A private `Channel<Message>` mailbox.
- A `Job` field tracking the launched receive loop.
- Private mutable state (the agent's "registers").
- Public methods for cast (fire-and-forget send) and call
  (request-reply send).
- An `init` block that launches the receive loop on the parent scope.
- A `close()` or `shutdown()` method that cancels the receive loop.

```kotlin
public class Counter(scope: CoroutineScope) {
    private val mailbox: Channel<Message> = Channel(Channel.UNLIMITED)
    private val job: Job
    private var count: Long = 0L

    init {
        job = scope.launch {
            try {
                for (msg in mailbox) {
                    handle(msg)
                }
            } finally {
                mailbox.close()
            }
        }
    }

    public fun increment() {
        mailbox.trySend(Message.Increment)
    }

    public suspend fun get(): Long {
        val reply = CompletableDeferred<Long>()
        mailbox.send(Message.Get(reply))
        return reply.await()
    }

    public fun shutdown() {
        job.cancel()
    }

    private suspend fun handle(msg: Message) {
        when (msg) {
            Message.Increment -> count += 1L
            is Message.Get -> msg.reply.complete(count)
        }
    }

    private sealed interface Message {
        public data object Increment : Message
        public data class Get(val reply: CompletableDeferred<Long>) : Message
    }
}
```

Anatomy notes:

- The `Channel<Message>` is constructed `UNLIMITED` (matching Mochi/
  BEAM unbounded mailbox semantics).
- The receive loop uses `for (msg in mailbox)`, which is sugar for
  `mailbox.consumeAsFlow().collect`. When the channel closes, the loop
  exits cleanly.
- `try/finally { mailbox.close() }` ensures the channel is closed
  even on cancellation, freeing any pending senders.
- `handle(msg)` is `suspend` so it can call other agents, await
  streams, etc.
- The `Message` sealed interface is *nested* inside the actor class.
  This avoids polluting the package namespace.
- `CompletableDeferred<Long>` carries the reply for call-style. It is
  a one-shot future; `complete()` returns false if already completed
  (used for cancellation handling).

## 2. Why not the `actor { }` builder

The deprecated builder looks like:

```kotlin
val counter = scope.actor<Message> {
    var count = 0L
    for (msg in channel) {
        when (msg) {
            Message.Increment -> count += 1L
            is Message.Get -> msg.reply.complete(count)
        }
    }
}
counter.send(Message.Increment)
```

Problems:

1. **Exposes the channel as the public API**: `counter: SendChannel<Message>`.
   Callers can call `counter.close()` directly, breaking encapsulation.
2. **No clean encapsulation of mutable state**: `count` lives in the
   builder lambda's closure. There's no class boundary for state.
3. **No request-reply ergonomics**: every reply needs a manual
   `CompletableDeferred`.
4. **Deprecation trajectory**: the kotlinx.coroutines ROADMAP lists
   `actor { }` for removal once a stable replacement is shipped (the
   "Reactive actors" RFC, KEEP-not-yet-numbered).

The custom actor class pattern solves all four issues at the cost of
~15 lines of boilerplate per actor. The transpiler emits this
boilerplate automatically; users never see it.

## 3. Cast (fire-and-forget)

Mochi `counter ! Message.Increment` lowers to:

```kotlin
counter.increment()
```

where `increment()` is a non-`suspend` public method on the actor
class that calls `mailbox.trySend(Message.Increment)`. `trySend`
returns a `ChannelResult<Unit>`; the result is discarded for
fire-and-forget semantics.

If the channel is closed (the actor is shut down), `trySend` returns
`ChannelResult.Closed`, which is silently discarded matching Mochi's
"send to dead agent is a no-op" semantic.

For agents that need to know whether the send succeeded, the
transpiler emits an alternative `tryIncrement(): Boolean` method
returning `trySend(Message.Increment).isSuccess`.

## 4. Call (request-reply)

Mochi `let n = counter ? Message.Get` lowers to:

```kotlin
val n: Long = counter.get()
```

where `get()` is a `suspend` method that:
1. Creates a `CompletableDeferred<Long>`.
2. Sends `Message.Get(reply)` to the mailbox.
3. Awaits `reply.await()`.

The actor handles `Message.Get(reply)` by calling `reply.complete(count)`,
which wakes the caller.

### 4.1 Timeouts

Mochi `let n = counter ? Message.Get within 1s` lowers to:

```kotlin
val n: Long = withTimeout(1000L) { counter.get() }
```

`withTimeout` throws `TimeoutCancellationException` after the deadline;
the transpiler catches it and converts to `MochiResult.Err(MochiTimeout)`
when the Mochi caller uses `try ... catch` semantics.

### 4.2 Cancellation propagation

If the calling coroutine is cancelled while awaiting `reply.await()`,
the cancellation propagates through the `CompletableDeferred` and the
caller exits cleanly. The actor itself is not cancelled (it remains
ready to handle the next message).

To cancel the work the actor was doing on behalf of the caller, the
message needs to include a `CoroutineContext` or a `Job` token; the
actor checks for cancellation on each loop iteration via
`coroutineContext.ensureActive()`.

## 5. Supervision

Mochi's supervision tree (BEAM-inspired) lowers to a `MochiSupervisor`
actor that holds a map of children and restarts on failure.

```kotlin
public class MochiSupervisor(
    scope: CoroutineScope,
    private val strategy: RestartStrategy = RestartStrategy.OneForOne
) {
    private val children = mutableMapOf<String, ChildSpec>()
    private val supervisorScope = CoroutineScope(scope.coroutineContext + SupervisorJob())

    public fun startChild(name: String, spec: ChildSpec) {
        children[name] = spec
        launchChild(name, spec)
    }

    private fun launchChild(name: String, spec: ChildSpec) {
        supervisorScope.launch {
            try {
                spec.start()
            } catch (e: Throwable) {
                if (e is CancellationException) throw e
                handleFailure(name, e)
            }
        }
    }

    private fun handleFailure(name: String, e: Throwable) {
        when (strategy) {
            RestartStrategy.OneForOne -> {
                children[name]?.let { launchChild(name, it) }
            }
            RestartStrategy.OneForAll -> {
                supervisorScope.coroutineContext.cancelChildren()
                children.forEach { (n, s) -> launchChild(n, s) }
            }
            RestartStrategy.RestForOne -> {
                val keys = children.keys.toList()
                val idx = keys.indexOf(name)
                keys.drop(idx).forEach { n ->
                    children[n]?.let { spec ->
                        launchChild(n, spec)
                    }
                }
            }
        }
    }

    public fun shutdown() {
        supervisorScope.cancel()
    }

    public enum class RestartStrategy { OneForOne, OneForAll, RestForOne }

    public data class ChildSpec(val start: suspend () -> Unit)
}
```

Key trick: the supervisor uses its own `CoroutineScope` built with
`SupervisorJob()`. A `SupervisorJob` does *not* cancel its parent
when a child fails (unlike a regular `Job`). This lets the supervisor
catch the child's failure and restart it without bringing down the
whole tree.

The three restart strategies (OneForOne, OneForAll, RestForOne) match
the BEAM/OTP naming exactly. See [[../0046/09-agent-streams]] for the
BEAM sibling.

### 5.1 Restart intensity

To prevent restart storms, supervisors carry a `(maxRestarts, period)`
pair. If more than `maxRestarts` failures occur within `period`, the
supervisor itself fails (propagating to its parent supervisor or
killing the program). The transpiler emits the default `(3, 60s)` from
the BEAM convention.

```kotlin
public class MochiSupervisor(
    scope: CoroutineScope,
    private val strategy: RestartStrategy = RestartStrategy.OneForOne,
    private val maxRestarts: Int = 3,
    private val period: Duration = 60.seconds
) {
    private val failures = mutableListOf<Instant>()

    private fun handleFailure(name: String, e: Throwable) {
        val now = Clock.System.now()
        failures.add(now)
        failures.removeAll { it < now - period }
        if (failures.size > maxRestarts) {
            throw MochiSupervisorOverwhelmedException("restart storm")
        }
        // ... apply strategy
    }
}
```

## 6. Cancellation discipline

Kotlin coroutines are cancelled cooperatively. A coroutine that does
no suspending operations cannot be cancelled until it reaches a
suspension point. The transpiler emits cancellation checkpoints at
every loop back-edge in long-running blocks:

```kotlin
while (condition) {
    coroutineContext.ensureActive()  // checkpoint
    // body
}
```

`ensureActive()` throws `CancellationException` if the coroutine has
been cancelled, terminating the block cleanly.

For tight CPU loops (Datalog evaluation, numeric crunching) the
transpiler emits `yield()` instead, which both checks cancellation
and yields the dispatcher (allowing other coroutines on the same
dispatcher to run).

### 6.1 Cancellation and resources

Resources (file handles, locks, network connections) are released via
`try/finally` blocks. Kotlin's `use { }` extension on `Closeable`
makes this idiomatic:

```kotlin
file.use { input ->
    while (true) {
        coroutineContext.ensureActive()
        val line = input.readLine() ?: break
        // process
    }
}
```

Cancellation propagates the exception out, `finally` runs, the file is
closed. This is the structural discipline Kotlin coroutines enforce.

## 7. Mailbox backpressure policy

Channel construction policies:

- `Channel.UNLIMITED` (default for Mochi agents): infinite buffer.
  Matches BEAM mailbox; never blocks sender. Risk: unbounded memory
  growth under sustained overload.
- `Channel.RENDEZVOUS` (Mochi `bounded(0)`): no buffer; sender suspends
  until receiver is ready.
- `Channel.BUFFERED(N)` (Mochi `bounded(N)`): bounded buffer of size N;
  sender suspends when full.
- `Channel.CONFLATED` (Mochi `conflated`): always replaces the buffered
  element; sender never blocks; receiver only ever sees the latest.

Mochi defaults to UNLIMITED to match the BEAM semantic and to keep the
codegen simple. Users override with the `bounded(N)` or `conflated`
qualifier on the `agent` declaration.

### 7.1 Memory pressure backstop

Even `UNLIMITED` channels have practical limits. The runtime exposes
`MochiSupervisor.setMailboxLimit(actor, N)`: when an actor's mailbox
exceeds N messages, the supervisor logs a warning and (optionally)
escalates. Default N is `Long.MAX_VALUE` (effectively unlimited); CI
fixtures set it lower for memory-leak detection.

## 8. Streams (cold flow)

Mochi `stream T = { ... }` lowers to `kotlinx.coroutines.flow.Flow<T>`.
The canonical producer is the `flow { }` builder:

```kotlin
public fun tickerStream(): Flow<Tick> = flow {
    while (true) {
        delay(1000L)
        emit(Tick(time = Clock.System.now()))
    }
}
```

`flow { }` is *cold*: the body only runs when a collector subscribes,
and runs once per collector. This matches Mochi's stream semantic
where each consumer sees its own evaluation.

### 8.1 Stream operators

The Mochi stream DSL maps onto kotlinx.coroutines.flow operators:

| Mochi | Kotlin |
|-------|--------|
| `stream.map(f)` | `flow.map { f(it) }` |
| `stream.filter(p)` | `flow.filter { p(it) }` |
| `stream.flatten` | `flow.flattenConcat()` (sequential) or `flattenMerge()` (parallel) |
| `stream.zip(other)` | `flow.zip(other) { a, b -> Pair(a, b) }` |
| `stream.combine(other)` | `flow.combine(other) { a, b -> Pair(a, b) }` |
| `stream.merge(other)` | `merge(flow, other)` (top-level operator) |
| `stream.debounce(d)` | `flow.debounce(d.inWholeMilliseconds)` |
| `stream.throttle(d)` | `flow.sample(d.inWholeMilliseconds)` |
| `stream.take(n)` | `flow.take(n)` |
| `stream.drop(n)` | `flow.drop(n)` |
| `stream.first()` | `flow.first()` |
| `stream.last()` | `flow.last()` |
| `stream.toList()` | `flow.toList()` |
| `stream.reduce(f)` | `flow.reduce { a, b -> f(a, b) }` |
| `stream.fold(z, f)` | `flow.fold(z) { a, b -> f(a, b) }` |
| `stream.collect { x -> body }` | `flow.collect { x -> body }` |
| `for x in stream` (sugar) | `flow.collect { x -> body }` |

### 8.2 Cold vs hot

Mochi streams are cold by default. For hot (broadcast, multi-subscriber)
streams the user writes:

```mochi
stream Hot<T> = hot
```

which lowers to `MutableSharedFlow<T>`:

```kotlin
public val hotStream: MutableSharedFlow<Tick> = MutableSharedFlow(
    replay = 0,
    extraBufferCapacity = 64,
    onBufferOverflow = BufferOverflow.DROP_OLDEST
)
```

`MutableSharedFlow` is the hot-flow primitive. `replay = N` keeps the
last N emissions for late subscribers. `extraBufferCapacity = M` buffers
M emissions between fast emitters and slow collectors. `onBufferOverflow`
picks DROP_OLDEST, DROP_LATEST, or SUSPEND.

### 8.3 StateFlow

For "current value plus subscription" semantics (Mochi `state<T>`),
the lowering is `MutableStateFlow<T>`:

```kotlin
public val tickState: MutableStateFlow<Tick> = MutableStateFlow(Tick(time = Clock.System.now()))
```

`StateFlow` is a `SharedFlow` with `replay = 1`, conflated, where new
subscribers always see the current value. Matches a "BehaviorSubject"
in RxJava terminology.

## 9. `every` / `at` / periodic emission

Mochi `every 1s emit Tick` lowers to:

```kotlin
public fun ticker(): Flow<Tick> = flow {
    while (true) {
        delay(1000L)
        emit(Tick(time = Clock.System.now()))
    }
}
```

We do *not* use the `kotlinx.coroutines.channels.ticker` channel
builder; it is deprecated in favour of `flow { }` builders.

For drift-free periodic emission (where the i-th tick should fire at
`start + i * period` regardless of body execution time), the transpiler
emits a more sophisticated form:

```kotlin
public fun driftFreeTicker(period: Duration): Flow<Instant> = flow {
    val start = Clock.System.now()
    var i = 0L
    while (true) {
        val target = start + period * i
        val now = Clock.System.now()
        val sleep = (target - now).inWholeMilliseconds
        if (sleep > 0) delay(sleep)
        emit(target)
        i += 1L
    }
}
```

This is opt-in via the `drift_free` qualifier on the `every` clause.

## 10. Spawn

Mochi `spawn f()` (fire-and-forget concurrent execution) lowers to:

```kotlin
scope.launch { f() }
```

where `scope` is the surrounding `CoroutineScope`. For top-level Mochi
programs the scope is `runBlocking { ... }` (on JVM) or the main
function's own coroutine context (on K/Native, K/JS, K/Wasm where
`main()` is `suspend`).

`launch` returns a `Job`. The transpiler discards the result for
fire-and-forget; assigns to a local for join-able cases:

```kotlin
val task = scope.launch { f() }
task.join()  // wait for completion
```

### 10.1 Spawn with result

Mochi `let result = spawn f()` (returns a future-like handle) lowers
to:

```kotlin
val result: Deferred<T> = scope.async { f() }
// later
val value: T = result.await()
```

`async` returns `Deferred<T>`, the await-able variant of `Job`.

### 10.2 Structured concurrency

Mochi `parallel { f(); g(); h() }` (wait for all three) lowers to:

```kotlin
coroutineScope {
    launch { f() }
    launch { g() }
    launch { h() }
}
```

`coroutineScope { }` suspends until all child coroutines complete.
Cancellation propagates: if any child throws, all siblings are
cancelled and the exception is rethrown.

Mochi `race { f(); g() }` (first to complete wins) lowers to:

```kotlin
val result = select<T> {
    async { f() }.onAwait { it }
    async { g() }.onAwait { it }
}
```

`select { }` is the kotlinx.coroutines waiting primitive for "first of
several events".

## 11. K/Native specific: the new memory model

Kotlin/Native pre-1.7.20 used the legacy "freeze" memory model: cross-
thread shared state required explicit freezing, and frozen objects
were immutable. The new memory model (default since 1.9, optional
since 1.7.20) drops this entirely, presenting a Java-like shared-
memory model.

MEP-50 requires Kotlin 2.1+; the new memory model is the only model
supported. We do not emit any `freeze()` calls, do not use
`@SharedImmutable`, do not depend on `AtomicReference` for cross-
thread sharing. Standard `var` mutable state is shared via the
coroutine scope as on JVM.

This is a *load-bearing requirement*. Without the new memory model,
the actor pattern would require freezing every message before sending,
making the codegen unworkably complex. See [[12-risks-and-alternatives]]
R3.

## 12. K/JS and K/Wasm specifics

On K/JS and K/Wasm, coroutines run on the JS event loop (single-
threaded). `delay(n)` becomes `setTimeout(..., n)`. There is no real
parallelism within a single JS context (no Web Workers integration in
v1).

`Dispatchers.Default` on K/JS maps to the JS microtask queue
(equivalent to Promise resolution).  `Dispatchers.IO` does not exist
on JS; the transpiler emits `Dispatchers.Default` everywhere.

For agents that need *real* parallelism on the web, the v2 path is Web
Workers via the kotlinx.coroutines `workerExecutor` plugin (not yet
stable as of Kotlin 2.1). Documented but deferred.

## 13. JVM-specific: comparing to Loom

MEP-47 (JVM bytecode) uses Loom virtual threads for its agent
lowering. MEP-50 uses coroutines on the JVM. Both are correct; the
choice diverges because:

- Loom is JVM-only (no Android, no Native, no JS, no Wasm).
- Coroutines work across every KMP target.
- Coroutines have lower per-task overhead than Loom virtual threads
  (a coroutine is ~100 bytes; a virtual thread is ~1-5KB).
- Loom blocks the carrier thread on `synchronized` blocks (until
  JEP 491, expected JDK 24); coroutines do not.

For agent-heavy Mochi workloads on JVM, coroutines win. For Mochi
programs that exclusively use JVM-blocking APIs (JDBC, java.io), Loom
might win; users with such workloads pick MEP-47.

## 14. Distributed agents

Out of scope for v1. Mochi `agent T at "host:port"` (remote actor)
is reserved for a future MEP. Candidate implementations:

- **gRPC**: Kotlin gRPC client/server with `kotlinx.coroutines` flow
  bindings; well-supported on JVM, less on K/Native.
- **Ktor remoting**: emerging in Ktor 3.x; promising but pre-stable.
- **kotlinx-rpc**: JetBrains's RPC library (2024-Q3+), built on Ktor;
  the natural choice once it stabilises.

The Mochi surface for remote actors is documented but transpilation
is gated behind `--enable-distributed-agents` (off in v1).

See [[12-risks-and-alternatives]] A7.

## 15. AsyncSequence / collect interop

When emitted Kotlin code interoperates with Swift's `AsyncSequence`
(via K/Native iOS bridges), the lowering must translate `Flow<T>` to
`AsyncSequence`. The bridge:

```kotlin
public fun <T : Any> Flow<T>.asAsyncSequence(): /* Swift AsyncSequence */ Any {
    // K/Native iOS only; uses Apple's KMP bridge tools
}
```

K/Native iOS exposes `Flow<T>` to Swift via the
`kotlinx-coroutines-core` extension; Swift code can `for await x in flow`
the result. See [[../0049/09-agent-streams]] for the Swift sibling
view.

## 16. Examples

### 16.1 Simple cast-only agent

Mochi:
```mochi
agent logger {
  state count: int = 0
  receive {
    Log(msg) => { print("[log] ", msg); count = count + 1 }
  }
}

let l = spawn logger
l ! Log("hello")
```

Kotlin:
```kotlin
public class Logger(scope: CoroutineScope) {
    private val mailbox = Channel<Message>(Channel.UNLIMITED)
    private val job: Job
    private var count: Long = 0L

    init {
        job = scope.launch {
            try {
                for (msg in mailbox) handle(msg)
            } finally {
                mailbox.close()
            }
        }
    }

    public fun log(msg: String) {
        mailbox.trySend(Message.Log(msg))
    }

    public fun shutdown() { job.cancel() }

    private fun handle(msg: Message) {
        when (msg) {
            is Message.Log -> {
                print("[log] $${msg.msg}")
                count += 1L
            }
        }
    }

    private sealed interface Message {
        public data class Log(val msg: String) : Message
    }
}

fun main() = runBlocking {
    val l = Logger(this)
    l.log("hello")
    l.shutdown()
}
```

### 16.2 Call-style agent

Mochi:
```mochi
agent counter {
  state n: int = 0
  receive {
    Inc => n = n + 1
    Get => reply n
  }
}

let c = spawn counter
c ! Inc
c ! Inc
let n = c ? Get
print(n)
```

Kotlin:
```kotlin
public class Counter(scope: CoroutineScope) {
    private val mailbox = Channel<Message>(Channel.UNLIMITED)
    private val job: Job
    private var n: Long = 0L

    init {
        job = scope.launch {
            try {
                for (msg in mailbox) handle(msg)
            } finally {
                mailbox.close()
            }
        }
    }

    public fun inc() {
        mailbox.trySend(Message.Inc)
    }

    public suspend fun get(): Long {
        val reply = CompletableDeferred<Long>()
        mailbox.send(Message.Get(reply))
        return reply.await()
    }

    public fun shutdown() { job.cancel() }

    private suspend fun handle(msg: Message) {
        when (msg) {
            Message.Inc -> n += 1L
            is Message.Get -> msg.reply.complete(n)
        }
    }

    private sealed interface Message {
        public data object Inc : Message
        public data class Get(val reply: CompletableDeferred<Long>) : Message
    }
}

fun main() = runBlocking {
    val c = Counter(this)
    c.inc()
    c.inc()
    val n = c.get()
    println(n)  // 2
    c.shutdown()
}
```

### 16.3 Periodic stream

Mochi:
```mochi
stream Tick = { time: time }

every 1s emit Tick { time: now() }

for t in tickStream {
  print(t.time)
}
```

Kotlin:
```kotlin
@Serializable
public data class Tick(public val time: Instant)

public fun tickStream(): Flow<Tick> = flow {
    while (true) {
        delay(1000L)
        emit(Tick(time = Clock.System.now()))
    }
}

fun main() = runBlocking {
    tickStream().take(3).collect { t ->
        println(t.time)
    }
}
```

### 16.4 Supervisor + child actors

Mochi:
```mochi
supervisor s {
  child a: agent counter
  child b: agent logger
  strategy one_for_one
}
```

Kotlin:
```kotlin
fun main() = runBlocking {
    val supervisor = MochiSupervisor(this, RestartStrategy.OneForOne)
    supervisor.startChild("a", MochiSupervisor.ChildSpec {
        val counter = Counter(this)
        // ... messages
    })
    supervisor.startChild("b", MochiSupervisor.ChildSpec {
        val logger = Logger(this)
        // ... messages
    })
    delay(60_000L)
    supervisor.shutdown()
}
```

## 17. Performance benchmarks (target, not yet measured)

Targets from the soft performance gate (see [[11-testing-gates]] §16):

- **Cast latency**: ≤ 400 ns per `trySend` on JVM, ≤ 200 ns on K/Native.
- **Call latency**: ≤ 5 μs per request-reply round-trip (uncontended).
- **1M-message throughput**: ≥ 5M msg/s on JVM (single mailbox),
  ≥ 8M msg/s on K/Native.
- **Memory per actor**: ≤ 2 KB per actor (channel + state).
- **Flow per-element overhead**: ≤ 50 ns on JVM, ≤ 30 ns on K/Native.

These are *targets* for the v0.11 release; v1 ship gate is "passes
fixture tests without obvious regression".

## 18. Cross-references

- [[01-language-surface]] §6 (stream and agent core surface).
- [[02-design-philosophy]] §12 (coroutines over threads).
- [[04-runtime]] §10 (MochiSupervisor implementation).
- [[06-type-lowering]] §13 §14 (`agent` and `stream<T>` types).
- [[10-build-system]] (kotlinx.coroutines Gradle setup).
- [[11-testing-gates]] §9 §10 (Phase 9 Agents, Phase 10 Streams gates).
- [[12-risks-and-alternatives]] R3 (K/Native memory model
  requirement), R12 (cancellation in receive loops), R13
  (UNLIMITED channel memory pressure).
- [[../0046/09-agent-streams]]: BEAM sibling. The supervision tree
  design here is directly modelled on BEAM/OTP's. Restart strategies
  use the BEAM names.
- [[../0049/09-agent-streams]]: Swift sibling. Both use a custom
  actor class with a mailbox; the difference is Swift `actor` type
  vs Kotlin class + `Channel`.
- [[../0047/09-agent-streams]]: JVM-bytecode sibling. Uses Loom
  virtual threads; this note documents why MEP-50 picks coroutines
  instead.
- [[../0048/09-agent-streams]]: .NET sibling. Uses
  `IAsyncEnumerable<T>` and `TaskScheduler`; semantically similar to
  Flow.
