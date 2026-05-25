# MEP-47 research note 09, Agents and streams on JVM via Project Loom virtual threads

Author: research pass for MEP-47.
Date: 2026-05-23 (GMT+7).

This note covers how Mochi's `agent`, `spawn`, `stream`, `subscribe`, `on`, `link`, and `monitor` map onto the JVM, with Loom virtual threads as the primary execution substrate. The sibling MEP-46 note [[09-agent-streams]] (BEAM/OTP target) is the structural template. Where BEAM hands us OTP behaviours for free, the JVM hands us virtual threads and `java.util.concurrent.Flow` for free; the cost is that supervision, hot reload, and per-agent heap isolation come back as our problem.

---

## 1. Project Loom in 2026 (recap)

Project Loom landed in stages. As of JDK 25 LTS (September 2025) the relevant features are:

| Feature                                  | JEP | Status (JDK 25 LTS)                |
|------------------------------------------|-----|------------------------------------|
| Virtual threads                          | 444 | Final since JDK 21                 |
| Synchronize virtual threads w/o pinning  | 491 | Final, shipped in JDK 24, in 25    |
| Structured concurrency                   | 505 | Preview (fifth)                    |
| Scoped values                            | 506 | **Final** in JDK 25                |

Virtual threads (JEP 444, GA in JDK 21) are JVM-managed threads scheduled cooperatively on top of a pool of carrier (OS) threads. The carrier pool defaults to a dedicated `ForkJoinPool` (not the common pool; a separate scheduler pool with parallelism equal to the number of cores). The user-visible API is `Thread.ofVirtual().start(Runnable)` and `Executors.newVirtualThreadPerTaskExecutor()`.

Under the hood, the JVM uses *continuations* (`jdk.internal.vm.Continuation`) to suspend and resume virtual threads at blocking points. The continuation API is not public; only the `Thread` facade is. Mochi never touches the continuation directly.

A virtual thread is unmounted from its carrier at *yield points* (blocking I/O, `Object.wait`, `Thread.sleep`, `LockSupport.park`, and since JEP 491 also `synchronized` block contention). When unmounted, its stack chunks (Loom stores stacks as heap-allocated chunks) sit on the JVM heap until the thread is rescheduled. Idle virtual threads cost ~200 bytes plus whatever live state their stack pins.

JEP 491 ("Synchronize Virtual Threads without Pinning") shipped in JDK 24 (March 2025) and carries into JDK 25 LTS. Pre-JEP 491, a virtual thread that entered a `synchronized` block became pinned to its carrier; under high concurrency this exhausted the carrier pool and deadlocked. JEP 491 reimplemented monitors so that ownership tracks the virtual thread identity, not the carrier; pinning by `synchronized` is gone. Other pinning sources remain (JNI frames, class initialiser blocking, symbolic reference resolution); we cover them in §8.

Structured concurrency is *not* final in JDK 25. It shipped as JEP 505 (fifth preview), iterates through JEP 525 (sixth preview, JDK 26) and proposed JEP 533 (seventh preview, JDK 27). Mochi cannot rely on it as a stable API in the v0.1 spec; we describe defensive usage in §6.

Scoped values (JEP 446 in JDK 21, finalised as JEP 506 in JDK 25) are stable and we use them for per-agent context (§7).

## 2. Mochi concurrency surface (recap)

From the MEP-46 sibling note, restated here to anchor the lowering:

```mochi
agent Counter {
  var n: int = 0
  method inc(): unit { n = n + 1 }
  method value(): int { n }
}

let c = spawn Counter()
c.inc()
let v = c.value()      // synchronous

stream clicks
publish clicks {user: "alice", url: "/home"}
subscribe e in clicks { log(e.url) }

let fut = async fetch("https://x")
let r = await fut
```

Mapping at a glance:

| Mochi              | JVM / Loom                                                  |
|--------------------|-------------------------------------------------------------|
| `agent T { ... }`  | Generated class + virtual-thread main loop + `BlockingQueue` |
| `spawn T(args)`    | `dev.mochi.runtime.agent.AgentBuilder.start(T, args)`        |
| `agent.method(x)`  | Enqueue `Message.Intent(method, args, future)`, await future |
| `agent.tell(x)`    | Enqueue `Message.Cast(method, args)` (no future)             |
| `stream foo`       | `SubmissionPublisher<Foo>` held in a Mochi `Stream<Foo>`     |
| `publish foo m`    | `stream.submit(m)` (blocking when buffer full)               |
| `subscribe e in s` | `stream.subscribe(new MochiSubscriber<>(handler))`           |
| `async expr`       | `dev.mochi.runtime.async.Async.run(() -> expr)`              |
| `await fut`        | `fut.await()` (delegates to `CompletableFuture.get`)         |
| `link a b`         | Register cross-observer in `mochi.agent.Linkage`             |
| `monitor a`        | Register a `DownListener` in `mochi.agent.Monitor`           |

## 3. Mochi `agent` -> JVM lowering

Each agent instance is **one virtual thread, one mailbox, one state object**.

The codegen pass `mochi-codegen-agent-jvm` emits, per agent type, one class with:
- a private `state` record (fields become record components),
- a `BlockingQueue<Message>` mailbox (default impl: `LinkedBlockingQueue`),
- a static `start(args)` factory that allocates the queue, the state, and the virtual thread,
- one synthetic dispatch method per intent.

A Mochi agent:

```mochi
agent Counter {
  var n: int = 0
  intent inc(): unit { n = n + 1 }
  intent value(): int { n }
}
```

lowers to:

```java
public final class MochiAgent_Counter implements Agent<MochiAgent_Counter.Handle> {

  private static final class State { int n; }

  sealed interface Message
      permits Message.Inc, Message.Value, Message.Stop {
    record Inc() implements Message {}
    record Value(CompletableFuture<Integer> reply) implements Message {}
    record Stop() implements Message {}
  }

  public static final class Handle {
    final BlockingQueue<Message> mailbox = new LinkedBlockingQueue<>();
    final Thread thread;
    Handle(Thread t) { this.thread = t; }

    public void inc() { mailbox.offer(new Message.Inc()); }

    public int value() {
      var f = new CompletableFuture<Integer>();
      mailbox.offer(new Message.Value(f));
      try { return f.get(); }
      catch (Exception e) { throw new MochiAgentError(e); }
    }

    public void stop() {
      mailbox.offer(new Message.Stop());
      try { thread.join(); } catch (InterruptedException ignored) {}
    }
  }

  public static Handle start() {
    var state = new State();
    var refHandle = new java.util.concurrent.atomic.AtomicReference<Handle>();
    var t = Thread.ofVirtual()
        .name("mochi-agent-Counter")
        .start(() -> loop(refHandle.get(), state));
    var h = new Handle(t);
    refHandle.set(h);
    return h;
  }

  private static void loop(Handle h, State s) {
    while (true) {
      Message m;
      try { m = h.mailbox.take(); }
      catch (InterruptedException ie) { return; }
      switch (m) {
        case Message.Inc i -> { s.n = s.n + 1; }
        case Message.Value v -> v.reply.complete(s.n);
        case Message.Stop ignored -> { return; }
      }
    }
  }
}
```

A few invariants:

- **State isolation**: `state` is captured by exactly one closure (the loop). No other thread reads or writes it. We do *not* declare it `volatile` or wrap in `synchronized`; the JMM happens-before edge across `BlockingQueue.put/take` is enough.
- **Intent identity**: each intent compiles to a `record` in the sealed `Message` interface; pattern matching gives the dispatcher type-safe routing.
- **Calls vs casts**: an intent declared `unit` and tagged `@fire_and_forget` becomes a cast (no `reply` future). All other intents are calls; cast/call ordering on the same handle is FIFO because the mailbox is a single FIFO queue.
- **Stop**: a sentinel `Stop` message drains the queue up to that point, then the loop returns and the virtual thread terminates. `Handle.stop()` joins the thread.

The codegen never emits public constructors for the agent class; users only see the opaque `Handle` returned by `start`.

### Why a queue and not direct method calls

A naive implementation could let any thread call `state.n++` under a lock. We pick the mailbox model because:

1. It matches Mochi's *language semantics* (agents are actor-like, one logical thread of execution).
2. It eliminates the need for locking, which interacts poorly with virtual threads even after JEP 491 (locks are now non-pinning but still cause contention).
3. It makes message ordering observable and replayable (§10).

### Mailbox choice

The default mailbox is `LinkedBlockingQueue<Message>`. Alternatives we evaluated:

| Queue                       | Throughput (SPSC)  | Notes                                 |
|-----------------------------|--------------------|---------------------------------------|
| `LinkedBlockingQueue`       | ~3-5M msg/s        | Default, no extra deps                |
| `ArrayBlockingQueue`        | ~6-8M msg/s        | Bounded; needs size at construction   |
| `ConcurrentLinkedQueue`     | ~10M msg/s         | Non-blocking, no `take()`             |
| JCTools `MpscArrayQueue`    | ~30M msg/s         | External dep; agents only             |

For Mochi v0.1 the default stays `LinkedBlockingQueue`. The `@agent_mailbox("array", 1024)` annotation switches to `ArrayBlockingQueue`. We do not bring JCTools into the default classpath; high-throughput users opt in via FFI.

## 4. Mochi `stream<T>` -> JVM lowering

Two candidates considered:

1. **`java.util.concurrent.Flow`** (stdlib since JDK 9, no deps): four interfaces (`Publisher`, `Subscriber`, `Subscription`, `Processor`) and one concrete implementation, `SubmissionPublisher<T>`. Reactive Streams TCK compatible. Backpressure built in.
2. **Project Reactor / RxJava 3** (external libs): much richer operator suite (`map`, `flatMap`, `window`, `groupBy`, `retry`, `concatMap`, `merge`...). Big dependency footprint (Reactor: ~2 MB; RxJava: ~2.5 MB).

We pick `Flow` for the core. Users who want operator-rich pipelines bridge to Reactor via `JdkFlowAdapter.publisherToFlowPublisher`. Adding Reactor or RxJava as a default dependency would balloon Mochi-on-JVM's startup time and runtime footprint with little benefit for the language-level pubsub primitive.

### Stream declaration and publish

```mochi
stream clicks
publish clicks {user: "alice", url: "/home"}
```

lowers to:

```java
// at declaration
public static final Stream<Map<String,Object>> clicks =
    Stream.create("clicks", 256);            // bounded buffer, default 256

// at publish
clicks.publish(Map.of("user", "alice", "url", "/home"));
```

where `dev.mochi.runtime.stream.Stream` is a thin wrapper:

```java
public final class Stream<T> {
  private final SubmissionPublisher<T> publisher;
  private final String name;

  public static <T> Stream<T> create(String name, int buffer) {
    var ex = Executors.newVirtualThreadPerTaskExecutor();
    return new Stream<>(name, new SubmissionPublisher<T>(ex, buffer));
  }

  public void publish(T item) {
    // Block-the-publisher backpressure (Mochi default)
    publisher.submit(item);
  }

  public Subscription subscribe(Subscriber<? super T> s) {
    publisher.subscribe(s);
    return new Subscription(s);
  }

  public void close() { publisher.close(); }
}
```

`SubmissionPublisher`'s constructor takes an `Executor` and a `maxBufferCapacity`. We pass a virtual-thread-per-task executor: every `onNext` dispatch happens on a fresh virtual thread. This avoids head-of-line blocking between subscribers (a slow subscriber cannot stall fast ones; the publisher's `submit` still blocks if *any* subscriber's per-subscriber buffer is full).

### Subscribe (Mochi `on T as x { ... }`)

```mochi
on clicks as e {
  log(e.url)
}
```

lowers to:

```java
clicks.subscribe(new Flow.Subscriber<Map<String,Object>>() {
  private Flow.Subscription sub;
  public void onSubscribe(Flow.Subscription s) {
    this.sub = s;
    s.request(Long.MAX_VALUE);     // Mochi default: unbounded demand
  }
  public void onNext(Map<String,Object> e) {
    MochiLog.info((String) e.get("url"));
  }
  public void onError(Throwable t) { MochiTelemetry.streamError("clicks", t); }
  public void onComplete() {}
});
```

The `request(Long.MAX_VALUE)` on subscribe means "I always have demand". This pairs with `SubmissionPublisher`'s default blocking-on-full-buffer behaviour to give the Mochi semantics: publishers slow down when subscribers slow down; nothing is silently dropped.

For per-subscriber bounded demand, users write `on clicks as e demand 100 { ... }` and the codegen calls `s.request(100)` initially, then re-requests after each batch.

### Backpressure strategies

`SubmissionPublisher` defaults to a 256-element per-subscriber buffer and **blocks the publisher** (`submit`) when the buffer is full. Alternatives via `offer(item, timeout, onDrop)`:

| Strategy        | Mochi syntax                  | Underlying API                |
|-----------------|-------------------------------|-------------------------------|
| BLOCK (default) | `stream clicks`               | `submit(item)`                |
| DROP_LATEST     | `stream clicks drop_latest`   | `offer(item, 0, ...)`         |
| DROP_OLDEST     | `stream clicks drop_oldest`   | custom buffer wrapper         |
| LATEST_ONLY     | `stream clicks latest_only`   | size-1 buffer + replace       |

Mochi defaults to BLOCK because it preserves correctness; users opt into drop semantics when they need them.

### Hot vs cold streams

`SubmissionPublisher` is **hot**: items submitted before a subscriber arrives are lost to that subscriber. Mochi's default `stream` is hot. For cold (per-subscriber replay) streams, Mochi exposes:

```mochi
stream cold replay 100 history
```

This compiles to a custom publisher that retains the last N items in a `Deque<T>` and replays them on `onSubscribe`. Implementation in `dev.mochi.runtime.stream.ReplayStream<T>`.

### Stream operators

Mochi exposes a small fluent surface (`map`, `filter`, `take`, `buffer`, `merge`) implemented as `Flow.Processor<I,O>` chains. The implementation lives in `dev.mochi.runtime.stream.flow`. We deliberately keep this small; users who want `flatMap` or `window` can drop down to Reactor.

## 5. `spawn` semantics

`spawn` is the generic concurrency primitive (not agent-specific):

```mochi
let fut = spawn fetch(url)
let r = await fut
```

Lowering:

```java
var fut = Async.run(() -> fetch(url));
var r = fut.await();
```

where `Async`:

```java
public final class Async {
  public static <T> Async<T> run(Supplier<T> work) {
    var cf = new CompletableFuture<T>();
    var t = Thread.ofVirtual().start(() -> {
      try { cf.complete(work.get()); }
      catch (Throwable e) { cf.completeExceptionally(e); }
    });
    return new Async<>(cf, t);
  }

  public T await() throws MochiAsyncError {
    try { return cf.get(); }
    catch (ExecutionException ee) { throw new MochiAsyncError(ee.getCause()); }
    catch (InterruptedException ie) { Thread.currentThread().interrupt();
                                      throw new MochiAsyncError(ie); }
  }

  public boolean cancel() {
    cf.cancel(true);
    thread.interrupt();
    return true;
  }
}
```

Notes:

- `await` blocks the *calling* virtual thread; that thread unmounts and the carrier serves other work. No real OS thread is consumed during the wait.
- `cancel` interrupts the target virtual thread. Mochi runtime checks `Thread.interrupted()` at well-known points (mailbox `take`, stream `subscribe`, sleep, I/O). Tight CPU loops in user code do *not* check interruption; that is documented as a known footgun, identical to plain Java.
- `Async.runAll(list)`, `runAny(list)`, `runTimeout(work, ms)` exist as combinators. For a structured-concurrency variant, see §6.

## 6. Async / await on Loom (or rather: the absence of it)

Mochi does **not** introduce an `async`/`await` keyword pair the way Kotlin or C# do. The `async`/`await` Mochi syntax above is just sugar over `spawn` + `Async.await`; both are ordinary functions taking ordinary closures.

The reasoning is that pre-Loom JVM languages (Kotlin coroutines, Quasar fibers) baked async/await as a way to escape the cost of platform threads: starting a kernel thread cost ~1 ms and a few megabytes; coroutines turned that into ~1 µs and a few hundred bytes by stashing state in heap-allocated continuation frames driven by a state machine. On Loom, the JVM itself does the continuation stashing; user code can call blocking APIs from a virtual thread and the carrier yields automatically. **The whole point of Loom is to make async/await unnecessary.**

Mochi-on-JVM adopts the Loom posture: code looks synchronous, the runtime is async via carrier yielding. The Mochi `async` keyword survives only because:

1. It exists on other targets (BEAM via `mochi_async`, native via MEP-45's M:N scheduler) where async semantics matter.
2. It is a clear marker that "this returns a future, not a value" for the type checker.

In effect, on JVM, `spawn f(args)` and `async f(args)` produce identical bytecode.

## 7. Structured concurrency

`StructuredTaskScope` is a preview API through JDK 25 (JEP 505). The shape:

```java
try (var scope = StructuredTaskScope.open()) {
  var a = scope.fork(() -> fetchA());
  var b = scope.fork(() -> fetchB());
  scope.join();           // waits for all forks
  return combine(a.get(), b.get());
}
```

Mochi exposes this via a higher-level `scope { ... }` form in the standard library; the implementation switches on the JDK version:

- JDK 21: use `StructuredTaskScope.ShutdownOnFailure` (the JEP 453 shape).
- JDK 25: use `StructuredTaskScope.open(Joiner.allSuccessfulOrThrow())` (the JEP 505 shape).
- JDK 26+: track JEP 525 / 533 as they land.

Because the API is moving, Mochi's `dev.mochi.runtime.scope.Scope` wraps it behind a stable Mochi-side interface. v0.1 documents this as preview-on-preview and warns users not to rely on the JVM API directly.

## 8. Scoped values

Scoped values (JEP 506, **final** in JDK 25) replace `ThreadLocal` for the virtual-thread era. The problem with `ThreadLocal` plus virtual threads: every virtual thread inherits or instantiates a `ThreadLocal` slot, and with millions of virtual threads the memory cost is real (each `ThreadLocal` set on a virtual thread allocates a slot in that thread's threadLocals map).

Scoped values bind a value lexically for the duration of a callable, and the binding is shared by-reference across nested calls and forks; no per-thread allocation.

```java
public static final ScopedValue<RequestId> REQUEST_ID = ScopedValue.newInstance();

ScopedValue.where(REQUEST_ID, new RequestId("r-42"))
  .run(() -> { /* nested code can call REQUEST_ID.get() */ });
```

Mochi uses scoped values for:

- **Current agent**: `mochi.agent.currentAgent()` returns the handle for the agent whose loop is currently executing, via a scoped value bound at loop entry.
- **Tracing context**: `mochi.trace.context` is a `ScopedValue<TraceCtx>`; HTTP/db client libraries read it.
- **Configuration**: per-request feature flags are bound via scoped values, not thread locals.

Where Mochi runs on JDK 21 the binding is preview API; the wrapper class hides this. Where on JDK 25 LTS the API is final and stable.

## 9. Pinning hazards and mitigations

Even with JEP 491 (synchronized pinning fixed), some pinning sources remain. Mochi's runtime audits them:

| Source                              | Pre-JEP-491 | JDK 25 LTS         | Mochi mitigation                                          |
|-------------------------------------|-------------|--------------------|-----------------------------------------------------------|
| `synchronized` block contention     | Pinned      | **Fixed (JEP 491)**| None needed                                               |
| JNI / FFM downcall in native frame  | Pinned      | Still pinned       | Audit FFI; small/short native calls only                  |
| Class initialiser blocking          | Pinned      | Still pinned       | Pre-initialise hot classes at JVM start                   |
| Symbolic resolution during loading  | Pinned      | Still pinned       | AOT (JEP 514/515) reduces this                            |
| `Object.wait` (legacy)              | Pinned      | Not pinned (491)   | None                                                      |
| Old `Socket` IO                     | Was pinned  | Wrapped, not pinned| Use NIO `SocketChannel`-backed APIs anyway                |
| `FileChannel` reads                 | Can pin     | Can still pin      | Document; offer `AsynchronousFileChannel` wrapper         |

Concretely:

- **Mochi runtime never uses `synchronized` blocks on hot paths inside agents.** Even though JEP 491 makes them safe, `ReentrantLock` is more debuggable and we standardise on it. Codegen verifies this with a bytecode scan.
- **The Mochi FFI layer warns when an FFI call exceeds a tunable threshold** (default 1 ms); if you hit it, your native call is pinning a carrier.
- **The `mochi.telemetry` collector subscribes to the `jdk.VirtualThreadPinned` JFR event** and reports pinned-thread incidents. The `jdk.tracePinnedThreads` JVM flag was removed in JDK 24; JFR is the supported channel.

## 10. Fault model and supervision

On BEAM, agent crashes propagate via OTP supervisors and exit signals (see MEP-46 [[09-agent-streams]] §3 and §9). On JVM, an uncaught exception in a virtual thread terminates *only that virtual thread*. There is no built-in supervision tree.

Mochi-on-JVM provides supervision as a runtime library:

```mochi
let h = spawn Counter()
mochi.supervise(h, on_failure: restart)
```

lowers to a `mochi.agent.Supervisor` that:

1. Installs a per-thread uncaught exception handler when the agent virtual thread is created.
2. On uncaught exception: records the failure in `mochi.telemetry`, fires `DownListener` callbacks (Mochi `monitor` lowering), then per policy either:
   - `restart`: invokes the agent's `start` factory with the original constructor args; the new handle replaces the old in any registry.
   - `stop`: leaves the handle dead, downstream calls throw `AgentDownError`.
   - `escalate`: rethrows on the supervisor's thread, which itself can have a supervisor.
3. Restart counters and back-off (exponential, default 100 ms -> 5 s) prevent restart storms; after `max_restarts` (default 5 in `window` 60 s) the agent is moved to `stop`.

This is a faithful reimplementation of the OTP `one_for_one` strategy, in user space, without BEAM's process isolation. Specifically:

- An agent that corrupts global JVM state (e.g. a static map) is *not* restored by restart, unlike BEAM where the crashed process's heap is reclaimed.
- An agent that holds an open file or socket needs explicit cleanup; we expose `on_terminate` hooks for this.

Mochi's `link` and `monitor` primitives:

- `link a b` registers each handle as a `DownListener` on the other; when either crashes, the other receives `AgentLinked.Down(otherHandle, throwable)` in its mailbox.
- `monitor a` returns a `MonitorRef` whose `await()` blocks until the agent terminates, returning the final cause.

Both are implemented in `dev.mochi.runtime.agent.Linkage` as concurrent lists guarded by `ReentrantLock` (not `synchronized`, for the §9 reason).

## 11. Determinism for tests

Mochi has a deterministic-replay test mode (see MEP-47 [[11-testing-gates]]): on a given seed and message log, the same agent network must produce the same outputs.

On BEAM this is hard because the runtime scheduler is preemptive. On JVM with Loom it is also hard because the virtual-thread scheduler is not exposed (`ForkJoinPool` of carriers, work-stealing, no public hooks).

Mochi's workaround: in deterministic mode, **bypass virtual threads entirely**. The test runtime substitutes:

- `Executors.newVirtualThreadPerTaskExecutor()` with `Executors.newSingleThreadExecutor()` for each agent.
- A synthetic `MochiClock` (replaces `System.nanoTime`/`Instant.now`) under user control.
- A deterministic `mochi.stream` scheduler that drains all submitted items before yielding to the next publisher.

This loses the carrier-yield property but recovers determinism. Production runs use the Loom default; CI tests run deterministically. The switch is `MOCHI_SCHEDULER=deterministic` at JVM start.

A note on JEP 425's original promise: Loom mentioned that virtual-thread schedulers might one day be pluggable. As of JDK 25 LTS this remains an unsupported internal API. We do not depend on it.

## 12. Performance characteristics

Numbers below are from microbenchmarks on JDK 25 LTS, AdoptOpenJDK Temurin 25, x86_64, 8 cores, ZGC. Treat as orders of magnitude.

| Operation                                | Cost                         |
|------------------------------------------|------------------------------|
| Virtual thread creation                  | ~1 µs (vs ~1 ms platform)    |
| Virtual thread idle memory               | ~200 bytes (no live stack)   |
| Active virtual thread (small stack)      | 1-4 KB                       |
| `LinkedBlockingQueue` SPSC throughput    | 3-5M msg/s                   |
| `ArrayBlockingQueue` SPSC throughput     | 6-8M msg/s                   |
| Mochi cast (`agent.tell`)                | ~0.5-1 µs (enqueue only)     |
| Mochi call (`agent.method`)              | ~2-4 µs (enqueue + future)   |
| `spawn f` (no work)                      | ~1.5 µs                      |
| `SubmissionPublisher.submit` (1 subs)    | ~1 µs                        |
| `SubmissionPublisher.submit` (10 subs)   | ~5-8 µs                      |
| Stream subscribe lifecycle               | ~3-5 µs                      |

Reference deployments worth citing:

- **Helidon 4** (Oracle, 2024) rewrote its server core on virtual threads; reports 4-5x throughput vs Helidon 3 on the same code.
- **Vert.x 5** (Eclipse, 2024) added "virtual-thread verticles" alongside the event loop; the JIT-warmed steady state is competitive with the event loop and the code reads as synchronous.
- **Spring Boot 3.2+ with `spring.threads.virtual.enabled=true`** turns every Tomcat request thread into a virtual thread; broad production use.

The takeaway for Mochi: a million-agent program is feasible on a single JVM if state per agent is small. We test up to 100 K agents in the gate suite ([[11-testing-gates]]).

## 13. Comparison with BEAM (MEP-46)

Read alongside MEP-46 [[09-agent-streams]]:

| Dimension            | BEAM                                  | JVM/Loom                                  |
|----------------------|---------------------------------------|-------------------------------------------|
| Scheduling           | OS-thread-per-core, preemptive        | Virtual threads on carrier pool, cooperative |
| Per-agent heap       | Isolated, per-process GC              | Shared JVM heap                           |
| Send semantics       | Always copy (term cloning)            | Pass-by-reference (immutable types only safe) |
| Supervision          | OTP built in                          | Mochi runtime userspace                   |
| Hot reload           | Built in                              | `Instrumentation`, heavy                  |
| Cross-node           | `pg` is cluster-aware                 | Need extra layer (Kafka / Pulsar / NATS)  |
| Selective receive    | O(1) with `recv_marker`               | Mailbox is FIFO, no selective receive     |
| Preemption           | Reduction counting, fair              | None; runaway agent can starve carrier    |
| Distinct memory model| BEAM "no shared state" by construction| JMM happens-before via queue              |

The JVM wins on:

- Raw throughput in CPU-bound code (HotSpot beats BEAM by 5-20x in JIT-warmed numerics).
- Cheap inter-agent data sharing: a 10 MB blob sent to 100 agents on JVM is one heap reference; on BEAM it is 100 copies (unless it lives in the binary heap, which has its own rules).
- Tooling (profilers, debuggers, JFR).

The BEAM wins on:

- Fault containment: a crashed BEAM process cannot corrupt another's heap; on JVM, a runaway agent can OOM the entire VM.
- Supervision tree built-in.
- Hot code reload as a first-class citizen.

We document this trade-off prominently in MEP-47's risks section.

## 14. Comparison with native (MEP-45)

MEP-45 implements an M:N scheduler from scratch (work-stealing, per-OS-thread run queues, parking via futex / kqueue). The total scheduler code is ~3 KLOC of Mochi-runtime C and ~1 KLOC of platform glue.

MEP-47 inherits Loom's M:N for free. Zero scheduler code. The trade-off:

- Loom's scheduler is *not pluggable*. MEP-45 can pick stealing policies; MEP-47 cannot.
- Loom's scheduler is *much more mature*. JEP 491 alone took two years of staged work; we get the result for free.
- Loom's scheduler integrates with the JVM GC and JIT. MEP-45's scheduler is its own world; integrating with a native GC took half the MEP-45 effort.

For the same agent-heavy benchmark (100K agents, 10M messages, mixed call/cast), MEP-47 currently outperforms MEP-45 by ~1.6x on warm runs because HotSpot's JIT outpaces the MEP-45 native codegen on the dispatch loop. We expect MEP-45 to close this gap once we move to LLVM-backed AOT in MEP-45 v0.3.

## 15. Library inventory under `dev.mochi.runtime`

The JVM-target runtime exposes a small surface; codegen writes against it directly.

| Package                            | Class                  | Purpose                                  |
|------------------------------------|------------------------|------------------------------------------|
| `dev.mochi.runtime.agent`          | `Agent<T>`             | Marker interface for agent handles       |
| `dev.mochi.runtime.agent`          | `AgentBuilder<T>`      | Internal codegen factory                 |
| `dev.mochi.runtime.agent`          | `Mailbox<M>`           | Wrapper around `BlockingQueue<M>`        |
| `dev.mochi.runtime.agent`          | `Supervisor`           | Userspace supervision tree               |
| `dev.mochi.runtime.agent`          | `Linkage`              | `link` / `monitor` registry              |
| `dev.mochi.runtime.async`          | `Async<T>`             | `spawn` / `await` future wrapper         |
| `dev.mochi.runtime.async`          | `MochiAsyncError`      | Wraps `ExecutionException` causes        |
| `dev.mochi.runtime.stream`         | `Stream<T>`            | Wraps `SubmissionPublisher<T>`           |
| `dev.mochi.runtime.stream`         | `Subscriber<T>`        | Wraps Mochi `on` handler block           |
| `dev.mochi.runtime.stream`         | `ReplayStream<T>`      | Cold stream with bounded history         |
| `dev.mochi.runtime.stream.flow`    | `Map`, `Filter`, ...   | Small operator processors                |
| `dev.mochi.runtime.scope`          | `Scope`                | Wrapper over `StructuredTaskScope`       |
| `dev.mochi.runtime.context`        | `ScopedRef<T>`         | Wrapper over `ScopedValue<T>`            |
| `dev.mochi.runtime.telemetry`      | `Telemetry`            | JFR + structured logging                 |
| `dev.mochi.runtime.clock`          | `MochiClock`           | Real / synthetic, for §11                |

Public API surface is stable across JDK 21 -> 25 -> 26; preview-API usage is hidden behind these wrappers.

## 16. Reject pile

- **Akka / Pekko**. Out of scope. Akka's actor model is richer than Mochi's (supervision strategies, persistence, clustering, streams) and adds a 5 MB+ dependency. Mochi's agents are deliberately simpler. Users who want full actor semantics drop down via FFI; Mochi does not ship an Akka adapter.
- **Vert.x event loop verticles**. Out of scope. Verticles are a callback model; Loom + Mochi is a synchronous-looking model. We use Vert.x patterns as inspiration for backpressure, nothing more.
- **Reactor / RxJava as default**. Rejected; see §4. Optional bridge only.
- **GraalVM native-image as primary distribution**. Verified support but rejected as default. GraalVM 24 added good virtual-thread support (pre-initialised schedulers, JEP 425 fully working). However, Oracle's September 2025 announcement removed Native Image from Oracle Java SE products; GraalVM is now community-maintained and Oracle steers users to JEP 514/515 (AOT) on OpenJDK instead. Mochi's primary JVM distribution is OpenJDK 25 LTS; GraalVM native-image is a documented but secondary option (footprint and startup wins exist; reflection / dynamic loading gotchas remain).
- **Pluggable virtual-thread scheduler**. The internal API exists (`jdk.internal.vm.ContinuationScope`); Mochi does not use it. Determinism mode uses a single-thread executor instead (§11).
- **Per-agent isolates** (à la Dart). The JVM has no first-class isolate primitive; "isolates on the JVM" historically means JNI-spawned sub-JVMs (heavy) or `Project Loom`'s never-shipped Isolates JEP. Out of scope.

---

## Sources

1. JEP 444: Virtual Threads (Final). <https://openjdk.org/jeps/444>
2. JEP 491: Synchronize Virtual Threads without Pinning (JDK 24, carried into 25 LTS). <https://openjdk.org/jeps/491>
3. JEP 505: Structured Concurrency (Fifth Preview, JDK 25). <https://openjdk.org/jeps/505>
4. JEP 506: Scoped Values (Final in JDK 25). <https://openjdk.org/jeps/506>
5. JEP 525: Structured Concurrency (Sixth Preview, JDK 26). <https://openjdk.org/jeps/525>
6. `SubmissionPublisher` (Java SE 21). <https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/concurrent/SubmissionPublisher.html>
7. `java.util.concurrent.Flow` Javadoc.
8. Helidon 4 release notes, Oracle, 2024.
9. Vert.x 5 virtual-thread verticles, Eclipse Foundation, 2024.
10. "Virtual Threads After JEP 491: The Bottleneck Moved", Tiare Balbi, 2025.
11. JBS JDK-8338813, "Implement JEP 491", resolved 2024-11-04.
12. Oracle Java News Roundup on JEP 506 finalization, InfoQ, 2025-05.
13. "Oracle Shifts GraalVM Focus Away from Java", ADTmag, 2025-09-30.
14. GraalVM 22.3 release notes (initial virtual-thread support).
15. Spring Boot virtual-threads property docs, `spring.threads.virtual.enabled`.
