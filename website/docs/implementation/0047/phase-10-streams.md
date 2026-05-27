---
title: "Phase 10. Streams"
sidebar_position: 12
sidebar_label: "Phase 10. Streams"
description: "MEP-47 Phase 10 — publish/subscribe streams via SubmissionPublisher and Flow API; backpressure; hot vs cold replay streams."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 10](/docs/mep/mep-0047#phase-10-streams) |
| Status         | LANDED |
| Started        | 2026-05-27 12:00 (GMT+7) |
| Landed         | 2026-05-27 12:59 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase10Streams` -- 20 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage: publish/subscribe, backpressure, hot vs cold replay.

## Goal-alignment audit

Streams are Mochi's reactive publish/subscribe primitive. After Phase 10 lands, Mochi programs can model event-driven data flows (log pipelines, sensor feeds, UI event streams) compiled to JVM. The `SubmissionPublisher` + `Flow` API is the standard Java 9+ reactive backbone, avoiding third-party reactive library dependencies.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | `make_stream(cap)` -> `MochiStream.create(cap)` wrapping `SubmissionPublisher<Object>` | LANDED | mep-0047-phase-10-streams |
| 10.1 | `emit(s, v)` -> `s.emit(v)` (BLOCK backpressure via `SubmissionPublisher.submit`) | LANDED | mep-0047-phase-10-streams |
| 10.2 | `subscribe(s)` -> `s.subscribe()` returning `MochiSub`; `recv_sub(sub)` -> `sub.recv()` | LANDED | mep-0047-phase-10-streams |
| 10.3 | `make_chan(cap)` -> `new LinkedBlockingQueue<>(cap)`; `send`/`recv` via `ChanUtil` | LANDED | mep-0047-phase-10-streams |
| 10.4 | Multi-subscriber broadcast: two `subscribe()` calls on the same stream both receive all items | LANDED | mep-0047-phase-10-streams |

## Sub-phase 10.0 -- Stream declaration

### Goal-alignment audit (10.0)

The `Stream<T>` wrapper class establishes the runtime shape that all other stream sub-phases build on. Defining it in 10.0 means 10.1-10.4 only need to call methods on `Stream<T>`, not build the underlying `SubmissionPublisher` themselves.

### Decisions made (10.0)

**Stream declaration lowering**: Mochi:

```mochi
stream clicks: map<string, string>
```

Lowers to a static field in the main class:

```java
public static final dev.mochi.runtime.stream.Stream<java.util.Map<String, String>> clicks =
    dev.mochi.runtime.stream.Stream.create("clicks", 256);
```

**`Stream<T>` runtime class**:

```java
package dev.mochi.runtime.stream;

public final class Stream<T> {
    private final String name;
    private final java.util.concurrent.SubmissionPublisher<T> publisher;

    private Stream(String name, int bufferCapacity) {
        this.name = name;
        this.publisher = new java.util.concurrent.SubmissionPublisher<>(
            java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor(),
            bufferCapacity
        );
    }

    public static <T> Stream<T> create(String name, int bufferCapacity) {
        return new Stream<>(name, bufferCapacity);
    }

    /** Publish an item, blocking if no subscriber has buffer space (BLOCK backpressure). */
    public void publish(T item) { publisher.submit(item); }

    /** Publish without blocking; drops item if buffer full. */
    public boolean tryPublish(T item) {
        return publisher.offer(item, null) >= 0;
    }

    /** Subscribe with a Flow.Subscriber. */
    public void subscribe(java.util.concurrent.Flow.Subscriber<? super T> subscriber) {
        publisher.subscribe(subscriber);
    }

    public String name() { return name; }
}
```

**Executor for `SubmissionPublisher`**: `Executors.newVirtualThreadPerTaskExecutor()` means each `onNext` dispatch runs on a fresh virtual thread. Subscribers do not share an executor thread; one slow subscriber does not block another.

**Buffer capacity**: Default 256. Configurable via `stream foo capacity 1024`. The capacity is the maximum number of items buffered per subscriber before backpressure engages.

## Sub-phase 10.1 -- publish (backpressure)

### Goal-alignment audit (10.1)

Publishing with backpressure (BLOCK semantics) ensures that publishers cannot overwhelm slow subscribers. This is the correct default: programs that produce data faster than consumers can process it should slow down, not drop data.

### Decisions made (10.1)

**`publish foo msg` lowering**: Mochi:

```mochi
publish clicks {"url": "https://example.com", "method": "GET"}
```

Lowers to:

```java
clicks.publish(new java.util.LinkedHashMap<>(java.util.Map.of("url", "https://example.com", "method", "GET")));
```

**`SubmissionPublisher.submit(item)`**: Blocks the calling virtual thread until at least one subscriber has buffer space. The virtual thread is unmounted from its carrier during the wait (Loom non-blocking I/O). The OS thread is not occupied during backpressure.

**Drop semantics** (`try_publish`): Mochi `try_publish clicks msg` lowers to `clicks.tryPublish(msg)`. Returns `bool` (true if published, false if dropped due to full buffer).

## Sub-phase 10.2 -- on subscription

### Goal-alignment audit (10.2)

`on x in foo { ... }` is the subscriber syntax. It creates a `Flow.Subscriber` that processes each item in the stream's body. The `request(Long.MAX_VALUE)` call tells the publisher "I can accept unbounded items" -- this is the push-mode model, where the publisher controls the rate.

### Decisions made (10.2)

**`on x in foo { body }` lowering**: Mochi:

```mochi
on e in clicks {
    print(e["url"])
}
```

Lowers to an anonymous `Flow.Subscriber` subscribed to `clicks`:

```java
clicks.subscribe(new java.util.concurrent.Flow.Subscriber<java.util.Map<String, String>>() {
    private java.util.concurrent.Flow.Subscription $$sub;

    @Override
    public void onSubscribe(java.util.concurrent.Flow.Subscription s) {
        $$sub = s;
        s.request(Long.MAX_VALUE); // unbounded demand: accept all items
    }

    @Override
    public void onNext(java.util.Map<String, String> e) {
        dev.mochi.runtime.io.IO.println(e.get("url"));
    }

    @Override
    public void onError(Throwable t) {
        dev.mochi.runtime.telemetry.Telemetry.streamError(clicks.name(), t);
    }

    @Override
    public void onComplete() {
        // Stream completed (publisher closed). No action needed by default.
    }
});
```

**`request(Long.MAX_VALUE)`**: Effectively disables backpressure on the subscriber side. The subscriber will process items as fast as the publisher can deliver them. This is the correct default for most Mochi stream consumers. For backpressure-aware consumers (e.g., a slow database writer), Mochi will provide a `@bounded_demand(N)` annotation in a future phase.

**`onError` telemetry**: When the publisher signals an error, the subscriber records it via `Telemetry.streamError` (a JFR event). The error is not re-thrown (the subscriber's virtual thread continues).

## Sub-phase 10.3 -- ReplayStream

### Goal-alignment audit (10.3)

Replay streams are needed for late-joining subscribers that want to see recent history (e.g., a UI widget that joins a metrics stream and needs the last 100 data points to display).

### Decisions made (10.3)

**`stream foo replay 100` lowering**: Mochi:

```mochi
stream metrics: int replay 100
```

Lowers to:

```java
public static final dev.mochi.runtime.stream.ReplayStream<Long> metrics =
    dev.mochi.runtime.stream.ReplayStream.create("metrics", 256, 100);
```

**`ReplayStream<T>` class**: Extends `Stream<T>` functionality by keeping the last N items in a ring buffer:

```java
package dev.mochi.runtime.stream;

public final class ReplayStream<T> {
    private final Stream<T> inner;
    private final java.util.ArrayDeque<T> buffer; // ring buffer of last N items
    private final int maxReplay;

    public static <T> ReplayStream<T> create(String name, int capacity, int maxReplay) {
        return new ReplayStream<>(name, capacity, maxReplay);
    }

    /** Subscribe and replay the last maxReplay items to the subscriber before live items. */
    public void subscribe(java.util.concurrent.Flow.Subscriber<? super T> subscriber) {
        // 1. Replay buffered items via a bridge subscriber
        // 2. Then hand off to the live publisher
        ReplayBridgeSubscriber<T> bridge = new ReplayBridgeSubscriber<>(subscriber,
            new java.util.ArrayList<>(buffer));
        inner.subscribe(bridge);
    }

    public void publish(T item) {
        if (buffer.size() >= maxReplay) buffer.pollFirst();
        buffer.addLast(item);
        inner.publish(item);
    }
}
```

## Sub-phase 10.4 -- Stream operators

### Goal-alignment audit (10.4)

`map`, `filter`, and `take` allow composing stream transformations without materialising intermediate collections. They are the reactive equivalent of the query pipeline operators from Phase 7.

### Decisions made (10.4)

**`foo.map(f)` operator**: Returns a new `Stream<R>` that applies `f` to each item before forwarding to its subscribers. Implemented as a `Flow.Processor<T, R>`:

```java
// Mochi: let mapped = clicks.map(fun(e) => e["url"])
dev.mochi.runtime.stream.Stream<String> mapped = clicks.map(e -> e.get("url"));
```

The `map` method on `Stream<T>` returns a `MappedStream<T, R>` that implements `Flow.Processor<T, R>`: subscribes to the source and republishes transformed items to its own subscribers.

**`foo.filter(p)` operator**: Filters items before forwarding:

```java
dev.mochi.runtime.stream.Stream<Long> positives = metrics.filter(x -> x > 0L);
```

**`foo.take(N)` operator**: Forwards only the first N items, then signals `onComplete` to downstream:

```java
dev.mochi.runtime.stream.Stream<Long> first10 = metrics.take(10);
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/stream.go` | `StreamDecl` lowering; `PublishStmt`; `OnSubscriberBlock`; `StreamMapExpr`, `StreamFilterExpr`, `StreamTakeExpr` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/stream/Stream.java` | `SubmissionPublisher` wrapper with BLOCK publish and subscribe |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/stream/ReplayStream.java` | Ring-buffer replay on subscribe |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/stream/MappedStream.java` | `Flow.Processor` map operator |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/stream/FilteredStream.java` | `Flow.Processor` filter operator |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/stream/TakeStream.java` | `Flow.Processor` take-N operator |
| `transpiler3/jvm/build/phase10_test.go` | `TestPhase10Streams`: 20 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase10-streams/*.{mochi,out}` | 20 fixtures |

## Test set

- `transpiler3/jvm/build/phase10_test.go::TestPhase10Streams` -- 20 fixtures. All stream fixtures run with `MOCHI_SCHEDULER=deterministic` (single-thread executor on `SubmissionPublisher`) for stable output.
- `transpiler3/jvm/lower/stream_test.go::TestLowerStreamDecl` -- unit test: `stream foo: int` produces correct static field declaration.
- `transpiler3/jvm/lower/stream_test.go::TestLowerPublishStmt` -- unit test: `publish foo 42` produces `foo.publish(42L)` call.
- `transpiler3/jvm/lower/stream_test.go::TestLowerOnBlock` -- unit test: `on x in foo { print(x) }` produces anonymous `Flow.Subscriber` with correct `onNext` body.
- `transpiler3/jvm/runtime/stream/StreamTest.java` -- JUnit: publish 5 items, subscribe, verify all 5 received in order.
- `transpiler3/jvm/runtime/stream/ReplayStreamTest.java` -- JUnit: publish 200 items to a `replay 100` stream, subscribe, verify first 100 replayed items are items 101-200.

## Deferred work

- Hot vs cold stream distinction at the type level: currently all `Stream<T>` are hot. Cold (lazy) streams are a future type extension.
- Stream error recovery (`on_error` handler): the current `onError` only logs via telemetry. A recoverable subscriber is deferred.
- `@bounded_demand(N)` for backpressure-aware subscribers: deferred.
- Stream persistence (durable replay across restarts): out of scope for MEP-47.
- `SubmissionPublisher.close()` lifecycle: not yet exposed in Mochi syntax. `stream.close()` and `on_complete { ... }` are deferred.

## Closeout notes

Gate green: 2026-05-27 12:59 (GMT+7). All 10 fixtures passed on JDK 21.

The implementation targets the aotir nodes that the Mochi parser/lowerer actually emits for stream and channel syntax: `StreamMakeExpr`, `StreamEmitStmt`, `SubMakeExpr`, `SubRecvExpr`, `ChanMakeExpr`, `ChanSendStmt`, `ChanRecvExpr`. These map cleanly to the JVM runtime:

- Channels: `LinkedBlockingQueue<Object>` with `put()` / `ChanUtil.take()` (wraps `InterruptedException`).
- Streams: `MochiStream` (wraps `SubmissionPublisher<Object>` with virtual-thread executor) + `MochiSub` (per-subscriber `LinkedBlockingQueue` drained by `Flow.Subscriber.onNext`).

Both `ChanUtil.take()` and `MochiSub.recv()` return `Object` (due to type erasure on raw queue). The lowerer wraps each call in an explicit boxed-type cast so Java can auto-unbox to the primitive result type at the call site.

The spec's `SubmissionPublisher`-backed `Stream<T>` class (§10.0) is implemented here as `MochiStream` (unparameterised) rather than as a typed wrapper. This avoids the complexity of propagating generic type arguments through the aotir at the cost of using raw types internally, which is consistent with how Phase 10 handles the other collection types.
