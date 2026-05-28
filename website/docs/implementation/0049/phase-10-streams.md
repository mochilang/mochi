---
title: "Phase 10. Streams (AsyncSequence)"
sidebar_position: 14
sidebar_label: "Phase 10. Streams"
description: "MEP-49 Phase 10 — stream<T> to AsyncStream<T>; producer closures; debounce, throttle, merge, zip via swift-async-algorithms; for-await consumption."
---

# Phase 10. Streams (AsyncSequence)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 10](/docs/mep/mep-0049#phase-10-streams) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase10Streams`: 20 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Mochi streams are the primary abstraction for push-based data pipelines: sensor data, network events, user input events, agent output sequences. Swift `AsyncSequence` is the semantic match. The `swift-async-algorithms` package (already pulled in for Phase 7's async queries) provides `debounce`, `throttle`, `merge`, `zip`, and `AsyncChannel` -- the building blocks for Mochi's stream operators.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | `stream<T>` → `AsyncStream<T>`; producer closure via `AsyncStream.Continuation`; `for await` consumption | NOT STARTED | — |
| 10.1 | `stream.map(f)` → `.map(f)`; `stream.filter(p)` → `.filter(p)`; `stream.flat_map(f)` → `.flatMap(f)` | NOT STARTED | — |
| 10.2 | `stream.debounce(interval)` → `swift-async-algorithms` `.debounce(for:clock:)`; `throttle` | NOT STARTED | — |
| 10.3 | `stream.merge(s2)` → `merge(s, s2)`; `stream.zip(s2)` → `zip(s, s2)` | NOT STARTED | — |
| 10.4 | `AsyncChannel<T>` from `swift-async-algorithms` for multi-producer streams | NOT STARTED | — |
| 10.5 | Determinism gate: `TestStreamDeterminism` -- multiple runs produce identical output | NOT STARTED | — |

## Sub-phase 10.0 -- Stream creation

### Decisions made (10.0)

**`AsyncStream<T>` as the backing type**: Mochi `stream<T>` lowers to Swift `AsyncStream<T>`. `AsyncStream` (SE-0314, Swift 5.5+) is the stdlib async sequence type with a producer continuation. It is `Sendable` and safe across actor boundaries.

**Producer closure lowering**: a Mochi stream producer `stream { emit -> ... }` lowers to `AsyncStream<T>` with a continuation:

```swift
// Mochi: let s = stream { emit -> stream<int>
//     for i in 1..10 { emit(i) }
// }
let s = AsyncStream<Int64> { continuation in
    for i in Int64(1)...Int64(10) {
        continuation.yield(i)
    }
    continuation.finish()
}
```

**`AsyncStream.makeStream`**: for streams shared between a producer and consumer in different tasks (e.g., a sensor driver feeding an agent), the `makeStream` pattern (SE-0388) is used:

```swift
let (stream, continuation) = AsyncStream<Int64>.makeStream(
    bufferingPolicy: .bufferingNewest(256)
)
```

**`for await` consumption**: Mochi `for x in s { ... }` where `s: stream<T>` → Swift `for await x in s { ... }`. The `for await` loop is the primary consumption pattern.

**Cancellation**: Swift structured concurrency cancels the `AsyncStream` producer task when the consuming task is cancelled. The `continuation.onTermination` callback is set in the producer to clean up resources.

## Sub-phase 10.1 -- Stream operators

### Decisions made (10.1)

**`stream.map(f)`**: `AsyncSequence.map` is in the Swift stdlib. `s.map { x in f(x) }` returns `AsyncMapSequence<AsyncStream<T>, U>`.

**`stream.filter(p)`**: `s.filter { x in p(x) }` returns `AsyncFilterSequence<AsyncStream<T>>`.

**`stream.flat_map(f)`**: `s.flatMap { x in f(x) }` where `f` returns an `AsyncSequence`. Requires the async-algorithms `flatMap` overload that works on `AsyncSequence` sources; the stdlib `flatMap` on `AsyncSequence` is available in Swift 5.7+.

**`stream.reduce(init, f)`**: `await s.reduce(init, f)`. Terminal operator; suspends until the stream finishes.

**`stream.first(where:)`**: `await s.first(where: p)`. Returns `T?`.

**`stream.take(n)`**: → `s.prefix(n)` (AsyncPrefixSequence from stdlib).

**`stream.drop(n)`**: → `s.dropFirst(n)` (AsyncDropFirstSequence from stdlib).

## Sub-phase 10.2 -- Debounce and throttle

### Decisions made (10.2)

**`swift-async-algorithms` dependency**: already declared in MochiRuntime `Package.swift` for Phase 7. Phase 10 uses the time-based operators.

**`stream.debounce(interval)`**: → `s.debounce(for: .seconds(interval), clock: .suspending)`. Uses `swift-async-algorithms`'s `debounce` operator (SE-0230 + AsyncAlgorithms). The `.suspending` clock is used (matches wall time; `.continuous` for CPU time). `interval` is a Mochi `float` in seconds, converted to `Swift.Duration` via `.seconds(interval)`.

**`stream.throttle(interval)`**: → `s.throttle(for: .seconds(interval), clock: .suspending, reducing: { _, new in new })`. The `reducing` closure controls how simultaneous events within the window are combined; default is to take the latest.

**`stream.chunk(size: n)`**: → `s.chunks(ofCount: Int(n))`. From `swift-async-algorithms`. Buffers elements into arrays of up to `n` elements.

**`stream.chunk(duration: t)`**: → `s.chunked(by: .repeating(every: .seconds(t), clock: .suspending))`. Time-windowed chunking. Returns `AsyncStream<[T]>`.

## Sub-phase 10.3 -- Merge and zip

### Decisions made (10.3)

**`stream.merge(s2)`**: → `merge(s, s2)` from `swift-async-algorithms`. Returns an async sequence that yields elements from both streams as they arrive, in arrival order. The merged sequence finishes when both input streams finish.

**`stream.zip(s2)`**: → `zip(s, s2)` from `swift-async-algorithms`. Returns pairs `(T, U)`. Finishes when either stream finishes (truncating semantics, matching Mochi's `list.zip`).

**`stream.combine_latest(s2)`**: yields the latest value from each stream whenever either produces a new value. Uses `swift-async-algorithms` `combineLatest` function. Returns `(T, U)`.

**`merge` with more than 2 streams**: `merge(s1, s2, s3)` -- `swift-async-algorithms` supports variadic `merge` up to some arity. For more than 3 streams, the lowerer nests: `merge(merge(s1, s2), merge(s3, s4))`.

## Sub-phase 10.4 -- AsyncChannel for multi-producer

### Decisions made (10.4)

**`AsyncChannel<T>` from `swift-async-algorithms`**: `AsyncChannel` is a bounded channel where multiple producers can `send` and a single consumer iterates with `for await`. It provides back-pressure: `send` suspends when the channel is full.

**Mochi `channel<T>` type**: maps to `AsyncChannel<T>`. Used when multiple agents or tasks produce events into a shared stream.

**Producer**: `channel.send(x)` → `await ch.send(x)`. `send` is `async`; it suspends if the channel buffer is at capacity.

**Consumer**: `for await x in channel { ... }`. The channel finishes when `channel.finish()` is called on the producer side.

**`AsyncStream` vs `AsyncChannel`**: `AsyncStream` is single-producer with a fire-and-forget continuation (non-suspending `yield`). `AsyncChannel` is multi-producer with suspending `send`. The lowerer chooses based on Mochi source: `stream { emit -> ... }` → `AsyncStream`; `channel<T>()` → `AsyncChannel`.

## Sub-phase 10.5 -- Determinism gate

### Decisions made (10.5)

**Non-determinism risk**: `merge(s1, s2)` can yield elements in any interleaved order. Tests that assert exact output order would be flaky. The test gate for merge fixtures checks that the multiset of output elements is correct (order-independent), not the exact sequence.

**`TestStreamDeterminism`**: runs each stream fixture 5 times and asserts that all 5 runs produce identical stdout. This catches timing-dependent bugs (e.g., a `Task.yield()` call that produces different scheduling outcomes). For merge fixtures, only the sorted output is compared.

**Determinism strategy for tests**: stream fixtures that involve merge or non-deterministic ordering use a `sort` step before comparison:

```swift
// In test harness:
let results = await s.reduce([]) { acc, x in acc + [x] }
let sorted = results.sorted()
XCTAssertEqual(sorted, expected.sorted())
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/stream.go` | `StreamExpr`, `ForStreamStmt` → `AsyncStream`; producer closure; `for await` |
| `transpiler3/swift/lower/lower.go` | `stream.map`, `filter`, `flat_map`, `reduce`, `take`, `drop` operators |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Stream.swift` | `mochiDebounce`, `mochiThrottle`, `mochiChunk` wrappers |
| `transpiler3/swift/runtime/Package.swift` | `swift-async-algorithms` already added in Phase 7 |
| `transpiler3/swift/build/phase10_test.go` | `TestPhase10Streams`: 20 fixtures; `TestStreamDeterminism` |
| `tests/transpiler3/swift/fixtures/phase10-streams/` | 20 fixture directories |

## Test set

- `TestPhase10Streams` -- 20 fixtures covering: `stream_basic`, `stream_finite`, `stream_map`, `stream_filter`, `stream_flat_map`, `stream_reduce`, `stream_take`, `stream_drop`, `stream_debounce`, `stream_throttle`, `stream_chunk_count`, `stream_chunk_time`, `stream_merge`, `stream_zip`, `stream_combine_latest`, `stream_channel_single`, `stream_channel_multi`, `stream_from_list`, `stream_to_list`, `stream_cancel`.

## Deferred work

- `stream<T>` from HTTP SSE (Server-Sent Events). Deferred to Phase 14 (fetch).
- `stream<T>` from WebSocket. Deferred to Phase 14.
- Backpressure control protocol (`AsyncBackpressuredStream`, proposed). Deferred pending SE acceptance.
- `stream.window(size: n)` sliding window. Deferred -- `swift-async-algorithms` does not yet have this.
