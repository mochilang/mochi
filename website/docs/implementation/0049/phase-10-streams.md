---
title: "Phase 10. Streams (MochiStream, synchronous)"
sidebar_position: 14
sidebar_label: "Phase 10. Streams"
description: "MEP-49 Phase 10 — stream<T> to MochiStream<T>/MochiSub<T>; synchronous emit/subscribe/recv model; for-in over subscriber buffer."
---

# Phase 10. Streams (MochiStream, synchronous)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 10](/docs/mep/mep-0049#phase-10-streams) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:54 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase10Streams`: 7 fixtures green on Swift 6.0+, macOS 15. Gate builds each fixture and compares stdout to `.expected`.

## Goal-alignment audit

The v1 stream gate uses synchronous fixtures inherited from the BEAM backend: all emissions happen before any receive, so there is no need for async scheduling. `MochiStream<T>` is an MPMC broadcast class: `emit` appends to each subscriber's buffer; `MochiSub<T>.recv()` reads from that buffer in order. This maps exactly onto the fixture semantics and keeps the generated code readable without async/await. The `AsyncStream<T>`-based pattern is deferred to a future sub-MEP.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | `StreamMakeExpr` → `MochiStream<T>(capacity:)`; `StreamSubExpr` → `.subscribe()` | LANDED | mep/0049-phase-10 |
| 10.1 | `StreamEmitStmt` → `stream.emit(value)` | LANDED | mep/0049-phase-10 |
| 10.2 | `StreamRecvExpr` → `sub.recv()` | LANDED | mep/0049-phase-10 |
| 10.3 | `AsyncStream<T>`, `for await`, debounce/throttle via swift-async-algorithms | DEFERRED | — |
| 10.4 | `stream.map`, `filter`, `merge`, `zip` operators | DEFERRED | — |

## Sub-phase 10.0 -- Stream and subscriber creation

### Decisions made (10.0)

**`MochiStream<T>`**: the runtime class in `Stream.swift`. Holds a list of `MochiSub<T>` subscribers. Created as:

```swift
let s = MochiStream<Int64>(capacity: Int64(0))
```

The `capacity` parameter is accepted but currently unused (retained for API compatibility with future bounded-buffer variants).

**`MochiSub<T>`**: created by calling `stream.subscribe()`. Holds an internal `buffer: [T]` array and a `readIndex`. `recv()` returns `buffer[readIndex]` and advances `readIndex`.

**Type mapping**: `stream<int>` → `MochiStream<Int64>`, `stream<string>` → `MochiStream<String>`, `stream<bool>` → `MochiStream<Bool>`, `stream<float>` → `MochiStream<Double>`.

## Sub-phase 10.1 -- Emit

### Decisions made (10.1)

**`StreamEmitStmt`**: the IR node for emitting a value into a stream. The lowerer emits:

```swift
s.emit(value)
```

Each subscriber's buffer receives the value.

## Sub-phase 10.2 -- Receive

### Decisions made (10.2)

**`StreamRecvExpr`**: the IR node for receiving the next value from a subscriber. The lowerer emits:

```swift
sub.recv()
```

`recv()` is a synchronous call; it panics (index out of bounds) if called when the buffer is empty, matching the semantics of the fixture suite where all emissions precede all receives.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `StreamMakeExpr`, `SubMakeExpr`, `SubRecvExpr`, `StreamEmitStmt` lowering |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Stream.swift` | `MochiStream<T>` and `MochiSub<T>` classes |
| `transpiler3/swift/build/phase10_test.go` | `TestPhase10Streams`: 7 fixtures |
| `tests/transpiler3/swift/fixtures/phase10-streams/` | 7 fixture directories |

## Test set

- `TestPhase10Streams` -- 7 fixtures: `stream_bool`, `stream_emit_after_sub`, `stream_float`, `stream_int`, `stream_loop`, `stream_multi_sub`, `stream_string`.

## Deferred work

- `AsyncStream<T>` + `for await` consumption. Deferred to Phase 10.3.
- `stream.map(f)`, `stream.filter(p)`, `stream.flat_map(f)`. Deferred to Phase 10.4.
- `stream.debounce(interval)`, `stream.throttle(interval)` via `swift-async-algorithms`. Deferred.
- `stream.merge(s2)`, `stream.zip(s2)` via `swift-async-algorithms`. Deferred.
- Multi-producer `AsyncChannel<T>` from `swift-async-algorithms`. Deferred.
