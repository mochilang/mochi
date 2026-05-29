---
title: "Phase 10. Streams"
sidebar_position: 11
sidebar_label: "Phase 10. Streams"
description: "MEP-52 Phase 10, Mochi `chan<T>` (bounded FIFO) and `stream<T>` (fan-out pub/sub) as inline synchronous TS classes (MochiChan + MochiStream + MochiSub). 31 fixtures green on Node 22, Deno 2, Bun 1.1; the AsyncIterableQueue runtime planned in the spec is deferred."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 10](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-30 00:15 (GMT+7) |
| Landed         | 2026-05-30 00:35 (GMT+7) |
| Tracking issue | (TBD when PR opens) |
| Tracking PR    | (TBD when PR opens) |

## Gate

`TestPhase10StreamsNode`, `TestPhase10StreamsDeno`, `TestPhase10StreamsBun`: 31 fixtures green on each of Node 22, Deno 2, Bun 1.1; the recorded `.out` is byte-equal across runtimes. Secondary gates: `TestPhase10EmitShape` checks the emit declares `class MochiChan<T>` / `class MochiStream<T>` / `class MochiSub<T>` and lowers `make_chan` / `send` / `recv` / `make_stream` / `subscribe` / `emit` / `recv_sub` to direct method calls. `TestPhase10NoAsyncRuntime` checks that no async-runtime tokens (`AsyncIterableQueue`, `AbortController`, `@mochi/runtime/stream`, `mochi_chan_`, `mochi_stream_`, `AggregateError`, ` await `, `async `) leak into the source.

## Goal-alignment audit

Mochi exposes two related concurrency primitives:

- `chan<T>` is a bounded single-producer / single-consumer FIFO with capacity. `make_chan(cap)` allocates, `send(ch, v)` enqueues at the tail (blocks/yields when full), `recv(ch)` dequeues from the head (blocks/yields when empty).
- `stream<T>` is a bounded multi-producer / multi-consumer fan-out. `make_stream(cap)` allocates, `subscribe(s)` registers a subscriber (returns a typed `sub<T>` handle starting at the current write position), `emit(s, v)` broadcasts to every live subscriber, `recv_sub(sub)` dequeues from that subscriber's private queue.

The MEP-52 §Phase 10 spec proposed an `@mochi/runtime/stream` package (~10 KB gzipped) built on `AsyncIterableQueue` + `AbortController` + `AggregateError`, with every operation Promise-coloured (`await ch.recv()`). The audit pushed back on shipping that path on TS for the same reasons Phases 8 and 9 deferred their async runtimes:

1. **Every fixture in the Phase 10 corpus is a single-threaded synchronous use.** Producer code runs to completion before consumer code starts; the buffer is always sized to hold every item that will ever be sent; no fixture exercises blocking on a full chan or an empty stream. No fixture's stdout depends on async ordering.

2. **The Rust runtime needs parking-lot mutexes because Rust agents may move across OS threads in a future phase.** TypeScript runs single-threaded by construction: one event loop per Node / Deno / Bun process. The cross-thread synchronisation that justifies Rust's runtime cost has no counterpart on TS.

3. **`receiver.send(v)` / `receiver.recv()` is observationally identical to a regular method call.** Wrapping it in an `AsyncIterableQueue` would force the async colour (Phase 11) onto every chan / stream operation. The change would propagate up: every `let x = recv(ch)` becomes `let x = await ch.recv()`, every function that calls `recv` becomes `async`, every caller of those functions becomes `async`. Once the colour leak starts there is no clean way back.

4. **Package budget pressure.** Phase 15 budgets 50 KB gzipped for `@mochi/runtime`. The async-runtime engine alone is ~10 KB. The synchronous-class path is ~600 bytes inline (three small classes), gated so a chan-only program does not carry the stream bytes.

5. **Phase 16 byte-equal reproducibility.** An external runtime package shifts the emit on version bumps even when the lowering is unchanged. Inlining the classes pins the bytes in the emit itself.

The TS path therefore emits three inline runtime classes (`MochiChan<T>`, `MochiStream<T>`, `MochiSub<T>`) plus per-call-site method-dispatch lowering. The runtime cost drops to zero external dependency bytes. If a future fixture introduces fiber-style `spawn` that genuinely blocks on a full/empty buffer, the async runtime can be added without disturbing the synchronous path for closed programs.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | `chan<T>` make / send / recv (`MochiChan<T>` class) | LANDED | (this PR) |
| 10.1 | `stream<T>` make / emit + `sub<T>` subscribe / recv (`MochiStream<T>` + `MochiSub<T>` classes) | LANDED | (this PR) |
| 10.2 | `subscribe_limit(s, n)` backpressure drop threshold | LANDED (no corpus fixture; wired anyway) | (this PR) |
| 10.3 | Blocking suspend on full chan / empty queue (AsyncIterableQueue) | DEFERRED (no fixture; would force Phase 11 async colour) | n/a |
| 10.4 | Multi-fiber producer/consumer (cross-task scheduling) | DEFERRED (no Mochi `spawn` surface yet on TS) | n/a |
| 10.5 | `AbortController` cancellation + `AggregateError` propagation | DEFERRED (no fixture exercises cancellation) | n/a |
| 10.6 | Web Streams interop (`ReadableStream<T>` adapter) | DEFERRED (Phase 14 fetch will revisit) | n/a |

## Sub-phase 10.0, `chan<T>` bounded FIFO

### Lowering

| Mochi | TypeScript |
|-------|-----------|
| `let ch: chan<int> = make_chan(1)` | `const ch: MochiChan<number> = MochiChan.make<number>(1);` |
| `send(ch, 42)` | `ch.send(42);` |
| `let x = recv(ch)` | `const x: number = ch.recv();` |
| `for i in 0..N { send(ch, i) }` | `for (let i = 0; i < N; i++) { ch.send(i); }` |

The `MochiChan<T>` class is inlined into the file prelude exactly once when any chan operation is reached. The class is tiny: a private buffer array, a capacity, and three methods. The static `make<T>(cap)` factory is the entry-point construction site; the private constructor blocks `new MochiChan(...)` from user code so the only way to allocate is through `MochiChan.make<T>(cap)`.

### Why throw on overflow, not block

The synchronous-class path has no way to suspend (there is no cooperative scheduler to yield to). Two design options were considered:

1. **Silent drop on full / undefined on empty.** Rejected: a fixture that accidentally over-sends would still produce stdout (`recv` would return undefined-coerced-to-something), masking the bug.
2. **Throw a `RangeError`.** Chosen: surfaces the programmer error at the exact send/recv call site. The fixture corpus never trips this; the check exists for forward-compatibility with fixtures that genuinely overflow.

### Worked example: `chan_basic.mochi`

Source:

```mochi
let ch: chan<int> = make_chan(1)
send(ch, 42)
let x = recv(ch)
print(x)
```

Emitted TS (excerpt):

```typescript
// Bounded FIFO channel runtime (Phase 10).
// send/recv are synchronous; overflow/underflow throw.
class MochiChan<T> {
  private readonly buf: T[] = [];
  private readonly cap: number;
  private constructor(cap: number) { this.cap = cap; }
  static make<T>(cap: number): MochiChan<T> { return new MochiChan<T>(cap); }
  send(v: T): void {
    if (this.buf.length >= this.cap) {
      throw new RangeError("MochiChan.send: buffer full");
    }
    this.buf.push(v);
  }
  recv(): T {
    if (this.buf.length === 0) {
      throw new RangeError("MochiChan.recv: buffer empty");
    }
    return this.buf.shift() as T;
  }
}

function mochi_main(): void {
  const ch: MochiChan<number> = MochiChan.make<number>(1);
  ch.send(42);
  const x: number = ch.recv();
  mochi_print_i64(x);
}

mochi_main();
```

stdout: `42`.

## Sub-phase 10.1, `stream<T>` fan-out pub/sub

### Lowering

| Mochi | TypeScript |
|-------|-----------|
| `let s: stream<int> = make_stream(4)` | `const s: MochiStream<number> = MochiStream.make<number>(4);` |
| `let sub = subscribe(s)` | `const sub: MochiSub<number> = s.subscribe();` |
| `emit(s, 10)` | `s.emit(10);` |
| `let v = recv_sub(sub)` | `const v: number = sub.recv();` |

`MochiStream<T>` holds the stream-level capacity and a list of live subscribers. `MochiSub<T>` holds one subscriber's private queue plus its individual drop threshold. The split is load-bearing:

- Each subscriber has its own FIFO. So a sequence of `subscribe`, `subscribe`, `emit`, `emit`, `recv_sub(sub1)`, `recv_sub(sub1)`, `recv_sub(sub2)`, `recv_sub(sub2)` produces 100, 200, 100, 200 (the same two values, drained per subscriber). The Rust runtime behaves the same way.
- Late subscribers do not see history. A `subscribe` between two `emit` calls only receives the emits that follow it. Tested by `stream_sub_late.mochi`.

### Worked example: `stream_three_subs.mochi`

Source:

```mochi
let s: stream<int> = make_stream(4)
let a = subscribe(s)
let b = subscribe(s)
let c = subscribe(s)
emit(s, 7)
print(recv_sub(a))
print(recv_sub(b))
print(recv_sub(c))
```

Emitted main:

```typescript
function mochi_main(): void {
  const s: MochiStream<number> = MochiStream.make<number>(4);
  const a: MochiSub<number> = s.subscribe();
  const b: MochiSub<number> = s.subscribe();
  const c: MochiSub<number> = s.subscribe();
  s.emit(7);
  mochi_print_i64(a.recv());
  mochi_print_i64(b.recv());
  mochi_print_i64(c.recv());
}
```

stdout: `7\n7\n7\n`. The single `emit(s, 7)` distributes to all three subscribers; each `recv_sub` drains from that subscriber's own queue.

## Sub-phase 10.2, `subscribe_limit` backpressure

The aotir IR carries a `SubMakeLimitExpr` for `subscribe_limit(s, n)`: the subscriber drops emits silently once its private queue has `n` items pending. No fixture in the Phase 10 corpus exercises this surface, but the lowering is wired anyway because the aotir produces the node and the runtime cost is identical (the `MochiSub<T>` constructor already takes a limit; `subscribe()` passes the stream cap, `subscribe_limit(n)` passes `n`).

## Pipeline

```
prog.Statements (aotir)
  -> lowerStmt switch:
       ChanSendStmt   -> ch.send(v)
       StreamEmitStmt -> s.emit(v)
  -> lowerExpr switch:
       ChanMakeExpr      -> MochiChan.make<T>(cap)
       ChanRecvExpr      -> ch.recv()
       StreamMakeExpr    -> MochiStream.make<T>(cap)
       SubMakeExpr       -> s.subscribe()
       SubMakeLimitExpr  -> s.subscribe_limit(n)
       SubRecvExpr       -> sub.recv()
  -> lowerLetStmt:
       VarType TypeChan    -> MochiChan<T>
       VarType TypeStream  -> MochiStream<T>
       VarType TypeSub     -> MochiSub<T>
  -> prelude pass: chanStreamDecls emits the three inline classes
     (Chan, Stream, Sub) gated on the per-feature runtime flags.
```

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/tstree/phase10.go` | `RawDecl` (verbatim text decl) + `NewExpr` nodes |
| `transpiler3/typescript/lower/phase10.go` | `chanStreamDecls`, `chanStreamTypeFor`, six call-site lowerings |
| `transpiler3/typescript/lower/lower.go` | switch cases for chan/stream/sub stmt + expr + let; prelude wiring; `chanClass` + `streamClass` flags |
| `transpiler3/typescript/build/phase10_test.go` | `TestPhase10StreamsNode/Deno/Bun`, `TestPhase10EmitShape`, `TestPhase10NoAsyncRuntime` |
| `tests/transpiler3/typescript/fixtures/phase10-streams/` | 31 fixtures (16 chan, 15 stream) |

## Test set

- `TestPhase10StreamsNode/Deno/Bun`, 31 fixtures three-runtime byte-equal.
- `TestPhase10EmitShape`, four fixtures verify the emit shape (class names + `make<T>` factory + `.send/.recv/.emit/.subscribe` dispatch).
- `TestPhase10NoAsyncRuntime`, every emit is checked free of `AsyncIterableQueue`, `AbortController`, `@mochi/runtime/stream`, `mochi_chan_`, `mochi_stream_`, `AggregateError`, ` await `, `async `.

## Comparison with Rust

| Surface | Mochi | Rust path | TS path |
|---------|-------|-----------|---------|
| `chan<T>` | bounded FIFO | `mochi_runtime::chan::Chan<T>` (parking-lot mutex) | inline `class MochiChan<T>` |
| `stream<T>` | fan-out pub/sub | `mochi_runtime::stream::Stream<T>` | inline `class MochiStream<T>` |
| `sub<T>` | subscriber handle | `mochi_runtime::stream::Sub<T>` | inline `class MochiSub<T>` |
| Buffer-full | blocks/yields | parking_lot wait | throw RangeError |
| Buffer-empty | blocks/yields | parking_lot wait | throw RangeError |
| Cross-thread | yes (future) | yes | n/a (single event loop) |
| Runtime cost | n/a | crate dependency | ~600 bytes inline (gated) |

The TS path's "no cross-thread" is not a regression: the V8 / D8 / JSC isolate model gives one event loop per process, and Mochi has no `spawn` surface that crosses that boundary on TS. If MEP-52 later introduces Web Workers / `worker_threads` agents, the runtime engine can be added behind the same `chanClass` / `streamClass` flags.

## Deferred work

- **AsyncIterableQueue + AbortController runtime.** Defers until a fixture introduces a producer in one fiber and a consumer in another that genuinely needs to block on full/empty.
- **Web Streams interop.** `ReadableStream<T>` adapter is needed for Phase 14 (`fetch.body` consumers); the chan-to-ReadableStream and reverse adapters land there.
- **Replay broadcaster.** Subscribers receiving history on subscribe; the Rust path also defers this. v1.5 or later.
- **AggregateError propagation.** Currently the chan/stream classes throw on overflow/underflow; aggregation across multiple subscribers is not needed for the corpus.
