---
title: "Phase 10. Streams"
sidebar_position: 11
sidebar_label: "Phase 10. Streams"
description: "MEP-52 Phase 10, Mochi streams as AsyncIterable<T> / AsyncGenerator<T, void, undefined>; cold/hot patterns; multicast broadcaster; back-pressure via for-await pull semantics; 25 fixtures."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 10](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase10Streams`: 25 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gate: tsc strict zero diagnostics; the streams runtime additions (broadcaster, take, drop, buffer, debounce) stay under 4 KB gzipped on top of Phase 9's concurrency budget.

## Goal-alignment audit

Mochi streams are typed asynchronous sequences with cooperative back-pressure. MEP-49 maps them to Swift `AsyncStream`, MEP-50 to Kotlin `Flow` + `Channel`, MEP-51 to Python `AsyncIterator`. The TypeScript surface offers `AsyncIterable<T>` and its async-generator literal (`async function* () { yield ... }`). MEP-52 commits to `AsyncIterable<T>` (or the more specific `AsyncGenerator<T, void, undefined>` when the source is a literal generator). The pull-based `for await` semantics give back-pressure automatically: the producer awaits the consumer at each yield.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | Cold streams: `async function* () { ... }` literal lowering; `for await` consumption | NOT STARTED | n/a |
| 10.1 | Hot streams: `MochiBroadcaster<T>` for multi-subscriber fan-out from a single producer | NOT STARTED | n/a |
| 10.2 | Stream operators: `take(n)`, `drop(n)`, `buffer(n)`, `debounce(ms)`, `throttle(ms)`, `map`, `filter` | NOT STARTED | n/a |
| 10.3 | Stream lifecycle: `AbortSignal` propagation for cancellation; cleanup via `try { ... } finally { ... }` in the generator body | NOT STARTED | n/a |
| 10.4 | Interop: `AsyncIterable<T>` to `ReadableStream<T>` adapter for Web Streams (used by `fetch.body` consumers); reverse adapter | NOT STARTED | n/a |

## Sub-phase 10.0, Cold streams

### Decisions made (10.0)

**Mochi**: `stream fun ticks() -> stream<int> { for i in 0.. { yield i; sleep(1s) } }`

**TypeScript**:

```typescript
export async function* ticks(): AsyncGenerator<bigint, void, undefined> {
  for (let i = 0n; ; i++) {
    yield i;
    await sleep(1000n);
  }
}
```

**Why `AsyncGenerator<T, void, undefined>` rather than `AsyncIterable<T>`**: the generator literal form has the more specific type. The emitter uses the specific type at declaration sites; consumers that don't care can accept `AsyncIterable<T>` (the wider type) via TypeScript's structural subtyping.

**Cold means**: each `for await (const t of ticks())` invocation calls `ticks()` afresh, creating a new generator. The generator's state is local; multiple consumers get independent sequences (each starts at 0n).

**`sleep(1s)`**: lowers to `await new Promise((r) => setTimeout(r, 1000n))`. The Mochi `1s` literal is a `duration` (Phase 14 binds it to Temporal); Phase 10 ships a bare-millisecond fallback (`sleep(1000n)`) and Phase 14 upgrades to `sleep(Temporal.Duration.from({seconds: 1}))`.

## Sub-phase 10.1, Hot streams (broadcaster)

### Decisions made (10.1)

**`MochiBroadcaster<T>`**: multi-subscriber fan-out from a single producer. Each subscriber gets its own `AsyncIterableQueue<T>` that the broadcaster pushes into.

```typescript
// @mochi/runtime/concurrency/broadcaster
export class MochiBroadcaster<T> {
  private readonly subscribers: Set<AsyncIterableQueue<T>> = new Set();

  subscribe(): AsyncIterable<T> {
    const q = new AsyncIterableQueue<T>();
    this.subscribers.add(q);
    return {
      [Symbol.asyncIterator]: () => {
        const iter = q[Symbol.asyncIterator]();
        return {
          next: () => iter.next(),
          return: async () => {
            this.subscribers.delete(q);
            q.close();
            return { value: undefined, done: true };
          },
        };
      },
    };
  }

  publish(value: T): void {
    for (const s of this.subscribers) s.push(value);
  }

  close(): void {
    for (const s of this.subscribers) s.close();
    this.subscribers.clear();
  }
}
```

**Why a `Set` of queues**: each subscriber has independent back-pressure. A slow subscriber buffers in its own queue without slowing the producer or the other subscribers. Bounded queues per subscriber are an opt-in.

**Replay semantics**: not provided in Phase 10 (would need to keep history; bounded buffer would be ambiguous). Replay broadcaster is a v1.5 add.

## Sub-phase 10.2, Stream operators

### Decisions made (10.2)

Stream operators are pure async generators that wrap a source:

```typescript
// @mochi/runtime/concurrency/operators
export async function* take<T>(source: AsyncIterable<T>, n: bigint): AsyncGenerator<T, void, undefined> {
  let i = 0n;
  for await (const v of source) {
    if (i >= n) return;
    yield v;
    i++;
  }
}

export async function* drop<T>(source: AsyncIterable<T>, n: bigint): AsyncGenerator<T, void, undefined> {
  let i = 0n;
  for await (const v of source) {
    if (i++ < n) continue;
    yield v;
  }
}

export async function* mapStream<T, U>(
  source: AsyncIterable<T>, f: (v: T) => U | Promise<U>,
): AsyncGenerator<U, void, undefined> {
  for await (const v of source) yield await f(v);
}

export async function* filterStream<T>(
  source: AsyncIterable<T>, p: (v: T) => boolean | Promise<boolean>,
): AsyncGenerator<T, void, undefined> {
  for await (const v of source) if (await p(v)) yield v;
}
```

`buffer(n)` and `debounce(ms)` are timing-sensitive; implementations use `AsyncIterableQueue` plus `setTimeout`. The full set lives in `@mochi/runtime/concurrency/operators/`.

## Sub-phase 10.3, Lifecycle and cancellation

### Decisions made (10.3)

**`AbortSignal` propagation**: a stream operator that takes a `signal: AbortSignal` exits early when `signal.aborted` becomes true. The generator's `try/finally` runs cleanup (closing inner queues, releasing handles).

**Generator `return`**: when the consumer's `for await` exits early (via `break`, `return`, or an exception), the generator's `return()` is called. The async-generator body's `try/finally` runs at that point. This is the canonical cleanup hook.

**Mochi `defer` in a stream**: lowers to a `try/finally` wrapping the generator body.

## Sub-phase 10.4, Web Streams interop

### Decisions made (10.4)

**`asyncIterableToReadableStream(source: AsyncIterable<T>): ReadableStream<T>`**:

```typescript
export function asyncIterableToReadableStream<T>(source: AsyncIterable<T>): ReadableStream<T> {
  const iter = source[Symbol.asyncIterator]();
  return new ReadableStream<T>({
    async pull(controller) {
      const { value, done } = await iter.next();
      if (done) { controller.close(); return; }
      controller.enqueue(value);
    },
    async cancel(reason) {
      if (typeof iter.return === "function") await iter.return(reason);
    },
  });
}
```

**Reverse adapter `readableStreamToAsyncIterable`**: `ReadableStream<T>` is already async-iterable in Node 22, Deno 2, Bun 1.1, and Chromium 124+ (the `Symbol.asyncIterator` was added to the spec in 2024). The reverse adapter is a no-op except on older Chromium where the emitter falls back to a manual `reader.read()` loop. Phase 10's runtime floor (Chromium 130) does not need the fallback.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/streams.go` | Stream decl to `async function*` generator; `yield` lowering |
| `transpiler3/typescript/lower/forAwait.go` | `for await` consumption form |
| `runtime3/typescript/src/concurrency/broadcaster.ts` | `MochiBroadcaster<T>` |
| `runtime3/typescript/src/concurrency/operators/` | take, drop, buffer, debounce, throttle, map, filter |
| `runtime3/typescript/src/concurrency/interop.ts` | AsyncIterable to/from ReadableStream adapters |
| `transpiler3/typescript/build/phase10_test.go` | `TestPhase10Streams` |
| `tests/transpiler3/typescript/fixtures/phase10-streams/` | 25 fixtures |

## Test set

- `TestPhase10Streams`, 25 fixtures four-runtime.
- `TestPhase10Cancellation`, fixtures that abort mid-stream confirm cleanup runs.
- `TestPhase10WebStreamsInterop`, an `AsyncIterable` round-trips through `ReadableStream` byte-equal.

## Deferred work

- Replay broadcaster (subscribers receive history on subscribe). v1.5.
- Hot/cold operator catalogue expansion (`scan`, `share`, `switchMap`). Phase 10 ships the core 8 operators; expansion is on-demand.
- Backpressure-aware sinks (`pipeTo` with explicit credit). The default for-await pull is sufficient for the v1 corpus.
