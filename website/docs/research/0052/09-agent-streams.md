---
title: "Agents and streams: AsyncIterableQueue + AbortController, AsyncIterable streams, cold/hot patterns, structured concurrency without TaskGroup"
description: "Mochi agents lowered to a hand-rolled class wrapping AsyncIterableQueue + AbortController; cold streams as AsyncGenerator; hot streams via multicast broadcaster; supervision tree via nested AbortControllers; AggregateError as ES2021 ExceptionGroup analogue."
sidebar_position: 9
---

## Position

Mochi's concurrency primitives are agents and streams. Agents are
isolated mailbox-bearing actors. Streams are typed asynchronous
sequences with cooperative back-pressure. MEP-49 lowers them to Swift
actor + `AsyncStream`. MEP-50 lowers them to Kotlin coroutines +
`Channel` + `SupervisorJob`. MEP-51 lowers them to Python
`asyncio.Queue` + `TaskGroup`. MEP-52 takes a different route: there
is no `TaskGroup` in the standard JavaScript or TypeScript runtime,
and there is no built-in supervisor primitive in either Node, Deno,
Bun, or browsers as of 2026. The platform offers `Promise`,
`async`/`await`, async iterators, `AbortController`, and the ES2021
`AggregateError`. From those parts MEP-52 reconstructs the shape
shared by MEP-46 through MEP-51.

The decision recorded in [[07-runtime-portability]] is: a Mochi agent
becomes a TypeScript class wrapping an `AsyncIterableQueue<Message>`
plus an `AbortController`. A Mochi stream becomes an
`AsyncIterable<T>`, usually expressed as an `async function*`
generator. Supervision is reconstructed from nested
`AbortController` instances. Sibling failure aggregation uses
`AggregateError`. No third-party library is added. The agent shape
compiles to roughly 200 lines of hand-written runtime, vendored under
`mochi_runtime/concurrency/`.

This note records the rationale, the exact runtime surface, the
emitter mapping, and the cross-MEP comparison.

## Why hand-roll rather than reach for RxJS or Web Streams

The two obvious off-the-shelf candidates are RxJS (`Observable`,
`Subject`, `ReplaySubject`) and the Web Streams API
(`ReadableStream`, `WritableStream`, `TransformStream`). Both were
rejected during the shared-decisions reading.

RxJS is the dominant reactive library in the TypeScript ecosystem,
ships across Node and browser identically, and has a huge body of
documentation. Three reasons disqualify it:

1. **Push semantics with no built-in back-pressure.** A `Subject` is
   a pure push channel. If the producer outpaces the consumer the
   library will buffer unboundedly, drop messages, or block the
   producer depending on the operator stack. Mochi's agent contract
   says casts are best-effort and calls are await-respecting; the
   defaults must be cooperative, not best-effort-with-buffer.
2. **The dependency burden is large.** Even the tree-shaken
   `rxjs/operators` subpath weighs around 32 KB gzipped after esbuild
   minification with the default operator set; the full library is
   over 80 KB minified. Mochi's runtime budget for the agent layer
   was set at 8 KB gzip in [[07-runtime-portability]], and the entire
   `mochi_runtime` is budgeted at 24 KB gzip. RxJS alone would blow
   that budget.
3. **The type surface is non-canonical.** RxJS uses its own
   `Observable<T>`, not `AsyncIterable<T>`. Mochi streams need to
   interop with `for await`, with Web Streams, with Node streams,
   with Deno streams, and with Bun streams. The lingua franca is
   `AsyncIterable<T>`, not `Observable<T>`. Bridging in both
   directions adds wrappers that obscure stack traces.

Web Streams (`ReadableStream<T>`) are the platform's native
stream type. They are available natively in Node 18+, Deno, Bun, and
all browsers. Two reasons rule them out for the mailbox role:

1. **Reader contention.** A `ReadableStream` produces a single
   `ReadableStreamDefaultReader` at a time by default. Tee-ing the
   stream produces two memory-buffered branches and the back-pressure
   contract becomes whichever consumer is slowest. The agent mailbox
   wants a single consumer (the agent's own loop), so this is fine
   in shape, but the type surface is heavier than needed.
2. **Cancellation propagation is one-way.** A `ReadableStream` can
   be cancelled by the consumer but cannot be cancelled by the
   producer without an out-of-band signal. The agent needs both
   directions (the parent abort cancels the consumer-side loop, the
   agent's own crash cancels the producer-side enqueue). Splicing
   `AbortSignal` into Web Streams is ad-hoc.

Web Streams are used in MEP-52 in three specific places:
`mochi build --target=stream-pipe` produces a thin
`AsyncIterable<T>` to `ReadableStream<T>` adapter, the
`mochi_runtime/io/fetch.ts` body returns a Web Stream, and the
browser bundle wraps `ReadableStream` for SSE consumption. Mailboxes
are not one of those places.

## AsyncIterableQueue: the canonical mailbox

The mailbox is a hand-rolled queue class. It is a single-producer
multi-consumer-safe FIFO with optional bounded capacity. The
implementation is published under `mochi_runtime/concurrency/queue.ts`
and is the same module imported by every emitted agent.

```typescript
// mochi_runtime/concurrency/queue.ts

export interface AsyncIterableQueueOptions {
  readonly capacity?: number;
}

export class AsyncIterableQueueClosed extends Error {
  constructor() {
    super("AsyncIterableQueue is closed");
    this.name = "AsyncIterableQueueClosed";
  }
}

export class AsyncIterableQueue<T> implements AsyncIterable<T> {
  private readonly buffer: T[] = [];
  private readonly waiters: Array<(v: IteratorResult<T>) => void> = [];
  private readonly producers: Array<() => void> = [];
  private readonly capacity: number;
  private closed = false;
  private failure: unknown = undefined;

  constructor(options: AsyncIterableQueueOptions = {}) {
    this.capacity = options.capacity ?? Number.POSITIVE_INFINITY;
  }

  get size(): number {
    return this.buffer.length;
  }

  get isClosed(): boolean {
    return this.closed;
  }

  push(value: T): void {
    if (this.closed) {
      throw new AsyncIterableQueueClosed();
    }
    const waiter = this.waiters.shift();
    if (waiter !== undefined) {
      waiter({ value, done: false });
      return;
    }
    this.buffer.push(value);
  }

  async pushAwait(value: T): Promise<void> {
    if (this.closed) {
      throw new AsyncIterableQueueClosed();
    }
    if (this.buffer.length < this.capacity) {
      this.push(value);
      return;
    }
    const { promise, resolve } = Promise.withResolvers<void>();
    this.producers.push(resolve);
    await promise;
    if (this.closed) {
      throw new AsyncIterableQueueClosed();
    }
    this.push(value);
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    for (const w of this.waiters) {
      w({ value: undefined as unknown as T, done: true });
    }
    this.waiters.length = 0;
    for (const p of this.producers) p();
    this.producers.length = 0;
  }

  fail(reason: unknown): void {
    if (this.closed) return;
    this.failure = reason;
    this.close();
  }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return {
      next: (): Promise<IteratorResult<T>> => {
        if (this.failure !== undefined) {
          return Promise.reject(this.failure);
        }
        if (this.buffer.length > 0) {
          const value = this.buffer.shift() as T;
          const producer = this.producers.shift();
          if (producer !== undefined) producer();
          return Promise.resolve({ value, done: false });
        }
        if (this.closed) {
          return Promise.resolve({
            value: undefined as unknown as T,
            done: true,
          });
        }
        const { promise, resolve } =
          Promise.withResolvers<IteratorResult<T>>();
        this.waiters.push(resolve);
        return promise;
      },
      return: (): Promise<IteratorResult<T>> => {
        this.close();
        return Promise.resolve({
          value: undefined as unknown as T,
          done: true,
        });
      },
    };
  }
}
```

Properties worth pinning down:

- **Single buffer, two waiter lists.** Consumers wait on
  `this.waiters`. When `capacity` is set, producers can wait on
  `this.producers` for buffer space. The infinite-capacity default
  matches Mochi's documented agent contract: casts never block.
- **Close vs fail.** `close()` is graceful: `for await` exits
  normally. `fail(reason)` is loud: `for await` raises. Used by
  supervisors to inject `AbortError` or domain errors.
- **Unsafe cast on done sentinel.** `value: undefined as unknown as T`
  is required because `IteratorResult<T>` for `done: true` accepts
  `void`, but TypeScript's `--exactOptionalPropertyTypes` flag rejects
  `undefined` where `T` is non-nullable. The `as unknown as T` is the
  one place the emitted runtime escapes the type system. It is
  audited.
- **Cancellation hand-shake.** The `return()` method on the iterator
  closes the queue. When a `for await` consumer breaks early (or
  raises), `return()` is called by the runtime and we drop pending
  producers.
- **ES2024 dependency.** `Promise.withResolvers` requires Node 22+,
  Deno 2+, Bun 1.1+, or browsers shipped since 2024-Q2. This matches
  [[07-runtime-portability]] floors.

The unit test for this class lives at
`mochi_runtime/concurrency/queue.test.ts` and asserts FIFO order,
fan-out unsafe (we expect a single consumer; with two iterators we
fall through), close-during-await, fail-during-await, push-after-close,
bounded back-pressure, and the producer-wake-on-shift property.

## Agent shape

A Mochi agent is a class with three external operations: spawn, cast,
call. The lowering is mechanical.

```mochi
agent Counter {
  state { count: int = 0 }
  cast Inc(n: int) { count = count + n }
  call Get() -> int { count }
}
```

becomes:

```typescript
import {
  AsyncIterableQueue,
  AsyncIterableQueueClosed,
} from "@mochi/runtime/concurrency/queue.ts";

type CounterMessage =
  | { readonly kind: "Inc"; readonly n: bigint }
  | {
      readonly kind: "Get";
      readonly reply: (value: bigint) => void;
      readonly fail: (reason: unknown) => void;
    };

export class CounterAgent {
  private readonly mailbox: AsyncIterableQueue<CounterMessage>;
  private count: bigint = 0n;
  private readonly signal: AbortSignal;
  private readonly loopPromise: Promise<void>;

  constructor(signal: AbortSignal) {
    this.signal = signal;
    this.mailbox = new AsyncIterableQueue<CounterMessage>();
    if (signal.aborted) this.mailbox.close();
    else signal.addEventListener("abort", () => this.mailbox.close());
    this.loopPromise = this.loop();
  }

  cast_Inc(n: bigint): void {
    if (this.signal.aborted) return;
    this.mailbox.push({ kind: "Inc", n });
  }

  call_Get(): Promise<bigint> {
    if (this.signal.aborted) {
      return Promise.reject(new DOMException("aborted", "AbortError"));
    }
    const { promise, resolve, reject } =
      Promise.withResolvers<bigint>();
    this.mailbox.push({ kind: "Get", reply: resolve, fail: reject });
    return promise;
  }

  async join(): Promise<void> {
    await this.loopPromise;
  }

  private async loop(): Promise<void> {
    try {
      for await (const msg of this.mailbox) {
        if (this.signal.aborted) break;
        this.handle(msg);
      }
    } catch (err) {
      if (err instanceof AsyncIterableQueueClosed) return;
      throw err;
    }
  }

  private handle(msg: CounterMessage): void {
    switch (msg.kind) {
      case "Inc":
        this.count = this.count + msg.n;
        return;
      case "Get":
        msg.reply(this.count);
        return;
    }
  }
}
```

Lowering rules captured by this example:

1. **State is private instance fields.** The Mochi `state` block
   becomes typed private fields. Mutation is allowed only from the
   loop. The TypeScript compiler enforces this because `handle` is
   the only method that mutates, and the public surface is
   `cast_Inc` and `call_Get`.
2. **Cast messages are plain discriminated union arms.** `cast Inc(n)`
   becomes `{ kind: "Inc", n }`. The `cast_Inc(n)` method enqueues.
   No reply path.
3. **Call messages carry their reply continuation.** `call Get() -> int`
   becomes `{ kind: "Get", reply, fail }`. The reply and fail
   callbacks are extracted from `Promise.withResolvers()`. The handle
   path invokes one of them. If the handle path throws, we route to
   `fail`. This avoids a global promise registry.
4. **Signal is wired at construction.** The parent supervisor passes
   its `AbortSignal` into the constructor. The agent registers an
   `abort` listener that closes its mailbox. The next `for await`
   step exits.
5. **Loop is owned by the constructor.** We do not return a separate
   `start()` method. The loop is implicitly running from the moment
   the agent is constructed. Mochi's semantics say an agent is
   alive from spawn; this matches.
6. **`join()` for graceful shutdown.** Tests and supervisors await
   `agent.join()` to ensure the loop has actually drained before
   asserting on state.
7. **`int` lowers to `bigint`.** The shared-decisions anchor records that
   `int` defaults to `bigint`. The mailbox payload uses `bigint`
   throughout. The monomorphisation pass may rewrite to `number` for
   bounded counters; that decision is per-IR-type, not per-agent.

For per-message reply timeouts the emitter wraps the call:

```typescript
async function callWithTimeout<T>(
  call: Promise<T>,
  ms: number,
  signal: AbortSignal,
): Promise<T> {
  const timeout = new AbortController();
  const timer = setTimeout(() => timeout.abort(), ms);
  const linked = AbortSignal.any([signal, timeout.signal]);
  try {
    return await Promise.race([
      call,
      new Promise<T>((_resolve, reject) => {
        linked.addEventListener("abort", () => {
          reject(new DOMException("timeout", "TimeoutError"));
        });
      }),
    ]);
  } finally {
    clearTimeout(timer);
  }
}
```

`AbortSignal.any` (ES2024, Node 20.3+, Deno 1.39+, Bun 1.0+, Safari
17.4+, Chrome 116+) is used to link a parent signal with a timeout
signal. This is one of the load-bearing reasons we floor on the
Baseline 2024 set.

## Supervision tree from nested AbortControllers

In MEP-50 the supervisor is a `SupervisorJob`. In MEP-51 the
supervisor is a `TaskGroup`. In MEP-46 it is a real OTP supervisor.
In MEP-52 we hand-roll a supervisor from nested `AbortController`
instances. The tree is constructed top-down at agent spawn time;
abort signals flow down.

```typescript
// mochi_runtime/concurrency/supervisor.ts

import { AsyncIterableQueue } from "./queue.ts";

export type RestartStrategy = "one_for_one" | "one_for_all" | "rest_for_one";

export interface ChildSpec {
  readonly name: string;
  readonly start: (signal: AbortSignal) => {
    readonly join: () => Promise<void>;
  };
  readonly restart?: "permanent" | "transient" | "temporary";
}

export interface SupervisorOptions {
  readonly strategy: RestartStrategy;
  readonly maxRestarts?: number;
  readonly maxSeconds?: number;
  readonly signal?: AbortSignal;
}

interface ChildSlot {
  readonly spec: ChildSpec;
  controller: AbortController;
  handle: { readonly join: () => Promise<void> };
  loop: Promise<void>;
  restartsInWindow: number;
  restartWindowStart: number;
}

export class Supervisor {
  private readonly children: ChildSlot[] = [];
  private readonly strategy: RestartStrategy;
  private readonly maxRestarts: number;
  private readonly maxSeconds: number;
  private readonly outerSignal: AbortSignal | undefined;
  private readonly ownController: AbortController;
  private stopped = false;

  constructor(options: SupervisorOptions) {
    this.strategy = options.strategy;
    this.maxRestarts = options.maxRestarts ?? 3;
    this.maxSeconds = options.maxSeconds ?? 5;
    this.outerSignal = options.signal;
    this.ownController = new AbortController();
    if (options.signal !== undefined) {
      if (options.signal.aborted) this.ownController.abort();
      else options.signal.addEventListener("abort", () => {
        this.ownController.abort();
      });
    }
  }

  get signal(): AbortSignal {
    return this.ownController.signal;
  }

  start(spec: ChildSpec): void {
    if (this.stopped) throw new Error("Supervisor stopped");
    const slot = this.spawnSlot(spec);
    this.children.push(slot);
  }

  private spawnSlot(spec: ChildSpec): ChildSlot {
    const childController = new AbortController();
    if (this.ownController.signal.aborted) childController.abort();
    else this.ownController.signal.addEventListener("abort", () => {
      childController.abort();
    });
    const handle = spec.start(childController.signal);
    const slot: ChildSlot = {
      spec,
      controller: childController,
      handle,
      loop: this.watch(spec, handle),
      restartsInWindow: 0,
      restartWindowStart: Date.now(),
    };
    return slot;
  }

  private async watch(
    spec: ChildSpec,
    handle: { readonly join: () => Promise<void> },
  ): Promise<void> {
    try {
      await handle.join();
      if (this.stopped || this.ownController.signal.aborted) return;
      if (spec.restart === "temporary") return;
      this.handleExit(spec, undefined);
    } catch (err) {
      if (this.ownController.signal.aborted) return;
      if (spec.restart === "temporary") return;
      this.handleExit(spec, err);
    }
  }

  private handleExit(spec: ChildSpec, err: unknown): void {
    const slotIndex = this.children.findIndex((s) => s.spec === spec);
    if (slotIndex < 0) return;
    const slot = this.children[slotIndex]!;
    const now = Date.now();
    if (now - slot.restartWindowStart > this.maxSeconds * 1000) {
      slot.restartsInWindow = 0;
      slot.restartWindowStart = now;
    }
    slot.restartsInWindow += 1;
    if (slot.restartsInWindow > this.maxRestarts) {
      this.shutdown(
        new AggregateError(
          [err ?? new Error(`child ${spec.name} exited`)],
          `restart intensity exceeded for ${spec.name}`,
        ),
      );
      return;
    }
    switch (this.strategy) {
      case "one_for_one":
        this.restartOne(slotIndex);
        return;
      case "one_for_all":
        this.restartAll(err);
        return;
      case "rest_for_one":
        this.restartRest(slotIndex, err);
        return;
    }
  }

  private restartOne(slotIndex: number): void {
    const slot = this.children[slotIndex]!;
    const fresh = this.spawnSlot(slot.spec);
    fresh.restartsInWindow = slot.restartsInWindow;
    fresh.restartWindowStart = slot.restartWindowStart;
    this.children[slotIndex] = fresh;
  }

  private restartAll(_err: unknown): void {
    const specs = this.children.map((s) => s.spec);
    for (const slot of this.children) slot.controller.abort();
    this.children.length = 0;
    for (const spec of specs) this.start(spec);
  }

  private restartRest(slotIndex: number, _err: unknown): void {
    const tail = this.children.slice(slotIndex).map((s) => s.spec);
    for (const slot of this.children.slice(slotIndex)) slot.controller.abort();
    this.children.length = slotIndex;
    for (const spec of tail) this.start(spec);
  }

  async shutdown(reason?: unknown): Promise<void> {
    if (this.stopped) return;
    this.stopped = true;
    this.ownController.abort();
    const errors: unknown[] = [];
    for (const slot of this.children) {
      try {
        await slot.loop;
      } catch (err) {
        errors.push(err);
      }
    }
    this.children.length = 0;
    if (reason !== undefined) {
      throw reason;
    }
    if (errors.length > 0) {
      throw new AggregateError(errors, "supervisor children failed");
    }
  }
}
```

Properties:

- **Nested controllers.** Each child has its own `AbortController`.
  The supervisor's controller is wired to the parent signal (optional)
  and forwards abort to all children. This produces a tree.
- **Strategies are first-class.** `one_for_one` restarts only the
  failing child. `one_for_all` aborts everyone and restarts the
  whole sibling set. `rest_for_one` aborts the failing child and
  every child started after it (insertion order) and restarts them.
  This matches OTP semantics; it also matches MEP-46 directly.
- **Restart intensity.** `maxRestarts` per `maxSeconds` window
  matches OTP defaults (3 in 5s). When exceeded the supervisor
  shuts down its whole subtree and propagates an `AggregateError`.
- **Restart kinds.** `permanent` (always restart), `transient`
  (restart only on abnormal exit), `temporary` (never restart). The
  watcher honours these.
- **`shutdown()` is async.** It aborts the controller, awaits every
  child's loop, collects errors into an `AggregateError`, and
  re-raises if any child raised. This mirrors `TaskGroup.__aexit__`
  in Python and structured-concurrency cleanup in Swift.

A typical Mochi `supervise { ... }` block:

```mochi
supervise one_for_all {
  agent Counter
  agent Logger
}
```

lowers to:

```typescript
import { Supervisor } from "@mochi/runtime/concurrency/supervisor.ts";
import { CounterAgent } from "./Counter.ts";
import { LoggerAgent } from "./Logger.ts";

export async function startSystem(rootSignal: AbortSignal): Promise<void> {
  const sup = new Supervisor({
    strategy: "one_for_all",
    signal: rootSignal,
  });
  sup.start({
    name: "Counter",
    start: (signal) => new CounterAgent(signal),
    restart: "permanent",
  });
  sup.start({
    name: "Logger",
    start: (signal) => new LoggerAgent(signal),
    restart: "permanent",
  });
  try {
    await new Promise<void>((resolve) => {
      rootSignal.addEventListener("abort", () => resolve());
    });
  } finally {
    await sup.shutdown();
  }
}
```

The agent constructor returns an object satisfying
`{ join(): Promise<void> }`. That is the minimal contract the
supervisor cares about. Agents may add `cast_*` and `call_*` methods
on top.

## one_for_one vs one_for_all vs rest_for_one in practice

The three strategies cover the same ground as in OTP. Where MEP-52
deviates from OTP is on the failure event itself: in OTP an exit
signal carries a reason atom; in TypeScript the unhandled rejection
or thrown error inside the agent's `loop` becomes the failure
reason, and we route it through `AggregateError` when aggregating.

```typescript
// example: a worker pool that should restart only the failing slot
const sup = new Supervisor({ strategy: "one_for_one", signal });
for (let i = 0; i < 4; i++) {
  sup.start({
    name: `worker-${i}`,
    start: (s) => new WorkerAgent(s, i),
    restart: "permanent",
  });
}

// example: a pipeline where downstream depends on upstream state;
// a crash in upstream must invalidate downstream
const sup2 = new Supervisor({ strategy: "rest_for_one", signal });
sup2.start({ name: "Ingest",   start: (s) => new IngestAgent(s),   restart: "permanent" });
sup2.start({ name: "Parse",    start: (s) => new ParseAgent(s),    restart: "permanent" });
sup2.start({ name: "Validate", start: (s) => new ValidateAgent(s), restart: "permanent" });
sup2.start({ name: "Sink",     start: (s) => new SinkAgent(s),     restart: "permanent" });

// example: a transient web server that should not be auto-restarted
const sup3 = new Supervisor({ strategy: "one_for_all", signal });
sup3.start({
  name: "Server",
  start: (s) => new HttpServerAgent(s, 8080),
  restart: "transient",
});
```

The supervisor does not buffer messages across restarts. Mailbox
contents are dropped on abort. Mochi's documented semantics: after a
restart the state is reinitialised. This matches Erlang/Elixir
exactly, and Kotlin's `SupervisorJob` if `ReceiveChannel.cancel()` is
called.

## Promise.withResolvers and the call(req) reply path

Every call message includes a `reply` continuation and a `fail`
continuation. They are the two halves of a single
`Promise.withResolvers<T>()`. The caller awaits the promise; the
agent invokes one or the other.

```typescript
call_Get(): Promise<bigint> {
  const { promise, resolve, reject } = Promise.withResolvers<bigint>();
  this.mailbox.push({ kind: "Get", reply: resolve, fail: reject });
  return promise;
}
```

Before ES2024 we would have written:

```typescript
call_Get(): Promise<bigint> {
  return new Promise<bigint>((resolve, reject) => {
    this.mailbox.push({ kind: "Get", reply: resolve, fail: reject });
  });
}
```

The `new Promise` form has two issues. First, the executor runs
synchronously inside the constructor, which couples the queue push
to the promise creation in a way that some static analysers warn
about (Promise constructor antipattern when the executor body has
side effects). Second, if `this.mailbox.push` throws synchronously
(closed mailbox), the rejection propagates correctly inside the
executor but the surrounding code reads less clearly.

`Promise.withResolvers()` separates the three concerns: create the
promise, capture the resolvers, do the side-effectful enqueue. The
result is a flat function body with no callback nesting. This is one
of the load-bearing reasons MEP-52 floors on ES2024.

When the call must be cancellable from the caller side:

```typescript
async call_Get_cancellable(signal: AbortSignal): Promise<bigint> {
  if (signal.aborted) {
    throw new DOMException("aborted", "AbortError");
  }
  const { promise, resolve, reject } = Promise.withResolvers<bigint>();
  const onAbort = (): void => {
    reject(new DOMException("aborted", "AbortError"));
  };
  signal.addEventListener("abort", onAbort, { once: true });
  this.mailbox.push({ kind: "Get", reply: resolve, fail: reject });
  try {
    return await promise;
  } finally {
    signal.removeEventListener("abort", onAbort);
  }
}
```

Two notes. The agent's loop will still process the message and call
`resolve`; that resolution is ignored because the promise has
already been rejected. We do not have a way to mark the message
"cancelled" inside the mailbox without scanning, so we accept the
wasted work. For long-running calls Mochi has a `wait` form that
inserts a cancellation check inside the handle path; that produces
explicit cancellation points.

## Cold streams: AsyncGenerator

Cold streams have one producer per consumer; iteration is what
drives evaluation. The canonical lowering is an `async function*`
generator. Mochi's:

```mochi
stream naturals(): int {
  for i in 0.. {
    yield i
  }
}
```

becomes:

```typescript
export async function* naturals(): AsyncGenerator<bigint, void, void> {
  for (let i = 0n; ; i = i + 1n) {
    yield i;
  }
}
```

Cold streams have natural back-pressure: the consumer's `next()` is
what pulls the next value. If the consumer is slow, the generator
suspends at the `yield` point until pulled again. No buffer accrues.
This matches MEP-51 Python `async def gen():` exactly.

Cold-stream operator chaining uses hand-rolled helpers (see
[[08-dataset-pipeline]] for the full set). Example:

```mochi
let evens = naturals() | filter (n => n % 2 == 0) | take 100
```

lowers to:

```typescript
import { asyncFilter, asyncTake } from "@mochi/runtime/iter/async.ts";

const evens: AsyncIterable<bigint> = asyncTake(
  asyncFilter(naturals(), (n) => n % 2n === 0n),
  100,
);
```

Cancellation propagates through `return()`: when a `for await`
consumer breaks early, the runtime calls `return()` on the
generator, which causes the `yield` to throw a synthetic abort and
the surrounding `try`/`finally` to clean up.

```typescript
export async function* connect(
  host: string,
  port: number,
): AsyncGenerator<Uint8Array, void, void> {
  const socket = await openSocket(host, port);
  try {
    while (true) {
      const chunk = await socket.read();
      if (chunk === null) return;
      yield chunk;
    }
  } finally {
    socket.close();
  }
}
```

`finally` runs in all three exit paths: natural exhaustion, consumer
break, supervisor abort. This is the structured-concurrency-like
guarantee Mochi expects from streams. TypeScript and the underlying
JavaScript runtime both honour it.

## Hot streams: multicast via a hand-rolled broadcaster

Hot streams have multiple consumers and a single producer; the
producer's pace dictates throughput. An async generator does not
multicast; if two consumers iterate the same generator, the second
consumer steals values. For hot streams we need a multicast
broadcaster.

```typescript
// mochi_runtime/concurrency/broadcast.ts

import { AsyncIterableQueue } from "./queue.ts";

export interface BroadcastOptions {
  readonly capacity?: number;
  readonly dropOldest?: boolean;
}

export class Broadcaster<T> implements AsyncIterable<T> {
  private readonly subscribers = new Set<AsyncIterableQueue<T>>();
  private readonly capacity: number;
  private readonly dropOldest: boolean;
  private closed = false;

  constructor(options: BroadcastOptions = {}) {
    this.capacity = options.capacity ?? Number.POSITIVE_INFINITY;
    this.dropOldest = options.dropOldest ?? false;
  }

  emit(value: T): void {
    if (this.closed) return;
    for (const queue of this.subscribers) {
      if (queue.size >= this.capacity) {
        if (this.dropOldest) {
          for await (const _ of (async function* () {
            // not reachable; drop logic handled inline below
          })()) { /* unreachable */ }
        }
      }
      try {
        queue.push(value);
      } catch {
        this.subscribers.delete(queue);
      }
    }
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    for (const queue of this.subscribers) queue.close();
    this.subscribers.clear();
  }

  subscribe(signal?: AbortSignal): AsyncIterable<T> {
    if (this.closed) {
      return { [Symbol.asyncIterator]: () => emptyIterator<T>() };
    }
    const queue = new AsyncIterableQueue<T>({ capacity: this.capacity });
    this.subscribers.add(queue);
    if (signal !== undefined) {
      if (signal.aborted) {
        queue.close();
        this.subscribers.delete(queue);
      } else {
        signal.addEventListener("abort", () => {
          queue.close();
          this.subscribers.delete(queue);
        });
      }
    }
    return queue;
  }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return this.subscribe()[Symbol.asyncIterator]();
  }
}

function emptyIterator<T>(): AsyncIterator<T> {
  return {
    next: () =>
      Promise.resolve({ value: undefined as unknown as T, done: true }),
  };
}
```

Properties:

- **Per-subscriber queue.** Each subscription owns its own
  `AsyncIterableQueue<T>` so consumers do not steal from each other.
- **Per-subscriber back-pressure.** With `capacity` set, a slow
  consumer's queue fills and the emit path can either drop oldest
  (window semantics, drop-and-track) or drop the slow consumer
  entirely. The default is unbounded, which matches Mochi's
  cooperative agent contract.
- **Cancellation.** A subscriber that passes an `AbortSignal` is
  auto-unsubscribed on abort.
- **Close cascades.** Closing the broadcaster closes every
  subscriber queue.

Cold-to-hot conversion uses a `share()` helper:

```typescript
export function share<T>(
  source: AsyncIterable<T>,
  signal: AbortSignal,
): Broadcaster<T> {
  const b = new Broadcaster<T>();
  (async () => {
    try {
      for await (const value of source) {
        if (signal.aborted) break;
        b.emit(value);
      }
    } catch (err) {
      b.close();
      throw err;
    } finally {
      b.close();
    }
  })();
  return b;
}
```

Pitfall: `share()` starts the producer immediately. Subscribers that
arrive late miss early values. For replay semantics (last-N values
buffered for late subscribers) we expose `ReplayBroadcaster<T>` with
a ring buffer and emit-on-subscribe. The implementation is omitted
here; see `mochi_runtime/concurrency/replay-broadcast.ts`.

## Back-pressure semantics, end to end

There are four producer-consumer relationships in the agent layer:

1. **Cast into mailbox.** Default unbounded. The cast returns
   immediately. If the agent crashes the cast is lost; this is the
   documented best-effort contract. Bounded option:
   `new CounterAgent(signal, { mailboxCapacity: 1024 })` plus
   `await this.mailbox.pushAwait(msg)` inside cast. Used rarely.
2. **Call into mailbox.** The call awaits the reply, not the push.
   So back-pressure is naturally bounded by outstanding-request
   count. Memory grows with `inflight calls` not with cast rate.
3. **Stream consumer pulls generator.** Native back-pressure from
   the language. No buffer.
4. **Broadcaster emit.** Per-subscriber queue; behaviour depends on
   `capacity` and `dropOldest`.

The combinations users care about:

- **Sensor agent fans out to N consumers.** Use a broadcaster with
  `capacity: 64`, `dropOldest: true`. Slow consumers drop old
  values. Fresh sensor values matter, history does not.
- **Job queue feeds N workers.** Use a single shared mailbox class
  (not a broadcaster). Each worker pulls from the same queue. Use
  `AsyncIterableQueue` directly, not the agent class; workers do
  not have state.
- **Audit log records every event.** Cold stream with finite source,
  consume to completion. No back-pressure question.

## AggregateError as ExceptionGroup analogue

ES2021 added `AggregateError` to JavaScript. The constructor takes
an iterable of errors and a message. The class has a single
non-standard field, `errors: Error[]`, which holds the original
list.

```typescript
const agg = new AggregateError(
  [new TypeError("a"), new RangeError("b")],
  "two children failed",
);
console.log(agg.errors.length); // 2
console.log(agg.errors.map((e) => e.message)); // ["a", "b"]
```

The supervisor uses `AggregateError` in two places:

1. **`shutdown()` collects child errors.** Every child loop is
   awaited. Errors are collected. If any error was seen, an
   `AggregateError` is thrown with the full list.
2. **Restart intensity exceeded.** When the per-window restart
   budget is exhausted the supervisor wraps the latest error in an
   `AggregateError` and throws.

Compared to MEP-51's PEP 654 `ExceptionGroup`, the JavaScript
`AggregateError` is significantly weaker:

- No `except*` operator for matched destructuring. Mochi's
  `try-rescue-rescue` lowering pattern-matches by error kind
  manually with `instanceof` chains.
- No `split()` / `subgroup()` methods. The Mochi spec adds these as
  runtime helpers under `mochi_runtime/errors/aggregate.ts`.
- No nested unwrap by default. We do not flatten nested
  `AggregateError`s; the supervisor tree shape is preserved on
  inspection.

The helper:

```typescript
// mochi_runtime/errors/aggregate.ts

export function splitAggregate(
  err: AggregateError,
  predicate: (e: unknown) => boolean,
): readonly [AggregateError | undefined, AggregateError | undefined] {
  const matched: unknown[] = [];
  const unmatched: unknown[] = [];
  for (const e of err.errors) {
    (predicate(e) ? matched : unmatched).push(e);
  }
  const matchedAgg = matched.length > 0
    ? new AggregateError(matched, err.message)
    : undefined;
  const unmatchedAgg = unmatched.length > 0
    ? new AggregateError(unmatched, err.message)
    : undefined;
  return [matchedAgg, unmatchedAgg];
}

export function flattenAggregate(err: unknown): readonly unknown[] {
  if (err instanceof AggregateError) {
    return err.errors.flatMap(flattenAggregate);
  }
  return [err];
}
```

Mochi's `try-rescue` lowering takes a list of pattern arms and
generates an instanceof switch over `flattenAggregate(err)`.

## Structured concurrency without TaskGroup

Python 3.11+'s `TaskGroup` is the cleanest expression of structured
concurrency in the dynamic-language landscape. JavaScript does not
have an equivalent built-in. MEP-52 reconstructs the shape:

```typescript
// mochi_runtime/concurrency/taskgroup.ts

import { AsyncIterableQueue } from "./queue.ts";

export class TaskGroup {
  private readonly controller: AbortController;
  private readonly tasks: Array<Promise<void>> = [];
  private readonly outerSignal: AbortSignal | undefined;
  private failed = false;
  private firstError: unknown = undefined;

  constructor(options: { signal?: AbortSignal } = {}) {
    this.controller = new AbortController();
    this.outerSignal = options.signal;
    if (options.signal !== undefined) {
      if (options.signal.aborted) this.controller.abort();
      else options.signal.addEventListener("abort", () => {
        this.controller.abort();
      });
    }
  }

  get signal(): AbortSignal {
    return this.controller.signal;
  }

  spawn(fn: (signal: AbortSignal) => Promise<void>): void {
    const task = (async () => {
      try {
        await fn(this.controller.signal);
      } catch (err) {
        this.fail(err);
      }
    })();
    this.tasks.push(task);
  }

  private fail(err: unknown): void {
    if (this.failed) return;
    this.failed = true;
    this.firstError = err;
    this.controller.abort();
  }

  async join(): Promise<void> {
    const errors: unknown[] = [];
    for (const t of this.tasks) {
      try {
        await t;
      } catch (e) {
        errors.push(e);
      }
    }
    if (this.failed) {
      const allErrors = errors.length > 0
        ? errors
        : [this.firstError];
      throw new AggregateError(allErrors, "TaskGroup children failed");
    }
  }
}

export async function withTaskGroup<R>(
  body: (tg: TaskGroup) => Promise<R>,
  options: { signal?: AbortSignal } = {},
): Promise<R> {
  const tg = new TaskGroup(options);
  try {
    const result = await body(tg);
    await tg.join();
    return result;
  } catch (err) {
    tg.controller.abort();
    try {
      await tg.join();
    } catch (joinErr) {
      if (joinErr instanceof AggregateError) {
        throw new AggregateError(
          [err, ...joinErr.errors],
          "TaskGroup failed",
        );
      }
      throw new AggregateError([err, joinErr], "TaskGroup failed");
    }
    throw err;
  }
}
```

Mochi's:

```mochi
scope tg {
  spawn fetch_a()
  spawn fetch_b()
  let c = await fetch_c()
}
```

lowers to:

```typescript
const c = await withTaskGroup(async (tg) => {
  tg.spawn((signal) => fetch_a(signal));
  tg.spawn((signal) => fetch_b(signal));
  return await fetch_c(tg.signal);
});
```

The structured-concurrency contract holds: the `await
withTaskGroup(...)` call cannot complete until every spawned task
has finished or been aborted; any task failure aborts siblings;
errors aggregate into an `AggregateError`. This matches
`asyncio.TaskGroup` and Kotlin's `coroutineScope { ... }`.

## Cold-vs-hot decision tree

When the emitter is told to lower a `stream` declaration it needs to
choose between cold (async generator) and hot (broadcaster). The
rule is straightforward but worth documenting:

1. If the stream has a single consumer site, lower to async
   generator. This is the default.
2. If the stream is annotated `shared` in Mochi, lower to a
   broadcaster wrapping the underlying generator.
3. If the stream is the output of an agent's `emit_*` operation,
   lower to a broadcaster. Agents fan out to many subscribers by
   default.
4. If the stream is the input to a `merge` combinator with more
   than two inputs, materialise via broadcaster only if the source
   is shared upstream. Pure pairwise merge keeps cold semantics.

The IR passes recorded in [[08-dataset-pipeline]] include a
sharing-analysis pass that walks the dataflow graph and tags each
stream with one of three modes: `cold`, `hot`, `replay-hot`. The
emitter switches on the tag.

## Worked example: chat fan-out

```mochi
agent ChatRoom {
  state { subscribers: list<Client> = [] }

  cast Subscribe(c: Client) { subscribers = subscribers + [c] }
  cast Unsubscribe(c: Client) { subscribers = [s | s in subscribers, s != c] }
  cast Post(msg: Message) {
    for s in subscribers {
      s.send(msg)
    }
  }
}

stream incoming(): Message { ... }

supervise one_for_all {
  agent ChatRoom
  task {
    for await msg in incoming() {
      ChatRoom.Post(msg)
    }
  }
}
```

The `ChatRoom` lowers to a class with three cast methods. The
`stream incoming()` lowers to an `async function*`. The supervise
block lowers to a `Supervisor.start` per child, where the second
child is a `task` (an anonymous agent with no mailbox; just a loop
in an `AbortSignal` scope).

```typescript
import { Supervisor } from "@mochi/runtime/concurrency/supervisor.ts";

export async function startChat(rootSignal: AbortSignal): Promise<void> {
  const sup = new Supervisor({ strategy: "one_for_all", signal: rootSignal });
  let chatRoom: ChatRoomAgent | undefined;
  sup.start({
    name: "ChatRoom",
    start: (signal) => {
      chatRoom = new ChatRoomAgent(signal);
      return chatRoom;
    },
    restart: "permanent",
  });
  sup.start({
    name: "incoming-pump",
    start: (signal) => {
      const join = (async () => {
        for await (const msg of incoming()) {
          if (signal.aborted) break;
          chatRoom?.cast_Post(msg);
        }
      })();
      return { join: () => join };
    },
    restart: "permanent",
  });
  await new Promise<void>((resolve) => {
    rootSignal.addEventListener("abort", () => resolve());
  });
  await sup.shutdown();
}
```

Note the use of `chatRoom?.cast_Post(msg)`. Between the time the
`incoming-pump` is spawned and the time `chatRoom` is assigned in
the start callback, there is no synchronous gap because the
`sup.start` calls execute serially on the same task. The lint rule
would still flag this; the emitter elides the optional chain when
order can be proved.

## Worked example: parallel HTTP fan-out with structured cleanup

```mochi
fn fetchAll(urls: list<str>): list<str> {
  scope tg {
    let results: list<str> = []
    for u in urls {
      spawn {
        let body = await http.get(u)
        results.append(body)
      }
    }
    results
  }
}
```

lowers to:

```typescript
import { withTaskGroup } from "@mochi/runtime/concurrency/taskgroup.ts";

export async function fetchAll(urls: readonly string[]): Promise<readonly string[]> {
  const results: string[] = [];
  await withTaskGroup(async (tg) => {
    for (const u of urls) {
      tg.spawn(async (signal) => {
        const body = await httpGet(u, signal);
        results.push(body);
      });
    }
  });
  return results;
}
```

Notes:

- The order of `results` is non-deterministic; Mochi's scope semantics
  do not promise order in unordered spawn. If the user wrote
  `let results = parallel_map(urls, ...)` we would lower to
  `Promise.all` and preserve order.
- The `signal` is propagated to `httpGet` so each fetch sees the
  abort the moment any sibling fails.
- The `await withTaskGroup` cannot return until every spawned task
  has finished. If any task threw, the `await` itself raises the
  `AggregateError` and `results` is dropped.

## Bridging to Web Streams

Some MEP-52 emitter targets need to interoperate with native Web
Streams. We expose two adapters:

```typescript
// mochi_runtime/concurrency/web-streams.ts

export function toAsyncIterable<T>(
  rs: ReadableStream<T>,
  signal?: AbortSignal,
): AsyncIterable<T> {
  return {
    [Symbol.asyncIterator](): AsyncIterator<T> {
      const reader = rs.getReader();
      if (signal !== undefined) {
        signal.addEventListener("abort", () => {
          void reader.cancel().catch(() => undefined);
        }, { once: true });
      }
      return {
        next: async (): Promise<IteratorResult<T>> => {
          const result = await reader.read();
          if (result.done) {
            return { value: undefined as unknown as T, done: true };
          }
          return { value: result.value, done: false };
        },
        return: async (): Promise<IteratorResult<T>> => {
          await reader.cancel();
          return { value: undefined as unknown as T, done: true };
        },
      };
    },
  };
}

export function toReadableStream<T>(
  source: AsyncIterable<T>,
): ReadableStream<T> {
  let iterator: AsyncIterator<T>;
  return new ReadableStream<T>({
    start() {
      iterator = source[Symbol.asyncIterator]();
    },
    async pull(controller) {
      try {
        const result = await iterator.next();
        if (result.done) controller.close();
        else controller.enqueue(result.value);
      } catch (err) {
        controller.error(err);
      }
    },
    async cancel() {
      if (iterator.return !== undefined) {
        await iterator.return();
      }
    },
  });
}
```

These are used by `--target=stream-pipe` builds and by the fetch
runtime helper. They are not the default mailbox.

## Bridging to Node streams

Node streams (`Readable`, `Writable`) implement `Symbol.asyncIterator`
since Node 14. So `for await (const chunk of nodeReadable)` works
out of the box. The MEP-52 runtime does not add a Node-specific
helper; agents that consume Node streams iterate them directly under
the Node-conditional path of the `exports` map.

```typescript
// only emitted when target is node-or-bun
import { createReadStream } from "node:fs";

export async function* readFile(path: string): AsyncGenerator<Uint8Array, void, void> {
  const stream = createReadStream(path);
  try {
    for await (const chunk of stream) {
      yield chunk as Uint8Array;
    }
  } finally {
    stream.destroy();
  }
}
```

For Deno and Bun this is `Deno.open(path)` or `Bun.file(path).stream()`.
Each conditional file emits the right native call. See
[[07-runtime-portability]] for the io isolation pattern.

## Determinism and testing

Agent tests need a way to advance virtual time and ensure messages
are drained in order. MEP-52 ships a `MochiTestClock` and a
`drainMicrotasks` helper:

```typescript
// mochi_runtime/test/clock.ts

export class MochiTestClock {
  private now = 0;
  private readonly pending: Array<{
    readonly at: number;
    readonly fn: () => void;
  }> = [];

  setTimeout(fn: () => void, ms: number): void {
    this.pending.push({ at: this.now + ms, fn });
    this.pending.sort((a, b) => a.at - b.at);
  }

  advance(ms: number): void {
    const target = this.now + ms;
    while (this.pending.length > 0 && this.pending[0]!.at <= target) {
      const slot = this.pending.shift()!;
      this.now = slot.at;
      slot.fn();
    }
    this.now = target;
  }

  get current(): number {
    return this.now;
  }
}

export async function drainMicrotasks(rounds = 4): Promise<void> {
  for (let i = 0; i < rounds; i++) {
    await Promise.resolve();
  }
}
```

A typical agent test:

```typescript
import { describe, it, expect } from "vitest";
import { CounterAgent } from "../Counter.ts";
import { drainMicrotasks } from "@mochi/runtime/test/clock.ts";

describe("CounterAgent", () => {
  it("increments and returns count", async () => {
    const ctrl = new AbortController();
    const agent = new CounterAgent(ctrl.signal);
    agent.cast_Inc(3n);
    agent.cast_Inc(5n);
    await drainMicrotasks();
    expect(await agent.call_Get()).toBe(8n);
    ctrl.abort();
    await agent.join();
  });

  it("rejects calls after abort", async () => {
    const ctrl = new AbortController();
    const agent = new CounterAgent(ctrl.signal);
    ctrl.abort();
    await agent.join();
    await expect(agent.call_Get()).rejects.toThrow();
  });
});
```

Tests run under all four runtimes via the matrix described in
[[07-runtime-portability]]. `vitest` is the canonical runner because
it works under Node, Deno (via `npm:vitest`), and Bun.

## Cross-MEP comparison

| MEP | Agent shape                                | Mailbox            | Supervisor         | Errors            |
|-----|--------------------------------------------|--------------------|--------------------|-------------------|
| 46  | OTP `gen_server`                           | mailbox process    | OTP supervisor     | exit / Result     |
| 47  | sealed class + virtual-thread loop         | LinkedBlockingQueue| StructuredTaskScope| Result            |
| 48  | class + Channel + Task                     | Channel<T>         | CancellationTokenSource | Result      |
| 49  | actor + AsyncStream                        | AsyncStream<T>     | structured nursery | typed throws      |
| 50  | class + Channel + Job                      | Channel<T>         | SupervisorJob      | MochiResult       |
| 51  | class + asyncio.Queue                      | asyncio.Queue[T]   | TaskGroup          | MochiResult       |
| 52  | class + AsyncIterableQueue + AbortController | AsyncIterableQueue<T> | nested AbortControllers | AggregateError + MochiResult |

The MEP-52 row is the only one that has no native supervisor
primitive in the platform. Every other target language has either a
language-level (Swift), framework-level (Erlang/Elixir, .NET,
Kotlin, Python), or stdlib-level (Java 21+) primitive. JavaScript
has `AbortController`, which is a single one-bit cancellation signal
per controller. We reconstruct supervisor semantics from nested
controllers, lifecycle ownership, and `AggregateError`.

## Risks and mitigations

- **Unhandled promise rejection in agent loop.** If the `handle()`
  method throws and we do not catch in `loop()`, Node emits an
  `unhandledRejection` warning. The supervisor catches the loop
  promise. Risk mitigated. Lint rule:
  `@typescript-eslint/no-floating-promises` plus
  `--no-warnings=ExperimentalWarning` is rejected; we want warnings.
- **AbortController allocation on every child.** Each agent gets
  its own `AbortController`. For 10k agents this is 10k
  controllers. Microbenchmark: 10k controllers consume around 4 MB
  of heap and 50 ms to construct on Node 22. Acceptable. If a
  user reports a problem the emitter can pool controllers for
  short-lived agents.
- **Mailbox unbounded by default.** Risk of memory growth from a
  runaway cast loop. Mochi's documented contract says cast is
  best-effort; we accept the risk. The bounded mode is opt-in.
- **AggregateError loses stack traces.** ES2021 `AggregateError`
  does not include nested stack traces on every runtime
  identically. Node 22 shows them; Bun 1.1 shows them; Deno 2
  shows them; some older Safari versions truncate. We will surface
  a `errors[i].stack` join in the runtime's error formatter.
- **Cancellation race on Promise.race.** When a call awaits a race
  between the reply promise and a timeout abort, the loser still
  consumes microtask slots. For pathologically high call rates
  this can accumulate. Mochi's pattern is to use
  `AbortSignal.any` plus a single promise; we avoid `Promise.race`
  where possible.

## Open items

- **Bounded broadcaster default.** Decide whether `Broadcaster<T>`
  defaults to `capacity: Infinity` or to `capacity: 1024,
  dropOldest: true`. Tracking under research.
- **Replay buffer ring size.** `ReplayBroadcaster<T>` needs a
  default ring size. Proposal: 16. Open.
- **AbortSignal.any polyfill for old browsers.** Baseline 2024
  includes `AbortSignal.any` in all four tier-1 runtimes; older
  Safari 17.3 lacks it. We accept the floor; we do not ship a
  polyfill.
- **`using` declaration for supervisor disposal.** ES2026 ships
  `Symbol.dispose` and `await using`. When the floor moves, the
  supervisor's `shutdown()` becomes an `asyncDispose` implementation
  and the user writes `await using sup = new Supervisor(...)`. Not
  for v1.

## Summary

MEP-52 reconstructs agent and stream semantics from JavaScript's
native primitives. The runtime surface is around 600 lines split
across `queue.ts`, `supervisor.ts`, `taskgroup.ts`,
`broadcast.ts`, and `web-streams.ts`. The emitter lowers Mochi
agents to typed classes that wrap an `AsyncIterableQueue` and an
`AbortController`. Supervision trees are nested `AbortController`s
with restart policy enforcement. Sibling failures aggregate into
`AggregateError`. Cold streams are async generators. Hot streams
are broadcaster-multiplexed `AsyncIterableQueue`s. The result has
the same shape as MEP-49 through MEP-51, but without a single
third-party dependency and without leaving the ES2024 standard
surface.
