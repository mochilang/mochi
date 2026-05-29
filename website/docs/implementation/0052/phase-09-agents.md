---
title: "Phase 9. Agents"
sidebar_position: 10
sidebar_label: "Phase 9. Agents"
description: "MEP-52 Phase 9, Mochi agents as TypeScript class wrapping AsyncIterableQueue<Message> + AbortController; cast, call, supervision via nested AbortControllers; AggregateError for sibling failure aggregation; 35 fixtures."
---

# Phase 9. Agents

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 9](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase9Agents`: 35 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: tsc strict zero diagnostics; eslint `no-floating-promises` enforced (no `void this.loop()` floating without an explicit `void` keyword or `.catch`); the concurrency runtime under `@mochi/runtime/concurrency/` stays under 8 KB gzipped (the budget from [[07-runtime-portability]] §3).

## Goal-alignment audit

Agents are Mochi's primary concurrency abstraction across every backend. MEP-45 to MEP-51 each lower agents to the target platform's native actor primitive: Erlang gen_server (MEP-46), Loom virtual thread (MEP-47), .NET `Channel<T>` (MEP-48), Swift actor (MEP-49), Kotlin coroutine + `Channel` (MEP-50), Python `asyncio.Queue` + `TaskGroup` (MEP-51). The JavaScript runtime offers no equivalent built-in. MEP-52 hand-rolls one. The shape (`AsyncIterableQueue<Message>` + `AbortController` + `Promise.withResolvers()` for call replies) is fixed by the abstract; Phase 9 lands it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | `AsyncIterableQueue<T>` runtime class with `push`, `pushAwait`, `close`, `fail`, `[Symbol.asyncIterator]()` | NOT STARTED | n/a |
| 9.1 | `MochiAgent` base class with mailbox, abort signal, loop driver; user `agent Counter { ... }` lowers to a subclass | NOT STARTED | n/a |
| 9.2 | `cast(msg)` (fire-and-forget) and `call(req): Promise<Reply>` (request-reply via `Promise.withResolvers`) | NOT STARTED | n/a |
| 9.3 | Supervision tree: nested `AbortController` instances; `one_for_all` and `one_for_one` strategies | NOT STARTED | n/a |
| 9.4 | Sibling failure aggregation via `AggregateError` (ES2021); parent surfaces a `MochiResult.Err` carrying the inner errors | NOT STARTED | n/a |

## Sub-phase 9.0, AsyncIterableQueue

### Decisions made (9.0)

**Shape** (per [[09-agent-streams]] §AsyncIterableQueue):

```typescript
// @mochi/runtime/concurrency/queue
export class AsyncIterableQueue<T> implements AsyncIterable<T> {
  private readonly buffer: T[] = [];
  private readonly waiters: Array<(v: IteratorResult<T>) => void> = [];
  private readonly producers: Array<() => void> = [];
  private readonly capacity: number;
  private closed = false;
  private failure: unknown = undefined;

  constructor(options: { capacity?: number } = {}) {
    this.capacity = options.capacity ?? Number.POSITIVE_INFINITY;
  }

  push(value: T): void { /* ... */ }
  async pushAwait(value: T): Promise<void> { /* ... */ }
  close(): void { /* ... */ }
  fail(reason: unknown): void { /* ... */ }
  [Symbol.asyncIterator](): AsyncIterator<T> { /* ... */ }
}
```

**Why not RxJS, Web Streams, EventEmitter**: dependency burden, type-surface mismatch, ad-hoc cancellation (see [[09-agent-streams]] §"Why hand-roll" and MEP-52 §Rationale).

**Backpressure**: `pushAwait` blocks the producer when `buffer.length >= capacity` until a consumer drains. `push` (sync) throws when the queue is closed and never blocks; intended for fire-and-forget producers.

**Closure ordering**: `close()` resolves all pending waiters with `{done: true}`. `fail(reason)` records the reason then closes; the next `next()` call rejects with the recorded reason instead of resolving with `done: true`.

## Sub-phase 9.1, MochiAgent base class

### Decisions made (9.1)

**Base class**:

```typescript
// @mochi/runtime/concurrency/agent
export abstract class MochiAgent<Msg> {
  protected readonly mailbox = new AsyncIterableQueue<Msg>();
  protected readonly signal: AbortSignal;

  constructor(signal: AbortSignal) {
    this.signal = signal;
    signal.addEventListener("abort", () => this.mailbox.close());
    void this.loop();
  }

  protected abstract handle(msg: Msg): void | Promise<void>;

  private async loop(): Promise<void> {
    try {
      for await (const msg of this.mailbox) {
        if (this.signal.aborted) break;
        await this.handle(msg);
      }
    } catch (e) {
      this.mailbox.fail(e);
    }
  }
}
```

**Generated subclass for `agent Counter`** (Mochi source):

```mochi
agent Counter {
  state: int = 0
  cast Inc(n: int) { state = state + n }
  call Value(): int { reply(state) }
}
```

**Generated TypeScript**:

```typescript
export type CounterMsg =
  | { readonly kind: "Inc"; readonly n: bigint }
  | { readonly kind: "Value"; readonly reply: (v: bigint) => void };

export class Counter extends MochiAgent<CounterMsg> {
  private state: bigint = 0n;

  protected handle(msg: CounterMsg): void {
    switch (msg.kind) {
      case "Inc":   this.state = this.state + msg.n; return;
      case "Value": msg.reply(this.state); return;
    }
  }

  cast(msg: Exclude<CounterMsg, { kind: "Value" }>): void {
    this.mailbox.push(msg);
  }

  async call(req: { kind: "Value" }): Promise<bigint> {
    const { promise, resolve } = Promise.withResolvers<bigint>();
    this.mailbox.push({ kind: req.kind, reply: resolve } as CounterMsg);
    return promise;
  }
}
```

## Sub-phase 9.2, cast and call

### Decisions made (9.2)

**`cast` (fire-and-forget)**: sync, never awaits; `mailbox.push(msg)` returns immediately. Caller does not observe success.

**`call` (request-reply)**: `Promise.withResolvers()` (ES2024) gives `{promise, resolve, reject}`. The agent's message variant carries the `reply` callback; the handler invokes `reply(value)` to fulfil the caller's promise. Errors in the handler propagate via the queue's `fail` path; the caller observes via `await call(...).catch(...)`.

**Timeout**: a `call(req, {timeout: 5_000})` overload wraps the promise in `Promise.race([promise, timeoutReject(timeout)])`. Phase 9 ships without timeout (callers compose `Promise.race` themselves); the overload is a v1.5 add.

## Sub-phase 9.3, Supervision

### Decisions made (9.3)

**`MochiSupervisor`**:

```typescript
// @mochi/runtime/concurrency/supervisor
export type Strategy = "one_for_one" | "one_for_all";

export class MochiSupervisor {
  private readonly controller = new AbortController();
  private readonly children: Array<{
    factory: (signal: AbortSignal) => MochiAgent<unknown>;
    instance: MochiAgent<unknown>;
  }> = [];
  constructor(private readonly strategy: Strategy = "one_for_all") {}
  spawn<M>(factory: (signal: AbortSignal) => MochiAgent<M>): MochiAgent<M> {
    const instance = factory(this.controller.signal);
    this.children.push({ factory: factory as any, instance: instance as MochiAgent<unknown> });
    return instance;
  }
  shutdown(): void { this.controller.abort(); }
}
```

**`one_for_all` (default)**: a child failure (handler throws) calls `controller.abort()`; every sibling observes `signal.aborted === true`, exits its `for await` loop, releases resources. Matches OTP `one_for_all`.

**`one_for_one`**: the failed child is restarted via its factory; siblings unaffected. Implemented by catching inside the agent's loop and re-spawning via the saved factory. Phase 9 ships `one_for_all`; `one_for_one` is sub-phase 9.3.1.

**Nested supervision**: `MochiSupervisor` can spawn child supervisors (each has its own `AbortController` whose signal is also added to the parent's). A parent abort propagates down; a child abort stays scoped.

## Sub-phase 9.4, AggregateError

### Decisions made (9.4)

**Sibling failure aggregation**: when the parent supervisor receives a child failure, it aborts siblings and collects their failure reasons (each agent's `mailbox.failure` field after close). The collected reasons are wrapped in an `AggregateError`:

```typescript
class MochiSupervisorFailure extends AggregateError {
  constructor(errors: unknown[]) {
    super(errors, "MochiSupervisor: one or more child agents failed");
    this.name = "MochiSupervisorFailure";
  }
}
```

`AggregateError` is ES2021, native on all four runtimes. It matches MEP-51's `ExceptionGroup` story and is the canonical multi-error type in JavaScript.

**Surface to user**: `await supervisor.run()` returns `MochiResult.Ok(())` if all children completed successfully, or `MochiResult.Err(new MochiSupervisorFailure([...]))` on failure. The MochiResult wrapper is Phase 11; Phase 9 emits the bare `AggregateError` and Phase 11 wires it through.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/colour/colour.go` | Sync/async colour pass (full activation, formerly trivial in Phase 1 to 8); seeds: agent intent handlers, calls to async functions, AsyncIterable consumers |
| `transpiler3/typescript/lower/agents.go` | Agent decl to MochiAgent subclass; message variant union; cast/call methods |
| `transpiler3/typescript/lower/supervisor.go` | Supervisor spawn lowering; AbortController plumbing |
| `runtime3/typescript/src/concurrency/queue.ts` | `AsyncIterableQueue<T>` |
| `runtime3/typescript/src/concurrency/agent.ts` | `MochiAgent<Msg>` base class |
| `runtime3/typescript/src/concurrency/supervisor.ts` | `MochiSupervisor` with one_for_all / one_for_one |
| `transpiler3/typescript/build/phase09_test.go` | `TestPhase9Agents` |
| `tests/transpiler3/typescript/fixtures/phase09-agents/` | 35 fixtures (counter, adder, balance, switch_agent, supervisor_one_for_all, etc.) |

## Test set

- `TestPhase9Agents`, 35 fixtures four-runtime.
- `TestPhase9NoFloatingPromise`, eslint `no-floating-promises: error` against every emitted file.
- `TestPhase9SupervisorAggregate`, fixture spawns two children, one fails, parent surfaces `AggregateError([err1, err2])`.
- `TestPhase9ConcurrencyBudget`, `@mochi/runtime/concurrency/` stays under 8 KB gzipped.

## Deferred work

- `one_for_one` strategy. Sub-phase 9.3.1.
- Per-call timeouts. v1.5.
- Distributed agents (remote mailbox via WebSocket). Out of scope.
- Persistent agents (durable mailbox via IndexedDB or SQLite). Out of scope.
- Hot reload (`agent_replace_state`). MEP-46 territory; not in MEP-52 v1.
