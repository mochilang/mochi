---
title: "MEP-72 Note 08: Promise / async bridge"
sidebar_position: 9
sidebar_label: "08. async bridge"
description: "Why the TS / JS async bridge needs no runtime singleton: the host JS event loop is already there. The `Promise<T>` ↔ Mochi `async fun` translation, the `AsyncIterable<T>` ↔ Mochi `stream<T>` translation, the microtask scheduling cost (~50ns on V8 16.x as of May 2026), the comparison to Rust tokio's block_on and Go cgo's handle pool."
---

# 08. Promise / async bridge

This note describes the async-side bridge work, and why it is structurally smaller than MEP-73's Rust async bridge and MEP-74's Go goroutine bridge. It is informative.

## 1. The host JS event loop

Every JavaScript runtime the bridge targets ships with a built-in event loop plus microtask queue:

| Runtime | Event loop implementation |
|---------|---------------------------|
| Node 22 LTS | libuv (worker threads, network I/O via libuv) plus V8 microtask queue |
| Deno 2 | Tokio (Rust async runtime, exposed to JS via op_call) plus V8 microtask queue |
| Bun 1.1 | Zig-native event loop plus JavaScriptCore (or V8 on newer Bun) microtask queue |
| Browser | DOM event loop plus task / microtask queues |
| Cloudflare Workers / Vercel Edge / Deno Deploy | workerd or platform-specific runtime; all expose V8 microtask queue |

In every case, the runtime provides:

- `Promise` (TC39 spec)
- `async function` / `await` (TC39 spec)
- `queueMicrotask` (TC39 spec)
- `setTimeout` (HTML / Node spec)
- `AsyncIterator` protocol (TC39 spec)

These are the same primitives the Mochi-emitted JS uses for its own async (MEP-52 phase 11.3 lowers Mochi `async fun` to a JS `async function`). The consumed package's `async function` and `Promise<T>` return values plug into the same event loop without any bridge layer.

## 2. The translation

| TS construct | Mochi construct |
|--------------|-----------------|
| `Promise<T>` (in return position) | `async fun(): T` |
| `PromiseLike<T>` | `async fun(): T` (structurally identical) |
| `AsyncIterable<T>` (in return position or `for await` source) | `stream<T>` |
| `AsyncIterator<T>` | `stream<T>` |
| `async function f(): Promise<T>` | `async fn f(): T` (Mochi-side declaration) |

The Mochi caller writes:

```mochi
import ts "node:fetch" as fetch

async fun example(): string {
    let resp = await fetch.fetch("https://api.example.com")
    return await resp.text()
}
```

The MEP-52-emitted JS is:

```typescript
import { fetch } from "node:fetch";

export async function example(): Promise<string> {
    const resp = await fetch("https://api.example.com");
    return await resp.text();
}
```

There is no bridge layer in between. The `await fetch(...)` call goes directly to the host runtime's `fetch` implementation; the returned `Response` is the host's `Response` object; `await resp.text()` goes directly to the `Response.text()` method.

## 3. Why no runtime singleton

MEP-73's Rust bridge needs a `tokio::runtime::Runtime` singleton because Rust's `async fn` is a state machine that requires a runtime to drive it. Calling an async Rust function from synchronous Mochi-emitted Rust requires `runtime.block_on(async { ... })`, which is a 3μs cold / 500ns warm operation per call.

MEP-74's Go bridge needs the cgo handle pool because the goroutine scheduler lives inside the c-archive; calling a Go function with channel parameters from Mochi requires marshalling the channel through a cgo.Handle, which is a 200ns per call operation.

MEP-72's TS / JS bridge needs neither. The host runtime's event loop is already there; the Mochi-emitted async code uses the same event loop the consumed package's async code uses. There is no boundary to cross.

Compounding effects:

- **No per-import async runtime to construct**: any number of async-fn-exposing packages add zero RAM cost for the runtime layer (the runtime is the host runtime; it is there regardless).
- **No `block_on` ceremony**: every async call is a direct microtask scheduling.
- **No thread pool**: the host runtime owns the thread pool decision (Node 22 LTS uses libuv's default 4-worker pool; Deno 2 uses tokio's tunable pool; Bun 1.1 uses Zig's pool).
- **No enter-runtime ceremony**: the JS engine's call stack IS the runtime.

## 4. Microtask scheduling cost

Measured on darwin-arm64, May 2026 microbenchmarks:

| Op | Cost |
|----|------|
| `await Promise.resolve(0)` | ~50 ns (V8 16.x on Node 22.11.0) |
| `await Promise.resolve(0)` | ~45 ns (V8 16.x on Deno 2) |
| `await Promise.resolve(0)` | ~55 ns (JavaScriptCore on Bun 1.1) |
| Rust tokio `block_on(async { 0 })` | ~3 μs cold, ~500 ns warm |
| Go cgo callback `mochi_<pkg>_<fn>(...)` | ~200 ns |

For a hot loop calling an async TS function 1M times, the cumulative cost is ~50ms. For the comparable hot loop in Mochi-Rust, the cumulative cost is ~500ms; for Mochi-Go, ~200ms.

## 5. `AsyncIterable<T>` and Mochi `stream<T>`

MEP-52 phase 10 ships Mochi `stream<T>` as a Mochi-native iterator type. The translation maps:

| TS construct | Mochi construct |
|--------------|-----------------|
| `AsyncIterable<T>` (return) | `stream<T>` (return) |
| `for await (const x of source)` | `for x in source { ... }` (the Mochi `for` loop is the AsyncIterator's consumer) |

The MEP-52-emitted JS for `for x in source { ... }` is `for await (const x of source) { ... }` when the source is `AsyncIterable<T>`, or `for (const x of source) { ... }` when the source is `Iterable<T>`. The bridge picks the right one at type-binding time based on the TS type.

## 6. Error handling

A consumed package's `async function` rejection (`throw new Error(...)` inside the async function, or a rejected Promise return) translates to a Mochi exception via MEP-52 phase 11's `try-catch` desugar. The Mochi caller writes:

```mochi
try {
    let resp = await fetch.fetch("https://api.example.com")
} catch e {
    log.error("fetch failed: " + e.message)
}
```

The MEP-52-emitted JS is:

```typescript
try {
    const resp = await fetch("https://api.example.com");
} catch (e) {
    log.error("fetch failed: " + e.message);
}
```

The bridge does no per-call wrapping. The JS engine's existing `await` machinery handles the rejection.

## 7. AbortController and cancellation

Many async APIs accept an `AbortSignal` parameter for cancellation:

```typescript
const controller = new AbortController();
const resp = await fetch(url, { signal: controller.signal });
```

The bridge translates `AbortSignal` as an opaque extern type; the Mochi caller can construct an `AbortController` via the global `AbortController` extern (or via a per-package wrapper) and pass `controller.signal` through. The Mochi-side `stream<T>` cancellation (MEP-52 phase 10.5 deferred work) maps to `AbortController.abort()` when wired.

## 8. Cross-references

- [[02-design-philosophy]] §4 — why no runtime singleton.
- [[05-type-mapping]] §10 — the Promise / async type table.
- [MEP-52 phase 11 async colour pass](/docs/implementation/0052/phase-11-async) — the Mochi-side async story.
