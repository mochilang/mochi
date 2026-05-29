---
title: "Phase 14. fetch"
sidebar_position: 15
sidebar_label: "Phase 14. fetch"
description: "MEP-52 Phase 14, Mochi fetch to platform built-in fetch (Node 18+, Deno, Bun, browser); WHATWG-compliant; HTTP/2 via undici on Node 22; streaming ReadableStream bodies; 15 fixtures."
---

# Phase 14. fetch

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 14](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase14Fetch`: 15 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130 (browser fetches a same-origin endpoint served by the Playwright harness). Secondary gates: tsc strict zero diagnostics; no `node-fetch`, no `axios`, no `got`, no `undici` direct import (all live entirely behind the platform `fetch` global); TLS verification is on by default (no `verify=false` opt-out).

## Goal-alignment audit

Mochi `fetch(url, opts)` is the portable HTTP client. All four tier-1 runtimes ship WHATWG-compliant `fetch` as a global since Node 18 (stable), Deno 1.x, Bun 1.0, and every modern browser. MEP-52 wires Mochi fetch directly to that global. The runtime additions are minimal: a typed wrapper plus a couple of helpers for the streaming-body case and the Mochi `bytes` to `Uint8Array` round-trip. This is the lowest-friction phase among the 18: most of the work is testing the byte-level equivalence across runtimes.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch(url)` to `fetch(url)`; await response; return `MochiHttpResponse {status, headers, body}` | NOT STARTED | n/a |
| 14.1 | POST with body: bytes, string, JSON; `Content-Type` defaults | NOT STARTED | n/a |
| 14.2 | Streaming responses: `response.body` is `ReadableStream<Uint8Array>`; expose as Mochi `stream<bytes>` via the Phase 10 adapter | NOT STARTED | n/a |
| 14.3 | Headers: case-insensitive read/write via `Headers` standard API | NOT STARTED | n/a |
| 14.4 | Errors: network errors throw `MochiPanic`; non-2xx returns the response (does not throw); the user dispatches on `response.status` | NOT STARTED | n/a |
| 14.5 | Temporal: `time` and `duration` lowering to `Temporal.*` via the `@js-temporal/polyfill` (until native ships, Open Q4); used by `Cache-Control` parsing and `If-Modified-Since` emission | NOT STARTED | n/a |

## Sub-phase 14.0, GET

### Decisions made (14.0)

**Mochi**: `let r = fetch("https://example.com/")`

**TypeScript**:

```typescript
// @mochi/runtime/fetch
export type MochiHttpResponse = {
  readonly status: number;
  readonly headers: Headers;
  readonly body: Uint8Array;
};

export async function mochiFetch(url: string, opts: RequestInit = {}): Promise<MochiHttpResponse> {
  const r = await fetch(url, opts);
  const body = new Uint8Array(await r.arrayBuffer());
  return { status: r.status, headers: r.headers, body };
}
```

**Why a thin wrapper**: Mochi's spec returns `MochiHttpResponse` (a record with `status`, `headers`, `body`); raw `Response` exposes a streaming API that does not match. The wrapper buffers the body eagerly for the simple case; the streaming case (sub-phase 14.2) returns `Response` directly.

## Sub-phase 14.1, POST with body

### Decisions made (14.1)

**Body**:

- `bytes` Mochi to `Uint8Array` TS to `BodyInit` (TypedArray is a valid `BodyInit`).
- `string` to `string` (UTF-8 encoded by `fetch` automatically).
- JSON object: emitter inserts `JSON.stringify(...)` and sets `content-type: application/json` if not already set.

```typescript
await mochiFetch("https://example.com/api", {
  method: "POST",
  headers: { "content-type": "application/json" },
  body: JSON.stringify({ name: "alice" }),
});
```

## Sub-phase 14.2, Streaming responses

### Decisions made (14.2)

**Mochi**: `for chunk in fetch_stream("https://...").body { ... }`

**TypeScript**:

```typescript
const r = await fetch("https://...");
if (r.body === null) throw new MochiPanic("response body is null");
for await (const chunk of r.body) {
  // chunk: Uint8Array
}
```

`Response.body` is `ReadableStream<Uint8Array>`, which is async-iterable on all four runtimes since 2024. The Phase 10 adapter is not needed (the platform already exposes the iterator); the emitter uses the `for await` form directly.

## Sub-phase 14.3, Headers

### Decisions made (14.3)

`Headers` API: case-insensitive `get`/`set`/`has`/`delete`/`append`. Mochi `r.headers["content-type"]` lowers to `r.headers.get("content-type") ?? ""` (the Mochi semantic returns empty string for missing headers; `Headers.get` returns `null`).

## Sub-phase 14.4, Errors

### Decisions made (14.4)

**Network errors** (DNS failure, TCP reset, TLS error): `fetch` rejects with a `TypeError`. The emitter wraps in `MochiPanic`:

```typescript
let r: Response;
try {
  r = await fetch(url, opts);
} catch (e) {
  throw new MochiPanic(`fetch failed: ${String(e)}`);
}
```

**Non-2xx**: returned to the user; no exception. The user checks `r.status` or `r.ok`.

**TLS verification**: on by default (the platform's default). No opt-out exposed at the Mochi layer.

## Sub-phase 14.5, Temporal

### Decisions made (14.5)

**Mochi `time` and `duration`** lower to `Temporal.ZonedDateTime` and `Temporal.Duration` respectively. The runtime imports `@mochi/runtime/temporal` which re-exports either the native `Temporal` (when available) or the `@js-temporal/polyfill` package.

```typescript
// @mochi/runtime/temporal
import { Temporal as PolyfillTemporal } from "@js-temporal/polyfill";
export const Temporal: typeof PolyfillTemporal =
  ((globalThis as any).Temporal as typeof PolyfillTemporal | undefined) ?? PolyfillTemporal;
```

**HTTP headers using Temporal**: `Date`, `Last-Modified`, `If-Modified-Since`, `Expires` parse via `Temporal.Instant.from(...)`. `Cache-Control: max-age=...` parses via `Temporal.Duration.from({seconds: n})`.

**Polyfill size**: roughly 60 KB minified. Phase 14 ships the polyfill as an opt-in `dependencies` entry; once native Temporal stabilises (Open Q4: likely Node 24, Deno 2.x, Bun 1.2, browsers Q3 2026 to Q1 2027), the polyfill drops to `peerDependenciesMeta` optional.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/fetch.go` | `fetch` call lowering to `mochiFetch` |
| `transpiler3/typescript/lower/temporal.go` | `time`/`duration` literal and operator lowering |
| `runtime3/typescript/src/fetch/index.ts` | `mochiFetch`, `MochiHttpResponse` |
| `runtime3/typescript/src/temporal/index.ts` | Native-or-polyfill Temporal re-export |
| `transpiler3/typescript/build/phase14_test.go` | `TestPhase14Fetch` |
| `tests/transpiler3/typescript/fixtures/phase14-fetch/` | 15 fixtures plus a local test server |
| `tests/transpiler3/typescript/fixtures/phase14-fetch/server.ts` | Local test server (Bun.serve or Node http) used by all fixtures |

## Test set

- `TestPhase14Fetch`, 15 fixtures four-runtime; harness starts the local test server then runs the fixture against it.
- `TestPhase14StreamingByteEqual`, streaming-body fixture captures chunks; the concatenated bytes match the equivalent eager-fetch fixture.
- `TestPhase14NoExtraDeps`, asserts emitted `package.json` does not list `node-fetch`, `axios`, `got`, or `undici` as dependencies.

## Deferred work

- HTTP/3 (QUIC). Node 22 fetch is HTTP/2-default; HTTP/3 is opt-in via undici options. Phase 14 ships without explicit HTTP/3 toggle.
- Connection pooling tuning. The platform default is sufficient for v1.
- HTTP/1.1 keep-alive timeout knobs. Default platform behaviour.
- Custom TLS certificate pinning. Out of scope; users who need it use FFI or a Node-specific path.
