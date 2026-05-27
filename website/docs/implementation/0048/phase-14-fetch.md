---
title: "Phase 14. fetch (HTTP)"
sidebar_position: 16
sidebar_label: "Phase 14. fetch"
description: "MEP-48 Phase 14 — fetch(...) to HttpClient via Mochi.Runtime.Fetch.FetchAsync; TLS 1.3; HTTP/3; local test server; 10 fixtures."
---

# Phase 14. fetch (HTTP)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 14](/docs/mep/mep-0048#phase-14-fetch-http) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase14Fetch`: 10 fixtures green against a local test HTTP server (no live network in CI). TLS 1.3 default verified. `HttpClient` instance reuse across calls (no socket exhaustion).

## Goal-alignment audit

`fetch(...)` is Mochi's built-in HTTP call surface. On .NET, `HttpClient` is the BCL's HTTP implementation — HTTP/3-capable, connection-pool-aware, and the reference design for async HTTP. Phase 14 ships a thin `Mochi.Runtime.Fetch.FetchAsync` wrapper that adds request/response type safety and a cassette-style test hook.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch(url)` → `await FetchAsync(url, ct)` returning `Result<string, string>` | NOT STARTED | — |
| 14.1 | `fetch(url, { method: "POST", body: json })` → POST with JSON body | NOT STARTED | — |
| 14.2 | `fetch_json<T>(url)` → deserialise response body via `System.Text.Json` | NOT STARTED | — |
| 14.3 | Local test server fixture harness | NOT STARTED | — |

## Sub-phase 14.0 -- Basic fetch

### Decisions made (14.0)

**`fetch(url)`** lowers to:

```csharp
Result<string, string> resp =
    await Mochi.Runtime.Fetch.FetchClient.GetAsync(url, ct).ConfigureAwait(false);
```

**`FetchClient`**: a singleton `HttpClient` with:
- `HttpVersion = HttpVersion.Version30` (HTTP/3, falls back to HTTP/2 and HTTP/1.1)
- TLS 1.3 default (`SslProtocols.Tls13`)
- Default timeout: 30 seconds (overridable via `MOCHI_FETCH_TIMEOUT_MS` env var)

**`HttpClient` singleton**: a single `static readonly HttpClient` in `FetchClient` is the entire connection pool. `HttpClient` is designed to be long-lived and reused; creating one per request causes socket exhaustion. The Mochi runtime creates exactly one instance per process.

**Return type**: `Result<string, string>` — `Ok<string, string>(body)` on 2xx, `Err<string, string>(statusCode.ToString())` on non-2xx or network error.

## Sub-phase 14.3 -- Local test server

### Decisions made (14.3)

**`TestHttpServer`** in `build_test.go`: starts a `System.Net.HttpListener` on a random port. Each fixture test registers expected request/response pairs. The fixture Mochi code calls `fetch("http://localhost:{port}/path")`.

**`MOCHI_FETCH_BASE_URL` env var**: the test harness injects the local server base URL. `FetchClient` prepends this URL to relative paths when the env var is set.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/expr.go` | `fetch(...)` → `FetchClient.GetAsync(...)` / `PostAsync(...)` |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Fetch/FetchClient.cs` | Singleton HttpClient; GetAsync, PostAsync, fetch_json |
| `transpiler3/dotnet/build/phase14_test.go` | `TestPhase14Fetch`: 10 fixtures + local test server |
| `tests/transpiler3/dotnet/fixtures/phase14-fetch/` | 10 fixture directories |

## Test set

- `TestPhase14Fetch` -- 10 fixtures: GET returns body, GET 404 returns Err, POST JSON body, POST and read response, fetch_json deserialise, fetch with custom header, fetch with timeout, fetch parallel (5 concurrent GETs), fetch in loop (3 sequential), fetch error handling with Result.

## Deferred work

- WebSocket support. Deferred to Phase 3 sub-MEP.
- HTTP/2 server push. Not planned.
- mTLS client certificates. Deferred to Phase 12 (FFI / security).

## Closeout notes

Phase 14 not yet started.
