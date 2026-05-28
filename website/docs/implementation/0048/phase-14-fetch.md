---
title: "Phase 14. fetch (HTTP)"
sidebar_position: 16
sidebar_label: "Phase 14. fetch"
description: "MEP-48 Phase 14 — HttpGetExpr to Mochi.Runtime.IO.Fetch.Get (synchronous HttpClient); local httptest server; 2 fixtures."
---

# Phase 14. fetch (HTTP)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 14](/docs/mep/mep-0048#phase-14-fetch-http) |
| Status         | LANDED |
| Started        | 2026-05-28 05:38 (GMT+7) |
| Landed         | 2026-05-28 05:50 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase14Fetch`: 2 fixtures green (fetch_hello, fetch_json) against a local httptest server (HTTPTEST_URL substitution). `Fetch.Get` uses `HttpClient.GetStringAsync().GetAwaiter().GetResult()` (synchronous). Full async HttpClient, TLS 1.3 gate, and additional fixture coverage are deferred.

## Goal-alignment audit

`fetch(...)` is Mochi's built-in HTTP call surface. On .NET, `HttpClient` is the BCL's HTTP implementation — HTTP/3-capable, connection-pool-aware, and the reference design for async HTTP. Phase 14 ships a thin `Mochi.Runtime.Fetch.FetchAsync` wrapper that adds request/response type safety and a cassette-style test hook.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch url into var` → `Mochi.Runtime.IO.Fetch.Get(url)` (synchronous HttpClient) | LANDED | — |
| 14.2 | `json_decode(s)` → `Mochi.Runtime.IO.JSON.Decode(s)` returning `Dictionary<string,string>` | LANDED | — |
| 14.3 | Local test server fixture harness (httptest.Server + HTTPTEST_URL substitution) | LANDED | — |

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
| `transpiler3/dotnet/build/phase14_test.go` | `TestPhase14Fetch`: 2 fixtures + local httptest server |
| `tests/transpiler3/dotnet/fixtures/phase14-fetch/` | 2 fixture directories (fetch_hello, fetch_json) |

## Test set

- `TestPhase14Fetch` -- 2 fixtures: fetch_hello (GET plain text), fetch_json (GET + JSON decode via `Mochi.Runtime.IO.JSON.Decode`).

## Deferred work

- WebSocket support. Deferred to Phase 3 sub-MEP.
- HTTP/2 server push. Not planned.
- mTLS client certificates. Deferred to Phase 12 (FFI / security).

## Closeout notes

Phase 14 landed. `TestPhase14Fetch` PASS: 2/2 fixtures on net10.0 (fetch_hello, fetch_json).

`HttpGetExpr` → `Mochi.Runtime.IO.Fetch.Get(url)` (synchronous `HttpClient.GetStringAsync().GetAwaiter().GetResult()`). `JsonDecodeExpr` → `Mochi.Runtime.IO.JSON.Decode(input)` returning `Dictionary<string, string>` via `System.Text.Json`. The test driver starts a `net/http/httptest` server that serves `/hello` (plain text) and `/json` (JSON object), substitutes `HTTPTEST_URL` in each fixture source, compiles to a fx-dependent DLL, and runs it. No live network in CI.
