---
title: "Phase 14. fetch (HTTP via httpx)"
sidebar_position: 19
sidebar_label: "Phase 14. fetch"
description: "MEP-51 Phase 14 -- Mochi fetch(url, ...) lowers to httpx.AsyncClient().request(...); HTTP/2 enabled; streaming bodies via AsyncIterator; TLS verification on by default with verify=False rejected at codegen; 15 fixtures."
---

# Phase 14. fetch (HTTP via httpx)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 14](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase14Fetch`: 15 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gates: byte-equal stdout against vm3 for every fixture; the local test server (`httpx.MockTransport` for unit-level + `aiohttp.web` for the HTTP/2 path-through gate) covers GET/POST/PUT/DELETE/PATCH; TLS verification is on by default and the codegen rejects `verify=false` with diagnostic M057_FETCH_E001.

## Goal-alignment audit

`fetch(url)` is Mochi's HTTP surface; combined with Phase 13 (LLM) it is what makes the Python target useful for FastAPI-style and notebook-style network programs. Without Phase 14 every Mochi program that hits the network has no Python target. Landing 14 uses `httpx.AsyncClient` (FastAPI's default, HTTP/2-capable, asyncio-native) as the runtime, exposes streaming bodies via `AsyncIterator[bytes]`, and locks down TLS so verification is on by default and cannot be opted out via the language surface. The user payload is `let resp = await fetch("https://api.example.com/users")` lowering to one `await` against `httpx.AsyncClient.request("GET", url)` with full type safety on the return.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | httpx 0.27+ async client; rejected: `requests` (sync only), `aiohttp` (heavier, fewer features), `urllib.request` (sync only, no HTTP/2) | NOT STARTED | -- |
| 14.1 | HTTP/2 support via `httpx.AsyncClient(http2=True)`; ALPN negotiation; falls back to HTTP/1.1 transparently | NOT STARTED | -- |
| 14.2 | Streaming bodies via `AsyncIterator[bytes]`: `resp.aiter_bytes()` for download; `Stream<bytes>` upload via request content iterator | NOT STARTED | -- |
| 14.3 | TLS verification on by default; `verify=False` rejected at codegen with diagnostic M057_FETCH_E001 | NOT STARTED | -- |

## Sub-phase 14.0 -- httpx async client

### Goal-alignment audit (14.0)

`httpx` is the canonical typed-async HTTP client on the Python ecosystem. FastAPI uses it as the default test client; Anthropic and OpenAI SDKs both build on it. Without 14.0 the Mochi fetch surface either falls back to `requests` (sync only, blocks the event loop) or to `aiohttp` (heavier, slower release cadence, asyncio-only with no Trio fallback). Landing 14.0 picks `httpx` so the Mochi fetch composes with every other async piece (agents, streams, LLM) and so the resulting Python wheel uses the same HTTP stack as the Python ecosystem default.

### Decisions made (14.0)

Mochi `let resp = await fetch("https://example.com/api/users", method: "GET")` lowers to:

```python
from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from typing import Final

import httpx

from mochi_runtime.result import Err, MochiResult, Ok


@dataclass(frozen=True, slots=True)
class FetchResponse:
    status: int
    headers: Mapping[str, str]
    body: bytes

    def text(self) -> str:
        return self.body.decode("utf-8")


@dataclass(frozen=True, slots=True)
class FetchError:
    message: str
    cause: Exception | None


_CLIENT: Final[httpx.AsyncClient] = httpx.AsyncClient(http2=True, timeout=30.0)


async def fetch(
    url: str,
    method: str = "GET",
    headers: Mapping[str, str] | None = None,
    body: bytes | None = None,
) -> MochiResult[FetchResponse, FetchError]:
    try:
        response = await _CLIENT.request(
            method,
            url,
            headers=dict(headers) if headers is not None else None,
            content=body,
        )
    except httpx.HTTPError as exc:
        return Err(FetchError(message=str(exc), cause=exc))
    return Ok(
        FetchResponse(
            status=response.status_code,
            headers=dict(response.headers),
            body=response.content,
        )
    )
```

Decisions:

- `_CLIENT` is a module-level singleton; `httpx.AsyncClient` is designed for reuse (connection pool, HTTP/2 multiplexing). Creating one per request would cause socket exhaustion on busy programs.
- The wrapper returns `MochiResult[FetchResponse, FetchError]` (Phase 11.2 surface), not a raw `httpx.Response`. The user sees a frozen-slots dataclass and can `match` on it.
- `headers` is `Mapping[str, str] | None`, not `dict`. The wrapper materialises to `dict` before passing to httpx because `httpx.Headers` constructor requires a concrete dict.
- `body: bytes | None` is the upload payload; the IR pass picks `content=body` (not `data=body` or `json=body`) so the user controls the encoding. JSON encoding is opt-in via `fetch(url, json={"k": "v"})` which the IR pass routes to `content=json.dumps(...).encode("utf-8")` plus `Content-Type: application/json`.
- `timeout=30.0` is the default; user overrides via Mochi `fetch(url, timeout: 60.0)` route to `httpx.Timeout(60.0)`.
- The IR pass rejects `requests` and `aiohttp` imports from Mochi code; httpx is the only HTTP backend in v1.

## Sub-phase 14.1 -- HTTP/2

### Goal-alignment audit (14.1)

HTTP/2 multiplexes multiple requests over a single connection, which is critical for any program that fans out (e.g. `await all(fetch(u) for u in urls)`). Without 14.1 each concurrent request takes a separate connection, the server's connection limit is hit fast, and the program's throughput collapses. `httpx` enables HTTP/2 with one constructor flag; the cost is one runtime dep (`h2`) that's already bundled with the httpx wheel. Landing 14.1 makes the fetch surface usable for fanout workloads.

### Decisions made (14.1)

`httpx.AsyncClient(http2=True)` is the default in `_CLIENT` above. The h2 library is added to `mochi_runtime`'s runtime deps; it is pulled in transitively as `httpx[http2]` in `pyproject.toml`:

```toml
[project]
dependencies = [
    "httpx[http2]>=0.27,<1",
    "anyio>=4,<5",
]
```

Decisions:

- ALPN negotiation is automatic: `httpx` advertises both `h2` and `http/1.1` in the ClientHello; the server picks. Fallback to HTTP/1.1 is transparent.
- HTTP/3 (`QUIC`) is not enabled; httpx 0.27 does not support it stably. Deferred until httpx ships HTTP/3 GA.
- Connection pool size defaults to httpx defaults (10 keepalive connections, 100 max connections per host). User overrides via Mochi `@fetch_config(max_connections=200)` route to `httpx.Limits`.
- The wire log captures HTTP/2 frames when `MOCHI_FETCH_DEBUG=1` is set; the log goes to stderr and excludes bodies (just method + URL + status).

## Sub-phase 14.2 -- Streaming bodies

### Goal-alignment audit (14.2)

Many production workloads stream bodies: file downloads, server-sent events (SSE), gRPC over HTTP/2. Without 14.2 the fetch surface always materialises the body fully, which OOMs on large downloads. Landing 14.2 exposes `AsyncIterator[bytes]` for both download and upload so a Mochi program can `for chunk in resp.body_stream { process(chunk) }` without buffering the whole response.

### Decisions made (14.2)

Streaming download:

```python
from __future__ import annotations

from collections.abc import AsyncIterator
from dataclasses import dataclass

import httpx

from mochi_runtime.result import Err, MochiResult, Ok


@dataclass(frozen=True, slots=True)
class FetchStreamResponse:
    status: int
    headers: dict[str, str]
    body_stream: AsyncIterator[bytes]


async def fetch_stream(url: str) -> MochiResult[FetchStreamResponse, FetchError]:
    async def _iter_response() -> AsyncIterator[bytes]:
        async with _CLIENT.stream("GET", url) as response:
            async for chunk in response.aiter_bytes():
                yield chunk

    try:
        async with _CLIENT.stream("GET", url) as initial:
            return Ok(
                FetchStreamResponse(
                    status=initial.status_code,
                    headers=dict(initial.headers),
                    body_stream=_iter_response(),
                )
            )
    except httpx.HTTPError as exc:
        return Err(FetchError(message=str(exc), cause=exc))
```

Streaming upload:

```python
from __future__ import annotations

from collections.abc import AsyncIterator


async def upload_stream(url: str, chunks: AsyncIterator[bytes]) -> MochiResult[FetchResponse, FetchError]:
    try:
        response = await _CLIENT.request("POST", url, content=chunks)
    except httpx.HTTPError as exc:
        return Err(FetchError(message=str(exc), cause=exc))
    return Ok(FetchResponse(status=response.status_code, headers=dict(response.headers), body=response.content))
```

Decisions:

- Download streaming uses `client.stream(method, url)` as an async context manager; the body iterator yields raw bytes from `aiter_bytes()`. The IR pass picks chunk size from the Mochi `@chunk_size(8192)` annotation; default is httpx's default (no explicit chunk size).
- The streaming response's `body_stream` field is `AsyncIterator[bytes]`; once consumed it cannot be re-iterated. The IR pass detects multi-use and rejects at codegen.
- Upload streaming passes any `AsyncIterator[bytes]` as `content=...`; httpx accepts it as the request body iterator natively.
- The streaming form is opt-in via Mochi `fetch_stream(url)`; the standard `fetch(url)` materialises the body. The IR pass picks based on the call site.

Worked example for SSE consumption:

```python
from __future__ import annotations

from collections.abc import AsyncIterator

from mochi_runtime.fetch import fetch_stream
from mochi_runtime.result import Err, Ok


async def consume_sse(url: str) -> None:
    match await fetch_stream(url):
        case Ok(value=resp):
            buffer = b""
            async for chunk in resp.body_stream:
                buffer += chunk
                while b"\n\n" in buffer:
                    event, _, buffer = buffer.partition(b"\n\n")
                    print(event.decode("utf-8"))
        case Err(error=err):
            print(f"sse failed: {err.message}")
```

## Sub-phase 14.3 -- TLS verification

### Goal-alignment audit (14.3)

TLS verification is the security gate for fetch. A program that hits HTTPS endpoints without verifying the certificate is wide open to MITM attacks. Without 14.3 a user could (or could be tricked into) writing `fetch(url, verify: false)` and bypass the entire TLS PKI. Landing 14.3 rejects `verify=false` at codegen, sets the httpx default (`verify=True`) explicitly in the singleton client, and documents the deprecation.

### Decisions made (14.3)

The `_CLIENT` singleton is constructed with `verify=True` (the default; explicit for documentation):

```python
_CLIENT: Final[httpx.AsyncClient] = httpx.AsyncClient(
    http2=True,
    timeout=30.0,
    verify=True,
)
```

The IR pass rejects any Mochi source that sets `verify=False` or `verify=false`:

```
M057_FETCH_E001: TLS verification cannot be disabled.
  --> source.mochi:42:18
   |
42 |     fetch(url, verify: false)
   |                ^^^^^^^^^^^^^ rejected at codegen
   |
   = note: Mochi enforces TLS verification on all HTTP calls.
   = note: To use a custom CA bundle, set SSL_CERT_FILE in the environment.
```

For private CAs, users set `SSL_CERT_FILE` env var (httpx reads it via the underlying `ssl` module). The IR pass does not surface a Mochi-level annotation for custom CAs; the env var is the only escape hatch and it is process-wide, not request-scoped.

Mutual TLS (client certificates) is out of scope for v1; the IR pass rejects `cert=` arguments with diagnostic M057_FETCH_E002. Mutual TLS lands in a v2 phase.

The `verify=True` value is also enforced at the streaming variants (`fetch_stream`, `upload_stream`); the same `_CLIENT` singleton is reused so the policy is consistent.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/fetch.go` | `fetch(url, ...)` to `await mochi_runtime.fetch.fetch(...)`; reject `verify=false`, `cert=` |
| `transpiler3/python/lower/fetch_stream.go` | `fetch_stream(url)` to streaming variant returning `FetchStreamResponse` |
| `runtime/python/mochi_runtime/fetch/__init__.py` | Public surface: `fetch`, `fetch_stream`, `upload_stream`, `FetchResponse`, `FetchStreamResponse`, `FetchError` |
| `runtime/python/mochi_runtime/fetch/_client.py` | `_CLIENT` singleton with `http2=True`, `verify=True`, `timeout=30` |
| `transpiler3/python/build/phase14_test.go` | `TestPhase14Fetch`: 15 fixtures + local test server harness |
| `tests/transpiler3/python/fixtures/phase14-fetch/` | 15 fixture directories with httptest server URLs |

## Test set

- `TestPhase14Fetch` -- 15 fixtures: fetch_get_text, fetch_post_json, fetch_put_no_body, fetch_delete, fetch_patch_json, fetch_headers_round_trip, fetch_404_err (7 from 14.0); fetch_http2_concurrent, fetch_http2_to_http1_fallback (2 from 14.1); fetch_stream_download, fetch_stream_sse, fetch_stream_upload, fetch_stream_break_early (4 from 14.2); fetch_verify_false_rejected (codegen rejection), fetch_custom_ca_via_env (2 from 14.3).

## Deferred work

- HTTP/3 (QUIC) support. Deferred until httpx ships GA HTTP/3; v1 uses HTTP/2 + HTTP/1.1.
- Mutual TLS (client certificates) via `cert=` argument. Deferred to v2; M057_FETCH_E002 rejects in v1.
- WebSocket support (would need `websockets` or `aiohttp` as a separate runtime dep). Deferred to a separate WebSocket MEP; the fetch surface stays HTTP-only.
- Cookie persistence across requests via `httpx.Cookies` (currently every call is stateless). Deferred to v1.5; user can pass cookies via headers in the meantime.
- Per-request retry/backoff policy (currently the user wraps in a Mochi loop). Deferred to v1.5; the runtime ships a simple `with_retry(fetch_call, max_attempts=3)` helper in `mochi_runtime.fetch.retry`.
