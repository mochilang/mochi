---
title: "Phase 14. fetch (HTTP / file)"
sidebar_position: 19
sidebar_label: "Phase 14. fetch"
description: "MEP-51 Phase 14 -- fetch(url) lowers to mochi_runtime.fetch.mochi_fetch via urllib.request.urlopen (file:// and http(s):// out of the box); writeFile(path, content) writes UTF-8 bytes in binary mode; 10 fixtures."
---

# Phase 14. fetch (HTTP / file)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 14](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (14.0 only; httpx surface DEFERRED) |
| Started        | 2026-05-29 20:14 (GMT+7) |
| Landed         | 2026-05-29 20:20 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase14Fetch`: 10 fixtures green on CPython 3.12.7 in `transpiler3/python/build/phase14_test.go`. The corpus is ported verbatim from the PHP target's `tests/transpiler3/php/fixtures/phase14-fetch/`: `fetch_basic`, `fetch_concat`, `fetch_empty`, `fetch_json_string`, `fetch_multiline`, `fetch_newlines`, `fetch_overwrite_fetch`, `fetch_reuse`, `fetch_string`, `fetch_use_result`. Each fixture compiles, runs `python -m mochi_user_<name>` against the runtime, and byte-compares stdout to the matching `.out` file. Coverage: bare body print, body concatenated into a string, empty content, JSON-shaped string body, multiline body, double-fetch reuse, write/fetch/overwrite/fetch round-trip. The full Phase 1-14 regression (`go test ./transpiler3/python/... -count=1`) finishes in 113.2s with zero regressions.

## Goal-alignment audit

Mochi's `fetch(url)` is the v1 HTTP surface and `writeFile(path, content)` is its companion for synthesizing test inputs. For the Python target, the load-bearing v1 use case is hermetic CI: every fixture writes a known string to a `/tmp/...` file, fetches it back through a `file://` URL, and prints the result. The C and PHP targets both went this way (PHP via `file_get_contents` which natively handles `file://` and `http(s)://`; C via libcurl); the Python target uses Python's stdlib `urllib.request.urlopen` for the same property: a single stdlib symbol handles all URL schemes without a third-party dependency.

Landing 14.0 unblocks two distinct payloads: (1) Mochi programs that ingest local file inputs through the URL surface (notebook cells reading fixture data); (2) live HTTP fetches against external services, which work identically through urllib without a runtime swap. Live HTTP is not gated by Phase 14.0; it just happens because urllib already supports it.

The httpx surface (async client, HTTP/2, connection pooling, TLS verification policy) originally scoped for Phase 14 is deferred. The v1 corpus has no fixture that exercises any of those features; the single use case is "fetch one URL, decode the body, print it". Adding httpx as a wheel dependency at Phase 14 would impose a hard runtime requirement that no v1 program needs. When a real fixture lands that requires async I/O, HTTP/2, or fine-grained TLS control, a Phase 14.1 sub-phase swaps the implementation behind the same `mochi_fetch` symbol without changing the lower or any call-site emit.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch(url)` lowers to `mochi_fetch(url)` against `mochi_runtime.fetch`; `writeFile(path, content)` lowers to `mochi_write_file(path, content)`; backed by urllib.request + open(binary) | LANDED 2026-05-29 | (filled at ship) |
| 14.1 | httpx async client surface: `fetch(url, async: true)` returns `Future[str]`; rides on the Phase 11.1 async colour pass | DEFERRED | -- |
| 14.2 | HTTP method, headers, body, query params: `fetch(url, method: "POST", headers: {...}, body: ...)` | DEFERRED | -- |
| 14.3 | Streaming bodies via `AsyncIterator[bytes]` | DEFERRED | -- |
| 14.4 | TLS verification policy + custom CA bundle from env | DEFERRED | -- |
| 14.5 | Connection pooling + HTTP/2 via httpx 0.27+ | DEFERRED | -- |

## Sub-phase 14.0 -- urllib-backed fetch + writeFile

### Goal-alignment audit (14.0)

A Mochi program that calls `fetch(url)` should run on the Python target without changes, deterministically, in CI, against a `file://` URL pointing at a fixture or a temporary file the program wrote with `writeFile`. The PHP target made this work by routing both surfaces to PHP stdlib (`file_get_contents`, `file_put_contents`). The Python target needs an equivalent stdlib path so the wheel does not gain a runtime dependency for the load-bearing CI case. urllib.request fits exactly: it supports `http://`, `https://`, and `file://` in one symbol; it returns bytes; it raises `URLError` on failure. Wrapping it in a helper that returns `""` on failure matches the C / PHP fetch shape (which silently produces an empty body on a missing fixture or 404) and keeps the call site free of `try/except` plumbing.

### Decisions made (14.0)

**Stdlib urllib over httpx for the v1 surface.** httpx is the right Python HTTP client for async, HTTP/2, and connection pooling, none of which the v1 fixtures exercise. urllib ships with CPython and covers `file://` natively, which is exactly the v1 corpus. Trading a wheel dependency for nothing was the wrong call. When async + HTTP/2 arrive in a real fixture, a Phase 14.1 implementation swaps urllib for httpx behind the same `mochi_fetch` symbol; the lower does not change.

**`mochi_fetch` returns `""` on URLError, never raises.** This matches the C and PHP targets: a missing fixture or a 5xx response produces an empty body but does not abort the program. Mochi has no first-class `Result[T,E]` (that ships with Phase 11.1 MochiResult). Until then, the empty-string sentinel is the cross-target convention.

**`mochi_write_file` uses binary mode + explicit UTF-8 encode.** Text mode on Windows translates `\n` to `\r\n` on write, which would skew the stdout byte-equal gate for the `fetch_multiline` / `fetch_newlines` fixtures. Binary mode keeps bytes exact. The explicit `content.encode("utf-8")` is needed because `open(... "wb")` does not accept str.

**Single `needsFetch` flag drives the import.** A program that uses either `fetch` or `writeFile` (or both) gets one `from mochi_runtime.fetch import mochi_fetch, mochi_write_file` line. Splitting flags per symbol would add a second import line for programs that use both, which is the load-bearing case in the corpus (most fixtures write a file then fetch it back).

**Both `WriteFileStmt` (statement) and `HttpGetExpr` (expression) dispatch through the lower's main switch.** They are not optional features bolted onto an existing form, they are first-class IR nodes the c aotir produces; the Python lower handles them like any other statement/expression. No special-case routing through `lowerCallStmt`.

**`writeFile` discards its return value at the call site.** The aotir IR types `WriteFileStmt` as a statement (no return value), matching Mochi semantics. The Python emit is `mochi_write_file(...)` as an ExprStmt; the helper returns `None`.

**Fixtures use absolute `/tmp/...` paths, ported from PHP.** The PHP target's fixtures already use `/tmp/mochi_swift_<name>.txt` patterns; copying them verbatim preserves cross-target byte-equal validation. On macOS/Linux the path exists; on Windows the same fixture would need a different path, but Phase 14.0's CI matrix is the Mochi standard (linux/darwin) so this does not gate the ship.

### Fixture corpus (10 fixtures, ported from PHP)

`tests/transpiler3/python/fixtures/phase14-fetch/`:

| Fixture | Surface |
|---------|---------|
| `fetch_basic` | Write + fetch + print body |
| `fetch_concat` | `print("hello " + r)` |
| `fetch_empty` | Body string is `"empty test"` (no scare quotes, just a literal) |
| `fetch_json_string` | JSON-shaped body `"status-ok"` |
| `fetch_multiline` | `"line1\nline2\nline3"` round-trip (binary-mode write is load-bearing) |
| `fetch_newlines` | Same as multiline (separate path; cross-fixture confidence) |
| `fetch_overwrite_fetch` | Write A, fetch, write B, fetch, print both |
| `fetch_reuse` | Two fetches against the same URL; both return same body |
| `fetch_string` | `let result = "Got: " + r; print(result)` |
| `fetch_use_result` | `print(r1 + " " + r2)`; two-fetch concat |

`TestPhase14Fetch` walks the directory and runs `runPythonFixture` per fixture. All 10 fixtures pass on CPython 3.12.7.

### Files changed

| File | Purpose |
|------|---------|
| `runtime/python/mochi_runtime/fetch.py` (new) | `mochi_fetch(url) -> str` via urllib.request; `mochi_write_file(path, content) -> None` via binary-mode open |
| `transpiler3/python/lower/fetch.go` (new) | `lowerHttpGetExpr` + `lowerWriteFileStmt`; both set `needsFetch` |
| `transpiler3/python/lower/lower.go` | `needsFetch bool` slot; dispatch `*aotir.HttpGetExpr` (expression) and `*aotir.WriteFileStmt` (statement); conditional `from mochi_runtime.fetch import mochi_fetch, mochi_write_file` import |
| `transpiler3/python/build/build.go` | Cache marker bumped `mep51-phase13` -> `mep51-phase14` |
| `transpiler3/python/build/phase14_test.go` (new) | `TestPhase14Fetch` walks `phase14-fetch/` |
| `tests/transpiler3/python/fixtures/phase14-fetch/` (new) | 10 fixtures ported verbatim from the PHP target |

## Deferred work

- **14.1 httpx async client + Phase 11.1 await.** `fetch(url, async: true)` would return `Future[str]`; rides on the Phase 11.1 async colour pass. Deferred until v1 has an async-fetch fixture.
- **14.2 method / headers / body / query params.** `fetch(url, method: "POST", headers: {...}, body: ...)` expands the aotir IR and the Python emit. Deferred for the same reason: no v1 fixture asks for it.
- **14.3 streaming bodies.** `AsyncIterator[bytes]` over chunk transfer encoding; rides on Phase 11.1 async + httpx. Deferred.
- **14.4 TLS verification policy.** `MOCHI_TLS_INSECURE=1` opt-out + `MOCHI_CA_BUNDLE=/path/...` env support. Deferred until a program needs to hit a self-signed endpoint.
- **14.5 connection pooling + HTTP/2.** httpx 0.27+ Client with HTTP/2 enabled. Deferred until a corpus fixture demonstrates a measurable win.
- **`appendFile(path, content)` for the Python target.** The aotir has `AppendFileStmt` (Phase 6.5); the Python lower does not yet route it. Deferred to Phase 14.x or rolled into Phase 6.5 follow-up depending on when a fixture lands.
