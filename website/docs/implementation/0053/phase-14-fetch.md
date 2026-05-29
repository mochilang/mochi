---
title: "Phase 14. Fetch + json_decode"
sidebar_position: 16
sidebar_label: "Phase 14. Fetch"
description: "MEP-53 Phase 14, httpGet over std::net::TcpStream and json_decode via a 90-LOC hand-rolled object decoder."
---

# Phase 14. Fetch + json_decode

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22609](https://github.com/mochilang/mochi/issues/22609) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 0e74a0deea |

## Gate

`TestPhase14Fetch` walks `tests/transpiler3/rust/fixtures/phase14-fetch/` (17 fixtures), starts a local `httptest.Server` to back the fetch calls, substitutes the test server URL into each fixture's source (replacing the `HTTPTEST_URL` token), builds the Rust crate, runs the binary, and diffs stdout. Coverage: simple GET, JSON-returning GET piped through `json_decode`, status-code error handling (4xx and 5xx panic with code 98), chunked-encoding response, large response body.

## Lowering decisions

`fetch <url>` and `httpGet(url)` both lower to:

```rust
let body: String = mochi_runtime::fetch::get(url);
```

`mochi_runtime::fetch::get`:

1. Parses the URL (strict: must start with `http://`, no TLS).
2. Connects via `std::net::TcpStream::connect(addr)`.
3. Sends an HTTP/1.1 GET with `Connection: close`, `User-Agent: mochi-rust/0.1`, and a Host header.
4. Reads the response to EOF.
5. Splits at `\r\n\r\n` to separate headers from body.
6. If status >= 400, raises panic code 98.
7. If `Transfer-Encoding: chunked`, decodes the chunked body.
8. Returns the body as a `String`.

No TLS support means no `https://` URLs. This is a deliberate choice to keep the runtime small enough to compile under the `embedded` feature (when std is re-enabled): rustls + ring would add ~50K LOC of crypto code. Mochi programs needing HTTPS are pointed at FFI'ing into libcurl or rolling their own TLS.

`json_decode(s)` lowers to:

```rust
let obj: std::collections::HashMap<String, String> = mochi_runtime::json::decode(s);
```

`mochi_runtime::json::decode` is a 90-LOC object decoder. It parses only top-level objects (`{...}`), coerces all values to strings:

- String values: literal string contents (with `\n`, `\r`, `\t`, `\\`, `\"`, `\/`, `\b`, `\f` escapes decoded).
- Integer / float values: their lexical representation.
- `true` / `false`: `"true"` / `"false"`.
- `null`: `""`.

This is intentionally narrow: Mochi's `json_decode` contract says "decode top-level object, all values are strings." The 90-LOC decoder matches this contract exactly with zero allocation past the output HashMap.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/fetch.go` | fetch / httpGet lowering |
| `transpiler3/rust/lower/json.go` | json_decode lowering |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `fetch` and `json` modules |
| `transpiler3/rust/build/phase14_test.go` | 17-fixture gate with httptest.Server |
| `tests/transpiler3/rust/fixtures/phase14-fetch/*.mochi` + `.out` | 17 fixtures |

## Test set

- `TestPhase14Fetch/<fixture>` for each `.mochi` in the fixture directory (17 fixtures).

## Closeout notes

`HTTPTEST_URL` substitution was the cleanest way to avoid baking a fixed port into fixtures. The test server picks a random port; the test harness reads the `.mochi` source as a string, replaces `HTTPTEST_URL` with the actual server URL, writes the substituted source to a temp file, and builds from there. This keeps fixtures portable across runs and across machines.

Chunked-encoding support landed in the same commit. Several test-server responses use chunked encoding for stream-style emit; the 30-line `decode_chunked` function handles the standard format (`{hex_size}\r\n{data}\r\n` repeating until `0\r\n`).
