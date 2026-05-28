---
title: "Phase 14. fetch (mochiHttpGet)"
sidebar_position: 18
sidebar_label: "Phase 14. fetch"
description: "MEP-49 Phase 14 — HTTP fetch lowering to mochiHttpGet via Foundation Data(contentsOf:); synchronous string response."
---

# Phase 14. fetch (mochiHttpGet)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 14](/docs/mep/mep-0049#phase-14-fetch) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase14Fetch`: 5 fixtures green on Swift 6.0+, macOS 15. Gate builds each fixture and compares stdout to `.expected`. Fixtures use a local file-scheme URL or a server started in the test.

## Goal-alignment audit

The v1 fetch gate uses synchronous `Data(contentsOf: url)` which works for the fixture suite and requires no async colouring. The full `URLSession.data(for:)` async path, SSE streaming, WebSocket, and mock `URLProtocol` are deferred. This keeps Phase 14 simple while unblocking fixtures that need to read remote (or local file-scheme) content.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `FetchExpr` → `mochiHttpGet(url)` | LANDED | mep/0049-phase-14 |
| 14.1 | `URLSession.data(for:)` async; JSON decode via `Codable` | DEFERRED | — |
| 14.2 | Streaming responses via `URLSession.bytes(for:)` + SSE parsing | DEFERRED | — |
| 14.3 | WebSocket via `URLSessionWebSocketTask` | DEFERRED | — |
| 14.4 | `MockURLProtocol` for in-process HTTP mocking | DEFERRED | — |

## Sub-phase 14.0 -- HTTP GET

### Decisions made (14.0)

**`mochiHttpGet(_ urlString: String) -> String`**: synchronous fetch via `Data(contentsOf:)`. Returns the response body as a UTF-8 string, trimmed of trailing newlines. Returns `""` on invalid URL or network error.

```swift
public func mochiHttpGet(_ urlString: String) -> String {
    guard let url = URL(string: urlString) else { return "" }
    guard let data = try? Data(contentsOf: url) else { return "" }
    return (String(data: data, encoding: .utf8) ?? "").trimmingCharacters(in: .newlines)
}
```

**Lowering**: `FetchExpr` in the aotir IR lowers to `mochiHttpGet(urlExpr)` as a `RawSwiftExpr`. The same pattern is also handled in the C backend's `lowerFetchExpr`.

**Synchronous**: `Data(contentsOf:)` blocks the calling thread. This is acceptable for the fixture suite and CLI programs; async `URLSession` is deferred.

**C backend parity**: `transpiler3/c/lower/lower.go` was updated in the same phase to add `lowerFetchExpr` for `parser.FetchExpr` and the `pr.Fetch != nil` branch in `lowerPrimary`, ensuring the fetch lowering pattern is consistent across backends.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `FetchExpr` lowering to `mochiHttpGet(url)` |
| `transpiler3/c/lower/lower.go` | `lowerFetchExpr`; `pr.Fetch != nil` branch in `lowerPrimary` |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Fetch.swift` | `mochiHttpGet` |
| `transpiler3/swift/build/phase14_test.go` | `TestPhase14Fetch`: 5 fixtures |
| `tests/transpiler3/swift/fixtures/phase14-fetch/` | 5 fixture directories |

## Test set

- `TestPhase14Fetch` -- 5 fixtures: `fetch_basic`, `fetch_json_string`, `fetch_multiline`, `fetch_string`, `fetch_use_result`.

## Deferred work

- `URLSession.data(for:)` async HTTP; `MochiFetchError` typed error enum. Deferred to Phase 14.1.
- Streaming responses via `URLSession.bytes(for:)` + SSE `data:` line parsing. Deferred to Phase 14.2.
- WebSocket via `URLSessionWebSocketTask`; `webSocketStream(url:)` helper. Deferred to Phase 14.3.
- `MockURLProtocol` for in-process HTTP mock (no external server needed in CI). Deferred to Phase 14.4.
- POST with body; custom headers. Deferred to Phase 14.1.
