---
title: "Phase 14. fetch (URLSession)"
sidebar_position: 18
sidebar_label: "Phase 14. fetch"
description: "MEP-49 Phase 14 — HTTP fetch via URLSession; JSON decode/encode via Codable; SSE streaming; WebSocket via URLSessionWebSocketTask."
---

# Phase 14. fetch (URLSession)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 14](/docs/mep/mep-0049#phase-14-fetch) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase14Fetch`: 10 fixtures green on Swift 6.0 and 6.1, linux-x64 (with mock HTTP server). `TestSwiftcClean` remains green.

## Goal-alignment audit

Mochi `fetch` is the HTTP client primitive. On Swift, it lowers to `URLSession` which is the system HTTP stack on all Apple platforms and is available (via Foundation) on Linux. Using `URLSession` (not a third-party HTTP library) minimises dependencies, uses system TLS (SecureTransport on Apple, BoringSSL on Linux via swift-cmark), and integrates with the platform's network stack including App Transport Security on iOS.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch(url, method, headers, body)` → `URLSession.data(for:)` async; JSON decode via `Codable` | NOT STARTED | — |
| 14.1 | Streaming responses → `URLSession.bytes(for:)` → `AsyncStream<String>`; SSE parsing | NOT STARTED | — |
| 14.2 | WebSocket → `URLSessionWebSocketTask`; `send` / `receive` messages | NOT STARTED | — |
| 14.3 | Mock HTTP server for tests (`MOCHI_FETCH_MOCK=1` environment flag) | NOT STARTED | — |

## Sub-phase 14.0 -- HTTP fetch

### Decisions made (14.0)

**`URLSession.shared`**: all Mochi `fetch` calls use `URLSession.shared` by default. The session is configurable via `MochiRuntime.configure(urlSession: mySession)` for testing or proxy configuration.

**`URLSession.data(for:)` async**: SE-0296 (Swift 5.5+). Returns `(Data, URLResponse)`. Available on all platforms (macOS 12+, iOS 15+, Linux via Foundation).

**Mochi `fetch` lowering**:

```swift
// Mochi: let res = fetch("https://api.example.com/users", method: "GET")
public func mochiGet(_ urlString: String, headers: OrderedDictionary<String, String> = [:]) async throws -> Data {
    guard let url = URL(string: urlString) else { throw MochiFetchError.invalidURL(urlString) }
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    for (key, value) in headers { request.addValue(value, forHTTPHeaderField: key) }
    let (data, response) = try await URLSession.shared.data(for: request)
    guard let httpResponse = response as? HTTPURLResponse,
          (200...299).contains(httpResponse.statusCode) else {
        throw MochiFetchError.httpError((response as? HTTPURLResponse)?.statusCode ?? -1)
    }
    return data
}
```

**JSON decode**: `fetch` + decode in Mochi → `URLSession.data` + `JSONDecoder().decode(T.self, from: data)`:

```swift
// Mochi: let user: User = fetch_json("https://api.example.com/user/1")
let data = try await mochiGet("https://api.example.com/user/1")
let user = try JSONDecoder().decode(User.self, from: data)
```

Since Mochi records conform to `Codable` (Phase 4.3), this works without additional code.

**JSON keys**: by default, Swift `Codable` synthesis uses the Swift property name (camelCase). The JSON API may use snake_case. `MochiRuntime` provides a `SnakeCaseDecoder` that sets `.keyDecodingStrategy = .convertFromSnakeCase` on `JSONDecoder`.

**POST with body**:

```swift
// Mochi: let res = fetch("https://api.example.com/users", method: "POST", body: new_user)
var request = URLRequest(url: url)
request.httpMethod = "POST"
request.httpBody = try JSONEncoder().encode(newUser)
request.addValue("application/json", forHTTPHeaderField: "Content-Type")
```

**`MochiFetchError`**: a `MochiRuntime` error type covering `invalidURL`, `httpError(statusCode)`, `decodeFailed`, `networkError(underlying)`.

## Sub-phase 14.1 -- Streaming responses

### Decisions made (14.1)

**`URLSession.bytes(for:)`**: SE-0310 (Swift 5.5+). Returns `(URLSession.AsyncBytes, URLResponse)`. `AsyncBytes` is an `AsyncSequence<UInt8>`.

**SSE (Server-Sent Events) parsing**: SSE streams emit lines of the form `data: <payload>\n\n`. The lowerer generates an SSE parser using `swift-async-algorithms`'s `.lines()` operator:

```swift
// Mochi: let stream = fetch_stream("https://api.example.com/events")
let (asyncBytes, _) = try await URLSession.shared.bytes(for: request)
let sseStream = asyncBytes.lines
    .filter { $0.hasPrefix("data: ") }
    .map { String($0.dropFirst(6)) }
    .compactMap { line -> Event? in
        guard line != "[DONE]" else { return nil }
        return try? JSONDecoder().decode(Event.self, from: Data(line.utf8))
    }
// Consumed as AsyncStream<Event>
```

**`AsyncBytes.lines`**: `swift-async-algorithms` provides `.lines` on `AsyncSequence<UInt8>` that splits by newline, returning `AsyncLineSequence`. This handles `\n`, `\r\n`, and `\r` line endings.

**LLM streaming**: Mochi `@llm(stream: true)` on Linux uses this SSE path (Phase 13.3).

## Sub-phase 14.2 -- WebSocket

### Decisions made (14.2)

**`URLSessionWebSocketTask`**: available on macOS 10.15+, iOS 13+, Linux via Foundation. Provides `send(_ message: URLSessionWebSocketTask.Message) async throws` and `receive() async throws -> URLSessionWebSocketTask.Message`.

**Mochi WebSocket lowering**:

```swift
// Mochi: let ws = websocket("wss://api.example.com/chat")
let session = URLSession.shared
let task = session.webSocketTask(with: URL(string: "wss://api.example.com/chat")!)
task.resume()

// Mochi: ws.send("hello")
try await task.send(.string("hello"))

// Mochi: let msg = ws.receive()
let message = try await task.receive()
switch message {
case .string(let text): ...
case .data(let bytes): ...
@unknown default: break
}
```

**WebSocket as stream**: `MochiRuntime` provides a `webSocketStream(url:)` helper that wraps the receive loop into an `AsyncStream<String>`:

```swift
public func webSocketStream(url: URL) -> AsyncStream<String> {
    AsyncStream { continuation in
        Task {
            let task = URLSession.shared.webSocketTask(with: url)
            task.resume()
            while true {
                guard let msg = try? await task.receive() else { break }
                if case .string(let text) = msg { continuation.yield(text) }
            }
            continuation.finish()
        }
    }
}
```

## Sub-phase 14.3 -- Mock HTTP for tests

### Decisions made (14.3)

**`URLProtocol` mock**: `MochiRuntime` registers a `MockURLProtocol` when `MOCHI_FETCH_MOCK=1` is set. The mock reads request URLs and returns pre-configured responses from a JSON fixture file:

```swift
// mock_responses.json:
{
    "https://api.example.com/users": {
        "status": 200,
        "body": "[{\"name\":\"alice\",\"age\":30}]"
    }
}
```

**`URLProtocol`**: `URLSession` routes all requests through registered `URLProtocol` subclasses. `MockURLProtocol` intercepts all HTTP requests in test mode and returns the fixture response synchronously.

**Test fixture structure**: each fetch fixture directory contains:
- `main.mochi`: the Mochi source
- `main.out`: expected stdout
- `mock_responses.json`: HTTP mock responses

**Linux CI**: the mock server runs in `URLProtocol` (in-process), so no external HTTP server is needed on linux-x64 CI.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/fetch.go` | `fetch`, `fetch_json`, `fetch_stream`, `websocket` lowering |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Fetch.swift` | `mochiGet`, `mochiPost`, `webSocketStream`, SSE parser |
| `transpiler3/swift/runtime/Sources/MochiRuntime/FetchError.swift` | `MochiFetchError` enum |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Mock/MockURLProtocol.swift` | Test mock URLProtocol |
| `transpiler3/swift/build/phase14_test.go` | `TestPhase14Fetch`: 10 fixtures with HTTP mock |
| `tests/transpiler3/swift/fixtures/phase14-fetch/` | 10 fixture directories with `mock_responses.json` |

## Test set

- `TestPhase14Fetch` -- 10 fixtures: `fetch_get_string`, `fetch_get_json`, `fetch_post_json`, `fetch_headers`, `fetch_error_404`, `fetch_error_network`, `fetch_stream_sse`, `fetch_stream_bytes`, `fetch_websocket_send_receive`, `fetch_websocket_stream`.

## Deferred work

- HTTP/2 push streams. Deferred -- `URLSession` handles HTTP/2 transparently.
- gRPC. Deferred to Phase 12 (FFI via grpc-swift).
- OAuth 2.0 token refresh. Deferred to Phase 14.1.
- Download task with progress reporting. Deferred to Phase 14.2.
- Background URLSession for iOS background fetch. Deferred to Phase 15 (iOS packaging).
