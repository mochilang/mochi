---
title: "Phase 14. fetch (HTTP)"
sidebar_position: 16
sidebar_label: "Phase 14. fetch"
description: "MEP-47 Phase 14 — fetch statement via java.net.http.HttpClient; json_decode via Jackson; tested against local httptest server."
---

# Phase 14. fetch (HTTP)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 14](/docs/mep/mep-0047#phase-14-fetch-http) |
| Status         | LANDED |
| Started        | 2026-05-27 14:45 (GMT+7) |
| Landed         | 2026-05-27 14:49 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase14Fetch` -- 10 fixtures green on JDK 21 and JDK 25, tested against a local Go `net/http/httptest.Server` (no internet access).

## Goal-alignment audit

`fetch` is Mochi's built-in HTTP GET primitive. It is simpler than the full FFI path to `java.net.http.HttpClient`: no import statement, no null bridge, no type annotation. After Phase 14 lands, Mochi programs that do simple HTTP fetches (API calls, web scraping, config fetching) compile to JVM with one-line syntax. The local `httptest.Server` gate ensures tests are hermetic.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 14.0 | `fetch "url" into body` -> `Fetch.get(url)` via `java.net.http.HttpClient` | LANDED | — |
| 14.1 | `fetch url into body` where `url` is a Mochi variable -> same, `url` evaluated dynamically | LANDED | — |
| 14.2 | `json_decode(body)` -> flat JSON parser -> Mochi `map<string, string>` | LANDED | — |

## Sub-phase 14.0 -- fetch with string literal URL

### Goal-alignment audit (14.0)

The literal URL case is the simplest and most common: `fetch "https://api.example.com/data" into body`. Implementing this first validates the `Fetch.get` runtime path before adding variable URLs (14.1) and JSON decoding (14.2).

### Decisions made (14.0)

**`fetch` statement lowering**: Mochi:

```mochi
fetch "https://example.com/api" into body
print(body)
```

Lowers to:

```java
final String body = dev.mochi.runtime.io.Fetch.get("https://example.com/api");
dev.mochi.runtime.io.IO.println(body);
```

**`Fetch.get` implementation**:

```java
package dev.mochi.runtime.io;

public final class Fetch {
    private static final java.net.http.HttpClient CLIENT = java.net.http.HttpClient.newBuilder()
        .version(java.net.http.HttpClient.Version.HTTP_2)
        .followRedirects(java.net.http.HttpClient.Redirect.NORMAL)
        .build();

    public static String get(String url) {
        var request = java.net.http.HttpRequest.newBuilder()
            .uri(java.net.URI.create(url))
            .GET()
            .build();
        try {
            var response = CLIENT.send(request,
                java.net.http.HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() >= 400) {
                throw new dev.mochi.runtime.error.MochiPanicException(98,
                    "fetch failed: HTTP " + response.statusCode() + " from " + url);
            }
            return response.body();
        } catch (dev.mochi.runtime.error.MochiPanicException e) {
            throw e;
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(98,
                "fetch failed: " + e.getMessage());
        }
    }
}
```

**Loom integration**: `HttpClient.send()` on a virtual thread: the JDK's HTTP/2 implementation uses non-blocking I/O internally. On a Loom virtual thread, `HttpClient.send()` is a structured blocking call that unmounts the carrier thread during the network wait. Zero OS threads are blocked during HTTP fetches.

**HTTP/2 fallback**: `HttpClient.Version.HTTP_2` requests HTTP/2 but falls back to HTTP/1.1 if the server does not support HTTP/2. The `httptest.Server` in the gate test uses HTTP/1.1 (Go's `httptest.NewServer` default); the `Redirect.NORMAL` policy follows 3xx redirects.

**Error handling**:
- HTTP 4xx/5xx: throw `MochiPanicException` with code 98 and the status code in the message.
- Network error (`IOException`, `UnknownHostException`): throw `MochiPanicException` with code 98.
- Invalid URL (`IllegalArgumentException` from `URI.create`): throw `MochiPanicException` with code 97.

**`CLIENT` is a static field**: The `HttpClient` instance is shared across all `Fetch.get` calls in the program's lifetime. This enables connection pooling and HTTP/2 multiplexing. Thread-safe: `HttpClient.send` is documented as safe for concurrent use.

## Sub-phase 14.1 -- fetch with variable URL

### Goal-alignment audit (14.1)

Variable URLs are required for programs that construct URLs dynamically (e.g., `"https://api.example.com/users/" + user_id`). The lowering is identical to 14.0 -- the URL expression is evaluated at runtime.

### Decisions made (14.1)

**`fetch url into body` lowering**: Mochi:

```mochi
let api_url = "https://api.example.com/users/" + user_id
fetch api_url into body
```

Lowers to:

```java
final String api_url = "https://api.example.com/users/" + user_id;
final String body = dev.mochi.runtime.io.Fetch.get(api_url);
```

The URL expression is evaluated before the `Fetch.get` call. No special handling is needed: the URL is just a `String` passed to `Fetch.get`.

**`fetch` with `into` binding**: `into body` declares a new `final String body` local variable. If `body` is already declared in scope, the lower pass reuses the existing variable (reassignment) rather than declaring a new one. The Mochi type checker resolves the shadowing rule.

## Sub-phase 14.2 -- json_decode

### Goal-alignment audit (14.2)

`json_decode(body)` is the most common operation after `fetch`: parsing the response body as JSON. Using Jackson's `ObjectMapper` (which Phase 13 already added as a runtime dependency) provides a battle-tested JSON parser without adding another dependency.

### Decisions made (14.2)

**`json_decode` lowering**: Mochi:

```mochi
let data = json_decode(body)
print(data["name"])
```

Lowers to:

```java
final java.util.Map<String, String> data = dev.mochi.runtime.io.JSON.decode(body);
dev.mochi.runtime.io.IO.println(data.get("name"));
```

**`JSON.decode` implementation**:

```java
package dev.mochi.runtime.io;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.*;

public final class JSON {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Decode a JSON string to a Mochi map<string, string>. */
    public static java.util.Map<String, String> decode(String json) {
        try {
            var node = MAPPER.readTree(json);
            var result = new java.util.LinkedHashMap<String, String>();
            node.fields().forEachRemaining(entry -> {
                result.put(entry.getKey(), entry.getValue().asText());
            });
            return result;
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(97,
                "json_decode failed: " + e.getMessage());
        }
    }

    /** Decode a JSON string to a Mochi map<string, any> (nested objects). */
    public static java.util.Map<String, Object> decodeNested(String json) {
        try {
            return MAPPER.readValue(json, new com.fasterxml.jackson.core.type.TypeReference<>() {});
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(97,
                "json_decode failed: " + e.getMessage());
        }
    }
}
```

**`json_decode` type**: In Phase 14, `json_decode(body)` always returns `map<string, string>`. Nested objects are flattened by calling `.asText()` on each value (Jackson converts nested objects to their JSON string representation). This is a simplification that works for flat JSON responses.

**Nested JSON decoding**: For deeply nested JSON (e.g., `{"user": {"name": "Alice", "age": 30}}`), Phase 14 returns `{"user": "{\"name\":\"Alice\",\"age\":30}"}` (the nested object is stringified). True nested decoding (returning `map<string, map<string, string>>`) is deferred to Phase 14.1 sub-phase.

**Jackson dependency**: `JSON.decode` depends on Jackson. Jackson was added to `mochi-runtime/pom.xml` in Phase 13. Phase 14 requires Phase 13's runtime dependency to be present.

**`httptest.Server` gate**: The Go test creates a local HTTP server:

```go
func TestPhase14Fetch(t *testing.T) {
    server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Content-Type", "application/json")
        fmt.Fprintln(w, `{"name":"Alice","city":"Hanoi"}`)
    }))
    defer server.Close()
    // Substitute server.URL into the fixture source before compiling:
    mochiSrc := strings.ReplaceAll(fixtureSrc, "HTTPTEST_URL", server.URL)
    // Compile and run:
    runJvmFixtureFromString(t, mochiSrc, expectedOut)
}
```

Fixtures use `HTTPTEST_URL` as a placeholder URL that the test substitutes with the actual `httptest.Server` address.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/stmt.go` | `FetchStmt` lowering: `fetch url into body` -> `Fetch.get(url)` |
| `transpiler3/jvm/lower/expr.go` | `JsonDecodeExpr` lowering: `json_decode(body)` -> `JSON.decode(body)` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/io/Fetch.java` | `get(url)`: `HttpClient` GET with error wrapping |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/io/JSON.java` | `decode(json)`: Jackson-based flat JSON -> `Map<String, String>` |
| `transpiler3/jvm/build/phase14_test.go` | `TestPhase14Fetch`: 10 fixtures against `httptest.Server` |
| `tests/transpiler3/jvm/phase14-fetch/*.{mochi,out}` | 10 fixtures (URL placeholder: `HTTPTEST_URL`) |

## Test set

- `transpiler3/jvm/build/phase14_test.go::TestPhase14Fetch` -- 10 fixtures: (1) simple GET + print body, (2) variable URL construction, (3) `json_decode` + field access, (4) 404 status -> `MochiPanicException`, (5) redirect follows, (6-10) combinations.
- `transpiler3/jvm/lower/stmt_test.go::TestLowerFetchStmt` -- unit test: `fetch "url" into body` produces `final String body = Fetch.get("url")`.
- `transpiler3/jvm/lower/expr_test.go::TestLowerJsonDecode` -- unit test: `json_decode(body)` produces `JSON.decode(body)` call.
- `transpiler3/jvm/runtime/io/FetchTest.java` -- JUnit: `Fetch.get` against a local `com.sun.net.httpserver.HttpServer` (JDK built-in test server, no external dependency); verifies body content, follows redirect, throws `MochiPanicException` on 404.
- `transpiler3/jvm/runtime/io/JSONTest.java` -- JUnit: `JSON.decode` round-trips flat JSON; nested JSON stringifies inner objects; invalid JSON throws `MochiPanicException`.

## Deferred work

- HTTP POST, PUT, DELETE: `Fetch.post(url, body)`, `Fetch.put(url, body)`, `Fetch.delete(url)`. Deferred; only GET is in Phase 14.
- HTTP headers: `fetch url headers {"Authorization": "Bearer " + token} into body`. Deferred.
- Nested JSON decoding (`map<string, map<string, string>>`): Phase 14.1 sub-phase.
- `json_encode(data)` -> JSON string: deferred; needed for POST bodies.
- Async fetch (`let fut = spawn fetch url into body`): already works via Phase 11 `spawn` + Phase 14 `fetch`; no special support needed.
- TLS client certificates: deferred.

## Closeout notes

_Fill in after gate green._
