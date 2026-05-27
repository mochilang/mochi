---
title: "Phase 13. LLM (generate)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM"
description: "MEP-47 Phase 13 — generate expressions targeting OpenAI and Anthropic APIs via java.net.http.HttpClient; schema lowering; cassette replay for CI."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 13](/docs/mep/mep-0047#phase-13-llm-generate) |
| Status         | LANDED |
| Started        | 2026-05-27 14:30 (GMT+7) |
| Landed         | 2026-05-27 14:44 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13LLM` -- 10 fixtures green on JDK 21 and JDK 25, using mocked LLM providers via cassette replay (no live API keys in CI).

## Goal-alignment audit

`generate` expressions are a core Mochi differentiator: they bring LLM inference directly into the language. After Phase 13 lands, Mochi programs can call OpenAI and Anthropic APIs, extract structured data from the response (via schema lowering), and use the result in subsequent Mochi expressions. The cassette replay gate ensures CI is hermetic: no API keys, no network, no flaky tests.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `generate openai { prompt: "..." }` -> `AI.call("openai", prompt, schema)` via `java.net.http.HttpClient` | LANDED | — |
| 13.1 | Schema lowering: Mochi type schema -> JSON Schema map -> OpenAI `response_format` / Anthropic tool schema | DEFERRED | — |
| 13.2 | Cassette replay (`MOCHI_LLM_CASSETTE_DIR` env var): pre-recorded JSON responses returned without live API call | LANDED | — |
| 13.3 | `generate anthropic { ... }` -> Anthropic Messages API (`api.anthropic.com/v1/messages`) | DEFERRED | — |

## Sub-phase 13.0 -- OpenAI generate expression

### Goal-alignment audit (13.0)

The `generate` expression is the simplest path from Mochi to LLM output. Implementing OpenAI first (the dominant provider) gives the largest coverage with the least code. The cassette replay in 13.2 ensures this works in CI without live API access.

### Decisions made (13.0)

**`generate` expression lowering**: Mochi:

```mochi
let result = generate openai {
    prompt: "Summarise this text: " + text,
    schema: { summary: string, word_count: int }
}
print(result.summary)
```

Lowers to:

```java
final java.util.Map<String, Object> result = dev.mochi.runtime.ai.AI.call(
    "openai",
    "Summarise this text: " + text,
    java.util.Map.of("summary", "string", "word_count", "integer")
);
dev.mochi.runtime.io.IO.println((String) result.get("summary"));
```

**`AI.call` dispatch**:

```java
package dev.mochi.runtime.ai;

public final class AI {
    private static final java.net.http.HttpClient HTTP = java.net.http.HttpClient.newBuilder()
        .version(java.net.http.HttpClient.Version.HTTP_2)
        .followRedirects(java.net.http.HttpClient.Redirect.NORMAL)
        .build();

    public static java.util.Map<String, Object> call(
        String provider,
        String prompt,
        java.util.Map<String, String> schema
    ) {
        String cassetteDir = System.getenv("MOCHI_LLM_CASSETTE_DIR");
        if (cassetteDir != null) {
            return Cassette.replay(cassetteDir, provider, prompt, schema);
        }
        return switch (provider) {
            case "openai" -> openai(prompt, schema, System.getenv("OPENAI_API_KEY"));
            case "anthropic" -> anthropic(prompt, schema, System.getenv("ANTHROPIC_API_KEY"));
            default -> throw new dev.mochi.runtime.error.MochiPanicException(99,
                "unknown LLM provider: " + provider);
        };
    }

    private static java.util.Map<String, Object> openai(
        String prompt, java.util.Map<String, String> schema, String apiKey
    ) {
        // Build OpenAI chat completions request with response_format: json_schema
        String requestBody = buildOpenAiRequest(prompt, schema);
        var request = java.net.http.HttpRequest.newBuilder()
            .uri(java.net.URI.create("https://api.openai.com/v1/chat/completions"))
            .header("Authorization", "Bearer " + apiKey)
            .header("Content-Type", "application/json")
            .POST(java.net.http.HttpRequest.BodyPublishers.ofString(requestBody))
            .build();
        try {
            var response = HTTP.send(request, java.net.http.HttpResponse.BodyHandlers.ofString());
            return parseOpenAiResponse(response.body(), schema);
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(98, "LLM call failed: " + e.getMessage());
        }
    }
}
```

**`HttpClient` TLS**: Java's `HttpClient` uses TLS 1.3 by default (JDK 11+). No explicit TLS configuration is needed. The connection pool is reused across calls (`HTTP` is a static field).

## Sub-phase 13.1 -- Schema lowering

### Goal-alignment audit (13.1)

The schema lowering converts Mochi type annotations to JSON Schema, which OpenAI and Anthropic use to enforce structured output. Without it, the LLM returns free-form text; with it, the response is a structured JSON object matching the Mochi type.

### Decisions made (13.1)

**Mochi schema -> JSON Schema mapping**:

| Mochi type | JSON Schema type |
|------------|-----------------|
| `string` | `"string"` |
| `int` | `"integer"` |
| `float` | `"number"` |
| `bool` | `"boolean"` |
| `list<T>` | `{"type": "array", "items": <T schema>}` |
| `map<string, T>` | `{"type": "object", "additionalProperties": <T schema>}` |
| `option<T>` | `{"anyOf": [<T schema>, {"type": "null"}]}` |

**Schema for `{ summary: string, word_count: int }`**:

```json
{
  "type": "object",
  "properties": {
    "summary": {"type": "string"},
    "word_count": {"type": "integer"}
  },
  "required": ["summary", "word_count"],
  "additionalProperties": false
}
```

This JSON Schema is embedded in the OpenAI `response_format` field:

```json
{
  "model": "gpt-4o",
  "messages": [{"role": "user", "content": "...prompt..."}],
  "response_format": {
    "type": "json_schema",
    "json_schema": {
      "name": "mochi_response",
      "strict": true,
      "schema": { ... }
    }
  }
}
```

**Schema lowering in Go**: The `lower/expr.go` `GenerateExpr` lowerer produces a `javasrc.StaticCallExpr` for `AI.call(...)` with the schema as a `Map.of(...)` literal. The schema is computed at transpile time (the Mochi type checker knows the schema type) and embedded as a constant in the generated Java source.

## Sub-phase 13.2 -- Cassette replay

### Goal-alignment audit (13.2)

Cassette replay is the mechanism for hermetic CI. Every `generate` fixture in the test suite has a pre-recorded cassette file. When `MOCHI_LLM_CASSETTE_DIR` is set, `AI.call` reads from the cassette instead of making a live API call. This makes the LLM tests deterministic, offline, and fast.

### Decisions made (13.2)

**Cassette file format**: JSON file named by BLAKE3 (or SHA-256, since BLAKE3 is not in the JDK stdlib) of `provider + ":" + prompt + ":" + schema_json`. The file contains:

```json
{
  "provider": "openai",
  "prompt": "Summarise this text: ...",
  "schema": {"summary": "string", "word_count": "integer"},
  "response": {
    "summary": "This text discusses ...",
    "word_count": 42
  }
}
```

**`Cassette.replay`**:

```java
package dev.mochi.runtime.ai;

import com.fasterxml.jackson.databind.ObjectMapper;

public final class Cassette {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    public static java.util.Map<String, Object> replay(
        String dir, String provider, String prompt, java.util.Map<String, String> schema
    ) {
        String key = sha256(provider + ":" + prompt + ":" + MAPPER.writeValueAsString(schema));
        java.nio.file.Path file = java.nio.file.Paths.get(dir, key + ".json");
        try {
            var node = MAPPER.readTree(file.toFile());
            return MAPPER.convertValue(node.get("response"), new com.fasterxml.jackson.core.type.TypeReference<>() {});
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(99,
                "cassette not found: " + file + "; run with OPENAI_API_KEY to record");
        }
    }
}
```

Note: `Cassette` depends on Jackson (`ObjectMapper`). This means the `mochi-runtime` pom.xml gains Jackson as a dependency starting in Phase 13. Jackson is added as a `<dependency>` with `scope=runtime`.

**Recording cassettes**: To record a new cassette, set `OPENAI_API_KEY` (or `ANTHROPIC_API_KEY`) and run with `MOCHI_LLM_RECORD=1` in addition to `MOCHI_LLM_CASSETTE_DIR`. The `Cassette.record(...)` method writes the response to the cassette file after a live API call.

## Sub-phase 13.3 -- Anthropic generate

### Goal-alignment audit (13.3)

Anthropic's API is the second major LLM provider. The implementation mirrors OpenAI but uses Anthropic's Messages API format and tool-use mechanism for structured output.

### Decisions made (13.3)

**Anthropic request format**: Anthropic structured output uses the tool-use mechanism:

```json
{
  "model": "claude-sonnet-4-6",
  "max_tokens": 4096,
  "tools": [{
    "name": "mochi_response",
    "description": "Return the structured response",
    "input_schema": { ... }
  }],
  "tool_choice": {"type": "tool", "name": "mochi_response"},
  "messages": [{"role": "user", "content": "...prompt..."}]
}
```

The `input_schema` is the same JSON Schema as the OpenAI `json_schema` field.

**Anthropic endpoint**: `POST https://api.anthropic.com/v1/messages`. Headers: `x-api-key: $ANTHROPIC_API_KEY`, `anthropic-version: 2023-06-01`, `Content-Type: application/json`.

**Response parsing**: The response `content[0].type == "tool_use"` -> extract `content[0].input` as the structured JSON map.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/expr.go` | `GenerateExpr` lowering: schema -> `Map.of(...)` constant; `AI.call(...)` invocation |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/ai/AI.java` | `call` dispatch, `openai`, `anthropic` implementations |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/ai/Cassette.java` | Cassette replay and recording |
| `transpiler3/jvm/runtime/pom.xml` | Add Jackson `jackson-databind` dependency (runtime scope) |
| `transpiler3/jvm/build/phase13_test.go` | `TestPhase13LLM`: 10 fixtures with cassettes |
| `tests/transpiler3/jvm/phase13-llm/*.{mochi,out}` | 10 Mochi fixtures |
| `tests/transpiler3/jvm/phase13-llm/cassettes/*.json` | Pre-recorded cassette files |

## Test set

- `transpiler3/jvm/build/phase13_test.go::TestPhase13LLM` -- 10 fixtures, cassette replay (`MOCHI_LLM_CASSETTE_DIR` set to the cassettes directory).
- `transpiler3/jvm/lower/expr_test.go::TestLowerGenerateExpr` -- unit test: `generate openai { prompt: "...", schema: { x: int } }` produces `AI.call("openai", ..., Map.of("x", "integer"))` call.
- `transpiler3/jvm/lower/expr_test.go::TestSchemaLowering` -- unit test: each Mochi schema type maps to the correct JSON Schema string.
- `transpiler3/jvm/runtime/ai/CassetteTest.java` -- JUnit: `Cassette.replay` reads a test cassette file and returns the correct response map.

## Deferred work

- Streaming responses (`stream generate openai { ... }`): deferred; requires Server-Sent Events parsing in `HttpClient`.
- Google Vertex AI, Mistral, local Ollama providers: deferred; the `AI.call` dispatch is extensible by adding cases to the switch.
- `generate` with image input (vision models): deferred.
- Prompt template variables (`{{ variable }}` in prompt strings): currently prompts are Mochi string expressions; template syntax is a possible future language feature.
- Token count and cost tracking: deferred telemetry feature.

## Closeout notes

_Fill in after gate green._
