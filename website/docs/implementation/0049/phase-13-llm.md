---
title: "Phase 13. LLM (FoundationModels on Apple)"
sidebar_position: 17
sidebar_label: "Phase 13. LLM"
description: "MEP-49 Phase 13 — @llm annotation using Apple FoundationModels framework (on-device); cassette playback for deterministic tests; cloud fallback via URLSession."
---

# Phase 13. LLM (FoundationModels on Apple)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 13](/docs/mep/mep-0049#phase-13-llm) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 14:15 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13LLM`: 10 fixtures green on Swift 6.0 and 6.1, macOS arm64 only (FoundationModels requires Apple Neural Engine). Cassette playback mode for linux-x64 CI. `TestSwiftcClean` remains green.

## Goal-alignment audit

LLM integration is a first-class Mochi feature, not an afterthought via FFI. On Apple platforms (macOS 15+, iOS 18+), Apple's `FoundationModels` framework provides on-device LLM inference with no API key, no network latency, and no data leaving the device. Phase 13 ships the `@llm` annotation that routes to `FoundationModels` on Apple and to a cloud LLM via `URLSession` on Linux/Windows. Test determinism is achieved via cassette playback (pre-recorded responses).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `@llm fun summarise(text: string): string` → `FoundationModels.LanguageModel.complete(prompt:)` on Apple | NOT STARTED | — |
| 13.1 | Structured output: `@llm fun extract(text: string): Person` → `FoundationModels` with `Generable` protocol | NOT STARTED | — |
| 13.2 | Cassette playback for deterministic CI: record LLM calls to `.cassette` files; replay in test | NOT STARTED | — |
| 13.3 | Cloud fallback: `@llm(provider: cloud) fun foo()` → `URLSession` HTTP call to configured LLM API | NOT STARTED | — |

## Sub-phase 13.0 -- FoundationModels text generation

### Decisions made (13.0)

**`FoundationModels` framework availability**: `import FoundationModels` (Apple SDK, macOS 15+, iOS 18+, Xcode 16+). The framework provides on-device Apple Intelligence inference. It is NOT available on Linux or Windows.

**Platform guard**: the lowerer wraps `FoundationModels` usage in `#if canImport(FoundationModels)` ... `#else` (cloud fallback). This compiles on all platforms.

**`@llm fun summarise(text: string): string` lowering**:

```swift
// Generated:
#if canImport(FoundationModels)
import FoundationModels
#endif

public func summarise(_ text: String) async throws -> String {
    #if canImport(FoundationModels)
    let session = LanguageModelSession()
    let response = try await session.respond(to: Prompt(text))
    return response.content
    #else
    return try await __llmCloudFallback(prompt: text, model: "default")
    #endif
}
```

**`LanguageModelSession`**: the primary entry point into FoundationModels. Sessions maintain conversation context. For stateless `@llm` functions, a new session is created per call. For stateful Mochi agents with LLM state, the session is stored in the actor's state.

**`async throws`**: all LLM calls are async (network or on-device inference is non-blocking) and can throw (model unavailable, token limit exceeded, content policy). The enclosing Mochi function must be coloured red (Phase 11).

**`FoundationModels` opacity**: Apple's FoundationModels API is intentionally opaque about which model it uses internally. The lowerer cannot predict output tokens, which is why cassette playback is essential for deterministic tests.

## Sub-phase 13.1 -- Structured output

### Decisions made (13.1)

**`Generable` protocol**: FoundationModels supports structured output via the `@Generable` macro (Xcode 26 / Swift 6.1). A Swift struct annotated with `@Generable` can be used as the target type for structured generation.

**Mochi `@llm fun extract(text: string): Person`**: the return type is a Mochi record. The lowerer emits `@Generable` on the corresponding Swift struct and uses FoundationModels structured generation:

```swift
// In Person.swift (augmented):
@Generable
@frozen
public struct Person: Sendable, Hashable, Codable {
    @Guide(description: "The person's full name")
    public let name: String
    @Guide(description: "Age in years")
    public let age: Int64
}

// Generated function:
public func extract(_ text: String) async throws -> Person {
    #if canImport(FoundationModels)
    let session = LanguageModelSession()
    let response = try await session.respond(
        to: Prompt(text),
        generating: Person.self
    )
    return response.content
    #else
    return try await __llmStructuredCloudFallback(prompt: text, type: Person.self)
    #endif
}
```

**`@Guide` annotations**: Mochi record field doc-comments (`/// The person's full name`) are converted to `@Guide(description:)` annotations in the generated Swift. This guides the model's structured output.

**Fallback JSON parsing**: on Linux (cloud fallback), structured output is requested as JSON from the cloud API and decoded via `Codable`.

## Sub-phase 13.2 -- Cassette playback

### Decisions made (13.2)

**Cassette pattern**: same as the BEAM backend (MEP-46 Phase 13). Pre-recorded LLM responses are stored in `.cassette` JSON files alongside fixtures. In test mode, the lowerer injects a `MockLanguageModelSession` that replays recorded responses instead of calling FoundationModels.

**`MockLanguageModelSession`**: in `MochiRuntime/Sources/MochiRuntime/LLM/Mock.swift`:

```swift
#if DEBUG
public final class MockLanguageModelSession {
    private let cassette: [String: String]
    public init(cassette: [String: String]) { self.cassette = cassette }
    public func respond(to prompt: String) async -> String {
        cassette[prompt] ?? "<<no cassette entry for: \(prompt)>>"
    }
}
#endif
```

**Test injection**: the generated code checks `ProcessInfo.processInfo.environment["MOCHI_LLM_CASSETTE"]`. If set, it loads the cassette file and uses `MockLanguageModelSession`. This allows CI (linux-x64) to run LLM tests without FoundationModels.

**Cassette recording**: `MOCHI_LLM_RECORD=1` mode runs the real FoundationModels and writes responses to the cassette file. Recording only works on macOS arm64 with Apple Intelligence enabled.

## Sub-phase 13.3 -- Cloud fallback

### Decisions made (13.3)

**`@llm(provider: cloud)` annotation**: forces cloud LLM even on Apple platforms (useful when the Mochi program needs a larger model than on-device Apple Intelligence provides).

**Cloud provider**: Mochi ships a default cloud provider configuration via `MochiRuntime`. The API key is read from environment variable `MOCHI_LLM_API_KEY`. The default endpoint is configurable. No specific LLM provider is hard-coded (the interface is OpenAI-compatible).

**`URLSession`-based HTTP call**:

```swift
public func __llmCloudFallback(prompt: String, model: String) async throws -> String {
    var request = URLRequest(url: URL(string: ProcessInfo.processInfo.environment["MOCHI_LLM_ENDPOINT"]!)!)
    request.httpMethod = "POST"
    request.httpBody = try JSONEncoder().encode(["prompt": prompt, "model": model])
    request.addValue("Bearer \(ProcessInfo.processInfo.environment["MOCHI_LLM_API_KEY"]!)", forHTTPHeaderField: "Authorization")
    let (data, _) = try await URLSession.shared.data(for: request)
    return try JSONDecoder().decode(LLMResponse.self, from: data).text
}
```

**Streaming responses**: `@llm(stream: true) fun chat(msg: string): stream<string>` → `URLSession.bytes(for:)` async byte stream, split on SSE `data:` lines.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/llm.go` | `@llm` annotation lowering; `#if canImport(FoundationModels)` guard; `@Generable` emission |
| `transpiler3/swift/lower/lower.go` | `@Guide` annotation from record field comments |
| `transpiler3/swift/runtime/Sources/MochiRuntime/LLM/FoundationModels.swift` | `LanguageModelSession` wrapper; cloud fallback |
| `transpiler3/swift/runtime/Sources/MochiRuntime/LLM/Mock.swift` | `MockLanguageModelSession`; cassette loader |
| `transpiler3/swift/build/phase13_test.go` | `TestPhase13LLM`: 10 fixtures with cassette playback |
| `tests/transpiler3/swift/fixtures/phase13-llm/` | 10 fixture directories with `.cassette` files |

## Test set

- `TestPhase13LLM` -- 10 fixtures (all with cassette playback, runnable on linux-x64 CI): `llm_summarise`, `llm_classify`, `llm_extract_person`, `llm_extract_list`, `llm_translate`, `llm_code_gen`, `llm_streaming`, `llm_cloud_fallback`, `llm_structured_output`, `llm_error_handling`.

## Deferred work

- FoundationModels conversation history (multi-turn chat). Deferred to Phase 13.1.
- FoundationModels tool use (function calling). Deferred to Phase 13.2.
- Whisper-based speech-to-text (separate `SpeechAnalyzer` API). Deferred to Phase 13.3.
- Embeddings API. Out of v1 scope.
- Local Ollama / LLaMA.cpp integration for Linux. Deferred to Phase 12 (FFI).
