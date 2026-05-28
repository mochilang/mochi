---
title: "Phase 13. LLM (generate)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM"
description: "MEP-48 Phase 13 — LLMGenerateExpr to Mochi.Runtime.Llm.Ai.Call; MOCHI_LLM_CASSETTE_DIR cassette playback; 2 fixtures."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 13](/docs/mep/mep-0048#phase-13-llm-generate) |
| Status         | LANDED |
| Started        | 2026-05-28 04:28 (GMT+7) |
| Landed         | 2026-05-28 05:37 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13LLM`: 2 fixtures green with cassette playback via `MOCHI_LLM_CASSETTE_DIR` (generate_hello, generate_concat). No live network in CI. OpenAI/Anthropic/Ollama provider abstractions and full async colouring are deferred.

## Goal-alignment audit

`ai(...)` is Mochi's built-in LLM call surface. On .NET, it lowers to `Mochi.Runtime.Llm.Ai.CallAsync` which dispatches to the configured provider (OpenAI, Anthropic, or Ollama). Phase 13 uses cassette playback for deterministic tests, matching the BEAM and JVM target strategies.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `ai(prompt)` → `await Ai.CallAsync(prompt, ct)` + provider dispatch | NOT STARTED | — |
| 13.1 | OpenAI provider: `HttpClient` + `System.Text.Json` JSON serialisation | NOT STARTED | — |
| 13.2 | Anthropic provider | NOT STARTED | — |
| 13.3 | Local (Ollama) provider via `http://localhost:11434` | NOT STARTED | — |
| 13.4 | Cassette playback: `MOCHI_LLM_CASSETTE` env var; record/replay JSON responses | NOT STARTED | — |

## Sub-phase 13.0 -- Lowering ai(...)

### Decisions made (13.0)

**`ai("translate to French: " + text)`** lowers to:

```csharp
string result = await Mochi.Runtime.Llm.Ai.CallAsync(
    "translate to French: " + text, ct).ConfigureAwait(false);
```

`Ai.CallAsync` returns `Task<string>`. The colour pass marks any function containing an `ai(...)` call as Red (async).

**Provider selection**: via `MOCHI_LLM_PROVIDER` env var (`openai`, `anthropic`, `ollama`). Default: `openai` if `OPENAI_API_KEY` is set; `ollama` otherwise.

## Sub-phase 13.4 -- Cassette playback

### Decisions made (13.4)

**`MOCHI_LLM_CASSETTE=path/to/cassette.json`**: when set, `Ai.CallAsync` reads the cassette file instead of making HTTP calls. Cassette format: JSON array of `{ "prompt": "...", "response": "..." }` objects. Matched by prompt string equality.

**Recording**: `MOCHI_LLM_RECORD=1` records live responses to a cassette file for later playback.

**Test fixtures**: both Phase 13 fixtures ship with pre-recorded cassettes. CI always runs in playback mode (`MOCHI_LLM_CASSETTE_DIR` set to the fixture's `cassette/` subdirectory, no network). The cassette key is SHA-256 of `provider + ":" + prompt`; the response is read from `<dir>/<hash>.txt`. This matches the JVM cassette format, so JVM cassette files are reusable. Live tests run only with `MOCHI_TEST_LLM_LIVE=1`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/expr.go` | `ai(...)` → `Ai.CallAsync(...)` |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/Ai.cs` | Provider dispatch + cassette playback |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/OpenAiProvider.cs` | OpenAI REST API client |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/AnthropicProvider.cs` | Anthropic Messages API client |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/OllamaProvider.cs` | Ollama local API client |
| `transpiler3/dotnet/build/phase13_test.go` | `TestPhase13LLM`: 2 fixtures with cassettes |
| `tests/transpiler3/dotnet/fixtures/phase13-llm/` | 2 fixture directories with cassette files (generate_hello, generate_concat) |

## Test set

- `TestPhase13LLM` -- 2 fixtures: generate_hello, generate_concat.

## Deferred work

- Structured output (JSON schema validation of LLM response). Deferred to Phase 3 sub-MEP.
- Streaming LLM responses via `IAsyncEnumerable<string>` (SSE). Deferred pending demand.
- Microsoft Semantic Kernel integration. Deferred pending demand.

## Closeout notes

Phase 13 landed. `TestPhase13LLM` PASS: 2/2 fixtures on net10.0 (generate_hello, generate_concat).

`LLMGenerateExpr` → `Mochi.Runtime.Llm.Ai.Call(provider, prompt)`. The `Ai` class checks `MOCHI_LLM_CASSETTE_DIR` env var; if set, computes SHA-256 of `provider + ":" + prompt` and reads the pre-recorded response from `<dir>/<hash>.txt`. This matches the JVM cassette format, so JVM cassette files are reusable. The test driver sets the env var for each fixture that has a `cassette/` subdirectory.
