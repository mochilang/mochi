---
title: "Phase 13. LLM (generate)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM"
description: "MEP-48 Phase 13 — ai(...) to Mochi.Runtime.Llm.Ai.CallAsync; OpenAI/Anthropic/Ollama provider abstractions; cassette playback; 10 fixtures."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 13](/docs/mep/mep-0048#phase-13-llm-generate) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13LLM`: 10 fixtures green with mocked providers (cassette playback, no live network). Async-clean: all LLM calls are `async Task<string>` and colour-propagated correctly.

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

**Test fixtures**: all 10 Phase 13 fixtures ship with pre-recorded cassettes. CI always runs in playback mode (`MOCHI_LLM_CASSETTE` set, no network). Live tests run only with `MOCHI_TEST_LLM_LIVE=1`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/expr.go` | `ai(...)` → `Ai.CallAsync(...)` |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/Ai.cs` | Provider dispatch + cassette playback |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/OpenAiProvider.cs` | OpenAI REST API client |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/AnthropicProvider.cs` | Anthropic Messages API client |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Llm/OllamaProvider.cs` | Ollama local API client |
| `transpiler3/dotnet/build/phase13_test.go` | `TestPhase13LLM`: 10 fixtures with cassettes |
| `tests/transpiler3/dotnet/fixtures/phase13-llm/` | 10 fixture directories with cassette files |

## Test set

- `TestPhase13LLM` -- 10 fixtures: simple generate, translate prompt, summarise prompt, code generation, multi-turn (context in prompt), generate with Option<string> result, generate in loop (5 prompts), generate with error (cassette returns error JSON), generate with Anthropic provider, generate with Ollama provider.

## Deferred work

- Structured output (JSON schema validation of LLM response). Deferred to Phase 3 sub-MEP.
- Streaming LLM responses via `IAsyncEnumerable<string>` (SSE). Deferred pending demand.
- Microsoft Semantic Kernel integration. Deferred pending demand.

## Closeout notes

Phase 13 not yet started.
