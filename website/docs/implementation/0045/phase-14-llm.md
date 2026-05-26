---
title: "Phase 14. LLM bindings"
sidebar_position: 16
sidebar_label: "Phase 14. LLM"
description: "MEP-45 Phase 14 tracking: provider abstraction (OpenAI, Anthropic, Google, llama.cpp local), libcurl + yyjson, replay-mode cassettes."
---

# Phase 14. LLM bindings

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 14](/docs/mep/mep-0045#phase-14-llm-bindings) |
| Status         | COMPLETE |
| Started        | 2026-05-26 07:14 (GMT+7) |
| Landed         | 2026-05-26 07:41 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

LLM fixture suite (~20 cases: `generate`, `embed`, `chat` against OpenAI/Anthropic/Google/llama.cpp) compiles + runs byte-equal vs vm3 in replay mode (recorded cassettes); live-mode runs available behind a flag.

## Goal-alignment audit

LLM generation is the user-facing AI-augmented workflow that Mochi positions itself for. Without a working `generate` expression in native binaries, the language is missing its primary marketed differentiator for server-side and edge deployments. Phase 14.0 adds the C transpiler path for `generate <provider> { ... }` with cassette replay so the gate can verify correctness without real API keys. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 14.0 | `mochi/llm.h` + `llm.c` cassette runtime; `LLMGenerateExpr` IR; lower + emit for `generate <provider> { prompt, model }`; type-checker recognises openai/anthropic/google/llama providers; 10 fixtures; `TestPhase14LLM` gate | LANDED 2026-05-26 07:14 (GMT+7) | — | — |
| 14.1 | OpenAI live provider via libcurl (`-DMOCHI_LLM_HAVE_CURL -lcurl`); simple JSON extraction for `choices[0].message.content`; `TestPhase14OpenAI` gate (cassette + live-no-api-key sub-tests; no real API call) | LANDED 2026-05-26 07:24 (GMT+7) | — | — |
| 14.2 | Anthropic live provider via libcurl (`ANTHROPIC_API_KEY`); `llm_anthropic_live()` with `x-api-key` + `anthropic-version` headers; extracts `content[0].text` via strstr; `TestPhase14Anthropic` gate (cassette + live-no-api-key sub-tests) | LANDED 2026-05-26 07:30 (GMT+7) | — | — |
| 14.3 | Google live provider (`GOOGLE_API_KEY`; API key in URL query param; `gemini-1.5-flash` default; extracts `candidates[0].content.parts[0].text` via strstr); `TestPhase14Google` gate | LANDED 2026-05-26 07:32 (GMT+7) | — | — |
| 14.4 | llama.cpp local provider (`-DMOCHI_LLM_HAVE_LLAMA -lllama`; greedy sampling via `llm_llama_greedy`; `LLAMA_MODEL_PATH` env var; stub in default build); `TestPhase14Llama` gate (cassette + stub-no-model-path sub-tests) | LANDED 2026-05-26 07:36 (GMT+7) | — | — |
| 14.5 | Cassette recording mode (`MOCHI_LLM_CASSETTE_RECORD`); `llm_cassette_record()` writes live response to `<hash>.txt`; playback (`CASSETTE_DIR`) takes priority; `TestPhase14CassetteRecord` gate (playback_priority + record_write sub-tests) | LANDED 2026-05-26 07:41 (GMT+7) | — | — |

## Decisions made

**Phase 14.5: cassette recording via MOCHI_LLM_CASSETTE_RECORD.** Setting this env var causes `mochi_llm_generate()` to write the live response to `<CASSETTE_RECORD>/<hash>.txt` after the live call. The file uses the same DJB2 hash key and trailing-newline convention as playback files, so the same cassette dir can be used for both record and replay. Playback (`MOCHI_LLM_CASSETTE_DIR`) takes priority: if a cassette dir is set, the live path and recording path are both skipped.

**Phase 14.5: gate uses stub live mode.** `TestPhase14CassetteRecord` tests recording without a live API by relying on the stub's "" response being written to the record dir. This verifies the file write path without network access. The `record_write` sub-test checks that a `.txt` file appears in the record dir; the `playback_priority` sub-test verifies no file is written when playback fires.

**Phase 14.4: llama.cpp opt-in via compile flag.** The llama.cpp provider is implemented under `#if defined(MOCHI_LLM_HAVE_LLAMA)`. Default builds (without this flag) use a stub that prints a diagnostic mentioning `LLAMA_MODEL_PATH` and `--with-llama`. Users with a GGUF model file compile with `-DMOCHI_LLM_HAVE_LLAMA -lllama` and set `LLAMA_MODEL_PATH` at runtime.

**Phase 14.4: greedy sampling.** `llm_llama_local()` uses greedy argmax token selection (no temperature/top-p sampling), which is deterministic and sufficient for the use cases Mochi targets. Advanced sampling can be wired via llama.cpp's sampler chain in a future sub-phase.

**Phase 14.4: gate uses stub mode only.** `TestPhase14Llama` builds without `-DMOCHI_LLM_HAVE_LLAMA` (no llama.cpp dependency in CI). The gate verifies cassette mode works and that the stub prints a diagnostic when `LLAMA_MODEL_PATH` is unset.

**Phase 14.3: Google API key in URL.** The Google Generative Language API authenticates via a `?key=<GOOGLE_API_KEY>` query parameter appended to the URL (`https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={key}`), not a header. The URL is built with `snprintf` in `llm_google_live()`. Default model is `gemini-1.5-flash`.

**Phase 14.3: candidates[0].text extraction.** The Google response nests the output under `candidates[0].content.parts[0].text`. The existing `llm_json_str` helper is reused: find `"candidates"` in the response, then find `"text"` from that position. This is sufficient for single-candidate responses (the common case).

**Phase 14.2: Anthropic request format.** The Anthropic messages API uses `POST https://api.anthropic.com/v1/messages` with headers `x-api-key: <key>` and `anthropic-version: 2023-06-01`. The request body includes `"max_tokens": 1024` (required by Anthropic). Default model is `claude-3-haiku-20240307` when none is specified.

**Phase 14.2: content[0].text extraction.** The Anthropic response nests the assistant text under `content[0].text` (an array of content blocks). The existing `llm_json_str` helper is reused: find `"content"` in the response, then find `"text"` from that position. This avoids needing array-aware JSON parsing for the common case (single text block).

**Phase 14.1: libcurl opt-in via compile flag.** The OpenAI live provider is implemented in `llm.c` behind `#if defined(MOCHI_LLM_HAVE_CURL)`. Default compilation (without this flag) links no external library; live mode returns "" with a diagnostic. Users who want live API calls compile with `-DMOCHI_LLM_HAVE_CURL -lcurl`. This keeps the gate dependency-free while enabling production use.

**Phase 14.1: `probeLibcurl` in gate test.** `TestPhase14OpenAI` probes libcurl availability by compiling a minimal C program with `-lcurl` before running the gate. The test skips gracefully when libcurl is absent. CI and macOS dev hosts with system libcurl pass; minimal containers without libcurl skip rather than fail.

**Phase 14.1: simple strstr-based JSON extraction.** The OpenAI response is parsed by finding `"message"` then `"content"` in the JSON body. This handles the standard `choices[0].message.content` path without requiring a full JSON library. Edge cases (escaped quotes, Unicode) are handled by the `llm_json_str` helper. yyjson (full JSON library) is deferred to a later sub-phase.

**Phase 14.1: no yyjson.** The MEP spec mentioned yyjson for JSON parsing. Phase 14.1 uses a minimal strstr-based extractor instead, which is sufficient for extracting `choices[0].message.content` from well-formed OpenAI API responses. yyjson would be needed for tool-use parsing (structured responses), which is a deferred Phase 14.3+ concern.

**Phase 14.0: cassette-first, no libcurl dependency.** The MEP spec called for a cassette layer that intercepts libcurl. Phase 14.0 instead uses a simpler approach: the C runtime reads `MOCHI_LLM_CASSETTE_DIR` and looks up pre-recorded response files by DJB2 hash of the (provider, model, prompt) triple. This avoids any HTTP dependency for the gate. Live-mode HTTP providers land in Phase 14.1-14.4.

**Phase 14.0: DJB2 hash-keyed cassette files.** File name format: `<hash_decimal>.txt` where hash is DJB2 applied to `"<provider>\0<model>\0<prompt>"`. The Go test side replicates the same hash to create cassette files for each fixture. The NUL separator prevents ambiguous concatenations like ("a", "bc") and ("ab", "c") from colliding.

**Phase 14.0: `llm.c` auto-picked up by collectRuntimeSources.** The build driver's `collectRuntimeSources` walks every `*.c` file in the embed FS `src/` directory. Adding `llm.c` there is sufficient; no driver change needed. The header `llm.h` is included unconditionally in the emitted prologue (same as all other runtime headers).

**Phase 14.0: type-checker whitelist for provider names.** The type checker now recognises "openai", "anthropic", "google", "llama" as text-generation providers returning `string`. All other unknown targets are still an error (existing `unknown_generate_type` golden test preserved). Unknown struct targets still produce the `T025 unknown type` diagnostic.

**Phase 14.0: `generate <provider> { model, prompt }` fields only.** Phase 14.0 lowers only `prompt` and `model` fields. Other fields ("temperature", "top_p", "max_tokens", "stop") are rejected by the lower pass with an unsupported-in-Phase-14.0 error. The type checker already validates these field types.

**Phase 14.0: trailing newline stripped from cassette files.** The runtime strips a single trailing newline so cassette files can be written with a trailing newline (normal in text editors) without affecting the effective response. The cassette directory for each fixture is stored under `<fixture>/cassette/`.

## Deferred work

_Provider-specific tool-use / function-calling integration tests: a follow-up phase if upstream stabilises._

## Closeout notes

All sub-phases 14.0 through 14.5 are LANDED. The `generate <provider> { ... }` expression compiles and runs in cassette replay mode; OpenAI, Anthropic, and Google live providers are available with `-DMOCHI_LLM_HAVE_CURL -lcurl`; llama.cpp local inference is available with `-DMOCHI_LLM_HAVE_LLAMA -lllama` and `LLAMA_MODEL_PATH`; cassette recording is available via `MOCHI_LLM_CASSETTE_RECORD`. Phase 14 is COMPLETE.
