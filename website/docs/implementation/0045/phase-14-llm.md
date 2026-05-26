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
| Status         | IN PROGRESS |
| Started        | 2026-05-26 07:14 (GMT+7) |
| Landed         | — |
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
| 14.1 | OpenAI provider (libcurl + yyjson)                                                                                 | NOT STARTED | —      | — |
| 14.2 | Anthropic provider                                                                                                 | NOT STARTED | —      | — |
| 14.3 | Google provider                                                                                                    | NOT STARTED | —      | — |
| 14.4 | llama.cpp local provider (linked only with `--with-llama`)                                                         | NOT STARTED | —      | — |
| 14.5 | Live HTTP providers via libcurl + yyjson; cassette recording mode                                                  | NOT STARTED | —      | — |

## Decisions made

**Phase 14.0: cassette-first, no libcurl dependency.** The MEP spec called for a cassette layer that intercepts libcurl. Phase 14.0 instead uses a simpler approach: the C runtime reads `MOCHI_LLM_CASSETTE_DIR` and looks up pre-recorded response files by DJB2 hash of the (provider, model, prompt) triple. This avoids any HTTP dependency for the gate. Live-mode HTTP providers land in Phase 14.1-14.4.

**Phase 14.0: DJB2 hash-keyed cassette files.** File name format: `<hash_decimal>.txt` where hash is DJB2 applied to `"<provider>\0<model>\0<prompt>"`. The Go test side replicates the same hash to create cassette files for each fixture. The NUL separator prevents ambiguous concatenations like ("a", "bc") and ("ab", "c") from colliding.

**Phase 14.0: `llm.c` auto-picked up by collectRuntimeSources.** The build driver's `collectRuntimeSources` walks every `*.c` file in the embed FS `src/` directory. Adding `llm.c` there is sufficient; no driver change needed. The header `llm.h` is included unconditionally in the emitted prologue (same as all other runtime headers).

**Phase 14.0: type-checker whitelist for provider names.** The type checker now recognises "openai", "anthropic", "google", "llama" as text-generation providers returning `string`. All other unknown targets are still an error (existing `unknown_generate_type` golden test preserved). Unknown struct targets still produce the `T025 unknown type` diagnostic.

**Phase 14.0: `generate <provider> { model, prompt }` fields only.** Phase 14.0 lowers only `prompt` and `model` fields. Other fields ("temperature", "top_p", "max_tokens", "stop") are rejected by the lower pass with an unsupported-in-Phase-14.0 error. The type checker already validates these field types.

**Phase 14.0: trailing newline stripped from cassette files.** The runtime strips a single trailing newline so cassette files can be written with a trailing newline (normal in text editors) without affecting the effective response. The cassette directory for each fixture is stored under `<fixture>/cassette/`.

## Deferred work

_Provider-specific tool-use / function-calling integration tests: a follow-up phase if upstream stabilises._

## Closeout notes

Sub-phase 14.0 is LANDED. The `generate <provider> { ... }` expression now compiles and runs in cassette replay mode with 10 fixtures covering openai, anthropic, and google providers, model fields, variable results, string concatenation, multiple calls, and function-scoped generation. Live HTTP providers (14.1-14.5) remain NOT STARTED.
