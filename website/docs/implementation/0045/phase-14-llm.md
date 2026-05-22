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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

LLM fixture suite (~20 cases: `generate`, `embed`, `chat` against OpenAI/Anthropic/Google/llama.cpp) compiles + runs byte-equal vs vm3 in replay mode (recorded cassettes); live-mode runs available behind a flag.

## Goal-alignment audit

_To be written before sub-phase 14.0 starts. LLM surface is the AI-augmented workflow Mochi positions itself for; without it, the language is missing a marketed feature in native builds. Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 14.0 | Provider abstraction in `transpiler3/c/runtime/src/llm/`: trait-like vtable per provider                           | NOT STARTED | —      | — |
| 14.1 | OpenAI provider (libcurl + yyjson)                                                                                 | NOT STARTED | —      | — |
| 14.2 | Anthropic provider                                                                                                 | NOT STARTED | —      | — |
| 14.3 | Google provider                                                                                                    | NOT STARTED | —      | — |
| 14.4 | llama.cpp local provider (linked only with `--with-llama`)                                                         | NOT STARTED | —      | — |
| 14.5 | Replay-mode cassettes under `tests/transpiler3/c/cassettes/llm/`; cassette layer intercepts libcurl                | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Provider-specific tool-use / function-calling integration tests: a follow-up phase if upstream stabilises._

## Closeout notes

_Fill in after gate green._
