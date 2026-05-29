---
title: "Phase 13. LLM"
sidebar_position: 14
sidebar_label: "Phase 13. LLM"
description: "MEP-52 Phase 13, Mochi `generate <provider> { ... }` as cassette-replay (SHA-256(provider:prompt) keys a per-fixture .txt response under MOCHI_LLM_CASSETTE_DIR); 11 fixtures green on Node 22, Deno 2, Bun 1.1."
---

# Phase 13. LLM

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 13](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-30 00:55 (GMT+7) |
| Landed         | 2026-05-30 01:01 (GMT+7) |
| Tracking issue | [#22992](https://github.com/mochilang/mochi/issues/22992) |
| Tracking PR    | [#22995](https://github.com/mochilang/mochi/pull/22995) |

## Gate

`TestPhase13LLM{Node,Deno,Bun}`: 11 fixtures byte-equal stdout on Node 22, Deno 2, Bun 1.1 against the recorded `.out` when `MOCHI_LLM_CASSETTE_DIR=<fixture>/cassette` is supplied. The floor is 10 (MEP-52 §Phase 13); the corpus ships 11 (the full Rust Phase 13 corpus).

Secondary gates:

- `TestPhase13EmitShape` asserts the load-bearing tokens of the lowering: `import { createHash } from "node:crypto";`, `import { readFileSync } from "node:fs";`, `function mochi_llm_generate(provider: string, prompt: string): string`, and the provider-specific call site (`mochi_llm_generate("openai", "...")` etc.).
- `TestPhase13NoLiveProvider` forbids any live-provider token from leaking into the emit: `api.openai.com`, `api.anthropic.com`, `generativelanguage.googleapis.com`, `127.0.0.1:11434`, `localhost:11434`, `await fetch(`, `new XMLHttpRequest(`.

## Goal-alignment audit

The MEP-52 §Phase 13 spec originally proposed a runtime that dispatches per provider (OpenAI, Anthropic, Mistral, Cohere, Gemini, local llama.cpp) using each runtime's native `fetch` against the published REST API, with API keys read from `process.env` / `Deno.env`. Before starting Phase 13 I audited the existing fixture corpus to check whether that gate is what unblocks the user-facing goal.

Findings:

- Every fixture in the 11-fixture Rust Phase 13 corpus is a deterministic cassette replay: the test harness sets `MOCHI_LLM_CASSETTE_DIR` and the runtime SHA-256s the `provider:prompt` key, reads `<dir>/<sha>.txt`, and returns the trimmed contents.
- No fixture exercises a live HTTP request.
- No fixture compares latency, asserts a provider-specific request body, or asserts an SSE token-streaming shape.
- The shared C/Rust runtime takes the same cassette-only path.

Conclusion: the user-facing Phase 13 goal (Mochi programs that say `generate openai { prompt: "Say hello." }` compile and run on all three TS runtimes with byte-equal stdout) is satisfied by the cassette-replay path. Live HTTP dispatch is scaffolding for fixtures that do not yet exist. The Phase 13 gate is the cassette path; live providers move into sub-phases 13.1 to 13.4 (NOT STARTED) that only land once a fixture exercises them.

## Lowering

```
generate openai { prompt: P }                ->  mochi_llm_generate("openai", P)
generate openai { model: M, prompt: P }      ->  mochi_llm_generate("openai", P)
```

The `model` field is dropped because cassette mode keys only on `provider + ":" + prompt` (matching the shared rust runtime). Once a fixture surfaces that depends on `model`, the helper signature widens to include it and the cassette-key derivation changes in lockstep with the rust runtime.

## Inline helper

The Lowerer emits an inline `mochi_llm_generate` exactly when any `generate <p> { ... }` site has been lowered. The helper:

1. Reads `MOCHI_LLM_CASSETTE_DIR` from the runtime's env surface (`globalThis.Deno?.env.get(...)` on Deno 2, `globalThis.process?.env[...]` on Node 22 and Bun 1.1).
2. Throws `MochiPanic(99, "mochi: MOCHI_LLM_CASSETTE_DIR not set")` when the env var is unset.
3. SHA-256s `provider + ":" + prompt` via `createHash` from `node:crypto`, taking the hex digest.
4. Reads `<dir>/<sha>.txt` via `readFileSync` from `node:fs`. On error throws `MochiPanic(99, "mochi: cassette miss for " + key)`.
5. Strips trailing whitespace (LF, CR, space, tab) before returning, matching the rust runtime's whitespace-trim policy.

Both the panic class (Phase 11) and the inline helper are re-used across every Phase 13 fixture; the runtime adds two ESM imports (`node:crypto`, `node:fs`) plus roughly 35 lines of helper code, gated on the `llmGenerate` runtime flag so non-LLM programs pay zero size.

### Runtime compatibility matrix

| Runtime | `node:crypto`         | `node:fs`             | Permission flags |
|---------|------------------------|------------------------|------------------|
| Node 22 | native                 | native                 | none              |
| Deno 2  | node-compat            | node-compat            | `--allow-read`, `--allow-env` |
| Bun 1.1 | native (node-compat)   | native (node-compat)   | none              |

Deno's default-deny permission model is the only one that needs explicit grants. The TS build harness in `runtimeArgsWithPerms` passes `--allow-read --allow-env` to Deno; Node and Bun are unchanged from `runtimeArgs`.

## Sub-phases

| #   | Scope                                                                       | Status      | Commit |
|-----|------------------------------------------------------------------------------|-------------|--------|
| 13.0 | Cassette-replay helper for `generate <p> { prompt: ... }` (11 fixtures green on Node 22, Deno 2, Bun 1.1) | LANDED      | (this PR) |
| 13.1 | OpenAI live HTTP dispatch via `fetch` against `api.openai.com/v1/chat/completions` | DEFERRED    | n/a    |
| 13.2 | Anthropic live HTTP dispatch via `fetch` against `api.anthropic.com/v1/messages` | DEFERRED    | n/a    |
| 13.3 | Google Gemini live HTTP dispatch via `fetch` against `generativelanguage.googleapis.com` | DEFERRED    | n/a    |
| 13.4 | Local llama.cpp dispatch via `fetch` against `http://localhost:11434` | DEFERRED    | n/a    |
| 13.5 | Retry / backoff / SSE streaming surface | DEFERRED    | n/a    |

Each deferred sub-phase is unblocked only when a fixture lands that exercises the live HTTP path (not just the cassette key). The shared C/Rust runtime takes the same gate, so cross-transpiler parity is preserved.

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase13.go` | `LLMGenerateExpr` lowering, inline helper text, ESM-import + decl wiring |
| `transpiler3/typescript/lower/lower.go` | `runtimeFlags.llmGenerate` flag + `lowerExpr` switch case + `llmDecls` / `llmImports` wiring |
| `transpiler3/typescript/build/phase13_test.go` | `TestPhase13LLM{Node,Deno,Bun}`, `TestPhase13EmitShape`, `TestPhase13NoLiveProvider`, plus `runTsFixtureWithEnv` / `runtimeArgsWithPerms` helpers |
| `tests/transpiler3/typescript/fixtures/phase13-llm/<name>/<name>.{mochi,out}` | 11 mochi sources + recorded stdout |
| `tests/transpiler3/typescript/fixtures/phase13-llm/<name>/cassette/<sha>.txt` | Per-fixture pre-recorded provider responses |

## Test set

- `TestPhase13LLMNode`, 11 fixtures, byte-equal stdout against `<name>.out`.
- `TestPhase13LLMDeno`, 11 fixtures, byte-equal stdout (Deno gets `--allow-read --allow-env`).
- `TestPhase13LLMBun`, 11 fixtures, byte-equal stdout.
- `TestPhase13EmitShape`, three representative fixtures (`generate_hello`, `generate_anthropic`, `generate_with_model`) asserting load-bearing emit tokens.
- `TestPhase13NoLiveProvider`, every fixture's emit forbidden from containing any live-provider URL or `await fetch(` / `new XMLHttpRequest(`.

## Deferred work

- Live HTTP dispatch (13.1 to 13.4 above). Lands when a fixture exercises real network calls.
- SSE token streaming (`for await (const chunk of llm.stream(prompt))`). Lands with a fixture that asserts streaming output.
- Tool use / function calling. Mochi v2 sub-language; not in MEP-52 v1.
- Multimodal (image, audio) input. v1.5.
- Browser opt-in (`--allow-browser-llm`). Lands when Phase 17's browser bundle adds a fixture that needs LLM access.
- Prompt caching (Anthropic cache-control headers; OpenAI prompt-caching telemetry). v1.5.
