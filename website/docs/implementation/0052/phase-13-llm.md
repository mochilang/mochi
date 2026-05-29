---
title: "Phase 13. LLM"
sidebar_position: 14
sidebar_label: "Phase 13. LLM"
description: "MEP-52 Phase 13, Mochi llm.generate to @mochi/runtime/llm provider dispatch (OpenAI, Anthropic, Mistral, Cohere, Google Gemini, local llama.cpp via HTTP); browser opt-in with user-supplied key; 10 fixtures."
---

# Phase 13. LLM

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 13](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase13LLM`: 10 fixtures green on Node 22, Deno 2, Bun 1.1. Browser is gated behind an explicit `--allow-browser-llm` flag (per MEP-52 §Security: no API keys baked into a browser bundle by default). Secondary gates: tsc strict zero diagnostics; provider dispatch budget (runtime addition stays under 4 KB gzipped); no logged API keys (eslint rule `no-console` with allowlist of `console.error` only, plus a custom rule that rejects any log call referencing a value sourced from `process.env`).

## Goal-alignment audit

`llm.generate` is Mochi's portable LLM call. The TS surface has no built-in LLM client; each provider (OpenAI, Anthropic, etc.) ships its own SDK on npm. MEP-52 ships `@mochi/runtime/llm` as a thin provider-dispatch table that reads API keys from environment variables (Node/Bun: `process.env`, Deno: `Deno.env.get`, browser: rejected unless the user explicitly opted in). The dispatch keeps Mochi user code provider-agnostic: a single `llm.generate(prompt)` call routes to whichever provider is configured.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `@mochi/runtime/llm` provider-dispatch table; provider read from `MOCHI_LLM_PROVIDER` env var (default `"openai"`) | NOT STARTED | n/a |
| 13.1 | OpenAI provider via `fetch` against `api.openai.com/v1/chat/completions` (no `openai` npm dep; pure fetch) | NOT STARTED | n/a |
| 13.2 | Anthropic provider via `fetch` against `api.anthropic.com/v1/messages` (claude-sonnet-4-6 default; configurable via `MOCHI_LLM_MODEL`) | NOT STARTED | n/a |
| 13.3 | Streaming via SSE: `for await (const chunk of llm.stream(prompt))` lowers to a `fetch` with `stream: true` then SSE parsing | NOT STARTED | n/a |
| 13.4 | Browser opt-in: `--allow-browser-llm` flag is required at codegen; the emitter injects a runtime check that the user has provided a key | NOT STARTED | n/a |

## Sub-phase 13.0, Provider dispatch

### Decisions made (13.0)

```typescript
// @mochi/runtime/llm
export type Provider = "openai" | "anthropic" | "mistral" | "cohere" | "gemini" | "local";

export type GenerateOptions = {
  model?: string;
  temperature?: number;
  maxTokens?: number;
  systemPrompt?: string;
};

export async function generate(
  prompt: string,
  opts: GenerateOptions = {},
): Promise<string> {
  const provider = (envGet("MOCHI_LLM_PROVIDER") ?? "openai") as Provider;
  switch (provider) {
    case "openai":    return openaiGenerate(prompt, opts);
    case "anthropic": return anthropicGenerate(prompt, opts);
    case "mistral":   return mistralGenerate(prompt, opts);
    case "cohere":    return cohereGenerate(prompt, opts);
    case "gemini":    return geminiGenerate(prompt, opts);
    case "local":     return localGenerate(prompt, opts);
  }
}

function envGet(name: string): string | undefined {
  if (typeof (globalThis as any).Deno !== "undefined") {
    return (globalThis as any).Deno.env.get(name);
  }
  if (typeof (globalThis as any).process !== "undefined") {
    return (globalThis as any).process.env[name];
  }
  return undefined;
}
```

**Env-var key reading**: `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`, `GEMINI_API_KEY`. The dispatch table reads only when its provider is selected, never logs the key, and never serialises it. The browser path rejects at runtime (and at codegen unless `--allow-browser-llm` is passed).

## Sub-phase 13.1, OpenAI provider

### Decisions made (13.1)

```typescript
async function openaiGenerate(prompt: string, opts: GenerateOptions): Promise<string> {
  const key = envGet("OPENAI_API_KEY");
  if (key === undefined) throw new Error("OPENAI_API_KEY not set");
  const r = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: { "content-type": "application/json", authorization: `Bearer ${key}` },
    body: JSON.stringify({
      model: opts.model ?? "gpt-4o-mini",
      messages: [
        ...(opts.systemPrompt ? [{ role: "system", content: opts.systemPrompt }] : []),
        { role: "user", content: prompt },
      ],
      temperature: opts.temperature,
      max_tokens: opts.maxTokens,
    }),
  });
  if (!r.ok) throw new Error(`openai: ${r.status} ${await r.text()}`);
  const json = await r.json() as { choices: Array<{ message: { content: string } }> };
  return json.choices[0]?.message.content ?? "";
}
```

**No npm SDK dependency**: the OpenAI npm SDK is roughly 200 KB minified and pulls additional transitive deps. A direct `fetch` against the published REST API is roughly 30 lines, fully typed, zero deps.

## Sub-phase 13.2, Anthropic provider

### Decisions made (13.2)

```typescript
async function anthropicGenerate(prompt: string, opts: GenerateOptions): Promise<string> {
  const key = envGet("ANTHROPIC_API_KEY");
  if (key === undefined) throw new Error("ANTHROPIC_API_KEY not set");
  const r = await fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: {
      "content-type": "application/json",
      "x-api-key": key,
      "anthropic-version": "2023-06-01",
    },
    body: JSON.stringify({
      model: opts.model ?? "claude-sonnet-4-6",
      max_tokens: opts.maxTokens ?? 1024,
      ...(opts.systemPrompt ? { system: opts.systemPrompt } : {}),
      messages: [{ role: "user", content: prompt }],
    }),
  });
  if (!r.ok) throw new Error(`anthropic: ${r.status} ${await r.text()}`);
  const json = await r.json() as { content: Array<{ text: string }> };
  return json.content[0]?.text ?? "";
}
```

**Default model**: `claude-sonnet-4-6`. Overridable via `MOCHI_LLM_MODEL` or `opts.model`.

## Sub-phase 13.3, Streaming

### Decisions made (13.3)

```typescript
export async function* stream(prompt: string, opts: GenerateOptions = {}): AsyncGenerator<string, void, undefined> {
  // dispatch to provider's streaming endpoint and parse SSE
  // ...
}
```

SSE parsing: split on `\n\n`, parse `data: {...}` JSON, yield the incremental content delta. Each provider has a slightly different SSE shape; the dispatch routes to the per-provider parser.

**Use site**:

```typescript
for await (const chunk of stream("hello")) {
  console.log(chunk);
}
```

## Sub-phase 13.4, Browser opt-in

### Decisions made (13.4)

**Default**: `--target=browser-bundle` rejects at codegen if `llm.generate` is reachable. The error directs the user to either remove the call, gate it on `mochiRuntime() !== "browser"`, or pass `--allow-browser-llm`.

**With `--allow-browser-llm`**: the emitter inlines a runtime check that the key is provided via `mochi.llm.setKey(provider, key)` rather than reading from `process.env`. The browser user is expected to source the key from their own credential store (BYOK pattern).

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/llm.go` | `llm.generate` call to `@mochi/runtime/llm.generate` import + call |
| `transpiler3/typescript/lower/llm_reachability.go` | Browser-target reachability check |
| `runtime3/typescript/src/llm/index.ts` | Provider dispatch |
| `runtime3/typescript/src/llm/openai.ts` | OpenAI fetch + SSE |
| `runtime3/typescript/src/llm/anthropic.ts` | Anthropic fetch + SSE |
| `runtime3/typescript/src/llm/mistral.ts` | Mistral |
| `runtime3/typescript/src/llm/cohere.ts` | Cohere |
| `runtime3/typescript/src/llm/gemini.ts` | Gemini |
| `runtime3/typescript/src/llm/local.ts` | local llama.cpp via HTTP |
| `transpiler3/typescript/build/phase13_test.go` | `TestPhase13LLM` |
| `tests/transpiler3/typescript/fixtures/phase13-llm/` | 10 fixtures using a local mock server (no real API calls in CI) |

## Test set

- `TestPhase13LLM`, 10 fixtures Node + Deno + Bun against a local mock server (`tests/transpiler3/typescript/fixtures/phase13-llm/mock_server.js`).
- `TestPhase13NoKeyLog`, asserts no emitted code logs a value sourced from `process.env`.
- `TestPhase13BrowserReject`, the fixture using `llm.generate` is rejected under `--target=browser-bundle` without `--allow-browser-llm`.

## Deferred work

- Tool use / function calling. Mochi v2 sub-language; not in MEP-52 v1.
- Multimodal (image, audio) input. v1.5.
- Token streaming with structured output (JSON Schema parser). v1.5.
- Prompt caching (Anthropic cache-control headers; OpenAI prompt-caching telemetry). Each provider has its own prompt-cache surface; the dispatch table forwards the relevant headers but Phase 13 ships without explicit cache-aware emit. v1.5.
