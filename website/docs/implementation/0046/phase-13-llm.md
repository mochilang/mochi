---
title: "Phase 13. LLM (generate)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM (generate)"
description: "MEP-46 Phase 13. LLM (generate) — detailed implementation spec."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 13. LLM (generate)](/docs/mep/mep-0046#phase-13-llm-generate) |
| Status         | LANDED |
| Started        | 2026-05-26 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's `generate` expression on the BEAM target. The runtime uses OTP's built-in `httpc` (from the `inets` application) for HTTP requests — **not** `gun`. There are no per-provider gen_server supervisors; the implementation is a stateless dispatch module `mochi_llm.erl` that checks a cassette directory first, then dispatches live to OpenAI or Anthropic via `httpc:request/4`. See also [Phase 13.1: Panic and try-catch](/docs/implementation/0046/phase-13-1-panic-try-catch).

---

## Gate

See [MEP-46 §Phases · Phase 13. LLM (generate)](/docs/mep/mep-0046) for the normative gate. All 10 fixtures must produce byte-equal output to vm3. All 10 fixtures run against pre-recorded cassettes committed to the repo.

---

## Goal-alignment audit

`generate` is a first-class Mochi primitive for LLM-backed computation. The gate requires 10 fixtures covering basic generation, multi-field schemas, mixed types, prompt interpolation, and provider selection. All fixtures are user-facing. The cassette system ensures CI is deterministic without live API calls. Runtime schema validation catches provider errors at the point of generation rather than propagating untyped maps into Mochi code.

---

## Sub-phases

### Sub-phase 13.0: mochi_llm.erl cassette dispatch (LANDED `78d817ae3b`)

**Actual architecture**

The LLM subsystem is a single stateless module `mochi_llm.erl`. There are no gen_server supervisors and no `gun` HTTP client. The implementation uses OTP's built-in `httpc` from the `inets` application.

`generate/3` dispatch order:

1. Check `MOCHI_LLM_CASSETTE_DIR` env var. If set, look for a matching cassette file (plain Erlang terms). If found, return its stored response.
2. Check `OPENAI_API_KEY` env var. If set, call `live_generate/3` targeting `api.openai.com/v1/chat/completions`.
3. Check `ANTHROPIC_API_KEY` env var. If set, call `live_generate/3` targeting `api.anthropic.com/v1/messages`.
4. If none are set, return `{error, no_provider_configured}`.

Default models: `gpt-4o-mini` (OpenAI), `claude-haiku-4-5-20251001` (Anthropic).

Cassette files are plain Erlang terms stored in `MOCHI_LLM_CASSETTE_DIR`. CI always runs with `MOCHI_LLM_CASSETTE_DIR` pointing to committed cassettes under `tests/transpiler3/beam/cassettes/`.

---

### Sub-phase 13.1: Panic and try-catch (LANDED `924dfd9901`)

This sub-phase implements `PanicStmt` and `TryCatchStmt` lowering — not "generate block lowering" as the original spec described.

- `panic(code, msg)` lowers to `erlang:error({mochi_panic, Code, Msg})`.
- `try { B } catch e { C }` lowers to a `c_try` node that catches `{mochi_panic, E, _}` and binds `e` to the panic code. Non-mochi exceptions are re-thrown via `erlang:throw/1`.

See the dedicated [Phase 13.1 page](/docs/implementation/0046/phase-13-1-panic-try-catch) for full details.

---

### Sub-phase 13.2: Live provider dispatch (LANDED `92d475a936`)

`live_generate/3` in `mochi_llm.erl` POSTs to provider APIs using `httpc:request/4`:

- **OpenAI:** `POST https://api.openai.com/v1/chat/completions` with `Authorization: Bearer $OPENAI_API_KEY`.
- **Anthropic:** `POST https://api.anthropic.com/v1/messages` with `x-api-key: $ANTHROPIC_API_KEY` and `anthropic-version: 2023-06-01`.

The response body is decoded via OTP 27's `json:decode/1`. No connection pooling, no gen_server, no `gun`.

---

## Test set

10 fixtures under `tests/transpiler3/beam/fixtures/phase13/`, all with pre-recorded cassettes:

| # | File | Description |
|---|------|-------------|
| 01 | `llm_summarize.mochi` | Summarize a short text, return `{summary: string}` |
| 02 | `llm_classify.mochi` | Classify sentiment, return `{sentiment: string}` |
| 03 | `llm_extract.mochi` | Extract name and age from text, return `{name: string, age: int}` |
| 04 | `llm_translate.mochi` | Translate English to French |
| 05 | `llm_multi_field.mochi` | Schema with 5 fields of mixed types |
| 06 | `llm_anthropic.mochi` | Same summarize task via Anthropic provider |
| 07 | `llm_schema_mismatch.mochi` | Provider returns wrong type; verify `schema_mismatch` error |
| 08 | `llm_prompt_interp.mochi` | Prompt with variable interpolation |
| 09 | `llm_nested_schema.mochi` | Schema with `map<string, string>` field |
| 10 | `llm_async_generate.mochi` | `async (generate ...)` combined with Phase 11 |

---

## Decisions made

**Why `httpc` instead of `gun` for LLM HTTP**

The original spec called for `gun` HTTP/2 gen_servers. The actual implementation uses OTP's built-in `httpc` because: (1) the cassette replay path means most CI calls never touch the network, (2) LLM calls are latency-bound by the model, not by connection overhead, and (3) adding `gun` as a dep for the LLM module would pull in a non-stdlib dependency. `httpc` is part of `inets` which is already in the OTP standard library. For production workloads that need concurrent LLM calls, users can wrap `mochi_llm` via FFI.

**Why stateless dispatch rather than gen_server supervisors**

The original spec described a `mochi_llm_sup` + `mochi_llm_openai` + `mochi_llm_anthropic` gen_server architecture. The actual implementation is simpler: `mochi_llm.erl` is a pure stateless module that reads env vars on each call. This avoids process management complexity, is easier to test, and handles the cassette-first dispatch path with less ceremony. State (e.g., a connection pool) can be added later if profiling shows it is needed.

**Why cassette files as plain Erlang terms**

Plain Erlang terms (readable via `file:consult/1`) are human-editable without tooling, can be committed as text, and round-trip through `file:write_file/2` + `io_lib:format("~p.~n", [Term])` with no external dependencies. JSON would require a JSON library dependency or OTP 27's `json` module; `.eterms` avoids that coupling.

---

## Closeout notes

Phase 13 landed across three commits. Sub-phase 13.0 (cassette dispatch + `mochi_llm.erl`) landed as `78d817ae3b` with 10 fixtures using pre-recorded cassettes. Sub-phase 13.1 (panic/try-catch lowering) landed as `924dfd9901` — this is a different scope than the original spec's "generate block lowering". Sub-phase 13.2 (live provider dispatch via `httpc`) landed as `92d475a936`. The implementation diverges significantly from the spec's gen_server architecture; see the Sub-phases section above for the actual design.
