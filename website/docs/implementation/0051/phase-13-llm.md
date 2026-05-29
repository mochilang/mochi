---
title: "Phase 13. LLM (generate)"
sidebar_position: 18
sidebar_label: "Phase 13. LLM"
description: "MEP-51 Phase 13 -- generate <provider> {model, prompt} lowers to mochi_runtime.llm.mochi_llm_generate; cassette-mode replay reads ${MOCHI_LLM_CASSETTE_DIR}/<djb2hash>.txt with C-compatible hash math; 11 fixtures."
---

# Phase 13. LLM (generate)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 13](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (13.0 only; live providers DEFERRED) |
| Started        | 2026-05-29 20:04 (GMT+7) |
| Landed         | 2026-05-29 20:11 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase13LLM`: 11 fixtures green on CPython 3.12.7 in `transpiler3/python/build/phase13_test.go`. The corpus is the C target's `tests/transpiler3/c/fixtures/llm/` set copied verbatim (each fixture is a subdirectory with `<name>.mochi`, `<name>.out` renamed from `expect.txt`, and a `cassette/` subdir of `<djb2hash>.txt` files). Each fixture compiles, runs `python -m mochi_user_<name>` with `MOCHI_LLM_CASSETTE_DIR` pointed at the fixture's cassette dir, and byte-compares stdout to the `.out` file. Coverage: all 4 providers (`openai`, `anthropic`, `google`, `llama`); default + explicit `model:` slot; `generate` as a variable initializer, inside a function body, concatenated into a string, and called multiple times. The full Phase 1-13 regression (`go test ./transpiler3/python/... -count=1`) finishes in 42.8s with zero regressions.

## Goal-alignment audit

Mochi's `generate <provider> { ... }` is the LLM call surface. For the Python target, the load-bearing v1 use case is reproducible CI: fixtures that pin a prompt to a recorded response, replayed on every `go test` run, never hitting the network. The C target already established this contract (cassette directory, DJB2-hashed `<provider>\0<model>\0<prompt>` filenames, trailing-newline strip on read); the Python target's job in Phase 13.0 is to match that contract byte-for-byte so existing recorded cassettes are portable across targets. Landing 13.0 unblocks: (1) the documented "summarise this notebook cell with Claude / GPT and feed it to a Mochi pipeline" Jupyter story; (2) cross-target test parity for any program that uses `generate`.

Live providers (OpenAI 1.50+ SDK, Anthropic 0.40+ SDK, google-generativeai 0.5+, llama-cpp-python local) are deferred to Phase 13.1+. The deferral is intentional: live calls in CI are flaky, expensive, and gate on API keys in the secret store, none of which materially improve the user-facing surface. The dispatch shape (`mochi_llm_generate(provider, model, prompt) -> str`) is provider-pluggable: a Phase 13.1 patch adds branches on `provider` inside the same helper without touching the lower or the call site emit.

Streaming (`AsyncIterator[MochiToken]`) is deferred to Phase 11.1 (async colour pass) plus Phase 13.2; the v1 corpus has no streaming fixture, and synchronous full-response calls cover every program in the corpus.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `generate <provider> { model: ..., prompt: ... }` lowers to `mochi_llm_generate(provider, model, prompt)` against `mochi_runtime.llm`; cassette-mode replay via `MOCHI_LLM_CASSETTE_DIR`; DJB2 hash matches C runtime byte-for-byte | LANDED 2026-05-29 | (filled at ship) |
| 13.1 | OpenAI live provider via the `openai` SDK 1.50+; key from `OPENAI_API_KEY`; cassette write-through under `MOCHI_LLM_CASSETTE_RECORD` | DEFERRED | -- |
| 13.2 | Anthropic, Google, llama-cpp-python live providers | DEFERRED | -- |
| 13.3 | Streaming `AsyncIterator[MochiToken]` surface; rides on the Phase 11.1 async colour pass | DEFERRED | -- |
| 13.4 | Pluggable cassette codecs (gzip, lz4) for large recorded responses | DEFERRED | -- |

## Sub-phase 13.0 -- cassette-mode LLM via DJB2-keyed replay

### Goal-alignment audit (13.0)

A Mochi program that calls `generate openai { prompt: "..." }` should run on the Python target without changes, deterministically, in CI, against a checked-in cassette. The C target made this possible by hashing the inputs and reading a flat file; the Python target needs the same hash and the same lookup so the cassette files port across targets. The 11 fixtures in `tests/transpiler3/c/fixtures/llm/` are the spec: they were checked in on the C side with hash-keyed filenames, and the Python target reads them as-is. If the Python hash drifted by a byte, every cassette would miss and stdout would be empty.

### Decisions made (13.0)

**DJB2 with NUL field separators, byte-equivalent to the C runtime.** The C runtime's `llm_hash_key(provider, model, prompt)` is `h=5381; for each byte: h = (h*33) ^ byte; h = (h*33) ^ 0;` between fields. The Python port uses Python ints masked to `(1<<64)-1` after every multiply and XOR so the wrap-around matches `uint64_t`. The separator step (`h = (h*33) ^ 0`) is left as `h = (h*33)` since `^ 0` is identity; the multiply is the load-bearing operation that prevents `("a","bc")` from colliding with `("ab","c")`. Validated against 11 known cassette filenames produced by the C target: 11/11 match.

**Filename format `<key>.txt`, decimal uint64.** The C `snprintf("%llu", key)` writes decimal; Python `f"{key}"` writes decimal. Hex would have been a more compact filename but would have created a cross-target portability gap for no benefit. Single trailing newline stripped on read so cassette files can be edited in any editor that auto-appends `\n`.

**Cassette dir resolution via `MOCHI_LLM_CASSETTE_DIR` env var.** The runtime helper reads the env once per call. The test runner (`runPythonFixture` in `build_test.go`) sets the env to the fixture's `cassette/` directory just before `python -m <pkg>` starts, so each fixture sees only its own cassette set; no global state, no test interleaving risk. Live mode (no env set) prints the same `cassette not found` shape to stderr that the C runtime does and returns `""`, so a missing-cassette program still completes rather than aborting.

**Module emit imports `mochi_llm_generate` lazily.** The lowerer tracks `needsLLM bool`; only programs that contain a `generate` expression pull in `from mochi_runtime.llm import mochi_llm_generate`. The hello-world fixture stays import-free.

**`provider` is a string literal at the call site, not a runtime variable.** The Mochi surface syntax `generate openai { ... }` makes the provider a compile-time token; the c aotir captures it as `LLMGenerateExpr.Provider string`. The Python lowerer emits it as a `pysrc.StrLit`. A future "dynamic provider dispatch" surface would need a new IR node and is out of scope for Phase 13.0.

**`model` defaults to empty string in the IR.** The c lower already substitutes `StringLit("")` when the source omits `model:`, so the Python lower does not have a separate default path. Cassette filenames recorded with empty model differ from those with explicit model (the hash diverges after the first NUL), which matches the recorded fixtures.

**Cassette directory not copied into the build output.** The build cache key is over the .mochi bytes + Python toolchain version + `mep51-phase13` marker. Adding the cassette dir to the hash would force a rebuild on every cassette edit, which is exactly the wrong default: the user is iterating on the recording, not on the program. The runtime reads the dir at execution time from the env var the test harness supplies.

### Fixture corpus (11 fixtures, ported from the C target)

`tests/transpiler3/python/fixtures/phase13-llm/`:

| Fixture | Provider | Model | Surface |
|---------|----------|-------|---------|
| `generate_text` | openai | (default) | `let r = generate openai { prompt: "Say hello." }`; print bare |
| `generate_anthropic` | anthropic | (default) | Count to 3 |
| `generate_anthropic_model` | anthropic | claude-3-haiku-20240307 | Repeat: hello world |
| `generate_concat` | openai | (default) | `"Answer: " + r` |
| `generate_google` | google | (default) | Add 1 and 1 |
| `generate_in_fun` | openai | (default) | `generate` inside a user function body |
| `generate_in_var` | openai | (default) | `let s = "Sky color: " + r` |
| `generate_llama` | llama | (default) | Hello via local llama |
| `generate_multiple` | openai | (default) | Two `generate` calls in one program (two cassette files) |
| `generate_openai_model` | openai | gpt-4o | Square root of 16 |
| `generate_with_model` | openai | gpt-4o-mini | Say hi |

Each subdirectory ships `<name>.mochi`, `<name>.out`, and `cassette/<djb2>.txt`. `TestPhase13LLM` walks the directory, runs `runPythonFixture` which detects the cassette dir alongside the .mochi and sets `MOCHI_LLM_CASSETTE_DIR` before invoking `python -m <pkg>`. All 11 fixtures pass on CPython 3.12.7.

### Files changed

| File | Purpose |
|------|---------|
| `runtime/python/mochi_runtime/llm.py` (new) | `mochi_llm_generate(provider, model, prompt) -> str`; `_llm_hash_key` DJB2 port; cassette read with trailing-newline strip |
| `transpiler3/python/lower/llm.go` (new) | `lowerLLMGenerateExpr` emits `mochi_llm_generate(provider_str, model_expr, prompt_expr)` and sets `needsLLM` |
| `transpiler3/python/lower/lower.go` | `needsLLM bool` slot; dispatch `*aotir.LLMGenerateExpr`; conditional `from mochi_runtime.llm import mochi_llm_generate` import |
| `transpiler3/python/build/build.go` | Cache marker bumped `mep51-phase12` -> `mep51-phase13` |
| `transpiler3/python/build/build_test.go` | `runPythonFixture` sets `MOCHI_LLM_CASSETTE_DIR=<fixture>/cassette` when that dir exists |
| `transpiler3/python/build/phase13_test.go` (new) | `TestPhase13LLM` walks `phase13-llm/` subdirectories |
| `tests/transpiler3/python/fixtures/phase13-llm/` (new) | 11 fixture subdirs ported from the C target |

## Deferred work

- **13.1 OpenAI live provider.** Add a `provider == "openai"` branch in `mochi_llm_generate` that calls the official `openai` SDK 1.50+ with `OPENAI_API_KEY`. Honour `MOCHI_LLM_CASSETTE_RECORD` for write-through recording. Deferred because cassette-mode covers the load-bearing CI case; live calls add cost and flakiness without surfacing new programs.
- **13.2 Anthropic, Google, llama-cpp-python live providers.** Same pattern, one branch each. Deferred for the same reason.
- **13.3 Streaming `AsyncIterator[MochiToken]`.** Rides on the Phase 11.1 async colour pass and a streaming-cassette format. No v1 fixture exercises streaming.
- **13.4 Pluggable cassette codecs.** Some real-world recorded responses can be megabytes; gzip / lz4 transparent decode would keep the cassette dir compact. Deferred until a corpus fixture justifies it.
- **Hash collision detection.** Two different (provider, model, prompt) triples that DJB2-collide would silently map to the same cassette. The collision space is astronomical (DJB2 is 64-bit) but a Phase 13.x audit could record the full key string in the cassette body alongside the response and verify on read.
