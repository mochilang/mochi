---
title: "Phase 13. LLM (generate, ai)"
sidebar_position: 18
sidebar_label: "Phase 13. LLM"
description: "MEP-51 Phase 13 -- Mochi ai(prompt) and generate(prompt) lower to mochi_runtime.llm.dispatch with provider-pluggable backends (OpenAI 1.50+, Anthropic 0.40+, local Ollama via httpx); streaming via AsyncIterator; API keys from env; 10 fixtures."
---

# Phase 13. LLM (generate, ai)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 13](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase13LLM`: 10 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gates: byte-equal stdout against vm3 for every fixture under cassette playback (`MOCHI_LLM_CASSETTE_DIR` set, no live network in CI); zero API keys appear in any emitted source, log, or wheel artifact; live tests run only with `MOCHI_TEST_LLM_LIVE=1`.

## Goal-alignment audit

Mochi's `ai(prompt)` and `generate(prompt)` builtins are the LLM call surface. The Python target's pitch includes "you can ship a Jupyter notebook that talks to Claude or GPT and processes results as a typed Mochi stream"; without Phase 13 the LLM surface has no Python target and the notebook story is empty. Landing 13 ships a provider-dispatch table that routes to OpenAI, Anthropic, or local Ollama based on env config, plus a streaming `AsyncIterator[MochiToken]` for incremental token consumption. The user payload is `let summary = ai("summarise", text)` lowering to one `await` against the configured provider with full type safety on the way out.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | Dispatch table for OpenAI / Anthropic / local llama.cpp (Ollama); provider selection via `MOCHI_LLM_PROVIDER` env var | NOT STARTED | -- |
| 13.1 | Streaming responses via `AsyncIterator[MochiToken]`: `generate(prompt)` yields incrementally | NOT STARTED | -- |
| 13.2 | API keys from env (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`); never logged, never persisted, never serialised into wheel | NOT STARTED | -- |

## Sub-phase 13.0 -- Dispatch table

### Goal-alignment audit (13.0)

The dispatch table is the load-bearing abstraction: it decouples Mochi-level `ai(prompt)` from provider-specific SDK calls. Without 13.0 the LLM target would hard-code one provider, which is wrong both for users (vendor lock-in at codegen) and for testing (cassette playback needs a provider-neutral hook). Landing 13.0 makes the provider a runtime concern, the codegen always emits the same call, and the runtime picks the backend based on env config.

### Decisions made (13.0)

The dispatch surface lives at `runtime/python/mochi_runtime/llm/dispatch.py`:

```python
from __future__ import annotations

import os
from collections.abc import AsyncIterator
from dataclasses import dataclass
from typing import Final, Protocol


@dataclass(frozen=True, slots=True)
class MochiToken:
    text: str
    logprob: float | None


class Provider(Protocol):
    async def call(self, prompt: str) -> str: ...
    def stream(self, prompt: str) -> AsyncIterator[MochiToken]: ...


_PROVIDERS: Final[dict[str, type[Provider]]] = {}


def register_provider(name: str, factory: type[Provider]) -> None:
    _PROVIDERS[name] = factory


def _select_provider() -> Provider:
    name = os.environ.get("MOCHI_LLM_PROVIDER")
    if name is None:
        if "OPENAI_API_KEY" in os.environ:
            name = "openai"
        elif "ANTHROPIC_API_KEY" in os.environ:
            name = "anthropic"
        else:
            name = "ollama"
    factory = _PROVIDERS.get(name)
    if factory is None:
        raise RuntimeError(f"unknown LLM provider: {name}")
    return factory()


async def call(prompt: str) -> str:
    return await _select_provider().call(prompt)


def stream(prompt: str) -> AsyncIterator[MochiToken]:
    return _select_provider().stream(prompt)
```

Mochi `let summary = await ai("summarise", text)` lowers to:

```python
from __future__ import annotations

from mochi_runtime.llm import call as _llm_call


async def main() -> None:
    text = "..."
    summary = await _llm_call(f"summarise {text}")
    print(summary)
```

Decisions:

- `MochiToken` is a frozen-slots dataclass carrying the text and optional logprob. The dataclass is part of the public surface so consumers can `match` on tokens.
- `Provider` is a `typing.Protocol`, not an ABC. Structural typing means a user can register a custom provider by passing any class that has `call` and `stream` methods with the right signatures; no inheritance required.
- The provider registry is module-level state in `mochi_runtime.llm.dispatch`. The IR pass never emits direct provider calls; it always goes through `dispatch.call` and `dispatch.stream` so the runtime can swap providers without recompilation.
- Built-in providers register themselves at import time:

  ```python
  # in mochi_runtime/llm/openai_provider.py
  from mochi_runtime.llm.dispatch import register_provider
  register_provider("openai", OpenAIProvider)
  ```

- Each built-in provider lives in its own module under `mochi_runtime.llm.<name>_provider`; the top-level `mochi_runtime.llm.__init__` imports each, registering them on import. Optional providers (Anthropic, Ollama) gracefully degrade if their SDK is missing: the import wraps in `try / except ImportError` and logs a warning.

## Sub-phase 13.1 -- Streaming responses

### Goal-alignment audit (13.1)

Mochi `generate(prompt)` returns `Stream<MochiToken>` for incremental token consumption: the user can show partial results in a UI, terminate early on a stop word, or pipe through downstream operators. Without 13.1 the stream surface has no Python target and `generate(...)` would collapse to a synchronous one-shot. Landing 13.1 makes streaming first-class: the consumer can `async for token in generate("...")` and the runtime delivers tokens as they arrive from the provider's SSE stream.

### Decisions made (13.1)

Each provider implements `stream` returning an `AsyncIterator[MochiToken]`:

```python
from __future__ import annotations

import os
from collections.abc import AsyncIterator
from typing import Final

from openai import AsyncOpenAI

from mochi_runtime.llm.dispatch import MochiToken, Provider


class OpenAIProvider:
    def __init__(self) -> None:
        self._client: Final[AsyncOpenAI] = AsyncOpenAI()

    async def call(self, prompt: str) -> str:
        completion = await self._client.chat.completions.create(
            model=os.environ.get("MOCHI_LLM_MODEL", "gpt-4o-mini"),
            messages=[{"role": "user", "content": prompt}],
        )
        return completion.choices[0].message.content or ""

    async def stream(self, prompt: str) -> AsyncIterator[MochiToken]:
        completion = await self._client.chat.completions.create(
            model=os.environ.get("MOCHI_LLM_MODEL", "gpt-4o-mini"),
            messages=[{"role": "user", "content": prompt}],
            stream=True,
        )
        async for chunk in completion:
            delta = chunk.choices[0].delta.content
            if delta is None:
                continue
            yield MochiToken(text=delta, logprob=None)
```

Mochi:

```mochi
async fun stream_summary(text: string) {
    for token in generate("summarise " + text) {
        print_no_newline(token.text)
    }
}
```

Emit:

```python
from __future__ import annotations

from mochi_runtime.llm import stream as _llm_stream


async def stream_summary(text: str) -> None:
    async for token in _llm_stream(f"summarise {text}"):
        print(token.text, end="")
```

Decisions:

- The `stream` method is `async def` returning `AsyncIterator[MochiToken]` so that initialisation (connecting to the provider, sending the request) is awaited before the first `yield`. A non-`async` `def` returning a coroutine that yields would split the API surface in a way mypy mishandles.
- Empty deltas (`continue` above) are filtered at the provider boundary so the consumer never sees zero-length tokens.
- `print_no_newline` (Mochi builtin) lowers to `print(..., end="")`. Stream consumers typically suppress newlines; the IR pass picks `end=""` from the Mochi declaration.
- The Anthropic provider uses the `anthropic` SDK 0.40+ with `client.messages.create(stream=True)` and yields each text delta. The Ollama provider uses `httpx.AsyncClient` against `http://localhost:11434/api/generate` with `stream=True` and parses NDJSON line-by-line.
- The stream's `aclose()` is called when the consumer breaks out of the loop; each provider's stream object handles cleanup (closing the SSE connection).

## Sub-phase 13.2 -- API keys from env

### Goal-alignment audit (13.2)

API keys are the security gate for LLM calls. If a Mochi-emitted wheel contained the key, every consumer would have it, every cache would have it, every PyPI mirror would have it. The threat model is "don't ship secrets". Without 13.2 a careless user could write `let key = "sk-..."; ai("...", key)` and the literal would land in the wheel. Landing 13.2 means the runtime reads keys exclusively from `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `MISTRAL_API_KEY` env vars, never logs them, never serialises them into any artifact, and the IR pass rejects any attempt to pass a literal key to the LLM call.

### Decisions made (13.2)

The provider SDKs read keys from env by convention:

- `AsyncOpenAI()` (no argument) reads `OPENAI_API_KEY`.
- `AsyncAnthropic()` reads `ANTHROPIC_API_KEY`.
- The Ollama provider reads `OLLAMA_HOST` (default `http://localhost:11434`); no API key is required for local inference.

The IR pass enforces "no literal key":

```python
# in transpiler3/python/lower/llm.go
# rejected at codegen with diagnostic M057_LLM_E001:
# `ai("...", "sk-...")` is a security error
```

Logging discipline:

```python
# Never:
logger.info("calling LLM with key %s", api_key)

# Always:
logger.info("calling LLM (provider=%s)", provider_name)
```

The `mochi_runtime.llm` package has a structured-logging wrapper that explicitly excludes keys from any log record. The wrapper lives at `mochi_runtime.llm._logging` and is used by all built-in providers.

Cassette playback for tests:

```python
# in mochi_runtime/llm/_cassette.py
from __future__ import annotations

import hashlib
import os
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Final

from mochi_runtime.llm.dispatch import MochiToken, Provider, register_provider


class CassetteProvider:
    def __init__(self) -> None:
        cassette_dir = os.environ.get("MOCHI_LLM_CASSETTE_DIR")
        if cassette_dir is None:
            raise RuntimeError("cassette provider requires MOCHI_LLM_CASSETTE_DIR")
        self._dir: Final[Path] = Path(cassette_dir)

    def _key(self, prompt: str) -> str:
        return hashlib.sha256(f"openai:{prompt}".encode("utf-8")).hexdigest()

    async def call(self, prompt: str) -> str:
        path = self._dir / f"{self._key(prompt)}.txt"
        return path.read_text(encoding="utf-8")

    async def stream(self, prompt: str) -> AsyncIterator[MochiToken]:
        text = await self.call(prompt)
        for ch in text:
            yield MochiToken(text=ch, logprob=None)


def _maybe_register() -> None:
    if os.environ.get("MOCHI_LLM_CASSETTE_DIR"):
        register_provider("cassette", CassetteProvider)
        os.environ.setdefault("MOCHI_LLM_PROVIDER", "cassette")


_maybe_register()
```

The cassette format matches the JVM and .NET targets so cassette files are reusable across backends; the key is `sha256(provider + ":" + prompt)` and the response lives at `<dir>/<hash>.txt`. CI sets `MOCHI_LLM_CASSETTE_DIR` per fixture; live calls run only with `MOCHI_TEST_LLM_LIVE=1`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/llm.go` | `ai(prompt)` to `await mochi_runtime.llm.call(prompt)`; `generate(prompt)` to `mochi_runtime.llm.stream(prompt)`; reject literal keys |
| `runtime/python/mochi_runtime/llm/__init__.py` | Public surface: `call`, `stream`, `MochiToken`; register built-in providers |
| `runtime/python/mochi_runtime/llm/dispatch.py` | `Provider` protocol, `_PROVIDERS` registry, `_select_provider` |
| `runtime/python/mochi_runtime/llm/openai_provider.py` | OpenAI SDK 1.50+ backend |
| `runtime/python/mochi_runtime/llm/anthropic_provider.py` | Anthropic SDK 0.40+ backend |
| `runtime/python/mochi_runtime/llm/ollama_provider.py` | Ollama via `httpx.AsyncClient` |
| `runtime/python/mochi_runtime/llm/_cassette.py` | Cassette provider for tests |
| `runtime/python/mochi_runtime/llm/_logging.py` | Structured logging wrapper that scrubs API keys |
| `transpiler3/python/build/phase13_test.go` | `TestPhase13LLM`: 10 fixtures + cassette gates |
| `tests/transpiler3/python/fixtures/phase13-llm/` | 10 fixture directories with cassette/ subfolders |

## Test set

- `TestPhase13LLM` -- 10 fixtures: ai_oneshot_summary, ai_translate, ai_classify, ai_multi_arg, ai_provider_select_anthropic (5 from 13.0); generate_stream_summary, generate_stream_break_early, generate_stream_token_count (3 from 13.1); ai_no_key_literal (codegen rejection), cassette_round_trip (2 from 13.2).

## Deferred work

- Structured-output JSON schema validation. Deferred to v1.5; v1 returns plain `str` and the user parses.
- Function calling / tool use. Deferred to v1.5; v1 supports prompt completion only.
- Anthropic computer-use API. Deferred to v2.
- Google Generative AI SDK (Gemini) provider. Deferred to v1.5 pending adoption signal; v1 ships OpenAI + Anthropic + Ollama.
- LangChain / LlamaIndex interop adapters. Deferred indefinitely; the dispatch table is the integration point and third-party adapters can layer on top.
