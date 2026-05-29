"""Mochi LLM runtime helper for the Phase 13.0 surface.

Mochi's `generate <provider> { model: ..., prompt: ... }` expression
lowers (on the c aotir layer) to an LLMGenerateExpr; the Python target
emits a `mochi_llm_generate(provider, model, prompt)` call against this
module.

Phase 13.0 ships cassette-mode replay only: when MOCHI_LLM_CASSETTE_DIR
is set, the response is read from ``<dir>/<hash>.txt`` where ``<hash>``
is the DJB2 hash of ``provider\\0model\\0prompt``. Hash math, NUL
separator, and trailing-newline strip exactly match the C runtime in
``transpiler3/c/runtime/src/llm.c`` so cassettes are interchangeable
across targets.

Live mode (OpenAI / Anthropic / Google / llama.cpp) is deferred to
Phase 13.1+: the load-bearing case in v1 is reproducible CI fixtures,
which only need cassette playback. When the live providers land, they
ride on top of this same module as additional dispatch branches keyed
on the provider name.
"""

from __future__ import annotations

import os
import sys

_UINT64_MASK = (1 << 64) - 1


def _llm_hash_key(provider: str, model: str, prompt: str) -> int:
    """DJB2 hash over provider, model, prompt with NUL separators.

    Byte-for-byte equivalent to ``llm_hash_key`` in the C runtime:

      h = 5381
      for byte in provider:  h = (h * 33) ^ byte
      h = (h * 33) ^ 0         # NUL separator (no-op for XOR but
                                 # multiplies the running hash)
      for byte in model:     h = (h * 33) ^ byte
      h = (h * 33) ^ 0
      for byte in prompt:    h = (h * 33) ^ byte

    All arithmetic is masked to 64-bit unsigned so the wrap-around
    matches C's ``uint64_t``.
    """
    h = 5381
    for b in provider.encode("utf-8"):
        h = ((h * 33) ^ b) & _UINT64_MASK
    h = (h * 33) & _UINT64_MASK  # XOR with 0 is identity
    for b in model.encode("utf-8"):
        h = ((h * 33) ^ b) & _UINT64_MASK
    h = (h * 33) & _UINT64_MASK
    for b in prompt.encode("utf-8"):
        h = ((h * 33) ^ b) & _UINT64_MASK
    return h


def mochi_llm_generate(provider: str, model: str, prompt: str) -> str:
    """Cassette-mode LLM generation.

    Reads ``$MOCHI_LLM_CASSETTE_DIR/<hash>.txt`` and returns its
    contents with a single trailing newline stripped (the C runtime
    does the same; text editors append a trailing newline by default
    and that newline is not part of the recorded response).

    Returns an empty string on any error so the calling Mochi program
    can continue. The C runtime prints a diagnostic on stderr in the
    same shape, so we match here for parity.
    """
    cassette_dir = os.environ.get("MOCHI_LLM_CASSETTE_DIR", "")
    if not cassette_dir:
        sys.stderr.write(
            "mochi_llm_generate: live providers not supported in Phase 13.0; "
            "set MOCHI_LLM_CASSETTE_DIR to a cassette directory\n"
        )
        return ""

    key = _llm_hash_key(provider, model, prompt)
    path = os.path.join(cassette_dir, f"{key}.txt")
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError:
        sys.stderr.write(f"mochi_llm_generate: cassette not found: {path}\n")
        return ""

    if data.endswith(b"\n"):
        data = data[:-1]
    return data.decode("utf-8")
