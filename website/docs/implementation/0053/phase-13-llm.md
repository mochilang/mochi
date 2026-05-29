---
title: "Phase 13. LLM generate"
sidebar_position: 15
sidebar_label: "Phase 13. LLM"
description: "MEP-53 Phase 13, generate <provider> { ... } lowered to cassette replay keyed by SHA-256 of provider:prompt."
---

# Phase 13. LLM generate with cassette replay

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22604](https://github.com/mochilang/mochi/issues/22604) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 819cb38daa |

## Gate

`TestPhase13LLM` walks `tests/transpiler3/rust/fixtures/phase13-llm/` (11 fixture subdirectories) and asserts byte-equal stdout. Each fixture is its own subdirectory with a `cassette/` folder containing `{sha256_hex(provider:prompt)}.txt` files; the test sets `MOCHI_LLM_CASSETTE_DIR` to that path before running the binary. Coverage: `generate openai`, `generate anthropic`, `generate llama`, model-override variant, multi-step prompts, generate-in-fun, generate-in-var, prompt concatenation.

## Lowering decisions

```mochi
let r = generate openai {
    prompt: "hello"
    model: "gpt-4o"
}
```

lowers to:

```rust
let r: String = mochi_runtime::llm::call("openai", "hello");
```

The model field is currently ignored in the runtime (it would be sent over the wire to the provider's API, which is itself out of scope for the cassette-replay-only mode). The provider name is folded into the cassette key alongside the prompt.

`mochi_runtime::llm::call(provider, prompt)`:

1. Reads `MOCHI_LLM_CASSETTE_DIR` from env. Raises panic code 99 if unset.
2. Computes `key = format!("{}:{}", provider, prompt)`.
3. Hashes `key` with the inline SHA-256 implementation (avoids the `sha2` crate dep).
4. Reads `{dir}/{hex(hash)}.txt`. Raises panic code 99 if missing.
5. Trims trailing whitespace (matching vm3 behavior) and returns.

Live calls to actual provider APIs are out of scope for MEP-53. The cassette-replay design lets the fixtures be fully reproducible without network access; a future sub-phase could add a live mode gated by `MOCHI_LLM_LIVE=1` for development.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/llm.go` | generate expression lowering |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `llm` module + inline SHA-256 |
| `transpiler3/rust/build/phase13_test.go` | 11-fixture-subdir gate, sets MOCHI_LLM_CASSETTE_DIR |
| `tests/transpiler3/rust/fixtures/phase13-llm/*/{*.mochi,*.out,cassette/*.txt}` | 11 fixtures |

## Test set

- `TestPhase13LLM/<fixture>` for each subdirectory (11 fixtures).

## Closeout notes

The choice to inline SHA-256 (~80 LOC) rather than pull in the `sha2` crate has two motivations: (1) it keeps the runtime crate dep-free past `itertools` for fast cargo builds, (2) it lets the embedded feature gate work cleanly — sha2 brings in `cpufeatures` which doesn't compile on no_std without alloc + portable-atomic. The inline implementation is the classic FIPS 180-4 algorithm with K constants and round function inlined; performance is irrelevant because the hash runs once per LLM call.

The trim-trailing-whitespace step matches vm3's behavior: vm3 strips trailing `\n` from cassette files when reading. Without this, a Mochi `print(generate ...)` would emit the LLM response plus an extra blank line.
