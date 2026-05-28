---
title: "Phase 13. LLM (cassette playback)"
sidebar_position: 17
sidebar_label: "Phase 13. LLM"
description: "MEP-49 Phase 13 — @llm lowering to mochiLLMGenerate; DJB2 XOR cassette lookup from MOCHI_LLM_CASSETTE_DIR; synchronous string return."
---

# Phase 13. LLM (cassette playback)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 13](/docs/mep/mep-0049#phase-13-llm) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 14:15 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13LLM`: 5 fixtures green on Swift 6.0+, macOS 15, with cassette playback via `MOCHI_LLM_CASSETTE_DIR`. Gate builds each fixture, sets the cassette dir env var, runs the binary, and compares stdout to `.out`.

## Goal-alignment audit

LLM calls are non-deterministic at runtime, so the gate uses pre-recorded cassette files (matching MEP-46 Phase 13 for the BEAM backend). The v1 implementation is synchronous: `mochiLLMGenerate` reads a `.txt` cassette file keyed by a DJB2 XOR hash of `provider + "\0" + model + "\0" + prompt`. This matches the BEAM backend's cassette format exactly. Apple `FoundationModels` on-device inference and cloud fallback are deferred.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | `LLMExpr` → `mochiLLMGenerate(provider, model, prompt)` | LANDED | mep/0049-phase-13 |
| 13.1 | Cassette lookup: DJB2 XOR hash → `MOCHI_LLM_CASSETTE_DIR/<hash>.txt` | LANDED | mep/0049-phase-13 |
| 13.2 | Apple `FoundationModels` on-device inference path | DEFERRED | — |
| 13.3 | Cloud fallback via URLSession for non-Apple platforms | DEFERRED | — |
| 13.4 | Structured output: `@Generable` protocol + `@Guide` annotations | DEFERRED | — |

## Sub-phase 13.0 -- LLM expression lowering

### Decisions made (13.0)

**`LLMExpr`**: the aotir IR node for an LLM call, carrying `Provider`, `Model`, and `Prompt` fields.

**Lowering**: the lowerer emits:

```swift
mochiLLMGenerate("anthropic", model, prompt)
```

as a `RawSwiftExpr`. The provider string is embedded as a string literal; model and prompt are lowered expressions.

**Return type**: `String`. The function is synchronous; no `async`/`await` needed.

## Sub-phase 13.1 -- Cassette lookup

### Decisions made (13.1)

**Hash function**: DJB2 XOR variant: `h = (h &* 33) ^ UInt64(byte)`, initialised at `5381`, over the UTF-8 bytes of `provider + "\0" + model + "\0" + prompt`. This matches the BEAM backend's cassette hash exactly, so cassette files are portable between backends.

**Cassette file path**: `$MOCHI_LLM_CASSETTE_DIR/<hash>.txt`. The file contains the raw LLM response text (no JSON envelope). The result is trimmed of trailing newlines before being returned.

**`mochiLLMGenerate` implementation**:

```swift
public func mochiLLMGenerate(_ provider: String, _ model: String, _ prompt: String) -> String {
    if let cassetteDir = ProcessInfo.processInfo.environment["MOCHI_LLM_CASSETTE_DIR"] {
        let key = mochiDJB2Key(provider, model, prompt)
        let path = "\(cassetteDir)/\(key).txt"
        if let content = try? String(contentsOfFile: path, encoding: .utf8) {
            return content.trimmingCharacters(in: .newlines)
        }
        fputs("mochi_llm: cassette not found: \(path)\n", stderr)
        return ""
    }
    fputs("mochi_llm: MOCHI_LLM_CASSETTE_DIR not set\n", stderr)
    return ""
}
```

**Fixture structure**: each LLM fixture is a subdirectory (not a flat `.mochi` file) containing `<name>.mochi`, `<name>.out`, and a `cassette/` subdirectory with `<hash>.txt` files.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `LLMExpr` lowering to `mochiLLMGenerate(provider, model, prompt)` |
| `transpiler3/swift/runtime/Sources/MochiRuntime/LLM.swift` | `mochiLLMGenerate`, `mochiDJB2Key` |
| `transpiler3/swift/build/phase13_test.go` | `TestPhase13LLM`: 5 fixtures with cassette playback |
| `tests/transpiler3/swift/fixtures/phase13-llm/` | 5 fixture subdirectories (each with `cassette/` dir) |

## Test set

- `TestPhase13LLM` -- 5 fixtures: `generate_anthropic`, `generate_concat`, `generate_in_var`, `generate_multiple`, `generate_text`.

## Deferred work

- Apple `FoundationModels` on-device inference (`LanguageModelSession`, `#if canImport(FoundationModels)`). Deferred to Phase 13.2.
- Cloud fallback via `URLSession` for Linux/Windows. Deferred to Phase 13.3.
- Structured output: `@Generable` macro + `@Guide` field annotations. Deferred to Phase 13.4.
- Streaming LLM responses (`@llm(stream: true)`). Deferred.
- Multi-turn conversation history. Deferred.
