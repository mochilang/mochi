---
title: "MEP-71 Phase 6: Mochi extern fn emitter"
sidebar_label: "Phase 6. Extern emit"
sidebar_position: 7
description: "Phase 6 of MEP-71. Renders the Mochi `<pkg>_shim.mochi` shim from the Phase 5 wrapper Items list."
---

# Phase 6: Mochi extern fn emitter

Phase 6 produces the Mochi-side counterpart to the Phase 5 Python wrapper. It takes the `wrapper.Wrapper.Items` list and emits a Mochi source file (`<package>_shim.mochi`) the importing program loads as if hand-written, plus a content-hash that the lockfile pins (`wrapper-sha256`).

## Gate

`go test ./package3/python/emit/...` is green. The gate covers:

- The `EmitShim` entry point: validates a non-nil wrapper with a non-empty package, computes deterministic output, and reports a SHA-256 over the rendered Source.
- Header banner: emits an auto-gen comment carrying the generator identifier, source package name, item count, skip count, and async loop mode. Can be suppressed via `Options.Header = false`.
- Function emit: `extern python fun <name>(P1, P2): R`, with the async wrapper rendering `: async R` per Phase 4's lowering of `Awaitable[T]`.
- Record emit: typed `type <Name> = { field1: t1, field2: t2 }` Mochi record alias when the wrapper resolved a structural Fields list; field-less records (and any record where Phase 4 left the body opaque) surface as `extern python type <Name>` instead.
- Interface emit: `extern python type <Name>` (Protocol semantics travel through the wrapper at call time; the `.pyi` carries the structural shape downstream typing needs).
- Constant emit: `extern python var <NAME>: <type>`.
- Grouping: records, opaque externs, constants, functions. Within each group, items appear in `SourceName` order.
- Skip report: trailing `// SKIPPED items` block lists the Phase 5 / Phase 4 refusals with reason tokens; suppressible via `Options.IncludeSkipReport = false`.
- Determinism: same Wrapper + Options produces the same Source and SHA-256 across calls.
- Sentinel `TestPhase6ExternEmit` walks a representative `.pyi` end-to-end through `stubs.ParsePYI` -> `wrapper.Synthesise` -> `EmitShim` and asserts the emitted shim contains the expected typed record, opaque interface, extern var, async extern fn, and skip line.

## Files

- `package3/python/emit/doc.go` — package overview.
- `package3/python/emit/shim.go` — `Shim`, `Decl`, `DeclKind`, `Options`, `EmitShim` entry point + per-kind renderers (function, record, interface, constant).
- `package3/python/emit/shim_test.go`, `phase06_test.go` — ~25 tests covering the gate above.

## Fixtures

The unit tests construct `Wrapper.Items` values directly and round-trip a representative `.pyi` through the Phase 3 -> 5 -> 6 pipeline. Corpus-wide validation against the 25-package fixture set lands with Phase 8 (build orchestration), where the shim is exercised against actual MEP-51 build flows.

## Skip count

Phase 6 does not introduce its own refusal cases; it forwards the Phase 4 / Phase 5 `Wrapper.Skipped` slice into the trailing comment block. The expected SkipReport count per fixture is therefore identical to Phase 5's.

## Notes

- `extern python type` is the opaque marker form for both Protocols and structure-less records. The MEP-71 §3 type table notes that the wrapper carries dispatch and the `.pyi` carries the structural surface; the Mochi side does not need a structural form here.
- The shim does not emit `import python ...` (Phase 7's surface). The importing program continues to use a regular `import "./python_wrap/<pkg>_shim.mochi" as <alias>` until Phase 7 lands the dedicated syntactic form.

## Timestamps

- 2026-05-29 23:50 (GMT+7): Phase 6 started.
- 2026-05-29 23:56 (GMT+7): Phase 6 LANDED.
