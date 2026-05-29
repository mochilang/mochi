---
title: "MEP-71 Phase 5: wrapper synthesiser"
sidebar_label: "Phase 5. Wrapper synthesiser"
sidebar_position: 6
description: "Phase 5 of MEP-71. Synthesises <pkg>_externs.py + .pyi from a typed ModuleSurface and a closed type-mapping pass."
---

# Phase 5: wrapper synthesiser

Phase 5 produces the Python-side bridge code for one consumed PyPI package: a `<pkg>_externs.py` shape-coercing wrapper plus a matching `<pkg>_externs.pyi` stub. The synthesiser walks the typed `ModuleSurface` from Phase 3 and the closed type-mapping pass from Phase 4. Items the type table refuses appear in `Wrapper.Skipped` and a generated `SKIPPED.txt`. A shared `_mochi_wrap.py` runtime helper module accompanies every wrapper.

## Gate

`go test ./package3/python/wrapper/...` is green. The gate covers:

- The `Synthesise` entry point: validates package name as a Python identifier, requires a non-nil surface, emits a deterministic wrapper.
- Function lowering: positional `arg0, arg1, ...` shim that forwards to `_src.<name>(...)`. Async functions produce a synchronous `<name>_sync` entry that runs through `_run_async` (per-call mode, default) or `_MOCHI_LOOP.run_until_complete(...)` (persistent mode, opt-in), plus an `async def <name>_async` direct entry.
- Record lowering: TypedDict + frozen `@dataclass` re-export the source class and emit a `_<Name>_to_mochi_dict` companion that walks every field via `getattr` + `_to_mochi_dict`. Mutable `@dataclass` and plain classes are refused with the Phase 4 override hint propagated.
- Interface lowering: Protocol classes re-export the source class verbatim; the `.pyi` mirrors the method set.
- Constant lowering: re-exports the source attribute and emits a typed stub entry. Unannotated constants are refused.
- Privacy: leading-underscore names (other than dunders) are skipped with `SkipPrivateName`.
- Refusal qualification: every `SkipReport.ItemPath` is rewritten to `<package>.<item>` so the `SKIPPED.txt` output groups by package.
- Deterministic ordering: Items are sorted by source name.
- Renderer correctness: `renderPyAnno` walks every `MochiType` Kind, including nested `dict[str, list[Optional[int]]]` shapes, and falls back to `Any` on `KindUnknown`.
- Helper imports: every wrapper imports `_to_mochi_dict`; async wrappers add `_run_async`; persistent-loop wrappers add `_persistent_loop`.
- Sentinel `TestPhase5WrapperSynthesiser` walks a representative `.pyi` end-to-end including the private + complex refusal subcases.

## Files

- `package3/python/wrapper/doc.go` — package overview.
- `package3/python/wrapper/wrapper.go` — `Wrapper`, `Item`, `Options`, `EventLoopMode` type set.
- `package3/python/wrapper/synth.go` — `Synthesise(pkg, surface, opts)` entry point + privacy / refusal qualification.
- `package3/python/wrapper/render.go` — `<pkg>_externs.py` + `<pkg>_externs.pyi` renderers; `renderPyAnno` (MochiType → Python annotation).
- `package3/python/wrapper/runtime.go` — `Runtime()` + `RuntimeStub()` returning the shared `_mochi_wrap.py` + `.pyi` text.
- `package3/python/wrapper/synth_test.go`, `render_test.go`, `phase05_test.go` — ~50 tests covering the gate above.

## Fixtures

The unit tests construct `ModuleSurface` values directly and also round-trip representative `.pyi` source via `stubs.ParsePYI`. The 25-package corpus run is staged for Phase 6 (extern emit), where the typed wrapper Items can be exercised end-to-end against actual PyPI wheels.

## Skip count

The Phase 5 surface is a pure projection of Phase 4 decisions plus three Phase-5-specific refusals (private name, unannotated constant, qualification rewrite). The expected SkipReport count per fixture package is therefore `phase4_skips + phase5_private_count` and is captured in the sentinel `TestPhase5WrapperSynthesiser`.

## Notes

- The synthesiser is deliberately Python-source-only at this phase. The CPython extension (`.so`) and the libpython link step land in Phase 8 (build orchestration); Phase 5 stops at the `<pkg>_externs.py` + `<pkg>_externs.pyi` Python source pair.
- `Iterator[T] → list<T>` is lowered by Phase 4; the wrapper currently leaves the lazy iterator companion off (the helper exists in `_mochi_wrap.py` as `_materialise_iter` for Phase 6 to wire in if a fixture demands lazy access).

## Timestamps

- 2026-05-29 23:35 (GMT+7): Phase 5 started.
- 2026-05-29 23:48 (GMT+7): Phase 5 LANDED.
