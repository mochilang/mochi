---
title: MEP-71 Phase 12 (Async bridge)
sidebar_position: 13
sidebar_label: "Phase 12. Async bridge"
description: "MEP-71 Phase 12: async-fn -> sync-fn shim renderer that bridges Mochi's eager evaluation model and Python's asyncio coroutines."
---

# MEP-71 Phase 12. Async bridge

Status: **LANDED (pending merge)** as of 2026-05-30 00:45 (GMT+7). Implements the async-fn shim renderer: for every imported `async def f(...) -> T`, the bridge emits a synchronous `f_sync(...) -> T` shim that drives the coroutine to completion. Two loop modes are supported (per-call `asyncio.run`, persistent cached loop) and a cross-loop hazard guard converts the asyncio nesting RuntimeError into a clear `MochiAsyncReentryError`.

## Gate

The umbrella sentinel `TestPhase12AsyncBridge` in `package3/python/asyncbridge/phase12_test.go` is green. The sentinel:

- Builds two `Module`s (one PerCall, one Persistent) carrying the same two `AsyncFn` descriptors (`fetch(url: str, timeout: float) -> bytes` and `compute(n: int) -> int`).
- Renders both and asserts the preamble carries `from __future__ import annotations` + `import asyncio`, the source import is re-emitted so the shims can call the async fns, and both variants embed the `MochiAsyncReentryError` class + the `_mochi_check_no_running_loop` guard.
- Asserts every shim signature carries the Mochi-side annotations (`def fetch_sync(url: str, timeout: float) -> bytes:`).
- Asserts the PerCall driver uses `asyncio.run(...)` per call and does NOT declare `_MOCHI_LOOP`; the Persistent driver uses `_mochi_get_loop().run_until_complete(...)` and declares the `_MOCHI_LOOP` cache + getter; neither variant invokes the other's primitive.
- Asserts every shim invokes `_mochi_check_no_running_loop("<sync-name>")` so the user-facing error names the shim that tripped the guard.
- Asserts `Render` is deterministic (re-rendering produces byte-identical output) and rejects invalid `AsyncFn` descriptors.
- Asserts `Mode.String` ↔ `ParseMode` round-trips for both modes.

Plus 23 unit tests (`go test ./package3/python/asyncbridge/... -count=1`) covering:

- `ParseMode`: accepts `""` / `"per-call"` / `"percall"` → `PerCall` and `"persistent"` → `Persistent`; rejects everything else (`"PerCall"`, `"Persistent"`, `"asyncio.run"`, `"loop"`, `"foo"`).
- `Mode.String`: canonical token for each known mode; fallback `Mode(99)` returns a non-empty diagnostic string.
- `Mode` round-trip: every known mode survives `ParseMode(String())`.
- `AsyncFn.Validate`: accepts the happy path; rejects empty `Name`, empty `SyncName`, `SyncName == Name`, parallel-list length mismatch (`ParamNames` vs `ParamTypes`), empty param name, empty param type, empty `Return`.
- `DefaultSyncName`: appends `_sync` to the source name.
- `RenderShim` (PerCall): emits the right signature + `_mochi_check_no_running_loop` guard + `asyncio.run` driver; does not reference `_mochi_get_loop`.
- `RenderShim` (Persistent): emits `_mochi_get_loop().run_until_complete(...)`; does not reference `asyncio.run`.
- `RenderShim` (zero params): handles the empty signature correctly (`def ping_sync() -> None:` + `asyncio.run(ping())`).
- `RenderShim` panics on `AsyncFn{}` — invalid descriptors must surface at the call site, not produce broken Python.
- `Module.Render` (PerCall): emits preamble + helper + every shim; does NOT emit `_MOCHI_LOOP`.
- `Module.Render` (Persistent): emits the `_MOCHI_LOOP` cache + `_mochi_get_loop` getter + persistent driver.
- `Module.Render` (empty fns): still emits asyncio import + cross-loop helper so the module is importable even when surface is empty.
- `Module.Render` propagates `AsyncFn.Validate` errors instead of producing broken source.
- `Module.Render` is deterministic.
- `Module.Render` normalises a missing trailing newline on `SourceImport`.

## Files

- `package3/python/asyncbridge/doc.go` — package overview (per-call vs persistent trade-off, cross-loop hazard rationale, forward to Phase 17 for free-threaded GIL handling and sub-phase 12.3 for cancellation/timeout propagation).
- `package3/python/asyncbridge/mode.go` — `Mode` enum, `ParseMode`, `String` for the `[python] runtime.event-loop` knob.
- `package3/python/asyncbridge/fn.go` — `AsyncFn` descriptor (Name / SyncName / ParamNames / ParamTypes / Return), `Validate`, `DefaultSyncName`.
- `package3/python/asyncbridge/render.go` — `CrossLoopHelper` source, `PersistentLoopGetter` source, `RenderShim`, `Module`, `Module.Render`.
- `package3/python/asyncbridge/mode_test.go` — `ParseMode` accept / reject + `String` + round-trip (4 cases).
- `package3/python/asyncbridge/fn_test.go` — `AsyncFn.Validate` happy + 7 error paths + `DefaultSyncName` (9 cases).
- `package3/python/asyncbridge/render_test.go` — `RenderShim` + `Module.Render` shape, determinism, error propagation, source-import normalisation (10 cases).
- `package3/python/asyncbridge/phase12_test.go` — Phase 12 umbrella sentinel.

## Sub-phase decomposition

Phase 12 ships the source-level shim renderer. Wiring the renderer into the wrapper synthesiser (so end-to-end `import python "<pkg>"` for async surfaces produces a runnable shim), free-threaded GIL handling, and cancellation/timeout propagation are deferred sub-phases so the umbrella gate stays focused on the renderer contract.

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 12 | Shim renderer (Mode + AsyncFn + RenderShim + Module.Render + cross-loop guard) | LANDED (pending merge) | This PR. |
| 12.1 | Wire asyncbridge into wrapper synthesiser (Phase 5) so async surfaces produce sync shims | NOT STARTED | Touch `package3/python/wrapper/` to invoke `Module.Render` for every `async def` discovered in `.pyi` ingest. |
| 12.2 | Free-threaded GIL handling (Phase 17 forward — `PyMutex` around the persistent loop cache when running on `cp3XYt`) | NOT STARTED | Tracked separately because Phase 17 owns free-threaded ABI tags + runtime selection. |
| 12.3 | Cancellation + timeout propagation (`mochi_async_timeout` ContextVar -> `asyncio.wait_for`) | NOT STARTED | Needs design alignment with Mochi's `cancel`/`timeout` syntax (MEP-51 Phase 11 deferred). |

## Fixtures

Phase 12 is source-renderer-only and does not exercise the fixture corpus. Sub-phase 12.1 (wrapper wire-up) will assert golden async shim counts against `httpx`, `aiohttp`, `fastapi`, `starlette`, and `uvicorn` from the corpus.

## Skip count

N/A. Phase 12 has no `SkipReport` surface; invalid `AsyncFn` descriptors are rejected by `AsyncFn.Validate()` and surfaced through `Module.Render` errors before any Python source is produced.

## Cross-references

- [MEP-71 spec §7 "Async bridge"](/docs/mep/mep-0071) for the normative two-mode design + cross-loop hazard guard requirement.
- [Phase 5](./phase-05-wrapper) for the wrapper synthesiser that sub-phase 12.1 wires into.
- [Phase 17](./phase-17-free-threaded) for the free-threaded ABI + PyMutex coverage sub-phase 12.2 depends on.
- [MEP-51 implementation tracking](/docs/implementation/0051) for the Python transpiler context (async colour pass deferred there).
- [PEP 3156 (asyncio)](https://peps.python.org/pep-3156/) and the CPython [asyncio.run docs](https://docs.python.org/3/library/asyncio-runner.html) for the underlying loop semantics.
