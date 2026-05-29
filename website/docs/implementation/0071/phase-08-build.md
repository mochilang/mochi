---
title: "MEP-71 Phase 8: build orchestration"
sidebar_label: "Phase 8. Build orchestration"
sidebar_position: 9
description: "Phase 8 of MEP-71. Composes Phase 3 / 5 / 6 / 7 into an Orchestrator that lays a python_workspace out under the driver's work-dir."
---

# Phase 8: build orchestration

Phase 8 introduces `package3/python/build.Orchestrator`, the composition layer that ties Phase 3 (stub ingest) -> Phase 5 (wrapper synthesis) -> Phase 6 (shim emit) into a single Build call, then materialises the result under the Phase 0 Driver's work-dir.

The phase ships the workspace synthesis side. The wheel-install loop (driving uv from Phase 2 against the rendered pyproject.toml) and the libpython link (the cgo side of the embedded runtime) decompose into sub-phases 8.1 and 8.2; both are tracked separately and ship after the orchestrator stabilises.

## Gate

`go test ./package3/python/build/...` is green. The gate covers:

- `Plan(req)` validates the request without touching the filesystem: rejects empty Targets, empty Alias, empty Spec.Name, duplicate aliases, and a nil Driver.
- `Plan(req)` populates `Result.Wrappers` and `Result.Shims` from `wrapper.Synthesise` and `emit.EmitShim`, propagating both `WrapperOpts` (`AllowPartial`) and `ShimOpts` (`Header`, `IncludeSkipReport`) into the per-target pipeline.
- `Plan(req)` augments the Venv with one `MemberWrapper` per target and adds the upstream PyPI distribution to `[project.dependencies]` with the PEP 440 specifier when the spec source is `SourceRegistry` or `SourceIndex`; git and local-path sources record only the bare name (the version is satisfied by the install side configured later).
- `Build(req)` runs Plan, then writes the workspace under the Driver's work-dir:
  - `<work-dir>/python_workspace/pyproject.toml` (deterministic TOML).
  - `<work-dir>/python_workspace/.gitignore` (`.venv/ __pycache__/ *.pyc`).
  - `<work-dir>/python_workspace/_mochi_wrap.py` plus `_mochi_wrap.pyi` (the shared runtime module).
  - `<work-dir>/python_workspace/python_wrap/<alias>/__init__.py`, `<pkg>_externs.py`, `<pkg>_externs.pyi`, `<pkg>_shim.mochi` per target.
  - `SKIPPED.txt` per target when the wrapper refused at least one item.
- The Build result returns the sorted `WrittenFiles` list, so callers can pipe the artifact set into the Phase 9 lockfile hash and the Phase 10 emit pass.
- Determinism: re-running Build with the same Request produces the same shim SHA-256 and the same shim Source bytes across calls.
- `sharedDepVersion` returns the PEP 440 specifier for registry / index sources and an empty string for git / path sources.
- Sentinel `TestPhase8BuildOrchestration` walks a two-target Request (`httpx@>=0.27,<0.30` aliased `httpx`, bare `util` aliased `u`) end-to-end and asserts the venv root, every per-target file, the sync extern `get`, the async `fetch_sync`, and the dependency entries.

## Files

- `package3/python/build/orchestrator.go` — `Target`, `Request`, `Result`, `Orchestrator`, `Plan`, `Build`, `initPyTemplate`, `renderSkipped`, `sharedDepVersion`.
- `package3/python/build/orchestrator_test.go`, `phase08_test.go` — ~20 tests + the umbrella sentinel.
- Pre-existing `package3/python/build/driver.go`, `venv.go` are unchanged; Phase 8 builds on top of them.

## Fixtures

The unit tests use synthetic `.pyi` fragments crafted to exercise the gate above. Corpus-wide validation against the 25-package fixture set lands with Phase 8.1 (wheel install) once the orchestrator can actually invoke uv against the rendered pyproject.toml and observe round-trip behaviour against real distributions.

## Sub-phase decomposition

Phase 8 splits into three sub-phases per the umbrella-phase coverage rule:

| Sub-phase | Scope | Status |
|-----------|-------|--------|
| 8 | Workspace synthesis: Spec list -> python_workspace tree on disk | LANDED |
| 8.1 | Wheel install: drive uv against the rendered pyproject.toml; populate `.venv/` | NOT STARTED |
| 8.2 | libpython link: cgo embed of CPython into the Mochi binary for `runtime-mode = "embedded"` | NOT STARTED |

Sub-phases 8.1 and 8.2 are tracked alongside Phase 9 (lockfile) since the install loop and the lock hash are the same artifact from different angles.

## Skip count

Phase 8 forwards every Phase 5 / Phase 4 refusal into `Result.Skipped` and writes a `SKIPPED.txt` next to each affected wrapper. The expected SkipReport count per fixture is the union of every Target's wrapper Skipped slice and remains identical to Phase 5's per-package counts.

## Notes

- The orchestrator is filesystem-only and does not invoke any subprocess. This keeps the Phase 8 gate independent of `python3` / `uv` availability in CI. Sub-phases 8.1 and 8.2 introduce the subprocess invocations, gated by the standard build matrix.
- Each wrapper alias lives at `python_wrap/<alias>/` rather than `python_wrap/<distribution>/`. The alias is what the user typed in `import python "<spec>" as <alias>`, so two imports of the same distribution under different aliases produce two wrapper modules with independent surfaces. The Phase 9 lockfile keys on `(distribution, alias)`.
- `__init__.py` re-exports the externs module under the name `externs` so importing code can write `from python_wrap.<alias> import externs`. The Mochi-side shim references the wrapper module directly by its file path; the `__init__.py` exists for Python tooling and editor introspection.

## Timestamps

- 2026-05-30 00:04 (GMT+7): Phase 8 started.
- 2026-05-30 00:10 (GMT+7): Phase 8 LANDED.
