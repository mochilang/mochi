---
title: MEP-71 Phase 10 (TargetPythonPackage emit)
sidebar_position: 11
sidebar_label: "Phase 10. TargetPythonPackage emit"
description: "MEP-71 Phase 10: render a Mochi-side library as a PEP 517 source distribution + wheel + bundled mochi_build backend + downstream .pyi."
---

# MEP-71 Phase 10. TargetPythonPackage emit

Status: **LANDED (pending merge)** as of 2026-05-30 00:27 (GMT+7). Implements the publish-direction artifact composer that turns a Mochi-side library into a PEP 517 source distribution, a pure-Python wheel, the bundled `mochi_build` PEP 517 backend, and a typed `.pyi` for downstream consumers.

## Gate

The umbrella sentinel `TestPhase10TargetPythonPackage` in `package3/python/pypackage/phase10_test.go` is green. The sentinel:

- Builds a `Package` carrying a frozen record (`Point` with two int fields), an async function (`fetch(string) -> Awaitable[string]`), and a scalar module-level constant (`VERSION: string`).
- Validates the package via `Package.Validate()` (PEP 503 distribution name, PEP 440 version, non-empty Summary/License, Python-identifier export names).
- Renders both layouts via `RenderSdist(p)` and `RenderWheel(p)`.
- Writes each layout to a `t.TempDir()` subdirectory via `Layout.Write(dst)` (mkdirall + 0644 file writes, sorted output).
- Asserts the sdist contains `pyproject.toml`, `PKG-INFO`, `README.md`, `<module>/__init__.py`, `<module>/__init__.pyi`, and `mochi_build/__init__.py` (the bundled PEP 517 backend).
- Asserts the wheel contains `<module>/__init__.py`, `<module>/__init__.pyi`, `<dist>-<ver>.dist-info/METADATA`, `WHEEL`, and `RECORD`.
- Asserts the `.pyi` re-emits the frozen record as a Python class, the async function as `def fetch(arg0: str) -> Awaitable[str]: ...`, the constant as `VERSION: str`, and pulls in the `Awaitable` typing import.
- Asserts the wheel `RECORD` carries `sha256=` lines for every artifact except RECORD itself (which is listed with empty hash and size per PEP 376).
- Asserts the bundled `mochi_build/__init__.py` exposes `build_wheel`, `build_sdist`, and dispatches to `mochi pkg build`.

Plus 38 unit tests (`go test ./package3/python/pypackage/... -count=1`) covering:

- PEP 503 distribution-name normalisation (`-`/digit/lowercase only, no leading or trailing `-`).
- PEP 426 module-name fallback (`mochi-httpx` -> `mochi_httpx`) plus an explicit `Module` override.
- `Validate()` error paths for empty Distribution / Version / Summary / License and for non-identifier export names.
- `PyprojectTOML` core fields and conditional omission of HomePage / authors / requires-python / dependencies.
- `PKGInfo` METADATA 2.1 headers (Name / Version / Summary / Home-page / Author / License / Requires-Python / Requires-Dist).
- `WheelMetadata` PEP 427 marker (Wheel-Version 1.0, Tag py3-none-any, Root-Is-Purelib true).
- `InitPy` re-exports `mochi_runtime` and populates `__all__` in sorted export order.
- `InitPYI` rendering for func / record / interface / constant exports (sync + async methods, empty record + empty Protocol bodies).
- `pyAnnotation` for every `typemap.Kind`: scalars, list/set/map/tuple, Optional, Sum, Async, Stream, Callable, named record/interface (and `Any` fallback when name is empty), Ref, TypeVar, and the unknown-kind fallback.
- `pyImportsFor` minimal `from typing import ...` set computed by walking exports (Optional, Union, Awaitable, AsyncIterator, Callable, Protocol, Any).
- `MochiBuildBackend` carries `build_wheel`, `build_sdist`, `prepare_metadata_for_build_wheel`, and shells to `mochi pkg build`.
- `RecordLine` rendering (empty self-line vs hashed line) and `HashBody` determinism + base64-urlsafe-no-padding shape.
- `Layout.Add` overwrite semantics, sorted `Paths()`, `Write` mkdirall + empty-destination rejection.
- `RenderSdist` artifact set, README content, module-override propagation.
- `RenderWheel` artifact set, RECORD line format, RECORD determinism across two renders.

## Files

- `package3/python/pypackage/doc.go` — package overview.
- `package3/python/pypackage/package.go` — `Package`, `Export`, `ExportKind`, `Validate`, `ModuleName`, PEP 503 / identifier validators.
- `package3/python/pypackage/layout.go` — `Layout` (flat path -> body map) with `Add`, sorted `Paths`, `Write`.
- `package3/python/pypackage/render.go` — `PyprojectTOML`, `PKGInfo`, `WheelMetadata`, `InitPy`, `InitPYI`, `pyAnnotation`, `pyImportsFor`, `MochiBuildBackend`, `RecordEntry` / `RecordLine` / `HashBody`.
- `package3/python/pypackage/sdist.go` — `RenderSdist(p) (*Layout, error)`.
- `package3/python/pypackage/wheel.go` — `RenderWheel(p) (*Layout, error)` + RECORD synthesis.
- `package3/python/pypackage/package_test.go` — `Package` validators (12 cases).
- `package3/python/pypackage/layout_test.go` — `Layout` mechanics (4 cases).
- `package3/python/pypackage/render_test.go` — renderers + type lowering (18 cases).
- `package3/python/pypackage/sdist_test.go` — sdist composition (4 cases).
- `package3/python/pypackage/wheel_test.go` — wheel composition + RECORD (4 cases).
- `package3/python/pypackage/phase10_test.go` — umbrella sentinel.

## Sub-phase decomposition

Phase 10 ships the in-memory artifact composer only. The actual archive packing (sdist `.tar.gz` and wheel `.whl` zip) and the surrounding CLI surface land as follow-ups so the umbrella gate stays subprocess-free and deterministic:

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 10 | Layout + renderers + `mochi_build` backend | LANDED (pending merge) | This PR. |
| 10.1 | Sdist tar.gz packer (deterministic mtimes, uid/gid 0) | NOT STARTED | Reproducible tarball + ZIP64-aware wheel packer. |
| 10.2 | `mochi pkg build --target=python-package` CLI verb | NOT STARTED | Drives `RenderSdist` / `RenderWheel`, copies output to `--out=<dir>`. |
| 10.3 | Mochi -> `Package` lowering | NOT STARTED | Walks a Mochi module's public surface and produces the `Package` value Phase 10 consumes. |

## Fixtures

Phase 10's surface is a renderer over the closed Phase 4 type table; it does not consume the 25-package corpus directly. The 10.3 sub-phase will exercise the fixture corpus when the Mochi-side surface walker lands.

## Skip count

N/A. Phase 10 has no `SkipReport` surface; non-renderable surfaces are rejected by `Validate()` before any artifact is touched.

## Cross-references

- [MEP-71 spec §10 "Publish direction"](/docs/mep/mep-0071) for the normative artifact schema.
- [Phase 8](./phase-08-build) for the consume-direction workspace synth that already covers `mochi_runtime` packaging.
- [Phase 9](./phase-09-lockfile) for the `wrapper-sha256` pin Phase 10 will eventually surface in the published `.dist-info`.
- [Phase 11](./phase-11-trusted-publish) for the PyPI upload + Sigstore / PEP 740 attestation pipeline that consumes Phase 10's output.
