---
title: "Phase 15. Wheel + sdist"
sidebar_position: 20
sidebar_label: "Phase 15. Wheel + sdist"
description: "MEP-51 Phase 15 -- TargetPythonWheel produces a self-contained PEP 427 wheel and TargetPythonSdist produces a PEP 517 sdist; both built via stdlib zip/tar with no external build backend; mochi_runtime is bundled so installs need no PyPI fetch; wheel is byte-deterministic across rebuilds."
---

# Phase 15. Wheel + sdist

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 15](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (15.0 only; uv/build-backend pluggability DEFERRED) |
| Started        | 2026-05-29 20:22 (GMT+7) |
| Landed         | 2026-05-29 20:29 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase15WheelSdist` ships three sub-gates in `transpiler3/python/build/phase15_test.go`:

1. `wheel_build_runs_and_prints`: `TargetPythonWheel` produces `<pkg>-0.1.0-py3-none-any.whl` containing the user package, the bundled `mochi_runtime/`, and a `dist-info/` with METADATA + WHEEL + RECORD. Extracting the wheel onto PYTHONPATH and running `python -m <pkg>` byte-equals the recorded `.out`.
2. `sdist_build_contains_pyproject_and_sources`: `TargetPythonSdist` produces `<pkg>-0.1.0.tar.gz` containing `<pkg>-0.1.0/pyproject.toml`, `<pkg>-0.1.0/PKG-INFO`, the `src/<pkg>/` tree, and the bundled `src/mochi_runtime/`.
3. `wheel_is_deterministic`: two consecutive wheel builds from the same source produce byte-equal `.whl` files. This is the load-bearing input to the Phase 16 reproducible-build gate.

All three sub-gates pass on CPython 3.14.x. The full Phase 1-15 regression (`go test ./transpiler3/python/... -count=1`) finishes in 41.9s with zero regressions.

## Goal-alignment audit

The Mochi v1 pitch for the Python target includes "your Mochi program ships as a `pip install`-able wheel". For that to be true, the build pipeline has to produce a wheel without requiring the user's machine to have a Python build backend installed (hatchling, setuptools, poetry). The reference path (`python -m build`) requires the `build` package and a build backend; not every environment has those. The Phase 15.0 path produces the wheel directly with stdlib zip in Go, so the only runtime dependency at build time is the Mochi binary itself.

The wheel must also be installable without network. The straightforward `pyproject.toml` route declares `dependencies = ["mochi-runtime>=0.1.0"]` which forces pip to resolve `mochi-runtime` from PyPI; this is the wrong default for v1 because the runtime is not yet published to PyPI and won't be until Phase 18. Phase 15.0 bundles `mochi_runtime/` inside the user's wheel as a sibling top-level package. The trade is wheel size (small: the runtime is under 10kB of pure Python) for installability (the wheel ships self-contained). When Phase 18 lands and `mochi-runtime` is on PyPI, a Phase 15.x patch can flip the default to "declare the dependency, do not bundle"; the lowerer does not change.

Determinism is in scope for 15.0, not deferred. The two-build byte-equal gate is small enough to ship inside the same test file as the runnability gate, and pinning mtime + sort order at wheel/sdist construction time is much cheaper than chasing reproducibility regressions in Phase 16.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | `TargetPythonWheel` produces a self-contained PEP 427 wheel; `TargetPythonSdist` produces a PEP 517 sdist; both via stdlib zip/tar; `mochi_runtime/` bundled; wheel byte-deterministic | LANDED 2026-05-29 | (filled at ship) |
| 15.1 | uv build backend (`uv build --wheel`) as an alternate path, behind `MOCHI_PYTHON_BUILD_BACKEND=uv` | DEFERRED | -- |
| 15.2 | hatchling as the declared build backend in `pyproject.toml`; honoured when the user builds the sdist manually | DEFERRED | -- |
| 15.3 | `project.scripts` entry point so `pip install <wheel>` exposes a `<pkg>` console command | DEFERRED | -- |
| 15.4 | C-extension wheels (manylinux / macOS universal2) for programs that compile Mochi extern C code | DEFERRED to Phase 17 (platform) |
| 15.5 | Wheel signature (PEP 458 + Sigstore) | DEFERRED to Phase 18 |

## Sub-phase 15.0 -- stdlib zip/tar wheel + sdist builder

### Goal-alignment audit (15.0)

A user runs `mochi build hello.mochi --target python-wheel` and gets a `.whl` file. Two correctness gates: the wheel installs into a fresh venv without network access (because `mochi_runtime` is bundled, not depended upon); the installed program runs and prints what vm3 would. A third gate (determinism) is added because Phase 16 builds on it: if 15.0 is non-deterministic, 16 has no anchor to test against.

### Decisions made (15.0)

**Build wheel + sdist in Go, not via `python -m build`.** Asking the user's machine to have `build` plus a backend installed adds friction for no benefit; the Mochi binary already knows how to walk a tree and write a zip. Go's `archive/zip` and `archive/tar` write PEP 427 / PEP 517 compatible archives directly. When a user wants the "official" path (e.g. for sdist-from-source rebuilds outside Mochi), Phase 15.2 will ship a `pyproject.toml` with `hatchling` declared so `python -m build` works against the sdist too.

**Bundle `mochi_runtime/` inside the wheel.** The user does not need to install `mochi-runtime` separately. Trade: the wheel grows by the runtime size (currently under 10kB pure Python), which is negligible. Each Mochi program ships with its own runtime copy; if the user has 100 Mochi programs installed, they have 100 runtime copies. This is the right trade for v1 (single-program installs are the common case); Phase 15.x can flip the default once PyPI publishing exists.

**Phase 12 sidecar (`<pkg>_externs.py`) ships at the same zip root as the user package.** Programs that declare `extern python fun` already require the sidecar at runtime; the Phase 12 build copies it to `src/<pkg>_externs.py`. The Phase 15 wheel includes that file at the zip root next to the user package and the bundled runtime, so the user's `from mochi_user_<modname>_externs import ...` resolves after install.

**PEP 376 RECORD with sha256 digest + size.** Without RECORD, `pip` cannot verify the wheel contents post-install. The Phase 15 builder walks the stage tree, sorts the paths, computes a sha256 per file, and writes one `<path>,sha256=<b64>,<size>` line per file plus a `<path>,,` self-line for the RECORD file (PEP 376 mandates the empty digest + size for the self-line).

**Reproducible mtime: 1980-01-01.** The zip file format has a DOS-encoded mtime with a 1980 epoch floor. Stamping every entry at 1980-01-01 means the zip serializes byte-identically across builds. Same constant for the tar archive (and the gzip header's `ModTime` is pinned to the same value).

**Sorted file order.** `filepath.Walk` orders by name within each directory but the cross-platform guarantee is fragile; an explicit `sort.Strings` over the collected paths makes the order pinned regardless of host filesystem behaviour.

**`.pyc` and `__pycache__/` filtered out.** The Phase 12 sidecar copy is a literal file copy; if the user has already imported the sidecar and produced a `.pyc`, that would slip into the wheel and break determinism (mtime in `.pyc` headers, plus Python version sensitivity). The `pyOnlyFilter` skips them when copying `mochi_runtime/`; the user package never has them because it is generated fresh in `t.TempDir`.

### Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/wheel.go` (new) | `buildWheel` + `buildSdist` + `zipDir` + `tarGzDir` + `buildRecord` + reproducible-mtime helper |
| `transpiler3/python/build/build.go` | `Driver.Build` dispatches `TargetPythonWheel` and `TargetPythonSdist` to the new builders; cache marker bumped `mep51-phase14` -> `mep51-phase15` |
| `transpiler3/python/build/phase15_test.go` (new) | Three sub-gates: wheel runs after extract, sdist contains expected layout, wheel byte-deterministic |

## Deferred work

- **15.1 uv build backend.** `uv build --wheel` as an alternate path behind `MOCHI_PYTHON_BUILD_BACKEND=uv`. Deferred because uv is not yet on every CI image and the stdlib path is sufficient for v1.
- **15.2 hatchling declared in `pyproject.toml`.** The Phase 15.0 `pyproject.toml` already declares hatchling, but the builder bypasses it. A Phase 15.2 sub-phase would route the sdist build through `python -m build` when `MOCHI_PYTHON_BUILD_BACKEND=hatchling` is set, so the sdist is interoperable with downstream tools (poetry, pdm) that pin a specific backend.
- **15.3 `project.scripts` console command.** `pip install <wheel>` should expose a `<pkg>` command on PATH that runs `python -m <pkg>`. Two-line change to the pyproject; deferred until a downstream user asks for it.
- **15.4 C-extension wheels.** Phase 12.1 ctypes + Phase 17 platform-specific manylinux/macOS universal2 wheels. Deferred.
- **15.5 Wheel signature.** PEP 458 (TUF) + Sigstore for Phase 18 PyPI Trusted Publishing.
- **Sdist runnability gate.** The current sdist gate validates contents but does not run `python -m build` to round-trip the sdist back into a wheel. Adding this would catch any drift in `pyproject.toml` between what hatchling expects and what Phase 15.0 writes; deferred to Phase 15.2.
- **Wheel size optimisation.** No-op for v1 (runtime is small); Phase 15.x could elide unused runtime modules per-program (e.g. only ship `mochi_runtime/llm.py` if `needsLLM` was set).
