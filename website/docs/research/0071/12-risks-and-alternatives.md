---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "The risk register (PEP 561 coverage, wrapper compile time, dynamic typing leakage, GIL contention, capability drift, abi2026 transition, OIDC issuer downtime, persistent loop hazard, uv stability, supply-chain surface, sub-interpreter promise, async runtime fragmentation, free-threaded ecosystem) and the rejected alternatives register."
---

# 12. Risks and alternatives

This note catalogs the known risks in MEP-71's design and the alternatives that were considered and rejected. Each entry is referenced from the spec and other research notes. The risks are open issues with mitigations; the alternatives are paths the design explicitly does not take and why.

## Risk register

### R1. PEP 561 coverage of the long tail

**Risk**: Many PyPI packages are not typed. The bridge's PEP 561 ladder ([[04-pep561-stub-ingest]]) ends at stubgen, which produces `Any`-heavy stubs. For packages whose long tail of behaviour matters (legacy data-processing libs, scraping helpers, ETL packages), the bridge gives a degraded experience.

**Mitigation**:
- Document the expected coverage clearly in the spec.
- Provide a `[python].stubgen.aggressive = true` flag that runs stubgen with extra heuristics (cross-referencing `__init__.py` imports against typeshed, inferring types from `**kwargs` usage in docstrings).
- Long-term: contribute to typeshed for high-priority untyped packages.

**Status**: Open. Tracked in the Phase 3 (type mapping) acceptance criteria; the fixture corpus picks 5 untyped packages to validate the stubgen path.

### R2. Wrapper compile time

**Risk**: Each Python package import triggers a wrapper-module synthesis pass that compiles a C extension. For ~50 deps in a typical project, this is ~50 wrapper compiles at lock time and on every Mochi build. The cumulative time is multi-minutes.

**Mitigation**:
- Content-address the wrapper output (`wrapper-sha256` in mochi.lock); rebuild only when the input changes.
- Parallelise wrapper compilation across CPU cores.
- Default to abi3 wrappers so one compilation works across CPython minors.

**Status**: Tracked in Phase 7 (build orchestration) goals. Target: <30s incremental lock time on a 50-dep project.

### R3. Dynamic-typing leakage

**Risk**: Python's dynamic typing leaks into Mochi even with stubs. A Python function annotated as `-> str` might return `None` at runtime when an error occurs (older packages predate `Optional[str]` conventions). Mochi's type system will see a Mochi `string` return type and crash at the boundary.

**Mitigation**:
- The wrapper module performs a runtime type check on every return value against the declared Mochi type. Mismatch raises a Mochi `Error::PythonTypeMismatch`.
- A `[python].strict-return-types = false` flag relaxes this for compatibility with older packages.

**Status**: Tracked in Phase 5 (extern-fn emit) acceptance criteria.

### R4. Capability drift

**Risk**: A Python dep's capability set ([[01-language-surface]] §`[python.capabilities]`) is computed at lock time. A later release of the dep adds network calls or filesystem access. The user's lockfile pins the older version, but a `mochi pkg update` advances and silently elevates capabilities.

**Mitigation**:
- `mochi pkg update` runs the capability check on the new version and diffs against the lockfile. Any capability addition triggers a confirmation prompt or `--allow-capability-elevation` flag.
- The curated capability database (`package3/python/capabilities/db.toml`) is the canonical source. Updates land via PR review.

**Status**: Tracked in Phase 8 (lockfile) acceptance criteria.

### R5. GIL contention under hot concurrency

**Risk**: Mochi's runtime is many-goroutine; calling into a single GIL-locked CPython serializes them. For workloads with many Mochi workers all calling Python, throughput collapses to single-threaded.

**Mitigation**:
- Document the GIL serialisation prominently.
- Free-threaded mode (`[python].free-threaded = true`) bypasses the GIL on CPython 3.13t+. Phase 17 scope.
- The opaque-handle pattern lets long-lived Python state stay on one Mochi worker so the GIL is held by one worker at a time without contention.

**Status**: Open. Phase 17 (free-threaded) is the canonical fix; pre-Phase 17 the workload must be sequential or use subprocess mode.

### R6. abi2026 transition

**Risk**: PEP 802/809 introduces abi2026 as a new wheel tag in 2026-Q1. The bridge needs to support both the legacy `cp3XY` tag and the new tag during the transition. Wheels built before the transition continue to work but cannot use new abi2026 features.

**Mitigation**:
- `[python.publish].abi-tag-policy = "legacy" | "abi2026" | "both"` controls the tags emitted.
- Default through 2026-Q4 is `"legacy"`; default flip to `"both"` in 2027-Q1; default flip to `"abi2026"` in 2028-Q1.
- The bridge's consume direction reads both tags transparently from the simple index.

**Status**: Open. Tracked in Phase 9 (publish) follow-ups.

### R7. OIDC issuer downtime

**Risk**: PyPI Trusted Publishing depends on the OIDC issuer (GitHub Actions, GitLab) being reachable. If the issuer is down, all releases fail.

**Mitigation**:
- The Mochi CLI implements exponential backoff with a 5-attempt cap.
- A `--force-legacy-token` flag is intentionally not provided ([[02-design-philosophy]] §5); recovery requires the OIDC issuer to come back.
- Document the workaround: defer the release until the issuer recovers.

**Status**: Accepted residual risk. The OIDC issuers (GitHub, GitLab) have ~99.95% historical uptime; releases blocked by ~4 hours per year is acceptable.

### R8. Persistent event-loop hazard

**Risk**: `[python].async-mode = "persistent"` ([[08-async-bridge]]) introduces a shared event loop. State leaks across Mochi calls can hang or crash.

**Mitigation**:
- The default is per-call mode. Persistent is opt-in.
- The opt-in includes a one-time warning at first use.
- Loop-bound objects (Future, Task, AsyncIterator) returned from Python require explicit opaque-handle conversion; they don't auto-convert across loops.

**Status**: Mitigated. Phase 11 (async bridge) acceptance criteria validate the hazard cases.

### R9. uv stability

**Risk**: uv is the youngest major resolver. Astral's commitment is strong but unproven over 5+ year horizons. If uv's lockfile format changes or Astral pivots, the bridge needs to follow.

**Mitigation**:
- The lockfile schema (mochi.lock `[[python-package]]`) is independent of uv.lock; the bridge writes its own format.
- uv is invoked as a subprocess for the resolve step; if uv breaks, the bridge falls back to pip's resolver (slower, but functional).
- Export to PEP 751 pylock.toml is interoperable with any PEP 751-conformant tool.

**Status**: Accepted residual risk. uv is the right choice today; the fallback path is documented.

### R10. Supply-chain surface

**Risk**: The Mochi bridge installs code from PyPI, runs stubgen subprocesses, and compiles native extensions. Every step is a supply-chain attack surface.

**Mitigation**:
- All downloads are content-addressed (`wheel-blake3`, `wheel-sha256`) and verified against the lockfile.
- The stubgen subprocess runs with no network and a restricted filesystem.
- PEP 740 attestations are required for high-trust deps (configurable via `[python].require-attestations`).
- Capability database limits accessible APIs even when a dep is compromised.

**Status**: Tracked in Phase 1 (sparse-index client) and Phase 10 (publish) acceptance criteria.

### R11. Sub-interpreter promise vs reality

**Risk**: PEP 684/734 sub-interpreters look attractive as a GIL bypass for the bridge. Real-world C extension compatibility with sub-interpreters is poor.

**Mitigation**:
- Sub-interpreters are not the default. Phase 17 (free-threaded) is the canonical concurrency story.
- Sub-interpreters may land as an experimental opt-in (`[python].sub-interpreters = true`) post-Phase 17.

**Status**: Deferred. Not in Phase 0-17 scope.

### R12. Async runtime fragmentation

**Risk**: The Python async ecosystem has asyncio, trio, anyio, uvloop. The bridge supports asyncio (via `asyncio.run`) and uvloop (transparently when installed); trio is incompatible.

**Mitigation**:
- Document trio incompatibility.
- The bridge's lock-time analyzer detects `import trio` in a dep's code and refuses with `SkipReason::IncompatibleAsyncRuntime`.
- Anyio-based code works transparently because anyio supports asyncio by default.

**Status**: Documented. trio support is not in scope.

### R13. Free-threaded ecosystem

**Risk**: Free-threaded Python (3.13t, 3.14t) ecosystem coverage is currently small. Many C extensions don't yet support no-GIL builds. A user opting into `[python].free-threaded = true` might have ~half their deps refuse to install.

**Mitigation**:
- The bridge's capability database tracks per-dep free-threaded compatibility.
- `mochi pkg lock` rejects free-threaded mode when any dep lacks a free-threaded build.
- Default through Phase 16 is GIL-only.

**Status**: Tracked in Phase 17 acceptance criteria.

## Rejected alternatives

### A1. Direct ctypes (no wrapper module)

**Considered**: Emit Go code that dlopens libpython.so and calls `Py_*` functions through ctypes-like CFFI.

**Rejected**: GIL handling becomes a Go-side concern, free-threaded transitions become invisible at the Go side, the C-extension boundary (~30% of PyPI) becomes intractable. See [[02-design-philosophy]] §3.

### A2. CFFI as the primary FFI layer

**Considered**: Use CFFI's API mode to define the wrapper surface.

**Rejected**: CFFI is right for C-library wrapping; CPython itself is the target. The wrapper-module-as-Python approach is closer to the source language. [[03-prior-art-bridges]] CFFI section.

### A3. Cython fork (Mochi-flavoured Cython)

**Considered**: Fork Cython and emit Mochi-style hybrid Mochi/Cython source.

**Rejected**: Mochi has its own type system; rebuilding Cython on top would duplicate the work. The wrapper-module pattern lets Mochi stay Mochi and Python stay Python.

### A4. Embedded MicroPython only

**Considered**: Skip CPython, target only MicroPython for compactness and the embedded story.

**Rejected**: MicroPython covers <1% of PyPI's ecosystem. The point of MEP-71 is access to PyPI's actual ecosystem. MicroPython remains an MEP-51 transpile target.

### A5. Lifetime-erased dataclasses

**Considered**: Treat all Python classes as runtime-typed `PyObject` handles; do not synthesise Mochi-side structs.

**Rejected**: Loses the dev-experience win of typed field access. The closed type table ([[05-type-mapping]]) provides typed access for the common patterns (dataclass, TypedDict, NamedTuple) while degrading to handles for unsupported classes.

### A6. Long-lived OIDC token

**Considered**: Accept long-lived PyPI API tokens for users who can't use Trusted Publishing.

**Rejected**: Defeats the supply-chain security story. ([[02-design-philosophy]] §5, [[07-sigstore-pypi-trusted-publishing]].) Private PyPI mirrors that don't speak OIDC will be supported through a separate `--to=private-index` target later.

### A7. Sub-interpreter default

**Considered**: Make sub-interpreters the default concurrency model.

**Rejected**: C-extension compatibility is too low. Free-threaded mode is the canonical path for concurrency improvements. See R11 above.

### A8. Per-build venv

**Considered**: Each Mochi build creates a fresh venv, installs deps, and runs the wrapper synthesiser.

**Rejected**: Build time becomes minutes. The content-addressed cache + persistent wrapper outputs design lets builds reuse work across runs. See R2 above.

### A9. Trio as a first-class async runtime

**Considered**: Support both asyncio and trio in the async bridge.

**Rejected**: Trio's cancellation model differs from asyncio; supporting both means two parallel code paths. Trio is not in scope. See R12 above.

### A10. Mochi-specific stub format

**Considered**: Define a Mochi-specific stub format (`.mochi-stubs`) for Python packages.

**Rejected**: Defeats the consume-without-boilerplate goal. PEP 561 stubs are the canonical Python type source; the bridge uses them. See [[02-design-philosophy]] §2.

### A11. Wheel-built sdist only

**Considered**: Skip sdist publication; publish only wheels.

**Rejected**: PyPI requires sdist for many use cases (custom platform builds, source-only installs). The Mochi backend ships both sdist and wheel.

### A12. Hand-edited synthesised shim files

**Considered**: Allow users to hand-edit the synthesised `shim.mochi` files for fine-grained type control.

**Rejected**: The shim file is regenerated on every `mochi pkg lock`. Hand edits would be lost. Customisation goes through the `<modname>_externs.py` sidecar pattern (MEP-51 Phase 12 convention).

### A13. Bundle CPython into the Mochi binary

**Considered**: Ship a CPython binary with every Mochi distribution so users don't need a system Python.

**Rejected**: The Mochi binary balloon (~50MB just for CPython) is not worth the consistency win. Users install CPython themselves; the bridge detects the version.

### A14. Lock-time wheel pre-compilation

**Considered**: Pre-compile all wheels at lock time and cache the compiled artifacts.

**Rejected**: The bridge's wrapper compilation is the only step where the bridge has a stake; the wheel's own native code is already compiled. The bridge's wrapper compilation IS lock-time; that's the design. (See R2.)

## Cross-references

- [MEP-71 spec](/docs/mep/mep-0071) for the normative open issues.
- Each numbered Rxx is referenced from the spec's Risks section.
- Each numbered Axx is referenced from the spec's Alternatives section.
- [[02-design-philosophy]] for the decisions that drove the rejections.
