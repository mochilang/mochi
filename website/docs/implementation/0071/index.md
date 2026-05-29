---
title: MEP-71 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 71. Mochi+Python package manager"
description: "Per-phase implementation tracking for MEP-71 (Mochi+Python package manager). Status + commit columns capture how each phase landed on main."
---

# MEP-71 implementation tracking

Per-phase tracking for [MEP-71 Mochi+Python package manager](/docs/mep/mep-0071). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR, the in-branch commit on `mep/0071-python-package`).

A phase is LANDED only when its gate is green for every target (consume direction + publish direction where applicable). Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: `package3/python/` layout + `Driver` / `Venv` / `SkipReason` / `BridgeError` | LANDED | `8e1ef75f` | [phase-00](/docs/implementation/0071/phase-00-skeleton) |
| 1 | Simple-index client (PEP 503 HTML + PEP 691 JSON + PEP 700 metadata, sha256 + blake3 download verify) | LANDED | `3dfc4490` | [phase-01](/docs/implementation/0071/phase-01-simple-index) |
| 2 | uv resolver bridge (subprocess + lockfile parsing) + PEP 751 pylock.toml round-trip | LANDED | (pending merge) | [phase-02](/docs/implementation/0071/phase-02-uv-resolver) |
| 3 | PEP 561 stub discovery (4-tier precedence, typeshed pin, stubgen sandbox) + `.pyi` parser | NOT STARTED | — | [phase-03](/docs/implementation/0071/phase-03-stub-ingest) |
| 4 | Closed type-mapping table (scalars / strings / collections / Optional / Union / dataclass / TypedDict / Protocol) | NOT STARTED | — | [phase-04](/docs/implementation/0071/phase-04-type-mapping) |
| 5 | Wrapper module synthesiser (CPython extension `.so` + `_mochi_wrap.py` + `.pyi`) | NOT STARTED | — | [phase-05](/docs/implementation/0071/phase-05-wrapper) |
| 6 | Mochi-side extern fn emitter + alias shim file generation + sidecar (`*_externs.py`) loader | NOT STARTED | — | [phase-06](/docs/implementation/0071/phase-06-extern-emit) |
| 7 | `import python "<package>@<semver>" as <alias>` grammar + parser | NOT STARTED | — | [phase-07](/docs/implementation/0071/phase-07-import-grammar) |
| 8 | Build orchestration: workspace synth + libpython link + wheel install + wrapper compile | NOT STARTED | — | [phase-08](/docs/implementation/0071/phase-08-build) |
| 9 | `mochi.lock` `[[python-package]]` integration + `--check` mode + capability database | NOT STARTED | — | [phase-09](/docs/implementation/0071/phase-09-lockfile) |
| 10 | `TargetPythonPackage` emit (sdist + wheel + `mochi-build` PEP 517 backend + `.pyi` for downstream typing) | NOT STARTED | — | [phase-10](/docs/implementation/0071/phase-10-python-package-emit) |
| 11 | Trusted publishing (`mochi pkg publish --to=pypi`) Sigstore OIDC + PEP 740 attestations | NOT STARTED | — | [phase-11](/docs/implementation/0071/phase-11-trusted-publish) |
| 12 | Async bridge (asyncio.run per-call + persistent loop opt-in + cross-loop hazard guards) | NOT STARTED | — | [phase-12](/docs/implementation/0071/phase-12-async-bridge) |
| 13 | abi3 wheel slimming + auditwheel-equivalent platform tag validation | NOT STARTED | — | [phase-13](/docs/implementation/0071/phase-13-abi3) |
| 14 | Subprocess runtime mode (`[python].runtime-mode = "subprocess"` with JSON-RPC protocol) | NOT STARTED | — | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 15 | Attestation verification at install time + `--require-attestations` enforcement | NOT STARTED | — | [phase-15](/docs/implementation/0071/phase-15-attestation-verify) |
| 16 | Pyodide / WASI target support (`wasm32-emscripten`, `wasm32-wasip2` wheel resolution + WIT interface) | NOT STARTED | — | [phase-16](/docs/implementation/0071/phase-16-pyodide-wasi) |
| 17 | Free-threaded CPython 3.13t / 3.14t (`cp3XYt` ABI tag, PyMutex, atomic refcount) | NOT STARTED | — | [phase-17](/docs/implementation/0071/phase-17-free-threaded) |
| 18 | abi2026 transition (`abi-tag-policy = "legacy" | "abi2026" | "both"`) + 2026-Q1 rollout | NOT STARTED | — | [phase-18](/docs/implementation/0071/phase-18-abi2026) |

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED.
- **Files to touch**: the bridge-side files (Go) and emit-side files (Python template + C glue template) the phase introduces or modifies.
- **Fixtures**: which of the 25-package fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture package (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 25-package fixture corpus (May 2026 top-30-most-downloaded-on-PyPI selection biased toward typed packages with PEP 561 markers):

numpy, pandas, scipy, scikit-learn, requests, httpx, urllib3, pillow, pydantic, attrs, click, typer, rich, tqdm, sqlalchemy, fastapi, starlette, uvicorn, aiohttp, pyyaml, toml, tomli, msgpack, orjson, pytest.

Each phase that touches the type-mapping or wrapper layer asserts golden counts against this corpus. The corpus is regenerated quarterly to track PyPI API drift.

Coverage of the four PEP 561 tiers across the corpus (approximate, as of 2026-Q2):

- Tier 1 (inline `py.typed`): pydantic, attrs, click, typer, rich, httpx, fastapi, starlette, sqlalchemy 2.x, msgpack, orjson, pytest. **12 / 25**.
- Tier 2 (sibling `<name>-stubs`): requests (types-requests), urllib3 (types-urllib3), pyyaml (types-PyYAML), toml (types-toml), tqdm (types-tqdm). **5 / 25**.
- Tier 3 (typeshed): tomli, uvicorn (partial). **2 / 25**.
- Tier 4 (stubgen fallback): numpy (partial; numpy ships partial inline), pandas (partial), scipy, scikit-learn, pillow, aiohttp (partial). **6 / 25**.

The mix is intentional to exercise all four tiers across the fixture set.

## Implementation location

The bridge lives at `package3/python/` in the repo root:

```
package3/python/
  README.md               # pointer to MEP-71 spec
  errors/                 # SkipReason + BridgeError (phase 0)
  build/                  # Workspace + Driver + Venv + libpython link (phase 0)
  semver/                 # PEP 440 version parser (phase 1)
  simple/                 # PEP 503 / 691 / 700 simple-index client + content-addressed cache (phase 1)
  toml/                   # minimal TOML reader scoped to uv.lock + pylock.toml + pyproject.toml (phase 2)
  uv/                     # uv subprocess bridge + uv.lock parser + pylock.toml round-trip (phase 2)
  stubs/                  # PEP 561 stub discovery + typeshed pin + stubgen sandbox + .pyi parser (phase 3)
  typemap/                # closed type table + Mochi/Python rendering (phase 4)
  wrapper/                # CPython extension synthesiser (phase 5)
  emit/                   # Mochi extern fn emitter (phase 6)
  publish/                # PyPI publish + Sigstore + PEP 740 attestations (phase 11)
  attest/                 # attestation verification (phase 15)
  pyodide/                # wasm32-emscripten + WASI Preview 2 target support (phase 16)
  freethread/             # free-threaded mode wrapper variants (phase 17)
  runtime/                # the embedded mochi_runtime Python package (phase 5 + phase 12)
```

The `package3/python/` location is shared with the broader MEP-57 polyglot package work (where `package3/` is the v3 package-system tree). MEP-73 occupies `package3/rust/` in parallel.

## Status snapshot

As of 2026-05-29 23:09 (GMT+7): MEP-71 spec and research bundle landed; phases 0-2 LANDED; phases 3-18 NOT STARTED. The implementation proceeds one phase per PR with auto-merge, following the MEP-73 cadence.

## Cross-references

- [MEP-71 spec](/docs/mep/mep-0071) for the normative design.
- [MEP-71 research bundle](/docs/research/0071/) for the 12-note deep-research collection.
- [MEP-51 implementation tracking](/docs/implementation/0051) for the underlying Python transpiler that MEP-71 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-71 builds on.
- [MEP-73 implementation tracking](/docs/implementation/0073) for the parallel Rust bridge that shares the polyglot package infrastructure.
