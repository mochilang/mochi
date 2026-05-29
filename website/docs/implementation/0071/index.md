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
| 2 | uv resolver bridge (subprocess + lockfile parsing) + PEP 751 pylock.toml round-trip | LANDED | `a38cb023` | [phase-02](/docs/implementation/0071/phase-02-uv-resolver) |
| 3 | PEP 561 stub discovery (4-tier precedence, typeshed pin, stubgen sandbox) + `.pyi` parser | LANDED | `55469e50` | [phase-03](/docs/implementation/0071/phase-03-stub-ingest) |
| 4 | Closed type-mapping table (scalars / strings / collections / Optional / Union / dataclass / TypedDict / Protocol) | LANDED | `39d45ea4` | [phase-04](/docs/implementation/0071/phase-04-type-mapping) |
| 5 | Wrapper module synthesiser (`<pkg>_externs.py` + `.pyi` + `_mochi_wrap.py` runtime) | LANDED | `627e3267` | [phase-05](/docs/implementation/0071/phase-05-wrapper) |
| 6 | Mochi-side extern fn emitter (`<pkg>_shim.mochi` with `extern python fun` + `type ... = { ... }`) | LANDED | `82aa45b6` | [phase-06](/docs/implementation/0071/phase-06-extern-emit) |
| 7 | `import python "<package>@<semver>" as <alias>` grammar + parser | LANDED | `cd5fe055` | [phase-07](/docs/implementation/0071/phase-07-import-grammar) |
| 8 | Build orchestration: workspace synth (+ wheel install + libpython link as sub-phases 8.1 / 8.2) | LANDED | `a8ec48f3` | [phase-08](/docs/implementation/0071/phase-08-build) |
| 8.1 | Wheel install loop: drive uv against rendered pyproject.toml | NOT STARTED | — | [phase-08](/docs/implementation/0071/phase-08-build) |
| 8.2 | libpython link: cgo embed for `runtime-mode = "embedded"` | NOT STARTED | — | [phase-08](/docs/implementation/0071/phase-08-build) |
| 9 | `mochi.lock` `[[python-package]]` integration + `--check` mode + capability database | LANDED | `cdd98e15` | [phase-09](/docs/implementation/0071/phase-09-lockfile) |
| 10 | `TargetPythonPackage` emit (sdist + wheel + `mochi-build` PEP 517 backend + `.pyi` for downstream typing) | LANDED | `60e7917b` | [phase-10](/docs/implementation/0071/phase-10-python-package-emit) |
| 10.1 | Sdist tar.gz + wheel zip packer (deterministic mtimes, ZIP64-aware) | NOT STARTED | — | [phase-10](/docs/implementation/0071/phase-10-python-package-emit) |
| 10.2 | `mochi pkg build --target=python-package` CLI verb | NOT STARTED | — | [phase-10](/docs/implementation/0071/phase-10-python-package-emit) |
| 10.3 | Mochi module -> `pypackage.Package` surface walker | NOT STARTED | — | [phase-10](/docs/implementation/0071/phase-10-python-package-emit) |
| 11 | Trusted publishing (`mochi pkg publish --to=pypi`) Sigstore OIDC + PEP 740 attestations | LANDED | `981e7baa` | [phase-11](/docs/implementation/0071/phase-11-trusted-publish) |
| 11.1 | Sigstore live signing harness (`sigstore-python` shell-out + Rekor log) | NOT STARTED | — | [phase-11](/docs/implementation/0071/phase-11-trusted-publish) |
| 11.2 | Live PyPI / TestPyPI HTTP (mint endpoint contract test) | NOT STARTED | — | [phase-11](/docs/implementation/0071/phase-11-trusted-publish) |
| 11.3 | `mochi pkg publish --to=pypi` CLI verb | NOT STARTED | — | [phase-11](/docs/implementation/0071/phase-11-trusted-publish) |
| 12 | Async bridge (asyncio.run per-call + persistent loop opt-in + cross-loop hazard guards) | LANDED | `df616a89` | [phase-12](/docs/implementation/0071/phase-12-async-bridge) |
| 12.1 | Wire asyncbridge into wrapper synthesiser so async surfaces emit sync shims end-to-end | NOT STARTED | — | [phase-12](/docs/implementation/0071/phase-12-async-bridge) |
| 12.2 | Free-threaded GIL handling around the persistent loop cache (PyMutex on `cp3XYt`) | NOT STARTED | — | [phase-12](/docs/implementation/0071/phase-12-async-bridge) |
| 12.3 | Cancellation + timeout propagation (`mochi_async_timeout` ContextVar -> `asyncio.wait_for`) | NOT STARTED | — | [phase-12](/docs/implementation/0071/phase-12-async-bridge) |
| 13 | abi3 wheel slimming + auditwheel-equivalent platform tag validation | LANDED | `d09e2919` | [phase-13](/docs/implementation/0071/phase-13-abi3) |
| 13.1 | Live ELF / Mach-O / PE reader behind `SymbolReader` | NOT STARTED | — | [phase-13](/docs/implementation/0071/phase-13-abi3) |
| 13.2 | Py_LIMITED_API discipline check before promoting to abi3 | NOT STARTED | — | [phase-13](/docs/implementation/0071/phase-13-abi3) |
| 13.3 | `mochi pkg audit --wheel <path>` + `mochi pkg slim --abi3` CLI verbs | NOT STARTED | — | [phase-13](/docs/implementation/0071/phase-13-abi3) |
| 14 | Subprocess runtime mode (`[python].runtime-mode = "subprocess"` with JSON-RPC protocol) | LANDED | `168d2ec7` | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 14.1 | Live `os/exec` spawn + stderr forwarding + worker lifetime management | NOT STARTED | — | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 14.2 | Request pipelining (multiple in-flight requests demultiplexed by ID) | NOT STARTED | — | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 14.3 | `mochi pkg run --runtime=subprocess` CLI verb + `[python].runtime-mode` dispatch | NOT STARTED | — | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 14.4 | Mixed sync + async worker surfaces | NOT STARTED | — | [phase-14](/docs/implementation/0071/phase-14-subprocess-mode) |
| 15 | Attestation verification at install time + `--require-attestations` enforcement | LANDED | `1cf131b7` | [phase-15](/docs/implementation/0071/phase-15-attestation-verify) |
| 15.1 | Live sigstore-go crypto verifier (X.509 chain + Rekor inclusion proof + SCT) | NOT STARTED | — | [phase-15](/docs/implementation/0071/phase-15-attestation-verify) |
| 15.2 | Live PyPI HTTP `<wheel-url>.provenance` fetcher with cache + retry | NOT STARTED | — | [phase-15](/docs/implementation/0071/phase-15-attestation-verify) |
| 15.3 | `mochi pkg install --require-attestations` + `--allowed-builder` + `--trusted-publisher` CLI verbs | NOT STARTED | — | [phase-15](/docs/implementation/0071/phase-15-attestation-verify) |
| 16 | Pyodide / WASI target support (`wasm32-emscripten`, `wasm32-wasip2` wheel resolution + WIT interface) | LANDED | `a2ac4f8c` | [phase-16](/docs/implementation/0071/phase-16-pyodide-wasi) |
| 16.1 | Wire WIT world emit into wrapper synthesiser so `extern python` -> WIT export for WASI targets | NOT STARTED | — | [phase-16](/docs/implementation/0071/phase-16-pyodide-wasi) |
| 16.2 | Live Pyodide distribution index client at `pyodide.org/distribution/v<X>/full/` | NOT STARTED | — | [phase-16](/docs/implementation/0071/phase-16-pyodide-wasi) |
| 16.3 | `mochi pkg install --target=pyodide` / `--target=wasi-p2` CLI verbs + lockfile target field | NOT STARTED | — | [phase-16](/docs/implementation/0071/phase-16-pyodide-wasi) |
| 17 | Free-threaded CPython 3.13t / 3.14t (`cp3XYt` ABI tag, PyMutex, atomic refcount) | LANDED | `332255f1` | [phase-17](/docs/implementation/0071/phase-17-free-threaded) |
| 17.1 | Live ELF / Mach-O / PE module-marker reader (PEP 703 `Py_mod_gil` slot) | NOT STARTED | — | [phase-17](/docs/implementation/0071/phase-17-free-threaded) |
| 17.2 | Wire RenderLockShim into wrapper synthesiser so `extern python` shims pick the right primitive | NOT STARTED | — | [phase-17](/docs/implementation/0071/phase-17-free-threaded) |
| 17.3 | `mochi pkg install --runtime=free-threaded` + `--allow-untested-freethread` + deny-list config | NOT STARTED | — | [phase-17](/docs/implementation/0071/phase-17-free-threaded) |
| 18 | abi2026 transition (`abi-tag-policy = "legacy" | "abi2026" | "both"`) + 2026-Q1 rollout | LANDED | `795a28e2` | [phase-18](/docs/implementation/0071/phase-18-abi2026) |
| 18.1 | `mochi.lock` `[python].abi-tag-policy` field + `mochi.toml` mirror | NOT STARTED | — | [phase-18](/docs/implementation/0071/phase-18-abi2026) |
| 18.2 | `mochi pkg promote --to=abi2026` CLI verb + `.dist-info/` interpreter-tag relink | NOT STARTED | — | [phase-18](/docs/implementation/0071/phase-18-abi2026) |
| 18.3 | Live PyPI two-tag publish (cp32-abi3 + cp314-abi2026 side-by-side during migration window) | NOT STARTED | — | [phase-18](/docs/implementation/0071/phase-18-abi2026) |

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
  wrapper/                # <pkg>_externs.py + .pyi + _mochi_wrap.py synthesiser (phase 5)
  emit/                   # Mochi shim emitter — <pkg>_shim.mochi with extern python fun + type aliases (phase 6)
  importspec/             # import python "<spec>" body parser (phase 7)
  lockfile/               # mochi.lock [[python-package]] table + --check diff + capability database (phase 9)
  pypackage/              # TargetPythonPackage emit: sdist + wheel + mochi_build PEP 517 backend (phase 10)
  publish/                # PyPI publish + Sigstore + PEP 740 attestations (phase 11)
  asyncbridge/            # async-fn -> sync-fn shim renderer + cross-loop guard (phase 12)
  abi3/                   # abi3 wheel slimming + auditwheel-equivalent platform validator (phase 13)
  subproc/                # JSON-RPC 2.0 stdio protocol + Python worker source renderer (phase 14)
  attest/                 # install-time PEP 740 verification + policy (phase 15)
  pyodide/                # pyodide / emscripten / wasi-p2 platform-tag matcher + WIT emitter (phase 16)
  freethread/             # cp3XYt ABI matcher + PEP 703 module-marker audit + PyMutex lock shim (phase 17)
  abi2026/                # 2026-Q1 ABI-tag transition: TagClass + Policy + Selector + Promote/Downgrade (phase 18)
  runtime/                # the embedded mochi_runtime Python package (phase 5 + phase 12)
```

The `package3/python/` location is shared with the broader MEP-57 polyglot package work (where `package3/` is the v3 package-system tree). MEP-73 occupies `package3/rust/` in parallel.

## Status snapshot

As of 2026-05-30 01:35 (GMT+7): MEP-71 spec and research bundle landed; **all 19 umbrella phases (0-18) LANDED on `main`** (8.1, 8.2, 10.1, 10.2, 10.3, 11.1, 11.2, 11.3, 12.1, 12.2, 12.3, 13.1, 13.2, 13.3, 14.1, 14.2, 14.3, 14.4, 15.1, 15.2, 15.3, 16.1, 16.2, 16.3, 17.1, 17.2, 17.3, 18.1, 18.2, 18.3 deferred sub-phases). Every umbrella phase shipped one-PR-per-phase with auto-merge, following the MEP-73 cadence; only the deferred sub-phases (live IO, CLI verbs, live crypto) remain.

## Cross-references

- [MEP-71 spec](/docs/mep/mep-0071) for the normative design.
- [MEP-71 research bundle](/docs/research/0071/) for the 12-note deep-research collection.
- [MEP-51 implementation tracking](/docs/implementation/0051) for the underlying Python transpiler that MEP-71 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-71 builds on.
- [MEP-73 implementation tracking](/docs/implementation/0073) for the parallel Rust bridge that shares the polyglot package infrastructure.
