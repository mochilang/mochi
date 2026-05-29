---
title: "Phase 16. Pyodide / WASI target support"
sidebar_position: 18
sidebar_label: "Phase 16. Pyodide / WASI"
description: "MEP-71 Phase 16 implementation tracking: WASM platform tag matcher, WASI Preview 2 + Pyodide wheel selector, and WIT world emitter."
---

# Phase 16. Pyodide / WASI target support

The first non-native target family for the MEP-71 bridge. Ships:

- A platform-tag parser + matcher for the three wheel tag families
  PyPI + the Pyodide distribution channel publish today
  (`pyodide_<year>_<rev>_wasm32`,
  `emscripten_<major>_<minor>_<patch>_wasm32`, and the still
  experimental `wasi_p2_wasm32`).
- A `Selector` that picks the best wheel for a given `Target`
  (runtime + Python ABI + optional Pyodide vintage floor).
- A WIT (WebAssembly Interface Type) world emitter that the wrapper
  synthesiser will eventually call to emit a `world ... { import:
  ... export: ... }` block for WASI Preview 2 components.

## Status

`LANDED` (pending PR merge). Phase 16 ships the offline platform-tag +
wheel-selector + WIT emitter surfaces; live Pyodide HTTP index, WIT
wiring into the wrapper synthesiser, and `mochi pkg install --target`
CLI verbs are split into sub-phases 16.1 / 16.2 / 16.3.

## Gate

`go test ./package3/python/pyodide/... -count=1`

Covers:

- Runtime parse + render + Vintage ordering (Less / Zero / String) +
  Target.Validate.
- ParsePlatformTag happy + 11 error paths across the three tag
  families (missing _wasm32 suffix, malformed segments, negative
  year, non-numeric major/minor/patch, ...).
- Tag.SatisfiesRuntime: Pyodide tag -> RuntimePyodide,
  Emscripten tag -> RuntimePyodide (Pyodide is the only modern
  consumer of the legacy emscripten_X_Y_Z_wasm32 wheels in the
  wild), Wasi tag -> RuntimeWASIPreview2.
- Selector: vintage descending, ABI mismatch hard reject, MinVintage
  floor, runtime mismatch hard reject, Pyodide preferred over
  Emscripten when both match, Emscripten version descending,
  WASI happy, empty-ABI accepts any ABI, malformed filenames
  recorded in Reasons, deterministic tie-break on filename.
- WIT type renderer for the 13 primitive + 2 composite (list, option)
  + 1 ref kinds; panic paths for malformed types.
- WITFunc renders with / without return.
- WITWorld.Validate enforces non-empty package, kebab-case world /
  record / record-field / function / function-param names; rejects
  duplicate record + duplicate function declarations.
- WITWorld.Render is deterministic (records sorted by name, imports
  sorted, exports sorted; rerunning the same world yields the
  same string).
- isKebab rejects double-hyphens, leading hyphens, trailing
  hyphens, leading digit, uppercase, underscore.
- `phase16_test.go` umbrella sentinel: numpy vintage walk,
  WASI server-side pick, end-to-end WIT world for a fetch surface,
  manylinux wheels do not leak into a wasm resolve.

## Files

```
package3/python/pyodide/
  doc.go               # 3-responsibility overview
  target.go            # Runtime enum + Target + Vintage
  platform.go          # ParsePlatformTag + Tag.SatisfiesRuntime
  wheel.go             # Selector + WheelCandidate + SelectionResult
  wit.go               # WITType / WITFunc / WITRecordDecl / WITWorld + Render
  target_test.go       # 5 tests
  platform_test.go     # 5 tests (parse / kind string / runtime satisfaction)
  wheel_test.go        # 11 tests
  wit_test.go          # 11 tests
  phase16_test.go      # 1 umbrella sentinel (4 sub-cases)
```

## Sub-phase decomposition

### 16.1. Wire WIT into wrapper synthesiser

The wrapper synthesiser (phase 5) emits `extern python` shims that
boil down to JSON / pickle hand-off when the runtime is embedded
CPython or subprocess. For WASI Preview 2 targets the bridge needs a
WIT world instead so the wasm component model can do the import /
export plumbing. Sub-phase 16.1 walks the wrapper's already-typed
function table and feeds it through `pyodide.WITWorld.Render` to emit
a `.wit` file next to the wrapper module.

### 16.2. Live Pyodide distribution index

Pyodide does not publish through PyPI Simple-Index; it lives at
`pyodide.org/distribution/v<X>/full/` and uses a custom JSON layout
that maps `package -> {wheel-filename, sha256, depends, imports}`.
Sub-phase 16.2 implements a `pyodide.IndexClient` that the resolver
can dial in when `target = pyodide`. Today the wheel selector accepts
any candidate list, so an upstream that hand-curates the wheel list
already works.

### 16.3. CLI verbs

The CLI surface:

- `mochi pkg install --target=pyodide` -> `RuntimePyodide`.
- `mochi pkg install --target=wasi-p2` -> `RuntimeWASIPreview2`.
- `--pyodide-min-vintage=<year>_<rev>` -> `Target.MinVintage`.
- `mochi.lock` gets a `[python].target` field so the lockfile can
  pin the choice for reproducible installs.

## Fixtures

Phase 16 is selector-only and does not change wrapper-synthesiser
fixtures. Sub-phase 16.1 will add WIT golden files under
`package3/python/pyodide/testdata/wit/` mirroring the wrapper-fixture
function tables so the round-trip is locked in.

## Cross-references

- [Pyodide distribution channel](https://pyodide.org/en/stable/usage/loading-packages.html)
- [WebAssembly Component Model](https://component-model.bytecodealliance.org/)
- [WIT IDL spec](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md)
- [PEP 425: compatibility tags](https://peps.python.org/pep-0425/)
- [CPython WASI port](https://docs.python.org/3.13/using/wasm.html)
- [phase 5](/docs/implementation/0071/phase-05-wrapper) for the
  wrapper synthesiser that sub-phase 16.1 will tap into.
- [phase 9](/docs/implementation/0071/phase-09-lockfile) for the
  lockfile sub-phase 16.3 will extend.
