---
title: "Phase 15. Wheel + sdist build via uv"
sidebar_position: 20
sidebar_label: "Phase 15. Wheel + sdist"
description: "MEP-51 Phase 15, uv build drives the hatchling PEP 517 backend to produce py3-none-any wheels and source distributions, install into a fresh venv, and gate python -m execution against vm3 stdout."
---

# Phase 15. Wheel + sdist build via uv

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 15](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase15WheelSdistPython`: 4 dedicated build fixtures (`wheel_basic_install`, `wheel_with_extras`, `sdist_basic_install`, `sdist_with_extras`) plus the cumulative re-run of every prior phase (Phases 1, 2, 3.1-3.4, 4-14, ~253 fixtures) through `uv build && uv pip install dist/*.whl && python -m <pkg>`. Tier 4 of the gate hierarchy dominates: the wheel must build, install into a fresh `uv venv`, and produce byte-equal stdout against the vm3 `expect.txt`. Tiers 1-3 (vm3 byte-equal, mypy 1.13 + pyright 1.1.380 strict, ruff 0.7+ fixed-point) continue to run on the emitted source. Secondary gate: `uv build` exits 0 with both `dist/<pkg>-<ver>-py3-none-any.whl` and `dist/<pkg>-<ver>.tar.gz` present and named per PEP 425 / PEP 427.

## Goal-alignment audit

Phase 15 is the first phase whose user-visible artifact is a wheel rather than a `.py` tree. Every prior phase emits Python source that runs under `python -m`, but a Mochi user shipping to a colleague, a CI host, or PyPI needs a wheel they can `pip install`. Phase 15 closes that loop: the same emit pipeline that fed `python -m` for Phases 1-14 now feeds `uv build`, the hatchling PEP 517 backend produces `mochi_<pkg>-<ver>-py3-none-any.whl`, and `uv pip install dist/*.whl` into a clean venv plus `python -m <pkg>` reproduces the vm3 stdout. Without Phase 15 a Mochi-emitted Python project is a source tree, not a shipable artifact.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | `pyproject.toml` emission: PEP 621 `[project]` + `[build-system] requires=["hatchling>=1.25"]` + `[tool.hatch.build.targets.wheel].packages = ["src/<pkg>"]` | NOT STARTED | — |
| 15.1 | `uv build --wheel` driver invocation; assert `<pkg>-<ver>-py3-none-any.whl` filename, wheel layout, `RECORD`, `METADATA`, `WHEEL` | NOT STARTED | — |
| 15.2 | `uv build --sdist`: source distribution with `PKG-INFO` per PEP 643 parity; assert `<pkg>-<ver>.tar.gz` filename and tree | NOT STARTED | — |
| 15.3 | Install gate: `uv venv && uv pip install dist/*.whl && python -m <pkg>` stdout byte-equal to vm3; cross-OS matrix (linux-x86_64, linux-aarch64, macos-arm64, windows-x86_64) | NOT STARTED | — |

## Sub-phase 15.0 -- pyproject.toml emission

### Goal-alignment audit (15.0)

The emitter has never written a `pyproject.toml` before. Phase 15.0 is the file that makes the source tree a buildable PEP 517 project. Without it, `uv build` exits with `No `pyproject.toml` found`; with it, `uv build` runs unmodified across every Mochi fixture.

### Decisions made (15.0)

**`[build-system]`** declares hatchling as the PEP 517 backend, pinned to the 1.25 floor that honours `SOURCE_DATE_EPOCH` (load-bearing for Phase 16):

```toml
[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"
```

**`[project]`** is fully PEP 621, no `[tool.poetry]`, no `[tool.flit]`, no `setup.py`, no `setup.cfg`. The emitter writes the minimum surface plus what Mochi's `mod` metadata implies:

```toml
[project]
name = "mochi-example-app"
version = "0.1.0"
description = "Emitted by mochi build --target=python-wheel (MEP-51)."
readme = "README.md"
requires-python = ">=3.12"
license = "Apache-2.0"
authors = [{ name = "Mochi project", email = "team@mochilang.dev" }]
classifiers = [
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.12",
    "Programming Language :: Python :: 3.13",
    "Typing :: Typed",
]
dependencies = ["mochi-runtime>=0.1.0,<0.2.0"]

[project.scripts]
mochi-example-app = "mochi_example_app.__main__:main"
```

**Floor of `>=3.12`** is non-negotiable: PEP 695 type-parameter syntax, `asyncio.TaskGroup` cancellation stability, and PEP 698 `@override` are all 3.12 features the emit relies on. No upper bound is set (Henry Schreiner, "Should You Use Upper Bound Version Constraints?", 2021).

**`[tool.hatch.build.targets.wheel]`** is required for src-layout (hatchling auto-detects flat layout only):

```toml
[tool.hatch.build.targets.wheel]
packages = ["src/mochi_example_app"]

[tool.hatch.build.targets.sdist]
include = ["src/", "tests/", "README.md", "LICENSE", "pyproject.toml"]
```

**Name normalisation**: PyPI normalises `mochi-example-app` to `mochi_example_app` for the wheel filename and the import path. The emitter writes the hyphenated form in `[project].name` and the underscored form in `[tool.hatch.build.targets.wheel].packages`; PEP 503 normalisation handles the rest.

**File**: `transpiler3/python/build/pyproject.go` (the Go emitter that writes `pyproject.toml`).

## Sub-phase 15.1 -- uv build --wheel

### Goal-alignment audit (15.1)

A `pyproject.toml` without a wheel build is just a TOML file. Phase 15.1 is the first time `uv build --wheel` shells out from the Mochi driver and produces an actual `.whl` under `dist/`. The wheel filename and `.dist-info` contents become the contract every downstream phase (16 reproducibility, 18 PyPI publish) depends on.

### Decisions made (15.1)

**Driver invocation** in `transpiler3/python/build/driver.go`:

```go
cmd := exec.Command("uv", "build", "--wheel", "--out-dir", outDir)
cmd.Env = append(os.Environ(),
    "SOURCE_DATE_EPOCH="+epoch, // Phase 16 dependency; harmless here
)
```

The driver never passes `--no-isolation`. PEP 517 build isolation guarantees that the backend sees only `[build-system].requires` plus stdlib; no host-environment contamination leaks into the wheel.

**Expected output**:

```
dist/mochi_example_app-0.1.0-py3-none-any.whl
```

The filename is fixed by PEP 425 (`py3` interpreter tag, `none` ABI, `any` platform). Mochi v1 emits pure Python; the optional `mochi_runtime` C extension ships as a separate cp312-abi3 wheel from a separate project. No `cp312-cp312-manylinux_2_17_x86_64` wheels in v1.

**Wheel layout** (asserted by the test runner):

```
mochi_example_app-0.1.0-py3-none-any.whl
├── mochi_example_app/
│   ├── __init__.py
│   ├── __main__.py
│   ├── py.typed                      # PEP 561 marker
│   └── generated/
│       ├── __init__.py
│       └── <module>.py               # ast.unparse + ruff format output
└── mochi_example_app-0.1.0.dist-info/
    ├── METADATA                       # PEP 643 v2.3
    ├── WHEEL
    ├── RECORD                         # SHA256 of every entry
    ├── entry_points.txt
    └── licenses/LICENSE
```

**`py.typed` marker** is always present. Without it, downstream `mypy` and `pyright` treat the package as untyped (PEP 561 §3.4); the dual-checker contract from `the shared-decisions anchor` requires it.

**Tests** assert (a) `dist/*.whl` exists; (b) the wheel is a valid zip; (c) `python -m zipfile -l dist/*.whl` shows `<pkg>/__main__.py`, `<pkg>/py.typed`, and the `.dist-info/RECORD` entry; (d) `METADATA` declares `Metadata-Version: 2.3` and `Requires-Python: >=3.12`.

## Sub-phase 15.2 -- uv build --sdist

### Goal-alignment audit (15.2)

PyPI requires an sdist alongside every wheel for security audit (the sdist is the immutable source-of-truth that any party can re-build into the wheel). Phase 15.2 makes Mochi-emitted projects PyPI-uploadable in full; without the sdist, Phase 18 (Trusted Publishing) would only have half the artifacts.

### Decisions made (15.2)

**Invocation**:

```
uv build --sdist --out-dir dist/
```

**Expected output**:

```
dist/mochi_example_app-0.1.0.tar.gz
```

**sdist contents**:

```
mochi_example_app-0.1.0/
├── PKG-INFO              # PEP 643: same fields as wheel METADATA
├── pyproject.toml
├── README.md
├── LICENSE
├── src/mochi_example_app/...
└── tests/...
```

**PEP 643 parity**: `PKG-INFO` and the wheel's `METADATA` must have byte-equal field values (hatchling 1.25 enforces this; the emitter relies on it).

**Default invocation**: `uv build` with no flag produces both wheel and sdist; the driver issues `uv build` for the combined case in Phase 18 and the split `--wheel` / `--sdist` flags only when one side is selectively rebuilt (Phase 16 reproducibility re-builds only the wheel for the SHA256 comparison).

**`tar.gz` reproducibility**: tar headers include mtime; `SOURCE_DATE_EPOCH` is read by hatchling 1.25 for tar entries the same way it is read for wheel zip entries. Phase 16 verifies.

## Sub-phase 15.3 -- install gate + python -m execution

### Goal-alignment audit (15.3)

The wheel and sdist are inert artifacts until someone installs them. Phase 15.3 is the gate that proves a Mochi-emitted wheel actually works: installed into a fresh `uv venv`, imported, run via `python -m <pkg>`, and producing the same stdout vm3 produced from the source `.mochi`. This is the senary gate from MEP-51 §Abstract.

### Decisions made (15.3)

**Install flow** in the runner:

```bash
uv venv --python 3.12 .venv-phase15
uv pip install --python .venv-phase15/bin/python dist/*.whl
.venv-phase15/bin/python -m mochi_example_app > actual.stdout
diff -u expect.txt actual.stdout    # Tier 1 master gate
```

**Cross-OS matrix** runs the install gate on each tier-1 platform from research note 07:

| Runner | Python | Arch | Notes |
|--------|--------|------|-------|
| `ubuntu-24.04` | 3.12.0 | x86_64 | floor |
| `ubuntu-24.04` | 3.12.7 | x86_64 | CI ceiling |
| `ubuntu-24.04` | 3.13.0 | x86_64 | advisory (non-gating) |
| `ubuntu-24.04-arm` | 3.12.7 | aarch64 | ARM verification |
| `macos-14` | 3.12.7 | arm64 | Apple Silicon |
| `windows-2022` | 3.12.7 | x86_64 | Windows |

**`uv python install 3.12.0`** downloads the python-build-standalone CPython at the floor when the runner ships only 3.12.7 or 3.13.0. The 3.12.0 floor is the first Python the gate must clear; 3.12.7 is the CI ceiling.

**Smoke test sample** (per the 4 dedicated build fixtures):

```python
# wheel_basic_install fixture, source.mochi -> emitted Python entry point
# src/mochi_basic/__main__.py
from .generated.main import main

if __name__ == "__main__":
    main()
```

```bash
# runner pseudocode
$ uv build --wheel
Successfully built dist/mochi_basic-0.1.0-py3-none-any.whl
$ uv pip install dist/mochi_basic-0.1.0-py3-none-any.whl
$ python -m mochi_basic
hello, mochi
$ diff -u expect.txt <(python -m mochi_basic)
$ echo $?
0
```

**Failure modes the gate catches**:

- Missing `py.typed` (pyright reports stubs missing on consumer side).
- Wrong package layout (hatchling can't find the package).
- Missing `__main__.py` (`python -m <pkg>` exits with `No module named __main__`).
- Mochi runtime not in `[project].dependencies` (`ImportError: mochi_runtime` at first call).
- Wrong `requires-python` (install fails on a non-matching interpreter; the negative test asserts the failure mode on a 3.11 runner).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/pyproject.go` | PEP 621 `pyproject.toml` emitter: `[project]`, `[build-system]`, `[tool.hatch.build.targets.wheel]`, `[tool.hatch.build.targets.sdist]` |
| `transpiler3/python/build/driver.go` | `uv build --wheel` / `--sdist` invocation; `--out-dir` plumbing; `SOURCE_DATE_EPOCH` passthrough |
| `transpiler3/python/build/install.go` | `uv venv` + `uv pip install dist/*.whl` + `python -m <pkg>` execution; stdout capture |
| `transpiler3/python/emit/init.go` | `src/<pkg>/__init__.py` and `src/<pkg>/__main__.py` re-export emitter |
| `transpiler3/python/emit/pytyped.go` | `py.typed` (PEP 561) marker emitter |
| `transpiler3/python/build/phase15_test.go` | `TestPhase15WheelSdistPython`: 4 fixtures + cumulative re-run; tier-1 OS matrix |
| `tests/transpiler3/python/fixtures/phase15-wheel-sdist/wheel_basic_install/` | minimal hello-world wheel fixture |
| `tests/transpiler3/python/fixtures/phase15-wheel-sdist/wheel_with_extras/` | wheel with `[project.optional-dependencies]` (`jupyter`, `ai`) |
| `tests/transpiler3/python/fixtures/phase15-wheel-sdist/sdist_basic_install/` | minimal sdist + `uv pip install dist/*.tar.gz` |
| `tests/transpiler3/python/fixtures/phase15-wheel-sdist/sdist_with_extras/` | sdist with extras + tests directory |

## Test set

- `TestPhase15WheelSdistPython` -- 4 dedicated fixtures plus cumulative re-run of every Phase 1-14 fixture through the wheel install path. Sub-tests:
  - `TestPhase15WheelSdistPython/wheel_basic_install`
  - `TestPhase15WheelSdistPython/wheel_with_extras`
  - `TestPhase15WheelSdistPython/sdist_basic_install`
  - `TestPhase15WheelSdistPython/sdist_with_extras`
  - `TestPhase15WheelSdistPython/cumulative_phase1_14` (re-runs prior fixtures through `uv build` + install + `python -m`)
- `TestPhase15PyprojectShape` -- parses emitted `pyproject.toml` and asserts PEP 621 field presence (`name`, `version`, `requires-python`, `dependencies`, `[build-system].requires`).
- `TestPhase15WheelLayout` -- inspects the wheel zip and asserts `py.typed`, `__main__.py`, `RECORD`, `METADATA` presence.

## Deferred work

- Editable installs (`uv pip install -e .`, PEP 660 hatchling hook). The Mochi dev workflow needs editable mode, but the wheel gate ships without it; deferred to a Phase 15.4 sub-phase.
- ABI3 / per-platform wheel tags (`cp312-abi3-manylinux_2_17_x86_64`) for any future runtime C extension. Out of v1 scope; tracked in research note 07 §3.
- `uv.lock` cross-platform lockfile emission. Useful for downstream CI but orthogonal to the build gate; deferred until a real consumer asks.
- `[project.optional-dependencies].dev` pin file generation. Currently the emitter writes the dev extras inline; a separate `requirements-dev.txt` generator is deferred.
