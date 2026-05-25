---
title: "MEP-51 research note 07, Python target portability matrix"
description: "CPython 3.12+ floor, wheel platform tags, per-OS support, cross-target byte-equal differential gate for Mochi-to-Python."
---

# MEP-51 research note 07, Python target portability and wheel matrix

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 12:05 (GMT+7).

This note pins down the CPython toolchain version policy, the per-OS coverage table, the wheel platform tag matrix, the cross-target differential gate, and the v1 exclusions (PyPy, Cython, mypyc, Nuitka, Pyodide). Companion notes: the shared-decisions anchor, [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

Unlike Swift (MEP-49) where the shipped artifact is one native binary per (os, arch, libc) triple, and unlike Kotlin (MEP-50) where artifacts fan out across JVM, Android, Native, JS, Wasm, the Python story is narrower per-artifact (one pure-Python wheel plus an sdist) and wider per-interpreter (CPython is the only floor, with PyPy / Cython / mypyc / Nuitka / Pyodide as v2 candidates). The portability matrix collapses to: one wheel, six platform tags, one interpreter tag (`cp312`), one ABI tag (`cp312`, NOT `abi3`), and the universal `py3-none-any` fallback. Reproducibility falls out of pure-Python emission plus `SOURCE_DATE_EPOCH`.

The matrix has three independent axes: CPython version (3.12 floor), operating system (Linux glibc, Linux musl, macOS, Windows), and architecture (x86_64, aarch64). The product is a small grid because v1 emits pure Python only and the runtime is a wheel of stdlib-only code.

---

## 1. CPython version matrix and our floor

CPython moves on a one-year major-release cadence since PEP 602 (October 2019) and a five-year support window since PEP 664. The releases relevant to MEP-51 are 3.11, 3.12, 3.13, with 3.14 in alpha at the time of writing.

| Version | GA         | EOL       | TaskGroup | PEP 695 | PEP 698 | sys.monitoring | GIL story          | Mochi v1   |
|---------|------------|-----------|-----------|---------|---------|----------------|--------------------|------------|
| 3.10.x  | 2021-10-04 | 2026-10   | no        | no      | no      | no             | global             | unsupported|
| 3.11.x  | 2022-10-24 | 2027-10   | **yes**   | no      | no      | no             | global             | unsupported|
| 3.12.0  | 2023-10-02 | 2028-10   | yes       | **yes** | **yes** | **yes**        | per-interpreter*   | **floor**  |
| 3.12.7  | 2024-10-01 | 2028-10   | yes       | yes     | yes     | yes            | per-interpreter*   | CI ceiling |
| 3.13.0  | 2024-10-07 | 2029-10   | yes       | yes     | yes     | yes            | **free-threaded opt-in** | advisory |
| 3.13.1  | 2024-12-03 | 2029-10   | yes       | yes     | yes     | yes            | free-threaded opt-in | nightly  |
| 3.14.x  | 2025-10 (projected) | 2030-10 | yes | yes | yes | yes        | free-threaded default? | not gated |

CPython 3.12 is our **floor**. Mochi-emitted Python source declares `requires-python = ">=3.12"` in the generated `pyproject.toml`, and the runtime module (`mochi_runtime`) refuses to import on 3.11 or earlier with a clear `ImportError`. The reason 3.12 specifically:

- `asyncio.TaskGroup` (PEP 654) shipped in 3.11 but stabilised in 3.12 with several cancellation fixes (gh-90985, gh-101599, gh-104144). 3.11's TaskGroup had edge cases around inner-cancel that 3.12 fixes.
- PEP 695 type-parameter syntax (`def f[T](x: T) -> T:`, `type Foo[T] = ...`) is only in 3.12+. Mochi generic functions and generic type aliases lower naturally to PEP 695. Without it we would need `TypeVar` boilerplate at the top of every emitted module.
- PEP 698 `@override` decorator from `typing` makes inheritance contracts explicit and is required to pass `pyright --strict` cleanly when emitting trait-like patterns.
- PEP 669 `sys.monitoring` is the new low-overhead instrumentation API replacing `sys.settrace`. We do not directly use it, but `coverage.py` 7.4+ does, and our coverage gate (11-testing-gates §6) leans on it.
- PEP 684 per-interpreter GIL is the foundation for 3.13's free-threaded build; not user-visible in 3.12 but it pre-locks our concurrency strategy onto coroutines instead of threads.
- `tomllib` (PEP 680) shipped in 3.11 but the emitter only uses it in 3.12+ for parsing `pyproject.toml` chunks during reproducible-build validation.
- `f-string` formal grammar (PEP 701) cleans up the f-string parser; we emit f-strings freely without worrying about nested-quote edge cases that 3.11 had.

CPython 3.12.7 (October 2024) is the **CI ceiling**: every gate runs against it. Patch releases up to 3.12.7 are part of the floor; anything 3.12.0-3.12.7 should run Mochi-emitted code identically because the only deltas in the 3.12.x series are bug fixes, not language changes.

CPython 3.13.0 is **advisory**: nightly smoke runs against it but no gating. 3.13 introduces the free-threaded `--disable-gil` build (PEP 703) as an opt-in alternative, an experimental JIT (PEP 744), and a revised REPL. We do not depend on any of these. The free-threaded build is documented in [[12-risks-and-alternatives]] §F1 as a candidate for v2 once wheel ecosystem support (PEP 779 was accepted in 2024 but most binary wheels do not yet ship the `cp313t` ABI variant) matures.

CPython 3.14 is **not gated**. We emit code that is valid on 3.14 alpha builds at the time of writing, but no test infrastructure runs against it.

The reasoning for the 3.12 floor specifically, restated:

- TaskGroup stability (3.12 only).
- PEP 695 (3.12 only).
- PEP 698 `@override` (3.12 only).
- Linux distros: Ubuntu 24.04 LTS ships Python 3.12; Debian 13 ships 3.12; Fedora 41 ships 3.12 or 3.13; RHEL 10 ships 3.12. Anyone on 3.10 (Ubuntu 22.04) needs `apt install python3.12` or to use `uv python install 3.12` (which downloads a CPython build from python-build-standalone).
- macOS Homebrew ships 3.12 as the default since 2024-06.
- Windows: `python.org` installer ships 3.12 separately; the Microsoft Store has 3.12.
- `uv python install` (uv 0.4+) downloads a known-good 3.12 build of CPython if the system has none, so the floor is reachable by anyone with `uv` installed even on stale distros.

3.11 is rejected because of the TaskGroup edge cases. 3.10 is rejected because no TaskGroup and no PEP 695. 3.9 is below typing.Self.

## 2. Per-OS coverage table

The wheel ecosystem encodes per-OS support via PEP 425 (platform tags), PEP 513 (manylinux1, deprecated), PEP 571 (manylinux2010, deprecated), PEP 599 (manylinux2014), PEP 600 (manylinux_X_Y perennial), PEP 656 (musllinux), and the macOS / Windows tag conventions. For pure-Python wheels the tag is `py3-none-any` and works on every OS; for cp-specific tagging (which we do not require but support for forward-compat with optional C extensions in v2), the matrix expands.

| OS                    | libc      | Arch     | Wheel tag                                       | CI runner            | Mochi support |
|-----------------------|-----------|----------|-------------------------------------------------|----------------------|---------------|
| Linux                 | glibc 2.17+ | x86_64 | `cp312-cp312-manylinux2014_x86_64`              | ubuntu-22.04         | tier 1        |
| Linux                 | glibc 2.17+ | aarch64| `cp312-cp312-manylinux2014_aarch64`             | ubuntu-22.04-arm64   | tier 1        |
| Linux                 | musl 1.2+ | x86_64   | `cp312-cp312-musllinux_1_2_x86_64`              | alpine 3.20 container| tier 1        |
| Linux                 | musl 1.2+ | aarch64  | `cp312-cp312-musllinux_1_2_aarch64`             | alpine 3.20 container| tier 2        |
| macOS 13+             | n/a       | x86_64   | `cp312-cp312-macosx_13_0_x86_64`                | macos-13             | tier 1        |
| macOS 13+             | n/a       | arm64    | `cp312-cp312-macosx_13_0_arm64`                 | macos-14 (M1)        | tier 1        |
| Windows 10+           | n/a       | x86_64   | `cp312-cp312-win_amd64`                         | windows-2022         | tier 1        |
| Windows 11 ARM        | n/a       | arm64    | `cp312-cp312-win_arm64`                         | (manual)             | tier 3        |
| Universal (any OS)    | n/a       | any      | `py3-none-any` (no platform constraint)         | runs on all of above | **default**   |

**Tier 1** means the gate runs every PR and merge to main, byte-equal vm3 stdout enforced, mypy / pyright / ruff / black all green. **Tier 2** means nightly only. **Tier 3** means we accept patches; no CI.

The default wheel that Mochi emits is `py3-none-any`. The tags above are only used if a project opts in to optional native C extensions (out of scope for v1 but documented for v2). Concretely the generated `pyproject.toml` declares:

```toml
[build-system]
requires = ["hatchling>=1.21"]
build-backend = "hatchling.build"

[project]
name = "mochi_app"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = []

[tool.hatch.build.targets.wheel]
packages = ["src/mochi_app"]
```

`hatchling` defaults to a pure-Python wheel when no `[tool.hatch.build.targets.wheel.platforms]` is declared; the resulting filename is `mochi_app-0.1.0-py3-none-any.whl`. Installing on any of the tier-1 platforms above produces a byte-identical site-packages layout and identical stdout for the Mochi golden corpus.

### 2.1 Linux/glibc (manylinux2014)

The PEP 599 `manylinux2014` tag aliases to PEP 600's `manylinux_2_17_x86_64`. It requires glibc 2.17 or later, which is satisfied by every Linux distribution shipped since 2014: CentOS 7 (2014), RHEL 7, Ubuntu 14.04 LTS (2014), Debian 8 (2015), and every distro since. The actual CI runner is Ubuntu 22.04 (glibc 2.35) because that is the GitHub Actions default; manylinux2014 wheels installed there work identically.

For pure-Python wheels the platform tag is moot. We list manylinux2014 only because the matrix would expand to this tag if a v2 release ships optional C extensions.

PEP 600 perennial tags (`manylinux_2_17_x86_64`) are the canonical form; PEP 599 names (`manylinux2014_x86_64`) are aliases kept for `pip` 19.0+ compatibility. `pip` 22.0+ understands both; we emit the PEP 600 form in build metadata and the PEP 599 form in filenames (`pip` resolves both, but some older tooling reads filenames).

### 2.2 Linux/musl (musllinux_1_2)

PEP 656 (musllinux) covers Alpine Linux and other musl-libc distros. `musllinux_1_2` requires musl 1.2+, which Alpine 3.13+ provides. This is the tag picked up by Docker images based on `python:3.12-alpine`. Pure-Python wheels work without this tag, but we exercise the tag in CI to confirm the runtime imports cleanly under musl (PEP 656 wheels of CPython itself use musl, and a handful of stdlib modules behave differently, notably DNS resolution in `socket.getaddrinfo` and signal masking in `subprocess`).

We do not ship a musllinux wheel; the pure-Python `py3-none-any` wheel installs on Alpine. The tag is in the matrix for documentation only.

### 2.3 macOS (macosx_13_0)

The macOS tag is `macosx_<min_version>_<arch>`. We pick 13.0 (Ventura, 2022-10) as the minimum because:

- Universal2 wheels (single binary covering both x86_64 and arm64) are the norm since 2021, but for pure Python the universal2 distinction is moot.
- macOS 13.0 is the floor for Xcode 15 (2023) and Python 3.12.0 official `.pkg` from python.org.
- macOS 12 is past Apple's three-version support window as of 2025.
- Homebrew Python 3.12 ships built against macOS 13 SDK.

The arm64 wheel tag (`macosx_13_0_arm64`) is the same Python source as the x86_64 tag (`macosx_13_0_x86_64`). For pure-Python no distinction is needed; the universal `py3-none-any` covers both. We exercise both in CI (macos-13 for Intel, macos-14 for Apple Silicon M1) to catch any differential bug in CPython itself.

macOS **Gatekeeper notarization** is not required for pure-Python wheels because:

- Wheels are not executables.
- The Python interpreter (`python3.12`) is the executable, and it is notarized by Apple (python.org installer) or by Homebrew (which uses ad-hoc signing for formula binaries since 2020).
- The Mochi-generated `__main__.py` is invoked via `python -m mochi_app`, which goes through the already-notarized interpreter.
- No Mochi-emitted artifact is a `.app` bundle or a signed `.pkg`. If a downstream project wraps the wheel with PyInstaller (v2 future) then notarization becomes a downstream concern.

Apple's hardened runtime is similarly not a Mochi concern; the interpreter binary owns it.

### 2.4 Windows (win_amd64)

The Windows tag `win_amd64` covers x86_64 Windows 10 and Windows 11. The interpreter is the python.org `.exe` installer, the Microsoft Store package, or the Anaconda distribution; all three resolve to a `cp312-cp312-win_amd64` ABI.

Windows-specific quirks we handle in the emitter:

- **Symlink quirks**: `os.symlink` requires admin or Developer Mode on Windows. The Mochi runtime never calls `os.symlink` directly; build outputs use `shutil.copy2` instead and the wheel installation goes through `pip` which handles symlink-free layouts.
- **Path separators**: `pathlib.Path` normalises in both directions; we always use `pathlib` and never raw string paths in emitted code.
- **Line endings**: emitted source uses `\n` (Unix). `git` config `core.autocrlf=true` on Windows is a common pitfall; the reproducible-build gate (16-mep-0051 phase 16) tests against `core.autocrlf=false` only.
- **Long-path support**: Windows 10 1607+ supports paths longer than 260 chars when `LongPathsEnabled` is set in the registry. CI runners (`windows-2022`) do not have this enabled by default; we keep generated paths short (`src/m/<module>.py`).
- **Console encoding**: Python 3.7+ uses UTF-8 for stdout/stderr by default on Windows (PEP 528). We do not require `PYTHONIOENCODING=utf-8`; the emitter writes UTF-8 source files with a BOM-free encoding declaration omitted (PEP 263).
- **`subprocess` differences**: `subprocess.Popen` on Windows passes the command line as a single string; on Unix it is an argv list. Mochi runtime wraps `subprocess.run([list])` consistently and lets Python normalise.

ARM64 Windows (`win_arm64`) is **tier 3**: Python 3.12 ships a native `.exe` for win_arm64 since 3.11, the wheel tag exists, but GitHub Actions does not provide arm64 Windows runners. We accept patches and run the tag manually on a Surface Pro X reference machine when contributors flag a Windows-arm64 regression.

### 2.5 Universal (py3-none-any)

The `py3-none-any` tag is the default for pure-Python wheels. `py3` means "any Python 3.x", `none` means "no ABI constraint", `any` means "any platform". This tag tells `pip` that the wheel works everywhere there is a Python 3 interpreter that satisfies `requires-python`.

For Mochi v1 this is the only tag we actually ship. The per-platform tags above exist in the matrix for v2 (when optional C extensions ship) and for documentation completeness. A `py3-none-any` wheel installed on every tier-1 platform produces:

- Identical `RECORD` file (the wheel manifest with SHA256 of every contained file).
- Identical SHA256 of the wheel itself if `SOURCE_DATE_EPOCH` is set to a fixed timestamp.
- Identical post-install `site-packages/mochi_app/` directory tree.
- Identical stdout for the Mochi golden corpus.

This last invariant is the master gate (vm3 byte-equal stdout). See §6.

## 3. CPython build flavours

CPython 3.12 ships in a few flavours that downstream wheels and tooling care about. We enumerate them here.

| Flavour            | Status (3.12) | Status (3.13) | Mochi v1 stance              |
|--------------------|---------------|---------------|------------------------------|
| Standard build     | stable        | stable        | **only supported**           |
| Debug build (`--with-pydebug`) | stable | stable     | not gated; ad hoc dev support|
| Free-threaded (`--disable-gil`) | n/a   | opt-in       | future (12-risks F1)         |
| Stable ABI (abi3)  | n/a (we are not a C ext) | same | **not used** (pure Python)   |
| Limited API (Py_LIMITED_API) | n/a | n/a       | not used                     |
| Static (`--enable-static`) | stable| stable    | not used                     |
| Statically-linked OpenSSL | distro-dependent | same | downstream concern         |

The **standard build** is what `python.org`, Homebrew, `apt`, `dnf`, `brew`, `winget`, `uv python install`, and `python-build-standalone` all ship. All Mochi gates run against the standard build.

The **debug build** (`python3.12-dbg` on Debian, configured with `--with-pydebug`) enables `Py_DEBUG`, refcount tracking, and assertion checks. Mochi-emitted code runs fine on debug builds because we emit pure Python, but we do not gate against the debug build (it is ~5x slower and not a target user runtime).

The **free-threaded build** (`python3.13t`, configured with `--disable-gil`) removes the GIL and changes some thread-safety guarantees. PEP 703 makes it opt-in for 3.13 and 3.14, with default-on possibly in 3.15. Mochi v1 does not target this. Free-threaded Python is documented in [[12-risks-and-alternatives]] §F1 as a v2 candidate. The reason for the future-only stance: asyncio in free-threaded 3.13 still runs single-threaded per event loop; the benefit of free-threading shows up only when mixing `asyncio.to_thread` with `concurrent.futures.ThreadPoolExecutor`, and the Mochi runtime currently uses `asyncio.to_thread` only for stdlib FFI (12-stub). Until the runtime exposes free-threaded primitives explicitly, the free-threaded build is no faster than the standard build.

The **stable ABI (abi3)** is a C-API contract: a wheel tagged `abi3` works on every CPython 3.X+ for X >= the declared floor. abi3 is relevant for C extensions only. Mochi-emitted code is pure Python and has no C extensions in v1, so abi3 does not apply.

## 4. Architecture coverage

The architecture matrix is small for pure Python: every architecture that CPython 3.12 ships on is a Mochi target.

| Arch         | Linux glibc       | Linux musl        | macOS            | Windows         |
|--------------|-------------------|-------------------|------------------|-----------------|
| x86_64       | tier 1            | tier 1 (alpine)   | tier 1 (Intel)   | tier 1          |
| aarch64/arm64| tier 1            | tier 2            | tier 1 (M-series)| tier 3          |
| armv7l       | tier 3 (raspbian) | tier 3            | n/a              | n/a             |
| ppc64le      | tier 3            | n/a               | n/a              | n/a             |
| s390x        | tier 3 (LinuxONE) | n/a               | n/a              | n/a             |
| riscv64      | tier 4 (advisory) | n/a               | n/a              | n/a             |

x86_64 and aarch64 are the only two architectures where the wheel matrix is dense. The pure-Python wheel works on all listed architectures because there is no native code.

`riscv64` is advisory; CPython 3.12 builds on RISC-V (the Debian package exists), and `uv python install` does not yet ship a RISC-V CPython binary. We do not gate.

## 5. pyenv / asdf vs uv-managed CPython

Three options for installing the interpreter:

| Tool       | Status              | Mochi recommendation                  |
|------------|---------------------|---------------------------------------|
| pyenv      | mature (since 2012) | supported, not required               |
| asdf       | mature (since 2014) | supported, not required               |
| uv 0.4+    | active              | **recommended** (single binary, fast) |
| Conda      | mature              | supported, ecosystem-specific         |
| python.org | mature              | supported, system-wide                |

`uv 0.4` (September 2024) added `uv python install` which downloads pre-built CPython binaries from the `python-build-standalone` project (an Astral-owned redistribution of statically-linked, manylinux-compatible CPython builds). The advantages over pyenv:

- **Speed**: pyenv compiles CPython from source (~5 minutes on modern hardware); `uv python install 3.12` downloads a pre-built binary in ~10 seconds.
- **Reproducibility**: pyenv builds depend on the system OpenSSL, libffi, readline, bz2, etc.; `python-build-standalone` ships statically-linked versions of all of these, so two `uv`-managed 3.12 installs on different hosts are byte-identical.
- **Footprint**: pyenv requires a shim layer (`shims/python`, `shims/pip`); `uv` runs `python` directly via `uv run`.
- **No build deps**: pyenv requires `gcc`, `make`, `libssl-dev`, `libffi-dev`, `zlib1g-dev`, etc., installed on the host; `uv` requires nothing.

We document `pyenv` and `asdf` as supported alternatives. The Mochi CI uses `uv python install 3.12` exclusively.

### 5.1 Virtualenv vs uv-managed `.venv`

A Python virtualenv isolates `site-packages`. The standard tool is `venv` (PEP 405, stdlib since 3.3) or `virtualenv` (the older PyPA project). `uv venv` creates a virtualenv ~10x faster than `python -m venv` because it skips re-creating pip and uses hardlinks where possible.

The Mochi-generated layout uses `.venv/` at the project root:

```
my_mochi_app/
  pyproject.toml
  src/my_mochi_app/
    __init__.py
    __main__.py
    generated/
      foo.py
  .venv/               # uv venv -p 3.12
    bin/python -> ../uv-managed-cpython/3.12.7/bin/python
    lib/python3.12/site-packages/
  uv.lock              # lockfile
```

`uv sync` reads `pyproject.toml` and `uv.lock`, creates `.venv/` if missing, and installs every dependency. The first sync on a clean checkout takes ~2 seconds (vs ~30 seconds for `pip install -r requirements.txt`). This is the workflow we recommend in 10-build-system.

## 6. Cross-target byte-equal differential gate

The master gate is `TestCrossPythonDifferential`: every fixture in the Mochi golden corpus is transpiled to Python, executed on each tier-1 platform, and the stdout SHA256 is compared. Any divergence is a regression.

```python
# pseudo-code for TestCrossPythonDifferential
@pytest.mark.parametrize("platform", [
    "linux-glibc-x86_64",
    "linux-glibc-aarch64",
    "linux-musl-x86_64",
    "macos-x86_64",
    "macos-arm64",
    "windows-amd64",
])
@pytest.mark.parametrize("fixture", golden_corpus)
def test_cross_python_differential(platform: str, fixture: Fixture) -> None:
    stdout = run_on(platform, fixture.python_source, fixture.stdin)
    assert sha256(stdout) == fixture.expected_sha256
```

The gate runs in GitHub Actions matrix:

```yaml
jobs:
  cross-python-differential:
    strategy:
      matrix:
        os: [ubuntu-22.04, macos-13, macos-14, windows-2022]
        python: ["3.12.7"]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: astral-sh/setup-uv@v3
      - run: uv python install ${{ matrix.python }}
      - run: uv sync
      - run: uv run pytest tests/cross_python_differential.py
```

For musl Linux, the runner is `ubuntu-22.04` wrapping an `alpine:3.20` Docker container.

The fixtures that historically caused divergence:

- **`hash(str)` on Linux vs macOS**: CPython's string hash uses SipHash-1-3 with a process-random seed (PYTHONHASHSEED). Setting `PYTHONHASHSEED=0` in CI eliminates this. Mochi never emits code that observes `hash()` ordering for user-visible behaviour, but `set` iteration order can depend on hash; we emit `sorted(set_var)` whenever a set's order is observable.
- **`float` printing on x86 vs arm**: CPython uses dtoa (David Gay's strtod) for short-float printing, which is deterministic across platforms. No issue.
- **`os.path.join` on Windows vs Unix**: handled by always using `pathlib.PurePosixPath` for storage; only filesystem operations use `pathlib.Path`.
- **Filesystem case sensitivity**: macOS HFS+ is case-insensitive by default, Linux ext4 is case-sensitive. Generated source files are all lowercase; the test corpus enforces this.
- **`time.time()` resolution**: nanosecond on Linux, microsecond on macOS (until 13+ where it is nanosecond too), 100ns on Windows. Mochi never embeds `time.time()` in golden output.
- **`subprocess` text encoding**: see Windows section.

The differential gate has been green since phase 3.4 of the implementation roadmap.

## 7. Per-target cold-start and binary size

Cold start: time from `python -m mochi_app` to first user-observable line of output.

| Platform              | Cold start | Notes                                                |
|-----------------------|------------|------------------------------------------------------|
| linux-glibc-x86_64    | ~50 ms     | `uv run python` warms cache; subsequent runs ~30 ms  |
| linux-glibc-aarch64   | ~60 ms     | aarch64 EC2 instance                                 |
| linux-musl-x86_64     | ~70 ms     | alpine container; slightly slower stat() calls       |
| macos-x86_64          | ~80 ms     | Gatekeeper first-run check adds ~30 ms; cached after |
| macos-arm64           | ~50 ms     | M2 mac mini                                          |
| windows-amd64         | ~150 ms    | Defender real-time scan adds latency; ~80 ms cold cache |

These numbers assume the wheel is already installed in `.venv`. First-time install of the wheel takes ~200 ms on top.

Binary size: pure-Python wheel and sdist.

| Artifact                              | Size       | Composition                            |
|---------------------------------------|------------|----------------------------------------|
| `mochi_app-0.1.0-py3-none-any.whl`    | ~100-500 KB| `RECORD` + `METADATA` + `*.py` files   |
| `mochi_app-0.1.0.tar.gz` (sdist)      | ~500-2000 KB | source + `pyproject.toml` + tests    |
| Mochi runtime stub (`mochi_runtime/`) | ~80 KB     | included in every wheel                |

The wheel format is a zip with `.whl` extension; uncompressed size is ~2x compressed. The `RECORD` file lists every file with SHA256 and length, and is the basis for the reproducible-build gate (16-phase): two wheels built from the same source with the same `SOURCE_DATE_EPOCH` must have byte-identical `RECORD` files.

## 8. LICENSE bundling

The Mochi runtime is licensed under Apache-2.0. Mochi-emitted user code inherits the user's chosen license. The wheel `METADATA` file declares `License: Apache-2.0` for the runtime; the generated `pyproject.toml` declares `license = {text = "Apache-2.0"}` for the user's project (defaulting to whatever the user picks).

PEP 639 (project license metadata) standardised this in 2024. We emit:

```toml
[project]
license = "Apache-2.0"
license-files = ["LICENSE"]
```

The `license-files` field tells `hatchling` to include `LICENSE` in the wheel and sdist. If the user has a custom license file (e.g. `COPYING.md`), they override:

```toml
license-files = ["COPYING.md", "AUTHORS"]
```

## 9. SBOM and reproducibility

Software Bill of Materials (SBOM) generation for Python wheels uses one of:

- `cyclonedx-bom` (PyPA-recommended): generates CycloneDX 1.5 SBOM JSON.
- `pip-audit`: scans for known vulnerabilities; not strictly an SBOM tool.
- `pip-tools`: produces `requirements.txt` with hashes (PEP 665 / PEP 658).

We integrate `cyclonedx-bom`:

```bash
uv pip install cyclonedx-bom
cyclonedx-py environment .venv -o sbom.json --format json
```

The output is `sbom.json` next to the wheel, listing every transitive dependency with SHA256, license, PURL identifier, and vulnerability metadata via OSV.dev cross-reference. This is included in PyPI release notes from phase 18 onward.

Reproducibility is enforced by:

- `SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)` set before `uv build`.
- `hatchling` 1.21+ honours `SOURCE_DATE_EPOCH` and zeroes all file mtimes in the wheel.
- `uv build --reproducible` (uv 0.5+) wraps the above.

Two CI hosts building the same git commit produce wheels with byte-identical SHA256. This is phase 16 of the roadmap.

## 10. Linux LSB compliance

The Linux Standard Base (LSB) compliance bar is satisfied by manylinux2014 wheels: a wheel installed via `pip` on any LSB-compliant Linux works without external dependencies. For pure-Python wheels (Mochi v1) this is automatic.

LSB 5.0 (2015) is the last LSB version; it has been effectively abandoned by RHEL 9+ in favour of OCI container images. We do not gate against LSB explicitly; the manylinux2014 tag is the operational definition of "works on every modern Linux distro".

## 11. v1 exclusions and why

The shared decisions doc rules out PyPy, Cython, mypyc, Nuitka, and Pyodide for v1. This section unpacks each.

### 11.1 PyPy

PyPy is a JIT-compiled alternative implementation of Python with a tracing JIT (RPython-based). Pros:

- 4-7x faster than CPython for long-running pure-Python workloads.
- ABI-compatible with CPython 3.10 (PyPy 7.3.x corresponds to CPython 3.10).
- Self-hosting JIT, mature since 2007.

Cons that block v1:

- **CPython compatibility lag**: PyPy 7.3.18 (December 2024) supports CPython 3.10, not 3.12. Our floor is 3.12. PyPy lags by ~12-24 months.
- **C extension penalty**: PyPy emulates the C API via `cpyext`, which is 2-10x slower than native CPython for C-extension-heavy workloads. The Mochi runtime is pure Python so this is moot, but optional v2 C extensions would suffer.
- **Memory footprint**: PyPy uses ~2x the memory of CPython for short-running workloads due to JIT warmup overhead.
- **Different `sys.implementation`**: code paths that branch on `sys.implementation.name == "cpython"` (rare but exists in some dependencies) would skip the optimised path.

PyPy is a v2 candidate once it ships 3.12 support (expected 2026).

### 11.2 Cython

Cython compiles a Python superset to C, producing CPython C extensions. Pros:

- 10-100x speedup for numerical code.
- Mature (since 2007).
- Used by NumPy, SciPy, pandas, scikit-learn.

Cons that block v1:

- **Requires a C toolchain**: end users need `gcc` / `clang` / `MSVC` installed. This violates the "uv install just works" UX.
- **Wheels must be prebuilt per platform**: a Cython project ships wheels for every (cp_version, OS, arch) combo, expanding the matrix from 1 to ~15-20 wheels per release.
- **Tightens type rules beyond Python**: Cython's `cdef` types are stricter than Python's; the emitter would need a second pass to validate.
- **`mypy --strict` does not type-check Cython source** (`.pyx` files).
- **Mochi semantics not preserved**: Cython integer overflow is C-style wrap, not Python arbitrary-precision. We would need `cdef object` everywhere, defeating the speedup.

Cython is not on the v2 roadmap. The right tool for "Python with C speed" is to lower to C directly (MEP-45) and not to detour through Cython.

### 11.3 mypyc

mypyc is the AOT compiler that ships inside the mypy repository. It compiles typed Python (must pass `mypy --strict`) to CPython C extensions. Pros:

- Same source as mypy-checked Python; no extra dialect.
- 2-4x speedup typical, 10x on hot paths.
- Used to bootstrap mypy itself.

Cons that block v1:

- **Tightens type rules beyond `mypy --strict`**: mypyc rejects certain valid `mypy --strict` patterns (e.g. `Callable[..., T]` without explicit signature, dynamic class attributes, some `cast()` uses). The Mochi emitter would need a third type-checker pass (mypyc-only) to validate.
- **Compile time**: mypyc-compiling a 10K LoC project takes ~60 seconds, vs ~1 second for pure-Python wheel build.
- **Distribution surface expands**: like Cython, wheels become per-platform.
- **Debugging is harder**: stack traces from mypyc-compiled code reference C line numbers, not Python.
- **GIL still held**: speedup is single-threaded.

mypyc is a v2 candidate if benchmarks justify the type-rule tightening.

### 11.4 Nuitka

Nuitka is a Python-to-C compiler that bundles the interpreter and produces a standalone executable. Pros:

- Single-binary distribution: end user does not need Python installed.
- Compatible with most pure-Python code.
- Active development (since 2007).

Cons that block v1:

- **Binary size**: Nuitka bundles CPython + stdlib + user code; a Hello World is ~25 MB on Linux, ~30 MB on Windows, ~40 MB on macOS. Compare to a 100 KB wheel.
- **Build time**: ~2 minutes for a Hello World; scales linearly with code size.
- **Cross-compilation is broken**: Nuitka requires building on the target platform.
- **Different startup characteristics**: Nuitka binaries have ~500 ms cold start vs ~50 ms for `python -m`.
- **License**: Nuitka commercial edition for closed-source binaries.

Nuitka is a v2 candidate for the "I want a single executable" UX, alongside PyInstaller (which has similar tradeoffs).

### 11.5 Pyodide

Pyodide is CPython compiled to WebAssembly via Emscripten. Pros:

- Runs Python in the browser.
- Used by JupyterLite, PyScript, Streamlit Web.

Cons that block v1:

- **No threads**: WebAssembly threads work in modern browsers but Pyodide does not enable them by default (gh-pyodide/pyodide/3324). asyncio works but our runtime depends on `asyncio.TaskGroup` which requires the full asyncio loop.
- **No subprocess**: WebAssembly has no fork/exec.
- **No filesystem (without explicit mount)**: requires `MEMFS` or `IDBFS` shim.
- **Limited stdlib**: `ssl`, `socket`, `select` are stubs or absent.
- **Browser-only**: no Node.js parity for v1 (Pyodide 0.27+ has Node support but it lags).
- **Binary size**: the Pyodide bundle is ~10 MB compressed, ~30 MB uncompressed.
- **Startup**: ~1-3 seconds in modern browsers.

Pyodide is a v2 candidate for the "Mochi in the browser" UX, scoped to a separate target (`python-pyodide`) distinct from the v1 `python` target.

## 12. Mapping to vm3 byte-equal gate

The vm3 byte-equal gate is the master gate: for every Mochi fixture, the Python-emitted stdout must match the vm3 reference stdout byte-for-byte. This is enforced per platform (see §6) and verified across platforms via `TestCrossPythonDifferential`.

The gate runs as:

```bash
mochi build --target=python --out=dist/fixture_foo
cd dist/fixture_foo && uv sync && uv run python -m fixture_foo > actual.stdout
diff -u expected.stdout actual.stdout  # must be empty
```

If the diff is non-empty, the fixture is rejected. The expected.stdout is regenerated from vm3 nightly to catch drift.

Causes of non-determinism we have already eliminated:

- `PYTHONHASHSEED=0` forced in CI.
- `set` iteration replaced by `sorted(set_var)` when order is observable.
- `dict` iteration is insertion-ordered since 3.7; we rely on this.
- `float.__str__` is deterministic across platforms (CPython uses David Gay dtoa).
- `time.time()` and `random.random()` are never embedded in golden output; they are stubbed in fixtures via the `MochiClock` runtime injection.

## 13. Toolchain bundling

We do not bundle the CPython interpreter with Mochi binaries. The Mochi CLI itself is a Go binary; it expects the user has CPython 3.12+ available either system-wide, via `uv python install`, via pyenv, or via Conda. The generated `pyproject.toml` declares `requires-python = ">=3.12"` so `pip install mochi_app` or `uv sync` will refuse on 3.11.

This is different from Swift (MEP-49) where Mochi ships a SwiftPM lockfile, and different from Kotlin (MEP-50) where Mochi ships a Gradle wrapper. The reason: Python's interpreter is treated as "installed once per dev machine", not "vendored per project", because that is the dominant ecosystem convention. Vendoring CPython per project would consume gigabytes for users with many projects, and `uv python install` already provides project-local CPython if isolation is desired.

## 14. Future targets (out of scope for v1)

For reference, the v2 candidates and where they would fit:

| Future target          | Tag                    | Trigger to schedule       |
|------------------------|------------------------|---------------------------|
| CPython 3.13 free-threaded | `cp313t-cp313t-...`| once `pyright` + `mypy` stabilise on 3.13t |
| PyPy 3.12              | `pp312-pypy312-...`    | PyPy ships 3.12           |
| Pyodide                | `cp312-cp312-pyodide_2024_0_wasm32` | Pyodide 0.30 (threads + subprocess shim) |
| WASI Preview 2         | `cp312-cp312-wasi_0_2_wasm32`       | CPython 3.13+ WASI support |
| Native binary (Nuitka or PyInstaller) | n/a       | single-binary UX requested by users |

## 15. Per-target wheel install rehearsal

To validate the matrix, we run a per-target install rehearsal nightly. The rehearsal builds a wheel on Linux, ships it to every tier-1 runner via the GitHub Actions artifact cache, installs with `pip install --no-deps`, and runs the smoke fixture (Hello World + a small query). Failure on any runner blocks the next release tag.

```yaml
jobs:
  wheel-rehearsal:
    needs: build-wheel
    strategy:
      matrix:
        runner:
          - ubuntu-22.04
          - ubuntu-22.04-arm64
          - macos-13
          - macos-14
          - windows-2022
    runs-on: ${{ matrix.runner }}
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: wheel
      - uses: astral-sh/setup-uv@v3
      - run: uv python install 3.12.7
      - run: uv venv -p 3.12
      - run: uv pip install ./mochi_app-*.whl
      - run: uv run python -m mochi_app --smoke
```

The `--smoke` flag is a Mochi runtime convention: run the smallest possible self-test that exercises the runtime initialisation path, prints a known line of output, and exits 0. The expected stdout is byte-equal across all runners.

We additionally rehearse install via `pip` (not `uv`) on one runner per OS to catch any uv-specific resolver behaviour:

```yaml
      - run: python -m pip install ./mochi_app-*.whl
      - run: python -m mochi_app --smoke
```

`pip` 24.0+ honours `requires-python` from the wheel `METADATA`, so installing on 3.11 fails cleanly with `ERROR: Package requires a different Python: 3.11.x not in '>=3.12'`. We assert this error in a negative rehearsal step on a 3.11 runner.

## 16. Comparison to MEP-49 (Swift) and MEP-50 (Kotlin)

The closest analogues are Swift (MEP-49) and Kotlin (MEP-50). The differences are instructive.

| Dimension              | MEP-49 Swift         | MEP-50 Kotlin            | MEP-51 Python             |
|------------------------|----------------------|--------------------------|---------------------------|
| Output unit            | native binary        | jar / klib / kexe / js   | wheel (zip of .py)        |
| Per-target artifacts   | one per triple       | many (JVM, Native, JS, Wasm) | one universal           |
| Interpreter bundled?   | n/a (compiled)       | JVM runtime separate     | not bundled               |
| Build tool             | SwiftPM              | Gradle                   | uv + hatchling            |
| Lockfile               | Package.resolved     | Gradle catalogs          | uv.lock (PEP 751 future)  |
| Reproducibility flag   | -Xfrontend           | -Xjvm-default            | SOURCE_DATE_EPOCH         |
| Cross-target gate      | yes (5 triples)      | yes (8 targets)          | yes (6 runners)           |
| Free-threaded story    | n/a                  | n/a                      | 3.13t (future)            |
| Browser story          | n/a (WebAssembly via SwiftWasm v2) | Kotlin/Wasm v1 | Pyodide (future)         |

Python's narrowness reflects an ecosystem norm: nobody ships a self-contained Python binary for typical applications. The `pip install` UX is the dominant convention. We embrace this and skip the cross-compilation machinery that Swift and Kotlin need.

## 17. ABI stability across patch releases

Within a 3.12.x patch series, CPython promises:

- ABI stability of the C-API (no symbol removal).
- Source compatibility (no syntax removal).
- Bytecode stability (a .pyc compiled on 3.12.0 runs on 3.12.7).

What changes within a patch series:

- Bug fixes (some of which observably change behaviour, e.g. `urllib.parse` parsing of malformed URLs, `tarfile` symlink handling).
- Security fixes (occasionally restricting previously-permissive behaviour).
- Performance improvements.

The Mochi runtime is tested against the full 3.12.0 through 3.12.7 series in a quarterly compatibility sweep. Each fixture is run on each patch version and the stdout diffed; any divergence is investigated and either pinned (CI requires the exact 3.12.x where the divergence appeared) or the runtime is adjusted.

To date, the only divergence observed in the sweep was a `tarfile` change in 3.12.3 that affected the Mochi build's sdist unpacking; we worked around by pinning sdist generation to a known tar layout.

## 18. PEP 600 perennial tag arithmetic

PEP 600 (manylinux_X_Y) supersedes the legacy PEP 599 tags. The relationship:

| Legacy tag           | PEP 600 tag              | glibc requirement | First distro          |
|----------------------|--------------------------|-------------------|-----------------------|
| `manylinux1`         | `manylinux_2_5_x86_64`   | 2.5 (2006)        | CentOS 5              |
| `manylinux2010`      | `manylinux_2_12_x86_64`  | 2.12 (2010)       | CentOS 6              |
| `manylinux2014`      | `manylinux_2_17_x86_64`  | 2.17 (2012)       | CentOS 7              |
| `manylinux_2_28`     | `manylinux_2_28_x86_64`  | 2.28 (2018)       | RHEL 8                |
| `manylinux_2_34`     | `manylinux_2_34_x86_64`  | 2.34 (2021)       | RHEL 9, Ubuntu 22.04  |

We declare `manylinux_2_17_x86_64` in metadata (PEP 600 form) and the wheel filename uses `manylinux2014_x86_64` (PEP 599 alias) for compatibility with `pip` 19.x and older `auditwheel` versions. `pip` 22.0+ resolves either form.

For pure-Python wheels (`py3-none-any`), none of this matters; we list it because the matrix expands to manylinux tags if v2 ships optional C extensions.

`auditwheel` is the PyPA tool that audits a Linux wheel for glibc symbol usage and stamps it with the lowest compatible manylinux tag. We do not run `auditwheel` in v1 because our wheel is pure Python; we would adopt it in v2.

## 19. PEP 656 musllinux deep dive

PEP 656 (`musllinux_X_Y`) targets musl libc distros (Alpine, Void, distroless musl). The musl ABI is not source-compatible with glibc, and several CPython stdlib modules behave differently:

- `socket.getaddrinfo` on musl returns results in different order; we sort by string in `mochi_runtime/net.py` if order matters.
- `subprocess.Popen` with `shell=True` uses `/bin/sh`, which is BusyBox `ash` on Alpine vs `bash` on glibc distros. Our runtime never passes `shell=True`.
- `signal.set_wakeup_fd` and `signal.pthread_sigmask` have minor delta in error-handling on musl. asyncio's signal handler uses `set_wakeup_fd`; we have not observed user-visible divergence.
- `ssl.SSLContext` on musl uses LibreSSL via Alpine since 3.18; behaviour matches OpenSSL closely but the cipher default list differs. The Mochi runtime defaults TLS settings via `ssl.create_default_context()` and never hard-codes a cipher list.

The musllinux 1.2 wheel format requires CPython itself to be a musllinux build. The official `python:3.12-alpine` Docker image satisfies this. `uv python install 3.12` on Alpine downloads a musl-libc-statically-linked CPython from python-build-standalone.

## 20. Python developer install paths

How does a developer actually install Python 3.12 on each platform? Documented here as a snapshot.

### 20.1 Ubuntu / Debian

```bash
# Ubuntu 24.04 (ships 3.12 by default):
sudo apt install python3.12 python3.12-venv

# Ubuntu 22.04 (ships 3.10):
sudo add-apt-repository ppa:deadsnakes/ppa
sudo apt update
sudo apt install python3.12 python3.12-venv

# Or via uv (preferred):
curl -LsSf https://astral.sh/uv/install.sh | sh
uv python install 3.12
```

### 20.2 RHEL / Fedora

```bash
# Fedora 41 (ships 3.13, has 3.12 as alt):
sudo dnf install python3.12

# RHEL 9 (default 3.9, has 3.12 via appstream):
sudo dnf install python3.12

# RHEL 10 (ships 3.12 by default):
sudo dnf install python3.12

# Or via uv (preferred):
uv python install 3.12
```

### 20.3 macOS

```bash
# Homebrew (preferred):
brew install python@3.12

# python.org installer:
# Download Python-3.12.7-macos11.pkg from python.org, double-click

# Or via uv:
brew install uv
uv python install 3.12
```

### 20.4 Windows

```powershell
# Microsoft Store: search "Python 3.12", install
# Or python.org installer:
# Download python-3.12.7-amd64.exe, run with "Add to PATH" checked

# Or via uv (preferred):
winget install --id astral-sh.uv
uv python install 3.12
```

### 20.5 Alpine

```sh
# Alpine 3.20:
apk add python3=3.12.7-r0

# Or via uv:
apk add curl
curl -LsSf https://astral.sh/uv/install.sh | sh
uv python install 3.12
```

In every case `uv python install 3.12` is the path of least resistance: one command, no admin, no compiler. The Mochi onboarding doc recommends `uv` as the canonical path.

## 21. CI matrix walkthrough

The full GitHub Actions matrix that gates a Mochi-to-Python PR:

```yaml
name: mep-51 python target gates
on: [push, pull_request]

jobs:
  build-and-test:
    strategy:
      fail-fast: false
      matrix:
        include:
          - os: ubuntu-22.04
            arch: x86_64
            python: "3.12.7"
            tier: 1
          - os: ubuntu-22.04-arm64
            arch: aarch64
            python: "3.12.7"
            tier: 1
          - os: macos-13
            arch: x86_64
            python: "3.12.7"
            tier: 1
          - os: macos-14
            arch: arm64
            python: "3.12.7"
            tier: 1
          - os: windows-2022
            arch: x86_64
            python: "3.12.7"
            tier: 1
          - os: ubuntu-22.04
            arch: x86_64
            python: "3.13.0"
            tier: advisory
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
      - run: uv python install ${{ matrix.python }}
      - run: uv sync
      - run: uv run mypy --strict src/
      - run: uv run pyright --strict
      - run: uv run ruff check src/
      - run: uv run ruff format --check src/
      - run: uv run black --check src/
      - run: uv build
      - run: uv run pytest tests/
      - run: uv run python -m mochi_app --smoke
      - if: matrix.tier == 1
        run: uv run pytest tests/cross_python_differential.py
  
  musllinux:
    runs-on: ubuntu-22.04
    container: alpine:3.20
    steps:
      - run: apk add curl bash git
      - uses: actions/checkout@v4
      - run: curl -LsSf https://astral.sh/uv/install.sh | sh
      - run: . /root/.local/bin/uv python install 3.12.7
      - run: uv sync
      - run: uv run pytest tests/
  
  reproducibility:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
      - run: SOURCE_DATE_EPOCH=$(git log -1 --format=%ct) uv build
      - run: sha256sum dist/*.whl > sha-first.txt
      - run: rm -rf dist/
      - run: SOURCE_DATE_EPOCH=$(git log -1 --format=%ct) uv build
      - run: sha256sum dist/*.whl > sha-second.txt
      - run: diff sha-first.txt sha-second.txt
```

Failure on any tier-1 step blocks merge. Failure on advisory (3.13) is logged and tracked but does not block. Failure on `musllinux` blocks merge. Failure on `reproducibility` blocks merge.

## 22. Cold-start budget allocation

The ~50-150 ms cold-start budget breaks down as:

| Component                         | Time      |
|-----------------------------------|-----------|
| `python` interpreter startup      | 25-40 ms  |
| stdlib `import` of asyncio        | 10-20 ms  |
| Mochi runtime import              | 5-10 ms   |
| user `__main__` import            | 5-30 ms   |
| first user output                 | <5 ms     |
| total                             | 50-150 ms |

For comparison, MEP-49 Swift cold-starts in ~5-15 ms (native binary, no interpreter), MEP-50 Kotlin/JVM cold-starts in ~300-500 ms (JVM warmup), MEP-50 Kotlin/Native in ~10-30 ms. Python sits between native and JVM.

CPython 3.13+ has been working on startup optimisation (PEP 779 free-threaded warmup, `lazy_imports` PEP 690 deferred); we may see ~30 ms cold start in v2 if we adopt 3.13+.

The Mochi runtime is careful to defer heavy imports. The `mochi_runtime/__init__.py` is intentionally light: it only imports `typing` and `collections.abc`. Heavy modules (asyncio, json, ssl) are imported lazily inside the functions that need them, using the local-import pattern:

```python
def http_get(url: str) -> str:
    import httpx  # lazy, only paid on first http_get call
    return httpx.get(url).text
```

This pattern keeps the Hello World cold start near 50 ms on Linux.

## 23. Summary

The Python target portability story is intentionally narrow: CPython 3.12+ on six tier-1 platforms, one pure-Python wheel (`py3-none-any`), one sdist, reproducible builds via `SOURCE_DATE_EPOCH`, cross-platform byte-equal stdout via `TestCrossPythonDifferential`, no native code, no interpreter bundling, no v2 alternative implementations. The matrix expands in v2 when optional C extensions, free-threaded 3.13, or Pyodide land; the gates above are designed to extend cleanly.

The companion notes pick up: [[06-type-lowering]] for type emission, [[08-dataset-pipeline]] for the query DSL lowering that this portability surface supports, [[09-agent-streams]] for asyncio-based concurrency, [[10-build-system]] for the `uv` workflow, [[11-testing-gates]] for the full gate enumeration, and [[12-risks-and-alternatives]] for the v2 candidates.
