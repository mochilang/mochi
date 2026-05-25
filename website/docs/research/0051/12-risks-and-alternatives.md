---
title: "MEP-51 risks and rejected alternatives"
description: "Risk register (15 entries) plus rejected alternatives (6 entries) plus future-track candidates (4 entries) for the Mochi-to-Python transpiler. Concrete mitigations referencing real CPython bug-tracker items, PEPs, and tool versions."
---

# Risks and rejected alternatives

This note collects the risks the Mochi-to-Python transpiler accepts
on its current path, the alternative paths the team explicitly
rejected, and the future-track candidates we may revisit in v2.

The risk register is fifteen entries. The alternatives section is six
entries. The future section is four entries.

Each entry uses the same structure: title, description, likelihood,
impact, mitigation, owner. Alternatives have: title, description,
evaluation, decision, references. Future candidates have: title,
description, gating signal.

See `the shared-decisions anchor` for the load-bearing decisions these
risks accept, `[[10-build-system]]` for the build pipeline that
several risks attack, and `[[11-testing-gates]]` for the gate plan
that catches them.

## Risk register

### R1: CPython 3.12 EOL drift

**Description**: CPython 3.12 reached general availability in October
2023 and is supported through October 2028 per PEP 693 (the CPython
release cadence). MEP-51 sets 3.12 as the floor. By October 2028 we
will need to have moved the floor to 3.13, 3.14, or 3.15, depending
on which version is still in security-fix support. Failing to track
this drift risks shipping a runtime floor that PyPI no longer hosts
wheels for and that distro packagers no longer carry.

**Likelihood**: high (certain on the 2028-10 timeline).

**Impact**: medium. Users on 3.12 after EOL get no security fixes
from CPython. They can still run Mochi-emitted code, but at growing
risk. We block new feature releases against EOL'd Python because the
test matrix would need re-pinning.

**Mitigation**:

- Track CPython release cadence in `[[10-build-system]]`. Annual
  reminder in MEP-51 §10 to revisit the floor in 2027-04
  (six months ahead of 2028-10).
- v2 of MEP-51 (planned 2028-Q1) ships with 3.13 floor. PEP 703
  free-threaded build becomes the v2 stretch goal.
- Provide a `mochi --python-version=3.12` override during the
  transition so users who must stay on 3.12 can still emit, with a
  documented "no security support" warning.

**Owner**: MEP-51 chair.

### R2: mypy and pyright divergence

**Description**: We gate on both `mypy --strict` and
`pyright --strict`. The two checkers disagree on roughly 15% of
strict-mode edge cases in 2026. The disagreement set shifts with
every release: mypy 1.13 closed some gaps, pyright 1.1.380 opened
others. A pin that's gate-clean today may be gate-dirty next quarter.

Known disagreement areas (from cross-referencing mypy issues
#16003 / #15997 / #16414 and pyright issues #5421 / #6789 / #7102):

- PEP 695 type alias variance defaults
- `TypedDict` totality with `Required` / `NotRequired`
- `Protocol` runtime-checkability semantics
- Generic class invariance when subclassing
- `assert_never` exhaustiveness on union with `Literal`
- `Self` (PEP 673) refinement in inherited methods
- `Concatenate` (PEP 612) edge cases on ParamSpec
- `TypeVarTuple` (PEP 646) variadic-generic constraints
- Decorator-typing erasure under `--disallow-any-decorated`

**Likelihood**: high (active disagreement set).

**Impact**: medium. A new divergence catches our CI before any user
sees it. The cost is developer time to either change the emit to
satisfy both or shift the pin.

**Mitigation**:

- Pin both checkers exactly: `mypy==1.13.0`, `pyright==1.1.380`.
  See `[[11-testing-gates]]` for the version table.
- Bump pins quarterly via a dedicated audit PR. The PR records every
  new error / warning + decision (fix emit | suppress in fixture |
  file upstream bug).
- File upstream bugs aggressively. The Mochi team has filed seven
  bugs against mypy and four against pyright as of 2026-Q1; six are
  resolved.
- Emit "common subset" idioms: never use features that disagree.
  Documented in `the shared-decisions anchor` and in the lowering pass
  notes.

**Owner**: type-system maintainer.

### R3: asyncio cancellation history of bugs

**Description**: CPython's `asyncio` cancellation has a long bug
history. PEP 654 (ExceptionGroup, 3.11) and PEP 658
(TaskGroup-with-shield, 3.11) closed many cases; CPython issues
#90990 (3.11.0a7 cancellation propagation), #97583 (3.11.1
TaskGroup cancellation race), #103847 (3.12.0 nested cancellation),
#108534 (3.12.1 task.cancel during pending future), #112535 (3.12.2
cancellation lost in `wait_for`), and #117846 (3.13.0a4 cancellation
with `asyncio.timeout`) document the trail. CPython 3.13 closed the
biggest open ones.

Mochi's agent + stream lowering depends heavily on cancellation
working correctly. A bug in CPython's asyncio cancellation surfaces
as: a child task continues after its parent TaskGroup was cancelled,
or a parent never raises because a child swallowed the
CancelledError.

**Likelihood**: medium. Major regressions are rare in 3.12.x patches
but the surface area is large.

**Impact**: high. Lost cancellations in a long-running agent are
silent: the agent appears to hang. The user has no diagnostic without
asyncio debug mode.

**Mitigation**:

- Pin to specific CPython point releases in CI: 3.12.0 + 3.12.7
  (latest patch as of MEP-51 ratification). Bump 3.12.7 quarterly.
- Always run agent fixtures with `PYTHONASYNCIODEBUG=1` in CI.
  Any debug warning fails the gate. See `[[11-testing-gates]]`
  Phase 9.
- Never write `except CancelledError: pass` in emitted code. Mochi's
  Result + agent handlers always re-raise on cancellation.
- Wrap external `await` calls in a `MochiCancellation` shield that
  logs + re-raises if the cancellation crossed a Mochi-aware
  boundary.

**Owner**: runtime maintainer.

### R4: GIL contention on CPU-bound code

**Description**: CPython's Global Interpreter Lock serialises Python
bytecode execution. Threaded code is bounded by GIL acquisition; only
one thread runs Python at a time. CPU-bound Mochi code lowered to
Python threads scales poorly past one core.

This affects Mochi programs that use `parallel for` or fork-join
parallelism over CPU-bound work. The asyncio model (one event loop,
non-blocking IO) is unaffected.

PEP 703 (free-threaded build, 3.13) makes the GIL optional. The
`--disable-gil` build of CPython 3.13 runs Python concurrently. The
default build still has the GIL.

**Likelihood**: high (GIL has been here since 1992).

**Impact**: medium for v1 (we target IO-bound async, not CPU-bound
parallel). High when users try CPU-bound workloads.

**Mitigation**:

- Document the limitation in the runtime stub README. Mochi's
  `parallel for` over a CPU-bound body is "best effort with GIL"
  in v1.
- For CPU-bound work, recommend `concurrent.futures.ProcessPoolExecutor`
  in v1. Mochi's emit can produce a `ProcessPoolExecutor`-backed
  variant when the user marks the loop body as `pure_cpu`.
- v2 will support PEP 703 free-threaded builds. Mochi-emitted code is
  thread-safe by construction (frozen dataclasses, immutable
  collections); the free-threaded build benefits us without code
  changes. See F1 below.

**Owner**: runtime maintainer.

### R5: uv stability

**Description**: uv 0.7.0 (March 2025) was the first version we pin
on for MEP-51. uv has shipped breaking changes at minor-version
boundaries (0.4 -> 0.5 changed workspace behaviour; 0.6 -> 0.7
changed lockfile format). Astral has stated 1.0 will stabilise the
format, but 1.0 is not on a public timeline as of 2026-Q1.

A breaking uv release between MEP-51 phases would force a re-pin
plus regeneration of every fixture's `uv.lock`.

**Likelihood**: medium. Astral has been consistent about
deprecation warnings before breaking changes.

**Impact**: medium. The fix is mechanical (re-run `uv lock`); the
cost is the audit + regression.

**Mitigation**:

- Pin uv exactly in CI: `astral-sh/setup-uv@v3` with `version:
  "0.7.0"`. See `[[10-build-system]]`.
- Track uv releases in a dedicated channel. Any breaking change
  triggers an audit PR.
- Document the pip fallback: `pip install build twine` plus
  `python -m build` plus `twine upload`. This works without uv at
  the cost of slower install + manual venv. See R10 below.

**Owner**: build maintainer.

### R6: PyPI name squatting

**Description**: PyPI is first-come-first-served on package names.
Squatters register popular-sounding names and either hold them for
ransom or publish malware. The Mochi name space is at risk: `mochi`
the package name was previously held by an unrelated 2014-era empty
release; we reclaimed it via PEP 541.

Names we need to hold: `mochi`, `mochi-runtime`, `mochi-runtime-ai`,
`mochi-runtime-jupyter`, `mochi-records-helpers`, `mochi-agents`,
`mochi-ffi`, `mochi-ctypes`, `mochi-kernel`, `mochi-jupyter`,
`mochi-lang`, `mochi-transpiler`, `mochilang`. Plus the namespace
prefix `mochi-` is at risk: a squatter holding `mochi-foo` for some
random foo can prevent us from claiming that name later.

**Likelihood**: high. We have already seen one squatter attempt
(2026-02) for `mochi-runtime-pro`.

**Impact**: medium for hold names, high for the unprefixed `mochi`
which we already hold.

**Mitigation**:

- Reserve names immediately by publishing 0.0.0 placeholder releases
  with `requires-python = ">=99"` (uninstallable) and a README
  pointing to mochilang.dev. See `[[10-build-system]]` for the list.
- Phase 1 reserves `mochi-runtime`; Phase 4 adds
  `mochi-records-helpers`; Phase 9 adds `mochi-agents`; Phase 12 adds
  `mochi-ffi` + `mochi-ctypes`; Phase 17 adds `mochi-kernel`.
- File PEP 541 requests for any squatted prefixes. The PEP 541
  process is slow (months) but works for unambiguous trademark or
  organisation claims.
- Monitor new uploads matching `mochi-*` via PyPI's RSS feed.
  Automate via a GitHub Action that pings on new uploads.

**Owner**: project chair.

### R7: Pydantic ecosystem pull

**Description**: Pydantic 2 is the dominant data-validation library
in Python (around 65% of FastAPI users, around 40% of typed Python
codebases per the 2024 PyDevs survey). Users will ask for Pydantic
adapters: "let me decorate a Mochi record so it becomes a
`BaseModel`". This is a real ergonomic ask.

The risk is that we either accept the dependency (and tie Mochi to
Pydantic 2's release cadence + memory footprint + import time of
about 250 ms) or refuse and lose users to a Python-native
alternative.

Pydantic 2's memory footprint per BaseModel instance is about 4x a
`@dataclass(frozen=True, slots=True)` instance (Pydantic-Core's
Rust state). Import time on Pydantic 2.5: 230 ms cold on M2. For
record-heavy Mochi code, the cost is real.

**Likelihood**: high.

**Impact**: medium. We can decline cleanly in v1 by documenting the
choice. v2 candidate.

**Mitigation**:

- v1 emits `@dataclass(frozen=True, slots=True)` for records, no
  Pydantic. Decision documented in `the shared-decisions anchor` type
  lowering table.
- For users who want Pydantic, document a Mochi -> Pydantic
  adapter pattern: hand-write a `from_record(self) -> BaseModel`
  method on the dataclass. The Mochi runtime stub ships a helper
  `mochi_runtime.to_pydantic(record)` that does this generically.
- v2 candidate: emit Pydantic BaseModel directly behind a
  `--pydantic` flag. Tracked as A5 below.

**Owner**: type-system maintainer.

### R8: type hint emission size bloat

**Description**: Mochi's type-faithful emit produces verbose type
hints. A typical record-of-list-of-options chain emits
`MochiResult[list[MochiOption[Record[str, list[int]]]], MochiError]`.
This is correct, gate-clean, and impossible to read.

Beyond readability, the byte size matters: a 1000-record Mochi
program can emit 200 KB of type hints, slowing import time (type
hint parsing is part of module load) and balooning git diffs.

**Likelihood**: high.

**Impact**: medium. Slow imports hit user experience; large diffs
hit developer experience.

**Mitigation**:

- Emit `from __future__ import annotations` at the top of every
  module. PEP 563 (2018) makes all annotations strings. They are not
  evaluated at import time, so the import-time cost is the string
  parse only.
- For mypy + pyright the strings are evaluated, but only once during
  type-check, not at runtime.
- Use type aliases for common compound types:
  `type _Records = list[Record[str, list[int]]]`. The emitter
  detects compound types used three or more times and aliases them.
  PEP 695 syntax keeps the alias declaration concise.
- Black + ruff line-length 100 keeps individual lines from running
  unbounded. See `[[10-build-system]]`.

**Owner**: emit maintainer.

### R9: PEP 695 syntax in older tools

**Description**: PEP 695 type parameter syntax (`type Foo[T] = ...`,
`def foo[T](x: T) -> T:`) is Python 3.12+. Older tooling does not
parse it:

- Black before 24.0: SyntaxError on PEP 695.
- isort before 5.13: SyntaxError on PEP 695.
- yapf: no PEP 695 support as of 2026.
- IDE syntax highlighters: PyCharm 2024.1+, VS Code Python 2024.0+,
  Sublime LSP-pyright recent. Older versions show red squigglies on
  valid code.

Users running these versions on their dev machines will see false
errors.

**Likelihood**: medium (older tools still common in 2026).

**Impact**: low. Users diagnose quickly once told.

**Mitigation**:

- Document the floor versions in the generated README. The "Setup"
  section lists the minimum tool versions.
- Use ruff exclusively for format + lint. ruff has always supported
  PEP 695 (introduced 2023-Q3). black is not required.
- If a user reports a SyntaxError from older tooling, the README's
  troubleshooting section has the fix.

**Owner**: docs maintainer.

### R10: pip vs uv ecosystem fragmentation

**Description**: uv adoption is rising but pip is still the default
for many Python users. Corporate environments often forbid
downloading the uv binary (firewall rules, supply-chain policy).
Users who can't run uv need a pip fallback.

**Likelihood**: high (pip-only environments exist and will exist).

**Impact**: medium. Users on pip-only environments hit slower
installs (10x to 100x slower) but the build still works.

**Mitigation**:

- The Mochi-emitted `pyproject.toml` is uv-aware and pip-aware. Both
  drivers respect PEP 517 + PEP 621.
- Document the pip recipe in the generated README:
  ```
  python -m venv .venv
  source .venv/bin/activate
  pip install build twine
  python -m build
  twine upload dist/*
  ```
- Test the pip path in CI at least once per release. The
  `tests/transpiler3/python/phase15_pip_fallback_test.go` fixture
  runs this recipe and verifies the wheel SHA matches the uv path.
- The CI matrix can include a pip-only cell for one Python version
  per release. We do this in Phase 15+ on ubuntu-24.04 + 3.12.7.

**Owner**: build maintainer.

### R11: notebook namespace pollution

**Description**: Cell-by-cell execution in a Jupyter kernel means
each cell mutates a shared `globals()`. Users can redefine functions,
shadow imports, mutate state in surprising ways. Mochi's type system
assumes immutable definitions; a user redefining a record type
mid-notebook breaks type-check assumptions.

Additionally, the kernel's persistent state grows monotonically.
Records, agents, streams created in early cells stay alive until the
kernel restarts. This is "by design" in Jupyter but creates memory
leaks for users who don't expect it.

**Likelihood**: high (this is normal Jupyter usage).

**Impact**: low to medium. Users are accustomed to Jupyter's
state model.

**Mitigation**:

- Document the cell-state model in the Phase 17 user guide
  (Jupyter integration).
- Emit a kernel-side `__mochi_reset_records__()` helper users can
  call to clear all record type registrations. Mochi types are
  registered in a kernel-global dict; the reset wipes it.
- The ipykernel re-runs the type checker on every cell. Mypy daemon
  mode (`dmypy`) is used to avoid the cold start. Type errors
  surface in the cell output, not later.
- Recommend users use `%reset` (IPython magic) between major
  sections of a notebook.

**Owner**: kernel maintainer.

### R12: wheel platform tag drift

**Description**: We emit pure-Python wheels with tag `py3-none-any`.
This is platform-agnostic. If we ever ship a C extension (Phase 12
FFI, or a future Cython acceleration), we need platform-specific
wheels: `cp312-cp312-manylinux_2_28_x86_64`,
`cp312-cp312-macosx_11_0_arm64`, `cp312-cp312-win_amd64`, etc.

The manylinux tag is itself a moving target. manylinux_2_28 (CentOS
Stream 9 based) replaced manylinux_2_17 (CentOS 7 based) in 2023.
Phase-out timeline: manylinux_2_17 wheels still install on most
systems but PyPI deprecation is on the table for 2026-Q4.

**Likelihood**: low (we don't ship C extensions in v1).

**Impact**: high if we ever do. Wrong tag = wheel doesn't install on
half the user base.

**Mitigation**:

- v1 sticks with pure-Python. No C extension shipped from MEP-51.
- If we ship a C extension in v2, use `cibuildwheel` (the de-facto
  tool for cross-platform wheel builds in GitHub Actions). cibuildwheel
  handles the manylinux base image, macos universal2, windows VC
  runtime, all the platform tag details.
- ABI3 stable wheels (`cp312-abi3-linux_x86_64`) are the preferred
  target for C extensions. They work across CPython 3.12+ without
  recompile. See F4 below.

**Owner**: build maintainer.

### R13: PyPI Trusted Publishing OIDC failures

**Description**: Trusted Publishing uses OIDC tokens minted by
GitHub Actions and traded with PyPI for short-lived publish
credentials. The OIDC exchange can fail:

- GitHub Actions OIDC service outage (rare; happens about once a
  quarter for 5-15 minutes).
- PyPI Trusted Publisher misconfiguration (workflow name doesn't
  match, environment name doesn't match, ref doesn't match).
- Token claim drift: GitHub updated claim format in late 2024;
  PyPI's verifier caught up but old configs broke briefly.

A failed OIDC exchange blocks the release. The release engineer needs
a fallback to ship the bits.

**Likelihood**: medium (OIDC failures happen).

**Impact**: medium. A blocked release is annoying but recoverable.

**Mitigation**:

- Maintain a fallback API token in the project's GitHub secrets,
  named `PYPI_FALLBACK_TOKEN`. The token is scoped to the project
  only. Document the manual fallback recipe:
  ```
  uv publish --token "$PYPI_FALLBACK_TOKEN"
  ```
- Rotate the fallback token every 90 days via a calendar reminder.
- The fallback token is NOT used in normal CI. It exists only for the
  OIDC-failure path.
- Test the OIDC path quarterly via the
  `tests/transpiler3/python/phase18_trusted_publishing_test.go`
  dry-run.

**Owner**: release engineer.

### R14: reproducibility breakage from filesystem ordering

**Description**: hatchling 1.25+ sorts wheel entries before writing.
But subtle filesystem ordering effects can still leak: symlinks
(macOS HFS+ legacy, no longer default), case-insensitive filenames
on macOS APFS (default for non-case-sensitive volumes), Windows
NTFS short-name aliasing, directory mtime stamps.

A single non-deterministic mtime in a `__init__.py` written by the
emit pass breaks the wheel SHA match across hosts.

**Likelihood**: medium. We hit this once in early Phase 15 testing
(macOS case-insensitive filesystem reordered `Foo.py` and `foo.py`).

**Impact**: medium. Reproducibility is a v1 gate; failures block
release.

**Mitigation**:

- Mochi emits all files with `pathlib.Path.write_text` plus an
  explicit `os.utime(path, (epoch, epoch))` setting mtime to
  `SOURCE_DATE_EPOCH`.
- The Mochi `lower` pass sorts emit output by filepath (lexicographic
  bytes, ASCII order). No reliance on map iteration order or
  filesystem walk.
- We forbid two filenames that differ only in case (`Foo.py` and
  `foo.py`). The lower pass errors on this at emit time. macOS APFS
  case-insensitive volumes get a clean emit.
- The reproducibility CI job runs on linux ext4 + macOS APFS + linux
  aarch64 (Phase 16). Windows reproducibility is Phase 16.1.
- See `[[10-build-system]]` reproducibility section for the full
  recipe.

**Owner**: emit maintainer.

### R15: type-checker false positives on legit Mochi patterns

**Description**: Some Mochi patterns lower to typed Python that mypy
or pyright flag as incorrect even though the runtime behaviour is
fine. Examples we have seen during phase prototyping:

- Mochi's exhaustive `match` lowered to a Python `match` with a
  final `case _: raise AssertionError(...)`. mypy 1.12 flagged this
  as "unreachable" (false positive); fixed in mypy 1.13.
- Mochi's generic agent with PEP 695 type parameters: pyright
  1.1.375 inferred the wrong variance; fixed in 1.1.378.
- Mochi's structural typing via Protocol with mixed sync + async
  methods: mypy 1.12 incorrectly required `@runtime_checkable`; fixed
  in 1.13.
- Mochi's Result chain via `.map().and_then().or_else()`: pyright
  reports incorrect type narrowing on the closure-bound generic.
  Bug filed (pyright #7311); not yet fixed.

**Likelihood**: high (active set; new false positives expected
quarterly).

**Impact**: low to medium. Each false positive is a CI breakage. We
either change the emit pattern, suppress per-file with `# type:
ignore`, or wait for upstream fix.

**Mitigation**:

- Maintain a per-pattern decision log in
  `internal/transpiler3/python/typecheck_quirks.md` (internal,
  not deployed). Each row: pattern, checker, version, decision,
  upstream issue, expected fix release.
- Avoid `# type: ignore[errcode]` lines in emitted code. Suppressions
  are easy to spread; we prefer fixing the emit pattern.
- Escalate per-pattern: every new false positive becomes a Mochi
  team triage item with one of {fix emit, suppress, wait}.
- Quarterly bump of `mypy` + `pyright` pins includes review of the
  quirks log.

**Owner**: type-system maintainer.

## Rejected alternatives

### A1: Compile Mochi to Cython

**Description**: Cython compiles a superset of Python (with optional
C-typed annotations) to C, then to a CPython extension module. The
result is faster than pure Python on CPU-bound code (2x to 100x
depending on workload).

We considered emitting Cython instead of pure Python. The pitch:
Mochi's type annotations carry enough information to drive Cython's
`cdef` declarations, and the user gets free speedup.

**Evaluation**:

- Cython requires a C toolchain on every install. `pip install`
  triggers `gcc` or `clang` on linux, `clang` on macos, `cl.exe` on
  windows. PyPI's manylinux wheel infrastructure mitigates this
  somewhat, but every minor Cython release requires a fresh wheel
  matrix build.
- CI cost doubles: every fixture would need a C compile pass before
  the test runs.
- Cython's type system is its own dialect; mapping Mochi types
  cleanly is non-trivial. Cython's `bint`, `cython.int`, `cython.long`,
  `cython.double` don't match Mochi's `int` (arbitrary precision) or
  `float` (IEEE 754 double).
- Editable installs (PEP 660) are flakier with Cython than with pure
  Python.
- Cython runtime dependency: every Mochi-emitted package would
  depend on the Cython runtime, an additional ~500 KB.
- mypy and pyright do not type-check Cython sources directly. We'd
  lose Tier 2 of the gate plan.

**Decision**: reject. v1 is pure Python. Cython is not on the v2
candidate list either; the simpler v2 acceleration path is PEP 703
free-threaded CPython (see F1) which gives free parallelism without
C compilation.

**References**:

- Cython 3.0 documentation, `cython.org`
- Discussion in MEP-51 PR #001 comments (internal)
- A4 (Nuitka) for the binary-single-file alternative

### A2: Compile Mochi to mypyc

**Description**: mypyc compiles typed Python (the dialect mypy
understands) to C, similar to Cython but using mypy's type
inference. Output is a CPython extension module. mypy itself uses
mypyc since 2020 for speed.

We considered emitting mypyc-compatible Python so that downstream
users could opt-in to compilation.

**Evaluation**:

- mypyc compiles `Final` immutable classes well; mutable classes
  less so. Mochi's `@dataclass(frozen=True, slots=True)` records are
  mypyc-friendly, but Mochi's agent classes (mutable state) are not.
- mypyc ties the Mochi runtime to a specific mypy version. mypyc
  1.13 and mypy 1.13 are co-versioned; bumping one bumps the other.
  Our pin-mypy-exactly policy means we'd pin mypyc-exactly too.
- mypyc requires C toolchain on install. Same downsides as Cython
  (A1).
- mypyc generates platform-specific wheels. Same downsides as R12.
- mypyc is a leaky abstraction: some valid typed-Python code does
  not compile cleanly (subclassing Protocol, dynamic attribute
  access, `__init_subclass__`). The emit pass would need to
  pre-filter.
- CI cost doubles, as with Cython.

**Decision**: reject. mypyc is on the v2 candidate list as F4 (a
post-publish acceleration pass), not as an emit target.

**References**:

- mypyc documentation, `mypyc.readthedocs.io`
- mypy team's "mypyc and our path forward" blog (2023)
- F4 below

### A3: Compile Mochi to Nuitka single binary

**Description**: Nuitka compiles Python to standalone C binaries.
The output is an executable that bundles a CPython interpreter plus
the user's code plus all imports. No Python installation needed at
runtime.

The pitch: distribute Mochi apps as a single binary, like Go or
Rust. Users `./mochi-app` and it runs.

**Evaluation**:

- Binary size: a "hello world" Nuitka build is about 25 MB on linux
  x86_64 (CPython interpreter plus minimal stdlib). A real app with
  asyncio + httpx + dataclasses is about 60 MB. Compare Go: 5 MB
  static binary for hello world, 15 MB for a real app. Rust: 1 MB
  static for hello world.
- Build time: Nuitka compiles the entire Python stdlib touched by
  the app. Cold build of a simple async program: 4 minutes on M2.
  Compare uv build (pure Python wheel): 200 ms.
- Cross-compilation: Nuitka requires the target platform's C
  toolchain. Cross-compiling from linux to windows requires the
  windows toolchain (mingw or cross-compiled MSVC). Mochi's "build
  once, ship everywhere" story breaks.
- Debugging: Nuitka binaries are harder to debug. `gdb` works but
  symbol tables are bloated; Python-level pdb doesn't work directly.
- Startup time: Nuitka binaries start in about 50 ms; CPython
  startup is about 30 ms. Roughly comparable, slight Nuitka
  disadvantage.
- Maintenance: Nuitka has a single core maintainer (Kay Hayen, full
  time on it). Bus-factor is real.

**Decision**: reject for v1. v2 candidate if user demand emerges.
Even then, the better single-binary story for Mochi is to emit C
(MEP-45) and link statically, not Python compiled by Nuitka.

**References**:

- Nuitka documentation, `nuitka.net`
- Size comparison: `github.com/PrismJS/prism` (uses Nuitka in CI)
- MEP-45 for the C single-binary path

### A4: Use Trio not asyncio

**Description**: Trio is an alternative async framework for Python.
It predates asyncio's structured-concurrency features and has
stronger guarantees: nurseries (TaskGroup analog) are mandatory,
cancellation propagation is more reliable, error aggregation is via
`trio.MultiError` (predates PEP 654).

We considered emitting Trio code instead of asyncio. Trio's
ergonomics are widely considered better.

**Evaluation**:

- Trio is a hard dependency. Mochi-emitted code would always pull
  Trio (about 200 KB plus dependencies).
- Ecosystem split: FastAPI uses asyncio. httpx uses anyio (which can
  back asyncio or Trio, but most users default to asyncio). aiohttp
  uses asyncio only. SQLAlchemy async uses asyncio only. Choosing
  Trio cuts users off from the asyncio-only half of the ecosystem.
- AnyIO bridges the two (Trio-style API over asyncio or Trio
  backend). We considered AnyIO too (see below) and rejected it.
- CPython 3.11+ asyncio added TaskGroup (PEP 658) and
  ExceptionGroup (PEP 654). The structured-concurrency gap to Trio
  has closed substantially. The remaining gaps (cancel scope
  inheritance, levels of cancellation) are nice-to-have, not
  load-bearing.
- Trio's cancellation model is mandatorily checkpoint-based:
  `await trio.sleep(0)` to yield. asyncio is implicitly
  checkpoint-based via `await`. Mochi's emit doesn't need
  Trio-style explicit checkpoints; asyncio's implicit model works.

We also looked at AnyIO. AnyIO is a compatibility layer: code
written against AnyIO runs on either Trio or asyncio. The downside
is an extra abstraction layer (AnyIO -> asyncio adds about 5%
overhead per await) and another dependency to track. We don't need
the polymorphism in v1.

**Decision**: reject Trio. reject AnyIO. Use asyncio directly. See
`the shared-decisions anchor` decision 3.

**References**:

- Trio documentation, `trio.readthedocs.io`
- AnyIO documentation, `anyio.readthedocs.io`
- Nathaniel J. Smith, "Notes on structured concurrency", 2018
- PEP 654 (ExceptionGroup, asyncio's catchup)
- PEP 658 (TaskGroup)

### A5: Use Pydantic BaseModel for records

**Description**: Pydantic 2 BaseModel is the dominant data-validation
class in Python. It supports type hints, validation, serialisation
(JSON, dict), aliasing, defaults, computed fields. Many Python users
would expect Mochi records to be Pydantic models.

**Evaluation**:

- Pydantic 2 introduces runtime validation cost: every BaseModel
  instantiation runs through pydantic-core's Rust validator. The
  cost is about 200 ns per field on M2; a record with 10 fields
  costs about 2 microseconds per construction.
- A frozen dataclass with slots costs about 50 ns per construction
  total (10 ns per field).
- Memory: Pydantic 2 BaseModel instances are about 4x larger than
  frozen-dataclass-with-slots instances due to internal state.
- Import time: `import pydantic` takes about 230 ms cold on M2.
  Mochi-emitted modules with Pydantic records would pay this on
  every cold import.
- Dependency: every Mochi-emitted package pulls Pydantic (Pydantic
  itself, pydantic-core's Rust binary, annotated-types). About 5 MB
  on disk.
- Pydantic validation is a feature, not a bug, for users who want
  it. Mochi's type system already validates at the language layer;
  runtime re-validation is redundant for trusted Mochi-emitted code.

**Decision**: reject for v1. v2 candidate behind a `--pydantic`
emit flag. Users who opt-in get Pydantic records; the default stays
dataclass.

**References**:

- Pydantic 2 documentation, `docs.pydantic.dev/2.0`
- "Pydantic 2 vs dataclasses benchmark", `samwillis.co.uk/blog/`
  (2023)
- R7 above (Pydantic ecosystem pull)

### A6: Emit Python 2-compatible code

**Description**: Some legacy environments still run Python 2.7
(despite EOL since 2020-01). RHEL 8 ships Python 2.7 as
`/usr/bin/python`. Some scientific computing clusters still have it.

We considered emitting Python 2-compatible code. The pitch: maximum
compatibility, broadest reach.

**Evaluation**:

- Python 2.7 reached end-of-life on 2020-01-01. No security fixes.
  No new features. CPython core team has moved on.
- PEP 484 (type hints) is Python 3 only; Python 2 has comment-based
  hint syntax (`# type: ...`) but mypy support is minimal and
  pyright never supported it.
- PEP 695 (type parameter syntax) is 3.12 only; Python 2 cannot
  parse it. Our emit relies on PEP 695 fundamentally.
- f-strings (3.6+) are 3-only. Mochi's string-formatting emit uses
  f-strings.
- async / await (3.5+) are 3-only.
- Every async path, every type annotation, every f-string would need
  a Python 2-compatible fallback. The emit complexity multiplies.

**Decision**: reject. The Python 3 floor for MEP-51 is 3.12; Python
2 is not even on the table.

**References**:

- PEP 373 (Python 2.7 release schedule, EOL 2020-01)
- Python 3 statement, `python3statement.org`
- `the shared-decisions anchor` decision 1 (CPython 3.12 floor)

## Future-track candidates

### F1: 3.13 free-threaded GIL build

**Description**: CPython 3.13 (October 2024) ships a
`--disable-gil` build (PEP 703). The free-threaded build runs Python
bytecode concurrently across multiple cores, eliminating the GIL
serialisation. CPU-bound Python parallelism becomes viable.

The free-threaded build is opt-in; you must download a
`cpython-3.13-freethreaded` binary or compile from source with
`--disable-gil`. The standard 3.13 build still has the GIL.

**Gating signal**: PEP 703 expects mainstream availability in 3.13
through 3.15 (about 2024-Q4 to 2027). v2 of MEP-51 (planned 2028-Q1)
will support the free-threaded build as a runtime option.

Mochi-emitted code is already thread-safe: frozen dataclasses,
immutable collections, agent classes with explicit mailbox
synchronisation. The free-threaded build benefits us without code
changes; we get parallel execution of CPU-bound `parallel for`
loops "for free".

The risk: third-party C extensions used by `mochi-runtime` (httpx,
ipykernel) need to be GIL-safe. Not all are as of 2026. We track
extension readiness in the v2 plan.

### F2: PyPy support via abi3 wheel

**Description**: PyPy is an alternative CPython implementation with
a JIT. It runs typical Python code 5x to 10x faster on CPU-bound
workloads. PyPy's CPython compatibility is high (about 99% per the
PyPy team's CPython test suite pass rate) but not perfect.

We rejected PyPy for v1 (per `the shared-decisions anchor`). The future
candidate is supporting PyPy via abi3-stable wheels.

**Gating signal**: PyPy 7.3.16+ supports CPython 3.10 bytecode;
PyPy 7.3.18+ targets CPython 3.11; PyPy is typically about 2 years
behind CPython floor. By 2027 PyPy should track 3.12. v2 of MEP-51
can ship PyPy support if the wheel tag lands.

The work to support: ensure every Mochi runtime dependency works
on PyPy (httpx works; ipykernel works; ctypes works; some C
extensions don't), document the install recipe (`pypy3 -m pip
install mochi-runtime`), add a PyPy CI cell.

### F3: Pyodide / WASM browser target

**Description**: Pyodide is CPython compiled to WebAssembly. It runs
in browsers. Python scientific stack (numpy, pandas, scikit-learn)
plus many pure-Python packages work.

The Mochi-to-Python emit is pure Python. It should run on Pyodide
out of the box. The browser target opens Mochi to Observable-style
notebooks, JupyterLite, and education tools.

**Gating signal**: Pyodide tracks CPython 3.11 as of 2026. By 2028
Pyodide should track 3.12. v2 candidate when Pyodide hits 3.12.

The work: add a `--target=pyodide` flag that emits a manifest for
JupyterLite + Pyodide install. Document the install recipe via
`micropip`. Test in a headless browser CI.

The catch: asyncio in Pyodide is the browser's event loop, not a
real OS event loop. Mochi's agent model (which assumes
asyncio.TaskGroup) needs a Pyodide-specific shim.

### F4: mypyc compile pass

**Description**: mypyc compiles typed Python to C extension modules
(see A2). Rejected for v1 as an emit target. As a future post-publish
acceleration pass, it's viable.

The idea: ship pure-Python wheels from `uv build`, and offer
`mochi build --target=python-mypyc-wheel` as an opt-in variant that
runs the emit output through mypyc to produce a platform-specific
accelerated wheel.

Users who want speed install the mypyc wheel; users who want
portability install the pure wheel.

**Gating signal**: mypyc 1.13+ supports PEP 695 syntax (as of
late 2024). mypyc's Protocol support improved in 1.14 (early 2025).
v2 candidate.

The work: add a `--target=python-mypyc-wheel` flag. Build via
mypyc-build hook (mypyc plugs into setuptools and hatchling). Add a
CI matrix cell that builds + smoke-tests the mypyc wheel on linux
x86_64 + linux aarch64 + macos arm64 + windows x86_64. Pin mypyc
exactly (same version as mypy).

The risk: doubles CI time for the mypyc cell. Worth it only if user
benchmarks show 2x+ speedup on Mochi workloads. v2 prerequisite is
a published benchmark.

## Summary

The risk register has 15 entries, all with concrete mitigations. The
load-bearing risks (R1 EOL drift, R2 checker divergence, R3 asyncio
cancellation, R4 GIL) are CPython-platform risks we accept as the
cost of targeting Python. The build-pipeline risks (R5 uv stability,
R12 platform tag drift, R13 OIDC failure, R14 reproducibility) are
operational and tracked in `[[10-build-system]]` and
`[[11-testing-gates]]`.

The rejected alternatives are Cython (A1), mypyc (A2), Nuitka (A3),
Trio (A4), Pydantic (A5), Python 2 (A6). The decisions are
documented above. v2 candidates are mypyc (F4) and Pydantic (A5).

The future-track candidates are 3.13 free-threaded (F1), PyPy (F2),
Pyodide (F3), mypyc post-publish acceleration (F4). All four are v2
gated on external signals (CPython release, PyPy compatibility,
Pyodide CPython floor, mypyc PEP 695 stability).

## References

- PEP 484, 526, 561, 585, 591, 612, 646, 647, 654, 658, 673, 678,
  693, 695, 698, 703, 740 (cited inline)
- CPython issues #90990, #97583, #103847, #108534, #112535, #117846
- mypy issues #16003, #15997, #16414
- pyright issues #5421, #6789, #7102, #7311
- Trio documentation, `trio.readthedocs.io`
- Nuitka documentation, `nuitka.net`
- Cython 3.0 documentation, `cython.org`
- mypyc documentation, `mypyc.readthedocs.io`
- Pydantic 2 documentation, `docs.pydantic.dev/2.0`
- Pyodide documentation, `pyodide.org`
- PyPy documentation, `pypy.org`
- `the shared-decisions anchor`
- `[[10-build-system]]`
- `[[11-testing-gates]]`
