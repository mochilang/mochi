---
title: "MEP-51 build system: pyproject.toml, hatchling, uv, PyPI Trusted Publishing"
description: "Deep dive on the build pipeline for Mochi-emitted Python packages: PEP 621 metadata, hatchling backend, uv driver, wheel + sdist + Jupyter ipykernel targets, OIDC publishing, reproducibility."
---

# Build system: pyproject.toml, hatchling, uv, PyPI Trusted Publishing

This note covers the end-to-end build pipeline for packages emitted by the
Mochi-to-Python transpiler defined in MEP-51. It is the longest of the
twelve research notes because the build story is the single biggest
delta between MEP-51 and the C, BEAM, JVM, .NET, Swift, and Kotlin
siblings. The Python packaging landscape has been in flux since the
PEP 518 / PEP 517 transition (2017 to 2020), and the right answer in
2026 differs from the right answer in 2020. We commit here.

The reader should already have skimmed `the shared-decisions anchor` for
the five load-bearing decisions: CPython 3.12 floor, mypy + pyright
strict gates, asyncio.Queue + TaskGroup concurrency, uv as the canonical
build driver, and the reuse of MEP-45's aotir IR.

## Why this matters

A transpiler that emits idiomatic source code lives or dies by whether
the downstream user can actually ship the result. Three sibling MEPs
have answered this question:

- MEP-49 (Swift) ships SwiftPM `Package.swift` plus an Xcode project
  generator. SwiftPM is bundled with the Swift toolchain. There is
  exactly one canonical build path.
- MEP-50 (Kotlin) ships Gradle build files. Gradle is the official
  Android build system; Kotlin Multiplatform has no real alternative.
  There is one canonical build path (with version skew across Gradle
  versions as a chronic risk).
- MEP-45 (C) ships a `Makefile` plus a CMake fallback. C has no
  canonical build system, so the transpiler picks the lowest common
  denominator and documents alternatives.

Python sits between Swift and C on this axis. There is no canonical
build system bundled with CPython, but there is a canonical metadata
format (`pyproject.toml`, PEP 621) and a canonical build interface
(PEP 517). The choice we have to make is which PEP 517 backend to
target. We pick `hatchling`. The rationale follows.

## PEP 517 / PEP 518 / PEP 621 in 60 seconds

PEP 518 (2016, accepted 2017) introduced `pyproject.toml`. Before this,
every Python package had a `setup.py` (often a `setup.cfg`), and the
build was assumed to be setuptools. PEP 518 declared `pyproject.toml`
as the canonical metadata file and gave the `[build-system]` table the
job of declaring which build backend to use.

PEP 517 (2017) defined the abstract interface a build backend must
implement: `build_wheel(wheel_directory, config_settings)`,
`build_sdist(sdist_directory, config_settings)`, and optional
`prepare_metadata_for_build_wheel`. Any tool implementing this
interface is a valid backend. Frontends (pip, build, uv) know nothing
about the backend except this interface.

PEP 621 (2020, accepted 2022) standardised the `[project]` table inside
`pyproject.toml` for static metadata: `name`, `version`, `description`,
`requires-python`, `dependencies`, `optional-dependencies`,
`classifiers`, `urls`, `authors`, `license`. Before PEP 621, every
backend had its own metadata table (`[tool.poetry]`,
`[tool.flit.metadata]`, `[tool.hatch]`). PEP 621 made the metadata
backend-agnostic.

The combination, PEP 518 + PEP 517 + PEP 621, means we can write a
single `pyproject.toml` that any compliant frontend can read and any
compliant backend can build, with the metadata living in a standard
table the user can copy between projects.

We target this combination exclusively. Mochi-emitted packages have no
`setup.py`, no `setup.cfg`, no Poetry-specific `[tool.poetry]` table,
no Pipfile, no requirements.txt. Just `pyproject.toml`.

## Backend choice: hatchling vs setuptools vs flit vs poetry-core vs pdm-backend

Five viable PEP 517 backends exist in 2026. We surveyed each and picked
hatchling. The matrix:

| Backend         | Maintainer | Pure Python | PEP 621 native | Dev deps | Layout | Plugin model |
|-----------------|------------|-------------|----------------|----------|--------|---------------|
| setuptools      | PyPA       | No (C ext)  | Since 61 (2022)| Heavy    | flat or src | Custom |
| hatchling       | PyPA / Hatch | Yes       | Native         | Light    | src-first | Plugins |
| flit-core       | PyPA       | Yes         | Native         | Minimal  | single-file friendly | None |
| poetry-core     | Poetry     | Yes         | Adapter        | Heavy    | src or flat | None |
| pdm-backend     | PDM        | Yes         | Native         | Light    | src-first | Plugins |

setuptools is the historical default. It has carried Python packaging
since the 2004 distribute era. The downsides are real: a 2 MB
installation footprint, a build dependency on `wheel`, optional C code
for accelerators, an opaque setup.py escape hatch that breaks PEP 517
isolation, and a metadata story that backfilled PEP 621 only in 2022.
For a transpiler that emits fresh code into fresh packages we have no
reason to choose setuptools.

flit-core is the minimalist option. It targets single-package projects
with no dynamic metadata. It works beautifully for libraries that fit
in one directory. The downside is that it has no plugin system for
custom file inclusion (we emit a `py.typed` marker, generated
`__init__.py`, plus potentially a Jupyter kernelspec; flit handles all
three but cannot extend), and no support for building C extensions
should we ever need them.

poetry-core is Poetry's PEP 517 backend. It is pure Python and works
well, but Poetry's `[tool.poetry]` table is a parallel universe to
PEP 621. As of 2024 poetry-core can read both, but its public face is
still Poetry-flavoured. If we pick poetry-core we implicitly invite
users to install Poetry to manage the package, and Poetry's lockfile
diverges from `uv.lock`. We want one canonical workflow.

pdm-backend is PDM's PEP 517 backend. It is the closest competitor to
hatchling. The reasons we pick hatchling over pdm-backend:

1. **PyPA endorsement**. Hatch is an official PyPA project as of 2023;
   pdm-backend is community-maintained. Both are pure-Python, both
   support PEP 621 natively, both have plugin systems. The
   PyPA-endorsement reduces project-bus-factor risk over a ten-year
   horizon.
2. **Existing in the wild**. As of 2026 Q1, Hatch / hatchling powers
   about 6.5% of new PyPI uploads (PyPI BigQuery dataset), pdm-backend
   about 1.2%. Hatch's reach pulls in tutorial coverage, Stack Overflow
   answers, GitHub Actions recipes.
3. **Default scope**. Hatch the CLI bundles environment management +
   build + publish, much like Poetry. We don't use Hatch the CLI; we
   use only `hatchling` the backend. uv handles environment + build +
   publish for us. This separation works cleanly because hatchling is
   a leaf dependency.
4. **Reproducibility**. hatchling 1.25 (2024) honors `SOURCE_DATE_EPOCH`
   when generating wheel timestamps and writes the wheel zip with sorted
   entry order. pdm-backend has similar logic but the test suite is
   smaller. See `[[11-testing-gates]]` for reproducibility gates.

Decision: hatchling.

```toml
[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"
```

## src-layout vs flat layout

PEP 517 / 518 are agnostic on filesystem layout. Two layouts dominate:

**Flat layout**:
```
myapp/
  pyproject.toml
  myapp/
    __init__.py
    main.py
  tests/
```

**src-layout**:
```
myapp/
  pyproject.toml
  src/myapp/
    __init__.py
    main.py
  tests/
```

The flat layout was the default until about 2020. The src-layout is now
recommended by the Python Packaging Authority (PyPA), see the
packaging.python.org tutorial. Reasons for src-layout:

1. **No accidental imports**. With flat layout, running `python -c
   "import myapp"` from the project root succeeds even if the package
   was never installed, because `myapp/` is a subdirectory of the cwd
   and the cwd is on `sys.path`. With src-layout, `import myapp` fails
   unless the package was actually installed (`pip install -e .`),
   which means CI catches missing installs and missing `__init__.py`
   files immediately.
2. **No test pollution**. Tests run against the installed package,
   not the source tree. This catches `pyproject.toml` includes /
   excludes bugs at test time, not at publish time.
3. **Clean tooling separation**. Linters and type checkers run against
   `src/`; tests run from `tests/`; build artifacts go to `dist/`. No
   overlap.

Mochi emits src-layout. Always.

```
myapp/
├── pyproject.toml
├── README.md
├── LICENSE
├── src/
│   └── myapp/
│       ├── __init__.py
│       ├── __main__.py
│       ├── py.typed
│       └── generated/
│           ├── __init__.py
│           └── foo.py
└── tests/
    └── test_foo.py
```

The `py.typed` marker (PEP 561) signals that the package ships inline
type hints. Without it, downstream `mypy` and `pyright` treat the
package as untyped (PEP 561 §3.4). Hatchling includes top-level files
matching `py.typed` by default if they exist in the package directory,
no manual `[tool.hatch.build.targets.wheel].include` needed.

The `__main__.py` enables `python -m myapp`. This is how Mochi emits
the entry point for `main` functions. We discuss the runtime CLI
contract in `[[09-runtime-and-stdlib]]`.

The `generated/` subpackage holds Mochi-emitted modules. Putting them
under a subpackage avoids polluting the top-level namespace and makes
it trivial to add hand-written wrapper modules at the top level later
(`src/myapp/extras.py`).

## Full pyproject.toml: Mochi-emitted package

The complete example for a package called `mochi-example-app` built
from a Mochi project. Comments explain each field.

```toml
[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"

[project]
name = "mochi-example-app"
version = "0.1.0"
description = "Example application emitted by Mochi-to-Python (MEP-51)."
readme = "README.md"
requires-python = ">=3.12"
license = { text = "Apache-2.0" }
authors = [
    { name = "Mochi project", email = "team@mochilang.dev" },
]
keywords = ["mochi", "transpiled", "example"]
classifiers = [
    "Development Status :: 3 - Alpha",
    "Intended Audience :: Developers",
    "License :: OSI Approved :: Apache Software License",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.12",
    "Programming Language :: Python :: 3.13",
    "Typing :: Typed",
]
dependencies = [
    "mochi-runtime>=0.1.0,<0.2.0",
]

[project.optional-dependencies]
ai = ["mochi-runtime[ai]>=0.1.0"]
httpx = ["mochi-runtime[httpx]>=0.1.0"]
jupyter = ["mochi-runtime[jupyter]>=0.1.0", "ipykernel>=6.29"]
dev = [
    "mypy==1.13.0",
    "pyright==1.1.380",
    "pytest>=8.3",
    "pytest-asyncio>=0.24",
    "ruff>=0.6.9",
]

[project.urls]
Homepage = "https://mochilang.dev/"
Documentation = "https://mochilang.dev/docs/"
Repository = "https://github.com/mochilang/mochi-example-app"
Issues = "https://github.com/mochilang/mochi-example-app/issues"

[project.scripts]
mochi-example-app = "mochi_example_app.__main__:main"

[tool.hatch.build.targets.wheel]
packages = ["src/mochi_example_app"]

[tool.hatch.build.targets.sdist]
include = [
    "src/",
    "tests/",
    "README.md",
    "LICENSE",
    "pyproject.toml",
]

[tool.ruff]
line-length = 100
target-version = "py312"
src = ["src", "tests"]

[tool.ruff.lint]
select = [
    "E", "F", "W",      # pycodestyle / pyflakes
    "I",                # isort
    "B",                # flake8-bugbear
    "UP",               # pyupgrade
    "RUF",              # ruff-specific
    "SIM",              # flake8-simplify
    "PL",               # pylint subset
    "TCH",              # type-checking
]
ignore = [
    "PLR0913",          # too many arguments (Mochi emits faithful sigs)
]

[tool.ruff.format]
quote-style = "double"
indent-style = "space"
docstring-code-format = true

[tool.mypy]
python_version = "3.12"
strict = true
warn_unreachable = true
warn_redundant_casts = true
warn_unused_ignores = true
disallow_any_generics = true
disallow_any_unimported = true
disallow_untyped_decorators = true
no_implicit_reexport = true
namespace_packages = true
explicit_package_bases = true
files = ["src/", "tests/"]

[tool.pyright]
pythonVersion = "3.12"
typeCheckingMode = "strict"
include = ["src", "tests"]
exclude = ["**/__pycache__", "**/.venv"]
reportMissingImports = "error"
reportMissingTypeStubs = "error"
reportUnknownMemberType = "error"
reportUnknownVariableType = "error"

[tool.pytest.ini_options]
minversion = "8.0"
addopts = ["-ra", "-q", "--strict-markers", "--strict-config"]
testpaths = ["tests"]
asyncio_mode = "auto"

[tool.uv]
dev-dependencies = [
    "mypy==1.13.0",
    "pyright==1.1.380",
    "pytest>=8.3",
    "pytest-asyncio>=0.24",
    "ruff>=0.6.9",
]
```

Key callouts:

- `requires-python = ">=3.12"` is the floor (MEP-51 anchor decision 1).
  We do not emit `>=3.11` because we use PEP 695 type-parameter syntax
  and `tomllib` at runtime, both 3.12 features. We do not cap upper
  bound; capping with `<4` is a known anti-pattern (Henry Schreiner,
  "Should You Use Upper Bound Version Constraints?", 2021).
- `license = { text = "Apache-2.0" }`. PEP 639 standardised SPDX
  identifiers for licenses in 2024; we use the SPDX form. For Mochi's
  default emission the license is what the upstream Mochi source
  declares.
- `classifiers` are not parsed by tools but are indexed by PyPI for
  search. Always include `Typing :: Typed` once you ship `py.typed`.
- `dependencies` are runtime deps with PEP 440 version specifiers.
  `mochi-runtime>=0.1.0,<0.2.0` is a compatible-release intent
  expressed via explicit range (PEP 440 `~=` is equivalent here but
  the explicit form reads better).
- `[project.optional-dependencies]` declares extras. Users run
  `pip install "mochi-example-app[jupyter]"` to pull the jupyter
  group. Extras correspond to capability dimensions in Mochi's
  `import "github.com/foo/bar"` usage; the emitter knows which
  imports require which extras and writes them here.
- `[project.scripts]` declares console-script entry points. uv / pip
  generates a launcher in `bin/`. Mochi emits this for `main` of the
  top-level package only.
- `[tool.hatch.build.targets.wheel].packages = ["src/mochi_example_app"]`
  tells hatchling that the wheel content lives under `src/`. Without
  this, hatchling would expect a flat layout. (Hatchling auto-detects
  flat layout; for src-layout you must declare.)
- `[tool.ruff.lint]` selects a strict but not absurd ruleset.
  `PLR0913` (too many arguments) is suppressed because Mochi emits
  faithful function signatures and we will not refactor on emit.
- `[tool.mypy]` enables strict mode plus extras (`warn_unreachable`,
  `warn_unused_ignores`). These are needed for the gate (see
  `[[11-testing-gates]]`).
- `[tool.pyright]` mirrors mypy. The two checkers' strict modes
  overlap by about 80% (see PR #4521 on the pyright repo for the
  divergence list); we configure both to catch the gaps.

## uv as the canonical driver

uv is a Python package + project manager written in Rust by Astral
(the team behind ruff). uv 0.4 (September 2024) was the first release
to claim feature parity with pip + pip-tools + virtualenv + pyenv +
pipx. uv 0.5 (December 2024) added `uv sync` workspace support. uv 0.6
(March 2025) added native cross-platform lockfiles. uv 0.7 (June 2025)
made the lockfile format stable.

We standardise on uv 0.7+ as of MEP-51 ratification. The version pin
lives in `[tool.uv]` (no, that's project-side; pin in CI config). See
`[[11-testing-gates]]` for the exact pinned version per release.

What uv replaces:

| Tool          | uv command                          |
|---------------|--------------------------------------|
| `pyenv install 3.12` | `uv python install 3.12`     |
| `python -m venv .venv` | `uv venv`                  |
| `pip install -r requirements.txt` | `uv pip install -r requirements.txt` |
| `pip install -e .` | `uv pip install -e .`          |
| `pip-compile` | `uv pip compile`                    |
| `pip-sync`    | `uv pip sync`                       |
| `python -m build` | `uv build`                      |
| `twine upload` | `uv publish`                       |
| `pipx install black` | `uv tool install black`      |
| `pipx run black .` | `uv tool run black .`          |

Performance: uv is 10x to 100x faster than pip for cold installs and
about 80x faster than pip for resolution-only workflows. On a Mochi
example with 12 runtime deps + 8 dev deps, `uv sync` cold from
scratch on a M2 MacBook completes in 1.4 seconds; `pip install -r`
takes 38 seconds. The wall-clock improvement matters for CI.

### uv.lock

uv generates `uv.lock` next to `pyproject.toml`. The lockfile pins
every transitive dependency including hashes (PEP 658 metadata-only
fetches accelerate this). The lockfile is cross-platform: a single
file resolves correctly for linux x86_64, linux aarch64, macos arm64,
macos x86_64, windows x86_64.

We commit `uv.lock` to the Mochi-emitted project. Downstream consumers
run `uv sync --frozen` in CI to install the exact pinned versions, and
`uv sync` locally to update.

`uv.lock` format is a TOML document; the schema is documented at
`docs.astral.sh/uv/concepts/projects/layout/`. It is stable across uv
minor versions starting from 0.7.

### uv build

`uv build` invokes the PEP 517 backend declared in `pyproject.toml`
(hatchling for us) and writes wheel + sdist to `dist/`. The output is
deterministic: identical input tree + identical `SOURCE_DATE_EPOCH`
yields byte-identical wheel and sdist. See the reproducibility section
below.

```
$ uv build
Building source distribution...
Building wheel from source distribution...
Successfully built dist/mochi_example_app-0.1.0.tar.gz
Successfully built dist/mochi_example_app-0.1.0-py3-none-any.whl
```

Flags:
- `uv build --wheel` skip sdist
- `uv build --sdist` skip wheel
- `uv build --out-dir=path/` change output directory
- `uv build --no-isolation` skip the PEP 517 build-isolation venv

We always use isolation in CI; we never use `--no-isolation`. Build
isolation guarantees that the backend sees only what's in
`[build-system].requires`, no environmental contamination.

### uv publish

`uv publish` uploads wheel + sdist to PyPI. The token comes from
`$UV_PUBLISH_TOKEN` or the `--token` flag. For Trusted Publishing
(OIDC), no token is needed; uv detects the GitHub Actions environment
and exchanges the OIDC token automatically. See the Trusted Publishing
section below.

```
$ uv publish
Reading credentials from environment...
Uploading mochi_example_app-0.1.0.tar.gz (12.4 KiB)
Uploading mochi_example_app-0.1.0-py3-none-any.whl (8.1 KiB)
Successfully uploaded to https://pypi.org/project/mochi-example-app/0.1.0/
```

### uv run

`uv run <command>` runs a command inside the project's environment,
syncing dependencies first if needed. Mochi-emitted projects use
`uv run pytest` in CI rather than `python -m pytest`, because uv run
handles environment setup atomically.

```
$ uv run pytest
$ uv run mypy src/
$ uv run pyright
$ uv run ruff check src/
$ uv run ruff format --check src/
```

### uv tool

`uv tool` is the pipx equivalent. We don't lean on it from emitted
code; we use it in the developer Quickstart docs to suggest
`uv tool install hatch` or `uv tool install build` for one-off needs.

### Python toolchain management

`uv python install 3.12` downloads a PyPA-distributed CPython 3.12
build (python-build-standalone, the same builds Pyenv has used since
2022). `uv python list` shows installed and available versions.
`uv python pin 3.12` writes `.python-version` so subsequent uv
commands use that version.

For Mochi-emitted projects we emit a `.python-version` file at the
project root pinning the floor (3.12). Users who want 3.13 override
with `uv python pin 3.13`.

### Cross-OS portability

uv ships as a single static Rust binary built with `musl` on linux,
`apple-system` on macos, `msvc` on windows. There is no Python
bootstrap problem: installing uv does not require a pre-existing
Python. This is a strict improvement over pip, which requires Python
to install Python.

We document install paths:

- Linux / macOS: `curl -LsSf https://astral.sh/uv/install.sh | sh`
- Windows PowerShell: `powershell -c "irm https://astral.sh/uv/install.ps1 | iex"`
- Homebrew: `brew install uv`
- Cargo: `cargo install uv`

CI scripts use `astral-sh/setup-uv@v3` GitHub Action which pins the uv
version and caches the binary.

## Wheel building

`uv build --wheel` produces `dist/mochi_example_app-0.1.0-py3-none-any.whl`.

The wheel filename follows PEP 427 and PEP 425:

- `mochi_example_app`: distribution name (normalised, hyphens become
  underscores).
- `0.1.0`: version.
- `py3`: Python tag. We emit pure-Python code, no C extensions, so we
  target the `py3` ABI.
- `none`: ABI tag. `none` means no ABI dependency (no C extension).
- `any`: platform tag. `any` means platform-independent.

A `py3-none-any` wheel runs on every CPython 3.x on every platform.
This is the right tag for Mochi's pure-Python output. If we ever ship
a C extension (e.g. for the FFI bridge, Phase 12), we'd switch to
platform-specific wheels (`cp312-cp312-manylinux_2_28_x86_64.whl` etc.).
That's a Phase-13 future, not v1.

Wheel contents:

```
mochi_example_app-0.1.0-py3-none-any.whl
├── mochi_example_app/
│   ├── __init__.py
│   ├── __main__.py
│   ├── py.typed
│   └── generated/
│       ├── __init__.py
│       └── foo.py
└── mochi_example_app-0.1.0.dist-info/
    ├── METADATA          # PEP 643 metadata
    ├── WHEEL             # wheel format metadata
    ├── RECORD            # file hashes
    ├── entry_points.txt  # [project.scripts] table
    └── licenses/
        └── LICENSE
```

The `.dist-info` directory is generated by hatchling from
`pyproject.toml`. The `RECORD` file lists every wheel entry plus its
SHA256; the wheel is invalid if any entry's hash doesn't match. The
`METADATA` file is PEP 621 metadata flattened into the
`Metadata-Version: 2.3` format (PEP 643).

## sdist building

`uv build --sdist` produces `dist/mochi_example_app-0.1.0.tar.gz`. The
sdist is a `tar.gz` of the source tree plus a `PKG-INFO` file (same
content as wheel METADATA). PEP 643 standardised sdist's `PKG-INFO` to
match wheel's `METADATA` exactly. Before PEP 643, the two diverged
subtly.

sdist contents:

```
mochi_example_app-0.1.0/
├── PKG-INFO
├── pyproject.toml
├── README.md
├── LICENSE
├── src/
│   └── mochi_example_app/
│       ├── __init__.py
│       ├── __main__.py
│       ├── py.typed
│       └── generated/
│           ├── __init__.py
│           └── foo.py
└── tests/
    └── test_foo.py
```

The sdist is what users `pip install` falls back to when no
compatible wheel exists. For pure-Python packages the wheel always
covers, but PyPI requires the sdist to be present for security audit
trails.

We always upload both `--wheel` and `--sdist`. uv build defaults to
both when no flag is passed.

## Editable installs

`uv pip install -e .` installs the package in editable / development
mode (PEP 660). Editable installs allow the developer to import the
package and have code changes picked up without reinstalling.

PEP 660 (2021) replaced the older `setup.py develop` mechanism with a
backend-driven editable hook. Hatchling implements PEP 660: editable
installs create a `.pth` file in site-packages that adds `src/` to
`sys.path`, then drops a stub at the wheel content location.

For Mochi development we use editable installs liberally. The Mochi
test fixture runner (`tests/transpiler3/python/`) does
`uv pip install -e .` once and then runs `pytest` from the source
tree.

## PyPI Trusted Publishing

Trusted Publishing (also called PEP 740 attestations + OIDC) is the
2023 PyPI feature that replaces long-lived API tokens with short-lived
OIDC tokens minted by the CI provider.

Before Trusted Publishing, the workflow was:

1. Project owner generates a PyPI API token in the PyPI web UI.
2. Owner copies the token into a GitHub Actions secret.
3. Workflow uses the secret to authenticate `twine upload`.

This works but has problems:

- The token is long-lived. If it leaks (mis-pushed to a public repo,
  compromised CI host) the attacker has indefinite publish access.
- The token is scoped to the user or the project, not the CI run. The
  attacker who steals the token from a CI host can use it from
  anywhere.
- Rotation is manual: someone has to remember to rotate.

Trusted Publishing fixes these:

1. Project owner registers the GitHub repo + workflow + environment
   as a trusted publisher in the PyPI web UI.
2. The workflow requests an OIDC token from GitHub Actions
   (`id-token: write` permission).
3. uv publish (or `pypa/gh-action-pypi-publish`) trades the OIDC token
   to PyPI for a 15-minute publish credential.
4. PyPI verifies the OIDC token's `repository`, `workflow`,
   `environment`, and `ref` claims match the configured trust.
5. The upload proceeds; the credential expires 15 minutes later.

No long-lived secret. No rotation. The trust is bound to the specific
repo + workflow + environment.

### Setup steps

In the PyPI project settings, the owner adds a Trusted Publisher:

- Owner: `mochilang`
- Repository name: `mochi-example-app`
- Workflow filename: `publish.yml`
- Environment name: `pypi` (optional but recommended)

In GitHub Actions:

```yaml
name: Publish to PyPI
on:
  release:
    types: [published]

jobs:
  publish:
    runs-on: ubuntu-24.04
    environment: pypi
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
      - run: uv python install 3.12
      - run: uv build
      - run: uv publish --trusted-publishing always
```

The `id-token: write` permission grants the workflow an OIDC token.
The `environment: pypi` constraint pairs with the PyPI configuration
to require the workflow to run in that GitHub environment (which can
require manual approval). `uv publish --trusted-publishing always`
tells uv to use OIDC and fail if not in a CI environment.

### PEP 740 attestations

PEP 740 (accepted 2024) adds signed attestations to PyPI uploads. The
attestation is a Sigstore-signed statement about the artifact: who
built it (workflow), when, from what commit, with what command. PyPI
stores the attestation alongside the wheel.

Trusted Publishing automatically generates a PEP 740 attestation when
the upload uses OIDC. Users can verify with `uv tool run sigstore
verify pypi mochi-example-app 0.1.0`.

We default to attesting all releases starting Phase 18.

## Name reservation strategy

PyPI follows a first-come-first-served name policy. If we want
`mochi-runtime`, `mochi-example-app`, `mochi-jupyter`, we must claim
them before any squatter.

PEP 541 governs reclaiming squatted names. The process is slow (months
of correspondence with PyPI admins) and only succeeds for unambiguous
trademark or organization claims. Better to register early.

The plan:

1. **Phase 1 (immediate)**: register `mochi-runtime` as version 0.0.0
   with a placeholder README pointing to mochilang.dev. This is a
   common practice (Pydantic did it, FastAPI did it, ruff did it).
2. **Phase 4 (records)**: register `mochi-records-helpers` as 0.0.0.
3. **Phase 9 (agents)**: register `mochi-agents` as 0.0.0.
4. **Phase 12 (FFI)**: register `mochi-ffi` and `mochi-ctypes` as 0.0.0.
5. **Phase 17 (Jupyter)**: register `mochi-kernel` as 0.0.0.

We also register the protective namespaces:

- `mochilang` (the org's own packages eventually)
- `mochi` (top-level squat protection; the project name happens to
  collide with an unrelated package `mochi` 0.0.0 on PyPI that has
  been a single empty release since 2014; we filed PEP 541 to take it
  over in 2026-Q1 and were granted ownership)

The placeholder release has `requires-python = ">=99"` to make it
impossible to install accidentally. The README says "Reserved for the
Mochi project; see mochilang.dev". This is documented in
`[[12-risks-and-alternatives]]` R6.

## Reproducibility

The wheel SHA256 must be identical across two CI hosts given the same
input. This is the v1 reproducibility gate (see `[[11-testing-gates]]`).

Sources of non-determinism in wheel building, with mitigations:

1. **Mtime in zip headers**. Wheels are zip files; zip stores mtime
   per entry. hatchling 1.25+ honors `SOURCE_DATE_EPOCH` and writes
   that epoch into every entry's mtime field. We set
   `SOURCE_DATE_EPOCH` to the git commit timestamp in CI:
   `export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)`.
2. **Filesystem walk order**. `os.walk()` returns entries in
   filesystem order, which on ext4 differs from XFS differs from APFS.
   hatchling 1.25+ sorts entries lexicographically before writing the
   zip. We verify with `unzip -l` and compare with a sorted reference.
3. **Python bytecode caches**. `__pycache__` directories must be
   excluded from the wheel. hatchling excludes them by default. We
   verify by inspecting the wheel.
4. **Locale-dependent string formatting**. The `WHEEL` metadata file
   has a `Generator:` field. hatchling writes
   `Generator: hatchling 1.25.0` (no locale variation).
5. **uv version drift**. Different uv versions might invoke the
   backend differently. We pin uv exactly: `astral-sh/setup-uv@v3`
   with `version: "0.7.0"`.
6. **CPython version drift**. The hash of pycache files can vary
   between CPython point releases (3.12.0 vs 3.12.7). We don't ship
   pycache, so this doesn't matter for the wheel; but we set the
   `Python-Version` metadata field to `>=3.12`, not a specific
   version, to keep wheel metadata constant.

The reproducibility test in CI:

```yaml
- name: Build wheel on host A
  run: SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) uv build --wheel
- name: Record SHA
  run: shasum -a 256 dist/*.whl > /tmp/host-a.sha
- name: Clean and rebuild
  run: rm -rf dist && SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) uv build --wheel
- name: Compare
  run: shasum -a 256 -c /tmp/host-a.sha
```

Across linux-x86_64 + linux-aarch64 + macos-arm64 + windows-x86_64 CI
hosts, the wheel SHA must match. We do not gate on windows
reproducibility in Phase 16 because windows filesystem case-insensitivity
introduces deltas we have not finished pinning; the gate is
linux + macos, with windows added in Phase 16.1 (see
`[[11-testing-gates]]`).

## Jupyter ipykernel target

`mochi build --target=python-ipykernel` produces a kernel spec usable
by JupyterLab. The kernel spec is a directory with a `kernel.json` and
optionally helper files.

Generated kernel spec:

```
~/.local/share/jupyter/kernels/mochi/
├── kernel.json
├── kernel.py
├── logo-32x32.png
└── logo-64x64.png
```

`kernel.json`:

```json
{
  "argv": [
    "{python}",
    "-m",
    "mochi_kernel",
    "-f",
    "{connection_file}"
  ],
  "display_name": "Mochi 0.1",
  "language": "mochi",
  "interrupt_mode": "signal",
  "metadata": {
    "mochi_version": "0.1.0",
    "transpiler_version": "MEP-51"
  }
}
```

The `kernel.py` is a thin wrapper around `ipykernel.kernelapp.IPKernelApp`
with a custom `Kernel` subclass that:

1. Receives Mochi source as a cell.
2. Calls `mochi transpile --target=python --partial=cell` to get
   Python source for that cell.
3. Maintains a persistent execution namespace across cells (a single
   `dict` passed to `exec`).
4. Executes the Python source and returns the result.

Cell-by-cell state management is non-trivial: each cell may declare
new types, redefine functions, mutate values. The kernel runs each
cell's Python in the same `globals()` so re-imports and re-definitions
take effect. This matches IPython's behaviour.

Install:

```
$ mochi build --target=python-ipykernel
$ jupyter kernelspec install kernels/mochi --user
```

`jupyter kernelspec list` should now show `mochi`. Opening JupyterLab
and creating a new notebook with kernel "Mochi 0.1" gives the user a
Mochi REPL.

The ipykernel target depends on the `ipykernel` package, which is
declared as an optional extra (`mochi-runtime[jupyter]`).

## mochi build CLI

The Mochi CLI exposes target flags for each output:

| Target                 | Output                                           |
|------------------------|--------------------------------------------------|
| `python-source`        | `src/<pkg>/generated/*.py` (no build)            |
| `python-sdist`         | `dist/<pkg>-<v>.tar.gz` via `uv build --sdist`   |
| `python-wheel`         | `dist/<pkg>-<v>-py3-none-any.whl` via `uv build --wheel` |
| `python-ipykernel`     | `kernels/mochi/kernel.json` + helpers            |
| `python-all`           | source + sdist + wheel + ipykernel               |

The default is `python-source`; explicit target required for build
artifacts. The CLI shells out to `uv` for the build targets; uv is
detected on PATH. If uv is missing, the CLI prints an install
suggestion and exits 2.

## CI integration

GitHub Actions workflow for a Mochi-emitted package. This is the
emitted `ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    name: ${{ matrix.os }} / py${{ matrix.python }}
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-24.04, ubuntu-24.04-arm, macos-14, windows-2022]
        python: ["3.12", "3.13"]
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
          enable-cache: true
      - name: Install Python ${{ matrix.python }}
        run: uv python install ${{ matrix.python }}
      - name: Sync deps
        run: uv sync --frozen
      - name: Lint (ruff)
        run: |
          uv run ruff check src/ tests/
          uv run ruff format --check src/ tests/
      - name: Type check (mypy)
        run: uv run mypy src/ tests/
      - name: Type check (pyright)
        run: uv run pyright
      - name: Test (pytest)
        run: uv run pytest
      - name: Build wheel + sdist
        run: uv build
      - name: Install wheel and smoke test
        run: |
          uv pip install --system dist/*.whl
          python -c "import mochi_example_app; print(mochi_example_app.__version__)"

  reproducibility:
    name: Wheel SHA reproducibility
    needs: test
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
      - run: uv python install 3.12
      - name: First build
        run: |
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          uv build --wheel
          shasum -a 256 dist/*.whl > /tmp/sha1.txt
      - name: Second build
        run: |
          rm -rf dist
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          uv build --wheel
          shasum -a 256 dist/*.whl > /tmp/sha2.txt
      - name: Compare
        run: diff /tmp/sha1.txt /tmp/sha2.txt

  publish:
    name: Publish to PyPI
    if: github.event_name == 'release'
    needs: [test, reproducibility]
    runs-on: ubuntu-24.04
    environment: pypi
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
      - run: uv python install 3.12
      - run: |
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          uv build
      - run: uv publish --trusted-publishing always
```

Key callouts:

- Matrix is `os × python = 8 cells`. We test both linux x86_64 and
  linux aarch64 (the GitHub-hosted ARM runners released GA 2024). We
  test macos arm64 (M-series) only; the x86_64 mac runners are
  deprecated. We test Windows on x86_64.
- `uv python install` ensures the requested Python is available.
  GitHub-hosted runners come with Python 3.10 to 3.13 pre-installed;
  uv detects and uses them when available, otherwise downloads.
- `uv sync --frozen` installs from `uv.lock` with hash verification.
  No resolution at CI time.
- `ruff check` + `ruff format --check` are the lint gates. We do not
  run black separately: ruff format is black-compatible (see below).
- mypy and pyright both run. Both must pass. This is the dual-gate
  decision from `the shared-decisions anchor`.
- The reproducibility job runs only on linux x86_64 in Phase 15.
  Phase 16 expands to linux aarch64 + macos arm64.
- The publish job runs only on a GitHub release event. It uses OIDC
  (`id-token: write`) and an environment-scoped trust.

## ruff configuration

ruff is the canonical Mochi-Python linter and formatter. ruff version
0.6.9 (October 2024) introduced `ruff format` as a stable
black-compatible formatter. We rely on this.

`[tool.ruff]` config above. Highlights:

- `line-length = 100`. Black's default is 88; ruff's default is 88;
  we override to 100 because Mochi-emitted code has long fully-qualified
  type hints (`MochiResult[list[dict[str, MochiOption[int]]]]`) that
  blow past 88 frequently.
- `target-version = "py312"`. Tells ruff which Python features are
  available. ruff uses this to enable PEP 695 syntax recognition.
- `src = ["src", "tests"]`. ruff uses this for import sorting (`I`
  rules) to distinguish first-party from third-party.
- Lint rule selection: pycodestyle, pyflakes, isort, flake8-bugbear,
  pyupgrade, ruff-specific, flake8-simplify, pylint subset,
  type-checking. We skip `D` (pydocstyle) because Mochi may or may
  not have docstrings on every function and we don't want to force
  rewriting.
- `PLR0913` (too many arguments) suppressed because Mochi emits
  faithful function signatures.

`ruff format` configuration:

- `quote-style = "double"`. Black's default. Mochi emits strings with
  double quotes; ruff format leaves them alone.
- `indent-style = "space"`. Four-space indent, Black-compatible.
- `docstring-code-format = true`. Format code blocks inside
  docstrings. Useful for the runtime stub which has examples in
  docstrings.

## Black: not used directly

We do not run `black` separately. Reasoning:

- `ruff format` reproduces `black`'s output exactly. The ruff team
  tracks black's behaviour as a regression test (the ruff repo has a
  "Black compatibility" CI job that diffs ruff format against black
  on a corpus of 50000+ files).
- Running both is duplication: same formatting, twice the wall-clock,
  twice the surface area for tool-version drift.

Mochi-emitted projects do not declare `black` as a dev dependency. If
a user prefers black for non-Mochi files in the same repo, they can
add it; ruff format will not fight black's choices.

## Pre-commit hooks

We do not emit a `.pre-commit-config.yaml` by default. Pre-commit is a
developer-workflow choice; some teams use it, others use Husky-style
git hooks, others use IDE integration. Forcing a choice on users is
overreach.

We document the recipe in the generated README:

```yaml
# .pre-commit-config.yaml (optional)
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.6.9
    hooks:
      - id: ruff
      - id: ruff-format
  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.13.0
    hooks:
      - id: mypy
        additional_dependencies: [pytest, types-requests]
```

The user adds this manually if they want it.

## Cross-OS path handling

Mochi-emitted Python uses `pathlib.Path` everywhere, never raw
strings. Reasons:

1. Windows uses `\` as path separator. `os.path.join("a", "b")`
   returns `a\b` on Windows, `a/b` on POSIX. Raw string concatenation
   like `dir + "/" + name` breaks.
2. `Path` handles edge cases: trailing slashes, double slashes,
   relative-to-absolute conversion, parent traversal.
3. `Path` interoperates with stdlib: `open(Path("foo"))` works,
   `subprocess.run(Path("script.py"))` works.

The emitter's IO layer is:

```python
from __future__ import annotations
from pathlib import Path

def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")

def write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")
```

All paths in user-visible APIs are `Path`, not `str`. Conversion
happens at the boundary with third-party libraries that demand `str`.

## Workspace mode

uv 0.5+ supports workspaces (think Cargo workspaces). A workspace is
a root `pyproject.toml` with `[tool.uv.workspace]` listing member
packages. Each member has its own `pyproject.toml`. `uv sync` syncs
all members into a single shared venv with `--editable` cross-links.

Mochi-emitted projects with multiple subpackages (Mochi's `mod` system
maps to Python subpackages) use a workspace root:

```toml
# myapp/pyproject.toml
[tool.uv.workspace]
members = ["packages/*"]
```

```
myapp/
├── pyproject.toml         # workspace root
├── packages/
│   ├── core/
│   │   ├── pyproject.toml
│   │   └── src/myapp_core/
│   └── api/
│       ├── pyproject.toml
│       └── src/myapp_api/
```

This is opt-in. The default Mochi-emitted layout is single-package.

## ABI3 wheels: deferred

We emit pure Python (`py3-none-any`). If we ever ship C extensions,
the right tag is the ABI3 stable interface (`cp312-abi3-linux_x86_64`).
ABI3 wheels work across CPython versions (3.12+ in our case) without
recompilation, in contrast to per-version wheels
(`cp312-cp312-linux_x86_64`). This is a future-track decision; see
`[[12-risks-and-alternatives]]` F2.

## Comparison to MEP-50 (Kotlin)

The most useful contrast is to Kotlin's Gradle build (MEP-50). The
deltas:

| Concern        | MEP-50 (Kotlin / Gradle)              | MEP-51 (Python / uv + hatchling)             |
|----------------|----------------------------------------|----------------------------------------------|
| Build file     | `build.gradle.kts` (Kotlin DSL)        | `pyproject.toml` (TOML)                       |
| Build driver   | Gradle 8.x                              | uv 0.7+                                       |
| Lockfile       | `gradle.lockfile` (per-config)         | `uv.lock` (cross-platform)                    |
| Toolchain mgmt | `kotlin.jvmToolchain(17)`              | `uv python install 3.12`                      |
| Artifact       | JAR + Maven Central POM                 | Wheel + sdist + PyPI metadata                 |
| Publish        | `gradle publish` to OSSRH               | `uv publish` to PyPI (Trusted Publishing)     |
| OIDC publish   | Sonatype's central-portal (2024+)      | PyPI Trusted Publishing (2023+)               |
| Reproducibility| `org.gradle.parallel.repro=true`       | `SOURCE_DATE_EPOCH` + sorted entries          |
| Plugins        | Gradle plugins                          | hatchling build hooks (rarely used)           |

The Python story is structurally simpler. Gradle is a polyglot
JVM-language build system with a learning curve; uv + hatchling are
single-purpose tools. The biggest delta in user experience is install
time: a cold Gradle build can take 30+ seconds before any code
compiles; `uv sync` cold completes in 1-2 seconds for typical
projects.

## Comparison to MEP-45 (C)

MEP-45 emits C source plus a `Makefile`. The build artifact is an
executable or a `.so` / `.dylib` / `.dll`. There is no canonical
package manager (vcpkg, conan, apt, brew, msys2 all compete).

MEP-51 emits Python source plus a `pyproject.toml`. The build
artifact is a `.whl` or `.tar.gz`. There is exactly one canonical
package registry (PyPI). The Python story is materially simpler at
this layer.

Where Python is harder: type-checking gates (`mypy`, `pyright`). C
has no equivalent. We handle that in `[[11-testing-gates]]`.

## Open questions

1. **uv vs pip fallback**. Some corporate environments forbid
   downloading binaries (uv). We must document the pip-only fallback.
   `pip install build twine` then `python -m build && twine upload`
   is the pip-equivalent. We test the fallback path in CI at least
   once per release. (See `[[12-risks-and-alternatives]]` R10.)
2. **PyPI mirror support**. Internal PyPI mirrors (devpi, artifactory)
   use the same JSON API. `uv pip install --index-url` and
   `uv publish --publish-url` handle the redirection. Documented in
   the package's README only when the user runs `mochi build
   --enterprise`.
3. **Pre-built wheels for runtime stub**. `mochi-runtime` itself is
   pure Python; no platform-specific wheels needed. If we ever ship
   native code in the runtime, we'd need manylinux + macos universal2
   + windows wheels. Tracked in `[[12-risks-and-alternatives]]` R12.

## References

- PEP 517 (build backend interface)
- PEP 518 (`pyproject.toml`)
- PEP 621 (`[project]` table)
- PEP 643 (sdist/wheel metadata parity)
- PEP 660 (editable installs)
- PEP 427 (wheel format)
- PEP 425 (compatibility tags)
- PEP 740 (PyPI attestations + sigstore)
- PEP 541 (package name reclamation)
- PyPA Hatch documentation, `hatch.pypa.io`
- Astral uv documentation, `docs.astral.sh/uv`
- PyPI Trusted Publishing guide, `docs.pypi.org/trusted-publishers/`
- Henry Schreiner, "Should You Use Upper Bound Version Constraints?",
  `iscinumpy.dev/post/bound-version-constraints/` (2021)
- `the shared-decisions anchor` for the load-bearing decisions
- `[[11-testing-gates]]` for the per-phase gate definitions
- `[[12-risks-and-alternatives]]` for build-related risks
