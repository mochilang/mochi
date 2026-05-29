---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import python \"<package>@<semver>\" as <alias>` import form, the `[python-dependencies]` / `[python]` / `[python.publish]` / `[python.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add python`, `mochi pkg lock`, `mochi pkg publish --to=pypi`, `mochi pkg sync python`, `mochi pkg export pylock`), and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-71 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust"
```

MEP-71 promotes `python` from a transpiler-only directive (MEP-51) to a full package-manager surface. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<package-name>` | Bare name (PEP 503 normalised). Resolves through `[python-dependencies]` plus `mochi.lock`. The lockfile records the picked version. |
| `<package-name>@<semver-req>` | Explicit PEP 440 constraint (`>=1.0,<2.0`, `~=1.4`, `==1.2.3`, `^1.0` shorthand auto-translated to `>=1.0,<2.0`). Must be compatible with `[python-dependencies]`. |
| `<package-name>@git+<url>` | Git source, branch default. |
| `<package-name>@git+<url>#<rev>` | Git source, pinned to commit, tag, or branch. |
| `<package-name>@path+<rel-path>` | Path source, relative to the manifest. Useful for editable in-tree installs. |
| `<package-name>[<extra>,<extra>]@<semver-req>` | PEP 508 extra-marker form. The bracketed extras pull additional optional deps. |

Example surface:

```mochi
import python "requests@>=2.31,<3" as requests
import python "httpx[http2]@>=0.27" as httpx
import python "numpy@~=2.0" as np
import python "pydantic@>=2.5" as pyd

fn fetch_user(uid: int): User {
    let resp = requests.get(f"https://api.example/{uid}")
    let payload = resp.json()
    let user = pyd.parse_obj_as(User, payload)
    return user
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` or `extern type` declaration the bridge generated for `<package>.<item>`. Names follow the package's public surface verbatim (snake_case Python names stay snake_case in Mochi; PascalCase classes stay PascalCase as types).

The `auto` modifier (already accepted for `import go ... auto` and `import rust ... auto`) is admitted for `import python ... auto`. With `auto`, every public top-level item of the package (filtered by `__all__` if defined, else by leading underscore) is bound at file scope rather than under the alias namespace. Default is namespaced; `auto` is opt-in.

## Manifest: `[python-dependencies]`

This table is the user-facing dependency declaration. It follows PEP 621's `[project.dependencies]` grammar plus the table-form override used by uv and Poetry:

```toml
[python-dependencies]
requests = ">=2.31,<3"
httpx = { version = ">=0.27", extras = ["http2"] }
numpy = "~=2.0"
pandas = { version = ">=2.2", markers = "platform_system != 'Windows'" }
pydantic = ">=2.5"
my-local-pkg = { path = "../my-pkg", editable = true }
my-git-pkg = { git = "https://github.com/example/my-pkg", tag = "v0.2.0" }
torch = { version = ">=2.3", index = "https://download.pytorch.org/whl/cpu" }
```

The grammar mirrors uv's:

- A bare string is shorthand for `{ version = "..." }`.
- The table form admits `version`, `extras`, `markers` (PEP 508 environment markers), `optional`, `path`, `editable`, `git`, `branch`, `rev`, `tag`, `index` (alternative index URL), and `groups` (PEP 735 dependency groups).
- Cyclic dependencies are rejected at lock time (the same rule Pip/uv enforce).

The user does not write a separate `pyproject.toml` for consumption. The bridge synthesises a private `pyproject.toml` at build time when invoking uv against an ephemeral venv, populating `[project.dependencies]` from `[python-dependencies]` and pinning to the exact resolved version from `mochi.lock`.

## Manifest: `[python]`

```toml
[python]
requires-python = ">=3.12,<3.15"
implementation = "cpython"
runtime-mode = "embedded"
async-mode = "per-call"
stubgen = { fallback = true, inspect = true }
sidecar-glob = "*_externs.py"
free-threaded = false
```

| Key | Default | Meaning |
|-----|---------|---------|
| `requires-python` | `">=3.12"` | PEP 440 constraint on the interpreter version. Matches MEP-51's transpiler floor. |
| `implementation` | `"cpython"` | One of `cpython`, `pypy`, `graalpy`. Currently only `cpython` is wired; the others surface a `SkipReason`. |
| `runtime-mode` | `"embedded"` | `embedded` links libpython into the Mochi binary; `subprocess` shells out to a `python` interpreter. Embedded is faster; subprocess is the only path under sandboxed deployments. |
| `async-mode` | `"per-call"` | `per-call` wraps each Mochi-to-Python async call in its own `asyncio.run` (no shared state); `persistent` keeps one event loop alive in a dedicated thread. See [[08-async-bridge]]. |
| `stubgen` | `{ fallback = true, inspect = true }` | When PEP 561 stubs are missing, the bridge falls back to mypy's `stubgen --inspect-mode` to synthesise types from live import. Disable to refuse imports of untyped packages. |
| `sidecar-glob` | `"*_externs.py"` | The MEP-51 Phase 12 sidecar convention. The bridge reads these files to discover hand-written wrapper functions. |
| `free-threaded` | `false` | When `true`, the bridge requires `cp313t` / `cp314t` wheels and refuses to install GIL-only wheels. |

## Manifest: `[python.publish]`

```toml
[python.publish]
distribution-name = "mochi-pkg-foo"
build-backend = "mochi-build"
wheel-tags = ["py3-none-any", "cp312-cp312-manylinux_2_28_x86_64"]
abi3 = true
abi3-min-version = "cp312"
publish-to = "pypi"
license-expression = "MIT"
trove-classifiers = ["Programming Language :: Python :: 3", "Programming Language :: Python :: 3.12"]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `distribution-name` | (required) | The PEP 503-normalised distribution name on PyPI. |
| `build-backend` | `"mochi-build"` | The PEP 517 entrypoint name. Mochi ships its own backend that produces sdist + wheel. |
| `wheel-tags` | `["py3-none-any"]` | The compatibility tags emitted by the wheel builder. Pure-Python wraps stay `py3-none-any`; native wraps explicitly enumerate `cpXY-abiZ-platform`. |
| `abi3` | `false` | When `true`, the wheel targets the limited API and gets the `abi3` tag, working across multiple CPython minors. |
| `abi3-min-version` | `"cp312"` | The minimum CPython version for the abi3 wheel. |
| `publish-to` | `"pypi"` | The publish target. Currently only `"pypi"` and `"testpypi"`; future: `"private-index"`. |
| `license-expression` | (none) | PEP 639 SPDX expression. Required by PyPI as of 2025-Q2. |
| `trove-classifiers` | `[]` | The PyPI classifier list. |

This table is only consulted when the user runs `mochi pkg publish --to=pypi`. Mochi packages that do not publish to PyPI can omit it.

## Manifest: `[python.capabilities]`

```toml
[python.capabilities]
net = true
fs = false
proc = false
ctypes = false
c-extension = true
```

These capability flags are a strict refinement of MEP-57's `[capabilities]` table. The bridge walks the Python dep graph at lock time, computes the union of capability marks across every reachable package (via a curated capability database; [[12-risks-and-alternatives]] §R6 documents the database maintenance), and asserts that the union is a subset of the user's `[python.capabilities]` declaration. If the union exceeds the declaration, lock fails with a diagnostic naming the package and the capability.

The five canonical capabilities for Python deps are:

- `net`: any reachable package opens sockets or makes HTTP calls. Detected via static imports of `socket`, `urllib`, `requests`, `httpx`, `aiohttp`.
- `fs`: any reachable package reads or writes files. Detected via imports of `open` calls outside of pyproject discovery and `pathlib`, `os.path`.
- `proc`: any reachable package calls `subprocess` or `os.exec*`.
- `ctypes`: any reachable package imports `ctypes` directly. C-extension wheels are tracked under `c-extension` instead.
- `c-extension`: any reachable wheel ships a `.so` / `.pyd` / `.dylib`. Pure-Python sdist-only deps have this false.

Capabilities outside this set (clock, env, random) are inherited from MEP-57's broader `[capabilities]` table and audited there.

## CLI surface

The `mochi pkg` subcommand gains six new operations:

### `mochi pkg add python <package>[@<semver>] [--extras=<a>,<b>] [--group=<name>]`

```
$ mochi pkg add python httpx@>=0.27 --extras=http2,brotli
Added httpx = { version = ">=0.27", extras = ["http2", "brotli"] } to [python-dependencies]
Running mochi pkg lock ...
Resolved 38 Python packages (httpx + 37 transitive)
Wrote mochi.lock (+38 [[python-package]] entries)
```

Equivalent to manually editing `mochi.toml` plus running `mochi pkg lock`. Idempotent if the entry already exists at a compatible version. Drives the uv resolver.

### `mochi pkg lock`

Walks `[python-dependencies]`, invokes uv to resolve against the PyPI simple index (PEP 503 / 691) and any extra indexes, downloads each wheel or sdist to the content-addressed cache, runs the PEP 561 stub-discovery pipeline on each, synthesises the wrapper per package, and writes a `[[python-package]]` entry per dep into `mochi.lock`.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `wheel-blake3`, `wheel-sha256`, `pypi-simple-sha256`, `stub-sha256`, `wrapper-sha256`, and `capabilities-declared`, and exits non-zero on any mismatch. This is the CI-enforced reproducibility gate.

### `mochi pkg publish --to=pypi [--dry-run] [--testpypi]`

- Builds the package via `Driver.Build` with `target = TargetPythonPackage, LibraryMode = true`.
- Runs the Mochi PEP 517 backend to produce sdist + wheel(s).
- Obtains an OIDC token from the CI environment (GitHub Actions, GitLab CI, etc.).
- Presents the token plus the wheel(s) to PyPI's trusted-publishing endpoint.
- Generates PEP 740 attestations via Sigstore-keyless OIDC, signs with Fulcio short-lived cert, records the Rekor log entry.

The `--dry-run` flag skips upload; the signing flow is still exercised against a `sigstore-mock-fulcio` harness for testing. The `--testpypi` flag routes to test.pypi.org.

### `mochi pkg sync python`

Re-runs the wrapper synthesiser from the existing `mochi.lock`, without re-resolving versions. Used after manual edits to the synthesised shim file, or after a bridge upgrade that changes the wrapper format.

### `mochi pkg export pylock`

Emits a PEP 751 `pylock.toml` from the Python-package subset of `mochi.lock`. The exported file is interchange-format compatible with pip, uv, Poetry, PDM, and other PEP 751-conformant tools. Useful for CI pipelines that need to install Python deps in a standalone container before invoking Mochi.

## Per-import alias resolution

The alias `<alias>` introduced by `import python "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/python_wrap/<package>/shim.mochi` containing a corpus of `extern fn` declarations like:

```mochi
extern type Response
extern fn get(url: string): Response from python "requests.get"
extern fn json(self: Response): any from python "requests.Response.json"
extern fn status_code(self: Response): int from python "requests.Response.status_code"
```

The import `import python "requests" as requests` becomes (post-resolution) `import "./python_wrap/requests/shim.mochi" as requests`. The synthesised shim is read by the parser exactly as a hand-written `.mochi` file would be.

The shim file is regenerated on every `mochi pkg lock` and is gitignored by default. Users who need to edit the synthesised bindings should override individual items in their own source via the MEP-51 Phase 12 sidecar pattern (`<modname>_externs.py`):

```mochi
import python "requests" as requests
extern fn requests_get_with_retries(url: string, retries: int): requests.Response from python "myapp_externs.requests_get_with_retries" custom
```

The `custom` modifier keeps the override stable across `mochi pkg sync python` runs.

## Cross-references

- [[02-design-philosophy]] for the rationale.
- [[04-pep561-stub-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-pypi-publish-flow]] for the `mochi pkg publish` path.
- [MEP-71 §4](/docs/mep/mep-0071#4-surface-syntax-import-python) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
- [MEP-51](/docs/mep/mep-0051) for the Python transpiler whose `*_externs.py` sidecar convention MEP-71 reuses.
