---
title: "06. PyPI publish flow"
sidebar_position: 7
sidebar_label: "06. PyPI publish flow"
description: "The PyPI upload protocol (PEP 700 JSON, PEP 691 JSON simple, PEP 503 HTML simple), the per-package metadata requirements (PEP 621 / 643 core metadata 2.4), the sdist / wheel format (PEP 517 / 518 / 425 / 600 / 656), the `mochi-build` PEP 517 backend, the publish-side gate."
---

# 06. PyPI publish flow

This note covers the publish direction: how a Mochi package becomes an installable PyPI distribution. The publish flow is built on PEP 517 (build-system contract), PEP 621 (project metadata), PEP 425 / 600 / 656 (wheel compatibility tags), PEP 503 / 691 / 700 (index protocols), and PEP 740 (attestations).

## The build chain at a glance

```
Mochi source
  │
  ▼
Driver.Build(target=TargetPythonPackage, LibraryMode=true)
  │
  ▼
mochi-build PEP 517 backend
  ├── build_sdist()   → mochi-pkg-1.2.3.tar.gz
  └── build_wheel()   → mochi_pkg-1.2.3-cp312-abi3-manylinux_2_28_x86_64.whl
  │
  ▼
Sigstore: cosign sigstore-keyless OIDC flow
  │
  ▼
PEP 740 attestation bundle
  │
  ▼
PyPI Trusted Publishing upload (HTTP POST to /legacy/)
  │
  ▼
Index visible at https://pypi.org/simple/<dist>/
```

The two artifacts produced are an sdist (source distribution, a tarball of the source tree) and one or more wheels (binary distributions, zip files with a specific layout). PyPI accepts both.

## The PEP 517 backend

PEP 517 defines an interface between a frontend (pip, uv, build) and a backend (the per-project build tool). The backend is named in `pyproject.toml`:

```toml
[build-system]
requires = ["mochi-build>=0.1"]
build-backend = "mochi_build.backend"
```

The backend exposes (at minimum) two hooks:

- `build_sdist(sdist_directory, config_settings=None) -> str` returns the name of the sdist tarball.
- `build_wheel(wheel_directory, config_settings=None, metadata_directory=None) -> str` returns the name of the wheel file.

PEP 660 added an editable-install hook (`build_editable`) that Mochi's backend also implements. PEP 643 / 660 / 662 govern the metadata and editable-install discipline.

The `mochi_build.backend` module is a small Python shim that subprocesses out to `mochi pkg build --backend-mode`. Mochi does the work in Go; the Python shim merely satisfies the PEP 517 contract.

## sdist format

An sdist is a gzipped tar archive containing:

```
<dist>-<version>/
  pyproject.toml          # PEP 517 build-system declaration
  PKG-INFO                # core metadata 2.4 (PEP 643 / PEP 621)
  <source files>          # the Mochi source tree
  <mochi.toml>            # project manifest
  <mochi.lock>            # dep lockfile
```

The PKG-INFO file holds the core metadata in RFC 822-style:

```
Metadata-Version: 2.4
Name: mochi-pkg-foo
Version: 1.2.3
Summary: A Mochi-authored utility package.
Author-email: Foo Bar <foo@example.com>
License-Expression: MIT
Requires-Python: >=3.12,<3.15
Requires-Dist: numpy>=2.0
Requires-Dist: pydantic>=2.5
Classifier: Programming Language :: Python :: 3
Project-URL: Source, https://github.com/foo/mochi-pkg-foo
```

The metadata is generated from `[python.publish]` in the project's mochi.toml, plus the deps from `[python-dependencies]`. PEP 643 mandates the metadata be static (no setup.py code execution); the Mochi backend complies by reading mochi.toml at build time and writing the metadata.

The sdist is the universal fallback: when no wheel matches the target platform, pip installs from sdist by running the PEP 517 backend on the user's machine. Mochi-built packages always ship sdist so that a Mochi-authored package can be installed on platforms where no prebuilt wheel exists.

## Wheel format

A wheel is a zip archive with a specific layout (PEP 427, normative now via PEP 491):

```
mochi_pkg-1.2.3-cp312-abi3-manylinux_2_28_x86_64.whl
  ├── mochi_pkg/                   # the importable package
  │     ├── __init__.py
  │     ├── _native.cpython-312-x86_64-linux-gnu.so   # compiled extension
  │     ├── py.typed                # PEP 561 marker
  │     └── _mochi_wrap.pyi         # synthesised stubs
  └── mochi_pkg-1.2.3.dist-info/
        ├── METADATA              # core metadata (same as PKG-INFO)
        ├── WHEEL                 # wheel metadata (tags, generator)
        ├── RECORD                # SHA256 of every file
        ├── entry_points.txt      # PEP 517 entry points
        └── attestations.json     # PEP 740 attestations (added 2024-Q4)
```

The wheel filename encodes the compatibility tag: `<dist>-<version>-<python_tag>-<abi_tag>-<platform_tag>.whl`. For Mochi packages:

- **Pure-Python wraps** (Mochi compiled to Python source, no native ext): `py3-none-any`. Installable everywhere.
- **abi3 native wraps** (Mochi compiled to native ext using the limited API): `cp312-abi3-<platform>`. Installable on CPython 3.12, 3.13, 3.14, ... on the matching platform.
- **Per-minor native wraps** (full CPython API): `cp312-cp312-<platform>`. Installable only on CPython 3.12 on the matching platform.

The platform tag follows PEP 600 (manylinux), PEP 656 (musllinux), or platform-specific conventions (`macosx_11_0_arm64`, `win_amd64`).

## Wheel compatibility tags in depth

The compatibility tags determine which platforms can install the wheel.

**Python tag** (`py3`, `cp312`, `cp313`, `pp310`): identifies the interpreter implementation and version. `py3` is the generic Python 3 tag (no implementation-specific features); `cp312` is CPython 3.12 specifically.

**ABI tag** (`none`, `abi3`, `cp312`, `cp313t`):
- `none`: no ABI dependency. Pure-Python or pure-bytecode.
- `abi3`: the limited API (PEP 384), stable across CPython minor versions.
- `cp312`: the full CPython 3.12 API and ABI.
- `cp313t`: the free-threaded CPython 3.13 ABI (PEP 779 / 803). Distinct from `cp313` because the structure of `PyObject` differs.

**Platform tag** (`any`, `manylinux_2_28_x86_64`, `musllinux_1_2_x86_64`, `macosx_11_0_arm64`, `win_amd64`):
- `any`: no platform dependency. Pure-Python.
- `manylinux_X_Y_<arch>`: Linux with glibc >= X.Y on the given architecture (PEP 600).
- `musllinux_X_Y_<arch>`: Linux with musl libc >= X.Y (PEP 656).
- `macosx_X_Y_<arch>`: macOS X.Y on the given architecture.
- `win_amd64`, `win32`, `win_arm64`: Windows on the given architecture.

The Mochi backend computes the right combination based on `[python.publish]` configuration and the actual compiled artifacts. auditwheel-equivalent validation runs on Linux wheels to verify the manylinux symbol-version requirements are satisfied.

## The PyPI upload protocol

PyPI's upload endpoint is `https://upload.pypi.org/legacy/`. The endpoint accepts a multipart POST with:

- The file (sdist tarball or wheel zip).
- The metadata fields (Name, Version, Author, etc.) as form fields.
- Authentication: legacy API token (deprecated) or Trusted Publishing OIDC token (the only path Mochi supports).
- PEP 740 attestation bundle as an attached attestations field.

The upload is atomic: either the file is published or the upload errors. Re-uploading the same `(name, version, filename)` triple is rejected (PEP 427 §Yanking notwithstanding). Yanking is a separate operation.

After upload, the file appears in three indexes:

- **PEP 503 HTML simple index**: `https://pypi.org/simple/<dist>/`. The historical legacy format, an HTML page listing every uploaded file with its hash.
- **PEP 691 JSON simple index**: `https://pypi.org/simple/<dist>/` with `Accept: application/vnd.pypi.simple.v1+json`. The modern JSON format with hashes, yanked status, and PEP 700 metadata side-channel.
- **PEP 700 metadata side-channel**: per-file `.metadata` URL serving the file's METADATA without downloading the full wheel.

uv and modern pip query the JSON simple index by default; legacy pip queries the HTML simple index.

## The publish-side gate

Before upload, the Mochi backend runs a series of checks:

1. **Metadata completeness.** Name, Version, Author-email, Description, Requires-Python are required. Missing fields fail with a diagnostic.
2. **License expression validity.** PEP 639 SPDX expression. Required since PyPI 2025-Q2.
3. **Wheel platform tag accuracy.** auditwheel-equivalent check: the wheel's declared platform tag must match the symbols its native ext requires.
4. **abi3 verification.** When `abi3 = true`, the native ext must reference only symbols in the limited API. The backend runs `nm -D` (Linux) or equivalent and checks against the PEP 384 allowed-symbol list.
5. **Capability declaration.** The Mochi runtime's reachable capability set must be a subset of `[python.capabilities]`. Same gate as the consume direction.
6. **Signature.** The Sigstore-keyless OIDC flow must succeed. The Fulcio cert, Rekor log entry, and PEP 740 attestation bundle are generated.

Any failure aborts the upload. The diagnostic surface names the specific check that failed.

## Yanking and post-publish operations

PyPI supports yanking a release (PEP 592): the file remains available for pinned installs but is not selected by version resolvers. The Mochi backend exposes `mochi pkg yank --version=1.2.3 --reason="security fix incoming"` which calls PyPI's yank endpoint.

Deletion is not supported by PyPI (immutability of published artifacts is a deliberate PyPI policy). The only path is to release a new version. The Mochi CLI surface does not include a delete operation.

## Cross-references

- [[07-sigstore-pypi-trusted-publishing]] for the OIDC and attestation flow.
- [[09-abi-stability]] for the wheel compatibility tag deep dive.
- [[02-design-philosophy]] §5 for why Trusted Publishing is the only path.
- [PEP 517](https://peps.python.org/pep-0517/), [PEP 621](https://peps.python.org/pep-0621/), [PEP 643](https://peps.python.org/pep-0643/) for the build system and metadata.
- [PEP 425](https://peps.python.org/pep-0425/), [PEP 600](https://peps.python.org/pep-0600/), [PEP 656](https://peps.python.org/pep-0656/) for compatibility tags.
- [PEP 503](https://peps.python.org/pep-0503/), [PEP 691](https://peps.python.org/pep-0691/), [PEP 700](https://peps.python.org/pep-0700/) for the index protocols.
- [PEP 740](https://peps.python.org/pep-0740/) for attestations.
