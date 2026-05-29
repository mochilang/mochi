---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "PyO3, maturin, Cython, CFFI, pybind11, nanobind, SWIG, JPype, Py4J, gopy, UniFFI, diplomat. What each gets right, what each requires the user to write, and what MEP-71 borrows from each."
---

# 03. Prior-art bridges

This note surveys the existing Python bridges that informed MEP-71's design. Each entry covers what the bridge does, what the user has to write, what works well, and what MEP-71 borrows or rejects.

## PyO3 (Rust → Python, Python → Rust)

PyO3 is the de-facto Rust↔Python bridge. It lets Rust crates expose a Python module (via `#[pymodule]`) and Python scripts call into Rust functions (via `#[pyfunction]`). The build tool is maturin, which packages the Rust crate as a wheel.

What the user writes: Rust source with PyO3 attribute macros. The macros generate the CPython C-API boilerplate.

What works: zero-cost type conversion via the `IntoPy` / `FromPyObject` traits, ergonomic GIL handling via the `Python<'py>` token, native async support via `pyo3-asyncio` (separate crate, bridging tokio↔asyncio). The free-threaded story is in flight: PyO3 0.23+ targets the GIL-free build via `Py_GIL_DISABLED`.

What MEP-71 borrows:

- **Wrapper-module pattern.** PyO3 generates a `_<name>.so` extension module the user imports from Python. MEP-71's synthesised CPython wrapper module is the analogue.
- **Token-style GIL handling.** PyO3 represents GIL ownership as a `Python<'py>` token. The Mochi wrapper module pattern avoids surfacing GIL tokens to the user but uses an internal equivalent.
- **abi3 strategy.** PyO3 has built-in support for the limited API; the wheel works on multiple CPython minors. MEP-71 inherits this for cross-version wheel deployment.

What MEP-71 rejects:

- **Rust as the implementation language.** Mochi's host runtime is Go; the wrapper module is generated Python, not generated Rust. PyO3's design assumes the source is Rust; MEP-71's source is Mochi.

## maturin (Rust crate → Python wheel)

maturin is the build tool that wraps cargo + PEP 517 to ship PyO3 / rust-cpython / cffi crates as Python wheels. It handles wheel-tag computation, abi3 wheel slimming, and PyPI upload via twine.

What works: tight integration with PEP 517 (`build-backend = "maturin"` in pyproject.toml), correct manylinux tags via auditwheel, abi3 support, Trusted Publishing support via the maturin-action GitHub Action.

What MEP-71 borrows:

- **PEP 517 backend pattern.** Mochi ships its own `mochi-build` PEP 517 backend that produces sdist + wheel. The backend integrates with pip / uv / pipx for installation.
- **abi3 wheel slimming.** When `[python.publish].abi3 = true`, the wheel is tagged `cp312-abi3-<plat>` and works on 3.12, 3.13, 3.14, ... without a per-minor build. This is maturin's contribution.
- **auditwheel-style platform validation.** Mochi's wheel builder runs the equivalent of auditwheel on Linux wheels to verify the manylinux platform tag matches the actual symbol requirements.

## Cython (Python with C-level types)

Cython lets users write Python with C-level type annotations that compile to C extensions. The mature path for performance-critical code in scipy, scikit-learn, lxml.

What works: extremely good native integration (Cython code can declare C structs, call C functions directly, manage memory). The compiled `.pyx` modules are first-class CPython extensions.

What MEP-71 borrows:

- **Generated `.pyx`-style wrapper.** The synthesised wrapper module is the Mochi equivalent of a Cython-compiled extension module: a CPython extension that exposes a typed surface backed by Mochi's runtime.
- **PEP 561 stub generation.** Cython has a Cython-to-stub generator. MEP-71's wrapper synthesiser produces `.pyi` files for the publish direction.

What MEP-71 rejects:

- **Cython as the source language.** Mochi is the source language; Cython's hybrid Python/C syntax is not in scope.

## CFFI (Python → C library)

CFFI lets Python scripts call into C libraries without writing CPython extension code. It uses cdef declarations to describe the C API and runtime FFI to call into it.

What works: pure-Python install (no C compilation needed for the FFI layer, only for the target C library), good portability, supports both ABI mode (dlopen) and API mode (compiled extension).

Why MEP-71 doesn't use CFFI as primary: CFFI is the right tool when the target is a C library with a stable C ABI; CPython itself is the target here. CFFI plus libpython is workable, but loses the GIL handling that the wrapper-module pattern provides for free.

## pybind11, nanobind (C++ → Python)

pybind11 and nanobind are the C++ equivalents of PyO3. pybind11 is mature and widely used (pytorch, tensorflow, opencv); nanobind is the modern, smaller, faster successor.

What MEP-71 borrows:

- **Type-caster pattern.** pybind11's type casters (the `pybind11::cast<T>` / `py::object::cast<T>()` mechanism) is the C++ analogue of MEP-71's closed type table. The mechanism generalises: each cast direction is an explicit conversion with explicit refusal cases.
- **abi3 support.** Both libraries support the limited API. MEP-71 inherits the wheel-tagging discipline.

## SWIG (multi-language wrapper generator)

SWIG generates language bindings from C/C++ headers across ~30 target languages. Long history, broad coverage, but the user writes interface files (`*.i`) describing the C surface.

What MEP-71 borrows:

- **Interface-file mental model.** The synthesised shim.mochi is the MEP-71 analogue of a SWIG `.i` file: a generated bridge surface between two languages.

What MEP-71 rejects:

- **Interface file as user-edited artifact.** SWIG users edit their `.i` files; MEP-71 users do not edit shim.mochi. Custom overrides live in `<modname>_externs.py` sidecars (the MEP-51 Phase 12 pattern).

## JPype, Py4J (Java → Python, Python → Java)

JPype and Py4J bridge Java and Python. JPype runs both in the same process via JNI; Py4J runs them in separate processes communicating over a socket.

What MEP-71 borrows:

- **JPype's in-process model.** MEP-71's embedded runtime mode (default) links libpython into the Mochi binary. JPype's JNI-based approach is the analogue.
- **Py4J's subprocess fallback.** When in-process linking is impossible (sandboxed deployments, certain platforms), MEP-71's `[python].runtime-mode = "subprocess"` shells out. The subprocess protocol is simpler than Py4J's (JSON-RPC over stdin/stdout vs Py4J's bidirectional socket protocol).

## gopy (Go → Python)

gopy generates Python bindings from Go packages. Closest prior art to MEP-71's direction (Mochi runs on Go). gopy uses `cgo` to expose Go functions to CPython.

What worked when it was maintained: Go-side type annotations were translated to Python type stubs. The CPython API was called via cgo from generated C glue.

What didn't: gopy has not had a release since 2023. The CPython API churn (especially the 3.12 deprecations of `Py_SetProgramName`, `PyEval_InitThreads`, etc.) has not been tracked. Free-threaded Python (3.13t) is not supported. abi3 is not supported.

What MEP-71 borrows:

- **Go-side type annotation extraction.** gopy parsed Go's AST and used the annotations to drive type translation. MEP-71's Mochi-side equivalent reads the Mochi type checker's output to drive the wrapper module synthesis.

What MEP-71 rejects:

- **Direct cgo embedding.** gopy uses cgo to call libpython from Go. MEP-71 uses a synthesised Python wrapper module that lives on the CPython side; the Go↔CPython boundary stays smaller and more stable.

## UniFFI (Mozilla, multi-target from Rust)

UniFFI generates Swift, Kotlin, Python, and Ruby bindings from Rust crates. The user writes a `.udl` IDL describing the Rust surface; UniFFI generates the foreign-language bindings.

What MEP-71 borrows:

- **Closed type table.** UniFFI's UDL has a closed set of types; user-defined types outside the set are opaque. MEP-71's closed type-mapping table is the same discipline.

What MEP-71 rejects:

- **Separate IDL.** UniFFI's UDL is a separate language users must learn. MEP-71's type discovery is driven by PEP 561 stubs, which the Python ecosystem already produces.

## diplomat (Rust → Swift/JS/Dart)

diplomat is similar to UniFFI but uses Rust source as the IDL: `#[diplomat::bridge]` attribute macros annotate the Rust source, and bindings are generated from the annotated subset.

What MEP-71 borrows:

- **Source-as-IDL discipline.** The bridge does not introduce a new language; the source language's own type system is the contract surface. PEP 561 stubs are MEP-71's source-as-IDL.

## Summary table

| Bridge | Source | Targets | What MEP-71 borrows |
|--------|--------|---------|---------------------|
| PyO3 | Rust | Python | Wrapper-module pattern, abi3 strategy |
| maturin | Rust crate | Python wheel | PEP 517 backend, auditwheel discipline |
| Cython | Python/C hybrid | C extension | Generated `.pyx`-style wrapper module |
| CFFI | Python | C ABI | (informative; not chosen as primary) |
| pybind11 / nanobind | C++ | Python | Type-caster pattern, abi3 support |
| SWIG | C/C++ headers | 30 languages | Interface-file mental model (generated, not edited) |
| JPype | Java in-process | Python | In-process embedded mode |
| Py4J | Java subprocess | Python | Subprocess fallback mode |
| gopy | Go | Python | Type-annotation extraction (but not the cgo embedding) |
| UniFFI | Rust + UDL | Swift/Kotlin/Python/Ruby | Closed type table |
| diplomat | Rust source | Swift/JS/Dart | Source-as-IDL discipline |

## What no prior art covers

Three areas where MEP-71 has no template to follow:

1. **Stub-discovery as the type source.** PyO3 / pybind11 / gopy all assume the source language has types and Python doesn't. MEP-71 inherits Mochi's types and Python's types simultaneously; the PEP 561 ladder is novel as a bridge type-source.
2. **uv-resolver-driven lockfile integration.** MEP-71 piggybacks on uv's PubGrub forking resolver and writes lockfile entries into mochi.lock. No prior bridge does resolver-level lockfile integration with a third-party resolver.
3. **PEP 740 attestation generation.** PEP 740 is new (PyPI 2024-Q4). PyO3/maturin support trusted publishing for upload but not yet attestation generation. MEP-71 generates attestations as part of the publish flow from the start.

## Cross-references

- [[02-design-philosophy]] for the rationale behind the chosen patterns.
- [[04-pep561-stub-ingest]] for the stub-discovery pipeline (the novel piece).
- [[05-type-mapping]] for the closed type table (UniFFI-style).
- [[06-pypi-publish-flow]] for the maturin-equivalent PEP 517 backend.
- [[07-sigstore-pypi-trusted-publishing]] for the PEP 740 attestation generation (the other novel piece).
