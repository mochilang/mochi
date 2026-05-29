---
title: "MEP-71 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-71: language surface, design philosophy, prior-art Python bridges, PEP 561 stub ingest, the closed type-mapping table, the PyPI publish flow, PyPI Trusted Publishing + PEP 740 attestations, the async (asyncio) bridge, CPython ABI stability and abi3, the GIL and C-extension boundary, the Pyodide / WASI / embedded subset, plus the risks and rejected alternatives register."
---

# MEP-71 research bundle

This bundle is the informative companion to [MEP-71](/docs/mep/mep-0071). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import python "..."` import shape, the `mochi.toml` `[python-dependencies]` + `[python]` tables, the CLI surface (`mochi pkg add python`, `mochi pkg publish --to=pypi`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why PEP 561 stubs over alternatives, why a synthesised CPython wrapper module over direct ctypes, why the async bridge sits on asyncio.run, why Sigstore-keyless + PyPI Trusted Publishing is the only publish path. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | PyO3, maturin, Cython, CFFI, pybind11, nanobind, SWIG, JPype, Py4J, gopy, UniFFI, diplomat. What each gets right, what each requires the user to write, and what MEP-71 borrows. |
| [04. PEP 561 stub ingest](04-pep561-stub-ingest.md) | The PEP 561 four-tier precedence (`py.typed` inline → `<name>-stubs` sibling → typeshed → stubgen fallback), the stubgen inspect mode, the partial-stub story, the Python-side parser shape. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic resolution rule, the `int` arbitrary precision boundary, the `Optional` and `Union` desugar, dataclass and TypedDict handling. |
| [06. PyPI publish flow](06-pypi-publish-flow.md) | The PyPI upload protocol (PEP 700 / 691 / 503), the per-package metadata requirements (PEP 621 / 643 core metadata 2.4), the sdist / wheel format (PEP 517 / 518 / 425 / 600 / 656), the publish-side gate. |
| [07. Sigstore and PyPI Trusted Publishing](07-sigstore-pypi-trusted-publishing.md) | The OIDC token exchange, the Fulcio short-lived cert, the Rekor transparency log, PEP 740 attestations, the verification path at install time, the PyPI Trusted Publishing GA timeline. |
| [08. Async bridge](08-async-bridge.md) | The asyncio event-loop model, per-call `asyncio.run` vs persistent loop choice, the cross-loop hazard, the `await` ceremony, cancellation semantics, the uvloop / trio incompatibility surface. |
| [09. ABI stability](09-abi-stability.md) | CPython API/ABI versioning (PEP 387 / 802 / 809), the abi3 stable ABI (PEP 384), the free-threaded ABI tag (PEP 779 / 803), the manylinux / musllinux wheel platform tags (PEP 600 / 656), the cdylib boundary. |
| [10. GIL and C extensions](10-gil-and-cextensions.md) | The GIL acquisition model, free-threaded CPython 3.13t / 3.14t (PEP 703), foreign C-extension boundary safety, fork-safety, sub-interpreters (PEP 684 / 734), import-time side-effects in the wild. |
| [11. Pyodide, WASI, and embedded](11-pyodide-wasi-embedded.md) | The Pyodide / wasm32-emscripten target, WASI Preview 2 (`wasm32-wasip2`) component model, the no-CPython subset, embedded MEP-53 target, what kind of Python packages Mochi can consume in the browser and on bare metal. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (PEP 561 coverage, wrapper compile time, dynamic typing leakage, GIL contention, capability drift, abi2026 transition) and the rejected alternatives (direct ctypes, cffi primary, cython fork, embedded micropython only, lifetime-erased dataclasses, long-lived OIDC token, sub-interpreter default, per-build venv). |

## Cross-references

- [MEP-71 spec](/docs/mep/mep-0071) — the normative document.
- [MEP-51](/docs/mep/mep-0051) — the Python transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the parallel Rust bridge that shares the polyglot package infrastructure.
- [Implementation tracking](/docs/implementation/0071/) — the per-phase delivery status.
