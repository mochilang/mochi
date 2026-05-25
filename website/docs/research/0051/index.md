---
title: "MEP-51 research bundle: Mochi to Python transpiler"
description: "Twelve research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, target portability, dataset pipeline, agent and stream lowering, build system, testing gates, and risks for the Mochi-to-Python (CPython 3.12+) transpiler proposed in MEP-51."
sidebar_position: 51
sidebar_label: "MEP-51"
---

# MEP-51 research bundle: Mochi to Python transpiler

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-51, the Mochi-to-Python transpiler](/docs/mep/mep-0051). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0051/language-surface) | Mochi language surface mapped onto CPython 3.12 lowering obligations |
| 02 | [Design philosophy](/docs/research/0051/design-philosophy) | Why Python, why CPython 3.12 floor, why dual mypy + pyright strict gates, why asyncio.Queue + TaskGroup over Trio / AnyIO / actor frameworks |
| 03 | [Prior art](/docs/research/0051/prior-art-transpilers) | Survey of source-to-Python tooling: 2to3, libcst, Cython, mypyc, Nuitka, Brython, Transcrypt, RustPython, Jython, IronPython, GraalPy, Mojo, LPython, Codon, pytype, pyrefly |
| 04 | [Runtime](/docs/research/0051/runtime) | The `mochi_runtime` PyPI package: stdlib + httpx + agent supervisor + Datalog evaluator + JSONValue + MochiResult + ZonedDateTime + ctypes FFI |
| 05 | [Codegen design](/docs/research/0051/codegen-design) | IR-to-Python lowering via Go-side AST + `ast.unparse` + ruff format, monomorphisation, closure conversion, match-to-decision-tree, source maps |
| 06 | [Type lowering](/docs/research/0051/type-lowering) | Per-type details for every Mochi type onto Python 3.12 (`int`, `float`, `str`, `list`, `dict`, `OrderedSet`, frozen-slots `@dataclass`, PEP 695 sum types, `T | None`, `Ok | Err`, `Callable`, `AsyncIterator`) |
| 07 | [Target portability](/docs/research/0051/python-target-portability) | CPython 3.12.0 / 3.12.7 / 3.13.0 matrix x linux/macos/windows x x86_64/arm64, wheel platform tags, abi3 stance, uv-managed Python, v1 exclusions (PyPy, Cython, mypyc, Nuitka, Pyodide) |
| 08 | [Dataset pipeline](/docs/research/0051/dataset-pipeline) | Query DSL lowering via generator expressions + `itertools` + `AsyncIterator`, hash/merge/nested-loop joins, Datalog semi-naive evaluation, deliberate rejection of pandas / polars / DuckDB |
| 09 | [Agents and streams](/docs/research/0051/agent-streams) | Mochi agents as a custom class wrapping `asyncio.Queue[Message]` + `TaskGroup` supervision (PEP 654 ExceptionGroup), `AsyncIterator[T]` streams, cold/hot patterns, GIL story |
| 10 | [Build system](/docs/research/0051/build-system) | `pyproject.toml` (PEP 621) + hatchling backend + uv 0.4+ as canonical driver, wheel + sdist, PyPI Trusted Publishing (OIDC + sigstore + PEP 740), Jupyter ipykernel target |
| 11 | [Testing gates](/docs/research/0051/testing-gates) | Per-phase Go test gates, CPython 3.12.0 / 3.13.0 matrix, mypy 1.13 + pyright 1.1.380 strict, ruff fixed-point, wheel build+install smoke test, ~400-fixture corpus by Phase 18 |
| 12 | [Risks and alternatives](/docs/research/0051/risks-and-alternatives) | 15 risks + 6 rejected alternatives (Cython, mypyc, Nuitka, Trio, Pydantic-records, Py2-compat) + 4 future candidates (3.13 free-threaded GIL, PyPy abi3, Pyodide, mypyc compile pass) |

## Cross-references

- [MEP-45 (C target)](/docs/mep/mep-0045)
- [MEP-46 (BEAM target)](/docs/mep/mep-0046)
- [MEP-47 (JVM target, direct bytecode)](/docs/mep/mep-0047)
- [MEP-48 (.NET target)](/docs/mep/mep-0048)
- [MEP-49 (Swift target)](/docs/mep/mep-0049)
- [MEP-50 (Kotlin target)](/docs/mep/mep-0050)
