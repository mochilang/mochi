---
title: MEP-51 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 51. Mochi-to-Python transpiler"
description: "Per-phase implementation tracking for MEP-51 (Mochi-to-Python transpiler for CPython 3.12+, mypy + pyright strict, asyncio.Queue + TaskGroup, uv + hatchling, Jupyter ipykernel). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-51 implementation tracking

Per-phase tracking for [MEP-51 Mochi-to-Python transpiler](/docs/mep/mep-0051). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-51 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                                | Status      | Commit |
|-------|------------------------------------------------------|-------------|--------|
| 1     | Hello world                                          | NOT STARTED | n/a    |
| 2     | Scalars (int, float, bool, str, bytes)               | NOT STARTED | n/a    |
| 3.1   | Lists                                                | NOT STARTED | n/a    |
| 3.2   | Maps (dict)                                          | NOT STARTED | n/a    |
| 3.3   | Sets (OrderedSet wrapper)                            | NOT STARTED | n/a    |
| 3.4   | List of records                                      | NOT STARTED | n/a    |
| 4     | Records (frozen + slots dataclass)                   | NOT STARTED | n/a    |
| 5     | Sum types (PEP 695 + dataclass variants)             | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions                  | NOT STARTED | n/a    |
| 7     | Query DSL (gen-expr + itertools + AsyncIterator)     | NOT STARTED | n/a    |
| 8     | Datalog                                              | NOT STARTED | n/a    |
| 9     | Agents (asyncio.Queue + TaskGroup)                   | NOT STARTED | n/a    |
| 10    | Streams (AsyncIterator)                              | NOT STARTED | n/a    |
| 11    | async coloring, MochiResult, ExceptionGroup          | NOT STARTED | n/a    |
| 12    | FFI (ctypes + CFFI)                                  | NOT STARTED | n/a    |
| 13    | LLM (provider dispatch)                              | NOT STARTED | n/a    |
| 14    | fetch (httpx)                                        | NOT STARTED | n/a    |
| 15    | Wheel + sdist build via uv                           | NOT STARTED | n/a    |
| 16    | Reproducible build (SOURCE_DATE_EPOCH)               | NOT STARTED | n/a    |
| 17    | Jupyter ipykernel                                    | NOT STARTED | n/a    |
| 18    | PyPI Trusted Publishing (OIDC + sigstore + PEP 740)  | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
