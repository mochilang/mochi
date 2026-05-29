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

| Phase | Title                                                                                  | Status      | Commit |
|-------|----------------------------------------------------------------------------------------|-------------|--------|
| 1     | [Hello world](./phase-01-hello.md)                                                     | NOT STARTED | n/a    |
| 2     | [Scalars (int, float, bool, str, bytes)](./phase-02-scalars.md)                        | NOT STARTED | n/a    |
| 3.1   | [Lists](./phase-03-1-lists.md)                                                         | NOT STARTED | n/a    |
| 3.2   | [Maps (dict)](./phase-03-2-maps.md)                                                    | NOT STARTED | n/a    |
| 3.3   | [Sets (OrderedSet wrapper)](./phase-03-3-sets.md)                                      | NOT STARTED | n/a    |
| 3.4   | [List of records](./phase-03-4-list-of-records.md)                                     | NOT STARTED | n/a    |
| 4     | [Records (frozen + slots dataclass)](./phase-04-records.md)                            | NOT STARTED | n/a    |
| 5     | [Sum types (PEP 695 + dataclass variants)](./phase-05-sums.md)                         | NOT STARTED | n/a    |
| 6     | [Closures and higher-order functions](./phase-06-closures.md)                          | NOT STARTED | n/a    |
| 7     | [Query DSL (gen-expr + itertools + AsyncIterator)](./phase-07-query.md)                | NOT STARTED | n/a    |
| 8     | [Datalog](./phase-08-datalog.md)                                                       | NOT STARTED | n/a    |
| 9     | [Agents (asyncio.Queue + TaskGroup)](./phase-09-agents.md)                             | NOT STARTED | n/a    |
| 10    | [Streams (AsyncIterator)](./phase-10-streams.md)                                       | NOT STARTED | n/a    |
| 11    | [async coloring, MochiResult, ExceptionGroup](./phase-11-async.md)                     | NOT STARTED | n/a    |
| 12    | [FFI (ctypes + CFFI)](./phase-12-ffi.md)                                               | NOT STARTED | n/a    |
| 13    | [LLM (provider dispatch)](./phase-13-llm.md)                                           | NOT STARTED | n/a    |
| 14    | [fetch (httpx)](./phase-14-fetch.md)                                                   | NOT STARTED | n/a    |
| 15    | [Wheel + sdist build via uv](./phase-15-wheel-sdist.md)                                | NOT STARTED | n/a    |
| 16    | [Reproducible build (SOURCE_DATE_EPOCH)](./phase-16-repro.md)                          | NOT STARTED | n/a    |
| 17    | [Jupyter ipykernel](./phase-17-ipykernel.md)                                           | NOT STARTED | n/a    |
| 18    | [PyPI Trusted Publishing (OIDC + sigstore + PEP 740)](./phase-18-pypi-publish.md)      | NOT STARTED | n/a    |
