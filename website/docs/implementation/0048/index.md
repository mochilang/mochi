---
title: MEP-48 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 48. Mochi-to-.NET transpiler"
description: "Per-phase implementation tracking for MEP-48 (Mochi-to-.NET/CLR transpiler). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-48 implementation tracking

Per-phase tracking for [MEP-48 Mochi-to-.NET transpiler](/docs/mep/mep-0048). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-48 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                            | Status      | Commit |
|-------|--------------------------------------------------|-------------|--------|
| 0     | Spec freeze and skeleton trees                   | NOT STARTED | n/a    |
| 1     | Hello world                                      | NOT STARTED | n/a    |
| 2     | Primitives and control flow                      | NOT STARTED | n/a    |
| 3     | Collections                                      | NOT STARTED | n/a    |
| 4     | Records                                          | NOT STARTED | n/a    |
| 5     | Sum types and pattern matching                   | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions              | NOT STARTED | n/a    |
| 7     | Query DSL (LINQ / PLINQ)                         | NOT STARTED | n/a    |
| 8     | Datalog                                          | NOT STARTED | n/a    |
| 9     | Agents (Channels-backed)                         | NOT STARTED | n/a    |
| 10    | Streams (IAsyncEnumerable)                       | NOT STARTED | n/a    |
| 11    | async (Task-based)                               | NOT STARTED | n/a    |
| 12    | FFI (P/Invoke)                                   | NOT STARTED | n/a    |
| 13    | LLM (generate)                                   | NOT STARTED | n/a    |
| 14    | fetch (HTTP)                                     | NOT STARTED | n/a    |
| 15    | Release packaging (NuGet)                        | NOT STARTED | n/a    |
| 16    | Reproducibility and perf                         | NOT STARTED | n/a    |
| 17    | NativeAOT                                        | NOT STARTED | n/a    |
| 18    | v1.0 release                                     | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
