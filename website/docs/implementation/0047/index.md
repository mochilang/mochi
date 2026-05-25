---
title: MEP-47 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 47. Mochi-to-JVM transpiler"
description: "Per-phase implementation tracking for MEP-47 (Mochi-to-JVM transpiler). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-47 implementation tracking

Per-phase tracking for [MEP-47 Mochi-to-JVM transpiler](/docs/mep/mep-0047). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-47 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                        | Status      | Commit |
|-------|----------------------------------------------|-------------|--------|
| 0     | Spec freeze and skeleton trees               | NOT STARTED | n/a    |
| 1     | Hello world                                  | NOT STARTED | n/a    |
| 2     | Primitives and control flow                  | NOT STARTED | n/a    |
| 3     | Collections                                  | NOT STARTED | n/a    |
| 4     | Records                                      | NOT STARTED | n/a    |
| 5     | Sum types and pattern matching               | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions          | NOT STARTED | n/a    |
| 7     | Query DSL                                    | NOT STARTED | n/a    |
| 8     | Datalog                                      | NOT STARTED | n/a    |
| 9     | Agents (virtual threads, Loom)               | NOT STARTED | n/a    |
| 10    | Streams                                      | NOT STARTED | n/a    |
| 11    | async (Loom-backed)                          | NOT STARTED | n/a    |
| 12    | FFI (JNI / Panama)                           | NOT STARTED | n/a    |
| 13    | LLM (generate)                               | NOT STARTED | n/a    |
| 14    | fetch (HTTP)                                 | NOT STARTED | n/a    |
| 15    | Release packaging (Maven Central)            | NOT STARTED | n/a    |
| 16    | Reproducibility and perf                     | NOT STARTED | n/a    |
| 17    | Native-image / GraalVM                       | NOT STARTED | n/a    |
| 18    | v1.0 release                                 | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
