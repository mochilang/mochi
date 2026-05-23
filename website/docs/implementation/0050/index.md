---
title: MEP-50 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 50. Mochi-to-Kotlin transpiler"
description: "Per-phase implementation tracking for MEP-50 (Mochi-to-Kotlin transpiler for Kotlin Multiplatform: JVM, Android, Kotlin/Native (iOS, macOS, Linux, Windows), Kotlin/JS, Kotlin/Wasm). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-50 implementation tracking

Per-phase tracking for [MEP-50 Mochi-to-Kotlin transpiler](/docs/mep/mep-0050). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-50 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                                | Status      | Commit |
|-------|------------------------------------------------------|-------------|--------|
| 1     | Hello world                                          | NOT STARTED | n/a    |
| 2     | Scalars                                              | NOT STARTED | n/a    |
| 3.1   | Lists                                                | NOT STARTED | n/a    |
| 3.2   | Maps (LinkedHashMap)                                 | NOT STARTED | n/a    |
| 3.3   | Sets (LinkedHashSet)                                 | NOT STARTED | n/a    |
| 3.4   | List of records                                      | NOT STARTED | n/a    |
| 4     | Records (data class)                                 | NOT STARTED | n/a    |
| 5     | Sum types and pattern matching (sealed interface)    | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions                  | NOT STARTED | n/a    |
| 7     | Query DSL (Sequence + Flow)                          | NOT STARTED | n/a    |
| 8     | Datalog                                              | NOT STARTED | n/a    |
| 9     | Agents (custom actor class + Channel)                | NOT STARTED | n/a    |
| 10    | Streams (Flow / SharedFlow / StateFlow)              | NOT STARTED | n/a    |
| 11    | suspend colouring, MochiResult                       | NOT STARTED | n/a    |
| 12    | FFI (JNI, cinterop, external, Wasm imports)          | NOT STARTED | n/a    |
| 13    | LLM (provider dispatch)                              | NOT STARTED | n/a    |
| 14    | fetch (Ktor client)                                  | NOT STARTED | n/a    |
| 15    | Android App Bundle (.aab via AGP 8.7+)               | NOT STARTED | n/a    |
| 16    | Reproducible build                                   | NOT STARTED | n/a    |
| 17    | Kotlin/Native single binaries                        | NOT STARTED | n/a    |
| 18    | Play Console pre-launch validation                   | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
