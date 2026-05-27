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

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton | LANDED | — | [phase-00](/docs/implementation/0047/phase-00-skeleton) |
| 1 | Hello world | LANDED | — | [phase-01](/docs/implementation/0047/phase-01-hello) |
| 2 | Primitives and control flow | LANDED | — | [phase-02](/docs/implementation/0047/phase-02-scalars) |
| 3 | Collections | LANDED | — | [phase-03](/docs/implementation/0047/phase-03-collections) |
| 4 | Records | LANDED | — | [phase-04](/docs/implementation/0047/phase-04-records) |
| 5 | Sum types and pattern matching | LANDED | — | [phase-05](/docs/implementation/0047/phase-05-sums) |
| 6 | Closures and higher-order functions | LANDED | — | [phase-06](/docs/implementation/0047/phase-06-closures) |
| 7 | Query DSL | LANDED | — | [phase-07](/docs/implementation/0047/phase-07-query) |
| 8 | Datalog | LANDED | — | [phase-08](/docs/implementation/0047/phase-08-datalog) |
| 9 | Agents (virtual threads, Loom) | LANDED | — | [phase-09](/docs/implementation/0047/phase-09-agents) |
| 10 | Streams | LANDED | — | [phase-10](/docs/implementation/0047/phase-10-streams) |
| 11 | async (Loom-backed) | LANDED | — | [phase-11](/docs/implementation/0047/phase-11-async) |
| 12 | FFI (JVM interop) | LANDED | — | [phase-12](/docs/implementation/0047/phase-12-ffi) |
| 13 | LLM (generate) | LANDED | — | [phase-13](/docs/implementation/0047/phase-13-llm) |
| 14 | fetch (HTTP) | LANDED | — | [phase-14](/docs/implementation/0047/phase-14-fetch) |
| 15 | Release packaging | LANDED | — | [phase-15](/docs/implementation/0047/phase-15-packaging) |
| 16 | Native image (GraalVM) | LANDED | — | [phase-16](/docs/implementation/0047/phase-16-native-image) |
| 17 | Matrix and reproducibility | LANDED | — | [phase-17](/docs/implementation/0047/phase-17-matrix-repro) |
| 18 | Maven Central + v0.14.0 release | LANDED | — | [phase-18](/docs/implementation/0047/phase-18-maven-central) |
