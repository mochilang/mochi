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

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Spec freeze and skeleton trees | NOT STARTED | n/a | [phase-00](/docs/implementation/0048/phase-00-skeleton) |
| 1 | Hello world | NOT STARTED | n/a | [phase-01](/docs/implementation/0048/phase-01-hello) |
| 2 | Primitives and control flow | NOT STARTED | n/a | [phase-02](/docs/implementation/0048/phase-02-scalars) |
| 3 | Collections | NOT STARTED | n/a | [phase-03](/docs/implementation/0048/phase-03-collections) |
| 4 | Records | NOT STARTED | n/a | [phase-04](/docs/implementation/0048/phase-04-records) |
| 5 | Sum types and pattern matching | NOT STARTED | n/a | [phase-05](/docs/implementation/0048/phase-05-sums) |
| 6 | Closures and higher-order functions | NOT STARTED | n/a | [phase-06](/docs/implementation/0048/phase-06-closures) |
| 7 | Query DSL (LINQ / PLINQ) | NOT STARTED | n/a | [phase-07](/docs/implementation/0048/phase-07-query) |
| 8 | Datalog | NOT STARTED | n/a | [phase-08](/docs/implementation/0048/phase-08-datalog) |
| 9 | Agents (Channel-backed) | NOT STARTED | n/a | [phase-09](/docs/implementation/0048/phase-09-agents) |
| 10 | Streams (IAsyncEnumerable) | NOT STARTED | n/a | [phase-10](/docs/implementation/0048/phase-10-streams) |
| 11 | async/await and structured concurrency | NOT STARTED | n/a | [phase-11](/docs/implementation/0048/phase-11-async) |
| 12 | .NET FFI and NuGet deps | NOT STARTED | n/a | [phase-12](/docs/implementation/0048/phase-12-ffi) |
| 13 | LLM (generate) | NOT STARTED | n/a | [phase-13](/docs/implementation/0048/phase-13-llm) |
| 14 | fetch (HTTP) | NOT STARTED | n/a | [phase-14](/docs/implementation/0048/phase-14-fetch) |
| 15 | NativeAOT packaging | NOT STARTED | n/a | [phase-15](/docs/implementation/0048/phase-15-native-aot) |
| 16 | Reproducibility | NOT STARTED | n/a | [phase-16](/docs/implementation/0048/phase-16-repro) |
| 17 | Self-contained packaging across RIDs | NOT STARTED | n/a | [phase-17](/docs/implementation/0048/phase-17-self-contained) |
| 18 | Trim cleanliness and NuGet publication | NOT STARTED | n/a | [phase-18](/docs/implementation/0048/phase-18-nuget-publish) |
