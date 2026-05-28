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
| 0 | Spec freeze and skeleton trees | LANDED | e12c666489 | [phase-00](/docs/implementation/0048/phase-00-skeleton) |
| 1 | Hello world | LANDED | 5064211521 | [phase-01](/docs/implementation/0048/phase-01-hello) |
| 2 | Primitives and control flow | LANDED | d0485c53a2 | [phase-02](/docs/implementation/0048/phase-02-scalars) |
| 3 | Collections | LANDED | 75cb560da3 | [phase-03](/docs/implementation/0048/phase-03-collections) |
| 4 | Records | LANDED | 3a4e5e6b86 | [phase-04](/docs/implementation/0048/phase-04-records) |
| 5 | Sum types and pattern matching | LANDED | 6ed3d343e3 | [phase-05](/docs/implementation/0048/phase-05-sums) |
| 6 | Closures and higher-order functions | LANDED | bdbc0789a6 | [phase-06](/docs/implementation/0048/phase-06-closures) |
| 7 | Query DSL (LINQ / PLINQ) | LANDED | 128f06fe23 | [phase-07](/docs/implementation/0048/phase-07-query) |
| 8 | Datalog | LANDED | 500259a3f1 | [phase-08](/docs/implementation/0048/phase-08-datalog) |
| 9 | Agents (Channel-backed) | LANDED | 500259a3f1 | [phase-09](/docs/implementation/0048/phase-09-agents) |
| 10 | Streams (IAsyncEnumerable) | LANDED | 3cb492549f | [phase-10](/docs/implementation/0048/phase-10-streams) |
| 11 | async/await and structured concurrency | LANDED | 4696545856 | [phase-11](/docs/implementation/0048/phase-11-async) |
| 12 | .NET FFI and NuGet deps | LANDED | 982cafcc31 | [phase-12](/docs/implementation/0048/phase-12-ffi) |
| 13 | LLM (generate) | LANDED | d1e0b9da42 | [phase-13](/docs/implementation/0048/phase-13-llm) |
| 14 | fetch (HTTP) | LANDED | 8221c3f26a | [phase-14](/docs/implementation/0048/phase-14-fetch) |
| 15 | NativeAOT packaging | LANDED | cb026c11d8 | [phase-15](/docs/implementation/0048/phase-15-native-aot) |
| 16 | Reproducibility | LANDED | cb026c11d8 | [phase-16](/docs/implementation/0048/phase-16-repro) |
| 17 | Self-contained packaging across RIDs | LANDED | cb026c11d8 | [phase-17](/docs/implementation/0048/phase-17-self-contained) |
| 18 | Trim cleanliness and NuGet publication | LANDED | cb026c11d8 | [phase-18](/docs/implementation/0048/phase-18-nuget-publish) |
