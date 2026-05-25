---
title: MEP-46 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 46. Mochi-to-BEAM transpiler"
description: "Per-phase implementation tracking for MEP-46 (Mochi-to-BEAM transpiler). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-46 implementation tracking

Per-phase tracking for [MEP-46 Mochi-to-BEAM transpiler](/docs/mep/mep-0046). Each phase has its own page; this index gives the at-a-glance view. Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-46 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                         | Status      | Commit | Tracking page |
|-------|-----------------------------------------------|-------------|--------|---------------|
| 0     | Spec freeze and skeleton trees                | NOT STARTED | n/a    | [phase-00](/docs/implementation/0046/phase-00-skeleton) |
| 1     | Hello world                                   | NOT STARTED | n/a    | [phase-01](/docs/implementation/0046/phase-01-hello) |
| 2     | Primitives and control flow                   | NOT STARTED | n/a    | [phase-02](/docs/implementation/0046/phase-02-primitives) |
| 3     | Collections (lists, maps, sets, omaps)        | NOT STARTED | n/a    | [phase-03](/docs/implementation/0046/phase-03-collections) |
| 4     | Records                                       | NOT STARTED | n/a    | [phase-04](/docs/implementation/0046/phase-04-records) |
| 5     | Sum types and pattern matching                | NOT STARTED | n/a    | [phase-05](/docs/implementation/0046/phase-05-sums) |
| 6     | Closures and higher-order functions           | NOT STARTED | n/a    | [phase-06](/docs/implementation/0046/phase-06-closures) |
| 7     | Query DSL                                     | NOT STARTED | n/a    | [phase-07](/docs/implementation/0046/phase-07-query) |
| 8     | Datalog                                       | NOT STARTED | n/a    | [phase-08](/docs/implementation/0046/phase-08-datalog) |
| 9     | Agents and gen_server                         | NOT STARTED | n/a    | [phase-09](/docs/implementation/0046/phase-09-agents) |
| 10    | Streams and pubsub                            | NOT STARTED | n/a    | [phase-10](/docs/implementation/0046/phase-10-streams) |
| 11    | async/await                                   | NOT STARTED | n/a    | [phase-11](/docs/implementation/0046/phase-11-async) |
| 12    | FFI                                           | NOT STARTED | n/a    | [phase-12](/docs/implementation/0046/phase-12-ffi) |
| 13    | LLM (generate)                                | NOT STARTED | n/a    | [phase-13](/docs/implementation/0046/phase-13-llm) |
| 14    | fetch (HTTP)                                  | NOT STARTED | n/a    | [phase-14](/docs/implementation/0046/phase-14-fetch) |
| 15    | Release packaging                             | NOT STARTED | n/a    | [phase-15](/docs/implementation/0046/phase-15-release) |
| 16    | Multi-OTP-version matrix                      | NOT STARTED | n/a    | [phase-16](/docs/implementation/0046/phase-16-otp-matrix) |
| 17    | Dialyzer cleanliness                          | NOT STARTED | n/a    | [phase-17](/docs/implementation/0046/phase-17-dialyzer) |
| 18    | Reproducibility and perf                      | NOT STARTED | n/a    | [phase-18](/docs/implementation/0046/phase-18-repro-perf) |
| 19    | v1.0 release                                  | NOT STARTED | n/a    | [phase-19](/docs/implementation/0046/phase-19-release) |

Each phase page records the gate, the goal-alignment audit, the sub-phase breakdown, decisions made, the test set, and the closeout notes once gate-green.
