---
title: MEP-45 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 45. Mochi-to-C transpiler"
description: "Per-phase implementation tracking for MEP-45 (Mochi-to-C transpiler). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-45 implementation tracking

Per-phase tracking for [MEP-45 Mochi-to-C transpiler](/docs/mep/mep-0045). Each phase has its own page; this index gives the at-a-glance view. Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-45 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                      | Targets          | Status                              | Commit | Tracking page |
|-------|--------------------------------------------|------------------|-------------------------------------|--------|---------------|
| 0     | Spec freeze and skeleton trees             | n/a              | LANDED 2026-05-22 19:01 (GMT+7)     | —      | [phase-00](/docs/implementation/0045/phase-00-skeleton) |
| 1     | Hello world                                | host             | LANDED 2026-05-22 19:50 (GMT+7)     | —      | [phase-01](/docs/implementation/0045/phase-01-hello-world) |
| 2     | Primitives and control flow                | host             | LANDED                              | —      | [phase-02](/docs/implementation/0045/phase-02-primitives-control-flow) |
| 3     | Records, lists, maps, sets                 | host             | LANDED                              | —      | [phase-03](/docs/implementation/0045/phase-03-records-collections) |
| 4     | Sum types and Maranget pattern matching    | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-04](/docs/implementation/0045/phase-04-sum-types-match) |
| 5     | Closures and higher-order functions        | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-05](/docs/implementation/0045/phase-05-closures) |
| 6     | Strings and I/O                            | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-06](/docs/implementation/0045/phase-06-strings-io) |
| 7     | Error model                                | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-07](/docs/implementation/0045/phase-07-error-model) |
| 8     | Query DSL                                  | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-08](/docs/implementation/0045/phase-08-query-dsl) |
| 9     | Streams, agents, M:N scheduler             | host             | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-09](/docs/implementation/0045/phase-09-streams-agents) |
| 10    | FFI shells                                 | host             | COMPLETE 2026-05-26 08:58 (GMT+7)   | —      | [phase-10](/docs/implementation/0045/phase-10-ffi) |
| 11    | Cross-compile tier-1 matrix                | tier-1 ×8        | LANDED 2026-05-26 00:01 (GMT+7)     | —      | [phase-11](/docs/implementation/0045/phase-11-cross-tier1) |
| 12    | WASM / WASI                                | wasm32-wasi      | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-12](/docs/implementation/0045/phase-12-wasm-wasi) |
| 13    | APE / Cosmopolitan                         | linux+mac+win+BSD| COMPLETE                            | —      | [phase-13](/docs/implementation/0045/phase-13-ape) |
| 14    | LLM bindings                               | host             | COMPLETE 2026-05-26 07:41 (GMT+7)   | —      | [phase-14](/docs/implementation/0045/phase-14-llm) |
| 15    | Datalog / logic                            | host             | COMPLETE 2026-05-26 07:57 (GMT+7)   | —      | [phase-15](/docs/implementation/0045/phase-15-datalog) |
| 16    | Sanitiser matrix                           | linux+mac        | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-16](/docs/implementation/0045/phase-16-sanitisers) |
| 17    | Reproducibility gate                       | tier-1           | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-17](/docs/implementation/0045/phase-17-reproducibility) |
| 18    | Performance gate                           | linux+mac        | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-18](/docs/implementation/0045/phase-18-perf) |
| 19    | v1.0 release                               | tier-1           | COMPLETE 2026-05-26 09:06 (GMT+7)   | —      | [phase-19](/docs/implementation/0045/phase-19-release) |

## Rules

1. **Goal-alignment audit.** Before a phase starts, its tracking page gets a one-paragraph audit confirming the gate moves the user-facing goal ("ship a Mochi program as a single native binary on this target") not spec-internal scaffolding.
2. **Spec-in-sync.** The PR that lands a phase's code also updates [MEP-45](/docs/mep/mep-0045) (close out the phase block, update Status / Commit) and this tracking page.
3. **Umbrella phase coverage.** A phase is LANDED only when every target in its row is green. Missing rows become N.1, N.2, ... sub-phases listed on the phase tracking page.
4. **Auto-ship.** Each phase or sub-phase ships as a single PR with auto-merge enabled (`--merge`, not squash, per repo convention).
5. **Reference oracle.** Goldens are recorded by running the same source through vm3; the AOT binary's stdout is diffed against the golden. vm3 is the oracle only, never a code dependency.

## See also

- [MEP-45 §Phases](/docs/mep/mep-0045#phases): canonical phase definitions.
- [Research notes (substrate)](https://brain.tamnd.com/research/transpiler-c/): twelve notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, portability, dataset pipeline, streams, build system, testing, risks.
