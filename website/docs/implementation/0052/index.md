---
title: MEP-52 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 52. Mochi-to-TypeScript transpiler"
description: "Per-phase implementation tracking for MEP-52 (Mochi-to-TypeScript/JavaScript transpiler for TS 5.6 strict + ES2024 + Node 22 LTS + Deno 2 + Bun 1.1 + browser, AsyncIterableQueue + AbortController agents, npm + tsc canonical, JSR + esbuild secondary, npm Trusted Publishing). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-52 implementation tracking

Per-phase tracking for [MEP-52 Mochi-to-TypeScript transpiler](/docs/mep/mep-0052). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-52 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                                                   | Tracking page                                                                | Status      | Commit |
|-------|-------------------------------------------------------------------------|------------------------------------------------------------------------------|-------------|--------|
| 1     | Hello world                                                             | [phase-01-hello](/docs/implementation/0052/phase-01-hello)                   | LANDED (Node + Deno + Bun) | (this PR) |
| 2     | Scalars (int via bigint/number, float, bool, string)                    | [phase-02-scalars](/docs/implementation/0052/phase-02-scalars)               | LANDED (Node + Deno + Bun); 2.1 ships `number` (bigint deferred), 2.5 deferred to Phase 3 | (this PR) |
| 3.1   | Lists (T[] with guarded index reads, scalar element types)              | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | LANDED (Node + Deno + Bun); readonly view + bigint deferred | (this PR) |
| 3.2   | Maps (Map\<K, V\>)                                                      | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | LANDED (Node + Deno + Bun); bigint keys + Option-return + delete + tuple-iter deferred | (this PR) |
| 3.3   | Sets (Set\<T\>; literal, has, add, len, for-each)                       | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | LANDED (Node + Deno + Bun); ES2024 set operators + set-typed fn params/returns deferred | (this PR) |
| 3.4   | List of records                                                         | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | LANDED (Node + Deno + Bun); record methods + comprehensions + deep equality deferred (methods + comprehensions to Phase 7, deep equality to Phase 4) | (this PR) |
| 4.0   | Record class shape (private ctor + readonly fields + static of())       | [phase-04-records](/docs/implementation/0052/phase-04-records)               | LANDED (inherited from Phase 3.4) | (Phase 3.4 PR) |
| 4.1   | Record methods                                                          | [phase-04-records](/docs/implementation/0052/phase-04-records)               | DEFERRED (vm3 bug in method bodies) | n/a    |
| 4.2   | Record structural equality (`mochi_eq_<R>` helper)                      | [phase-04-records](/docs/implementation/0052/phase-04-records)               | LANDED (Node + Deno + Bun); nested-record + list-field + map-field equality deferred | (this PR) |
| 4.3   | Multi-file module layout                                                | [phase-04-records](/docs/implementation/0052/phase-04-records)               | DEFERRED (single-file emit works for Phase 15) | n/a    |
| 4.4   | Identifier mangling for reserved-word collisions                        | [phase-04-records](/docs/implementation/0052/phase-04-records)               | DEFERRED (no fixture surfaces a collision) | n/a    |
| 5     | Sum types (discriminated union)                                         | [phase-05-sums](/docs/implementation/0052/phase-05-sums)                     | LANDED (Node + Deno + Bun); 5.3 (pattern guards), 5.4 (record-payload variants) deferred | (this PR) |
| 6     | Closures and higher-order functions                                     | [phase-06-closures](/docs/implementation/0052/phase-06-closures)             | LANDED (Node + Deno + Bun); 6.5 mutable capture, 6.6 multi-level capture, 6.7 void-return closures deferred | (this PR) |
| 7     | Query DSL (Iterator helpers + AsyncIterable)                            | [phase-07-query](/docs/implementation/0052/phase-07-query)                   | NOT STARTED | n/a    |
| 8     | Datalog                                                                 | [phase-08-datalog](/docs/implementation/0052/phase-08-datalog)               | NOT STARTED | n/a    |
| 9     | Agents (AsyncIterableQueue + AbortController)                           | [phase-09-agents](/docs/implementation/0052/phase-09-agents)                 | NOT STARTED | n/a    |
| 10    | Streams (AsyncIterable)                                                 | [phase-10-streams](/docs/implementation/0052/phase-10-streams)               | NOT STARTED | n/a    |
| 11    | async coloring, MochiResult, AggregateError                             | [phase-11-async](/docs/implementation/0052/phase-11-async)                   | NOT STARTED | n/a    |
| 12    | FFI (Node N-API + Deno FFI + Bun FFI dispatch)                          | [phase-12-ffi](/docs/implementation/0052/phase-12-ffi)                       | NOT STARTED | n/a    |
| 13    | LLM (provider dispatch)                                                 | [phase-13-llm](/docs/implementation/0052/phase-13-llm)                       | NOT STARTED | n/a    |
| 14    | fetch (built-in fetch on Node 18+, Deno, Bun, browser)                  | [phase-14-fetch](/docs/implementation/0052/phase-14-fetch)                   | NOT STARTED | n/a    |
| 15    | npm package build via tsc + npm pack                                    | [phase-15-npm-package](/docs/implementation/0052/phase-15-npm-package)       | NOT STARTED | n/a    |
| 16    | Reproducible build (SOURCE_DATE_EPOCH + sorted tar)                     | [phase-16-repro](/docs/implementation/0052/phase-16-repro)                   | NOT STARTED | n/a    |
| 17    | Deno JSR publish + Jupyter (Deno kernel) + browser bundle (esbuild)     | [phase-17-jsr-jupyter-browser](/docs/implementation/0052/phase-17-jsr-jupyter-browser) | NOT STARTED | n/a    |
| 18    | npm Trusted Publishing (Sigstore + OIDC + provenance)                   | [phase-18-trusted-publishing](/docs/implementation/0052/phase-18-trusted-publishing) | NOT STARTED | n/a    |
