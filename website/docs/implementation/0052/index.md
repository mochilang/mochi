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
| 2     | Scalars (int via bigint/number, float, bool, string)                    | [phase-02-scalars](/docs/implementation/0052/phase-02-scalars)               | NOT STARTED | n/a    |
| 3.1   | Lists (readonly T[] / T[])                                              | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | NOT STARTED | n/a    |
| 3.2   | Maps (Map\<K, V\>)                                                      | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | NOT STARTED | n/a    |
| 3.3   | Sets (Set\<T\> with ES2024 methods)                                     | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | NOT STARTED | n/a    |
| 3.4   | List of records                                                         | [phase-03-collections](/docs/implementation/0052/phase-03-collections)       | NOT STARTED | n/a    |
| 4     | Records (class with readonly fields + private ctor + static factory)    | [phase-04-records](/docs/implementation/0052/phase-04-records)               | NOT STARTED | n/a    |
| 5     | Sum types (discriminated union)                                         | [phase-05-sums](/docs/implementation/0052/phase-05-sums)                     | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions                                     | [phase-06-closures](/docs/implementation/0052/phase-06-closures)             | NOT STARTED | n/a    |
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
