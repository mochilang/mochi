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

| Phase | Title                                                                   | Status      | Commit |
|-------|-------------------------------------------------------------------------|-------------|--------|
| 1     | Hello world                                                             | NOT STARTED | n/a    |
| 2     | Scalars (int via bigint/number, float, bool, string)                    | NOT STARTED | n/a    |
| 3.1   | Lists (readonly T[] / T[])                                              | NOT STARTED | n/a    |
| 3.2   | Maps (Map<K, V>)                                                        | NOT STARTED | n/a    |
| 3.3   | Sets (Set<T> with ES2024 methods)                                       | NOT STARTED | n/a    |
| 3.4   | List of records                                                         | NOT STARTED | n/a    |
| 4     | Records (class with readonly fields + private ctor + static factory)    | NOT STARTED | n/a    |
| 5     | Sum types (discriminated union)                                         | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions                                     | NOT STARTED | n/a    |
| 7     | Query DSL (Iterator helpers + AsyncIterable)                            | NOT STARTED | n/a    |
| 8     | Datalog                                                                 | NOT STARTED | n/a    |
| 9     | Agents (AsyncIterableQueue + AbortController)                           | NOT STARTED | n/a    |
| 10    | Streams (AsyncIterable)                                                 | NOT STARTED | n/a    |
| 11    | async coloring, MochiResult, AggregateError                             | NOT STARTED | n/a    |
| 12    | FFI (Node N-API + Deno FFI + Bun FFI dispatch)                          | NOT STARTED | n/a    |
| 13    | LLM (provider dispatch)                                                 | NOT STARTED | n/a    |
| 14    | fetch (built-in fetch on Node 18+, Deno, Bun, browser)                  | NOT STARTED | n/a    |
| 15    | npm package build via tsc + npm pack                                    | NOT STARTED | n/a    |
| 16    | Reproducible build (SOURCE_DATE_EPOCH + sorted tar)                     | NOT STARTED | n/a    |
| 17    | Deno JSR publish + Jupyter (Deno kernel) + browser bundle (esbuild)     | NOT STARTED | n/a    |
| 18    | npm Trusted Publishing (Sigstore + OIDC + provenance)                   | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
