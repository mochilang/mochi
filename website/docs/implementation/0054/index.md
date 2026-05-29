---
title: MEP-54 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 54. Mochi-to-Go transpiler"
description: "Per-phase implementation tracking for MEP-54 (Mochi-to-Go transpiler). Status + commit columns capture how each phase landed on main."
---

# MEP-54 implementation tracking

Per-phase tracking for [MEP-54 Mochi-to-Go transpiler](/docs/mep/mep-0054). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every Go target listed for it in MEP-54 §Phase plan.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: gotree / lower / emit / build / runtime package | LANDED | see PR #22485 | [phase-00-skeleton](/docs/implementation/0054/phase-00-skeleton) |
| 1 | Hello world | LANDED | see PR #22485 | [phase-01-hello](/docs/implementation/0054/phase-01-hello) |
| 2 | Scalars, arithmetic, control flow, string helpers | LANDED | see PR #22485 | [phase-02-scalars](/docs/implementation/0054/phase-02-scalars) |
| 3 | Lists, maps, sets | NOT STARTED | — | [phase-03-collections](/docs/implementation/0054/phase-03-collections) |
| 4 | Records (structs) | NOT STARTED | — | [phase-04-records](/docs/implementation/0054/phase-04-records) |
| 5 | Sum types (tagged struct union) | NOT STARTED | — | [phase-05-sums](/docs/implementation/0054/phase-05-sums) |
| 6 | Closures (function literals) | NOT STARTED | — | [phase-06-closures](/docs/implementation/0054/phase-06-closures) |
| 7 | Query DSL (range + filter + map pipeline) | NOT STARTED | — | [phase-07-query](/docs/implementation/0054/phase-07-query) |
| 8 | Datalog (compile-time semi-naive eval) | NOT STARTED | — | [phase-08-datalog](/docs/implementation/0054/phase-08-datalog) |
| 9 | Agents (goroutine + channel mailbox) | NOT STARTED | — | [phase-09-agents](/docs/implementation/0054/phase-09-agents) |
| 10 | Streams (fan-out broadcast chan) | NOT STARTED | — | [phase-10-streams](/docs/implementation/0054/phase-10-streams) |
| 11 | Async/await (goroutine + chan rendezvous) | NOT STARTED | — | [phase-11-async](/docs/implementation/0054/phase-11-async) |
| 12 | FFI via CGo | NOT STARTED | — | [phase-12-ffi](/docs/implementation/0054/phase-12-ffi) |
| 13 | LLM (cassette replay) | NOT STARTED | — | [phase-13-llm](/docs/implementation/0054/phase-13-llm) |
| 14 | HTTP fetch (net/http) | NOT STARTED | — | [phase-14-fetch](/docs/implementation/0054/phase-14-fetch) |
| 15 | go module publish (pkg.go.dev) | NOT STARTED | — | [phase-15-publish](/docs/implementation/0054/phase-15-publish) |
| 16 | Reproducible build (-trimpath SHA-256) | NOT STARTED | — | [phase-16-repro](/docs/implementation/0054/phase-16-repro) |
| 17 | Cross-compile matrix (5 GOOS/GOARCH) | NOT STARTED | — | [phase-17-cross](/docs/implementation/0054/phase-17-cross) |
| 18 | Wasm (GOOS=wasip1 GOARCH=wasm) | NOT STARTED | — | [phase-18-wasm](/docs/implementation/0054/phase-18-wasm) |
