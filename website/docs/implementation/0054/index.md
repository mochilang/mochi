---
title: MEP-54 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 54. Mochi-to-Go transpiler"
description: "Per-phase implementation tracking for MEP-54 (Mochi-to-Go transpiler under transpiler3/go). Status + commit columns get filled in as sub-PRs land."
---

# MEP-54 implementation tracking

Per-phase tracking for the MEP-54 Mochi-to-Go transpiler under `transpiler3/go/`. Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for staged sub-phases, the in-branch commit on `worktree-mep54-impl`).

The pipeline is `parser.Parse` -> `types.Check` -> `transpiler3/c/lower.Lower` (shared aotir) -> `transpiler3/go/lower.Lower` -> `transpiler3/go/gotree` -> `go/format` -> `go build`. The aotir layer is shared with MEP-45/46/47/48/49/50/51/52/53/55/56, so each Go phase typically lights up the corresponding `aotir.*` node in the Go lowerer rather than re-deriving the typed IR.

A phase is LANDED only when its gate is green on `go test ./transpiler3/go/...` against every supported Go toolchain listed in the runtime matrix below. Missing target tuples become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: gotree / lower / emit / build / runtime | LANDED | b836c348fb | [phase-00](/docs/implementation/0054/phase-00-skeleton) |
| 1 | Hello world end-to-end | LANDED | b836c348fb | [phase-01](/docs/implementation/0054/phase-01-hello) |
| 2 | Scalars and arithmetic | LANDED | 20727c019e | [phase-02](/docs/implementation/0054/phase-02-scalars) |
| 3.1 | `list<T>` for scalar T | LANDED | 8bb154ea8f | [phase-03-1](/docs/implementation/0054/phase-03-1-lists) |
| 3.2 | `map<K, V>` for scalar K and V | LANDED | c86fd6a412 | [phase-03-2](/docs/implementation/0054/phase-03-2-maps) |
| 3.3 | `set<T>` for scalar T | LANDED | 87eed650a6 | [phase-03-3](/docs/implementation/0054/phase-03-3-sets) |
| 3.4 | `list<record>` + record declarations | LANDED | 6e189d3819 | [phase-03-4](/docs/implementation/0054/phase-03-4-list-of-records) |
| 4 | Record equality | LANDED | 17ccd7e15c | [phase-04](/docs/implementation/0054/phase-04-records) |
| 5 | Sum types + pattern matching | LANDED | 52e07d4998 | [phase-05](/docs/implementation/0054/phase-05-sums) |
| 6 | Top-level user functions | LANDED | 9f8c04e82e | [phase-06](/docs/implementation/0054/phase-06-functions) |
| 6.1 | Closures (FunLit + Captures) | LANDED | dcb6d58867 | [phase-06-1](/docs/implementation/0054/phase-06-1-closures) |
| 7.1 | Query DSL: filter + map | LANDED | f9270a397b | [phase-07-1](/docs/implementation/0054/phase-07-1-query-filter-map) |
| 7.2 | Query order_by + skip/take | LANDED | 5e2e396f95 | [phase-07-2](/docs/implementation/0054/phase-07-2-query-order-skip-take) |
| 7.3 | Query string ops (contains, len) | LANDED | 3510d3295d | [phase-07-3](/docs/implementation/0054/phase-07-3-query-string-ops) |
| 7.4 | Query joins (inner / cross / left) | LANDED | 2ec49d122f | [phase-07-4](/docs/implementation/0054/phase-07-4-query-joins) |
| 7.5 | List aggregations + arena_query | LANDED | cb2410a13d | [phase-07-5](/docs/implementation/0054/phase-07-5-list-aggregations) |
| 7.6 | String builtins (substring/index/reverse/split/join/str) | LANDED | 9e73065a33 | [phase-07-6](/docs/implementation/0054/phase-07-6-string-builtins) |
| 7.7 | Math builtins (abs/floor/ceil) | LANDED | 28fe55f223 | [phase-07-7](/docs/implementation/0054/phase-07-7-math-builtins) |
| 7.8 | File I/O (writeFile/appendFile/readFile/lines) | LANDED | 492a45a1c9 | [phase-07-8](/docs/implementation/0054/phase-07-8-file-io) |
| 7.9 | CSV I/O (loadCSV / saveCSV) | LANDED | 6e1f8dca4a | [phase-07-9](/docs/implementation/0054/phase-07-9-csv-io) |
| 7.10 | Error model (try / catch / panic) | LANDED | 04ea3ca645 | [phase-07-10](/docs/implementation/0054/phase-07-10-try-catch) |
| 7.11 | omap (OMapLiteral / Get / Set / Has / Len / Put) | LANDED | af82c4243c | [phase-07-11](/docs/implementation/0054/phase-07-11-omap) |
| 7.12 | List HOFs (map / filter / reduce) | LANDED | 24b8ac0274 | [phase-07-12](/docs/implementation/0054/phase-07-12-list-hofs) |
| 8 | Datalog (compile-time semi-naive eval) | NOT STARTED | - | - |
| 9.1 | Channels (`chan<T>`, send, recv) | LANDED (staging) | 6c8caa8f52 | [phase-09-1](/docs/implementation/0054/phase-09-1-channels) |
| 9.2 | Streams + subscribers | NOT STARTED | - | - |
| 10 | Agents | NOT STARTED | - | - |
| 11 | async / await | NOT STARTED | - | - |
| 12 | FFI (extern Go) | NOT STARTED | - | - |
| 13 | LLM driver | NOT STARTED | - | - |
| 14 | fetch (HTTP client) | NOT STARTED | - | - |
| 15 | TargetGoModule | NOT STARTED | - | - |
| 16 | Reproducibility (-trimpath, -buildvcs=false) | NOT STARTED | - | - |
| 17 | TargetGoWasmJS / TargetGoWasiP1 | NOT STARTED | - | - |
| 18 | Publish (pkg.go.dev metadata) | NOT STARTED | - | - |

## Runtime matrix

A phase is fully LANDED only after its gate passes on every required Go toolchain. Phases not yet exercised on a toolchain become N.1, N.2, ... sub-phases.

| Toolchain | Phases 0-7.x (language) | Phases 8-18 (concurrency + packaging) | Source |
|-----------|--------------------------|----------------------------------------|--------|
| Go 1.22 (ubuntu CI) | LANDED | IN PROGRESS | `go test ./transpiler3/go/...` |
| Go 1.22 (macos local) | LANDED | IN PROGRESS | `go test ./transpiler3/go/...` |
| Go 1.23 | LANDED (forward-compat) | IN PROGRESS | local |
| Go 1.24+ | NOT EXERCISED | NOT EXERCISED | future audit sub-phase |
| `GOOS=windows` | DEFERRED to Phase 16 | DEFERRED to Phase 16 | `phase01_test.go` skips on Windows |
| `GOOS=js`, `GOOS=wasip1` | NOT STARTED (Phase 17) | NOT STARTED (Phase 17) | wasm targets land at the end |

The Phase 1 fixture runner currently skips on Windows because the driver hard-codes a POSIX `go` invocation; the cross-tuple sweep moves under Phase 16 once the host build is fully reproducible. WASM targets (`go-wasm-js`, `go-wasip1`) are reserved for Phase 17.
