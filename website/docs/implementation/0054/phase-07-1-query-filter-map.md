---
title: "Phase 7.1. Query DSL: filter + map"
sidebar_position: 13
sidebar_label: "Phase 7.1. Query filter + map"
description: "MEP-54 Phase 7.1, QueryScopeStmt flattened into an idiomatic slice-building loop with GC instead of the C arena."
---

# Phase 7.1. Query DSL: filter + map

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22555](https://github.com/mochilang/mochi/pull/22555) |
| Commit         | f9270a397b |

## Gate

Fixtures cover the simplest query surface: `where` filter, `select` map, `filter+map` composition, bool predicate, query-in-function, nested queries. All pass byte-equal under `go test ./transpiler3/go/build/... -run TestPhase1Hello/query_`.

## Lowering decisions

The C lowerer emits a `QueryScopeStmt` that wraps the query body with an arena scope (the C runtime uses a bump allocator); the Go transpiler relies on Go's GC instead, so `QueryScopeStmt` lowers to a flat splat of its `Body` into the surrounding block. The `ResultVar` pattern (declared outside the scope, mutated inside via `= append(...)`) translates directly to an idiomatic slice-building loop in Go: `var results []T; for _, row := range xs { if pred(row) { results = append(results, project(row)) } }`.

The flatten-not-wrap decision was driven by Go's GC: there is no Go-side analogue for the C arena's "free everything in this scope at end" behaviour, and synthesising one (a scoped allocator with manual rooting) would have been a Phase 7 detour with no fixture demanding it. Going through Go's `append` loop is idiomatic enough that gofmt produces a stable output and Go's escape analysis hoists short-lived rows correctly.

Order-by, skip, take, joins, and string-specific predicates remain for later Phase 7.X sub-phases. Phase 7.1 deliberately covers only what fixtures need to demonstrate that the flatten-the-scope decision works.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/stmt.go` | `QueryScopeStmt` splats `Body` into the surrounding block; `ResultVar` -> outer `var results []T` |
| `tests/transpiler3/go/fixtures/query_*/` | Filter / map / composition / nested fixtures |

## Test set

- `TestPhase1Hello/query_*` subtests covering `where`, `select`, `where+select`, bool predicate, query-in-function, nested query.

## Closeout notes

Eliding `QueryScopeStmt` rather than translating it into a `func() T { ... }()` IIFE meant the slice-building loop runs in the surrounding scope with no allocation hop. This was important for later phases (7.5 aggregations, 7.4 joins) where the result must be observable from the outer scope without an extra capture rewrite.
