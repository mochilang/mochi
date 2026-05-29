---
title: "Phase 7.3. Query string ops"
sidebar_position: 15
sidebar_label: "Phase 7.3. Query string ops"
description: "MEP-54 Phase 7.3, StrLenExpr (rune count) and StrContainsExpr (strings.Contains) for query where/select clauses."
---

# Phase 7.3. Query string ops

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22561](https://github.com/mochilang/mochi/pull/22561) |
| Commit         | 3510d3295d |

## Gate

4 fixtures cover the cases needed to unlock string predicates and `len(s)` in query clauses: `contains` as a where predicate, `len(s)` as the select expression, `contains` across multiple matches, `len(s)` in a numeric comparison. All pass under `go test ./transpiler3/go/build/...`.

## Lowering decisions

`StrLenExpr` lowers to `int64(len([]rune(s)))` so the count follows Mochi semantics (Unicode codepoints, matching the C runtime's `mochi_str_len` which counts rune index positions). The naive `int64(len(s))` would return UTF-8 byte count, which diverges from C for any non-ASCII input. Wrapping with `int64(...)` preserves the int pin established in Phase 2.

`StrContainsExpr` lowers to `strings.Contains(s, sub)`, matching the C runtime's substring search (empty `sub` returns true). The `strings` import is registered on demand. Phase 7.3 covers only the two ops needed by the query DSL; the broader string surface (`substring`, `index`, `reverse`, `split`, `join`, `str`) lands in Phase 7.6.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `StrLenExpr` -> `int64(len([]rune(s)))`; `StrContainsExpr` -> `strings.Contains(s, sub)` |
| `tests/transpiler3/go/fixtures/query_str_*/` | 4 fixtures |

## Test set

- 4 `TestPhase1Hello/query_str_*` subtests covering `contains` predicate, `len` select, `contains` multi-match, `len` comparison.

## Closeout notes

The rune-count decision was load-bearing for the Phase 7.6 fixtures (which exercise `len` on multibyte strings). Catching it in Phase 7.3 cost one wrap; catching it later would have required a Phase 7.6-time audit of every existing query golden.
