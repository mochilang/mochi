---
title: "Phase 4. Record equality"
sidebar_position: 9
sidebar_label: "Phase 4. Record equality"
description: "MEP-54 Phase 4, record == and != lower to Go's native struct comparison (works for any all-scalar record)."
---

# Phase 4. Record equality

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22515](https://github.com/mochilang/mochi/pull/22515) |
| Commit         | 17ccd7e15c |

## Gate

22 new fixtures cover the record-equality surface. Organised by axis: field-by-type (int / string / bool / float), multi-field (two-fields, three-fields, two record types), equality (`==`, `!=`, eq with string field, eq with bool field, mixed compare), field usage (arith, concat, used-twice, in-if, in-while, negate/!), assignment and aliasing (var-assign, alias-check, swap, pair-print). Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/record_`.

## Lowering decisions

The Go-side dispatch adds `BinEqRec` and `BinNeRec` to the infix-op map so the C lowerer's tagged record-equality ops compile to Go's native `==` / `!=` on struct values. Go's struct equality is field-wise and works for any struct whose fields are themselves comparable, which is exactly the all-scalar-record case the upstream type-checker already constrains. No runtime helper is needed; the comparison is generated inline.

Phase 4 deliberately did not extend records with non-comparable fields (e.g., records that contain slices, maps, or function values), because Go's `==` would fail to compile against those. The C lowerer enforces the all-scalar invariant via `aotir`'s `BinEqRec` / `BinNeRec` tagging, so the Go lowerer can lean on that without re-checking.

Record assignment (`var b = a`) is a Go struct value copy. Aliasing semantics match Mochi's value-typed record contract: mutating `b.Field` does not affect `a.Field`. The `pair-print`, `swap`, and `alias-check` fixtures exercise this corner.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | Add `BinEqRec` / `BinNeRec` arms in the infix-op map |
| `tests/transpiler3/go/fixtures/record_*/` | 22 fixtures |

## Test set

- 22 `TestPhase1Hello/record_*` subtests covering equality, multi-field, assignment, and field-usage axes.

## Closeout notes

Phase 4 was a one-arm patch in the lowerer because all the heavy lifting was done in Phase 3.4 (record declarations + lowering). Reusing Go's native `==` instead of synthesising a per-record `Equal()` method keeps the printed source small and avoids the C runtime's `mochi_eq_rec` helper. The "non-comparable-record" question is deferred indefinitely: when Mochi lifts that constraint, the Go path will need a synthesised helper, but no current fixture motivates it.
