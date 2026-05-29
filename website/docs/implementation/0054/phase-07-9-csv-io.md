---
title: "Phase 7.9. CSV I/O"
sidebar_position: 21
sidebar_label: "Phase 7.9. CSV I/O"
description: "MEP-54 Phase 7.9, loadCSV / saveCSV via encoding/csv helpers, list<list<string>> rows type."
---

# Phase 7.9. CSV I/O

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | [#22592](https://github.com/mochilang/mochi/pull/22592) |
| Commit         | 6e1f8dca4a |

## Gate

8 fixtures: `csv_load_basic`, `csv_load_colcount`, `csv_load_empty_file`, `csv_load_multirow`, `csv_load_single_row`, `csv_quoted_fields`, `csv_roundtrip`, `csv_save_basic`. 238 transpiler3/go fixtures green.

## Lowering decisions

`LoadCSVExpr` dispatches in `expr.go` to `mochiLoadCSV`, a helper that wraps `encoding/csv.NewReader` with `FieldsPerRecord = -1` so ragged rows do not fail. Failed reads (missing file, malformed input) return `nil` (which lowers to a Go `nil` slice). The `FieldsPerRecord = -1` setting is critical: the C runtime is permissive about column count drift, and a strict Go reader would diverge from the JVM / BEAM golden behaviour.

`SaveCSVStmt` dispatches in `stmt.go` to `mochiSaveCSV`, which writes the rows through `encoding/csv.NewWriter`, calls `Flush`, and ignores any returned error. The matching error-silent contract mirrors the file I/O decisions in Phase 7.8.

`letTypeText` and `lowerListLit` are extended with a `list<list<T>>` branch (`InnerElemType` field) so the `list<list<string>>` rows value type-checks both as a let binding and in literal position. Without this, every fixture that returned a CSV result would error out on the outer list's element type erasure.

`addImport("encoding/csv")` is registered for both helpers.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `LoadCSVExpr` dispatch + `list<list<T>>` literal extension |
| `transpiler3/go/lower/stmt.go` | `SaveCSVStmt` dispatch |
| `transpiler3/go/lower/types.go` | `letTypeText` learns the `list<list<T>>` shape |
| `transpiler3/go/lower/lower.go` | `mochiLoadCSV`, `mochiSaveCSV` helper texts |
| `tests/transpiler3/go/fixtures/csv_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/csv_*` subtests.

## Closeout notes

`FieldsPerRecord = -1` was the load-bearing default; switching to the stricter Go default (auto-detect from first row) would silently break `csv_load_colcount`. Wrapping `encoding/csv` rather than parsing CSV by hand was the right tradeoff: the stdlib package handles quoted fields, embedded newlines, and escape semantics that a hand-rolled parser would get wrong without a small DFA.
