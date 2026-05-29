---
title: "Phase 3.4. list<record> + record declarations"
sidebar_position: 8
sidebar_label: "Phase 3.4. list<record>"
description: "MEP-54 Phase 3.4, record types lower to Go structs (capitalised exported field names); list<record> uses []Name element type."
---

# Phase 3.4. `list<record>` + record declarations

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22508](https://github.com/mochilang/mochi/pull/22508) |
| Commit         | 6e189d3819 |

## Gate

22 new fixtures cover record declarations and lists of records: `record_basic` plus 21 `list_record_*` fixtures (`basic`, `iter`, `field_sum`, `index`, `len`, `append`, `set`, `filter_loop`, `lookup`, `count_matching`, `max_field`, `two_records`, `string_field`, `bool_field`, `float_field`, `three_fields`, `empty`, `print_loop`, `predicate_print`, `string_lookup`, `reverse_loop`). Run: `go test ./transpiler3/go/build/... -run TestPhase1Hello/list_record_`.

## Lowering decisions

Each `aotir.RecordDecl` emits a Go `type Name struct { ... }` declaration with capitalised (exported) field names. The capitalisation step is uniform: Mochi `name` becomes Go `Name`, `score` becomes `Score`. This matches Go's visibility convention and avoids the temptation of mapping Mochi `x` to Go `x` (which would make every field invisible to other packages even though Mochi has no package boundary inside a single transpiled binary).

`RecordLit` lowers to a Go struct composite literal `Name{Field1: v1, Field2: v2}`. `FieldAccess` lowers to a Go `SelectorExpr` `r.Field`. Lists of records use `[]Name` as the element type, threaded via the new `ElemRecordName` on aotir's list nodes; the lowerer routes through `lowerListType` for the slice header and treats record-element lists symmetrically with scalar-element lists.

The Go struct field order matches the Mochi declaration order so the composite literal can use positional or named form interchangeably; the lowerer prefers the named form for readability of the generated source (and to keep the printed source stable when field order is reshuffled at the source level).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/lower.go` | `lowerRecordDecl` -> `type Name struct {...}` with capitalised exports |
| `transpiler3/go/lower/expr.go` | `RecordLit` -> Go struct composite; `FieldAccess` -> SelectorExpr |
| `transpiler3/go/lower/types.go` | `lowerListType` accepts a record-element name via `ElemRecordName` |
| `tests/transpiler3/go/fixtures/record_basic/`, `list_record_*/` | 22 fixtures |

## Test set

- `TestPhase1Hello/record_basic`
- `TestPhase1Hello/list_record_basic`, `iter`, `field_sum`, `index`, `len`, `append`, `set`, `filter_loop`, `lookup`, `count_matching`, `max_field`, `two_records`, `string_field`, `bool_field`, `float_field`, `three_fields`, `empty`, `print_loop`, `predicate_print`, `string_lookup`, `reverse_loop`.

## Closeout notes

Capitalising every field unconditionally simplified later phases: Phase 7.1's query DSL emits `r.Field` selectors without having to remember which fields were lowercase in the source. The "list of records" surface was bundled into the same phase as record declarations because every meaningful test of records requires iterating a collection of them; splitting the two would have produced a phase that could not exercise its own primitives.
