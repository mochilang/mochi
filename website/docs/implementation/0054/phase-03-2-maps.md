---
title: "Phase 3.2. map<K, V> for scalar K and V"
sidebar_position: 6
sidebar_label: "Phase 3.2. map<K, V>"
description: "MEP-54 Phase 3.2, map<K, V> lowering to native Go map[K]V with sorted-key iteration via slices.Sorted(maps.Keys)."
---

# Phase 3.2. `map<K, V>` for scalar K and V

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22495](https://github.com/mochilang/mochi/pull/22495) |
| Commit         | c86fd6a412 |

## Gate

10 fixtures cover the scalar map surface: `map_lit`, `map_get`, `map_put`, `map_len`, `map_has`, `map_string_keys`, `map_keys_sorted`, `map_values_sorted`, `map_bool_values`, `map_float_values`. All 39 fixtures pass byte-equal under `go test ./transpiler3/go/build/... -run TestPhase1Hello`.

## Lowering decisions

Mochi `map<K, V>` lowers to native Go `map[K]V`. `MapLit` emits a composite literal `map[K]V{k1: v1, k2: v2}`. `MapGetExpr` emits `m[k]`. `MapPutStmt` emits `m[k] = v`. `MapLenExpr` wraps with `int64(...)` to keep the Mochi int pin consistent with `list` and `set` len results. `MapHasExpr` lowers to an IIFE that returns the `ok` result of a two-value index: `func() bool { _, ok := m[k]; return ok }()`. The IIFE form keeps `has` as an expression so it composes inside `where` clauses and conditional expressions without a temporary binding.

`MapKeysExpr` uses `slices.Sorted(maps.Keys(m))` so the result preserves Mochi's sorted-iteration semantics. The C runtime sorts by key on iteration; the Go transpiler does the same by materialising the sorted-keys slice up front. `MapValuesExpr` walks the sorted keys via an IIFE (`func() []V { ks := slices.Sorted(maps.Keys(m)); out := make([]V, 0, len(ks)); for _, k := range ks { out = append(out, m[k]) }; return out }()`) so the values come out in the same order. The `slices` and `maps` imports are registered on demand.

`gotree.FuncLit` is taught to use the new `writeInlineNoNewline` path so expression-position func literals (now used by `MapHasExpr` and `MapValuesExpr`) do not emit a trailing newline that would break the surrounding token stream. `LetStmt` routes through `letTypeText` so map bindings get the right `map[K]V` annotation.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerMapType(key, value)` + `lowerMapTypeWithList(key, value, listValueElem)` |
| `transpiler3/go/lower/expr.go` | `MapLit`, `MapGetExpr`, `MapHasExpr`, `MapLenExpr`, `MapKeysExpr`, `MapValuesExpr` |
| `transpiler3/go/lower/stmt.go` | `MapPutStmt` -> `m[k] = v` |
| `transpiler3/go/gotree/expr.go` | `FuncLit.writeInlineNoNewline` for expression-position lambdas |
| `tests/transpiler3/go/fixtures/map_*/` | 10 map fixtures |

## Test set

- `TestPhase1Hello/map_lit`, `map_get`, `map_put`, `map_len`, `map_has`, `map_string_keys`, `map_keys_sorted`, `map_values_sorted`, `map_bool_values`, `map_float_values`.

## Closeout notes

Choosing sorted-key iteration over Go's runtime-randomised map order keeps Mochi-to-Go output deterministic by default, which the reproducibility phase (16) depends on. The two-value index trick for `has` is the canonical Go idiom; rejecting `m[k] != zero(V)` was important because a value-typed map (`map[string]int`) cannot distinguish "missing" from "present with zero value". Wrapping `len` in `int64` matches the list / set pattern set in Phase 3.1.
