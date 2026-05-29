---
title: "Phase 3.5. Maps"
sidebar_position: 7
sidebar_label: "Phase 3.5. Maps"
description: "MEP-56 Phase 3.5, map literal and get and put on Ruby Hash."
---

# Phase 3.5. Maps

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | bdc3ca1fc7 |

## Gate

`TestPhase35Maps` in `transpiler3/ruby/build/phase035_test.go`: two inline subtests, `map_lit_get` and `map_put`. Each compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. Maps lower to Ruby `Hash`.

## Lowering decisions

Mochi maps lower to Ruby `Hash` literals using the explicit `=>` (hash-rocket) form. `aotir.MapLit` is rendered by `lowerMapLit` (lower.go lines 1143 to 1160) as `{k1 => v1, k2 => v2}`. The hash-rocket form is preferred over the colon form because Mochi keys can be arbitrary string or int values; the colon form is sugar that only works when keys are symbols, which Mochi keys are not.

`aotir.MapGetExpr` lowers to `recv.fetch(key)` rather than `recv[key]`. `Hash#fetch` raises `KeyError` on a missing key, matching Mochi's panic-on-missing semantics, whereas `[]` returns `nil`, which would silently corrupt downstream arithmetic. `aotir.MapPutStmt` lowers to a `RawStmt` rendering `m[key] = value`, using Ruby's `Hash#[]=`. The asymmetry (read via `fetch`, write via `[]=`) is intentional: write does not need a missing-key check, and `Hash#store(key, value)` is equivalent but less idiomatic.

`aotir.MapHasExpr` reaches the lowerer in this phase too (per the `has(m, k)` surface), rendered as `recv.key?(key)`. `aotir.MapLenExpr` lowers to `recv.size`, `aotir.MapKeysExpr` to `recv.keys`, and `aotir.MapValuesExpr` to `recv.values`. The ordered-map (`OMap*`) variants alias one-to-one to the same Ruby Hash methods because Ruby's Hash has preserved insertion order since 1.9, which §1 of the MEP relies on as the canonical lowering target for `omap`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerMapLit`, `MapGetExpr`/`MapHasExpr`/`MapLenExpr`/`MapKeysExpr`/`MapValuesExpr` arms, `MapPutStmt`/`OMapPutStmt` arms, `OMap*` literal/get/has/len arms |
| `transpiler3/ruby/build/phase035_test.go` | `TestPhase35Maps` with 2 subtests |

## Test set

- `TestPhase35Maps/map_lit_get`, `map_put`.

## Closeout notes

Phase 3.5 landed on CRuby 3.4 with both subtests green. Reading via `Hash#fetch` rather than `Hash#[]` was the load-bearing decision: a downstream `print(m["nope"])` would have printed `nil` (Ruby) versus panicking (vm3), breaking the byte-equal stdout gate. Key implementation insight: ordered-map (`OMap*`) reuses the same Hash rendering, so phase 3.5 implicitly covers Mochi's `omap` literal/get/has/len without a separate code path; the C lowerer carries the order invariant in the IR, and Ruby's Hash preserves it on iteration.
