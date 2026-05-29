---
title: "Phase 7.11. omap (insertion-ordered map)"
sidebar_position: 23
sidebar_label: "Phase 7.11. omap"
description: "MEP-54 Phase 7.11, omap<K, V> lowered to *mochiOMap[K, V] (Go map + keys slice) with O(1) get/has + insertion-order iteration."
---

# Phase 7.11. omap

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | [#22608](https://github.com/mochilang/mochi/pull/22608) |
| Commit         | af82c4243c |

## Gate

8 fixtures: `omap_literal`, `omap_get`, `omap_has`, `omap_len`, `omap_set`, `omap_overwrite`, `omap_int_keys`, `omap_put_stmt`. 253 transpiler3/go fixtures green (was 245).

## Lowering decisions

`omap<K, V>` lowers to `*mochiOMap[K, V]`, a small generic struct in the emitted helpers:

```go
type mochiOMap[K comparable, V any] struct {
    m    map[K]V
    keys []K
}
```

The pointer form lets `OMapPutStmt` mutate the receiver in place, matching Mochi's mutable-ordered-map semantics. The C lowerer rebinds via `orddict:store` (functional update + reassignment); the Go path mutates through the pointer directly so no re-assignment is needed at the call site.

Six aotir nodes lower in Phase 7.11:

- `OMapLiteralExpr` -> IIFE: `mochiOMapNew[K, V]()` followed by sequential `mochiOMapSet(o, k, v)` calls, preserving insertion order across both first-write and overwrite.
- `OMapGetExpr` -> `mochiOMapGet(o, k)` returning the value (zero value if missing).
- `OMapHasExpr` -> `mochiOMapHas(o, k)` returning a bool.
- `OMapLenExpr` -> `int64(mochiOMapLen(o))` keeping the int pin.
- `OMapSetExpr` (value form) -> IIFE matching the literal pattern that returns the receiver, though the C lowerer typically prefers the statement form.
- `OMapPutStmt` -> `mochiOMapSet(o, k, v)` (statement form).

`TypeOMap` is added to `letTypeText`, `paramTypeText`, and `returnTypeText`, all of which return `*mochiOMap[K, V]` and register the `mochiOMap` helper.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerOMapType(K, V)` -> `*mochiOMap[K, V]` |
| `transpiler3/go/lower/expr.go` | `OMapLiteralExpr`, `OMapGetExpr`, `OMapHasExpr`, `OMapLenExpr`, `OMapSetExpr` |
| `transpiler3/go/lower/stmt.go` | `OMapPutStmt` -> `mochiOMapSet(o, k, v)` |
| `transpiler3/go/lower/lower.go` | `mochiOMap[K, V]`, `mochiOMapNew`, `mochiOMapGet`, `mochiOMapHas`, `mochiOMapSet`, `mochiOMapLen` helper texts |
| `tests/transpiler3/go/fixtures/omap_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/omap_*` subtests covering literal construction, read, has, len, mutation, overwrite-preserves-order, int keys, and statement-form put.

## Closeout notes

Choosing the (map + keys slice) representation over Go's stdlib `container/list` was a soundness call: `container/list` would give O(1) ordered insertion but turn `Get` and `Has` into O(N). The hybrid representation costs one extra word per key (the slice entry) and a small append on insert, but keeps every operation at the expected complexity. The pointer-typed `*mochiOMap[K, V]` matters because Go's struct values would be copied at every assignment, breaking the mutate-through-binding semantics every fixture relies on.
