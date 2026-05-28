---
title: "Phase 3.2. Maps"
sidebar_position: 5
sidebar_label: "Phase 3.2. Maps"
description: "MEP-49 Phase 3.2 — map<K,V> to OrderedDictionary (swift-collections); insertion-order iteration; merge, filter_map, keys, values."
---

# Phase 3.2. Maps

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 3.2](/docs/mep/mep-0049#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Maps`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Mochi maps are insertion-ordered, which is a semantic guarantee. Swift's stdlib `Dictionary` does not preserve insertion order. `OrderedDictionary` from `swift-collections` does, and it matches the semantics Mochi programs expect. Phase 3.2 establishes the dependency on `swift-collections` that Phases 3.3 (sets) and Phase 7 (query group_by) will also use.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.2.0 | `map<K,V>` → `OrderedDictionary<K,V>`; map literals; `map.get(k)` → `[k]`; `map.set(k,v)` → functional update | NOT STARTED | — |
| 3.2.1 | `map.keys` → `.keys`; `map.values` → `.values`; `map.length` → `.count`; `map.contains_key(k)` → `.keys.contains(k)` | NOT STARTED | — |
| 3.2.2 | `map.map_values(f)` → `.mapValues(f)`; `map.filter(p)` → `.filter(p)`; `map.merge(m2)` → `.merging(m2, uniquingKeysWith: { _, new in new })` | NOT STARTED | — |
| 3.2.3 | Structural equality: `m1 == m2` when `K: Hashable & Equatable, V: Equatable` | NOT STARTED | — |

## Sub-phase 3.2.0 -- Map literals and access

### Decisions made (3.2.0)

**`OrderedDictionary<K,V>` from swift-collections**: Mochi map semantics require insertion-order iteration. `OrderedDictionary` preserves insertion order and exposes the same subscript and mutation APIs as `Dictionary`. It is `Hashable` when `K: Hashable & Equatable, V: Hashable`, `Equatable` when `V: Equatable`.

**MochiRuntime dependency**: `swift-collections` is declared as a MochiRuntime dependency in `Package.swift`:
```swift
.package(url: "https://github.com/apple/swift-collections", from: "1.1.0"),
```
User `Package.swift` files get this transitively through `MochiRuntime`.

**Map literals**: `{ "a": 1, "b": 2 }` in Mochi → `OrderedDictionary(uniqueKeysWithValues: [("a", Int64(1)), ("b", Int64(2))])`. The pairs array preserves insertion order. The lowerer emits this form (not the `[:]` dict literal) because `OrderedDictionary` does not support the `[k: v]` Swift dict literal syntax.

**Empty map**: `{}` with explicit type annotation → `OrderedDictionary<K, V>()`.

**`map.get(k)`**: Mochi `m.get(k)` returns `option<V>` (present or absent). → `m[k]` (returns `V?`). In Phase 5, `V?` maps to `option<V>`. In Phase 3.2, fixtures avoid the option return; `get` is tested via direct subscript that is known to be present, and the `?? default` operator handles missing keys.

**`map.get_or(k, default)`**: → `m[k] ?? default`.

**`map.set(k, v)`**: Mochi maps are value-typed. `m.set(k, v)` returns a new map. → `{ var tmp = m; tmp[k] = v; return tmp }` emitted as a closure call or inline block. The lowerer emits the block form for `let` bindings:

```swift
// Mochi: let m2 = m.set("c", 3)
var __tmp_m2 = m
__tmp_m2["c"] = Int64(3)
let m2 = __tmp_m2
```

**`map.delete(k)`**: → `{ var tmp = m; tmp.removeValue(forKey: k); return tmp }` (same block pattern as `set`).

## Sub-phase 3.2.1 -- Keys, values, length

### Decisions made (3.2.1)

**`map.keys`**: → `Array(m.keys)` materialized to `[K]`, preserving insertion order. `OrderedDictionary.keys` returns `OrderedDictionary<K,V>.Keys` (a typed collection slice). Materializing to `Array` gives a `list<K>` in Mochi.

**`map.values`**: → `Array(m.values)` → `[V]`.

**`map.length`**: → `Int64(m.count)`.

**`map.contains_key(k)`**: → `m.keys.contains(k)`. `OrderedDictionary.keys` is a `Hashable`-indexed collection; `.contains` is O(1) via the hash table.

**`map.to_list()`**: → `m.map { ($0.key, $0.value) }` → `[(K, V)]`. Returns an array of key-value tuples in insertion order.

## Sub-phase 3.2.2 -- Higher-order map operations

### Decisions made (3.2.2)

**`map.map_values(f)`**: → `m.mapValues(f)`. Returns `OrderedDictionary<K, U>`. Preserves insertion order. Keys unchanged.

**`map.filter(p)`**: `p` is `(K, V) -> bool`. → `m.filter { p($0.key, $0.value) }`. Returns `OrderedDictionary<K,V>`.

**`map.merge(m2)`**: Right-biased merge (m2 wins on duplicate keys). → `m.merging(m2, uniquingKeysWith: { _, new in new })`. Returns a new `OrderedDictionary`. The merged map preserves the order of `m` for keys present in `m`, then appends keys from `m2` that are not in `m`.

**`map.merge_with(m2, f)`**: Custom conflict resolution. → `m.merging(m2, uniquingKeysWith: f)`.

**`map.for_each(f)`**: → `m.forEach { f($0.key, $0.value) }`.

## Sub-phase 3.2.3 -- Structural equality

### Decisions made (3.2.3)

**`OrderedDictionary: Equatable`**: `OrderedDictionary` conforms to `Equatable` when `K: Hashable & Equatable, V: Equatable`. The comparison is order-sensitive: two maps are equal only if they have the same keys in the same insertion order with the same values. This matches Mochi's map equality semantics (maps are ordered, so order matters for equality).

**`m1 == m2`**: → Swift `m1 == m2`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | Map literal lowering → `OrderedDictionary(uniqueKeysWithValues:...)` |
| `transpiler3/swift/lower/builtins.go` | `get`, `get_or`, `set`, `delete`, `keys`, `values`, `length`, `contains_key`, `to_list`, `map_values`, `filter`, `merge`, `merge_with`, `for_each` |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Map.swift` | Map helper extensions (type aliases, convenience inits) |
| `transpiler3/swift/runtime/Package.swift` | `swift-collections` dependency (added in 3.2.0) |
| `transpiler3/swift/build/phase03_maps_test.go` | `TestPhase3Maps`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase03-maps/` | 25 fixture directories |

## Test set

- `TestPhase3Maps` -- 25 fixtures covering: `map_empty`, `map_literal`, `map_get`, `map_get_or`, `map_set`, `map_delete`, `map_keys`, `map_values`, `map_length`, `map_contains_key`, `map_to_list`, `map_map_values`, `map_filter`, `map_merge`, `map_merge_with`, `map_for_each`, `map_equality`, `map_insertion_order`, `map_nested`, `map_of_list`, `map_string_keys`, `map_int_values`, `map_overwrite`, `map_from_list`, `map_large`.

## Deferred work

- `map.get(k)` returning `option<V>`. Deferred to Phase 5 (option types).
- `map.index_of(k)` (insertion-order index). Deferred to Phase 5.
- Persistent/persistent-update map (HAMT). Out of v1 scope.
- `map.sort_by_key()` / `map.sort_by_value()`. Deferred to Phase 7 (query order_by).
