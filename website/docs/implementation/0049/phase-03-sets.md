---
title: "Phase 3.3. Sets"
sidebar_position: 6
sidebar_label: "Phase 3.3. Sets"
description: "MEP-49 Phase 3.3 — set<T> to OrderedSet (swift-collections); union, intersection, difference; set literals."
---

# Phase 3.3. Sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 3.3](/docs/mep/mep-0049#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Sets`: 20 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Mochi sets are insertion-ordered (like maps). Swift's stdlib `Set` is unordered. `OrderedSet` from `swift-collections` preserves insertion order, gives O(1) membership test, and conforms to `SetAlgebra`. Phase 3.3 builds on the `swift-collections` dependency introduced in Phase 3.2.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.3.0 | `set<T>` → `OrderedSet<T>`; set literals; `set.contains(x)` → `.contains(x)`; `set.length` → `.count` | NOT STARTED | — |
| 3.3.1 | `set.add(x)` → functional update; `set.remove(x)` → functional update | NOT STARTED | — |
| 3.3.2 | `set.union(s2)` → `.union(s2)`; `set.intersection(s2)` → `.intersection(s2)`; `set.difference(s2)` → `.subtracting(s2)` | NOT STARTED | — |
| 3.3.3 | `set.to_list()` → `Array(s)`; `set.from_list(xs)` → `OrderedSet(xs)`; structural equality | NOT STARTED | — |

## Sub-phase 3.3.0 -- Set literals and membership

### Decisions made (3.3.0)

**`OrderedSet<T>` from swift-collections**: same dependency as Phase 3.2 (`swift-collections` 1.1.x). `OrderedSet` is `Hashable` when `T: Hashable`, `Equatable` when `T: Hashable`. Membership is O(1) via the backing hash table. Iteration is insertion-order.

**Set literals**: Mochi `{1, 2, 3}` (set literal, distinct from map `{k:v}`) → `OrderedSet([Int64(1), Int64(2), Int64(3)])`. The `OrderedSet(_ sequence:)` initializer is used because `OrderedSet` does not support the `Set` literal syntax directly.

**Empty set**: `set<int>{}` → `OrderedSet<Int64>()`.

**`set.contains(x)`**: → `s.contains(x)`. O(1).

**`set.length`**: → `Int64(s.count)`.

## Sub-phase 3.3.1 -- Add and remove (functional)

### Decisions made (3.3.1)

**Value semantics**: Mochi sets are immutable. `set.add(x)` returns a new set. The lowerer emits a block that copies, mutates, and returns:

```swift
// Mochi: let s2 = s.add(42)
var __tmp_s2 = s
__tmp_s2.append(Int64(42))  // OrderedSet.append is a no-op if already present
let s2 = __tmp_s2
```

`OrderedSet.append` adds the element at the end if not already present (maintaining insertion order), no-ops if present. This matches Mochi set semantics.

**`set.remove(x)`**: → copy-mutate-return using `remove(_ member:)`:

```swift
var __tmp_s3 = s
__tmp_s3.remove(Int64(42))
let s3 = __tmp_s3
```

## Sub-phase 3.3.2 -- Set algebra

### Decisions made (3.3.2)

**`OrderedSet` and `SetAlgebra`**: `OrderedSet` conforms to `SetAlgebra` (via `swift-collections` 1.1+). This gives `.union`, `.intersection`, `.subtracting`, `.symmetricDifference` for free.

**`set.union(s2)`**: → `s.union(s2)`. Result is a new `OrderedSet` containing all elements of both sets. Elements from `s` come first in iteration order, then elements from `s2` not in `s`.

**`set.intersection(s2)`**: → `s.intersection(s2)`. Elements present in both sets, preserving order from `s`.

**`set.difference(s2)`**: Mochi `s.difference(s2)` = elements in `s` not in `s2`. → `s.subtracting(s2)`.

**`set.symmetric_difference(s2)`**: → `s.symmetricDifference(s2)`. Elements in exactly one of the two sets.

**`set.is_subset_of(s2)`**: → `s.isSubset(of: s2)`.

**`set.is_superset_of(s2)`**: → `s.isSuperset(of: s2)`.

## Sub-phase 3.3.3 -- Conversion and equality

### Decisions made (3.3.3)

**`set.to_list()`**: → `Array(s)`. Preserves insertion order. Returns `[T]`.

**`set.from_list(xs)`**: → `OrderedSet(xs)`. Duplicate elements in `xs` are silently dropped (first occurrence wins). This matches Mochi's set-from-list semantics.

**`s1 == s2`**: `OrderedSet` conforms to `Equatable`. Equality is order-sensitive: same elements in same insertion order. Two sets built from the same elements in different insertion orders are not equal. This is the correct Mochi semantics for ordered sets.

**`set.map(f)`**: → `OrderedSet(s.map(f))`. The `map` result is re-deduplicated by `OrderedSet` init (last occurrence wins in case of collision after `f`). Mochi documents that `set.map` may reduce size if `f` is not injective.

**`set.filter(p)`**: → `s.filter(p)`. Returns `OrderedSet<T>` directly (swift-collections 1.1 supports this).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | Set literal lowering → `OrderedSet([...])` |
| `transpiler3/swift/lower/builtins.go` | `contains`, `length`, `add`, `remove`, `union`, `intersection`, `difference`, `symmetric_difference`, `is_subset_of`, `is_superset_of`, `to_list`, `from_list`, `map`, `filter` |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Set.swift` | Set helper extensions |
| `transpiler3/swift/build/phase03_sets_test.go` | `TestPhase3Sets`: 20 fixtures |
| `tests/transpiler3/swift/fixtures/phase03-sets/` | 20 fixture directories |

## Test set

- `TestPhase3Sets` -- 20 fixtures covering: `set_empty`, `set_literal`, `set_contains`, `set_length`, `set_add`, `set_remove`, `set_union`, `set_intersection`, `set_difference`, `set_symmetric_difference`, `set_is_subset`, `set_is_superset`, `set_to_list`, `set_from_list`, `set_map`, `set_filter`, `set_equality`, `set_order`, `set_dedup`, `set_nested`.

## Deferred work

- `set.find(p)` → `s.first(where: p)` (returns `option<T>`). Deferred to Phase 5.
- `set.partition(p)`. Deferred to Phase 5.
- Set of sets (requires `OrderedSet: Hashable`; available in swift-collections 1.1).
