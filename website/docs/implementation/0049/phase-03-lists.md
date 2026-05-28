---
title: "Phase 3.1. Lists"
sidebar_position: 4
sidebar_label: "Phase 3.1. Lists"
description: "MEP-49 Phase 3.1 — list<T> to Swift Array with COW; map, filter, reduce, sort; list literals; structural equality."
---

# Phase 3.1. Lists

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 3.1](/docs/mep/mep-0049#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Lists`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Lists are Mochi's primary sequential collection. The lowering to Swift `Array<T>` is natural: Swift arrays are value types with copy-on-write semantics, exactly matching Mochi's immutable-by-default list model. Phase 3.1 ships `map`, `filter`, `reduce`, `sort`, and structural equality so that Phases 4+ can use lists of records without additional work.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.1.0 | `list<T>` → `[T]`; list literals `[1, 2, 3]`; `list.length` → `.count`; indexing `xs[i]` → `xs[Int(i)]` | NOT STARTED | — |
| 3.1.1 | `list.map(f)` → `.map(f)`; `list.filter(p)` → `.filter(p)`; `list.foldl(f, init)` → `.reduce(init, f)` | NOT STARTED | — |
| 3.1.2 | `list.sort()` → `.sorted()`; `list.sort_by(f)` → `.sorted(by: f)`; stable sort guaranteed | NOT STARTED | — |
| 3.1.3 | Structural equality: `xs == ys` on `[T]` where `T: Equatable`; `list.contains(x)` → `.contains(x)` | NOT STARTED | — |
| 3.1.4 | `list.concat(ys)` → `xs + ys`; `list.append(x)` → `xs + [x]` (returns new list, value semantics) | NOT STARTED | — |

## Sub-phase 3.1.0 -- List literals and indexing

### Decisions made (3.1.0)

**`list<T>` → `[T]`**: Swift array literal `[T]` is the natural target. It is a value type with COW, matching Mochi's value semantics. No `ImmutableList` wrapper needed (unlike .NET where `ImmutableList<T>` was required).

**List literals**: `[1, 2, 3]` in Mochi → `[Int64(1), Int64(2), Int64(3)]` in Swift. Element types are explicit when the array type annotation is needed:

```swift
// Mochi: let xs: list<int> = [1, 2, 3]
let xs: [Int64] = [Int64(1), Int64(2), Int64(3)]
```

**Empty list**: `[]` → `[T]()` or `[] as [T]` with explicit type. The lowerer always annotates the type when the list is empty to help the Swift type checker.

**Indexing**: Mochi `xs[i]` (where `i: int`) → `xs[Int(i)]`. Swift `Array` subscript takes `Int` (platform-width). The `Int(i)` cast is explicit to convert from Mochi's `Int64`. The lowerer always emits `Int(i)` to avoid platform-width confusion.

**`list.length`**: → `xs.count` (returns `Int`). The result is cast to `Int64` immediately: `Int64(xs.count)`, so Mochi code sees an `int` result, not a platform-width integer.

**`list.first` / `list.last`**: → `xs.first` / `xs.last` (returns `T?`, which is Mochi `option<T>`). Lowered in Phase 5 after option types are available.

## Sub-phase 3.1.1 -- Higher-order list operations

### Decisions made (3.1.1)

**`list.map(f)`**: → `xs.map(f)`. Swift's `Array.map` returns a new `[U]`. The function `f` is `(T) -> U`. No materialization needed; Swift `map` already returns an `Array`.

**`list.filter(p)`**: → `xs.filter(p)`. Returns `[T]`.

**`list.foldl(f, init)`**: Mochi `foldl` is left-fold. → `xs.reduce(init, f)`. Swift's `reduce` is left-to-right, matching Mochi `foldl`.

**`list.foldr(f, init)`**: Mochi `foldr` is right-fold. → `xs.reversed().reduce(init, { acc, x in f(x, acc) })`. The argument order swap is required because Mochi `foldr f init [a,b,c]` = `f a (f b (f c init))` while Swift's `reduce` threads the accumulator as the first argument.

**`list.for_each(f)`**: → `xs.forEach(f)`. `f` is `(T) -> Void` (Mochi `unit`).

**`list.flat_map(f)`**: → `xs.flatMap(f)`. `f` returns `[U]`.

**`list.zip(ys)`**: → `zip(xs, ys).map { ($0, $1) }`. Returns `[(T, U)]`. Truncates to the shorter list, matching Mochi semantics.

## Sub-phase 3.1.2 -- Sort

### Decisions made (3.1.2)

**Stability**: Swift 5.8+ `Array.sort` and `sorted()` are stable (guaranteed by the stdlib). This matches Mochi's specification that `sort` is stable.

**`list.sort()`**: → `xs.sorted()`. Requires `T: Comparable`. The lowerer adds the `Comparable` conformance constraint when emitting generic functions over sorted lists.

**`list.sort_by(f)`**: → `xs.sorted(by: f)`. `f` is `(T, T) -> bool`, lowered to `(T, T) -> Bool`. The function must be a strict weak ordering; the lowerer trusts the programmer here (no static verification in Phase 3).

**`list.sort_desc()`**: → `xs.sorted(by: >)`. Descending sort using the `>` operator.

**Multi-key sort**: Not directly supported in Phase 3. Phase 7 (query DSL) handles `order_by k1 asc, k2 desc` via chained `.sorted`.

## Sub-phase 3.1.3 -- Structural equality

### Decisions made (3.1.3)

**`[T]: Equatable` when `T: Equatable`**: Swift arrays conform to `Equatable` automatically when their element type does. Mochi `xs == ys` → Swift `xs == ys`. Element-wise comparison is structural, matching Mochi's value equality.

**`list.contains(x)`**: → `xs.contains(x)`. Requires `T: Equatable`. Always emitted with the method form, not `xs.contains(where: { $0 == x })`.

**`list.index_of(x)`**: → `xs.firstIndex(of: x).map { Int64($0) }`. Returns `option<int>`. Lowered in Phase 5 after option types are available.

## Sub-phase 3.1.4 -- Concat and append

### Decisions made (3.1.4)

**Value semantics**: Mochi lists are immutable. `list.append(x)` returns a new list; it does not mutate the original. Swift's `+` operator on arrays returns a new array, matching this.

**`list.concat(ys)`**: Mochi `xs.concat(ys)` → Swift `xs + ys`. Returns `[T]`.

**`list.append(x)`**: Mochi `xs.append(x)` → Swift `xs + [x]`. The `[x]` singleton is cheap (one allocation) for small types; for large payloads, the lowerer could emit `{ var tmp = xs; tmp.append(x); return tmp }` but this is deferred to a performance pass.

**`list.prepend(x)`**: Mochi `xs.prepend(x)` → Swift `[x] + xs`. O(n) copy; acceptable for Phase 3.

**`list.drop(n)`**: → `Array(xs.dropFirst(Int(n)))`. The `Array(...)` materialization is required because `dropFirst` returns a `Slice`, not an `Array`.

**`list.take(n)`**: → `Array(xs.prefix(Int(n)))`.

**`list.reverse()`**: → `xs.reversed()` (returns `ReversedCollection<[T]>`). Materialized to `[T]` via `Array(xs.reversed())` when the result is bound to a `list<T>` variable.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | List literal, indexing, `length` lowering |
| `transpiler3/swift/lower/builtins.go` | `map`, `filter`, `foldl`, `foldr`, `sort`, `sort_by`, `concat`, `append`, `prepend`, `drop`, `take`, `reverse`, `contains`, `zip` |
| `transpiler3/swift/build/phase03_lists_test.go` | `TestPhase3Lists`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase03-lists/` | 25 fixture directories |

## Test set

- `TestPhase3Lists` -- 25 fixtures covering: `list_empty`, `list_literal`, `list_index`, `list_length`, `list_map`, `list_filter`, `list_foldl`, `list_foldr`, `list_for_each`, `list_flat_map`, `list_zip`, `list_sort`, `list_sort_by`, `list_sort_desc`, `list_contains`, `list_concat`, `list_append`, `list_prepend`, `list_drop`, `list_take`, `list_reverse`, `list_nested`, `list_equality`, `list_of_string`, `list_of_float`.

## Deferred work

- `list.index_of`, `list.first`, `list.last` (require option types from Phase 5).
- `list.partition` (returns two lists; deferred to Phase 5 for tuple support).
- Lazy evaluation via `LazySequence`. Deferred to Phase 7 (query DSL).
- Parallel map via `withTaskGroup`. Deferred to Phase 9 (agents/concurrency).
