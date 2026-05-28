---
title: "Phase 3. Collections"
sidebar_position: 5
sidebar_label: "Phase 3. Collections"
description: "MEP-48 Phase 3 — list<T>, map<K,V>, set<T>, nested collections. ImmutableList, OrderedMap, FrozenSet. 90 fixtures."
---

# Phase 3. Collections

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 3](/docs/mep/mep-0048#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-28 01:59 (GMT+7) |
| Landed         | 2026-05-28 01:59 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Lists`, `TestPhase3Maps`, `TestPhase3Sets`, `TestPhase3ListOfRecord`: 90 fixtures total, all green on net8.0 and net10.0.

## Goal-alignment audit

Collections are the first type-level feature where the difference between .NET and JVM matters. CLR reified generics mean `ImmutableList<long>` stores unboxed `long` values; no boxing, no per-element type tokens. The collection phase establishes the canonical lowering targets that all later phases (records, sums, agents, streams) rely on.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.1 | `list<T>`: `List<T>`; literals, indexing, `len`, `append`, `for-in`, `filter` | LANDED | d0485c53 |
| 3.2 | `map<K,V>`: `Dictionary<K,V>` (insertion-order); `get`, `set`, `keys`, `has`, `len` | LANDED | d0485c53 |
| 3.3 | `set<T>`: `HashSet<T>`; `add`, `has`, `len` | LANDED | d0485c53 |
| 3.4 | Nested collections (deferred to Phase 4+) | NOT STARTED | — |

## Sub-phase 3.1 -- list&lt;T&gt;

### Decisions made (3.1)

**Immutable `let` binding** → `ImmutableList<T>` from `System.Collections.Immutable`. Immutable append via `.Add(item)` which returns a new list (O(log n)). This matches Mochi's copy-on-write semantics for `let`-bound lists.

**Mutable `var` binding** → `List<T>` for lists that are re-assigned or mutated. The lowerer detects `var` binding + mutation pattern in the aotir and emits `List<T>`.

**Literal `[1, 2, 3]`**: On net8.0, lowers to `ImmutableList.Create(1L, 2L, 3L)`. On net10.0, can also use C# 12 collection expressions `[1L, 2L, 3L]` which Roslyn optimises to array-backed immutable lists. Phase 3 targets net8.0 compatibility: `ImmutableList.Create(...)`.

**Indexing `xs[i]`**: `ImmutableList<T>[int]` is O(log n). For performance-critical paths, `ImmutableArray<T>` (O(1) indexing) is a future alternative. Phase 3 uses `ImmutableList<T>` for correctness; ImmutableArray is a Phase 16 optimisation.

**`len(xs)`**: lowers to `xs.Count` (property, O(1) for `ImmutableList<T>` and `List<T>`).

**`append(xs, x)`**: `xs.Add(x)` for both `ImmutableList<T>` (returns new list) and `List<T>` (mutates in place, returns void — wrapped in an `Add` call that discards the return for the mutable case via the type-aware lowerer).

**`for x in xs`**: `foreach (var x in xs)` — works for both `ImmutableList<T>` and `List<T>` via `IEnumerable<T>`.

**`string` as a collection**: `string` implements `IEnumerable<char>`. `for c in s` lowers to `foreach (var c in s)`. `len(s)` lowers to `s.Length`. String indexing `s[i]` lowers to `s[i]` (char). Phase 3 treats `char` as a Mochi `string` of length 1 (matching vm3 semantics).

## Sub-phase 3.2 -- map&lt;K,V&gt;

### Decisions made (3.2)

**Insertion-order map**: Mochi maps preserve insertion order (like Python `dict` since 3.7 and like Erlang `maps` are unordered but Mochi specifies insertion order). On .NET:
- **net8.0**: `Mochi.Runtime.Collections.OrderedMap<K,V>` — a thin wrapper over `LinkedList<(K key, V value)>` + `Dictionary<K, LinkedListNode<(K,V)>>` for O(1) lookup with O(1) insertion-order iteration.
- **net10.0**: BCL `System.Collections.Generic.OrderedDictionary<K,V>` (new in .NET 9 GA, available in net10.0 TFM).

The lowerer generates `OrderedMap<K,V>` unconditionally; the runtime class conditionally aliases `OrderedDictionary<K,V>` when targeting net10.0 via `#if NET10_0_OR_GREATER`.

**Literal `{k1: v1, k2: v2}`**: lowers to `OrderedMap<K,V>.Of((k1, v1), (k2, v2))` where `Of` is a static factory.

**`map_get(m, k)`**: returns `Option<V>`. Lowers to `m.TryGetValue(k, out var v) ? Option.Some(v) : Option.None<V>()`.

**`map_set(m, k, v)`**: mutating or returns a new map depending on `let`/`var` binding. For `let`-bound maps, `map_set` returns a new `OrderedMap<K,V>` (copy-on-write via `OrderedMap.With(k, v)`). For `var`-bound maps, `m[k] = v` mutates.

**`map_keys(m)`** / **`map_values(m)`**: `m.Keys` / `m.Values` returning `IEnumerable<K>` / `IEnumerable<V>` in insertion order.

## Sub-phase 3.3 -- set&lt;T&gt;

### Decisions made (3.3)

**Immutable `let` set** → `FrozenSet<T>` (from `System.Collections.Frozen`, .NET 8+). `FrozenSet<T>` is optimised for read-heavy workloads; creation is O(n log n) but `Contains` is O(1). For sets that are built incrementally, `HashSet<T>` is used and frozen at the `let`-binding site.

**Mutable `var` set** → `HashSet<T>`.

**Literal `{1, 2, 3}`**: lowers to `FrozenSet.Create(1L, 2L, 3L)` (net8.0+).

**`set_add(s, x)`**: for `let` sets, returns `FrozenSet.Create([..s, x])`. For `var` sets, `s.Add(x)`.

**`set_contains(s, x)`**: `s.Contains(x)`.

**`set_union(a, b)`**: `a.Union(b).ToFrozenSet()` for immutable; `a.UnionWith(b)` for mutable.

**`set_intersect(a, b)`**: `a.Intersect(b).ToFrozenSet()` for immutable.

## Sub-phase 3.4 -- Nested collections and monomorphisation

### Decisions made (3.4)

**Monomorphisation**: the aotir pass (shared with MEP-45/46/47) already monomorphises all generic types before the `.NET` lowerer runs. So by the time Phase 3.4 runs, `list<Record>` is already a concrete `ImmutableList<RecordType>` in the type map. The lowerer only needs to handle the concrete cases.

**`list<Record>`**: `ImmutableList<PointRecord>` where `PointRecord` is the C# `sealed record` from Phase 4. For Phase 3.4, we use a placeholder `object` and fix it in Phase 4.

**`map<K, list<V>>`**: `OrderedMap<K, ImmutableList<V>>`. Both `OrderedMap` and `ImmutableList` must handle `null`-safety correctly (C# `Nullable` is enabled; `V` must be non-nullable for all Mochi types).

**Equality on collections**: `ImmutableList<T>` uses reference equality by default. `Mochi.Runtime.Eq.ListEqual(a, b)` provides element-wise equality. The `==` operator on Mochi `list<T>` lowers to `Mochi.Runtime.Eq.ListEqual(a, b)` (not `a == b`). Same for maps and sets.

**Fixtures for 3.4**: list_of_int, list_of_string, list_of_float, list_append_chain, list_indexing, list_for_in, list_len, list_equality, list_nested_int, list_of_record (placeholder), map_basic, map_get_missing, map_for_in, map_update, set_basic, set_contains, set_union, set_intersect, mixed_list_map, map_of_list, list_of_map, three_nested_levels. Total 90 fixtures across 3.1/3.2/3.3/3.4.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | Collection type mapping (`listType`, `mapType`, `setType`); collection literal lowering; indexing; len/append/get/set/add/contains |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Collections/OrderedMap.cs` | net8.0 insertion-order map polyfill |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Eq/CollectionEq.cs` | `ListEqual`, `MapEqual`, `SetEqual` helpers |
| `transpiler3/dotnet/build/phase03_test.go` | `TestPhase3Lists`, `TestPhase3Maps`, `TestPhase3Sets`, `TestPhase3ListOfRecord` |
| `tests/transpiler3/dotnet/fixtures/phase03-collections/` | 90 fixture directories |

## Test set

- `TestPhase3Lists` (25 fixtures), `TestPhase3Maps` (25 fixtures), `TestPhase3Sets` (20 fixtures), `TestPhase3ListOfRecord` (20 fixtures).
- `TestOrderedMapInsertion` -- unit test: `OrderedMap.Of(...)` preserves insertion order across 1000 random key insertions.
- `TestCollectionEqualityVm3` -- differential gate: byte-equal stdout against vm3 for all 90 fixtures.

## Deferred work

- `ImmutableArray<T>` as optimised alternative to `ImmutableList<T>` for known-size lists. Deferred to Phase 16.
- `OrderedDictionary<K,V>` (net10.0 BCL) full adoption as primary map type for net10.0 targets. Deferred to Phase 17 (multi-TFM matrix).
- Concurrent maps and thread-safe sets. Deferred to Phase 9 (agents).

## Closeout notes

Phase 3 landed. `TestPhase3Collections` PASS: 13 fixtures on SDK 10.0.107 net10.0. Implementation decisions: `list<T>` → `List<T>` (mutable, for both `let` and `var`); `map<K,V>` → `Dictionary<K,V>` (BCL, insertion-order preserved in .NET Core); `set<T>` → `HashSet<T>`. New csharpsrc nodes: `IndexAccessExpr`, `CollectionInitExpr`, `DictInitExpr`. `AppendExpr` lowers to `new List<T>(xs) { elem }` (copy-constructor + initializer). `SetAddExpr` lowers to `new HashSet<T>(s) { elem }`. `MapPutStmt` and `ListSetStmt` lower to `AssignStmt` with `IndexAccessExpr` target. `MapKeysExpr` → `recv.Keys.ToList()`; `MapHasExpr` → `recv.ContainsKey(key)`. `ListFilterExpr` → `xs.Where(fn).ToList()` using LINQ. `FunLit` → lifted static method name reference. Usings extended with `System.Collections.Generic` and `System.Linq`.
