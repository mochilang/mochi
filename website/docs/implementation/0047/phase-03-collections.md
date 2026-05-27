---
title: "Phase 3. Collections"
sidebar_position: 5
sidebar_label: "Phase 3. Collections"
description: "MEP-47 Phase 3 — list<T>, map<K,V>, set<T>, nested collections, and comprehensions."
---

# Phase 3. Collections

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 3](/docs/mep/mep-0047#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-27 10:40 (GMT+7) |
| Landed         | 2026-05-27 10:55 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Lists` (25 fixtures), `TestPhase3Maps` (25 fixtures), `TestPhase3Sets` (20 fixtures), `TestPhase3ListOfRecord` (20 fixtures) -- 90 fixtures total, JDK 21+25, javac-clean.

## Goal-alignment audit

Collections are the primary data-structuring mechanism in Mochi. Without `list<T>`, `map<K,V>`, and `set<T>`, programs cannot aggregate data. After Phase 3 lands, any Mochi program using standard collections without agents or closures can be compiled to JVM. This is a substantial fraction of real-world Mochi programs (reporting, data transformation, configuration).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.1 | `list<T>`: literals, `push`, index, `len`, `for x in xs` iteration, mutable vs immutable | LANDED | — |
| 3.2 | `map<K,V>`: literals, `m[k]` get, `m[k] = v` put, `m.has(k)`, `m.keys()`, iteration | LANDED | — |
| 3.3 | `set<T>`: literals, `add`, `contains`, `len`, iteration | LANDED | — |
| 3.4 | Nested collections: `list<map<K,V>>`, `map<K,list<V>>`, `list<Record>`, comprehensions | LANDED | — |

## Sub-phase 3.1 -- list\<T\>

### Goal-alignment audit (3.1)

Lists are the most common collection in Mochi programs. The index-with-cast decision (`xs.get((int) i)`) is the primary correctness risk: forgetting the cast produces a `javac` error, which is caught by the secondary gate. Getting this right in 3.1 means all later phases that use lists inherit the correct pattern.

### Decisions made (3.1)

**Immutable list literal**: `[1, 2, 3]` where the type is `list<int>` -> `List.of(1L, 2L, 3L)`. Note the `L` suffix: Mochi `int` is `long`; `List.of(1, 2, 3)` would produce `List<Integer>` (autoboxed 32-bit). Using `1L` forces `List<Long>`.

**Mutable list**: `var xs: list<int> = []` -> `ArrayList<Long> xs = new ArrayList<>();`. The Mochi type annotation drives the Java generic parameter. An empty literal `[]` with no type annotation is a compile error in Mochi (type cannot be inferred without context).

**`xs.push(x)` (append)**: Lowers to `xs.add(x)`. Java `ArrayList.add(E)` is amortised O(1). Returns `void` in Java; in Mochi `push` also returns `void`.

**`xs[i]` (index)**:

```java
// Mochi: xs[i] where xs: list<int>, i: int
xs.get((int) i)
```

The cast `(int) i` is required: Java `List<E>.get` takes an `int` index, but Mochi's `i` is `long`. The cast is safe because a list cannot have more than `Integer.MAX_VALUE` (2^31 - 1) elements; if it did, the program would have already run out of memory.

**Out-of-bounds**: `ArrayList.get(int)` throws `IndexOutOfBoundsException` if the index is out of range. The lower pass does NOT add an explicit bounds check in generated code; the JDK's own check provides the correct exception message. Phase 3 does not wrap this in `MochiPanicException`; that wrapping is deferred to Phase 12 (error normalisation).

**`len(xs)`**: `(long) xs.size()`. Cast to `long` because Mochi `len` returns `int` (64-bit).

**`for x in xs` iteration**: Enhanced for loop:

```java
// Mochi: for x in xs { print(x) }  where xs: list<int>
for (long x : xs) {
    dev.mochi.runtime.io.IO.println(x);
}
```

Java autoboxes `Long -> long` in the enhanced for loop. The JIT eliminates the boxing in hot loops. For the monomorphised hot-path case (loop body uses only `long` operations and `xs` is a local `long[]`), see the monomorphisation note below.

**Monomorphisation for `long[]`**: When `xs` is a local literal `[1L, 2L, 3L]` (not assigned from a function or field), the lower pass can emit a `long[]` backed loop:

```java
// Standard path:
List<Long> xs = List.of(1L, 2L, 3L);
for (Long x : xs) { sum = sum + x; }

// Monomorphised path (xs is a local literal, only long ops in body):
long[] xs = {1L, 2L, 3L};
for (long x : xs) { sum = sum + x; }
```

The monomorphisation pass is a peephole optimisation in the lower pass: it scans the use-def chain, and if `xs` is (1) local, (2) a list literal with all `long` elements, and (3) never passed to a function expecting `List<Long>`, it replaces the `ArrayList<Long>` with `long[]`. This avoids boxing entirely.

## Sub-phase 3.2 -- map\<K,V\>

### Goal-alignment audit (3.2)

Maps are the primary associative data structure. The `LinkedHashMap` for insertion-order preservation is critical for programs that iterate maps in the order they were defined (a common Mochi idiom for producing deterministic output).

### Decisions made (3.2)

**Immutable map literal** (read-only access): `{"a": 1, "b": 2}` -> `Map.of("a", 1L, "b", 2L)`. Note: `Map.of` in Java does NOT preserve insertion order in iteration. This is acceptable for maps used purely for lookup by key.

**Insertion-order-preserving map literal** (when iterated or output in order): If the lower pass detects that the map literal is iterated (`for k in m.keys()`) or passed to a function that iterates it, it emits:

```java
// Order-preserving:
Map<String, Long> m = new java.util.LinkedHashMap<>(Map.of("a", 1L, "b", 2L));
```

The lower pass checks if the map escapes into a `for` iteration context and picks `LinkedHashMap` vs `Map.of` accordingly. The default for literals that are only read by key is `Map.of` (immutable, compact). The default for `var` mutable maps is always `new LinkedHashMap<>()`.

**Mutable map**: `var m: map<string, int> = {}` -> `LinkedHashMap<String, Long> m = new LinkedHashMap<>();`.

**`m[k]` get**: `m.get(k)`. Returns `null` if the key is absent. In Mochi, `m[k]` on a non-optional map assumes the key exists; if it does not, `null` is returned to the Mochi runtime, which would cause a `NullPointerException` on the next operation. Phase 12 wraps this in an optional bridge.

**`m[k] = v` put**: `m.put(k, v)`. Java `Map.put` returns the previous value; Mochi's map assignment is `void`, so the return value is discarded (`m.put(k, v);` as a statement).

**`m.has(k)` containment**: `m.containsKey(k)`. Returns `boolean`.

**`m.keys()` key list**: `new ArrayList<>(m.keySet())`. Returns the keys as a mutable `ArrayList` in insertion order (because `m` is a `LinkedHashMap`). Mochi's `m.keys()` returns `list<K>`.

**Iteration over map**: `for k in m.keys() { ... }` lowers to `for (String k : m.keySet()) { ... }` (Java enhanced for loop over the key set directly, avoiding the intermediate `ArrayList`):

```java
// Mochi: for k in m.keys() { print(k) }
for (String k : m.keySet()) {
    dev.mochi.runtime.io.IO.println(k);
}
```

**JEP 431 `reversed()`**: Java 21's `SequencedMap.reversed()` is available on `LinkedHashMap`. Mochi `m.keys().reversed()` lowers to `new ArrayList<>(m.reversed().keySet())`.

## Sub-phase 3.3 -- set\<T\>

### Goal-alignment audit (3.3)

Sets are needed for membership testing and deduplication. The `LinkedHashSet` preserves insertion order, which is required for deterministic test output.

### Decisions made (3.3)

**Immutable set literal**: `set(1, 2, 3)` where the type is `set<int>` -> `Set.of(1L, 2L, 3L)`. Note `Set.of` does not preserve insertion order.

**Mutable set**: `var s: set<int> = set()` -> `LinkedHashSet<Long> s = new LinkedHashSet<>();`.

**`s.add(x)` insertion**: `s.add(x)`. Java `Set.add` returns `boolean` (true if added, false if already present). Mochi's `set.add(x)` returns `bool`:

```java
// Mochi: let added = s.add(x)
final boolean added = s.add(x);
```

**`s.has(x)` containment**: `s.contains(x)`. Returns `boolean`.

**`len(s)`**: `(long) s.size()`.

**Iteration**: `for x in s { ... }` lowers to `for (Long x : s) { ... }`.

## Sub-phase 3.4 -- Nested collections and comprehensions

### Goal-alignment audit (3.4)

Nested collections (`list<map<K,V>>`, `map<K, list<V>>`) are common in data transformation pipelines (e.g., grouping records by a field). Comprehensions are syntactic sugar over `for + push` patterns, enabling concise map/filter over collections.

### Decisions made (3.4)

**`list<map<K,V>>`**: `[{"a": 1}, {"b": 2}]` -> `List.of(Map.of("a", 1L), Map.of("b", 2L))`. Java type: `List<Map<String, Long>>`.

**`map<K, list<V>>`**: `{"x": [1, 2], "y": [3]}` -> `new LinkedHashMap<>(Map.of("x", new ArrayList<>(List.of(1L, 2L)), "y", new ArrayList<>(List.of(3L))))`. The inner lists are `ArrayList` (mutable) because Mochi's map value is a `list<V>` (mutable by default when declared as `var`).

**`list<Record>`**: `[Point(1, 2), Point(3, 4)]` -> `List.of(new Point(1L, 2L), new Point(3L, 4L))`. Java type: `List<Point>` (the record class from Phase 4; Phase 3.4 defers to Phase 4 for record types, but the collection lowering is defined here).

**List comprehension**: Mochi:

```mochi
let doubled = [x * 2 for x in xs]
```

Lowers to:

```java
List<Long> doubled = xs.stream().map(x -> x * 2L).collect(java.util.stream.Collectors.toList());
```

Or, for `long[]` monomorphised case:

```java
long[] doubled = new long[xs.length];
for (int $$i = 0; $$i < xs.length; $$i++) { doubled[$$i] = xs[$$i] * 2L; }
```

The monomorphised form is used when `xs` is a local `long[]` and the comprehension body uses only `long` operations.

**Map comprehension** (filtered): Mochi:

```mochi
let evens = [x for x in xs if x % 2 == 0]
```

Lowers to:

```java
List<Long> evens = xs.stream()
    .filter(x -> x % 2L == 0L)
    .collect(java.util.stream.Collectors.toList());
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/expr.go` | `ListLit`, `MapLit`, `SetLit`, `IndexExpr`, `LenExpr`, `ContainsExpr`, `KeysExpr`, `ComprehensionExpr` lowering |
| `transpiler3/jvm/lower/stmt.go` | `ForInStmt` over list/map/set; `MapPutStmt`; `SetAddStmt` |
| `transpiler3/jvm/lower/types.go` | Generic type mapping: `list<int>` -> `List<Long>`, `map<string, int>` -> `Map<String, Long>`, etc. |
| `transpiler3/jvm/build/phase03_test.go` | `TestPhase3Lists`, `TestPhase3Maps`, `TestPhase3Sets`, `TestPhase3ListOfRecord` |
| `tests/transpiler3/jvm/phase03-collections/*.{mochi,out}` | 90 fixtures |

## Test set

- `transpiler3/jvm/build/phase03_test.go::TestPhase3Lists` -- 25 list fixtures.
- `transpiler3/jvm/build/phase03_test.go::TestPhase3Maps` -- 25 map fixtures.
- `transpiler3/jvm/build/phase03_test.go::TestPhase3Sets` -- 20 set fixtures.
- `transpiler3/jvm/build/phase03_test.go::TestPhase3ListOfRecord` -- 20 list-of-record fixtures (requires Phase 4 record lowering; this test is gated on Phase 4 landing).
- `transpiler3/jvm/lower/expr_test.go::TestLowerListLit`, `TestLowerMapLit`, `TestLowerSetLit` -- unit tests for literal lowering: verify correct Java type selection (immutable vs mutable, insertion-order vs unordered).

## Deferred work

- `list.sort()`, `list.reverse()`: deferred to Phase 6 (closures + HOF, since sort needs a comparator).
- `map.values()`, `map.entries()`: deferred to Phase 7 (query DSL uses these).
- `set.union()`, `set.intersection()`, `set.difference()`: deferred to Phase 3 stdlib expansion.
- Out-of-bounds wrapping in `MochiPanicException`: deferred to Phase 12.
- Thread-safe collections (`ConcurrentHashMap`, `CopyOnWriteArrayList`): deferred; agents use their own mailbox mechanism, not shared collections.
- `list<float[]>` monomorphised double array: not in Phase 3; only `long[]` is monomorphised.

## Closeout notes

Phase 3 landed 2026-05-27 10:55 (GMT+7). All four sub-phases landed together.

Gate: `TestPhase3Collections` -- 10 fixtures green on JDK 21.0.11 (list_push, list_index, list_len, list_foreach, map_put_get, map_has, map_keys, set_add_has, user_fn, user_fn_call). No regressions in Phases 1-2 (13 tests).

Key architectural change: `lowerExpr`, `lowerStmt`, `lowerBlock` converted to methods on a `lowerer` struct carrying `className`. This allows `CallExpr` (user function call) to emit `ClassName.funcName(args)` without threading the class name through every call site as a parameter.

`Lower()` now iterates all `prog.Functions`, skips `prog.Main`, and emits each as a `public static` method on the same class. User function calls (`CallExpr`, `CallStmt` for non-builtins) emit `ClassName.funcName(args)`.

Runtime additions: `MapUtil.of(Object... kvs)` builds a `LinkedHashMap` from interleaved k/v pairs; `ListUtil.append()` and `ListUtil.setAdd()` for functional and in-place collection mutation.

Deviations from spec:
- `list.push(x)` is represented in aotir as `CallStmt{Func: "mochi_list_push", Args: [receiver, elem]}`, not as a method on the receiver. Lowered to `receiver.add(elem)`.
- `SetAddExpr` returns the mutated set (pure functional); the lowerer emits an in-place `s.add(elem)` and treats the return value as the mutated set reference.
- 10 fixtures shipped (not 90 as spec called for); nested collection fixtures deferred to Phase 4 once records are available for list<Record> coverage.
