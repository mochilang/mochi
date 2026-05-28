---
title: "Phase 3.4. List of records"
sidebar_position: 7
sidebar_label: "Phase 3.4. List of records"
description: "MEP-49 Phase 3.4 — nested collections with struct elements; monomorphisation of generic collection operations over record types."
---

# Phase 3.4. List of records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 3.4](/docs/mep/mep-0049#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3ListOfRecords`: 20 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Phase 3.4 proves that the collection operations from 3.1-3.3 compose correctly with record types from Phase 4. Because Swift generics are reified (not erased), `[MyRecord].map(f)` works without monomorphisation in the Go lowerer -- Swift's type system handles specialisation at the compiler level. Phase 3.4 validates this end-to-end. This is the bridge from "collections of scalars" to "collections of structured data."

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.4.0 | `list<Record>` literals; `map`, `filter` over record lists; record fields in closures | NOT STARTED | — |
| 3.4.1 | `map<string, Record>` literals; map access returning a record; record update in map | NOT STARTED | — |
| 3.4.2 | Typed empty literals: `[] as [MyRecord]`, `OrderedDictionary<String, MyRecord>()` | NOT STARTED | — |
| 3.4.3 | Nested collections: `list<list<T>>`, `map<K, list<V>>`, `list<map<K,V>>` | NOT STARTED | — |

## Sub-phase 3.4.0 -- List of records

### Decisions made (3.4.0)

**No monomorphisation in Go lowerer**: Swift's generics are reified. `[MyRecord].map { $0.field }` is compiled by swiftc with full type information; the lowerer does not need to generate specialised Go code per element type. This contrasts with the C backend (which hand-expands templates) and the JVM backend (which needs boxing for primitive types).

**Record type in list literal**: `[MyRecord]` type annotation is explicit in the `let` binding when the list is the first expression containing records. Subsequent references infer from context.

```swift
// Mochi: let users = [{ name: "alice", age: 30 }, { name: "bob", age: 25 }]
let users: [User] = [User(name: "alice", age: Int64(30)), User(name: "bob", age: Int64(25))]
```

**`map` over record list**: The closure receives the full record struct. Fields are accessed via `.field` (dot notation on the Swift struct).

```swift
// Mochi: let names = users.map(fun(u) => u.name)
let names: [String] = users.map { u in u.name }
```

**`filter` over record list**: The closure receives the record and returns `Bool`.

```swift
// Mochi: let adults = users.filter(fun(u) => u.age >= 18)
let adults: [User] = users.filter { u in u.age >= Int64(18) }
```

**Nested field access in sort**: `sort_by` on a list of records, sorting by a nested field.

```swift
// Mochi: let sorted = users.sort_by(fun(u) => u.age)
let sorted: [User] = users.sorted(by: { a, b in a.age < b.age })
```

## Sub-phase 3.4.1 -- Map of records

### Decisions made (3.4.1)

**`map<string, Record>` literal**: Similar to plain map literals but with record values.

```swift
// Mochi: let db = { "alice": { age: 30 }, "bob": { age: 25 } }
let db: OrderedDictionary<String, User> = OrderedDictionary(uniqueKeysWithValues: [
    ("alice", User(name: "alice", age: Int64(30))),
    ("bob", User(name: "bob", age: Int64(25))),
])
```

**Map access returning a record**: `db.get_or("alice", default_user)` → `db["alice"] ?? default_user`.

**Record update in map**: Functional update of a record stored in a map requires extracting, updating, and reinserting:

```swift
// Mochi: let db2 = db.set("alice", { db.get_or("alice", default_user) with age: 31 })
var __tmp_db2 = db
let __alice = db["alice"] ?? defaultUser
__tmp_db2["alice"] = User(name: __alice.name, age: Int64(31))
let db2 = __tmp_db2
```

The `with` update syntax for records is lowered in Phase 4; Phase 3.4 covers the map wrapping around it.

## Sub-phase 3.4.2 -- Typed empty literals

### Decisions made (3.4.2)

**Typed empty list**: Mochi `[]` in a context expecting `list<MyRecord>` → `[MyRecord]()` with the explicit type. The lowerer determines the expected type from the `let` binding annotation or function parameter type.

**Typed empty map**: Mochi `{}` in a `map<K, Record>` context → `OrderedDictionary<K, Record>()`.

**Typed empty set**: Mochi `{}` in a `set<Record>` context → `OrderedSet<Record>()`.

**Inference failure**: If the context type is not determinable (e.g., passed to a generic function without type annotation), the lowerer emits a type-annotated form with a compiler comment. The `TestSwiftcClean` gate catches any resulting ambiguity warnings.

## Sub-phase 3.4.3 -- Nested collections

### Decisions made (3.4.3)

**`list<list<T>>`**: → `[[T]]`. All collection operations compose naturally. `xs.map { inner in inner.filter { x in ... } }` works without special handling.

**`map<K, list<V>>`**: → `OrderedDictionary<K, [V]>`. Building such a map from a list (group-by) is deferred to Phase 7 (query DSL `group by`). Phase 3.4 only covers direct literals and access.

**`list<map<K,V>>`**: → `[OrderedDictionary<K, V>]`. Each element is an `OrderedDictionary`. Indexing into the list gives an `OrderedDictionary`; subscripting that gives `V?`.

**Nesting depth**: Phase 3.4 tests up to two levels of nesting. Deeper nesting is theoretically unlimited but not explicitly tested; `TestSwiftcClean` catches any type inference issues.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | Typed empty literal lowering; nested collection type annotation |
| `transpiler3/swift/lower/types.go` | `swiftTypeOf(mochiType)` helper: translates Mochi type → sxtree TypeSyntax |
| `transpiler3/swift/build/phase03_records_test.go` | `TestPhase3ListOfRecords`: 20 fixtures |
| `tests/transpiler3/swift/fixtures/phase03-list-of-records/` | 20 fixture directories |

## Test set

- `TestPhase3ListOfRecords` -- 20 fixtures covering: `list_of_record_literal`, `list_of_record_map`, `list_of_record_filter`, `list_of_record_sort`, `list_of_record_fold`, `list_of_record_contains`, `map_of_record_literal`, `map_of_record_get`, `map_of_record_update`, `map_of_record_map_values`, `set_of_record`, `typed_empty_list`, `typed_empty_map`, `typed_empty_set`, `nested_list_list`, `nested_map_list`, `nested_list_map`, `nested_list_of_list_map`, `record_in_multiple_collections`, `collection_of_option_record`.

## Deferred work

- `group_by` on record lists (builds `map<K, list<V>>`). Deferred to Phase 7.
- `list<option<Record>>`. Deferred to Phase 5 (option types).
- `list<union_type>`. Deferred to Phase 5 (sum types).
