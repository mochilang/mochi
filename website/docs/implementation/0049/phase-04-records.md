---
title: "Phase 4. Records"
sidebar_position: 8
sidebar_label: "Phase 4. Records"
description: "MEP-49 Phase 4 — record types to @frozen public struct: Sendable, Hashable, Codable; functional update with(); Equatable; generic records."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 4](/docs/mep/mep-0049#phase-4-records) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase4Records`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Records are Mochi's primary data aggregation. On Swift, they lower to `@frozen struct` with `Sendable`, `Hashable`, and `Codable` conformances. The `@frozen` attribute is critical for library evolution: it tells the Swift compiler that no new fields will be added, enabling optimal layout and exhaustive switch. `Sendable` conformance ensures records can cross actor boundaries (Phase 9). `Codable` (Phase 4 sub-phase) enables JSON serialisation without extra runtime metadata.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | `record User { name: string; age: int }` → `@frozen public struct User: Sendable, Hashable` | NOT STARTED | — |
| 4.1 | Memberwise init: `User(name: "alice", age: 30)` → Swift memberwise initializer with labeled params | NOT STARTED | — |
| 4.2 | Functional update `{ u with age: 31 }` → `with()` extension method pattern | NOT STARTED | — |
| 4.3 | `Codable` conformance for JSON; `Equatable` for structural equality | NOT STARTED | — |
| 4.4 | Generic records `record Pair<A, B> { first: A; second: B }` → generic Swift struct | NOT STARTED | — |

## Sub-phase 4.0 -- Struct declaration

### Decisions made (4.0)

**`@frozen public struct`**: every Mochi record lowers to a `@frozen public struct`. `@frozen` means the struct's stored properties are stable; clients compiled against the module can access properties directly without an indirection layer. Since Mochi programs are compiled as a single SwiftPM module (or a library + executable pair), `@frozen` is always safe and gives the best code generation.

**Protocol conformances**: every record gets `Sendable, Hashable` synthesised by the Swift compiler. `Sendable` enables crossing actor isolation boundaries (Phase 9). `Hashable` enables use in `Set<Record>` and as `Dictionary` keys.

**`Equatable`** is implied by `Hashable` (Swift requires `Equatable` for `Hashable`). Structural equality is synthesised.

**Field ordering**: Mochi record fields are declared in source order. Swift struct fields are emitted in the same order. Swift's synthesised memberwise init follows field declaration order.

**Naming**: Mochi field names are snake_case (e.g., `user_name`). Swift convention is camelCase. The lowerer converts snake_case to camelCase: `user_name` → `userName`. This is a one-way mapping; Mochi programs access fields via snake_case; the generated Swift uses camelCase.

```swift
// Mochi: record User { name: string; age: int }
@frozen
public struct User: Sendable, Hashable {
    public let name: String
    public let age: Int64
}
```

**Stored properties**: all fields are `let` (immutable). Mochi records are value-typed and immutable. Mutation is via functional update (Phase 4.2).

**One file per record**: each Mochi record type generates a dedicated `.swift` file (e.g., `User.swift`). Functions defined over a record are placed in extension blocks in the same file. This makes the generated Swift navigable in IDE source trees.

## Sub-phase 4.1 -- Memberwise init

### Decisions made (4.1)

**Swift synthesises memberwise init automatically**: for a struct with all `let` stored properties, Swift synthesises a memberwise initializer with labeled arguments in field-declaration order. No explicit `init` is emitted.

**External label form**: `User(name: "alice", age: Int64(30))`. All arguments are labeled. The Mochi lowerer emits the labeled form always (not positional).

**Nil-safe init**: if a Mochi record field has type `option<T>` (Phase 5), the corresponding Swift type is `T?`. The memberwise init takes `T?` directly.

**Nested record init**: a Mochi record field that is itself a record type → the Swift struct field is the inner struct type. The init takes the inner struct as a labeled argument.

```swift
// Mochi:
// record Address { city: string; zip: string }
// record User { name: string; address: Address }
// let u = { name: "alice", address: { city: "SF", zip: "94105" } }

let u = User(
    name: "alice",
    address: Address(city: "SF", zip: "94105")
)
```

## Sub-phase 4.2 -- Functional update with()

### Decisions made (4.2)

**`with()` extension method**: Mochi `{ u with age: 31 }` produces a new record with one field changed. Swift structs do not have a built-in `with` syntax. The lowerer generates a `with` extension method for each record:

```swift
// Emitted in User.swift:
extension User {
    func with(
        name: String? = nil,
        age: Int64? = nil
    ) -> User {
        User(
            name: name ?? self.name,
            age: age ?? self.age
        )
    }
}
```

Usage:

```swift
// Mochi: let u2 = { u with age: 31 }
let u2 = u.with(age: Int64(31))
```

**Type collision**: if a field is already `Optional<T>` (from `option<T>` in Mochi), the `with()` parameter cannot be `T??` to distinguish "not provided" from "provided as nil". The lowerer uses a sentinel enum instead for optional fields:

```swift
// For an option<T> field:
enum __WithSentinel<T> { case unchanged; case set(T?) }
```

This is generated per-field when needed; not emitted for non-optional fields.

**Performance**: each `with()` call creates a new struct value. Swift's COW does not apply here (structs are not COW by default); the copy is trivially cheap for small structs and is inlined by the Swift compiler's optimizer.

## Sub-phase 4.3 -- Codable and Equatable

### Decisions made (4.3)

**`Codable` synthesis**: the Swift compiler synthesises `Codable` for a struct when all stored properties are `Codable`. `String`, `Int64`, `Double`, `Bool`, `[T]`, `OrderedDictionary` are all `Codable` (via `swift-collections` 1.1). The synthesised implementation uses field names as JSON keys. Since Mochi fields are converted to camelCase in Swift, the JSON output uses camelCase keys. A `CodingKeys` enum is NOT emitted by default (the synthesised behaviour is camelCase-to-camelCase, which is the Swift/JavaScript convention).

**`Hashable` implies `Equatable`**: structural equality is synthesised. `u1 == u2` compares all fields.

**`Codable` declared in struct**: the protocol conformance list becomes `Sendable, Hashable, Codable`:

```swift
@frozen
public struct User: Sendable, Hashable, Codable {
    public let name: String
    public let age: Int64
}
```

## Sub-phase 4.4 -- Generic records

### Decisions made (4.4)

**Generic struct**: Mochi `record Pair<A, B> { first: A; second: B }` → Swift generic struct:

```swift
@frozen
public struct Pair<A, B>: Sendable, Hashable, Codable
where A: Sendable & Hashable & Codable, B: Sendable & Hashable & Codable {
    public let first: A
    public let second: B
}
```

**Where clause**: the lowerer emits `where` constraints for every type parameter to propagate `Sendable & Hashable & Codable`. This is required for the synthesised conformances to work.

**Monomorphisation**: Swift's generics are reified. `Pair<String, Int64>` is a concrete type; the Swift compiler specialises it. The Go lowerer does NOT need to monomorphise generic records (unlike the C backend). This is one of the main ergonomic advantages of the Swift target.

**Recursive generic records** (e.g., `record Tree<A> { value: A; children: list<Tree<A>> }`): require `indirect` for the recursive field. Swift structs cannot contain themselves directly. The lowerer emits the field as a computed property backed by an `@inline(never)` heap allocation:

```swift
// Lowered as a class-based box to break the recursive layout:
@frozen
public struct Tree<A>: Sendable, Hashable, Codable where A: Sendable & Hashable & Codable {
    public let value: A
    private let _children: _Box<[Tree<A>]>
    public var children: [Tree<A>] { _children.value }
    public init(value: A, children: [Tree<A>]) {
        self.value = value
        self._children = _Box(children)
    }
}
private final class _Box<T>: @unchecked Sendable { let value: T; init(_ v: T) { value = v } }
```

Recursive records are uncommon; the `indirect enum` pattern (from Phase 5) is the idiomatic Swift way to handle recursive types.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `RecordDecl` → `@frozen public struct` with conformances |
| `transpiler3/swift/lower/record.go` | `with()` extension method generation; recursive field boxing |
| `transpiler3/swift/lower/generics.go` | Generic type parameter mapping; `where` clause emission |
| `transpiler3/swift/lower/types.go` | Updated `swiftTypeOf` for record types |
| `transpiler3/swift/build/phase04_test.go` | `TestPhase4Records`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase04-records/` | 25 fixture directories |

## Test set

- `TestPhase4Records` -- 25 fixtures covering: `record_basic`, `record_nested`, `record_equality`, `record_in_list`, `record_in_map`, `record_update_single`, `record_update_multi`, `record_update_nested`, `record_generic_pair`, `record_generic_triple`, `record_generic_nested`, `record_codable_json`, `record_hashable`, `record_function_field`, `record_recursive_tree`, `record_list_field`, `record_map_field`, `record_option_field`, `record_variant_field`, `record_multiple_types`, `record_large_fields`, `record_snake_camel`, `record_init_labeled`, `record_structural_eq`, `record_in_set`.

## Deferred work

- `@dynamicMemberLookup` for duck-typed record access. Out of v1 scope.
- `Codable` with custom `CodingKeys` for snake_case JSON output. Deferred to Phase 14 (fetch/JSON).
- `NSCopying` for Objective-C bridging. Deferred to Phase 12 (FFI).
- `description` / `CustomStringConvertible` synthesisation. Deferred -- `print(record)` uses `MochiRuntime.print` which calls `String(reflecting:)` for now.
