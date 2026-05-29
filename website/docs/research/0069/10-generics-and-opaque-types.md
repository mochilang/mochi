---
title: "10. Generics and opaque types"
sidebar_position: 11
sidebar_label: "10. Generics and opaque types"
description: "Swift's generic type system (type parameters, where clauses, associated types, some, any, primary associated types), why opaque return types cannot be mapped without a concrete monomorphisation, the [swift.monomorphise] table, the existential container cost, and the future potential for a Swift 6 typed-throws bridge."
---

# 10. Generics and opaque types

## Swift's generic type system

Swift has one of the most expressive generic type systems among mainstream languages (more expressive than Java's or C#'s, comparable to Rust's in scope). For the bridge, the relevant constructs are:

**Type parameters with constraints:**

```swift
public func sorted<T: Comparable>(_ array: [T]) -> [T]
public func zip<A, B>(_ a: [A], _ b: [B]) -> [(A, B)]
```

**`where` clause constraints:**

```swift
public func uniqued<T>(_ array: [T]) -> [T] where T: Hashable & Equatable
public func merge<K, V>(_ a: [K: V], _ b: [K: V]) -> [K: V] where K: Hashable
```

**Associated types in protocols:**

```swift
public protocol Collection {
    associatedtype Element
    associatedtype Index: Comparable
    subscript(position: Index) -> Element { get }
}
```

**Opaque return types (`some P`, SE-0244):**

```swift
public func makeBody() -> some View  // SwiftUI
public func makeAsyncSequence() -> some AsyncSequence  // Combine/AsyncAlgorithms
```

**Existential types (`any P`, SE-0352, Swift 5.7):**

```swift
public func process(handler: any RequestHandler) -> any Response
public var body: any View { ... }
```

**Primary associated types (SE-0346, Swift 5.7):**

```swift
public func asyncFilter<S: AsyncSequence>(_ seq: S) -> some AsyncSequence<S.Element, any Error>
```

## Why generic functions cannot be auto-translated

For `@_cdecl` to work, the exported symbol must have a concrete, fixed C type signature. `@_cdecl` explicitly prohibits generic parameters:

```swift
// This is a Swift compiler error:
@_cdecl("mochi_sorted")
public func mochi_sorted<T: Comparable>(_ array: [T]) -> [T] { ... }
// Error: global function 'mochi_sorted' with attribute @_cdecl cannot be generic
```

To export a generic function via `@_cdecl`, the type parameter must be monomorphised: a separate `@_cdecl` symbol per concrete type instantiation:

```swift
@_cdecl("mochi_sorted_Int")
public func mochi_sorted_Int(_ array: UnsafeRawPointer, _ count: Int, _ out: UnsafeMutableRawPointer) { ... }

@_cdecl("mochi_sorted_String")
public func mochi_sorted_String(_ ptrs: UnsafePointer<UnsafePointer<CChar>?>, _ count: Int, _ out: ...) { ... }
```

The `[swift.monomorphise]` table is the user's mechanism for requesting these concrete instantiations:

```toml
[swift]
monomorphise = [
    { item = "sorted", T = "Int" },
    { item = "sorted", T = "String" },
    { item = "OrderedDictionary", K = "String", V = "MyValueType" },
]
```

For each entry, the bridge emits one `@_cdecl` symbol per listed instantiation. The symbol name includes the type arguments: `mochi_<pkg>_sorted_Int`, `mochi_<pkg>_sorted_String`.

## Why `some P` opaque return types are refused by default

`some P` in a return position means "the return type conforms to `P` but the concrete type is hidden". The Swift compiler knows the concrete type at compile time (it is a form of generics where the type variable is resolved by the implementation, not the caller). But the ABI of `some P` depends on the concrete type; from the C caller's perspective, the return type is unknowable without the Swift type checker's input.

Example:

```swift
public func makeBody() -> some View { Text("Hello") }
```

The concrete return type is `Text`, a `@frozen` struct. But the C wrapper cannot say `returns_Text` without knowing this at bridge-synthesis time. If the concrete type is listed in `[swift.monomorphise]`, the bridge can emit a specific wrapper:

```toml
monomorphise = [{ item = "makeBody", result = "SwiftUI.Text" }]
```

Without an explicit `result` annotation, the bridge emits `SkipReport: opaque return type cannot be mapped`.

## Why `any P` existential return types are refused by default

`any P` means "a value of any type conforming to `P`, wrapped in an existential container". The existential container has a fixed ABI on Apple platforms (a 5-word structure: 3 words for inline storage or a heap pointer, 1 word for the metadata pointer, 1 word for the conformance witness table pointer). However, the bridge cannot expose this 5-word structure to Mochi in a type-safe way: Mochi has no representation for "a value that conforms to some protocol".

The bridge uses the opaque-handle strategy for `any P` in return position: the existential container is boxed in a `MochiHandle` and returned as an opaque pointer. The Mochi user can call protocol-defined methods on the opaque handle via synthesised `extern fn` declarations for each protocol requirement.

This is the only case where the bridge generates bindings for `any P` return types: if the protocol has requirements that are all in-table, those requirements are exposed as `extern fn` calls on the opaque handle. If the protocol has generic requirements (e.g., `associatedtype Element`), the handle is opaque with no exposed methods.

## Primary associated types (SE-0346)

Swift 5.7 introduced primary associated types, allowing constrained existential types like `any Collection<Int>` (a collection whose `Element` is `Int`). These are partially translatable:

```swift
public func process(_ items: any Collection<Int>) -> Int
```

The bridge translates `any Collection<Int>` as an opaque handle (the collection could be any conforming type), but generates a `count` method and an indexed-element accessor:

```mochi
extern type AnyCollectionInt
extern fn any_collection_int_count(c: AnyCollectionInt): int
extern fn any_collection_int_get(c: AnyCollectionInt, index: int): int
```

This is a best-effort translation; the full `Collection` protocol surface is not synthesised.

## The `[swift.monomorphise]` table in practice

From the fixture corpus, the monomorphise table typically covers:

- **swift-collections** (`OrderedDictionary`, `OrderedSet`, `Heap`): 3-5 entries per value type needed.
- **swift-algorithms** (`chunks`, `chunked`, `windows`, `combinations`): 2-4 entries per algorithm.
- **GRDB** (`fetch` generic on `FetchableRecord`): 1 entry per model type.
- **SwiftProtobuf** (generic decode functions): 1-2 entries per proto message type.

A typical production Mochi program using 3-4 Swift packages with generics needs 10-20 `monomorphise` entries. This is the user-visible cost of the closed-table approach; the alternative (auto-monomorphisation) would generate thousands of `@_cdecl` symbols per package, exploding the binary size and wrapper compile time.

## Swift 6 typed throws

Swift 6 (2024) introduced typed throws: `throws(E)` where `E` is a concrete error type. This reduces the `SkipReport` rate for functions that previously used `throws` (untyped, mapped to `Result<T, Error>` with stringified error) by allowing the bridge to translate `E` directly via the type table:

```swift
// Swift 6:
func fetch(url: URL) throws(NetworkError) -> Data

// Bridge maps to:
extern fn fetch(url: string): bytes throws(NetworkError) from swift "..."
// where NetworkError is translated as a Swift enum with associated values
```

Typed-throws support is included in the Swift 6.0 parser compatibility layer of the `.swiftinterface` ingest (the `throws(E)` grammar was added to `.swiftinterface` in Swift 6.0).

## Cross-references

- [[05-type-mapping]] for the complete translation table including struct, enum, and class handling.
- [[04-swiftinterface-ingest]] for how generic constraints appear in `.swiftinterface`.
- [[09-abi-stability]] for the ABI impact of monomorphised `@_cdecl` symbols.
