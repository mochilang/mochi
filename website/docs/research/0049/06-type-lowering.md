# MEP-49 research note 06, Type-system lowering for MEP-49

Author: research pass for MEP-49 (Mochi to Swift transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies, type-by-type, how Mochi's type system lowers
to Swift 6.0 types. It is the load-bearing reference for
[[05-codegen-design]]'s codegen pass.

## 1. Primitive types

| Mochi type   | Swift type        | Boxed (for any) | Notes                              |
|--------------|-------------------|-----------------|------------------------------------|
| `int`        | `Int64`           | `AnyHashable`   | Always 64-bit signed, never `Int`. |
| `float`      | `Double`          | `AnyHashable`   | IEEE 754 double precision.         |
| `bool`       | `Bool`            | `AnyHashable`   |                                    |
| `string`     | `String`          | `AnyHashable`   | UTF-8 native (Swift 5.7+).         |
| `bytes`      | `Data` or `[UInt8]` | `AnyHashable` | `Data` for I/O, `[UInt8]` in core. |
| `char`       | `Character`       | `AnyHashable`   | Extended grapheme cluster.         |
| `unit`       | `Void`            | n/a             | Or `()`.                           |
| `null`       | `nil` literal     | n/a             | Used with Optional only.           |

**Why `Int64`, not `Int`.** Swift's `Int` is platform-pointer-width
(32-bit on watchOS Series 3 and below, 64-bit elsewhere). Mochi
`int` is fixed 64-bit. Always emit `Int64` to keep semantics stable
across platforms.

**Why `Double`.** Mochi `float` is 64-bit IEEE 754. Swift `Double`
matches. Swift's `Float80` is x86-only; Mochi never uses it.

**Why `Data` vs `[UInt8]`.** `Data` is `Foundation.Data`,
heap-allocated with copy-on-write, optimised for I/O (URLSession,
FileHandle). `[UInt8]` is the stdlib array, also COW. Mochi codegen
picks `Data` for I/O boundaries and `[UInt8]` for in-memory
manipulation.

## 2. Boxing and AnyHashable

Swift uses `AnyHashable` for type-erased hashable values. Mochi
heterogeneous containers (e.g., `list<any>`) lower to
`[AnyHashable]`. `Any` is reserved for non-hashable (`[Any]` works
for the equivalent without dictionary-key use).

Boxing into `Any` is implicit in Swift. Unboxing requires
conditional cast: `if let x = a as? Int64 { ... }`.

Mochi pattern matching on `any` lowers to a Swift `switch` with
type patterns:

```swift
switch boxed {
case let i as Int64: ...
case let s as String: ...
default: ...
}
```

## 3. Reified generics

Swift generics are reified (the compiler specialises per type at
WMO). Unlike JVM erasure, the runtime type is preserved. Mochi's
generic functions lower directly:

```mochi
fun first<T>(xs: list<T>): T { ... }
```

becomes:

```swift
public func first<T>(_ xs: [T]) -> T { ... }
```

No type erasure, no JVM-style synthetic accessors, no reflection
boundary. The Swift compiler may specialise `first<Int64>` and
`first<String>` separately under `-O -whole-module-optimization`,
producing efficient monomorphic code; outside WMO it keeps a
generic dispatch path.

Mochi's monomorphisation pass (shared with MEP-45/46/47/48) may
pre-specialise hot paths in the codegen output, emitting both
`func firstInt64(_ xs: [Int64]) -> Int64` and the generic
`first<T>` for cold paths.

## 4. Collections

| Mochi          | Swift                                 | Source                              |
|----------------|---------------------------------------|-------------------------------------|
| `list<T>`      | `[T]` (`Array<T>`)                    | stdlib                              |
| `map<K,V>`     | `[K: V]` (`Dictionary<K,V>`)          | stdlib                              |
| `set<T>`       | `Set<T>`                              | stdlib                              |
| `ordered_map<K,V>` | `OrderedDictionary<K,V>`           | apple/swift-collections             |
| `ordered_set<T>`   | `OrderedSet<T>`                    | apple/swift-collections             |
| `deque<T>`     | `Deque<T>`                            | apple/swift-collections             |
| `heap<T>`      | `Heap<T>`                             | apple/swift-collections             |

**Hashable requirement.** Swift `Dictionary` and `Set` require `K: Hashable`.
Mochi's static check enforces this at the Mochi level. The codegen
pass emits `: Hashable` constraints on generic type parameters.

**Copy-on-write.** Swift Array, Dictionary, Set, and the swift-
collections variants are all COW. Mochi value semantics work
naturally without explicit copy.

**Why OrderedDictionary for group_by.** Mochi's `group_by` produces
output ordered by first-occurrence key. Swift `Dictionary` is
unordered. `OrderedDictionary` from apple/swift-collections (1.1.x)
gives insertion-ordered iteration with O(1) lookup, matching vm3's
behavior.

## 5. Records

Mochi records lower to Swift `struct`:

```mochi
record Point { x: int, y: int }
```

becomes:

```swift
@frozen public struct Point: Sendable, Hashable, Codable {
    public var x: Int64
    public var y: Int64
    public init(x: Int64, y: Int64) {
        self.x = x
        self.y = y
    }
}
```

**Why `@frozen`.** Tells the Swift compiler the struct layout is
ABI-stable, enabling efficient cross-module access without indirect
field offsets. Mochi codegen marks every record `@frozen` because
Mochi records' field set is part of the type identity.

**Why three protocol conformances by default.** `Sendable` because
Mochi records cross actor boundaries freely (all fields must be
Sendable, enforced by Swift). `Hashable` because Mochi records can
be set elements and dictionary keys. `Codable` because Mochi records
serialise to/from JSON via the runtime.

**`with` expressions.** Mochi `point with x: 10` lowers to:

```swift
extension Point {
    public func with(x: Int64? = nil, y: Int64? = nil) -> Point {
        Point(x: x ?? self.x, y: y ?? self.y)
    }
}
```

This per-field-optional pattern keeps the call site readable:
`point.with(x: 10)`. The codegen pass emits one `with` method
per record.

**Memberwise initializer.** Swift's compiler auto-generates a
memberwise init for `struct`. Mochi codegen relies on this and
doesn't emit one explicitly except when public ABI demands
(`@frozen public struct` requires an explicit init for stable
ABI under Library Evolution).

## 6. Sum types

Mochi unions lower to Swift `enum` with associated values:

```mochi
union Tree { Leaf | Branch(left: Tree, value: int, right: Tree) }
```

becomes:

```swift
public indirect enum Tree: Sendable, Hashable, Codable {
    case leaf
    case branch(left: Tree, value: Int64, right: Tree)
}
```

**`indirect`.** Required when the enum is recursive. Mochi codegen
detects recursion and emits `indirect`. The cost is one heap
allocation per recursive case (Swift boxes the payload).

**Lowercased cases.** Swift convention is lowerCamelCase for enum
cases. Mochi `Leaf` → `case leaf`. Case-name mangling table is
emitted alongside for FFI back to Mochi.

**Pattern matching.** Mochi `match` lowers to Swift `switch`:

```swift
switch tree {
case .leaf:
    return 0
case let .branch(left, value, right):
    return f(left) + value + f(right)
}
```

Swift `switch` is exhaustive. Mochi's exhaustiveness check is the
same check; the Swift compiler will repeat it.

**Guards.** Mochi `when` clauses → Swift `where`:

```swift
case let .branch(_, value, _) where value > 0:
    ...
```

## 7. Option and Result

| Mochi               | Swift               |
|---------------------|---------------------|
| `option<T>`         | `T?` (`Optional<T>`)|
| `result<T, E>`      | `Result<T, E>`      |

Both are built-in Swift types. No codegen work beyond direct
lowering.

**Option pattern.** Mochi `if-let some(x) = opt then ...` lowers
to Swift `if let x = opt { ... }`. Mochi `some(x)` → `Optional.some(x)`,
Mochi `none` → `nil`.

**Result pattern.** Mochi `match r { ok(x) -> ...; err(e) -> ... }`
lowers to:

```swift
switch r {
case let .success(x): ...
case let .failure(e): ...
}
```

**Optional chaining.** Mochi `?.` lowers to Swift `?.` directly.

## 8. Function and closure types

Mochi `fun(int, int) -> int` → Swift `(Int64, Int64) -> Int64`.

**`@Sendable`.** Closures crossing actor boundaries must be
`@Sendable`. The codegen pass annotates based on Mochi's purity
analysis: pure closures get `@Sendable`, closures that capture
mutable state don't.

**`@escaping`.** Closures stored in struct fields or otherwise
outliving the call must be `@escaping`. Mochi's escape analysis
identifies these.

**Captures.** Mochi closures capture by value (Swift default for
let bindings) or by reference (for actor-state). Capture lists like
`[x = x, weak self]` are emitted when needed.

**Async closures.** Mochi `async fun(T) -> R` → Swift `(T) async -> R`
or `@Sendable (T) async -> R`.

**Throwing closures.** Typed throws: `(T) throws(E) -> R`.

## 9. Generics and constraints

| Mochi constraint  | Swift constraint           |
|-------------------|----------------------------|
| `T: Eq`           | `T: Equatable`             |
| `T: Ord`          | `T: Comparable`            |
| `T: Hash`         | `T: Hashable`              |
| `T: Show`         | `T: CustomStringConvertible`|
| `T: Copy`         | implicit (Swift values are Copyable unless `~Copyable`) |
| `T: Sendable`     | `T: Sendable`              |

**Multiple constraints.** Mochi `T: Eq + Hash` → Swift `T: Hashable`
(Hashable: Equatable implies Eq + Hash).

**Where clauses.** Mochi `where T: Foo, U: Bar` → Swift
`where T: Foo, U: Bar`.

**Associated types.** Mochi protocols with associated types lower
to Swift protocols with `associatedtype`.

## 10. Protocols (Mochi traits)

Mochi traits lower to Swift `protocol`. Default implementations
become protocol extensions:

```mochi
trait Show { fun show(self): string }
```

becomes:

```swift
public protocol Show {
    func show() -> String
}

extension Show {
    public func showDefault() -> String { String(describing: self) }
}
```

**Protocol existentials.** Swift 5.7+ requires `any Protocol` for
existential types. Mochi `Show` as a value type lowers to `any Show`.

**Opaque return types.** Mochi `-> some Show` → Swift `-> some Show`.

**Conformance synthesis.** `: Hashable, Codable, Sendable` is often
synthesised; Mochi codegen relies on this when all fields conform.

## 11. Non-copyable and non-escapable types

| Mochi                | Swift                  |
|----------------------|------------------------|
| `linear T` (planned) | `~Copyable T`          |
| `borrowed T` (planned) | `~Escapable T`       |

Mochi v1 surface does not expose linear types but the codegen pass
must handle them when added in v2. `~Copyable` types must be either
consumed or borrowed; the Swift compiler enforces this.

## 12. Strings

Mochi `string` → Swift `String`. Mochi assumes UTF-8; Swift's
String is UTF-8-native since 5.7 (SE-0335). No encoding conversion
at boundaries.

**Substrings.** `Substring` is a view into a `String`. Mochi slicing
emits `Substring` for chains, converting to `String` only at the
end via `String(substr)`.

**StringBuilder pattern.** Mochi `string_builder` → Swift
appending. For hot loops, Mochi codegen uses `var s = ""` plus
`s.append(...)` or `String(reserveCapacity:)`.

**Regex literals.** Mochi `regex /.../ ` → Swift `/.../` literal.
Requires Swift 5.7+. The Swift Regex is type-safe with capture
groups as tuple elements.

## 13. Dates and times

| Mochi              | Swift                                       |
|--------------------|---------------------------------------------|
| `instant`          | `Date` (Foundation)                         |
| `duration`         | `TimeInterval` (typealias for `Double`)     |
| `zoned_datetime`   | `MochiRuntime.ZonedDateTime` (custom)       |
| `local_date`       | `MochiRuntime.LocalDate` (custom)           |

**Why custom ZonedDateTime.** Foundation `Date` is wall-clock only;
it doesn't carry a time zone. Mochi requires zoned datetime
serialisation. The runtime ships a `ZonedDateTime` struct with a
`Date` plus a `TimeZone`, Codable to RFC 3339.

## 14. JSON values

Mochi JSON literal → `MochiRuntime.JSONValue` enum:

```swift
public indirect enum JSONValue: Sendable, Hashable, Codable {
    case null
    case bool(Bool)
    case int(Int64)
    case double(Double)
    case string(String)
    case array([JSONValue])
    case object(OrderedDictionary<String, JSONValue>)
}
```

**OrderedDictionary for objects.** JSON object key order is
preserved (matches vm3 behavior).

**Codable conformance.** Custom encode/decode using SingleValueContainer
to dispatch by JSON node type.

## 15. Logic terms (Datalog)

Mochi facts and rules lower to:

```swift
public struct Predicate: Hashable, Sendable {
    public let name: String
    public let arity: Int
}

public struct Fact: Hashable, Sendable {
    public let predicate: Predicate
    public let args: [Term]
}

public indirect enum Term: Hashable, Sendable {
    case constant(JSONValue)
    case variable(String)
    case functor(name: String, args: [Term])
}
```

Rule evaluation uses tabled SLG resolution implemented in
MochiRuntime. See [[01-language-surface]] §8.

## 16. Boundary types (FFI)

| Mochi extern            | Swift attribute                       |
|-------------------------|---------------------------------------|
| `extern fun foo() -> ...` from C | `@_silgen_name("foo") func foo()` |
| Mochi function exported to C | `@_cdecl("mochi_foo") public func mochiFoo() -> ...` |
| C struct                | Swift `struct` matching C layout      |
| C union                 | Manual via `UnsafeMutableRawPointer`  |
| C function pointer      | `@convention(c) (T) -> R`             |

**Module map.** For C library imports, Mochi emits a `module.modulemap`
declaring the C module. Swift `import CLibrary` then sees the C
symbols. The Mochi codegen pass writes the module.modulemap to
`out/Sources/<Module>.modulemap`.

**Bridging headers.** Apple-platform-only mechanism. Mochi v1 uses
module maps consistently (Linux/Windows compatible).

## 17. Sendability inference

Mochi codegen infers Sendable conformance:

- All primitive types: Sendable.
- Records where all fields are Sendable: Sendable.
- Enums where all associated values are Sendable: Sendable.
- Closures with no captures or only Sendable captures: `@Sendable`.
- Actor types: Sendable by definition.
- Classes: Sendable only if all stored properties are Sendable and
  immutable (let bindings) or protected by a lock.

The codegen pass emits explicit `: Sendable` on every type that
should be sendable. The Swift compiler verifies the conformance at
build time; a Mochi codegen bug that misses a non-Sendable field
fails the build, not just runtime.

## 18. Library Evolution mode

For Mochi.Runtime and other publicly versioned packages, Mochi
codegen emits with `-enable-library-evolution`. This forces:

- All public types `@frozen` or non-frozen explicitly.
- All public functions have explicit return types (no `some`).
- Public stored properties have backing computed-property
  trampolines (compiler-handled).

Mochi.Runtime targets stable ABI across Swift minor versions.
User code (Mochi → Swift output for an application) does not use
Library Evolution; it's a per-package decision in Package.swift.

## 19. Type-erasure helpers

Where Mochi needs runtime polymorphism (heterogeneous lists), Swift
provides `AnyHashable`, `AnySequence`, `AnyIterator`, etc. Mochi
codegen prefers `any Protocol` over type-erased wrappers when
Swift 5.7+ existential types suffice.

For protocols with `Self` requirements or associated types, an
existential is invalid; Mochi codegen falls back to a type-erased
wrapper.

## 20. Summary table

| Mochi kind          | Swift kind                               | Sendable | Copyable | ABI-frozen by default |
|---------------------|------------------------------------------|----------|----------|-----------------------|
| primitive           | Int64/Double/Bool/String/Data            | yes      | yes      | yes                   |
| record              | @frozen struct                           | yes      | yes      | yes                   |
| sum                 | enum                                     | yes      | yes      | yes                   |
| trait               | protocol                                 | n/a      | n/a      | yes                   |
| generic fun         | generic func                             | n/a      | n/a      | n/a                   |
| closure             | (T) -> R, +@Sendable as needed           | inferred | yes      | n/a                   |
| collection          | Array/Dictionary/Set/Ordered* (COW)      | yes      | yes      | yes                   |
| optional            | Optional<T>                              | inferred | yes      | yes                   |
| result              | Result<T, E>                             | inferred | yes      | yes                   |
| agent               | actor                                    | yes      | no (reference) | no             |
| stream              | AsyncSequence (AsyncStream<E>)           | yes      | yes      | n/a                   |
| extern fun          | @_silgen_name / @_cdecl func             | n/a      | n/a      | n/a                   |
| linear (v2)         | ~Copyable struct                         | yes      | no       | yes                   |

Cross-references: [[01-language-surface]] for the Mochi side;
[[05-codegen-design]] for the emit-pass mechanics;
[[09-agent-streams]] for actor/AsyncStream details;
[[11-testing-gates]] for the type-mapping regression gate.
