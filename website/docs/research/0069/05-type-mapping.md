---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping table"
description: "The complete closed Swift-to-Mochi translation table, refusal cases, generic monomorphisation rule, Optional<T> vs T? handling, Result<T,E> and throws desugar, the class-as-opaque-handle strategy, protocol-as-opaque-handle, and the throws error propagation ABI."
---

# 05. Type mapping table

## The closed translation table

The bridge translates Swift types to Mochi types using the following closed table. Items not in the table are refused (see §Refusal cases below).

### Primitive types

| Swift type | Mochi type | ABI convention | Notes |
|-----------|-----------|---------------|-------|
| `Swift.Int` | `int` | C `int64_t` | Swift `Int` is pointer-width; MEP-69 defines it as 64-bit. 32-bit Swift targets are refused. |
| `Swift.Int64` | `int` | C `int64_t` | Exact. |
| `Swift.Int32` | `int` | C `int32_t` → sign-extended to `int64_t` | Translated with truncation warning in SkipReport. |
| `Swift.Double` | `float` | C `double` | Exact. |
| `Swift.Float` | `float` | C `float` → widened to `double` | Precision loss noted in SkipReport. |
| `Swift.Bool` | `bool` | C `_Bool` | Exact. |
| `Swift.String` | `string` | `(UnsafePointer<CChar>, Int)` + `_free` symbol | UTF-8; the bridge allocates with `strdup`-equivalent and the Mochi side owns the allocation. |
| `Swift.Void` / `()` | `unit` | C `void` | Exact. |
| `Swift.Character` | `string` | Single-UTF8-codepoint string | Treated as a one-character string. |
| `Swift.UInt8` | `int` | C `uint8_t` → zero-extended | |
| `Swift.UInt16` | `int` | C `uint16_t` → zero-extended | |
| `Swift.UInt32` | `int` | C `uint32_t` → zero-extended | |
| `Swift.UInt64` | `int` | C `uint64_t` | Overflow possible for values > `Int64.max`; documented. |

### Collection types

| Swift type | Mochi type | ABI convention |
|-----------|-----------|---------------|
| `[T]` (Array) | `list<T>` | `(UnsafeRawPointer, Int, Int)` triple: base pointer / count / element stride. Plus `_free` symbol. T must be in-table. |
| `[K: V]` (Dictionary) | `map<K, V>` | Serialised as interleaved key/value pairs in a flat C array. K must be `String` or an integer type. V must be in-table. |
| `Set<T>` | `set<T>` | Serialised as a flat C array of elements. T must be `Hashable` and in-table. |
| `Data` | `bytes` | `(UnsafeRawPointer, Int)` pair + `_free` symbol. Alias for `[UInt8]` in the ABI. |
| `Substring` | `string` | Copied to `String` at the wrapper boundary. |

### Optional and result types

| Swift type | Mochi type | ABI convention |
|-----------|-----------|---------------|
| `T?` (Optional) | `T\|nil` | Presence flag (`Int32` 1/0) plus `T` out-pointer. When presence is 0, the out-pointer is null. |
| `Result<T, E>` | `try-catch` | Status `Int32` (0=ok, 1=error) plus two out-pointers: one for `T`, one for the error description string. |
| `throws` functions | `try-catch` | Treated as `Result<T, Error>` where the error is stringified via `localizedDescription`. |
| `throws(E)` functions (Swift 6) | `try-catch` | Typed error; E is translated via the table; error is passed as-is if in-table, stringified otherwise. |

### Struct types

A Swift `public struct S { public var a: A; public var b: B }` where all field types are in-table maps to a Mochi record:

```mochi
record S { a: A; b: B }
```

ABI: the `@_cdecl` wrapper emits a C struct that mirrors the Swift struct's layout (field-by-field, same order, using the C-ABI types from this table). Swift structs with a stable memory layout (structs that do not contain resilient types, which includes all `@frozen` structs and all structs whose fields are all primitive types or other in-table structs) can be passed by value across the C boundary. Non-frozen structs (all structs in non-`@frozen` packages, which is most library packages that are not in the Swift standard library) are passed via an opaque handle instead.

**Frozen-struct detection:** The bridge checks for `@frozen` attribute in the `.swiftinterface`. If present, the struct is translated as a record (by-value). If absent, the struct is translated as an opaque handle (see §Opaque handles below). This is conservative: non-frozen structs may have a stable layout in practice but cannot be guaranteed across package versions.

### Enum types

| Swift enum form | Mochi type | Notes |
|----------------|-----------|-------|
| C-like `enum E { case v1, v2, v3 }` | `type E = v1 \| v2 \| v3` (ADT, no payload) | ABI: `Int32` discriminant. |
| `@frozen` C-like enum | Same as above | `@frozen` is noted; the ABI is identical. |
| Associated-value `enum E { case ok(A); case err(B) }` where A, B in-table | `type E = ok(A) \| err(B)` | ABI: discriminant `Int32` plus a union of the largest payload. |
| `RawRepresentable` enum with `RawValue = String` | `type E = v1 \| v2` + `extern fn e_rawValue(e: E): string` | The raw value accessor is synthesised. |
| Recursive enums (`indirect enum`) | Opaque handle | Recursive layout cannot be represented in Mochi's record system. |
| Enums with more than 16 cases | Opaque handle | Large discriminant enums are refused for direct translation; too much wrapper code. |

### Class types

Swift classes are reference types with Swift ARC (Automatic Reference Counting). They cannot be passed by value across the C boundary.

**Opaque handle strategy:** A Swift class instance is wrapped in a heap-allocated `MochiHandle` Swift class that holds a strong reference to the original instance. The `MochiHandle` is boxed as an `UnsafeMutableRawPointer` on the C side. The wrapper emits a `_retain` symbol (increments the `MochiHandle`'s retain count) and a `_release` symbol (decrements it, triggering dealloc when the count reaches zero). The Mochi runtime calls `_release` when the opaque handle falls out of scope.

```swift
// Generated for: public final class Session
public class MochiHandle_Session {
    public let value: Alamofire.Session
    public init(_ v: Alamofire.Session) { self.value = v }
}

@_cdecl("mochi_Alamofire_Session_retain")
public func mochi_Alamofire_Session_retain(_ ptr: UnsafeMutableRawPointer) {
    let h = Unmanaged<MochiHandle_Session>.fromOpaque(ptr).retain()
    _ = h
}

@_cdecl("mochi_Alamofire_Session_release")
public func mochi_Alamofire_Session_release(_ ptr: UnsafeMutableRawPointer) {
    Unmanaged<MochiHandle_Session>.fromOpaque(ptr).release()
}
```

Non-`final` class types whose subclasses might override methods create aliasing issues (the handle might hold a subclass instance). The bridge generates a `SkipReport` for non-`final` class types unless the user opts in via `[swift.capabilities] non-final-class = true`.

### Protocol types

Protocols as parameter types (e.g., `func foo(_ p: any MyProtocol)`) are represented via opaque handles with the same retain/release pattern as classes. The actual conforming type is hidden behind the handle. Protocols in return position using `any P` existential syntax are represented the same way.

Protocols with associated types (`associatedtype`) are refused unless the user specifies a concrete monomorphisation in `[swift.monomorphise]`.

### Tuple types

| Swift type | Mochi type |
|-----------|-----------|
| `(A, B)` | `tuple<A, B>` | ABI: flat struct with field types. Both A and B must be in-table. |
| `(A, B, C)` | `tuple<A, B, C>` | Same. |
| Tuples with more than 4 elements | Refused. |
| Labeled tuples `(a: A, b: B)` | `tuple<A, B>` (labels stripped in the ABI). |

### Closure types

Closures in Swift carry a context pointer and are not directly representable as C function pointers unless they are `@convention(c)`. The bridge handles closures in two ways:

1. **`@convention(c)` closures:** Translated directly as C function pointer + context pointer pairs.
2. **`@escaping` closures with declared arity:** If the user lists the closure type in `[swift.monomorphise]` with explicit parameter types, the bridge emits a wrapper that materialises the closure as a heap object with an `invoke` method.
3. **All other closures:** `SkipReport`.

## Refusal cases

The bridge emits a `SkipReport` entry for any item that cannot be translated:

| Refused item | Reason |
|-------------|--------|
| `some P` opaque return types without a concrete monomorphisation | No Mochi representation for existential-erased return. |
| `any P` in return position | Existential container returned by value; layout unknown at compile time. |
| Non-`@frozen` struct (by default) | Layout may change across package versions. Passes as opaque handle instead (treated as class handle). |
| Non-`final` class | Subclass aliasing. |
| `@escaping` closure without declared arity | Function pointer layout unknown. |
| `inout` parameters | Mochi has no `inout` semantics. |
| Variadic parameters (`T...`) | `CVarArg` protocol required; no Mochi equivalent. |
| Generic functions without a `[swift.monomorphise]` entry | Would require a separate `@_cdecl` symbol per instantiation. |
| Protocol with associated types without a `[swift.monomorphise]` entry | Same. |
| `@MainActor`-isolated functions (by default) | Risk of deadlock from the `DispatchGroup` wrapper; opt-in via `[swift.capabilities] main-actor = true`. |
| Swift macros (`@freestanding`, `@attached`) | Compile-time only; no ABI entry point. |
| `@_alwaysEmitIntoClient` items | Inlined; no ABI entry point to call. |
| `withUnsafePointer` / `withUnsafeMutableBytes` items | Requires `[swift.capabilities] unsafe = true` opt-in. |
| `@objc` protocol types | Requires Objective-C runtime; `[swift.capabilities] objc = true` to opt in. |
| Operator declarations | No Mochi operator FFI syntax. |
| Subscript declarations (by default) | No Mochi subscript syntax; future: generate named `get_`/`set_` functions. |

## Generic monomorphisation

The `[swift.monomorphise]` table in `mochi.toml` allows the user to enumerate explicit generic instantiations:

```toml
[swift]
monomorphise = [
    { item = "swift-collections.OrderedDictionary", K = "String", V = "Int" },
    { item = "swift-algorithms.chunks", Element = "String" },
    { item = "Alamofire.DataRequest.responseDecodable", T = "UserResponse" },
]
```

For each entry, the bridge emits a separate `@_cdecl` wrapper with the concrete type substituted:

```swift
@_cdecl("mochi_SwiftCollections_OrderedDictionary_String_Int_init")
public func mochi_SwiftCollections_OrderedDictionary_String_Int_init() -> UnsafeMutableRawPointer {
    let d = OrderedDictionary<String, Int>()
    return Unmanaged<MochiHandle_OD_String_Int>.passRetained(MochiHandle_OD_String_Int(d)).toOpaque()
}
```

The monomorphisation table is the only path to import a generic Swift item. The bridge does not auto-monomorphise (the combinatorial explosion for packages like `swift-collections` would generate thousands of wrappers).

## The `throws` ABI in depth

Swift's `throws` keyword marks a function that can return an error of type `any Error`. The ABI convention the bridge uses:

```
C signature: int32_t mochi_Pkg_fn(/* args */, void** out_value, void** out_error)
Returns: 0 = success (out_value set, out_error null)
         1 = error (out_error set to a heap-allocated C string of error.localizedDescription, out_value null)
```

The Mochi extern declaration maps this to a `try-catch` desugar:

```mochi
extern fn pkg_fn(args...): string throws from swift "Pkg.fn"
# internally: extern fn pkg_fn_raw(args..., out_val: ptr, out_err: ptr): int
```

For Swift 6 typed throws (`throws(E)` where E is an in-table type), the error out-pointer carries the translated E value rather than a stringified `localizedDescription`.

## Cross-references

- [[04-swiftinterface-ingest]] for how the `Module` AST is produced.
- [[06-spm-git-resolution]] for the `[swift.monomorphise]` table's interaction with the lock step.
- [[09-abi-stability]] for ABI guarantees at the C boundary.
- [[10-generics-and-opaque-types]] for deeper treatment of generics and `some`/`any`.
