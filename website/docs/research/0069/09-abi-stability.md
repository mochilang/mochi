---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "@_cdecl guarantees on Apple platforms and Linux, opaque-handle strategy for non-@_cdecl-compatible value types, String and [T] round-trip ABI, drop semantics across the wrapper boundary (Swift ARC vs Mochi GC), static archive vs dylib decision, and Swift ABI stability per-platform."
---

# 09. ABI stability

## `@_cdecl` guarantees

The `@_cdecl("symbol_name")` attribute marks a Swift function as exported with:
- C calling convention (no Swift ABI mangling).
- The exact symbol name specified (no further mangling).
- Stability across Swift minor releases (the attribute is part of Swift's ABI stability commitment on Apple platforms).

On Apple platforms (macOS 10.14.4+, iOS 12.2+, tvOS 12.2+, watchOS 5.2+), Swift ABI is fully stable: a binary compiled with Swift 5.0 is ABI-compatible with one compiled with Swift 5.10 without recompilation. `@_cdecl` symbols exported from a Swift 5.1 binary remain callable from a Swift 5.10 binary with no changes.

On Linux, Swift ABI is **not stable** across major Swift releases. A `@_cdecl` symbol exported from a library built with Swift 5.10 cannot be called from a binary linked against Swift 6.0's runtime. The bridge enforces toolchain version consistency on Linux via the `swift-tools-version` lockfile field; all packages in a given `mochi.lock` must use the same toolchain major version.

## Opaque-handle memory model

Non-trivial Swift types (classes, non-`@frozen` structs, enums with associated values beyond the closed table) are represented as opaque handles: `UnsafeMutableRawPointer` values that point to a heap-allocated `MochiHandle_<Type>` Swift class instance.

```swift
// Generated for every wrapped reference or non-frozen value type T
public final class MochiHandle_T {
    public let value: T
    public init(_ v: T) { self.value = v }
    deinit { /* Swift ARC cleans up `value` here */ }
}
```

The lifecycle is:

1. **Creation.** The `@_cdecl` factory function allocates a `MochiHandle_T` via `Unmanaged<MochiHandle_T>.passRetained(MochiHandle_T(value)).toOpaque()`. `passRetained` increments the retain count to 1 and surrenders ownership to the raw pointer (Swift ARC no longer manages the object; the pointer owns it).

2. **Mochi ownership.** The Mochi runtime holds the raw pointer as an opaque value. Mochi's GC does not know the pointer is a heap allocation; it treats it as an integer. To prevent leaks, the Mochi runtime calls the `_release` symbol when the opaque value falls out of Mochi scope.

3. **Release.** The `@_cdecl` release function calls `Unmanaged<MochiHandle_T>.fromOpaque(ptr).release()`. This decrements the retain count; when the count reaches 0, Swift ARC calls `MochiHandle_T.deinit`, which releases `value` and deallocates the `MochiHandle_T` instance.

4. **Retain (for shared ownership).** If the same opaque handle is passed to multiple Mochi variables (aliased), the `_retain` symbol is called. `Unmanaged<MochiHandle_T>.fromOpaque(ptr).retain()` increments the retain count.

The key invariant: **the Mochi runtime must call `_release` exactly once for each live opaque handle, and `_retain` once for each additional owner**. The synthesised Mochi shim file declares `extern fn <T>_free` and `extern fn <T>_retain` and annotates them with lifetime hints that the MEP-53 Swift emit pass uses to insert calls at appropriate scope exit points.

## `String` round-trip ABI

`Swift.String` → C → Mochi:

1. The `@_cdecl` wrapper calls `withCString { ptr in ... }` to materialise a temporary `const char*` UTF-8 pointer.
2. Because `withCString`'s lifetime ends at the closure boundary, the bridge cannot directly return the pointer. Instead, it copies the UTF-8 bytes to a heap allocation: `let copy = strdup(ptr)`, returning `UnsafePointer<CChar>` (the pointer) plus `Int` (the byte count) as a C struct.
3. The Mochi runtime owns the allocation and calls `mochi_swift_string_free(ptr)` when the Mochi `string` value falls out of scope. The `_free` symbol calls `Foundation.free(ptr)` (or the Swift allocator's dealloc if `strdup` uses a different allocator; the bridge uses `malloc` explicitly for portability).

Mochi `string` → C → Swift:

1. The Mochi runtime passes a `const char*` pointer + length to the `@_cdecl` wrapper.
2. The wrapper constructs a Swift `String` via `String(bytes: UnsafeBufferPointer(start: ptr, count: len), encoding: .utf8)!`. This copies the bytes into Swift-owned storage.
3. The original `const char*` remains valid for the duration of the call; the Mochi runtime owns and manages it.

The round-trip is copy-based (no zero-copy path for strings in v1). This matches MEP-73's strategy for Rust `String` and MEP-68's strategy for .NET `string`.

## `[T]` (Array) round-trip ABI

`Swift.[T]` → C → Mochi (when T is a C-ABI-compatible type):

The bridge uses a three-word C struct `(base: UnsafeRawPointer, count: Int, element_size: Int)` plus a `_free` symbol. The `base` pointer points to a contiguous C-layout copy of the array elements. Element stride (`element_size`) is the C size of the translated element type; for `[Int]`, it is 8 (C `int64_t`). The Mochi `list<T>` value owns the allocation.

For `[String]` (and other types that are not themselves C-ABI-compatible), the bridge serialises to a null-terminated pointer array: each element is copied to a heap allocation, and the array is a `char**` terminated by a null pointer. The `_free` symbol frees each element allocation plus the outer array.

## Static archive vs dylib

The synthesised wrapper package is built as a **static library** (`TYPE_LIBRARY_STATIC` in SPM). The final binary is produced by the MEP-53 build driver linking the static library directly into the executable (or the user's library). A `dylib`/`.so` wrapper would:

- Require distributing the wrapper alongside the binary.
- Require the Swift standard library to be available at runtime on Linux (unless statically linked).
- Introduce `dlopen` symbol-resolution order complexity when multiple Mochi packages import the same Swift package.

Static linking resolves all these issues: the final binary is self-contained, all `@_cdecl` symbols are resolved at link time, and there is no runtime dependency on Swift's dynamic library loader.

The trade-off: static linking increases binary size. For a Mochi program importing Alamofire, swift-log, and swift-crypto, the static archive adds approximately 8 MB to the binary (vs 2 MB for a dylib on macOS, where the Swift standard library is already in the system runtime). The bridge accepts this trade-off for correctness and portability.

## Cross-references

- [[05-type-mapping]] for the type-level guarantees that the ABI layer implements.
- [[08-async-bridge]] for the memory safety of opaque handles across async dispatch.
- [[11-xcframework-and-multiplatform]] for the ABI slices bundled in an XCFramework.
- Swift [ABI stability manifesto](https://github.com/apple/swift/blob/main/docs/ABIStabilityManifesto.md) — the authoritative Apple document on Swift ABI stability.
