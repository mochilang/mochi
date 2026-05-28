---
title: "Phase 12. FFI (module maps, @_silgen_name)"
sidebar_position: 16
sidebar_label: "Phase 12. FFI"
description: "MEP-49 Phase 12 — C FFI via @_silgen_name and module.modulemap; Swift library import via @_implementationOnly; Objective-C bridging header."
---

# Phase 12. FFI (module maps, @_silgen_name)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 12](/docs/mep/mep-0049#phase-12-ffi) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase12FFI`: 25 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

FFI is the escape hatch that makes Mochi practical for real iOS/macOS/Linux applications. Phase 12 ships the three FFI paths: (1) calling C functions via `module.modulemap`, (2) calling Swift libraries directly as SwiftPM dependencies, (3) exporting Mochi functions to C callers via `@_cdecl`. This enables the ecosystem integrations (SQLite, OpenSSL, system APIs) that the other phases depend on.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | C FFI: `module.modulemap` → ClangImporter; `@_silgen_name` for name mangling; unsafe pointer bridging | NOT STARTED | — |
| 12.1 | Swift library FFI: `external module "github.com/..."` → SwiftPM `.package` + `.product` dependency | NOT STARTED | — |
| 12.2 | Export to C: `@export fun foo()` → `@_cdecl("foo") public func foo()` | NOT STARTED | — |
| 12.3 | Objective-C bridging: `@objc` annotation; bridging header; `NSObject` subclassing | NOT STARTED | — |
| 12.4 | Unsafe Swift: `UnsafePointer<T>`, `UnsafeMutablePointer<T>`, `UnsafeRawPointer`; pointer arithmetic | NOT STARTED | — |

## Sub-phase 12.0 -- C FFI via module map

### Decisions made (12.0)

**`module.modulemap`**: Mochi `@c_import "sqlite3"` → generates a `module.modulemap` file and adds it to the SwiftPM target's `CSettings`:

```
// Generated: Sources/CModuleSqlite3/module.modulemap
module CSqlite3 {
    header "sqlite3.h"
    export *
}
```

The SwiftPM target includes a system library target:

```swift
// In Package.swift:
.systemLibrary(
    name: "CSqlite3",
    path: "Sources/CModuleSqlite3",
    pkgConfig: "sqlite3",
    providers: [.brew(["sqlite"]), .apt(["libsqlite3-dev"])]
)
```

**`@_silgen_name`**: for calling C functions with non-Swift-mangled names:

```swift
// Mochi: @c_func extern fun sqlite3_open(filename: string, db: ptr<ptr<sqlite3>>): int
@_silgen_name("sqlite3_open")
public func sqlite3Open(_ filename: UnsafePointer<CChar>?, _ db: UnsafeMutablePointer<OpaquePointer?>?) -> Int32
```

The lowerer emits `@_silgen_name` when the Mochi FFI declaration specifies the exact C symbol name.

**ClangImporter**: SwiftPM uses ClangImporter to import C headers into Swift. This is automatic when a `.systemLibrary` or `.target` with `publicHeadersPath` is declared in `Package.swift`. The Mochi lowerer generates the necessary SwiftPM target declarations.

**Type bridging at FFI boundary**:
- Mochi `string` → C `const char*`: `s.withCString { ptr in cFunc(ptr) }` (safe, no copy).
- Mochi `int` → C `int32_t`: `Int32(n)`.
- Mochi `list<int>` → C array: `xs.map { Int32($0) }.withUnsafeBufferPointer { buf in cFunc(buf.baseAddress!, Int32(buf.count)) }`.
- C `int` return → Mochi `int`: `Int64(cReturn)`.

## Sub-phase 12.1 -- Swift library FFI

### Decisions made (12.1)

**`external module` in Mochi**: `external module "https://github.com/apple/swift-crypto" version: "3.0.0"` → SwiftPM dependency added to the generated `Package.swift`:

```swift
.package(url: "https://github.com/apple/swift-crypto", from: "3.0.0"),
```

And the product added to the executable target's dependencies:

```swift
.product(name: "Crypto", package: "swift-crypto"),
```

**Mochi `use` declaration**: `use Crypto.SHA256` → `import Crypto` in the emitted `.swift` file. The lowerer generates the import statement.

**Type forwarding**: Swift types from the external library are used directly in Mochi via type annotations: `let hash: Crypto.SHA256.Digest = SHA256.hash(data: ...)`. The Mochi type system treats imported Swift types as opaque `extern<T>` types.

**`@_implementationOnly import`**: for internal implementation dependencies that should not be re-exported from the Mochi module, the lowerer emits `@_implementationOnly import ExternalLib`. This is the swift-evolution approved way to hide transitive dependencies.

## Sub-phase 12.2 -- Export to C

### Decisions made (12.2)

**`@export fun foo()`**: Mochi `@export fun add(a: int, b: int): int => a + b` → Swift with `@_cdecl`:

```swift
@_cdecl("mochi_add")
public func mochiAdd(_ a: Int64, _ b: Int64) -> Int64 {
    return add(a, b)
}
```

The C symbol name is `mochi_` + the Mochi function name by default. The Mochi source can override with `@export("my_symbol_name") fun foo()`.

**Generated C header**: the lowerer generates a `MochiExports.h` header for C callers:

```c
// MochiExports.h (auto-generated)
#pragma once
#include <stdint.h>
int64_t mochi_add(int64_t a, int64_t b);
```

**Thread safety**: `@_cdecl` functions are called from C, potentially from non-Swift threads. They are `nonisolated` by nature. The lowerer adds a `Task` wrapper when the function needs to interact with actors.

## Sub-phase 12.3 -- Objective-C bridging

### Decisions made (12.3)

**`@objc` annotation**: Mochi `@objc record MyView { ... }` → Swift class with `@objc`:

```swift
@objc
public final class MyView: NSObject {
    @objc public var title: String
    @objc public init(title: String) { self.title = title }
}
```

Records become `NSObject` subclasses when `@objc` is applied. The `@frozen struct` from Phase 4 is converted to a `final class` for Objective-C compatibility.

**Bridging header**: for mixed Obj-C/Swift targets (iOS projects using storyboards), the lowerer generates a bridging header `Mochi-Bridging-Header.h` and adds it to the `xcodebuild` settings.

**`NSCopying`**: `@objc` records that need to be stored in `NSArray` or `NSDictionary` get a `copy(with:)` implementation synthesised by the lowerer.

## Sub-phase 12.4 -- Unsafe pointer operations

### Decisions made (12.4)

**`UnsafePointer<T>`**: Mochi `unsafe_ptr<T>` type → Swift `UnsafePointer<T>`. Used for passing buffers to C APIs.

**`UnsafeMutablePointer<T>`**: Mochi `unsafe_mut_ptr<T>` → Swift `UnsafeMutablePointer<T>`.

**`UnsafeRawPointer`**: Mochi `raw_ptr` → Swift `UnsafeRawPointer`. For untyped memory (e.g., `malloc` return).

**`withUnsafePointer(to:_:)` scope**: all unsafe pointer access is scoped. The lowerer emits `withUnsafePointer(to: &val) { ptr in ... }` to ensure the pointer is only valid within the scope.

**`UnsafeBufferPointer<T>`**: `withUnsafeBufferPointer` for accessing array storage as a C-style pointer + length pair.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/ffi.go` | `@c_import`, `external module`, `@export`, `@objc` lowering |
| `transpiler3/swift/build/package.go` | External SwiftPM dependency injection from `external module` declarations |
| `transpiler3/swift/lower/unsafe.go` | `unsafe_ptr` / `raw_ptr` type lowering; `withUnsafePointer` scoping |
| `transpiler3/swift/emit/emit.go` | `module.modulemap` file generation; bridging header generation; `MochiExports.h` |
| `transpiler3/swift/build/phase12_test.go` | `TestPhase12FFI`: 25 fixtures |
| `tests/transpiler3/swift/fixtures/phase12-ffi/` | 25 fixture directories |

## Test set

- `TestPhase12FFI` -- 25 fixtures covering: `ffi_c_open_close`, `ffi_c_string_to_cstring`, `ffi_c_int_roundtrip`, `ffi_c_buffer`, `ffi_c_callback`, `ffi_c_struct`, `ffi_c_opaque_pointer`, `ffi_swift_lib_import`, `ffi_swift_type_use`, `ffi_export_basic`, `ffi_export_string`, `ffi_export_int64`, `ffi_objc_class`, `ffi_objc_method`, `ffi_objc_nsobject`, `ffi_unsafe_pointer`, `ffi_unsafe_buffer`, `ffi_unsafe_raw`, `ffi_unsafe_scope`, `ffi_c_errno`, `ffi_c_malloc_free`, `ffi_module_map_system`, `ffi_clang_import`, `ffi_mixed_swift_c`, `ffi_pkg_config`.

## Deferred work

- Swift-to-JVM interop (Android via KMP). Out of v1 scope.
- COM interface export (Windows). Deferred to a Phase 12 sub-MEP.
- Swift Package Index registry resolution. Deferred (manually specified URLs for now).
- `@convention(c)` function pointer type. Deferred to a Phase 12 extension.
