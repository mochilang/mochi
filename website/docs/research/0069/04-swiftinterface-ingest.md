---
title: "04. .swiftinterface ingest"
sidebar_position: 5
sidebar_label: "04. .swiftinterface ingest"
description: "The .swiftinterface textual format, the swift-tools-version header, the grammar for type signatures and generic constraints, stability guarantees across major Swift releases, the Go-side parser shape, and the difference between .swiftmodule (binary, not stable) and .swiftinterface (textual, stable)."
---

# 04. .swiftinterface ingest

## What `.swiftinterface` is

A `.swiftinterface` file is the textual, compiler-parseable representation of a Swift module's public API. It was introduced in SE-0235 (Swift 5.1, 2019) to solve the *module stability* problem: before SE-0235, consuming a pre-compiled Swift module (via a `.swiftmodule` file) required the exact same Swift compiler version used to build it, making binary framework distribution fragile. The `.swiftinterface` format decouples the public API description from the compiler binary format.

The `.swiftinterface` file looks like a Swift source file, but it is compiler-generated and is a strict subset of Swift syntax:

```swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 5.10 (swiftlang-5.10.0.13 clang-1500.3.9.4)
// swift-module-flags: -module-name Alamofire -O -disable-objc-attr-requires-objc-root
// swift-module-flags-ignorable: -enable-bare-slash-regex ...
import Foundation
import _Concurrency
public typealias HTTPMethod = Alamofire.HTTPMethod
public struct Request : @unchecked Swift.Sendable {
    public var description: Swift.String { get }
    public func response(completionHandler: @escaping (Alamofire.AFDataResponse<Swift.Data>) -> Swift.Void) -> Self
    public func responseDecodable<T>(of type: T.Type = T.self, queue: Dispatch.DispatchQueue = .main,
                                    decoder: any Alamofire.DataDecoder = JSONDecoder(),
                                    completionHandler: @escaping (Alamofire.AFDecodableResponse<T>) -> Swift.Void) -> Self
        where T : Swift.Decodable
}
public final class Session : @unchecked Swift.Sendable {
    public static let `default`: Alamofire.Session
    public func request(_ convertible: any Alamofire.URLConvertible,
                        method: Alamofire.HTTPMethod = .get,
                        parameters: (any Alamofire.Parameters)? = nil) -> Alamofire.DataRequest
}
```

Key properties:

- The `// swift-interface-format-version:` header encodes the format major version (currently `1.0` for all Swift 5.x and 6.x releases; the bridge validates this header).
- The `// swift-compiler-version:` header records the exact Swift toolchain that generated it. The bridge records this in the lockfile's `swift-tools-version` field.
- The `// swift-module-flags:` and `// swift-module-flags-ignorable:` headers carry compile flags the parser must understand. Flags on the `ignorable` line are forward-compatible; an older parser can skip them safely.
- The body is Swift declarations (`struct`, `class`, `enum`, `func`, `var`, `typealias`, `protocol`, `extension`, etc.) with full type annotations and generic constraints.
- Only `public` and `open` items appear; `internal` and `private` items are absent.

## `.swiftinterface` vs `.swiftmodule`

| Property | `.swiftmodule` | `.swiftinterface` |
|----------|---------------|------------------|
| Format | Binary (LLVM bitcode-based) | Text |
| Stability | Unstable across Swift versions | Stable across minor versions |
| Toolchain coupling | Exact version match required | Compatible within major version |
| Machine-readable? | Requires Swift's own decoder | Parseable with a Go-side text parser |
| Ships with SDK? | Often | Always (Apple SDK ships both) |

The bridge reads `.swiftinterface`, not `.swiftmodule`. Binary `.swiftmodule` files are not guaranteed stable even across minor Swift releases; Apple explicitly documents that binary modules should not be committed to version control or distributed as API descriptions.

## How `.swiftinterface` files are produced

At lock time (`mochi pkg lock`), the bridge:

1. Shallow-clones the package source at the pinned tag (or fetches from the SE-0292 registry).
2. Runs `swift build --configuration release --target <module>` in the cloned directory. This is the minimal build needed to produce the `.swiftinterface` files.
3. Reads the `.swiftinterface` files from `.build/<triple>/release/Modules/<Module>.swiftmodule/<triple>.swiftinterface`.
4. Falls back to `swiftc -emit-module-interface-path <out.swiftinterface> -module-name <M> -typecheck <sources...>` if the package layout does not produce the standard `.build/` structure.

Alternative: for Apple SDK frameworks (UIKit, SwiftUI, etc.), the `.swiftinterface` files are pre-built and ship in the Xcode SDK at `$(xcrun --show-sdk-path)/usr/lib/swift/<framework>.swiftmodule/<arch>-apple-macos.swiftinterface`. The bridge reads these directly when the user declares an Apple platform target; no compilation is needed for SDK frameworks.

## Go-side parser architecture

The `.swiftinterface` parser lives in `package3/swift/swiftinterface/`. It is a purpose-built recursive-descent parser for the subset of Swift syntax that appears in `.swiftinterface` files; it does not aim to be a general Swift parser.

The parser produces a `Module` struct:

```go
type Module struct {
    Name         string
    CompilerVersion string
    ToolsVersion string
    Declarations []Decl
}

type Decl interface{ declNode() }

type FuncDecl struct {
    Name       string
    GenericParams []GenericParam
    Params     []Param
    Result     TypeExpr
    IsAsync    bool
    IsThrowing bool
    IsStatic   bool
    IsMutating bool
    Attrs      []Attr  // @_cdecl, @available, @discardableResult, etc.
}

type StructDecl struct {
    Name       string
    GenericParams []GenericParam
    Fields     []VarDecl
    Methods    []FuncDecl
    Conformances []string  // protocol names
}
// ... EnumDecl, ClassDecl, ProtocolDecl, TypealiasDecl, ExtensionDecl
```

The parser handles:

- Function signatures including `async`, `throws`, and `rethrows`.
- Generic type parameters with `where` clause constraints (`T : Swift.Equatable`, `T : Swift.Hashable`, `T == Swift.String`).
- `some P` opaque return types (parsed but flagged as `SkipReport` unless monomorphised).
- `any P` existential types (parsed; concrete-type existentials in parameter position are translatable, existentials in return position are flagged).
- Associated type declarations in protocols.
- `@available` attributes (used to detect platform-restricted items).
- `@_cdecl` attributes on existing bridge wrappers (rare but possible in packages that already expose a C API).
- Operator declarations (skipped; operators do not translate to Mochi).

The parser does NOT handle:
- `#if` conditional compilation blocks (these do not appear in `.swiftinterface`; the compiler resolves them before emitting the interface for a specific platform/architecture).
- Swift macro declarations (macros appear in the interface as `@freestanding(expression)` or `@attached(member)` declarations but are not callable and are always `SkipReport`).
- `@_implementationOnly import` shadowed types (the compiler omits `_implementationOnly` items from the interface).

## Stability across Swift versions

The `.swiftinterface` format has been backward-compatible within major Swift versions:

| Swift version | Format change | Bridge impact |
|---------------|---------------|---------------|
| 5.1 (SE-0235) | Initial stable format | First supported version |
| 5.3 | Added `@_alwaysEmitIntoClient` attribute | Bridge skips `@_alwaysEmitIntoClient` items (no ABI entry point) |
| 5.5 | Added `async`, `actor`, `@Sendable` | Bridge handles `async func` and `actor` type declarations |
| 5.7 | Added `some P` primary associated types, `any P` existential syntax | Bridge handles both; `any P` in return position is `SkipReport` |
| 5.9 | Added Swift macros, `@backDeployed` | Bridge skips macro declarations; `@backDeployed` functions are translated normally |
| 6.0 | Added typed throws (`throws(E)`) | Bridge handles `throws(E)` as `Result<T, E>` desugar; untyped `throws` maps to `Result<T, Error>` |

The bridge ships a format-version compatibility matrix keyed by the `// swift-interface-format-version:` header and the `// swift-compiler-version:` record. When a new Swift major version introduces a format-breaking change, the bridge emits a clear error pointing to the required bridge update.

## SHA-256 and drift detection

After parsing, the bridge computes the SHA-256 of the concatenation of all `.swiftinterface` files for the package (sorted by module name for determinism) and writes it to `mochi.lock` as `swiftinterface-sha256`. At `mochi pkg lock --check` time, the bridge recomputes this SHA-256 and fails the check if it differs. This catches:

- A `git tag -f` that updated the tag to a different commit with a different API.
- A transitive dependency upgrade that changed the API surface of an indirectly consumed package.
- A Swift toolchain upgrade that regenerated `.swiftinterface` files with a different set of inlined items (`@_alwaysEmitIntoClient` content can vary by toolchain version).

## Cross-references

- [[02-design-philosophy]] §1 for why `.swiftinterface` was chosen over source parsing and SourceKit-LSP.
- [[05-type-mapping]] for the translation pass that reads the `Module` AST produced here.
- [[09-abi-stability]] for the ABI guarantees of the generated `@_cdecl` wrapper.
- [SE-0235](https://github.com/apple/swift-evolution/blob/main/proposals/SE-0235-module-stability.md) — the original module stability proposal.
