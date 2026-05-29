---
title: "MEP-69 implementation tracking"
sidebar_position: 1
sidebar_label: "Overview"
description: "Per-phase delivery status for MEP-69 (Mochi and Swift package bridge). 18 phases from package3/swift/ skeleton through SPM git resolver, SE-0292 registry client, .swiftinterface ingest, type-mapping, @_cdecl wrapper synthesiser, extern emitter, import swift grammar, build orchestration, mochi.lock integration, TargetSwiftLibrary emit, git-tag publish, SE-0292 publish, async bridge, generics monomorphisation, XCFramework bundle, and Objective-C subset."
---

# MEP-69 implementation tracking

This page tracks the delivery status of each MEP-69 phase. A phase is LANDED only when its gate is green against the curated 20-package fixture corpus.

## Fixture corpus

The fixture corpus used to gate each phase is drawn from the Swift Package Index April 2026 most-downloaded snapshot:

| # | Package | URL | Covers |
|---|---------|-----|--------|
| 1 | Alamofire | github.com/Alamofire/Alamofire | HTTP client, async, class types |
| 2 | swift-argument-parser | github.com/apple/swift-argument-parser | Value types, protocol conformances, generics |
| 3 | swift-log | github.com/apple/swift-log | Protocol-based API, `any Logger` |
| 4 | swift-crypto | github.com/apple/swift-crypto | Value types, `@frozen` structs, binary data |
| 5 | swift-collections | github.com/apple/swift-collections | Generic containers, monomorphisation |
| 6 | swift-nio | github.com/apple/swift-nio | async/await, EventLoopFuture, actors |
| 7 | swift-http-types | github.com/apple/swift-http-types | Value types, enum with associated values |
| 8 | SwiftyJSON | github.com/SwiftyJSON/SwiftyJSON | subscript, dynamic dispatch |
| 9 | RxSwift | github.com/ReactiveX/RxSwift | Observable, generic types, closures |
| 10 | Vapor | github.com/vapor/vapor | async, actors, large dependency tree |
| 11 | Leaf | github.com/vapor/leaf | Template engine, String-heavy API |
| 12 | GRDB | github.com/groue/GRDB.swift | Generic fetch, SQLite, protocol witnesses |
| 13 | SQLite.swift | github.com/stephencelis/SQLite.swift | operators, generic expression builder |
| 14 | KeychainAccess | github.com/kishikawakatsuki/KeychainAccess | Apple-platform integration, optional types |
| 15 | SwiftProtobuf | github.com/apple/swift-protobuf | Generic decode, `@frozen` message types |
| 16 | CombineExt | github.com/CombineCommunity/CombineExt | Combine Publisher extensions, closures |
| 17 | Defaults | github.com/sindresorhus/Defaults | Generic UserDefaults, `@propertyWrapper` |
| 18 | Files | github.com/johnsundell/Files | Value types, Result, throws |
| 19 | Rainbow | github.com/onevcat/Rainbow | Cross-platform (Linux + macOS), enums |
| 20 | Ink | github.com/johnsundell/Ink | Pure Swift, structs, parsing |

## Phase status

| Phase | Description | Status | Gate |
|-------|-------------|--------|------|
| 0 | `package3/swift/` skeleton | NOT STARTED | directory structure, empty Go packages, CI job |
| 1 | SPM git resolver | NOT STARTED | `git ls-remote` + shallow clone for all 20 fixture packages |
| 2 | SE-0292 registry client | NOT STARTED | GET/POST against a local SE-0292 harness |
| 3 | Blob cache | NOT STARTED | BLAKE3-256 + SHA-256 of source archives; cache hit on second lock |
| 4 | `.swiftinterface` ingest | NOT STARTED | Parse `.swiftinterface` for all 20 fixture packages; produce `Module` AST |
| 5 | Type-mapping table | NOT STARTED | Translate all in-table items; emit `SkipReport` for out-of-table items; 20-package corpus |
| 6 | `@_cdecl` wrapper synthesiser | NOT STARTED | Generate compilable Swift wrapper for all 20 packages; `swift build` succeeds |
| 7 | Mochi extern emitter | NOT STARTED | Generate `.mochi` shim file for all 20 packages; parses cleanly |
| 8 | `import swift` grammar | NOT STARTED | MEP-1 grammar extension; `import swift "..." as x` parses and resolves |
| 9 | Build orchestration | NOT STARTED | MEP-53 driver PrepareWorkspace; end-to-end `mochi build` with one Swift import |
| 10 | `mochi.lock` integration | NOT STARTED | `[[swift-package]]` table; `mochi pkg lock --check` validates all fields |
| 11 | `TargetSwiftLibrary` emit | NOT STARTED | Emit full `Package.swift` + `Sources/` for a sample Mochi library |
| 12 | Git-tag publish | NOT STARTED | `mochi pkg publish --to=swift-git --dry-run` succeeds; tag is local |
| 13 | SE-0292 registry publish | NOT STARTED | `mochi pkg publish --to=swift-registry --dry-run` against local registry harness |
| 14 | Async bridge | NOT STARTED | `DispatchGroup`-bounded `Task` for async functions in swift-nio and Vapor |
| 15 | Generics monomorphisation | NOT STARTED | `[swift.monomorphise]` for swift-collections, GRDB, SwiftProtobuf |
| 16 | XCFramework bundle | NOT STARTED | `xcodebuild -create-xcframework`; macOS + iOS slices; binary target `Package.swift` |
| 17 | Objective-C subset | NOT STARTED | `@objc` protocol types; `NSObject` subclasses; `[swift.capabilities] objc = true` |

## Implementation notes

### Phase 0: skeleton

Create the `package3/swift/` directory with the following Go package layout:

```
package3/swift/
  spmresolver/    # git-based SPM dependency resolver
  registryclient/ # SE-0292 registry client
  blobcache/      # content-addressed BLAKE3/SHA-256 archive store
  swiftinterface/ # .swiftinterface textual parser
  typemap/        # Swift-to-Mochi type translation table
  wrapsyn/        # @_cdecl wrapper package synthesiser
  externemit/     # Mochi extern fn / extern type emitter
  lockfile/       # [[swift-package]] lockfile read/write
  publishgit/     # git-tag publish flow
  publishreg/     # SE-0292 registry publish flow
  xcframework/    # XCFramework bundle builder
  bridge.go       # top-level Bridge type (PrepareWorkspace, etc.)
```

### Phase 4: `.swiftinterface` ingest

The parser must handle the 20-package corpus. Key parsing challenges:

- **Alamofire**: 200+ `public` declarations; many `async throws` functions; `@unchecked Sendable` conformances.
- **swift-collections**: `OrderedDictionary<K, V>` and `Heap<T>` with `where` clause constraints; primary associated types.
- **RxSwift**: `Observable<Element>` with deeply nested generic constraints; closure-heavy API.
- **swift-nio**: actor types (`NIOSingletons`), `any EventLoop` existential, `@_alwaysEmitIntoClient` items.

### Phase 15: Generics monomorphisation

The monomorphise test fixture should include:

```toml
[swift]
monomorphise = [
    # swift-collections
    { item = "OrderedDictionary", K = "String", V = "Int" },
    { item = "OrderedDictionary", K = "String", V = "String" },
    { item = "Heap", T = "Double" },
    # GRDB
    { item = "Database.fetch", T = "UserRecord" },
    # SwiftProtobuf
    { item = "BinaryDecoder.decode", M = "UserProto" },
]
```

The gate passes when the generated wrappers for these entries compile without errors and the Mochi `extern fn` corpus parses cleanly.

## Cross-references

- [MEP-69 spec](/docs/mep/mep-0069) — the normative document.
- [MEP-69 research bundle](/docs/research/0069/) — the informative companion notes.
- `package3/swift/` — the implementation directory (created in phase 0).
