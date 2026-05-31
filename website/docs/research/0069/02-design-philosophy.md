---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional bridge, why .swiftinterface over source parsing or SourceKit-LSP, why @_cdecl wrappers over direct Objective-C bridging or swift-bridge, why DispatchGroup-bounded Task dispatch for Swift Concurrency, and why the git-tag publish path ships alongside SE-0292."
---

# 02. Design philosophy

Five architectural decisions shape MEP-69 in ways that are not obvious from the spec text alone. Each one was made after surveying the alternatives (documented in [[03-prior-art-bridges]] and [[12-risks-and-alternatives]]) and involves a real trade-off.

## §1. Why `.swiftinterface` over source parsing or SourceKit-LSP

The three candidate bind-sources for Swift module APIs are: the Swift source files, the SourceKit-LSP index, and the `.swiftinterface` module interface file. The bridge chooses `.swiftinterface`.

**Source parsing.** A source-level parse would require a Swift grammar implemented in Go. The Swift grammar is large (the grammar appendix of The Swift Programming Language book covers 150+ productions), and it changes with every major Swift release. More importantly, source-level parsing is defeated by conditional compilation: `#if canImport(UIKit)` blocks appear in virtually every cross-platform Swift package, and their bodies are only compiled on specific platforms. The post-compilation API surface differs from what a source parse would see. Swift macros (SE-0382) expand at compile time and do not appear in source in callable form. Source parsing is untenable.

**SourceKit-LSP.** SourceKit-LSP is Apple's Language Server Protocol implementation for Swift. It can answer "what symbols are defined in this package?" queries. However, it is designed for interactive editor latency, not batch API extraction: it requires a warm index, takes 5-15 seconds to initialise per package, and its output format is not versioned for programmatic consumption. Running SourceKit-LSP at lock time would add 60+ seconds per 5 imported packages, and the output schema can change silently across Xcode releases. SourceKit-LSP is rejected.

**`.swiftinterface`.** The `.swiftinterface` format (SE-0235, Swift 5.1, 2019) was explicitly designed for the scenario MEP-69 needs: a stable, textual, compiler-parseable representation of a Swift module's public API that is stable across Swift compiler versions within a major release. The format is human-readable (it looks like a subset of Swift source with type annotations), carries a `// swift-tools-version:` header, and is produced deterministically by `swiftc -emit-module-interface-path`. Apple ships `.swiftinterface` files for all Apple SDK frameworks (CoreFoundation, Foundation, UIKit, SwiftUI, etc.), making the same ingest path work for SDK types when the user targets an Apple platform.

The bridge invokes `swift build --configuration release` on each dependency at lock time (shallow-cloned from the git remote) to produce the `.build/` directory, then locates `.swiftinterface` files under `.build/<triple>/release/Modules/`. This is the one Swift toolchain invocation at lock time; the Go binary parses the resulting text files.

## §2. Why `@_cdecl` wrappers over Objective-C bridging or direct Swift calls

The three candidate FFI strategies are: `@_cdecl` (C ABI export), `@objc` (Objective-C bridging), and direct Swift-to-Swift calls (no wrapper, Mochi emits Swift code that imports the package).

**Objective-C bridging.** `@objc` allows Swift types to be accessed from Objective-C and from languages that understand the Objective-C runtime. It requires classes (reference types), Apple platform runtime, and `NSObject` compatibility. Modern Swift packages (swift-nio, swift-collections, swift-argument-parser) are struct-first and have no `@objc` surface. Linux Swift does not ship the Objective-C runtime. `@objc` is rejected as the primary path.

**Direct Swift-to-Swift calls.** If Mochi's MEP-53 Swift transpiler emits `import Alamofire` at the top of the generated `.swift` file, the user's `swift build` will simply compile against Alamofire directly. No wrapper is needed. This is superficially attractive but creates an architectural problem: Mochi's type system does not know that `AFDataResponse<Data>` is a generic type or that `DataRequest` conforms to `URLConvertible`. The MEP-53 lowering pass produces `aotir.Program` nodes that reference opaque foreign types; without a type-mapping pass, the emitted Swift code cannot call methods on those types without Mochi having a full Swift type-checker. The type-mapping pass (the closed table in §3 of the spec) is what makes zero-boilerplate imports possible, and it requires an explicit ABI boundary where the wrapped type is reduced to a Mochi-representable form (opaque handle, primitive, or translated value type).

**`@_cdecl`.** The `@_cdecl("symbol_name")` attribute (documented in Swift's ABI stability documentation since Swift 5.0) marks a Swift `public func` as exported with C calling convention and a predictable mangled name. It is the standard mechanism for Swift → C interop, is stable on all Apple platforms and Linux, and does not require classes or Objective-C. The synthesised wrapper package uses `@_cdecl` for every wrapped function, adopting the same sidecar pattern MEP-45's C transpiler establishes. The trade-off is that `@_cdecl` functions cannot take generic parameters (each monomorphisation must be a separate `@_cdecl` function), which is why the `[swift.monomorphise]` table exists.

## §3. Why a closed type-translation table with explicit refusal

The alternative to a closed table is an open translation: attempt to translate any Swift type, falling back to an opaque handle when the type is not directly representable. This is UniFFI's approach (it translates anything that has a `Serializable` FFI lowering, falling back to an error for unsupported types).

The MEP-69 bridge takes the closed-table approach for three reasons:

1. **Explicit is better than implicit for supply-chain correctness.** A Mochi user who runs `mochi pkg lock` sees exactly which functions in the imported package are callable and which are refused (via `SkipReport`). There is no silent "it mostly works" behaviour. This matches MEP-57's capability-declaration philosophy.

2. **The Mochi type system is not Swift's type system.** Swift has reference counting, mutable value semantics (`inout`), protocol witness tables, associated types, and existential containers; Mochi has none of these. An "open translation" that silently wraps every unsupported type as an opaque handle would produce Mochi code that compiles but does the wrong thing at runtime (leaking Swift objects when Mochi's GC collects the opaque handle without calling the Swift deinitialiser).

3. **The closed table is auditable.** Every entry in the table is documented in [[05-type-mapping]]. Security-conscious users can review the table and verify that no unexpected coercions are happening at the ABI boundary.

## §4. Why `DispatchGroup`-bounded `Task` dispatch for Swift Concurrency

Swift Concurrency (`async`/`await`, SE-0296, Swift 5.5) is deeply integrated into the modern Apple SDK and growing server-side Swift ecosystem. A bridge that cannot call `async` Swift functions is not useful for Vapor, swift-nio, or Alamofire 6.x. The question is how to bridge Swift Concurrency to Mochi's synchronous execution model.

Three strategies were considered:

**`Task.detached` + `RunLoop`.** Fire a detached task and spin a `RunLoop` on the calling thread until completion. Rejected: `RunLoop` spinning has race conditions on non-main threads and is not available in Linux's Swift Foundation.

**`DispatchGroup` + `Task { }`.** Enqueue a structured `Task` that captures the async call, and block the calling thread on a `DispatchGroup`. This is the pattern used by swift-bridge and UniFFI's Swift backend. It works on both Apple platforms and Linux (Grand Central Dispatch is shipped as open-source `swift-corelibs-libdispatch`). It is safe when the calling thread has no Swift Concurrency executor attached (the Mochi main thread never enters a Swift continuation context). The cost is one thread context switch per call; acceptable for IO-bound operations.

**Exposing as Mochi `async fun`.** Surface Swift `async func` as Mochi `async fun`, propagating the Swift continuation through Mochi's own colour pass. Attractive for high-throughput use cases (avoids the blocking thread), but MEP-53's Swift transpiler currently lowers Mochi `async fun` to synchronous evaluation; teaching it to propagate Swift continuations would require significant changes to the MEP-53 emit pass. This is the opt-in `[swift.runtime] async-mode = "actor"` path, deferred to phase 14.

The bridge defaults to `DispatchGroup`-bounded `Task` dispatch because it is universally safe, correct, and requires no changes to the MEP-53 emit pass. The opt-in actor mode is forward-compatible: users can upgrade to it by changing one `mochi.toml` line.

## §5. Why both git-tag and SE-0292 registry publish paths

SPM's dependency model is historically git-first: a Swift package is identified by its git URL, and versions are git tags. This has been the model since Swift Package Manager was introduced in Swift 3 (2016). SE-0292 (Package Registry Service, accepted 2022, available since Swift 5.7) adds a structured registry API modelled on npm's and PyPI's, allowing packages to be discovered and fetched without knowing the git URL upfront.

Two years after SE-0292, the ecosystem is split: major packages (Alamofire, Swift NIO, Vapor) are still primarily consumed via git URLs; the Swift Package Index registry pilot lists a small fraction of SPI's total index. A bridge that ships only the SE-0292 path would be unusable for most real packages today. A bridge that ships only the git-tag path cannot publish to registries.

MEP-69 ships both, with the git-tag path as the default for `mochi pkg publish` (it requires only `git push`, a universally available operation) and the SE-0292 path as opt-in (`--to=swift-registry`). The `--emit-ci` flag generates a GitHub Actions workflow that does both in sequence on a version tag push, matching the workflow Apple recommends in the SE-0292 implementation guide.

The absence of Sigstore-keyless OIDC trusted publishing for the git-tag path (unlike MEP-73's Cargo RFC #3724 or MEP-71's PyPI PEP 740 flow) is intentional: the git-tag push is signed with the user's existing git signing key (GPG or SSH), which is the established trust mechanism for git tags. The SE-0292 registry path uses GitHub Actions OIDC when the registry supports it (as the Swift Package Index registry pilot does); otherwise it falls back to a registry API token stored as a CI secret (documented in [[07-spm-publish-flow]] §4 as the only acceptable token use case in MEP-69).

## Cross-references

- [[01-language-surface]] for the user-visible surface.
- [[03-prior-art-bridges]] for the comparison table of existing Swift bridges.
- [[04-swiftinterface-ingest]] for the `.swiftinterface` parser implementation detail.
- [[07-spm-publish-flow]] for the dual publish path architecture.
- [[08-async-bridge]] for the `DispatchGroup` + `Task` pattern in depth.
- [[12-risks-and-alternatives]] for the rejected alternatives register.
