---
title: "MEP-69 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-69: language surface, design philosophy, prior-art Swift bridges, .swiftinterface ingest, the closed type-mapping table, the SPM git publish flow, the SE-0292 registry protocol, the Swift Concurrency async bridge, ABI stability, generics and opaque types, XCFramework and multi-platform distribution, plus the risks and rejected alternatives register."
---

# MEP-69 research bundle

This bundle is the informative companion to [MEP-69](/docs/mep/mep-0069). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import swift "..."` import shape, the `mochi.toml` `[swift-dependencies]` + `[swift]` tables, the CLI surface (`mochi pkg add swift`, `mochi pkg publish --to=swift-git`, `mochi pkg publish --to=swift-registry`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why `.swiftinterface` over alternatives, why a synthesised `@_cdecl` wrapper over direct Objective-C bridging or swift-bridge, why the async bridge uses `DispatchGroup`-bounded `Task` dispatch, why the git-tag publish path ships alongside SE-0292. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | swift-bridge, UniFFI (Swift backend), PyO3-analogues, cxx, swift-bindgen, Swig-Swift, ObjectMapper, DynamicCodable. What each gets right, what each requires the user to write, and what MEP-69 borrows. |
| [04. .swiftinterface ingest](04-swiftinterface-ingest.md) | The `.swiftinterface` textual format, the swift-tools-version header, the grammar for type signatures and generic constraints, the stability guarantees across major Swift releases, the Go-side parser shape, and the difference between `.swiftmodule` (binary, not stable) and `.swiftinterface` (textual, stable). |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic monomorphisation rule, `Optional<T>` vs `T?` handling, `Result<T, E>` desugar, the class-as-opaque-handle strategy, protocol-as-opaque-handle, and the `throws` error propagation ABI. |
| [06. SPM git resolution](06-spm-git-resolution.md) | The SPM git dependency model (tags, branches, revisions), the shallow-clone strategy, the `Package.swift` dump-package JSON extraction, the dependency graph walk, the version resolution algorithm (lowest-compatible-version vs Cargo's highest), and the content-addressed archive format. |
| [07. SPM publish flow](07-spm-publish-flow.md) | The git-tag publish path (tag creation, signing, push), the SE-0292 registry API (POST /packages, source archive format, signed manifest), the GitHub Actions OIDC token exchange for registry publish, the XCFramework binary distribution option, and the Swift Package Index indexing pipeline. |
| [08. Async bridge](08-async-bridge.md) | Swift Concurrency fundamentals (`async`/`await`, `Task`, actors, `@Sendable`), the `DispatchGroup`-bounded `Task { }` synchronous dispatch pattern, the risk of `@MainActor` isolation deadlocks, the opt-in `actor`-mode async bridge, cancellation semantics, and the contrast with the tokio singleton approach MEP-73 uses. |
| [09. ABI stability](09-abi-stability.md) | `@_cdecl` guarantees on Apple platforms and Linux, the opaque-handle strategy for non-`@_cdecl`-compatible value types, `String` and `[T]` round-trip ABI, drop semantics across the wrapper boundary (Swift ARC vs Mochi GC), the static archive vs dylib decision, and Swift ABI stability per-platform (stable on macOS 10.14.4+, iOS 12.2+; not stable on Linux). |
| [10. Generics and opaque types](10-generics-and-opaque-types.md) | Swift's generic type system (type parameters, where clauses, associated types, `some`, `any`, primary associated types), why opaque return types (`some P`) cannot be mapped without a concrete monomorphisation, the `[swift.monomorphise]` table, the existential container cost, and the future potential for a Swift 6 typed-throws bridge. |
| [11. XCFramework and multi-platform](11-xcframework-and-multiplatform.md) | XCFramework structure (per-sdk slice layout, `Info.plist` manifest), the `xcodebuild -create-xcframework` invocation, macOS fat binary production, iOS simulator fat binary, Swift module interface embedding, binary size impact, and the GitHub Releases upload path for binary XCFramework distribution. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (.swiftinterface stability, Linux ABI, wrapper compile times, git-tag drift, Apple-only packages, SE-0292 fragmentation, generic explosion, MainActor deadlock, Swift macros, XCFramework size, static stdlib size) and the rejected alternatives register (Swift source parsing, swift-syntax, SourceKit-LSP, swift-bridge, UniFFI, Objective-C primary, cxx intermediary, WIT, required annotations, dlopen pre-built, `@_expose(Cxx)`, long-lived tokens). |

## Cross-references

- [MEP-69 spec](/docs/mep/mep-0069) — the normative document.
- [MEP-53](/docs/mep/mep-0053) — the Swift transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the Rust package bridge whose architecture MEP-69 follows.
- [Implementation tracking](/docs/implementation/0069/) — the per-phase delivery status.
