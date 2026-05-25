# MEP-49 research note 02, Design philosophy

Author: research pass for MEP-49 (Mochi to Swift transpiler).
Date: 2026-05-23 (GMT+7).

This note explains the load-bearing design choices behind MEP-49 and
the constraints they impose. It is the "why" companion to
[[01-language-surface]]'s "what" and [[05-codegen-design]]'s "how".

## 1. Why a fifth target at all

Mochi already has four mature lowering targets: vm3 (the reference
tree-walker), MEP-45 (C, single-binary AOT), MEP-46 (BEAM,
supervision and hot reload), MEP-47 (JVM, Maven Central and Loom),
MEP-48 (.NET, NuGet and NativeAOT). Each target picks up an ecosystem
Mochi cannot reach from the others.

The Apple platforms are the remaining unreached ecosystem. iOS alone
runs on roughly 1.4B active devices (Apple Q1 2026 earnings, January
2026). macOS 15 ships on every Mac sold since 2024. visionOS, while
small, is the only viable path to Apple Vision Pro and the AR/VR
hardware the platform anchors. tvOS and watchOS round out the Apple
ecosystem.

The Apple developer ecosystem is also walled. iOS apps must be Swift
or Objective-C compiled by Apple's own toolchain, signed by an Apple
Developer Program account, distributed through App Store Connect or
TestFlight (sideloading exists on EU iOS via the Digital Markets Act
since iOS 17.4 in March 2024 but remains a fringe distribution
channel). A Mochi-to-WebView app is possible via Capacitor or
Cordova but fails Apple's "novelty" review criterion in section 4.2
of the App Review Guidelines. The only path to a first-class iOS app
is native code, and the path of least resistance is Swift.

Therefore MEP-49: Mochi compiles to idiomatic Swift, drops into a
SwiftPM project, and produces Apple-platform binaries that pass App
Review.

## 2. Why Swift, not Objective-C

Objective-C is still a supported language on Apple platforms but it
is on a long decommissioning runway. Apple's own first-party code has
been Swift-first since iOS 13 (2019). New Apple frameworks
(SwiftData, Observation, SwiftUI, FoundationModels) ship Swift-only
APIs with no Objective-C bridge. The Objective-C runtime survives for
backward compatibility but receives no language-level investment.

For Mochi to remain useful in 2030 and beyond, the target must be
Swift. Objective-C is also a much worse lowering target: dynamic
dispatch by default (negates Mochi's type checking), no value types
(forces boxing of everything), no generics (forces Any/id), no
sealed enums (sum types become NSError-style int constants),
manual reference counting before ARC (1980s memory model).

Swift gives us reified generics (no JVM-style erasure), value types
with copy-on-write semantics (no .NET-style class explosion), sealed
sum types via enum with associated values (matches Mochi's union
type exactly), typed throws (since Swift 6.0 SE-0413), and complete
sendable checking under strict concurrency (catches sharing bugs at
compile time).

## 3. Why Swift 6.0 as the floor

Swift 6.0 shipped on 2024-09-17 alongside Xcode 16 and is the first
release with Swift 6 language mode (strict concurrency by default,
complete sendable checking, region-based isolation per SE-0414,
typed throws per SE-0413). The decision to floor at 6.0 rather than
the older 5.10 or 5.9 LTS lines locks in several language features
that materially simplify Mochi's codegen:

- **Typed throws (SE-0413).** Mochi error types are nominal (each
  function declares which error union it can throw). Pre-6.0 Swift
  forces `throws` (existential `any Error`) which loses Mochi's
  static error tracking. With typed throws, `func parse(_ s: String)
  throws(ParseError) -> AST` preserves the error type through the
  call.
- **Strict concurrency by default.** Mochi agents must be sendable.
  Under Swift 5.x with `-strict-concurrency=complete` the same checks
  are available but the language mode default differs. By floor-ing
  at 6.0 we always emit code that compiles cleanly under the strict
  default.
- **Region-based isolation (SE-0414).** Lets sendable-free types
  cross actor boundaries when the compiler proves region separation.
  This is critical for Mochi's value types (records, list literals)
  which the user never thinks of as sendable but in fact are safe
  to pass.
- **Non-copyable types (SE-0390) plus non-escapable (SE-0446).**
  Mochi linear types (planned, not yet in surface) lower to
  `~Copyable` and `~Escapable` Swift types.
- **C++ interop (SE-0381).** Bidirectional Swift-C++ interop since
  Swift 5.9. Mochi `extern` declarations for C++ libraries (planned
  v2) require this.
- **Embedded Swift (SE-0410, partial).** Subset of Swift for
  microcontrollers. Mochi v1 excludes it but the language surface
  Mochi uses must remain Embedded-compatible where possible (no
  String in hot paths, no protocol existentials).

Floor-ing at 6.0 means dropping Xcode 15 and pre-2024 toolchains.
That cost is acceptable: Xcode 16 has been the default since the
2024-09 release and Apple deprecates older Xcodes from App Store
submissions on a ~12 month cadence. By the MEP-49 implementation
landing horizon (2026 H2), Xcode 16 will be the floor App Store will
accept anyway.

The ceiling is Swift 6.2 (shipped 2025-09-15 with Xcode 17), which
adds approachable concurrency (closures default to non-isolated),
parameter packs improvements, and lifetime-dependent return values
(SE-0456). MEP-49 v1 stays on 6.0-compatible code and runs the 6.2
matrix in advisory mode (warning-only).

## 4. Why all Swift-supported platforms, not Apple-only

The user-facing question that drove this decision: "is Mochi-to-Swift
just an iOS thing, or can a Mochi backend service compile to Linux
too?". A Swift-only iOS story would force users to use MEP-47 (JVM)
or MEP-48 (.NET) for their backend even though their frontend is
Swift, which means two type systems, two query DSLs, two error
models, and a serialisation boundary they would otherwise not need.

Swift-on-Linux has matured since the 2015 open-source release. As of
Swift 6.0 (Sept 2024), swift-corelibs-foundation provides ~90%
parity with Apple Foundation, URLSession works (libcurl-backed),
Regex literals work, async/await works (the runtime is the same).
The Static Linux SDK (musl-based, introduced Swift 5.9 in Sept 2023)
gives single-binary static deployment, comparable to Go's
`go build` output.

Swift-on-Windows reached production quality in Swift 5.9. The
toolchain ships as MSI, integrates with MSVC linker, bundles ICU.
Mochi-on-Windows lets a single Mochi codebase produce a Mac App
Store binary, a Linux daemon, a Windows .exe, and an iOS app.

The cost is a larger test matrix (5 OS, 3 architectures, 3 Swift
versions in advisory mode). The MEP-46 BEAM and MEP-47 JVM targets
already pay a similar matrix cost; adding Swift to the mix is
incremental.

A narrower scope (Apple-only) would have simplified the runtime
(no Foundation polyfills) and the build pipeline (no static SDK,
no Windows toolchain). But it would also have ceded the
fastest-growing Swift segment, server-side Swift (Vapor and
Hummingbird both hit production at major employers in 2024-2025),
and would have made Mochi-on-Swift a strictly worse choice than
Mochi-on-Go or Mochi-on-Rust for any non-iOS workload.

## 5. Why Swift actors plus AsyncStream for agents

Mochi agents need three properties: serial mailbox processing,
isolation from caller threads, and the ability to send messages
asynchronously (cast) and synchronously (call). The candidate
lowerings:

- **GCD DispatchQueue serial + DispatchQueue.async**: works pre-5.5
  Swift but ignores the entire Swift Concurrency stack. Type
  checking of sendability does not propagate through dispatch.
  Combine for streams, which is Apple-only (no Linux/Windows).
- **Custom actor runtime in MochiRuntime**: ship our own scheduler
  on top of unstructured Task plus an unbounded MPSC queue. Decouples
  Mochi from Swift's evolution but duplicates work the language
  already does well.
- **Swift `actor` plus `AsyncStream<Message>` mailbox**: the actor
  isolation gives us serial processing and sendable enforcement for
  free. AsyncStream gives us a buffered, backpressure-aware mailbox.
  Selected.

The actor approach has three concrete wins. First, sendable checking
at compile time: Mochi's type checker can lower a non-sendable type
to a Swift type and the Swift compiler will refuse to compile if a
non-sendable value crosses the boundary. Second, AsyncStream
buffering: the `.bufferingOldest(bound)` policy gives Mochi a
bounded mailbox with drop-oldest semantics, matching Mochi's
bounded-channel default. Third, integration with structured
concurrency: Mochi's `parallel { ... }` block lowers to
`withThrowingTaskGroup`, and child tasks of an actor are cancelled
when the parent goes out of scope.

The trade-off is reentrancy: Swift actors are reentrant by default
(an actor method awaiting another actor releases the lock). Mochi's
default semantics are non-reentrant (one message processed at a
time, no interleaving). MEP-49 codegen wraps the mailbox loop in a
manual serial dispatcher to force non-reentrant semantics; see
[[09-agent-streams]] for the implementation pattern.

## 6. Why SwiftSyntax for codegen, not raw strings

The codegen pass produces Swift source files. The natural impulse is
to print Swift text directly with formatted-string templates. Three
reasons we don't:

- **Indentation and trailing-comma correctness.** Swift is whitespace-
  tolerant but the test gate verifies byte-identical output across
  runs. Hand-formatted code drifts on the slightest schema change.
- **Syntactic validity.** A typo in a template emits invalid Swift
  that compiles only on the second run after the user fixes it.
  Unacceptable.
- **Round-trippability via swift-format.** Emitting through a real
  syntax tree means `swift-format format` can canonicalise the
  output. The pretty-printer is the source of truth, not the
  template.

The decision: emit Swift through a structural builder, then post-
process via `swift-format`. We do *not* take a hard dependency on
the apple/swift-syntax Swift package (it requires a Swift host
toolchain at Mochi's build time, which complicates Mochi's
self-bootstrap on a machine without Swift installed). Instead, the
Mochi codegen pass implements a thin Go-side syntax-tree model that
emits canonical Swift text directly, then optionally pipes through
`swift-format` (which is available wherever the Swift toolchain is).

This is the same approach MEP-47 takes for Java (JavaPoet pattern
mirrored in Go) and MEP-48 takes for C# (a Go-side equivalent of
SyntaxFactory). See [[05-codegen-design]] for the full pipeline.

## 7. Why SwiftPM as the build driver

SwiftPM (`swift build`, `swift test`, `swift run`, `swift package
init`) is the canonical Swift package and build tool, shipped with
every Swift toolchain since 3.0 (2016). It does dependency
resolution against the Swift Package Index, compiles per-target, and
emits .o and .swiftmodule files.

Alternatives:
- **Bazel rules_swift**: hermetic, scales to monorepo, but adds a
  Bazel dependency. Mochi v2 evaluates.
- **CMake with Swift support**: viable, especially for mixed C++
  projects, but neither idiomatic Swift nor first-class on iOS.
- **Direct `swiftc` invocations**: works for small projects, but
  reinvents dependency resolution and dynamic library packaging.
- **Xcode build system (xcodebuild)**: required for iOS .ipa
  packaging but uses .xcodeproj/.xcworkspace files. SwiftPM can
  produce Xcode projects via `swift package generate-xcodeproj`
  (deprecated as of SwiftPM 5.7; the recommended path is to add a
  SwiftPM package as an Xcode dependency directly).

Decision: SwiftPM is the default for libraries and Linux/Windows
executables. For Apple-platform applications (iOS apps, Mac apps),
Mochi emits a SwiftPM library plus a thin Xcode project that
references it. `xcodebuild archive` produces the .ipa or .app from
that Xcode project.

## 8. Why a thin runtime library, not a fat one

The MochiRuntime SwiftPM package is intentionally thin: it re-
exports apple/swift-collections, apple/swift-algorithms, apple/
swift-async-algorithms, apple/swift-numerics, apple/swift-system,
apple/swift-log, plus a small layer of Mochi-specific helpers
(agent supervisor, datalog evaluator, query DSL extensions, JSON
roundtrip helpers, ZonedDateTime).

Reasons to keep it thin:
- The Apple swift-* packages are independently maintained and Apple
  signs off on their quality. Bundling them via re-export gives
  Mochi users a stable surface.
- A thin runtime is more legible. Mochi users can read MochiRuntime
  and see what Mochi adds vs. what is just Foundation/swift-
  collections.
- Embedded Swift (v2) requires a runtime subset. Keeping MochiRuntime
  small from day one makes the Embedded subset extraction tractable.

The fat alternative (ship our own Date type, our own URLSession
wrapper, our own JSON encoder) was rejected because Apple Foundation
exists and is already the canonical Swift surface on every Swift
platform. Mochi's runtime adds value only where Apple's surface is
genuinely missing or hostile (e.g., ZonedDateTime with stable
serialisation; Foundation's Date is wall-clock only).

## 9. Why ARC, not GC

Swift uses Automatic Reference Counting (ARC) for class instances.
Value types (struct, enum) live on the stack or are copied by value.
This is fundamentally different from JVM (tracing GC), .NET
(tracing GC with generational collector), Erlang (per-process GC),
and C (manual or none). Mochi must respect ARC's rules.

Implications:
- **Reference cycles leak.** Mochi closures captured into class
  fields need explicit `weak` or `unowned` annotations. The codegen
  pass uses `unowned` for parent references (supervisor → child
  link) and `weak` for caches.
- **Deinit timing is deterministic.** `deinit` runs when the last
  reference drops. Mochi can rely on this for resource cleanup (file
  handles, network sockets) without try/finally blocks.
- **Atomic refcounting overhead.** Each Sendable class type pays for
  atomic increment/decrement on every retain/release. Mochi prefers
  value types where possible to avoid this.

The ARC vs. GC choice favours Mochi: most Mochi data is value-type
(records, sums, primitives) and lowers to Swift struct/enum which
have zero refcount overhead. Only escape-stored closures and shared
mutable state (which Mochi discourages anyway) hit the ARC path.

## 10. Why complete sendability over diagnostic-only

Swift 6.0 default language mode enforces sendable across actor
boundaries as an error. Pre-6.0 modes treat it as a warning. The
choice to floor at 6.0 (decision §3) implies committing to complete
sendability.

This is the right call. Mochi already type-checks values, and its
agent model assumes message types are sendable (you can send a
message but you can't share a mutable reference). Swift's
sendability is the formal proof of that assumption. Emitting
6.0-mode code means the Swift compiler catches any Mochi codegen
bug that would have produced a data race.

The cost: Mochi codegen must mark every emitted type as `Sendable`
where Mochi guarantees sendability, and as non-Sendable elsewhere.
The codegen pass infers this from the Mochi type plus Mochi's
purity annotations.

## 11. Why typed throws

SE-0413 (Swift 6.0) introduces typed throws: `func f() throws(E) ->
T`. Mochi error types are already nominal: each fallible function
declares which error union it throws. Pre-6.0 Swift forced
existential `throws` (any Error), losing Mochi's static error type.
Typed throws preserves Mochi's error tracking through the call.

Cost: typed throws is contagious. A caller of `throws(E1)` that
catches and re-throws `E2` must explicitly convert. Mochi's
type-checker already does this conversion at the Mochi level, so the
codegen pass just emits the matching Swift typed throws clauses.

## 12. Why not Combine

Combine is Apple's reactive framework, available on iOS 13+ and
macOS 10.15+. It predates Swift Concurrency and is Apple-only (no
Linux, no Windows, no first-party non-Apple support). Mochi streams
could lower to Combine `Publisher` types but:

- Combine is functionally superseded by AsyncSequence and
  swift-async-algorithms in Apple's own framework direction (since
  iOS 17, 2023).
- Combine is Apple-only, ruling out Linux/Windows targets.
- Combine has known correctness issues with backpressure (no built-
  in support for demand signalling; you have to use Subscriber.Demand
  manually).

Decision: Mochi streams lower to AsyncSequence. Adapters between
AsyncSequence and Combine are easy (Combine has `.values`); Mochi
emits them for users on Apple platforms who want Combine interop.

## 13. Why Foundation over Mochi-native stdlib

The Apple Foundation (Date, URL, URLSession, Data, FileManager,
JSONEncoder, JSONDecoder) is the canonical Swift standard library
above the language stdlib. swift-corelibs-foundation reimplements it
on Linux/Windows. Mochi could ship a Mochi-native equivalent but
that duplicates a large, well-tested surface for marginal benefit.

Decision: Mochi runtime depends on Foundation. The dependency is
moderate (Foundation is a large library) but the alternative is
worse.

## 14. Why no IL2CPP-equivalent

Unity's IL2CPP transpiles .NET IL to C++ for AOT compilation on
platforms (notably iOS) that disallow JIT. Swift has no JIT and
always AOT-compiles, so no IL2CPP equivalent is needed.

This simplifies MEP-49 significantly: there is exactly one code
path (Swift source → swiftc → object → native binary), regardless
of platform. Compare MEP-48 (.NET) which has three code paths (JIT,
ReadyToRun, NativeAOT) and each requires its own gate.

## 15. Why no Mochi-on-Swift-Macros

Swift Macros (SE-0382, SE-0389, Swift 5.9) let library authors
write SwiftSyntax-based source generators that run during
compilation. A natural impulse: ship MochiRuntime as a Swift Macro
that expands Mochi source at compile time.

Rejected, three reasons:
- **Macros run during Swift compilation, not before.** A Swift Macro
  expands per-call-site, not per-module. Mochi codegen operates per-
  module (whole-module monomorphisation, datalog rules across
  modules). A macro-driven Mochi would force a per-call-site model
  that loses cross-module optimisation.
- **Macros require a Swift toolchain at compile time.** Mochi self-
  bootstraps from Go; a macro dependency would couple Mochi to a
  Swift toolchain version.
- **Macros are not portable to Embedded Swift.** Embedded Swift
  v1 forbids macro use because macros expand to potentially-
  non-Embedded code. A macro-driven Mochi could not target
  microcontrollers.

Macros remain useful for specific Mochi-Swift interop scenarios
(annotation-driven JSON encoding, observed properties) but they
are not the primary codegen mechanism.

## 16. Open philosophical questions

- **Should Mochi expose SwiftUI?** SwiftUI is the canonical Apple UI
  framework. Mochi has no UI surface currently. A future MEP-N could
  add a `view` keyword that lowers to SwiftUI `View` types. Not v1.
- **Should Mochi expose SwiftData?** SwiftData is the persistence
  framework (replaces Core Data). A `persisted record` keyword that
  lowers to `@Model` is a natural fit. Not v1.
- **Should Mochi expose distributed actors?** Swift's `distributed
  actor` (SE-0336, Swift 5.7) lets actors live across processes
  with a pluggable ActorSystem. Mochi remote agents are a natural
  fit. Not v1.
- **Should Mochi target Embedded Swift?** Embedded Swift unlocks
  microcontroller deployment. Mochi runtime would need a subset
  surface. Reserved for v2.

These are documented in [[12-risks-and-alternatives]] as candidate
follow-ups.

## 17. Decision summary

| Decision                        | Choice                                        | See   |
|---------------------------------|-----------------------------------------------|-------|
| Target language                 | Swift                                         | §2    |
| Swift version floor             | 6.0                                           | §3    |
| Platform matrix                 | Apple + Linux + Windows                       | §4    |
| Agent primitive                 | actor + AsyncStream<Message>                  | §5    |
| Codegen IR                      | Mochi-side syntax tree, emit text             | §6    |
| Build driver                    | SwiftPM (+ Xcode for iOS apps)                | §7    |
| Runtime library                 | Thin re-export over apple/swift-*             | §8    |
| Memory model                    | ARC (no GC)                                   | §9    |
| Sendability mode                | Complete (Swift 6 default)                    | §10   |
| Error model                     | Typed throws (SE-0413)                        | §11   |
| Reactive primitive              | AsyncSequence, not Combine                    | §12   |
| Standard library                | Foundation + swift-* packages                 | §13   |
| AOT-vs-JIT                      | AOT only (Swift has no JIT)                   | §14   |
| Code emission                   | Source text, optional swift-format            | §15   |

These thirteen decisions form the contract of MEP-49. The rest of
the spec body is mechanical consequence; the research notes
([[01-language-surface]], [[03-prior-art-transpilers]], [[04-runtime]],
[[05-codegen-design]], [[06-type-lowering]], [[07-swift-target-portability]],
[[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]],
[[11-testing-gates]], [[12-risks-and-alternatives]]) elaborate the
how-and-why for each.
