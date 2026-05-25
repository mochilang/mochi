# MEP-49 research note 03, Prior art: transpiling to Swift, compiling to Apple platforms, source-to-source on Swift (2014-2026)

Author: research pass for MEP-49.
Date: 2026-05-23 (GMT+7).
Method: structured web research; report distilled below, cross-referenced
against the MEP-45 (C), MEP-46 (BEAM), MEP-47 (JVM), and MEP-48 (.NET / CLR)
prior-art surveys.

The report is the canonical survey for the MEP body's "Rationale" and
"Prior Art" sections. References at the foot are the authoritative source
list. Cross-references to sibling notes use double-bracket slugs
([[02-design-philosophy]], [[05-codegen-design]], [[07-swift-target-portability]],
[[09-agent-streams]]) where applicable.

---

## Survey: state of the art in compiling high-level languages to Swift and the Apple platform matrix (2014-2026)

This survey covers the Swift compiler and runtime, source-to-source Swift
transpilers, languages adjacent to Swift, Swift on non-Apple platforms,
the Swift package and build ecosystem, Apple platform packaging, and
deployment-time considerations relevant to designing a Mochi-to-Swift
transpiler in 2026 against a Swift 6.0 language-mode floor (with Swift 6.2
as the secondary target). It is structured by *system* (sections 1-13),
then by *toolchain* (sections 14-16), then *lessons distilled* (section 17).

## 1. Swift itself: history, evolution, pipeline, ABI

Swift was unveiled by Apple at WWDC 2014 (2014-06-02) and shipped with
Xcode 6.0 on 2014-09-17. Chris Lattner (creator of LLVM, then at Apple)
led the original design; Ted Kremenek inherited project leadership in
2017 after Lattner's departure to Tesla and later SiFive. The language
was open-sourced on 2015-12-03 under Apache 2.0 (with a Runtime Library
Exception) at github.com/apple/swift, which moved to github.com/swiftlang/swift
during the 2024-08 Swift Foundation transition. The Swift Foundation, a
vendor-neutral foundation modelled on the Rust Foundation and announced
at WWDC 2024, governs the project from 2024 onward; Apple remains the
dominant contributor but no longer the sole maintainer.

The release cadence is roughly one major version per year aligned to
Apple's September Xcode release. Swift 1.0 (2014-09-09) → Swift 2.0
(2015-09-21) → Swift 3.0 (2016-09-13, the famous API-breaking rename
release) → Swift 4.0 (2017-09-19) → Swift 5.0 (2019-03-25, the ABI-
stability release) → Swift 5.5 (2021-09-20, async/await + actors) →
Swift 5.9 (2023-09-18, macros + C++ interop + parameter packs) →
Swift 6.0 (2024-09-17, strict concurrency by default in Swift 6 language
mode) → Swift 6.1 (2025-03-25) → Swift 6.2 (2025-09-16, expanded
embedded mode and Swift Testing 1.0 LTS). In 2026, Swift 6.3 is in
prerelease with full SE-0494 macro plug-ins as compiled artifacts and
expanded `@_extern(c)` support.

The swift-evolution process is the canonical model for community-driven
language design and the explicit lineage for Mochi's own design process.
Proposals live at github.com/swiftlang/swift-evolution, are numbered
SE-NNNN, and go through Pitch (forums) → Review (forums + manager) →
Decision → Implementation. Each proposal lists authors, review managers,
status (Active Review, Accepted, Implemented, Rejected, Withdrawn), and
implementation toolchain. The pull request workflow on swift-evolution is
the same as Rust RFCs and Python PEPs, with the distinction that Apple-
employed core team members historically had decision-making authority
that the Swift Foundation transition is gradually rebalancing. Notable
proposals include SE-0001 (Allow `(most) Keywords` as Argument Labels,
2015-12-08), SE-0381 (DiscardingTaskGroups, 2023), SE-0382 (Expression
Macros, 2023), SE-0389 (Attached Macros, 2023), SE-0407 (Member Macros),
SE-0411 (Isolated default values), SE-0414 (Region-based isolation),
SE-0444 (Member-import visibility, 2024), SE-0490 (Concurrency in Swift 6
language mode), SE-0494 (Macros as Compiled Plug-ins, 2025).

The Swift compiler pipeline is one of the most thoroughly documented
production compilers and consists of: lexer/parser → AST → Sema (semantic
analysis, type checking, name lookup, generic instantiation, protocol
conformance checking) → SILGen (lowering AST to Swift Intermediate
Language) → SIL Optimisation (mandatory passes for correctness,
optional passes for performance, including ARC optimisations, generic
specialization, dead code elimination, devirtualization, function
inlining, and Ownership SSA verification) → IRGen (lowering SIL to LLVM
IR) → LLVM backend (target codegen). SIL is Swift's distinguishing
high-level IR: it preserves Swift-specific semantics like ARC reference
counts, generic parameters, witness tables for protocol conformances,
and ownership annotations long after the source AST is gone. SIL is what
makes Swift's optimizer competitive with C++ on monomorphic code despite
the apparent overhead of protocols, generics, and ARC.

ABI stability landed in Swift 5.0 (2019-03-25), released alongside macOS
10.14.4, iOS 12.2, and Xcode 10.2. Before Swift 5.0, every Swift app
bundled its own copy of the Swift standard library inside the app bundle
(the `Frameworks/libswiftCore.dylib` and friends), so app sizes were
inflated by ~15-30 MB and runtime calling conventions could change
between compiler releases. Swift 5.0 froze the calling convention,
metadata layout, and standard library exports, after which Apple shipped
`libswiftCore.dylib` as part of the OS in `/usr/lib/swift/`. Apps built
with Swift 5.1+ on macOS 10.14.4+ / iOS 12.2+ no longer carry their own
runtime; module stability (the ability to consume a `.swiftmodule`
compiled with a *different* compiler version) followed in Swift 5.1
(2019-09) under the `-enable-library-evolution` flag.

Library Evolution (sometimes called "resilient" mode) is the compiler
flag that produces ABI-stable framework binaries. When enabled, the
compiler emits indirect access to public fields (so adding a stored
property to a public struct does not break clients), routes function
calls through a stable thunk, and emits a `.swiftinterface` textual
module description (instead of a binary `.swiftmodule`) so future
compilers can re-derive types. Apple's system frameworks (`Foundation`,
`SwiftUI`, `Combine`, `OSLog`, etc.) are built with library-evolution.
Apps and most SwiftPM packages are not, because library-evolution
incurs a measurable performance cost (8-15% on tight loops, per
swift-compiler-performance bench results) and a binary-size cost.
This dual mode (fragile for whole-module optimisation, resilient for
ABI-stable system frameworks) is *the* defining trade-off of the Swift
runtime story.

**Lesson for MEP-49:** Mochi's emitted Swift code should *not* be
library-evolution by default; emit fragile (whole-module-optimised)
code for application binaries, and reserve library-evolution mode for
explicit `mochi build --target=swift --resilient` framework builds.
Track Swift's release cadence rigorously, the 12-month rhythm aligns
with WWDC; Mochi compatibility windows should advance one Swift major
per year.

## 2. Source-to-source Swift transpilers

The Swift world has comparatively few source-to-source transpilers
because Swift's ABI stability and SwiftPM ecosystem reduce the
incentive to generate code through another language. The notable
exceptions:

**Skip.tools / Skip** (Skip framework by Marc Prud'hommeaux and Abe White,
since 2023-09; v1.0.0 in 2024-03; v1.5 in 2025-06; github.com/skiptools/skip)
is the most ambitious Swift transpiler shipping in 2026. Skip is a
SwiftPM plug-in that transpiles Swift source to Kotlin source at build
time, so you write a single Swift codebase that builds simultaneously
to iOS (via xcodebuild) and to Android (via Gradle / Kotlin Multiplatform).
The transpiler operates on the parsed Swift AST produced by SwiftSyntax
(see §11), maps Swift constructs to Kotlin equivalents (`struct` →
`data class`, `enum` with associated values → `sealed class`, `actor`
→ Kotlin coroutine actor pattern with `Mutex`, `async/await` → Kotlin
coroutines), and emits Gradle build files alongside the Kotlin sources.
Skip Lite (free tier) transpiles language constructs only; Skip Pro
(paid) adds a SwiftUI-to-Jetpack-Compose layer (SkipUI). The model is
deliberately *source-level*: Skip does not introduce a runtime, every
transpiled construct must map to existing Kotlin/Jetpack APIs. The
build-time penalty is significant (a fresh Skip build of a 50kLOC app
adds ~30-60s to the Android side of the build), but incremental builds
are fast.

What Skip got right: incremental adoption (you can transpile one Swift
file at a time, mixing with existing Kotlin), source-level interop with
existing Android libraries, no runtime overhead at execution time, and
clean integration with both SwiftPM (for the iOS side) and Gradle (for
the Android side). What it got wrong: the flow is unidirectional
(Swift → Kotlin only, no Kotlin-to-Swift back-transpile), the SwiftUI
→ Compose mapping is incomplete (no `Canvas`, limited `GeometryReader`
support), and the Pro pricing model has limited adoption inside open-
source projects. Skip nonetheless proves that source-to-source Swift
transpilation is viable when you commit to staying inside the host
ecosystem's idioms.

**J2ObjC** (Google, since 2012; v3.0.0 in 2024-04, archived 2025-09) was
the canonical Java → Objective-C transpiler. Used internally by Google
to share business logic between Android (Java) and iOS (Objective-C)
apps, J2ObjC translated Java source to Objective-C source with
equivalent semantics, including a runtime helper library (`libjre_emul`)
implementing `java.util.*`, `java.io.*`, and parts of `java.nio.*` in
Objective-C. The project supported Java 8 lambdas via `__block` closures
and method references via `__block` typed proxies, but never gained
production-grade Swift support (`-Xobjective-c-header` produced
Objective-C headers callable from Swift via the bridging header
mechanism, which incurs a 20-30% method-dispatch overhead). The project
was archived in 2025-09 with the rationale "Kotlin Multiplatform and
Skip serve the same use case better"; the documentation site
(developers.google.com/j2objc) is preserved as historical reference.

**Swiftify** (swiftify.com; Yalantis, since 2017; commercial product
with v3.0 released 2025-04) is the leading C-to-Swift transpiler. It
ingests C source (with optional C99/C11/C23 features), produces idiomatic
Swift code, and handles common patterns like manual memory management
(translated to `UnsafePointer`/`UnsafeBufferPointer` plus optional
`@_unsafeNonisolated` markers), bitfields (translated to bitwise
operations on backing storage), and tagged unions (translated to Swift
enums with associated values). Swiftify is *not* open source; it ships
as a web service with a CLI client and a freemium pricing model. The
v3.0 release added C++ → Swift translation for a subset of C++17 (no
templates, no operator overloading, limited STL coverage), but the
ambition is bounded.

**c2nim-style tools for Swift** do not exist in 2026, in the sense that
there is no Swift equivalent of the Nim community's `c2nim` (which
translates C headers into Nim bindings). The closest is Apple's own
`ClangImporter` inside the Swift compiler, which on-the-fly imports any
Clang-parseable C/Objective-C header as a Swift module. This is *not*
a transpiler; it is a header-translation layer that runs during
compilation, exposing C types and functions to Swift code through
generated bridge symbols. Combined with `module.modulemap` files (see §13),
ClangImporter makes any C library callable from Swift without ever
seeing Swift source for the bindings. Mochi-to-Swift can target the same
mechanism: declare C runtime helpers via `module.modulemap`, let the
Swift compiler import them, never generate bridge code.

**Tart** (Cirrus Labs, since 2022; v2.5 in 2025-08; github.com/cirruslabs/tart)
is a CI/CD tool that runs macOS and Linux virtual machines on Apple Silicon
hosts. While not a transpiler per se, Tart is the de facto standard for
CI runners that build Swift code targeting Apple platforms, because the
licence terms of macOS restrict virtualisation to Apple hardware. Tart
runs VMs on top of Apple's `Virtualization.framework`, supports nested
virtualisation (since macOS 15), and integrates with GitHub Actions,
GitLab Runner, and Buildkite via the `tart-runner` daemon. Mochi's CI
strategy for the Swift target should use Tart-on-Mac-mini infrastructure
or rent it from Cirrus' hosted offering.

**Sourcery** (Krzysztof Zabłocki, since 2017; v2.2.5 in 2025-04;
github.com/krzysztofzablocki/Sourcery) is a Swift *metaprogramming*
tool that scans Swift source files via SourceKit, exposes the parsed
AST as a Stencil template context, and generates Swift source files
from user-supplied templates. Sourcery predates Swift Macros and remains
in widespread use for boilerplate generation (`AutoEquatable`, `AutoHashable`,
`AutoMockable`, etc.). Unlike Swift Macros, Sourcery operates as a
build-time pre-processor that produces committed source files, which
makes the generated code reviewable in pull requests. The tool's
declining relevance since Swift 5.9 macros shipped is a useful case
study: macros subsumed about 60% of Sourcery's use cases, but the
remaining cases (cross-file scanning, project-wide enum collection)
keep Sourcery alive.

**Lesson for MEP-49:** Mochi-to-Swift should emit *Swift source* (not
SIL, not LLVM IR, not binary), use SwiftSyntax (see §11) to construct
the AST, then hand the result to `swiftc` or to `swift build` for
compilation. The Skip model (source-to-source, no runtime, leverage
the host SwiftPM ecosystem) is the closest match for Mochi's design.
Sourcery proves that build-time source generation is acceptable to the
Swift community when the generated code is readable.

## 3. Languages that compile to Swift or interop with Swift

**Hylo (formerly Val)** (Dimi Racordon, Dave Abrahams, Sean Parent;
v0.1 in 2022-08, v0.5 in 2024-11; github.com/hylo-lang/hylo) is a
research language designed by Dave Abrahams (former Apple Swift core
team) and Sean Parent (Adobe; classic STL contributor) exploring
"mutable value semantics" as an alternative to ARC. Hylo's core
contribution is the *subscript* language feature that makes mutable
access to a value-typed component first-class without exposing pointers.
Hylo compiles via its own front-end to LLVM IR, but the team has
explicit roadmaps for emitting Swift source as a back-end (Hylo → Swift
transpilation is on the v1.0 milestone) and for leveraging Swift's
ABI for FFI. The language is research, not production; the relevance
to MEP-49 is the *idea* that Swift can be a portable IR for languages
that share its value-semantics design.

**Mojo** (Modular Inc., Chris Lattner founder; v0.1 in 2023-05, v1.0
in 2024-09; modular.com; github.com/modular/mojo since 2024-04) is
Lattner's post-Apple language, designed as a Python superset compiled
via MLIR for AI/ML workloads. Mojo is Apple-adjacent only in personnel
(Lattner, Tim Davis, and many of the original Swift compiler engineers
moved to Modular). The language explicitly does not target Swift; it
compiles to LLVM IR via MLIR dialects (largely the Linalg and Tensor
dialects). Mojo is on the roadmap for full Python compatibility,
gradual type strictness, and SIMD-first numeric programming, but the
ABI and runtime are entirely separate from Swift's. The interop story
with Swift is via C ABI (`@convention(c)` on the Swift side,
`fn ... -> Int64` with C ABI on the Mojo side).

**Pkl** (Apple's configuration-as-code language; v0.25 in 2024-02 when
first open-sourced, v0.27 in 2025-03; pkl-lang.org;
github.com/apple/pkl) is Apple's internal config language, used to
generate Kubernetes manifests, Xcode project files, App Store Connect
configurations, and CI definitions. Pkl ships with first-class Swift
bindings (`pkl-swift`; github.com/apple/pkl-swift) that let a Swift
program load `.pkl` files and decode them to Swift types. The Pkl
runtime is implemented in Java/GraalVM; the Swift binding uses the
Pkl evaluator binary via subprocess. Mochi could plausibly support
emitting Pkl from Mochi datasets, or consuming Pkl as a build-time
configuration source for Mochi-to-Swift builds, mirroring Apple's own
config workflow. SE-0455 (proposed 2025-04) explores native Pkl
integration into SwiftPM as a config format alongside `Package.swift`.

**Swift Embedded** (Embedded Swift; first announced WWDC 2024 in the
"Go small with Embedded Swift" session 2024-06-12; available since
Swift 5.9 as an early-access feature, promoted to officially-supported
status in Swift 6.0; SE-0428 Embedded Swift was accepted 2024-08-17)
is Apple's bare-metal subset of Swift. It runs without the ARC runtime,
without `String` (replaced by `StaticString` and `UnsafeBufferPointer<UInt8>`),
without dynamic casting (`as?`), without metatypes by default, and
without protocol existentials. The use cases announced at WWDC are
microcontrollers (Apple uses Embedded Swift on the Secure Enclave and
on Apple Watch's secondary processor), kernel modules (parts of iOS's
ANE driver are Embedded Swift), and WebAssembly (see §9). The compiler
flag is `-enable-experimental-feature Embedded` plus
`-wmo -Osize`. Embedded Swift proves that Swift can target environments
without the standard Swift runtime, which is forward-looking relevance
for Mochi-on-microcontroller scenarios.

**Lesson for MEP-49:** Mochi-to-Swift should *not* target Embedded
Swift in v0; the language subset is too restrictive for general-purpose
Mochi programs (no `String`, no maps backed by hashing, no arbitrary
closures). Reserve Embedded as a future profile (`mochi build
--target=swift --profile=embedded`) for the microcontroller story.
Pkl is the relevant Apple-ecosystem config language to support as a
companion format; Mojo is irrelevant to MEP-49 except as an example of
"life after Swift" for Lattner.

## 4. Swift on non-Apple platforms

The cross-platform story has matured significantly between 2018 and 2026.

**swift-corelibs-foundation** (github.com/swiftlang/swift-corelibs-foundation,
formerly apple/swift-corelibs-foundation) is the open-source Linux
reimplementation of Apple's Foundation framework. The Apple-platform
Foundation is closed-source Objective-C wrapped in Swift overlays; the
corelibs version is pure Swift (with some Objective-C-style classes
mapped to plain Swift classes for source compatibility). Coverage in
2026 is roughly 90% of Apple's Foundation: `URLSession`, `JSONEncoder`,
`Date`, `Data`, `Bundle`, `FileManager`, `DateFormatter`, and `Calendar`
all work. Gaps remain in `XPCConnection` (Apple-only), `NSXPCInterface`
(Apple-only), `NSUbiquitousKeyValueStore` (iCloud, Apple-only), and a
handful of distributed-objects APIs. The 2022 rewrite (corelibs-
foundation 2.0) reimplemented the core in Swift instead of forking
Apple's old NSFoundation Objective-C codebase, dramatically improving
portability and reducing the maintenance burden.

**swift-foundation** (github.com/swiftlang/swift-foundation, since 2023-09;
v0.0.2 with Swift 5.10, v1.0 with Swift 6.0) is the *new* unified
Foundation implementation announced at WWDC 2023. It replaces both
the Apple-platform Objective-C Foundation and the Linux corelibs
Foundation with a single Swift codebase, with platform-specific
overlays for Apple-only APIs. As of 2025-09 (Swift 6.2), swift-foundation
is the default Foundation on macOS 15, iOS 18, Linux (when Swift 6.0+
is installed), and Windows (Swift 6.1+). This is one of the most
significant Apple-led open-source moves in the Swift ecosystem in 2025.

**Swift on Windows** has matured from "experimental" in 2020 to "fully
supported" by 2024. Swift for Windows ships installable from
swift.org/install with native Windows SDK integration; the toolchain
includes `swiftc.exe`, `swift-build.exe`, the Swift Package Manager,
and `lldb`. The runtime uses Windows-native ABI (`__stdcall` for
C interop, COM bindings via `WinSDK`), and Swift on Windows can build
both console apps and `.exe` GUI apps (via WinUI 3 bindings in
`Microsoft/swift-winrt`). The MSYS2 distribution
(msys2.org/docs/package-manager/) ships an `mingw-w64-x86_64-swift`
package as an alternative installation method. Swift 6.0 (2024-09) was
the first release where Windows was treated as a tier-1 platform with
the same test coverage as macOS/Linux.

**Swift on Android** has two tracks. Track one is **Skip.tools** (see §2),
which transpiles Swift to Kotlin and uses the existing Android toolchain.
Track two is **swift-android-toolchain** (originally Vladimir Vukićević,
Mozilla; now maintained by the community at github.com/finagolfin/swift-android-sdk),
which builds the native Swift compiler to target Android NDK. The
toolchain produces ELF binaries linked against Bionic libc and the
Android NDK. As of Swift 6.1 (2025-03), Android NDK r26+ is supported,
arm64-v8a and x86_64 are tier-1, armeabi-v7a is best-effort. The catch:
Swift's standard library and Foundation must be bundled inside the
Android APK (around 35 MB per ABI), and the Android JIT has no concept
of Swift ABI stability, so every app ships its own Swift runtime. SE-
0510 (proposed 2025-10) explores official tier-1 Android support inside
swift-corelibs and SwiftPM.

**Swift Static Linux SDK** (announced WWDC 2024; available with Swift
5.9 via `swift sdk install`, promoted to GA in Swift 6.0 on 2024-09-17)
is a SwiftPM SDK target that builds fully static Linux binaries linked
against musl libc instead of glibc. The result is a single executable
that runs on any Linux without needing a Swift runtime installed,
exactly the model that Go pioneered and that Rust achieves via `musl`
targets. The size is around 8-15 MB for a hello-world (vs ~80 MB for
glibc-dynamic), and the build is invoked via `swift build --swift-sdk
x86_64-swift-linux-musl`. The Static Linux SDK is the *single most
important deliverability feature* of Swift 6 for server-side workloads;
Vapor and Hummingbird both default to it in their Dockerfile templates
as of 2025.

**Vapor** (Tanner Nelson, since 2016; v4.99 in 2025-03, v5.0 expected
2026-08; vapor.codes; github.com/vapor/vapor) is the dominant server-
side Swift framework. Built on SwiftNIO (Apple's high-performance
networking library, github.com/apple/swift-nio), Vapor exposes a Rails-
shaped routing/model/migration layer with Fluent ORM. Production users
in 2026 include Tesla, RobinHood, Spotify (internal services), and
many Stripe-funded fintechs. The Vapor team partners with Apple on
swift-on-server roadmaps; the Swift Server Workgroup (SSWG; swift.org/sswg)
is the cross-vendor body coordinating this work.

**Hummingbird** (Adam Fowler, since 2022; v2.0 in 2024-09, v2.5 in
2025-12; github.com/hummingbird-project/hummingbird) is the newer,
more performance-focused server framework. Built on swift-nio,
Hummingbird is roughly 2x faster than Vapor on TechEmpower benchmark
single-table queries, with a smaller surface area and zero ORM
opinion. The framework is the de facto "Hummingbird is what you reach
for when you want SwiftNIO ergonomics without the Vapor stack." Both
Vapor and Hummingbird produce static Linux musl binaries via Swift 6's
Static Linux SDK and deploy to AWS Lambda, Fly.io, and Render.

**Lesson for MEP-49:** Mochi-to-Swift's server-side story is well
supported, the Static Linux SDK plus Vapor or Hummingbird is the
deployment model for a server-side Mochi program. The Windows and
Android stories are sufficient for tier-2 support; Apple-platform
deployment (iOS, macOS, watchOS, tvOS, visionOS) is the tier-1 user
experience that drives MEP-49's design priorities.

## 5. AOT/JIT story: Swift is always AOT, ARC vs tracing GC

Swift has no JIT. Period. Every Swift program is ahead-of-time
compiled to native machine code via the LLVM backend; there is no
bytecode, no runtime compilation, no interpretation. This is the
single most consequential architectural choice in the language: it
makes Swift's startup time competitive with C (typically 5-15 ms for
a hello-world on macOS), it makes Swift suitable for kernel-adjacent
and bootloader-style use cases (Embedded Swift, §3), and it makes
SwiftUI's preview machinery possible (Xcode's `XCBuild` compiles each
preview as a tiny standalone framework and loads it dynamically).

The trade-off is the absence of runtime adaptive optimisation. Swift
cannot speculatively devirtualise a call based on observed types; it
cannot inline across module boundaries unless `@inlinable` is declared
on the source side; it cannot profile-guide its hot loops. The
compensating mechanisms are: aggressive whole-module optimisation
(`-wmo`, the default for `swift build -c release`), specialization of
generic functions over their concrete types (a generic `Array<Int>.map`
gets a separate specialised version produced at the call-site module),
and `@inlinable` / `@usableFromInline` markers that let library authors
opt-in to cross-module inlining at the cost of binary-ABI exposure.

Memory management is ARC (Automatic Reference Counting), Apple's
deterministic refcount-based scheme inherited from Objective-C 2.0
(2007) and adapted for Swift. ARC inserts `swift_retain` /
`swift_release` calls at compile time around every reference move,
copy, and scope exit. The compiler optimiser then collapses redundant
retain/release pairs via an SSA-based ARC optimiser running on SIL.
The result is deterministic destruction (no GC pauses), reference-
counted heap, and a small per-allocation overhead (the strong refcount
is 32 bits stored inline in the object header). Value types (`struct`,
`enum`, tuples) are stack-allocated by default, copied on assignment,
and bypass ARC entirely. Cycles must be broken manually via `weak` or
`unowned` references; Swift has no cycle collector.

In 2026 the trend is towards *ownership* annotations (SE-0377 inout/
borrow/consume, SE-0390 noncopyable structs/enums, SE-0432 move-only
generics) that let the programmer opt into Rust-style affine ownership
where ARC is unwanted. Swift 6.0's strict concurrency model
intersects with ownership: `Sendable` requires value-types or
internally-isolated reference types; `~Copyable` types cannot escape
their isolation domain.

Versus tracing GC (the JVM, the CLR, BEAM): ARC trades worst-case
latency (no GC pauses) for sustained throughput cost (refcount updates
are atomic on shared references) and for the cycle-collection problem.
On Apple platforms, ARC was a deliberate choice to make Swift suitable
for low-power embedded devices and tight-memory mobile workloads
where GC pauses are user-visible.

**Lesson for MEP-49:** Mochi-to-Swift inherits ARC, which is the right
default for Apple targets. Mochi's existing ref-counted runtime (per
[[MEP-45]] for C) maps cleanly onto Swift's ARC. Mochi's `agent`
construct should *not* use ARC for the inter-actor message queues
(SE-0306 Concurrency, see §9); use `AsyncStream` or `AsyncChannel`
instead, which have their own bounded-buffer semantics independent of
the object graph.

## 6. SwiftPM as build system

SwiftPM (Swift Package Manager) is the build system bundled with the
Swift toolchain since Swift 3.0 (2016-09). The manifest is
`Package.swift`, a Swift program (not JSON, not TOML) that imports
`PackageDescription` and constructs a `Package` value. The compiler
itself loads and evaluates this manifest to determine targets,
products, dependencies, and resources. The deliberate use of Swift-as-
manifest means the build configuration has access to Swift's type
system, generic helpers, and platform-conditional logic
(`#if os(macOS)`).

The package layout is opinionated: `Sources/<target>/...` for source,
`Tests/<target>Tests/...` for tests, `Resources/` for bundled assets,
`Package.swift` at the root. Dependencies are URL-shaped (Git
repository URLs) with version requirements (semver ranges, branch
pinning, exact revision pinning). The dependency graph is resolved at
build time, fetched via `git clone --depth 1` or via the optional
package registry, and cached in `~/.swiftpm/`.

Plugins were introduced in SwiftPM 5.6 (Xcode 13.3, 2022-03) and
expanded in 5.9 (Xcode 15, 2023-09). There are two plugin kinds:
*build plugins* run during `swift build` and produce source files or
resources (e.g. Protocol Buffers generation, SwiftGen, SwiftLint),
and *command plugins* are invoked manually via `swift package <name>
<args>` and can have side effects (formatting, regenerating, deploying).
Plugins are themselves Swift packages with a special target type. The
official Apple plugins ecosystem includes `swift-format`, `swift-docc`,
`swift-protobuf`, `swift-syntax`, and `swift-build-tools-plugin`.

Package Collections (SwiftPM 5.5, 2021-09) and Package Registry
(SwiftPM 5.7 draft, 2022; finalised SE-0292 Package Registry on
2022-04-18) are the discoverability layer. The Apple-hosted reference
registry at swiftpackageregistry.com (formerly swift-package-index.com,
maintained by Sven A. Schmidt and Dave Verwer) is the de facto index.
The full registry protocol RFC is at swift.org/blog/swift-5.7-released
and remains in draft for the auth/publish flow. Binary targets
(SwiftPM 5.3, 2020-09) let a package distribute precompiled
XCFrameworks (Apple's multi-platform fat-framework format introduced
Xcode 11, 2019), which is how distributing closed-source SDKs works
in 2026.

**Lesson for MEP-49:** Mochi-to-Swift should generate a `Package.swift`
alongside the emitted sources, marking Mochi runtime support as a
dependency. The build flow is `mochi build --target=swift` →
generated Swift sources + Package.swift → `swift build` → executable
or framework. A SwiftPM *build plugin* could in principle invoke the
Mochi compiler on `.mochi` source files in `Sources/`; this is the
ergonomic ideal but adds Swift-side tooling overhead. The first cut
should be a separate `mochi → Swift source` step, with the SwiftPM
plugin as a later refinement.

## 7. Apple platform packaging: Xcode, codesign, notarization, App Store

The Apple platform packaging story is famously deep. Production-grade
deployment of a Swift program to the App Store, TestFlight, or even a
notarised macOS download requires navigating Xcode projects, code-
signing, entitlements, provisioning profiles, and Apple's notary service.

The **Xcode project model** is a `.xcodeproj` directory containing
`project.pbxproj` (a plist describing the build graph, target
configurations, build phases, and file references). For most modern
projects, the Xcode project is generated from `Package.swift` by Xcode
itself (`xed Package.swift` opens a SwiftPM package as an Xcode
workspace) or by tools like XcodeGen (yonaskolb/XcodeGen) or Tuist
(tuist/tuist), which materialise a checked-in `.xcodeproj` from a
declarative YAML/Swift specification. Hand-editing `project.pbxproj`
is a known source of merge conflicts and tooling-incompatibility bugs.

**xcodebuild** is the command-line driver for Xcode. The recipe to
produce a distributable .ipa or .app is:

1. `xcodebuild archive -scheme <name> -archivePath <path>` produces
   a `.xcarchive` bundle.
2. `xcodebuild -exportArchive -archivePath <path> -exportPath <out>
   -exportOptionsPlist <opts>` produces the signed `.ipa` (iOS) or
   `.pkg` (macOS).

**codesign** is the macOS code-signing tool. Every Swift binary
distributed to users must be signed with a Developer ID certificate
(for macOS direct download) or an App Store Distribution certificate
(for App Store). The signature embeds a hash of the binary and a
chain back to Apple's root CA. Apple's `Hardened Runtime` opt-in
(since macOS 10.14, 2018-09) adds runtime protections (no JIT, no
unsigned dylib loading) that are required for notarization.

**notarization** (via `notarytool`, introduced in Xcode 13, 2021-09;
the older `altool` was deprecated in 2023-11) is Apple's cloud
malware scan. You upload a signed binary, Apple's service scans it,
and on success returns a *ticket* that you `stapler staple` onto the
binary. Without notarization, macOS Gatekeeper (since macOS 10.15,
2019-10) refuses to launch downloaded binaries. The notary service
takes 1-15 minutes; rate limits apply.

**App Store Connect** is the web portal and `appstoreconnect-cli`
toolchain for submitting builds to the App Store. **TestFlight** is
Apple's beta-distribution service, supporting up to 10,000 external
testers per app with a 90-day expiration per build. TestFlight builds
are uploaded via `xcrun altool --upload-app` or `xcrun notarytool
submit --apple-id ... --team-id ...`, and require the same code-
signing and entitlements as App Store production.

The **.ipa / .app structure** is a ZIP archive (`.ipa` for iOS,
`.app` is a directory bundle on macOS) containing:
- `Payload/<name>.app/<name>` (the binary executable),
- `Payload/<name>.app/Info.plist` (the bundle metadata),
- `Payload/<name>.app/Frameworks/` (embedded dylibs and frameworks),
- `Payload/<name>.app/_CodeSignature/CodeResources` (the code
  signature manifest),
- `Payload/<name>.app/embedded.mobileprovision` (iOS only, the
  provisioning profile).

**Entitlements** are a `.plist` file embedded in the code signature
that grants the binary specific OS capabilities (iCloud, push
notifications, app groups, hardened runtime exceptions). Entitlements
are app-specific and must match the provisioning profile.

**MAS (Mac App Store) receipt validation** is the App Store's anti-
piracy mechanism for macOS apps. Every MAS-purchased app receives a
signed receipt in its bundle; the app must validate this receipt at
launch (typically using `StoreKit` framework's `Transaction` API in
StoreKit 2, available since iOS 15 / macOS 12, 2021-09). Failure to
validate is what produces "this app is damaged" warnings.

**Lesson for MEP-49:** Mochi-to-Swift's end-to-end build for Apple
platforms must handle this stack. The `mochi build --target=swift
--platform=ios --distribution=appstore` invocation needs to: produce
Swift source → invoke xcodebuild archive → invoke xcodebuild
exportArchive → optionally invoke notarytool. This is a substantial
shelling-out exercise; the cleanest design is to delegate to Apple's
own toolchain rather than reimplement signing or archive packaging.
The `Info.plist` and entitlements should be generated from Mochi
project metadata (see [[10-build-system]]).

## 8. Embedded Swift in depth

Embedded Swift (SE-0428, accepted 2024-08-17, available behind
`-enable-experimental-feature Embedded` since Swift 5.9 and promoted
to officially-supported with Swift 6.0 on 2024-09-17) is Swift's
"no-runtime" mode. The constraints:

- *No ARC runtime* (no `swift_retain` / `swift_release`; instead,
  all values must be value-types or `~Copyable` move-only types).
- *No String* (use `StaticString`, `UnsafeBufferPointer<UInt8>`, or
  user-defined types).
- *No generic existentials* (no `any Protocol`; `some Protocol` is
  fine because it monomorphises at compile time).
- *No metatypes* (no `T.self` at runtime; you can use them at compile
  time for `_static` reflection).
- *No dynamic casts* (`as?`, `is`; only `as` for guaranteed casts).
- *No Foundation, no Dispatch, no Combine* (you bring your own).

What you get in return: a Swift binary that runs without an OS,
without a heap allocator (if you choose), and without a 5-20 MB
runtime baseline. The Embedded Swift example in the WWDC 2024 session
shipped a Swift program for an STM32F4 microcontroller in 30 KB of
flash, comparable to equivalent C.

Apple uses Embedded Swift internally for the Secure Enclave Processor
firmware (per Quinn Nelson's WWDC 2024 hallway-track comments) and
parts of the kernel boot-time code. Externally, the Embedded Swift
community targets Raspberry Pi Pico (RP2040), ESP32, STM32, and
Nordic nRF52 microcontrollers. Tooling support for these is in
swift-embedded-examples (github.com/apple/swift-embedded-examples).

WebAssembly is the *other* major Embedded Swift target (see §9), since
WASM has no Swift runtime; Embedded mode is the natural fit.

**Lesson for MEP-49:** Embedded Swift is out of scope for v0 (Mochi
programs would lose their entire stdlib). Reserve it for a future
profile.

## 9. SwiftWasm: Swift compiled to WebAssembly

SwiftWasm (originally by Maxim Cramer, then SwiftWasm-Working-Group;
v5.7 in 2022, v5.9 in 2023, v6.0 in 2024-09 the first upstreamed
release; swiftwasm.org; github.com/swiftwasm) was the long-running
out-of-tree fork that ported the Swift compiler to emit WebAssembly.
The work was partially upstreamed into the Swift compiler in Swift
5.7 (toolchain support); the full standard library port (using
WASI-libc instead of glibc) was upstreamed in Swift 6.0 (2024-09-17)
when WebAssembly became an officially-supported tier-2 platform.

The runtime model uses **Embedded Swift mode** (§8) plus a minimal
WASI shim. Swift compiles to a `.wasm` file containing a `_start`
entry point; the host (browser via WebKit/V8, or wasmtime on the
server) loads and invokes it. For browser integration, **JavaScriptKit**
(github.com/swiftwasm/JavaScriptKit, v0.20 in 2025-10) is the Swift
binding library that lets Swift code call JavaScript and DOM APIs:

```swift
import JavaScriptKit
let document = JSObject.global.document
let div = document.createElement("div")
div.innerText = "Hello from Swift"
document.body.appendChild(div)
```

The binding works via `JSValue` (a Swift type wrapping a JS handle),
function pointers passed through a generated thunk, and a small JS
shim that the WASM module imports.

Performance: SwiftWasm in 2026 is roughly 1.3-1.8x slower than native
Swift on CPU-bound workloads, comparable to Rust-on-WASM. The runtime
cost is mostly the WASM linear-memory model (no native pointers)
plus the Embedded-Swift restrictions on metatypes and existentials.

Production users are still niche: experimental Swift Playgrounds in
the browser, some interactive documentation sites, and the swift-wasm
demos at swiftwasm.org. Compared to the JVM (GraalJS, TeaVM) or .NET
(Blazor WebAssembly), Swift-on-WASM is younger and smaller.

**Lesson for MEP-49:** Mochi-to-Swift-to-WASM is plausible but
constrained by Embedded Swift's limitations. Defer until the Mochi
WASM story crystallises through other backends.

## 10. IL2CPP-equivalents in the Swift world

There is no IL2CPP-style "transpile then compile" pipeline in Swift,
because Swift is always AOT compiled and there is no portable
intermediate form intended for redistribution. The closest analogues:

**Swift Static Linux SDK + musl** (§4) is the deliverability story
for single-binary deployment. The output is a fully statically-linked
ELF binary on Linux/musl, similar to Go's default and Rust's
`x86_64-unknown-linux-musl` target. This is closest in spirit to
.NET's NativeAOT or Java's GraalVM native-image, but achieved via the
ordinary Swift toolchain rather than a separate AOT compiler.

**XCFrameworks** are the closest analogue to a "redistributable
intermediate." An XCFramework is a directory bundle containing
multiple precompiled framework slices (one per platform/architecture
combination), with `Info.plist` describing the slices. Apple uses
XCFrameworks for closed-source SDK distribution; SwiftPM consumes them
via `binaryTarget(name:url:checksum:)`. The slices themselves are still
native binaries, just packaged for multi-platform redistribution.

**Macro expansion** (§11) is the closest thing to "transpile to
Swift then compile." When the Swift compiler encounters a `#macro(...)`
call site or a `@SomeMacro`-annotated declaration, it invokes a
separate compiled macro plug-in (`SwiftCompilerPlugin` from
swift-syntax), receives the macro's emitted Swift source, splices it
into the compilation unit, and proceeds. The macro-emitted source goes
through ordinary type-checking and codegen; in that sense, every
macro-using Swift program *transpiles* a portion of itself before
compilation. This is the operational model Mochi-to-Swift most resembles,
except Mochi takes the role of the "macro" emitting Swift source from a
different surface language.

**Lesson for MEP-49:** Mochi-to-Swift can frame itself as a "whole-file
macro expansion": Mochi source is the input, Swift source is the
emitted output, the Swift compiler is the back-end. This framing is
useful for educating users but does not actually use Swift's macro
machinery (which is for sub-expression / sub-declaration scope, not
whole-program compilation).

## 11. Swift Macros: prior art for SwiftSyntax-based codegen

Swift Macros are the language's compile-time metaprogramming facility,
introduced in Swift 5.9 (2023-09-18). Two proposals define them:

- **SE-0382** (Expression Macros, accepted 2023-03-17, implemented in
  5.9): `#stringify(x + 1)` syntax for compile-time expression
  rewriting.
- **SE-0389** (Attached Macros, accepted 2023-05-15, implemented in
  5.9): `@AddCompletionHandler` style attached macros that decorate
  declarations and synthesise members, peers, or extensions.

Additional proposals fill out the model:
- **SE-0397** (Freestanding Declaration Macros, 2023-08) for `#decl`-
  style macros that introduce new declarations at file scope.
- **SE-0407** (Member Macros, 2023-09).
- **SE-0411** (Isolated default value expressions, 2024) interactions
  with macro-generated code.
- **SE-0494** (Macros as Compiled Plug-ins, 2025-08) the
  performance-critical refinement that distributes macros as
  pre-compiled `.dylib` plug-ins instead of source modules, eliminating
  the per-build cold-start cost.

Macros are implemented as Swift packages that depend on
`swift-syntax` (github.com/swiftlang/swift-syntax). A macro implementation
is a Swift type conforming to `ExpressionMacro`, `DeclarationMacro`,
`MemberMacro`, `PeerMacro`, `ExtensionMacro`, or `AccessorMacro`
(depending on the macro kind). The implementation receives a
`SyntaxNode` for the macro call site, parses any user-provided
arguments via `swift-syntax`, and returns a `Syntax` tree of the
expanded code. The Swift compiler invokes the macro implementation in
a separate process (`SwiftSyntaxMacrosPluginProvider`) for safety; the
compiler and the plug-in communicate via a JSON-RPC-like protocol over
stdin/stdout.

The **swift-syntax** library is the canonical Swift parser. It is a
pure-Swift parser (no C, no LLVM dependency) that produces a faithful
AST including trivia (whitespace, comments). The library is used by
swift-format, swift-syntax-macro-plugin, Xcode's source editor for
syntax highlighting and refactoring, and any third-party tool that
needs to parse Swift source. Crucially for MEP-49, swift-syntax can also
*construct* Swift source: every `SyntaxNode` has a builder API and a
`formatted()` method that produces well-formatted Swift text.

The reasoning for emitting Swift via swift-syntax instead of raw
string concatenation:

- Whitespace and trivia are handled automatically.
- Generated code is syntactically guaranteed valid (the builder
  rejects malformed trees at compile time).
- Refactoring the emitter is type-safe.
- Roundtrip stability: parsed code can be modified and re-emitted
  losslessly.

**Lesson for MEP-49:** Mochi's Swift emit pass MUST use swift-syntax,
not raw string templates. The emit pass should construct a
`SourceFileSyntax` tree per Mochi source file, run swift-format-style
formatting, and write the result. This is the prior-art model Skip
(§2), sourcery (§2), and every modern Swift code-generator uses. The
downside is that swift-syntax is a Swift library, and Mochi's compiler
is in Go; the cleanest workaround is to drive a small Swift-side
emitter via stdin/stdout JSON RPC, exactly like Apple's own macro
plug-in protocol. Reuse Apple's infrastructure rather than reimplement
it.

## 12. Bridging languages adjacent to Swift

**Objective-C** is Swift's historical companion and Apple platform's
lingua franca from 1989 to 2014. Every Swift program on an Apple
platform can call any Objective-C class declared in a bridging header
or `module.modulemap`. The bridging is mostly automatic: Objective-C
selectors become Swift methods, properties become Swift properties,
`NSError**` out-parameters become `throws`. Foundation types
(`NSString`, `NSArray`, `NSDictionary`) auto-bridge to Swift natives
(`String`, `Array`, `Dictionary`) at function boundaries with
identity-preserving casts.

Going the other direction (Swift → Objective-C) is more restricted:
only classes inheriting from `NSObject` (or marked `@objc`) are
visible to Objective-C, and only methods/properties marked `@objc`
(or implicitly inferred for `NSObject` subclasses) are callable. Swift
structs, enums (other than `@objc` enums), and generics are not
visible to Objective-C. This is the main limitation when bridging a
Swift library to a mixed Swift/Objective-C codebase.

**Objective-C++** is Apple's compiler mode that lets Objective-C and
C++ coexist in a single source file (file extension `.mm`). Used for
bridging Swift to C++ libraries before SE-0381's direct C++ interop
(2023) made `.mm` shim files unnecessary in most cases.

**C++ interop in Swift 5.9+** (SE-0381 Mixed-language interop with C++,
accepted 2023-04-27, implemented in 5.9 on 2023-09-18) is the
direct-bidirectional interop. Swift code can import C++ headers
directly (after declaring the C++ module via `module.modulemap` or
`-cxx-interop`), use C++ classes with proper destruction semantics,
call C++ methods, and pass Swift types to C++ where ABI allows. C++
code can also call Swift code (via Swift's C ABI when functions are
`@_cdecl`-annotated, or via the generated C++ header from Swift). The
interop covers: C++ classes (with constructors, destructors,
member-functions, virtual methods), enums (mapped to Swift enums or
`Int32` per opt), templates (limited; only fully-instantiated forms
are imported), and the STL (subset; `std::string`, `std::vector`,
`std::optional` work). Apple's USDZ/USD libraries use C++ interop in
production as of 2024.

The detailed proposals defining this are SE-0381 (the overarching
mixed-language proposal), SE-0455 (Foreign reference types, 2025
draft, for opting Swift classes into C++-style explicit ownership), and
the ongoing C++ Interoperability Workgroup at
forums.swift.org/c/development/c-interoperability.

**Lesson for MEP-49:** Mochi's runtime helpers for the Swift target
should be **pure Swift** with `@_extern(c)` exports for hot paths if
necessary, not C++ or Objective-C. The bridging cost is meaningful
only if Mochi needs to integrate with existing C++ codebases; for the
default Mochi-only codepath, stick to Swift.

## 13. Foreign function interface

Swift's FFI surface is rich and well-documented but has a deep
"underscore prefix" history that conflates official and unofficial
APIs.

**`@_silgen_name`** (private, no official proposal) is the oldest
mechanism, dating to Swift 1.x. It binds a Swift function to an
arbitrary SIL/LLVM symbol name. Used internally by the Swift standard
library to bind Swift functions to runtime entry points like
`swift_retain`. *Not for public use*; Apple has stated repeatedly that
`@_silgen_name` is private and may break without warning.

**`@_cdecl("name")`** (private, but widely used) exports a Swift
function with C ABI under the given symbol name. The function must
have C-compatible parameters (no Swift generics, no `String`, no
classes; primitives, `UnsafePointer`, `OpaquePointer`, `@convention(c)`
function pointers). Used to write callbacks for C libraries that
require a function pointer of a specific shape.

**`@convention(c)`** (public, on function types) marks a function-type
expression as having C calling convention. A `@convention(c) (Int32)
-> Int32` is a function pointer compatible with `int(*)(int32_t)` in
C. Used pervasively in `UIKit`, `CoreGraphics`, and any binding to a
C library.

**`@_extern(c, "name")`** (private experimental, since Swift 5.9; will
become `@extern(c, "name")` under SE-0455 or a successor, currently
in pitch as of 2025-12) is the long-awaited *importable* C function
declaration. Instead of needing a `module.modulemap` to expose C
functions, Swift code can write:

```swift
@_extern(c, "memcpy") 
func cMemcpy(_ dst: UnsafeMutableRawPointer, 
             _ src: UnsafeRawPointer, 
             _ n: Int) -> UnsafeMutableRawPointer
```

and the linker resolves `cMemcpy` to the standard C `memcpy` symbol.
This is the cleanest FFI declaration syntax Swift has produced and is
what Mochi-to-Swift should target for runtime helpers in v0+1.

**Unsafe pointers** are the bulk of Swift's FFI vocabulary:
- `UnsafePointer<T>` (immutable T-pointer)
- `UnsafeMutablePointer<T>` (mutable T-pointer)
- `UnsafeRawPointer` / `UnsafeMutableRawPointer` (untyped pointer,
  for byte-level access)
- `UnsafeBufferPointer<T>` / `UnsafeMutableBufferPointer<T>` (bounded
  array slice)
- `UnsafeRawBufferPointer` / `UnsafeMutableRawBufferPointer` (untyped
  byte slice)
- `AutoreleasingUnsafeMutablePointer<T>` (Objective-C-only, for
  `inout` parameters bridging to `id *`)

The functions `withUnsafePointer(to:)`, `withUnsafeBytes(of:)`,
`withUnsafeMutableBufferPointer`, and friends are the Swift-blessed
way to get a pointer to a Swift value with explicit scoped lifetime.

**Module maps** (LLVM's `module.modulemap` format, since 2013) are how
Swift consumes C libraries that are not pre-built as Swift modules.
A `module.modulemap` declares a C header bundle as a *Clang module*;
the Swift compiler imports it via `import CModuleName` and the
ClangImporter generates Swift-side bindings on the fly.

**SE-0518** (Pointer-family operations, in active review 2026-02) is
the in-flight proposal to unify the unsafe pointer APIs and add
typed-throws ergonomics; not yet final.

**Lesson for MEP-49:** Mochi-to-Swift's runtime helper layer should
use `@_extern(c, "name")` (or its eventual stable form) for C-ABI
imports, and `@_cdecl("mochi_xyz")` for Swift-side helpers called
from generated C-helper shims. Pointer-typed APIs in Mochi (rare, but
the `bytes` and `cptr` types in mochi_runtime) map to
`UnsafeMutableRawBufferPointer`.

## 14. Lessons learned from Skip.tools (Swift → Kotlin)

What Skip got right (recapitulating §2 with sharper framing):

1. **Source-level transpilation, no runtime.** Skip generates Kotlin
   source files that get compiled by the ordinary Kotlin compiler
   alongside hand-written Kotlin. There is no Skip runtime library
   that programs must link against beyond the standard SkipUI / Skip
   Foundation bindings. This is the model MEP-49 should follow.

2. **Incremental adoption.** A team can introduce Skip on one Swift
   file at a time, mixing transpiled and native Kotlin. The
   `@Skip` attribute marks files / declarations to be transpiled;
   un-marked files are pure Kotlin.

3. **Build system integration.** Skip plugs into both SwiftPM (on the
   iOS side) and Gradle (on the Android side) as ordinary build
   plug-ins, not separate command-line tools.

4. **Use of swift-syntax.** Skip's transpiler is itself a Swift
   program built on swift-syntax, which gives it perfect Swift parsing
   and faithful trivia preservation.

What Skip got wrong:

1. **Unidirectional flow.** Skip only transpiles Swift → Kotlin, never
   Kotlin → Swift. A mixed Skip / Kotlin team has to keep "the Swift
   side is the source of truth" mentally everywhere.

2. **Build-time penalty.** A fresh build of a 50kLOC Skip app adds
   30-60s to the Android build. Incremental builds are fast, but the
   first-time build experience is jarring.

3. **SwiftUI → Compose impedance.** Some SwiftUI constructs (`Canvas`,
   `GeometryReader`, complex `PreferenceKey` chains) don't have clean
   Jetpack Compose equivalents and produce either unimplemented
   warnings or non-faithful translations.

4. **Closed-source Pro tier.** SkipUI is paid; SkipLite is free. This
   has slowed enterprise adoption and contributed to a fragmented
   open-source ecosystem.

## 15. Lessons learned from Kotlin Multiplatform

KMP (Kotlin Multiplatform; v1.6 official-multiplatform in 2021, v1.9
production-stable in 2023-11, v2.0 K2-baseline in 2024-05) is the
incumbent "share business logic across iOS and Android" technology.

What KMP got right:

1. **True shared codebase.** `commonMain` is portable Kotlin; each
   platform's source set supplies platform-specific impls via
   `expect`/`actual` declarations. There is no transpilation; each
   target uses the appropriate Kotlin backend (JVM, Native, JS, Wasm).

2. **Multi-IDE support.** Android Studio (with the KMP plug-in) is
   tier-1; AppCode and Fleet support KMP; Xcode integration is via
   the Kotlin/Native binary framework export.

3. **Mature ecosystem.** kotlinx.serialization, kotlinx.coroutines,
   Ktor, SQLDelight, and Decompose are all KMP-friendly. Production
   users (Cash App, Wrike, Physics Wallah, JetBrains) prove KMP's
   readiness for large apps.

4. **Compose Multiplatform** (v1.8 in 2025-05) for shared UI across
   iOS and Android is the killer feature differentiating KMP from
   sharing-business-logic-only stories.

What KMP got wrong:

1. **kotlin-native ABI churn.** Kotlin/Native (the LLVM-backed
   backend) had ABI changes between every major Kotlin release until
   1.9, which made consuming kotlin-native binaries from Swift code
   fragile. The 2.0 release stabilized the situation but did not fix
   pre-existing artifact rot.

2. **IDE friction on iOS.** Xcode has no native Kotlin support; KMP
   developers must use Android Studio for Kotlin editing and Xcode
   for Swift / Storyboard / asset editing. Round-tripping is
   awkward.

3. **Build time on iOS.** Kotlin/Native compilation to an iOS XCFramework
   is slow (90-300s on a fresh build for a moderately sized module).
   Incremental builds are faster but still not as fast as native
   Swift.

4. **Swift interop limitations.** Generics are erased, suspend
   functions surface as completion-handler-style callbacks in Swift,
   no Swift-side concurrency types are visible. The interop layer is
   functional but not idiomatic from the Swift side.

## 16. What SwiftWasm proves; what Embedded Swift proves

**SwiftWasm** proves that Swift can be retargeted to non-LLVM-mainline
backends through the `swift-driver` plus stdlib porting work. The
upstreaming of WebAssembly support (Swift 5.7 partial, Swift 6.0 full)
into the official Apple-maintained compiler shows that the Swift
project's governance model can absorb significant third-party
contributions. For Mochi, this is the precedent that Mochi-to-Swift's
own target additions (Mochi runtime helpers as a Swift package) are
welcome contributions to the broader ecosystem, not friction.

**Embedded Swift** proves that the language can target environments
without ARC, without `String`, without dynamic features. This in turn
proves that Swift is not architecturally locked into Apple-platform
mid-tier mobile, it can scale from microcontrollers (Embedded mode)
through WASM (Embedded + WASI) through Linux (musl-static) through
servers (Vapor / Hummingbird) through Apple platforms (full Swift).
Mochi-to-Swift therefore has a wider deployment range than initially
visible from "Swift = iOS apps."

## 17. What MEP-49 takes from prior art

Distilled to actionable guidance:

1. **Lower through staged IRs, never directly to Swift source.** Mochi-AST
   → Mochi-IR (shared with the C, BEAM, JVM, .NET backends per [[MEP-45]],
   [[MEP-46]], [[MEP-47]], [[MEP-48]]) → Swift-IR (a small Mochi-side
   layer encoding the Swift-emit decisions) → swift-syntax tree → Swift
   source. The Swift-IR layer is where ARC-vs-`~Copyable` decisions,
   `actor` vs `class` choices, and `Sendable` annotations are made.

2. **Emit Swift source via swift-syntax**, not raw string concatenation.
   The Mochi compiler is in Go; the cleanest design is a small
   Swift-side emitter daemon driven via stdin/stdout JSON RPC, exactly
   the macro plug-in protocol Apple already ships. Reuse Apple's
   infrastructure (`SwiftCompilerPlugin`, `SwiftSyntaxMacrosPluginProvider`).
   Alternatively, ship a Go-native Swift-AST builder for the subset of
   syntax Mochi emits, accepting the maintenance burden of staying in
   sync with Swift grammar evolution.

3. **SwiftPM as the canonical build driver.** Generate `Package.swift`
   alongside Swift sources. Reserve a SwiftPM build plugin as a v0+1
   refinement so Mochi sources can sit under `Sources/` and trigger
   transpilation at `swift build` time.

4. **Strict concurrency on by default (Swift 6 language mode).** Mochi
   programs are concurrency-safe by construction in MEP-49's design;
   the emit pass must mark all Mochi-emitted types `Sendable`,
   isolate `agent`-mapped types as `actor`s, and use `nonisolated(unsafe)`
   only when the type is genuinely thread-safe via internal locking.

5. **AsyncStream / AsyncSequence over Combine for agent message passing.**
   Combine has been Apple's reactive framework since iOS 13 (2019), but
   since iOS 15 (2021) the AsyncStream / AsyncSequence APIs from
   Swift Concurrency have been the recommended replacement. WWDC 2023's
   "Meet AsyncSequence" session and Apple's own framework direction
   (Observation, SwiftData, SwiftUI's task modifier) all point to
   AsyncSequence as the post-Combine future. Mochi's `agent` streams
   should emit `AsyncStream<Message>` for inbound queues and
   `AsyncStream<Event>` for outbound, with structured concurrency
   (TaskGroup) for supervision. See [[09-agent-streams]].

6. **ARC for value lifetimes, actors for concurrency, AsyncStream for
   queues.** This is the Apple-blessed combination as of Swift 6. Mochi
   inherits this triple without inventing alternatives.

7. **Apple platform packaging via shelling-out.** Do not reimplement
   codesign, notarytool, or xcodebuild. Delegate to Apple's toolchain
   from `mochi build --target=swift --platform=ios`. See [[10-build-system]].

8. **Static Linux SDK for server builds.** The `mochi build
   --target=swift --platform=linux` default should produce a static
   musl-linked binary via Swift 6.0's Static Linux SDK, exactly the
   model Vapor and Hummingbird use.

9. **No JIT.** Mochi-to-Swift inherits Swift's AOT-only model. The
   start-up advantages over JVM/.NET are real; the cost is no
   adaptive runtime optimisation. For Mochi's typical agent-style
   workloads, AOT is the right choice.

10. **Reserve Embedded Swift and SwiftWasm as future profiles.** Both
    are technically interesting and ecosystem-relevant, but v0 of
    MEP-49 targets the full Swift language with full stdlib. Future
    profiles can subset.

11. **C++ interop is unnecessary for Mochi.** The Mochi runtime is
    pure Swift; C interop via `@_extern(c)` covers FFI for system calls.
    Reserve C++ interop for users who voluntarily bridge Mochi to
    C++ codebases.

12. **swift-syntax is the only acceptable Swift parser/emitter.**
    Anything else (regex-based hacks, hand-rolled string templates)
    will desync from Swift grammar evolution and produce un-parseable
    output. The swift-syntax dependency is non-negotiable.

13. **Multi-platform CI on Tart for Apple targets.** Mochi's CI for the
    Swift target needs macOS runners on Apple Silicon for tier-1
    coverage, plus Linux runners for the Vapor/server side, plus
    Windows runners for Swift-on-Windows tier-2 coverage. Tart on
    Mac mini hardware (or Cirrus' hosted service) is the practical
    answer in 2026.

14. **Swift 6.0 LTS floor (2024-09); Swift 6.2 secondary baseline.**
    Swift 6.0 introduced the strict-concurrency language mode that
    Mochi's design assumes. Swift 6.2 (2025-09) added expanded
    Embedded mode and Swift Testing 1.0 LTS. Targeting both means
    generating Swift 6-language-mode-compatible source while opting
    into Swift 6.2 features behind language-mode flags. Swift 7.0
    is not yet on the swift-evolution roadmap for 2026.

15. **Reserve a `.mochi-meta` resource in SwiftPM packages** for the
    analogue of Scala 3's TASTy, Kotlin's `@Metadata`, or .NET's
    embedded PE metadata: Mochi type information, Datalog facts,
    agent topology, query DSL ASTs, LLM prompt templates. Mochi's
    generated Swift sources are the lossy ground truth; the
    `.mochi-meta` block in the package resources is the precise
    truth. Future IDE tooling and incremental compilation will need
    this.

## Open questions to flag

- **Should Mochi expose Swift interop syntactically?** Skip's success
  is largely because transpiled Kotlin reads like hand-written Kotlin
  and can call any Kotlin/JVM library. Mochi-emitted Swift should
  similarly read like hand-written Swift; Mochi types should be
  `Codable`, `Hashable`, `Sendable` by default when their fields
  allow, and Mochi agents should be `actor`s callable from any Swift
  caller. See [[06-type-lowering]].

- **How does Mochi's query DSL lower on Swift?** Candidates: emit
  Swift sequence/collection operations (`map`/`filter`/`reduce`),
  emit SwiftData (Apple's SQLite-backed ORM since iOS 17, 2023-09)
  queries for persisted data, or bind to GRDB.swift (the most popular
  third-party Swift SQLite library). See [[08-dataset-pipeline]].

- **How does Mochi's LLM/Datalog feature interact with Apple's
  on-device ML?** Apple Intelligence (announced WWDC 2024) and the
  `FoundationModels` framework (introduced in iOS 18.1, 2024-10) let
  Swift apps call Apple's on-device LLMs without network round-trips.
  Mochi's LLM prim could lower to FoundationModels on Apple platforms
  with a fallback to OpenAI/Anthropic HTTP on Linux/server targets.

- **Should Mochi-to-Swift share a build pipeline with Skip?** If
  Mochi-to-Swift emits Skip-compatible Swift, Mochi programs gain free
  Android targeting via Skip's Kotlin transpilation. The coupling is
  significant but the leverage is enormous. Worth a v0+1 prototype.

- **Tail-call elimination on Swift.** Swift has no general TCO. Mochi's
  `func f() = f()`-shape recursion must either trampoline or compile
  self-recursive tail calls to `while`-loops in the same function.
  Self-recursive TCO is straightforward at the IR level; mutual
  recursion needs trampolining via a continuation-style transformation.
  Recommendation: detect self-recursion at the Mochi-IR level and emit
  `while`; provide a `@trampoline` opt-in for mutual recursion.

- **How does Mochi-to-Swift survive Swift evolution?** SE-0494 (macros
  as compiled plug-ins) is the kind of toolchain change that affects
  Mochi's emit strategy. Pin a Swift version per Mochi release; track
  swift-evolution Accepted proposals in the Mochi changelog;
  participate in the swift-evolution review process for proposals that
  affect generated-code shapes (e.g. SE-0455 foreign reference types,
  SE-0510 Android tier-1, SE-0518 pointer ops).

## Sources

(URLs and references gathered during this research pass.)

Swift compiler / SIL / ABI:
- swift.org/blog (especially "Swift ABI Stability" 2019-03-25,
  "Swift 6.0 Released" 2024-09-17, "Library Evolution in Swift" 2018-12)
- github.com/swiftlang/swift (compiler source)
- docs.swift.org/swift-compiler/Swift_Compiler_Internals.html
- github.com/swiftlang/swift/blob/main/docs/SIL.rst
- "Understanding Swift's ABI Stability" (apple.com/swift/blog/?id=42, 2018-12)
- "Module Stability" (swift.org/blog/swift-5-1-released/, 2019-09-20)

swift-evolution:
- github.com/swiftlang/swift-evolution (proposals)
- forums.swift.org/c/evolution (review threads)
- swift.org/swift-evolution (proposal listings)
- specific SE proposals cited: SE-0381 (C++ interop), SE-0382 (Expression
  Macros), SE-0389 (Attached Macros), SE-0407 (Member Macros),
  SE-0411 (Isolated defaults), SE-0414 (Region isolation), SE-0428
  (Embedded Swift), SE-0444 (Member-import visibility), SE-0455
  (Foreign reference types), SE-0490 (Concurrency in Swift 6),
  SE-0494 (Macros as Compiled Plug-ins), SE-0510 (Android tier-1, draft),
  SE-0518 (Pointer ops, in review)

Skip / Kotlin Multiplatform:
- skip.tools and github.com/skiptools/skip
- skip.tools/docs/skipui
- jetbrains.com/lp/multiplatform/
- kotlinlang.org/docs/multiplatform.html
- blog.jetbrains.com/kotlin/2025/05/compose-multiplatform-1-8-0/
- developers.google.com/j2objc (archived 2025-09)
- skip.tools/blog/skip-and-kotlin-multiplatform

Swiftify / C-to-Swift:
- swiftify.com
- swiftify.com/blog/c-to-swift-translation-v3

Sourcery / swift-syntax:
- github.com/krzysztofzablocki/Sourcery
- github.com/swiftlang/swift-syntax
- swift.org/documentation/articles/swift-syntax.html
- WWDC 2023 "Write Swift macros" (developer.apple.com/videos/play/wwdc2023/10166/)
- WWDC 2024 "Expand Swift macros" (developer.apple.com/videos/play/wwdc2024/10092/)

Hylo / Mojo / Pkl:
- hylo-lang.org and github.com/hylo-lang/hylo
- "Mutable Value Semantics for Swift" (Racordon, 2022)
- modular.com/mojo and github.com/modular/mojo
- pkl-lang.org and github.com/apple/pkl
- github.com/apple/pkl-swift
- "Introducing Pkl, a programmable configuration language" (apple opensource blog, 2024-02)

Swift on non-Apple platforms:
- swift.org/install for Windows / Linux / Android
- github.com/swiftlang/swift-corelibs-foundation
- github.com/swiftlang/swift-foundation
- github.com/finagolfin/swift-android-sdk
- "Swift Static Linux SDK" (WWDC 2024 session 10210)
- vapor.codes and github.com/vapor/vapor
- github.com/hummingbird-project/hummingbird
- swift.org/sswg (Swift Server Workgroup)
- github.com/apple/swift-nio

Embedded Swift / SwiftWasm:
- WWDC 2024 "Go small with Embedded Swift" (developer.apple.com/videos/play/wwdc2024/10197/)
- github.com/apple/swift-embedded-examples
- swiftwasm.org and github.com/swiftwasm
- github.com/swiftwasm/JavaScriptKit
- "Swift goes WebAssembly" (forums.swift.org, 2024)

Apple platform packaging:
- developer.apple.com/documentation/xcode/build-system
- developer.apple.com/documentation/security/notarytool
- developer.apple.com/documentation/xcode/distributing-your-app
- developer.apple.com/documentation/storekit (StoreKit 2)
- "Bundle Programming Guide" (developer.apple.com/library/archive)
- xcrun notarytool, xcrun stapler man pages

ARC / ownership / strict concurrency:
- "Automatic Reference Counting" chapter of The Swift Programming Language
- "Ownership Manifesto" (github.com/swiftlang/swift/blob/main/docs/OwnershipManifesto.md)
- SE-0377 inout/borrow/consume
- SE-0390 noncopyable structs and enums
- SE-0432 move-only generics
- "Migrating to Swift 6" (swift.org/migration/documentation/migrationguide)

SwiftPM:
- swift.org/package-manager
- swift.org/blog/swift-5.6-released (build plugins)
- swift.org/blog/swift-5.9-released (command plugins, expanded)
- github.com/SwiftPackageIndex
- swiftpackageregistry.com
- SE-0292 Package Registry

WWDC sessions referenced:
- WWDC 2019 "Modern Swift API Design"
- WWDC 2021 "Meet async/await in Swift"
- WWDC 2022 "Meet distributed actors"
- WWDC 2023 "Meet AsyncSequence", "Discover Observation in SwiftUI",
  "Write Swift macros", "Demystify SwiftUI performance"
- WWDC 2024 "Go small with Embedded Swift", "Migrate your app to Swift 6",
  "Bring your app to Mac and visionOS"
- WWDC 2025 "What's new in Swift" (Swift 6.2)
