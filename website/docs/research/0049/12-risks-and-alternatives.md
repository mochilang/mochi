# MEP-49 research note 12, Risks and alternatives for MEP-49

Author: research pass for MEP-49 (Mochi to Swift transpiler).
Date: 2026-05-23 (GMT+7).

This note catalogues the load-bearing risks of MEP-49 and the
alternatives that were considered and rejected. Each entry links
back to the load-bearing decision in [[02-design-philosophy]] and
the affected implementation note.

## 1. Risk register

### R1: Swift Concurrency runtime evolution

**Risk.** Swift Concurrency landed in Swift 5.5 (Sept 2021) and has
seen breaking-ish changes through 5.7 (existential `any`), 5.9
(macros and ownership), 6.0 (strict concurrency, region isolation,
typed throws), 6.2 (approachable concurrency, closures default
non-isolated). A Mochi codebase compiled against Swift 6.0 may emit
warnings or even fail to compile under Swift 6.5 if Apple adjusts
the language mode default again.

**Mitigation.** MEP-49 floors at Swift 6.0 language mode and pins
to it. The codegen pass emits explicit isolation annotations on
every closure, function, and actor method so the meaning is
unambiguous regardless of future default changes. CI runs the
ceiling (Swift 6.1, 6.2) advisory matrix to catch regressions
early.

**Residual risk.** Medium. Swift's evolution velocity is higher
than JVM's; the analogous .NET risk (R3 in [[mep-0048]]) is lower
because .NET LTS cadence is more conservative.

See [[02-design-philosophy]] §3, [[09-agent-streams]] §15.

### R2: App Store / Apple Developer Program gatekeeping

**Risk.** Apple controls the iOS distribution chain. App Review can
reject apps for arbitrary reasons: novelty, "spam," competition
with first-party apps. Mochi-generated apps may face higher
scrutiny if Apple flags them as transpiled or AI-generated.

**Mitigation.** MEP-49 emits idiomatic Swift indistinguishable from
hand-written Swift after `swift-format`. The generated code does
not embed Mochi compiler watermarks, does not call private SPI,
does not bundle a runtime evaluator (no JIT). App Review heuristics
target webviews and HyperCard-style content engines; Mochi output
is plain native Swift.

**Residual risk.** Low-medium. We cannot influence Apple's
discretion; we can only minimise the attack surface.

See [[07-swift-target-portability]] §10-11, [[11-testing-gates]] §8.

### R3: Sendable enforcement false positives

**Risk.** Swift 6.0's complete sendable checking is conservative.
Mochi codegen may emit a value that Mochi's static check knows is
sendable (because Mochi's type system already proves it) but Swift's
checker rejects (because it requires explicit `: Sendable`).

**Mitigation.** Mochi codegen emits explicit `: Sendable`
conformances on every type. Where Mochi knows a closure is sendable
(no captures or only sendable captures), the codegen emits
`@Sendable`. Where Swift's check fails, the codegen falls back to
boxing the value in a wrapper class that uses a lock (the "we know
this is fine, but Swift doesn't" escape hatch).

**Residual risk.** Low. The fallback is correct but slower; Mochi
emits a warning when the fallback fires, prompting the user to
adjust Mochi-side type annotations.

See [[06-type-lowering]] §17, [[11-testing-gates]] §4.

### R4: swift-corelibs-foundation parity gaps

**Risk.** swift-corelibs-foundation (Linux, Windows) is not a 1:1
reimplementation of Apple Foundation. As of Swift 6.0: NSCalendar
is partial, NSTimeZone needs tzdata, Process semantics differ,
NSXMLParser is missing.

**Mitigation.** Mochi runtime polyfills the gaps via swift-system
(FilePath, FileDescriptor), swift-foundation-icu (Foundation
substitute being built), and pure-Swift implementations of missing
features. The polyfill is tested against both Apple and corelibs
Foundation in CI.

**Residual risk.** Medium. Apple is rewriting Foundation in Swift
(swift-foundation, github.com/swiftlang/swift-foundation, Sept 2023
announcement) targeting full parity by Swift 6.x. By 2026-2027 the
gap should close.

See [[04-runtime]] §3, [[07-swift-target-portability]] §5.

### R5: swift-syntax dependency size

**Risk.** apple/swift-syntax is ~10MB compiled. If Mochi codegen
uses it directly, every Mochi build pulls a large dependency.

**Mitigation.** MEP-49 does NOT take a direct dependency on
swift-syntax. The codegen pass implements a Go-side syntax model
that emits canonical Swift text. swift-format is invoked optionally
post-emit if the user has a Swift toolchain installed.

**Residual risk.** Low. The trade-off is hand-rolling Swift
formatting logic; we lean on swift-format for canonicalisation.

See [[05-codegen-design]] §1, §7, §8.

### R6: SwiftPM build performance

**Risk.** SwiftPM is slower than `go build` or `cargo build` for
medium-sized projects. Cold build of a 50K-LOC Swift project on M1
macOS arm64 takes 2-5 minutes. Mochi adds 4000 LOC of generated
Swift per Mochi module on top.

**Mitigation.** Mochi codegen emits multiple `.swift` files per
module to enable parallel compilation. SwiftPM build cache
(`~/.swiftpm/build`) reuses incremental compilation results. CI
uses `actions/cache` to persist .swiftpm-cache across runs.

**Residual risk.** Medium. Slow builds hurt iteration loop; mitigated
by `mochi run` keeping vm3 path fast.

See [[10-build-system]] §22.

### R7: iOS app bundle reproducibility

**Risk.** Apple's codesign tool embeds the codesign timestamp into
the binary. Notarization adds a unique ticket. .ipa archives are
zips that may have non-deterministic file ordering. Two runs of
`mochi build --target=swift-ios` may produce different bytes even
on the same machine.

**Mitigation.** MEP-49 reproducibility gate (Phase 16) excludes
codesign and notarization from the byte-equality check. The
unsigned, unstapled binary is bit-identical. Signing is a final
distribution step.

**Residual risk.** Low (with the scope carve-out).

See [[11-testing-gates]] §10.

### R8: Static Linux SDK breakage

**Risk.** swift-static-linux-sdk is a young addition (Swift 5.9,
Sept 2023). Toolchain bugs may surface when statically linking
complex Mochi programs that pull C dependencies. Specifically: ICU
must be statically embedded; Foundation networking via libcurl is
statically linked; TLS via mbedTLS (swift-static-linux-sdk's choice).

**Mitigation.** MEP-49 v1 ships static Linux as Phase 17 (not v1
landing), letting the SDK stabilise. We test against Static Linux
SDK 6.0 + 6.1 and pin to the working versions.

**Residual risk.** Medium. Apple ships the SDK; quality is high
but young.

See [[07-swift-target-portability]] §4, [[11-testing-gates]] §9.

### R9: Foundation-models opacity

**Risk.** Apple's FoundationModels framework (iOS 18+, macOS 15+
Apple Silicon only) is the on-device LLM. API stability is not
guaranteed; the framework is marked `Beta` in some headers.

**Mitigation.** Mochi `generate` lowers to FoundationModels on
eligible Apple platforms with a swift-openai-async fallback for
ineligible platforms (Linux, Windows, old Apple). The fallback path
is the default for CI.

**Residual risk.** Medium-high. FoundationModels may break
between iOS minor versions.

See [[04-runtime]] §4, [[01-language-surface]] §9.

### R10: Cross-target divergence on streams

**Risk.** Streams have observable nondeterminism: ordering of
interleaved AsyncSequence elements depends on Task scheduler. Five
backends scheduling Tasks differently produce different stdouts.

**Mitigation.** Stream fixtures are explicitly marked as
non-deterministic and excluded from differential testing
([[11-testing-gates]] §3). Mochi assertions on stream output use
set-equality or sort-then-compare.

**Residual risk.** Low (with scope carve-out).

See [[09-agent-streams]] §20, [[11-testing-gates]] §12.

### R11: Windows toolchain stability

**Risk.** Swift on Windows reached 1.0 quality in Swift 5.9 (Sept
2023) but the ecosystem is thin. Some packages (apple/swift-nio)
have spotty Windows CI. Mochi runtime depends on swift-collections,
swift-algorithms, swift-async-algorithms; verify Windows builds.

**Mitigation.** CI runs windows-2025 matrix on every PR. Verified
all swift-* runtime deps build clean on Windows as of Swift 6.0.

**Residual risk.** Low-medium.

See [[07-swift-target-portability]] §6-7, [[10-build-system]] §18.

### R12: ARC reference cycles in agents

**Risk.** Mochi agents lower to Swift `actor`. If two agents
reference each other strongly through stored closures, ARC leaks.
Erlang-style supervised actor topologies are common; the leak
risk is real.

**Mitigation.** MochiRuntime.Supervisor holds children as `weak`
references. Cross-agent references emitted by the codegen pass use
`unowned` for parent links and `weak` for sibling caches. Mochi's
type system distinguishes "owning" from "referencing" links.

**Residual risk.** Medium. Cycle detection in ARC requires manual
care; a future MEP could ship an ARC cycle detector.

See [[09-agent-streams]] §10-12, [[02-design-philosophy]] §9.

### R13: ABI churn in Swift 6.x

**Risk.** Apple promised ABI stability in Swift 5.0 (Mar 2019) and
has held it through 6.x. But ABI applies only to the standard
library; third-party packages (swift-collections etc.) have no ABI
guarantee. A swift-collections 2.0 could break Mochi.Runtime
binary compatibility.

**Mitigation.** Mochi.Runtime pins exact versions of swift-*
dependencies in Package.resolved and ships a major-version pin in
Package.swift (e.g., `from: "1.1.0"`). Major-version bumps require
a Mochi.Runtime major bump.

**Residual risk.** Low. swift-collections has not had a major
version bump and is unlikely to in the MEP-49 v1 timeframe.

See [[04-runtime]], [[10-build-system]] §3.

### R14: Mochi codegen complexity drift

**Risk.** Five target codegens (C, BEAM, JVM, .NET, Swift) sharing
an aotir IR but each with target-specific lowering is operationally
complex. A Mochi language change that lands easily for vm3 may
require five separate target updates.

**Mitigation.** All five targets share monomorphisation and
closure-conversion passes ([[05-codegen-design]] §5). Target-specific
codegen passes are small (~4000 LOC each). The fixture corpus is
shared, so a Mochi feature must produce identical output across
targets, catching divergence early.

**Residual risk.** Medium. As Mochi grows, target maintenance
burden grows. Mitigated by automated cross-target gates.

See [[11-testing-gates]] §12.

## 2. Alternatives considered

### A1: Objective-C as the target language

**Rejected.** See [[02-design-philosophy]] §2. Objective-C is on a
long decommissioning path; no Swift 6 features map cleanly; new
Apple frameworks are Swift-only.

### A2: Swift as the target language, but Swift 5.10 as floor

**Rejected.** See [[02-design-philosophy]] §3. Swift 5.10 lacks
typed throws (SE-0413), strict concurrency by default, region-based
isolation (SE-0414). Floor-ing at 5.10 forces dual codepaths (5.10
language mode vs. 6.0 mode) for marginal benefit (Xcode 15.x
support, which will be EOL by App Store cadence by 2026 H2).

### A3: Apple-only platform scope

**Rejected.** See [[02-design-philosophy]] §4. Locks Mochi out of
server-side Swift, which is the fastest-growing Swift segment.
Forces users to choose a different Mochi target for backend code.

### A4: GCD DispatchQueue for agent mailboxes

**Rejected.** See [[02-design-philosophy]] §5. Bypasses Swift
Concurrency's sendable checking. Combine, the natural reactive
companion, is Apple-only.

### A5: Custom Mochi-runtime actor scheduler

**Rejected.** See [[02-design-philosophy]] §5. Duplicates work
Swift Concurrency does well. Decouples Mochi from Swift's evolution
at the cost of reinventing the scheduler.

### A6: Swift Macros for code generation

**Rejected.** See [[02-design-philosophy]] §15. Macros expand per-
call-site, not per-module. Whole-program Mochi codegen needs
module-level analysis. Macros also require a Swift toolchain at
Mochi's compile time, complicating self-bootstrap.

### A7: LLVM IR direct emission

**Rejected.** Bypasses the Swift type system, loses ARC and
sendable checks, makes the output non-debuggable, prevents
Library Evolution. Closest analogue is .NET IL emission, where
MEP-48 chose Roslyn source over IL for the same reasons.

### A8: SwiftSyntax as a hard dependency

**Rejected.** See [[05-codegen-design]] §7. swift-syntax is ~10MB
compiled and requires a Swift host toolchain at build time. Mochi
self-bootstraps from Go; pulling Swift into Mochi's build chain
adds friction.

### A9: Combine for streams

**Rejected.** See [[02-design-philosophy]] §12. Apple-only.
Backpressure correctness issues. Superseded by AsyncSequence in
Apple's own framework direction.

### A10: Foundation-only, no Apple Frameworks

**Rejected.** Loses access to SwiftData, Observation,
FoundationModels. These provide value on Apple platforms; making
them opt-in via `@available` plus runtime detection is better.

### A11: Kotlin Multiplatform / Skip.tools as the Swift target

**Rejected.** Kotlin Multiplatform compiles Kotlin to Swift via
the Skip.tools transpiler. Using KMP as Mochi's Swift target would
add a Kotlin compilation layer (Mochi → Kotlin → Swift) for no
benefit. Mochi's type system is already at the Swift level of
expressiveness; KMP adds latency.

### A12: Direct .ipa packaging without Xcode

**Rejected.** iOS .ipa requires codesigning against a Provisioning
Profile that only Apple's Developer Portal issues. xcodebuild is
the canonical packaging tool; replacing it requires reverse-
engineering the Mach-O format, embedded provisioning profile, and
asset catalog format. Out of scope for v1.

## 3. Open questions

### Q1: Should Mochi-on-Swift expose SwiftUI?

A future MEP-N could add a `view` keyword that lowers to SwiftUI
`View` types. Not in MEP-49 scope; tracked as candidate v2.

### Q2: Should Mochi-on-Swift expose SwiftData?

A `persisted record` keyword that lowers to `@Model`. Tracked as
candidate v2.

### Q3: Should Mochi target Embedded Swift?

Microcontroller deployment via the Embedded Swift subset. Tracked
as v2 (deferred to MEP-49 Phase 19, not in v1 scope per
[[11-testing-gates]] §16).

### Q4: Should Mochi target SwiftWasm?

WebAssembly via swift-wasm. Tracked as v2 (deferred to MEP-49
Phase 20, [[11-testing-gates]] §17). Likely a separate MEP given
the differing constraints (no Foundation, no URLSession).

### Q5: Should Mochi target Windows ARM64?

Swift on Windows ARM64 is in early development (Swift 6.0 ships
x86_64 only). Tracked as candidate v2 when toolchain matures.

### Q6: Should Mochi support distributed actors?

`distributed actor` (SE-0336) for cross-process actor systems. A
natural fit for Mochi remote agents but requires a separate MEP
to design the ActorSystem (decouple from Swift Distributed Tracing,
swift-cluster, etc.).

### Q7: Should Mochi ship App Store submission automation?

`mochi build --target=swift-ios --publish-testflight` could shell
out to `xcrun notarytool` and App Store Connect API. Tracked as
candidate v2 (out of Phase 18 scope, which only validates).

## 4. Comparison with sibling MEPs

| Dimension              | MEP-45 (C)         | MEP-46 (BEAM)         | MEP-47 (JVM)             | MEP-48 (.NET)           | MEP-49 (Swift)          |
|------------------------|--------------------|-----------------------|--------------------------|--------------------------|--------------------------|
| Target lang            | C11                | Erlang (Core Erlang)  | Java 21                  | C# 12                    | Swift 6.0                |
| Codegen IR             | C source           | Core Erlang via cerl  | Java source via JavaPoet | C# via Roslyn            | Swift source             |
| Toolchain version      | clang/gcc          | OTP 27/28             | JDK 21/25 LTS            | .NET 8/10 LTS            | Swift 6.0/6.1            |
| Concurrency primitive  | pthreads           | OTP process           | Loom virtual thread      | Channel<T> + async       | actor + AsyncStream      |
| Streams primitive      | callback           | gen_event             | Flow.Publisher           | IAsyncEnumerable         | AsyncSequence            |
| Memory                 | manual + arena     | per-process GC        | Tracing GC               | Tracing GC               | ARC                      |
| Build driver           | make/cmake         | rebar3                | Gradle/Maven             | dotnet CLI               | SwiftPM                  |
| Package registry       | (none / system)    | hex.pm                | Maven Central            | NuGet                    | swift-package-index      |
| AOT/JIT                | AOT                | BEAM bytecode + JIT   | HotSpot JIT + GraalVM    | JIT + ReadyToRun + AOT   | AOT only                 |
| Distribution shapes    | single binary      | escript / OTP release | uberjar / jlink / native | dll / self-contained / AOT | .ipa / .app / static     |
| Hot reload             | no                 | yes (BEAM purge)      | yes (HotSwap, limited)   | no                       | no                       |
| Mobile target          | (via Embed manual) | no                    | Android via D8/R8        | MAUI via .NET MAUI       | iOS/iPad/visionOS native |
| App-store-friendly     | no                 | no                    | partial (Android)        | partial (.NET MAUI)      | yes (iOS/Mac App Store)  |

The Swift target uniquely covers Apple platforms App-Store-friendly
deployment, complementing the other four backends.

## 5. Out of scope for v1

Documented elsewhere but listed for closure:

- SwiftUI lowering (Q1).
- SwiftData persistence (Q2).
- Embedded Swift (Q3).
- SwiftWasm (Q4).
- Windows ARM64 (Q5).
- Distributed actors (Q6).
- App Store automation (Q7).
- Property-based testing.
- Fuzzing harness.
- ARC cycle detector.

Each of these is a candidate v2 follow-up; none block v1 landing.

## 6. Failure modes if MEP-49 is not done

The "do nothing" alternative: skip MEP-49 entirely, keep Mochi on
vm3 plus C/BEAM/JVM/.NET. The cost:

- No first-class iOS app development from Mochi.
- No first-class macOS app development from Mochi.
- No visionOS reach (the only AR/VR platform Apple ships).
- Server-side Swift users (Vapor/Hummingbird shops) must use a
  different Mochi backend or switch languages.
- Mochi positioning is "general-purpose" but missing the Apple
  ecosystem dents that claim.

The risk-adjusted value of doing MEP-49 is high: the user-facing
goal (Mochi on Apple platforms) is gated entirely on this MEP.

Cross-references: [[02-design-philosophy]] for the decision rationale,
[[01-language-surface]] for the surface contract, [[11-testing-gates]]
for the gate matrix that validates each decision.
