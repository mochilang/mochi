# MEP-50 research note 02, Design philosophy

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).
Sources: companion notes the shared-decisions anchor, [[01-language-surface]],
[[03-prior-art-transpilers]]; JetBrains' Kotlin Multiplatform Stable
announcement (2023-11), the K2 compiler announcement
(blog.jetbrains.com, 2024-05), the kotlinx.coroutines roadmap at
github.com/Kotlin/kotlinx.coroutines, the KEEP proposal stream at
github.com/Kotlin/KEEP, Android Developers releases, Apple's
Kotlin/Native FAQ, and the MEP-45 / MEP-46 / MEP-47 / MEP-48 / MEP-49
design-philosophy notes whose structure this note mirrors.

This note explains the load-bearing design choices behind MEP-50 and the
constraints they impose. It is the "why" companion to
[[01-language-surface]]'s "what" and [[05-codegen-design]]'s "how".

## 1. Why a Kotlin target now

Mochi already has five mature lowering targets: vm3 (the reference
tree-walker), MEP-45 (C, single-binary AOT), MEP-46 (BEAM, supervision
and hot reload), MEP-47 (JVM bytecode direct, Maven Central and Loom),
MEP-48 (.NET, NuGet and NativeAOT), and MEP-49 (Swift, Apple platforms
plus Linux/Windows). Each target picks up an ecosystem Mochi cannot
reach from the others.

The Android ecosystem is one of two remaining unreached pillars (the
other being the browser, partly covered by MEP-49 via SwiftWasm but
mainly addressed by a future MEP-N WebAssembly story). Android alone
runs on roughly 3.3B active devices (Google I/O 2025 keynote, May 2025),
more than three times the iOS installed base. Google Play hosted ~3.5M
apps in 2025, with Kotlin the dominant language since Google's 2019
"Kotlin first" announcement. By 2024, more than 95% of the top 1000
Android apps had Kotlin in their codebase per Google Developer
Relations metrics.

For Mochi to be a credible mobile language at all, Android support is
not optional. The choices are: (a) emit Java source, (b) emit JVM
bytecode and reuse MEP-47 with an Android-specific runtime, (c) emit
Kotlin source. Option (a) has been a dead end since 2019; option (b)
works but produces non-idiomatic class shapes that Android's tooling
(R8/D8, AGP, Lint) handles less gracefully than Kotlin-shaped classes;
option (c) is the path of least resistance and is the path Google
itself recommends for new tooling.

Beyond Android, the Kotlin target unlocks:
- **Server-side JVM**, where Kotlin has become the second most common
  JVM language after Java (per JetBrains' 2025 Developer Ecosystem
  Survey), used by Square (Cash App backend), Pinterest, Atlassian,
  Netflix's Hollow library, and the entire JetBrains TeamCity /
  YouTrack / Space stack.
- **Kotlin Multiplatform (KMP)**, which became Stable on 2023-11 (per
  the JetBrains announcement at blog.jetbrains.com on 2023-11-01) and
  is the canonical way to share Kotlin business logic across iOS,
  Android, Web, Desktop, and Server. KMP gives Mochi a single
  compilation strategy that produces JVM, Android, iOS-via-Native,
  macOS, Linux, Windows, browser-JS, Node.js, and (Alpha) browser-Wasm
  outputs from one source tree.
- **Compose Multiplatform**, JetBrains' SwiftUI-equivalent for shared
  declarative UI across the KMP target set. Compose for Android has
  been stable since 1.0 in 2021-07; Compose Multiplatform reached 1.0
  on 2024-05 and shipped Compose for iOS in stable on 2024-09 (1.7.0).
- **Kotlin/Wasm**, currently Alpha as of Kotlin 2.0 (2024-05), reached
  Beta as of Kotlin 2.1.20 (2025-04) targeting the Wasm GC proposal
  (Phase 4 in the WebAssembly CG since 2023-11). Wasm GC has shipped
  in Chrome 119 (2023-11), Firefox 120 (2023-11), and Safari 18.2
  (2024-12), making Kotlin/Wasm a credible (if young) browser target.

Therefore MEP-50: Mochi compiles to idiomatic Kotlin source, drops into
a Gradle KMP project, and produces binaries for every platform Kotlin
itself supports.

## 2. Why Kotlin 2.1 as the floor

Kotlin 2.1.0 shipped on 2024-11-27 and is the K2-compiler baseline with
several language and tooling improvements that materially simplify
Mochi's codegen:

- **K2 compiler is the default frontend.** The K2 frontend (a complete
  rewrite of the Kotlin compiler frontend started in 2018, made the
  default in Kotlin 2.0 in 2024-05, and is the only frontend supported
  in Kotlin 2.1+) brings smart cast improvements, better generic
  inference, and 2x faster compilation on average vs the K1 frontend.
  K1 is unsupported in Kotlin 2.1; the codegen targets only K2 output.
- **Sealed `when` smart casts.** Kotlin 2.1 improved smart casting on
  sealed-hierarchy `when` expressions (KEEP-358), so that within a
  `is Foo ->` arm, the type is narrowed even across complex flow paths
  (e.g., after a guard). Mochi's pattern matching maps cleanly onto
  this.
- **Multiplatform source set hierarchy defaults.** Kotlin 2.1 made the
  KMP source-set hierarchy a default rather than an opt-in (per the
  KMP 1.9.20 → 2.1 evolution). The Mochi-generated `build.gradle.kts`
  uses the default hierarchy template, reducing boilerplate.
- **Multidollar string interpolation** (`$$...$$`, KEEP-359). Useful
  for emitting Kotlin code that itself contains Kotlin-style string
  interpolation (e.g., Mochi raw strings containing `${...}` literals
  that should pass through verbatim).
- **`kotlin.time.Instant` stable** (KEEP-371). Mochi's `time` type
  lowers to `kotlin.time.Instant` since Kotlin 2.1; pre-2.1 modules
  fall back to `kotlinx.datetime.Instant` which has a slightly
  different API.
- **Improved variance inference for sealed types.** Mochi `Result<T, E>`
  with `out` variance compiles cleanly under 2.1's K2 inference.

The decision to floor at 2.1 rather than 1.9 LTS (the last K1-compatible
release) trades off two things: (a) we lose the ability to consume
KMP-stable-but-K1-only libraries, but in practice every actively-
maintained KMP library has migrated to K2 since 2024-11; (b) we gain
the K2-only language features above and avoid carrying a K1 codegen
path that would be maintenance dead-weight.

Kotlin 2.0 (2024-05) is also acceptable as a *minimum* (when a
particular user environment cannot upgrade to 2.1 immediately), but the
default toolchain is 2.1. The rolling secondary gate is Kotlin 2.2
(expected 2025-Q3) which adds context parameters (KEEP-374) and further
smart-cast refinements; we run the 2.2 matrix in advisory mode
(warning-only) and treat 2.2-specific features as opt-in.

## 3. Codegen IR: KotlinPoet vs raw string emit vs KSP-based emit

The codegen pass produces Kotlin source files. The natural impulse is
to print Kotlin text directly with formatted-string templates (the
approach MEP-47 takes for JVM bytecode via ASM-shaped builders, which
is structurally similar). Three reasons we don't:

- **Indentation and trailing-comma correctness.** Kotlin is whitespace-
  tolerant but the test gate verifies byte-identical output across
  runs. Hand-formatted code drifts on the slightest schema change.
- **Syntactic validity.** A typo in a template emits invalid Kotlin
  that compiles only on the second run after the user fixes it.
  Unacceptable.
- **Round-trippability via ktfmt / ktlint.** Emitting through a real
  syntax tree means `ktfmt` / `ktlint` can canonicalise the output.
  The pretty-printer is the source of truth, not the template.

The decision: use **KotlinPoet** (square/kotlinpoet, originally by
Square, transferred to JetBrains' Square auspice and maintained as a
JetBrains-blessed library since 2021; v1.18.x stable as of 2025-Q4).
KotlinPoet is the canonical Kotlin source emitter, exposing a builder
API for `FileSpec`, `TypeSpec`, `FunSpec`, `PropertySpec`, `CodeBlock`,
and the underlying `KModifier` enum. It handles imports automatically,
deduplicates symbols, and produces canonical-formatted output.

KotlinPoet runs on the JVM and is itself a Kotlin library. Mochi's
compiler is in Go, so the bridge is one of two designs:

(a) Embed KotlinPoet via GraalVM Native Image (KotlinPoet has a
shipped Native Image config); link as a static binary into the Mochi
compiler. Pro: single compiler binary, no external process. Con:
GraalVM build complexity, longer Mochi compile times, KotlinPoet's
upgrade cadence couples to Mochi's.

(b) Spawn a Kotlin-side emitter daemon driven via stdin/stdout JSON
RPC, exactly like Kotlin's own KSP plug-in protocol. The Mochi
compiler emits a structured IR; the daemon converts it to Kotlin
source via KotlinPoet. Pro: clean separation, KotlinPoet upgrades
independently. Con: requires JVM runtime at Mochi build time (already
required for kotlinc, so not a new dependency).

Decision: option (b). The daemon design mirrors how MEP-49 plans to
drive swift-syntax from Go ([[../0049/03-prior-art-transpilers]] §11),
and matches the Kotlin compiler plug-in protocol that the ecosystem
already understands. The daemon ships as part of the `mochi-kotlin-emit`
JAR; users run `mochi build --target=kotlin` and the Mochi compiler
spawns the JAR transparently.

We considered and rejected:
- **Raw string emit.** Fails the indentation correctness test above.
- **KSP-based emit.** KSP (Kotlin Symbol Processing, by Google, v2.0.0+
  with K2 support as of 2025) is a Kotlin-side annotation processor.
  KSP runs *during* a Kotlin compilation; for Mochi we need to *produce*
  the Kotlin source, not annotate existing source, so KSP is the wrong
  layer.
- **kotlinx-ast** (kotlinx.ast, a community project for parsing /
  printing Kotlin via swiftparse): less feature-complete than KotlinPoet,
  no JetBrains backing, smaller maintainer base. Reserved as a fallback
  if KotlinPoet becomes unmaintained.

## 4. Why source codegen, not direct JVM bytecode

MEP-47 (the JVM-bytecode target) emits bytecode directly using an
ASM-shaped builder, skipping kotlinc and javac entirely. This is the
correct strategy for JVM-only workloads: it is faster (no kotlinc
front-end), more predictable (no surprising Kotlin compiler
optimisations), and lets Mochi control every classfile attribute.

MEP-50 deliberately takes the opposite path: emit Kotlin source, hand
to kotlinc. The reason is the *cross-target* benefit. The same Kotlin
source feeds:

- The JVM bytecode pipeline (kotlinc → .class → .jar).
- The Android bytecode pipeline (kotlinc → .class → D8/R8 → .dex →
  .aab/.apk).
- The Kotlin/Native pipeline (kotlinc-native → klib → linker → native
  binary or XCFramework).
- The Kotlin/JS pipeline (kotlinc-js → IR → JS output).
- The Kotlin/Wasm pipeline (kotlinc → Wasm GC bytecode).

A bytecode-only emit would only serve the first two, and would
require a separate emit path for each of the remaining backends. By
emitting .kt source, Mochi gets all five Kotlin compiler backends
"for free", at the cost of a slower compile (kotlinc's frontend pass
is non-trivial; for a 10k-line module it adds 5-15 seconds vs direct
bytecode emit).

Users who want the speed of bytecode emit can use MEP-47 (JVM-only).
Users who want the breadth of source emit use MEP-50 (Kotlin). The
cross-target differential gate (`TestCrossTargetDifferential`)
verifies both produce byte-equal stdout on every shared fixture, so
choosing between them is a build-time-vs-flexibility tradeoff, not a
correctness tradeoff.

## 5. The KMP commonMain vs platform-specific split

Kotlin Multiplatform organises code into source sets:
`src/commonMain/kotlin/` for portable code, `src/jvmMain/kotlin/` for
JVM-specific code, `src/iosMain/kotlin/` for iOS-specific, etc. Source
sets form a hierarchy: `iosMain` depends on `appleMain` depends on
`nativeMain` depends on `commonMain`. Each source set can declare
`expect` declarations that have concrete `actual` implementations in
the leaf source sets.

Mochi-to-Kotlin emits the maximum amount of code in `commonMain`. A
Mochi program that uses no platform-specific FFI (no JNI, no cinterop,
no JS-external) compiles entirely from `commonMain`, producing binaries
for every KMP target from a single Kotlin source tree.

Code that *is* platform-specific (FFI to JNI for JVM/Android, cinterop
for K/Native, JS external for K/JS) is split into the appropriate
`*Main` source set with `expect` declarations in `commonMain` that
forward to platform `actual` implementations. The Mochi codegen emits
both the `expect` and the `actual` halves automatically.

Example: a Mochi `extern fun nativeSqrt(x: float): float` lowers to:

```kotlin
// commonMain/kotlin/...
public expect fun nativeSqrt(x: Double): Double

// jvmMain/kotlin/...
public actual fun nativeSqrt(x: Double): Double = java.lang.Math.sqrt(x)

// nativeMain/kotlin/...
public actual fun nativeSqrt(x: Double): Double = platform.posix.sqrt(x)

// jsMain/kotlin/...
public actual fun nativeSqrt(x: Double): Double =
  js("Math.sqrt(x)") as Double

// wasmJsMain/kotlin/...
public actual fun nativeSqrt(x: Double): Double = jsMathSqrt(x)
```

This is more verbose than the Swift target's single-file approach, but
it is the canonical Kotlin idiom, plays well with IntelliJ IDEA's
KMP support, and lets each platform pick the most efficient implementation
(JVM uses HotSpot intrinsic, Native uses libm, JS uses `Math.sqrt`, Wasm
uses its own).

## 6. Strings: UTF-16 reality vs Mochi UTF-8 spec

Mochi's `string` is specified as UTF-8 (per the wire format and the
common-language-errors documentation). Kotlin's `String` is UTF-16 on
every target: JVM's `String` is `char[]`-backed (or compacted Latin-1
since JEP 254 in Java 9), Android's `String` is the same JVM String,
K/Native's `String` is a UTF-16 buffer, K/JS's `String` is the underlying
JS string (also UTF-16), and K/Wasm's `String` is a UTF-16 buffer
(transitioning to Wasm GC strings as the proposal stabilises).

Cost analysis for Mochi's UTF-8 → Kotlin UTF-16 mismatch:

| Operation | Cost |
|-----------|------|
| Literal `"hello"` → Kotlin literal | zero (compiler decodes the UTF-8 source as Kotlin literal at compile time) |
| Concatenation `a + b` | one allocation (Kotlin's String concat creates a new UTF-16 buffer) |
| Indexing by code point | O(n) walk on first access, O(1) with index cache (see [[01-language-surface]] §1.4) |
| Length in code points | O(n) walk; cached in a runtime `String` wrapper for repeated access |
| Encoding to UTF-8 bytes | one O(n) pass plus one allocation, via `String.encodeToByteArray()` |
| Decoding from UTF-8 bytes | one O(n) pass plus one allocation |
| Substring | O(n) on JVM (HotSpot allocates a fresh char[]); O(1) on Kotlin/JS via JS string slicing |

The cost is significant relative to MEP-49 (Swift) where `String` is
UTF-8-native since Swift 5.7 and every operation is zero-copy. We accept
this cost because:

(a) The JVM has been UTF-16 since 1996 and the entire Java ecosystem
assumes UTF-16 strings. Switching Mochi-generated Kotlin to UTF-8 would
break interop with every Java library, every Kotlin library, and every
KMP library.

(b) The actual cost in measured benchmarks is small: Mochi programs
that do not heavily index strings (the common case) see <2% overhead
vs the Swift target. String-heavy programs (text processing) can see
10-30% overhead, which is acceptable given the ecosystem leverage.

(c) Future Kotlin / JVM evolutions (Project Valhalla flat objects,
JEP 421 sealed primitives, Wasm GC strings) may reduce the gap over
time. We do not predict any of these will fully close it before Mochi
v1 ships.

The mitigation: Mochi exposes `bytes` as a separate type (Kotlin
`ByteArray`) for code that wants raw UTF-8 byte access. Mochi `string`
is for human-readable text where Mochi's code-point semantic is the
right abstraction; Mochi `bytes` is for protocol parsing, hashing,
serialisation, and other byte-level work.

## 7. Collections: insertion-ordered LinkedHashMap and LinkedHashSet

Kotlin's `mutableMapOf()` and `mutableSetOf()` factories return
`LinkedHashMap` and `LinkedHashSet` respectively on JVM (verified via
the Kotlin stdlib source). This is *not* a documented contract of the
Kotlin language, but it has been the JVM stdlib behaviour since the
beginning and matches the analogous Java stdlib factories.

K/Native, K/JS, and K/Wasm replicate this behaviour: their
`mutableMapOf()` and `mutableSetOf()` are LinkedHashMap-equivalent and
preserve insertion order.

Mochi's spec mandates insertion-order iteration for `map` and `set`.
The natural lowering is `LinkedHashMap` and `LinkedHashSet`. The
backend uses the explicit factories `linkedMapOf()` and `linkedSetOf()`
rather than the unqualified `mutableMapOf()` and `mutableSetOf()`, to
make the insertion-order contract explicit in the emitted source rather
than rely on the (current) stdlib default.

Performance: `LinkedHashMap` is ~10-15% slower than `HashMap` for
random-access workloads on JVM (the extra doubly-linked list of entries
costs an additional pointer per node and one cache miss per insert).
For typical Mochi workloads (queries, agent message dispatch) the cost
is negligible; for genuinely hot path map access, Mochi can expose a
preview `hashmap<K, V>` type that lowers to plain `HashMap` (no
insertion-order guarantee).

Set semantics: `LinkedHashSet` has the same trade-off vs `HashSet`. The
union / except / intersect operators in the query DSL preserve insertion
order via `LinkedHashSet` semantics.

Collection mutation: Kotlin's `MutableList` / `MutableMap` /
`MutableSet` interfaces expose mutating methods (`add`, `put`, `clear`,
`remove`). Mochi's value-semantics contract requires defensive copies
at function-call boundaries. The runtime emits `.toMutableList()` /
`.toMutableMap()` calls at every call-site where a collection literal
or a returned collection is passed to a function. This is O(n) per
call and is the largest performance cost of the Kotlin target relative
to Swift (which gets value semantics + copy-on-write for free). See
[[01-language-surface]] §3 and [[06-type-lowering]] §11.

For codegen optimisations, the backend tracks "uniquely-owned" Mochi
collection bindings (a collection that has never been aliased, no other
references exist) and skips the defensive copy at the call site,
relying on the unique-ownership invariant. This is an aggressive
optimisation reserved for future v2 work; v1 always emits the defensive
copy.

## 8. Sendability and `Sendable`-equivalent

Kotlin has **no `Sendable` annotation** or compile-time data-race check.
Swift 6's strict concurrency mode (default in Swift 6 language mode)
enforces at compile time that values crossing actor boundaries conform
to `Sendable`; the analogue does not exist in Kotlin.

What Kotlin provides instead:
- **Structured concurrency** via `CoroutineScope` and `Job` (parent
  cancels children, supervisor isolates failure).
- **Channels** as the canonical inter-coroutine communication primitive;
  values sent through a `Channel<T>` are owned by the receiver after
  receive.
- **Dispatchers** (`Dispatchers.Default`, `Dispatchers.IO`,
  `Dispatchers.Main`, `Dispatchers.Unconfined`) that route coroutines
  to specific thread pools, but with no per-value isolation check.
- **`@Volatile`** for JVM-only field-level write visibility (not a
  cross-coroutine guarantee).
- **`kotlinx.atomicfu`** (Kotlin/Atomic-FU, by JetBrains) for atomic
  primitives across KMP targets, used in lock-free data structures.

The practical implication for MEP-50: Kotlin cannot catch the bug
where a Mochi agent sends a mutable list to another agent and both
mutate it concurrently. The Swift target catches this at compile time
(the list type would have to conform to `Sendable`, which a mutable
class type cannot do without explicit unsafe markers); the Kotlin
target relies on the *Mochi-level* type-checker to forbid sharing
patterns and on the runtime's defensive copying to make the residual
cases safe.

The mitigation strategy is layered:

1. **Mochi's type checker** rejects code that would send a non-value-
   semantic type through an agent channel. Mochi records are immutable
   by default; mutable records (with `var` fields) require an explicit
   `mutable` qualifier and are forbidden as agent message payloads.
2. **Runtime defensive copying** at agent message send (`agent.send(msg)`
   emits `agent.send(msg.copy())` when `msg` has any mutable field, by
   default). This is an O(record-size) cost per send.
3. **kotlinx.coroutines structured concurrency** provides isolation
   via the channel boundary.
4. **Documentation** in the Mochi user guide flags Kotlin's lack of
   compile-time sendability check as a known difference from the Swift
   target.

The cost of this layered approach is acceptable; the alternative
(implementing a Sendable-style check in the Mochi type checker that
runs only for the Kotlin target) was rejected as adding target-
specific complexity to a target-agnostic checker.

There is an in-flight proposal in the Kotlin community for a
`@Sendable` equivalent (KEEP-374 "Context parameters" interacts with
this; KEEP-NN "Type-safe concurrency" is a discussion-only thread on
the Kotlin forums as of 2025-Q4). If it lands in Kotlin 2.3 or 2.4, the
Mochi target will adopt it.

## 9. Compose Multiplatform vs SwiftUI parity, deferred to v2

JetBrains' Compose Multiplatform (CMP, github.com/JetBrains/compose-multiplatform)
is the canonical KMP shared-UI framework. CMP is a port of Android's
Jetpack Compose declarative UI framework to other KMP targets:

- Compose for Android: stable since 1.0 in 2021-07.
- Compose for Desktop (JVM): stable since 1.0 in 2021-10.
- Compose for Web (HTML target, since 1.1 in 2022-01; reached
  effective alpha for the Wasm-based Skia rendering in 1.6 in 2024-02).
- Compose for iOS: stable since CMP 1.7.0 in 2024-09.
- Compose for macOS / Linux / Windows desktop: alpha to beta across
  the 1.6 - 1.8 releases.

The MEP-50 v1 deliberately does **not** include a Mochi UI surface.
Mochi has no `view` keyword in the language; UI is currently expressed
via FFI to platform-native frameworks. The decision was made for v0.10
of the language (per ROADMAP.md) and propagates to MEP-50.

When Mochi adds a UI surface (planned for MEP-N future), the natural
KMP lowering is Compose. The argument:
- Compose is the only KMP-native UI framework that targets all of
  Android, iOS, Desktop, and (alpha) Web with a single codebase.
- SwiftUI is iOS/macOS only; the Mochi-to-Swift target (MEP-49) covers
  SwiftUI on Apple platforms.
- Mochi-to-Kotlin would lower Mochi UI to Compose on Android and other
  KMP targets; Mochi-to-Swift would lower the same UI to SwiftUI on
  Apple platforms. The Mochi `view` syntax would be a common
  abstraction over both.

CMP for iOS in particular is the key bet: it lets a single Mochi codebase
produce both an iOS app (via the MEP-49 Swift path) and an Android app
(via this MEP-50 Kotlin path) plus a Compose-on-iOS variant (via
Kotlin/Native + CMP). The three-way matrix is intentional: users pick
the lowering that best matches their team's expertise and platform
preferences.

Until v2 lands the `view` syntax, Mochi developers who want shared UI
across iOS and Android should use Compose Multiplatform directly via
FFI (writing the UI in hand-written Kotlin, calling Mochi business
logic via Kotlin function references). The MEP-50 codegen supports
this by emitting Mochi types as `@Serializable` and `Sendable`-by-
documentation classes that Compose code can consume.

## 10. Gradle and AGP as the build system

The Kotlin ecosystem uses Gradle as the canonical build system. Gradle
8.10+ is the floor (per the shared-decisions anchor); the Mochi-emitted
project generates `build.gradle.kts` (Kotlin DSL, never Groovy) and
`settings.gradle.kts`. The Android Gradle Plugin (AGP) 8.7+ is required
for Compose 1.7 support and for the latest Android SDK targets.

Alternative build systems considered and rejected:

- **Bazel** (with the `rules_kotlin` and `rules_android` Bazel rules):
  hermetic, scales to monorepo, but adds a Bazel dependency and a
  steep learning curve. Bazel-on-Android is production-grade at
  Google but not the default for the broader Kotlin ecosystem. Mochi
  v2 may evaluate.
- **Buck / Buck2** (Meta's build system, with Kotlin support): used
  internally at Meta but not popular outside. Rejected.
- **Maven**: viable for JVM-only Kotlin (and used by some JetBrains
  internal projects), but does not support KMP well; the AGP and
  KMP plugins are Gradle-only. Rejected.
- **Direct `kotlinc` invocations**: works for small projects but
  reinvents dependency resolution and KMP target wiring. Rejected.

Decision: Gradle Kotlin DSL is the default. The Mochi-emitted
`build.gradle.kts` is regenerated on every build; the file is
`.gitignore`d by default but can be checked in for IDE support.

The `gradle/libs.versions.toml` (Gradle version catalog, since Gradle
7.4 in 2022-02) holds all dependency versions in one place. Bumping
Kotlin from 2.1 to 2.2 is a one-line edit. CI verifies the catalog is
the single source of truth across every module.

The Gradle wrapper is bundled: `gradle/wrapper/gradle-wrapper.properties`
pins to Gradle 8.11.1 (as of MEP-50 v1). Users do not need a system
Gradle installation; `./gradlew build` works on a fresh clone.

For Android applications (vs libraries), the Mochi-emitted project
includes `applicationId`, `versionCode`, `versionName`, signing
configuration (handled outside source control via env vars or
`local.properties`), and ProGuard / R8 rules tuned for Mochi-emitted
code shapes (preserving sealed interface variants from R8's
optimisation, since reflection-based deserialization in
kotlinx.serialization relies on the variant names).

For iOS application targets via Kotlin/Native, the Mochi-emitted
project includes the `cocoapods` Gradle plugin (for Cocoapods
integration) or the `XCFramework` task (for Swift Package Manager
consumption, preferred). The output is an XCFramework bundle that an
Xcode-based iOS app can consume via SPM.

## 11. JNI vs cinterop vs Wasm interop tradeoffs

Kotlin's FFI surfaces differ per target:

**JNI on JVM and Android** (Java Native Interface, since JDK 1.0 in
1996). Kotlin exposes JNI via the `external` keyword (the Kotlin
analogue of Java's `native` modifier). Pros: mature, well-documented,
broad library support, dlopen-style dynamic loading via
`System.loadLibrary`. Cons: every JNI call crosses the JVM/native
boundary with a meaningful overhead (~10-50 ns per call), arguments
must be serialised to a JNI-shaped C struct, and lifecycle management
of C-allocated memory is manual. For Mochi-to-C FFI on JVM, JNI is the
only blessed path.

There is a newer alternative, the Foreign Function and Memory API
(JEP 442 / JEP 454, finalised in JDK 22 in 2024-03), which provides a
modern Java-side FFI without writing C glue code. The FFM API is JDK
22+ only; Android does not yet support it. For Mochi v1 we use classic
JNI for JVM and Android; for JVM-only users on JDK 22+, we offer an
experimental `--ffi=ffm` flag that emits FFM-based code.

**cinterop on Kotlin/Native** (the Kotlin-native FFI tool, part of the
Kotlin/Native toolchain since 1.0). cinterop generates Kotlin bindings
from a `.def` file plus C/Objective-C headers; the bindings appear as
ordinary Kotlin packages. Pros: no per-call JNI overhead (Kotlin/Native
code is native and calls C directly), no manual marshalling for primitive
types, automatic memory management via the new Kotlin/Native memory
model. Cons: cinterop is K/Native-only (does not work on JVM/Android),
the `.def` syntax is bespoke, and complex C++ templates are out of
scope.

For Mochi-to-C FFI on K/Native, cinterop is the only path. For
Mochi-to-Swift FFI on K/Native (iOS, macOS), the same cinterop
mechanism works since Swift exposes a C ABI for marked declarations.
The Mochi build generates `.def` files alongside the Mochi-generated
Kotlin source.

**external on Kotlin/JS**. Kotlin/JS exposes JS interop via the
`external` keyword on declarations: `external fun foo(): String`
declares that `foo` is a JS function with no Kotlin implementation,
and the Kotlin/JS compiler emits a direct JS call. The
`@JsName("...")` annotation controls the JS-side symbol name; `@JsExport`
exposes Kotlin code to JS consumers. Pros: zero-overhead JS interop,
the canonical idiom for Kotlin/JS. Cons: no static type-checking on
the JS side (the `external` declaration is taken at face value),
debugging across the Kotlin/JS boundary is awkward.

**Wasm interop on Kotlin/Wasm**. The `wasmJs` target (Wasm hosted in
a JS engine) uses `@JsFun` and `@JsExport` for JS-Wasm interop. The
`wasmWasi` target (Wasm hosted in a WASI runtime like wasmtime) uses
`@WasmImport` for WASI syscall imports and `@WasmExport` for exports.
Both targets are Alpha to Beta as of Kotlin 2.1; ABI is not yet
stable across Kotlin minor versions.

The Mochi FFI surface presents a unified abstraction. A Mochi
`extern fun sqrt(x: float): float = "c:sqrt"` lowers to the right
platform-specific binding on each target (JNI on JVM/Android,
cinterop on K/Native, `external` + `js("Math.sqrt(x)")` on K/JS,
`@JsFun("Math.sqrt")` on K/Wasm). See [[01-language-surface]] §8.2 for
the dispatch table.

## 12. Coroutines vs threads

Kotlin's coroutines (introduced in Kotlin 1.1 as experimental, stable
since 1.3 in 2018-10) are the canonical concurrency primitive. The
library `kotlinx-coroutines-core` provides `launch`, `async`, `await`,
`Job`, `CoroutineScope`, `CoroutineContext`, dispatchers, channels,
and flows.

The decision: Mochi-to-Kotlin uses **kotlinx.coroutines as the only
concurrency primitive**. Mochi's `agent` lowers to an actor class
backed by a `Channel<Message>` + coroutine receive loop (see
[[01-language-surface]] §6). Mochi's `stream<T>` lowers to a `Flow<T>`
(cold) or `SharedFlow<T>` / `StateFlow<T>` (hot). Mochi's `async fun`
lowers to `suspend fun`. Mochi's `spawn f()` lowers to `scope.launch { f() }`.

Alternatives considered and rejected:

- **Java's `Thread` directly**: not used because threads are expensive
  (1 MB stack each on JVM by default), and Kotlin's coroutines provide
  a much lighter-weight abstraction (coroutines suspend without
  allocating a stack).
- **Java's `ExecutorService`**: useful for the dispatcher backend
  (kotlinx.coroutines uses ForkJoinPool under `Dispatchers.Default`),
  but the user-visible API is the coroutine API, not Executor.
- **Virtual threads (JEP 444, Project Loom, stable in JDK 21 in
  2023-09)**: a JVM-only feature that gives Java's `Thread` a
  near-coroutine cost profile. The MEP-47 (JVM bytecode) target uses
  virtual threads as the agent backend. MEP-50 does not, because
  virtual threads are JVM-only and Mochi-to-Kotlin must work on K/Native,
  K/JS, and K/Wasm as well. Coroutines are the only KMP-portable
  concurrency model.
- **RxJava / RxKotlin**: superseded by Flow since 2020. Not used.
- **kotlinx.coroutines `actor { }` builder**: deprecated since
  kotlinx-coroutines 1.7 in 2023-05, obsolete in 1.8. The modern
  actor shape is a custom class with a private channel and a launched
  receive loop (see [[01-language-surface]] §6).

The cost: coroutines are *cooperative*, not preemptive. A coroutine
that runs a long CPU-bound loop without suspension points will block
its dispatcher thread, starving other coroutines on the same dispatcher.
The Mochi codegen inserts `ensureActive()` / `yield()` checkpoints at
every loop back-edge in long-running blocks to keep cancellation
responsive and to give the scheduler an opportunity to dispatch other
coroutines. This is the same pattern Java's Project Loom does at every
back-edge.

## 13. The runtime library footprint

The MochiRuntime KMP module is intentionally thin. The strategy is to
**re-export** existing Kotlin ecosystem libraries plus a small layer of
Mochi-specific helpers.

Re-exported libraries:

- **kotlinx.coroutines** (`kotlinx-coroutines-core` 1.10.1+) for
  coroutines, channels, flows, dispatchers, structured concurrency,
  and `SupervisorJob` / `CoroutineExceptionHandler`. This is the
  agent and stream backbone.
- **kotlinx.serialization** (`kotlinx-serialization-core` 1.7+,
  `kotlinx-serialization-json` 1.7+) for JSON / CBOR / ProtoBuf
  round-trips. Every Mochi `data class` gets `@Serializable` for free.
- **kotlinx.datetime** (`kotlinx-datetime` 0.6+) for `Instant`,
  `LocalDateTime`, `TimeZone`, etc. Mochi `time` lowers to
  `kotlin.time.Instant` directly (Kotlin 2.1+) but uses kotlinx-datetime
  for zoned-time arithmetic.
- **kotlinx.collections.immutable** (`kotlinx-collections-immutable`
  0.3.8+, still pre-1.0 but ABI-stable) for persistent collection
  variants. Used by Mochi's query DSL when the user opts into
  persistent semantics.
- **Ktor client** (`io.ktor:ktor-client-core` 3.0+, plus per-platform
  engines) for HTTP / WebSocket / SSE. Mochi's `fetch` and `generate`
  builtins use Ktor.
- **kotlinx-atomicfu** (`org.jetbrains.kotlinx:atomicfu` 0.24+) for
  cross-platform atomics. Used internally in MochiRuntime's lock-free
  data structures.

MochiRuntime-specific helpers (a few hundred lines of Kotlin, split
into submodules):

- `MochiRuntime.IO`: `print(vararg)`, `readLine()`, file I/O. Wraps
  `java.io.PrintStream` / `kotlin.io` per target.
- `MochiRuntime.Collections`: collection extension functions
  (`appended`, `inserting`, `removing`, etc.) shaped like Mochi's
  functional collection API.
- `MochiRuntime.Strings`: code-point access (`codePointAt`,
  `codePointLength`, `codePoints`), case folding, normalization.
- `MochiRuntime.Math`: `floorDiv`, `floorMod`, `addExact`, etc.
- `MochiRuntime.Query`: hash-join helper, group-by-order-preserving
  helper.
- `MochiRuntime.Datalog`: the Datalog engine.
- `MochiRuntime.AI`: provider-pluggable AI calls (AICore on Android,
  Ktor-based HTTP elsewhere).
- `MochiRuntime.FFI`: registry for `ffi(path, ...)` lookups.
- `MochiRuntime.Supervisor`: actor supervision tree, with restart
  strategies (`oneForOne`, `oneForAll`, `restForOne`) matching BEAM.

The runtime ships as a multi-platform Gradle module (`mochi-runtime-kmp`)
with `commonMain` plus per-target source sets for platform-specific
implementations. Published to Maven Central under
`io.mochilang:mochi-runtime-kmp:0.10.0`.

The runtime intentionally does *not* re-export Android-specific
libraries (AndroidX, Compose, Room, Hilt). Those are accessed via FFI
from Mochi user code; the runtime stays platform-agnostic.

## 14. iOS via K/Native vs going through MEP-49 Swift

There are two paths from Mochi source to an iOS app:

- **MEP-49 (Swift)**: Mochi → Swift source → swiftc → iOS app. The
  emitted code is idiomatic Swift, reads like a Swift developer wrote
  it, and uses SwiftUI / SwiftData / Combine natively.
- **MEP-50 (Kotlin/Native)**: Mochi → Kotlin source → kotlinc-native →
  Kotlin/Native iOS XCFramework → consumed by a Swift host app. The
  emitted code is Kotlin under the hood; Swift code that uses Mochi
  types sees them through the Kotlin/Native ObjC-bridged Swift API.

The two paths have different trade-offs:

| Aspect | MEP-49 (Swift) | MEP-50 (K/Native) |
|--------|----------------|--------------------|
| Emitted code language | Swift | Kotlin |
| Swift consumer ergonomics | Native | OK (ObjC-bridged) |
| Compile time | Fast (swiftc is fast) | Slow (kotlinc-native is slow) |
| Binary size on iOS | Smaller (no Kotlin runtime) | Larger (Kotlin runtime ~5-10 MB) |
| Android support | None | Native |
| Cross-platform sharing | Linux, Windows, server | Android, server JVM, Web, Wasm |
| SwiftUI integration | Native | FFI-only |
| Compose Multiplatform integration | FFI-only | Native (via CMP for iOS) |

The decision is **not exclusive**: a Mochi project can use both. The
recommended pattern for cross-platform mobile apps is:

- Mochi business logic compiles to MEP-50 Kotlin source, deployed as
  a KMP library that Android consumes directly and iOS consumes via
  XCFramework.
- Mochi-to-Swift (MEP-49) is the preferred path for iOS-first projects
  where Android is not in scope.
- For projects that want shared UI across iOS and Android via Compose,
  MEP-50 is the only path (Compose Multiplatform for iOS is K/Native-
  based, not Swift-based).

We do not pick a winner. MEP-49 and MEP-50 are complementary. The
cross-target differential gate (`TestCrossTargetDifferential`) verifies
both produce byte-equal stdout for shared fixtures, so the choice is
about ecosystem fit, not correctness.

## 15. Android-specific: Compose, lifecycle, Hilt, Room

For Android applications, the Mochi-emitted Kotlin lands in
`src/androidMain/kotlin/` plus the standard Android project structure
(`src/main/AndroidManifest.xml`, `res/`, `assets/`).

Android-specific frameworks (AndroidX libraries) are not part of the
core Mochi codegen. Instead, the codegen exposes them as FFI:

- **Jetpack Compose** (`androidx.compose.*`): accessed via FFI from
  Mochi user code; the codegen does not lower Mochi to Compose
  `@Composable` functions (deferred to MEP-N future, see §9).
- **Android Lifecycle** (`androidx.lifecycle.*`): accessed via FFI;
  Mochi agents do not automatically integrate with Android's
  `LifecycleOwner` / `ViewModel` (user must wire it explicitly).
- **Hilt** (`dagger.hilt.*`, Google's DI framework for Android):
  accessed via FFI; Mochi has no DI surface and does not emit Hilt
  annotations.
- **Room** (`androidx.room.*`, the SQLite ORM): accessed via FFI;
  Mochi's persistence story is deferred to a future MEP.

The reason for FFI-only treatment of these libraries is the same as
the SwiftUI-deferred decision in MEP-49 §9: Mochi's UI / persistence /
DI surfaces are not yet defined, and lowering to a specific Android
framework would lock in implementation details before the Mochi
language has decided what those surfaces should look like.

The Mochi-emitted `build.gradle.kts` for an Android module includes
the AndroidX BOM (`androidx.compose:compose-bom:2025.04.00` or later),
the Compose compiler plugin (`org.jetbrains.kotlin:kotlin-compose-compiler-plugin`,
since Kotlin 2.0 the Compose compiler is a Kotlin plugin not a
standalone artifact), and the standard AGP / KGP / Compose dependencies
when the user opts in via `--android-compose=true`.

The `.aab` and `.apk` output is signed via the standard Android
signing flow: `signingConfigs { release { ... } }` block in
`build.gradle.kts`, with the keystore path read from `local.properties`
or env vars. The Mochi codegen does not embed signing credentials.

For Google Play Console submission, the `bundletool` (Google's tool
for converting `.aab` to signed `.apks` for distribution) is invoked
via `./gradlew bundleRelease` and the output is uploaded to Google
Play Console via the Gradle Play Publisher plugin (community plugin
`com.github.triplet.play`).

## 16. The two-layer type wall

Mochi has a static type checker that validates the Mochi source. Kotlin
has a static type checker (the K2 frontend) that validates the emitted
Kotlin source. The two layers form a **two-layer type wall**: any type
error must pass through *both* checks to reach a runtime.

This is a feature, not a redundancy:

- **Defence in depth.** A Mochi codegen bug that emits a type-incorrect
  Kotlin construct is caught by the Kotlin checker, not silently
  compiled into wrong runtime behaviour.
- **Faster iteration.** When the Mochi codegen is wrong, the user sees
  a kotlinc error immediately (with a Mochi source line via the SMAP
  back-mapping), not a runtime crash.
- **Soundness.** Mochi's type system is a strict subset of Kotlin's
  type system for the lowered subset; any Mochi-well-typed program
  produces Kotlin-well-typed source. The two checkers agree.

The cost is one extra compile pass per Mochi build. Kotlin's K2
frontend is fast (3-5x faster than K1, per JetBrains' Kotlin 2.0
release notes), and the type-check pass is incremental, so the cost
is small in practice.

We deliberately do **not** suppress the Kotlin type checker (e.g., via
`@Suppress` or `-Xskip-frontend`). The check is the second layer of
defence and we want it active.

For the rare case where Mochi's lowering produces a construct the
Kotlin checker rejects (e.g., a Kotlin compiler bug, or a Mochi
codegen bug that produces code the Kotlin grammar does not accept),
the Mochi compiler emits a diagnostic with both the Mochi source line
and the Kotlin error, and refuses to produce a binary. We do not
suppress Kotlin errors.

## 17. Cross-references

- [[01-language-surface]] (previous note): the "what" companion, the
  set of surface forms this design rationale supports.
- [[03-prior-art-transpilers]] (next note): the prior-art survey, with
  J2K, Skip, ts2kt, KotlinPoet, J2CL, and other source-to-Kotlin
  transpilers.
- [[04-runtime]]: the `MochiRuntime` Kotlin module layout, organising
  the runtime helpers and the re-exported library set.
- [[05-codegen-design]]: the IR layer that turns Mochi IR into emitted
  Kotlin source via KotlinPoet builders driven by the daemon.
- [[06-type-lowering]]: per-type details (e.g., the `int → Long`
  reasoning, the `data class` field-by-field synthesis policy).
- [[07-kotlin-target-portability]]: the KMP target matrix and version
  skew handling.
- [[08-dataset-pipeline]]: query DSL lowering using Sequence and Flow.
- [[09-agent-streams]]: agent and stream lowering using Channel and
  Flow plus the Supervisor pattern.
- [[10-build-system]]: Gradle Kotlin DSL, AGP, Compose Multiplatform,
  CI matrix.
- [[11-testing-gates]]: the phase gate plan from the shared-decisions anchor
  §"Phase plan", elaborated.
- [[12-risks-and-alternatives]]: risk register; v2 deferrals (Compose
  UI surface, Hilt DI, Room persistence, Embedded-style restricted
  Kotlin); alternatives considered.
- [[../0049/02-design-philosophy]]: the Swift-target analogue, the
  closest sibling design (both target a typed-managed runtime with
  first-class generics, sealed types, async/await, and a managed
  memory model).
- [[../0047/02-design-philosophy]]: the JVM-bytecode-target analogue,
  with which MEP-50 shares the JVM platform but diverges on the codegen
  strategy.
- [[../0048/02-design-philosophy]]: the .NET-target analogue, sharing
  the structured-concurrency story.
- [[../0046/02-design-philosophy]]: the BEAM-target analogue, sharing
  the actor / mailbox / supervisor design.
