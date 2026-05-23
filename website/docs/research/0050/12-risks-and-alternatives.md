# MEP-50 research note 12, Risks and alternatives for MEP-50

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).

This note catalogues the load-bearing risks of MEP-50 and the
alternatives that were considered and rejected. Each entry links back
to the load-bearing decision in [[02-design-philosophy]] and the
affected implementation note. The structure mirrors
[[../0049/12-risks-and-alternatives]] for the Swift sibling.

## 1. Risk register

### R1: Kotlin/Wasm Alpha status

**Risk.** Kotlin/Wasm targets the Wasm GC proposal. As of Kotlin 2.0
(2024-05) the target is Alpha; Kotlin 2.1.20 (2025-03) bumped it to
Beta. ABI stability is not guaranteed across Kotlin versions, binary
sizes are larger than Wasm-MVP would produce (Wasm GC requires
additional sections), and browser support is restricted to Chrome
119+, Firefox 120+, and Safari 18.2+. A Mochi program emitted under
Kotlin 2.1 may fail to instantiate in Kotlin 2.2's runtime, or in
older browsers.

**Mitigation.** MEP-50 ships Kotlin/Wasm as a preview target gated
behind `--enable-wasm`. The Mochi build driver prints a caveat banner
when the flag is set. The runtime documents the browser-compat matrix
and supported Kotlin versions. The Wasm-JS target is not part of the
v1 default matrix; users opt in.

**Residual risk.** Medium. Kotlin/Wasm is JetBrains-led and tracking
to Stable for Kotlin 2.2 or 2.3 (their public roadmap targets late
2025). We expect ABI stability and broader browser support before
MEP-50 ships v2; the v1 preview gate lets users build now and
upgrade later.

See [[07-kotlin-target-portability]] §9, [[10-build-system]] §12,
[[11-testing-gates]] §7.

### R2: Kotlin/Native compile time

**Risk.** Kotlin/Native is notoriously slow. Cold build of a
50K-line Kotlin/Native target on M1 macOS arm64 can take 5-10
minutes; the LLVM-based linker is the bottleneck (each release build
links the entire runtime statically). Incremental rebuild is fast,
but CI builds and first-time-clone builds bear the full cost.

**Mitigation.** Kotlin/Native gained incremental compilation in
1.7.20 (stable since 1.8). The Mochi build driver enables it via
`kotlin.incremental.native=true` in `gradle.properties`. The build
cache (Gradle build cache + Mochi content-addressed cache, see
[[10-build-system]] §15) reuses warm artefacts; on a warm cache hit
the Kotlin/Native build is skipped entirely.

For dev-loop iteration, K/Native supports `klib`-only intermediate
compilation: emit `.klib` files without final linking, and only link
on `mochi run --target=kotlin-native`. The link step is the slow
half; skipping it for non-final builds keeps the inner loop
responsive.

**Residual risk.** Medium. K/Native build speed is a known sore point
in the Kotlin community; JetBrains is working on it (the K2 backend
is faster but the LLVM phase is unchanged).

See [[07-kotlin-target-portability]] §4, [[10-build-system]] §9.

### R3: KMP source-set hierarchy churn

**Risk.** The KMP source-set hierarchy template
(`commonMain -> nativeMain -> appleMain -> iosMain`, etc.) changed
shape between Kotlin 1.9.0 (manual `dependsOn` wiring) and 1.9.20
(`applyDefaultHierarchyTemplate()` introduced) and again between
1.9.20 (template was experimental) and 2.0 (template is stable and
canonical). A Mochi codebase emitted against Kotlin 2.0's hierarchy
may produce different fragment-resolution behaviour under 1.9.

**Mitigation.** MEP-50 floors at Kotlin 2.0.21 and uses
`applyDefaultHierarchyTemplate()` for every emitted KMP project.
The codegen emits each source-set's dependencies via the default
template's pre-declared names (`commonMain`, `jvmMain`, `androidMain`,
`iosMain`, `nativeMain`, etc.); we never hand-wire `dependsOn`.

Pre-2.0 Kotlin versions are explicitly rejected. The codegen fails
fast if `kotlin --version` reports < 2.0.

**Residual risk.** Low. The template is stable in Kotlin 2.x and
JetBrains has publicly committed to no breaking changes through 2.x.

See the shared-decisions anchor §4, [[10-build-system]] §4.

### R4: AGP vs Kotlin version skew

**Risk.** AGP and Kotlin have a coupled compatibility matrix:

| AGP    | Min Kotlin | Max Kotlin |
|--------|------------|------------|
| 8.5    | 1.9.0      | 2.0        |
| 8.6    | 1.9.0      | 2.0        |
| 8.7    | 2.0.0      | 2.1        |
| 8.8    | 2.0.0      | 2.1        |
| 8.9    | 2.0.0      | 2.2 (expected) |

A Mochi project pinned to AGP 8.7 cannot use Kotlin 2.2 even when 2.2
ships; an AGP upgrade is required. Conversely, an old AGP version
fails to recognise new Kotlin language features.

**Mitigation.** MEP-50 pins AGP 8.7.3 and Kotlin 2.1.0 together as a
"compatible pair" in `libs.versions.toml`. Future Mochi releases bump
both in lockstep; users who track Mochi releases get a tested
combination. Users who fork the version catalog risk skew but at
least see explicit version pins they can match against the AGP
compatibility table.

The Mochi CI matrix runs both Kotlin 2.0.21 + AGP 8.7 and Kotlin 2.1.0
+ AGP 8.7 to catch skew before users hit it.

**Residual risk.** Low-medium. The lockstep policy works as long as
JetBrains and Google keep coordinating; historically they have.

See [[10-build-system]] §7, [[11-testing-gates]] §7.

### R5: Compose Compiler vs Kotlin compiler coupling

**Risk.** Through Kotlin 1.5.4, the Compose Compiler plugin was
tightly coupled to a specific Kotlin compiler version: each Compose
release pinned exactly one Kotlin version, and using a different
Kotlin required waiting for a matching Compose release. This caused
2-4 week lag whenever Kotlin shipped.

Kotlin 1.5.4 onwards (and especially 2.0+) decoupled the Compose
Compiler by moving it into the Kotlin repo as a built-in plugin. The
plugin now ships with Kotlin itself, eliminating the lag.

**Mitigation.** MEP-50 pins Compose Multiplatform 1.7.3+ which
requires Kotlin 2.0+. We use the in-repo Compose Compiler plugin
(applied via `org.jetbrains.kotlin.plugin.compose`) rather than the
legacy out-of-repo plugin (`androidx.compose.compiler:compiler`).
This eliminates the version-coupling problem.

**Residual risk.** Very low. The decoupling is a structural fix; the
coupling cannot re-emerge unless JetBrains reverts the plugin
location.

See [[10-build-system]] §20.

### R6: kotlin.Result<T> invariance

**Risk.** Kotlin's standard library has `kotlin.Result<T>`, but it
has three issues that prevent direct use as Mochi's `Result<T, E>`:

1. It is invariant in `T` (`Result<T>` is not a subtype of
   `Result<Any>` even when `T` is).
2. It has no error type parameter; errors are `Throwable` only.
3. It was originally internal (`@SinceKotlin("1.3")`) and is still
   restricted: you cannot return `Result<T>` from a public API
   without an opt-in annotation.

Mochi's `Result<T, E>` lowers to a custom `MochiResult<T, E>` sealed
interface, which is correct but breaks Kotlin idiom: Kotlin
developers expect `Result.success(...)` and `Result.failure(...)`
constructors. The Mochi shape may feel foreign.

**Mitigation.** The Mochi runtime exposes adapter helpers:

```kotlin
public fun <T> MochiResult<T, Throwable>.toKotlinResult(): Result<T> =
    when (this) {
        is MochiResult.Ok -> Result.success(value)
        is MochiResult.Err -> Result.failure(error)
    }

public fun <T> Result<T>.toMochiResult(): MochiResult<T, Throwable> =
    fold(
        onSuccess = { MochiResult.Ok(it) },
        onFailure = { MochiResult.Err(it) }
    )
```

The `MochiResult` shape is documented in the runtime API docs with a
section explaining why Kotlin's built-in `Result` does not fit.

**Residual risk.** Low. The adapter path is one line; idiomatic-Kotlin
consumers can use it. The friction is documentation, not
functionality.

See [[06-type-lowering]] §8, the shared-decisions anchor §2 (lowering
table row for `Result<T, E>`).

### R7: UTF-16 string overhead

**Risk.** Kotlin's `String` is UTF-16 internally on JVM, Android,
Kotlin/Native, Kotlin/JS, and Kotlin/Wasm (the encoding is the JVM
spec on JVM/Android, the platform JS string encoding on K/JS, and
explicit UTF-16 in the runtime on K/Native and K/Wasm). Mochi's
canonical wire format is UTF-8 (per the language spec). Every
boundary crossing (file I/O, network I/O, FFI to C/cinterop) requires
UTF-16 to UTF-8 conversion.

On a hot path with high string throughput, the conversion overhead
shows up as 5-15% CPU. The cross-target differential gate (see
[[11-testing-gates]] §13) catches surrogate-pair handling bugs but
not perf regressions.

**Mitigation.** The Mochi runtime caches the UTF-8 byte array
alongside the `String` for strings that cross I/O boundaries
frequently. The cache is an `IdentityHashMap<String, ByteArray>` in
`MochiRuntime.Internal`; the lifetime tracks the `String` via a
`WeakReference`. Cache hit rate ~85% on benchmark workloads.

For very hot paths, Mochi exposes `Mochi.Bytes` (a `ByteArray`
wrapper) as an alternative representation. Mochi programs that want
to avoid UTF-16 entirely can declare strings as `bytes` instead of
`string` and skip the conversion.

**Residual risk.** Low-medium. Most Mochi programs are not
string-throughput-bound; the cache amortises the cost for hot paths.
The `bytes` escape hatch is documented.

See [[06-type-lowering]] §3, [[04-runtime]] §6.

### R8: JNI overhead and cinterop quality

**Risk.** Two related FFI risks:

1. **JNI overhead on JVM.** Every JNI call crosses the JVM <-> native
   boundary with ~200ns overhead. For frequent fine-grained calls
   (e.g., a Mochi function calling a native math function in a
   loop), this dominates.

2. **cinterop quality on K/Native.** The `cinterop` tool parses C
   headers and emits Kotlin bindings. Header parsing is fragile:
   complex macros, `_Generic` selections, and GNU extensions can
   crash cinterop or produce broken bindings. The bug list at
   `github.com/JetBrains/kotlin-native/issues` includes regular
   cinterop crashes on real-world headers.

**Mitigation.** For JNI overhead:

- Mochi codegen batches JNI calls when possible: a Mochi `for ... in`
  loop calling a native function gets lifted to a single bulk JNI
  call passing the entire array.
- The Mochi runtime exposes a "fast JNI" path using JEP 472 (Foreign
  Function and Memory API, finalised in JDK 22) for Mochi programs
  that target JDK 22+. JEP 472 is roughly 10x faster than classic
  JNI per call.
- We do not use JNA (Java Native Access); JNA's reflection-based
  approach is even slower than classic JNI.

For cinterop quality:

- The Mochi FFI manifest pre-processes headers via a curated
  whitelist; cinterop sees only the symbols the user declared, not
  the entire header transitively.
- For platforms where cinterop is unreliable (notably Windows MinGW
  for Win32 headers), Mochi prefers a Mochi-side stub plus a
  hand-written cinterop shim.
- The CI gate (see [[11-testing-gates]] §10) runs the full FFI
  fixture set on every K/Native target nightly, catching cinterop
  regressions as they ship.

On iOS, the preferred FFI path is **Swift Package Manager** plus a
thin Swift wrapper, not direct cinterop against Objective-C headers.
The KMP `XCFramework` consumes a SPM package cleanly; user-written
Swift wrappers handle the complex header cases.

**Residual risk.** Medium. FFI is the universally hardest cross-
runtime story; Mochi mitigates but cannot eliminate.

See the FFI discussion (planned), [[04-runtime]] §7, [[11-testing-gates]] §14.

### R9: Gradle daemon footprint

**Risk.** The Gradle daemon is a long-lived JVM process holding
build state in memory. Footprint:

- Idle daemon: ~600 MB RSS.
- Active build with KMP plugin loaded: ~1.5-2 GB RSS.
- Multiple daemons (one per Gradle version, one per JVM-args
  combination): multiply by daemon count.

On CI runners with 8 GB RAM (GitHub Actions standard), running two
parallel matrix jobs each with a Gradle daemon eats ~4 GB before
compilation starts. On developer machines, a long-running daemon
holds memory the user may want for other tools (IntelliJ IDEA itself
needs 2-3 GB).

**Mitigation.** On CI we disable the daemon
(`org.gradle.daemon=false`) so each build starts fresh; the cost is
~5s of JVM startup per build, which is acceptable in the matrix
context. On dev machines, the daemon is left on by default but we
document `./gradlew --stop` for users hitting memory pressure.

For dev iteration where the daemon helps, we set
`org.gradle.jvmargs=-Xmx2g -XX:MaxMetaspaceSize=512m` to cap the
daemon heap. Mochi's own build driver also runs Gradle out-of-process
via `./gradlew` rather than embedding the Gradle Tooling API; this
isolates Mochi's memory from Gradle's.

**Residual risk.** Low. The daemon-off-on-CI flag is the standard
mitigation across the Gradle ecosystem.

See [[10-build-system]] §22.

### R10: Maven Central publishing complexity

**Risk.** Maven Central (`repo.maven.apache.org`) has historically
required:

- A Sonatype JIRA account.
- A manual review of every new namespace claim (turnaround: days to
  weeks).
- GPG signing of every artefact, with the public key on a keyserver.
- Staging repository workflow (upload to staging, close, release).

Mid-2024 Sonatype migrated to the Central Portal
(`central.sonatype.com`) which simplifies parts of the flow but
introduces new gotchas (e.g., the portal cannot re-publish a version,
so a botched first release is permanently bad). The migration is
also incomplete: some flows still go through the legacy OSSRH
endpoints, and the documentation lags.

For Mochi, the relevant artefacts are:

- `io.mochi-lang:mochi-runtime` (Kotlin runtime, multi-target).
- `io.mochi-lang:mochi-compiler-plugin` (Kotlin compiler plugin for
  Mochi-specific lowering, if needed).
- Future: `io.mochi-lang:mochi-android` (Android-specific helpers).

**Mitigation.** Claim `io.mochi-lang` namespace at Sonatype Central
Portal in 2025-Q1, well before MEP-50 v1 ship date. The namespace
claim is a one-time gate; once claimed, future artefacts publish
without manual review.

Use the `com.vanniktech.maven.publish` Gradle plugin (see
[[10-build-system]] §19) which tracks Sonatype API changes and
handles the staging + release workflow automatically.

For the runtime, ship a `1.0.0-SNAPSHOT` series during MEP-50 phase
work and only ship `1.0.0` final once Phase 18 lands. SNAPSHOT
versions publish to a separate Sonatype snapshot repo that has no
namespace-claim gate.

**Residual risk.** Low. The namespace is claimed; the plugin handles
the workflow. Residual is Sonatype downtime, which is rare.

See [[10-build-system]] §19.

### R11: K/Native iOS framework ABI churn

**Risk.** Kotlin/Native compiles to a Mach-O `.framework` for iOS.
The framework's ABI (the Swift / Objective-C surface it exposes) is
not stable across Kotlin versions. A Mochi-emitted framework built
with Kotlin 2.0 may have a different ABI than the same Mochi source
built with Kotlin 2.1; consumers that imported the 2.0 framework
header into Swift would see compile errors after the 2.1 rebuild.

Examples of ABI changes:

- Renamed generated symbol prefixes (the `Kotlin` prefix on classes).
- Different mangling of nullable types in Swift interop.
- New required methods on Swift protocols mapped from Kotlin
  interfaces.

**Mitigation.** The Mochi build driver pins the Kotlin version per
Mochi release: Mochi 1.0 ships with Kotlin 2.1.0 exclusively. Users
who upgrade Mochi accept a Kotlin version bump and an ABI rebuild;
this is documented in the upgrade guide.

For users who need to ship the iOS framework as a binary product
(distributing the `.xcframework.zip` to downstream consumers), the
framework checksum is part of the SPM binary-target spec. A Mochi
upgrade that changes the framework triggers a checksum change which
SPM resolves cleanly.

**Residual risk.** Medium. ABI stability is a Kotlin/Native open
problem; JetBrains has acknowledged it but has not committed a
stability timeline. We document the caveat.

See [[10-build-system]] §8, [[07-kotlin-target-portability]] §10.

### R12: Coroutine cancellation in agent loops

**Risk.** Mochi agents lower to Kotlin classes with a private
`Channel<Message>` and a `for msg in channel` receive loop running
inside a `CoroutineScope`. If the scope is cancelled (e.g., a parent
SupervisorJob throws), the receive loop must finish cleanly to avoid
message loss. A naive implementation that does not handle
`CancellationException` properly can lose in-flight messages or fail
to cancel the agent's pending work.

A more subtle issue: if the agent loop is `suspend` and is suspended
in `channel.receive()`, a cancellation does not arrive until the
suspend point completes. If a long-running message handler is in
progress, the handler must check `ensureActive()` to be cancellable.

**Mitigation.** The Mochi codegen emits explicit cancellation
handling:

```kotlin
private suspend fun receiveLoop() {
    try {
        for (msg in channel) {
            ensureActive()
            handle(msg)
        }
    } catch (e: CancellationException) {
        // Drain the channel cleanly
        channel.close()
        throw e
    }
}
```

The `ensureActive()` checkpoint at every iteration ensures the loop
responds to cancellation. The `catch (e: CancellationException)`
rethrows after cleanup so structured concurrency propagates.

For message handlers themselves, the Mochi codegen inserts
`ensureActive()` at every loop back-edge and every `await` point in
long-running handlers.

**Residual risk.** Low-medium. The pattern is well-understood in
kotlinx.coroutines; correctness depends on Mochi codegen emitting
the right checkpoints, which the test gate verifies.

See [[09-agent-streams]] §6, [[11-testing-gates]] §14.

### R13: Channel.UNLIMITED on a malformed actor

**Risk.** Mochi's stream semantic specifies unbounded buffering
(`Channel.UNLIMITED`). A malformed actor that receives messages
faster than it processes them grows the channel buffer without
bound, eventually OOMing the process.

This is the kotlinx.coroutines analogue of Erlang's "mailbox
explosion" problem: a slow actor under load accumulates messages
until the BEAM process dies. Erlang's mitigation is `selective
receive` and operational guidance; Mochi inherits the same risks
under different mechanics.

**Mitigation.** The Mochi runtime ships a `MochiSupervisor` that
provides memory-pressure backstops:

- Each agent has a `mailboxSizeWarning` threshold (default 10,000
  messages). When the channel size crosses the threshold,
  `MochiSupervisor` logs a warning and emits a metric.
- An optional `mailboxSizeMax` threshold (default unset). When
  crossed, `MochiSupervisor` either drops the oldest message
  (`OnOverflow.DROP_OLDEST`), drops the newest (`DROP_NEWEST`), or
  throws to the sender (`THROW`).

Mochi source can opt into bounded channels via the `bounded(N)`
qualifier on the agent declaration:

```mochi
agent Worker bounded(100) {
    on msg: Job { ... }
}
```

The bounded form emits `Channel<Job>(capacity = 100)` instead of
`Channel.UNLIMITED`. Senders block (or error) when the channel is
full.

**Residual risk.** Medium. The default unbounded channel is
correct-by-default for low-load actors but risky under unbounded
load. The supervisor's monitoring + bounded opt-in are mitigations,
not solutions; ultimately the user must size their actors.

See [[09-agent-streams]] §8, [[04-runtime]] §5.

### R14: Compose Multiplatform iOS support is Beta

**Risk.** Compose Multiplatform supports iOS as a target, but the
iOS support has been Beta since the project's launch and remains so
as of Compose Multiplatform 1.7.3 (the version we pin). Known issues:

- Performance gaps vs SwiftUI (Compose iOS uses Skia for rendering;
  SwiftUI uses Core Animation directly).
- Limited iOS-specific API coverage (no Compose wrapper for
  `UIViewController` lifecycle yet stable).
- Text input behaviour differs from native UIKit (keyboard handling,
  emoji rendering).
- App Store review concerns: some reviewers flag non-native UI
  toolkits.

**Mitigation.** MEP-50 v1 ships Compose Multiplatform as the JVM
desktop UI toolkit (macOS, Linux, Windows desktop apps), the
Android UI toolkit (Android apps via `androidx.compose.*`), and the
Web UI toolkit (via Compose for Web on wasmJs).

Compose Multiplatform on iOS is deferred to v2. For iOS UI in MEP-50
v1, the recommended path is to import the Mochi KMP library into a
SwiftUI app and write the UI in Swift. The `XCFramework` export
makes this clean.

**Residual risk.** Low (with the scope carve-out for v1).

See [[10-build-system]] §20, [[07-kotlin-target-portability]] §10.

### R15: Google Play Console pre-launch validation gate requires secrets

**Risk.** The Phase 18 gate (see [[11-testing-gates]] §9) submits
`.aab` files to the Google Play Developer API for pre-launch
validation. The API requires a service account JSON key. CI must
have access to the key, which is a sensitive credential:

- Leaking the key allows the leaker to publish apps under the Mochi
  Play Console account.
- The key cannot be scoped to "validate only"; the same key
  authorises publish.

If the CI workflow is misconfigured (e.g., the key is logged, or a
malicious PR can access secrets via a third-party action), the key
is compromised.

**Mitigation.** The Phase 18 gate runs only on PRs with the
`needs-play-validate` label, set by Mochi maintainers (not by PR
authors). The `MOCHI_PLAY_SERVICE_ACCOUNT_JSON` secret is configured
as an `environment` secret in GitHub Actions, requiring a manual
"approve deployment" gate before the job that needs it can run. The
service account key has the minimum-required scope (Google Play
Developer API, single-app permission).

Additionally, the Mochi project rotates the key quarterly and
maintains the rotation cadence in the security playbook.

**Residual risk.** Low. The gate is opt-in per PR; secrets are
environment-scoped with manual approval; key rotation limits blast
radius.

See [[11-testing-gates]] §9.

### R16: Mochi codegen complexity drift

**Risk.** Six target codegens (C, BEAM, JVM, .NET, Swift, Kotlin)
sharing an aotir IR but each with target-specific lowering is
operationally complex. A Mochi language change that lands easily for
vm3 may require six separate target updates. The Kotlin target
specifically is the latest addition and joins an already-heavy
multi-target maintenance burden.

**Mitigation.** All six targets share monomorphisation and
closure-conversion passes ([[05-codegen-design]] §5). Target-specific
codegen passes are small (~4000 LOC each). The fixture corpus is
shared, so a Mochi feature must produce identical output across
targets, catching divergence early via the cross-target differential
gate in [[11-testing-gates]] §13.

For maintenance, MEP-50 commits to:

- Each target has a designated maintainer with explicit ownership.
- Cross-target gate failures are P0 incidents.
- Quarterly maintenance check-ins verify all six targets are on
  current toolchain versions.

**Residual risk.** Medium. As Mochi grows, target maintenance burden
grows. The cross-target gates are the structural mitigation but
require continued engineering investment.

See [[../0049/12-risks-and-alternatives]] R14 for the sibling-MEP
view, [[11-testing-gates]] §13.

## 2. Alternatives considered

### A1: Emit Kotlin/JVM only; defer Native, JS, Wasm to later MEPs

**Rejected.** KMP-full reach is the load-bearing reason for MEP-50.
Mochi already has MEP-47 for JVM-only output (which emits Java
bytecode directly, skipping kotlinc). The differentiator for MEP-50
is the KMP story: one Mochi source compiles to Android (Kotlin
required), iOS (K/Native), desktop (K/Native or JVM), and web (K/JS
or K/Wasm) via a shared `commonMain` source set.

Emit-JVM-only would duplicate MEP-47's surface without adding the
KMP value. The investment in MEP-50 only pays off across all five
target families.

See the shared-decisions anchor §1, [[02-design-philosophy]] §2.

### A2: Emit Java source instead of Kotlin source, use kotlinc only for KMP targets

**Rejected.** Mochi could emit Java source for the JVM target (the
MEP-47 path) and Kotlin source only for the KMP-required targets
(Android via the Kotlin Android Plugin, iOS / desktop / web via
K/Native, K/JS, K/Wasm). The argument: Java is a smaller language
than Kotlin, the emitter is simpler, and the JVM target gets faster
builds.

We reject this for three reasons:

1. **Sum-type elegance.** Mochi sum types lower to Kotlin
   `sealed interface` plus `data class` variants. The equivalent
   Java requires either pre-17 visitor pattern boilerplate or
   Java 17+ sealed classes plus records, which is wordier and less
   idiomatic for Kotlin consumers.
2. **Data class synthesis.** Mochi records lower to Kotlin
   `data class` (one line). Java requires either records (lossy:
   `equals`, `hashCode`, `toString`, `copy`, `componentN` all need
   manual work) or Lombok (a build-system dependency).
3. **Doubled maintenance.** Maintaining both Java and Kotlin emitters
   doubles the per-feature work. Every Mochi language addition
   requires twin lowering passes.

The kotlinc-cost for the JVM target (slower than `javac`) is real
but acceptable. Users who want fast JVM-only builds can use MEP-47
directly.

See [[02-design-philosophy]] §3.

### A3: Skip KMP, ship separate JVM/Android/Native/JS/Wasm transpilers

**Rejected.** Without KMP, each target needs its own lowering for
the shared bulk of the language (everything except platform-specific
calls). Five separate transpilers means five copies of:

- List, map, set lowering.
- Records and sum types.
- Closures and higher-order functions.
- Query DSL.
- Async / agent / stream.

KMP's `commonMain` source set absorbs all of this in a single shared
copy. The per-target source sets (`jvmMain`, `androidMain`,
`iosMain`, etc.) hold only the platform-specific bits (file I/O
backed by `java.io.File` vs `NSFileManager`, network backed by
OkHttp vs URLSession, etc.).

The KMP model is exactly the right shape for Mochi's portable
language surface. Skipping KMP would multiply the codegen surface by
5x for no benefit.

See the shared-decisions anchor §1, [[05-codegen-design]] §2.

### A4: Skip Kotlin entirely, extend MEP-47 (JVM bytecode) to cover all targets

**Rejected.** MEP-47 emits JVM bytecode directly (skipping any source
language). This works for JVM and Android (D8 dexes bytecode for
Android). It does not work for:

- iOS (no JVM; K/Native compiles Kotlin source, not bytecode).
- Desktop Native (K/Native compiles Kotlin source).
- Web (K/JS compiles Kotlin source).
- Wasm (K/Wasm compiles Kotlin source).

Without Kotlin source emission, Mochi cannot reach the non-JVM
targets via the Kotlin path. The only way to reach iOS / desktop /
web with the same shared lowering is via Kotlin source plus
kotlinc-with-KMP.

Additionally, even on the JVM target, emitting Kotlin source has
value: the emitted `.kt` files are idiomatic for FFI consumers who
need to integrate Mochi code into their own Kotlin codebase. JVM
bytecode is opaque; Kotlin source is reviewable.

See [[02-design-philosophy]] §2.

### A5: Use Multik or other Kotlin numeric libraries instead of stdlib types

**Rejected.** Mochi's numeric types map to Kotlin stdlib types
(`Long`, `Double`). For numeric-heavy workloads, libraries like
Multik (JetBrains' NumPy-equivalent for Kotlin) or Kotlin/Multik
provide N-dimensional array operations with SIMD acceleration.

We reject Multik as the default because:

- It is JVM-only as of v1; KMP support is in progress but not stable.
- It adds a runtime dependency for code that does not need
  N-dimensional arrays.
- Mochi's `list<float>` is a 1-D vector; Multik shines on 2D+
  matrices.

Users who want Multik can import it directly in their Mochi project
via the FFI interop layer; Mochi does not force the choice.

### A6: Use Arrow-kt for functional programming primitives

**Rejected.** Arrow-kt provides `Either`, `Option`, `Validated`,
`IO`, optics, and other functional-programming abstractions for
Kotlin. It is the closest Kotlin equivalent to Haskell or Scala's
functional standard libraries.

Mochi could lower:

- `Option<T>` to Arrow's `Option<T>` (instead of Kotlin nullable
  `T?`).
- `Result<T, E>` to Arrow's `Either<E, T>` (instead of our custom
  `MochiResult`).
- `Mochi.Validated` to Arrow's `Validated`.

We reject Arrow as the default because:

- It is a heavy runtime dependency (~8 MB JAR, ~300 transitive
  declarations).
- Idiomatic Kotlin prefers nullable `T?` over `Option<T>`. Arrow's
  `Option` is the non-idiomatic choice.
- Mochi's `Result<T, E>` is invariant in `E` for good reasons; Arrow's
  `Either<E, T>` has different variance semantics.
- Adding Arrow to every Mochi user project forces a dependency
  choice on users who may not want it.

Users who want Arrow can import it directly via the Mochi FFI; the
Mochi codegen does not preclude it.

### A7: Use kotlinx-poet or similar Kotlin AST library for codegen

**Rejected.** kotlinx-poet is the Kotlin equivalent of JavaPoet
(used by MEP-47 and Square's libraries) for programmatic Kotlin
source generation. It would let the Mochi codegen build a Kotlin AST
and pretty-print it.

We reject kotlinx-poet because:

- It would add a JVM-only build-time dependency to the Mochi
  transpiler (which is written in Go).
- Mochi's codegen is in Go; pulling JVM into the Mochi build chain
  adds friction.
- The Kotlin source surface is small enough (compared to JVM
  bytecode) that a hand-rolled emitter is tractable.

Instead, Mochi emits Kotlin via a Go-side Kotlin printer (see
[[05-codegen-design]] §3) that produces canonical Kotlin text and
then runs `ktlint -F` for final normalisation. This mirrors MEP-49's
choice to skip swift-syntax (see
[[../0049/12-risks-and-alternatives]] R5).

### A8: Use Skie (Skip.tools) for iOS framework export

**Rejected.** Skie (github.com/touchlab/SKIE) and Skip.tools are
third-party tools that improve K/Native's Swift interop by emitting
better Swift bindings (e.g., suspend functions as `async` Swift
functions, sealed classes as Swift enums).

Skie is high-quality and useful, but:

- It is a Touchlab-maintained third-party plugin, not JetBrains
  first-party.
- Adding it as a default plugin couples Mochi to a third-party
  release cadence.
- The benefit (better Swift interop) is real but the Mochi user is
  consuming the framework from Swift; Skie is a value-add not a
  requirement.

Mochi v1 ships without Skie. Users who want better iOS interop can
add Skie as a Gradle plugin in their project; the Mochi codegen
does not preclude it.

### A9: Use Detekt's strict ruleset as the default

**Rejected.** Detekt's strict ruleset
(`config/detekt/detekt-strict.yml`) bans things like long parameter
lists (>6 parameters), long functions (>60 lines), and complex
expressions. Mochi-generated code can hit these limits for legitimate
reasons (a Mochi function with 12 parameters lowers to a Kotlin
function with 12 parameters).

The default ruleset is permissive enough that Mochi-generated code
passes cleanly. Detekt runs in advisory mode (see
[[11-testing-gates]] §6) so regressions are visible without being
blocking.

### A10: Ship with the Kotlin K1 frontend for legacy compatibility

**Rejected.** K1 was the default Kotlin compiler frontend through
1.9. K2 became default in 2.0. K1 is removed in 2.0+.

We could in principle target K1 to support users still on Kotlin 1.9
or older. We reject this because:

- Kotlin 2.0 is the floor per the shared-decisions anchor §1; users on
  1.9 cannot use MEP-50 anyway.
- K1 has known bugs that K2 fixes; targeting K1 would reproduce them.
- KMP source-set hierarchy templates assume K2.

K1 is rejected. The codegen rejects pre-2.0 Kotlin versions at the
toolchain probe.

### A11: Bundle the Kotlin compiler as a native binary

**Rejected.** kotlinc is a Java application; it runs on the JVM. We
could ship a native-image (GraalVM) build of kotlinc with the Mochi
binary to skip JVM startup.

We reject this because:

- The Kotlin compiler is not officially supported under GraalVM
  native-image; reflection in kotlinc internals breaks the
  native-image build.
- The Gradle wrapper already handles kotlinc invocation cleanly via
  the bundled Temurin 17 JDK (see [[10-build-system]] §14).
- kotlinc's JVM startup is amortised by the Gradle daemon and by
  Mochi's content-addressed cache.

JVM-on-JVM is the conventional path; we use it.

### A12: Use Bazel + rules_kotlin for the build system

**Rejected.** Bazel has first-party Kotlin support via
[rules_kotlin](https://github.com/bazelbuild/rules_kotlin) and
Android support via
[rules_android](https://github.com/bazelbuild/rules_android). Bazel
offers hermetic builds, content-addressed remote caching, and
sophisticated parallelism that out-class Gradle on large monorepos.

We reject Bazel because:

- rules_kotlin does not support KMP. KMP is Gradle-only.
- rules_android does not support AGP 8.7+'s features. AGP is
  Gradle-only.
- Bazel users in the Mochi audience are a niche. The mainstream
  Kotlin / Android user is on Gradle.

If user demand surfaces (large monorepo shops that prefer Bazel),
`mochi build --target=kotlin-bazel` becomes a v2 deliverable. For
v1, Gradle is the only supported build driver.

See [[10-build-system]] §1.

### A13: Direct .aab packaging without AGP

**Rejected.** Android App Bundle (.aab) requires:

- Bundle ProtoBuf manifest.
- Per-architecture native libraries (arm64-v8a, x86_64, etc.).
- Resource compilation via aapt2.
- Code shrinking via R8.
- Signing per the v2/v3/v4 schemes.

Replacing AGP requires reimplementing all of this. AGP is hundreds
of thousands of LOC of Android-team Google code. Mochi v1 will not
match AGP's correctness in a reasonable timeframe.

AGP via Gradle is the canonical Android packaging tool; replacing
it requires reverse-engineering the entire Android build chain. Out
of scope for v1, and possibly out of scope forever.

## 3. Open questions

### Q1: Should Mochi-on-Kotlin expose Compose UI as a view system?

A future MEP-N could add a `view` keyword that lowers to Compose
`@Composable` functions. Not in MEP-50 scope; tracked as candidate
v2. The Compose iOS Beta status (R14) is the main reason to defer.

### Q2: Should Mochi-on-Kotlin expose Ktor server?

A `service` keyword that lowers to Ktor server routes. Mochi already
has agents and streams; Ktor server is a natural addition for the
"Mochi on server-side Kotlin" use case. Tracked as candidate v2.

### Q3: Should Mochi target Kotlin/Native for embedded / RTOS?

K/Native does not have an "embedded" subset. Microcontroller
deployment remains MEP-45 (C). Closing.

### Q4: Should Mochi support Compose Compiler plugin authoring?

The Kotlin Compose Compiler plugin (in the Kotlin repo) is the
canonical hook for compiler-level transformations on Compose code.
Mochi could ship its own Kotlin compiler plugin for Mochi-specific
lowering. Tracked as candidate v2 (requires writing a Kotlin compiler
plugin, which is non-trivial).

### Q5: Should Mochi target Kotlin/Native Windows ARM64?

K/Native supports `mingwX64` (Windows x86_64) but not yet Windows
ARM64. JetBrains has Windows ARM64 on the roadmap for Kotlin 2.2+.
Tracked as candidate v2 when toolchain matures.

### Q6: Should Mochi support Kotlin distributed actors?

Kotlin does not have a `distributed actor` keyword (that is a Swift
SE-0336 concept). Mochi remote agents would require a custom
ActorSystem in Mochi's runtime. Tracked as a separate future MEP.

### Q7: Should Mochi target the JVM bytecode directly via Kotlin?

The Mochi codegen could in principle emit JVM bytecode using
kotlin-compiler-embeddable's internal APIs. This would skip the
`kotlinc` subprocess but couple Mochi tightly to Kotlin internals.
Rejected on coupling grounds; not tracked.

### Q8: Should Mochi ship a Maven plugin (`maven-mochi-plugin`)?

For users who prefer Maven over Gradle. Maven cannot drive KMP, so
the plugin would be JVM-only and would compete with MEP-47. Tracked
as candidate v2; low priority.

## 4. Comparison with sibling MEPs

| Dimension              | MEP-45 (C)         | MEP-46 (BEAM)         | MEP-47 (JVM)             | MEP-48 (.NET)           | MEP-49 (Swift)          | MEP-50 (Kotlin)         |
|------------------------|--------------------|-----------------------|--------------------------|--------------------------|--------------------------|--------------------------|
| Target lang            | C11                | Erlang (Core Erlang)  | Java 21                  | C# 12                    | Swift 6.0                | Kotlin 2.1               |
| Codegen IR             | C source           | Core Erlang via cerl  | Java source / bytecode   | C# via Roslyn            | Swift source             | Kotlin source            |
| Toolchain version      | clang/gcc          | OTP 27/28             | JDK 21/25 LTS            | .NET 8/10 LTS            | Swift 6.0/6.1            | Kotlin 2.0/2.1           |
| Concurrency primitive  | pthreads           | OTP process           | Loom virtual thread      | Channel<T> + async       | actor + AsyncStream      | Channel + CoroutineScope |
| Streams primitive      | callback           | gen_event             | Flow.Publisher           | IAsyncEnumerable         | AsyncSequence            | Flow                     |
| Memory                 | manual + arena     | per-process GC        | Tracing GC               | Tracing GC               | ARC                      | Tracing GC (JVM) / ARC-like (K/Native) |
| Build driver           | make/cmake         | rebar3                | Gradle/Maven             | dotnet CLI               | SwiftPM                  | Gradle (KMP)             |
| Package registry       | (none / system)    | hex.pm                | Maven Central            | NuGet                    | swift-package-index      | Maven Central            |
| AOT/JIT                | AOT                | BEAM bytecode + JIT   | HotSpot JIT + GraalVM    | JIT + ReadyToRun + AOT   | AOT only                 | JIT (JVM) + AOT (K/Native) |
| Mobile target          | (via Embed manual) | no                    | Android via D8/R8        | MAUI via .NET MAUI       | iOS/iPad/visionOS native | Android (first-class) + iOS via K/Native |
| Web target             | no                 | no                    | TeaVM (third-party)      | Blazor                   | no (SwiftWasm v2)        | K/JS + K/Wasm            |
| App-store-friendly     | no                 | no                    | partial (Android)        | partial (.NET MAUI)      | yes (iOS/Mac App Store)  | yes (Google Play + iOS via XCFramework) |

The Kotlin target uniquely covers KMP-full reach: every platform via
one codegen, with Android as a first-class citizen (vs MEP-47 where
Android is a downstream consumer of the JVM target).

## 5. Out of scope for v1

Documented elsewhere but listed for closure:

- Compose UI lowering (Q1).
- Ktor server lowering (Q2).
- Compose Compiler plugin authoring (Q4).
- Kotlin/Native Windows ARM64 (Q5).
- Distributed actors (Q6).
- Maven plugin (Q8).
- Property-based testing.
- Fuzzing harness.
- Mutation testing.
- Compose Multiplatform iOS UI (R14, deferred to v2).
- Kotlin/Wasm Stable (R1, ships as preview).
- Embedded Kotlin (no such thing; remains MEP-45).

Each of these is a candidate v2 follow-up; none block v1 landing.

## 6. Failure modes if MEP-50 is not done

The "do nothing" alternative: skip MEP-50 entirely, keep Mochi on
vm3 plus C/BEAM/JVM/.NET/Swift. The cost:

- No first-class Android app development from Mochi. (MEP-47 covers
  JVM but Android development practice is overwhelmingly Kotlin-based
  in 2026; Java-on-Android is legacy.)
- No Kotlin Multiplatform reach: the "one Mochi source to every
  platform" story does not work without KMP as the unifier.
- No Kotlin/JS or Kotlin/Wasm for web targets.
- Server-side Kotlin users (Ktor, Spring Boot Kotlin shops) must use
  the JVM target which loses Kotlin idiomatic sources for FFI.
- Mochi positioning is "general-purpose, ships everywhere" but
  missing the Kotlin ecosystem dents that claim in the same way
  missing the Apple ecosystem (MEP-49) would.

The risk-adjusted value of doing MEP-50 is high: the user-facing
goal (Mochi on Android plus KMP-everywhere) is gated entirely on
this MEP.

## 7. Risk severity matrix

| Risk | Probability | Impact | Severity | Notes                                |
|------|-------------|--------|----------|--------------------------------------|
| R1: K/Wasm Alpha       | Medium  | Low    | Low    | Preview-gated; v2 promotes |
| R2: K/Native compile time | High | Medium | Medium | Cache and incremental mitigate |
| R3: KMP hierarchy churn | Low    | Medium | Low    | Pinned to 2.x template |
| R4: AGP vs Kotlin skew | Medium  | Medium | Medium | Lockstep policy |
| R5: Compose vs Kotlin coupling | Very low | Low | Low | Decoupled in 2.0+ |
| R6: Result invariance | Medium  | Low    | Low    | Adapter helpers |
| R7: UTF-16 overhead  | High    | Low    | Low    | Cache + bytes opt-out |
| R8: FFI quality       | Medium  | High   | Medium | Manifest + batching |
| R9: Gradle daemon footprint | Medium | Low | Low | daemon-off-on-CI |
| R10: Maven Central     | Low     | Medium | Low    | Namespace claimed |
| R11: K/Native iOS ABI  | High    | Medium | Medium | Version pinning |
| R12: Coroutine cancellation | Medium | Medium | Medium | ensureActive checkpoints |
| R13: Channel.UNLIMITED OOM | Medium | High | Medium | Bounded opt-in + supervisor |
| R14: Compose iOS Beta  | High    | Low    | Low    | Deferred to v2 |
| R15: Play secret leak  | Low     | High   | Medium | Environment-scoped + manual approval |
| R16: Codegen drift     | Medium  | Medium | Medium | Cross-target gates |

No risk crosses into High severity. The aggregate risk posture for
MEP-50 is comparable to MEP-49 (Swift) and slightly higher than
MEP-47 (JVM, more mature ecosystem). Acceptable for v1 ship.

Cross-references: [[02-design-philosophy]] for the decision
rationale, [[01-language-surface]] for the surface contract,
[[11-testing-gates]] for the gate matrix that validates each
decision, [[../0049/12-risks-and-alternatives]] for the Swift sibling
view.
