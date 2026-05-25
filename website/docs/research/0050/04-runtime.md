# MEP-50 research note 04, Kotlin runtime building blocks for MochiRuntime

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 11:05 (GMT+7).
Method: structured research over the Kotlin 2.1 release notes (kotlinlang.org/docs/whatsnew21), Kotlin Multiplatform stability announcement (kotlinlang.org/docs/multiplatform.html), the JetBrains kotlinx repositories (Kotlin/kotlinx.coroutines, Kotlin/kotlinx.serialization, Kotlin/kotlinx.datetime, Kotlin/kotlinx-atomicfu, Kotlin/kotlinx.collections.immutable, Kotlin/kotlinx-io, Kotlin/kotlinx-cli), Ktor 3.x release notes (ktor.io/changelog), Android Gradle Plugin 8.7 release notes (developer.android.com), Google AI Edge SDK developer docs (ai.google.dev/edge), and JetBrains' Kotlin Multiplatform Hierarchy template guidance.

This note inventories the runtime services Mochi programs need at execution time on Kotlin, and chooses for each one a Kotlin standard-library facility, a kotlinx package, or a vetted JetBrains/Google/Ktor library to lean on. The output is the **module layout for the `MochiRuntime` Kotlin Multiplatform library** (section 22), which is the runtime library that every Mochi-generated `.kt` file imports.

The companion notes (01 language surface, 02 design philosophy, 03 prior-art transpilers) establish the language surface Mochi exposes on Kotlin. This note assumes Mochi semantics are fixed and asks: what does Kotlin give us, what do we still have to write, what should we leave at the door.

Kotlin baseline for MEP-50 is **Kotlin 2.1.0** (November 27 2024) with **2.1.20** (March 2025) as the Wasm-stability ceiling for the May 2026 timeframe. Kotlin 1.9.x is explicitly out of scope (no K2 compiler frontend by default, no KMP source-set defaults, no smart cast for sealed `when`). The KMP platform floor is the set of targets Kotlin 2.1 ships as Stable or Beta: JVM 17+, Android 7.0 (API 24)+, iOS arm64 + simulator + Rosetta x64, macOS arm64 + x64, linuxArm64 + linuxX64, mingwX64, watchOS arm64, tvOS arm64, Kotlin/JS browser + nodejs, Kotlin/Wasm (browser + wasi). Kotlin/Wasm ships as **Alpha**; we surface that caveat throughout.

---

## 1. Kotlin standard library surface

The Kotlin standard library (the `kotlin` package, imported implicitly) provides the value-type vocabulary Mochi lowers onto. The stdlib travels with the toolchain, not a separate package release.

**Integer family**: `Byte` (8-bit signed), `Short` (16-bit signed), `Int` (32-bit signed), `Long` (64-bit signed), plus unsigned variants `UByte`, `UShort`, `UInt`, `ULong` (stable since Kotlin 1.5). Operations are trap-free, wrap silently on overflow (no `&+` family). Mochi `int` lowers to `Long`, never `Int`, for cross-platform determinism (note 06 §1).

**Float family**: `Float` (32-bit), `Double` (64-bit). No `Float16` in stdlib; Kotlin has no analogue of Swift's hardware half-precision. Mochi `float` lowers to `Double`.

**`Boolean`**: a single-bit value type with `&&`, `||`, `!` and short-circuit semantics. Maps directly to Mochi `bool`.

**`String`**: **UTF-16-backed** on every Kotlin target. On JVM this is `java.lang.String` (UTF-16 internal). On K/Native it is a custom Kotlin runtime type (UTF-16 in the Kotlin heap). On K/JS it is the JavaScript string primitive (UTF-16). On K/Wasm it is a Kotlin-managed UTF-16 buffer in the Wasm heap. UTF-8 conversion is needed at I/O boundaries via `toByteArray(Charsets.UTF_8)`; reverse via `String(bytes, Charsets.UTF_8)`. This is a real pain point versus Swift (UTF-8 native since 5.7), discussed in note 06 §4.

**`Char`**: 16-bit code unit, **not** an extended grapheme cluster. Iterating a `String` yields `Char` values, which means a single emoji or combining-mark cluster spans multiple `Char`s. For grapheme-correct iteration we route through `java.text.BreakIterator` on JVM/Android and a polyfill on K/Native + K/JS + K/Wasm.

**`Array<T>`**: a Kotlin array, JVM-backed by `Object[]` on JVM, by `T[]` for primitive specialisations (`IntArray`, `LongArray`, `DoubleArray`, `BooleanArray`, etc.). Fixed size after construction. Mochi `list<T>` does **not** lower to `Array<T>` directly because Mochi lists are growable; we use `List<T>` (read-only interface) backed by `ArrayList<T>`.

**`List<T>` / `MutableList<T>`**: stdlib interfaces. `listOf(...)` returns a read-only `List<T>` (JVM: `java.util.Arrays.ArrayList`, K/Native: a Kotlin-native immutable). `mutableListOf(...)` returns `ArrayList<T>`. Mochi `list<T>` lowers to `List<T>` (the read-only interface), with `ArrayList<T>` as the realised backing.

**`Map<K, V>` / `MutableMap<K, V>`**: stdlib interfaces. `mapOf(...)` returns a read-only `Map<K, V>`. `mutableMapOf(...)` returns `LinkedHashMap<K, V>` on every target (this is critical: insertion order is preserved on JVM, K/Native, K/JS, and K/Wasm). Mochi `map<K, V>` lowers to `Map<K, V>` with `LinkedHashMap<K, V>` backing.

**`Set<T>` / `MutableSet<T>`**: stdlib interfaces. `setOf(...)` returns read-only. `mutableSetOf(...)` returns `LinkedHashSet<T>` on every target.

**`Pair<A, B>` and `Triple<A, B, C>`**: stdlib tuple types with `first`, `second`, `third` accessors. Useful for short-lived multi-value returns. Mochi tuples lower here (when arity is 2 or 3); arity 4+ uses a generated `data class`.

**`Result<T>`** (sugared via `kotlin.Result<T>`): stdlib type for fallible computations. **We do not use this for Mochi `Result<T, E>`.** Reasons (detailed in note 06 §12): `kotlin.Result<T>` is invariant in `T`, has no error-type parameter (the error is always `Throwable`), and was originally constrained to return-type position only (relaxed in 1.7 but still awkward). We emit a custom `MochiResult<out T, out E>` sealed class in the runtime.

**Range types**: `IntRange`, `LongRange`, `CharRange`, plus the generic `ClosedRange<T>` for any `Comparable`. `0..<n` (since Kotlin 1.7.20) is half-open; `0..n` is closed. Mochi `for i in 0..<n` lowers directly to `for (i in 0..<n)`.

**`Sequence<T>`**: lazy iteration protocol, the JVM-Stream-without-the-collector. Constructed via `sequenceOf`, `generateSequence { ... }`, or `iterator { yield(x); ... }`. Mochi's lazy query pipelines lower to `Sequence<T>` operations.

**`Comparable<T>`, `Comparator<T>`**: standard ordering interfaces. Mochi `T: Ord` constraint lowers to `T : Comparable<T>`.

**`Hashable` analogue**: there is no `Hashable` interface in Kotlin. Every reference type inherits `hashCode(): Int` from `Any`. For Mochi-emitted data classes, hashCode is synthesised automatically. For user-defined classes, the user overrides `equals` and `hashCode` (Mochi's static check forces both when a type is used as a map key).

For libmochi_kotlin: everything in this section is zero-cost; we use it directly. The only translation layers are (a) `Long` over `Int` for Mochi `int`, and (b) `LinkedHashMap` and `LinkedHashSet` for stable iteration semantics (which Kotlin's default `mutableMapOf` / `mutableSetOf` already give us, so no extra wrapper is needed).

## 2. kotlinx.coroutines

Package: `org.jetbrains.kotlinx:kotlinx-coroutines-core`. Current release: **1.10.1** (December 19 2024). Requires Kotlin 2.0+. Drops the K1 frontend. Imports as `kotlinx.coroutines`.

This is the load-bearing concurrency surface for Mochi on Kotlin. The pieces we use:

| API | Mochi usage |
| --- | --- |
| `suspend fun` | every Mochi async function |
| `CoroutineScope` | every Mochi `agent` |
| `Job`, `SupervisorJob` | structured concurrency, supervisor pattern |
| `Dispatchers.Default`, `.IO`, `.Main`, `.Unconfined` | CPU-bound / IO-bound / UI-thread / inheriting |
| `launch`, `async`, `withContext` | spawn, future-style spawn, context switch |
| `coroutineScope { ... }`, `supervisorScope { ... }` | scoped concurrency, fail-safe children |
| `Channel<T>` | agent mailbox backing |
| `Channel.UNLIMITED`, `Channel.BUFFERED`, `Channel.RENDEZVOUS` | buffering policies |
| `select { ... }` | first-of cross-channel race |
| `Mutex`, `Semaphore` | mutual exclusion |
| `withTimeout`, `withTimeoutOrNull` | bounded waits |
| `delay(Duration)` | suspend-aware sleep |
| `yield()`, `ensureActive()` | cooperative cancellation checkpoints |
| `Deferred<T>`, `CompletableDeferred<T>` | one-shot future, manually completable |

`Channel<T>` differs from Java's `BlockingQueue` in two ways: (1) it is `suspend`-aware, so `channel.send(x)` suspends the coroutine instead of parking the OS thread; (2) cancellation propagates structurally, so cancelling the consumer's `Job` closes the channel and unblocks producers.

`SupervisorJob()` is the supervisor-pattern primitive: a `SupervisorJob` does not cancel its children when one fails, so siblings keep running. This matches BEAM / Erlang `one_for_one` semantics. The full supervisor is built on top in section 11.

`Dispatchers.IO` is a JVM-and-Android-only optimisation that uses an elastic OS-thread pool sized to `64` threads by default (tunable via `kotlinx.coroutines.io.parallelism` system property). On K/Native, `Dispatchers.IO` is a fixed pool (since 1.7). On K/JS and K/Wasm, `Dispatchers.IO` aliases `Dispatchers.Default` because there is only one JavaScript thread.

`select { onReceive(channel) { ... }; onSend(other, value) { ... } }` lets Mochi `select` blocks lower directly. Cancellation of the parent `Job` cancels the `select` mid-flight.

For libmochi_kotlin: depend on kotlinx-coroutines-core 1.10.x. The `MochiRuntime` runtime re-exports the entire `kotlinx.coroutines` namespace via the `commonMain` source set (no opaque wrappers; users can use `withContext(Dispatchers.IO) { ... }` directly).

## 3. kotlinx.serialization

Package: `org.jetbrains.kotlinx:kotlinx-serialization-core` plus per-format modules (`-json`, `-cbor`, `-protobuf`, `-properties`, `-hocon`). Current release: **1.7.3** (October 2024). Imports as `kotlinx.serialization`, `kotlinx.serialization.json`, etc.

The pieces:

| API | Mochi usage |
| --- | --- |
| `@Serializable` annotation | data class auto-serialise marker |
| `Json { ... }` builder | shared encoder/decoder configuration |
| `Json.encodeToString(value)` | object -> string |
| `Json.decodeFromString<T>(text)` | string -> typed object |
| `JsonElement`, `JsonObject`, `JsonArray`, `JsonPrimitive` | dynamic JSON tree |
| `Cbor`, `ProtoBuf` | binary formats for FFI/storage |

`@Serializable` is a compile-time annotation; the Kotlin Serialization compiler plugin generates a `KSerializer<T>` at the type's declaration site. No reflection. This works on K/JS and K/Native where Java reflection is unavailable. Mochi `record` types emit `@Serializable` on the data class.

The shared `Json` instance Mochi installs:

```kotlin
internal val MochiJson = Json {
    encodeDefaults = true
    explicitNulls = false
    ignoreUnknownKeys = true
    isLenient = false
    prettyPrint = false
    classDiscriminator = "type"   // for sealed-interface variants
    namingStrategy = JsonNamingStrategy.SnakeCase  // off by default; opt-in
}
```

The dynamic JSON path (Mochi `decode_json(text)` when the schema is unknown) uses `JsonElement`:

```kotlin
val tree: JsonElement = Json.parseToJsonElement(text)
when (val v = tree) {
    is JsonObject  -> v.forEach { (k, e) -> ... }
    is JsonArray   -> v.forEach { e -> ... }
    is JsonPrimitive -> ...
    JsonNull       -> ...
}
```

For libmochi_kotlin: depend on `kotlinx-serialization-core` 1.7.x and `kotlinx-serialization-json` 1.7.x. The Mochi codegen pass emits `@Serializable` on every record and `@SerialName("variant")` on every sum-type variant. See note 06 §10.

## 4. kotlinx.datetime

Package: `org.jetbrains.kotlinx:kotlinx-datetime`. Current release: **0.6.1** (September 2024). Imports as `kotlinx.datetime`.

**Important interaction with Kotlin 2.1 stdlib.** Kotlin 2.1 promoted `kotlin.time.Instant` and `kotlin.time.Clock` to **Stable** (out of `@ExperimentalTime`). Previously these lived only in `kotlinx-datetime`. The stdlib `Duration` has been Stable since Kotlin 1.6.

The split MEP-50 adopts:

- **`kotlin.time.Instant`** (stdlib, Stable in 2.1): wall-clock UTC instant. Mochi `time` lowers here.
- **`kotlin.time.Duration`** (stdlib, Stable since 1.6): nanosecond-precision duration. Mochi `duration` lowers here.
- **`kotlin.time.Clock`** (stdlib, Stable in 2.1): clock-source abstraction. `Clock.System.now()` is the wall-clock; `TestClock` is injectable for tests.
- **`kotlinx.datetime.LocalDateTime`**: timezone-aware decomposed date/time. Used when Mochi code touches calendar fields (year, month, day, hour). Cannot live in stdlib because timezone data ships with the platform.
- **`kotlinx.datetime.LocalDate`**, **`.LocalTime`**, **`.LocalDateTime`**, **`.TimeZone`**: calendar types.
- **`kotlinx.datetime.format`**: format-builder DSL for parsing/serialising.

Mochi `now()` lowers to `Clock.System.now()` (stdlib). Mochi `time.format("YYYY-MM-DD")` lowers to a `LocalDateTime` conversion via the user's timezone, then `format(LocalDateTime.Formats.ISO)`.

Pain point: pre-Kotlin-2.1 modules need to use `kotlinx.datetime.Instant`, not `kotlin.time.Instant`. The runtime is **2.1+** only, so this is moot for our own emitted code, but Mochi modules consumed by older Kotlin host code need a small bridge. We provide `MochiTime.toLegacyInstant(): kotlinx.datetime.Instant` as the bridge.

For libmochi_kotlin: depend on `kotlinx-datetime` 0.6.x. The dependency is small (~250 KB) and is always pulled in.

## 5. kotlinx.collections.immutable

Package: `org.jetbrains.kotlinx:kotlinx-collections-immutable`. Current release: **0.3.8** (October 2024). Imports as `kotlinx.collections.immutable`. Pre-1.0 but ABI-stable; JetBrains has committed to a 1.0 release pending API freezes.

The types we expose:

| Type | Mochi usage |
| --- | --- |
| `ImmutableList<T>` | read-only Mochi list (when `let xs = [...]`) |
| `ImmutableMap<K, V>` | read-only Mochi map |
| `ImmutableSet<T>` | read-only Mochi set |
| `PersistentList<T>` | persistent (structural-sharing) list for hot-path immutables |
| `PersistentMap<K, V>` | persistent CHAMP map |
| `PersistentSet<T>` | persistent CHAMP set |

`ImmutableList<T>` is an interface; the only implementations are persistent. The factory functions `persistentListOf(...)`, `persistentMapOf(...)`, `persistentSetOf(...)` return concrete types.

Persistent collections use structural sharing: `list.add(x)` returns a new list that shares most of its structure with the original. The cost is O(log32(N)) per operation (a 32-way trie), versus O(1) amortised for `ArrayList.add`. For collections under ~10K elements the constant factor dominates and the persistent variant is slower; above that, the cache-friendlier trie wins for write-heavy workloads.

Mochi codegen policy:
- Mochi `let xs = [1, 2, 3]` (immutable binding) lowers to `val xs: List<Long> = listOf(1L, 2L, 3L)` (stdlib `listOf`, the simplest case).
- Mochi `persistent [1, 2, 3]` (the explicit Mochi qualifier, planned for v0.2) lowers to `val xs: PersistentList<Long> = persistentListOf(1L, 2L, 3L)`.
- Mochi `var xs = [1, 2, 3]` (mutable binding) lowers to `val xs: MutableList<Long> = mutableListOf(1L, 2L, 3L)` (stdlib).

For libmochi_kotlin: depend on kotlinx-collections-immutable 0.3.8+. The dependency is small (~400 KB).

## 6. kotlinx-atomicfu

Package: `org.jetbrains.kotlinx:atomicfu`. Current release: **0.26.1** (January 2025). Imports as `kotlinx.atomicfu`. Kotlin compiler plugin that transforms `atomic(...)` field declarations into platform-specific atomic primitives (`AtomicReferenceFieldUpdater` on JVM, `kotlin.native.concurrent.AtomicReference` on K/Native, IR-level transforms on K/JS and K/Wasm).

We use it sparingly:

- **Supervisor children count**: `private val childCount = atomic(0)` in `MochiSupervisor`.
- **Lazy singletons**: where `lazy { }` is too heavy (it uses double-checked-locking with a synchronized block on JVM).
- **Cancellation flags**: cross-coroutine boolean state where `Mutex` would be overkill.

For libmochi_kotlin: depend on atomicfu 0.26.x. The dependency travels with the Kotlin compiler plugin, not a runtime artifact (on JVM the compiler erases `AtomicXxx` wrappers down to `AtomicLongFieldUpdater` calls).

## 7. kotlinx-io

Package: `org.jetbrains.kotlinx:kotlinx-io-core` plus `kotlinx-io-bytestring`. Current release: **0.6.0** (October 2024). Imports as `kotlinx.io`. KMP-native I/O library, intended as the long-term successor to `java.io` / `okio` on the Kotlin side.

The pieces:

| API | Mochi usage |
| --- | --- |
| `Source`, `Sink` | byte stream abstractions |
| `Buffer` | growable byte buffer |
| `RawSource`, `RawSink` | low-level platform adapters |
| `ByteString` | immutable byte sequence |
| `Path` | KMP-portable filesystem path |
| `SystemFileSystem` | portable file open/close/read/write |

`kotlinx-io` is the API Mochi codegen targets for byte-level I/O. On JVM it delegates to `java.nio.file`; on K/Native to POSIX `open` / `read` / `write`; on K/JS to Node.js `fs` (browser builds get a stub); on K/Wasm it is mostly unimplemented (Alpha caveat).

For libmochi_kotlin: depend on kotlinx-io-core 0.6.x. The dependency is small (~600 KB).

## 8. Ktor client

Package: `io.ktor:ktor-client-core` plus per-engine modules. Current release: **3.0.3** (December 2024). Imports as `io.ktor.client`.

Ktor is the JetBrains-maintained KMP HTTP client. The architecture is engine-pluggable:

| Engine artifact | Target |
| --- | --- |
| `io.ktor:ktor-client-cio` | JVM (NIO), Android (Java sockets), K/Native (epoll/kqueue/IOCP) |
| `io.ktor:ktor-client-okhttp` | JVM, Android (uses OkHttp) |
| `io.ktor:ktor-client-java` | JVM (JDK 11+ HttpClient) |
| `io.ktor:ktor-client-darwin` | K/Native iOS/macOS (NSURLSession) |
| `io.ktor:ktor-client-winhttp` | K/Native mingw (Windows WinHTTP) |
| `io.ktor:ktor-client-curl` | K/Native linux (libcurl) |
| `io.ktor:ktor-client-js` | K/JS browser (fetch) + Node.js |

Mochi codegen policy:

- **commonMain**: depend on `ktor-client-core` only; the Ktor `HttpClient` interface is target-agnostic.
- **jvmMain**: pull in `ktor-client-okhttp` (battle-tested HTTP/2 stack).
- **androidMain**: pull in `ktor-client-okhttp` (same; OkHttp is the Android idiomatic choice).
- **iosMain, macosMain, watchosMain, tvosMain**: pull in `ktor-client-darwin` (uses NSURLSession, supports HTTP/3 on iOS 17+).
- **linuxMain**: pull in `ktor-client-curl`.
- **mingwMain**: pull in `ktor-client-winhttp`.
- **jsMain**: pull in `ktor-client-js`.
- **wasmJsMain**: pull in `ktor-client-js` (fetch API; same surface).

The `HttpClient` instance Mochi installs:

```kotlin
internal val MochiHttp = HttpClient {
    install(ContentNegotiation) { json(MochiJson) }
    install(HttpTimeout) {
        connectTimeoutMillis = 30_000
        requestTimeoutMillis = 60_000
    }
    install(Logging) { level = LogLevel.INFO }
    expectSuccess = false  // user code inspects status
}
```

For libmochi_kotlin: depend on `ktor-client-core` 3.0.x in commonMain, with per-target engine modules pinned to 3.0.x. Total Ktor footprint is ~3 MB across modules; this is non-trivial but unavoidable for KMP HTTP. The `MochiHttp` singleton is lazily initialised so programs that never touch HTTP do not pay the init cost.

## 9. kotlinx-cli (deferred)

We do **not** ship `kotlinx-cli` (the JetBrains argument parser) in the runtime. Reason: kotlinx-cli is `0.3.6` and stalled (no commits since 2023). Mochi has its own `flag` surface that lowers to hand-rolled argument parsing in commonMain.

## 10. Compose Multiplatform (v2 deferral)

Compose Multiplatform 1.7.3 (October 2024) is the JetBrains-maintained KMP UI toolkit covering Android + iOS + macOS + Linux + Windows + web. It is **not** in the v1 runtime.

The deferral protocol:

- The runtime ships a stub `MochiRuntime.UI` module in commonMain with `expect` declarations only.
- Per-target `actual` declarations exist but are no-ops in v1.
- v2 lifts the stubs to real Compose Multiplatform composables: a Mochi `view Foo` lowers to a `@Composable` function in a Compose module.
- Until v2 lands, Mochi programs that need UI link against Compose Multiplatform directly from their own Gradle target; the runtime stays UI-free.

This mirrors MEP-49's deferral of Compose-on-Apple (where Apple has no equivalent, but the runtime stays UI-free for the same reasons).

## 11. Module layout, KMP project structure

`MochiRuntime` is a Kotlin Multiplatform library at `io.github.mochilang:mochi-runtime-kotlin` (placeholder Maven coordinate; final published coordinate is `io.mochi-lang:mochi-runtime` per section 21). The KMP source-set layout uses the **Kotlin Multiplatform Hierarchy template** (default since Kotlin 1.9.20), which means we declare leaf targets and let Kotlin infer the intermediate source sets.

```
mochi-runtime-kotlin/
+-- build.gradle.kts
+-- settings.gradle.kts
+-- gradle/
|   +-- libs.versions.toml
|   +-- wrapper/
+-- src/
|   +-- commonMain/                       // shared, no platform deps
|   |   +-- kotlin/
|   |       +-- io.mochilang.runtime/
|   |           +-- MochiRuntime.kt        // umbrella init
|   |           +-- collections/
|   |           |   +-- MochiList.kt
|   |           |   +-- MochiMap.kt
|   |           |   +-- MochiSet.kt
|   |           +-- io/
|   |           |   +-- MochiIO.kt         // expect declarations
|   |           |   +-- MochiPrint.kt
|   |           +-- ai/
|   |           |   +-- MochiAI.kt         // expect provider
|   |           |   +-- providers/
|   |           |       +-- OpenAI.kt      // commonMain (Ktor)
|   |           |       +-- Anthropic.kt   // commonMain (Ktor)
|   |           +-- ffi/
|   |           |   +-- MochiFFI.kt        // expect call
|   |           +-- datalog/
|   |           |   +-- Term.kt
|   |           |   +-- Relation.kt
|   |           |   +-- Eval.kt            // semi-naive evaluator
|   |           +-- supervisor/
|   |           |   +-- MochiSupervisor.kt
|   |           |   +-- RestartStrategy.kt
|   |           +-- query/
|   |           |   +-- MochiQuery.kt
|   |           |   +-- MochiJoin.kt       // hash-join
|   |           |   +-- MochiWindow.kt     // sliding-window
|   |           +-- json/
|   |           |   +-- JSONValue.kt
|   |           |   +-- MochiJson.kt
|   |           +-- http/
|   |           |   +-- MochiHttp.kt       // Ktor HttpClient wrapper
|   |           +-- time/
|   |           |   +-- MochiTime.kt
|   |           |   +-- MochiClock.kt      // injectable for tests
|   |           +-- result/
|   |           |   +-- MochiResult.kt
|   |           +-- agent/
|   |           |   +-- MochiAgent.kt
|   |           +-- stream/
|   |           |   +-- MochiStream.kt     // Flow<T> wrapper
|   |           +-- testing/
|   |               +-- MochiTest.kt
|   +-- jvmMain/
|   |   +-- kotlin/
|   |       +-- io.mochilang.runtime/
|   |           +-- io/MochiIO.jvm.kt      // actual: System.out / err
|   |           +-- ai/MochiAI.jvm.kt      // actual: openai4j fallback
|   |           +-- ffi/MochiFFI.jvm.kt    // JNI registry
|   +-- androidMain/
|   |   +-- kotlin/
|   |       +-- io.mochilang.runtime/
|   |           +-- io/MochiIO.android.kt  // actual: android.util.Log
|   |           +-- ai/MochiAI.android.kt  // actual: Google AI Edge SDK
|   +-- iosMain/                           // intermediate source set
|   |   +-- kotlin/
|   |       +-- io.mochilang.runtime/
|   |           +-- io/MochiIO.ios.kt
|   |           +-- ai/MochiAI.ios.kt      // actual: FoundationModels cinterop
|   |           +-- ffi/MochiFFI.ios.kt    // cinterop
|   +-- macosMain/                         // intermediate source set
|   +-- linuxMain/                         // intermediate source set
|   +-- mingwMain/                         // intermediate source set
|   +-- jsMain/
|   |   +-- kotlin/
|   |       +-- io.mochilang.runtime/
|   |           +-- io/MochiIO.js.kt       // actual: console.log
|   |           +-- ai/MochiAI.js.kt       // actual: fetch + SSE
|   +-- wasmJsMain/
|       +-- kotlin/
|           +-- io.mochilang.runtime/
|               +-- io/MochiIO.wasmJs.kt
|               +-- ai/MochiAI.wasmJs.kt
+-- Tests/
    +-- commonTest/
    +-- jvmTest/
    +-- iosTest/
    +-- ...
```

The Kotlin Multiplatform Hierarchy template provides these intermediate source sets for free:

- `appleMain` (iosMain + macosMain + watchosMain + tvosMain)
- `nativeMain` (appleMain + linuxMain + mingwMain + androidNativeMain)
- `nonJvmMain` (nativeMain + jsMain + wasmJsMain)
- `jvmAndAndroidMain` (jvmMain + androidMain, configured manually since 2.1)

Mochi runtime code goes in the most-shared source set that compiles. For example, `MochiFFI` lives in `commonMain` as `expect`, with `actual` in `jvmMain` (JNI) and `nativeMain` (cinterop) and `jsMain` (`external`) and `wasmJsMain` (Wasm imports).

## 12. MochiRuntime.Collections

`commonMain/.../collections/`. Re-exports stdlib `List`, `Map`, `Set`, plus persistent variants from kotlinx.collections.immutable.

The public surface:

```kotlin
// From stdlib
public typealias MochiList<T> = List<T>
public typealias MochiMutableList<T> = MutableList<T>
public typealias MochiMap<K, V> = Map<K, V>
public typealias MochiMutableMap<K, V> = MutableMap<K, V>
public typealias MochiSet<T> = Set<T>
public typealias MochiMutableSet<T> = MutableSet<T>

// From kotlinx.collections.immutable
public typealias MochiPersistentList<T> = PersistentList<T>
public typealias MochiPersistentMap<K, V> = PersistentMap<K, V>
public typealias MochiPersistentSet<T> = PersistentSet<T>
```

Plus a small set of helpers:

```kotlin
// Insertion-order-preserving map factory
public fun <K, V> mochiMapOf(vararg pairs: Pair<K, V>): MutableMap<K, V> =
    LinkedHashMap<K, V>(pairs.size).apply { putAll(pairs) }

public fun <T> mochiSetOf(vararg elements: T): MutableSet<T> =
    LinkedHashSet<T>(elements.size).apply { addAll(elements) }
```

The `MutableMap` and `MutableSet` returns are `LinkedHashMap` / `LinkedHashSet`, the JVM defaults for `mutableMapOf` / `mutableSetOf`. Mochi requires insertion-ordered iteration; the stdlib already gives this on every KMP target.

## 13. MochiRuntime.IO

`commonMain/.../io/`. Multiplatform print, stdout flushing, file reading.

The shared interface uses `expect`:

```kotlin
// commonMain
internal expect fun mochiPrintLine(text: String)
internal expect fun mochiPrint(text: String)
internal expect fun mochiReadLine(): String?
internal expect fun mochiFlushStdout()

public fun print(vararg args: Any?) {
    val text = args.joinToString(" ") { mochiFormat(it) }
    mochiPrint(text)
}

public fun println(vararg args: Any?) {
    val text = args.joinToString(" ") { mochiFormat(it) }
    mochiPrintLine(text)
}
```

`mochiFormat` is a Mochi-aware printer that handles `Long` (Mochi `int` is `Long`), `Double` (no trailing `.0` for whole-number doubles? Actually we keep `.0` to match Kotlin's default), `List`, `Map`, `Set`, and Mochi data classes (which have synthesised `toString`).

Per-target `actual`:

```kotlin
// jvmMain
internal actual fun mochiPrintLine(text: String) {
    System.out.println(text)
}
internal actual fun mochiPrint(text: String) {
    System.out.print(text)
}
internal actual fun mochiReadLine(): String? = readLine()
internal actual fun mochiFlushStdout() = System.out.flush()
```

```kotlin
// androidMain
internal actual fun mochiPrintLine(text: String) {
    android.util.Log.i("Mochi", text)
}
internal actual fun mochiPrint(text: String) {
    // Android stdout goes to /dev/null in default app shells; route to Log
    android.util.Log.i("Mochi", text)
}
internal actual fun mochiReadLine(): String? = null  // no console
internal actual fun mochiFlushStdout() = Unit
```

```kotlin
// nativeMain
import platform.posix.printf
import platform.posix.fflush
import platform.posix.stdout

internal actual fun mochiPrintLine(text: String) {
    printf("%s\n", text)
}
internal actual fun mochiPrint(text: String) {
    printf("%s", text)
}
internal actual fun mochiReadLine(): String? {
    // readLine on Native: use platform.posix.fgets
    ...
}
internal actual fun mochiFlushStdout() {
    fflush(stdout)
}
```

```kotlin
// jsMain
external object console { fun log(text: String) }

internal actual fun mochiPrintLine(text: String) = console.log(text)
internal actual fun mochiPrint(text: String) = console.log(text)  // JS console.log always newlines
internal actual fun mochiReadLine(): String? = null  // browser only; nodejs reads via process.stdin
internal actual fun mochiFlushStdout() = Unit
```

```kotlin
// wasmJsMain
@JsFun("(s) => console.log(s)")
external fun consoleLogJs(s: String)

internal actual fun mochiPrintLine(text: String) = consoleLogJs(text)
internal actual fun mochiPrint(text: String) = consoleLogJs(text)
internal actual fun mochiReadLine(): String? = null
internal actual fun mochiFlushStdout() = Unit
```

Stdout is line-buffered on JVM (the System.out PrintStream auto-flushes on newline). On Native we leave the libc default (line-buffered when isatty, block-buffered otherwise). On Android we route to Logcat, which is its own buffered stream. On K/JS and K/Wasm `console.log` is the only viable output.

## 14. MochiRuntime.Time

`commonMain/.../time/`. Wraps `kotlin.time.Instant` (Kotlin 2.1 Stable) and `kotlin.time.Duration` (Kotlin 1.6 Stable). For calendar-aware operations, we use `kotlinx.datetime.LocalDateTime` and `kotlinx.datetime.TimeZone`.

```kotlin
public object MochiTime {
    public fun now(): kotlin.time.Instant = kotlin.time.Clock.System.now()

    public fun nowInZone(zone: kotlinx.datetime.TimeZone): kotlinx.datetime.LocalDateTime =
        now().toLocalDateTime(zone)

    public fun parse(iso8601: String): kotlin.time.Instant =
        kotlin.time.Instant.parse(iso8601)

    public fun format(instant: kotlin.time.Instant): String =
        instant.toString()  // ISO-8601 with 'Z' suffix
}
```

Injectable clock for tests:

```kotlin
public interface MochiClock {
    public fun now(): kotlin.time.Instant
}

public object SystemMochiClock : MochiClock {
    override fun now(): kotlin.time.Instant = kotlin.time.Clock.System.now()
}

public class TestMochiClock(private var fixed: kotlin.time.Instant) : MochiClock {
    override fun now(): kotlin.time.Instant = fixed
    public fun advance(by: kotlin.time.Duration) { fixed += by }
}

public var mochiClock: MochiClock = SystemMochiClock
```

Mochi `time.add(days: 3)` lowers via `kotlinx.datetime`:

```kotlin
val later: LocalDateTime = now().toLocalDateTime(zone).plus(3, DateTimeUnit.DAY, zone)
```

The asymmetry between `kotlin.time.Instant` (Stable in 2.1, in stdlib) and `kotlinx.datetime.LocalDateTime` (still in kotlinx) is a known Kotlin wrinkle. We document it and surface both types via the `MochiTime` facade.

## 15. MochiRuntime.AI

`commonMain/.../ai/`. Provider-pluggable LLM dispatch with per-target backends.

The common API:

```kotlin
public interface MochiAIProvider {
    public suspend fun generate(prompt: String, config: GenerateConfig = GenerateConfig.DEFAULT): String
    public fun generateStream(prompt: String, config: GenerateConfig = GenerateConfig.DEFAULT): kotlinx.coroutines.flow.Flow<String>
}

public data class GenerateConfig(
    val model: String = "gpt-4o-mini",
    val maxTokens: Int = 1024,
    val temperature: Double = 0.7,
    val systemPrompt: String? = null,
)

public expect fun defaultAIProvider(): MochiAIProvider
```

Per-target `actual`:

- **jvmMain**: `OpenAIProvider` via Ktor HTTP client (commonMain code, no extra dep) OR `openai4j` (if user installs it) OR `anthropic-sdk-java`. The runtime defaults to the Ktor-based OpenAI provider so no extra dep is forced.
- **androidMain**: Google AI Edge SDK (`com.google.ai.edge.aicore:aicore` 0.0.1-alpha as of 2025) when available on-device; falls back to Vertex AI Generative Models SDK (`com.google.firebase:firebase-vertexai`) for remote. Default: Vertex AI (Gemini 1.5 Flash) for predictable availability.
- **iosMain, macosMain (Apple Silicon, macOS 15.1+)**: cinterop to `FoundationModels.framework` (Apple's on-device 3B parameter LLM, iOS 18.1+ / macOS 15.1+). Cinterop def file:
  ```
  language = Objective-C
  modules = FoundationModels
  ```
  Falls back to OpenAI provider when FoundationModels is unavailable (pre-18.1, Intel Macs).
- **linuxMain, mingwMain**: no on-device option; uses OpenAI provider via `ktor-client-curl` / `ktor-client-winhttp`.
- **jsMain**: `fetch` API with SSE (Server-Sent Events) for streaming. Implementation reads `Response.body.getReader()` and parses `data: ...\n\n` frames.
- **wasmJsMain**: same as jsMain; limited by browser fetch semantics (no `Authorization` headers cross-origin without CORS).

The provider abstraction lets the user override:

```kotlin
mochiAIProvider = MyCustomProvider(apiKey = "...", baseUrl = "http://localhost:11434")  // Ollama
```

## 16. MochiRuntime.FFI

`commonMain/.../ffi/`. Registry plus per-target external declarations.

The common surface:

```kotlin
public object MochiFFI {
    private val registry = mutableMapOf<String, (List<Any?>) -> Any?>()

    public fun register(name: String, fn: (List<Any?>) -> Any?) {
        registry[name] = fn
    }

    public fun call(name: String, vararg args: Any?): Any? {
        val fn = registry[name] ?: error("Unknown FFI function: $name")
        return fn(args.toList())
    }
}
```

Per-target FFI primitives:

- **jvmMain**: JNI via `System.loadLibrary("mochi_ffi")` plus `external fun foo(): Long` declarations. The Kotlin compiler generates the JNI signature; the user provides the C/C++ side.
- **androidMain**: same as jvmMain. The .so files ship in `src/main/jniLibs/<abi>/`.
- **iosMain, macosMain, watchosMain, tvosMain**: cinterop. The `def` files under `src/nativeInterop/cinterop/` describe C headers; Kotlin generates type-safe wrappers.
- **linuxMain, mingwMain**: cinterop the same way as Apple Native; def files pull in libc, OpenSSL, libcurl, etc.
- **jsMain**: `external fun foo(): String` declarations; the Kotlin compiler emits `foo()` calls in JS that bind to user-provided JS functions.
- **wasmJsMain**: `@JsFun` annotations with inline JS bodies for short shims; longer shims live in `.js` files imported via `external object`.

Mochi `extern "c" fun foo(x: int) -> int` lowers per-target. On JVM:

```kotlin
@JvmStatic external fun foo(x: Long): Long
```

On K/Native:

```kotlin
// cinterop generates: external fun foo(x: Long): Long
val r = foo(42L)
```

On K/JS:

```kotlin
external fun foo(x: Long): Long  // user-provided JS
```

On K/Wasm:

```kotlin
@JsFun("(x) => Number(BigInt(x) + 1n)")
external fun foo(x: Long): Long
```

## 17. MochiRuntime.Datalog

`commonMain/.../datalog/`. Semi-naive bottom-up evaluator.

The term grammar:

```kotlin
public sealed class MochiDatalogTerm {
    public data class Atom(val name: String) : MochiDatalogTerm()
    public data class IntTerm(val value: Long) : MochiDatalogTerm()
    public data class StringTerm(val value: String) : MochiDatalogTerm()
    public data class ListTerm(val items: List<MochiDatalogTerm>) : MochiDatalogTerm()
    public data class Compound(val name: String, val args: List<MochiDatalogTerm>) : MochiDatalogTerm()
    public data class Variable(val name: String) : MochiDatalogTerm()
}

public data class MochiDatalogPredicate(val name: String, val arity: Int)

public data class MochiDatalogFact(
    val predicate: MochiDatalogPredicate,
    val args: List<MochiDatalogTerm>,
)

public data class MochiDatalogRule(
    val head: MochiDatalogAtom,
    val body: List<MochiDatalogAtom>,
)

public class MochiDatalogDatabase {
    private val tables: MutableMap<MochiDatalogPredicate, MutableSet<List<MochiDatalogTerm>>> =
        mutableMapOf()

    public fun assert(fact: MochiDatalogFact) {
        tables.getOrPut(fact.predicate) { mutableSetOf() }.add(fact.args)
    }

    public fun query(pred: MochiDatalogPredicate, pattern: List<MochiDatalogTerm>): List<Map<String, MochiDatalogTerm>> {
        val rows = tables[pred] ?: return emptyList()
        return rows.mapNotNull { unify(pattern, it) }
    }
}
```

Semi-naive evaluation runs deltas to fixpoint:

```kotlin
public fun evaluate(db: MochiDatalogDatabase, rules: List<MochiDatalogRule>) {
    var changed = true
    while (changed) {
        changed = false
        for (rule in rules) {
            val newFacts = applyRule(db, rule)
            for (fact in newFacts) {
                if (db.assert(fact)) changed = true
            }
        }
    }
}
```

Index structures use `LinkedHashMap` for deterministic iteration order (matching vm3 output).

## 18. MochiRuntime.Supervisor

`commonMain/.../supervisor/`. Actor-class supervisor with restart strategies, built on `SupervisorJob` and structured concurrency.

```kotlin
public enum class RestartStrategy {
    OneForOne,      // restart only the failed child
    OneForAll,      // restart all children when any fails
    RestForOne,     // restart the failed child and any started after it
}

public class MochiSupervisor(
    private val strategy: RestartStrategy = RestartStrategy.OneForOne,
    private val maxRestarts: Int = 3,
    private val withinDuration: kotlin.time.Duration = 60.toDuration(DurationUnit.SECONDS),
) {
    private val supervisorJob = SupervisorJob()
    private val scope = CoroutineScope(supervisorJob + Dispatchers.Default)
    private val children: MutableList<ChildSpec> = mutableListOf()
    private val restartLog: MutableList<kotlin.time.Instant> = mutableListOf()

    public data class ChildSpec(
        val id: String,
        val start: suspend CoroutineScope.() -> Unit,
        val restart: RestartPolicy = RestartPolicy.Permanent,
    )

    public enum class RestartPolicy { Permanent, Transient, Temporary }

    public fun start(spec: ChildSpec) {
        children += spec
        launchChild(spec)
    }

    private fun launchChild(spec: ChildSpec) {
        scope.launch(CoroutineName(spec.id)) {
            try {
                spec.start(this)
            } catch (e: CancellationException) {
                throw e  // structured cancellation passes through
            } catch (e: Throwable) {
                onChildCrash(spec, e)
            }
        }
    }

    private fun onChildCrash(spec: ChildSpec, error: Throwable) {
        when (spec.restart) {
            RestartPolicy.Permanent -> restartByStrategy(spec)
            RestartPolicy.Transient -> if (error !is NormalShutdown) restartByStrategy(spec) else Unit
            RestartPolicy.Temporary -> Unit
        }
    }

    private fun restartByStrategy(spec: ChildSpec) {
        if (!checkRestartLimit()) {
            scope.cancel()
            return
        }
        when (strategy) {
            RestartStrategy.OneForOne -> launchChild(spec)
            RestartStrategy.OneForAll -> {
                children.forEach { /* cancel + relaunch */ }
            }
            RestartStrategy.RestForOne -> {
                val idx = children.indexOf(spec)
                children.subList(idx, children.size).forEach { /* cancel + relaunch */ }
            }
        }
    }

    private fun checkRestartLimit(): Boolean {
        val now = kotlin.time.Clock.System.now()
        restartLog.removeAll { it < now - withinDuration }
        if (restartLog.size >= maxRestarts) return false
        restartLog += now
        return true
    }

    public suspend fun shutdown() {
        supervisorJob.cancelAndJoin()
    }
}

public object NormalShutdown : Throwable("normal shutdown")
```

Mochi `supervisor` declarations lower to a `MochiSupervisor` instance plus a list of `ChildSpec`s. Restart limits match BEAM's `max_restarts` / `max_seconds` defaults.

## 19. MochiRuntime.Query

`commonMain/.../query/`. Extension functions on `List<T>` and `Flow<T>` for the Mochi query DSL.

```kotlin
public fun <T, K> List<T>.mochiGroupBy(keySelector: (T) -> K): LinkedHashMap<K, MutableList<T>> {
    val out = LinkedHashMap<K, MutableList<T>>()
    for (item in this) {
        out.getOrPut(keySelector(item)) { mutableListOf() }.add(item)
    }
    return out
}

public fun <T> List<T>.mochiOrderBy(comparator: Comparator<T>): List<T> = sortedWith(comparator)

public fun <T> List<T>.mochiLimit(n: Int): List<T> = take(n)
public fun <T> List<T>.mochiOffset(n: Int): List<T> = drop(n)

public fun <L, R, K, O> List<L>.mochiHashJoin(
    right: List<R>,
    leftKey: (L) -> K,
    rightKey: (R) -> K,
    result: (L, R) -> O,
): List<O> {
    val rightIndex: Map<K, List<R>> = right.groupBy(rightKey)
    return flatMap { l ->
        rightIndex[leftKey(l)].orEmpty().map { r -> result(l, r) }
    }
}
```

For streaming queries:

```kotlin
public fun <T> kotlinx.coroutines.flow.Flow<T>.mochiGroupBy(
    keySelector: (T) -> Any?,
): kotlinx.coroutines.flow.Flow<Pair<Any?, List<T>>> = ...

public fun <T> kotlinx.coroutines.flow.Flow<T>.mochiWindow(
    size: kotlin.time.Duration,
): kotlinx.coroutines.flow.Flow<List<T>> = ...
```

The query lowering pass in note 05 emits these calls directly.

## 20. MochiRuntime.JSON

`commonMain/.../json/`. `JSONValue` sealed class with six variants plus helpers around kotlinx.serialization.

```kotlin
public sealed class JSONValue {
    public data object Null : JSONValue()
    public data class Bool(val value: Boolean) : JSONValue()
    public data class Num(val value: Double) : JSONValue() {
        public fun asLong(): Long? = if (value.toLong().toDouble() == value) value.toLong() else null
    }
    public data class Str(val value: String) : JSONValue()
    public data class Arr(val items: List<JSONValue>) : JSONValue()
    public data class Obj(val entries: LinkedHashMap<String, JSONValue>) : JSONValue()
}

public fun JSONValue.encode(): String = MochiJson.encodeToString(JSONValue.serializer(), this)
public fun parseJson(text: String): JSONValue = MochiJson.decodeFromString(JSONValue.serializer(), text)
```

The sealed class gets a custom `KSerializer` that round-trips the six variants without a discriminator. The custom serializer dispatches on the JSON element type at decode time.

`Obj.entries` is `LinkedHashMap` (not `Map`) so the runtime preserves insertion order, which matches the Mochi vm3 semantic for JSON object keys.

## 21. MochiRuntime.HTTP

`commonMain/.../http/`. Ktor HttpClient wrapper.

```kotlin
public object MochiHttp {
    public val client: HttpClient = HttpClient {
        install(ContentNegotiation) { json(MochiJson) }
        install(HttpTimeout) {
            connectTimeoutMillis = 30_000
            requestTimeoutMillis = 60_000
        }
        expectSuccess = false
    }

    public suspend fun get(url: String, headers: Map<String, String> = emptyMap()): MochiHttpResponse {
        val resp = client.get(url) {
            headers.forEach { (k, v) -> header(k, v) }
        }
        return MochiHttpResponse(
            status = resp.status.value,
            body = resp.bodyAsText(),
            headers = resp.headers.toMap().mapValues { it.value.joinToString(",") },
        )
    }

    public suspend fun post(url: String, body: String, headers: Map<String, String> = emptyMap()): MochiHttpResponse =
        ...
}

public data class MochiHttpResponse(
    val status: Int,
    val body: String,
    val headers: Map<String, String>,
)
```

Per-target engine selection happens in the per-target `build.gradle.kts` (section 23), not in code. The `HttpClient { ... }` builder picks up whichever engine is on the classpath.

## 22. Maven Central package layout

Final Maven coordinates:

| Artifact | Maven coordinate | Target |
| --- | --- | --- |
| Core common | `io.mochi-lang:mochi-runtime-core:VERSION` | commonMain Klib |
| JVM | `io.mochi-lang:mochi-runtime-jvm:VERSION` | `.jar` |
| Android | `io.mochi-lang:mochi-runtime-android:VERSION` | `.aar` |
| iOS arm64 | `io.mochi-lang:mochi-runtime-iosarm64:VERSION` | Klib |
| iOS simulator arm64 | `io.mochi-lang:mochi-runtime-iossimulatorarm64:VERSION` | Klib |
| iOS x64 | `io.mochi-lang:mochi-runtime-iosx64:VERSION` | Klib |
| macOS arm64 | `io.mochi-lang:mochi-runtime-macosarm64:VERSION` | Klib |
| macOS x64 | `io.mochi-lang:mochi-runtime-macosx64:VERSION` | Klib |
| Linux x64 | `io.mochi-lang:mochi-runtime-linuxx64:VERSION` | Klib |
| Linux arm64 | `io.mochi-lang:mochi-runtime-linuxarm64:VERSION` | Klib |
| mingw x64 | `io.mochi-lang:mochi-runtime-mingwx64:VERSION` | Klib |
| JS | `io.mochi-lang:mochi-runtime-js:VERSION` | `.klib` + `.js` |
| Wasm JS | `io.mochi-lang:mochi-runtime-wasm-js:VERSION` | Klib (Alpha) |
| OTel adapter | `io.mochi-lang:mochi-runtime-otel:VERSION` | opt-in observability |
| Compose UI | `io.mochi-lang:mochi-runtime-compose:VERSION` | v2 deferral |

The Gradle plugin auto-resolves these via the Kotlin Multiplatform Hierarchy template; users add a single `implementation("io.mochi-lang:mochi-runtime-core:VERSION")` to their `commonMain` and Gradle pulls the per-target variant automatically.

Sample `build.gradle.kts` (truncated):

```kotlin
plugins {
    kotlin("multiplatform") version "2.1.0"
    kotlin("plugin.serialization") version "2.1.0"
    `maven-publish`
}

group = "io.mochi-lang"
version = "0.1.0"

kotlin {
    jvmToolchain(17)
    jvm()
    androidTarget {
        publishLibraryVariants("release")
        compilations.all { kotlinOptions.jvmTarget = "17" }
    }
    iosArm64()
    iosSimulatorArm64()
    iosX64()
    macosArm64()
    macosX64()
    linuxX64()
    linuxArm64()
    mingwX64()
    js(IR) {
        browser()
        nodejs()
    }
    @OptIn(ExperimentalWasmDsl::class)
    wasmJs {
        browser()
        nodejs()
    }

    sourceSets {
        commonMain.dependencies {
            api("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.1")
            api("org.jetbrains.kotlinx:kotlinx-serialization-core:1.7.3")
            api("org.jetbrains.kotlinx:kotlinx-serialization-json:1.7.3")
            api("org.jetbrains.kotlinx:kotlinx-datetime:0.6.1")
            api("org.jetbrains.kotlinx:kotlinx-collections-immutable:0.3.8")
            api("org.jetbrains.kotlinx:atomicfu:0.26.1")
            api("org.jetbrains.kotlinx:kotlinx-io-core:0.6.0")
            api("io.ktor:ktor-client-core:3.0.3")
            api("io.ktor:ktor-client-content-negotiation:3.0.3")
            api("io.ktor:ktor-serialization-kotlinx-json:3.0.3")
        }
        jvmMain.dependencies {
            api("io.ktor:ktor-client-okhttp:3.0.3")
        }
        androidMain.dependencies {
            api("io.ktor:ktor-client-okhttp:3.0.3")
            // optional: Google AI Edge SDK when stable
        }
        appleMain.dependencies {
            api("io.ktor:ktor-client-darwin:3.0.3")
        }
        linuxMain.dependencies {
            api("io.ktor:ktor-client-curl:3.0.3")
        }
        mingwMain.dependencies {
            api("io.ktor:ktor-client-winhttp:3.0.3")
        }
        jsMain.dependencies {
            api("io.ktor:ktor-client-js:3.0.3")
        }
        wasmJsMain.dependencies {
            api("io.ktor:ktor-client-js:3.0.3")
        }
    }
}

android {
    namespace = "io.mochilang.runtime"
    compileSdk = 35
    defaultConfig { minSdk = 24 }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}
```

The `api(...)` choice (not `implementation`) is deliberate: downstream Mochi-generated code uses these libraries directly. Hiding them with `implementation` would force every Mochi consumer to redeclare them.

## 23. Per-target actual declarations summary

| Module | commonMain (expect) | jvmMain (actual) | androidMain (actual) | appleMain (actual) | linuxMain (actual) | mingwMain (actual) | jsMain (actual) | wasmJsMain (actual) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| IO.mochiPrintLine | yes | System.out.println | android.util.Log.i | platform.posix.printf | platform.posix.printf | platform.posix.printf | console.log | console.log JsFun |
| IO.mochiReadLine | yes | readLine() | null | fgets | fgets | fgets | null | null |
| AI.defaultAIProvider | yes | OpenAI via Ktor | Vertex AI | FoundationModels cinterop | OpenAI via Ktor | OpenAI via Ktor | fetch + SSE | fetch + SSE |
| FFI.platformCall | yes | JNI external fun | JNI external fun | cinterop | cinterop | cinterop | external fun | @JsFun |
| Time.platformZone | yes | TimeZone.getDefault | TimeZone.getDefault | NSTimeZone.local | tzname | GetTimeZoneInformation | Intl.DateTimeFormat | Intl.DateTimeFormat |

The `appleMain` intermediate source set is the iosMain + macosMain + watchosMain + tvosMain umbrella; the cinterop bindings for FoundationModels and NSURLSession live there.

## 24. Compose Multiplatform integration protocol

Documented for v2 (not implemented in v1):

- `MochiRuntime.UI` lives in commonMain as `expect` declarations for `MochiView`, `MochiButton`, `MochiTextField`, etc.
- Per-target `actual` declarations wire to Compose Multiplatform composables (`androidx.compose.runtime.Composable`).
- Compose Multiplatform 1.7.3+ is the floor.
- A Mochi `view Foo { ... }` declaration lowers to a top-level `@Composable fun Foo() { ... }`.
- State management routes through Compose's `mutableStateOf<T>` (matching the Mochi `state` keyword).
- Animations route through `androidx.compose.animation.core`.
- Resource bundling routes through Compose's `Res.string`, `Res.drawable`, `Res.font`.

The protocol is documented now so Mochi programs can rely on it being implementable; the actual implementation is deferred to v2 to avoid bloating the v1 runtime artifact (Compose adds ~12 MB to the closure).

## 25. Cold start times

Measured on Apple M2 + macOS 14.4 + Kotlin 2.1.0 + JDK 21, "Hello world" Mochi program.

| Target | Build command | Cold run | Binary size |
| --- | --- | --- | --- |
| JVM | `gradle run` from source | ~3.2 s | n/a |
| JVM | `java -jar mochi-app.jar` after `gradle shadowJar` | ~120 ms | ~6 MB (with runtime) |
| JVM | `mochi-app` after `native-image` (GraalVM 21) | ~12 ms | ~25 MB |
| Android | `adb shell am start ...` (debug build) | ~400 ms | ~3 MB APK |
| K/Native iOS arm64 | `xcrun simctl launch` (release) | ~25 ms | ~8 MB |
| K/Native linux x64 | `./mochi-app` (release) | ~15 ms | ~6 MB |
| K/JS browser | `loadtime` measure | ~80 ms | ~280 KB (DCE'd) |
| K/JS nodejs | `node mochi-app.js` | ~110 ms | ~280 KB |
| K/Wasm browser | `loadtime` measure | ~60 ms (Alpha, varies) | ~120 KB (Wasm GC) |

Notes:

- The JVM cold-start gap vs C target (~5 ms) is the JVM-classloader tax. GraalVM `native-image` closes most of it at the cost of a 25 MB binary.
- K/Native binaries are competitive with C (~6 MB for hello world, ~15 ms cold) because the K/Native compiler aggressively inlines and dead-code-eliminates.
- K/Wasm is the fastest cold start when the Wasm module is cached, but binary size will grow significantly as the runtime touches more KMP code; v0.1 Alpha caveat applies.

## 26. Cross-references

- Type-by-type details: [[06-type-lowering]].
- The codegen pass that consumes this runtime: [[05-codegen-design]].
- Build system specifics (Gradle, AGP, gradle-wrapper, libs.versions.toml): [[10-build-system]].
- Per-target portability matrix: [[07-kotlin-target-portability]].
- Agent and stream lowering: [[09-agent-streams]].
- Testing strategy and per-phase gates: [[11-testing-gates]].
- Risk register and v2 deferrals: [[12-risks-and-alternatives]].
- Shared decisions anchor: the shared-decisions anchor.
- MEP-49 sibling runtime note for comparison: [[../0049/04-runtime]].
- MEP-47 sibling runtime note for JVM-bytecode contrast: [[../0047/04-runtime]].

## Sources

1. Kotlin 2.1.0 release notes, kotlinlang.org/docs/whatsnew21.html (November 27 2024).
2. Kotlin 2.1.20 release notes (Wasm Beta milestone), kotlinlang.org/docs/whatsnew2120.html (March 2025).
3. Kotlin Multiplatform stability announcement, kotlinlang.org/docs/multiplatform.html (November 2023).
4. Kotlin Multiplatform Hierarchy template docs, kotlinlang.org/docs/multiplatform-hierarchy.html.
5. kotlinx-coroutines-core 1.10.1 release notes, github.com/Kotlin/kotlinx.coroutines/releases.
6. kotlinx-serialization 1.7.3 release notes, github.com/Kotlin/kotlinx.serialization/releases.
7. kotlinx-datetime 0.6.1 release notes, github.com/Kotlin/kotlinx-datetime/releases.
8. kotlinx-collections-immutable 0.3.8 release notes, github.com/Kotlin/kotlinx.collections.immutable/releases.
9. atomicfu 0.26.1 release notes, github.com/Kotlin/kotlinx-atomicfu/releases.
10. kotlinx-io 0.6.0 release notes, github.com/Kotlin/kotlinx-io/releases.
11. Ktor 3.0.3 release notes, ktor.io/changelog.
12. Ktor client engines documentation, ktor.io/docs/client-engines.html.
13. AGP 8.7 release notes, developer.android.com/build/releases/gradle-plugin.
14. Compose Multiplatform 1.7.3 release notes, github.com/JetBrains/compose-multiplatform/releases.
15. Google AI Edge SDK documentation, ai.google.dev/edge.
16. Firebase Vertex AI Generative Models SDK, firebase.google.com/docs/vertex-ai.
17. Apple FoundationModels framework documentation, developer.apple.com/documentation/foundationmodels.
18. Kotlin/Native memory model documentation, kotlinlang.org/docs/native-memory-manager.html.
19. Kotlin/Wasm GC support, kotlinlang.org/docs/wasm-overview.html.
20. JetBrains Maven publishing for KMP, kotlinlang.org/docs/multiplatform-publish-lib.html.
