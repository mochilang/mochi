# MEP-50 research note 11, Testing strategy and gates for MEP-50

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies the test-as-spec gates that govern each MEP-50
phase. It mirrors the MEP-45 (C), MEP-46 (BEAM), MEP-47 (JVM), MEP-48
(.NET), and MEP-49 (Swift) gate patterns: one deterministic Go test
per phase, plus a differential gate against vm3, plus a kotlinc clean
gate, plus a platform matrix.

## 1. Gate philosophy

A **gate** is a Go test (`Test...` in the Mochi repo, under
`tests/transpiler3/kotlin/`) that:

1. Iterates fixtures in `tests/transpiler3/kotlin/...`.
2. Compiles each fixture with the MEP-50 toolchain
   (`mochi build --target=kotlin-...`).
3. Runs the resulting binary (executable for executable targets,
   `./gradlew test` for library targets, `connectedCheck` for the
   Android emulator).
4. Compares actual stdout against `<fixture>.out`.
5. Compares against the vm3 oracle (running the same Mochi source on
   the reference VM).

A gate **fails closed**: any fixture mismatch fails the test. Phases
land only when every fixture in the phase's matrix is green. This is
the same closed-gate philosophy as MEP-49 in
[[../0049/11-testing-gates]] §1; the Kotlin pipeline adds
Kotlin-specific cleanliness gates on top.

The Kotlin-specific additions:

- **kotlinc clean gate.** Every emitted `.kt` file must compile under
  `kotlinc -Werror -Xexplicit-api=strict -language-version 2.1
  -api-version 2.1` with zero warnings. A Mochi program producing
  uncompilable Kotlin is a transpiler bug, not a runtime issue.
- **ktlint gate.** Mochi output, after `ktlint -F`, must be a fixed
  point: running ktlint again produces no diff. The default ruleset
  applies (ktlint-standard).
- **Detekt gate (advisory).** Mochi output passes `detekt` static
  analysis with the default ruleset. This is advisory (a regression
  produces a warning, not a hard fail) because detekt's defaults can
  drift between releases.
- **Google Play gate.** The emitted `.aab` must pass Google Play
  Console's pre-launch validation. This catches missing manifest
  permissions, unsupported `targetSdk` regressions, and unsigned
  bundles.

## 2. Phase gates (planned matrix)

Following [[01-language-surface]] and the MEP body's phase plan in
the shared-decisions anchor:

| Phase | Gate                              | Fixture count target | Surface covered                                  |
|-------|-----------------------------------|----------------------|--------------------------------------------------|
| 1     | `TestPhase1Hello`                 | 5                    | hello world, print, basic let, basic int         |
| 2     | `TestPhase2Scalars`               | 20                   | int/float/bool/string ops, comparisons           |
| 3.1   | `TestPhase3Lists`                 | 25                   | list literal, index, len, for-each               |
| 3.2   | `TestPhase3Maps`                  | 25                   | map literal, index, len, keys, values, has       |
| 3.3   | `TestPhase3Sets`                  | 20                   | set literal, add, has, len                       |
| 3.4   | `TestPhase3ListOfRecord`          | 20                   | list[record], comprehensions over records        |
| 4     | `TestPhase4Records`               | 25                   | records, methods, equality, copy                 |
| 5     | `TestPhase5Sums`                  | 25                   | sum types, exhaustive `when`                     |
| 6     | `TestPhase6Funs`                  | 25                   | closures, higher-order, suspend                  |
| 7     | `TestPhase7Query`                 | 30                   | from/where/select, groupBy, orderBy, joins       |
| 8     | `TestPhase8Datalog`               | 20                   | facts, rules, recursion                          |
| 9     | `TestPhase9Agents`                | 25                   | actor pattern, spawn, call, cast                 |
| 10    | `TestPhase10Streams`              | 20                   | Flow, AsyncSequence, await foreach               |
| 11    | `TestPhase11Async`                | 15                   | suspend colouring, structured concurrency        |
| 12    | `TestPhase12FFI`                  | 25                   | JNI (JVM), cinterop (Native), external (JS)      |
| 13    | `TestPhase13LLM`                  | 10                   | generate (provider-pluggable mock)               |
| 14    | `TestPhase14Fetch`                | 10                   | fetch (Ktor client, local test server)           |
| 15    | `TestPhase15AndroidAppBundle`     | 20                   | .aab/.apk bundle, signing, AGP build             |
| 16    | `TestPhase16Reproducible`         | 10                   | reproducible build (byte-identical .jar / .aab)  |
| 17    | `TestPhase17NativeBinaries`       | 20                   | K/Native single binary, Linux/macOS/Windows      |
| 18    | `TestPhase18PlayConsoleValidate`  | 15                   | Google Play pre-launch validation                |

Total target by Phase 18: ~400 fixtures, all green on Kotlin 2.0 +
Kotlin 2.1 across Linux x86-64 / arm64, macOS arm64, Windows x86-64,
plus Android emulator (Phase 15+) and iOS Simulator (Phase 12+ since
K/Native iOS lands in Phase 17).

## 3. Differential testing vs vm3

The vm3 oracle is **the reference Mochi interpreter**. For each
fixture:

1. Run vm3: `mochi run <fixture>.mochi > <fixture>.vm3.out`.
2. Run Kotlin build:
   `mochi build --target=kotlin-jvm <fixture>.mochi -o /tmp/f.jar &&
    java -jar /tmp/f.jar > <fixture>.kotlin.out`.
3. `diff <fixture>.vm3.out <fixture>.kotlin.out` must be empty.

The fixture's checked-in `<fixture>.out` file is the **vm3 oracle
output**; CI verifies vm3 produces it (catching vm3 regressions) and
then verifies the Kotlin target matches.

For non-deterministic fixtures (random, time, streams, agents, LLM),
the fixture is excluded from differential testing and runs only the
static check (stdout vs checked-in `.out`).

The MEP-45 (C), MEP-46 (BEAM), MEP-47 (JVM), MEP-48 (.NET), and MEP-49
(Swift) targets share the fixture pool. Per-target overrides via
`*.kotlin.skip` files for fixtures that exercise target-specific
behaviour we choose to exclude (e.g., JNI-only fixtures skip on
Kotlin/JS).

## 4. kotlinc clean gate

`TestKotlincClean`:

1. Build all fixtures with `mochi build --target=kotlin-source`. This
   emits `.kt` files without invoking Gradle.
2. Compile with `kotlinc -Werror -Xexplicit-api=strict -language-version
   2.1 -api-version 2.1 -jvm-target 17 *.kt -d /tmp/out`.
3. Verify zero warnings on Mochi-generated code.

Warning categories that must be zero:

- Unused variable (`UNUSED_VARIABLE`, `UNUSED_PARAMETER`).
- Implicit conversions (kotlinc has none, but `IMPLICIT_CAST_TO_ANY`
  fires on accidentally unbounded generics).
- Deprecation warnings (`Deprecation`).
- Sealed `when` non-exhaustive (`NO_ELSE_IN_WHEN`).
- Smart-cast impossible (`SMARTCAST_IMPOSSIBLE`).
- Nullable receiver not null-checked (`UNSAFE_CALL`).
- API-visibility leak (`EXPOSED_PROPERTY_TYPE`,
  `EXPOSED_FUNCTION_RETURN_TYPE`).
- Experimental API used without opt-in (`OPT_IN_USAGE`).

Vendor code in `Sources/mochi-runtime-kotlin/` may have its own
suppressed warnings (declared via `@Suppress("...")` per file with a
tracked entry in `SUPPRESSIONS.md` explaining why).

The `-Xexplicit-api=strict` flag forces every public declaration to
have an explicit visibility modifier (`public`, `internal`, `private`,
`protected`). Mochi codegen always emits explicit visibility, so this
is a defensive gate: any leak of an implicit public declaration is
caught here.

## 5. ktlint fixed-point gate

`TestKtlintFixedPoint`:

1. For each emitted `.kt` file: run `ktlint -F <file>` (format
   in-place).
2. Run `ktlint -F <file>` again.
3. Diff must be empty.

ktlint runs once during codegen as a final formatting pass. The gate
verifies idempotence: a non-fixed-point output means a Mochi codegen
bug (e.g., emitting trailing whitespace that ktlint strips, then
re-emits on the next codegen).

ktlint version pinned to **1.5.0** (released 2024-12). The pinned
version is in the Mochi repo's `tools/ktlint-version` file and CI
fetches the binary from the GitHub release.

Default ruleset (`ktlint-standard`) includes:

- `indent`: 4-space indent.
- `max-line-length`: 120 characters.
- `no-trailing-spaces`.
- `final-newline`.
- `parameter-list-spacing`.
- `function-signature` (Kotlin 1.6+ function signature formatting).

Mochi codegen emits Kotlin that already conforms to these defaults, so
ktlint mostly normalises whitespace; the gate catches drift.

## 6. Detekt advisory gate

`TestDetektAdvisory` (advisory, regression-warning only):

1. For each emitted `.kt` file: run `detekt --build-upon-default-config
   --input <file>`.
2. Verify zero issues at the `error` severity.
3. Count issues at `warning` severity; record in a baseline file.
4. A regression (warning count increases) fires a CI warning but does
   not fail the build.

detekt version pinned to **1.23.7** (released 2024-10). The default
config plus a small overlay (`detekt-config.yml` in the Mochi repo)
disables a handful of rules that Mochi codegen reasonably violates
(e.g., `MagicNumber` because Mochi numeric literals lower verbatim).

The advisory status is because detekt's default rules drift between
1.x minor versions; a strict gate would constantly break on detekt
upgrades. The warning lane gives us visibility without blocking.

## 7. Kotlin version matrix

| Kotlin version | Released   | Floor / ceiling      | Required CI status |
|----------------|------------|----------------------|--------------------|
| Kotlin 2.0.21  | 2024-10-10 | Floor                | required           |
| Kotlin 2.1.0   | 2024-11-27 | Ceiling              | required           |
| Kotlin 2.2.x   | 2025-Q3    | Advisory             | warning-only       |
| Kotlin 1.9.x   | 2023-2024  | not supported (pre-K2 mandatory) | rejected |

Every gate runs on Kotlin 2.0.21 and 2.1.0 in parallel. A phase lands
only when both are green. The 2.2 matrix runs advisory; a regression
triggers a warning, not a block.

The K2 compiler is the only frontend we target. K1 (the legacy
frontend) was the default through Kotlin 1.9 and is removed in 2.0+;
any Mochi-generated code that requires K1 is a bug.

Matrix implementation:

```yaml
strategy:
  matrix:
    kotlin: [2.0.21, 2.1.0]
    os: [ubuntu-24.04, macos-15, windows-2022]
    target: [jvm, native, js, wasm-js, android]
    exclude:
      - os: ubuntu-24.04
        target: ios      # iOS K/Native requires macOS host
      - os: windows-2022
        target: ios      # iOS K/Native requires macOS host
      - os: ubuntu-24.04
        target: macos    # macOS K/Native requires macOS host
      - os: windows-2022
        target: macos    # macOS K/Native requires macOS host
```

## 8. Android packaging gate (Phase 15)

`TestPhase15AndroidAppBundle`:

1. For each phase-15-eligible fixture (declares `@app` in Mochi):
   - Emit Kotlin sources, Gradle project, and `AndroidManifest.xml`.
   - `./gradlew bundleRelease assembleRelease`.
   - Verify `app/build/outputs/bundle/release/app-release.aab` exists
     and is non-empty.
   - Verify `app/build/outputs/apk/release/app-release.apk` exists
     and is non-empty.
   - Sign both with the debug keystore (CI has no production keystore
     by default).
   - Run `bundletool validate --bundle app-release.aab` to confirm
     bundle integrity.
   - Run `apksigner verify --verbose app-release.apk` to confirm
     signature.
2. For each fixture that opts into instrumented tests:
   - `./gradlew connectedCheck` on the Android emulator (API 35).
   - Verify all instrumented tests pass.
3. Optionally upload to Play internal track:
   - Gated behind `MOCHI_PLAY_PUBLISH=1` env var (default off).
   - Uses `play-publisher-cli` to push to the internal-test track.

This gate requires Linux CI runner with Android SDK installed and KVM
support for the emulator. The KVM requirement filters out
GitHub-hosted runners other than `ubuntu-24.04`; the emulator job
takes ~5-10 minutes per fixture due to emulator boot time.

The AAB validation uses Google's `bundletool` (open source at
github.com/google/bundletool, Apache-2.0). bundletool checks the AAB's
ProtoBuf manifest, resource references, native-library architecture
matrix, and minSdk/targetSdk consistency.

## 9. Google Play Console validation gate (Phase 18)

`TestPhase18PlayConsoleValidate`:

1. For each phase-18-eligible fixture:
   - Build `.aab` as in Phase 15.
   - `play-publisher-cli validate <aab>` against the Google Play
     Developer API. Exit code 0 means validation passed.
   - The Pre-launch report runs in Firebase Test Lab on a matrix of
     emulated devices (Pixel 6, Pixel 8, Samsung Galaxy S24, low-end
     budget device).

Validation catches:

- Missing required manifest entries (`<application>`, `<activity>`).
- Forbidden API usage (private system intents, restricted
  permissions).
- Asset issues (missing launcher icon, wrong density).
- Architecture mismatches (e.g., missing arm64-v8a slice).
- targetSdk regression (must be within Google Play's current floor).
- App size regression (Google Play imposes a 150 MB cap on the base
  module, larger via Play Asset Delivery).

Requires Google Play Developer API service account credentials (stored
as CI secret `MOCHI_PLAY_SERVICE_ACCOUNT_JSON`). Runs on PR with the
`needs-play-validate` label only, not on every commit, to control
cost (Firebase Test Lab is billed per minute).

## 10. K/Native single-binary gate (Phase 17)

`TestPhase17NativeBinaries`:

1. For each phase-17-eligible fixture:
   - `mochi build --target=kotlin-linux-x64`.
   - Verify the produced binary at `target/kotlin/dist/<name>` is an
     ELF executable: `file <binary>` reports
     `ELF 64-bit LSB executable`.
   - Verify dynamic dependencies via `ldd <binary>`: only system libc,
     libpthread, libdl, libm are allowed.
   - Run the binary, compare stdout against vm3.
2. Repeat for `kotlin-linux-arm64` on an aarch64 runner.
3. Repeat for `kotlin-macos-arm64` on macOS arm64 runner.
4. Repeat for `kotlin-windows-x64`:
   - Verify the produced `.exe` is a valid PE32+: `file <exe>` reports
     `PE32+ executable`.
   - Verify it runs on a clean Windows runner (no DLL not in
     %SystemRoot%\System32).
5. Repeat for `kotlin-ios-arm64` (XCFramework build only, no device
   execution gate at this phase; device tests run only on the Apple
   self-hosted runner for Phase 12 FFI).

This gate validates the single-binary story. Kotlin/Native produces
self-contained executables that link statically against the Kotlin
runtime; the only dynamic deps are system libraries. The Linux x64
hello-world binary is ~3.5 MB; the macOS arm64 hello-world is ~2.8
MB; the Windows x64 hello-world is ~4.2 MB.

## 11. Reproducible build gate (Phase 16)

`TestPhase16Reproducible`:

1. For each phase-16-eligible fixture:
   - Build with `mochi build --target=kotlin-jvm --deterministic`.
   - Record the `.jar` SHA-256.
   - Build again on a different CI runner (e.g., first build on
     `ubuntu-24.04`, second on `macos-15`).
   - Verify the SHA-256 matches.
2. Repeat for `--target=kotlin-android` against the `.aab`.
3. Repeat for `--target=kotlin-linux-x64` against the static binary.

Reproducibility requires:

- Pinned Kotlin toolchain (`libs.versions.toml`).
- Pinned Gradle version (`gradle-wrapper.properties` with checksum).
- Pinned JDK (bundled Temurin 17).
- `kotlinc` flag `-Xklib-relative-path-base=.` to avoid embedding host
  absolute paths.
- Gradle `Jar { isPreserveFileTimestamps = false;
  isReproducibleFileOrder = true }`.
- AGP `android.experimental.cacheCompileLibResources = true`.
- `SOURCE_DATE_EPOCH` environment variable set to the most recent
  Mochi source mtime.
- For K/Native: `-Xlinker --build-id=none` to suppress randomised
  linker build IDs.

The gate uses `diffoscope` to compare two builds when SHA-256 mismatch
occurs, producing a structured report of which bytes differ. The
Mochi CI archives the diffoscope report on failure so the regression
can be debugged.

This mirrors MEP-49's reproducibility gate in
[[../0049/11-testing-gates]] §10 and MEP-47's JVM jar reproducibility
gate.

## 12. Kotlin test gate

For Mochi `test` blocks, the codegen emits `kotlin-test` compatible
methods that use JUnit 5 Jupiter under the hood for JVM:

```kotlin
import kotlin.test.Test
import kotlin.test.assertEquals

class Phase1HelloTest {
    @Test
    fun helloOutput() {
        val result = mochi.user.greet("World")
        assertEquals("Hello, World!", result)
    }
}
```

`kotlin-test` is the multiplatform test API that delegates to:
- JUnit 5 Jupiter on JVM/Android.
- XCTest on iOS/macOS via Kotlin/Native.
- mocha on Kotlin/JS.
- a Kotlin/Wasm-specific runner on Wasm-JS.

The gate runs `./gradlew test` (JVM), `./gradlew connectedAndroidTest`
(Android emulator), `./gradlew iosSimulatorArm64Test` (iOS Simulator,
macOS-only), `./gradlew jsTest` and `./gradlew wasmJsTest` (JS
runners), and `./gradlew nativeTest` (per-host K/Native) and verifies
every test passes.

Kotest is the alternative test framework; MEP-50 v1 sticks with
`kotlin-test` because it is JetBrains-maintained and the only one
that integrates cleanly with KMP source sets. Users who prefer Kotest
can override via Mochi config but it is not the default.

## 13. Cross-target differential gate

Beyond vm3-equality, the six backends (C, BEAM, JVM, .NET, Swift,
Kotlin) all produce byte-equal stdout on every fixture in their
shared matrix. The cross-target gate:

```
TestCrossTargetDifferential
```

1. For each fixture:
   - Run on vm3, C, BEAM, JVM, .NET, Swift, Kotlin (whichever are
     eligible).
   - Verify every pair produces byte-equal stdout.
2. Report any divergence as a "target divergence" error.

A divergence is *always* a bug in at least one target (assuming the
fixture is deterministic). The most common cause when Kotlin is
involved is UTF-16 vs UTF-8 boundary translation: Kotlin's `String` is
UTF-16, and when Mochi source contains a string with a surrogate-pair
emoji the printf representation may differ from the UTF-8 backends.
The Mochi codegen normalises at the I/O boundary; the gate catches
regressions.

This gate runs on every PR for the fixture intersection where all
seven targets compile cleanly. Roughly ~300 of the ~400 fixtures fall
into the shared intersection.

## 14. Memory and resource gates (Phase 9+)

`TestPhase18MemoryStable`:

For streams and agent phases, a long-running fixture (e.g., 1M agent
messages) must not leak heap. The gate:

1. Run the fixture for N seconds.
2. Capture peak heap via:
   - JVM: VisualVM remote attach, `jcmd <pid> GC.heap_info`.
   - Android: Android Studio Profiler exporting heap dumps via
     `adb shell am dumpheap`.
   - K/Native: built-in
     `kotlin.native.runtime.GC.detectCycles()` plus `top -p` for RSS.
   - iOS Simulator: Instruments `Allocations` tool via
     `instruments -t Allocations -D /tmp/trace.trace -l 30000
     <pid>`.
3. Verify the steady-state heap is bounded (no monotonic growth).
4. Verify the working set RSS is bounded across the workload.

For actor-heavy fixtures, also capture `Job` counts via
`kotlinx.coroutines.debug.DebugProbes`:

```kotlin
DebugProbes.install()
// ... run workload ...
val coroutines = DebugProbes.dumpCoroutinesInfo()
check(coroutines.size < expectedCount) { "coroutine leak" }
```

For long-running Flow fixtures, also verify that cancellation works:
the gate sends `coroutineScope.cancel()` after N seconds and verifies
all coroutines finish within a deadline.

## 15. Security gates

Per the threat model and memory-safety spec:

- **TLS pinning gate.** The Mochi `fetch` runtime, when configured
  with a pinned certificate, must reject mismatched cert chains. The
  gate uses a local test server with a known-bad cert and verifies
  Ktor rejects (via the OkHttp engine on JVM/Android, the Darwin
  engine on iOS, the curl engine on Linux, the WinHttp engine on
  Windows).
- **FFI sandbox gate.** Mochi external/JNI/cinterop calls require a
  manifest entry. The gate verifies that a call not in the manifest
  is rejected at compile time (Mochi-level check) and at runtime
  (when reflection bypasses).
- **No `dynamic` from user surface.** Mochi codegen never emits
  `dynamic` type (Kotlin's escape hatch for JS interop). The
  `kotlin-source` output is grepped for `: dynamic` and `dynamic` as
  a type annotation; any hit fails the gate.
- **No `unsafeCast` or reflection in user code.** Mochi codegen never
  emits `unsafeCast`, `Class.forName`, `KClass<*>.starProjectedType`,
  or `KFunction<*>.call`. The runtime library may use reflection
  (`MochiRuntime.Internal`) but Mochi user code paths must not.
- **No external linker via FFI without manifest.** The K/Native
  cinterop `def` files are checked against a Mochi-declared FFI
  manifest; an unlisted `def` fails the build.
- **Android permission audit.** AGP's manifest merger combines library
  manifests; Mochi's CI runs `aapt2 dump permissions <apk>` and
  verifies the merged permission set matches the Mochi `@permissions`
  declaration. An undeclared permission is a build failure.

## 16. Performance gates

Soft gates (regression-warning, not block):

- Hello-world JVM cold-start: <= 1500ms on Linux x86_64 (cold JVM is
  slow; ART on Android is much faster).
- Hello-world JVM warm-start: <= 200ms.
- Hello-world Android cold-start: <= 300ms on Pixel 8 (Android 14).
- Hello-world K/Native cold-start: <= 50ms on Linux x86_64.
- Hello-world Kotlin/JS cold-start: <= 200ms in Node 20.
- Hello-world Kotlin/Wasm cold-start: <= 100ms in Chrome 130.
- `.jar` size: <= 2 MB for hello-world (with bundled runtime).
- `.aab` size: <= 6 MB for hello-world.
- K/Native binary: <= 8 MB Linux x86_64, <= 6 MB macOS arm64, <= 10 MB
  Windows x86_64.
- 1M-element list iteration on JVM: <= 1.5x vm3.
- 1M-element map insert on JVM: <= 1.5x vm3.
- Actor message dispatch (Channel-based): <= 500ns per message on
  JVM, <= 1us on K/Native.

Regressions of >10% fail the soft gate with a warning; >25% fail
hard. The performance gate runs nightly on a dedicated benchmark
runner (a fixed AWS m6i.large instance) so wall-clock numbers are
comparable across runs.

## 17. Embedded Kotlin gate (no such thing)

Reserved for a hypothetical future "Embedded Kotlin" subset analogous
to Embedded Swift. No such subset exists today; the Kotlin compiler
has no "embedded" mode. Microcontroller deployment via Mochi remains
the domain of MEP-45 (C, freestanding).

Not in MEP-50 v1 scope. No gate.

## 18. Phase gate summary

For Phase N to land:

1. `TestPhaseN` passes on Kotlin 2.0.21 and 2.1.0, Linux x86_64/arm64,
   macOS arm64, Windows x86_64, plus Android emulator for Phase 15+
   and iOS Simulator for Phase 12+.
2. `TestKotlincClean` passes on the Phase N fixtures.
3. `TestKtlintFixedPoint` passes.
4. `TestDetektAdvisory` records the warning count baseline.
5. Cross-target gate green where applicable.
6. Google Play validation gate green for Phase 18 fixtures.
7. K/Native single-binary gate green for Phase 17 fixtures.
8. Reproducibility gate green for Phase 16 fixtures.
9. Android packaging gate green for Phase 15 fixtures.
10. The phase commit includes:
    - The new gate test under `tests/transpiler3/kotlin/`.
    - The new fixtures under
      `tests/transpiler3/kotlin/fixtures/phase<NN>/`.
    - The phase entry in the MEP-50 progress log.

A phase that misses any of these is "in-progress", not "landed".

## 19. Out of scope for the testing notes

- Property-based testing (kotlinx-rpc has no proptest framework; Kotest
  has property testing but we do not use Kotest): documented as v2
  follow-up.
- Fuzzing (kotlinx.fuzz is experimental): future work.
- Mutation testing (PIT for JVM, Stryker has no Kotlin backend):
  future work.
- UI testing (Compose UI Testing, Espresso): future work, not a Mochi
  codegen concern.
- Snapshot testing of emitted `.kt` files (we have the kotlinc-clean
  gate which is stronger than snapshot equality).

These are documented in [[12-risks-and-alternatives]].

## 20. Test fixture layout

Each fixture lives in
`tests/transpiler3/kotlin/fixtures/phase<NN>/<name>/`:

```
phase01/hello/
|-- hello.mochi              # Mochi source
|-- hello.out                # vm3 oracle stdout
|-- hello.kotlin.skip        # (optional) skip marker with reason
|-- expected/
|   |-- mochi/user/Hello.kt  # expected Kotlin output (golden file)
```

The `expected/` directory contains golden Kotlin files that the
codegen must reproduce byte-for-byte. A golden mismatch is a
test failure; updating the golden requires running the update tool
with the explicit `--update-goldens` flag (which never runs on CI).

The golden file approach catches regressions in formatting,
parameter naming, and explicit-API visibility annotation. It is the
same pattern MEP-49 uses for `.swift` goldens and MEP-47 uses for
`.java` goldens.

## 21. Continuous integration topology

The Mochi monorepo's CI topology for the Kotlin target:

- **Pre-merge.** Every PR runs the full matrix described in §7 plus
  the per-phase gates for any phase whose fixtures are touched.
  Target wall-clock: <= 25 minutes from PR open to all-green.
- **Post-merge.** Every commit on `main` runs the full ~400 fixtures
  on every (kotlin, os, target) combination. Wall-clock: ~45-60
  minutes due to Android emulator boot.
- **Nightly.** Performance gate, reproducibility gate, cross-target
  differential against the latest commits of all six backends.
  Wall-clock: ~2 hours.
- **Weekly.** Google Play validation gate (Firebase Test Lab is
  rate-limited), App Store Connect validation gate cross-referenced
  from MEP-49 for iOS shared fixtures.

Total monthly CI cost for Kotlin matrix: ~$1500 USD (GitHub Actions
billing). Comparable to MEP-47 JVM matrix; smaller than MEP-49 Swift
matrix because the macOS runner footprint is lighter.

## 22. CI runner topology

```yaml
strategy:
  matrix:
    kotlin: [2.0.21, 2.1.0]
    os: [ubuntu-24.04, macos-15, windows-2022]
    target: [jvm, native, js, wasm-js]
    include:
      - os: ubuntu-24.04
        kotlin: 2.1.0
        target: android       # only on Linux to maximise KVM compat
      - os: ubuntu-24.04-arm
        kotlin: 2.1.0
        target: native-arm64
      - os: macos-15
        kotlin: 2.1.0
        target: ios           # iOS K/Native build only
    exclude:
      - os: ubuntu-24.04
        target: macos
      - os: windows-2022
        target: macos
      - os: ubuntu-24.04
        target: ios
      - os: windows-2022
        target: ios
```

The `ubuntu-24.04` runner is the primary workhorse. It has KVM
acceleration (needed for Android emulator), the Kotlin compiler
cache, and the Gradle wrapper cache. `macos-15` runs Apple-target
builds (iOS, macOS) plus a JVM-on-macOS smoke test. `windows-2022`
runs the JVM-on-Windows path plus Kotlin/Native MinGW.

## 23. Caching for CI

```yaml
- uses: actions/cache@v4
  with:
    path: |
      ~/.gradle/caches
      ~/.gradle/wrapper
      ~/.konan
      ~/.android/build-cache
    key: kotlin-${{ runner.os }}-${{ hashFiles('**/*.gradle*', '**/libs.versions.toml', '**/gradle-wrapper.properties') }}
    restore-keys: |
      kotlin-${{ runner.os }}-
```

Cache hit rates after the first successful run on a branch:

- Gradle distribution: 100% (cached by `gradle-wrapper.properties`).
- Gradle dependency cache: 95% (some fixtures pull additional deps).
- Kotlin/Native dependencies: 90% (per-host LLVM toolchain).
- Android SDK build tools: 100% after first install.
- Android emulator system image: 100% after first install (~2 GB).

Cold cache cost (no `actions/cache` hit): ~8 minutes for Gradle
distribution and dependency cache plus ~12 minutes for Kotlin/Native
toolchain plus ~10 minutes for Android SDK; total ~30 minutes added
to first build on a new branch. Subsequent builds hit warm cache and
take ~5 minutes per matrix cell.

## 24. Test execution model

Tests run in three modes depending on phase:

- **Phase 1-11 (language surface)**: pure unit tests. `./gradlew test`
  on each target. No external dependencies. Wall-clock per fixture:
  ~2 seconds.
- **Phase 12-14 (FFI, LLM, fetch)**: integration tests with mocked
  external dependencies. `./gradlew test` plus a local mock server
  for fetch and a stub LLM provider. Wall-clock per fixture: ~5
  seconds.
- **Phase 15-18 (packaging, native, reproducible, Play validation)**:
  packaging tests requiring real toolchains. Wall-clock per fixture:
  ~30-90 seconds.

Parallel execution: Gradle's test task parallelises across modules but
not across fixtures by default. Mochi's test runner shards fixtures
across CI cells via the matrix configuration.

## 25. Test isolation

Each fixture runs in a fresh Gradle project directory (a temp dir
created per test). No shared state between fixtures. The Gradle daemon
is disabled on CI (`org.gradle.daemon=false`) so each invocation
starts fresh; on dev machines the daemon is reused across runs but
not across project directories.

Mochi's content-addressed cache (see [[10-build-system]] §15) lives
outside the project tree and is shared across fixtures; this is safe
because the cache is keyed by source hash + transpiler version +
target identifier.

Cross-references: [[01-language-surface]] for the phase plan,
[[06-type-lowering]] for the type regression matrix,
[[09-agent-streams]] for the actor/stream test patterns,
[[10-build-system]] for the build driver under test,
[[12-risks-and-alternatives]] for the risks each gate mitigates,
[[../0049/11-testing-gates]] for the Swift sibling gate.
