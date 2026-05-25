# MEP-47 research note 11, Testing strategy and gates for MEP-47

Author: research pass for MEP-47.
Date: 2026-05-23 (GMT+7).

This note specifies the test-as-spec gates that govern each MEP-47 phase. It
mirrors the MEP-45 `TestPhase*` and MEP-46 `TestPhase*` patterns: a single
deterministic Go test per phase, plus a differential gate against vm3, plus a
Java type-check gate, plus a JDK version matrix, plus a native-image gate.

## 1. Gate philosophy

A **gate** is a Go test (`Test...` in the Mochi repo, under
`tests/transpiler3/jvm/`) that:

1. Iterates fixtures in `tests/transpiler3/jvm/...`
2. Compiles each fixture with the MEP-47 toolchain (`mochi build --target=jvm`).
3. Runs the resulting `.class` / `.jar` (via `java -cp ...` or `java -jar`).
4. Compares actual stdout against `<fixture>.out`.
5. Compares against the vm3 oracle (running the same Mochi source on the
   reference VM).

A gate **fails closed**: any fixture mismatch fails the test. Phases land only
when every fixture in the phase's matrix is green.

Mochi's tradition (from MEP-45 and MEP-46) is one gate per phase, gates remain
in CI forever, and the fixture matrix only grows. We follow the same pattern
for MEP-47.

The JVM-specific addition is that *javac itself is a gate*: every emitted
.java file must compile under `--release 21 -Xlint:all -Werror`. A Mochi
program that produces uncompilable Java is a transpiler bug, not a runtime
issue.

## 2. Phase gates (planned matrix)

Following [[01-language-surface]] and the MEP body's phase plan:

| Phase | Gate                       | Fixture count target | Surface covered                                |
|-------|----------------------------|----------------------|------------------------------------------------|
| 1     | `TestPhase1Hello`          | 5                    | hello world, print, basic let, basic int       |
| 2     | `TestPhase2Scalars`        | 20                   | int/float/bool/string ops, comparisons         |
| 3.1   | `TestPhase3Lists`          | 25                   | list literal, index, len, for-each             |
| 3.2   | `TestPhase3Maps`           | 25                   | map literal, index, len, keys, values, has     |
| 3.3   | `TestPhase3Sets`           | 20                   | set literal, add, has, len                     |
| 3.4   | `TestPhase3ListOfRecord`   | 20                   | list[record], comprehensions over records      |
| 4     | `TestPhase4Records`        | 25                   | records, methods, equality                      |
| 5     | `TestPhase5Sums`           | 25                   | sum types, pattern matching                     |
| 6     | `TestPhase6Funs`           | 25                   | closures, higher-order, partial app             |
| 7     | `TestPhase7Query`          | 30                   | from/where/select, group_by, order_by, joins   |
| 8     | `TestPhase8Datalog`        | 20                   | facts, rules, recursion                         |
| 9     | `TestPhase9Agents`         | 25                   | agent definitions, spawn, call, cast            |
| 10    | `TestPhase10Streams`       | 20                   | streams, subscribe, publish                     |
| 11    | `TestPhase11Async`         | 15                   | structured concurrency, futures                |
| 12    | `TestPhase12FFI`           | 25                   | extern Java imports, Maven Central deps        |
| 13    | `TestPhase13LLM`           | 10                   | generate (mocked LLM provider)                 |
| 14    | `TestPhase14Fetch`         | 10                   | fetch (HTTP, against local test server)        |
| 15    | `TestPhase15NativeImage`   | 30                   | GraalVM native-image AOT compilation           |
| 16    | `TestPhase16Reproducible`  | 10                   | reproducible build (byte-identical class)      |

Total target by Phase 16: ~360 fixtures, all green on JDK 21 LTS and JDK 25
LTS, both Linux x86-64, Linux aarch64, macOS aarch64, and Windows x86-64.

## 3. Differential testing vs vm3

The vm3 oracle is **the reference Mochi interpreter** (the original
tree-walker in `interp/`). For each fixture:

1. Run vm3: `mochi run <fixture>.mochi > <fixture>.vm3.out`.
2. Run JVM build: `mochi build --target=jvm-uberjar <fixture>.mochi -o
   /tmp/f.jar && java -jar /tmp/f.jar > <fixture>.jvm.out`.
3. `diff <fixture>.vm3.out <fixture>.jvm.out` must be empty.

The fixture's checked-in `<fixture>.out` file is the **vm3 oracle output**;
CI verifies vm3 produces it (catching vm3 regressions) and then verifies
the JVM target matches.

For non-deterministic fixtures (random, time, streams, agents), the fixture
is excluded from differential testing and runs only the static check
(stdout vs checked-in `.out`).

The MEP-45 (C) and MEP-46 (BEAM) targets run the same fixtures. The
`tests/transpiler3/` directory has a shared fixture pool with per-target
overrides for fixtures that exercise target-specific behaviour (e.g.,
`*.jvm.skip` to skip a fixture on JVM).

## 4. javac warnings gate

`TestJavacClean`:

1. Build all fixtures with `mochi build --target=jvm-source`.
2. Run `javac --release 21 -Xlint:all -Werror -d /tmp/out <generated>.java`.
3. Verify zero warnings on Mochi-generated code.

This gate validates that the emitted Java is high-quality: no rawtypes
warnings, no unchecked casts, no deprecation warnings, no preview-feature
warnings (preview features rejected by policy, see note 02). Vendor code
in `target/jvm/vendor/` may have its own warnings.

False positives (javac over-conservative on certain generic bounds) are
documented in `tests/transpiler3/jvm/javac_allowlist.txt` with a
rationale.

## 5. ErrorProne / NullAway gate (optional)

For users opting into stricter null analysis,
`TestErrorProneClean` runs Google's ErrorProne against generated code:

1. Build with `-Xplugin:ErrorProne`.
2. Verify zero errors.

NullAway adds tighter null-flow analysis, useful because Mochi has no
`null` at the language level (see [[06-type-lowering]] §15). Initially
**disabled in CI** (ErrorProne can be brittle across JDK releases);
promoted to required gate in Phase 13+.

## 6. JDK version matrix

CI runs every gate on:

- JDK 21.0.latest (Temurin) - the floor
- JDK 25.0.latest (Temurin) - the current LTS
- JDK 26 EA (Temurin) - the next non-LTS, smoke-tested

We do **not** test JDK 22, 23, or 24 because they are out of support and
not the version of the JVM most users will run.

Failure on JDK 26 EA is **non-blocking** during the first month after
release; it becomes blocking once Temurin promotes 26 to GA. This gives
us a one-month-window to react to forward-compat breakage. The same
policy applied for JDK 25 EA and worked well: we caught two GraalVM
native-image incompatibilities before users hit them.

The matrix runs on:

- Linux x86_64 (Ubuntu 24.04, primary)
- Linux aarch64 (Ubuntu 24.04, primary)
- macOS aarch64 (macOS 14, GitHub Actions M-series)
- macOS x86_64 (macOS 13, GitHub Actions deprecated-but-available)
- Windows x86_64 (windows-2022)

JDK 25 dropped 32-bit x86 (JEP 503), so we do not test it. JDK 21 still
supports it, but we skip the matrix entry; users wanting 32-bit must build
themselves.

Total CI cells: 5 OS x 3 JDK = 15 cells. Each cell runs all phase gates.

## 7. GraalVM native-image gate

`TestPhase15NativeImage`:

1. For each fixture in the native-image-eligible pool (most of the
   surface; some excluded for reflection / dynamic class loading
   reasons), build a native image:
   `mochi build --target=jvm-native <fixture>.mochi -o /tmp/f`.
2. Run `/tmp/f` and compare stdout against the oracle.
3. Measure startup time; reject if >100 ms on the CI runner (which is
   slower than real hardware; the user-facing target is <50 ms).

GraalVM native-image config (reachability metadata) lives in
`tests/transpiler3/jvm/native-image/META-INF/native-image/`. The metadata
is hand-curated for the runtime jar and auto-generated by running
fixtures with `-agentlib:native-image-agent` in CI.

Failures here include: reflection used without metadata; serialization
classes missing from the metadata; resources not registered; locale
data missing. Each failure mode has a documented fix path in note 04.

## 8. Reproducible-build gate

`TestPhase16Reproducible`:

1. Build a fixture twice: `mochi build --jvm-uberjar -o /tmp/a.jar`
   followed by `mochi build --jvm-uberjar -o /tmp/b.jar`.
2. Verify `sha256sum /tmp/a.jar == sha256sum /tmp/b.jar`.

The build uses `SOURCE_DATE_EPOCH` for jar timestamps, the `--release 21`
flag for stable bytecode, and `maven-jar-plugin >= 3.4.0` for
reproducible-by-default packaging.

A subset of fixtures runs the `diffoscope` extra check (compares jar
contents structurally) on every PR; the full set runs nightly.

## 9. JFR gate

`TestPhase9AgentsJFR`:

1. Run an agent-heavy fixture with `-XX:StartFlightRecording=filename=/tmp/f.jfr`.
2. Parse the JFR file with `jdk.jfr.consumer.RecordedEvent`.
3. Verify expected events emitted: `dev.mochi.AgentStart`,
   `dev.mochi.AgentStop`, `dev.mochi.MessageSend`, `dev.mochi.QueryStart`,
   `dev.mochi.QueryEnd`.
4. Verify no `jdk.VirtualThreadPinned` events fired (regression gate
   against accidental `synchronized` use, see [[09-agent-streams]] §9).

This gate is critical: it's the only way to validate that we're not
accidentally pinning carrier threads, which would silently degrade
performance under load.

## 10. Maven Central round-trip gate

`TestPhase12MavenRoundtrip`:

1. Take a Mochi program that imports a known Maven Central dep
   (`com.fasterxml.jackson.databind.ObjectMapper`).
2. Resolve dep via `mochi build --jvm-mvn-resolve`, which calls Maven
   resolver to fetch the jar.
3. Build, run, verify output.

This gate validates the Maven coordinate parsing, the lockfile format,
the SHA-256 verification, and the offline cache.

Runs nightly (not per-PR) because it depends on network. Caches
artifacts in a CI volume for stability.

## 11. JDK FFI gate

`TestPhase12JdkFFI`:

For each of a curated list of JDK API surfaces:

- `java.time.*` (Instant, Duration, ZoneId, LocalDate, LocalDateTime)
- `java.util.UUID`
- `java.net.URI`, `java.net.http.HttpClient`
- `java.nio.file.Path`, `java.nio.file.Files`
- `java.util.regex.Pattern`, `java.util.regex.Matcher`
- `java.security.MessageDigest`

verify that Mochi can `import` the class, call its methods, and round-trip
values correctly.

This is the gate that validates note 04's "thin runtime, fat JDK" thesis:
if users can't seamlessly call into `java.time.*`, we've failed.

## 12. Loom pinning regression gate

`TestPhase9NoSyncPinning`:

1. Run every fixture under `-Djdk.virtualThreadScheduler.parallelism=1`
   so virtual threads contend for a single carrier.
2. Enable `jdk.VirtualThreadPinned` JFR events.
3. Assert no events fire from generated code.

JEP 491 (JDK 24+) fixes pinning on `synchronized`, but only for
synchronisation-only pins; pins from JNI / FFM, class init, and symbolic
resolution still happen. The gate catches accidental introductions of
`synchronized` in the runtime or in generated code.

## 13. Determinism gate (agents)

`TestPhase9AgentsDeterministic`:

When `MOCHI_DETERMINISTIC=1` is set, agent fixtures run on a single-thread
executor with a synthetic clock (see [[09-agent-streams]] §11). The
oracle output is bit-stable across runs.

The gate validates that the deterministic mode produces stable output
on the JVM, matching vm3's deterministic mode.

## 14. Performance gates (non-blocking)

`BenchPhase*` Go benchmarks run nightly and post results to a
performance dashboard. Regressions of >10% relative to the 7-day
trailing average open an issue but do not block.

Metrics tracked:

- Cold start (hello world): target <50 ms native-image, <500 ms JVM.
- Steady-state throughput (hot loop): target >=80% of vm3's Go-native
  throughput on the same micro-benchmark.
- Memory (RSS at 100 concurrent agents): target <100 MB.
- Build time: target <5 s for a 100-fixture test pass on a 4-core
  machine.

## 15. Backend-equivalence gate

`TestBackendEquivalence`:

For each fixture, run all available targets (vm3, MEP-45 C, MEP-46 BEAM,
MEP-47 JVM) and compare outputs. Any divergence is a bug; first triage:

1. Is the fixture deterministic? If no, mark and skip.
2. Does it use target-specific features? If yes, mark and skip.
3. Otherwise: file an issue, halve the fixture, find the smallest
   reproducer, decide which backend is wrong.

This gate is the strongest correctness check we have; it's the reason
MEP-47 will not ship divergent semantics for things that should be
identical (numeric, collections, query results).

## 16. Static-analysis gates (Go side)

Standard Mochi-repo gates apply to the MEP-47 transpiler code itself:

- `go vet ./transpiler3/jvm/...` must pass.
- `staticcheck ./transpiler3/jvm/...` must pass.
- `golangci-lint run ./transpiler3/jvm/...` must pass with the repo's
  golangci config.
- `go test ./transpiler3/jvm/... -race` must pass.

## 17. CI infrastructure

The MEP-47 gates extend the existing repo CI:

- A new GitHub Actions workflow `jvm.yml` triggers on PRs that touch
  `transpiler3/jvm/**`, `runtime/jvm/**`, or `tests/transpiler3/jvm/**`.
- Per-OS matrix uses `setup-java@v4` (Temurin) and `setup-graalvm@v1`
  (Liberica NIK 25 for native-image).
- Per-PR runs: javac, scalars, lists, maps, sets, records, sums, funs
  (the fast surface).
- Per-merge runs: query, datalog, agents, streams, FFI (the full
  surface).
- Nightly runs: native-image, reproducible-build, Maven round-trip,
  determinism, JFR.

Caching: the GitHub Actions cache holds the local Maven repository
(`~/.m2/repository`), the GraalVM toolchain, and the build cache from
note 10 §13.

## 18. Test fixture layout

```
tests/transpiler3/jvm/
  phase01-hello/
    hello.mochi
    hello.out
    hello.skip       (empty if not skipped)
  phase02-scalars/
    arith.mochi
    arith.out
    ...
  phase15-native/
    hello_native.mochi
    hello_native.out
    hello_native.reachability.json
  phase16-repro/
    deterministic_build.mochi
    deterministic_build.out
  javac_allowlist.txt
  errorprone_allowlist.txt
  native-image/
    META-INF/
      native-image/
        reachability-metadata.json
```

Per-fixture skip files: `<name>.jvm.skip` if the fixture is excluded on
this target with a one-line reason. Skip rate is a quality metric; PR
that increases the skip count needs a written justification.

## 19. Manual exploratory testing (preview)

Phases marked "preview" in MEP-47 (e.g., Android via D8, see
[[07-jvm-target-portability]] §10) have looser gates: a smoke test
fixture, manual sign-off, no differential vs vm3 (because vm3 doesn't
target Android).

These graduate to full gates when the surface stabilises.

## 20. What we deliberately do NOT gate on

- **Performance must beat vm3**: not required. We target parity with
  vm3 and out-perform on the hot path (where JIT helps); but the gate
  doesn't fail when JVM is slower on a 10-line script (the JIT can't
  warm up).
- **Identical Java source byte-for-byte across runs**: the emitted
  .java is reproducible from a Mochi source, but cross-toolchain
  identical-Java is not a goal (newer transpiler versions may
  legitimately emit different Java). The *resulting class files*
  are byte-identical when build inputs match (the reproducibility
  gate).
- **All Maven Central artefacts work**: we cannot test every artefact;
  we test a curated list of widely-used ones (Jackson, Guava,
  HttpClient, Jakarta, log4j). Users importing other libraries are
  not covered by gates; they're covered by the FFI mechanism's
  type-safety.

## 21. Cross-references

- [[01-language-surface]] - the surface each gate covers.
- [[10-build-system]] - how the build invokes the gates.
- [[09-agent-streams]] - why the Loom pinning gate matters.
- [[06-type-lowering]] - the type contract Java type-checks against.
- [[../0046/11-testing-gates]] - the BEAM-target analogue.
- [[../0045/11-testing-gates]] - the C-target analogue.

This note is the testing contract. Updates require a paired update to
the matching section of `mep-0047.md` (the phase plan §9).
