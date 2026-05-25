# MEP-47 research note 02, Design philosophy

Author: research pass for MEP-47 (Mochi → JVM transpiler).
Date: 2026-05-23 (GMT+7).

This note records the *why*. It is the design-rationale charter for MEP-47
and explicitly contrasts the JVM target with the C target (MEP-45) and the
Erlang/BEAM target (MEP-46). All three backends share a frontend (parser
plus type checker), share a correctness gate (byte-equal stdout against
vm3), and share the fixture corpus, but their runtime models, distribution
shapes, and operational profiles are deeply different. This note states the
position MEP-47 takes on each axis.

The TL;DR position:

- **JVM is the right third target after C and BEAM** because it
  complements both: C buys distribution shape (single-file native binary)
  and ceiling performance; BEAM buys fault tolerance, hot reload, and
  distribution-transparent actor semantics; the JVM buys the largest
  third-party-library ecosystem in software history (Maven Central had
  10M+ artefacts in 2024, growing), a battle-hardened generational JIT
  (HotSpot C2 plus Graal), virtual threads as a first-class runtime
  primitive (Project Loom, GA since JDK 21), and the broadest commercial
  deployment footprint of any managed runtime.
- **JDK 21 LTS is the floor; JDK 25 LTS is fully supported.** JDK 21
  (Sept 2023) GA'd virtual threads, sealed classes, records, pattern
  matching for switch, generational ZGC, and sequenced collections.
  Together these are exactly the features Mochi needs to map cleanly:
  sum types to `sealed interface` + `record`, agents to virtual threads,
  match to switch patterns. JDK 25 LTS (Sept 2025) adds the ClassFile API
  GA, structured concurrency GA, and Compact Object Headers. The 21 floor
  is a hard requirement; the 25 ceiling is recommended for new projects.
- **The IR-layer choice is deferred to note 05** (codegen design),
  which surveys Java source via JavaPoet, the stdlib ClassFile API,
  third-party ASM, ByteBuddy, and Kotlin/Scala source emission. The
  recommendation lands in note 05 §10 and is reflected in the spec body
  (MEP-47 §5). Position taken here: we will NOT emit Kotlin or Scala
  source as the IR (extra compiler dependency, locks us to another
  language's evolution). The choice is between Java source and JVM
  bytecode, with a likely hybrid: source for ordinary code, bytecode for
  invokedynamic-driven constructs (closures, sum-type dispatch).
- **Reuse Maven Central wholesale.** The Mochi runtime jar
  (`dev.mochi:runtime`) is a thin shim over the JDK stdlib plus a tiny
  number of vetted dependencies (Jackson for JSON/CSV, snakeyaml-engine
  for YAML, optional Bouncy Castle for crypto). The runtime is published
  to Maven Central; users include one Gradle/Maven coordinate.
- **Three deployment shapes:** uberjar (default, runs on any JDK 21+),
  jlink custom JRE (self-contained directory, no host JDK requirement),
  and GraalVM native-image (single static binary, sub-50ms startup).
  Different users, different tradeoffs; the build driver picks per
  `--target=` flag.
- **Differential testing against vm3 is the master gate.** Same as
  MEP-45 and MEP-46. vm3 is the recording oracle; the JVM artefact's
  stdout must diff byte-for-byte against `expect.txt` for every fixture,
  on JDK 21 and JDK 25, on x86_64-linux and aarch64-darwin at minimum.

## 1. Why the JVM is the right third target

Mochi already has two complementary native-class targets (C and BEAM)
landing through MEP-45 and MEP-46. The JVM completes the deployment
triangle for a fundamentally different reason from either of those: the
JVM ecosystem is the largest the software industry has produced.

Concretely, in 2026:

- Maven Central, the canonical JVM package host, contains more than
  ten million unique artefact versions across millions of distinct
  group/artefact pairs. (Sonatype's annual State of the Software Supply
  Chain reports tracked the count exceeding 10M in 2024; 2026 estimates
  cluster around 15M.)
- The JVM remains the dominant runtime in Fortune 500 backend stacks,
  in fintech (every major investment bank's trading and clearing
  infrastructure includes substantial JVM deployments), and in big-data
  pipelines (Hadoop, Spark, Flink, Kafka are all JVM).
- Android, the dominant mobile OS, runs Mochi-compatible bytecode
  through ART (with D8/R8 lowering JVM bytecode to DEX). This puts
  Mochi within reach of mobile shipping, even if MEP-47 v1 defers the
  Android-specific build target to a sub-MEP (see [[07-jvm-target-portability]]
  §8 and [[12-risks-and-alternatives]]).
- The combination of Loom (virtual threads), Graal (native-image and
  the polyglot story), and Valhalla (value objects, in active preview
  as of 2026) makes the JVM a genuinely modern runtime, not the
  bloated legacy stack it was a decade ago.

For Mochi specifically, the JVM offers the following capabilities that
neither C nor BEAM can match:

- **Maven Central as the FFI substrate.** A Mochi user can import any
  of those ten million libraries with one build-file line. The C target
  needs every C library to be statically linkable and ABI-stable. The
  BEAM target needs NIFs, ports, or a hand-written wrapper. The JVM
  target gets the entire ecosystem nearly for free (with caveats around
  reflection for native-image, addressed in [[10-build-system]] §10).
- **A mature JIT, with two competing implementations.** HotSpot C2 has
  been refined since 1999. GraalVM CE provides an alternative JIT plus
  the native-image AOT compiler. The combination gives Mochi-on-JVM
  performance that hand-tuned C struggles to beat for highly polymorphic
  code paths and that BEAM cannot match for tight numeric loops.
- **Virtual threads as a runtime primitive.** Project Loom shipped
  GA in JDK 21 (JEP 444). A Mochi `agent` lowers to one virtual thread
  plus a mailbox; the JVM scheduler handles M:N multiplexing onto
  carrier OS threads. We get BEAM-class concurrency on a different
  runtime model, without paying for OTP's process-isolated heaps.
- **A first-class FFI for native code.** The Foreign Function and
  Memory API (FFM, GA in JDK 22, refined through 23/24/25) replaces
  JNI and gives Mochi-on-JVM the same C-interop story as MEP-45 at
  source level (call any C library; share memory via MemorySegment).
- **GraalVM native-image** closes the distribution-shape gap with
  MEP-45. A Mochi-on-JVM hello-world built with native-image is a
  ~10MB static binary, comparable to MEP-45's C output, and starts in
  under 50ms.

The MEP-45 C target solves the *distribution shape and ceiling
performance* problem. The MEP-46 BEAM target solves the *fault
tolerance and operational profile* problem. The MEP-47 JVM target
solves the *ecosystem and toolchain reach* problem. They are
complementary; users pick by deployment context, library access
requirements, and team familiarity.

## 2. Why JDK 21 LTS as the floor

JDK 21 (Sept 2023) consolidated the language features Mochi needs:

- **Sealed classes and interfaces** (JEP 409, GA in JDK 17): the
  natural lowering for Mochi sum types. `sealed interface Result<T>
  permits Result.Ok, Result.Err {}`.
- **Records** (JEP 395, GA in JDK 16): the natural lowering for Mochi
  records. Structural equality, hash, toString for free.
- **Pattern matching for switch** (JEP 441, GA in JDK 21): the
  natural lowering for Mochi `match` expressions. With record patterns
  (JEP 440, GA in JDK 21), exhaustiveness checking against sealed
  hierarchies is enforced at compile time.
- **Virtual threads** (JEP 444, GA in JDK 21): the natural lowering
  for Mochi agents and `spawn`. Without Loom, Mochi-on-JVM would have
  to ship a coroutines-style scheduler (Kotlin-coroutines or Quasar
  style) and pay the maintenance cost.
- **Generational ZGC** (JEP 439, GA in JDK 21): low-pause GC suitable
  for long-running Mochi services.
- **Sequenced collections** (JEP 431, GA in JDK 21): clean mapping for
  insertion-ordered `omap` (preview Mochi feature).

JDK 17 LTS (Sept 2021) lacks Loom. Without Loom, the lowering for
agents would either rely on platform threads (~1MB stack each, ~thousands
of agents max) or pull in a coroutines library. Either path adds
material code and runtime cost.

JDK 25 LTS (Sept 2025) is fully supported and is the recommended
runtime for new Mochi-on-JVM projects. JDK 25 adds:

- **ClassFile API GA** (JEP 484, final in JDK 24, GA semantics in 25):
  a stdlib alternative to ASM. We may target this directly for the
  bytecode-emission code paths. See [[05-codegen-design]] §3.7.
- **Structured concurrency GA** (JEP target for 25; verify in
  [[09-agent-streams]]).
- **Compact object headers** (JEP 519 or similar, preview-to-GA path):
  smaller per-object memory, useful for long-running Mochi services
  holding millions of records.

JDK 29 LTS (Sept 2027) is not yet released; MEP-47 commits to forward
compatibility but tests on the developer-preview builds in CI.

The version-matrix policy is documented in [[07-jvm-target-portability]] §1.

## 3. Why a survey-driven codegen IR choice

The BEAM target had a defensible default (Core Erlang via `cerl`).
The C target had a defensible default (custom `aotir` IR plus C
emission). The JVM target has at least eight plausible codegen front
doors, each with substantial real-world precedent:

1. Java source text → `javac` API.
2. Java AST via JavaPoet → source emit → `javac` API.
3. Kotlin source → `kotlinc`. (Reject as default; locks us to Kotlin.)
4. Scala source. (Reject as default; same reason.)
5. JVM bytecode via ASM.
6. JVM bytecode via ByteBuddy (high-level wrapper over ASM).
7. JVM bytecode via the stdlib ClassFile API (JEP 484, GA in JDK 25).
8. JVM bytecode via Janino (in-memory `javac`, niche).
9. A custom IR (`aotir`-style) lowered to ClassFile API.

The choice is non-trivial: it changes which JVM features we can
exercise (invokedynamic for closures and sum-type dispatch), how much
work the existing javac optimisation pipeline does for us, how much
control we keep over class file layout and debugging information, and
whether the result is friendly to GraalVM native-image's closed-world
assumption.

[[05-codegen-design]] surveys all eight, scores them on a decision
matrix, and recommends a hybrid: **Java source via `javax.tools` for
the ordinary lowering, plus ClassFile API for invokedynamic-heavy
constructs (closures, sum-type dispatch).** The hybrid is the only
choice that keeps debuggability (line-number tables come for free
from `javac`), inherits javac's optimisations (escape analysis,
StringConcatFactory bootstraps, lambda metafactory machinery), AND
gives us direct control where we need it (custom invokedynamic
bootstraps for sum-type dispatch are awkward to express in Java
source).

The position taken in note 02 is therefore: we do not pre-commit;
the codegen IR is a defensible engineering choice that note 05
makes with full data. The spec body (MEP-47 §5) cites note 05's
recommendation as normative.

## 4. Why reuse Maven Central wholesale

The Mochi runtime jar (`dev.mochi:runtime`) is a *thin shim*, not a
re-implementation:

| Mochi concept             | JVM construct or library used                                   |
|---------------------------|-----------------------------------------------------------------|
| agent                     | virtual thread + `LinkedBlockingQueue` mailbox                  |
| stream                    | `java.util.concurrent.Flow.Publisher` + `SubmissionPublisher`   |
| supervision               | `Thread.UncaughtExceptionHandler` plus user-defined supervisor  |
| in-memory query state     | `ArrayList` / `HashMap` / `LinkedHashMap` plus Stream API       |
| Datalog fact tables       | `HashMap<Predicate, HashSet<Tuple>>` plus semi-naive runtime    |
| persistent in-memory cfg  | static final fields                                             |
| HTTP client (fetch)       | `java.net.http.HttpClient` (stdlib, virtual-thread aware)       |
| JSON                      | `com.fasterxml.jackson.core` (Jackson)                          |
| YAML                      | `org.snakeyaml:snakeyaml-engine`                                |
| CSV                       | `jackson-dataformat-csv`                                        |
| TLS, crypto               | `javax.net.ssl`, `javax.crypto` (stdlib)                        |
| logging                   | `java.util.logging` or `org.slf4j` facade (option)              |
| telemetry                 | JDK Flight Recorder (`jdk.jfr`) plus OpenTelemetry SDK (opt.)   |
| escript-style packaging   | jar with `Main-Class` plus shaded deps (uberjar)                |
| release packaging         | `jlink` custom JRE                                              |
| single binary             | GraalVM `native-image`                                          |
| FFI to C                  | FFM API (`java.lang.foreign`, GA in JDK 22)                     |
| async I/O                 | `java.nio.channels.*` + virtual threads                         |

The Mochi runtime jar adds, in `dev.mochi.runtime.*` packages:

- `mochi.runtime`: helpers: print formatters, panic, error conversion.
- `mochi.str`: string ops layered on `String` and `StringBuilder`.
- `mochi.list`, `mochi.map`, `mochi.set`: convenience helpers wrapping
  ArrayList / HashMap / HashSet.
- `mochi.query`: Mochi query DSL runtime (group_by, hash_join, sort,
  set ops layered on the Stream API).
- `mochi.stream`: `Stream<T>` wrapper around `Flow.Publisher`.
- `mochi.agent`: virtual-thread agent template, mailbox, intent dispatch.
- `mochi.datalog`: semi-naive evaluator over HashMap-of-tuples.
- `mochi.llm`: provider abstraction over HTTP.
- `mochi.fetch`: HTTP fetch wrapper with JSON decode shim.
- `mochi.ffi`: FFM-backed C interop helpers.
- `mochi.test`: JUnit-compatible expect/test driver.
- `mochi.io`: variadic print, per-type formatter dispatch.

Total LOC target for v1: ~5000 lines of Java. This is between MEP-45
(~15000 lines of C, because C makes us write a GC integration shim,
scheduler, hash table, and fiber library) and MEP-46 (~3000 lines of
Erlang, because OTP provides nearly everything). The JVM lands in the
middle: Loom gives us the scheduler and Maven Central gives us hash
tables and HTTP, but we still write the query runtime, the datalog
evaluator, and the agent dispatch loop.

## 5. Why three deployment shapes

Different users have different shipping needs. MEP-47 supports three:

### 5.1 `mochi build --target=jvm-uberjar` (default)

Produces a single jar containing the user's compiled classes plus all
transitive dependencies, plus a `Main-Class` manifest entry. Runs on
any JDK 21+ via `java -jar app.jar`.

Hello-world size: ~10 MB (mostly Jackson). Realistic-app size: 15-50 MB.
Stripped-mode (`--no-json --no-csv` to drop Jackson): ~2 MB.

Use case: default. Most Mochi programs ship this way. User installs a
JDK separately (analogous to "install Erlang" for MEP-46's escript).

### 5.2 `mochi build --target=jvm-jlink`

Produces a custom JRE directory bundled with the user code. Uses
`jlink` to strip JDK modules the program does not reference. Self-
contained; no host JDK required.

Hello-world size: ~50 MB (custom JRE plus uberjar). Per-arch (jlink
must run on the target architecture; cross-arch via Docker buildx).

Use case: server deployments where "no JDK on the host" is a
requirement; air-gapped environments; containerised services.

### 5.3 `mochi build --target=jvm-native`

Produces a single static native binary via GraalVM `native-image`.
Closed-world AOT compilation. Sub-50ms startup. No JVM warmup.

Hello-world size: ~10 MB (with `-Os`; default ~30 MB). Per-arch.
Reflection must be declared in `reachability-metadata.json` (Mochi
codegen emits this).

Use case: CLI tools, serverless functions, containerised services
where startup time and memory matter, embedded systems where a JVM
is too heavy.

### 5.4 `mochi build --target=jvm-jpackage` (Phase 2)

Produces a platform installer (.deb, .rpm, .dmg, .pkg, .msi). Wraps
jlink output plus a platform launcher. Code-signed and notarised on
macOS / Windows as needed. Useful for desktop GUI apps.

### 5.5 `mochi build --target=jvm-aar` and `--target=jvm-dex` (Phase 2, MEP-47.1)

Android target. Out of scope for v1; covered in
[[07-jvm-target-portability]] §8 and [[12-risks-and-alternatives]].

## 6. Why differential testing against vm3 is the master gate

vm3 is the existing reference implementation. Byte-equal stdout from
the JVM artefact versus vm3, on every fixture, is the strictest
behaviour check available. vm3 is used here only as the recording
oracle for `expect.txt`; the transpiler does not consume any of vm3's
IR, runtime, or codegen. Property tests, fuzzing, and reproducibility
are secondary gates.

This is the same gate MEP-45 and MEP-46 use; sharing the gate means
we share the fixture corpus and the recorded goldens. A change to a
Mochi source file re-records `expect.txt` from vm3 in one pass, and
all three backends are validated against the same byte sequence.

For JVM-specific test infrastructure we add:

- A per-JDK matrix: JDK 21 (LTS floor), JDK 25 (LTS ceiling), and
  the current developer-preview build (warning-only).
- A native-image gate. The fixture corpus must build with
  `native-image --no-fallback` and produce byte-equal stdout. Reflection
  metadata correctness is a load-bearing piece.
- A jlink gate. The fixture corpus must `jlink` into a custom JRE
  that runs producing byte-equal stdout.
- A JUnit pass on the test functions emitted from Mochi `test` blocks.

[[11-testing-gates]] details the gates.

## 7. Why NOT compile to Kotlin or Scala source

A reasonable alternative would be to lower Mochi to Kotlin source and
let `kotlinc` do the rest. Kotlin has clean lambdas, data classes,
sealed classes, and a mature compiler. This was considered and
rejected:

- **Kotlin has its own type system.** Kotlin's null-safety,
  reified-generics-via-inline, suspend-coroutines, and contracts add
  a layer of semantics Mochi does not have. Lowering Mochi to Kotlin
  forces a reconciliation: how do Mochi nullable-free types map to
  Kotlin non-null defaults? How does Mochi `match` map to Kotlin
  `when`? Each mapping is one more failure mode.
- **Kotlin compiler as a build-time dependency.** kotlinc is a heavy
  JAR (~50 MB) and slow (cold compile of a hello-world is multiple
  seconds). Bundling it doubles the Mochi build CLI size; not bundling
  it requires the user to install Kotlin. Either choice is worse than
  the Java-via-`javax.tools` path.
- **Kotlin evolution risk.** Mochi-on-Kotlin would couple us to
  Kotlin's roadmap (K2 compiler, multiplatform pivot, KSP versus
  annotation processors, KMP-vs-KMM rename churn). Java source is
  defined by the JLS, evolves slowly, and is JDK-version-controlled.
- **Distribution shape.** Kotlin's stdlib (`kotlin-stdlib.jar`,
  ~1.5MB) becomes a mandatory transitive dependency of every Mochi-on-
  JVM uberjar. Adds size for no benefit Mochi controls.

Scala is similarly rejected, with the same arguments plus the
additional concern that Scala's compilation model (TASTy IR,
incremental compilation via Zinc, dotty compiler) is even more
complex than Kotlin's.

The position: emit Java source (or bytecode directly, per note 05),
not source in another JVM language. The Java target is the lingua
franca of the JVM and the only one whose evolution is JLS-governed.

## 8. Why JVM is *not* the right primary target

Symmetric to §1: things the JVM cannot do that the C target or BEAM
can.

- **JVM startup cost.** A cold HotSpot JVM takes 100-300ms to start;
  with JIT warmup, a few hundred milliseconds more. CLI tools that
  must respond in <50ms need GraalVM native-image (which is supported)
  or MEP-45's C output (which is faster still).
- **Memory footprint.** A minimal JVM resident set is 50-150 MB.
  Generational ZGC plus compact headers helps but does not eliminate
  the floor. The C target's hello-world is ~3 MB resident; the BEAM
  release is ~30 MB. JVM uberjar with a JRE is ~250 MB total disk.
- **Hot reload.** The JVM has `Instrumentation` plus `Agent`s for
  class redefinition, but the model is heavy (every class must be
  reloadable; method-signature changes require restart). BEAM hot
  reload is first-class and supported by the language. JVM is not
  in the same league.
- **Single-file ship without ANY runtime dependency.** GraalVM
  native-image closes this gap but introduces closed-world
  constraints. The C target's static binary has no constraints.
- **Embedded targets without a JVM.** Most microcontrollers, most
  resource-constrained edge devices, lack a JVM. The C target
  reaches further (newlib, freestanding, wasm32-wasi).
- **WASM target.** The JVM has TeaVM, CheerpJ, and J2CL as JVM-to-
  WASM/JS compilers; in 2026 none of them is at the maturity of
  Emscripten or wasi-sdk for C. The C target ships to wasm cleanly
  today; the JVM target via TeaVM is research-quality.

The three targets are complementary. Users with library-access
requirements pick JVM. Users with operational uptime requirements
pick BEAM. Users with embedded or single-binary requirements pick
C. Many non-trivial Mochi programs will eventually ship two or all
three.

## 9. Why this is not "just transpile to Java"

A common shorthand for the project is "transpile to Java." That's
broadly accurate but obscures three load-bearing design choices:

1. **The IR-layer choice matters** (note 05). Java source is one
   defensible choice; the stdlib ClassFile API is another;
   invokedynamic-heavy paths force a hybrid. Calling MEP-47 "transpile
   to Java" pre-commits to source-only emission, which is wrong for
   sum-type dispatch.
2. **The runtime layer is a Mochi-controlled module**, with about a
   dozen packages in the `dev.mochi.runtime.*` namespace. Without it,
   Mochi's higher-level features (queries, streams, agents) have no
   place to land. See [[04-runtime]].
3. **The build driver owns the ship-format story** (uberjar, jlink,
   native-image, jpackage, Android), the Maven Central publishing
   pipeline, the per-JDK-version matrix, and the reproducibility
   gate. See [[10-build-system]].

"Transpile to Java" without these three is a toy. MEP-47 specifies all
three.

## 10. Position on JVM ecosystem interoperability

The JVM's defining strength is library access. Mochi-on-JVM must make
this strength reachable from Mochi code without sacrificing Mochi's
type-safety guarantees. The position:

- **Mochi can `import "java:com.example.Foo"`** to expose a Java class
  to Mochi. The Mochi type checker reads the class's bytecode (or its
  source if available) and constructs Mochi-typed signatures. Java
  null-bearing return types are converted to Mochi `Result<T>` or
  `Option<T>` at the import boundary, with the user choosing the
  policy in a per-import annotation.
- **A Mochi module compiled to JVM bytecode is callable from Java**
  via standard interop: each Mochi public function becomes a public
  static method on a Java class named after the Mochi module.
- **The build system surfaces both directions.** `mochi build` can
  produce a jar that includes both Mochi-generated classes and any
  Java classes from a `src/main/java/` directory; Mochi can include
  pre-compiled jars on the classpath via `--with-jar`. Defer surface
  details to [[10-build-system]].
- **Annotations.** Mochi exposes a `@java(...)` modifier on functions
  and types that controls how they appear from Java: visibility,
  naming, throwables. Phase 2.

v1 aims for "Java can call Mochi cleanly". v2 aims for "Mochi can
import any Maven dependency". The deeper integration is sequenced
through the 19-phase plan in the spec body.

## 11. Position on GraalVM native-image

GraalVM native-image is the JVM's distribution-shape escape hatch.
Mochi-on-JVM commits to making it a first-class supported target:

- **The fixture corpus must build with `native-image --no-fallback`**
  as a CI gate ([[11-testing-gates]] §3).
- **The Mochi runtime jar ships `reachability-metadata.json`** in
  `META-INF/native-image/dev.mochi/runtime/`, so it works for
  downstream native-image builds without user configuration.
- **Mochi codegen emits per-app reachability metadata** based on
  the program's actual reflection use (only sum-type dispatch and
  Jackson model classes need entries; Mochi has very little
  intrinsic reflection).
- **Build-time vs run-time class init.** Mochi defaults to run-time
  initialisation (avoids static-init traps for `mochi.fetch` and
  `mochi.llm`).

Trade-off: native-image rules out plugin loading via reflection at
runtime. Mochi programs that rely on dynamic class loading (rare;
mostly the LLM provider plugin system) lose this when compiled to
native. The build CLI warns when a feature is incompatible with the
selected target.

## 12. Position on Project Valhalla

Valhalla is the JVM's value-types initiative. As of 2026, JEPs 401
(value classes), 402 (Q-types, removed/reshaped), and the various
follow-ups are in preview, not GA. MEP-47 v1 does NOT depend on
Valhalla:

- Mochi records lower to `record` (reference types), not value
  classes.
- Mochi primitive types (int, float, bool) lower to JVM primitives
  in monomorphised paths and box at generic boundaries (today's
  reality).
- When Valhalla ships GA (likely JDK 27 or 29), MEP-47 v2 may
  emit `value record` declarations for Mochi records that are
  reference-immutable, eliminating one heap allocation per
  instance. This is a future optimisation, not a v1 commitment.

## 13. Position on Project Loom (virtual threads)

Loom is the JVM's killer feature for Mochi-on-JVM, because Mochi's
agent and stream model maps onto virtual threads almost trivially:

- One Mochi agent = one virtual thread plus a mailbox.
- One Mochi stream subscriber = one virtual thread per `on T as x { ... }`
  handler.
- `spawn f(args)` = `Thread.ofVirtual().start(...)`.
- No async/await needed; Mochi blocking calls inside agents are
  carrier-yielding via Loom continuations.

The "no async/await" position is load-bearing. Languages predating
Loom (Kotlin, Rust, JavaScript, C#) had to invent `suspend`/`async`
to escape platform-thread costs. Loom obviates this. Mochi-on-JVM
should not introduce async/await syntax just because the runtime is
the JVM; the runtime is the JVM with Loom, which is a different beast.

The implementation is detailed in [[09-agent-streams]].

Two caveats:

- **Pinning hazards.** Code paths inside the Mochi runtime that use
  `synchronized` blocks pin to carrier (JDK 21 behaviour; JEP 491
  in JDK 25 removes this). The runtime must use
  `java.util.concurrent.locks.ReentrantLock` instead of `synchronized`
  on any path that may run inside a virtual thread.
- **GraalVM native-image and virtual threads.** Loom is supported in
  GraalVM 24+; verify behaviour for each shipping GraalVM version.

## 14. Position on JDK Flight Recorder (JFR)

JFR is a stdlib telemetry framework (open-sourced in JDK 11). Mochi
runtime emits JFR events for:

- Agent spawn / stop / panic.
- Stream subscribe / unsubscribe.
- Query execution (with input cardinality and duration).
- LLM provider calls.
- HTTP fetch with status code and latency.

JFR events are recorded into `.jfr` files and analysable via JDK
Mission Control. Mochi-on-JVM thus gets first-class observability
without depending on OpenTelemetry. (OpenTelemetry is supported as
an opt-in via a separate `mochi-telemetry-otel` runtime module.)

## 15. Summary of position

MEP-47 is a focused, complementary target that:

- Inherits the parser, type checker, and fixture corpus from the
  shared Mochi frontend.
- Targets JDK 21 LTS as the floor and JDK 25 LTS as the supported
  ceiling.
- Defers the codegen IR choice to [[05-codegen-design]], with a
  likely Java-source-plus-ClassFile-API hybrid.
- Reuses Maven Central wholesale, with a thin `dev.mochi.runtime.*`
  jar.
- Ships via uberjar (default), jlink custom JRE, or GraalVM
  native-image, with jpackage and Android targets sequenced through
  later phases.
- Lowers Mochi agents to Loom virtual threads, streams to
  `java.util.concurrent.Flow.Publisher`, and avoids async/await.
- Validates against vm3 byte-equal as the master gate, with JDK
  matrix, native-image, and jlink gates layered on top.
- Treats GraalVM native-image as a first-class shipping target.
- Defers Project Valhalla until it ships GA.
- Does not duplicate MEP-45's embedded story or MEP-46's hot-reload
  story; complements both with ecosystem reach.

The next eleven notes flesh out each axis of this position.
