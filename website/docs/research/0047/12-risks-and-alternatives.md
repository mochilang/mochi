# MEP-47 research note 12, Risks and alternatives for the JVM target

Author: research pass for MEP-47.
Date: 2026-05-23 (GMT+7).

This note catalogues the risks of the design adopted in notes 01-11 and the
alternatives we considered and rejected. It is written so that a future
maintainer can see *why* a choice was made and which trade was accepted.
Mirrors MEP-46 note 12 and MEP-45 note 12.

## 1. Top-level risks

### R1: Loom is young

Project Loom shipped GA in JDK 21 (2023-09-19). JEP 491 fixed
`synchronized` pinning in JDK 24 (2025-03). Structured concurrency
(`StructuredTaskScope`, JEP 505) is still preview in JDK 25, with no
firm GA date. Scoped values became final in JDK 25 (JEP 506).

The Loom-related parts of MEP-47 sit on a moving floor. The user-facing
risk: agents may pin carrier threads silently on JDK 21 if generated
code uses `synchronized`. The mitigation: a CI gate watches for
`jdk.VirtualThreadPinned` events ([[11-testing-gates]] §12), and the
runtime never emits `synchronized` (only `ReentrantLock`).

Secondary risk: structured concurrency API changes between JDK 21 and
its GA, since it has been in preview for many releases. Mitigation: we
do not depend on `StructuredTaskScope` from generated code; the runtime
provides `dev.mochi.runtime.async.MochiScope` that wraps either the
preview API (if available) or a hand-rolled equivalent. This isolates
us from API churn.

Tertiary risk: JEP 491's pinning fix only covers `synchronized`. JNI/FFM
boundaries, class initialisation, and symbolic resolution still pin.
Mitigation: documented in [[09-agent-streams]] §9; the JFR pinning
channel surfaces violations at runtime; perf gates catch regressions.

### R2: GraalVM native-image reachability metadata is brittle

GraalVM AOT compilation requires explicit metadata for every reflective
operation, every dynamic proxy, every serialization class, every
resource bundle. Mochi programs that use the AI/FFI shells (which load
provider classes dynamically) need this metadata.

Risk: a Mochi program that works with `java -jar` fails at runtime under
native-image because the metadata is incomplete.

Mitigation:

- The native-image build automatically runs the `native-image-agent`
  against the fixture suite, accumulating reachability metadata.
- The `META-INF/native-image/` directory is checked into the runtime
  jar so users inherit it.
- A CI gate ([[11-testing-gates]] §7) builds every fixture as a native
  image and runs it; missing metadata fails the gate.
- For users importing arbitrary Maven Central deps that have their own
  metadata gaps: `mochi build --jvm-native` can pull from the
  GraalVM Reachability Metadata Repository (an open-source database
  of metadata for popular libraries, maintained by the GraalVM team).

Risk magnitude: medium. Most pure-Mochi programs work; programs heavy on
FFI to legacy reflective frameworks (Spring, Hibernate) may not. We
document this as a known limitation and recommend the uberjar path for
those programs.

### R3: Maven Central is a supply-chain attack surface

Pulling deps from Maven Central means trusting (a) the network, (b) the
Sonatype OSSRH / new Central Publisher Portal, (c) the dep authors. The
log4shell precedent (CVE-2021-44228) shows that even widely-used JVM
libraries can carry critical vulnerabilities for years.

Mitigation:

- Every Mochi-side dep has a pinned version and a SHA-256 in
  `mochi.lock.json`.
- The build verifies the lockfile before invoking the resolver.
- A `mochi audit` subcommand checks pinned versions against the OSV
  vulnerability database (which mirrors Maven Central CVEs).
- The runtime itself uses a deliberately small dep set (Jackson,
  snakeyaml-engine, bouncycastle), all vendored at a pinned major
  version with documented update policy.
- We reject Spring, Guava, Apache Commons, Netty as runtime deps
  (see [[10-build-system]] §6), specifically because they each have
  large transitive trees we cannot audit.

Risk magnitude: medium. The mitigations cover the known-known
threats; the unknown-known (yet-to-be-found CVEs in a pinned dep) is
inherent in any package-based ecosystem.

### R4: JVM startup time is poor for short-lived programs

Even with class-data sharing (CDS), a fresh JVM takes ~100ms before
`main` runs. For a 10-line Mochi script, that's the entire runtime.

Mitigation:

- The native-image path produces sub-50ms binaries.
- AOT class-data sharing (JEP 483 in JDK 24, JEP 514 in JDK 25, JEP
  515 in JDK 25 for ahead-of-time method profiling) cuts JVM cold
  start to ~50ms.
- For *very* short-lived programs, vm3 (the Go reference) remains the
  recommended target; MEP-47 is positioned for longer-running
  workloads.

Risk magnitude: low. We don't claim JVM beats vm3 on cold start; we
claim JVM beats vm3 on long-running and beats C on FFI breadth.

### R5: GraalVM ownership has shifted

Oracle announced in September 2025 that GraalVM Community Edition for
Java SE would no longer receive new feature work; Oracle is focusing
on the GraalVM Enterprise (now under a separate licence) and on
Project Leyden (the in-JDK AOT effort).

Risk: GraalVM Community may stagnate. The Liberica NIK and Mandrel
forks may diverge.

Mitigation:

- We track Liberica NIK (BellSoft) as the recommended distribution.
- We track Project Leyden's progress (JEP 483 / 514 / 515 are early
  wins); when Leyden subsumes native-image, we pivot.
- The MEP-47 codegen is GraalVM-independent: we emit standard
  bytecode, GraalVM is just one consumer.

Risk magnitude: medium. The pivot path is clear; the cost is
recompiling the documentation and changing build defaults.

### R6: Java records and sealed interfaces are recent

JEP 395 (records) finalised in JDK 16 (2021). JEP 409 (sealed
classes) finalised in JDK 17 (2021). JEP 440 (record patterns) and
JEP 441 (pattern matching for switch) finalised in JDK 21 (2023).

Risk: edge cases in the interaction (e.g., generic record patterns,
sealed interfaces with type parameters) may surface bugs in javac or
in IDE tooling.

Mitigation:

- We test against multiple Temurin builds per JDK version (the
  bugfix releases).
- We hand-curate a workaround table for known javac bugs.
- The Mochi front-end can fall back from record patterns to
  `instanceof` + accessor calls (uglier but stable).

Risk magnitude: low. Records and sealed have been stable for 3+ years
of LTS use.

### R7: Performance regression vs vm3 on dataset code

Mochi's query DSL is a load-bearing feature ([[../0046/02-design-philosophy]] §3
and [[02-design-philosophy]] §1). vm3 has a hand-tuned query engine in
Go; the JVM target uses `java.util.stream.Stream` + collectors.

Risk: a particular shape of query (e.g., big group-by over a billion
rows) runs slower on the JVM than vm3, undermining the "JVM for big
datasets" pitch.

Mitigation:

- The benchmark suite ([[08-dataset-pipeline]] §11 + [[11-testing-gates]] §14)
  measures the same query shapes on vm3 and JVM and publishes both.
- Where Stream is slow, the runtime provides
  hand-tuned helpers (`dev.mochi.runtime.query.HashJoin`,
  `dev.mochi.runtime.query.SortMergeJoin`) and the compiler picks them
  for the right shapes.
- For embarrassingly parallel queries, parallel streams give a
  multi-core boost the Go vm3 cannot match easily.

Risk magnitude: medium-low. We expect parity on most shapes and wins
on parallelisable shapes; isolated regressions go on the perf board.

### R8: Android is half-supported

D8 (the Android dex compiler) handles JDK 17 bytecode. JDK 21 features
like virtual threads do not exist on ART (the Android runtime). Records
desugar; sealed interfaces desugar; pattern matching desugars; virtual
threads do not.

Risk: a Mochi program that works on the JVM uses an agent and silently
falls back to platform threads on Android, with very different
performance characteristics.

Mitigation:

- We do not promise virtual threads on Android. The build emits a
  warning when `--target=android` is combined with an agent-heavy
  program. (See [[07-jvm-target-portability]] §10.)
- Android is a Phase-2 target; the first release of MEP-47 does not
  ship Android support. Sub-MEP MEP-47.1 owns it.
- The runtime has an `AgentRuntime` SPI that can use platform threads
  on platforms without Loom; this isolates the user code from the
  difference.

Risk magnitude: low-but-strategic. Android matters because the user
asked for it; we don't ship it broken.

### R9: Mochi semantics drift between targets

We have three transpilers (C, BEAM, JVM) plus the reference vm3. Each
has subtle areas where the host language pulls semantics one way
(BEAM term ordering, C signed-overflow UB, JVM `==` reference identity).

Risk: a Mochi program returns subtly different results on different
backends, eroding the "Mochi is one language" claim.

Mitigation:

- The `TestBackendEquivalence` gate ([[11-testing-gates]] §15) runs
  every deterministic fixture on every backend and diffs.
- The spec (`docs/spec.md`, [[../0046/02-design-philosophy]] §3) is
  normative; backend behaviour is checked against it.
- Each backend's note 06 (type lowering) documents the host-language
  edge cases it must work around.

Risk magnitude: ongoing. This is what the gates exist for.

### R10: Codegen IR layer adds maintenance burden

Note 05 chose a hybrid Java source + ClassFile API IR. That's two
emitters, each of which has to track Mochi feature growth.

Risk: features added to the language need to land in both emitters
before they ship.

Mitigation:

- The default emitter is Java source. The ClassFile API path is
  reserved for hot lowerings where source is too verbose or too slow.
- A feature lands in source first; the ClassFile API version is
  performance-only and lags.
- The Mochi build flag `--jvm-emit=source` forces the source path,
  useful for debugging.

Risk magnitude: low. We have a clear primary/secondary distinction.

## 2. Alternatives considered

### A1: Target Kotlin source instead of Java source

We considered emitting Kotlin (a more expressive JVM language with
data classes, sealed classes, coroutines). Rejected for these reasons:

- **Toolchain dependency**: Kotlin requires the Kotlin compiler, which
  pins on a specific JDK and has its own release cadence
  (compatibility table at https://kotlinlang.org/docs/compiler-reference.html).
  This is one more moving piece.
- **Output predictability**: Kotlin's compiler output uses
  Kotlin-specific runtime classes (`kotlin.collections.*`,
  `kotlin.Function*`) which we'd have to ship.
- **Debugging**: a stack trace from a Mochi program going through
  Kotlin going through bytecode is three layers deep.
- **No semantic gain**: Mochi already has the features we'd want from
  Kotlin (data classes ⇒ records; sealed classes ⇒ sealed
  interfaces; coroutines ⇒ Loom virtual threads). Emitting Kotlin
  would be a translation step that loses nothing but adds complexity.

Decision: emit Java source directly. See [[02-design-philosophy]] §9.

### A2: Target Scala source

Same arguments as Kotlin plus: Scala 3's compiler is slower, the macros
ecosystem is large but irrelevant to us, the runtime jar (`scala-library`)
is bigger than ours. Scala 2 is on long-term maintenance only. Rejected.

### A3: Emit bytecode directly (ASM-only, no Java source)

We considered using ASM (the long-standing bytecode manipulation
library) as the sole IR.

Pros:

- Skips javac entirely.
- Smaller compiler binary (no embedded javac).
- Direct control over emitted code.

Cons:

- We must implement type checking ourselves (javac does it for free
  when we emit source).
- Debugging emitted code is harder (no .java to inspect; stack traces
  point to synthetic line numbers).
- ASM has its own version cadence and JDK compatibility table.
- Java records, sealed interfaces, pattern matching all require
  non-trivial bytecode shapes; getting them wrong is easy.

Decision: hybrid. Use Java source as primary (because javac is robust
and free with every JDK); use ClassFile API (the new in-JDK successor
to ASM, JEP 484) for the few hot lowerings that benefit from direct
emission. See [[05-codegen-design]] §1.

### A4: GraalVM polyglot embedding

We considered embedding Mochi inside GraalVM's Truffle framework
(`com.oracle.truffle.api`), which would have given us interop with
JavaScript, Python, Ruby, R running on GraalVM.

Pros:

- Free FFI to other GraalVM languages.
- AST-based execution, no codegen step.
- Truffle's partial-evaluation framework gives us a JIT for free.

Cons:

- We'd need a Truffle-shaped AST representation, which is a different
  IR than what vm3 and the other transpilers use.
- Truffle's API surface is large and Oracle-specific.
- Truffle programs only run on GraalVM JDK, not stock OpenJDK.
- The "polyglot embedding" use case (calling JS from Mochi) is not a
  user need that has come up.

Decision: rejected. Mochi targets stock OpenJDK first; GraalVM is one
consumer (for native-image), not the platform. See
[[02-design-philosophy]] §10.

### A5: Java bytecode interpreter (no compile, just interp)

We considered shipping a bytecode interpreter on the JVM, similar to
vm3 but written in Java.

Decision: rejected. The whole point of targeting the JVM is to use
javac/HotSpot/native-image. A new interpreter would discard that.

### A6: Use existing JVM language as IR (Clojure, Groovy)

Same logic as Kotlin/Scala. Each adds a layer.

### A7: WASM-on-JVM (TeaVM, JWebAssembly)

If we had a WASM target (which we don't have yet), we could compile
Mochi → WASM → JVM via something like TeaVM. Layering Mochi → WASM
adds a step; layering WASM → JVM adds another. Each step is a
compatibility surface. Rejected for v1.

### A8: ByteBuddy or JavaPoet (instead of ClassFile API)

ByteBuddy is a popular runtime bytecode-generation library. JavaPoet is
a source-emission library. Both are 3rd-party deps with their own
release cadences.

Decision: rejected in favour of the JDK's built-in tools.
`javax.tools.JavaCompiler` (since Java 6) for source emission. ClassFile
API (JEP 484, in JDK 24+) for bytecode. Both are stdlib; zero external
dep. ASM is the fallback for JDK 21 (where ClassFile API doesn't exist
yet). See [[05-codegen-design]] §1.

### A9: Persistent collections from Vavr / Eclipse Collections / Capsule

We considered using a well-known library (Vavr, Eclipse Collections,
or Capsule's CHAMP) for the immutable persistent collections that
back Mochi `list` / `map` / `set` semantics.

Pros:

- Battle-tested implementations.
- Maintained by experts.

Cons:

- Adds a 1-3 MB jar to the runtime.
- Pins us to that library's version cadence.
- API surfaces ours; we'd have to wrap.

Decision: ship our own minimal persistent collection layer (HAMT-based
`PersistentMap` / `PersistentSet`, RRB-tree-based `PersistentList`).
The JDK 21 sequenced-collection interfaces (JEP 431) give us the API
shape; the implementation is ~2000 LOC and stays maintainable. See
[[06-type-lowering]] §11.

This is the same conclusion MEP-46 reached for BEAM (no external
library; lean on stdlib `lists`/`maps`/`array`) and MEP-45 reached for
C (hand-rolled). Three backends, three runtimes, all minimal-dep.

### A10: Akka / Pekko for agents

Akka was the canonical JVM actor framework for a decade. Lightbend
relicensed it commercially in 2022; the Apache Foundation forked it as
Pekko.

Decision: rejected. With Loom, an agent is one virtual thread plus one
mailbox; we don't need an actor framework. Akka/Pekko bring scheduling,
clustering, persistence, supervision, all of which Mochi already
specifies independently. See [[09-agent-streams]] §16.

### A11: Reactor / RxJava for streams

Project Reactor (the runtime under Spring WebFlux) and RxJava are the
two dominant JVM reactive-streams libraries. Both implement the
Reactive Streams spec (which `java.util.concurrent.Flow` also
implements, since JDK 9).

Decision: rejected. Mochi's `stream<T>` lowers to JDK `Flow.Publisher`,
which is the de-facto interop ABI between reactive libraries. Users
who want Reactor / RxJava in their pipeline can convert via the
adapter; we don't ship them as defaults. See [[09-agent-streams]] §16.

### A12: Spring Boot integration

A "Mochi-on-Spring-Boot" mode would target Spring's dependency-injection
container.

Decision: rejected for v1. Spring is heavyweight (~30MB), opinionated,
and pulls in dozens of transitive deps. Mochi values are immutable and
explicitly constructed, so DI is a poor fit. Helidon SE, Quarkus, or
Vert.x are lighter-weight options if users want a framework; we let
users wire those up via FFI.

## 3. Dependencies on external decisions

These items are tracked because they're outside Mochi's control but
affect MEP-47.

### D1: JDK 26 / 27 / 28 cadence

OpenJDK releases every 6 months. Each release potentially deprecates
APIs we use, adds features we want, or breaks something. We track
each EA cycle:

- JDK 26 EA: smoke-test gate runs starting at EA1.
- JDK 27 EA: same.
- JDK 28 EA (the next LTS, expected 2027-09): full matrix add when EA
  stabilises.

### D2: GraalVM Community future

If GraalVM CE stagnates (per R5), we pivot to Project Leyden inside
the JDK. JEP 483 (AOT class loading, JDK 24), JEP 514 (AOT command-line
ergonomics, JDK 25), JEP 515 (AOT method profiling, JDK 25) are the
early steps; we monitor JEP 483-followers for AOT compilation in JDK
26+.

### D3: Maven Central transition

Sonatype OSSRH sunset 2025-06-30 in favour of the Central Publisher
Portal. We must publish to the new portal; we test the publish flow
in CI ([[11-testing-gates]] §10).

### D4: Loom evolution

JEP 491 fixed `synchronized` pinning in JDK 24. JEP 505 (structured
concurrency) is still preview in JDK 25. We track each release for
Loom-related changes.

### D5: Reactive Streams TCK

The Reactive Streams TCK validates our `Flow.Publisher` implementations.
We run it as a CI gate; failures here would indicate spec
non-conformance.

## 4. Risk register summary

| Risk | Severity | Likelihood | Mitigation strength |
|------|----------|-----------|---------------------|
| R1 (Loom young) | medium | medium | strong (CI gate) |
| R2 (native-image metadata) | medium | high | medium (auto-gen) |
| R3 (Maven supply chain) | high | low-medium | strong (pin+audit) |
| R4 (JVM startup) | low | high | strong (native-image) |
| R5 (GraalVM ownership) | medium | medium | medium (pivot path) |
| R6 (records/sealed bugs) | low | low | strong (Temurin matrix) |
| R7 (perf vs vm3 on queries) | medium | medium | medium (per-shape bench) |
| R8 (Android partial) | low | medium | medium (Phase-2 sub-MEP) |
| R9 (semantics drift) | high | medium | strong (equivalence gate) |
| R10 (two emitters) | low | low | strong (primary/secondary) |

Highest leverage: R3 (Maven), R9 (semantic drift). Both are addressed
by gating policy ([[11-testing-gates]]) and by spec discipline.

## 5. What we explicitly defer to a future MEP

- **Mochi on Android with virtual threads**: blocked on ART
  implementing Loom (not on Google's roadmap as of 2026-05). Tracked
  as MEP-47.1 sub-MEP for limited Android via platform threads.
- **Mochi on iOS via JVM**: GraalVM native-image targets iOS as of
  GraalVM 25, but the toolchain is alpha. Tracked as MEP-47.2.
- **WASM target**: separate MEP (MEP-48 placeholder). Possibly
  consumes JVM bytecode via TeaVM, or directly via the new wasm-jvm
  spec.
- **Mochi compile-time reflection / macros**: a language-level
  feature, not a backend feature. Tracked at the language MEP layer.
- **Project Valhalla value types**: when Valhalla GA's (JEP 401 etc.),
  Mochi records can become value classes and the runtime can skip
  heap allocation. Tracked as MEP-47.3.

## 6. Cross-references

- [[02-design-philosophy]] - the decisions whose risk we catalog here.
- [[05-codegen-design]] - IR choices (A3, A8) live here.
- [[09-agent-streams]] - Loom risks (R1) live here.
- [[10-build-system]] - Maven supply-chain mitigations (R3) live here.
- [[07-jvm-target-portability]] - GraalVM / JDK matrix (R5, R6) lives here.
- [[11-testing-gates]] - the gates that mitigate everything above.
- [[../0046/12-risks-and-alternatives]] - the BEAM-target analogue.
- [[../0045/12-risks-and-alternatives]] - the C-target analogue.

This note is the risk register. Updates require a paired update to
the matching §11 "Risks and alternatives considered" section of
`mep-0047.md`.
