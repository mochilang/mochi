# MEP-47 research note 07, JVM target portability and version matrix

Author: research pass for MEP-47.
Date: 2026-05-23 01:30 (GMT+7).

This note pins down the JDK version policy, OpenJDK distribution stance, CPU/OS matrix, GraalVM native-image plan, jlink and jpackage shipping modes, Android (D8/R8/ART) compatibility profile, reproducibility, and CI matrix for Mochi-on-JVM. Companion notes: [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The JVM target sits on top of the OpenJDK class-file format and the HotSpot runtime that ships with it. Unlike BEAM (see the sibling MEP-46 note), the JVM has more than one supported runtime vendor and more than one execution model (HotSpot, GraalVM native-image, Android ART). The portability story therefore needs three independent axes: language version (JEP-defined feature set), runtime (which JVM), and platform (OS plus arch).

---

## 1. JDK release cadence and our position

Since JDK 9 (September 2017), OpenJDK has shipped a feature release every six months, in March and September. Starting with JDK 17, Oracle moved the LTS cadence to two years (previously three). The schedule that matters for us:

| Version | GA            | Status            | Mochi support       |
|---------|---------------|-------------------|---------------------|
| 21      | Sept 2023     | LTS               | minimum floor       |
| 22      | March 2024    | Non-LTS, EOL      | CI smoke, not gating|
| 23      | Sept 2024     | Non-LTS, EOL      | CI smoke, not gating|
| 24      | March 2025    | Non-LTS, EOL      | CI smoke, not gating|
| 25      | Sept 2025     | LTS               | fully supported     |
| 26      | March 2026    | Non-LTS           | CI smoke, not gating|
| 27      | Sept 2026     | Non-LTS (planned) | CI smoke when GA    |
| 28      | March 2027    | Non-LTS (planned) | CI smoke when GA    |
| 29      | Sept 2027     | LTS (planned)     | forward-compat ready|

JDK 21 is our **floor**. Mochi-emitted bytecode targets `--release 21`. The runtime jar (`mochi-runtime.jar`) is compiled with `javac --release 21` and is forward-compatible to every later JVM, since OpenJDK only removes deprecated APIs across LTS boundaries with multi-release deprecation cycles.

JDK 25 is **fully supported**: every gate runs against it, and we exercise features that only became GA in 25 (compact source files JEP 512 for `mochi run` inline mode, scoped values JEP 506 for request-scoped context, generational Shenandoah JEP 521 as the optional low-pause GC choice; see [[09-agent-streams]]). We do **not** emit bytecode that requires 25, since that would lock out 21 users.

JDK 29 is **forward-compat-ready**: when 29 ships in September 2027, we add it to the gate matrix and re-spec any code paths that 29's feature set (Valhalla value classes, Leyden AOT preview, etc.) lets us simplify.

Non-LTS versions (22, 23, 24, 26, 27, 28) get a nightly smoke run in CI but no support guarantee. A regression that affects only a non-LTS JDK is logged as a known issue and not a release blocker.

The reasoning for the 21 floor specifically:

- Virtual threads (JEP 444) are GA, which `mochi run --threads=virtual` relies on (see [[09-agent-streams]]).
- Pattern matching for switch (JEP 441) is GA, which our codegen leans on for ADT lowering (see [[06-type-lowering]] §4).
- Record patterns (JEP 440) GA, also for ADT lowering.
- Sealed classes (JEP 409, GA since 17) plus records (JEP 395, GA since 16) give us the algebraic surface we need without external libraries.
- `Files.lines`, `Stream`, `Collectors.toUnmodifiableList`, `Map.entry` are all GA and stable.

JDK 17 would also have most of this, but lacks virtual threads and record patterns, so we picked 21 instead and avoid the temptation to lower the floor.

## 2. OpenJDK distributions

OpenJDK is a single upstream codebase; the binaries that users actually run come from at least eight vendors. All major vendors ship TCK-verified builds under GPLv2 with the Classpath Exception. We care about which ones we **certify** (CI runs every gate, we promise it works) versus which we **acknowledge** (we know users run it; smoke tested but not gating).

### 2.1 Certified

- **Eclipse Temurin** (Adoptium project). Default recommendation. Vendor-neutral governance under the Eclipse Foundation. Wide arch coverage including riscv64 since JDK 17, plus an Alpine/musl variant. We pin our base Docker image to `eclipse-temurin:21-jdk-alpine` and `eclipse-temurin:25-jdk-alpine`.
- **Oracle OpenJDK / Oracle JDK 25 under NFTC**. The reference build. Oracle's Java SE 25 under the Oracle No-Fee Terms and Conditions is free for production use until September 2028 and we test against it in CI to catch divergences between the reference and Temurin.

### 2.2 Acknowledged (tested via CI, not blocking)

- **Amazon Corretto**: LTS-only builds (8, 11, 17, 21, 25). Heavy real-world soak inside AWS. We run a Corretto-21 smoke job because many users on AWS use it.
- **Azul Zulu** (community) and **Azul Zulu Prime** (commercial; Coordinated Restore at Checkpoint, CRaC). CRaC support is interesting for Mochi process-restart scenarios (see §9.3) but we do not depend on it.
- **BellSoft Liberica** (full, standard, lite, plus the Native Image Kit bundle). Spring Framework's recommended JDK. Native Image Kit ships a GraalVM-equivalent that we may use as an alternate native-image builder.
- **Microsoft Build of OpenJDK**: Linux, Windows, macOS for LTS only. Smoke tested.
- **SapMachine**: SAP's build, notable for ppc64le and s390x support.
- **Red Hat OpenJDK**: shipped with RHEL and OpenShift. Mostly equivalent to Temurin on Linux x86_64/aarch64.
- **IBM Semeru / Eclipse OpenJ9**: alternative JVM (not HotSpot). We do **not** test against OpenJ9 because its JIT and GC differ enough that some performance assumptions break; users on OpenJ9 are on their own. Code correctness should hold since `.class` files are vendor-independent.

### 2.3 Recommendation

Certify Temurin 21 + Temurin 25. Document Oracle, Corretto, Zulu, Liberica, Microsoft, SapMachine, Red Hat as "tested via CI smoke, not gating". Document Semeru/OpenJ9 as "best-effort, no JIT-specific tuning".

This mirrors the BEAM matrix policy from [[../0046/07-erlang-target-portability]] where one vendor (Erlang Solutions) is certified and the rest are acknowledged.

## 3. Architecture matrix

| Architecture        | HotSpot | native-image | Tier                                  |
|---------------------|---------|--------------|---------------------------------------|
| x86_64 (Linux)      | yes     | yes          | Tier 1, gating                        |
| x86_64 (macOS)      | yes     | yes          | Tier 2, CI but not gating             |
| x86_64 (Windows)    | yes     | yes          | Tier 1, gating                        |
| aarch64 (Linux)     | yes     | yes          | Tier 1, gating                        |
| aarch64 (macOS)     | yes     | yes          | Tier 1, gating (Apple Silicon)        |
| aarch64 (Windows)   | yes     | partial      | Tier 2, CI but not gating             |
| riscv64 (Linux)     | yes     | no           | Tier 3, best-effort                   |
| ppc64le (Linux)     | yes     | no           | Tier 3, best-effort (SapMachine, RHEL)|
| s390x (Linux)       | yes     | no           | Tier 3, best-effort                   |
| 32-bit x86          | no      | no           | unsupported (JEP 503 removed)         |
| 32-bit ARM          | no      | no           | unsupported                           |

aarch64 reached parity with x86_64 for HotSpot in JDK 17 and for native-image in GraalVM 22.3. We treat them as equals.

riscv64 has shipped in Temurin since the 21 release cycle on a best-effort basis. The HotSpot JIT works but is less optimised than x86_64. Mochi-emitted `.class` files run identically. We do not promise performance parity on riscv64. native-image for riscv64 is still upstream-experimental as of GraalVM for JDK 25 and Temurin does not ship a riscv64 native-image; users on riscv64 must use HotSpot or jlink.

ppc64le and s390x matter for enterprise (RHEL, IBM Z). SapMachine and Red Hat OpenJDK ship them. We confirm correctness via a manual quarterly run; no nightly CI.

32-bit x86 was removed by JEP 503 in JDK 25. Starting with JDK 25 there are no 32-bit OpenJDK binaries. We follow upstream and drop 32-bit silently. 32-bit ARM follows the same logic.

## 4. OS matrix

| OS                  | Notes                                                                  |
|---------------------|------------------------------------------------------------------------|
| Linux glibc         | Tier 1. Ubuntu 22.04/24.04 LTS, Debian 12, RHEL 9/10, AlmaLinux, Rocky.|
| Linux musl (Alpine) | Tier 1 for Docker shipping. Temurin ships musl tarballs since 21.      |
| macOS aarch64       | Tier 1. macOS 13 Ventura and later.                                    |
| macOS x86_64        | Tier 2. macOS 13 Ventura and later. Phasing out as Apple Silicon takes over.|
| Windows x86_64      | Tier 1. Windows Server 2019/2022/2025, Windows 10/11.                  |
| Windows aarch64     | Tier 2. Windows on ARM, Snapdragon X laptops; Temurin ships aarch64 Windows builds.|
| Android             | Separate target; see §8. Deferred to MEP-47.1 sub-MEP.                 |
| FreeBSD             | Tier 3. Community OpenJDK port; we acknowledge users exist.            |
| Illumos/SmartOS     | Tier 3. Community.                                                     |

The Alpine/musl tier matters because most Mochi-on-JVM users will ship inside a Docker image and Alpine's musl base is the de-facto small-image standard. Temurin ships an Alpine-friendly tarball; we use it for the `mochilang/mochi:jvm-jdk21-alpine` and `mochilang/mochi:jvm-jdk25-alpine` images, and the `:jvm-jdk21-glibc` / `:jvm-jdk25-glibc` images for users who need glibc.

Notarisation on macOS is required for any `jpackage` `.app` or `.dmg` shipped to end users; see §7. Windows code signing is required for `.msi` and `.exe` installers; see §7 for the JDK 21+ signtool regression.

BSDs use the community OpenJDK port (https://www.bsdjava.org/). We do not ship BSD binaries; users build from source.

## 5. GraalVM native-image

GraalVM compiles a Mochi program plus its transitive dependencies into a single AOT binary. This is the path for users who want one-file distribution with sub-50ms startup.

### 5.1 Version policy

- GraalVM Community Edition for JDK 21 and JDK 25: free, GPL-licensed, primary target.
- Oracle GraalVM for JDK 21 and JDK 25: free under the GraalVM Free Terms and Conditions (GFTC) since 2023; we test against it to catch divergences but do not require it.

We support GraalVM 21+ and 25+. We do not support GraalVM 17 (too old for our language-feature floor) or pre-25 releases of Oracle GraalVM that predate GFTC.

### 5.2 Closed-world assumption and reachability metadata

native-image uses a closed-world assumption: every class, method, and field that the binary will use at runtime must be reachable at build time. Reflection, JNI, resource loading, and dynamic proxies that the build cannot statically prove are reachable must be declared in `reachability-metadata.json` (formerly `reflect-config.json`, `resource-config.json`, etc.; consolidated as of GraalVM for JDK 21).

Mochi's codegen must emit this metadata. Concrete cases that require entries:

- Reflective JSON serialization (`mochi to_json` / `mochi from_json` if implemented via Jackson): every Mochi record and ADT variant must register all fields as accessed. We have a custom emitter that walks the typed IR (see [[05-codegen-design]]) and produces a metadata JSON alongside the jar.
- Resource files (`mochi read_file` with a static path that lives inside the jar): the path goes into the resources block.
- `Class.forName` in user FFI: Mochi user code cannot do this directly, but FFI bindings to Java libraries may. The FFI declaration syntax (see [[01-language-surface]] §10) requires reachability info.
- Sealed-class permits: GraalVM resolves these statically, no manual entry needed.

The Mochi build emits `META-INF/native-image/dev.mochilang/mochi-app/reachability-metadata.json` inside the user jar, following the GraalVM convention for library-provided metadata. native-image picks it up automatically.

For third-party Java libraries, Mochi's FFI integration looks up the GraalVM Reachability Metadata Repository (https://github.com/oracle/graalvm-reachability-metadata) at build time. If a library is in the repo, we use the upstream metadata; if not, the build fails with a hint to either add the library to the repo or write local metadata.

### 5.3 Polyglot embeddings

Out of scope for MEP-47. GraalVM polyglot lets you embed JavaScript, Python, etc. inside a Java program. Mochi does not expose this. If a future MEP wants Mochi-from-Java or Java-from-Mochi via polyglot, it goes in a separate MEP.

### 5.4 Build matrix

native-image is **not** a cross-compiler. The build host arch and OS must match the target. Our CI matrix:

| Build host          | Output                                  |
|---------------------|-----------------------------------------|
| Linux x86_64 glibc  | linux-x86_64 binary (glibc-dynamic)     |
| Linux x86_64 musl   | linux-x86_64 binary (musl-static)       |
| Linux aarch64 glibc | linux-aarch64 binary (glibc-dynamic)    |
| Linux aarch64 musl  | linux-aarch64 binary (musl-static)      |
| macOS aarch64       | macOS-aarch64 binary                    |
| macOS x86_64        | macOS-x86_64 binary                     |
| Windows x86_64      | Windows-x86_64 binary                   |

To produce cross-arch outputs in a single CI job, we use Docker buildx with QEMU emulation. This works but is 10x slower than native; we use it for release builds only, never for PR-blocking CI.

GraalVM does not ship a native-image for Windows on ARM64 as of GraalVM for JDK 25; users targeting Windows ARM64 must use HotSpot + jlink. We document this gap.

### 5.5 Size and startup expectations

A Mochi hello-world (`println("hello")`) native-image binary should land:

- Linux x86_64 dynamic-glibc: ~8 MB, ~20 ms cold start.
- Linux x86_64 static-musl: ~10 MB (musl-libc included), ~20 ms cold start.
- Linux aarch64: ~8 MB, ~25 ms.
- macOS aarch64: ~9 MB, ~20 ms.
- Windows x86_64: ~12 MB, ~30 ms.

We publish these as **expected** sizes; the gate fails if hello-world grows past 15 MB or starts slower than 50 ms.

### 5.6 Linking modes

GraalVM offers three linking modes:

- **Dynamic** (default): links against the host's glibc and other system libraries. Smallest binary, but glibc version must match the deployment host.
- **Mostly-static** (`--static-nolibc`): all GraalVM/JDK libs static-linked, libc dynamic. Good for distroless containers.
- **Fully-static with musl** (`--static --libc=musl`): everything statically linked against musl libc. Runs on `FROM scratch` containers. Recommended for our Alpine-shipping mode.

The Mochi build emits all three for tier-1 arches in release mode. Users pick which to ship.

## 6. jlink, custom JRE

For users who prefer HotSpot over native-image (better peak throughput, dynamic class loading, easier profiling, hot-swap during development), Mochi's default "no-native-image" shipping mode bundles a stripped JRE produced by `jlink`.

### 6.1 What jlink does

`jlink` walks the module graph rooted at a set of `--add-modules` (Mochi's runtime declares its required JDK modules: `java.base`, `java.logging`, `java.net.http`, `jdk.crypto.ec`, etc.) and produces a custom runtime image containing only those modules. A typical Mochi-app jlink output is ~50 MB versus the full ~200 MB JDK.

### 6.2 Modular jar requirement

Mochi must emit a **modular** jar (with `module-info.class`) for jlink to consume it. Our codegen always emits `module-info.class` declaring `module dev.mochilang.app` with `requires` clauses for every Mochi-runtime module and every Java FFI dependency. See [[10-build-system]] §4 for the build-side details.

### 6.3 JEP 493 (run-time images without JMODs)

JEP 493 lands in JDK 24 and is enabled in Temurin 24+. It lets jlink work without needing the JMOD files (which add ~25% to JDK size). For Temurin 25, jlink builds custom runtimes by extracting from the run-time image itself; the user-facing experience is identical.

One JEP 493 caveat: `jlink --add-modules ALL-MODULE-PATH` no longer implicitly includes JDK modules; you must specify `--module-path` explicitly. Mochi's build driver always passes an explicit module path, so this does not affect us.

Cross-arch jlink is **not** supported by JEP 493 (you cannot build a Windows-x86_64 runtime from a Linux-aarch64 host). For cross-arch, users either install the upstream JMODs for the target arch (Adoptium ships them as a separate download) or use a per-arch CI runner.

### 6.4 Per-arch build

Like native-image, jlink is per-arch. The Mochi release matrix runs jlink on each tier-1 target arch (Linux x86_64, Linux aarch64, macOS aarch64, Windows x86_64) and bundles the resulting runtime.

### 6.5 jlink + Mochi = default no-native-image shipping mode

When the user runs `mochi build --target=jvm-release`, the default output is:

```
my-app/
  bin/my-app       # platform launcher script
  lib/my-app.jar   # Mochi-emitted user code + Mochi runtime
  runtime/         # jlink-built custom JRE, ~50 MB
```

Users can also produce a fat jar (`mochi build --target=jvm-uberjar`) that requires a system-installed JRE, but the jlink path is the default because it gives a self-contained deliverable without requiring the user to install Java.

## 7. jpackage, native installers

`jpackage` (GA in JDK 16, JEP 392) wraps a jlink output plus a platform launcher into an OS-native installer.

| Platform   | Installer types                           |
|------------|-------------------------------------------|
| Linux      | `.deb`, `.rpm`, `app-image` (directory)   |
| macOS      | `.pkg`, `.dmg`, `.app` bundle             |
| Windows    | `.msi`, `.exe`                            |

Mochi exposes this as `mochi build --target=jvm-installer --installer-type=deb` (or `dmg`, `msi`, etc.).

### 7.1 macOS notarisation

For end-user distribution on macOS, the `.app` or `.dmg` must be code-signed with a "Developer ID Application" certificate (not "Apple Development" or "Mac Development") and then notarised via Apple's notary service. The Mochi build accepts `--macos-signing-identity` and `--macos-notarize` flags and shells out to `jpackage --sign` and `xcrun notarytool`.

There is a recurring Temurin-specific notarisation issue (Adoptium issue #829): Temurin 19/20 binaries occasionally fail notarisation due to missing secure timestamps. For Mochi 0.10 we recommend Temurin 21+ where the issue is resolved. We document a workaround using Oracle JDK if a user hits this on a specific Temurin patch level.

### 7.2 Windows code signing

For `.msi` and `.exe` installers, Windows code signing via signtool is required for end-user distribution (SmartScreen otherwise warns the user). There is a JDK 21+ regression in the linker version used by Temurin that breaks signtool on the `.exe` installer wrapper; the fix is in OpenJDK PR #23732, backported in JDK 21.0.6+ and 25.0.0+. We document this and pin Temurin 21.0.6+ as the minimum for Windows installer shipping.

### 7.3 Linux installer

Linux `.deb` and `.rpm` are unsigned by default. We expose `--linux-sign` for users who want to sign for their own apt/yum repository.

## 8. Android (D8, R8, ART)

Android is a substantially different target from server-side JVM. It uses Dalvik Executable (DEX) bytecode instead of JVM `.class` files, the Android Runtime (ART) instead of HotSpot, and a different stdlib surface. Mochi-on-Android is a Phase-2 secondary target gated separately.

### 8.1 The Android toolchain

- **D8**: the dex compiler; replaces the older `dx`. Reads `.class` files and emits `.dex`.
- **R8**: shrinker, optimiser, and obfuscator; replaces ProGuard. Built on top of D8.
- **ART**: the runtime; replaced Dalvik in Android 5 (2014). Since Android 7 (2016), ART is a hybrid JIT + AOT (dex2oat) runtime.
- **Android Gradle Plugin (AGP)**: the build system layer that orchestrates D8/R8/aapt/etc. AGP 8.0 (April 2023) requires JDK 17 as the build-host JDK.

### 8.2 Java language level on Android

Android source code today is most often Kotlin, but Java is fully supported. The Java language level ART understands is roughly JDK 11 baseline, extended via D8 library desugaring (via the `desugar_jdk_libs` library) to cover many JDK 17/21 features and APIs.

What desugaring covers:

- Most JDK 8 APIs (`Stream`, `Optional`, `LocalDateTime`, `CompletableFuture`) backported to API level 21.
- JDK 11 APIs since AGP 7.4 (subset of `java.nio.file`, `String.repeat`, etc.).
- Records (JDK 14): D8 handles records by lowering them to plain classes with synthetic accessors.
- Sealed classes (JDK 17): D8 supports them since AGP 8.1.
- Pattern matching for switch (JDK 21): D8 compiles to chained instanceof + cast.
- Text blocks (JDK 15): pure compile-time syntax, D8 handles trivially.

What desugaring does **not** cover:

- **Virtual threads (JEP 444)**: Project Loom is a HotSpot feature. ART does not implement it. There is no library shim; Mochi-on-Android must fall back to `CompletableFuture` on platform threads, or to Kotlin coroutines via interop. This is the biggest semantic difference between Mochi-on-JVM and Mochi-on-Android.
- **Structured concurrency (JEP 505)**: depends on Loom plumbing. Not available on Android.
- **Scoped values (JEP 506)**: not available; Mochi on Android falls back to `ThreadLocal`.
- **Foreign function and memory API (JEP 454)**: not available; Mochi FFI on Android uses JNI directly.

### 8.3 Multidex and method-count limit

A single `.dex` file holds at most 65,536 methods. Real Mochi apps exceed this, requiring multidex (multiple `.dex` files). Multidex is on by default in AGP 8 when `minSdkVersion >= 21`. Mochi-on-Android requires `minSdkVersion = 24` (Android 7) to align with the JDK 11 language baseline assumed by desugar, so multidex is implicit and Mochi does not need to configure it.

### 8.4 API level matrix

| Android version | API level | Mochi-on-Android |
|-----------------|-----------|------------------|
| 7 (Nougat)      | 24        | minimum target   |
| 8 (Oreo)        | 26-27     | supported        |
| 9 (Pie)         | 28        | supported        |
| 10              | 29        | supported        |
| 11              | 30        | supported        |
| 12-12L          | 31-32     | supported        |
| 13              | 33        | supported        |
| 14              | 34        | supported (partial Java 17 APIs)|
| 15              | 35        | supported (more Java 17 APIs)|
| 16              | 36        | supported        |

`minSdkVersion = 24` gives us virtual-thread-free concurrency, multidex by default, and most desugar coverage. Users targeting older Android (5/6) are out of scope.

### 8.5 Recommendation

Mochi-on-Android is a Phase-2 secondary target. The MEP-47 core ships JVM HotSpot + GraalVM only. Android, with its substantially different concurrency model and stdlib subset, is deferred to a follow-up **MEP-47.1 sub-MEP** (Mochi on Android via D8/R8/ART). MEP-47.1 will spec the concurrency fallback, Kotlin-coroutine interop, AAR packaging, and Gradle plugin.

## 9. GraalVM vs HotSpot choice

These three runtimes are not mutually exclusive; Mochi emits portable `.class` files and lets the user choose how to ship.

### 9.1 native-image for single-file shipping

Choose GraalVM native-image when:
- You want a single executable file with no JRE dependency.
- You want sub-50ms startup (CLI tools, FaaS, scratch containers).
- You can pay 1-5 minute build times.
- You are willing to declare reachability metadata for reflection.

### 9.2 HotSpot + jlink for hot-reload-friendly shipping

Choose HotSpot + jlink when:
- You want peak throughput after warmup (servers, batch jobs).
- You need dynamic class loading (plugins, hot-reload during development).
- You want lower build times (jlink is seconds, native-image is minutes).
- You need full reflection without metadata declaration.

### 9.3 HotSpot + ZGC + Loom for ultra-low-latency

Choose HotSpot with the `-XX:+UseZGC` flag and Mochi's `--threads=virtual` mode when:
- You need sub-millisecond GC pauses.
- You have hundreds of thousands of concurrent agents/streams (see [[09-agent-streams]]).
- You can pay the ZGC memory overhead (a few percent on top of heap).

Generational Shenandoah (JEP 521, GA in JDK 25) is an alternative to ZGC; we test both and let users pick.

Azul Zulu Prime adds CRaC (Coordinated Restore at Checkpoint), which snapshots a warmed-up JVM and restores it. Useful for fast-restart server scenarios. Not part of the certified matrix, but documented.

## 10. Reproducibility

### 10.1 JDK build reproducibility

Temurin builds are themselves reproducible: the Adoptium project publishes SHA-256 hashes for every tarball and the Reproducible Builds project verifies them. We document this and pin to specific Temurin patch versions in our CI matrix (e.g., `eclipse-temurin:21.0.6_7` not `eclipse-temurin:21`).

### 10.2 jar/uberjar reproducibility

OpenJDK's `jar` tool supports `SOURCE_DATE_EPOCH` since JDK 15. The Mochi build sets `SOURCE_DATE_EPOCH` to a fixed epoch derived from the source tree (the git commit timestamp by default, or a user-provided value) when `mochi build --reproducible` is set.

Concretely:
- All `.class` files have no embedded timestamps (javac is already deterministic).
- All entries in the jar have `mtime = SOURCE_DATE_EPOCH`.
- Directory ordering inside the jar is alphabetically sorted (we do not rely on filesystem order, which differs across glibc/musl and ext4/xfs/apfs).
- The jar's `MANIFEST.MF` declares `Created-By: Mochi 0.10` and `Build-Jdk-Spec: 21` (and not `Build-Jdk: ...` with a patch level, which would break across CI agents on different Temurin patch versions).
- `META-INF/native-image/reachability-metadata.json` is sorted lexicographically.

This produces bit-identical jar files from the same Mochi source across different CI agents, OSes, and Temurin patch versions.

### 10.3 native-image reproducibility

native-image output is deterministic **given identical input** including the same GraalVM patch level, the same glibc/musl version (for dynamic builds), and the same `--march` target. We pin GraalVM patch level in CI and use `--march=compatibility` for portable binaries (slightly slower but reproducible across host CPUs).

We document that fully-static musl binaries are the most reproducible target (no glibc version dependency).

### 10.4 jlink output reproducibility

jlink output (the custom JRE) includes a `release` file with `OS_NAME`, `OS_ARCH`, `JAVA_VERSION`, etc. These are deterministic from the input JDK and arguments. The `lib/modules` file (a single archive of all included modules) is reproducible if the source JDK is reproducible and inputs are sorted.

## 11. Tier matrix table

Final summary, the canonical table:

| Target                                | Tier  | Runtime           | Notes                                  |
|---------------------------------------|-------|-------------------|----------------------------------------|
| Linux x86_64 glibc, Temurin 21/25     | 1     | HotSpot, native-image | full CI, gating                    |
| Linux x86_64 musl, Temurin 21/25      | 1     | HotSpot, native-image | Docker shipping, gating            |
| Linux aarch64 glibc, Temurin 21/25    | 1     | HotSpot, native-image | full CI, gating                    |
| Linux aarch64 musl, Temurin 21/25     | 1     | HotSpot, native-image | Docker shipping, gating            |
| macOS aarch64, Temurin 21/25          | 1     | HotSpot, native-image | full CI, gating, Apple Silicon     |
| Windows x86_64, Temurin 21/25         | 1     | HotSpot, native-image | full CI, gating                    |
| macOS x86_64, Temurin 21/25           | 2     | HotSpot, native-image | CI smoke, not gating               |
| Windows aarch64, Temurin 21/25        | 2     | HotSpot only      | CI smoke, no native-image yet          |
| Linux ppc64le, SapMachine 21/25       | 3     | HotSpot only      | quarterly manual run                   |
| Linux s390x, SapMachine 21/25         | 3     | HotSpot only      | quarterly manual run                   |
| Linux riscv64, Temurin 21/25          | 3     | HotSpot only      | best-effort, no native-image           |
| Android API 24+                       | -     | ART (D8/R8)       | separate MEP-47.1 sub-MEP              |
| FreeBSD, Illumos                      | -     | community OpenJDK | best-effort, no CI                     |

Tier 1: every gate runs, blocking. Tier 2: nightly CI, non-blocking. Tier 3: quarterly manual or weekly best-effort. Android: separate MEP.

This matrix is reviewed at each major Mochi release and when a new JDK LTS ships.

## 12. Cross-compile and CI

The cross-compile story differs sharply by output type:

- **Plain jar / uberjar**: fully cross-arch. One jar built on any host runs on every JVM. CI builds the jar on Linux x86_64 only.
- **jlink runtime**: per-arch. CI matrix runs jlink on each tier-1 host.
- **native-image binary**: per-arch. CI matrix runs native-image on each tier-1 host. For tier-2/3 arches, we use Docker buildx with QEMU (slow but works) or the Adoptium cross-platform JMODs trick (see §6.3).
- **jpackage installer**: per-OS. CI runs jpackage on Linux (for `.deb`/`.rpm`), macOS (for `.dmg`/`.pkg`), Windows (for `.msi`/`.exe`).

The GitHub Actions matrix for the JVM gate (see [[11-testing-gates]]) is:

```yaml
strategy:
  matrix:
    os: [ubuntu-24.04, ubuntu-24.04-arm, macos-15, windows-2025]
    jdk: [21, 25]
    libc: [glibc, musl]  # musl only on Linux
    runtime: [hotspot, native-image]
    exclude:
      - os: macos-15
        libc: musl
      - os: windows-2025
        libc: musl
      - os: windows-2025
        runtime: native-image  # see Windows aarch64 caveat
```

This yields about 28 combinations; runtimes are about 15 minutes per cell for HotSpot, 30 minutes per cell for native-image. The full matrix runs nightly; PR-blocking CI runs the Tier 1 subset.

## 13. Version-floor enforcement

The Mochi-emitted bytecode must run on JDK 21. Concrete enforcement:

- `mochi-runtime.jar` is compiled with `javac --release 21`, which sets the class-file version to 65 (`major_version = 65`) and forbids API usage from JDK 22+.
- User-emitted classes likewise use `--release 21`.
- The MANIFEST sets `Created-By: Mochi 0.10` and `Build-Jdk-Spec: 21`. We do **not** set `Build-Jdk` with a patch level since that breaks reproducibility across Temurin patch versions.
- `module-info.class` declares `requires java.base` etc. without version constraints; jlink and the launcher reject any JVM with `java.base@<21`.
- A user attempting to run a Mochi jar on JDK 17 gets `UnsupportedClassVersionError: ... class file version 65.0, this JRE only supports up to 61.0`.

For users who want to target JDK 21 specifically but build on JDK 25 (the common dev setup), `javac --release 21` is the right flag (not `--target 21 --source 21`, which would miss API-level checks). The Mochi build always uses `--release`.

## 14. Security and updates

Oracle ships a Critical Patch Update (CPU) for the JDK every quarter (January, April, July, October). Temurin and other downstream vendors follow within days. CPUs ship as patch increments (e.g., 21.0.5 -> 21.0.6).

Mochi documentation will say: "Upgrade your JDK every quarter on the next CPU. We do not pin Mochi releases to a specific JDK patch level for this reason. If a CPU breaks a Mochi gate, we ship a patch-level Mochi release."

We track JDK CVEs via the Adoptium security advisories feed and ship a Mochi advisory when a CVE affects a Mochi-shipped runtime image.

For users who ship a Mochi-built jlink runtime or native-image, **the user is responsible for rebuilding** when a JDK CPU lands; the runtime is statically bound at build time. We document this prominently and recommend either:

- Re-running `mochi build --target=jvm-release` quarterly in CI, or
- Shipping a fat jar plus a recommended Temurin version and letting the deploy environment provide the JRE.

For Docker images, our `mochilang/mochi:jvm-jdk21-alpine` tag rolls forward to the latest 21 CPU automatically; users who want a fixed patch level pin `mochilang/mochi:jvm-jdk21.0.6-alpine`.

---

## Sources

1. JDK 25 release announcement and JEP list. https://openjdk.org/projects/jdk/25/
2. Oracle Java SE Support Roadmap (LTS dates, JDK 29 in 2027). https://www.oracle.com/java/technologies/java-se-support-roadmap.html
3. JEP 493, linking run-time images without JMODs. https://openjdk.org/jeps/493
4. Adoptium Temurin supported platforms (arch and OS matrix). https://adoptium.net/supported-platforms
5. Eclipse Temurin JDK 24 enables JEP 493. https://adoptium.net/news/2025/08/eclipse-temurin-jdk24-JEP493-enabled
6. JEP 503, remove the 32-bit x86 port (JDK 25). https://openjdk.org/jeps/503
7. JEP 444, virtual threads (JDK 21). https://openjdk.org/jeps/444
8. JEP 521, generational Shenandoah (JDK 25). https://openjdk.org/jeps/521
9. JEP 506, scoped values (JDK 25). https://openjdk.org/jeps/506
10. GraalVM native-image static images guide. https://www.graalvm.org/jdk24/reference-manual/native-image/guides/build-static-executables/
11. GraalVM reachability metadata reference. https://docs.oracle.com/en/graalvm/jdk/21/docs/reference-manual/native-image/metadata/
12. GraalVM Reachability Metadata Repository. https://github.com/oracle/graalvm-reachability-metadata
13. Android `desugar_jdk_libs` documentation. https://developer.android.com/studio/write/java8-support
14. Android Gradle Plugin 8.0 release notes (JDK 17 requirement). https://developer.android.com/build/releases/agp-8-0-0-release-notes
15. Reproducible Builds JVM documentation. https://reproducible-builds.org/docs/jvm/
16. SOURCE_DATE_EPOCH specification. https://reproducible-builds.org/specs/source-date-epoch/
17. jpackage Windows code signing regression (OpenJDK bug 8326447). https://bugs.openjdk.org/browse/JDK-8326447
18. jpackage macOS notarisation issue (Adoptium support 829). https://github.com/adoptium/adoptium-support/issues/829
19. BellSoft Liberica comparison of OpenJDK distributions. https://bell-sw.com/blog/oracle-java-alternatives-comparison-of-openjdk-distributions/
20. whichjdk.com, picking an OpenJDK distribution. https://whichjdk.com/
