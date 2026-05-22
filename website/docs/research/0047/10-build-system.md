# MEP-47 research note 10, Build system: Gradle, Maven, jlink, jpackage, native-image

Author: research pass for MEP-47.
Date: 2026-05-23 (GMT+7).

This note specifies how `mochi build --target=jvm-...` produces JVM artifacts: when to drive `javac` in-process, when to write classfiles directly via the ClassFile API or ASM, how to package as jar / uberjar / modular jar / jlink image / jpackage installer / native-image binary, how to publish the Mochi runtime to Maven Central via the new Central Publisher Portal, and how the optional `mochi-gradle-plugin` and `mochi-maven-plugin` fit into existing Java projects.

The companion note for the BEAM target is [[../0046/10-build-system]]; structurally this note mirrors it.

---

## 1. The `mochi build --target=jvm-...` matrix

| Target              | Output                                                              |
|---------------------|---------------------------------------------------------------------|
| `jvm-jar`           | Plain jar with `Main-Class`. Dependencies NOT included.             |
| `jvm-uberjar`       | Fat jar with all transitive dependencies shaded in. Default.        |
| `jvm-modular`       | Modular jar with `module-info.class` plus dependency layer.         |
| `jvm-jlink`         | Custom JRE image (directory or zip).                                |
| `jvm-jpackage`      | Platform installer (.deb / .rpm / .dmg / .pkg / .msi).              |
| `jvm-native`        | GraalVM native-image binary.                                        |
| `jvm-aar`           | Android Archive (Phase 2, MEP-47.1).                                |
| `jvm-dex`           | Android DEX bytecode (Phase 2).                                     |

The default for `mochi build` on a Mochi project's `main.mochi` is `jvm-uberjar`. The unprefixed alias `mochi build --target=jvm` resolves to `jvm-uberjar` because that is the artifact 90% of users want to ship: one file, runs anywhere with a JRE.

Power users pick `jvm-modular` to feed `jlink`, `jvm-native` for cold-start workloads, or `jvm-jpackage` for desktop installers.

## 2. The shape of generated output

All Mochi compilation output for the JVM target lives under `target/jvm/`, mirroring MEP-45's `target/c/`:

```
target/jvm/
├── classes/                       # .class files, organised by package
│   └── dev/mochi/user/main/Main.class
├── sources/                       # .java files (only if source-emission path)
│   └── dev/mochi/user/main/Main.java
├── META-INF/
│   ├── MANIFEST.MF                # Main-Class, Built-By, etc.
│   ├── services/                  # ServiceLoader entries
│   ├── native-image/              # reachability metadata for native-image
│   │   └── dev.mochi/user-main/reachability-metadata.json
│   └── versions/                  # MR-JAR (multi-release) layout if needed
│       └── 21/
├── module-info.class              # JPMS module descriptor for the user app
└── mochi.json                     # Mochi-specific metadata (sources, hashes, incrementality)
```

`mochi.json` is the equivalent of `mochi.toml` for a built artifact. It records source file digests so incremental rebuilds can skip work. The format is small JSON, not a binary cache index; the cache index lives in `~/.cache/mochi/aotir/`.

## 3. Source-emission path: driving `javac` in-process

When the codegen chooses the source-emission path (see [[05-codegen-design]]), Mochi compiles `.java` files in-process via the standard `javax.tools` API. We do NOT shell out to `javac`; that adds 300ms cold-start per invocation and creates a hard dependency on the user's `PATH`.

```java
JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
StandardJavaFileManager fm = compiler.getStandardFileManager(null, null, UTF_8);
fm.setLocation(StandardLocation.SOURCE_PATH, List.of(new File("target/jvm/sources")));
fm.setLocation(StandardLocation.CLASS_OUTPUT, List.of(new File("target/jvm/classes")));
List<String> opts = List.of(
    "--release", "21",         // bytecode target
    "-implicit:none",          // don't recursively compile referenced sources
    "-parameters",             // keep parameter names in class files (for reflection-heavy users)
    "-Xlint:all",              // surface anything javac sees; we treat warnings as fatal in CI
    "-proc:none"               // no annotation processors (we generate no annotations)
);
JavaCompiler.CompilationTask task = compiler.getTask(null, fm, diag, opts, null, units);
task.call();
```

Note that `ToolProvider.getSystemJavaCompiler()` requires the Mochi binary to run on a JDK, not a JRE. The Mochi distribution ships an embedded JDK (Temurin 21 trimmed via `jlink`) so users do not need `JAVA_HOME` set. If `JAVA_HOME` is set and points at a JDK >= 21, the Mochi binary will use that one instead, which lets users target newer bytecode levels without waiting for a Mochi release.

Annotation processing is unconditionally disabled (`-proc:none`). Mochi does not generate annotations and we do not want to be on the hook for AP version compatibility.

## 4. Bytecode-emission path: ClassFile API and ASM

When the codegen chooses the bytecode-emission path, Mochi writes `.class` files directly. The preferred path on JDK 24+ is the JDK ClassFile API (JEP 484, finalized in JDK 24). For JDK 21 LTS support, the build driver falls back to ASM 9.7.

```java
byte[] bytes = ClassFile.of().build(
    ClassDesc.of("dev.mochi.user.main", "Main"),
    cb -> cb.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL)
            .withVersion(ClassFile.JAVA_21_VERSION, 0)
            .withMethodBody("main", MethodTypeDesc.of(CD_void, CD_String.arrayType()),
                            ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
                            cob -> cob.return_()));
Files.write(Path.of("target/jvm/classes/dev/mochi/user/main/Main.class"), bytes);
```

No external tool. No shell-out. The output is bit-identical between runs given the same input (see §11 reproducible builds).

ASM remains the fallback for two reasons. First, it works on JDK 21 LTS which does not have the ClassFile API. Second, several legacy IR rewriters in [[05-codegen-design]] start from ASM tree-API nodes; rewriting them to ClassFile API is Phase 3 work.

## 5. Maven Central publishing of `dev.mochi.runtime`

The Mochi runtime jar is the only direct Maven dependency a user app needs. Coordinates:

- groupId: `dev.mochi`
- artifactId: `runtime`
- version: matches the Mochi binary release. Mochi 0.11 ships `dev.mochi:runtime:0.11.0`.
- license: Apache-2.0.
- packaging: jar (modular jar: contains `module-info.class` for `module dev.mochi.runtime`).

Sonatype OSSRH reached end of life on 2025-06-30 and all namespaces were migrated to the Central Publisher Portal at `central.sonatype.com`. The legacy `oss.sonatype.org` and `s01.oss.sonatype.org` staging endpoints are gone. New publishers register namespaces through the Portal UI, and authentication uses Portal user tokens (separate from any legacy OSSRH tokens), generated at `central.sonatype.com/usertoken`.

The Mochi project publishes via the `central-publishing-maven-plugin` (groupId `org.sonatype.central`), which uploads a deployment bundle to `https://central.sonatype.com` and either auto-releases on validation or lets the publisher review before release. Required artifacts:

- `runtime-VERSION.jar` (binary)
- `runtime-VERSION-sources.jar` (sources)
- `runtime-VERSION-javadoc.jar` (Javadoc; Mochi runtime is Java + Kotlin, so Dokka feeds an `-javadoc.jar` alongside Javadoc)
- `runtime-VERSION.pom` (POM with SCM, license, developers metadata)
- PGP `.asc` signatures for each of the above. Key must be uploaded to a keyserver Central polls (`keyserver.ubuntu.com`, `keys.openpgp.org`, or `pgp.mit.edu`).

The Maven Central Portal validates the bundle synchronously and surfaces errors immediately, which is a major UX improvement over the OSSRH staging-repo workflow. Failed validations (missing javadoc, broken signatures, wrong POM coordinates) come back in seconds rather than minutes.

The Mochi release process invokes the plugin from CI (see §14) and gates each Mochi binary release on a green Central publication. We do not maintain a separate cadence for the runtime jar; runtime version == Mochi version always.

## 6. Dependency management

The Mochi runtime jar pulls in a small, deliberate set of transitive dependencies. Anything bigger gets pushed out of the runtime and into an opt-in module.

| Dependency                                           | Version | Why                              |
|------------------------------------------------------|---------|----------------------------------|
| `com.fasterxml.jackson.core:jackson-databind`        | 2.18.x  | JSON; Mochi `json` builtin       |
| `com.fasterxml.jackson.dataformat:jackson-dataformat-csv` | 2.18.x  | CSV; Mochi `csv` builtin         |
| `org.snakeyaml:snakeyaml-engine`                     | 2.x     | YAML; Mochi `yaml` builtin       |
| `org.bouncycastle:bcpkix-jdk18on`                    | 1.78    | Optional, only if `crypto` used  |

Explicitly rejected for default bundling:

- **Spring**: out of scope; Mochi runtime is not a web framework. Users can pull Spring themselves on top of a Mochi-built jar.
- **Guava**: ~3MB and most of its surface has been absorbed into the JDK since Java 11.
- **Apache Commons (lang3/io/collections)**: same reason; JDK stdlib covers what Mochi codegen emits.
- **Netty**: heavyweight, version-sensitive, and only needed for `mochi.fetch` if we choose async HTTP. Phase 1 uses the JDK 21 `java.net.http.HttpClient` instead.

Slim mode `mochi build --no-json --no-yaml` produces a runtime jar with the jackson and snakeyaml dependencies stripped, dropping the uberjar size from ~10MB to ~2MB. The Mochi codegen tracks which builtins the user code actually references and emits a `mochi.json` manifest listing required runtime modules; the build driver uses that manifest to decide whether to shade jackson in or out.

## 7. Gradle integration (user side)

For users with an existing Gradle project who want to drop `.mochi` files alongside their Java / Kotlin sources, Mochi ships a `mochi-gradle-plugin` (plugin ID `dev.mochi.compile`), published to the Gradle Plugin Portal.

```kotlin
plugins {
    java
    id("dev.mochi.compile") version "0.11.0"
}

mochi {
    source.from("src/main/mochi")
    target.set("jvm")            // default
    release.set(21)              // bytecode target
}

dependencies {
    implementation("dev.mochi:runtime:0.11.0")
}
```

The plugin registers a `mochiCompile` task that depends on `processResources` and is depended on by `compileJava`. It feeds its outputs into `sourceSets.main.output.classesDirs` so Gradle's incremental compile sees them. Build cache integration is automatic via Gradle's `@CacheableTask` annotation; inputs are the `.mochi` source tree plus the Mochi binary version.

Tested against Gradle 9.5.1 (current stable as of 2026-05-14). The plugin requires Gradle 9.0 minimum because it relies on the Configuration Cache becoming the preferred execution mode in Gradle 9 and the Kotlin 2.2 runtime. For Gradle 8.x users, we maintain the older `mochi-gradle-plugin:0.10.x` line which uses the legacy Kotlin DSL.

`mochi build --gradle-init` emits a fresh `build.gradle.kts` plus settings file in a project directory; this is the path for greenfield projects that want a Gradle-driven workflow from day one.

## 8. Maven integration (user side)

The companion is `mochi-maven-plugin` (Maven Mojo), published to Maven Central alongside `dev.mochi:runtime`. It binds the `mochi-compile` goal to the `generate-sources` (source-emission path) or `process-classes` (bytecode-emission path) lifecycle phase.

```xml
<plugin>
    <groupId>dev.mochi</groupId>
    <artifactId>mochi-maven-plugin</artifactId>
    <version>0.11.0</version>
    <executions>
        <execution>
            <goals><goal>mochi-compile</goal></goals>
            <configuration>
                <sourceDir>src/main/mochi</sourceDir>
                <release>21</release>
            </configuration>
        </execution>
    </executions>
</plugin>
```

Maven 4.0.0-rc-5 is the current preview as of May 2026; Maven 3.9.16 remains the recommended stable. The plugin targets Maven 3.9 minimum so existing user projects work unchanged. Once Maven 4 ships GA the plugin will adopt the build-POM / consumer-POM separation, which lets us strip Mochi-specific configuration out of the published consumer POM users see when they depend on a Mochi-built library.

`mochi build --maven-init` emits a fresh `pom.xml`.

## 9. jlink integration

jlink produces a custom JRE containing only the modules the application actually needs. Output size drops from a stock JDK's ~300MB to ~50MB for a "hello world" Mochi app. The catch: jlink requires every input jar to be a modular jar. The Mochi runtime is modular (`module dev.mochi.runtime { exports ...; requires java.net.http; ... }`) and Mochi codegen emits `module-info.class` for the user application. Transitive dependencies that lack `module-info` (jackson 2.18 is partially modular, snakeyaml-engine is not) get fed through `jdeps --generate-module-info` to synthesise an automatic module descriptor, which jlink accepts.

```
mochi build --target=jvm-jlink
```

internally runs:

```
jlink \
  --module-path target/jvm/modules:$JAVA_HOME/jmods \
  --add-modules dev.mochi.user.main \
  --launcher mochi=dev.mochi.user.main/dev.mochi.user.main.Main \
  --strip-debug --no-header-files --no-man-pages \
  --compress=zip-6 \
  --output target/jvm/jlink-image
```

For modules that genuinely cannot be modularised (a small but real category: dependencies that use split packages or reflection across module boundaries), the build driver falls back to wrapping them in a tiny shim module with explicit `requires` and `opens` directives. This is the same trick the badass-jlink Gradle plugin uses; we copy the technique rather than re-inventing it.

On JDK 25 jlink supports the `--add-options` flag to bake JVM flags into the launcher script (e.g., `-XX:+UseZGC -Xmx512m`); Mochi sets sensible defaults but lets the user override via `mochi.toml [jvm.jlink].jvm_args`.

## 10. jpackage integration

`jpackage` wraps a jlink runtime plus the application into a native OS installer:

```
mochi build --target=jvm-jpackage
```

invokes:

```
jpackage \
  --type deb \                          # or rpm, dmg, pkg, msi, app-image
  --name mochi-user-main \
  --module dev.mochi.user.main/Main \
  --module-path target/jvm/modules \
  --runtime-image target/jvm/jlink-image \
  --app-version 0.1.0 \
  --vendor "Mochi User" \
  --icon assets/icon.icns \
  --dest dist/
```

When `--runtime-image` is supplied, jpackage skips its internal jlink call and uses the provided image. This matters because Mochi has already produced a tuned jlink image with the right `--add-modules` set; letting jpackage re-jlink would lose those decisions.

jpackage is platform-specific in two ways. First, the installer formats are gated on the host OS (cannot build a `.msi` on macOS; cannot build a `.dmg` on Linux). Second, signing is host-tooled: macOS needs `codesign` and notarisation via `notarytool`; Windows needs `signtool`. The Mochi CI matrix (§14) runs one jpackage job per target OS to sidestep both constraints.

## 11. GraalVM native-image integration

```
mochi build --target=jvm-native
```

drives `native-image` from Oracle GraalVM 25 (released 2025-09-16; latest patch 25.0.2 from the January 2026 CPU). The build is in-process via `org.graalvm.nativeimage.NativeImage` if available on the build classpath, otherwise the driver invokes the `native-image` CLI.

Flags Mochi sets by default:

- `--no-fallback`: produce a real native binary, not a JVM-on-startup fallback. The fallback is a footgun because it silently masks reachability metadata bugs.
- `-Os`: optimise for size (default `-O2` produces ~30MB binaries; `-Os` brings hello-world down to ~10MB).
- `--enable-sbom`: a Software Bill of Materials is embedded by default in GraalVM 25 native images, listing all third-party dependencies with versions.
- `--future-defaults=run-time-initialize-jdk`: shift JDK init from build time to run time. This is the GraalVM 25 default trajectory; opting in now avoids the static-initialiser trap where `mochi.fetch` would try to open the HTTP client during native-image build.
- `--initialize-at-run-time=dev.mochi.runtime.Net,dev.mochi.runtime.Crypto,dev.mochi.runtime.Random`: explicit run-time init for any Mochi runtime class that touches OS resources at `<clinit>` time.
- `--libc=musl --static` for Linux targets, producing a fully static binary that runs in any container. macOS and Windows do not support `--static`; on those platforms we emit a dynamic binary.

The Mochi codegen emits a single `reachability-metadata.json` (JEP'd as the unified format since GraalVM JDK 23; individual `reflect-config.json` / `resource-config.json` / `jni-config.json` / `proxy-config.json` / `serialization-config.json` are deprecated). It lives at `target/jvm/META-INF/native-image/dev.mochi/user-main/reachability-metadata.json` and is picked up automatically by native-image because of the standard `META-INF/native-image/<group>/<artifact>/` discovery path.

The Mochi runtime jar ships its own `META-INF/native-image/dev.mochi/runtime/reachability-metadata.json` pre-computed, so users compiling Mochi apps to native do not need to run the tracing agent themselves. This is the same pattern Spring Boot, Quarkus, and Micronaut use.

A note on platform support: GraalVM 25 dropped macOS x64; only macOS aarch64 (Apple Silicon) is supported. For x64 macOS users we fall back to JDK 25 plain native execution (no AOT compilation), with a clear error message rather than a silent miscompile.

## 12. Reproducible builds

Bit-identical output across machines is a project-wide invariant (see [[02-design-philosophy]]). For JVM artifacts:

- jar timestamps: set `SOURCE_DATE_EPOCH` (or pass `--date` to the `jar` tool, JDK 19+). The Mochi build driver always sets `SOURCE_DATE_EPOCH` to the most recent source-file mtime, which gives deterministic timestamps without freezing to epoch zero (which trips Windows file systems).
- jar entry order: sort alphabetically. The default `jar` tool order is filesystem-dependent.
- manifest: write a stable `Built-By: mochi/VERSION` line, no `Built-Date`, no machine-specific paths.
- bytecode: `javac --release 21` produces stable output given identical inputs. Bytecode emission via ClassFile API / ASM is deterministic by construction.
- `maven-jar-plugin` >= 3.4.0 supports `reproducible` mode; `mochi-maven-plugin` sets it.
- Gradle 8+ has built-in reproducible jar support; `mochi-gradle-plugin` enables it.
- native-image: deterministic given deterministic input. We pin the GraalVM base image digest and the musl/zlib versions inside the build container to remove drift.

Verification: Mochi CI rebuilds the runtime jar twice on two different runners and `diffoscope`s them. Any non-determinism is treated as a release-blocking bug.

## 13. Caching and incremental rebuilds

Three cache layers in series:

1. **Mochi source -> JVM IR** (`aotir` cache, shared with the MEP-45 C target). Keyed on file digest plus Mochi compiler version. Cache lives at `~/.cache/mochi/aotir/`.
2. **JVM IR -> .class / .java**. Per-module, keyed on IR hash plus codegen flags. Mochi only re-emits files whose IR changed.
3. **native-image build cache**. GraalVM 25 has a built-in image build cache; we pass `--bundle-create=cache` on the first run and `--bundle-apply=cache` on subsequent runs.

The Gradle and Maven plugins respect each tool's native cache (Gradle build cache, Maven `-o` offline mode plus reactor incrementality) so a Mochi-mixed project rebuilds at Gradle / Maven speed.

`mochi build --watch` is the dev-loop entry point. It tails the `.mochi` source tree via `fsnotify`, recompiles changed modules' IR, regenerates affected `.class` files, and (when a Mochi JVM process is already running with `-agentpath:hotswap`) hot-loads them via JDWP `RedefineClasses`. This matches the BEAM watch-mode story in [[../0046/10-build-system]] §12.

## 14. CI shipping recipe (GitHub Actions)

The Mochi project ships its own runtime jar plus Mochi-compiler binaries from a matrix:

```yaml
name: JVM CI
on: [push, pull_request]
jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-22.04, ubuntu-22.04-arm, macos-14, windows-2022]
        java: [21, 25]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: 'temurin'
          java-version: ${{ matrix.java }}
      - run: mochi build --target=jvm-uberjar
      - run: mochi test --target=jvm
      - uses: actions/upload-artifact@v4
        with:
          name: mochi-${{ matrix.os }}-jdk${{ matrix.java }}
          path: target/jvm/*.jar

  native:
    strategy:
      matrix:
        os: [ubuntu-22.04, ubuntu-22.04-arm, macos-14, windows-2022]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: graalvm/setup-graalvm@v1
        with:
          java-version: '25'
          distribution: 'graalvm-community'
      - run: mochi build --target=jvm-native
      - uses: actions/upload-artifact@v4

  publish:
    if: startsWith(github.ref, 'refs/tags/v')
    needs: [build, native]
    runs-on: ubuntu-22.04
    steps:
      - run: mvn -pl runtime deploy -P central-publish
        env:
          MAVEN_USERNAME: ${{ secrets.CENTRAL_TOKEN_USER }}
          MAVEN_PASSWORD: ${{ secrets.CENTRAL_TOKEN_PASS }}
          MAVEN_GPG_KEY: ${{ secrets.GPG_PRIVATE_KEY }}
```

`graalvm/setup-graalvm@v1` is the official action and tracks GraalVM 25.x line releases. `actions/setup-java@v4` provides Temurin 21 (LTS) and Temurin 25 (LTS) in the same matrix, which is how Mochi verifies it stays compatible with both LTS lines.

The native job intentionally has no `java: 21` row; native-image binaries are produced once per OS / arch and the GraalVM version pins the underlying JDK.

## 15. Local dev

Two entry points serve the inner dev loop:

- `mochi run --jvm main.mochi`: compile to in-memory classes, load them via a custom `ClassLoader`, invoke `Main.main`. No jar on disk. ~200ms cold; ~50ms warm via the `mochi --daemon` background process.
- `mochi test --target=jvm`: discover `test "..." { ... }` blocks, compile them into `*Tests.class`, invoke via JUnit Platform's launcher API (in-process). Test reports written to `target/jvm/test-results/` in the standard JUnit XML format so IDEs and CI dashboards pick them up.

For step-through debugging, `mochi run --jvm --debug=5005` starts the JVM with `-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005` so IntelliJ / VS Code can attach. Source maps (`.mochi` line -> `.java` or `.class` line) are emitted into the `SourceDebugExtension` attribute, so breakpoints set on `.mochi` files land on the right bytecode.

## 16. Default vs power-user

The default path is one line: `mochi build`. That produces `target/jvm/app.jar` (an uberjar) runnable with `java -jar app.jar`.

Power users opt into:

- `--target=jvm-modular` if they will compose with jlink themselves.
- `--target=jvm-jlink` for a self-contained JRE image.
- `--target=jvm-jpackage` for desktop installers.
- `--target=jvm-native` for cold-start sensitive workloads (CLIs, lambdas, containers under 50MB).

We do NOT ask the user to learn the difference between a jar, an uberjar, a modular jar, and a jmod on day one. Most production Java code ships uberjars; that is the right default.

## 17. Output size budget

Reference numbers for `hello world` (Mochi `println("hello, world")`):

| Target          | Size       | Cold start         |
|-----------------|------------|--------------------|
| `jvm-jar`       | ~30 KB     | ~120 ms (JVM warmup) |
| `jvm-uberjar`   | ~10 MB     | ~150 ms            |
| `jvm-uberjar --no-json --no-yaml` | ~2 MB | ~140 ms |
| `jvm-jlink`     | ~50 MB     | ~80 ms             |
| `jvm-jpackage --type dmg` | ~55 MB | n/a (installer) |
| `jvm-native -O2` | ~30 MB    | ~5 ms              |
| `jvm-native -Os` | ~10 MB    | ~5 ms              |

The native binary numbers are sensitive to GraalVM version and Mochi's reachability metadata precision. GraalVM 25 enabling Whole-Program SCCP by default and the new XGBoost-based static profiler bring binary size down meaningfully versus GraalVM 23 / 24.

## 18. Cross-compile

| Target           | Cross-compile story                                                |
|------------------|---------------------------------------------------------------------|
| `jvm-jar`        | Arch-independent. One jar runs on any JRE 21+.                      |
| `jvm-uberjar`    | Arch-independent.                                                    |
| `jvm-modular`    | Arch-independent.                                                    |
| `jvm-jlink`      | Must run jlink on each target arch / OS. Use Docker buildx + QEMU, or a CI matrix. |
| `jvm-jpackage`   | Must run jpackage on each target OS. CI matrix only.                |
| `jvm-native`     | native-image runs on the same OS / arch as the target. CI matrix.   |

`mochi build --target=jvm-jlink --docker linux/arm64` invokes the equivalent of `docker run --platform linux/arm64 mochilang/mochi:jvm-jdk25 mochi build --target=jvm-jlink`. The Mochi runtime image (`mochilang/mochi:jvm-jdk25-musl`) has GraalVM 25 + musl toolchain pre-installed so the native-image cross-build path works without per-developer setup.

## 19. Verification: round-trip with Gradle and Maven

For every codegen change in MEP-47, the test suite runs:

1. Compile Mochi sources to `.class` via the in-process build.
2. Emit a Gradle project (`mochi build --gradle-init`).
3. Run `./gradlew build` from scratch against the emitted project.
4. Diff the resulting `.class` files against the in-process output. Bytes must match (modulo timestamps, which we zero out).
5. Same with `mvn package` against an emitted Maven project.

This catches drift between in-process compilation and user-facing build-tool paths. The same gate ran for MEP-46's rebar3 round-trip; we copy the pattern.

## 20. Phase mapping

| Phase     | Build-system deliverable                                       |
|-----------|------------------------------------------------------------------|
| 1.0       | `jvm-jar`, `jvm-uberjar`, `mochi build --gradle-init`.           |
| 1.1       | `mochi-gradle-plugin`, `mochi-maven-plugin` (basic).             |
| 1.2       | `jvm-modular`, `jvm-jlink`.                                     |
| 1.3       | `jvm-jpackage` for the three desktop OSes.                       |
| 2.0       | `jvm-native` with GraalVM 25, including reachability metadata.   |
| 2.1       | `jvm-aar`, `jvm-dex` for the Android target (MEP-47.1).          |
| 3.0       | Reproducible-build gate enforced in CI.                          |

## 21. Out of scope

- **sbt** (Scala). Mochi codegen does not target Scala; users who already have sbt projects can consume the published `dev.mochi:runtime` jar by adding it as a library dependency, but Mochi does not ship an sbt plugin.
- **Mill**. Same story.
- **Bazel**. Out-of-band integration only: users can `java_import` the uberjar. We do not publish a `rules_mochi` Starlark ruleset in Phase 1; it is on the long-term roadmap once Mochi has Bazel users asking.
- **Ant**. Out of scope; users on Ant can consume the jar via `<javac>` plus `<copy>` but get no Mochi-side tooling.

## 22. Summary

The JVM build system reuses the Java ecosystem at every layer:

- `javac` (via `javax.tools`) or ClassFile API for compilation.
- jar tool for packaging, with deterministic timestamps.
- jlink for custom JREs.
- jpackage for native installers.
- GraalVM `native-image` for ahead-of-time native binaries.
- Maven Central (via the Central Publisher Portal) for the runtime jar.
- Gradle Plugin Portal for the Gradle plugin.
- GitHub Actions + Temurin 21/25 + GraalVM 25 for CI.

We add Mochi-specific glue (the build driver, the source-map emission into `SourceDebugExtension`, the consolidated `reachability-metadata.json`, the `mochi-gradle-plugin` and `mochi-maven-plugin`) but contribute nothing new to the JVM build space. That is the point. The same way MEP-46 leaned on rebar3 and relx without inventing a new BEAM build tool, MEP-47 leans on Gradle, Maven, jlink, jpackage, and native-image without inventing a new JVM build tool.

---

## Sources

1. Sonatype Central Portal docs. https://central.sonatype.org/
2. OSSRH Sunset notice (effective 2025-06-30). https://central.sonatype.org/pages/ossrh-eol/
3. Register to Publish Via the Central Portal. https://central.sonatype.org/register/central-portal/
4. Gradle 9 release notes. https://gradle.org/whats-new/gradle-9/
5. Gradle 9.5.1 release notes. https://docs.gradle.org/current/release-notes.html
6. Apache Maven 4.0.0-rc-5 release notes. https://maven.apache.org/docs/4.0.0-rc-5/release-notes.html
7. What's new in Maven 4. https://maven.apache.org/whatsnewinmaven4.html
8. GraalVM for JDK 25 release notes. https://www.graalvm.org/release-notes/JDK_25/
9. Oracle GraalVM 25 docs. https://docs.oracle.com/en/graalvm/jdk/25/docs/release-notes/
10. GraalVM Reachability Metadata. https://www.graalvm.org/latest/reference-manual/native-image/metadata/
11. JDK 25 jpackage spec. https://docs.oracle.com/en/java/javase/25/docs/specs/man/jpackage.html
12. JDK 25 jlink spec. https://docs.oracle.com/en/java/javase/25/docs/specs/man/jlink.html
13. JEP 484: Class-File API. https://openjdk.org/jeps/484
14. JEP 275: Modular Java Application Packaging (jpackage). https://openjdk.org/jeps/275
15. badass-jlink Gradle plugin. https://github.com/beryx/badass-jlink-plugin
16. central-publishing-maven-plugin. https://central.sonatype.org/publish/publish-portal-maven/
17. graalvm/setup-graalvm GitHub Action. https://github.com/graalvm/setup-graalvm
18. actions/setup-java GitHub Action. https://github.com/actions/setup-java
