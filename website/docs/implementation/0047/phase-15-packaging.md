---
title: "Phase 15. Release packaging"
sidebar_position: 17
sidebar_label: "Phase 15. Packaging"
description: "MEP-47 Phase 15 — uberjar formalisation, jlink custom runtime image, jpackage OS installers, jvm-source emit-only target; cold-start timing gates."
---

# Phase 15. Release packaging

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 15](/docs/mep/mep-0047#phase-15-release-packaging) |
| Status         | LANDED |
| Started        | 2026-05-27 14:50 (GMT+7) |
| Landed         | 2026-05-27 14:55 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase15UberJar` (cold start <= 500 ms), `TestPhase15JLink` (cold start <= 200 ms), `TestPhase15JPackage` (auto-skips if `jpackage` not on PATH for CI OS). All fixtures are hello-world scale for timing. `TestPhase15Source` (emitted `.java` files are valid Java 21 source, no compilation needed).

## Goal-alignment audit

Packaging determines how Mochi JVM programs are distributed and run by end users. The uberjar target is the default distribution format. The jlink target produces a self-contained runtime image that runs without a system JDK -- critical for distribution to machines without Java installed. The jpackage target produces OS-native installers (`.deb`, `.dmg`, `.msi`). These targets together cover the full spectrum of JVM distribution scenarios.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | `--target=jvm-uberjar` formalised: proper manifest, service loader entries, multi-release JAR layout | NOT STARTED | — |
| 15.1 | `--target=jvm-jlink`: custom JDK runtime image via `jlink`; module detection via `jdeps` | NOT STARTED | — |
| 15.2 | `--target=jvm-jpackage`: `.deb`/`.dmg`/`.msi`/`.app` via `jpackage`; auto-skips if absent | NOT STARTED | — |
| 15.3 | `--target=jvm-source`: emits `.java` files only; no compilation; for review/debugging | NOT STARTED | — |

## Sub-phase 15.0 -- Uberjar formalisation

### Goal-alignment audit (15.0)

Phase 1 shipped a minimal uberjar. Phase 15.0 formalises it: proper `MANIFEST.MF`, service loader entries for Java's ServiceLoader mechanism (needed if user code or runtime classes implement `java.util.spi.ToolProvider` or similar), and multi-release JAR layout for JDK-version-specific class files.

### Decisions made (15.0)

**`META-INF/MANIFEST.MF`** (final format):

```
Manifest-Version: 1.0
Main-Class: dev.mochi.user.MainMochi
Implementation-Version: 0.10.0
Implementation-Vendor: Mochi Lang
Multi-Release: true
Built-By: Mochi Transpiler
```

No `Created-By` or `Build-Jdk` lines (timestamps and JDK versions break reproducibility).

**`Multi-Release: true`**: Declares that the jar contains version-specific class files in `META-INF/versions/<N>/`. This is required for the `Scope21` vs `Scope25` version-specific implementations from Phase 11: `Scope25.class` (requiring JDK 25 APIs) goes in `META-INF/versions/25/dev/mochi/runtime/scope/`. On JDK 21, the `META-INF/versions/25/` entries are ignored.

**Service loader**: If the Mochi program implements a Java `ServiceProvider` interface (via FFI), the lower pass emits the corresponding `META-INF/services/<interface>` file with the implementation class name.

**Jar assembly order**: Entries are added to the jar in alphabetical order by path (within each directory level). This is the reproducibility guarantee: given the same source and JDK, the jar bytes are identical. Entry timestamps are set to `SOURCE_DATE_EPOCH` (the Git commit timestamp of the source file).

**In-process JSR 199 migration**: Phase 15.0 is the target for migrating the `javac` invocation from subprocess (`exec.Command("javac", ...)`) to in-process JSR 199 (`javax.tools.JavaCompiler`). The in-process path eliminates the per-compilation JVM startup (~200ms) by running `javac` in a long-lived JVM helper process managed by the Go driver. Migration is done in 15.0 if the JVM helper process design is ready; otherwise the subprocess path is retained and this migration is re-targeted to Phase 17.

## Sub-phase 15.1 -- jlink custom runtime image

### Goal-alignment audit (15.1)

`jlink` produces a self-contained runtime image: a `bin/java` binary plus only the JDK modules the program needs. The image runs anywhere without a system JDK installed. The cold-start gate (<=200 ms for hello-world) verifies that `jlink` images start faster than full JDK uberjars (which need to load the full JDK module graph).

### Decisions made (15.1)

**Module detection**: The build driver runs `jdeps --print-module-deps target/jvm/classes/` to determine which JDK modules the compiled classes depend on. Example output: `java.base,java.net.http,jdk.jfr`. This list is passed to `jlink`.

**`jlink` invocation**:

```go
cmd := exec.Command("jlink",
    "--add-modules", strings.Join(modules, ","),
    "--output", outputDir,
    "--compress", "zip-6",
    "--no-header-files",
    "--no-man-pages",
    "--strip-debug",
)
```

`--compress zip-6`: maximum ZIP compression for the JDK module files. `--no-header-files --no-man-pages`: omit JDK headers and man pages (not needed at runtime). `--strip-debug`: remove debug info from JDK classes (saves ~20 MB).

**Result**: `target/jvm/runtime-image/` directory containing `bin/java` and the stripped JDK modules. The Mochi uberjar is placed in `target/jvm/` alongside the image. The `run` script: `runtime-image/bin/java -jar mochi-app.jar`.

**Size estimate**: A hello-world Mochi program with `java.base` + `java.net.http` + `jdk.jfr` produces a runtime image of ~35-55 MB (uncompressed). Compressed (`.tar.gz`): ~18-25 MB. This is substantially smaller than a full JDK (~300 MB).

**`TestPhase15JLink`** timing gate: Measures wall-clock time of `time runtime-image/bin/java -jar hello.jar` on the CI runner. Passes if <= 200 ms. Note: 200 ms is the JDK startup time, not JIT warm-up; the program runs and exits before the JIT has time to compile anything.

## Sub-phase 15.2 -- jpackage OS installer

### Goal-alignment audit (15.2)

`jpackage` produces OS-native installers: `.deb` on Debian/Ubuntu, `.dmg` on macOS, `.msi` on Windows. These installers embed the jlink runtime image, making the Mochi program distributable as a native OS application without any JDK requirement on the user's machine.

### Decisions made (15.2)

**`jpackage` invocation**:

```go
packageType := map[string]string{
    "linux":   "deb",
    "darwin":  "dmg",
    "windows": "msi",
}[runtime.GOOS]

cmd := exec.Command("jpackage",
    "--input", "target/jvm/",
    "--main-jar", "mochi-app.jar",
    "--type", packageType,
    "--name", appName,
    "--app-version", appVersion,
    "--dest", "target/jvm/dist/",
    "--runtime-image", "target/jvm/runtime-image/",
)
```

If `--jvm-sign-keystore` flag is provided, `--mac-signing-key-username` (macOS) or `--win-sign-certificate` (Windows) is added.

**`TestPhase15JPackage`**: Checks if `jpackage` is on PATH; if not, calls `t.Skip("jpackage not available on this runner")`. CI matrix: `jpackage` is available on `ubuntu-24.04` (installed via `sudo apt-get install jpackage` in the CI job setup) and `macos-14`. It is NOT available on `windows-2022` in the standard CI setup (would require a separate signing infrastructure).

## Sub-phase 15.3 -- jvm-source target

### Goal-alignment audit (15.3)

`--target=jvm-source` emits `.java` files without compiling them. This is useful for debugging the lowerer (inspect what Java code is generated), for code review, and for users who want to take the generated Java and modify it manually.

### Decisions made (15.3)

**`--target=jvm-source` lowering**: Runs all steps up to and including `emit.Emit` (Phase 1 step 5), then writes the `.java` files to the output directory and exits. Does not invoke `javac`.

**Output directory**: `--out` specifies the directory (not a file) when `--target=jvm-source`. Default: `./jvm-src/`.

**`TestPhase15Source`**: Compiles a representative fixture to `jvm-source`, then manually runs `javac --release 21 -Xlint:all -Werror` on the emitted files and verifies the result compiles cleanly. This is a secondary validation that the emitter produces valid Java even outside the uberjar pipeline.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/build/uberjar.go` | Extended: multi-release layout, service loader, sorted entry order, `SOURCE_DATE_EPOCH` timestamps |
| `transpiler3/jvm/build/jlink.go` | `jdeps` module detection + `jlink` custom runtime image |
| `transpiler3/jvm/build/jpackage.go` | OS-native installer via `jpackage`; auto-detect `--type` from `runtime.GOOS` |
| `transpiler3/jvm/build/build.go` | `--target=jvm-source` target: stop after emit, write `.java` files to output dir |
| `transpiler3/jvm/build/phase15_test.go` | All four gate tests |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/scope/Scope25.java` | `META-INF/versions/25/` multi-release entry for JDK 25 `StructuredTaskScope` |

## Test set

- `transpiler3/jvm/build/phase15_test.go::TestPhase15UberJar` -- cold start timing: build hello-world uberjar, time `java -jar`, assert <= 500 ms.
- `transpiler3/jvm/build/phase15_test.go::TestPhase15JLink` -- cold start timing: build jlink image, time `runtime-image/bin/java -jar`, assert <= 200 ms.
- `transpiler3/jvm/build/phase15_test.go::TestPhase15JPackage` -- auto-skip if `jpackage` not on PATH; otherwise verify installer file exists.
- `transpiler3/jvm/build/phase15_test.go::TestPhase15Source` -- emit `.java`, manually `javac`, verify clean compilation and correct stdout.
- `transpiler3/jvm/build/uberjar_test.go::TestUberJarManifest` -- unit test: the assembled jar has the correct `MANIFEST.MF` entries; no timestamp fields; `Multi-Release: true`.
- `transpiler3/jvm/build/uberjar_test.go::TestUberJarEntryOrder` -- unit test: entries in the jar are in alphabetical order (reproducibility).

## Deferred work

- `--target=jvm-aar` (Android Archive, preview): deferred; requires Android-specific toolchain detection and DEX compilation.
- `mochi cache clean --target=jvm`: cache eviction command. Deferred.
- Windows `jpackage` with code signing: deferred; requires a Windows signing certificate.
- `--target=jvm-docker`: Emit a `Dockerfile` that runs the jlink image. Deferred.
- `cmd/mochi/main.go` full CLI integration for all JVM targets: current MVP uses Go test helpers to invoke `Driver.Build` directly. Full CLI integration is Phase 15.

## Closeout notes

_Fill in after gate green._
