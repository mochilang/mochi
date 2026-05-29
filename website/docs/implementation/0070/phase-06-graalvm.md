---
title: "Phase 06: GraalVM native image"
sidebar_position: 8
sidebar_label: "Phase 06: GraalVM"
description: "Drive native-image to compile the JNI wrapper JAR + all dependency JARs into a platform-native libwrap.so, emit the C header, and record the native-image-sha256 in mochi.lock."
---

# Phase 06: GraalVM native image

**Status:** Planned

## Deliverables

1. `package3/kotlin/graalvm/driver.go` — `CompileNativeImage(wrapperJAR string, depJARs []string, outputDir string, opts Options) error` — invoke `native-image` CLI with correct flags.
2. `package3/kotlin/graalvm/detect.go` — `FindNativeImage() (path string, version string, err error)` — locate `native-image` binary (PATH, `JAVA_HOME`, `GRAALVM_HOME`, platform-specific default paths), validate version matches `mochi.lock`.
3. `package3/kotlin/graalvm/header.go` — parse the generated `.h` file from `native-image --shared` to extract the `graal_create_isolate` signature and all `@CEntryPoint` function signatures.
4. `package3/kotlin/graalvm/isolate.go` — Go bindings for the GraalVM isolate lifecycle: `NewIsolate()`, `(*Isolate).AttachCurrentThread()`, `(*Thread).Detach()`, `(*Isolate).TearDown()`.
5. `package3/kotlin/graalvm/hash.go` — `HashNativeImage(soPath string) ([32]byte, error)` — SHA-256 of the compiled `libwrap.so`.

## `native-image` invocation flags

```bash
native-image \
  --shared \
  --no-fallback \
  -H:Name=libwrap \
  -H:Path=<outputDir> \
  -H:ReflectionConfigurationFiles=reflect-config.json \
  -H:ResourceConfigurationFiles=resource-config.json \
  --initialize-at-run-time=<runtime-init-classes> \
  -cp <wrapper.jar>:<dep1.jar>:<dep2.jar>:... \
  com.mochi.kotlin.bridge.Main
```

The `--shared` flag produces `libwrap.so` + `libwrap.h`. `--no-fallback` ensures the build fails rather than producing a fallback image that embeds a JVM (which would defeat the purpose).

## GraalVM version detection

```go
// graalvm/detect.go
func FindNativeImage() (path, version string, err error) {
    // 1. $GRAALVM_HOME/bin/native-image
    // 2. $JAVA_HOME/bin/native-image
    // 3. PATH lookup
    // 4. Platform defaults: /usr/lib/jvm/graalvm-ce-21/bin/native-image (Linux)
    //                       /Library/Java/JavaVirtualMachines/graalvm-ce-21/Contents/Home/bin/native-image (macOS)
}
```

If `native-image` is not found, the bridge prints installation instructions and exits with `ErrGraalVMNotFound`. The instructions include:

```
GraalVM is required for the Kotlin bridge but was not found.
Install GraalVM CE 21 from: https://github.com/graalvm/graalvm-ce-builds/releases
Or via SDKMAN: sdk install java 21.0.2-graalce
Or via Homebrew: brew install --cask graalvm-jdk
Set GRAALVM_HOME to the installation root.
```

## Gate

Compile `okhttp@4.12.0` wrapper JAR to `libwrap.so`:

1. `native-image` invocation succeeds without `--fallback` (no embedded JVM).
2. `libwrap.so` is produced and is a valid ELF shared library (Linux) or Mach-O dylib (macOS).
3. `libwrap.h` contains `graal_create_isolate` signature and at least one `com_squareup_okhttp3_*` entry point.
4. `HashNativeImage` returns a stable hash (same image produces same hash on same machine).
5. The Go-side `NewIsolate()` + `AttachCurrentThread()` + `Detach()` + `TearDown()` lifecycle calls succeed without crashes.
6. A simple JNI call through the isolate (call `okhttp_client_new()`, receive a handle) succeeds.
