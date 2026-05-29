---
title: "11. Kotlin Multiplatform and Android"
sidebar_position: 12
sidebar_label: "11. KMP and Android"
description: "Kotlin Multiplatform artifact structure, commonMain vs JVM-specific API surface, Android AAR format (classes.jar + res/ + AndroidManifest.xml), the consumer path for AARs on non-Android hosts, and the Android-target bridge for mobile Mochi."
---

# 11. Kotlin Multiplatform and Android

MEP-70 targets the JVM variant of Kotlin artifacts. Two important artifact types extend beyond plain JVM JARs: Kotlin Multiplatform (KMP) libraries and Android libraries (AARs). This note documents how the bridge handles both.

## Kotlin Multiplatform artifacts

### The KMP publication model

A KMP library publishes multiple artifacts to Maven Central under the same group/artifact coordinates:

```
org.jetbrains.kotlinx:kotlinx-datetime:0.5.0
  ├── kotlinx-datetime-0.5.0.jar                  (JVM classes)
  ├── kotlinx-datetime-0.5.0-sources.jar           (JVM sources)
  ├── kotlinx-datetime-0.5.0.klib                  (Kotlin/Native metadata)
  ├── kotlinx-datetime-android-0.5.0.jar           (Android classes, may differ)
  ├── kotlinx-datetime-js-0.5.0-sources.jar        (Kotlin/JS sources)
  └── kotlinx-datetime-0.5.0.module                (Gradle Module Metadata)
```

The `.module` file (Gradle Module Metadata, GMM) declares all variants and their capabilities. The bridge reads the `.module` file to identify the JVM variant and fetches only the JVM JAR (and its sources JAR for KDoc extraction).

### `commonMain` vs JVM-specific API

KMP libraries split their code between:

- `commonMain`: platform-independent code, available on all targets.
- `jvmMain`: JVM-specific extensions and implementations.
- `androidMain`: Android-specific (may add `@JvmName` overloads).

The JVM JAR contains both `commonMain` and `jvmMain` classes. The bridge ingests the full JVM JAR; the distinction between `commonMain` and `jvmMain` is transparent.

However, if the user imports the same artifact for different Mochi targets (JVM host vs. Android host), the API surface may differ because `androidMain` adds different functions. The bridge handles this by selecting the variant that matches the `[kotlin] jvm-target` + `[kotlin] android-target` combination in the manifest.

### KMP source sets and `expect`/`actual`

KMP uses `expect`/`actual` declarations: `expect fun platformName(): String` is declared in `commonMain` and implemented in each platform's `*Main`. The bridge sees only the `actual` implementation in the JVM JAR, not the `expect` declaration. This is correct: the JVM ABI is determined by the `actual` class file.

### KMP non-JVM targets: out of scope for MEP-70

Kotlin/Native `.klib` artifacts and Kotlin/JS `.js` artifacts are out of scope for MEP-70. Consuming `.klib` artifacts would require parsing LLVM IR or Kotlin/Native binary format, which is a separate MEP. The bridge silently ignores non-JVM artifacts in a KMP publication.

## Android Library (AAR) format

### AAR structure

An Android Archive (`.aar`) is a ZIP with this structure:

```
my-library-1.0.0.aar
  ├── classes.jar               (JVM bytecode, same as a plain JAR)
  ├── res/                      (Android resource files: layout XML, drawables, etc.)
  ├── assets/                   (raw asset files)
  ├── AndroidManifest.xml       (permissions, components, min SDK version)
  ├── R.txt                     (resource ID declarations)
  ├── proguard.txt              (ProGuard/R8 rules)
  ├── lint.jar                  (optional: custom Lint rules)
  └── kotlin_module              (Kotlin metadata for the module)
```

### Consumer path for AARs on non-Android hosts

The bridge unpacks the AAR (treated as a ZIP), extracts `classes.jar`, and processes it identically to a plain JAR. The `res/`, `assets/`, and `AndroidManifest.xml` are ignored on non-Android hosts.

This means Android libraries that use only `classes.jar` APIs (pure Kotlin code, no Android framework calls) work transparently on JVM/Linux/macOS hosts. Libraries that call Android APIs (`android.content.Context`, `android.app.Activity`, etc.) will produce `extern type` declarations for those types but no callable functions (since the Android framework APIs are stubs on non-Android hosts). Using Android-specific functions on a non-Android host will cause a JNI call to throw `UnsatisfiedLinkError` at runtime; the bridge emits a warning for each such function at lock time.

### AAR consumer: what works on non-Android hosts

| Android library type | Non-Android host behaviour |
|---------------------|--------------------------|
| Pure Kotlin (no Android APIs) | Full compatibility. |
| Kotlin + Room (SQLite) | Works if SQLite is on the host; Room's Android annotations are ignored. |
| Kotlin + Retrofit | Works (pure JVM HTTP). |
| Kotlin + Ktor client | Works. |
| Kotlin + WorkManager | WorkManager requires Android scheduler; functions are stubs. |
| Kotlin + Jetpack Compose | Compose requires Android rendering; not usable. |
| Kotlin + Coroutines | Works (coroutines are pure Kotlin). |

### Android-target Mochi (Phase 14)

When the Mochi build target is Android (`mochi build --target=android-arm64`), the bridge uses the AAR's full `classes.jar` plus the Android stubs JAR (from the Android SDK) to compile the native image. The native image is compiled with the Android-specific GraalVM variant and packaged as a `.so` in the final APK. This path is Phase 14 of the delivery plan and is out of scope for Phase 0-13.

## `[kotlin-dependencies]` with AAR sources

The bridge auto-detects AARs by checking the Maven POM's `packaging` element (`<packaging>aar</packaging>`) before fetching. If the POM declares AAR packaging, the bridge fetches the `.aar` file instead of the `.jar` file and proceeds with the unpack-and-extract path.

The user does not need to know whether an artifact is a JAR or AAR; the `[kotlin-dependencies]` entry is the same either way:

```toml
[kotlin-dependencies]
"androidx.room:room-runtime" = "2.6.1"       # AAR, auto-detected
"com.squareup.retrofit2:retrofit" = "2.11.0" # JAR, auto-detected
```

## Gradle Module Metadata (GMM) and variant selection

Modern Maven Central artifacts ship with a `.module` file (Gradle Module Metadata 1.1 schema) that describes multiple artifact variants (JVM, Android, JS, Native, etc.) with their capabilities and attributes. The bridge reads the `.module` file if present to select the correct JVM variant:

1. Prefer the variant with `org.gradle.usage = "java-api"` and `org.jetbrains.kotlin.platform.type = "jvm"`.
2. Fall back to the variant with `org.gradle.usage = "java-api"` and no Kotlin platform attribute (Java-only library).
3. If no `.module` file exists, use the plain `<artifact>-<version>.jar` directly.

Variant selection respects the `classifier` key in `[kotlin-dependencies]` if set: a `classifier = "jdk8"` entry overrides the variant selection and fetches the classified JAR directly.

## Cross-references

- [[02-design-philosophy]] §Decision-2 for why Kotlin/Native is out of scope for MEP-70.
- [[04-kotlin-metadata-ingest]] for the class-file parsing pipeline that processes `classes.jar` from AARs identically to plain JARs.
- [MEP-70 §3](/docs/mep/mep-0070#3-scope-and-non-goals) for the formal scope boundary.
- [MEP-70 §10](/docs/mep/mep-0070#10-delivery-plan) for Phase 14 (Android AAR full target).
