# MEP-50 research note 10, Build system: Gradle, KGP, AGP, KMP

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies how `mochi build --target=kotlin-...` produces
Kotlin artefacts: how the codegen emits a Gradle KMP project, how the
Gradle wrapper drives the actual compilation, how cross-target
multiplatform builds work for JVM plus Android plus Kotlin/Native plus
Kotlin/JS plus Kotlin/Wasm, how Android `.aab` and `.apk` flow through
the Android Gradle Plugin (AGP), how iOS XCFrameworks integrate with
Swift Package Manager, how the runtime library is published to Maven
Central, and how the Mochi CI matrix is shaped around the toolchains
JetBrains ships versus the Android Studio bundles.

The companion notes for the Swift target ([[../0049/10-build-system]]),
the .NET target ([[../0048/10-build-system]]), and the JVM target
([[../0047/10-build-system]]) cover the same territory for those
runtimes; structurally this note mirrors them. Kotlin 2.1 is the floor
language version. KMP is the load-bearing surface per
the shared-decisions anchor, and the runtime described in [[04-runtime]] is
published as a KMP library to Maven Central.

---

## 1. Gradle as canonical build driver

Gradle (`./gradlew build`, `./gradlew test`, `./gradlew assembleRelease`,
`./gradlew bundleRelease`, `./gradlew publishToMavenCentral`) is the
canonical driver for every Mochi Kotlin target. The Mochi codegen does
not invoke `kotlinc` directly. We emit a deterministic `build.gradle.kts`,
`settings.gradle.kts`, and `gradle/libs.versions.toml` at the output root
and shell out to `./gradlew`, letting Gradle resolve the dependency
graph, schedule per-target compilation via the Kotlin Gradle Plugin
(KGP), and choose between incremental and clean rebuilds.

The reason for this layering matches MEP-47's choice to drive Gradle /
Maven rather than re-implement them and MEP-49's choice to drive SwiftPM
rather than `swiftc` directly: Gradle already handles toolchain
selection (Foojay toolchain resolver, `java { toolchain { ... } }`),
platform conditionals (per-target source set hierarchies), resource
processing (`processResources`), test discovery (`kotlin-test`, JUnit 5
Jupiter, Kotest), and cross-compilation to Kotlin/Native and
Kotlin/Wasm. Re-implementing any of that would create a parallel build
graph that drifts from upstream the moment Kotlin 2.2 ships in 2025 Q3.

We do not consider Bazel, Buck2, or Pants as primary drivers:

- **Bazel** has `rules_kotlin` and `rules_android` but neither tracks
  KMP. The KMP plugin is Gradle-only. Bazel cannot consume the KMP
  source-set hierarchy without re-implementing it; doing so would lock
  Mochi to a Bazel maintenance burden that the much larger Kotlin
  community absorbs through Gradle.
- **Buck2** has the same KMP gap as Bazel plus a smaller user base.
- **Pants** has a fledgling Kotlin backend but no AGP integration; the
  Android build chain is Gradle-only because AGP itself is implemented
  as a Gradle plugin.
- **Maven** has `kotlin-maven-plugin` but no KMP plugin; the JVM-only
  build path through Maven would force a separate driver for KMP.

Gradle is the only build tool whose first-party support covers every
Mochi Kotlin target. The decision is identical to MEP-47's JVM choice
(Gradle for KMP-adjacent JVM workflows) but more load-bearing here: KMP
forces the Gradle choice rather than just preferring it.

The Mochi binary expects a Java toolchain on `PATH`: `java --version`
must report >= 17. The Gradle wrapper bundles the Gradle binary itself,
so users do not need a system Gradle install. Kotlin compiler is pulled
through Gradle (via the `kotlin-multiplatform` plugin), so users do not
need a system `kotlinc` install either.

## 2. Gradle Kotlin DSL, never Groovy

Every emitted Gradle file is written in Kotlin DSL (`build.gradle.kts`,
`settings.gradle.kts`). The Mochi codegen never emits Groovy-syntax
`.gradle` files. The reasons:

- **Type safety.** Kotlin DSL gets full IDE completion, type checking,
  refactoring, and structural search in IntelliJ IDEA and Android
  Studio. Groovy DSL gets dynamic resolution and stale autocomplete.
- **Refactor surface.** The Kotlin DSL is itself Kotlin, so the codegen
  can reuse the same Kotlin pretty-printer (see [[05-codegen-design]])
  for both user source and build script emission.
- **JetBrains direction.** Gradle Kotlin DSL is the recommended path for
  new projects since Gradle 8.0 (2023 Q1). Android Studio's "New
  Project" wizard defaults to Kotlin DSL since Iguana (2024 Q1).
- **No Groovy runtime.** Kotlin DSL scripts compile to JVM bytecode and
  cache cleanly. Groovy DSL scripts require the Groovy runtime in the
  Gradle classpath and have worse cache behaviour.

The trade-off is build-script compile time: a Kotlin DSL script takes
~3 seconds to compile cold versus ~500ms for Groovy. The Gradle
configuration cache (`org.gradle.configuration-cache=true`, enabled by
default since Gradle 8.1) skips the recompile on warm builds, so the
cost only surfaces on first build per session.

The Mochi codegen emits a header comment marking the file as generated:

```kotlin
// Generated by mochi v1.0.0. Do not edit by hand; rerun `mochi build`
// to regenerate. Edits will be overwritten.
```

The header is the same convention as MEP-49's `Package.swift` header
(see [[../0049/10-build-system]] §25) and MEP-47's `pom.xml` header.

## 3. Generated project layout

The codegen produces a deterministic directory tree under
`target/kotlin/`:

```
target/kotlin/
|-- settings.gradle.kts
|-- build.gradle.kts
|-- gradle.properties
|-- gradle/
|   |-- libs.versions.toml
|   |-- wrapper/
|   |   |-- gradle-wrapper.jar
|   |   |-- gradle-wrapper.properties
|-- gradlew
|-- gradlew.bat
|-- src/
|   |-- commonMain/
|   |   |-- kotlin/
|   |   |   |-- mochi/user/...
|   |   |-- resources/
|   |-- commonTest/
|   |   |-- kotlin/
|   |-- jvmMain/
|   |   |-- kotlin/
|   |-- jvmTest/
|   |-- androidMain/
|   |   |-- kotlin/
|   |   |-- AndroidManifest.xml
|   |-- iosMain/
|   |-- iosTest/
|   |-- macosMain/
|   |-- linuxMain/
|   |-- mingwMain/
|   |-- jsMain/
|   |-- wasmJsMain/
```

`settings.gradle.kts` declares the KMP module structure, applies the
Foojay toolchain resolver, and configures `pluginManagement` and
`dependencyResolutionManagement`:

```kotlin
pluginManagement {
    repositories {
        gradlePluginPortal()
        google()
        mavenCentral()
    }
}

plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        mavenCentral()
    }
}

rootProject.name = "MochiUserApp"
```

`gradle/libs.versions.toml` is the version catalog (Gradle 7.0+) that
pins every dependency in one place:

```toml
[versions]
kotlin = "2.1.0"
agp = "8.7.3"
coroutines = "1.10.1"
serialization = "1.7.3"
datetime = "0.6.1"
collections-immutable = "0.3.8"
ktor = "3.0.1"
compose-multiplatform = "1.7.3"
android-minSdk = "24"
android-targetSdk = "35"
android-compileSdk = "35"

[libraries]
kotlinx-coroutines-core = { module = "org.jetbrains.kotlinx:kotlinx-coroutines-core", version.ref = "coroutines" }
kotlinx-coroutines-android = { module = "org.jetbrains.kotlinx:kotlinx-coroutines-android", version.ref = "coroutines" }
kotlinx-serialization-json = { module = "org.jetbrains.kotlinx:kotlinx-serialization-json", version.ref = "serialization" }
kotlinx-datetime = { module = "org.jetbrains.kotlinx:kotlinx-datetime", version.ref = "datetime" }
kotlinx-collections-immutable = { module = "org.jetbrains.kotlinx:kotlinx-collections-immutable", version.ref = "collections-immutable" }
ktor-client-core = { module = "io.ktor:ktor-client-core", version.ref = "ktor" }
ktor-client-cio = { module = "io.ktor:ktor-client-cio", version.ref = "ktor" }
ktor-client-okhttp = { module = "io.ktor:ktor-client-okhttp", version.ref = "ktor" }
ktor-client-darwin = { module = "io.ktor:ktor-client-darwin", version.ref = "ktor" }
ktor-client-curl = { module = "io.ktor:ktor-client-curl", version.ref = "ktor" }
ktor-client-winhttp = { module = "io.ktor:ktor-client-winhttp", version.ref = "ktor" }
ktor-client-js = { module = "io.ktor:ktor-client-js", version.ref = "ktor" }
mochi-runtime = { module = "io.mochi-lang:mochi-runtime", version = "1.0.0" }

[plugins]
kotlin-multiplatform = { id = "org.jetbrains.kotlin.multiplatform", version.ref = "kotlin" }
kotlin-android = { id = "org.jetbrains.kotlin.android", version.ref = "kotlin" }
kotlin-serialization = { id = "org.jetbrains.kotlin.plugin.serialization", version.ref = "kotlin" }
android-application = { id = "com.android.application", version.ref = "agp" }
android-library = { id = "com.android.library", version.ref = "agp" }
compose-multiplatform = { id = "org.jetbrains.compose", version.ref = "compose-multiplatform" }
```

`gradle/wrapper/gradle-wrapper.properties` pins the Gradle distribution:

```properties
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
distributionUrl=https\://services.gradle.org/distributions/gradle-8.11.1-bin.zip
distributionSha256Sum=f397b287023acdba1e9f6fc5ea72d22dd63669d59ed4a289a29b1a76eee151c6
networkTimeout=10000
validateDistributionUrl=true
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
```

The `distributionSha256Sum` line is the canonical way to lock the
Gradle distribution; if a mirror serves tampered bytes the wrapper
refuses to run. The codegen sources the SHA from a baked-in table that
ships with the Mochi binary so the user does not have to compute it.

The Gradle binary version is pinned to **8.11.1** as of MEP-50 v1. The
reasons:

- 8.11.1 was released 2024-11-13 and is the patch over 8.11 (which
  introduced configuration cache for parallel test execution).
- AGP 8.7 requires Gradle 8.9+; 8.11.1 is comfortably above.
- Kotlin 2.1.0 was tested against Gradle 8.10+ and works through 8.11.x.
- The configuration cache is stable enough for the KMP plugin only at
  Gradle 8.7+; we go higher for safety margin.

## 4. KMP plugin setup

The top-level `build.gradle.kts` applies the KMP plugin and configures
every target. The structure follows the JetBrains-canonical layout:

```kotlin
plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.android.library)
    alias(libs.plugins.kotlin.serialization)
}

group = "mochi.user"
version = "1.0.0"

kotlin {
    explicitApi()

    jvmToolchain(17)

    jvm {
        compilations.all {
            kotlinOptions {
                jvmTarget = "17"
                freeCompilerArgs += listOf(
                    "-Xjsr305=strict",
                    "-Xjvm-default=all",
                )
            }
        }
        testRuns["test"].executionTask.configure {
            useJUnitPlatform()
        }
    }

    androidTarget {
        publishLibraryVariants("release")
        compilations.all {
            kotlinOptions {
                jvmTarget = "17"
            }
        }
    }

    iosArm64()
    iosSimulatorArm64()
    iosX64()
    macosArm64()
    macosX64()
    linuxX64 {
        binaries.executable {
            entryPoint = "mochi.user.main"
        }
    }
    linuxArm64()
    mingwX64 {
        binaries.executable {
            entryPoint = "mochi.user.main"
        }
    }
    js(IR) {
        browser()
        nodejs()
        binaries.executable()
    }
    @OptIn(org.jetbrains.kotlin.gradle.ExperimentalWasmDsl::class)
    wasmJs {
        browser()
        binaries.executable()
    }

    applyDefaultHierarchyTemplate()

    sourceSets {
        commonMain.dependencies {
            implementation(libs.kotlinx.coroutines.core)
            implementation(libs.kotlinx.serialization.json)
            implementation(libs.kotlinx.datetime)
            implementation(libs.kotlinx.collections.immutable)
            implementation(libs.ktor.client.core)
            implementation(libs.mochi.runtime)
        }
        commonTest.dependencies {
            implementation(kotlin("test"))
        }
        jvmMain.dependencies {
            implementation(libs.ktor.client.okhttp)
        }
        androidMain.dependencies {
            implementation(libs.kotlinx.coroutines.android)
            implementation(libs.ktor.client.okhttp)
        }
        iosMain.dependencies {
            implementation(libs.ktor.client.darwin)
        }
        macosMain.dependencies {
            implementation(libs.ktor.client.darwin)
        }
        linuxMain.dependencies {
            implementation(libs.ktor.client.curl)
        }
        mingwMain.dependencies {
            implementation(libs.ktor.client.winhttp)
        }
        jsMain.dependencies {
            implementation(libs.ktor.client.js)
        }
    }
}

android {
    namespace = "mochi.user"
    compileSdk = libs.versions.android.compileSdk.get().toInt()
    defaultConfig {
        minSdk = libs.versions.android.minSdk.get().toInt()
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}
```

`explicitApi()` is the Kotlin 1.4+ feature that requires every public
declaration to be explicitly annotated with `public`, `internal`,
`private`, or `protected`. Mochi codegen always emits explicit
visibility, so this is a defensive gate: any leak of an implicit public
declaration is caught at compile time.

`applyDefaultHierarchyTemplate()` is the Kotlin 1.9.20+ shorthand for
the standard source-set hierarchy (commonMain to nativeMain to
appleMain to iosMain, etc.). Before 1.9.20 the user had to wire each
`dependsOn` edge manually; the template makes the wiring deterministic.
Kotlin 2.1 made the template the canonical path; legacy manual wiring
is still supported but discouraged.

The KMP plugin version is **2.1.0** as of MEP-50 v1. Kotlin 2.1.0 was
released 2024-11-27. The plugin ID is
`org.jetbrains.kotlin.multiplatform` (the singular `kotlin` plugin
ID, `org.jetbrains.kotlin.jvm`, only configures JVM and is too narrow
for our KMP-full output).

## 5. Per-target setup blocks

Each KMP target gets a configuration block in the `kotlin { }` extension.
Mochi emits the full set for KMP-full output:

- **`jvm { }`**: Kotlin/JVM target. JVM bytecode 17. Test runner uses
  JUnit 5 Jupiter via `useJUnitPlatform()`.
- **`androidTarget { }`**: Kotlin/Android target. Publishes the `release`
  library variant by default. Bytecode 17. Requires the AGP
  `com.android.library` plugin (or `com.android.application` for app
  modules).
- **`iosArm64()`**: iOS device, 64-bit ARM. The framework export goes
  here.
- **`iosSimulatorArm64()`**: iOS Simulator on Apple Silicon.
- **`iosX64()`**: iOS Simulator on Intel Mac (legacy, Rosetta only). We
  emit this for completeness but the matrix excludes it by default.
- **`macosArm64()`**: macOS on Apple Silicon.
- **`macosX64()`**: macOS on Intel.
- **`linuxX64 { }`**: Linux x86_64. Configured with
  `binaries.executable { entryPoint = "mochi.user.main" }` so the
  output is a single statically-linkable executable.
- **`linuxArm64()`**: Linux aarch64. Same `binaries.executable` config.
- **`mingwX64 { }`**: Windows x86_64 via MinGW. Produces a `.exe`.
- **`js(IR) { browser(); nodejs() }`**: Kotlin/JS via the IR backend.
  Targets both browser (via webpack) and Node.js. Legacy backend is
  rejected (it was removed in Kotlin 1.9).
- **`wasmJs { browser() }`**: Kotlin/Wasm via the Wasm GC target. Browser
  only for v1; standalone Wasm runtimes (wasmtime, wasmer) require the
  WASI binding layer which is not yet stable in Kotlin/Wasm.

Targets that are not requested by the Mochi user are omitted from the
emitted `build.gradle.kts` to keep the dependency graph minimal. The
Mochi build driver detects target needs from the Mochi source (e.g.,
`import android.app` triggers `androidTarget`, `import platform.UIKit`
triggers `iosArm64`).

## 6. Dependency resolution

The runtime dependency set is pinned in `libs.versions.toml`. Per-target
dependency selection happens in the `sourceSets { }` block:

```kotlin
sourceSets {
    commonMain.dependencies {
        implementation(libs.kotlinx.coroutines.core)
        implementation(libs.kotlinx.serialization.json)
        implementation(libs.kotlinx.datetime)
        implementation(libs.kotlinx.collections.immutable)
        implementation(libs.ktor.client.core)
    }
    jvmMain.dependencies {
        implementation(libs.ktor.client.okhttp)
    }
    androidMain.dependencies {
        implementation(libs.kotlinx.coroutines.android)
        implementation(libs.ktor.client.okhttp)
    }
    iosMain.dependencies {
        implementation(libs.ktor.client.darwin)
    }
    linuxMain.dependencies {
        implementation(libs.ktor.client.curl)
    }
    mingwMain.dependencies {
        implementation(libs.ktor.client.winhttp)
    }
    jsMain.dependencies {
        implementation(libs.ktor.client.js)
    }
}
```

Per-target Ktor engine choices follow the Ktor docs:

| Target       | Engine        | Notes                                    |
|--------------|---------------|------------------------------------------|
| JVM          | `OkHttp`      | HTTP/2 support, mature                   |
| Android      | `OkHttp`      | Same as JVM, Android-tuned               |
| iOS / macOS  | `Darwin`      | Native NSURLSession backend              |
| Linux        | `Curl`        | Native libcurl backend                   |
| Windows      | `WinHttp`     | Native Win32 backend                     |
| JS           | `Js`          | Fetch API or XHR                         |
| Wasm-JS      | `Js`          | Fetch via JS interop                     |

The runtime libraries are pinned to:

- **kotlinx.coroutines: 1.10.1** (released 2024-12-19). Drops the K1
  frontend, requires Kotlin 2.0+. Tested against Kotlin 2.1.
- **kotlinx.serialization: 1.7.3** (released 2024-10-22). Supports
  JSON, CBOR, ProtoBuf, ProtoBuf with `@OptIn`, HOCON.
- **kotlinx.datetime: 0.6.1** (released 2024-10-15). The 1.0
  release is still under draft; 0.6.x is ABI-stable per JetBrains.
- **kotlinx.collections.immutable: 0.3.8** (released 2024-10-04).
  Pre-1.0 but ABI-stable per JetBrains.
- **Ktor: 3.0.1** (released 2024-10-22). 3.0 is the major bump that
  added Wasm-JS support and dropped Kotlin 1.9 compatibility.

## 7. Android Gradle Plugin (AGP) setup

For library output (the default Mochi `mochi build` shape), the
`com.android.library` AGP plugin handles the Android source set:

```kotlin
android {
    namespace = "mochi.user"
    compileSdk = 35
    defaultConfig {
        minSdk = 24
        // targetSdk is set on the application module, not library
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    buildFeatures {
        compose = false
        viewBinding = false
        buildConfig = false
    }
    lint {
        warningsAsErrors = true
        abortOnError = true
    }
}
```

For application output (Mochi `@app` declaration, see Phase 15 in
[[11-testing-gates]]), the `com.android.application` plugin is applied
instead and the configuration adds:

```kotlin
android {
    namespace = "mochi.user.app"
    compileSdk = 35
    defaultConfig {
        applicationId = "mochi.user.app"
        minSdk = 24
        targetSdk = 35
        versionCode = 1
        versionName = "1.0.0"
    }
    buildTypes {
        release {
            isMinifyEnabled = true
            isShrinkResources = true
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
            signingConfig = signingConfigs.getByName("release")
        }
        debug {
            isMinifyEnabled = false
            signingConfig = signingConfigs.getByName("debug")
        }
    }
    signingConfigs {
        create("release") {
            storeFile = file(System.getenv("MOCHI_KEYSTORE_PATH") ?: "release.keystore")
            storePassword = System.getenv("MOCHI_KEYSTORE_PASSWORD")
            keyAlias = System.getenv("MOCHI_KEY_ALIAS") ?: "mochi"
            keyPassword = System.getenv("MOCHI_KEY_PASSWORD")
        }
    }
    buildFeatures {
        compose = true
    }
    composeOptions {
        kotlinCompilerExtensionVersion = "1.7.3"
    }
}
```

AGP version is **8.7.3** as of MEP-50 v1. AGP 8.7.0 shipped 2024-10
and requires:

- Android Studio Ladybug (2024.2.1) or newer.
- Kotlin 2.0+ (we pin 2.1).
- Gradle 8.9+.
- JDK 17+.
- compileSdk 35 (Android 15) as the minimum compile target.

Compose Compiler is decoupled from the Kotlin compiler since Kotlin
2.0 (the Compose Compiler plugin moved into the Kotlin repository at
2.0). We pin Compose Multiplatform to 1.7.3 (released 2024-12) for
all Compose-eligible fixtures.

`minSdk = 24` is Android 7.0 Nougat (2016-08). The rationale:

- 24 is the floor where ART (Android Runtime) is stable enough to
  ignore Dalvik quirks.
- 24 supports Java 8 lambdas and `java.util.function.*` natively;
  pre-24 needed D8 desugaring.
- Google Play's distribution data (2026 Q1) shows >99.5% of active
  devices on Android 7.0+; the 0.5% tail is rounding error.

`targetSdk = 35` is Android 15 (2024-10). Google Play requires apps to
target the latest SDK within ~1 year of its release.

## 8. iOS framework export via XCFramework

KMP's iOS target produces a `.framework` per Native target
(`iosArm64`, `iosSimulatorArm64`, `iosX64`). The `XCFramework` task
bundles them into a single `.xcframework` that Apple Xcode and Swift
Package Manager consume:

```kotlin
import org.jetbrains.kotlin.gradle.plugin.mpp.apple.XCFramework

kotlin {
    val xcf = XCFramework("MochiUserApp")
    listOf(iosArm64(), iosSimulatorArm64(), iosX64()).forEach {
        it.binaries.framework {
            baseName = "MochiUserApp"
            xcf.add(this)
            isStatic = true
        }
    }
}
```

The `assembleMochiUserAppXCFramework` Gradle task produces
`build/XCFrameworks/release/MochiUserApp.xcframework`, which an Xcode
project consumes via "Add Files" or a Swift Package Manager binary
target:

```swift
// Package.swift (consumer side)
.binaryTarget(
    name: "MochiUserApp",
    url: "https://github.com/mochilang/mochi-user-app/releases/download/1.0.0/MochiUserApp.xcframework.zip",
    checksum: "..."
)
```

The legacy `cocoapods { }` block (Cocoapods plugin shipped with KMP
since 1.3) is **deprecated** as of Kotlin 2.0; new projects should use
the SPM binary-target path. The Mochi codegen emits SPM by default and
the Cocoapods path only if the user passes `--ios-pod` to `mochi
build`. The deprecation is signalled but not enforced; Cocoapods still
works through Kotlin 2.1.

The framework is configured `isStatic = true` so the consuming Xcode
project gets a static framework (no separate dynamic library to ship).
This matches MEP-49's static-library default in
[[../0049/10-build-system]] §4 and gives App Store reviewers a single
Mach-O slice to evaluate.

## 9. Static Linux binary via Kotlin/Native

The `linuxX64 { }` and `linuxArm64()` targets produce single-binary
executables. Configured with `binaries.executable { ... }`:

```kotlin
linuxX64 {
    binaries.executable {
        entryPoint = "mochi.user.main"
        baseName = "mochi-user-app"
        runTask?.args = emptyList<String>()
        // Strip symbols for size
        freeCompilerArgs += listOf("-Xstrip-debug-info")
    }
}
```

The `linkReleaseExecutableLinuxX64` task produces
`build/bin/linuxX64/releaseExecutable/mochi-user-app.kexe`. The `.kexe`
extension is a Kotlin/Native quirk; the file is a real ELF executable
and we rename to `mochi-user-app` at the Mochi build driver layer.

Kotlin/Native links statically against the Kotlin runtime by default;
the only dynamic dependencies are the system libc and libpthread. For
a fully static binary (musl-libc, no dynamic linker), Kotlin/Native
gained `-Xstatic` support in Kotlin 2.1 but the path is still
experimental. MEP-50 v1 ships dynamic-against-glibc binaries for
Linux; a future iteration can add the musl path.

Binary size for hello-world on `linuxX64` with `-Xstrip-debug-info`:
~3.5 MB versus ~12 MB for the Swift static binary (per
[[../0049/10-build-system]] §8). Kotlin/Native's runtime is leaner than
Swift's Foundation-bundled runtime.

## 10. Windows .exe

The `mingwX64 { }` target produces a Windows PE32+ executable. Same
shape as Linux:

```kotlin
mingwX64 {
    binaries.executable {
        entryPoint = "mochi.user.main"
        baseName = "mochi-user-app"
    }
}
```

The output is `build/bin/mingwX64/releaseExecutable/mochi-user-app.exe`.
The MinGW path links against the MinGW-w64 runtime; we do not target
MSVC ABI directly (Kotlin/Native does not support MSVC linking as of
Kotlin 2.1).

Code signing for Authenticode happens outside Gradle, via `signtool.exe`
from the Windows SDK. The Mochi build driver wraps the signing step:

```
mochi build --target=kotlin-windows-x64 \
  --sign-cert authenticode.pfx \
  --sign-password "$SIGN_PASSWORD"
```

The signing pipeline mirrors MEP-49's Windows code-signing flow in
[[../0049/10-build-system]] §18.

## 11. Kotlin/JS

The `js(IR) { browser(); nodejs() }` target produces JavaScript
bundles. The IR backend is the only supported backend; the legacy
backend was removed in Kotlin 1.9.

```kotlin
js(IR) {
    browser {
        commonWebpackConfig {
            cssSupport { enabled.set(true) }
            outputFileName = "mochi-user-app.js"
        }
        testTask {
            useKarma { useChromeHeadless() }
        }
    }
    nodejs()
    binaries.executable()
}
```

NPM dependencies surface via the `implementation(npm("...", "..."))`
DSL:

```kotlin
sourceSets {
    jsMain.dependencies {
        implementation(npm("axios", "1.7.7"))
        implementation(npm("uuid", "11.0.3"))
    }
}
```

The KMP plugin generates a `package.json` from these declarations and
invokes `yarn install` (Yarn 1.x is bundled with the KMP plugin) to
materialise the `node_modules/`. The cache lives at
`build/js/node_modules/` and is content-addressed by the merged
`package.json`.

The webpack output goes to `build/dist/js/productionExecutable/`. For
production builds the plugin runs webpack in production mode (minified,
tree-shaken). For development builds (`./gradlew jsBrowserDevelopmentRun`)
webpack runs in dev mode with a webpack-dev-server on
`http://localhost:8080/`.

## 12. Kotlin/Wasm

The `wasmJs { browser() }` target produces WebAssembly modules with
the Wasm GC proposal:

```kotlin
@OptIn(org.jetbrains.kotlin.gradle.ExperimentalWasmDsl::class)
wasmJs {
    browser {
        commonWebpackConfig {
            outputFileName = "mochi-user-app.wasm.js"
        }
    }
    binaries.executable()
}
```

The `@OptIn(ExperimentalWasmDsl)` annotation is required because the
DSL is still flagged experimental in Kotlin 2.1. The Wasm GC target
itself is **Alpha** (as of Kotlin 2.0) / **Beta** (as of Kotlin 2.1.20
which shipped 2025-03). MEP-50 ships Wasm as a preview target with
the caveat in [[12-risks-and-alternatives]] R1.

Binary size optimisation requires Dead Code Elimination (DCE):

```kotlin
wasmJs {
    browser {
        commonWebpackConfig {
            mode = org.jetbrains.kotlin.gradle.targets.js.webpack.KotlinWebpackConfig.Mode.PRODUCTION
        }
    }
    binaries.executable {
        optimization {
            dceEnabled.set(true)
        }
    }
}
```

The Wasm GC target requires a browser that supports the Wasm GC
proposal: Chrome 119+, Firefox 120+, Safari 18.2+. Older browsers
fail at module instantiation. MEP-50 documents this caveat at the
fixture gate level (see [[11-testing-gates]] §10).

There is no standalone-Wasm path in MEP-50 v1; that would require WASI
support which is not yet stable in Kotlin/Wasm.

## 13. The `mochi build --target=kotlin-android` CLI flag

The Mochi build driver maps `--target=kotlin-...` to specific Gradle
task invocations:

| Mochi flag                              | Gradle invocation                                              |
|-----------------------------------------|----------------------------------------------------------------|
| `--target=kotlin-jvm`                   | `./gradlew jvmJar`                                             |
| `--target=kotlin-jvm-fatjar`            | `./gradlew shadowJar` (Shadow plugin)                          |
| `--target=kotlin-android`               | `./gradlew assembleRelease bundleRelease`                      |
| `--target=kotlin-android-debug`         | `./gradlew assembleDebug`                                      |
| `--target=kotlin-ios`                   | `./gradlew assembleMochiUserAppXCFramework`                    |
| `--target=kotlin-macos`                 | `./gradlew linkReleaseExecutableMacosArm64`                    |
| `--target=kotlin-linux-x64`             | `./gradlew linkReleaseExecutableLinuxX64`                      |
| `--target=kotlin-linux-arm64`           | `./gradlew linkReleaseExecutableLinuxArm64`                    |
| `--target=kotlin-windows-x64`           | `./gradlew linkReleaseExecutableMingwX64`                      |
| `--target=kotlin-js`                    | `./gradlew jsBrowserProductionWebpack jsNodeProductionRun`     |
| `--target=kotlin-wasm-js`               | `./gradlew wasmJsBrowserProductionWebpack`                     |
| `--target=kotlin-all`                   | `./gradlew build`                                              |

The driver shells out to `./gradlew` (using the wrapper, not a system
Gradle). Output artefacts are copied to `target/kotlin/dist/` after the
Gradle task completes:

```
target/kotlin/dist/
|-- mochi-user-app.jar           # JVM
|-- mochi-user-app-fat.jar       # JVM with bundled deps
|-- mochi-user-app.aab           # Android App Bundle
|-- mochi-user-app.apk           # Android APK
|-- MochiUserApp.xcframework/    # iOS / macOS XCFramework
|-- mochi-user-app-linux-x64     # Linux ELF
|-- mochi-user-app-windows-x64.exe   # Windows PE
|-- mochi-user-app.js            # Kotlin/JS bundle
|-- mochi-user-app.wasm          # Kotlin/Wasm module
```

## 14. Toolchain selection

The Mochi binary depends on:

1. **JDK 17+** for Gradle and `kotlinc` (which runs on the JVM).
2. **Android SDK** for AGP (only when targeting Android).
3. **Xcode** for iOS framework export (only when targeting iOS, and
   only on macOS host).
4. **MinGW-w64** for Windows cross-compilation from non-Windows hosts
   (Kotlin/Native bundles this for `mingwX64`).

**JDK selection.** Recommendation: bundle a JDK with the Mochi binary.
We pick **Eclipse Temurin 17 LTS** (released 2021-09, supported through
2029) because:

- Temurin is the AdoptOpenJDK successor, hosted at adoptium.net.
- Apache-2.0 + GPLv2 with Classpath Exception licensing, compatible
  with bundling.
- Native binaries for Linux x64/arm64, macOS arm64/x64, Windows x64.
- No telemetry, no usage tracking, no Oracle field-of-use restrictions.

The bundled JDK lives at `<mochi-install>/jdk/`. Gradle picks it up via
the `JAVA_HOME` environment variable set by the Mochi launcher script,
or via the Foojay toolchain resolver (which queries Foojay's API for
toolchain candidates).

Total bundled JDK size: ~180 MB (compressed ~50 MB). The Mochi binary
size grows from ~30 MB (vm3 only) to ~80 MB (with Kotlin support).
Acceptable for a transpiler-bundled distribution.

**Android SDK selection.** Not bundled. The Android SDK is too large
(~3 GB minimum) and Google licenses it under terms incompatible with
redistribution. The Mochi build driver locates the SDK via:

1. `ANDROID_HOME` environment variable.
2. `ANDROID_SDK_ROOT` environment variable.
3. `local.properties` file in the project root (`sdk.dir=...`).
4. Platform defaults: `~/Android/Sdk` (Linux),
   `~/Library/Android/sdk` (macOS), `%LOCALAPPDATA%\Android\Sdk`
   (Windows).

If no SDK is found, the driver downloads `commandlinetools-...-latest.zip`
from `dl.google.com` and runs `sdkmanager --install "platforms;android-35"
"build-tools;35.0.0"` to materialise the minimum SDK. The download
prompts the user to accept the Android SDK license; CI mode
(`MOCHI_ACCEPT_LICENSES=1`) skips the prompt.

**Xcode selection.** Not bundled (Apple terms prohibit redistribution).
Required only for `--target=kotlin-ios`. Mochi probes
`xcrun --find swift` to confirm Xcode is installed; missing Xcode on a
macOS host targeting iOS fails fast with a clear error message.

**Gradle wrapper.** Always bundled with the emitted project. Users
never need a system Gradle install; the wrapper downloads the pinned
Gradle distribution to `~/.gradle/wrapper/dists/` on first run and
caches forever.

## 15. Cache hierarchy

The build pipeline has three cache layers:

**Layer 1: Gradle build cache.** Enabled by default since Gradle 8.0
via `org.gradle.caching=true` in `gradle.properties`. Content-addressed
by task inputs (source hash, classpath hash, build script hash). Cache
location: `~/.gradle/caches/build-cache-1/`. Mochi configures the cache
to a project-local path on CI (`./.gradle-cache/`) for hermetic
behaviour.

```properties
# gradle.properties
org.gradle.caching=true
org.gradle.configuration-cache=true
org.gradle.parallel=true
org.gradle.daemon=false
org.gradle.jvmargs=-Xmx2g -XX:MaxMetaspaceSize=512m
```

**Layer 2: Kotlin compiler daemon.** The KGP runs `kotlinc` in a
long-lived daemon process to skip JVM startup on every compile. The
daemon listens on a Unix socket / Windows named pipe. On CI we disable
the daemon (`kotlin.compiler.execution.strategy=in-process`) for
hermetic behaviour; on dev machines we leave it on for the warm-build
speedup.

**Layer 3: Mochi content-addressed cache.** The Mochi build driver
caches transpiler output keyed by:

- BLAKE3 hash of the Mochi source files in dependency order.
- BLAKE3 hash of the transpiler version (`mochi --version` string).
- BLAKE3 hash of the target identifier (`kotlin-jvm`, `kotlin-android`,
  etc.).

Cache location: `~/.mochi/cache/kotlin/` keyed by composite hash. A
warm cache hit skips the entire codegen and Gradle pipeline; the
output `.jar` / `.aab` / `.kexe` is copied from cache to `target/kotlin/`.

Cache invalidation is automatic on Mochi source change, transpiler
upgrade, or target change. Users can force-clean via `mochi build
--clean`.

## 16. Hermetic builds

Hermetic builds are the property that two runs of `mochi build` produce
byte-identical output regardless of host state. The hermetic guarantees
for Kotlin target:

- **Network isolation.** Once dependencies are resolved (first build
  populates `~/.gradle/caches/modules-2/`), subsequent builds use
  `./gradlew --offline` to forbid network access. The Mochi CI matrix
  pre-populates the cache from a checked-in `gradle.lockfile` plus a
  local Maven mirror.
- **Toolchain pinning.** The bundled JDK is at a fixed path; Gradle's
  toolchain resolver always picks it. Kotlin compiler version is pinned
  in `libs.versions.toml`. AGP version is pinned. Gradle version is
  pinned in `gradle-wrapper.properties`.
- **No daemon.** `org.gradle.daemon=false` on CI ensures every build
  starts from a fresh JVM. The Kotlin compiler is run in-process.
- **Deterministic ordering.** `kotlinc` is deterministic when given the
  same inputs in the same order; Gradle's task scheduler is
  deterministic when configured with `org.gradle.parallel=false` on CI.
  We accept the wall-clock cost.

Hermetic-build verification: the Phase 16 gate ([[11-testing-gates]] §12)
rebuilds the same fixture on two different CI runners and verifies the
`.jar` / `.aab` / `.kexe` SHA-256 match.

## 17. Reproducibility

Beyond hermetic builds, reproducibility requires:

- **Pinned versions.** Every dependency in `libs.versions.toml` has an
  exact version string (`1.10.1`, not `1.10.+` or `latest.release`).
- **Lockfile.** Gradle 8.0+ supports `dependency-locking { lockAllConfigurations() }`
  which writes a `gradle.lockfile` per configuration. The Mochi build
  driver writes the lockfile on first build and checks it on every
  subsequent build.
- **Deterministic timestamps.** Jar/Aar archives embed file mtimes.
  Mochi sets `SOURCE_DATE_EPOCH` (the Reproducible Builds standard) to
  the most recent Mochi source file mtime, and configures Gradle's
  `Jar` task with `preserveFileTimestamps = false` and
  `reproducibleFileOrder = true`:

```kotlin
tasks.withType<Jar>().configureEach {
    isPreserveFileTimestamps = false
    isReproducibleFileOrder = true
}
```

- **No build IDs.** The K/Native linker embeds a random build ID by
  default; we pass `-Xlinker --build-id=none` to suppress it.
- **No host paths.** kotlinc embeds the absolute path of source files
  in debug info; we pass `-Xklib-relative-path-base=.` to make paths
  relative.

The reproducibility gate is detailed in [[11-testing-gates]] §12.

## 18. Android signing

Android requires every `.apk` / `.aab` to be signed before installation.
The signing configuration is in the `android { signingConfigs { } }`
block:

```kotlin
android {
    signingConfigs {
        create("release") {
            storeFile = file(System.getenv("MOCHI_KEYSTORE_PATH") ?: "release.keystore")
            storePassword = System.getenv("MOCHI_KEYSTORE_PASSWORD")
            keyAlias = System.getenv("MOCHI_KEY_ALIAS") ?: "mochi"
            keyPassword = System.getenv("MOCHI_KEY_PASSWORD")
        }
        getByName("debug") {
            // Default Android debug keystore
            storeFile = file(System.getProperty("user.home") + "/.android/debug.keystore")
            storePassword = "android"
            keyAlias = "androiddebugkey"
            keyPassword = "android"
        }
    }
}
```

The release keystore is **not** stored in the project (it would be a
catastrophic credential leak). For local dev, the user provides
keystore credentials via environment variables. For CI, the keystore is
imported from a base64-encoded GitHub secret:

```yaml
- name: Decode release keystore
  run: |
    echo "$RELEASE_KEYSTORE_BASE64" | base64 -d > release.keystore
  env:
    RELEASE_KEYSTORE_BASE64: ${{ secrets.RELEASE_KEYSTORE_BASE64 }}
```

For Mochi's own release pipeline, the keystore lives in a 1Password
vault accessed via the 1Password GitHub Action.

The signing scheme is **APK Signature Scheme v3** (Android 9+) plus
**v4** (Android 11+) for incremental install. AGP 8.7 enables both by
default; the codegen does not override.

For Google Play upload, the **app signing key** is a separate concept:
Google Play Console manages a single app signing key per-app while the
developer signs with an **upload key**. The upload key signs the `.aab`
that Google Play accepts; Google Play re-signs with the app signing key
before distribution. This separation lets the developer lose the
upload key without losing the app (Google can re-issue an upload key).

## 19. Maven Central publishing

The Mochi runtime library is published to Maven Central as
`io.mochi-lang:mochi-runtime:1.0.0`. The recommended plugin is
`com.vanniktech.maven.publish` (NOT the default `maven-publish` plugin
which has known bugs with KMP):

```kotlin
plugins {
    id("com.vanniktech.maven.publish") version "0.30.0"
}

mavenPublishing {
    publishToMavenCentral(SonatypeHost.CENTRAL_PORTAL)
    signAllPublications()

    coordinates("io.mochi-lang", "mochi-runtime", "1.0.0")

    pom {
        name.set("Mochi Runtime for Kotlin")
        description.set("Kotlin Multiplatform runtime library for Mochi.")
        url.set("https://github.com/mochilang/mochi-runtime-kotlin")
        licenses {
            license {
                name.set("Apache-2.0")
                url.set("https://www.apache.org/licenses/LICENSE-2.0")
            }
        }
        developers {
            developer {
                id.set("mochilang")
                name.set("Mochi Lang")
                email.set("release@mochi-lang.io")
            }
        }
        scm {
            url.set("https://github.com/mochilang/mochi-runtime-kotlin")
            connection.set("scm:git:git://github.com/mochilang/mochi-runtime-kotlin.git")
            developerConnection.set("scm:git:ssh://git@github.com/mochilang/mochi-runtime-kotlin.git")
        }
    }
}
```

The `com.vanniktech.maven.publish` plugin (developed by Niklas Baudy of
Square) handles:

- POM generation with the right metadata for Maven Central.
- GPG signing of all artefacts (Maven Central requires signed JARs).
- Upload to Sonatype Central Portal (the new Maven Central endpoint
  that replaced OSSRH in mid-2024).
- KMP-aware publication (one publication per KMP target plus a `kotlinMultiplatform`
  meta-publication).

Reasons to prefer vanniktech over the default `maven-publish`:

- The default plugin requires hand-rolled `signing { }` and
  `publishing { }` blocks per target; vanniktech wraps that.
- The default plugin has open issues against KMP module metadata; we
  validated vanniktech 0.30 against KMP 2.1.
- vanniktech tracks Sonatype's Central Portal API changes (the API
  changed significantly in 2024 when Sonatype migrated off OSSRH).

Namespace claim: **`io.mochi-lang`** at Sonatype Central Portal. Claimed
2025-Q1 ahead of MEP-50 v1 ship to avoid Sonatype's manual review
process at release time. The namespace also includes
`io.mochi-lang.kotlin` for KMP-specific helpers.

## 20. Compose Multiplatform integration

For Mochi `view` declarations (a candidate v2 feature not in MEP-50 v1
scope per [[12-risks-and-alternatives]] R14), Compose Multiplatform is
the target UI framework:

```kotlin
plugins {
    alias(libs.plugins.compose.multiplatform)
}

kotlin {
    sourceSets {
        commonMain.dependencies {
            implementation(compose.runtime)
            implementation(compose.foundation)
            implementation(compose.material3)
            implementation(compose.ui)
        }
    }
}

compose {
    desktop {
        application {
            mainClass = "mochi.user.MainKt"
            nativeDistributions {
                targetFormats(TargetFormat.Dmg, TargetFormat.Msi, TargetFormat.Deb)
                packageName = "mochi-user-app"
                packageVersion = "1.0.0"
            }
        }
    }
}
```

Compose Multiplatform 1.7.3 supports Android, iOS, JVM desktop (macOS,
Linux, Windows), Web (via wasmJs target). Compose iOS is still Beta as
of 1.7 (per [[12-risks-and-alternatives]] R14); MEP-50 documents this
caveat but does not block on it.

The `compose.material3` library is the Material 3 design system; Mochi
defaults to Material 3 for `view` declarations. Material 2 is also
available via `compose.material` for users targeting older Android
themes.

## 21. CI matrix

The Mochi project ships its runtime plus the `mochi` binary from a
matrix that covers every target we promise. GitHub Actions is the
canonical CI provider (matches the JVM and .NET targets):

```yaml
name: Kotlin CI
on: [push, pull_request]
jobs:
  jvm:
    strategy:
      matrix:
        os: [ubuntu-24.04, macos-15, windows-2022]
        kotlin: [2.0.21, 2.1.0]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: temurin
          java-version: 17
      - uses: gradle/actions/setup-gradle@v4
      - run: mochi build --target=kotlin-jvm
        env:
          MOCHI_KOTLIN_VERSION: ${{ matrix.kotlin }}
      - run: mochi test --target=kotlin-jvm

  android:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: temurin
          java-version: 17
      - uses: android-actions/setup-android@v3
      - run: mochi build --target=kotlin-android
      - uses: ReactiveCircus/android-emulator-runner@v2
        with:
          api-level: 35
          target: google_apis
          arch: x86_64
          script: ./gradlew connectedCheck

  native:
    strategy:
      matrix:
        include:
          - os: ubuntu-24.04
            target: kotlin-linux-x64
          - os: ubuntu-24.04-arm
            target: kotlin-linux-arm64
          - os: macos-15
            target: kotlin-macos
          - os: macos-15
            target: kotlin-ios
          - os: windows-2022
            target: kotlin-windows-x64
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: temurin
          java-version: 17
      - run: mochi build --target=${{ matrix.target }}

  js-wasm:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: temurin
          java-version: 17
      - uses: actions/setup-node@v4
        with:
          node-version: 20
      - run: mochi build --target=kotlin-js
      - run: mochi build --target=kotlin-wasm-js
```

The Android emulator job uses `ReactiveCircus/android-emulator-runner`
which boots a KVM-accelerated emulator on the CI runner. KVM
acceleration requires `nested-virt`-capable runners; GitHub's
`ubuntu-24.04` runners support this since 2024-Q2.

iOS device testing (Phase 12 in [[11-testing-gates]] §10) runs on a
self-hosted macOS runner with a physical iPhone attached, identical to
MEP-49's setup in [[../0049/10-build-system]] §22.

## 22. Gradle daemon footprint

The Gradle daemon is a long-lived JVM process that keeps build state
in memory between invocations. Footprint:

- Idle daemon: ~600 MB RSS (Gradle + Kotlin compiler classes loaded).
- Active build: ~1.5-2 GB RSS (KMP plugin + kotlinc + KGP cache).
- Multiple daemons (one per Gradle version, one per JVM args set):
  multiply by daemon count.

On CI we disable the daemon (`org.gradle.daemon=false`) so each build
starts fresh. The cost is ~5s of JVM startup per build; acceptable
because CI builds are sequential anyway.

On dev machines we leave the daemon enabled. The Mochi build driver
explicitly does not stop the daemon between Mochi commands (which would
defeat the purpose).

## 23. Toolchain caching on CI

The Gradle wrapper distribution lives at `~/.gradle/wrapper/dists/`.
The Kotlin compiler downloads lives at
`~/.konan/dependencies/` (Kotlin/Native toolchains, ~500 MB each).
The Android SDK lives at `~/Android/Sdk/` (1-3 GB depending on
installed platforms).

CI caches all three via `actions/cache@v4`:

```yaml
- uses: actions/cache@v4
  with:
    path: |
      ~/.gradle/caches
      ~/.gradle/wrapper
      ~/.konan
    key: gradle-${{ runner.os }}-${{ hashFiles('**/*.gradle*', '**/gradle-wrapper.properties', '**/libs.versions.toml') }}
    restore-keys: |
      gradle-${{ runner.os }}-
```

The `gradle/actions/setup-gradle@v4` action handles the bulk of this
automatically (it knows about `~/.gradle/caches/` and writes a cache
report at the end of each build). For Kotlin/Native and Android SDK we
add explicit cache entries.

Cache hit rate stays above 90% for the JVM matrix; cold-fetch cost on
a miss is ~60 seconds for Gradle distribution plus ~120 seconds for
the K/Native dependencies. The Android SDK is the heaviest single
cache at ~3 GB; cache hit rate matters more here.

## 24. Cross-compilation matrix

Not every target can build on every host:

| Target          | Linux host | macOS host | Windows host |
|-----------------|------------|------------|--------------|
| kotlin-jvm      | yes        | yes        | yes          |
| kotlin-android  | yes        | yes        | yes          |
| kotlin-linux-x64    | yes    | yes        | no (MinGW only for windows) |
| kotlin-linux-arm64  | yes    | yes        | no           |
| kotlin-macos-arm64  | no (no Apple SDK) | yes | no   |
| kotlin-macos-x64    | no     | yes        | no           |
| kotlin-ios          | no     | yes (Xcode required) | no |
| kotlin-windows-x64  | yes (MinGW cross) | yes (MinGW cross) | yes |
| kotlin-js           | yes    | yes        | yes          |
| kotlin-wasm-js      | yes    | yes        | yes          |

The Apple-target restriction (iOS, macOS) is because Kotlin/Native
requires the Apple SDK from Xcode, which only runs on macOS hosts.
This mirrors MEP-49's iOS/macOS restriction in
[[../0049/10-build-system]] §22.

The CI matrix in §21 accounts for this: Apple targets only run on
`macos-15` runners; the Linux and Windows jobs exclude them.

## 25. Sample build.gradle.kts for a small Mochi project

The codegen output for a small Mochi project (one `.mochi` source file,
no extra dependencies, targets jvm + linux + ios):

```kotlin
// Generated by mochi v1.0.0. Do not edit by hand; rerun `mochi build`
// to regenerate. Edits will be overwritten.

import org.jetbrains.kotlin.gradle.plugin.mpp.apple.XCFramework

plugins {
    alias(libs.plugins.kotlin.multiplatform)
}

group = "mochi.user"
version = "1.0.0"

kotlin {
    explicitApi()
    jvmToolchain(17)

    jvm {
        compilations.all {
            kotlinOptions { jvmTarget = "17" }
        }
    }

    val xcf = XCFramework("Hello")
    listOf(iosArm64(), iosSimulatorArm64()).forEach {
        it.binaries.framework {
            baseName = "Hello"
            xcf.add(this)
            isStatic = true
        }
    }

    linuxX64 {
        binaries.executable {
            entryPoint = "mochi.user.main"
            baseName = "hello"
        }
    }

    applyDefaultHierarchyTemplate()

    sourceSets {
        commonMain.dependencies {
            implementation(libs.kotlinx.coroutines.core)
            implementation(libs.mochi.runtime)
        }
        commonTest.dependencies {
            implementation(kotlin("test"))
        }
    }
}

tasks.withType<Jar>().configureEach {
    isPreserveFileTimestamps = false
    isReproducibleFileOrder = true
}
```

Running `./gradlew build` against this produces a JVM `.jar`, an iOS
`.xcframework`, and a Linux ELF binary; `mochi build --target=kotlin-all`
copies all three into `target/kotlin/dist/`.

## 26. Summary

The Kotlin build system reuses the Gradle and Android Studio ecosystems
at every layer:

- Gradle (8.11.1) for compilation orchestration, with the Kotlin Gradle
  Plugin (KGP 2.1) for the multiplatform compiler, and AGP (8.7.3) for
  Android packaging.
- Kotlin/Native for K/Native targets (Linux, macOS, iOS, Windows),
  Kotlin/JS IR backend for JavaScript, Kotlin/Wasm for WebAssembly GC.
- Gradle Kotlin DSL for every build script; no Groovy.
- Version catalog (`libs.versions.toml`) for centralised dependency
  pinning.
- Gradle wrapper for hermetic Gradle distribution.
- Bundled Eclipse Temurin 17 JDK for hermetic Java toolchain.
- vanniktech maven-publish plugin for Maven Central publishing.
- GitHub Actions runners (`ubuntu-24.04`, `ubuntu-24.04-arm`, `macos-15`,
  `windows-2022`) for the CI matrix, with self-hosted macOS for
  iOS device testing.

Mochi adds Kotlin-specific glue (the deterministic build script
emitter, the per-target source-set arrangement, the Compose
Multiplatform plumbing, the Android signing config) but contributes
nothing new to the Kotlin build space itself. The same way MEP-47
leaned on Gradle, Maven, jlink, and jpackage without inventing a new
JVM build tool, and MEP-49 leaned on SwiftPM, Xcode, and notarytool,
MEP-50 leans on Gradle, KGP, AGP, and Maven Central without inventing
a new Kotlin build tool.

---

## Sources

1. Kotlin Multiplatform documentation. https://kotlinlang.org/docs/multiplatform.html
2. Kotlin 2.1.0 release notes. https://kotlinlang.org/docs/whatsnew21.html
3. Kotlin Gradle Plugin documentation. https://kotlinlang.org/docs/gradle.html
4. Android Gradle Plugin release notes. https://developer.android.com/build/releases/gradle-plugin
5. Gradle 8.11 release notes. https://docs.gradle.org/8.11/release-notes.html
6. Gradle wrapper documentation. https://docs.gradle.org/current/userguide/gradle_wrapper.html
7. Gradle Kotlin DSL primer. https://docs.gradle.org/current/userguide/kotlin_dsl.html
8. Kotlin/Native documentation. https://kotlinlang.org/docs/native-overview.html
9. Kotlin/JS documentation. https://kotlinlang.org/docs/js-overview.html
10. Kotlin/Wasm documentation. https://kotlinlang.org/docs/wasm-overview.html
11. Compose Multiplatform documentation. https://www.jetbrains.com/lp/compose-multiplatform/
12. Compose Multiplatform 1.7 release notes. https://github.com/JetBrains/compose-multiplatform/releases/tag/v1.7.3
13. kotlinx.coroutines 1.10.1 release notes. https://github.com/Kotlin/kotlinx.coroutines/releases/tag/1.10.1
14. kotlinx.serialization documentation. https://github.com/Kotlin/kotlinx.serialization
15. kotlinx.datetime documentation. https://github.com/Kotlin/kotlinx-datetime
16. kotlinx.collections.immutable documentation. https://github.com/Kotlin/kotlinx.collections.immutable
17. Ktor client documentation. https://ktor.io/docs/client.html
18. vanniktech maven-publish plugin. https://github.com/vanniktech/gradle-maven-publish-plugin
19. Sonatype Central Portal. https://central.sonatype.org/publish/publish-portal-guide/
20. Foojay toolchain resolver. https://github.com/gradle/foojay-toolchains
21. Eclipse Temurin. https://adoptium.net/
22. android-actions/setup-android. https://github.com/android-actions/setup-android
23. ReactiveCircus/android-emulator-runner. https://github.com/ReactiveCircus/android-emulator-runner
24. gradle/actions. https://github.com/gradle/actions
25. Reproducible Builds standard. https://reproducible-builds.org/specs/source-date-epoch/
