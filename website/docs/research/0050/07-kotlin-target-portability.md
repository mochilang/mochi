# MEP-50 research note 07, Kotlin target portability and KMP matrix

Author: research pass for MEP-50.
Date: 2026-05-23 11:05 (GMT+7).

This note pins down the Kotlin toolchain version policy, the Kotlin Multiplatform (KMP) source-set hierarchy, the per-target shipping plan (JVM, Android, iOS via Kotlin/Native, macOS via K/Native, Linux via K/Native, Windows via K/Native mingw, Kotlin/JS, Kotlin/Wasm), the toolchain bundling decision, cross-target gating, and the per-target binary size and cold-start targets for Mochi-on-Kotlin. Companion notes: the shared-decisions anchor, [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The Kotlin target sits on top of the Kotlin 2.1 language level, the K2 compiler frontend, the Kotlin Multiplatform (KMP) build surface (Gradle Kotlin DSL + KMP plugin), and target-specific backends (Kotlin/JVM, Kotlin/Android via AGP, Kotlin/Native via LLVM, Kotlin/JS via the IR backend, Kotlin/Wasm via the Wasm GC backend). Unlike Swift (MEP-49) where the shipped artifact is one native binary per (os, arch, libc) triple, Kotlin ships a fan of artifacts: `.jar` for JVM, `.aab`/`.apk` for Android, `.framework` or `.xcframework` for Apple platforms, `.kexe` for K/Native Linux/Windows desktops, `.js`/`.mjs` for Kotlin/JS, and `.wasm` (plus a JS loader) for Kotlin/Wasm. The portability story is therefore wider in count of targets but narrower per target.

The matrix has three independent axes: Kotlin language version (2.1 floor), backend (JVM, Native, JS, Wasm) and per-backend target (linuxX64, mingwX64, iosArm64, browser, nodejs, wasmJs, ...).

---

## 1. Kotlin version matrix and our position

Kotlin moved to a roughly six-month major-release cadence after Kotlin 1.4 (August 2020). Kotlin 2.0 (May 2024) was the first release where the K2 compiler frontend was the default; K2 had been opt-in via `languageVersion = "2.0"` since 1.7. Kotlin 2.1 (November 2024) is the floor for MEP-50.

| Version | GA         | K2 default | KMP stable | Mochi support       |
|---------|------------|------------|------------|---------------------|
| 1.9.0   | 2023-07-06 | no (opt-in)| no (Beta)  | unsupported         |
| 1.9.20  | 2023-11-01 | no (opt-in)| **yes**    | unsupported         |
| 2.0.0   | 2024-05-21 | **yes**    | yes        | minimum (legacy)    |
| 2.0.20  | 2024-08-22 | yes        | yes        | minimum (legacy)    |
| 2.1.0   | 2024-11-27 | yes        | yes        | **floor, gating**   |
| 2.1.20  | 2025-03-19 | yes        | yes        | CI ceiling, gating  |
| 2.2.0   | 2025-09 (projected) | yes | yes      | advisory, CI smoke  |

Kotlin 2.1 is our **floor**. Mochi-emitted Kotlin source declares `kotlin("multiplatform") version "2.1.0"` in the generated `build.gradle.kts`, sets `kotlinOptions.languageVersion = "2.1"`, and pins `kotlinOptions.apiVersion = "2.1"`. The runtime module (`MochiRuntime`) is compiled at Kotlin 2.1 and produces standard `.klib` (Kotlin library) outputs per target, which are forward-compatible with later toolchains because the `.klib` ABI is stable across patch releases of a minor version (2.1.0 through 2.1.20) and source-compatible across minor releases (2.1 source compiles on 2.2 with deprecation warnings only).

Kotlin 2.1.20 is the **CI ceiling**: every gate runs against it, and we exercise features that became stable in 2.1 (multidollar string interpolation, smart casts for sealed `when` expressions, KMP source-set defaults, the new K/Wasm exception proposal). We do **not** emit code that requires 2.1.20-only features; 2.1.0 is the minimum source level.

Kotlin 2.2 (projected late 2025) is **advisory**: nightly smoke runs against it but no gating. Kotlin 2.2 is expected to land the stable Wasm GC target, refined context receivers, and the second wave of K2 IDE plugin maturity. If a 2.2-only regression surfaces, we log it as a known issue and the next release floor decides.

The reasoning for the 2.1 floor specifically:

- K2 compiler is the only frontend we target. K1 was deprecated in 2.0 and removed from active development. Emitting code that depends on K1-specific behaviour (the old `inline class` shape, certain inference corners) would be a foot-gun.
- KMP source-set hierarchy templates (the "default hierarchy template", `applyDefaultHierarchyTemplate()`) shipped in 1.9.20 and stabilised in 2.0; 2.1 made them the default, saving ~100 lines of boilerplate in every generated `build.gradle.kts`.
- `kotlin.time.Instant` (the cross-platform time type) became stable in Kotlin 2.1, replacing the `kotlinx.datetime.Instant` dependency for in-process time. See [[06-type-lowering]] §3.
- Multidollar string interpolation (`$$"raw $literal"`) ships in 2.1 and is used by Mochi codegen for emitting Kotlin templates that themselves contain `$` (e.g. shell scripts in generated Gradle tasks).
- Smart casts on sealed `when` expressions are tightened in 2.1, giving us cleaner generated `when (x)` exhaustiveness without redundant `else` branches.
- `kotlinx-coroutines-core` 1.10 (December 2024) drops K1 frontend support and requires Kotlin 2.0+; 1.10 is the floor we ship against, and it pairs cleanly with Kotlin 2.1.

Kotlin 2.0 source mostly works on the floor; we accept it with a deprecation warning. Kotlin 1.9 and earlier are explicitly rejected.

## 2. KMP source-set hierarchy

Kotlin Multiplatform organises code into **source sets**. A source set is a directory of `.kt` files plus a set of declared targets it applies to. The build flattens all relevant source sets into one compilation per target.

Before Kotlin 1.9.20, every KMP project hand-rolled the source-set graph in `build.gradle.kts`:

```kotlin
kotlin {
  sourceSets {
    val commonMain by getting
    val nativeMain by creating { dependsOn(commonMain) }
    val linuxMain by creating { dependsOn(nativeMain) }
    val linuxX64Main by getting { dependsOn(linuxMain) }
    val linuxArm64Main by getting { dependsOn(linuxMain) }
    // ... 30+ lines per project
  }
}
```

Kotlin 1.9.20 (November 2023) introduced the **default hierarchy template** (`applyDefaultHierarchyTemplate()`), and Kotlin 2.0 made it the implicit default when a KMP project declares targets without overriding the source-set graph. The template provides:

```
commonMain
├── jvmAndAndroidMain          (when both jvm and androidTarget declared)
│   ├── jvmMain
│   └── androidMain
├── nativeMain                 (parent of all K/Native)
│   ├── appleMain
│   │   ├── iosMain
│   │   │   ├── iosArm64Main
│   │   │   ├── iosSimulatorArm64Main
│   │   │   └── iosX64Main
│   │   ├── macosMain
│   │   │   ├── macosArm64Main
│   │   │   └── macosX64Main
│   │   ├── tvosMain (etc.)
│   │   └── watchosMain (etc.)
│   ├── linuxMain
│   │   ├── linuxX64Main
│   │   └── linuxArm64Main
│   ├── mingwMain
│   │   └── mingwX64Main
│   └── androidNativeMain       (for NDK builds)
├── webMain                    (parent of JS + Wasm)
│   ├── jsMain
│   ├── wasmJsMain
│   └── wasmWasiMain
└── (per-target test counterparts mirrored under commonTest, etc.)
```

Mochi codegen relies on the default template. The generated `build.gradle.kts` calls `applyDefaultHierarchyTemplate()` exactly once (or omits the call entirely, since it is the default in 2.1) and declares only the leaf targets the user has opted into. Mochi never overrides the template; if a user needs a custom intermediate source set, the Mochi build accepts a `--kotlin-source-set-overlay=path/to/sourceset.gradle.kts` snippet that is included verbatim.

Mochi's lowering rule:

- A Mochi source file that touches **no platform-specific API** lives in `commonMain`. It compiles to every declared target with no fragmentation.
- A Mochi source file that calls a JVM-specific FFI (e.g. JNI bridge) is split: a stub in `commonMain` declares the `expect fun foo(): T`, and an `actual fun` lives in `jvmMain`. See [[06-type-lowering]] §12 for expect/actual lowering.
- Per-target tweaks (e.g. iOS-only memory model overrides) land in the per-target source set. Mochi codegen for these is rare; most Mochi code is platform-agnostic.

## 3. JVM target

Kotlin/JVM is the oldest backend, the most mature, and the one with the smallest per-fixture surprise budget. The compiler (`kotlinc-jvm`) emits standard JVM bytecode that any JVM 8+ runtime executes.

| Setting               | MEP-50 value                                                        |
|-----------------------|---------------------------------------------------------------------|
| Bytecode target       | 17 (`jvmTarget = JvmTarget.JVM_17`)                                 |
| Source level          | Kotlin 2.1                                                          |
| JDK floor (runtime)   | Java 17 LTS (2021-09)                                               |
| JDK ceiling (runtime) | Java 21 LTS (2023-09), tested; Java 25 LTS (2025-09) advisory       |
| Module system         | Optional JPMS (`module-info.java` if user opts in)                  |
| ABI                   | Stable across patch releases; minor releases may change `@Metadata` |
| Stdlib                | `kotlin-stdlib-jdk8` (since 1.8 the merged jdk7+jdk8 stdlib)        |

Bytecode target 17 is picked because:

- Java 17 LTS is the de facto minimum in 2025; Spring Boot 3.x, Quarkus 3.x, Micronaut 4.x all require it.
- Java 17 ships records and sealed classes, which give clean interop targets for Mochi records (lower to Kotlin `data class` -> Java `record` on the bytecode side) and sum types (Kotlin `sealed interface` -> Java sealed interface).
- Pattern matching (`switch` patterns in Java 21) is not yet relied on by Mochi codegen; that lands when we raise the floor to Java 21.

ABI stability across Kotlin versions: the **Kotlin Metadata** annotations (`@Metadata` on every emitted class) are forward-compatible (a 2.1-emitted class loads fine in a 2.2 reader) but not backward-compatible (a 2.2-emitted class with new metadata fields may confuse a 2.1 reader). The JVM bytecode itself is fully backward-compatible: `.class` files compiled for JVM 17 load unchanged on JVM 17, 21, 25, etc.

**ProGuard/R8 considerations**:

ProGuard (the GuardSquare obfuscator) and R8 (Google's replacement, default in AGP 3.4+) are bytecode shrinkers/optimisers used in Android release builds. Kotlin-specific R8 rules ship with the Kotlin Gradle plugin:

- `keep` rules for `@Metadata` (so Kotlin reflection works post-shrink).
- `keep` rules for `@kotlin.coroutines.jvm.internal.DebugMetadata` (so coroutine stack frames decode in debugger).
- `keep` rules for `kotlinx-serialization` runtime types (so `KSerializer.descriptor` is not stripped).

Mochi codegen emits a `proguard-rules.pro` file with the standard rules pre-populated when the JVM target is selected. R8 in full mode (with `android.enableR8.fullMode=true`) is the default for Android release builds and is also the default Mochi recommends for JVM release builds via a thin Gradle wrapper.

For JVM release `.jar` shipping (not Android), the relevant tools are:

- `kotlinc-jvm -Xemit-jvm-type-annotations` (emit JSR-308 annotations, default in 2.1).
- ProGuard 7.4+ for general optimisation.
- `jlink` for custom JRE bundling (Java 9+).
- `jpackage` for single-file `.exe`/`.dmg`/`.deb` desktop bundles (Java 14+).

Mochi exposes these as `mochi build --target=kotlin-jvm --release` with `--jpackage` and `--proguard` flags.

## 4. Android target

Kotlin Android compilation routes through the Android Gradle Plugin (AGP), which wraps `kotlinc-jvm` plus the Android-specific bytecode transforms (D8 dexer, R8 shrinker, AAPT2 resource compiler). The toolchain is bundled with Android Studio but is also installable standalone via the Android SDK Command-line Tools.

| Setting        | MEP-50 value                                              |
|----------------|-----------------------------------------------------------|
| AGP            | 8.7+ (2024-10), tested through 8.8                        |
| compileSdk     | 35 (Android 15)                                           |
| targetSdk      | 35 (Android 15)                                           |
| minSdk         | 24 (Android 7.0 Nougat, 2016-08)                          |
| JVM target     | 17 (`compileOptions.targetCompatibility = JavaVersion.VERSION_17`) |
| Kotlin plugin  | 2.1+ (Android Studio Ladybug Feature Drop ships 2.1)      |
| Build tools    | 34.0.0+                                                   |
| NDK            | r27+ for Kotlin/Native-on-Android (optional, see §5)      |

`minSdk 24` is chosen because:

- Android 7.0 Nougat (2016) is the lowest version still covered by Google's Play Console "supported" matrix. As of 2025-05, Android 7.0 sits at ~1.2% market share (down from ~3% in 2024); the floor is right at the cutoff where keeping it is cheap and dropping it would gain nothing.
- Java 8+ language features (default methods, static methods on interfaces, try-with-resources) are supported via desugaring in AGP since 4.0.
- ART (Android Runtime) replaces Dalvik on every Android 5.0+ device; Mochi never needs to worry about Dalvik VM quirks. ART supports tiered JIT + AOT compilation (the OS recompiles hot code after install).

`targetSdk 35` is chosen because:

- Google Play Console requires `targetSdk >= 34` for new apps as of 2024-08 and for updates as of 2024-11. `targetSdk 35` is the current ceiling that future-proofs for 2025-08's next bump (likely to require 35).
- Android 15 adds edge-to-edge enforcement, predictive back gestures (stable), and revised foreground-service categories; Mochi inherits these defaults.

**R8 minification**: AGP 8.7 ships R8 8.4. Mochi's `proguard-rules.pro` (described in §3) extends the AGP default rules. The release build runs with `minifyEnabled = true`, `shrinkResources = true`, and `useR8 = true` (the default).

**ART vs Dalvik**: irrelevant for Mochi. Every Android device Mochi supports (API 24+) ships ART. ART's quirks Mochi cares about: AOT compilation profiles (`baseline-prof.txt`) influence startup; Mochi emits a default baseline profile generated from the gate-suite startup traces (similar to Jetpack's baseline-profile-gradle-plugin).

**Android NDK** is optional for the Mochi-on-Kotlin/Android path. Pure-Kotlin Mochi code compiles to dex, not native. If the user's Mochi code calls into a `cinterop` binding (e.g. `libcurl`), that binding lives in the K/Native side; on Android we route via JNI rather than Kotlin/Native. The cinterop story for Android is covered in [[12-risks-and-alternatives]] §6.

## 5. iOS via Kotlin/Native

Kotlin/Native is the LLVM-based AOT backend for non-JVM platforms. For iOS, K/Native compiles to a `.framework` that Xcode embeds in the iOS app target.

| K/Native target          | Triple                       | Status                                |
|--------------------------|------------------------------|---------------------------------------|
| `iosArm64`               | aarch64-apple-ios            | Tier 1, gating; all current iPhones   |
| `iosSimulatorArm64`      | aarch64-apple-ios-simulator  | Tier 1, gating; M-series Macs         |
| `iosX64`                 | x86_64-apple-ios-simulator   | Tier 3, Rosetta-only; advisory CI     |

`iosX64` (the Intel-Mac iOS simulator) is deprecated by Apple and JetBrains. Kotlin 2.1 still supports it but emits a deprecation warning; we accept the warning in CI and plan to drop in MEP-50 v2.

The iOS shipping artifact is an **`.xcframework`** (multi-architecture framework bundle). The KMP Gradle plugin's `XCFramework` task assembles per-target `.framework`s into one `.xcframework`:

```kotlin
kotlin {
  val xcf = XCFramework("MochiApp")
  iosArm64 { binaries.framework { baseName = "MochiApp"; xcf.add(this) } }
  iosSimulatorArm64 { binaries.framework { baseName = "MochiApp"; xcf.add(this) } }
}
```

Mochi codegen always emits `.xcframework` output for the iOS target (not the older `.framework` directly), because `.xcframework` is the format Swift Package Manager consumes natively.

**Swift Package Manager integration**: the KMP plugin 2.0 added the `swiftPackage` DSL (formally `kotlin.experimental.swift-export` in 2.1) which generates a `Package.swift` next to the `.xcframework`, exposing Kotlin classes as Swift-friendly types. Mochi enables this when the user passes `--kotlin-swift-export`. The Swift export feature is **Beta** as of Kotlin 2.1, so we ship it with a caveat: the generated Swift types do not yet support coroutines (only suspend functions exposed as callback-taking Objective-C selectors), and generic Kotlin types lose their parameterisation.

For users who want full Swift interop, the alternative is **CocoaPods integration** via the older `cocoapods` Gradle plugin. Mochi documents this but does not generate Podspec by default.

**Framework export Kotlin types**:

- Public Kotlin classes appear as Objective-C classes with name prefix (`MochiAppCounter` for `Counter`).
- Suspend functions become callback-taking Objective-C selectors (`-incWithCompletionHandler:`).
- Sealed classes flatten to Objective-C class hierarchies.
- Sum types lose exhaustiveness in Swift (the Swift compiler does not know the Kotlin sealed contract).
- `Long` (Mochi `int`) maps to `int64_t` in Objective-C, which Swift sees as `Int64`. Note this differs from Swift's native `Int` which is platform-word-sized.

## 6. macOS via Kotlin/Native

| K/Native target | Triple                 | Status                                 |
|-----------------|------------------------|----------------------------------------|
| `macosArm64`    | aarch64-apple-macosx   | Tier 1, gating; Apple Silicon          |
| `macosX64`      | x86_64-apple-macosx    | Tier 2, gating; Intel Mac fallback     |

macOS K/Native ships as a `.kexe` (Kotlin executable) for CLI tools or a `.framework`/`.xcframework` for library distribution. Unlike iOS, macOS allows direct CLI binaries, so Mochi codegen for macOS CLI lowers to:

```
mochi build --target=kotlin-macos --arch=arm64,x86_64
# produces dist/myapp-macos-arm64.kexe, dist/myapp-macos-x86_64.kexe
```

For universal macOS distribution, the Mochi build runs `lipo -create -output myapp-universal arm64.kexe x86_64.kexe` (the Xcode tool, installed with Xcode Command Line Tools).

**Differences from iOS**:

- **No UIKit**. macOS uses AppKit, accessed from K/Native via the platform.AppKit cinterop binding (`platform.AppKit.NSApplication.sharedApplication`, etc.). Mochi never emits AppKit calls directly; UI work routes through Compose Multiplatform (1.7.3+) which abstracts over UIKit / AppKit / Skia.
- **No app sandbox required for CLI**. Mac App Store distribution requires sandboxing, but direct download with Developer ID does not. Mochi exposes both via `--macos-distribution=appstore` vs `--macos-distribution=direct`.
- **No code signing required for ad-hoc local builds**. Distribution builds need either Developer ID (with notarisation) or Mac App Store provisioning. Mochi pipeline: `codesign --options runtime`, then `notarytool submit`, then `stapler staple`. Identical to MEP-49's macOS pipeline ([[../0049/07-swift-target-portability]] §14).

K/Native on macOS has full access to the platform's Objective-C frameworks via cinterop. The Mochi runtime exposes a curated subset (`MochiRuntime.Platform.macOS`) that wraps NSFileManager, NSURLSession, and NSWorkspace; user code calls Mochi APIs, the runtime routes to the platform binding.

## 7. Linux via Kotlin/Native

| K/Native target  | Triple                       | Status                                |
|------------------|------------------------------|---------------------------------------|
| `linuxX64`       | x86_64-unknown-linux-gnu     | Tier 1, gating                        |
| `linuxArm64`     | aarch64-unknown-linux-gnu    | Tier 1, gating                        |
| `linuxMipsel32`  | mipsel-unknown-linux-gnu     | dropped in 1.9; not supported         |
| `linuxArm32Hfp`  | arm-unknown-linux-gnueabihf  | Tier 3 (Raspberry Pi 32-bit)          |

K/Native uses LLVM to emit native binaries; the linker is `ld.lld` (bundled with the K/Native toolchain). The runtime depends on `libc.so.6` (glibc) and `libpthread.so.0` (now part of glibc since glibc 2.34).

**musl libc vs glibc**: K/Native officially targets glibc. There is **no upstream `linuxX64-musl` target** as of Kotlin 2.1, unlike Swift's Static Linux SDK ([[../0049/07-swift-target-portability]] §4). This is a real gap. Mochi documents two workarounds:

1. **Build inside a musl container** (e.g. Alpine Linux): K/Native's `linuxX64` target *can* be compiled inside a musl host if the user installs a glibc-compatible runtime layer (`libc6-compat` package on Alpine). The resulting binary still depends on glibc symbols, so the deployment target must have glibc available. Practical: build on Ubuntu 24.04, deploy on Ubuntu 22.04+, Debian 12+, RHEL 9+.

2. **Static-link glibc via `-Xoverride-konan-properties`**: K/Native exposes a knob to link against a custom libc. We document this as community-supported, not officially blessed.

A future MEP-50 v2 may add a Mochi-curated `linuxX64-musl` triple via a custom K/Native sysroot build, similar to how some Rust users build musl targets out-of-tree. Out of scope for v1.

**Static linking**: K/Native's `binaries.executable { entryPoint = "main"; runTask?.standardInput = ... }` produces a dynamically-linked ELF by default. The `-Xstatic-framework` flag is iOS-specific; for Linux, the user passes `-Xoverride-konan-properties=linkerKonanFlags.linux_x64=-static` to force static linking, at the cost of a much larger binary (~30 MB hello-world vs ~3 MB dynamic).

Mochi's Linux release artifact defaults to **dynamic glibc** with the runtime bundled as `.so` siblings (similar to MEP-49's "dynamic Swift runtime" mode but with the Kotlin runtime); the static mode is an explicit `--kotlin-static-linking` flag.

## 8. Windows via Kotlin/Native

| K/Native target  | Triple                       | Status                                |
|------------------|------------------------------|---------------------------------------|
| `mingwX64`       | x86_64-pc-windows-gnu        | Tier 1, gating                        |
| `mingwArm64`     | aarch64-pc-windows-gnu       | Tier 3, experimental                  |

K/Native on Windows uses **MinGW-w64** (the open-source Windows toolchain), not MSVC. The K/Native compiler bundles the MinGW headers, libraries, and linker. This is a deliberate choice by JetBrains: MSVC interop would require a separate code path for C ABI mangling.

**MSVC interop limitations**:

- K/Native cinterop cannot directly consume MSVC-compiled `.lib` files (the symbol naming convention differs between MinGW and MSVC).
- MSVC-only Windows APIs (some `__declspec(dllimport)` patterns) are not reachable from K/Native without a MinGW shim.
- The C++ ABI is MinGW's (Itanium ABI variant), not MSVC's. C++ libraries compiled with MSVC are not directly callable.

In practice, almost all Windows system APIs (Win32, COM, Direct3D) are exposed via `extern "C"` with the standard `__stdcall` calling convention, and these work fine through MinGW. The cases where MSVC interop matters are narrow (Microsoft-specific C++ libraries like MFC or Microsoft Speech SDK). Mochi documents this in [[12-risks-and-alternatives]] §7.

**Windows version floor**: K/Native binaries require Windows 7 SP1 or later. Mochi's runtime additionally requires Windows 10 1809+ for the same reasons as Swift (long-path support, modern stream APIs). Mochi codegen emits a runtime check at startup; on older Windows the program exits with an error message.

Windows release artifact: `.exe` plus a small set of MinGW runtime DLLs (`libgcc_s_seh-1.dll`, `libstdc++-6.dll`, `libwinpthread-1.dll`). For single-file shipping, K/Native supports static linking via `-Xstatic-framework=true` on the binary task (this links the MinGW runtime statically). Mochi exposes this as `--windows-static-runtime`.

## 9. Kotlin/JS target

Kotlin/JS compiles Kotlin source to JavaScript via the IR backend (the only backend since Kotlin 1.9; the legacy backend was removed). The compilation target is configurable:

| Target          | Use case                                  | Module system          |
|-----------------|-------------------------------------------|------------------------|
| `js(IR) { browser() }`  | Browser apps via webpack       | ES modules or UMD       |
| `js(IR) { nodejs() }`   | Node.js scripts                | ES modules or CommonJS  |
| `js(IR) { browser().webpackTask {} }` | Webpack-bundled SPA | ES modules            |

**IR backend only**: Kotlin 1.9 (2023-07) removed the legacy JS backend. The IR backend uses the same Kotlin IR as K/Native and emits JS via a separate codegen. Advantages: smaller output, better dead-code elimination, support for modern JS features.

**Browser vs nodejs**: the distinction is which stdlib polyfills get included:

- `browser()` provides `kotlinx.browser.document`, `kotlinx.browser.window`, DOM bindings.
- `nodejs()` provides `process`, `fs`, `path` bindings via auto-generated extern declarations.

Mochi's pure-logic code (no DOM, no Node-specific APIs) compiles to either target with the same output; the distinction is in `kotlin-js-extensions` and in the webpack/Rollup config. Mochi defaults to `browser()` for the web target and `nodejs()` for the CLI target.

**ES modules vs CommonJS**: Kotlin 2.0 (May 2024) added stable ES modules output (`useEsModules = true` on the compilation task). Mochi defaults to ES modules. CommonJS remains available via `useEsModules = false` for Node.js scripts pinned to older runtimes.

**Webpack / Rollup integration**: the KMP Gradle plugin includes a built-in webpack 5 integration via `org.jetbrains.kotlin:kotlin-gradle-plugin`. The `browserProductionWebpack` task produces a bundled `.js` file. Mochi exposes this as `mochi build --target=kotlin-js --output=bundle`.

Rollup is supported via a third-party Gradle plugin (`org.jetbrains.kotlin.js.rollup`); Mochi does not generate Rollup config by default.

**JS dynamic types**: Kotlin/JS has a `dynamic` type that opts out of static typing for JS interop. Mochi codegen never emits `dynamic` for user code; if a Mochi FFI needs to call a JS function with an unknown signature, the binding lives in handwritten Kotlin glue in the FFI module, not in generated code. See §17.

## 10. Kotlin/Wasm target

Kotlin/Wasm is the newest backend, shipping the **Wasm GC** proposal (Garbage-Collected WebAssembly) which the W3C standardised in late 2024 after years of stage-3 incubation.

| Target          | Status as of Kotlin 2.1               | Mochi support       |
|-----------------|----------------------------------------|---------------------|
| `wasmJs`        | Alpha in 2.0, Beta in 2.1.20           | shipped with caveat |
| `wasmWasi`      | Experimental                           | not shipped in v1   |

**Wasm GC** requires browser support for the WebAssembly garbage collection proposal. Browser support timeline:

| Browser         | Wasm GC stable     | Notes                                   |
|-----------------|--------------------|------------------------------------------|
| Chrome 119      | 2023-10-31         | First stable shipping                    |
| Firefox 120     | 2023-11-21         | Second                                   |
| Safari 18.2     | 2024-12-11         | Latest (Safari was the holdout)          |
| Edge 119        | 2023-11            | Chromium-based, inherits Chrome support  |

As of MEP-50 v1 (May 2026), every evergreen browser shipped in the last 18 months supports Wasm GC. The floor is **Chrome 119 / Firefox 120 / Safari 18.2**.

**Alpha status caveats**: as of Kotlin 2.1.20 the Wasm target is Beta, not stable. Known limitations:

- Reflection is partial (`KClass.isInstance` works; full reflection does not).
- `kotlin.time.Instant` works; some `kotlinx-datetime` APIs are not yet available.
- Coroutines work; some kotlinx.coroutines flow operators have not been ported.
- Binary size is larger than the equivalent JS output (1-5 MB hello world).

Mochi ships the Wasm target with a clear "Alpha/Beta" caveat in the docs and a warning at codegen time.

**Binary size for hello world**:

| Output                       | Size      | Notes                                  |
|------------------------------|-----------|----------------------------------------|
| `.wasm` (hello world)        | ~1.2 MB   | Compressed (gzip): ~400 KB             |
| Wasm + JS loader (.mjs)      | ~50 KB    | Loader only                            |
| Total over-the-wire (gzip)   | ~450 KB   | gzipped wasm + gzipped loader          |

Compare to Kotlin/JS hello world: ~200 KB raw, ~70 KB gzipped. Wasm is larger because it bundles the Kotlin stdlib (the JS target relies on the JS runtime's built-in types).

**Threading**: WebAssembly threads (the threads proposal) are separate from Wasm GC and require browser support for shared memory. Kotlin/Wasm does not yet support threading; Mochi's `agent` and `stream` lowering on Wasm runs all coroutines on a single thread (the JS main thread). This is acceptable for v1 because Mochi's web fixtures are predominantly UI-driven.

## 11. watchOS / tvOS / wearOS: deferred to v2

| Target        | K/Native or other | Status in MEP-50 v1     |
|---------------|-------------------|--------------------------|
| `watchosArm64` (Apple Watch S6+) | K/Native | deferred to v2 |
| `watchosSimulatorArm64` | K/Native | deferred to v2          |
| `tvosArm64` (Apple TV 4K) | K/Native | deferred to v2         |
| `tvosSimulatorArm64` | K/Native | deferred to v2            |
| Wear OS (Android-based) | Standard Android target | covered by §4 |

watchOS and tvOS are technically supported by K/Native (the `watchosArm64`, `tvosArm64` targets compile), but Mochi v1 does not gate on them. Reasons:

- Apple Watch apps have severe memory constraints (~50 MB working set); the Kotlin/Native runtime + Mochi runtime exceeds this comfortably. Optimisation work is required.
- tvOS has a TVUIKit (a variant of UIKit) that Mochi has not yet bound.
- The market for "Mochi on Apple Watch" is small in v1.

Wear OS (Google's wearable platform) is built on Android, so the existing Android target works; we document the limitations (smaller battery, limited screen) but the lowering is identical.

A future MEP-50.1 sub-MEP will add `watchosArm64` and `tvosArm64` gating once the runtime memory profile is acceptable.

## 12. Embedded Kotlin (not a thing officially)

Unlike Swift, which has the official "Embedded Swift" subset for bare-metal targets ([[../0049/07-swift-target-portability]] §8), Kotlin has no analogous officially-supported subset. The closest things are:

- **Kotlin/Native headless**: K/Native binaries run on Linux/macOS/Windows without a GUI. This is what Mochi uses for CLI tools. It is *not* bare-metal; it requires an operating system.
- **Kotlin Multiplatform Mobile (KMM)**: Marketing term for KMP-targeting-iOS-and-Android. Not a distinct technical capability.
- **Compose for HTML / Compose Multiplatform**: A UI framework, not an embedded runtime.

**Out of scope for MEP-50**: bare-metal, microcontroller, or RTOS deployment. If a user needs Mochi on a Raspberry Pi Pico, the answer is "use the Mochi-on-C transpiler (MEP-45) or wait for a future Mochi-on-Rust embedded MEP".

## 13. Per-target binary size

Mochi's release-mode hello-world target sizes (after release-mode build with shrinking enabled where applicable):

| Target                                      | Size           | Notes                                       |
|---------------------------------------------|----------------|---------------------------------------------|
| JVM `.jar` (no shrinking)                   | ~3.5 MB        | Kotlin stdlib + Mochi runtime               |
| JVM `.jar` (ProGuard shrunk)                | ~800 KB        | Aggressive shrink, keep Metadata            |
| Android `.aab` (release, R8)                | ~1.2 MB        | Includes baseline profile                   |
| Android `.apk` (universal, R8)              | ~1.8 MB        | Universal APK with all ABIs                 |
| iOS `.xcframework` (arm64 + simArm64)       | ~6 MB          | Each `.framework` is ~3 MB                  |
| macOS arm64 `.kexe` (dynamic)               | ~5 MB          | Includes K/Native runtime                   |
| macOS arm64 `.kexe` (static)                | ~30 MB         | Full static link                            |
| Linux x86_64 `.kexe` (dynamic glibc)        | ~5 MB          | Plus ~2 MB of sidecar `.so`                 |
| Linux x86_64 `.kexe` (static)               | ~30 MB         | Full static link                            |
| Windows x86_64 `.exe` (dynamic, MinGW)      | ~5 MB          | Plus ~6 MB of sidecar DLLs                  |
| Windows x86_64 `.exe` (static)              | ~25 MB         | Full static link                            |
| Kotlin/JS browser bundle (production, gzip) | ~75 KB         | Webpack-bundled                             |
| Kotlin/Wasm `.wasm` + loader (gzip)         | ~450 KB        | Wasm GC binary                              |

Gate thresholds (the CI fails if hello-world exceeds these):

- JVM shrunk: ≤ 1 MB.
- Android AAB: ≤ 2 MB.
- iOS xcframework: ≤ 8 MB.
- macOS/Linux dynamic: ≤ 8 MB.
- Windows dynamic: ≤ 8 MB.
- JS gzipped: ≤ 150 KB.
- Wasm gzipped: ≤ 600 KB.

Compared to MEP-49 (Swift) and MEP-47 (JVM bytecode direct):

- Swift wins on iOS / macOS because the Swift runtime is system-provided; Kotlin/Native must bundle it.
- MEP-47 (JVM direct) wins on `.jar` size because it skips the Kotlin stdlib entirely (emits direct bytecode); MEP-50 JVM keeps the Kotlin stdlib for source-level interop.
- Kotlin/JS is competitive with Swift's WASM story (both are ~500 KB gzipped for hello world).

## 14. Per-target cold-start latency

| Target                                      | Cold start  | Notes                                       |
|---------------------------------------------|-------------|---------------------------------------------|
| JVM `.jar` (Java 17, no AOT)                | ~250 ms     | Class loading + JIT warmup                  |
| JVM `.jar` (with CDS, Java 17)              | ~150 ms     | Class Data Sharing pre-loads stdlib         |
| JVM `.jar` (GraalVM native-image)           | ~25 ms      | Out of scope for MEP-50; that is MEP-47     |
| Android cold start (R8, baseline profile)   | ~400 ms     | App startup, first frame                    |
| iOS cold start (K/Native, framework)        | ~150 ms     | Plus host app startup                       |
| macOS/Linux/Windows K/Native                | ~30 ms      | Native binary, no VM warmup                 |
| Kotlin/JS Node.js                           | ~80 ms      | Node.js startup + bundle parse              |
| Kotlin/JS browser (parse + first paint)     | ~120 ms     | Excludes network                            |
| Kotlin/Wasm browser                         | ~100 ms     | Wasm compile is the dominant cost           |

**Loom comparison**: Java 21 introduced virtual threads (Project Loom). Loom does not reduce cold-start latency on the JVM, since the JVM itself still warms up; Loom's win is per-thread overhead at scale (millions of virtual threads at ~100 bytes each). Mochi-on-JVM-Kotlin uses kotlinx.coroutines, not Loom, for cross-target consistency: the same code compiles to JVM (coroutines on threadpool), K/Native (coroutines on cooperative executor), JS (coroutines on event loop), and Wasm (coroutines on event loop). Loom would only help on JVM.

For users who prefer Loom on the JVM-only deployment, MEP-47 (direct JVM bytecode) is the answer; MEP-50 picks coroutines for portability.

## 15. ABI stability

Kotlin's ABI stability story is target-dependent:

| Target          | Bytecode/binary ABI                   | Stdlib ABI                                   |
|-----------------|---------------------------------------|----------------------------------------------|
| JVM             | Stable across Kotlin patch + minor    | `kotlin-stdlib` ABI stable since 1.0         |
| Android (.dex)  | Stable; D8 dexer handles versioning   | Same as JVM stdlib                           |
| K/Native (.klib)| Stable across patch; minor may change | Per-target stdlib `.klib` per Kotlin version |
| Kotlin/JS       | Source-compatible across minor        | JS stdlib ABI may shift across minor         |
| Kotlin/Wasm     | Not yet ABI-stable (Alpha/Beta)       | Wasm stdlib changes across minor             |

**.klib forward compatibility**: A `.klib` produced by Kotlin 2.1.0 can be consumed by Kotlin 2.1.x (any patch) but not necessarily by 2.2.x. The KMP Gradle plugin records the producing Kotlin version in `.klib` metadata; consuming a `.klib` with a newer Kotlin version emits a warning and may fail.

**Mochi's pinning strategy**: the generated `gradle/libs.versions.toml` pins exact Kotlin versions:

```toml
[versions]
kotlin = "2.1.0"
agp = "8.7.0"
coroutines = "1.10.1"
serialization = "1.7.3"
ktor = "3.0.1"
compose = "1.7.3"

[libraries]
kotlinx-coroutines-core = { module = "org.jetbrains.kotlinx:kotlinx-coroutines-core", version.ref = "coroutines" }
# ... etc
```

The version catalog is the single source of truth. Bumping Kotlin requires bumping every related entry; Mochi's `mochi upgrade kotlin 2.1.20` command does this transactionally.

The Mochi project itself pins to Kotlin 2.1; user projects can override via `mochi.toml`'s `[kotlin] version = "2.1.20"` if they need a specific patch. The Mochi build refuses Kotlin <2.1.

## 16. Matrix CI plan

Mochi CI runs the following combinations on every PR that touches the Kotlin target:

| CI host                  | Targets                                                          | Gate phases                |
|--------------------------|------------------------------------------------------------------|----------------------------|
| `macos-15` (Apple Silicon) | jvm, ios{Arm64,SimulatorArm64}, macos{Arm64,X64}, js, wasmJs   | 1-14, 17                   |
| `macos-15-large` (x86_64 advisory) | jvm, macosX64                                          | smoke only                 |
| `ubuntu-24.04` (amd64)   | jvm, linuxX64, linuxArm64 (cross), js, wasmJs                    | 1-14, 17                   |
| `ubuntu-24.04-arm`       | jvm, linuxArm64 (native), js                                     | 1-14                       |
| `windows-2022`           | jvm, mingwX64                                                    | 1-14, 17                   |
| `ubuntu-24.04` + AVD     | android (API 24, API 35 emulators)                               | 15, 18                     |

**macOS 15 runners are arm64-only** as of 2025-Q1. GitHub Actions deprecated the x86_64 macOS runner ("macos-13") in favour of `macos-15` (arm64). x86_64 macOS testing happens in the `macos-15-large` advisory runner, which has Intel cores for Rosetta-based cross builds.

**Ubuntu 24.04** is the primary Linux runner. We test both amd64 (the default) and arm64 (`ubuntu-24.04-arm`, available in GitHub Actions since 2024-Q4). The arm64 runner is native, not emulated, so Linux/arm64 K/Native tests run at full speed.

**Windows 2022** runs the mingwX64 target. Windows arm64 runners are not yet available in GitHub Actions; Tier 3 for Mochi.

**Android emulator** runs API 24 (Nougat, the floor) and API 35 (current targetSdk). The AVD spins up under KVM acceleration on the Linux runner; per-fixture runtime is ~30 seconds.

## 17. Cross-target stdout equality gate

Every Mochi fixture that runs across multiple targets is subject to the **cross-target stdout equality gate** (`TestCrossTargetDifferential`). The gate:

1. Compiles the fixture for every target the fixture is not skipped on.
2. Runs the binary (or executes the script for JS/Wasm via Node.js / a headless browser).
3. Captures stdout and exit code.
4. Computes a SHA-256 hash of normalised stdout (trailing newline removed; locale-specific number formatting normalised).
5. Compares all per-target hashes. If any differ, the gate fails with a diff.

The gate runs in the CI matrix described in §16. It is the strongest portability guarantee Mochi offers: every fixture provably produces byte-identical output across JVM, Android (via instrumentation tests with stdout capture), Linux K/Native, macOS K/Native, Windows K/Native, Node.js, and Wasm.

Known sources of cross-target divergence we explicitly handle:

- **Floating-point printing**: Different platforms have different default float-to-string rules. Mochi forces a fixed precision (15 digits) at the runtime layer.
- **Map/set iteration order**: Mochi specifies insertion order (the shared-decisions anchor §3); the lowering uses LinkedHashMap / LinkedHashSet which is consistent across all targets.
- **Hashing**: Mochi never relies on hash-code values being identical across targets (they are not; JVM `String.hashCode` differs from K/Native and from JS).
- **Error messages**: Mochi-generated error messages are deterministic; platform-error wrappers include only Mochi-visible content, not platform stack traces.

## 18. Per-target unsafe APIs

Mochi codegen refuses to emit calls to per-target unsafe APIs:

| Target       | Unsafe API                                                    | Why forbidden                                |
|--------------|---------------------------------------------------------------|----------------------------------------------|
| K/Native     | `interpretCPointer` (raw pointer arithmetic)                  | Defeats Mochi's memory safety                |
| K/Native     | `nativeHeap.alloc<T>()` outside of FFI bindings               | Bypass of K/Native GC                        |
| Kotlin/JS    | `dynamic` type                                                | Defeats Kotlin's type checker                |
| Kotlin/JS    | `js("rawJsExpression")` outside of FFI module                 | Arbitrary JS injection                       |
| Kotlin/Wasm  | `unsafeJSValue` (JS interop escape hatch)                     | Defeats Wasm memory safety                   |
| JVM          | `sun.misc.Unsafe` / `jdk.internal.misc.Unsafe`                | JDK internal, not API                        |
| JVM          | Reflection without `@Reflect` annotation on the target        | Breaks R8 shrinking                          |
| Android      | `runtime.exec(String)` without `@ExternalProcess` annotation  | Audit surface                                |

The forbidden list is enforced by a Mochi IR pass (`mochi-ir-unsafe-check`) that runs before codegen. The pass walks the AST and reports any unsafe call with a Mochi-side diagnostic pointing at the equivalent safe API.

FFI modules (in the user's `ffi/` directory) are allowed to use the unsafe APIs; that is the entire point of FFI. The Mochi linter flags `ffi/` modules that mix unsafe FFI with non-FFI Mochi code, to keep the unsafe blast radius bounded.

## 19. Skipping a target per fixture

Some Mochi fixtures cannot run on every target (e.g. a fixture that uses JVM-specific JNI). The convention:

- A file `tests/foo.mochi` with `tests/foo.mochi.kotlin-wasmJs.skip` is skipped on the wasmJs target.
- A file `tests/foo.mochi.kotlin-android.skip` is skipped on Android.
- The skip file is empty or contains a one-line reason (`# uses java.nio.file, no equivalent on Wasm`).

The cross-target gate (§17) honours skip files: if a fixture skips Wasm, the gate computes the hash from the remaining targets and ignores Wasm. Skipping all but one target effectively turns off the differential.

Mochi tracks skip-file count per target as a portability metric. A target with too many skips is a candidate for either polyfilling or downgrading to Tier 2.

## 20. Toolchain bundling

**Decision**: Mochi bundles the Kotlin toolchain (kotlinc, Kotlin/Native compiler, KMP plugin, AGP, Gradle wrapper) as part of the Mochi distribution. The user does not need a pre-installed JDK or Kotlin compiler.

Rationale (similar to MEP-45's zig fallback decision and MEP-49's Swift toolchain bundling):

- **Reproducibility**: a Mochi project pinned to Kotlin 2.1.0 always builds with that exact toolchain. Users do not have to manage Kotlin versions per project.
- **First-run experience**: a fresh `mochi build --target=kotlin-jvm` works without prior setup beyond installing Mochi.
- **CI hygiene**: CI runners do not need a Kotlin install step; the Mochi binary brings its own.

Implementation:

- Mochi ships with a vendored OpenJDK 17 (Temurin distribution) per host platform. The JDK is in `~/.local/share/mochi/jdk/17/` (Linux/macOS) or `%LOCALAPPDATA%\mochi\jdk\17\` (Windows).
- Kotlin toolchains are vendored per Kotlin version: `~/.local/share/mochi/kotlin/2.1.0/`.
- AGP is fetched lazily via Gradle's standard mechanism on first Android build.
- The K/Native compiler (`konan`) bundles its own LLVM and lld, so no system LLVM is required.
- Gradle wrapper is generated per project, pinned to Gradle 8.11.1.

The bundled toolchain footprint is ~600 MB per host platform (JDK ~250 MB, kotlinc ~200 MB, K/Native LLVM ~150 MB). We document this prominently; users who want to use a system Kotlin install pass `--kotlin-toolchain=system` to skip the vendored path.

Versions can be overridden per project via `mochi.toml`:

```toml
[kotlin]
version = "2.1.20"
toolchain = "vendored"   # or "system"
gradle = "8.11.1"
agp = "8.7.0"
```

The Mochi build verifies SHA-256 checksums of every downloaded toolchain against a pinned manifest (`mochi/toolchains.lock`).

## 21. Locale and i18n

Kotlin inherits locale support from its host platform:

| Target       | Locale source                          | Notes                                        |
|--------------|----------------------------------------|----------------------------------------------|
| JVM          | `java.util.Locale` + JDK ICU           | JDK 17 bundles ICU 67; JDK 21 bundles ICU 72 |
| Android      | `java.util.Locale` + Android ICU       | Android 11+ bundles ICU 68+                  |
| K/Native     | Platform CFLocale (Apple) / glibc locale / Windows NLS | No bundled ICU                |
| Kotlin/JS    | `Intl` (the JS Intl API)               | Modern browsers; Node.js 14+                 |
| Kotlin/Wasm  | `Intl` (via JS interop)                | Wasm GC has no native ICU                    |

Mochi exposes locale-sensitive ops via `MochiRuntime.Locale`:

- `string.localizedCompare(other)` -> per-platform routing.
- `date.formatted(locale: "ja_JP")` -> per-platform date formatting.
- `number.formatted(currency: "JPY")` -> per-platform currency formatting.

For K/Native, where there is no bundled ICU, Mochi optionally links against a vendored `libicu` via cinterop. The vendored ICU adds ~30 MB to the binary. Mochi defaults to using the platform's native locale APIs (which are less feature-rich but free in binary size); users opt into ICU via `--kotlin-bundle-icu`.

Kotlin/JS and Kotlin/Wasm have the strongest locale story by virtue of routing to the browser's Intl API, which is comprehensively ICU-backed and updated with the browser.

## 22. TLS and transport security

Kotlin code that talks HTTPS goes through Ktor client (the JetBrains-maintained HTTP client), which abstracts the platform TLS stack:

| Target       | Underlying TLS                                   | Trust store                                |
|--------------|---------------------------------------------------|--------------------------------------------|
| JVM          | OkHttp engine -> JSSE (Java's TLS)                | JDK default trust store (`cacerts`)        |
| Android      | OkHttp engine -> Conscrypt (Android's TLS)        | Android system trust store                 |
| iOS / macOS  | Darwin engine -> URLSession -> Network.framework  | iOS / macOS keychain                       |
| Linux K/Native | curl engine -> libcurl + OpenSSL                | `/etc/ssl/certs/ca-certificates.crt`       |
| Windows K/Native | winhttp engine -> WinHTTP                       | Windows Certificate Store                  |
| Kotlin/JS    | Js engine -> fetch / XMLHttpRequest               | Browser / Node.js trust store              |
| Kotlin/Wasm  | Js engine -> fetch                                | Browser trust store                        |

Mochi's `fetch` builtin lowers to a `HttpClient { ... }` block in Ktor. The engine is selected per target:

```kotlin
// commonMain
expect fun mochiHttpClient(): HttpClient

// jvmMain
actual fun mochiHttpClient(): HttpClient = HttpClient(OkHttp)

// iosMain
actual fun mochiHttpClient(): HttpClient = HttpClient(Darwin)

// linuxMain
actual fun mochiHttpClient(): HttpClient = HttpClient(Curl)

// mingwMain
actual fun mochiHttpClient(): HttpClient = HttpClient(WinHttp)

// jsMain / wasmJsMain
actual fun mochiHttpClient(): HttpClient = HttpClient(Js)
```

TLS 1.3 is supported on every target. TLS 1.2 is the fallback. Older TLS (1.0, 1.1) is disabled by Ktor by default.

Certificate pinning is exposed via `HttpClient { engine { /* pinning config */ } }`; the configuration is engine-specific. Mochi wraps the common cases (pin a specific certificate by SHA-256) in a portable `MochiRuntime.TrustOverride` API.

## 23. Filesystem case-sensitivity

Identical to the Swift target's story ([[../0049/07-swift-target-portability]] §21):

- APFS, HFS+, NTFS, FAT32, exFAT: case-insensitive (case-preserving) by default.
- ext4, btrfs, xfs, zfs: case-sensitive.

Mochi treats every path as case-sensitive at the protocol level. The Mochi `mochi audit --paths` check walks the source tree at build time and flags any case mismatch between `import` statements and on-disk filenames.

For runtime file I/O, Kotlin's `java.nio.file.Path` (JVM/Android) and `kotlinx-io` (K/Native/JS/Wasm) honour the OS's case sensitivity. Mochi never lowercases paths and never canonicalises case beyond what the underlying API does.

## 24. Reproducible builds

Reproducible builds (byte-identical `.jar` / `.aab` / `.kexe` from byte-identical inputs) are gated by `TestPhase16Reproducible`. Sources of non-determinism Mochi explicitly suppresses:

- **Timestamps in `.jar` entries**: set to a fixed epoch (`SOURCE_DATE_EPOCH` env var, default 1980-01-01).
- **File ordering in `.jar`**: sorted alphabetically (`zip.setSort = true`).
- **Hashmap iteration order in generated code**: deterministic since Mochi IR uses ordered containers.
- **R8 shrink decisions**: deterministic given the same input + same R8 version + same proguard rules.
- **Gradle build timestamps**: suppressed via `org.gradle.parallel=false` + `org.gradle.caching=false` in reproducible mode.

Kotlin compiler output (`.class` files) is reproducible given the same source and same compiler version. The `@Metadata` annotation embeds a `mv` (metadata version) field that changes across Kotlin minor releases, so cross-version comparison is meaningless; same-version comparison is byte-identical.

K/Native binaries are reproducible given the same toolchain + same flags + same input. LLVM has had reproducible output since LLVM 13.

## 25. Per-target debug tooling

| Target       | Debugger                              | Profiler                                       |
|--------------|---------------------------------------|------------------------------------------------|
| JVM          | IntelliJ debugger, jdb, JDWP          | JFR (Java Flight Recorder), async-profiler    |
| Android      | Android Studio debugger, perfetto     | Perfetto, Android Profiler                     |
| iOS          | LLDB via Xcode                        | Instruments                                    |
| macOS K/Native | LLDB                                | Instruments, dtrace                            |
| Linux K/Native | LLDB or GDB (compatible)            | perf, valgrind (limited)                       |
| Windows K/Native | LLDB                                | Windows Performance Analyzer                   |
| Kotlin/JS    | Chrome DevTools, Firefox Developer    | Chrome DevTools Performance tab                |
| Kotlin/Wasm  | Chrome DevTools (Wasm debug support)  | Chrome DevTools (limited Wasm profiling)       |

Source maps are generated for Kotlin/JS (`-source-map`), enabling stack traces in Kotlin source. For Kotlin/Wasm, DWARF debug info is emitted in development builds; production builds strip it for size.

Mochi's `mochi build --debug-symbols` flag enables full debug info on every target. The default release build strips symbols (smaller binaries, no debugger attach).

## 26. Per-target packaging tooling

| Target       | Final artifact          | Tool                                  | Mochi exposes               |
|--------------|-------------------------|---------------------------------------|-----------------------------|
| JVM          | `.jar` (executable)     | `kotlinc -d output.jar`               | `mochi build --output=jar`  |
| JVM          | `.app` / `.dmg` / `.deb` / `.exe` (single-file) | `jpackage`         | `--jpackage`                |
| Android      | `.apk` / `.aab`         | AGP `assembleRelease` / `bundleRelease` | `--output=apk` / `--output=aab` |
| iOS          | `.xcframework`          | KMP plugin `XCFramework` task         | default for iOS target      |
| macOS        | `.kexe` / `.app`        | `kotlinc-native` + `lipo`             | `--output=app`              |
| Linux        | `.kexe` / `.deb` / `.rpm` | `kotlinc-native` + `fpm`/`nfpm`     | `--output=deb` / `--output=rpm` |
| Windows      | `.exe` / `.msi`         | `kotlinc-native` + WiX                | `--output=msi`              |
| Kotlin/JS    | bundled `.js` / `.mjs`  | webpack 5                             | `mochi build --target=kotlin-js --output=bundle` |
| Kotlin/Wasm  | `.wasm` + loader        | KMP plugin Wasm tasks                 | default                     |

Code signing per target:

- **JVM `.jar`**: optional signature via `jarsigner`. Mochi exposes `--jvm-sign-keystore=mykeystore.jks`.
- **Android `.apk` / `.aab`**: required for Play Store. Signed with the user's keystore via AGP's `signingConfigs` block. Mochi reads the keystore path from `mochi.toml`'s `[android.signing]`.
- **iOS `.xcframework`**: signed as part of the host app build, not by Mochi directly. Mochi documents the Xcode steps.
- **macOS `.app`**: signed via `codesign`, notarised via `notarytool`, stapled via `stapler`. Identical to MEP-49.
- **Linux `.deb` / `.rpm`**: optional GPG signing via `--linux-sign-gpg=KEYID`.
- **Windows `.exe` / `.msi`**: Authenticode signing via `signtool.exe`. Mochi exposes `--windows-signing-cert=mycert.pfx`.

## 27. Comparison with MEP-49 (Swift)

Read alongside [[../0049/07-swift-target-portability]] for the Swift counterpart.

| Dimension              | Swift (MEP-49)                          | Kotlin (MEP-50)                          |
|------------------------|------------------------------------------|------------------------------------------|
| Single-toolchain output | Native binary per (os, arch, libc)     | Fan: .jar, .aab, .kexe, .js, .wasm       |
| iOS deployment         | Native; Xcode required                  | `.xcframework`; Xcode for app shell      |
| Android deployment     | Not natively (community ports only)     | Native via AGP; first-class              |
| Linux musl             | Static Linux SDK (official)             | Not yet (workaround required)            |
| Windows               | MSVC toolchain                          | MinGW toolchain                          |
| JS / Wasm              | Not in v1                               | Native via Kotlin/JS, Kotlin/Wasm         |
| Concurrency runtime    | Swift Concurrency (actors, AsyncStream) | kotlinx.coroutines (Flow, Channel, etc.) |
| ABI stability          | Stable since Swift 5.0 (Apple platforms) | Per-target; `.klib` stable across patch |
| Embedded               | Embedded Swift (official subset)         | Not offered                              |
| FFI surface            | C interop via swift-bridging-header     | cinterop (K/Native), JNI (JVM), dynamic (JS) |

Kotlin's strongest cards are **Android first-class** (Mochi-on-Swift cannot ship to Play Store), **Kotlin/JS and Kotlin/Wasm** (Swift has neither shipped), and **Compose Multiplatform** for cross-target UI. Swift's strongest cards are **smaller iOS binaries** (system Swift runtime) and **Embedded Swift** for bare-metal.

For Mochi positioning: Kotlin is the right target when the deployment includes Android, or when the user wants one source tree to ship to both Apple platforms and Android. Swift is the right target when the deployment is Apple-only and minimum binary size matters.

---

## Sources

1. Kotlin 2.1.0 release announcement. <https://kotlinlang.org/docs/whatsnew21.html>
2. Kotlin 2.0.0 release announcement. <https://kotlinlang.org/docs/whatsnew20.html>
3. Kotlin Multiplatform default hierarchy template. <https://kotlinlang.org/docs/multiplatform-hierarchy.html>
4. Kotlin/Native targets list. <https://kotlinlang.org/docs/native-target-support.html>
5. Kotlin/JS IR backend reference. <https://kotlinlang.org/docs/js-ir-compiler.html>
6. Kotlin/Wasm announcement. <https://kotlinlang.org/docs/wasm-overview.html>
7. Android Gradle Plugin release notes. <https://developer.android.com/build/releases/gradle-plugin>
8. R8 release notes. <https://r8.googlesource.com/r8/+log>
9. Ktor client engines. <https://ktor.io/docs/http-client-engines.html>
10. JetBrains Compose Multiplatform 1.7. <https://www.jetbrains.com/lp/compose-multiplatform/>
11. kotlinx.coroutines 1.10 release notes. <https://github.com/Kotlin/kotlinx.coroutines/releases/tag/1.10.0>
12. WebAssembly GC proposal status. <https://github.com/WebAssembly/gc>
13. Chrome 119 Wasm GC announcement. <https://chromestatus.com/feature/6062463212126208>
14. Firefox 120 release notes. <https://www.mozilla.org/en-US/firefox/120.0/releasenotes/>
15. Safari 18.2 release notes. <https://developer.apple.com/documentation/safari-release-notes/safari-18_2-release-notes>
16. Android 15 release notes. <https://developer.android.com/about/versions/15>
17. Google Play targetSdk requirements. <https://support.google.com/googleplay/android-developer/answer/11926878>
18. WiX Toolset documentation. <https://wixtoolset.org/docs/>
19. nfpm packaging tool. <https://nfpm.goreleaser.com/>
20. Temurin OpenJDK distribution. <https://adoptium.net/>
21. Kotlin/Native memory model documentation. <https://kotlinlang.org/docs/native-memory-manager.html>
22. Kotlin .klib ABI stability. <https://kotlinlang.org/docs/native-libraries.html>
