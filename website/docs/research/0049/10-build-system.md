# MEP-49 research note 10, Build system: SwiftPM, Xcode, xcodebuild, notarization

Author: research pass for MEP-49.
Date: 2026-05-23 (GMT+7).

This note specifies how `mochi build --target=swift-...` produces Swift artefacts: how the codegen emits a SwiftPM package, how `swift build` drives the actual compilation, how cross-compilation works via the Static Linux SDK, how iOS / macOS app bundles flow through `xcodebuild` and `notarytool`, how the runtime library is published to the Swift Package Index, and how the Mochi CI matrix is shaped around the toolchains Apple supplies versus the swift.org cross-platform builds.

The companion notes for the JVM target ([[../0047/10-build-system]]) and the .NET target ([[../0048/10-build-system]]) cover the same territory for those runtimes; structurally this note mirrors them. Swift 6.0 is the floor language version. Strict concurrency is on by default per [[02-design-philosophy]] and the runtime described in [[04-runtime]].

---

## 1. SwiftPM as canonical build driver

SwiftPM (`swift build`, `swift test`, `swift run`, `swift package`) is the canonical driver for every Mochi Swift target. The Mochi codegen does not invoke `swiftc` directly. We emit a deterministic `Package.swift` at the output root and shell out to `swift build`, letting SwiftPM resolve the dependency graph, schedule per-target compilation, and choose between incremental and clean rebuilds.

The reason for this layering matches MEP-47's choice to drive Gradle / Maven rather than re-implement them: SwiftPM already handles toolchain selection (`xcrun --toolchain`), platform conditionals (`.when(platforms:)`), resource bundling (`.process`, `.copy`), test discovery (`@Suite`, `@Test` via Swift Testing as well as legacy XCTest), and cross-compilation (`--swift-sdk`). Re-implementing any of that would create a parallel build graph that drifts from upstream the moment Swift 6.1 ships.

Mochi's build driver invokes SwiftPM in three modes:

1. **Library / executable**: `swift build --configuration release --package-path target/swift/`. Pure SwiftPM, no Xcode required. Works on macOS, Linux, Windows.
2. **iOS / watchOS / tvOS / visionOS app bundle**: SwiftPM emits a static library; `xcodebuild` consumes a generated `.xcodeproj` referencing that library and produces an `.ipa` (or `.app` for Simulator).
3. **Cross-compile**: `swift build --swift-sdk <triple>` selects an installed Swift SDK bundle. For Linux, the Static Linux SDK from swift.org. For other triples, the Cross-compilation SDK family.

The Mochi binary expects a Swift toolchain on `PATH`: `swift --version` must report >= 6.0. On macOS this is the Xcode-bundled toolchain (`/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/`); on Linux and Windows it is the swift.org installer.

## 2. Package.swift structure

The codegen writes a single `Package.swift` at `target/swift/Package.swift`. Format is fixed and deterministic so reproducible-build verification (§19) can byte-compare across runs.

```swift
// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MochiUserApp",
    platforms: [
        .macOS(.v14),
        .iOS(.v17),
        .watchOS(.v10),
        .tvOS(.v17),
        .visionOS(.v1),
    ],
    products: [
        .executable(name: "mochi-user-app", targets: ["MochiUserApp"]),
        .library(name: "MochiUserAppCore", targets: ["MochiUserAppCore"]),
    ],
    dependencies: [
        .package(url: "https://github.com/mochilang/swift-runtime", from: "1.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "MochiUserApp",
            dependencies: [
                "MochiUserAppCore",
                .product(name: "MochiRuntime", package: "swift-runtime"),
            ],
            path: "Sources/MochiUserApp",
            swiftSettings: [
                .enableExperimentalFeature("StrictConcurrency"),
                .enableUpcomingFeature("ExistentialAny"),
            ]
        ),
        .target(
            name: "MochiUserAppCore",
            dependencies: [
                .product(name: "MochiRuntime", package: "swift-runtime"),
            ],
            path: "Sources/MochiUserAppCore",
            resources: [.process("Resources")]
        ),
        .testTarget(
            name: "MochiUserAppTests",
            dependencies: ["MochiUserAppCore"],
            path: "Tests/MochiUserAppTests"
        ),
    ],
    swiftLanguageModes: [.v6]
)
```

The codegen splits user code into a `Core` library plus a thin `executable` shim so that iOS / watchOS targets, which cannot ship as `.executableTarget` directly, can still pull in the core library through Xcode. The library / executable split is the same trick MEP-47 uses for the `module-info` / `Main-Class` separation.

`swiftLanguageModes: [.v6]` is the Swift 6.0 floor. Mixed-mode packages (some targets in Swift 5 mode, some in Swift 6) are supported by SwiftPM but Mochi does not emit them; every Mochi-generated target is Swift 6 with strict concurrency on.

## 3. Mochi.Runtime as SwiftPM dependency

The Mochi Swift runtime is published as the `swift-runtime` repository under the `mochilang` GitHub org. SwiftPM consumes it via the standard URL form:

```swift
.package(url: "https://github.com/mochilang/swift-runtime", from: "1.0.0"),
```

Coordinates:

- Repository: `https://github.com/mochilang/swift-runtime`
- Library product: `MochiRuntime`
- Module name: `MochiRuntime`
- License: Apache-2.0
- Version: matches the Mochi binary release. Mochi 1.0 ships `swift-runtime 1.0.0`; Mochi 1.1 ships `swift-runtime 1.1.0`. Semver matches Mochi binary semver.

The runtime is also indexed at the [Swift Package Index](https://swiftpackageindex.com/mochilang/swift-runtime), which provides documentation hosting (built via DocC), platform-compatibility badges, and a CI status surface that mirrors the per-platform matrix in §22. The Package Index is a community-run service but is the de facto registry for Swift open source: indexing there is the lowest-friction way to make `MochiRuntime` discoverable without waiting for Apple's draft Package Registry RFC to land (see §24).

Internal layout of `swift-runtime`:

```
swift-runtime/
├── Package.swift
├── Sources/
│   ├── MochiRuntime/             # Public Swift API
│   ├── MochiRuntimeC/            # C shims for fast paths
│   └── MochiRuntimeBridge/       # Foundation bridging
├── Tests/
│   └── MochiRuntimeTests/
└── docs/                          # DocC catalog
```

The `MochiRuntimeC` target is a `.target(type: .C)` so SwiftPM picks the right compiler for the `.c` source files (UTF-8 transcoding fast paths, SIMD-accelerated hash routines). `MochiRuntimeBridge` is Foundation-bridged code; on Apple platforms it uses the system Foundation, on Linux and Windows it uses swift-corelibs-foundation.

## 4. Per-target build artefacts

`swift build` deposits everything under `.build/`. The layout depends on configuration and triple:

```
.build/
├── debug/                          # swift build --configuration debug
│   ├── MochiUserApp                # executable (no extension on Unix)
│   ├── MochiUserAppCore.o
│   ├── libMochiRuntime.a           # or .dylib / .so depending on product type
│   └── ModuleCache/
├── release/                        # swift build --configuration release
│   ├── MochiUserApp
│   ├── MochiUserAppCore.o
│   └── libMochiRuntime.a
├── arm64-apple-macosx/             # per-triple subdir under cross-compile
└── checkouts/                      # SwiftPM-resolved package sources
```

`.executableTarget` produces a binary at `.build/<config>/<name>`. `.target` (library) produces an `.o` per source file plus a `.swiftmodule` interface file. SwiftPM links library targets into the consuming executable statically by default; this is the v1 choice for Mochi because we ship single-file binaries on Linux (see §17) and code-signed bundles on macOS / iOS where dynamic linking buys nothing.

Mochi mirrors the SwiftPM output under `target/swift/.build/` instead of the SwiftPM default `.build/` so the parent `target/` directory cleanly contains every artefact and a single `rm -rf target/` is a full clean.

## 5. Build configurations

Three configurations matter:

| Configuration | Flags                                     | Use case                       |
|---------------|-------------------------------------------|--------------------------------|
| debug         | `-Onone -g`                               | Inner dev loop                 |
| release       | `-O -whole-module-optimization`           | Default for `mochi build`      |
| size          | `-Osize -whole-module-optimization`       | Embedded, watchOS              |

Mochi defaults to `release` for `mochi build`. The reasoning: a transpiler is shipped to end users who run the resulting binary; the user almost never cares about Mochi-compile-time speed once they hit `mochi build`, and `-Onone` binaries are slow enough that benchmark numbers (see [[11-testing-gates]]) would be meaningless.

`mochi run` uses `debug` because the inner dev loop wants fast iteration; that path goes through `swift run --configuration debug`.

`mochi build --size` opts into `-Osize`. The intended user is the watchOS / visionOS developer who is up against a binary size budget. Apple's watchOS apps still cap at 75 MB compressed for the executable slice; `-Osize` typically trims 15-20% off a `-O` build.

## 6. Whole-module optimisation (WMO)

`-O -whole-module-optimization` is on by default in `release`. Swift's compilation model is per-module, and WMO lets the optimiser see every function in the module at once, enabling cross-file specialisation, generic specialisation, and dead-code elimination that a per-file compile cannot do.

Mochi codegen exploits WMO in two ways:

1. Public symbols on the boundary of `MochiUserAppCore` (the library half of the split in §2) are marked `@inlinable` and `@usableFromInline` where the body is short enough to specialise across module boundaries. This is the same pattern the Swift Standard Library uses to keep `Array<Int>.append(_:)` cheap across module boundaries.
2. Closures that capture concrete types are emitted without the existential-any box, so WMO can specialise them. Where the type is genuinely unknown, Mochi emits `any` explicitly (Swift 6 requires it under `ExistentialAny`).

```swift
@inlinable
public func mochi_map<T, U>(_ xs: [T], _ f: (T) -> U) -> [U] {
    var out: [U] = []
    out.reserveCapacity(xs.count)
    for x in xs { out.append(f(x)) }
    return out
}
```

The `@inlinable` annotation requires the body to be visible in the module interface (`.swiftinterface`), so Mochi codegen never references private types from `@inlinable` bodies. The lowering pass in [[06-type-lowering]] enforces this invariant.

## 7. Cross-compilation via SDK

SwiftPM's `--swift-sdk` flag selects a Swift SDK bundle. An SDK bundle is a self-contained directory containing the cross sysroot, the cross-built Swift standard library, and a manifest that tells SwiftPM how to invoke the compiler and linker for the target triple.

```
mochi build --target=swift-linux-x86_64
```

becomes:

```
swift build \
  --swift-sdk x86_64-swift-linux-musl \
  --configuration release \
  --package-path target/swift/
```

The SDK identifier `x86_64-swift-linux-musl` resolves to the Static Linux SDK bundle (see §8). Other supported identifiers in v1:

| Mochi target               | SwiftPM `--swift-sdk` value           | Notes                                |
|----------------------------|---------------------------------------|--------------------------------------|
| `swift-linux-x86_64`       | `x86_64-swift-linux-musl`             | Static, musl libc                    |
| `swift-linux-aarch64`      | `aarch64-swift-linux-musl`            | Static, musl libc                    |
| `swift-linux-x86_64-gnu`   | `x86_64-unknown-linux-gnu`            | Dynamic, glibc                       |
| `swift-windows-x86_64`     | `x86_64-pc-windows-msvc`              | MSVC ABI, dynamic                    |
| `swift-android-aarch64`    | `aarch64-unknown-linux-android24`     | NDK level 24 (matches Mochi Android) |

`mochi build` without `--target=` defaults to the host triple. The build driver also accepts `mochi build --target=swift-host` as an explicit alias.

## 8. Static Linux SDK

The Static Linux SDK is swift.org's official cross-compilation bundle for producing fully static Linux binaries on any host (macOS or Linux). It bundles musl libc and a statically linked Swift runtime, so the resulting binaries have no dynamic-linker dependency and run on any Linux kernel >= 3.10.

Install once per developer / CI runner:

```
swift sdk install \
  https://download.swift.org/swift-6.0-release/static-sdk/swift-6.0-RELEASE/swift-6.0-RELEASE_static-linux-0.0.1.artifactbundle.tar.gz \
  --checksum 67f765e0030e661a7450f7e4877cfe008db4f57f177d5a08e6ed26d9c1a883ca
```

Verify:

```
swift sdk list
# x86_64-swift-linux-musl
# aarch64-swift-linux-musl
```

Mochi v1 uses this SDK for the single-binary Linux story. The resulting binary is statically linked against musl libc, the Swift stdlib, Foundation, and the Mochi runtime; `ldd ./mochi-user-app` reports `not a dynamic executable`, and the binary runs on Alpine, Debian, RHEL, distroless, and `scratch` containers without modification.

The trade-off is binary size: a `hello world` static binary is ~12 MB, versus ~600 KB for a dynamic build against glibc. For the v1 portability story we accept the size cost; users who care about size can opt into `--target=swift-linux-x86_64-gnu` and ship `.deb` / `.rpm` packages that link against the system Swift runtime.

## 9. xcodebuild for iOS apps

SwiftPM cannot directly produce a signed `.ipa` for App Store submission. SwiftPM produces libraries; Apple's app-store deliverable format requires an Xcode project (or workspace) that knows about provisioning profiles, entitlements, asset catalogs, and the `Info.plist` layout an app bundle needs.

Mochi's iOS pipeline therefore emits two artefacts:

1. The SwiftPM package from §2 (containing the user's Swift source under `Sources/MochiUserAppCore/`).
2. An Xcode project at `target/swift/ios/MochiUserApp.xcodeproj` that references the SwiftPM package as a local dependency and adds an iOS `App` target containing `@main struct MochiUserAppApp: App { ... }`.

`xcodebuild` then drives the archive:

```
xcodebuild \
  -project target/swift/ios/MochiUserApp.xcodeproj \
  -scheme MochiUserApp \
  -configuration Release \
  -destination 'generic/platform=iOS' \
  -archivePath build/MochiUserApp.xcarchive \
  archive

xcodebuild \
  -exportArchive \
  -archivePath build/MochiUserApp.xcarchive \
  -exportOptionsPlist export-options.plist \
  -exportPath build/ipa/
```

`export-options.plist` is generated by Mochi based on `mochi.toml` (signing identity, team ID, provisioning profile UUID, distribution method).

## 10. Xcode project generation

Generating a working `.xcodeproj` by hand is painful; the `project.pbxproj` format is undocumented, line-noisy, and changes shape between Xcode releases. Mochi takes two paths:

1. **XcodeGen** (Apache-2.0 licensed, declarative YAML to `.xcodeproj`): Mochi emits a `project.yml` and shells out to `xcodegen generate`. XcodeGen is commercial-friendly and stable; it tracks Xcode's project format additions and Mochi inherits that maintenance.
2. **SwiftPM iOS app plugins** (experimental in Swift 5.9, stabilising in Swift 6.x): SwiftPM gained the ability to build iOS apps directly via build-tool plugins. The flow is still rough on Swift 6.0 (asset catalog handling, storyboard compilation, Info.plist merging) but tightens with each Swift release.

Mochi v1 ships with XcodeGen as the default path because it is the only path that produces an Xcode project archive byte-identical to what an Xcode user would commit. The SwiftPM-plugin path is opt-in via `mochi build --target=swift-ios --no-xcodegen` and is documented as preview.

Sample `project.yml` (XcodeGen):

```yaml
name: MochiUserApp
options:
  bundleIdPrefix: dev.mochi.userapp
  deploymentTarget:
    iOS: "17.0"
packages:
  MochiUserAppCore:
    path: ".."
targets:
  MochiUserApp:
    type: application
    platform: iOS
    sources:
      - path: Sources
    dependencies:
      - package: MochiUserAppCore
    info:
      path: Info.plist
      properties:
        CFBundleDisplayName: Mochi User App
```

## 11. Swift toolchain bundling

The Swift toolchain is large (~700 MB unpacked on macOS, ~1.5 GB on Linux including Foundation, Dispatch, XCTest, Swift Testing). Mochi does not bundle a toolchain in the `mochi` binary; we require a `swift` on `PATH`.

Platform sources:

- **macOS / iOS / watchOS / tvOS / visionOS**: bundled with Xcode under `/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/`. Mochi probes `xcrun --find swift` to locate it. Users who install Xcode have a usable toolchain automatically.
- **Linux**: swift.org tarball, or distribution package (`apt install swiftlang` on Ubuntu 24.04 from the swift.org PPA). Mochi probes `swift --version`.
- **Windows**: swift.org `.exe` installer adds `swift` to `PATH`. Visual Studio 2022 Build Tools with the C++ workload is a hard prerequisite (Swift uses the MSVC linker).

A pinned toolchain inside Mochi's repo (via `.swift-version`) drives the swiftly toolchain manager when present: `swiftly install 6.0` materialises the toolchain on a fresh CI runner with a single command, which is how the CI matrix in §22 keeps the toolchain version locked.

## 12. Bundling resources

Mochi's `embed!` directive (compile-time string / bytes embed) and the broader `resources/` directory pattern lower to SwiftPM resource declarations:

```swift
.target(
    name: "MochiUserAppCore",
    resources: [
        .process("Resources"),           // .png, .json, .txt: SwiftPM applies any platform-specific processing
        .copy("Datasets/seed.csv"),      // verbatim copy, no processing
    ]
)
```

At runtime, the user accesses bundled resources via the auto-generated `Bundle.module`:

```swift
let url = Bundle.module.url(forResource: "seed", withExtension: "csv")!
let data = try Data(contentsOf: url)
```

Mochi's lowering of `embed!("data/seed.csv")` emits exactly this pattern, hidden behind a runtime helper `MochiRuntime.embeddedResource(_:)` so user code does not have to type `Bundle.module` directly.

`.process(_:)` applies platform-aware processing: on iOS, `.xcassets` catalogs get compiled by `actool`; `.storyboard` and `.xib` files get compiled by `ibtool`; everything else is copied. On Linux and Windows, `.process` is a verbatim copy.

## 13. Codesign

macOS and iOS binaries must be code-signed before they can run outside the developer's machine. Unsigned macOS binaries open with a Gatekeeper warning; unsigned iOS binaries cannot be installed at all (the only exception is the simulator).

The Mochi build pipeline shells out to Apple's `codesign` tool after the binary is produced and before notarization (§14). Identity selection follows `mochi.toml`:

```toml
[swift.signing]
identity = "Developer ID Application: Mochi Lang Inc. (ABCDE12345)"
entitlements = "Resources/MochiUserApp.entitlements"
```

The driver invokes:

```
codesign \
  --sign "Developer ID Application: Mochi Lang Inc. (ABCDE12345)" \
  --options runtime \
  --entitlements Resources/MochiUserApp.entitlements \
  --timestamp \
  --deep \
  build/MochiUserApp.app
```

`--options runtime` enables the hardened runtime, which is required for notarization (binaries without hardened runtime are rejected by `notarytool`). `--timestamp` adds a secure timestamp from Apple's timestamping service; without it, the signature becomes invalid when the signing certificate expires.

The signing identity must be installed in the user's login keychain. For CI, the Mochi `release.yml` workflow imports a base64-encoded `.p12` into a temporary keychain on each run, then deletes the keychain at job end.

## 14. Notarization

Mac apps distributed outside the Mac App Store must be notarized through Apple's notarization service or Gatekeeper will block first-launch. iOS apps go through a separate App Store Connect path and are never notarized standalone.

```
xcrun notarytool submit \
  build/MochiUserApp.zip \
  --apple-id "release@mochi.dev" \
  --team-id "ABCDE12345" \
  --password "@keychain:AC_PASSWORD" \
  --wait
```

`--wait` blocks until Apple's service finishes scanning the bundle (typically 30 seconds to 5 minutes for a small app, longer for larger archives). The exit code reflects accept / reject. `notarytool` replaced `altool` in 2022; `altool` notarization endpoints were turned off on 2023-11-01 and stay off.

For CI, the `@keychain:AC_PASSWORD` indirection looks up a stored app-specific password from the temporary keychain set up in §13. Apple no longer accepts the developer's primary password for notarization; only app-specific passwords or App Store Connect API keys are honoured.

## 15. Stapling

After notarization succeeds, the notarization ticket is recorded in Apple's CRL servers. Gatekeeper will fetch the ticket on first launch over the network; the cleaner UX is to staple the ticket directly into the bundle so offline launches still verify cleanly:

```
xcrun stapler staple build/MochiUserApp.app
```

After stapling, `spctl --assess --type execute --verbose build/MochiUserApp.app` reports `accepted source=Notarized Developer ID`. The stapled ticket survives `tar`, `zip`, and `.dmg` archiving as long as the `.app` directory structure stays intact.

Mochi's release driver runs `stapler staple` automatically after a successful `notarytool submit --wait`. The two commands are paired in every Mochi release pipeline and on every developer-machine `mochi release` invocation.

## 16. TestFlight / App Store Connect

Mochi v1 documents the distribution path but does not automate App Store submission. The reason is that App Store Connect's API requires sustained per-team metadata (screenshots, App Privacy questionnaires, content ratings) that does not lend itself to a one-shot CLI command.

The supported v1 path for distributing an iOS app:

1. `mochi build --target=swift-ios --configuration release`.
2. `xcrun notarytool submit ... --wait` (Mac binaries) or `xcrun altool --upload-app` (deprecated).
3. For TestFlight upload, the modern path uses `xcrun notarytool` for Mac and `xcrun altool` for iOS, with `altool` planned for deprecation; current Xcode (16.x as of 2026 Q1) still ships both.

Once App Store Connect's full submission API stabilises across SKUs, Mochi will add `mochi release --target=swift-ios` that wraps the upload step. For now, Mochi emits the `.ipa` and points the user at Transporter.app (Apple's official upload tool) or `xcrun altool --upload-app`.

## 17. Linux distribution shapes

Three shapes, each appropriate for a different audience:

**a. Static binary via Static Linux SDK (musl):** Default for `mochi build --target=swift-linux-x86_64`. One file, no dependencies, runs on every Linux kernel >= 3.10. Best for cloud-native deployments, distroless containers, and "drop on a server" usage. Binary size ~12 MB for hello-world.

**b. Dynamic binary linking against system Swift runtime:** Opt-in via `--target=swift-linux-x86_64-gnu`. Smaller binary (~600 KB) but requires the user to install the Swift runtime separately. Mochi emits a `.deb` (via `dpkg-deb`) and `.rpm` (via `rpmbuild`) that declares `Depends: swiftlang (>= 6.0)`. Best for Linux distribution maintainers.

**c. Container image:** Opt-in via `--target=swift-docker`. Emits a `Dockerfile` of the form:

```dockerfile
FROM swift:6.0 AS build
WORKDIR /app
COPY . .
RUN swift build --configuration release

FROM swift:6.0-slim
COPY --from=build /app/.build/release/MochiUserApp /usr/local/bin/
ENTRYPOINT ["MochiUserApp"]
```

`mochi build --target=swift-docker` chains `docker build .` after the Dockerfile emission. The final image is ~250 MB (Swift runtime base) versus ~12 MB for the static binary; the container path is for users who already have a Docker deployment pipeline and want a familiar artefact.

Mochi defaults to **a** (static binary) for portability. The static binary works in every other shape (FROM scratch in Docker, dropped into `/usr/local/bin/` on any distro, included in a `.deb` payload directly).

## 18. Windows distribution

Windows distribution is the least mature of the three desktop OSes. Swift on Windows works (the swift.org installer is reliable; SwiftPM builds correctly against MSVC), but the deployment story is hand-rolled because Apple ships no Windows-specific packaging tools.

Shapes:

- **Plain `.exe` plus Swift runtime DLLs**: The output of `swift build --configuration release` on a Windows runner. Mochi copies the executable plus the Swift runtime DLLs from `%SDKROOT%\usr\bin\` into `dist/`. The user double-clicks; SmartScreen warns once until the binary is reputation-trusted.
- **MSI installer via WiX Toolset**: The classic Windows packaging path. Mochi emits a `Product.wxs` and shells out to `candle.exe` + `light.exe`. WiX is open source (MS-RL); v4 (released 2023) is the current major version and `wix.exe` is the supported entry point.
- **MSIX for Microsoft Store**: The Store-required format. Mochi emits a `Package.appxmanifest` and shells out to `makeappx.exe` from the Windows SDK. The signing identity must be a Microsoft Store certificate, separate from the Authenticode certificate used for `.exe` and `.msi` signing.

Mochi v1 ships path 1 by default and path 2 (`mochi build --target=swift-windows-x86_64-msi`) as the recommended distribution path for non-Store apps. MSIX support is preview.

## 19. Build determinism

Bit-identical output across machines is a project-wide invariant. For Swift artefacts:

- **Compiler output**: `swiftc --enable-deterministic-build` (Swift 6.0+) plus `-no-clang-module-breadcrumbs` produces deterministic `.o` files for a given source tree.
- **SwiftPM cache**: `swift build --cache-path target/swift/.cache --build-path target/swift/.build` keeps caches in the project tree, not in `~/.cache/`, so two CI runners with empty caches produce identical first-build outputs.
- **Environment**: `SWIFTPM_DETERMINISTIC_BUILD=1` disables non-deterministic features in SwiftPM itself (random build orderings, timestamp embedding in `.swiftmodule`).
- **Linker**: macOS `ld` embeds timestamps and randomised build IDs by default; Mochi passes `-Xlinker -no_uuid -Xlinker -no_dtrace_dof` to suppress them. Linux uses `lld` with `--build-id=none`.
- **Static archive**: `ar` embeds mtime; Mochi sets `SOURCE_DATE_EPOCH` to the most recent source-file mtime, which gives deterministic timestamps without freezing to epoch zero.

Verification: Mochi CI rebuilds the runtime on two different runners (macOS-15 and ubuntu-24.04 cross-compiling to macOS via the macOS SDK) and `diffoscope`s them. Any non-determinism is treated as a release-blocking bug. The pattern mirrors MEP-47's reproducible-build gate.

## 20. CMake interop

Swift integrates with CMake-based projects through CMake's first-party Swift support (`enable_language(Swift)`, CMake 3.16+). This is the path the Swift compiler itself takes: the swift.org build is CMake-driven.

For Mochi, CMake interop is out of scope for v1. The reasoning: Mochi targets new Swift codebases or codebases willing to adopt SwiftPM, and `swift-create-bundle` / `swift package generate-xcodeproj` give us paths into Xcode workflows already. Users with CMake-driven Swift projects can consume the `MochiRuntime` library via `swift package` plus a thin CMake `add_custom_command` shim that invokes `swift build`, but Mochi does not ship a `mochi.cmake` module.

If user demand surfaces (likely from the Swift-on-server crowd that mixes Swift with C++ CMake builds), `mochi build --target=swift-cmake` becomes a v2 deliverable.

## 21. Bazel rules_swift

Bazel has first-party Swift support via [rules_swift](https://github.com/bazelbuild/rules_swift). It supports SwiftPM-style packages through the `swift_package` rule, integrates with `rules_apple` for iOS / macOS app bundles, and provides hermetic builds with content-addressed caches.

For Mochi, Bazel is a v2 target. The case for it: hermetic CI is increasingly the default at large engineering orgs; Bazel's remote cache lets a 1000-engineer team share a single warmed cache so first-time clones build instantly. The case against: rules_swift's integration with SwiftPM's package resolution lags upstream SwiftPM by 6-12 months, which means new SwiftPM features (`--swift-sdk`, Swift Testing) take a year to flow through.

Mochi v1 leaves Bazel users with the same opt-out path as MEP-47 leaves Bazel JVM users: `bazel build //:mochi-runtime` via a hand-rolled `BUILD.bazel` that wraps the published `MochiRuntime` library. v2 adds `mochi build --target=swift-bazel` that emits a complete `BUILD.bazel` for the user project.

## 22. CI matrix

The Mochi project ships its runtime plus the `mochi` binary from a matrix that covers every triple we promise. GitHub Actions is the canonical CI provider (matches the JVM and .NET targets).

```yaml
name: Swift CI
on: [push, pull_request]
jobs:
  apple:
    strategy:
      matrix:
        os: [macos-15]
        sdk: [macos, iphonesimulator, watchsimulator, xrsimulator]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: swift-actions/setup-swift@v2
        with:
          swift-version: '6.0'
      - run: mochi build --target=swift-${{ matrix.sdk }}
      - run: mochi test --target=swift-${{ matrix.sdk }}

  device:
    runs-on: [self-hosted, macOS, arm64, iphone-attached]
    steps:
      - uses: actions/checkout@v4
      - run: mochi test --target=swift-ios-device

  linux:
    strategy:
      matrix:
        os: [ubuntu-24.04, ubuntu-24.04-arm]
        target: [gnu, musl]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: swift-actions/setup-swift@v2
        with:
          swift-version: '6.0'
      - run: swift sdk install --checksum ${{ env.STATIC_SDK_SHA }} ${{ env.STATIC_SDK_URL }}
        if: matrix.target == 'musl'
      - run: mochi build --target=swift-linux-${{ matrix.target }}

  windows:
    runs-on: windows-2025
    steps:
      - uses: actions/checkout@v4
      - uses: compnerd/gha-setup-swift@main
        with:
          branch: swift-6.0-release
          tag: 6.0-RELEASE
      - run: mochi build --target=swift-windows-x86_64
```

The `device` job runs on a self-hosted runner with a physical iPhone attached via USB. Device-attached integration tests catch regressions that simulator runs miss (Metal differences, low-level networking, AVFoundation hardware paths). The self-hosted runner is the minimum-cost way to keep an iPhone in the loop; managed CI providers do not offer device-attached macOS runners.

## 23. Swift toolchain caching

Swift toolchains are large (~700 MB on macOS, ~1.5 GB on Linux). The actions/setup-swift family caches them via `actions/cache@v4` keyed on the Swift version string plus the runner OS. Mochi's CI pins the cache key to `swift-6.0-RELEASE-${{ runner.os }}-${{ runner.arch }}` so a flag day to Swift 6.1 invalidates the cache cleanly.

The cache hit rate stays above 95% for the macOS runners; the cold-fetch cost on a cache miss is ~90 seconds on a `macos-15` runner. For self-hosted runners the cache lives on local disk and the cost is near-zero.

For Linux musl cross-compile, the Static Linux SDK is a separate cache layer because the SDK is published independently of the host toolchain. Mochi's CI caches the SDK under `~/.config/swiftpm/swift-sdks/` keyed on the SDK SHA-256 from the `swift sdk install` command.

Windows toolchain caching is more fragile because the swift.org Windows installer registers itself in the registry; cache restoration requires re-running the installer's registry steps. `compnerd/gha-setup-swift` handles this automatically; Mochi follows its conventions.

## 24. Package registry

Swift Package Index (`swiftpackageindex.com`) is the de facto registry today. It indexes Swift packages by Git URL, builds documentation via DocC, and provides platform compatibility badges. Mochi's `swift-runtime` repository publishes there at `https://swiftpackageindex.com/mochilang/swift-runtime`.

Apple's draft [Swift Package Registry RFC](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0292-package-registry-service.md) defines a content-addressed registry protocol independent of Git URLs. The protocol stabilised in SE-0292 and is implemented in SwiftPM 5.7+, but no major registry has launched: `swiftpackageregistry.com` (community-run) is the only public deployment, and adoption is < 1% of the Swift ecosystem.

Mochi v1 publishes via:

1. Git tag on `mochilang/swift-runtime` (the SwiftPM canonical source).
2. Swift Package Index registration (manual, one-time).
3. Swift Package Registry endpoint (preview, content-addressed manifest).

Once Apple ships a first-party Swift Package Registry (rumoured for Swift 6.2 timeframe), Mochi adopts it as the primary publishing channel and demotes the Git URL path to a mirror.

## 25. Sample Package.swift for a Mochi project

The codegen output for a small Mochi project (one `.mochi` source file, no extra dependencies):

```swift
// swift-tools-version: 6.0
// Generated by mochi v1.0.0. Do not edit by hand; rerun `mochi build` to regenerate.
import PackageDescription

let package = Package(
    name: "Hello",
    platforms: [
        .macOS(.v14),
        .iOS(.v17),
    ],
    products: [
        .executable(name: "hello", targets: ["Hello"]),
    ],
    dependencies: [
        .package(url: "https://github.com/mochilang/swift-runtime", from: "1.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "Hello",
            dependencies: [
                .product(name: "MochiRuntime", package: "swift-runtime"),
            ],
            path: "Sources/Hello",
            swiftSettings: [
                .enableUpcomingFeature("ExistentialAny"),
                .enableUpcomingFeature("StrictConcurrency"),
                .unsafeFlags(["-warnings-as-errors"], .when(configuration: .release)),
            ]
        ),
        .testTarget(
            name: "HelloTests",
            dependencies: ["Hello"],
            path: "Tests/HelloTests"
        ),
    ],
    swiftLanguageModes: [.v6]
)
```

Running `swift build --package-path target/swift/ --configuration release` against this manifest produces `.build/release/hello`, a single executable that the Mochi build driver then copies to `target/swift/hello` (or `target/swift/hello.exe` on Windows). For the iOS / macOS app shapes, the same package is consumed by an XcodeGen-generated `.xcodeproj` (see §10).

## 26. Summary

The Swift build system reuses the SwiftPM and Xcode ecosystems at every layer:

- `swift build` for compilation, with `--configuration release` and WMO on by default.
- `swift sdk install` for cross-compilation to Linux musl and other triples.
- `xcodebuild archive` for iOS / macOS / watchOS / tvOS / visionOS app bundles.
- `codesign`, `notarytool`, and `stapler` for macOS distribution outside the Mac App Store.
- Swift Package Index for runtime publication; Apple's draft Package Registry for the future.
- GitHub Actions runners (`macos-15`, `ubuntu-24.04`, `ubuntu-24.04-arm`, `windows-2025`) for the CI matrix.

Mochi adds Swift-specific glue (the deterministic `Package.swift` emitter, the XcodeGen wrapper, the codesign / notarize / staple chain, the Static Linux SDK auto-install) but contributes nothing new to the Swift build space itself. The same way MEP-47 leaned on Gradle, Maven, jlink, and jpackage without inventing a new JVM build tool, MEP-49 leans on SwiftPM, Xcode, and notarytool without inventing a new Swift build tool.

---

## Sources

1. Swift Package Manager documentation. https://www.swift.org/documentation/package-manager/
2. Swift 6.0 release notes. https://www.swift.org/blog/swift-6/
3. SE-0292: Package Registry Service. https://github.com/swiftlang/swift-evolution/blob/main/proposals/0292-package-registry-service.md
4. Static Linux SDK for Swift. https://www.swift.org/documentation/articles/static-linux-getting-started.html
5. Apple Developer: Notarizing macOS software before distribution. https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution
6. notarytool man page. https://keith.github.io/xcode-man-pages/notarytool.1.html
7. stapler man page. https://keith.github.io/xcode-man-pages/stapler.1.html
8. xcodebuild man page. https://keith.github.io/xcode-man-pages/xcodebuild.1.html
9. Swift Package Index. https://swiftpackageindex.com/
10. XcodeGen. https://github.com/yonaskolb/XcodeGen
11. swift-actions/setup-swift. https://github.com/swift-actions/setup-swift
12. compnerd/gha-setup-swift. https://github.com/compnerd/gha-setup-swift
13. Bazel rules_swift. https://github.com/bazelbuild/rules_swift
14. WiX Toolset v4. https://wixtoolset.org/
15. Swift on Windows documentation. https://www.swift.org/install/windows/
16. SE-0444: Member-Import Visibility (used in module surface lowering). https://github.com/swiftlang/swift-evolution/blob/main/proposals/0444-member-import-visibility.md
17. Swift compiler determinism flags. https://github.com/swiftlang/swift/blob/main/docs/DeterministicBuilds.md
