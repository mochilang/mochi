# MEP-49 research note 07, Swift target portability and version matrix

Author: research pass for MEP-49.
Date: 2026-05-23 10:11 (GMT+7).

This note pins down the Swift toolchain version policy, Apple platform deployment targets, Linux distribution matrix, Windows shipping plan, embedded/SwiftWasm posture, packaging modes (`.app`, `.ipa`, `.deb`, `.msi`, MSIX), cross-compilation rules, locale/TLS/filesystem assumptions, and binary size targets for Mochi-on-Swift. Companion notes: [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The Swift target sits on top of the Swift 6 language mode, the Swift Package Manager (`swift build`, `swift run`, `swift test`) and the platform SDK shipping with the toolchain (Apple platforms via Xcode, Linux/Windows via swift.org). Unlike the JVM target (see the sibling MEP-47 note), Swift is not a vendor-neutral bytecode runtime: it is an AOT-only toolchain whose shipped artifact is a native binary per (os, arch, libc) triple. The portability story therefore needs three independent axes: Swift language version (6.0 floor, 6.1 ceiling, 6.2 advisory), platform (Apple OS family, Linux, Windows), and CPU arch (arm64, x86_64).

---

## 1. Swift version matrix and our position

Swift adopted a roughly twelve-month major-release cadence after Swift 5.0 (March 2019). Swift 6.0 was the first language mode with a complete data-race-safe concurrency model and is the floor for MEP-49.

| Version | GA            | Xcode      | Status            | Mochi support       |
|---------|---------------|------------|-------------------|---------------------|
| 5.9     | Sept 2023     | Xcode 15.0 | Older, deprecated | unsupported         |
| 5.10    | March 2024    | Xcode 15.3 | Older, deprecated | unsupported         |
| 6.0     | Sept 2024     | Xcode 16.0 | Stable LTS-like   | minimum floor       |
| 6.1     | March 2025    | Xcode 16.3 | Stable            | CI ceiling, gating  |
| 6.2     | Sept 2025     | Xcode 17.0 | Stable            | advisory, CI smoke  |
| 6.3     | March 2026    | Xcode 17.3 | Recent            | tracked, not gating |

Swift 6.0 is our **floor**. Mochi-emitted Swift source targets `swift-tools-version:6.0` and `// swift-tools-version: 6.0` in the generated `Package.swift`. The runtime module (`MochiRuntime`) is compiled with `-swift-version 6` and produces a Swift 6 module interface (`.swiftinterface`), which is forward-compatible with later toolchains because the module ABI on Apple platforms has been stable since Swift 5.0 (Sept 2019) and the textual `.swiftinterface` is stable across minor releases.

Swift 6.1 is the **CI ceiling**: every gate runs against it, and we exercise features that became stable in 6.1 (`InlineArray<N, T>` syntax sugar, refined typed throws in standard library APIs, `Span<T>` as a non-escapable type). We do **not** emit code that requires 6.1, since that would lock out 6.0 users.

Swift 6.2 is **advisory**: nightly smoke runs against it but no gating. Swift 6.2 adds `@concurrent` annotation for explicit off-main offloading, default actor isolation per module (the `-default-isolation MainActor` flag), and Containerization support on Apple Silicon. If a 6.2-only regression surfaces, we log it as a known issue and let the next release floor decide.

The reasoning for the 6.0 floor specifically:

- Strict concurrency checking is the default in Swift 6 language mode, which gives us data-race safety at compile time for Mochi-generated actors (see [[09-agent-streams]]).
- Typed throws (SE-0413) lets `func foo() throws(E)` carry a precise error type, which we use to lower Mochi's `!E` error union (see [[06-type-lowering]] §5).
- Non-copyable types (SE-0390) and `~Copyable` generics (SE-0427) let us model linear resources without ARC overhead.
- `Embedded Swift` first stabilized in 5.9 but matured in 6.0 (see §8).
- Foundation on Linux switched to swift-corelibs-foundation 6.0 with substantial parity gains (see §5).

Swift 5.10 has typed-throws as an experimental flag only, lacks the full Swift 6 concurrency model, and pre-dates the Swift 6 standard library reshuffling. We deliberately do not lower the floor.

## 2. Apple platform deployment targets

Mochi-emitted `Package.swift` declares the following minimum OS versions, picked to align with the Swift 6.0 toolchain and Xcode 16:

| Platform   | Minimum version       | Ships with          | Notes                                  |
|------------|-----------------------|---------------------|----------------------------------------|
| iOS        | 18.0                  | Sept 2024           | iPhone XS+ devices                     |
| iPadOS     | 18.0                  | Sept 2024           | iPad Pro 2018+ and most iPad lineups   |
| macOS      | 15.0 (Sequoia)        | Sept 2024           | 2018+ Macs                             |
| watchOS    | 11.0                  | Sept 2024           | Apple Watch SE 2 / Series 6+           |
| tvOS       | 18.0                  | Sept 2024           | Apple TV HD and 4K                     |
| visionOS   | 2.0                   | Sept 2024           | Apple Vision Pro                       |
| Mac Catalyst | 18.0                | Sept 2024           | iPad-on-Mac surface                    |

These are the floors. Mochi codegen emits `platforms:` in `Package.swift` with these exact versions, e.g. `platforms: [.iOS(.v18), .macOS(.v15), .watchOS(.v11), .tvOS(.v18), .visionOS(.v2)]`. Users can raise the floor in their `mochi.toml` if they need an API that only exists in a later OS; the build driver propagates that to the generated `Package.swift`.

iOS 17 / macOS 14 / watchOS 10 / tvOS 17 / visionOS 1 were the floors of last year's Xcode 15 cycle. We pick the iOS 18 family floor because:

- Swift 6 strict concurrency runtime behaviour (cooperative-thread executors, `MainActor` hops) is best tuned against the libdispatch shipped in iOS 18 / macOS 15. Older OS versions ship the older Swift 5.x runtime as a system component; the Swift 6 standalone runtime backwards-compatibility shim works but adds ~600 KB to each binary on iOS 17 and below.
- `Observation` framework (`@Observable`) shipped in iOS 17 but was buggy until iOS 17.4; iOS 18 is the first cycle where it is stable.
- `String.UTF8View` and `Span<T>` borrow semantics are runtime-checked on older OSes via the compatibility shim. iOS 18 / macOS 15 have native implementations.

For users who absolutely must target iOS 17 or macOS 14, we document a `--legacy-apple-floor` build mode that drops to iOS 17 / macOS 14 / watchOS 10 / tvOS 17 and forces inclusion of the back-deployment runtime. Apple OS versions below those are out of scope for MEP-49 v1.

## 3. Apple platform CPU architectures

| Arch    | Apple platforms                                                  | Notes                                                                   |
|---------|------------------------------------------------------------------|-------------------------------------------------------------------------|
| arm64   | iOS, iPadOS, macOS, watchOS, tvOS, visionOS, Mac Catalyst        | Tier 1. All current Apple Silicon: A12+ on iOS, M1+ on Mac, S6+ on Watch|
| arm64e  | iOS, iPadOS, macOS                                               | Opt-in. Pointer authentication; iPhone XS+, M1+ Mac. Strict ABI.        |
| x86_64  | macOS, Mac Catalyst, simulators                                  | Legacy Intel Mac, Rosetta-compatible binaries. Phasing out.             |
| armv7   | -                                                                | Unsupported. Dropped after iOS 10 (Sept 2016).                          |
| i386    | -                                                                | Unsupported. Dropped after watchOS 6 / macOS 10.14.                     |

arm64 is universal across the modern Apple lineup. Mochi codegen produces a single Swift module and lets SwiftPM produce per-arch binaries through the Xcode `ARCHS` setting. The default fat binary is `arm64;x86_64` on macOS (a "universal2" binary in Python parlance) and `arm64` only on iOS device builds; simulators get both `arm64` (Apple Silicon Mac simulator) and `x86_64` (Intel Mac simulator) until x86_64 simulator support is removed in a future Xcode (Apple has signalled this without naming a date).

arm64e (with pointer authentication codes, PAC) is opt-in and ABI-incompatible with arm64. Mochi does not emit arm64e by default because most third-party Swift packages do not ship arm64e slices. Users targeting an arm64e-only system feature (kernel extensions, certain entitled processes) pass `--apple-arch=arm64e` and accept the responsibility for arm64e-compatible dependencies.

Apple Silicon Macs run iOS apps natively on macOS 11+ (Designed-for-iPad). Mochi-built iOS apps from a Mochi project with `mochi build --target=swift-ios` are automatically usable on Mac via the iPadOS-on-Mac runtime, with Mac Catalyst as the more deeply integrated alternative.

x86_64 macOS support is **Tier 2**: every Mochi gate that runs on macOS arm64 also runs in CI on macOS x86_64 (via GitHub Actions `macos-15-large` runners which still have x86_64 capacity through Rosetta 2 or Intel-host runners while available). When Apple drops x86_64 (likely with macOS 16 in late 2026), we move it to Tier 3.

## 4. Linux distribution matrix

Mochi ships Linux binaries via the official swift.org toolchain. The Swift 6.0 toolchain release notes name Ubuntu 22.04 LTS, Ubuntu 24.04 LTS, Amazon Linux 2, RHEL UBI 9, Debian 12, and Fedora 39 as the certified distributions. Mochi follows that list.

| Triple                              | Distribution coverage                          | libc      | Tier                  |
|-------------------------------------|------------------------------------------------|-----------|-----------------------|
| x86_64-unknown-linux-gnu            | Ubuntu 22.04/24.04, Debian 12, RHEL 9          | glibc     | Tier 1, gating        |
| aarch64-unknown-linux-gnu           | Ubuntu 22.04/24.04 arm64, Debian 12 arm64      | glibc     | Tier 1, gating        |
| x86_64-unknown-linux-musl           | Static SDK, scratch/distroless containers      | musl      | Tier 1, gating        |
| aarch64-unknown-linux-musl          | Static SDK arm64                                | musl      | Tier 2, CI smoke      |
| armv7-unknown-linux-gnueabihf       | Raspberry Pi 3/4 32-bit Raspberry Pi OS         | glibc     | Tier 3, best-effort   |
| x86_64-unknown-linux-android        | Android-on-x86_64 (NDK r27+)                    | bionic    | excluded from v1      |

Ubuntu 22.04 LTS is the minimum because the swift.org Swift 6.0 builds require glibc 2.34+ and libstdc++ 11+, which 22.04 provides and 20.04 does not. RHEL 9 and Debian 12 sit on glibc 2.34 / glibc 2.36 respectively and are fine. Older distributions (Ubuntu 20.04, RHEL 8, Debian 11) are out of scope.

The **Static SDK** (`swift-static-linux-sdk`, formally the Static Linux SDK) is the headline Linux shipping mode: a single statically-linked binary against musl libc, runnable on any Linux kernel 3.2+ regardless of distribution, distroless or scratch container. Apple introduced it as a supported product in Swift 5.10 (March 2024) and expanded coverage in Swift 6.0. Mochi's default Linux release artifact is a static-musl binary; the gnu-libc path is offered for users who need NSS, dlopen, or system libraries that the static SDK cannot link in.

aarch64-static-musl is Tier 2 because at the time of MEP-49 the Static SDK arm64 path has slightly less third-party-package coverage than x86_64 (some C packages have x86_64-only musl bindings). The gate runs but does not block release.

armv7 (Raspberry Pi 32-bit) is a community-maintained Swift port. We do not gate on it but accept patches.

## 5. Linux Foundation matrix and runtime polyfills

`swift-corelibs-foundation` is Apple's open-source reimplementation of Foundation for Linux and Windows. As of Swift 6.0 it covers an estimated 90% of the Apple Foundation surface. The Mochi runtime explicitly polyfills the gaps so Mochi code can run identically on Apple and Linux.

| Foundation API           | Apple    | Linux (corelibs-foundation 6.0) | Mochi polyfill                                |
|--------------------------|----------|---------------------------------|-----------------------------------------------|
| `Calendar`/`NSCalendar`  | full     | partial (Gregorian only)        | `MochiCalendar` wraps ICU `UCalendar` directly |
| `TimeZone`/`NSTimeZone`  | full     | needs tzdata package installed  | Mochi runtime ships a vendored IANA tz snapshot|
| `URLSession`/`NSURLSession` | Network.framework / Secure Transport | libcurl + OpenSSL | parity; Mochi `fetch` documents per-platform TLS differences (see §20) |
| `NSRegularExpression`    | full (ICU) | full (ICU)                    | parity; Swift 6 `Regex<Output>` literal preferred when possible |
| `Process`/`NSTask`       | full     | different exec semantics (no `posix_spawn_chdir` on older glibc) | Mochi `spawn` shells out to `/bin/sh -c` on Linux fallback |
| `NSXMLParser`            | full (Apple libxml2) | full (system libxml2) | parity; requires `libxml2-dev` on Linux build host |
| `FileManager`            | full     | mostly full; some attributes missing (`isExcludedFromBackup`) | Mochi runtime silently ignores Apple-only attrs on Linux |
| `NSFileCoordinator`      | full     | not implemented                 | Mochi uses `flock(2)` fallback on Linux       |
| `NSMetadataQuery`        | full     | not implemented (Spotlight)     | unsupported on Linux; Mochi `query_metadata` returns empty |
| `NSAppleScript`          | macOS-only | n/a                            | not exposed by Mochi                          |

Mochi runtime polyfills are exposed via the `MochiRuntime` module so user code can write `MochiCalendar.current` once and have it lower to `Calendar.current` on Apple platforms (zero-cost typealias) and to the polyfill struct on Linux/Windows.

Concrete cases the codegen handles:

- **Calendar arithmetic**: Mochi's `date.addMonths(3)` lowers to `Calendar.current.date(byAdding: .month, value: 3, to: ...)` on Apple and to `MochiCalendar.gregorian.addMonths(...)` on Linux. The polyfill links against ICU directly via `swift-icu` (a thin wrapper around `libicuuc.so`).
- **Time zone lookups**: On Apple, `TimeZone(identifier: "Asia/Tokyo")` consults the system `/var/db/timezone`. On Linux it consults `/usr/share/zoneinfo` if present; the Mochi static-musl binary ships a vendored copy of tzdata 2025b at `MochiRuntime/Resources/zoneinfo/` and falls back to it.
- **TLS chains**: see §20.

The Mochi build prints a per-platform Foundation coverage report (`mochi build --report=foundation-coverage`) listing which APIs the user code touched and which polyfills were activated.

## 6. Windows toolchain and version matrix

Swift on Windows ships as an MSI installer from swift.org. The installer bundles the Swift compiler, swift-corelibs-foundation, Dispatch, XCTest, the Swift Package Manager, and a curated ICU build. It does not bundle a C/C++ toolchain; Visual Studio 2022 with the "Desktop development with C++" workload is a build-host prerequisite.

| Component                      | Minimum                                       |
|--------------------------------|-----------------------------------------------|
| Windows version                | Windows 10 1809 (LTSC 2019) or Server 2019    |
| Visual Studio                  | 2022 17.0+, "Desktop development with C++"    |
| Windows SDK                    | 10.0.17763 or later                           |
| MSVC runtime (deploy)          | VS 2022 redistributable (`vcruntime140.dll`)  |
| Architecture                   | x86_64 (`x86_64-pc-windows-msvc`)             |
| arm64 Windows                  | experimental in Swift 6.0; not gated by Mochi |

The toolchain uses MSVC's `link.exe` as the system linker (no `ld`/`lld` by default; an `lld-link` opt-in exists). It uses `clang.exe` shipped with Swift, not the MSVC `cl.exe`. ICU is bundled inside the toolchain at `Library/icu-*/usr` and is linked statically by default into Swift Foundation.

Windows 10 1809 (October 2018) is the floor because that is the oldest Windows release Microsoft still actively patches via LTSC channel and because earlier Windows lacks the `\\?\` long-path support that Swift Foundation assumes. Server 2019 has the same kernel and is supported. Windows 11 22H2+ is the recommended development host.

arm64 Windows (Snapdragon X laptops) is experimental in the Swift 6.0 toolchain. The compiler runs but several stdlib components have known issues. Mochi-on-Windows-arm64 is Tier 3: best-effort, no nightly CI.

The Mochi Windows release artifact is a `.exe` plus a small set of `.dll` dependencies (`MochiRuntime.dll`, `swift_Concurrency.dll`, `swiftCore.dll`, etc.). For single-file shipping we offer the `--windows-static-runtime` flag (links the Swift runtime statically), at the cost of a larger binary (~6 MB vs ~2 MB dynamic).

## 7. Windows quirks

Windows differs from POSIX in several ways that the Mochi runtime smooths over.

**File paths**: Windows uses backslash (`\`) as the path separator and is case-insensitive. Swift's `URL` and `FilePath` (from `swift-system`) abstract this: `URL(fileURLWithPath:)` constructs a platform-native path, and `FilePath.appending(component:)` uses the correct separator. Mochi-emitted code never embeds a hardcoded `/`; the codegen always lowers `path/sub` literals to `FilePath("path").appending("sub")`.

**Long paths**: Windows file APIs traditionally cap at MAX_PATH (260 characters). Swift Foundation prefixes paths with `\\?\` to opt into the long-path namespace on Windows 10 1607+. Mochi inherits this for free.

**Case sensitivity**: NTFS is case-insensitive by default (per-directory case-sensitivity opt-in exists since Windows 10 1803). Mochi treats every path as case-sensitive at the protocol level; if a user writes `read_file("Foo.txt")` and the file is named `foo.txt`, the read succeeds on Windows and fails on Linux. This is documented as a portability hazard in [[10-build-system]] §9.

**No `fork()`**: Windows has no `fork(2)`. `Process` on Apple/Linux uses `posix_spawn`; on Windows it uses `CreateProcess`. Swift Foundation's `Process` abstracts this and supports both. Mochi's `spawn()` builtin lowers to `Foundation.Process` and works identically on all three platforms, except that the `Process.environment` inheritance semantics differ slightly (Windows inherits the parent's environment by default; POSIX requires explicit inheritance via `posix_spawn`).

**Line endings**: Mochi text I/O assumes LF. The runtime's `read_text` strips CRLF on Windows; `write_text` writes LF by default and emits CRLF only if the user passes `lineEndings: .crlf`.

**Symbolic links**: Windows symbolic links require either Developer Mode enabled or SeCreateSymbolicLinkPrivilege. Mochi `symlink()` returns an error documenting this on Windows.

**Process abstraction package**: `swift-system` (Apple's POSIX-like wrapper) exposes `Errno`, `FilePath`, `FileDescriptor` on all three platforms with platform-appropriate semantics. Mochi runtime depends on `swift-system` 1.5.0+.

## 8. Embedded Swift mode

Embedded Swift is a subset of Swift introduced in Swift 5.9 and matured in 6.0 / 6.1. It compiles to bare-metal targets without an operating system, no garbage-collected runtime, and no Swift standard library beyond a curated subset.

What Embedded Swift removes:

- No `String` (replaced by `StaticString` and user-provided collection types).
- No `Array<T>` (replaced by `InlineArray<N, T>` and user-managed storage).
- No `Dictionary<K, V>` (out of scope; users implement intrusive maps).
- No existentials, no protocol witness tables, no class metadata records.
- No Objective-C interop, no Foundation, no Dispatch.
- No reflection (`Mirror`), no key paths, no dynamic casts beyond same-type.

What it keeps:

- Generics with full monomorphisation (every generic call is specialised at the call site, no boxing).
- Value types (struct/enum) with full ergonomics.
- Result builders, property wrappers, async/await (with a custom executor the user provides).
- `~Copyable` and `~Escapable` types.

Useful targets:

- **Raspberry Pi Pico** (RP2040, RP2350): Cortex-M0+ / Cortex-M33. Pico SDK + Swift produces a `.uf2` flashable to the board.
- **ESP32-S3**: Tensilica Xtensa LX7. Requires the ESP-IDF + Swift fork; experimental.
- **STM32**: Cortex-M0/M3/M4/M7. Works with `swift build --triple thumbv7em-none-none-eabi`.
- **Apple Secure Enclave** (internal Apple use; not exposed to third parties).

**MEP-49 v1 explicitly excludes Embedded mode.** The Mochi language surface assumes `String`, `[T]`, `[K: V]`, dynamic dispatch, reflection, and a runtime. Lowering Mochi to Embedded Swift would require an entirely separate codegen path with a different type system mapping (e.g., `String` -> `InlineArray<256, UInt8>` with explicit length tracking). A future MEP-49.2 may add a `--embedded` flag to Mochi codegen that emits a restricted subset; that is out of scope for v1.

## 9. SwiftWasm

SwiftWasm is the Swift-to-WebAssembly compiler. Upstream Swift 6.x carries partial WASM support: the `wasm32-unknown-wasi` triple compiles, but the standard library is incomplete (no `URLSession`, no `Process`, no threading) and the Foundation port is minimal.

| Triple                       | Status in Swift 6.0                            |
|------------------------------|------------------------------------------------|
| wasm32-unknown-wasi          | partial; stdlib works, Foundation is a stub    |
| wasm32-unknown-wasi-threads  | experimental                                    |
| wasm32-unknown-emscripten    | community SwiftWasm project, not upstream      |

Mochi v1 excludes WebAssembly. Reasons:

- Mochi's dataset pipeline (see [[08-dataset-pipeline]]) depends on file I/O and worker concurrency, neither of which is fully supported in `wasm32-unknown-wasi` yet.
- Mochi's agent streams (see [[09-agent-streams]]) depend on `URLSession` for outbound HTTP; the WASI HTTP API is a draft and not in the upstream Foundation.
- The WASM binary size penalty (each Mochi hello-world ships ~3 MB of compiled stdlib in WASM) is higher than acceptable for a v1 web target.

A future **MEP-49.3 sub-MEP** would add `--target=swift-wasm` once Swift 7.x lands with first-class WASI threads + HTTP + filesystem. We track the SwiftWasm project's milestones and contribute issues when Mochi-relevant gaps surface.

## 10. Apple platform packaging

Mochi codegen produces a SwiftPM `Package.swift` for the library case and an Xcode-style project (via `xcodegen` or the `mochi build --gen-xcodeproj` flag) for the app case, because SwiftPM alone cannot produce an `.app` bundle for iOS/macOS GUI apps.

| Output            | Platform     | Container format         | Signing required           |
|-------------------|--------------|--------------------------|----------------------------|
| `.app` bundle     | macOS        | directory                | yes (Developer ID or Mac App Store) |
| `.dmg`            | macOS        | disk image               | yes; notarised             |
| `.pkg`            | macOS        | flat installer package   | yes; notarised             |
| `.ipa`            | iOS/iPadOS/tvOS | zip with `Payload/`   | yes (provisioning profile) |
| `.app` bundle     | iOS simulator| directory                | no (simulator)             |
| `.xpi` (xrOS pkg) | visionOS     | bundle                   | yes                        |

The `.app` is a structured directory: `MyApp.app/Contents/MacOS/MyApp` (binary), `MyApp.app/Contents/Info.plist` (metadata), `MyApp.app/Contents/Resources/` (assets), `MyApp.app/Contents/PkgInfo`. The Mochi build assembles this from the SwiftPM build output.

The `.ipa` is the iOS distribution archive: a zip containing `Payload/MyApp.app/...` plus signing metadata. Mochi `mochi build --target=swift-ios --output-ipa` produces this.

Codesign is required for any non-simulator distribution. The signing identity is supplied via `--apple-signing-identity` (a SHA-1 hash of a certificate in the user's keychain or a developer team identifier prefixed with `Developer ID Application:`).

Notarisation is required for any Developer ID-signed app distributed outside the Mac App Store as of macOS 10.14.5 (June 2019). The Mochi build invokes `xcrun notarytool submit` and `xcrun stapler staple` automatically when `--macos-notarize` is passed. Without notarisation, Gatekeeper blocks first launch on macOS 10.15+.

For visionOS, the `.xpi` extension is internal to the build chain; the App Store distribution format is the same `.ipa` family. Mochi treats visionOS like a sibling of iOS for packaging.

## 11. App Store gatekeeping

Apple's App Store has a review process with documented and undocumented rules. Mochi codegen aims to emit only public APIs so that submissions are not blocked on private-SPI usage.

Categorically prohibited:

- `UIWebView` (deprecated since iOS 12, blocked at submission since April 2020). Mochi never emits `UIWebView`; the `web_view` builtin lowers to `WKWebView`.
- `UIDevice.uniqueIdentifier` (removed in iOS 7). Not exposed by Mochi.
- Private Objective-C selectors (the `_internalFoo` family). Mochi's FFI bindings to Apple frameworks are scanned at codegen time against the public framework header inventory; any reference to a non-public selector fails the build.
- Use of fingerprinting APIs (`UIDevice.systemUptime`, accelerometer at high rates, etc., when the app does not declare a privacy purpose).

Required:

- `Info.plist` privacy-purpose strings for every entitled capability (camera, microphone, location, contacts, photos, ATT). Mochi emits these based on the user's `mochi.toml` `[apple.privacy]` section.
- Privacy manifest (`PrivacyInfo.xcprivacy`) listing data collection categories and tracking purposes. Required by Apple since November 2023 for apps using "required reason" APIs. Mochi auto-generates this from a coarse manifest in `mochi.toml`.
- Symbol-level static analysis for "required reason" APIs (file timestamps, system boot time, disk space, etc.). Mochi codegen tags every emitted call with the reason category.

The Mochi build runs `mochi audit --apple-app-store` to pre-flight check these before submission. The audit fails the build if any required-reason API lacks a declared reason.

## 12. TestFlight

TestFlight is Apple's beta distribution channel for iOS, iPadOS, tvOS, visionOS, watchOS, and Mac Catalyst apps. It supports internal testing (up to 100 testers, no review) and external testing (up to 10,000 testers, requires beta app review).

Mochi exposes this as `mochi build --target=swift-ios --testflight`. The pipeline:

1. SwiftPM/Xcode produces an archive (`.xcarchive`) with the release configuration.
2. The archive is exported to an `.ipa` with the App Store distribution method.
3. `xcrun altool --upload-app` (deprecated) or `xcrun notarytool` (notarisation; not needed for TestFlight) and `xcrun iTMSTransporter` upload the `.ipa` to App Store Connect.
4. App Store Connect creates a new build under the configured app record. Internal testers see it within minutes; external testers see it after Apple's beta review (typically 24 hours for first build, near-instant for subsequent builds).

The Mochi build accepts an API key (`AuthKey_*.p8`) and issuer ID for authentication. We document the required App Store Connect roles (App Manager for upload, Developer for build creation).

TestFlight builds expire 90 days after upload. Mochi documents this and warns when uploading: a user shipping a 6-month internal beta needs to refresh the build every 90 days.

## 13. Mac App Store

Mac App Store distribution is similar to iOS App Store but with macOS-specific requirements.

Requirements:

- App Sandbox enabled (`com.apple.security.app-sandbox` entitlement = true). Without sandbox, submission is rejected.
- Hardened Runtime enabled (`--options runtime` in codesign). Required since June 2019.
- Mac App Store provisioning profile embedded in the `.app` bundle.
- Signed with a "3rd Party Mac Developer Application" certificate (not "Developer ID").
- Distributed as a `.pkg` (not `.app` or `.dmg`).

Sandbox restricts file access (only the app's container by default), network access (must declare `com.apple.security.network.client` or `.server`), hardware access (camera/microphone/location require entitlements + Info.plist purpose strings), and process spawning (only via `NSXPCConnection` to declared XPC services).

Mochi emits a default entitlements plist (`MochiApp.entitlements`) that the user customises. The default set:

```xml
<dict>
  <key>com.apple.security.app-sandbox</key>
  <true/>
  <key>com.apple.security.network.client</key>
  <true/>
  <key>com.apple.security.files.user-selected.read-only</key>
  <true/>
</dict>
```

Additional entitlements (camera, microphone, hardened-runtime exceptions for JIT, etc.) are toggled per the user's `mochi.toml` `[apple.entitlements]` section. Mochi's audit catches the common rejections (e.g., declaring `com.apple.security.cs.disable-library-validation` without a JIT use case).

## 14. Direct distribution with Developer ID

For distribution outside any App Store, Apple's Developer ID program signs and notarises an app with a Developer ID Application certificate. The Mochi pipeline:

1. **Build**: `swift build -c release --arch arm64 --arch x86_64` produces a universal binary.
2. **Bundle**: Assemble `MyApp.app/Contents/{MacOS, Resources, Info.plist, ...}`.
3. **Sign**: `codesign --deep --force --options runtime --sign "Developer ID Application: Acme (TEAMID)" MyApp.app`. The `--options runtime` enables Hardened Runtime, required for notarisation since Feb 2020.
4. **Container**: Wrap in a `.dmg` (`hdiutil create`) or `.zip` for upload.
5. **Notarise**: `xcrun notarytool submit MyApp.dmg --keychain-profile mochi-notary --wait`. Typically completes in 5-15 minutes.
6. **Staple**: `xcrun stapler staple MyApp.dmg`. Embeds the notarisation ticket so Gatekeeper can verify offline.
7. **Distribute**: Upload to the user's CDN or website.

Mochi exposes this end-to-end as `mochi build --target=swift-macos --release --notarize --output=dmg`. The pipeline reads the notary credentials from a keychain item or environment variables, never from a checked-in file.

For users who want a `.pkg` instead (so end users get the OS-native install experience with a graphical wizard), `--output=pkg` invokes `productbuild` and signs with a "Developer ID Installer" certificate.

The notarisation log is saved alongside the artifact for debugging. If notarisation fails (e.g., binary embeds a copy of a flagged third-party library, or codesign metadata is missing), Mochi parses the log and surfaces the offending file.

## 15. Linux distribution

Linux deliverables come in three shapes:

**Static binary via the Static Linux SDK**: a `.tar.gz` containing the executable, optional config files, and a `LICENSES/` directory. The binary is statically linked against musl libc and bundles every Swift runtime dependency. Runnable on any Linux 3.2+ regardless of distribution. This is Mochi's preferred Linux shipping mode.

```
mochi build --target=swift-linux --static --arch=x86_64,aarch64
# produces dist/myapp-linux-x86_64.tar.gz, dist/myapp-linux-aarch64.tar.gz
```

**Dynamic binary via `.deb` or `.rpm`**: built with `swift build -c release` on a glibc host, packaged with `fpm` or `nfpm`. Depends on the system Swift runtime (`libswiftCore.so`) if installed, or on bundled `.so` files in `/opt/myapp/lib/`. Mochi recommends bundling rather than depending on the system runtime, because Linux distributions do not ship a Swift runtime as standard.

```
mochi build --target=swift-linux --output=deb --arch=x86_64
# produces dist/myapp_1.0.0_amd64.deb with /opt/myapp/{bin,lib}
```

**Container image via Docker**: a multi-stage build that pulls `swift:6.0-jammy` for the build stage and `gcr.io/distroless/cc-debian12` or `alpine:3.20` for the runtime stage. The static-musl variant uses `FROM scratch`. Mochi auto-generates a `Dockerfile` when `--output=docker` is set.

| Format          | Distribution channel                 | Notes                                       |
|-----------------|--------------------------------------|---------------------------------------------|
| `.tar.gz` static| GitHub releases, direct download     | Recommended; runs everywhere                |
| `.deb`          | apt repository (`apt.fury.io`, PPA) | Debian/Ubuntu users                         |
| `.rpm`          | dnf/yum repository, COPR             | Fedora/RHEL users                           |
| Docker image    | Docker Hub, GHCR, ECR                | `mochilang/myapp:1.0-static-musl`           |
| Snap / Flatpak  | not officially supported in v1       | community can add                           |

Mochi does not sign `.deb` or `.rpm` packages by default; users distributing through an apt/yum repository pass `--linux-sign-gpg=KEYID` to sign with their own GPG key.

## 16. Windows distribution

Windows deliverables:

**Bare `.exe` + sidecar `.dll`s**: simplest, no installer. Users extract a zip and run the `.exe`. Requires Visual C++ 2022 redistributable installed (or bundled). Mochi can bundle it via the `--bundle-vcredist` flag, which extracts the redistributable next to the `.exe`.

**MSI installer via WiX Toolset**: Mochi generates a WiX `.wxs` file describing the install directory layout, registry entries, Start Menu shortcut, and uninstall hook. `candle.exe` + `light.exe` compile it to an `.msi`. Users install via `msiexec /i myapp.msi` or by double-clicking. Mochi exposes this as `--output=msi`.

**MSIX for Microsoft Store**: MSIX is the modern app package format for Windows 10 1809+. Mochi generates an `AppxManifest.xml` and packages with `MakeAppx.exe`. Distribution is via Microsoft Store or Microsoft Store for Business / sideloading. MSIX requires the package to be signed (self-signed for sideload, EV cert for Store). Mochi exposes this as `--output=msix`.

**Microsoft Store**: Apps must pass Microsoft's app certification process. Restrictions include no driver installation, no kernel-mode code, no telemetry without user consent. Mochi audit (`mochi audit --windows-store`) pre-flight checks for prohibited APIs.

Authenticode signing (with an EV or OV code-signing certificate) is required for any distribution that should avoid SmartScreen warnings. Mochi accepts `--windows-signing-cert=mycert.pfx` and signs both the `.exe` and the `.msi`/`.msix` wrapper.

| Format    | Distribution channel       | Signing                          |
|-----------|----------------------------|----------------------------------|
| zip       | direct download            | optional Authenticode on `.exe`  |
| MSI       | direct download / Chocolatey | Authenticode + MSI signing      |
| MSIX      | Microsoft Store / sideload | Authenticode (EV for Store)      |
| Winget    | community package          | wraps the MSI                    |
| Chocolatey| community package          | wraps the zip or MSI             |

## 17. Cross-compilation rules

Swift's cross-compilation story is asymmetric:

| Build host       | Targetable platforms                                                |
|------------------|----------------------------------------------------------------------|
| macOS (Xcode)    | iOS, iPadOS, watchOS, tvOS, visionOS, macOS, Mac Catalyst, Linux (via Static SDK), Windows (via the open-source cross SDK, experimental) |
| Linux            | Linux (any arch via Static SDK), partially WASM                     |
| Windows          | Windows only                                                         |

The hard rules:

- **Apple platforms cross-compile from macOS only.** Xcode and the Apple platform SDKs are macOS-exclusive. There is no supported way to build an iOS binary from Linux or Windows.
- **Linux-to-Linux is fully cross-arch via the Static Linux SDK.** From a Linux x86_64 host, `swift build --swift-sdk x86_64-swift-linux-musl` produces an x86_64 binary; `--swift-sdk aarch64-swift-linux-musl` produces an arm64 binary. Both work on the same host with no qemu needed.
- **macOS-to-Linux works via the Static Linux SDK on macOS host.** `swift sdk install` accepts a Linux SDK on a macOS host since Swift 5.10. This is the recommended path for Mac developers who want to ship Linux binaries.
- **Linux-to-macOS is not supported.** You need Xcode.
- **Windows-to-Windows only.** No cross-compilation from Windows to other platforms in v1.
- **Windows-to-Linux** via WSL2 effectively makes the host a Linux host. Same rules apply.

Practical CI matrix:

| CI host          | Produces                                                       |
|------------------|----------------------------------------------------------------|
| macos-15 (ARM)   | macOS arm64, macOS x86_64, iOS arm64, watchOS, tvOS, visionOS  |
| ubuntu-24.04     | Linux x86_64 (static-musl + gnu)                                |
| ubuntu-24.04-arm | Linux aarch64 (static-musl + gnu)                               |
| windows-2025     | Windows x86_64                                                  |

We deliberately use the `macos-15` runner for all Apple platforms (Xcode 16.3+ installed) and run the Apple simulator tests on the same runner. The Linux + Windows runners are stateless.

## 18. Toolchain pinning

Mochi pins to a specific Swift toolchain version per project via a `.swift-version` file at the project root (the same convention `swiftenv` and Swift.org's toolchain selector use).

```
6.0.3
```

Behaviour by platform:

- **Apple platforms**: the `.swift-version` is consulted by the Mochi build driver, which selects an Xcode version with that toolchain via `xcode-select -p` plus `DEVELOPER_DIR` overrides. If the requested toolchain is not installed, Mochi prints a `xcodes install 16.3` hint.
- **Linux**: Mochi installs the swift.org tarball for the requested version into `~/.local/share/mochi/toolchains/` (or `MOCHI_TOOLCHAIN_DIR`) and prepends to `PATH`. SHA-256 verified against swift.org's released checksums.
- **Windows**: Mochi installs the MSI from swift.org silently (`msiexec /quiet /i swift-6.0.3.exe`) into `C:\Library\Developer\Toolchains\swift-6.0.3.xctoolchain`. Future installs do not require admin if `INSTALLDIR_ALLUSERS=0`.

The `.swift-version` is the single source of truth. CI honours it; pre-commit hooks check that the developer's installed toolchain matches.

The Mochi distribution itself ships with a vendored toolchain for the platforms where it makes sense (macOS, Linux, Windows). Users who want a self-contained Mochi binary (no separate Swift install) get one by default.

## 19. Locale and i18n

Swift Foundation uses ICU (International Components for Unicode) for collation, calendar arithmetic, locale formatting, date parsing, and regex Unicode handling. ICU is bundled with the Swift toolchain on Linux and Windows; Apple platforms use the system ICU shipped in `/usr/lib/libicucore.dylib`.

| Platform   | ICU source                                  | Version (Swift 6.0)        |
|------------|---------------------------------------------|-----------------------------|
| Apple      | System `/usr/lib/libicucore.dylib`          | Whatever the OS ships (typically ICU 73 on macOS 15) |
| Linux      | Bundled with Swift toolchain                | ICU 73 (vendored)           |
| Windows    | Bundled with Swift toolchain (`icu.dll`)    | ICU 73 (vendored)           |

Mochi assumes ICU is available and documents it as a runtime dependency. The static-musl binary statically links ICU (`libicuuc.a`, `libicui18n.a`, `libicudata.a`), adding ~30 MB to the binary; we mitigate by using `--icu-data-filter` to strip locales the user does not need.

Locale-sensitive operations Mochi exposes:

- `string.localizedCompare(other)` -> ICU collation
- `date.formatted(.dateTime.locale(.init(identifier: "ja_JP")))` -> ICU date formatting
- `number.formatted(.currency(code: "JPY"))` -> ICU number formatting
- `regex.matches(string, options: .caseInsensitive)` -> ICU regex casefolding (where supported)

Mochi defaults to the user's current locale (Foundation's `Locale.current`) for all locale-sensitive operations unless the user overrides. We document that ICU data is required at runtime; static-musl includes it, dynamic-libc may not (if the host lacks `icuuc`).

## 20. TLS and transport security

TLS is the highest-variance area of the Foundation matrix. Each platform uses a different TLS stack with different trust-store semantics and different supported ciphersuites.

| Platform   | TLS stack                                  | Trust store                                   |
|------------|--------------------------------------------|-----------------------------------------------|
| iOS/macOS  | Network.framework + Secure Transport       | System keychain                               |
| Linux gnu  | libcurl + OpenSSL                          | `/etc/ssl/certs/ca-certificates.crt`          |
| Linux musl | libcurl + BoringSSL (static)               | Bundled `cacert.pem` (Mozilla bundle)         |
| Windows    | WinHTTP                                    | Windows Certificate Store (system)            |

Practical consequences:

- **Certificate pinning** differs per platform. On Apple, `URLSessionDelegate.urlSession(_:didReceive:completionHandler:)` lets the app override trust. On Linux + Windows, the same delegate API works (corelibs-foundation routes through libcurl), but Apple-specific `SecTrust` APIs are not available; Mochi wraps trust handling in a portable `MochiTrust` interface.
- **TLS 1.3** is supported on all platforms in Swift 6.0. TLS 1.2 is the fallback. TLS 1.0/1.1 are disabled by default on Apple (since iOS 11) and must be re-enabled via plist exception; Mochi does not expose that knob.
- **Custom CA certificates** on Linux/Windows are added by appending to the trust bundle (Linux: `/usr/local/share/ca-certificates/`, run `update-ca-certificates`; Windows: `certutil -addstore Root`). Mochi documents this for enterprise users on internal PKI.
- **Static-musl bundles** ship the Mozilla CA bundle as a vendored `cacert.pem`. The bundle is updated quarterly when we cut a Mochi release. Users on long-running binaries should rebuild on schedule.

Mochi's `fetch` builtin lowers to `URLSession.shared.data(from:)` everywhere. Per-platform differences in trust handling are smoothed by `MochiRuntime.TrustOverride` (a delegate that can pin specific certificates or accept self-signed certs for development).

## 21. Filesystem case-sensitivity

Filesystems differ in case sensitivity:

| Filesystem      | Default case-sensitivity      | Notes                                              |
|-----------------|-------------------------------|----------------------------------------------------|
| APFS (Apple)    | case-insensitive (case-preserving) | macOS default; can be formatted case-sensitive |
| HFS+ (legacy Apple) | case-insensitive (case-preserving) | Old Macs; HFSX variant is case-sensitive       |
| ext4 (Linux)    | case-sensitive                | Default everywhere on Linux                        |
| btrfs / xfs / zfs | case-sensitive              | Standard POSIX                                     |
| NTFS (Windows)  | case-insensitive (case-preserving) | Per-directory case-sensitive opt-in since Win 10 1803 |
| FAT32 / exFAT   | case-insensitive              | USB drives, SD cards                               |
| HFS+ case-sensitive (HFSX) | case-sensitive       | Optional on Mac at format time                     |

Mochi runtime treats every path as **case-sensitive at the protocol level**: comparing two `FilePath` values uses byte equality, and lookup uses whatever the OS returns. This means:

- A Mochi project developed on macOS (case-insensitive APFS) where the developer wrote `import "Foo/bar.mochi"` referring to a file named `foo/Bar.mochi` will build successfully on Mac and fail on Linux CI.
- The Mochi build runs a `mochi audit --paths` check that walks the source tree and flags any case mismatch between `import` statements and on-disk filenames.

For runtime file I/O, the OS resolves the path however it does. Users who care about portability are advised to use lowercase-with-hyphens for filenames; the Mochi style guide recommends this in [[10-build-system]] §9.

The Mochi runtime never normalises paths to lowercase, never canonicalises case beyond what `realpath(3)` returns, and surfaces filesystem errors verbatim.

## 22. Binary size targets

Mochi's release-mode hello-world target sizes (after `swift build -c release` with link-time optimisation and dead-code stripping):

| Target                                   | Size (KB) | Cold start | Notes                          |
|------------------------------------------|-----------|------------|--------------------------------|
| macOS arm64, dynamic Swift runtime       | ~800      | ~25 ms     | Default                        |
| macOS arm64, static Swift runtime        | ~3,500    | ~25 ms     | `--static-swift-stdlib`        |
| macOS universal (arm64+x86_64), dynamic  | ~1,500    | ~25 ms     | Fat binary                     |
| iOS arm64, dynamic                       | ~750      | ~30 ms     | Stripped, in `.ipa`            |
| Linux x86_64, glibc-dynamic              | ~1,200    | ~25 ms     | Depends on system libswift     |
| Linux x86_64, static-musl                | ~10,000   | ~30 ms     | Self-contained scratch image   |
| Linux aarch64, static-musl               | ~12,000   | ~35 ms     | Self-contained                 |
| Windows x86_64, dynamic                  | ~2,500    | ~40 ms     | Plus ~30 MB of sidecar DLLs    |
| Windows x86_64, static                   | ~6,000    | ~40 ms     | `--windows-static-runtime`     |

Gate thresholds (the CI fails if hello-world exceeds these):

- macOS arm64 dynamic: ≤ 800 KB.
- Linux arm64 static-musl: ≤ 12 MB.
- Windows x86_64 dynamic: ≤ 4 MB (excluding sidecar DLLs).

These are equivalent in spirit to the .NET NativeAOT size targets and to the GraalVM native-image targets in the JVM MEP. Swift on Apple platforms is the smallest because the OS provides `libswiftCore.dylib` as a system component; on Linux/Windows the runtime must be bundled.

We publish weekly size-tracking dashboards under [[11-testing-gates]] and treat a sustained 5% size regression as a release blocker.

---

## Sources

1. Swift 6.0 release announcement. https://www.swift.org/blog/announcing-swift-6/
2. Swift 6.1 release announcement. https://www.swift.org/blog/swift-6.1-released/
3. Swift 6.2 release announcement. https://www.swift.org/blog/swift-6.2-released/
4. Apple developer, iOS 18 release notes. https://developer.apple.com/documentation/ios-ipados-release-notes/ios-ipados-18-release-notes
5. Apple developer, macOS Sequoia 15 release notes. https://developer.apple.com/documentation/macos-release-notes/macos-15-release-notes
6. Apple developer, visionOS 2 release notes. https://developer.apple.com/documentation/visionos-release-notes/visionos-2-release-notes
7. Swift Static Linux SDK announcement. https://www.swift.org/documentation/articles/static-linux-getting-started.html
8. swift-corelibs-foundation repository and release notes. https://github.com/swiftlang/swift-corelibs-foundation
9. Swift on Windows installation guide. https://www.swift.org/install/windows/
10. Embedded Swift vision document. https://github.com/swiftlang/swift/blob/main/docs/EmbeddedSwift/EmbeddedSwiftStatus.md
11. SwiftWasm project. https://swiftwasm.org/
12. SE-0413, typed throws. https://github.com/swiftlang/swift-evolution/blob/main/proposals/0413-typed-throws.md
13. SE-0427, non-copyable generics. https://github.com/swiftlang/swift-evolution/blob/main/proposals/0427-noncopyable-generics.md
14. Apple developer, notarisation requirements. https://developer.apple.com/documentation/security/notarizing-macos-software-before-distribution
15. Apple developer, App Sandbox. https://developer.apple.com/documentation/security/app_sandbox
16. Apple developer, privacy manifest files. https://developer.apple.com/documentation/bundleresources/privacy_manifest_files
17. Apple developer, TestFlight overview. https://developer.apple.com/testflight/
18. WiX Toolset documentation. https://wixtoolset.org/docs/
19. MSIX overview. https://learn.microsoft.com/en-us/windows/msix/overview
20. swift-system package. https://github.com/apple/swift-system
21. ICU project. https://icu.unicode.org/
22. Mozilla CA Certificate Store. https://wiki.mozilla.org/CA/Included_Certificates
