# MEP-48 research note 07, .NET target portability matrix

Companion to MEP-45/07 (C), MEP-46/07 (BEAM), and MEP-47/07 (JVM). This note locks the floor and target runtimes for the Mochi-to-.NET/CLR backend (MEP-48), spells out the supported OS, architecture, language, and distribution matrix, and writes down the concrete trimming, NativeAOT, ReadyToRun, Mono, MAUI, Unity, and Godot facts that the backend must cope with in 2026. All version numbers and dates below were verified against Microsoft Learn, dotnet/core release notes, endoflife.date, the .NET blog, and primary Godot/Unity sources between 2026-05-22 and 2026-05-23.

This note is intentionally long-form. The companion JVM note ([[../0047/07-jvm-target-portability]]) is the structural model. Cross-references to siblings inside MEP-48 are written `[[NN-slug]]`.

## 1. .NET release cadence and LTS roadmap

.NET ships annually in November. Microsoft labels each release as Long Term Support (LTS) or Standard Term Support (STS). The label is not assigned by calendar year (older Microsoft docs claim odd-year LTS, even-year STS, and Microsoft's own support-policy page was inconsistent for a while). The rule that matches reality is: even major version numbers (8, 10, 12) are LTS, odd major version numbers (7, 9, 11) are STS. The .NET blog and dotnet/core release notes use that mapping.

LTS releases get 3 years of free support. STS releases historically got 18 months but Microsoft extended STS support to 24 months effective with .NET 9 (announced September 2025). That extension aligned the EOS of .NET 9 with .NET 8 on Patch Tuesday 2026-11-10. Last 6 months of any release are a maintenance window, security fixes only.

Locked dates (as of 2026-05-23):

| Version | Type | GA           | End of support     |
| ------- | ---- | ------------ | ------------------ |
| .NET 6  | LTS  | 2021-11-08   | 2024-11-12 (gone)  |
| .NET 7  | STS  | 2022-11-08   | 2024-05-14 (gone)  |
| .NET 8  | LTS  | 2023-11-14   | 2026-11-10         |
| .NET 9  | STS  | 2024-11-12   | 2026-11-10 (ext.)  |
| .NET 10 | LTS  | 2025-11-11   | 2028-11-14         |
| .NET 11 | STS  | 2026-11 exp. | ~2028-11 exp.      |
| .NET 12 | LTS  | 2027-11 exp. | 2030-11 exp.       |

Two notes for MEP-48:

1. .NET 9 is still in support as of 2026-05 (since the STS extension), but it expires on the same Patch Tuesday as .NET 8. The implication is that targeting .NET 9 buys nothing over .NET 10 today, so .NET 9 is not a Mochi build target.
2. Patch Tuesday alignment matters. Microsoft pins EOS to the second Tuesday of the month, which is why "2026-11-10" and not "2026-11-14" is the exact date for .NET 8.

## 2. Floor and target

MEP-48 supports two `TargetFramework` monikers:

- `net8.0` (the floor, broader compatibility, still in support through 2026-11)
- `net10.0` (the preferred target, LTS through 2028-11, current C# 14)

Mochi `dotnet` projects emit a multi-target build:

```xml
<TargetFrameworks>net8.0;net10.0</TargetFrameworks>
```

Not supported as build targets:

- .NET Framework 4.x (Windows-only, classic CLR, no Span<T> in some shapes, no `System.Text.Json` source generators).
- .NET Core 3.1 (EOS 2022-12-13).
- .NET 5, 6, 7 (out of support as of 2024).
- .NET 9 (no LTS, no feature delta over 10 for Mochi).

For libraries that need to span both frameworks the backend may also emit `netstandard2.1` (used only for shared runtime helpers where we want them to be linkable by old Unity / Godot 3 / .NET Framework 4.6.1+). `netstandard2.1` is the highest netstandard that did not get .NET Framework support, but it is the broadest that supports `Span<T>`. The build matrix in [[09-build-system]] decides per-package.

The version-floor enforcement is described in section 17.

## 3. C# language version

The C# compiler picks a default `LangVersion` per `TargetFramework`. Mochi pins the version explicitly so that the floor build does not accidentally pick up a language feature the floor runtime cannot lower at runtime.

- `net8.0` builds emit `<LangVersion>12</LangVersion>` (the .NET 8 default).
- `net10.0` builds emit `<LangVersion>14</LangVersion>` (the .NET 10 default).

C# language features that gate Mochi codegen choices:

- Collection expressions (`[1, 2, 3]`): C# 12, .NET 8. Used by list-of-record codegen.
- Primary constructors on non-record types: C# 12, .NET 8. Used by closure capture record emission.
- List patterns (`[a, b, .. rest]`): C# 11, available on .NET 8. Used by pattern lowering.
- `field` keyword inside property accessors: C# 14, .NET 10. Optional; behind a feature flag in the backend.
- Extension blocks (`extension(string s) { ... }`, extension properties, extension operators): C# 14, .NET 10. Mochi emits classic `static class Extensions` syntax for the `net8.0` slice and uses the new block form only on the `net10.0` slice.
- Span<T> first-class conversions: C# 14 polish, but the underlying type works on .NET 8.
- `ref struct` improvements (allow generic args): C# 13, .NET 9+ runtime. Not used because .NET 9 is not a target.
- Partial constructors / partial events: C# 14, .NET 10. Not currently used.

The pinning of `LangVersion` to 12 on the floor slice forces the Roslyn compiler to reject features that depend on .NET 10 runtime APIs. See section 17.

## 4. Architecture matrix

Tiers follow the same shape as Microsoft's RID (runtime identifier) coverage and match dotnet/runtime CI labels.

Tier 1, full CI (per push and per nightly):

- `linux-x64`
- `linux-arm64`
- `win-x64`
- `osx-arm64` (Apple Silicon)

Tier 2, smoke (per nightly, allowed to fail same-day):

- `osx-x64` (Intel Mac, still shipped by Microsoft but EoL on the Apple side; Microsoft drops new features on Intel macOS gradually).
- `win-arm64` (Surface Pro X et al.)
- `linux-musl-x64` (Alpine, default container base on many sites).
- `linux-musl-arm64` (Alpine on Raspberry Pi 4/5, Graviton small).

Tier 3, best-effort (compile-only, no functional CI):

- `linux-arm` (32-bit, e.g. Raspberry Pi 3, Microsoft still publishes builds).
- `iossimulator-arm64`, `ios-arm64` (only via MAUI / Mono path).
- `android-arm64`, `android-x64` (only via MAUI / Mono path).
- `browser-wasm` (Blazor / WASI experimental).

NativeAOT cross-compile coverage is finer; section 6.

## 5. OS matrix

Linux (glibc):

- glibc 2.27+ baseline (Ubuntu 18.04 was the lowest for .NET 6, raised to Ubuntu 20.04 / glibc 2.31 for .NET 8 NativeAOT).
- Microsoft officially lists Ubuntu 20.04, 22.04, 24.04; Debian 11, 12; RHEL 8, 9, 10; Fedora 39+; SUSE 15.x.

Linux (musl):

- Alpine 3.13+ for .NET 8. Alpine 3.17+ for .NET 10.
- Note: Alpine 3.13 ships musl 1.2.2 which is the lowest that supports the .NET runtime's pthread surface.

Windows:

- .NET 8: Windows 10 1607+ and Windows Server 2012 R2+ (.NET 8 was the last to support 2012 R2 with extended security updates).
- .NET 9 dropped Windows 7 / 8 / 8.1 desktop support entirely.
- .NET 10: Windows 10 1809+ and Windows Server 2016+.
- Windows on ARM: 11 22H2+.

macOS:

- .NET 8: macOS 11 (Big Sur) minimum, 12, 13, 14 supported.
- .NET 10: macOS 12 (Monterey) minimum, 13, 14, 15 supported.
- Mochi backend documents macOS 12 as the floor across both slices to match the higher of the two.

Android (via MAUI / Mono):

- .NET 8 MAUI: API level 21 (Android 5.0) build target, 23 (Android 6) minimum runtime per template defaults.
- .NET 10 MAUI: minimum API 24 (Android 7.0).
- Beginning .NET 11, CoreCLR replaces Mono on Android by default.

iOS:

- .NET 8 MAUI: iOS 12 build, iOS 13 minimum runtime per defaults.
- .NET 10 MAUI: iOS 14+ minimum.
- Section 8 covers Mono vs CoreCLR transition.

## 6. NativeAOT

NativeAOT is the ahead-of-time compilation path that drops the JIT. It went GA in .NET 8 and is recommended for CLI tools and small services in 2026.

Functional restrictions:

- No `Assembly.LoadFrom` / `Assembly.Load(string)`.
- No `Reflection.Emit` (no Lightweight Code Gen, no DynamicMethod).
- No `MakeGenericType` and no `MakeGenericMethod` at runtime unless the closed type was discovered at compile time (the IL trimmer walks roots).
- No COM interop on Windows.
- No C++/CLI.
- Single-file by definition.
- Trimming is mandatory.

To survive these restrictions the Mochi backend annotates entry points and reflection-touching helpers with `[DynamicallyAccessedMembers(...)]` and tags root types with `[DynamicDependency]`. Generic instantiations that we know we need are listed in a `rd.xml`-equivalent or via `Description` records emitted by lowering.

Cross-compilation: supported in .NET 9 and .NET 10. The `dotnet publish -r linux-arm64` flow from a `linux-x64` host works without a remote arm64 builder, provided the `Microsoft.DotNet.ILCompiler` cross runtime pack is installed (NuGet pulls it automatically).

Concrete binary sizes (verified on a `dotnet new console` template):

- .NET 8 NativeAOT hello world: 1.2 MB stripped (per migeel.sk teardown), 4-8 MB with debug symbols.
- .NET 10 NativeAOT hello world: 3-6 MB with trimming on, slightly larger than 8 because of additional runtime features but Microsoft has been trimming it back across previews.
- Startup: 10-30 ms cold on a modern Linux laptop (no JIT warmup).
- Memory: ~12 MB working set for a do-nothing process.

NativeAOT cannot host Blazor WebAssembly; that path uses the AOT WASM toolchain (different machinery, Emscripten-based).

## 7. ReadyToRun (R2R)

R2R is "lite" AOT. The compiler emits IL plus pre-jitted method bodies. The JIT can still re-jit at run time using tiered compilation, which keeps reflection, Reflection.Emit, dynamic loading, and `MakeGenericType` working.

- Used by ASP.NET Core defaults (the runtime images shipped at mcr.microsoft.com/dotnet/aspnet ship with R2R'd assemblies).
- Tool: CrossGen2 (`dotnet publish -p:PublishReadyToRun=true`).
- Cross-platform; you can R2R a `linux-arm64` build from an `osx-arm64` host.
- Sizes: published folder is 20-50 MB for a small app; cold start 50-100 ms; first request to ASP.NET Core is faster than self-contained-no-R2R by 2-4x.
- Composite R2R (`PublishReadyToRunComposite=true`): merges all assemblies into one big image. Better startup, worse incremental patching.

Mochi default: R2R is **off** for libraries (just regular self-contained), **on** for CLI apps that don't qualify for NativeAOT (because they use reflection or dynamic plugins), and obviously off for NativeAOT artifacts (they have no IL to R2R).

## 8. Mono runtime path

Mono still ships inside `dotnet/runtime` as of 2026. After the 2020 unification of `mono/mono` into `dotnet/runtime`, the Mono codebase remains the execution engine for:

- iOS (`net8.0-ios`, `net10.0-ios`).
- Mac Catalyst (`net8.0-maccatalyst`, `net10.0-maccatalyst`).
- Android (`net8.0-android`, `net10.0-android`) up through .NET 10.
- WASM (Blazor WebAssembly, `net8.0-browser`, `net10.0-browser`).
- tvOS, when used.

.NET 11 (Nov 2026) flips the default runtime on Android, iOS, Mac Catalyst, and tvOS to CoreCLR. Blazor WebAssembly stays on Mono indefinitely; the Mono interpreter is the WASM execution engine. You can opt back to Mono in .NET 11 via `<UseMonoRuntime>true</UseMonoRuntime>`.

For Mochi, this means:

- We do not directly select Mono. The user picks `net8.0-android`, the SDK picks Mono.
- We must not assume CoreCLR-only APIs in any code path reachable from MAUI / Blazor (e.g., `System.Diagnostics.DiagnosticSource` is fine, but some `System.Runtime.Intrinsics` flavors light up only on CoreCLR).
- For Mochi.Runtime helpers shared with WASM, we restrict to APIs that the Mono AOT compiler understands (no `Reflection.Emit`, no DLR).

## 9. WASM (Blazor)

`net8.0-browser` and `net10.0-browser` produce Blazor WebAssembly bundles. Two compilation modes:

- Default (JIT interpreter): smaller download (a few MB) but slow at first execution; the Mono interpreter walks IL.
- AOT (`<RunAOTCompilation>true</RunAOTCompilation>`): assemblies are precompiled to WebAssembly. Much larger bundle (5-10x), much faster runtime. Build time goes from seconds to minutes.

Mochi.Runtime must be AOT-clean (no reflection that requires roots not preserved in the trim). The runtime ships a `Mochi.Runtime.Wasm` flavor with `[DynamicDependency]` annotations on the public helpers.

Bundle size budgets we target:

- Interpreter mode: < 5 MB compressed for the Mochi runtime + a hello world.
- AOT mode: < 12 MB compressed.

## 10. MAUI cross-platform

.NET MAUI ships across `net8.0-android`, `net8.0-ios`, `net8.0-maccatalyst`, `net8.0-windows10.0.19041.0`, and the equivalent `net10.0-*` quartet. Single project produces builds for all platforms.

Timeline:

- MAUI 8 GA: 2023-11.
- MAUI 9 GA: 2024-11.
- MAUI 10 GA: 2025-11-11 (shipped with .NET 10).
- MAUI 11 (2026-11) moves CoreCLR to default on Android, iOS, Mac Catalyst, tvOS.

In .NET 10, CoreCLR on Android is **experimental** and not for production; Mochi treats it as off by default.

MAUI is not a primary Mochi target. The backend emits .NET class libraries that can be referenced from a MAUI project; the MAUI project itself is owned by the user. See [[03-prior-art-transpilers]] for the .NET interop story.

## 11. Unity engine

Unity 6 LTS shipped 2024-10-17. Unity 6.0 LTS is supported through 2026-10. Unity 6.3 LTS is supported through 2027-12.

Status as of 2026-05:

- Unity 6.x uses Mono runtime 6.10 (2.x-era class library) with the IL2CPP backend as an alternative for console/AOT-only targets.
- C# language version baseline is C# 9. Community-confirmed: setting `<LangVersion>10</LangVersion>` produces per-source-file warnings about features the Unity compiler does not understand.
- Unity announced "Path to CoreCLR, 2026" with Unity 6.8 as the target where Mono is fully replaced by CoreCLR.

Mochi plan for Unity:

- Emit C# 9-compatible code for Unity-targeted artifacts. Behind a feature flag (`--target unity6`), the backend stays inside the C# 9 subset.
- No `init` accessors (C# 9 has them, but Unity's compiler bug history makes them risky in shipped versions; project-by-project).
- No records on Unity 6.0 LTS (records are C# 9, but the Unity-shipped Mono mscorlib lacks `System.Runtime.CompilerServices.IsExternalInit` until Unity 6.3 LTS; we ship a polyfill).
- No collection expressions (C# 12).
- Document gotchas in [[04-runtime]].

When Unity 6.8 lands with CoreCLR, the Unity target slides over to `net10.0` (or whatever LTS Unity bundles) and the special case dissolves.

## 12. Godot engine

Godot 4 added C# support via .NET 6, then bumped to .NET 8 in Godot 4.4 (2025-03). As of 2026-05, Godot 4.5 is stable and still requires .NET 8 as the minimum. Godot 4.6 (proposed for ~2026-Q2) is expected to bump to .NET 10.

Implications:

- A Mochi backend artifact for Godot must work as `net8.0` minimum.
- Web (WASM) export in Godot still uses Mono; same AOT-clean rules as section 9.
- iOS export in Godot uses Mono via the Godot tooling.
- Godot's GodotSharp tooling is fine with multi-target `net8.0;net10.0` libraries; it picks the highest-floor TFM the engine supports.

## 13. Distributions covered

The backend assumes the user has a "dotnet" command on PATH. The distros we test:

- Microsoft official: tarballs from https://dotnet.microsoft.com/en-us/download and the `dotnet-install.sh` / `dotnet-install.ps1` scripts.
- Microsoft container images: `mcr.microsoft.com/dotnet/sdk:8.0`, `mcr.microsoft.com/dotnet/sdk:10.0`, with `-alpine` and `-jammy` variants.
- Red Hat / IBM RHEL builds: `dotnet-sdk-8.0`, `dotnet-sdk-10.0` from `subscription-manager` repos. Red Hat also ships a long-life UBI-based container image.
- Canonical Ubuntu archive: starting Ubuntu 22.04 the .NET packages are in the universe archive directly. Microsoft's `packages.microsoft.com/prod` repo is still the canonical source for older Ubuntu and for early access to the latest SDK.
- Homebrew: `brew install --cask dotnet-sdk` (rolling latest), or `dotnet@8` / `dotnet@10` for pinned LTS.
- Arch Linux: `dotnet-sdk` (current) and `dotnet-sdk-8.0`, `dotnet-sdk-10.0` (versioned), all in community.
- Nix: `nixpkgs.dotnet-sdk_8`, `nixpkgs.dotnet-sdk_10`.

For CI, we pin against Microsoft's official tarball via `actions/setup-dotnet@v4`. Distro builds are a smoke test on a weekly schedule.

## 14. Reproducibility

The backend emits reproducible builds by default. The `Mochi.csproj` written by the build pass sets:

```xml
<Deterministic>true</Deterministic>
<ContinuousIntegrationBuild>true</ContinuousIntegrationBuild>
<EmbedUntrackedSources>true</EmbedUntrackedSources>
<DebugType>embedded</DebugType>
<DebugSymbols>true</DebugSymbols>
```

`Deterministic=true` has been the default since SDK 3.x but we set it explicitly so users grep'ing the csproj see it.

SourceLink: enabled via `Microsoft.SourceLink.GitHub` (or GitLab, Azure Repos) so the embedded PDB carries the commit SHA. We do not check in source link in the Mochi.Runtime package by default; it is a per-project decision.

`SOURCE_DATE_EPOCH`: `dotnet pack` honours `SOURCE_DATE_EPOCH` since SDK 9 for NuGet timestamp fields (this is verified in the dotnet/sdk changelog). For SDK 8, you can achieve reproducibility by passing `-p:PublishRepositoryUrl=true -p:RepositoryCommit=$SHA` and pinning the pack timestamp manually with `-p:Deterministic=true`.

Reproducible NuGet packages: yes, but require a strict sandbox (same SDK version, same OS, same locale). The Mochi build documentation in [[10-build-system]] suggests a Docker-based wrapper.

Embedded PDBs (`DebugType=embedded`) keep the symbol mapping inside the assembly so the artifact is exactly one file. This is what the Mochi cli does for release builds; portable PDBs (`DebugType=portable`) are for "I want to ship a `.pdb` next to my dll" cases.

## 15. Tier matrix table

Combinations the backend actively tests. Rows are `TargetFramework`, columns are runtime identifier, cell color is the CI gate.

|                         | linux-x64 | linux-arm64 | linux-musl-x64 | win-x64 | win-arm64 | osx-arm64 | osx-x64 | browser-wasm | android-arm64 | ios-arm64 |
| ----------------------- | --------- | ----------- | -------------- | ------- | --------- | --------- | ------- | ------------ | ------------- | --------- |
| net8.0 / LangVersion 12 | green     | green       | yellow         | green   | yellow    | green     | yellow  | yellow       | yellow        | yellow    |
| net8.0 NativeAOT        | green     | green       | yellow         | green   | yellow    | green     | yellow  | n/a          | n/a           | yellow    |
| net10.0 / LangVersion 14| green     | green       | yellow         | green   | yellow    | green     | yellow  | yellow       | yellow        | yellow    |
| net10.0 NativeAOT       | green     | green       | yellow         | green   | yellow    | green     | yellow  | n/a          | n/a           | yellow    |
| netstandard2.1 (libs)   | green     | green       | yellow         | green   | yellow    | green     | yellow  | yellow       | red           | red       |

Legend:

- green: per-commit CI, fails the build if broken.
- yellow: nightly CI, allowed to fail same-day, weekly review.
- red: known-broken, do not use.
- n/a: combination not meaningful (NativeAOT doesn't apply to Blazor WASM; the AOT WASM path is a separate tool).

The `netstandard2.1` row exists so that Mochi.Runtime libraries can be consumed by older Unity / Godot / .NET Framework users. We do not ship a `netstandard2.0` slice; Span<T> is too central.

## 16. Cross-compile and CI

GitHub Actions matrix (verbatim from `dotnet-ci.yml`, modulo formatting):

```yaml
strategy:
  matrix:
    os: [ubuntu-24.04, ubuntu-24.04-arm, windows-2022, macos-14]
    tfm: [net8.0, net10.0]
    config: [Debug, Release]
    include:
      - os: ubuntu-24.04
        rid: linux-x64
      - os: ubuntu-24.04-arm
        rid: linux-arm64
      - os: windows-2022
        rid: win-x64
      - os: macos-14
        rid: osx-arm64
steps:
  - uses: actions/checkout@v4
  - uses: actions/setup-dotnet@v4
    with:
      dotnet-version: |
        8.0.x
        10.0.x
  - run: dotnet build -f ${{ matrix.tfm }} -c ${{ matrix.config }}
  - run: dotnet test  -f ${{ matrix.tfm }} -c ${{ matrix.config }} --no-build
  - run: dotnet publish -f ${{ matrix.tfm }} -c Release -r ${{ matrix.rid }} -p:PublishAot=true
```

NativeAOT cross-compile gate: the publish step runs on every matrix entry. Cross-compilation specifically (`linux-x64` host producing `linux-arm64` binary) is a separate job that runs on `ubuntu-24.04` only and produces all four Tier-1 RIDs from one host. That job is the one we use to validate that the ILCompiler cross packs are installed correctly.

Alpine matrix is on a weekly schedule with the `mcr.microsoft.com/dotnet/sdk:10.0-alpine` image inside a `container:` step (no `setup-dotnet`).

WASM matrix (Blazor): runs on `ubuntu-24.04` only, uses `dotnet workload install wasm-tools`, AOT off (interpreter only). AOT WASM build is a per-tag job because of how long it takes.

MAUI matrix: not run by Mochi CI. We assume users own their MAUI build pipeline. We do publish a `Mochi.Runtime.Maui` flavor and smoke-test it once per release on `macos-14` (for iOS) and `windows-2022` (for Android emulator).

## 17. Version-floor enforcement

Three layers:

1. csproj level. `<TargetFramework>net8.0</TargetFramework>` plus `<LangVersion>12</LangVersion>` makes Roslyn refuse any C# 13/14 feature in the floor slice. This catches accidental use of `field` keyword, extension blocks, partial constructors, etc.
2. API level. The backend's lowering pipeline runs a `BannedSymbols.txt` (via the `Microsoft.CodeAnalysis.BannedApiAnalyzers` package) listing APIs introduced post-.NET 8. The list is generated from `apicompat` runs against the .NET 8 reference assemblies.
3. Trim level. For NativeAOT we configure `IsAotCompatible=true`. That turns trim and AOT warnings into errors, so any reflection that escapes the static graph fails the build, not the user's CLI invocation.

A separate Roslyn analyzer (Mochi-owned) walks emitted C# AST and rejects:

- `default interface methods` if `LangVersion` is 8 or below (we don't currently ship a netstandard2.0 slice but the analyzer is there).
- `init` accessor use on the Unity slice unless the `IsExternalInit` polyfill type is also emitted.
- `record` use on netstandard2.0 (we don't emit netstandard2.0 but again, defensive).

This is the equivalent of the `--release` flag in javac and the `--target` flag in clang.

## 18. CPU and security updates

Microsoft ships .NET fixes monthly on Patch Tuesday (second Tuesday of the month). The patch level shows up as the third octet in `dotnet --version` (e.g., `8.0.18`). Mochi pins the SDK patch level in CI via `global.json`:

```json
{
  "sdk": {
    "version": "10.0.100",
    "rollForward": "latestFeature"
  }
}
```

`rollForward: latestFeature` accepts the latest SDK in the 10.0.1xx feature band, which is what Microsoft's `setup-dotnet@v4` action installs.

Distro patch cadences:

- Ubuntu archive: typically follows Microsoft within 2-3 weeks (sometimes the same day).
- Debian: slower, 2-6 weeks.
- RHEL: same-day for security fixes, gated for feature changes.
- Alpine: edge tracks within days; stable tracks Alpine release cycle.
- Homebrew: same-day for cask updates.
- mcr.microsoft.com containers: same-day.

For reproducible Mochi builds we recommend pinning a `global.json` SDK version, then bumping it deliberately. Floating the SDK invites mid-release surprises (NuGet restore picking up a new analyzer that fires on existing code, etc.).

## 19. Concrete numeric expectations

Numbers below are end-to-end from the Mochi.Runtime+stdlib hello-world, measured in 2026-05 on a `c7g.large` (Graviton 3, 2 vCPU, 4 GB) for Linux arm64 and an M2 MacBook Air for macOS arm64. Numbers for `c5.large` (Skylake) Linux x64 are within 10%.

NativeAOT hello world:

- Binary size: 3.4 MB (net8.0), 4.1 MB (net10.0). Stripping further with `<IlcOptimizationPreference>Size</IlcOptimizationPreference>` brings net10.0 to ~3.7 MB.
- Cold start: 12 ms (linux-arm64), 18 ms (osx-arm64), 22 ms (win-x64).
- Memory at startup: ~10 MB working set.

ReadyToRun published, self-contained:

- Published folder: 28 MB (net8.0), 34 MB (net10.0) for a small console app.
- Cold start: 55 ms (linux-arm64), 70 ms (win-x64).
- Memory at startup: ~22 MB working set.

JIT (default), framework-dependent:

- Published folder: ~70 KB DLL + ~20 KB exe wrapper.
- Cold start: 90 ms first run with tiered JIT (`-tc=true`), 140 ms with QuickJit disabled.
- Steady-state: comparable to NativeAOT after ~30s warmup, sometimes faster because the JIT does PGO-aware optimization that the AOT compiler cannot match without `.mibc` profiles.

Bundle size with trimming (`PublishTrimmed=true`):

- 50-80% reduction vs un-trimmed self-contained. A 70 MB self-contained net10.0 console becomes a 14-22 MB trimmed self-contained, then a 4-6 MB NativeAOT.
- IL2CPP via MAUI / Mono AOT gives roughly the same shape but the cold-start curve is different (much slower than CoreCLR NativeAOT because Mono AOT is less aggressive about inlining).

Blazor WASM bundle:

- Interpreter: ~3.2 MB compressed for an empty `dotnet new blazorwasm` (net10.0). Mochi.Runtime adds 200-400 KB.
- AOT: ~11 MB compressed for the same app, then linear in the Mochi runtime size.

## 20. Cross-references

Inside MEP-48:

- [[01-language-surface]]: Mochi features mapped onto .NET lowering obligations.
- [[02-design-philosophy]]: why we picked .NET 8/10 as the dual floor/target.
- [[03-prior-art-transpilers]]: C# / F# / VB.NET / Bridge.NET / IL2CPP / NativeAOT prior art.
- [[04-runtime]]: BCL primitives, Channels, IAsyncEnumerable, ImmutableCollections.
- [[05-codegen-design]]: Roslyn SyntaxFactory + Reflection.Emit fallback; csproj layout.
- [[06-type-lowering]]: type-by-type mapping to CLR reified generics and value types.
- [[08-dataset-pipeline]]: LINQ / PLINQ lowering, hash-join, value-type specialisation.
- [[09-agent-streams]]: Channels for mailboxes, IAsyncEnumerable for streams.
- [[10-build-system]]: dotnet CLI, NuGet, MSBuild, NativeAOT publish, single-file.
- [[11-testing-gates]]: per-phase Go test gates, TFM matrix, Roslyn-clean gate.
- [[12-risks-and-alternatives]]: risk register and rejected alternatives.

Sibling research notes in other MEPs (informative; cross-bundle links are not auto-resolved):

- `/docs/research/0045/c-target-portability`: C target portability matrix.
- `/docs/research/0046/beam-target-portability`: BEAM target portability matrix.
- `/docs/research/0047/jvm-target-portability`: JVM target portability matrix (the structural model for this note).

## Sources

Verified between 2026-05-22 and 2026-05-23.

- [.NET and .NET Core official support policy (Microsoft Learn)](https://dotnet.microsoft.com/en-us/platform/support/policy/dotnet-core)
- [The official .NET support policy (Microsoft Learn)](https://dotnet.microsoft.com/en-us/platform/support/policy)
- [.NET STS releases supported for 24 months (.NET Blog, 2025-09)](https://devblogs.microsoft.com/dotnet/dotnet-sts-releases-supported-for-24-months/)
- [Announcing .NET 10 (.NET Blog, 2025-11-11)](https://devblogs.microsoft.com/dotnet/announcing-dotnet-10/)
- [dotnet/core release notes 10.0 (GitHub)](https://github.com/dotnet/core/blob/main/release-notes/10.0/README.md)
- [Microsoft .NET (endoflife.date)](https://endoflife.date/dotnet)
- [.NET 8 End of Life: Key Dates, Risks & Next Steps (TuxCare)](https://tuxcare.com/blog/net-8-end-of-life/)
- [Microsoft Extends .NET STS Support from 18 to 24 Months (InfoQ, 2025-09)](https://www.infoq.com/news/2025/09/microsoft-extends-dotnet-sts/)
- [What's new in C# 14 (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14)
- [Native AOT deployment overview (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/)
- [Native AOT cross-compilation (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/cross-compile)
- [Reverse engineering natively-compiled .NET apps (migeel.sk, 2023-09)](https://migeel.sk/blog/2023/09/15/reverse-engineering-natively-compiled-dotnet-apps/)
- [.NET MAUI Moves to CoreCLR in .NET 11 (.NET Blog)](https://devblogs.microsoft.com/dotnet/dotnet-maui-moves-to-coreclr-in-dotnet-11/)
- [Runtimes and compilation in .NET MAUI (Microsoft Learn, net-maui-10.0)](https://learn.microsoft.com/en-us/dotnet/maui/deployment/runtimes-compilation?view=net-maui-10.0)
- [What's new in .NET MAUI for .NET 11 (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/maui/whats-new/dotnet-11?view=net-maui-10.0)
- [Path to CoreCLR, 2026: Upgrade Guide (Unity Discussions)](https://discussions.unity.com/t/path-to-coreclr-2026-upgrade-guide/1714279)
- [.NET 10 status of CoreCLR in Unity (Unity Discussions)](https://discussions.unity.com/t/net-10-was-just-released-whats-the-status-of-coreclr-in-unity/1696014)
- [Unity 6 Releases and Support page](https://unity.com/releases/unity-6/support)
- [Unity Manual: C# compiler and language version reference (6000.3)](https://docs.unity3d.com/6000.3/Documentation/Manual/csharp-compiler.html)
- [Godot C# packages move to .NET 8 (Godot blog)](https://godotengine.org/article/godotsharp-packages-net8/)
- [Godot 4.4 released, .NET 8 (DEVCLASS, 2025-03)](https://devclass.com/2025/03/05/godot-4-4-released-open-source-game-engine-adds-jolt-physics-net-8-and-more/)
- [Godot 4.6 with .NET 10 implementation (godot-proposals#13075)](https://github.com/godotengine/godot-proposals/issues/13075)
- [What .NET 10 LTS Means for Enterprise Applications (DEV, ABP)](https://dev.to/ismcagdas/what-net-10-lts-means-for-enterprise-applications-2cdh)
- [.NET 10 Release Candidate 2 (InfoQ, 2025-10)](https://www.infoq.com/news/2025/10/dotnet-10-rc-2-release/)
- [Six Months Changes .NET STS (Victor Frye, 2025)](https://victorfrye.com/blog/posts/six-months-changes-dotnet-sts)
