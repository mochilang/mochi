# MEP-48 research note 10, Build system

Status: draft, 2026-05-23

This note covers the build, packaging, and distribution story for the .NET / CLR target of the Mochi transpiler. It mirrors the structure of [[../0047/10-build-system]] (the JVM equivalent: Gradle, Maven, jlink, jpackage, GraalVM native-image). The goal is to define how `mochi build`, `mochi run`, and `mochi pack` behave once the C# output from notes 04 through 09 has to leave a developer's machine and reach a server, a container, a desktop, or nuget.org.

The .NET tooling story is in some ways simpler than the JVM (one official SDK, one official package manager) and in other ways messier (R2R vs NativeAOT vs SingleFile, three distinct flavors of "ahead of time"). This note enumerates the choices, picks defaults, and pins flag names and property names that the implementation will encode.

## 1. Target matrix

`mochi build --target` accepts the following .NET shapes:

- `dotnet-dll`. Produces a managed assembly (`Mochi.App.dll`) plus PDB. Intended for libraries and for chaining into other C# projects. Default RID is the host RID, e.g. `linux-x64`.
- `dotnet-exe`. Produces a framework-dependent executable. The user must have `dotnet` runtime installed. Output is `app.dll` + `app` (or `app.exe`) launcher of a few kB. This is the minimal-size CLI option but assumes a runtime is present.
- `dotnet-single-file`. Produces a self-contained single-file bundle (typically 70 to 80 MB) that includes the runtime. Cross-platform, no reflection restrictions, no AOT restrictions. **This is the default for `mochi build`** because it is the option closest in spirit to a Go binary or a Kotlin fat jar: drop it on a machine, run it.
- `dotnet-r2r`. Self-contained ReadyToRun build. Larger than `single-file` (R2R adds native code alongside IL) but offers significantly faster startup. Used when reflection or dynamic loading is needed and startup matters.
- `dotnet-aot`. NativeAOT publish. Smallest binary (3 to 6 MB hello world, up to 10 to 20 MB for typical Mochi programs), no JIT, no runtime needed. Restrictions documented in notes 04 and 07: no `Reflection.Emit`, limited `Expression.Compile`, source-generated JSON only, no dynamic assembly load. Power-user option.
- `dotnet-nuget`. Produces a `.nupkg` (and matching `.snupkg` symbols package). Used by Mochi libraries that ship via nuget.org. Implies `dotnet-dll` for the compiled bits.

`mochi run` defaults to `dotnet run`, which is closer to `dotnet-exe` (framework-dependent, in-place). `mochi pack` is an alias for `mochi build --target=dotnet-nuget`.

## 2. Output shape `target/dotnet/`

The transpiler reserves a `target/dotnet/` directory under the Mochi project root. Layout:

```
target/dotnet/
  src/                  emitted .cs files, one per Mochi module
    Program.cs
    Mochi/
      Runtime/...       optional inlined runtime when --inline-runtime
  bin/                  msbuild OutputPath, intermediate assemblies
    Debug/net10.0/...
    Release/net10.0/...
  obj/                  msbuild scratch (BaseIntermediateOutputPath)
    project.assets.json
    project.nuget.cache
    Debug/net10.0/...
    mochi/              fingerprints + lower-IR cache (see section 14)
  publish/              final artefacts, one subdir per RID
    linux-x64/
      app                 native launcher or AOT binary
      app.dll             managed entry (single-file mode bundles into app)
      *.pdb
    osx-arm64/...
    win-x64/
      app.exe
  packages/             local NuGet cache (configurable, see section 5)
  cache/                Mochi-side incremental fingerprints (note 09)
  app.csproj            generated project file
  Directory.Build.props generated, holds Mochi-wide MSBuild knobs
  nuget.config          generated, scoped feed list
  packages.lock.json    NuGet pin file (see section 5)
  mochi.lock.json       Mochi-side superset: NuGet pins + SHA-256 + Mochi version
  global.json           pins the .NET SDK version
```

Two lock files exist on purpose. `packages.lock.json` is the standard NuGet artifact (consumed by `dotnet restore --locked-mode`). `mochi.lock.json` is a Mochi-side superset that adds SHA-256 of each .nupkg, the Mochi compiler version, the lowering pass version, and the c# output fingerprint. The Mochi lock file is what `mochi verify` checks.

## 3. `dotnet` CLI in-process vs MSBuild API

The Mochi build driver needs to actually invoke MSBuild. Two options:

**Option A: child process `dotnet build` / `dotnet publish`.** The driver shells out to `dotnet` and streams stdout/stderr through a parser that extracts diagnostics. Simpler, more diagnostic, matches what users do interactively. Failure modes are familiar (exit code, plus parsed CSC errors). This is the default.

**Option B: in-process MSBuild via `Microsoft.Build` NuGet (currently 18.6.3, May 2026).** The driver loads `Microsoft.Build.Evaluation.Project`, configures global properties programmatically, and runs targets in-process. Saves the per-invocation `dotnet` startup cost (which on cold cache is 300 to 800 ms). Requires `Microsoft.Build.Locator` to find a compatible MSBuild and matching SDK on the host. More fragile across SDK versions.

Defaulting to A keeps the diagnostic story simple: every Mochi build is reproducible from the command line by copying the displayed `dotnet ...` invocation. Option B is available behind `--in-process-msbuild` for batch scenarios (e.g. CI fan-out where each `dotnet` cold start adds up).

The driver itself runs on the host's Go binary (the existing Mochi compiler is Go); it shells to `dotnet` via `exec.Cmd`. No Mochi component links `Microsoft.Build.*` natively.

## 4. csproj generation

Mochi emits exactly one `.csproj` per Mochi module unit (typically per Mochi project root). SDK-style, minimal, source-controlled-friendly.

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFrameworks>net8.0;net10.0</TargetFrameworks>
    <RootNamespace>Mochi.App</RootNamespace>
    <AssemblyName>app</AssemblyName>
    <LangVersion>latest</LangVersion>
    <Nullable>enable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <NoWarn>$(NoWarn);CS1591</NoWarn>
    <Deterministic>true</Deterministic>
    <ContinuousIntegrationBuild
        Condition="'$(GITHUB_ACTIONS)' == 'true' or '$(CI)' == 'true'">true</ContinuousIntegrationBuild>
    <RestorePackagesWithLockFile>true</RestorePackagesWithLockFile>
    <RestoreLockedMode
        Condition="'$(ContinuousIntegrationBuild)' == 'true'">true</RestoreLockedMode>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Mochi.Runtime" Version="0.48.*"/>
    <!-- user @nuget pragmas land here -->
  </ItemGroup>
  <ItemGroup>
    <Compile Include="src/**/*.cs"/>
  </ItemGroup>
</Project>
```

Notes on each line:

- `TargetFrameworks` is multi-target. `net8.0` is the LTS that runs through November 2026. `net10.0` is the upcoming LTS. Both are produced so library users can pick. CLI builds typically use only `net10.0`.
- `Nullable=enable`. Mochi's nullable lowering (note 06) already encodes nullability statically; emitting NRT annotations keeps the C# output honest.
- `ImplicitUsings=disable`. Mochi emits explicit `using` directives. We do not want the SDK to inject `using System.Linq;` silently.
- `TreatWarningsAsErrors=true` plus a narrow `NoWarn` (CS1591 = missing XML docs on public APIs). Mochi-emitted code should be warning-clean.
- `Deterministic=true` is the SDK default since SDK 3.x. Re-stating it costs nothing and documents intent.
- `ContinuousIntegrationBuild` switches `Deterministic` into full reproducibility mode (path normalization, no host-specific data). Gated on CI env vars so local debugging still resolves source paths.
- `RestorePackagesWithLockFile=true` activates `packages.lock.json` generation. `RestoreLockedMode=true` (CI-only) enforces strict pin enforcement.

A separate `Directory.Build.props` carries Mochi-wide overrides that apply to any sub-project; for the single-project case it is generated but mostly empty.

## 5. NuGet integration

NuGet is the .NET equivalent of Maven Central. Mochi modules declare NuGet dependencies via a source-level pragma:

```mochi
@nuget("Newtonsoft.Json", "13.0.4")
@nuget("Serilog", "4.0.0")
import "csharp:Newtonsoft.Json"
```

The build pipeline:

1. The transpiler collects all `@nuget(name, version)` pragmas.
2. It writes `<PackageReference>` entries into the generated csproj.
3. It runs `dotnet restore` once, which produces `packages.lock.json`.
4. It hashes each restored `.nupkg` (SHA-256 of the package file, the format NuGet itself stores in `.signature.p7s` for signed packages) and writes `mochi.lock.json`. The Mochi lock file records package id, version, target framework alias, content hash, and (for transitive deps) the resolution path.
5. Subsequent builds run with `RestoreLockedMode=true`, so a divergence between csproj and `packages.lock.json` is an error. `mochi update` re-runs with `--force-evaluate` to refresh.

Sources are typically nuget.org plus optionally a local feed (`packages/` under `target/dotnet/`). The generated `nuget.config` scopes the feed list to avoid pulling from a user's global config.

## 6. Vendored runtime dependencies

`Mochi.Runtime` (the NuGet package that ships the runtime helpers from note 04) carries a deliberately slim transitive set:

- **System.Text.Json**. In-box since .NET 6, source-generator friendly for NativeAOT. The default JSON path for Mochi.
- **System.Linq.AsyncEnumerable**. In-box on .NET 10 as part of the BCL. On .NET 8 it ships as a NuGet package of the same name (the community `System.Linq.Async` package is deprecated as of .NET 10, see breaking-change notice). Mochi multi-targets both: a `Condition="'$(TargetFramework)' == 'net8.0'"` PackageReference on `System.Linq.AsyncEnumerable` covers the LTS path.
- **System.Collections.Immutable**. In-box since .NET 5. Used by Mochi's persistent collections lowering.
- **System.Threading.Channels**. In-box since .NET 5. Used by the agent stream lowering (note 09).
- **Microsoft.Extensions.Logging.Abstractions**. Optional; only pulled in if the user opts into the logging facade. The runtime falls back to a tiny internal no-op logger otherwise.

Explicitly **rejected** runtime deps:

- **Newtonsoft.Json**. We do not want two JSON stacks in the runtime. Users can still take a NuGet dep on it via `@nuget`, but Mochi's own JSON helpers route through `System.Text.Json`.
- **AutoMapper**. Mochi's record-to-record copy lowering is mechanical; no reflective mapper needed.
- **FluentAssertions**. Test-only and license-encumbered (commercial as of 2024); test helpers stay in `Mochi.Testing` which uses Xunit/NUnit primitives.
- **Polly**. Retry/policy concerns live in user code or in a separate opt-in `Mochi.Resilience` package, not in core runtime.
- **MediatR**. Out of scope. Mochi agents (note 09) handle their own dispatch via channels.

Keeping `Mochi.Runtime`'s closure narrow matters for NativeAOT: every transitive dep is one more set of trimmer warnings to triage.

## 7. MSBuild integration for existing C# projects

Some users will want to drop Mochi sources into an existing `.csproj` rather than have Mochi own the csproj. We support this through a `Mochi.Build` MSBuild SDK and tasks package:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <Sdk Name="Mochi.Build" Version="0.48.*"/>
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <MochiSource Include="src/**/*.mochi"/>
  </ItemGroup>
</Project>
```

The SDK's `.targets` file hooks the Mochi transpiler into `BeforeCompile`. Generated `.cs` files land in `obj/mochi/` and are added to `@(Compile)` via an `ItemGroup` so Roslyn sees them as normal sources. Incremental: the task hashes `.mochi` inputs against a `.cache` sibling and only re-runs the transpiler when inputs change.

An alternative is a C# source generator (`IIncrementalGenerator`) that runs the transpiler in-process inside Roslyn. This is more elegant but slower for large Mochi programs because the generator runs on every keystroke in the IDE. The MSBuild-task path stays on disk and integrates cleanly with watch mode (`dotnet watch`).

## 8. `dotnet pack` and publishing to nuget.org

`mochi pack` (alias of `mochi build --target=dotnet-nuget`) runs `dotnet pack -c Release` with these properties already set in the csproj:

```xml
<PackageId>$(AssemblyName)</PackageId>
<Version>0.0.0</Version>             <!-- overridden by mochi pack --version -->
<Authors>$(MochiPackageAuthors)</Authors>
<Description>$(MochiPackageDescription)</Description>
<PackageLicenseExpression>$(MochiPackageLicense)</PackageLicenseExpression>
<RepositoryUrl>$(MochiPackageRepoUrl)</RepositoryUrl>
<PackageReadmeFile>README.md</PackageReadmeFile>
<IncludeSymbols>true</IncludeSymbols>
<SymbolPackageFormat>snupkg</SymbolPackageFormat>
<PublishRepositoryUrl>true</PublishRepositoryUrl>
<EmbedUntrackedSources>true</EmbedUntrackedSources>
```

`mochi publish` wraps `dotnet nuget push *.nupkg -k $NUGET_API_KEY -s https://api.nuget.org/v3/index.json`. The companion `.snupkg` (symbols package, the new format that replaces the legacy `.symbols.nupkg`) is pushed to the same endpoint; nuget.org auto-routes symbols.

Mochi user libraries can use the same path. A user calling `mochi pack --version 1.2.3` gets a `.nupkg` they can publish under their own account.

## 9. R2R (ReadyToRun)

ReadyToRun pre-JITs IL into native code at publish time. The IL is still present (so reflection and tiered re-JIT still work), making the binary larger but the startup faster. Property:

```xml
<PublishReadyToRun>true</PublishReadyToRun>
```

Optional refinements:

- `<PublishReadyToRunComposite>true</PublishReadyToRunComposite>` produces a single composite R2R image across the whole closure; better cross-assembly inlining at the cost of larger size and slower compile. Recommended when tiered compilation is disabled.
- `<PublishReadyToRunEmitSymbols>true</PublishReadyToRunEmitSymbols>` emits `.r2rmap` (Linux) or `.ni.pdb` (Windows) for profilers.
- `<PublishReadyToRunExclude>` excludes specific assemblies (useful when an R2R-incompatible package blows up the compile).

Crossgen2 is the implementation; since .NET 6 it is the default and the legacy crossgen1 is gone. The Mochi build does not set `PublishReadyToRunUseCrossgen2` explicitly. R2R requires a RID, so `mochi build --target=dotnet-r2r` always implies `-r <host-rid>` (or whatever the user passes).

R2R is the option ASP.NET Core uses by default in container images, so it has the strongest production track record of the three AOT-ish modes. Mochi does not default to it for CLI builds because the size hit (20 to 50 MB on top of the bundled runtime) is significant for a hello-world.

## 10. NativeAOT

Property:

```xml
<PublishAot>true</PublishAot>
<InvariantGlobalization>true</InvariantGlobalization>
<StackTraceSupport>true</StackTraceSupport>
```

Setting `PublishAot=true` in the project file (not just on the CLI) is recommended: it activates the IL trim/AOT analyzers during normal `dotnet build`, so warnings surface in the IDE rather than at the final publish step. `InvariantGlobalization=true` further shrinks the binary (no ICU) and is appropriate for most CLI tools.

Mochi-emitted code must be **zero-warning** under the AOT analyzers. This is a hard gate in the test matrix (note 11). The lowering pass guarantees this by:

- never calling `Activator.CreateInstance(Type)` with a dynamic type,
- always invoking `System.Text.Json` through source-generated `JsonSerializerContext` instances,
- avoiding `Expression.Compile` (replaced by source-generated delegates),
- never loading assemblies dynamically (no `Assembly.Load(string)` in the runtime).

A regression here is a Mochi compiler bug, not a user bug. The Phase 16 gate (see section 21) wires a fixture that builds every example with `PublishAot=true -warnaserror` and fails on the first AOT warning.

Cross-architecture caveat: a recent .NET 10 distribution change moved the runtime-specific bits out of `Microsoft.DotNet.ILCompiler` into `Microsoft.NETCore.App.Runtime.NativeAOT.*` runtime packs. Cross-RID AOT may need a `PackageDownload` of the target runtime pack in CI. The implementation tracks this through a small RID-aware shim in the csproj generator.

## 11. Single-file deployment

```xml
<PublishSingleFile>true</PublishSingleFile>
<SelfContained>true</SelfContained>
<IncludeNativeLibrariesForSelfExtract>true</IncludeNativeLibrariesForSelfExtract>
<DebugType>embedded</DebugType>
```

Single-file bundles the managed entry, all assembly deps, and the .NET runtime into one OS-native executable. Native libraries are extracted to a temp dir on first launch unless `IncludeNativeLibrariesForSelfExtract=true` is set (then they extract from the bundle in memory). Size is bounded by the runtime itself; 70 to 80 MB is typical on .NET 8/10 with the standard BCL.

Note the SDK 10 behavior change: when `<SelfContained>` is not explicitly set in the csproj, SDK 10 ignores `--self-contained false` and produces a self-contained bundle anyway (see dotnet/sdk #51888). Mochi always writes `<SelfContained>true</SelfContained>` explicitly to avoid relying on SDK defaults that have shifted between releases.

Single-file is the **default** for `mochi build` because it has no reflection or runtime restrictions: every Mochi feature works exactly as it does in `dotnet run`. NativeAOT is faster and smaller but limits user code; single-file trades binary size for compatibility.

## 12. `dotnet publish` flag inventory

The set of flags the Mochi driver assembles for each target:

```
# dotnet-exe (framework-dependent)
dotnet publish -c Release -r linux-x64 --no-self-contained

# dotnet-single-file (default)
dotnet publish -c Release -r linux-x64 --self-contained \
  -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true

# dotnet-r2r
dotnet publish -c Release -r linux-x64 --self-contained \
  -p:PublishReadyToRun=true

# dotnet-aot
dotnet publish -c Release -r linux-x64 \
  -p:PublishAot=true -p:InvariantGlobalization=true

# dotnet-nuget
dotnet pack -c Release
```

Common across all: `-c Release` (Configuration), `--nologo`, `-bl:target/dotnet/bin/msbuild.binlog` (binary log for post-mortem, gated on `--verbose`).

## 13. Reproducible builds

`Deterministic=true` is on by default. `ContinuousIntegrationBuild=true` (set conditionally on CI env vars) finishes the job by normalizing source paths to `/_/`-style virtual roots and stripping host-local data.

SourceLink is wired through the optional `DotNet.ReproducibleBuilds` 2.0.2 metapackage:

```xml
<PackageReference Include="DotNet.ReproducibleBuilds" Version="2.0.2"
                  PrivateAssets="All"/>
```

This package requires MSBuild 17.8+ (i.e. SDK 8.0.100 or newer). It configures `DebugType=embedded`, embeds untracked sources, and turns on `PublishRepositoryUrl`. The result is byte-identical `.dll`/`.nupkg` outputs across machines given the same SDK, same NuGet pins, and the same source tree.

`SOURCE_DATE_EPOCH` is the cross-ecosystem env var that some distros (Debian especially) use to pin timestamps in packaging. The .NET SDK does **not** read `SOURCE_DATE_EPOCH` natively as of SDK 10 (verify, current behavior is to derive embedded timestamps from `Deterministic=true` logic instead). For Linux distro packaging the Mochi build wrapper passes the env var through to a small MSBuild target that sets `<SourceRevisionId>` and `<AssemblyOriginatorKeyFile>` derived values. This is good-enough for Debian's `dh-dotnet` workflow.

## 14. Three-layer caching

1. **Mochi aotir cache.** Per-module fingerprint over the `.mochi` source plus the lowering pass version. Stored under `target/dotnet/cache/aotir/`. Hits skip the entire C# emit step.
2. **Mochi-to-csproj cache.** The csproj generator hashes the set of `@nuget` pragmas, the resolved Mochi runtime version, the target framework list, and the emit fingerprint. A hit skips csproj regeneration (so MSBuild does not pick up a spurious timestamp change).
3. **MSBuild incremental.** Built into the SDK. Tracks input/output timestamps per target. Hits skip `Csc` invocations entirely.

Underneath those is the **NuGet global cache** at `~/.nuget/packages` (configurable via `NUGET_PACKAGES`). The Mochi driver does not duplicate this; it relies on the global cache for download dedup. Per-project `target/dotnet/packages/` is opt-in for fully air-gapped builds via `<RestorePackagesPath>$(MSBuildThisFileDirectory)packages</RestorePackagesPath>`.

Cache invalidation is conservative: any change to the global Mochi compiler version invalidates layers 1 and 2. SDK upgrades invalidate layer 3 automatically via MSBuild's own input tracking.

## 15. GitHub Actions setup

A minimal CI workflow for a Mochi-to-.NET project:

```yaml
name: ci
on: [push, pull_request]
jobs:
  build:
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        dotnet-version: ['8.0.x', '10.0.x']
        include:
          - os: ubuntu-latest
            rid: linux-x64
          - os: ubuntu-latest
            rid: linux-arm64
          - os: macos-latest
            rid: osx-arm64
          - os: windows-latest
            rid: win-x64
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v6
      - uses: actions/setup-dotnet@v4
        with:
          dotnet-version: ${{ matrix.dotnet-version }}
      - run: dotnet --info
      - run: mochi build --target=dotnet-single-file -r ${{ matrix.rid }}
      - run: mochi test
```

`actions/setup-dotnet@v4` is the current stable major (v5 exists but requires runner v2.327.1+; v4 is conservative). `actions/checkout@v6` is the current major.

For NativeAOT cross-architecture jobs (e.g. building `linux-arm64` from `ubuntu-latest`'s amd64 runner), the workflow installs cross toolchains:

```yaml
- name: install cross toolchain
  if: matrix.rid == 'linux-arm64'
  run: |
    sudo dpkg --add-architecture arm64
    sudo apt-get update
    sudo apt-get install -y clang llvm binutils-aarch64-linux-gnu \
        gcc-aarch64-linux-gnu zlib1g-dev:arm64
```

This is the path Microsoft documents for cross-AOT on Linux. For `osx-arm64` builds, GitHub provides M1 runners (`macos-latest` is arm64 since 2024). For `win-arm64` we rely on Windows arm64 runners (GA on GitHub since late 2024).

## 16. `mochi run --dotnet`

`mochi run --dotnet` invokes `dotnet run --project target/dotnet/app.csproj -- <args>` after the C# emit. Cold-start cost is dominated by JIT (200 to 400 ms hello-world). Warm runs reuse the `bin/Debug/` outputs.

Debugging is delegated to whichever debugger the user prefers:

- **VS Code with C# DevKit**: `dotnet run --launch-profile Debug` plus the Mochi VS Code extension wiring `launch.json` to point at `target/dotnet/app.csproj`.
- **JetBrains Rider**: attach via the `.csproj` directly; Rider treats Mochi-generated `.cs` as ordinary sources.
- **Visual Studio**: same as Rider; the generated project opens as a normal SDK-style project.

Source maps from `.mochi` to `.cs` are emitted as `#line` directives in the C# output so breakpoints set on `.cs` lines map to `.mochi` lines in the debugger UI. This is the same trick the F# compiler uses.

## 17. Default vs power user

The defaults are tuned for two personas:

- **Default user** (`mochi build`): wants a runnable artifact that "just works" everywhere. Gets `dotnet-single-file`. 70 to 80 MB, no .NET runtime needed on target, no reflection restrictions, no surprises. Ship it.
- **Power user** (`mochi build --target=dotnet-aot --rid linux-x64`): wants the smallest possible binary, accepts the AOT restrictions, has reviewed the trimmer warnings. Gets a 3 to 10 MB native binary.

Library authors target `dotnet-nuget`. Container builds typically pair `mochi build --target=dotnet-aot -r linux-musl-x64` with a `FROM scratch` Dockerfile.

## 18. Size budgets

Reference sizes from public .NET 10 benchmarks plus internal Mochi measurements (will be re-measured in Phase 16):

- **.NET 8 NativeAOT hello-world**: 4 to 6 MB stripped, Linux x64.
- **.NET 10 NativeAOT hello-world**: 3 to 5 MB stripped. Trimming improvements landed in 9 and 10.
- **.NET 8 single-file self-contained**: 70 to 80 MB. Bundles the runtime, BCL, and the launcher.
- **.NET 10 single-file self-contained**: 65 to 75 MB. BCL slimming and ICU removal trims a few MB.
- **R2R self-contained**: 20 to 50 MB extra over the equivalent JIT-only single-file because R2R duplicates IL with native code.

Mochi-side overhead (`Mochi.Runtime` plus the lowered code) typically adds 0.5 to 2 MB on top of the bare runtime, much less than what the JVM target adds (note 04 measures 8 to 12 MB for `mochi-runtime.jar` on the JVM side).

## 19. Cross-compile

`dotnet publish -r <target-rid>` cross-compiles from any host for any RID, **for managed-only builds** (no NativeAOT). For NativeAOT cross-architecture:

- x64 host to arm64 target: supported on Linux/macOS/Windows since .NET 8. Requires the cross toolchain (clang, lld, target sysroot) installed.
- arm64 host to x64 target: supported since .NET 8.
- Cross-**OS** is not supported. AOT to win-x64 from Linux is not possible.

The `linux-musl-*` RIDs (Alpine) are particularly useful for container builds because the resulting binary is smaller and links against musl libc, matching Alpine's libc.

Note .NET 9's runtime distribution change (mentioned in section 10) means some NuGet `runtime.*.Microsoft.DotNet.ILCompiler` packages no longer carry their runtime bits; cross-RID AOT in CI must pre-download the matching `Microsoft.NETCore.App.Runtime.NativeAOT.<rid>` runtime pack. Mochi's build driver handles this automatically by emitting a `<PackageDownload>` line when it detects a cross-RID AOT request.

## 20. Round-trip verification

The Mochi test suite includes a `roundtrip-nuget` fixture (Phase 16 gate). The fixture does:

1. `mochi pack --target=dotnet-nuget --version 0.0.1-test` on a small library project.
2. `mochi nuget push --source ./target/dotnet/packages` to a local feed.
3. `mochi build` on a consumer project that takes a NuGet dep on the library via `@nuget("FixtureLib", "0.0.1-test")`.
4. `mochi run` to exercise the imported API.
5. Re-run the consumer build with `RestoreLockedMode=true` to validate the lock file pinned the right SHA-256.

This catches whole categories of bugs: csproj emit producing invalid `PackageReference`s, lock file SHA mismatch, `Mochi.Runtime` framework-version skew, missing `using` directives in the emitted public surface.

## 21. Phase mapping

Mirroring MEP-47's phase numbering for the JVM build pipeline:

- **Phase 12**: csproj emit, `dotnet build`, `dotnet run`. Default target = `dotnet-exe`. NuGet pragmas implemented.
- **Phase 13**: `dotnet-single-file`. Becomes default for `mochi build`. Size and startup benchmarks recorded.
- **Phase 14**: `dotnet-nuget`. `mochi pack` and `mochi publish`. SourceLink wiring. Roundtrip fixture.
- **Phase 15**: `dotnet-r2r`. Composite R2R benchmark and ASP.NET Core fixture.
- **Phase 16**: `dotnet-aot`. Trimmer-warning-zero gate. NativeAOT cross-RID fixture (`linux-x64` to `linux-arm64`).
- **Phase 17**: Reproducibility. `DotNet.ReproducibleBuilds`, `ContinuousIntegrationBuild`, deterministic-build CI matrix.

Each phase has a gate test under `tests/dotnet/phase-<n>/` matching the Mochi MEP-47 convention.

## 22. Rejection of alternative build systems

The .NET community has several side ecosystems for build automation. We chose to stay on stock MSBuild plus the `dotnet` CLI for all of them:

- **Fake (F# Make)**. F#-flavored build DSL, popular in 2018-2022. Niche, requires F# in the build pipeline. Mochi-to-.NET is C#-shaped; adding F# tooling is pure overhead.
- **Paket**. F#-community alternative to NuGet. Solves problems NuGet has since solved (lock files, version pinning). No reason to take the dep.
- **Cake (C# Make)**. Build orchestration scripting in C#. Useful for very large solutions with bespoke automation. For a transpiler-generated single-project layout, MSBuild's built-in incrementality is enough.
- **Bullseye, NUKE**. Build DSLs. Same answer as Cake: out of scope.
- **Bazel for .NET (`rules_dotnet`)**. Bazel is excellent for hermetic monorepos. For an indie .NET app, the setup cost vastly exceeds the value. We will not generate Bazel BUILD files.
- **InvokeBuild (PowerShell)**. Build automation in PowerShell. Same out-of-scope rationale.

The implementation tracks a single happy path: `dotnet` CLI + generated csproj. Anything else stays user-driven.

## 23. Cross-references

Within MEP-48:

- [[01-language-surface]] for which Mochi constructs survive into the C# layer the build compiles.
- [[04-runtime]] for `Mochi.Runtime` content and how its NuGet packaging shapes section 6.
- [[05-codegen-design]] for how the emitter writes the `src/` tree section 2 references.
- [[06-type-lowering]] for the nullable / generic emit decisions that the AOT analyzer in section 10 will scrutinize.
- [[07-dotnet-target-portability]] for AOT restrictions and trim-safety.
- [[08-dataset-pipeline]] for the data ingestion / parquet deps that some user projects pull in via section 5.
- [[09-agent-streams]] for the channel-based runtime that section 6 reserves space for.
- [[11-testing-gates]] for how Phase 12 to 17 gates wire into the per-PR CI.
- [[12-risks-and-alternatives]] for the AOT-vs-JIT tradeoff at the program level.

Sibling MEP cross-ref:

- [[../0047/10-build-system]] for the JVM equivalent. The structure is intentionally parallel: Gradle/Maven plays the role of MSBuild/NuGet, jlink the role of single-file, jpackage the role of `dotnet-exe`, GraalVM native-image the role of NativeAOT.

## Sources

Verified May 2026 against current docs.

- [.NET Native AOT deployment overview, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/)
- [Native AOT in .NET 10, Sanjay Chaudhari](https://sanjaychaudhari.com/blog/native-aot-dotnet-10/)
- [Native AOT in .NET 10 for C# Developers, dev.to](https://dev.to/chandana_pushpakumara_4bf/native-aot-in-net-10-everything-for-c-developers-2m7e)
- [Native AOT cross-compilation, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/cross-compile)
- [dotnet/runtime NativeAOT compiling docs](https://github.com/dotnet/runtime/blob/main/src/coreclr/nativeaot/docs/compiling.md)
- [Single-file deployment overview, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/deploying/single-file/overview)
- [dotnet publish command reference, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/tools/dotnet-publish)
- [dotnet/sdk #51888 SDK 10 SelfContained behavior change](https://github.com/dotnet/sdk/issues/51888)
- [ReadyToRun deployment overview, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/deploying/ready-to-run)
- [.NET RID catalog, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/rid-catalog)
- [RuntimeIdentifier change in .NET 8, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/compatibility/core-libraries/8.0/runtimeidentifier)
- [System.Linq.AsyncEnumerable breaking change in .NET 10, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/compatibility/core-libraries/10.0/asyncenumerable)
- [System.Linq.AsyncEnumerable NuGet 10.0.7](https://www.nuget.org/packages/System.Linq.AsyncEnumerable/)
- [Ix.NET v7.0, .NET 10 and LINQ for IAsyncEnumerable, endjin](https://endjin.com/blog/2025/11/ix-v7-dotnet-10-linq-iasyncenumerable)
- [DotNet.ReproducibleBuilds 2.0.2 on NuGet](https://www.nuget.org/packages/DotNet.ReproducibleBuilds/)
- [dotnet/reproducible-builds on GitHub](https://github.com/dotnet/reproducible-builds)
- [Microsoft.Build 18.6.3 on NuGet, May 2026](https://www.nuget.org/packages/microsoft.build/)
- [Find and use MSBuild via API, Microsoft Learn](https://learn.microsoft.com/en-us/visualstudio/msbuild/find-and-use-msbuild-versions)
- [NuGet repeatable restore using lock file, .NET Blog](https://devblogs.microsoft.com/dotnet/enable-repeatable-package-restores-using-a-lock-file/)
- [Enable repeatable package restore using lock file, NuGet/Home wiki](https://github.com/NuGet/Home/wiki/Enable-repeatable-package-restore-using-lock-file)
- [SourceLink docs on GitHub](https://github.com/dotnet/sourcelink/blob/main/docs/README.md)
- [Producing Packages with Source Link, .NET Blog](https://devblogs.microsoft.com/dotnet/producing-packages-with-source-link/)
- [.NET 5 Deterministic Builds and Source Linking, Mitchel Sellers](https://mitchelsellers.com/blog/article/net-5-deterministic-builds-source-linking)
- [actions/setup-dotnet on GitHub](https://github.com/actions/setup-dotnet)
- [GitHub Docs, building and testing .NET](https://docs.github.com/en/actions/use-cases-and-examples/building-and-testing/building-and-testing-net)
- [Create a NuGet package using MSBuild, Microsoft Learn](https://learn.microsoft.com/en-us/nuget/create-packages/creating-a-package-msbuild)
- [NuGet pack and restore as MSBuild targets, Microsoft Learn](https://learn.microsoft.com/en-us/nuget/reference/msbuild-targets)
- [PackageReference docs, Microsoft Learn](https://learn.microsoft.com/en-us/nuget/consume-packages/package-references-in-project-files)
- [ASP.NET Core support for Native AOT, Microsoft Learn](https://learn.microsoft.com/en-us/aspnet/core/fundamentals/native-aot?view=aspnetcore-10.0)
- [.NET application publishing overview, Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/deploying/)
