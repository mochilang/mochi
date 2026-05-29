---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "The MEP-68 risk register (CLR hosting API versioning, TFM mismatch, shim compile time, CLR startup overhead, offline NuGet restore, NativeAOT compat, Windows-only packages, trusted publishing config, generic explosion, GetAwaiter deadlock, package signing, version conflicts) and the rejected alternatives (C# source parsing, XML docs, NativeAOT default, direct P/Invoke, COM interop, GraalVM, IKVM, dotnet-embed, long-lived API keys, WIT, auto-monomorphise, TFM mirror)."
---

# 12. Risks and alternatives

This note collects the risks MEP-68 carries plus the alternative approaches that were considered and rejected. The risk register is a forward-looking inventory; the alternatives section documents the reasoning so future maintainers can understand why the chosen path was chosen.

## R1: CLR hosting API versioning

**Risk**: the `hostfxr` API surface is documented as stable since .NET 5, but future .NET versions may deprecate or change the runtime configuration file format (`.runtimeconfig.json`) or the `hostfxr_get_runtime_delegate` function signature. MEP-68 pins to the .NET 8 LTS hosting contract.

**Likelihood**: low. Microsoft has maintained `hostfxr` compatibility across .NET 5/6/7/8/9. The runtime configuration format has been stable since .NET Core 3.0.

**Impact**: medium. A breaking change in `hostfxr` would require a bridge update.

**Mitigation**: the bridge reads the `hostfxr` version from the SDK header at lock time and errors if outside the supported range. The bridge ships a new minor version alongside each .NET LTS release.

**Residual**: a .NET 10 breaking change to `hostfxr` semantics (unlikely but possible) requires a bridge update. Users on .NET 8 LTS are unaffected until 2026-EOL (November 2026).

## R2: TFM mismatch at lock and build time

**Risk**: `mochi.lock` records the TFM used at lock time. If the user changes `[dotnet] framework` between locks, or if CI builds on a different .NET SDK version than the developer, the shim built at lock time may reference assemblies not present at build time.

**Likelihood**: medium. Developers frequently work on multiple .NET versions; CI is often configured differently.

**Impact**: medium. A TFM mismatch at build time produces a `dotnet build` error, which is immediately visible.

**Mitigation**: the lockfile records `target-framework` per `[[dotnet-package]]` entry. A `mochi pkg lock --check` detects TFM drift and errors. The `[dotnet] framework` key defaults to `"net8.0"` to reduce accidental drift.

**Residual**: a first-time `mochi pkg lock` on a machine with only .NET 9 SDK installed, when `[dotnet] framework = "net8.0"` is declared, requires the .NET 8 targeting packs. Mitigation: the bridge checks SDK presence at lock time and emits guidance.

## R3: C# shim compile time

**Risk**: every imported NuGet package triggers a C# shim project build. For a program with 10 .NET imports, cold shim builds add ~150 seconds total to the first build.

**Likelihood**: certain. The shim compile cost is intrinsic.

**Impact**: medium. Slow first-time builds increase iteration cost.

**Mitigation**: shim build artefacts are cached in `~/.cache/mochi/dotnet-deps/shims/<shim-sha256>/`. Subsequent builds with the same shim SHA-256 are instant cache hits. The shim SHA-256 changes only when the package version or the `mochi-dotnet-meta` output changes.

**Residual**: a cache-cold user pays the full compile cost. First-time CI runs pay it. We accept this.

## R4: CLR startup overhead

**Risk**: `hostfxr_initialize_for_runtime_config` takes 150-300 ms on cold start. Short-lived CLI tools that use MEP-68 .NET packages suffer disproportionate startup latency.

**Likelihood**: certain. The CLR startup cost is intrinsic to CLR hosting.

**Impact**: medium. A tool that does `mochi run ./mytool.mochi` sees 150-300 ms CLR startup on the first run.

**Mitigation**: NativeAOT (phase 13) eliminates this cost for AOT-compatible packages. For CLR hosting, the startup cost is paid once at process startup; long-running programs are unaffected. Users who need fast startup and have AOT-compatible deps should use `[dotnet] bridge = "nativeaot"`.

**Residual**: CLR-hosted tools on cold start are slower than pure-native Mochi binaries by 150-300 ms. This is documented and expected.

## R5: NuGet offline restore in hermetic CI

**Risk**: the C# shim project references NuGet packages via `<PackageReference>`. The `dotnet restore` step requires network access to `api.nuget.org` unless a local feed is configured. Hermetic CI environments (no outbound NuGet access) fail at restore time.

**Likelihood**: medium. Hermetic CI is common in enterprise environments.

**Impact**: medium. CI builds fail with a NuGet restore error if offline feeds are not configured.

**Mitigation**: the bridge copies each `.nupkg` from the content-addressed cache into a local NuGet feed directory (`<workdir>/dotnet_shim/_localfeed/`) before running `dotnet restore`, then adds `--source <workdir>/dotnet_shim/_localfeed/` to the restore invocation. This enables fully offline restore from the content-addressed cache.

**Residual**: a user who runs `mochi pkg lock` on a machine without NuGet access cannot populate the content-addressed cache. The initial `mochi pkg lock` always requires NuGet access.

## R6: NativeAOT compatibility detection is imperfect

**Risk**: the `IsAotCompatible` NuGet metadata property is advisory (the package author declares it; it is not verified by a build-time check). A package that declares `IsAotCompatible=true` may still fail the trim step.

**Likelihood**: low to medium. Package authors are increasingly careful, but false declarations exist.

**Impact**: medium. The trim step fails with an analysis error; the NativeAOT build fails.

**Mitigation**: phase 13 gate exercises AOT trim against the full 20-package corpus and maintains a known-good/known-bad registry. The bridge checks the registry before trusting `IsAotCompatible`. A package known-bad in the registry overrides the declared property.

**Residual**: a new package (not in the registry) that declares `IsAotCompatible=true` but fails trim will cause phase 13 gate failures. The registry is updated quarterly.

## R7: Windows-only packages in a cross-platform deployment

**Risk**: some NuGet packages expose Windows-only P/Invoke surfaces (e.g., `System.Drawing.Common` before .NET 7's platform-guard attributes). On Linux/macOS, `PlatformNotSupportedException` is thrown at runtime.

**Likelihood**: low. Most popular packages have eliminated Windows-only APIs since .NET 6's cross-platform initiative.

**Impact**: medium. A runtime exception from a package that appeared to work at lock time.

**Mitigation**: the bridge checks `<platform-guard>` attributes in the package's `.nuspec` and platform-conditional compilation symbols. A package with `[SupportedOSPlatform("windows")]` on its primary API surface is warned at lock time when `[dotnet] runtime` is non-Windows.

**Residual**: packages that do not use `[SupportedOSPlatform]` correctly will slip through. This is a NuGet ecosystem quality issue.

## R8: Trusted publishing configuration required before first publish

**Risk**: a first-time publisher who runs `mochi pkg publish --to=nuget.org` without having configured a trusted publisher on nuget.org receives a 403.

**Likelihood**: certain for first-time publishers.

**Impact**: low. The error message is clear; the fix is a one-time web UI step.

**Mitigation**: the bridge detects the 403, emits step-by-step configuration guidance, and provides `--emit-ci` to generate the workflow template.

**Residual**: the one-time setup requirement is unavoidable. It is a nuget.org design constraint.

## R9: Generic explosion via `[dotnet.monomorphise]`

**Risk**: a user who lists 50+ monomorphisations generates 50+ shim methods and 50+ Mochi extern declarations. Compile time and binary size grow.

**Likelihood**: low for typical users; possible for power users working with heavily generic APIs.

**Impact**: low to medium. Compile time increases; binary size grows by ~10-50 KB per 10 monomorphisations.

**Mitigation**: the bridge warns at 10+ monomorphisations per item and errors at a configurable maximum (default 200 total). The error message explains the trade-off.

**Residual**: a determined user can override the limit. We accept this.

## R10: `.GetAwaiter().GetResult()` deadlock in non-standard contexts

**Risk**: a user who calls MEP-68 shim methods from a .NET managed thread that has an existing `SynchronizationContext` (e.g., a WPF or ASP.NET Core context) can hit the classic `.GetAwaiter().GetResult()` deadlock.

**Likelihood**: low. MEP-68's primary use case is calling from the Mochi main thread (no sync context). A deadlock requires the user to mix Mochi + CLR-hosted .NET managed threads.

**Impact**: high when it occurs. A deadlock hangs the process.

**Mitigation**: the shim uses `.ConfigureAwait(false)` on all async calls to prevent sync context capture. This eliminates the most common deadlock scenario. Users who mix threads should review async patterns in their custom extern overrides.

**Residual**: exotic sync contexts (custom task schedulers, custom `SynchronizationContext` implementations) may still deadlock despite `.ConfigureAwait(false)`. This is a known .NET async limitation.

## R11: Package signature verification coverage

**Risk**: as of May 2026, NuGet package signing is optional; the majority of packages on nuget.org are unsigned. The bridge cannot verify the integrity of unsigned packages beyond the SHA-512 hash.

**Likelihood**: certain. Most packages are unsigned.

**Impact**: medium. An unsigned package from nuget.org could, in theory, be tampered with in transit; SHA-512 integrity is the only guarantee.

**Mitigation**: SHA-512 of the `.nupkg` is recorded in `mochi.lock` and verified at every `mochi pkg lock --check`. A future `[dotnet.capabilities] require-signed = true` option will reject unsigned packages. The trusted-publishing path for the publish direction does not affect the consumption-side signing story.

**Residual**: the SHA-512 pin is a strong tamper-evident mechanism for the content-addressed cache path. Transit security relies on HTTPS to nuget.org.

## R12: NuGet version resolution conflicts

**Risk**: two packages that both depend on `Newtonsoft.Json` at incompatible constraints (`^12.0` and `^13.0`) cannot be unified by the NuGet resolver.

**Likelihood**: medium. Version conflicts are common in large dependency graphs.

**Impact**: medium. Lock fails with a version conflict error.

**Mitigation**: the bridge surfaces the conflict with a specific diagnostic:
```
ERROR: version conflict
  Serilog.Extensions.Logging@8.0 requires Serilog@>=3.1.0
  OldLibrary@1.0 requires Serilog@>=2.0.0, <3.0.0
  Resolution: add `Serilog = "^3.1"` to [dotnet-dependencies] to force the newer version,
              or remove OldLibrary from [dotnet-dependencies].
```
The user can override by pinning the conflicting package explicitly in `[dotnet-dependencies]`.

**Residual**: some conflicts have no resolution (a package hardcodes an incompatible version constraint). The user must find an alternative package.

## Alternatives considered

### A1: C# source parsing via Roslyn

Roslyn (the C# compiler as a library) can parse C# source files and produce a semantic model. MEP-68 could use Roslyn to parse the NuGet package source (when included in the `.nupkg`) instead of reading assembly metadata.

Why rejected:
- Most NuGet packages do not include C# source in the `.nupkg`. The source is in a separate SourceLink-referenced repository.
- Roslyn source generators, partial classes, and `#if` compile-time conditionals mean the source-level surface differs from the compiled surface.
- Shipping Roslyn as a Go dependency would be impractical. The `mochi-dotnet-meta` CLI tool is the right shape.

### A2: XML documentation files

NuGet packages ship optional `.xml` documentation files alongside the `.dll`. MEP-68 could use these as the binding source.

Why rejected:
- The `.xml` file contains documentation strings, not type signatures. There is no parameter type information in the XML format.
- Reconstructing method signatures from XML documentation is impossible without the assembly.

### A3: NativeAOT as the default bridge mode

NativeAOT could be the default, with CLR hosting as the fallback for AOT-incompatible packages.

Why rejected:
- As shown in [[11-nativeaot-and-trimming]], 8 of the 20 fixture packages are not AOT-compatible. A default of NativeAOT would silently fail for 40% of the fixture corpus.
- NativeAOT requires a more complex build step and an LLVM or MSVC toolchain on the host. The developer experience is worse for the common case.

CLR hosting is the conservative default; NativeAOT is the opt-in for users who need lower startup time.

### A4: Direct P/Invoke from the Mochi binary

Mochi could generate P/Invoke declarations that call directly into the NuGet package's assembly. P/Invoke targets native entry points.

Why rejected:
- P/Invoke requires the target function to be a native (unmanaged) export. NuGet package assemblies contain managed IL, not native exports. P/Invoke cannot call a managed CLR method without a native wrapper.
- The `[UnmanagedCallersOnly]` C# shim generates exactly the native wrappers that P/Invoke requires. The shim IS the necessary intermediary.

### A5: COM interop as the bridge mechanism

COM (Component Object Model) allows cross-language object invocation via `IDispatch`. MEP-68 could use COM interop to call .NET objects.

Why rejected:
- COM is Windows-only. MEP-68 targets Linux, macOS, and Windows.
- Not all .NET types implement COM-visible interfaces. The majority of NuGet packages do not.
- The CLR hosting API + `[UnmanagedCallersOnly]` is the modern, cross-platform equivalent.

### A6: GraalVM polyglot as the bridge

GraalVM's polyglot API allows multiple languages to share a heap inside the GraalVM JVM.

Why rejected:
- GraalVM does not host the .NET CLR. It hosts JVM languages, JavaScript, Python, Ruby.
- Mochi targets native binaries; running inside a JVM is not applicable.

### A7: IKVM.NET (JVM-to-.NET translation)

IKVM.NET translates JVM bytecode to .NET MSIL. MEP-68 could use IKVM to translate Mochi (JVM-compiled) to .NET.

Why rejected:
- Mochi does not target the JVM. IKVM is not applicable to a native Mochi binary.
- IKVM adds a JVM dependency which is heavier than a CLR hosting dependency.

### A8: `dotnet-embed` (static archive of the .NET runtime)

The .NET runtime could hypothetically be linked as a static archive into the Mochi binary.

Why rejected:
- The .NET runtime is not published as a static archive. The only supported embedding mechanism is the CLR hosting API.
- Statically linking the CLR would produce binaries of 30-50 MB even before the user's code.

### A9: Long-lived NuGet API keys

The publish flow could use the `NUGET_API_KEY` environment variable for `nuget push` authentication.

Why rejected:
- NuGet trusted publishing has been GA since March 2024. Long-lived API keys are the historical supply-chain attack vector.
- MEP-57 and MEP-73 both mandate Sigstore-keyless / OIDC publish. Consistency requires MEP-68 to follow the same path.
- `--allow-token-fallback` exists for the transition period with explicit deprecation warning.

### A10: WIT (WebAssembly Interface Types) as the bridge protocol

WIT (the WebAssembly Component Model Interface Types) is a candidate for cross-language binding. MEP-68 could require .NET packages to be compiled as Wasm components with WIT descriptions.

Why rejected:
- .NET's Wasm Component Model support (`dotnet-wasi` WASI preview 2) is experimental as of May 2026.
- The vast majority of NuGet packages do not ship WIT descriptions.
- MEP-68 targets native binaries, not Wasm.

WIT may become an additional bridge mode in a future sub-phase for Wasm-targeting Mochi programs.

### A11: Auto-monomorphise all generic instantiations

The bridge could automatically discover all generic instantiations used in the user's Mochi source and generate shim methods for each.

Why rejected:
- The combinatorial explosion for packages like `System.Linq.Enumerable` (dozens of generic methods, each needing a monomorphisation for each type in the user's program) is unbounded.
- The explicit `[dotnet.monomorphise]` table makes the explosion bounded and user-visible (the same argument as [[02-design-philosophy]] §6).

### A12: Mirror the transitive dep graph verbatim from each package's `.nuspec`

The bridge could copy every `<dependency>` from each package's `.nuspec` into `mochi.lock`, including all framework-unconditional entries.

Why rejected:
- NuGet's `<group targetFramework="...">` attribute scopes dependencies to specific TFMs. Mirroring all groups would introduce spurious dependencies (e.g., .NET Standard 2.0 polyfills that are not needed on .NET 8).
- The bridge resolves against the declared `[dotnet] framework` TFM and records only the TFM-scoped transitive deps in `mochi.lock`. This is the NuGet-correct behaviour.

## Cross-references

- [[02-design-philosophy]] for the load-bearing decisions that drove these choices.
- [[03-prior-art-bridges]] for the broader landscape of .NET language bridges.
- [[07-nuget-trusted-publishing]] for the trusted-publishing rationale.
- [MEP-68 §Alternatives](/docs/mep/mep-0068#alternatives-considered) for the normative alternatives list.
- [MEP-68 §Risks](/docs/mep/mep-0068#risks) for the normative risk register.
