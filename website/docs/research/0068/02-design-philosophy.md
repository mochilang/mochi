---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional bridge, why assembly metadata over C# source parsing, why a C# shim with [UnmanagedCallersOnly] over direct P/Invoke or COM interop, why CLR hosting is the default over NativeAOT, why NuGet trusted publishing is the only publish path, and why the type-mapping table is closed not open."
---

# 02. Design philosophy

This note frames the six load-bearing design decisions in MEP-68 alongside the alternatives that were considered and rejected. Each section follows the same structure: the decision, the alternatives, the trade-offs.

## 1. Why bidirectional

MEP-68 could have shipped only the consume direction (`import dotnet "..."`) and deferred Mochi-as-nuget.org-publisher to a future MEP. Or only the publish direction. The two directions are structurally independent: consuming a package uses assembly metadata ingest plus C# shim generation; publishing uses the `TargetDotNetLibrary` emit path plus the nuget.org upload API. They share infrastructure (the manifest tables, the lockfile, the trusted-publishing OIDC flow) but no code paths.

Shipping both directions in one MEP is justified because:

- **Symmetric distribution.** A library author writes Mochi, depends on .NET packages, and publishes to nuget.org. A library consumer either writes Mochi (uses `import dotnet`) or writes C# (uses `dotnet add package`). A unidirectional bridge would leave one side of the symmetry broken.

- **Shared OIDC infrastructure.** nuget.org trusted publishing requires a working OIDC token exchange in CI. Implementing this once (for nuget.org publish) and not at all (because consumers do not use it) would mean duplicating the infrastructure later. Doing both directions at once amortises the work.

- **Lockfile coherence.** The `[[dotnet-package]]` lockfile entry records both the consumer-side hashes (`nupkg-sha512`, `metadata-sha256`, `shim-sha256`) and the capability surface. If the consume and publish paths are separate MEPs, the lockfile section would need to evolve across two MEPs, introducing migration cost.

The trade-off is that MEP-68 is a larger spec than a single-direction bridge. The alternative (split into MEP-68 consume + MEP-69 publish) was rejected because the seam is artificial and would force two redundant lockfile-section migrations.

## 2. Why assembly metadata

The bridge needs a machine-readable description of every NuGet package's public surface. Four candidate sources existed:

- **`System.Reflection.Metadata.MetadataReader`** (reading the ECMA-335 binary directly from the `.dll`). Produces the complete type system surface: namespaces, types, methods, fields, properties, events, generic parameters, custom attributes, and return types, all with full CLR type signatures. Available for every .NET assembly since .NET Core 2.1. The output is deterministic for a given `.dll` binary.

- **Roslyn (`Microsoft.CodeAnalysis.CSharp`)** (parsing the C# source via the Roslyn compiler API). Would require accessing the NuGet package's source (not shipped in most `.nupkg` files; only the compiled `.dll` is required by NuGet). Even when source is available, Roslyn source generators, partial classes, `#if` conditional compilation, and code generation from `.resx` files all mean the post-compilation surface can differ from the source-level surface.

- **XML documentation files** (the `.xml` file shipped alongside many NuGet packages, containing `<member name="...">` documentation nodes). The `.xml` file contains documentation strings but not type signatures. Recovering method parameter types from XML documentation is not possible; the documentation format omits type information for parameters.

- **`dotnet-dump` or `ilspy` decompilation** (re-decompiling the assembly to C# source). Fragile, slow, non-deterministic across compiler versions, and overkill. The decompiled output is useful for human reading, not for machine-structured ingest.

`System.Reflection.Metadata.MetadataReader` wins on every axis. It reads the ECMA-335 binary directly; the output is the post-compilation ground truth; it is fast (reading a 2 MB assembly takes under 100 ms); and it ships with the .NET BCL, requiring no additional tool installation.

The `mochi-dotnet-meta` CLI tool wraps `MetadataReader` into a single-file .NET executable that reads a `.dll` and emits a JSON document. Wrapping in a CLI tool (rather than calling MetadataReader from Go via CGO or a Go .NET binding) is the right architecture: the .NET tooling for reading ECMA-335 is far more complete than any Go port would be, and a CLI tool is simpler to invoke, version-pin, and test.

## 3. Why [UnmanagedCallersOnly] C# shim

Given an ingested assembly surface, the bridge has three routes to making CLR methods callable from Mochi:

- **Direct P/Invoke**: Mochi's emit pass generates P/Invoke declarations for each method. P/Invoke targets `extern "C"` entry points in native DLLs; it cannot call managed CLR methods directly. A managed method must first be exposed as a native entry point to be P/Invoke-callable. Direct P/Invoke without a shim is not feasible for arbitrary NuGet packages.

- **`[UnmanagedCallersOnly]` C# shim assembly**: the bridge generates a C# project (`dotnet_shim/<pkg>/`) that depends on the source package and exposes a flat native-callable surface using `[UnmanagedCallersOnly]`. This attribute (stable since .NET 5) marks a static method as callable from native code via a function pointer obtained from the CLR hosting API or as a P/Invoke target. Each translatable public method becomes one `[UnmanagedCallersOnly]` static method that calls the source package's method and marshals the result.

- **COM interop**: COM (Component Object Model) allows inter-process and cross-language calls via `IDispatch` and vtable-based interfaces. COM interop in .NET (via `System.Runtime.InteropServices.ComObject`) is Windows-only and requires the package to expose COM-visible types. Not all NuGet packages do; the Windows-only restriction disqualifies COM for the default path.

The `[UnmanagedCallersOnly]` shim is the only path that:

1. Works on Linux, macOS, and Windows.
2. Works with any NuGet package regardless of COM visibility.
3. Produces a stable, auditable, lockfile-pinned surface (each shim SHA-256 is recorded in `mochi.lock`).
4. Does not require the user to write any P/Invoke declarations.

The shim is analogous to MEP-73's synthesised `extern "C"` wrapper crate: both generate a thin language-specific glue layer that the Mochi side calls through a known ABI. The Mochi user writes `import dotnet "..."` and gets a native-callable surface without seeing the shim.

## 4. Why CLR hosting is the default over NativeAOT

The bridge has two runtime modes:

- **CLR hosting** (`hostfxr_initialize_for_runtime_config` + `load_assembly_and_get_function_pointer`): the .NET CLR is embedded in the Mochi process. The shim assembly is loaded into the CLR at startup; function pointers to the `[UnmanagedCallersOnly]` entry points are obtained via `hostfxr_get_runtime_delegate`. All CLR features (reflection, generics at runtime, the thread pool, GC) are available.

- **NativeAOT** (`dotnet publish -r <rid> --self-contained -p:PublishAot=true`): the shim project is ahead-of-time compiled to a native shared library. No CLR at runtime; no JIT; no reflection (unless marked `[DynamicDependency]`). Startup is instant; memory footprint is lower; the binary is fully self-contained.

CLR hosting is the default because:

- **Universal package compatibility.** As of May 2026, the majority of the top-downloaded NuGet packages are not fully NativeAOT-compatible. Entity Framework Core, Serilog (with reflection-based sink configuration), RestSharp (with dynamic proxies), and AutoMapper (with runtime code generation) all require CLR features that NativeAOT cannot provide. A bridge that defaults to NativeAOT would silently fail for the majority of the fixture corpus.

- **Simpler developer experience.** CLR hosting requires only a .NET 8 SDK installation. NativeAOT requires the `dotnet publish` command with AOT toolchain, a native compiler (LLVM or MSVC), and platform-specific trimmable versions of all dependencies. The extra toolchain requirement is inappropriate for the default path.

- **Graceful degradation.** A CLR-hosting shim that calls `Serilog.Log.Information(...)` works; a NativeAOT shim that tries to load a Serilog sink via reflection fails at trim time with a diagnostic. The CLR hosting path silently handles features that NativeAOT rejects.

NativeAOT is opt-in via `[dotnet] bridge = "nativeaot"` for users whose dep graph supports it. The gate at lock time checks AOT-compatibility and errors early when a package in the dep graph is AOT-incompatible. See [[11-nativeaot-and-trimming]] for the AOT compatibility landscape.

## 5. Why NuGet trusted publishing only

nuget.org has historically used long-lived API keys for `nuget push` authentication. NuGet trusted publishing (GitHub Actions OIDC, GA March 2024) introduced a keyless OIDC path.

MEP-68 supports only the trusted-publishing path. Long-lived NuGet API keys are rejected:

- **Supply-chain incident pattern.** Compromised long-lived tokens are the primary vector for package registry attacks. The xz-utils backdoor (March 2024), event-stream (2018), and a cascade of PyPI injection attacks in 2024-2025 trace to stolen or leaked long-lived credentials. Removing long-lived tokens from the trust boundary eliminates this attack class.

- **nuget.org GA precedence.** NuGet trusted publishing reached GA in March 2024, before PyPI's PEP 740 (late 2025) and before Cargo RFC #3724 (Q4 2025). MEP-68 shipping trusted publishing in 2026 is following an 18-month-old GA, not cutting edge.

- **Symmetry with MEP-57 and MEP-73.** MEP-57 mandates Sigstore-keyless for the Mochi central registry publish path. MEP-73 mandates Cargo RFC #3724 trusted publishing for crates.io. MEP-68 following the same pattern for nuget.org is the consistent choice.

The transition flag `--allow-token-fallback` exists for users on organisations whose nuget.org account has not yet configured trusted publishing. It emits a deprecation warning and is removed in MEP-68 v2.

## 6. Why a closed type-mapping table

The bridge translates CLR types to Mochi types via a fixed enumerated table. Items whose types fall outside the table are skipped with a `SkipReport`. The alternative would be an open table that synthesises a Mochi wrapper for every CLR type encountered.

The closed table wins because:

- **Predictable user surface.** A Mochi user can read the table and predict whether a given .NET method will translate. An open table would require reading the bridge's internal synthesis logic to predict outcomes.

- **Refusal is information.** The `SkipReport` entry names the item and the reason (e.g., `SkipPointerType`, `SkipUnconcretisedGeneric`). The user can then hand-write an `extern fn` override or skip the item. An open table would silently translate non-trivial types in unexpected ways.

- **Reified generics are combinatorially dangerous.** Unlike Java's type-erased generics, .NET generics are reified: `List<int>` and `List<string>` are distinct CLR types with distinct IL representations. An open table would auto-instantiate generic types for every type argument it encountered, which is unbounded. The explicit `[dotnet.monomorphise]` table bounds the explosion.

- **Auditability.** The closed table fits in a single source file (~250 LOC of Go). Changes are reviewable as a unit. An open synthesis routine would be order-of-magnitude larger.

The escape hatch is the `extern fn ... custom` override path: the user can always bypass the table by taking responsibility for the type at the CLR-to-Mochi boundary.

## Cross-references

- [[01-language-surface]] for the user-visible surface.
- [[03-prior-art-bridges]] for the comparison with pythonnet, CsWin32, and uniffi.
- [[04-assembly-metadata-ingest]] for the `mochi-dotnet-meta` CLI tool detail.
- [[05-type-mapping]] for the closed-table contents.
- [[07-nuget-trusted-publishing]] for the trusted-publishing flow detail.
- [[08-async-bridge]] for the `Task<T>` synchronous dispatch decision.
- [MEP-68](/docs/mep/mep-0068) for the normative spec.
