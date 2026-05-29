---
title: "11. NativeAOT subset and CoreCLR fallback"
sidebar_position: 12
sidebar_label: "11. NativeAOT subset"
description: "NativeAOT compatibility surface, RequiresDynamicCodeAttribute and ILLink pre-check, compatibility rate across the 25-package corpus, CoreCLR hosting API (coreclr_initialize, coreclr_create_delegate), per-process CoreCLR instance, version-skew risk, and the phase-13 delivery plan."
---

# 11. NativeAOT subset and CoreCLR fallback

NativeAOT compiles all reachable managed code to native machine code ahead of time. This means any code that generates new managed code at runtime is incompatible. This note documents the compatibility surface, the pre-check mechanism, and the CoreCLR hosting fallback for incompatible packages.

## What NativeAOT supports

NativeAOT supports the vast majority of .NET BCL and NuGet packages. Supported features include:

- All value types (structs, enums, Span<T>)
- All reference types (classes, records, interfaces, delegates)
- Generics (instantiated at publish time based on reachability analysis; the linker trims unreachable generic instantiations)
- Reflection for types known at compile time (via `[DynamicallyAccessedMembers]` annotations)
- `System.Text.Json` source-generated JSON (using `JsonSerializerContext`)
- `System.Runtime.InteropServices` P/Invoke (calling native libraries from .NET)
- `[UnmanagedCallersOnly]` (native calling .NET, as used by the bridge)
- Most of `Microsoft.Extensions.*` (DI, logging, configuration, options)
- Entity Framework Core (with source-generated contexts via `partial class`)
- Dapper (with explicit column mapping, not `dynamic`)
- Serilog (with static sinks configured at startup)
- Polly (all policy types)
- RestSharp (via HttpClient; no dynamic deserialization without `[DynamicallyAccessedMembers]`)
- Newtonsoft.Json (with `[JsonObject]` / `[JsonProperty]` annotations; runtime-dynamic deserialization requires `[DynamicallyAccessedMembers]` opt-in or source generation)

## What NativeAOT does NOT support

| Feature | Why incompatible | Detection mechanism |
|---------|-----------------|---------------------|
| `Assembly.Load(string)` / `Assembly.LoadFile()` | Loads new managed assemblies at runtime; linker cannot know what types will be needed | `RequiresUnreferencedCodeAttribute` + `RequiresDynamicCodeAttribute` on these methods |
| `Activator.CreateInstance(Type)` with unknown type | Dynamic instantiation of types not known at publish time | `RequiresDynamicCodeAttribute` on `Activator.CreateInstance` |
| `Type.GetMethod()` / `Type.GetProperty()` without `[DynamicallyAccessedMembers]` | Reflective access to members not annotated for preservation | Trimming warnings in `dotnet publish` output |
| `DynamicMethod` / `MethodBuilder` / `TypeBuilder` (Emit) | Runtime IL emission; not possible in AOT | These types throw `PlatformNotSupportedException` in NativeAOT |
| `Expression.Compile()` | Compiles LINQ expression trees to delegates at runtime | Throws `PlatformNotSupportedException` in NativeAOT |
| MEF (Managed Extensibility Framework) | Uses `Assembly.Load` + `Activator.CreateInstance` | `RequiresDynamicCodeAttribute` |
| WPF / WinForms / MAUI (desktop UI) | XAML loaded at runtime; designer infrastructure uses reflection | `RequiresDynamicCodeAttribute` on XAML parser |
| ASP.NET Core MVC action routing via attributes | Uses reflection for controller discovery | Requires source-generated route registration |
| Dapper with `dynamic` return type | `dynamic` deserialization uses `DynamicMethod.Emit` | `RequiresDynamicCodeAttribute` on Dapper's dynamic mapper |

## The pre-check: RequiresDynamicCodeAttribute and ILLink

The bridge runs a pre-check at `mochi pkg lock` time to classify each NuGet package:

1. **Static classification**: the ECMA-335 reader checks for `RequiresDynamicCodeAttribute` on the package's public methods. Any method annotated with this attribute is classified as NativeAOT-incompatible (it will throw at runtime). If the package's primary entry points are incompatible, the whole package is classified as fallback-required.

2. **ILLink TrimAnalysis**: when `dotnet publish /p:PublishAot=true /p:TreatTrimWarningsAsErrors=true` runs, the ILLink linker emits trimming warnings for any code path that accesses types or members not preserved by `[DynamicallyAccessedMembers]` or a custom `ILLink.Descriptors.xml`. Packages with trimming warnings are classified as "requires NativeAOT-with-workaround" (a category between fully compatible and fallback-required).

3. **Test AOT compilation**: as part of phase 4's fixture corpus gate, the bridge actually runs `dotnet publish /p:PublishAot=true` for each of the 25 packages and records which ones succeed and which fail. Packages that fail are documented in the SkipReport and routed to phase 13.

### 25-package fixture corpus NativeAOT classification

Based on the May 2026 versions of the 25 fixture packages:

| Package | NativeAOT compatible? | Notes |
|---------|-----------------------|-------|
| Newtonsoft.Json 13.0.3 | Partial | Dynamic deserialization requires `[DynamicallyAccessedMembers]`; source-gen mode is fully compatible |
| Microsoft.Extensions.DependencyInjection 9.0 | Yes | Fully compatible |
| Microsoft.Extensions.Logging 9.0 | Yes | Fully compatible |
| Microsoft.Extensions.Configuration 9.0 | Yes | Fully compatible |
| Serilog 3.1 | Yes | Fully compatible |
| AutoMapper 13.0 | No | Uses `Expression.Compile()` for mapping; fallback required |
| FluentValidation 11.10 | Partial | Rule registration via lambdas is compatible; `Must(validator)` with `Activator.CreateInstance` requires opt-in |
| MediatR 12.3 | No | Pipeline behavior registration uses `Assembly.GetTypes()` for discovery; fallback required |
| Polly 8.3 | Yes | Fully compatible |
| RestSharp 111.4 | Yes | Fully compatible |
| Dapper 2.1 | Partial | Typed queries are compatible; `dynamic` queries require fallback |
| StackExchange.Redis 2.8 | Yes | Fully compatible |
| Grpc.Core 2.67 | Yes | Fully compatible |
| Microsoft.EntityFrameworkCore 9.0 | Partial | Requires source-generated `DbContext`; runtime model building uses reflection |
| Microsoft.Data.SqlClient 6.0 | Yes | Fully compatible |
| Npgsql 8.0 | Yes | Fully compatible |
| MongoDB.Driver 3.1 | Partial | BSON serialization requires `[BsonSerializer]` annotations for AOT |
| RabbitMQ.Client 7.0 | Yes | Fully compatible |
| NATS.Client.Core 2.3 | Yes | Fully compatible |
| CsvHelper 33.0 | Partial | Dynamic member mapping requires fallback; `[CsvHelper]` attribute mapping is compatible |
| YamlDotNet 16.0 | Partial | Reflection-based deserialization requires fallback; `[YamlMember]` attribute mode is compatible |
| Markdig 0.38 | Yes | Fully compatible |
| BCrypt.Net-Next 4.0 | Yes | Fully compatible |
| SixLabors.ImageSharp 3.1 | Yes | Fully compatible |
| Humanizer 2.14 | No | Heavy use of `Assembly.Load` for culture resources; fallback required |

Summary: 11 fully compatible, 9 partially compatible (compatible with workaround/annotation), 3 fallback required (AutoMapper, MediatR, Humanizer).

For partially-compatible packages, the bridge emits a `SkipReport` for the incompatible methods and wraps only the compatible subset with NativeAOT.

## CoreCLR hosting fallback (phase 13)

For packages that require a JIT runtime, the bridge uses the CoreCLR hosting API:

```c
// CoreCLR hosting entry points from libcoreclr.so / coreclr.dll
typedef int (*coreclr_initialize_fn)(
    const char* exe_path, const char* app_domain_friendly_name,
    int property_count, const char** property_keys, const char** property_values,
    void** host_handle, unsigned int* domain_id);

typedef int (*coreclr_create_delegate_fn)(
    void* host_handle, unsigned int domain_id,
    const char* entry_point_assembly_name,
    const char* entry_point_type_name,
    const char* entry_point_method_name,
    void** delegate);
```

The bridge:

1. At first call to a CoreCLR-hosted symbol: `dlopen("libcoreclr.so")` (Linux), `dlopen("libcoreclr.dylib")` (macOS), `LoadLibrary("coreclr.dll")` (Windows). The path is resolved from the .NET installation directory recorded in `mochi.lock`.

2. Calls `coreclr_initialize` with the app domain properties (TPA list, app base, native DLL search path). This initialises the CLR in the current process; the CLR then loads its own GC, JIT, and thread pool.

3. Calls `coreclr_create_delegate` for each method the bridge needs to call. The delegate is a C function pointer that, when called, invokes the corresponding managed method via the JIT.

4. Caches the delegate function pointers for subsequent calls.

The startup cost is 100-300ms for `coreclr_initialize`. Subsequent calls use the cached delegates at near-JIT-call speed (~50 ns overhead per call, vs. ~0 for NativeAOT).

### Phase 13 opt-in

The CoreCLR fallback is opt-in per package:

```toml
[dotnet-dependencies]
AutoMapper = { version = "^13.0", nativeaot = false }

[dotnet.capabilities]
coreclr-hosting = true
```

The `nativeaot = false` flag tells the bridge to use CoreCLR hosting for `AutoMapper`. A `SkipReport` entry is emitted for each method that the NativeAOT wrapper would have covered but now goes through CoreCLR.

### Version skew mitigation

The CoreCLR version is recorded in `mochi.lock`:

```toml
[[dotnet-package]]
id = "AutoMapper"
coreclr-version = "9.0.5"
coreclr-path = "/usr/lib/dotnet/shared/Microsoft.NETCore.App/9.0.5/libcoreclr.so"
```

At `mochi pkg lock --check` time, the bridge verifies that the recorded `coreclr-path` exists and has the matching `FileVersion` (read from the PE header). If the .NET SDK was updated (e.g., from 9.0.5 to 9.0.6), the check fails with: `"coreclr version mismatch: expected 9.0.5, found 9.0.6; run mochi pkg lock to update"`.

The Mochi binary distribution for CoreCLR-fallback-enabled builds bundles the `libcoreclr.so` version recorded at lock time (the `--bundle-coreclr` flag for `mochi build`). Without bundling, the target machine must have the exact .NET version installed.
