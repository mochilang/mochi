---
title: "11. NativeAOT and trimming"
sidebar_position: 12
sidebar_label: "11. NativeAOT"
description: "The NativeAOT compile path (PublishAot=true), the IL trimmer and AOT-compatibility metadata (IsAotCompatible NuGet property), which of the 20-package fixture corpus packages pass and fail AOT trimming (April 2026 analysis), the CLR startup cost trade-off, and the opt-in activation via [dotnet] bridge = nativeaot."
---

# 11. NativeAOT and trimming

This note covers the NativeAOT opt-in path for the C# shim assembly. NativeAOT compiles the shim to a native shared library with no CLR runtime dependency, eliminating the CLR startup overhead at the cost of requiring the dep graph to be AOT-compatible.

## What NativeAOT provides

NativeAOT (`dotnet publish -r <rid> -p:PublishAot=true`) is a .NET publishing mode that:

1. Runs the IL trimmer to remove all unreachable code (dead code elimination).
2. Compiles the surviving IL to native machine code via LLVM (on Linux/macOS) or MSVC (on Windows).
3. Produces a self-contained native shared library: `libshim.so` (Linux), `libshim.dylib` (macOS), `shim.dll` (Windows).
4. Does not require a CLR or .NET SDK installation on the target machine.

For the bridge, NativeAOT produces a shim that is loadable via `dlopen` / `LoadLibrary` without any CLR initialisation step. The `[UnmanagedCallersOnly]` entry points are standard native exports in the shared library.

## AOT-compatibility constraints

NativeAOT imposes constraints that break some .NET programming patterns:

- **No reflection over dynamically loaded types.** `Type.GetType("Fully.Qualified.Name")` fails at AOT time if the type is not statically reachable. Many configuration and dependency injection frameworks use runtime reflection.
- **No `System.Reflection.Emit`.** Dynamic IL generation (used by some ORM and serialisation libraries to generate fast accessor code) is not supported.
- **No runtime code generation.** Expression trees compiled via `Expression.Compile()` are not supported by default; the trimmer removes the compile path.
- **No `dynamic` keyword.** The `dynamic` CLR type uses `System.Dynamic.ExpandoObject` and reflection-based dispatch.
- **Limited `Assembly.Load`.** Loading assemblies at runtime from arbitrary paths is not supported.

The .NET ecosystem has been adopting AOT-compatible patterns since .NET 7, but progress varies by package.

## AOT compatibility analysis of the 20-package corpus

Analysis from the April 2026 fixture corpus (tested with `dotnet publish -r linux-x64 -p:PublishAot=true -p:TrimmerRootDescriptor=AllPublicTypes.xml`):

| Package | Version | AOT-compatible | Issues |
|---------|---------|----------------|--------|
| System.Text.Json | 8.0.0 | Yes | The System.Text.Json source generator path is AOT-compatible; the reflection path is not. |
| Serilog | 3.1.1 | Partial | Core is AOT-compatible; most sinks use reflection for configuration. |
| Polly | 8.3.0 | Yes | Pure functional policies; no reflection. |
| FluentValidation | 11.9.0 | Partial | Property access via expressions; requires `[DynamicDependency]` annotations. |
| NUnit | 4.1.0 | No | Test discovery uses `Assembly.GetTypes()` and `Activator.CreateInstance()`. |
| xUnit | 2.7.0 | No | Same as NUnit. |
| Bogus | 35.3.0 | Partial | Most generators work; locale loading uses `Assembly.GetManifestResourceStream`. |
| Newtonsoft.Json | 13.0.3 | No | Extensive use of `System.Reflection.Emit` for fast accessor generation. |
| Dapper | 2.1.28 | No | Generates IL for mapping SQL rows to objects via `ILGenerator`. |
| AutoMapper | 13.0.1 | No | Uses `Expression.Compile()` for mapping functions. |
| MediatR | 12.2.0 | Partial | Core pipeline is AOT-compatible; handlers discovered via reflection. |
| RestSharp | 110.2.0 | Partial | HTTP client core is AOT-compatible; serialisation uses reflection by default. |
| FluentAssertions | 6.12.0 | No | Extensive reflection for assertion message generation. |
| Moq | 4.20.70 | No | Mock generation uses `System.Reflection.Emit`. |
| Microsoft.Extensions.DependencyInjection | 8.0.0 | Yes | The source-generator path is AOT-compatible. |
| Microsoft.Extensions.Http | 8.0.0 | Yes | Wraps `HttpClient`; no reflection. |
| StackExchange.Redis | 2.7.23 | Partial | Core is AOT-compatible; some serialisation helpers use reflection. |
| Npgsql | 8.0.3 | Yes | PostgreSQL provider has AOT-compatible mode since 8.0. |
| EntityFramework Core | 8.0.3 | Partial | Compiled queries are AOT-compatible; dynamic queries use `ILGenerator`. |
| AWSSDK.Core | 3.7.200 | No | Service client generation uses runtime `Type.GetType` and `Activator.CreateInstance`. |

Summary: 4 fully AOT-compatible, 8 partially compatible, 8 not AOT-compatible. The partially-compatible packages require package-specific `[DynamicDependency]` annotations in the shim or use of the AOT-compatible API surface only.

## The `IsAotCompatible` NuGet metadata property

NuGet packages can declare AOT compatibility via the `<IsAotCompatible>true</IsAotCompatible>` property in their `.csproj`:

```xml
<PropertyGroup>
    <IsAotCompatible>true</IsAotCompatible>
</PropertyGroup>
```

This sets the `build_metadata.PackageReference.IsAotCompatible` NuGet item metadata that consumers can check. The bridge reads this property from the package's `.nuspec` / `.props` files and uses it as an advisory hint.

The bridge's lock-time AOT check:

```
$ mochi pkg lock --bridge=nativeaot
[1/3] Resolving packages ...
[2/3] Checking AOT compatibility:
  System.Text.Json 8.0.0: IsAotCompatible=true (advisory)
  Newtonsoft.Json 13.0.3: IsAotCompatible=false
  Dapper 2.1.28: IsAotCompatible=false (no property declared, heuristic: ILGenerator usage detected)
[3/3] WARNING: 2 packages are not AOT-compatible.
  Consider switching to AOT-compatible alternatives:
    - Newtonsoft.Json: use System.Text.Json source generators
    - Dapper: use Dapper.AOT (AOT-compatible fork)
  Or switch to CLR hosting: [dotnet] bridge = "clr-hosting"
  Proceeding with partial AOT (AOT-incompatible types will be skipped).
```

## Trimming roots and `[DynamicDependency]`

The IL trimmer requires explicit roots (types/methods that must be kept) for types that are only referenced via runtime strings (reflection). The bridge generates a trimmer roots descriptor for each `[UnmanagedCallersOnly]` entry:

```xml
<!-- dotnet_shim/Serilog/TrimmerRoots.xml -->
<linker>
  <assembly fullname="SerilogShim">
    <type fullname="SerilogShim.SerilogShimEntry" preserve="All" />
  </assembly>
</linker>
```

For types accessed via `[DynamicDependency]`, the shim adds the attribute:

```csharp
[DynamicDependency(DynamicallyAccessedMemberTypes.All, typeof(Serilog.Core.Logger))]
public static class SerilogShimEntry { ... }
```

This preserves the trimmer root for `Logger` even though it is accessed via a string at runtime.

## CLR startup cost trade-off

The CLR hosting API (`hostfxr_initialize_for_runtime_config`) incurs a startup cost:

| Scenario | Startup cost |
|----------|-------------|
| CLR hosting, cold start (no .NET runtime pre-loaded) | 150-300 ms |
| CLR hosting, warm start (runtime already in memory) | 20-50 ms |
| NativeAOT, cold start | 1-5 ms (library loading via dlopen) |
| NativeAOT, warm start | <1 ms |

For long-running processes (web servers, daemons), the CLR startup cost is paid once at process startup and amortised across the lifetime. For short-lived CLI tools that make one or two .NET calls, the CLR startup cost (150-300 ms) is noticeable.

For short-lived tools where startup time matters and the dep graph is AOT-compatible, `[dotnet] bridge = "nativeaot"` eliminates the CLR overhead.

## Activation

The NativeAOT path is activated via `mochi.toml`:

```toml
[dotnet]
bridge = "nativeaot"
runtime = "linux-x64"   # required for NativeAOT (must specify target RID)
```

The lock step checks AOT compatibility and warns or errors as configured by `[dotnet.nativeaot] compat-mode`:

- `"warn"` (default): warn on incompatible packages, proceed.
- `"error"`: fail lock on any incompatible package.
- `"skip"`: silently skip incompatible packages; they receive no shim.

The build step compiles the shim via `dotnet publish -r <runtime> -p:PublishAot=true -p:StripSymbols=true`.

## Cross-references

- [[02-design-philosophy]] §4 for why CLR hosting is the default.
- [[09-abi-stability]] §5 for the NativeAOT vs CLR hosting ABI difference.
- [[05-type-mapping]] for which types are available in the NativeAOT shim.
- [MEP-68 §Risks](/docs/mep/mep-0068#risks) for the NativeAOT compatibility detection risk.
- [.NET NativeAOT documentation](https://learn.microsoft.com/dotnet/core/deploying/native-aot/) for the upstream AOT publishing model.
