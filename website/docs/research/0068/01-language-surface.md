---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import dotnet \"<package>@<semver>\" as <alias>` import form, the `[dotnet-dependencies]` / `[dotnet]` / `[dotnet.publish]` / `[dotnet.capabilities]` manifest tables, the CLI subcommands, and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-68 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything here is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "dotnet"
```

MEP-68 adds `dotnet` as the fifth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<package-id>` | Bare name. Resolves through `[dotnet-dependencies]` plus `mochi.lock`. The lockfile records the picked version. |
| `<package-id>@<semver-req>` | Explicit NuGet version constraint (`^13.0`, `~3.1`, `>=9.0.0`, `[13.0.3]`, `(,14.0)`). |
| `<package-id>@git+<url>#<commit>` | Git source pinned to commit hash or tag. |
| `<package-id>@path+<rel-path>` | Path source relative to `mochi.toml`. |

NuGet package identifiers are case-insensitive on the registry side but the bridge normalises them to the canonical casing returned by the NuGet V3 registration endpoint (e.g., `newtonsoft.json` → `Newtonsoft.Json`).

Example surface:

```mochi
import dotnet "Newtonsoft.Json@^13.0" as Json
import dotnet "Polly" as Polly
import dotnet "Dapper" as Dapper
import dotnet "Serilog" as Log

fn setup_logger(): Log.ILogger {
    return Log.LoggerConfiguration
        .WriteTo.Console()
        .CreateLogger()
}

fn fetch_users(conn: DbConnection): list<User> {
    return Dapper.Query(conn, "SELECT id, name FROM users")
}

fn with_retry(fn work: () -> string): string {
    let policy = Polly.Policy
        .Handle(HttpRequestException)
        .WaitAndRetry(3, fn(i) { TimeSpan.FromSeconds(i) })
    return policy.Execute(work)
}
```

### Alias and namespace binding

The `<alias>` introduces a Mochi namespace. `<alias>.<Item>` resolves to the synthesised `extern fn` or `extern type` declaration the bridge generated for `<PackageId>.<Namespace>.<Item>`. For packages with a single root namespace matching the package name (the typical case: `Newtonsoft.Json` → `Newtonsoft.Json.*`), the bridge flattens the namespace one level, so `Json.JsonConvert.SerializeObject(...)` works with `import dotnet "Newtonsoft.Json" as Json`.

For packages with multiple top-level namespaces (e.g., `Microsoft.EntityFrameworkCore` which exposes `Microsoft.EntityFrameworkCore.*`, `Microsoft.EntityFrameworkCore.Infrastructure.*`, and `Microsoft.EntityFrameworkCore.Storage.*`), the alias binds to all namespaces; collisions (same type name in two namespaces) are resolved by preferring the shorter namespace depth, with a warning.

Item names follow C# PascalCase convention verbatim. The bridge does not rename or convert to snake_case. Mochi users working with .NET packages are expected to use PascalCase for .NET-origin names; this is consistent with how `import go "strings"` exposes `strings.Contains` (not `strings.contains`).

### The `auto` modifier

`import dotnet "Serilog" as Log auto` binds every public top-level item of the `Serilog` namespace at file scope (not namespaced under `Log`). This is an opt-in escape hatch for packages with very flat namespaces. The default is namespaced; `auto` should be used sparingly to avoid shadowing Mochi built-ins.

## Manifest: `[dotnet-dependencies]`

```toml
[dotnet-dependencies]
"Newtonsoft.Json" = "^13.0"
"Polly" = { version = "^8.3", include-assets = "all" }
"Dapper" = "^2.1"
"Microsoft.EntityFrameworkCore" = { version = "^9.0", assets = ["compile", "runtime"] }
"Serilog.Sinks.Console" = "^5.0"
"MyLocalLib" = { path = "../MyLib/MyLib.csproj" }
"MyGitLib" = { git = "https://github.com/example/mylib", tag = "v0.2.0" }
```

The grammar mirrors NuGet's `<PackageReference>` grammar:

- A bare string is shorthand for `{ version = "..." }`.
- The table form admits: `version` (NuGet version range), `include-assets` (`"all"`, `"compile"`, `"runtime"`, `"contentFiles"`, `"build"`, `"native"`, `"analyzers"`, `"none"`), `assets` (array of asset kinds), `exclude-assets`, `path` (local project reference), `git` + `tag`/`branch`/`commit` (git source), `package` (rename the package id as resolved), and `prerelease = true` (allow prerelease versions).
- Cyclic dependencies are rejected at lock time.

The user does not write a separate `.csproj`. The bridge synthesises the wrapper project's `.csproj` from this table, populating `<PackageReference>` entries with the exact resolved versions from `mochi.lock`.

## Manifest: `[dotnet]`

```toml
[dotnet]
framework = "net9.0"
rust-version = "1.85"
runtime-identifiers = ["linux-x64", "linux-arm64", "osx-arm64", "win-x64"]
nullable = true
implicit-usings = true
monomorphise = [
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject", T = "MyRecord" },
    { item = "System.Collections.Generic.List", T = "string" },
]
```

Fields:

- `framework`: the target framework moniker (TFM). Default `"net9.0"`. `"net8.0"` is the minimum NativeAOT-capable TFM.
- `runtime-identifiers`: list of .NET RIDs for NativeAOT cross-compilation. Default: the four primary MEP-53 targets.
- `nullable`: enables `<Nullable>enable</Nullable>` in the wrapper project. Default `true`.
- `implicit-usings`: enables `<ImplicitUsings>enable</ImplicitUsings>` (adds `using System;`, `using System.Collections.Generic;`, etc. globally). Default `true`.
- `monomorphise`: explicit generic instantiations. Each entry `{ item = "<fully-qualified-name>", T = "<Mochi-type>" }` causes the bridge to emit a wrapper for the specified generic instantiation. Without an entry, generics with unresolved type parameters are skipped with `SkipReport`.

## Manifest: `[dotnet.publish]`

```toml
[dotnet.publish]
nativeaot = true
native-lib = "Static"
cbindgen-header = false
strip = true
invariant-globalization = true
```

Fields for Mochi-as-NuGet-library:

- `nativeaot`: whether to emit `[UnmanagedCallersOnly]` entry points and publish with `/p:PublishAot=true`. Default `false`. When `true`, the emitted library is callable from non-.NET code.
- `native-lib`: `<NativeLib>` MSBuild property. `"Static"` (`.a` / `.lib`), `"Shared"` (`.so` / `.dylib` / `.dll`). Default `"Static"`.
- `cbindgen-header`: whether to emit a C header for non-.NET/non-Mochi callers. Default `false`.
- `strip`: whether to strip debug symbols from the NativeAOT output (`/p:StripSymbols=true`). Default `true` (release builds).
- `invariant-globalization`: enables `<InvariantGlobalization>true</InvariantGlobalization>`, which disables culture-sensitive string operations and reduces binary size by ~1MB. Default `true`.

## Manifest: `[dotnet.capabilities]`

```toml
[dotnet.capabilities]
net = true
fs = false
proc = false
unsafe = false
coreclr-hosting = false
```

The capability flags refine MEP-57's `[capabilities]` table. A flag set to `true` here that contradicts the parent `[capabilities]` declaration is a manifest validation error (you cannot claim `net = false` globally but `[dotnet.capabilities] net = true`).

- `net`: package dep graph opens network connections. Must be `true` if the graph includes `System.Net.Http`, `HttpClient`, `StackExchange.Redis`, `Npgsql`, any `Azure.*` or `AWSSDK.*` package, etc.
- `fs`: reads or writes files via `System.IO`.
- `proc`: spawns processes via `System.Diagnostics.Process`.
- `unsafe`: user has hand-overridden an out-of-table item via `extern fn ... from dotnet "..."`.
- `coreclr-hosting`: uses the CoreCLR hosting fallback (phase 13) for NativeAOT-incompatible packages. Default `false`.

## CLI subcommands

### `mochi pkg add dotnet <PackageId>[@<constraint>]`

Adds an entry to `[dotnet-dependencies]` and immediately runs `mochi pkg lock` to resolve and pin the version.

```sh
mochi pkg add dotnet "Newtonsoft.Json"
mochi pkg add dotnet "Polly@^8.3"
mochi pkg add dotnet "Dapper@^2.1"
```

### `mochi pkg lock`

Extended to walk `[dotnet-dependencies]`, query the NuGet V3 registration endpoint, resolve the transitive dep tree, download `.nupkg` archives into the content-addressed cache, parse ECMA-335 metadata, synthesise the NativeAOT wrapper projects, and write `[[dotnet-package]]` entries to `mochi.lock`.

```sh
mochi pkg lock                     # resolve + synthesise
mochi pkg lock --check             # verify hashes, no network
mochi pkg lock --rust-nightly=...  # (MEP-73 only) irrelevant here
```

### `mochi pkg publish --to=nuget.org [--dry-run]`

Emits `TargetDotnetLibrary`, runs `dotnet pack`, obtains a CI OIDC token, exchanges it with nuget.org's trusted-publishing endpoint, uploads the `.nupkg`.

```sh
mochi pkg publish --to=nuget.org            # upload to nuget.org
mochi pkg publish --to=nuget.org --dry-run  # pack + sign, skip upload
mochi pkg publish --to=nuget.org --allow-apikey-fallback  # transition escape hatch
```

### `mochi pkg sync dotnet`

Regenerates the NativeAOT wrapper projects from scratch from the current `mochi.lock` (does not re-resolve versions or re-download archives). Used after manual edits to the synthesised shim file.

## Mochi-as-NuGet: the emitted project structure

When `TargetDotnetLibrary` runs (direction 2), the driver emits:

```
<outdir>/
  MyPackage.csproj
  global.json
  README.md
  MyPackage.nuspec
  Src/
    MyPackage.cs         # public API bindings
    MyPackage.Types.cs   # record / enum / sealed-class hierarchy types
  docs/
    MyPackage.xml        # XML documentation
  assets/
    icon.png             # from [package] icon field
```

The `.csproj` contains:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFramework>net9.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <Version>$(VERSION)</Version>
    <AssemblyName>MyPackage</AssemblyName>
    <RootNamespace>MyPackage</RootNamespace>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <GeneratePackageOnBuild>false</GeneratePackageOnBuild>
    <!-- NativeAOT fields emitted only when [dotnet.publish] nativeaot = true -->
    <PublishAot>true</PublishAot>
    <NativeLib>Static</NativeLib>
    <StripSymbols>true</StripSymbols>
    <InvariantGlobalization>true</InvariantGlobalization>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="mochi-dotnet-runtime" Version="0.1.*" />
    <!-- [dotnet-dependencies] entries from mochi.toml -->
  </ItemGroup>
</Project>
```

`mochi-dotnet-runtime` is a small NuGet package (shipped alongside mochi) containing the `GCHandleTable`, `MochiString`, `MochiList<T>`, and `MochiResult<T>` helper types that the NativeAOT wrapper and the emitted library both depend on.

## End-to-end example

The following `mochi.toml` declares a dependency on `Dapper` for database queries:

```toml
[package]
name = "user-service"
version = "1.0.0"
description = "A user service backed by PostgreSQL via Dapper"

[dotnet-dependencies]
Dapper = "^2.1"
Npgsql = "^8.0"

[dotnet]
framework = "net9.0"
runtime-identifiers = ["linux-x64", "linux-arm64"]

[dotnet.capabilities]
net = true
```

The corresponding Mochi source:

```mochi
import dotnet "Dapper" as Dapper
import dotnet "Npgsql" as Npgsql

record User { id: int, name: string, email: string }

fn get_user(conn_str: string, id: int): User|nil {
    let conn = Npgsql.NpgsqlConnection(conn_str)
    conn.Open()
    let results = Dapper.Query(conn, "SELECT * FROM users WHERE id=@Id", {Id: id})
    conn.Close()
    return results.FirstOrDefault()
}
```

After `mochi pkg lock`, `mochi.lock` contains:

```toml
[[dotnet-package]]
id = "Dapper"
version = "2.1.35"
source = { kind = "registry", registry = "https://api.nuget.org/v3/index.json" }
nupkg-blake3 = "..."
nupkg-sha512 = "..."
metadata-sha256 = "..."
wrapper-sha256 = "..."
capabilities-declared = []

[[dotnet-package]]
id = "Npgsql"
version = "8.0.5"
source = { kind = "registry", registry = "https://api.nuget.org/v3/index.json" }
nupkg-blake3 = "..."
nupkg-sha512 = "..."
metadata-sha256 = "..."
wrapper-sha256 = "..."
capabilities-declared = ["net"]
dependencies = ["Microsoft.Extensions.Logging.Abstractions@>=8.0.0"]
```

`mochi build` invokes `Driver.Build`, which runs `dotnet publish` for the two wrapper projects, produces `libDapper_dotnet_wrap.a` and `libNpgsql_dotnet_wrap.a`, and links them into the final binary. The Mochi source imports the synthesised shim that the extern emitter produced, providing the `Dapper.Query` and `Npgsql.NpgsqlConnection` bindings.
