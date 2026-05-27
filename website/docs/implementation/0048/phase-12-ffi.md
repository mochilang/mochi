---
title: "Phase 12. .NET FFI and NuGet deps"
sidebar_position: 14
sidebar_label: "Phase 12. .NET FFI"
description: "MEP-48 Phase 12 — import dotnet/...; @nuget pragma; lockfile pinning; [LibraryImport] P/Invoke; BCL API gate; 25 fixtures."
---

# Phase 12. .NET FFI and NuGet deps

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 12](/docs/mep/mep-0048#phase-12-net-ffi-and-nuget-deps) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase12FFI`: 25 fixtures green. `TestPhase12BclFFI`: curated BCL-API gate (10 specific BCL calls round-trip correctly). `TestPhase12NuGetRoundtrip`: nightly gate verifies that a fixture with `@nuget("Polly", "8.5.0")` restores, compiles, and runs correctly.

## Goal-alignment audit

FFI is what makes the .NET target attractive beyond the Mochi standard library. A Mochi program that can call any NuGet package or any native library via P/Invoke is a program that has the full .NET ecosystem at its disposal. Phase 12 ships the import resolution, NuGet lockfile pinning, and the `[LibraryImport]` source-generator path for safe P/Invoke.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | `import "dotnet/System.Text.Json"` → `using System.Text.Json;` + method dispatch via Mochi-typed signature | NOT STARTED | — |
| 12.1 | `@nuget("Polly", "8.5.0")` pragma → `<PackageReference>` in generated `.csproj`; NuGet lockfile | NOT STARTED | — |
| 12.2 | SHA-256 lockfile: `mochi.lock.json` extending `packages.lock.json` with package SHA-256 | NOT STARTED | — |
| 12.3 | `[LibraryImport]` P/Invoke source generator for native library FFI | NOT STARTED | — |

## Sub-phase 12.0 -- BCL import

### Decisions made (12.0)

**`import "dotnet/System.Text.Json"`**: resolves to a `using System.Text.Json;` directive in the emitted C# file. Method calls on imported BCL types are lowered to direct C# method calls.

**Type mapping for FFI**: Mochi types passed to BCL methods are automatically converted at the call site. `string` → `string` (no conversion). `int` → `long` (BCL methods that accept `int` receive an implicit `(int)i` downcast; the lowerer prefers BCL overloads that accept `long` when available).

**BCL API gate** (`TestPhase12BclFFI`): 10 curated BCL calls verified to round-trip:
1. `System.Text.Json.JsonSerializer.Serialize<T>(value)` → JSON string
2. `System.Text.Json.JsonSerializer.Deserialize<T>(json)` → `Option<T>`
3. `System.IO.File.ReadAllText(path)` → `string`
4. `System.IO.File.WriteAllText(path, content)`
5. `System.Text.RegularExpressions.Regex.IsMatch(input, pattern)`
6. `System.DateTime.UtcNow` → `long` (Unix epoch ms)
7. `System.Environment.GetEnvironmentVariable(name)` → `Option<string>`
8. `System.Console.ReadLine()` → `Option<string>`
9. `System.Convert.ToBase64String(bytes)` → `string`
10. `System.Security.Cryptography.SHA256.HashData(bytes)` → `byte[]`

## Sub-phase 12.1 -- NuGet @nuget pragma

### Decisions made (12.1)

**`@nuget("Polly", "8.5.0")` pragma**: declared at the top of a Mochi source file; the build driver adds the corresponding `<PackageReference Include="Polly" Version="8.5.0" />` to the generated `.csproj`.

**`<RestorePackagesWithLockFile>true</RestorePackagesWithLockFile>`**: always on. `dotnet restore --locked-mode` on CI to prevent unintended upgrades.

**`<CentralPackageManagement>` (CPM)**: for multi-file projects, all `@nuget` pragmas are consolidated into a `Directory.Packages.props` file with `<PackageVersion Include="Polly" Version="8.5.0" />`. Individual `.csproj` files reference packages without version: `<PackageReference Include="Polly" />`.

## Sub-phase 12.2 -- SHA-256 lockfile

### Decisions made (12.2)

**`mochi.lock.json`**: extends NuGet's `packages.lock.json` with a SHA-256 hash for each resolved package `.nupkg`. Format:

```json
{
  "version": 1,
  "packages": {
    "Polly/8.5.0": {
      "sha256": "abc123...",
      "nuget_hash": "sha512:XYZ..."
    }
  }
}
```

The `nuget_hash` field is the `PackageHash` from the NuGet lock file (SHA-512); the `sha256` field is computed by the Mochi driver from the `.nupkg` content. On `mochi build`, the driver verifies both hashes before restoring.

**OSV audit**: `mochi nuget audit` fetches `https://api.osv.dev/v1/query` with each package name+version and reports known CVEs. Runs nightly in CI.

## Sub-phase 12.3 -- P/Invoke via [LibraryImport]

### Decisions made (12.3)

**`[LibraryImport]` source generator** (C# 11+, .NET 7+): NativeAOT-safe P/Invoke. Replaces `[DllImport]` (which uses runtime reflection). The Mochi FFI for native libraries emits:

```csharp
// Mochi: import "native/libssl" { fun sha256(data: bytes): bytes }
internal static partial class LibSsl
{
    [LibraryImport("libssl")]
    internal static partial unsafe void SHA256(
        byte* data, ulong len, byte* md);
}
```

The `LibraryImportGenerator` (part of `dotnet/runtime`) generates the marshalling glue at compile time (no runtime reflection), making the P/Invoke NativeAOT-clean.

**`unsafe` context**: P/Invoke with pointer args requires `<AllowUnsafeBlocks>true</AllowUnsafeBlocks>` in the `.csproj`. The driver adds this flag only when the program has a native import.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | `import "dotnet/..."` → `using` directive; method call resolution |
| `transpiler3/dotnet/build/csproj.go` | `@nuget` pragma → `<PackageReference>`; lockfile flags |
| `transpiler3/dotnet/build/nuget.go` | `mochi.lock.json` read/write/verify; `mochi nuget audit` |
| `transpiler3/dotnet/lower/decl.go` | Native `import` → `[LibraryImport]` partial class |
| `transpiler3/dotnet/build/phase12_test.go` | `TestPhase12FFI`, `TestPhase12BclFFI`, `TestPhase12NuGetRoundtrip` |
| `tests/transpiler3/dotnet/fixtures/phase12-ffi/` | 25 fixture directories |

## Test set

- `TestPhase12FFI` -- 25 fixtures: JSON serialize, JSON deserialize, File read/write, Regex match, DateTime now, env var, console readline, base64, SHA256, HTTP GET (mock), HTTP POST (mock), Polly retry (NuGet), Polly circuit breaker (NuGet), BCL cryptography, BCL compression (GZip), BCL collections (SortedDictionary), BCL threading (Mutex), BCL diagnostics (Stopwatch), BCL numerics (BigInteger), native libz crc32 (P/Invoke), COM object (Windows-only), unsafe pointer arithmetic, multiple NuGet packages, package lockfile verification, OSV audit clean.
- `TestPhase12BclFFI` -- 10 BCL-API fixtures from the curated list above.
- `TestPhase12NuGetRoundtrip` -- nightly: Polly + Serilog fixture restores, compiles, runs.

## Deferred work

- EF Core integration (`IQueryable<T>` against a database). Deferred to Phase 3 sub-MEP.
- ASP.NET Core as a host. Deferred to Phase 3 sub-MEP.
- COM Interop beyond basic `[ComImport]`. Deferred pending Windows CI maturity.

## Closeout notes

Phase 12 not yet started.
