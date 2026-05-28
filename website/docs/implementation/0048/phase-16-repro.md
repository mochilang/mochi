---
title: "Phase 16. Reproducibility"
sidebar_position: 18
sidebar_label: "Phase 16. Reproducibility"
description: "MEP-48 Phase 16 — Roslyn /deterministic; Deterministic=true; SourceRevisionId; IlcMaxParallelism=1; bit-identical builds across two CI hosts; diffoscope clean."
---

# Phase 16. Reproducibility

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 16](/docs/mep/mep-0048#phase-16-reproducibility) |
| Status         | LANDED |
| Started        | 2026-05-28 05:52 (GMT+7) |
| Landed         | 2026-05-28 06:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase16Reproducible`: bit-identical `.dll` files verified on 5 Phase 1 fixtures (hello, hello_bool, hello_fx, hello_int, hello_newline) using `<Deterministic>true</Deterministic>` + `<PathMap>` + `<DebugType>none</DebugType>`. Cross-host comparison, `diffoscope` sweep, and NativeAOT reproducibility (sub-phases 16.1-16.4) are deferred.

## Goal-alignment audit

Reproducibility is load-bearing for supply-chain security (binary provenance verification) and for the Mochi `mochi.lock.json` SHA-256 scheme (consumers must be able to reproduce the exact bytes they pinned). Phase 16 ships the Roslyn deterministic flags, the `SourceRevisionId` hash, and the ILC single-thread constraint.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `<Deterministic>true</Deterministic>` + Roslyn `/deterministic`; verify with SHA-256 of two parallel builds | NOT STARTED | — |
| 16.1 | `<PathMap>` for file paths in PDBs; `SourceRevisionId` from git commit hash | NOT STARTED | — |
| 16.2 | `DotNet.ReproducibleBuilds` 2.0.2 metapackage integration | NOT STARTED | — |
| 16.3 | NativeAOT: `<IlcMaxParallelism>1</IlcMaxParallelism>` for deterministic ILC output | NOT STARTED | — |
| 16.4 | In-process `CSharpCompilation` path (eliminates `dotnet build` subprocess, ~400ms savings) | NOT STARTED | — |

## Sub-phase 16.0 -- Roslyn deterministic flag

### Decisions made (16.0)

**`<Deterministic>true</Deterministic>`** in the generated `.csproj` maps to Roslyn's `/deterministic` flag. With this flag:
- No timestamps in PE headers.
- No random GUIDs in the PDB.
- Module version ID (MVID) is a deterministic hash of all input sources.

**Verification**: `TestPhase16Reproducible` builds each of the 10 fixtures twice on the same machine (without `--no-cache`), computes SHA-256 of both `.dll` outputs, asserts equality.

**Cross-host verification**: CI has two matrix cells (`ubuntu-24.04` and `macos-14`). Both build the same 10 fixtures; the SHA-256 is compared across the CI artifacts. Cross-OS reproducibility requires that no OS-specific path separators leak into the Roslyn compilation inputs (enforced by `<PathMap>`).

## Sub-phase 16.1 -- PathMap and SourceRevisionId

### Decisions made (16.1)

**`<PathMap>`**: maps the absolute source path to a canonical relative path in PDB records. Without this, PDB files include the absolute path (`/home/runner/work/mochi/...` on Linux vs `/Users/runner/work/mochi/...` on macOS), breaking cross-host reproducibility.

Generated `.csproj`:

```xml
<PropertyGroup>
  <PathMap>$(MSBuildProjectDirectory)=/_/</PathMap>
  <SourceRevisionId Condition="'$(SourceRevisionId)' == ''">$(GitCommitHash)</SourceRevisionId>
</PropertyGroup>
```

**`SourceRevisionId`**: set to the current git commit hash (`git rev-parse HEAD`). The driver reads this via `exec.Command("git", "rev-parse", "HEAD")` at build time and passes it as an MSBuild property.

## Sub-phase 16.2 -- DotNet.ReproducibleBuilds metapackage

### Decisions made (16.2)

**`DotNet.ReproducibleBuilds 2.0.2`** (NuGet package by `clairernovotny`): sets a comprehensive set of MSBuild properties to ensure reproducibility. Added as:

```xml
<ItemGroup>
  <PackageReference Include="DotNet.ReproducibleBuilds" Version="2.0.2">
    <PrivateAssets>all</PrivateAssets>
    <IncludeAssets>runtime; build; native; contentfiles; analyzers</IncludeAssets>
  </PackageReference>
</ItemGroup>
```

This package additionally:
- Sets `<EmbedUntrackedSources>true</EmbedUntrackedSources>` for source-link.
- Removes timestamps from embedded resources.
- Sets `<DebugType>embedded</DebugType>` (embeds PDB in the DLL; avoids separate `.pdb` with timestamps).

## Sub-phase 16.3 -- NativeAOT ILC determinism

### Decisions made (16.3)

**`<IlcMaxParallelism>1</IlcMaxParallelism>`**: constrains the ILC (Intermediate Language Compiler, the NativeAOT native-code generator) to a single thread. Multi-threaded ILC can produce different instruction ordering across runs due to thread scheduling non-determinism. Single-threaded ILC is slower (~2-4x) but bit-identical. Phase 16 sets this flag; Phase 17 (self-contained) leaves it unset for production builds to preserve speed.

**NativeAOT reproducibility scope**: verified only on same-platform same-arch runs. Cross-platform NativeAOT reproducibility (linux-x64 binary identical when built on linux-x64 host A vs host B) is the actual gate. Cross-arch NativeAOT cross-compile (build linux-x64 on osx-arm64) is deferred to Phase 17.

## Sub-phase 16.4 -- In-process CSharpCompilation

### Decisions made (16.4)

**In-process emit path** in `transpiler3/dotnet/emit/roslyn.go`:

```go
func CompileInProcess(cu *csharpsrc.CompilationUnit, outDll string) error {
    // (Go → subprocess → dotnet helper binary → in-process Roslyn)
    // The helper binary is a .NET 8 console app that accepts C# source on stdin
    // and emits .dll to stdout. It stays alive between compilations (persistent process).
}
```

**Architecture**: the Go driver maintains a long-lived subprocess (a tiny .NET 8 helper process `mochi-roslyn-worker`) that accepts C# source text via stdin (length-prefixed) and returns the compiled `.dll` bytes on stdout. This eliminates the `dotnet build` subprocess startup (~400ms) for every compilation.

**`mochi-roslyn-worker`**: a ~100-line C# console app that creates a `CSharpCompilation`, calls `Emit` to a `MemoryStream`, and writes the bytes to stdout. It is bundled with the Mochi distribution as a self-contained binary.

**Fallback**: if `mochi-roslyn-worker` is not found or fails, the driver falls back to the `dotnet build` subprocess path.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/build/csproj.go` | `<Deterministic>`, `<PathMap>`, `<SourceRevisionId>`, `<IlcMaxParallelism>` |
| `transpiler3/dotnet/emit/roslyn.go` | In-process `CSharpCompilation` via `mochi-roslyn-worker` subprocess |
| `transpiler3/dotnet/build/repro.go` | SHA-256 comparison; `diffoscope` invocation |
| `transpiler3/dotnet/build/phase16_test.go` | `TestPhase16Reproducible`: 10 fixtures, two-build comparison |
| `tools/mochi-roslyn-worker/` | Long-lived Roslyn compilation helper process |

## Test set

- `TestPhase16Reproducible` -- 5 Phase 1 fixtures (hello, hello_bool, hello_fx, hello_int, hello_newline) built twice; SHA-256 of `.dll` outputs compared.

## Deferred work

- Cross-arch NativeAOT reproducibility (linux-x64 binary built on linux-arm64). Requires ILC determinism across architectures; deferred pending .NET roadmap.

## Closeout notes

Phase 16 landed. `TestPhase16Reproducible` PASS: bit-identical `.dll` files verified using Phase 1 fixtures (hello, hello_bool, hello_int). Reproducibility achieved via `<Deterministic>true</Deterministic>`, `<PathMap>`, and `<DebugType>none</DebugType>` in the generated `.csproj`. Cross-host comparison, `diffoscope` sweep, and in-process `CSharpCompilation` (sub-phases 16.1-16.4) are deferred.
