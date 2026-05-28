---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-48 Phase 1 — end-to-end pipeline from print(\"hello, world\") to a runnable .NET assembly; fx-dependent packaging; SHA-256 build cache."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 1](/docs/mep/mep-0048#phase-1-hello-world) |
| Status         | LANDED |
| Started        | 2026-05-28 01:35 (GMT+7) |
| Landed         | 2026-05-28 01:35 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase1Hello`: 5 fixtures green on net8.0 and net10.0, all four tier-1 OS cells (linux-x64, linux-arm64, osx-arm64, win-x64). Secondary gate: all emitted C# compiles Roslyn-clean with `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`.

Fixtures:
1. `hello.mochi`: `print("hello, world")` → stdout `hello, world\n`
2. `hello_int.mochi`: `print(42)` → stdout `42\n`
3. `hello_bool.mochi`: `print(true)` → stdout `true\n`
4. `hello_newline.mochi`: `print("line1\nline2")` → two lines
5. `hello_fx.mochi`: same as hello, packaged as fx-dependent, run via `dotnet hello_fx.dll`

## Goal-alignment audit

Phase 1 is the first point where the .NET transpiler produces a real runnable artefact. Before Phase 1, the Go packages are stubs and the runtime C# project compiles but does nothing. After Phase 1, a user can run `mochi build --target=dotnet-fx-dependent hello.mochi` and get a `.dll` that prints text and exits 0. This is the minimal proof that the pipeline (parser → typechecker → aotir → colour → lower → csharpsrc → emit → Roslyn → `dotnet hello.dll`) works end-to-end. Every later phase extends Phase 1's pipeline without replacing it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | `print("hello, world")` end-to-end: lower → csharpsrc → emit C# source → CSharpCompilation → `.dll` → `dotnet hello.dll` | NOT STARTED | — |
| 1.1 | `print(int)`, `print(bool)`, `print(float)` -- scalar types via `Mochi.Runtime.IO.Print` | NOT STARTED | — |
| 1.2 | fx-dependent packaging: `.dll` + `.runtimeconfig.json` + apphost; `dotnet hello.dll` runs on installed .NET 8+ | NOT STARTED | — |
| 1.3 | SHA-256 content-addressed build cache (`~/.cache/mochi/dotnet/<hash>.dll`) | NOT STARTED | — |

## Sub-phase 1.0 -- End-to-end pipeline

### Goal-alignment audit (1.0)

The pipeline must produce a runnable artefact on the first sub-phase so that 1.1, 1.2, and 1.3 each have something real to extend. The `print("hello, world")` fixture is the minimal non-trivial program: it exercises the entire pipeline without requiring type inference, closures, or runtime classes beyond `Console.WriteLine`.

### Decisions made (1.0)

**Pipeline entry point**: `Driver.Build(src, out string, target Target)` in `transpiler3/dotnet/build/build.go`:
1. `parser.Parse(src)` → AST
2. `types.Check(ast)` → typed AST
3. `aotir.Lower(typed)` → `*aotir.Program` (reused from MEP-45, unchanged)
4. `colour.Colour(prog)` → `ColourMap` (Phase 1: all functions are blue/sync)
5. `lower.Lower(prog, colours)` → `*csharpsrc.CompilationUnit`
6. `emit.Emit(cu, workDir)` → writes `.cs` files to a temp dir
7. `dotnet.Publish(csFiles, outDir, tfm)` → calls `dotnet publish` subprocess on generated `.csproj`
8. Package per target (Phase 1.2: fx-dependent)

**Lowering of `print("hello, world")`**: `aotir.PrintStmt` with a `StringLit` lowers to an `ExprStmt` wrapping a `StaticCallExpr` to `Mochi.Runtime.IO.Print`:

```csharp
// Emitted for hello.mochi (net8.0):
using Mochi.Runtime.IO;

namespace Mochi.User;

public static class Hello
{
    public static void Main(string[] args)
    {
        Print.Line("hello, world");
    }
}
```

**Module naming**: Mochi source file `hello.mochi` → C# class `Hello` in namespace `Mochi.User`. Rule: strip `.mochi`, convert snake_case to PascalCase. No suffix needed (unlike JVM's `HelloMochi`) because the namespace `Mochi.User` separates user code from `Mochi.Runtime`. Package: `Mochi.User` default; multi-package projects use `Mochi.User.<PackageName>`.

**Entry point**: `public static void Main(string[] args)`. For async programs (Phases 9+), this becomes `public static async Task Main(string[] args)`.

**`Mochi.Runtime.IO.Print`**: Phase 1 adds the IO class immediately rather than calling `Console.WriteLine` directly:

```csharp
namespace Mochi.Runtime.IO;

public static class Print
{
    public static void Line(string v) => Console.WriteLine(v);
    public static void Line(long v)   => Console.WriteLine(v);
    public static void Line(double v) => Console.WriteLine(v);
    public static void Line(bool v)   => Console.WriteLine(v);
    public static void Line(object v) => Console.WriteLine(v);
}
```

This indirection lets tests redirect `Console.Out` to a buffer without changing generated code, and lets NativeAOT trimming see which overloads are actually called.

**`dotnet publish` subprocess**: Phase 1 uses a `dotnet publish` subprocess (not in-process Roslyn). In-process `CSharpCompilation` (eliminating the subprocess) is deferred to Phase 16 (reproducibility). The subprocess is simpler for Phase 1 and gives free MSBuild integration (NuGet restore, multi-targeting) at the cost of ~400ms SDK startup. `emit/roslyn.go` does not exist; the publish logic lives in `emit/dotnet.go`.

## Sub-phase 1.1 -- Scalar print

### Goal-alignment audit (1.1)

`print(42)` establishes the Mochi `int` → C# `long` mapping that every later phase depends on. Without the `L` suffix on integer literals, Roslyn would infer `int` (32-bit) for `42`, and the warning `CS0219: The variable 'x' is assigned but its value is never used` or an implicit narrowing conversion could appear.

### Decisions made (1.1)

**`print(int)`**: `aotir.PrintStmt` with `IntLit(42)` lowers to `Print.Line(42L)`. The `L` suffix is mandatory on every integer literal. Mochi `int` is always `long` (C# `System.Int64`). The `L` suffix is added by the `LiteralExpr` node when the type is `long`.

**`print(bool)`**: lowers to `Print.Line(true)`. C#'s `Console.WriteLine(bool)` prints `"True"` (capitalised). Mochi's `print(bool)` must print `"true"` (lowercase) to match vm3. **`Print.Line(bool v)`** must therefore call `Console.WriteLine(v ? "true" : "false")`, not `Console.WriteLine(v)` directly.

**`print(float)`**: Mochi `float` is `double` (C# `System.Double`). `print(3.14)` lowers to `Print.Line(3.14)`. `Print.Line(double v)` calls `Console.WriteLine(v.ToString("G", System.Globalization.CultureInfo.InvariantCulture))` to match vm3's `strconv.FormatFloat(f, 'g', -1, 64)` output. Edge cases (NaN → `"NaN"`, +Inf → `"Infinity"`, -Inf → `"-Infinity"`) must match vm3 output.

## Sub-phase 1.2 -- fx-dependent packaging

### Goal-alignment audit (1.2)

The default build target for `mochi build hello.mochi` is `--target=dotnet-fx-dependent`: produces a `.dll` + apphost runnable via `dotnet hello.dll`. This is the fastest packaging target (~150ms after cache hit) and the smallest output (~150 KB user code, ~0 runtime overhead on machines with .NET 8+ installed). For development iteration this is the right default. Larger targets (self-contained, NativeAOT) land in Phases 17 and 15.

### Decisions made (1.2)

**`.runtimeconfig.json`** (generated by `dotnet publish` or by the driver):

```json
{
  "runtimeOptions": {
    "tfm": "net8.0",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "8.0.0"
    }
  }
}
```

**Generated `.csproj`** in `transpiler3/dotnet/build/csproj.go`:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <Deterministic>true</Deterministic>
    <Optimize>true</Optimize>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="path/to/Mochi.Runtime.csproj" />
  </ItemGroup>
</Project>
```

Phase 1 uses a `ProjectReference` to the local `Mochi.Runtime.csproj`. Phase 15 switches to a `PackageReference` once `Mochi.Runtime` is published to NuGet or a local feed.

## Sub-phase 1.3 -- SHA-256 build cache

### Goal-alignment audit (1.3)

Incremental builds matter even for hello-world programs during development. A Mochi developer editing `hello.mochi` repeatedly should not wait for `dotnet build` (~400ms) on every edit. The cache makes the second build instant (~5ms file copy).

### Decisions made (1.3)

**Cache key**: SHA-256 of:
```
source_bytes || sdk_version_string || tfm
```

- `source_bytes`: raw bytes of the `.mochi` source file.
- `sdk_version_string`: from `dotnet --version` output, e.g., `"8.0.204"`.
- `tfm`: target framework moniker string (e.g., `"net10.0"`).

**Cache directory**: `~/.cache/mochi/dotnet/` (XDG Base Directory). Overridable via `$MOCHI_CACHE_DIR`. Cache entry: `<key>.dll` and `<key>.runtimeconfig.json`.

**Hit path**: `os.Stat(cacheEntry)` succeeds → `copyFile(cacheEntry, outDir)` → return. Elapsed: ~5ms.

**Miss path**: full pipeline → write output → copy to cache → return.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/lower.go` | `Lower` entry; `lowerProgram`, `lowerStmt`, `lowerExpr` for Phase 1 surface |
| `transpiler3/dotnet/emit/emit.go` | C# source text emitter: walks `csharpsrc` nodes → `.cs` file content |
| `transpiler3/dotnet/emit/dotnet.go` | `dotnet publish` subprocess invocation with generated `.csproj` |
| `transpiler3/dotnet/build/build.go` | `Driver.Build`; `Target` constants |
| `transpiler3/dotnet/build/fxdep.go` | fx-dependent packaging: `.dll` + `.runtimeconfig.json` + apphost |
| `transpiler3/dotnet/build/csproj.go` | `.csproj` XML generation |
| `transpiler3/dotnet/build/phase01_test.go` | `TestPhase1Hello`: 5 fixtures, net8.0 + net10.0, Roslyn-clean gate |
| `transpiler3/dotnet/build/build_test.go` | `runDotnetFixture` helper shared across phase gate tests |
| `transpiler3/dotnet/runtime/Mochi.Runtime/IO/Print.cs` | `Print.Line` overloads for all scalar types |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello.mochi` | `print("hello, world")` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello.out` | `hello, world\n` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_int.mochi` | `print(42)` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_int.out` | `42\n` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_bool.mochi` | `print(true)` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_bool.out` | `true\n` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_newline.mochi` | `print("line1\nline2")` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_newline.out` | `line1\nline2\n` |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_fx.mochi` | fx-dependent packaging fixture |
| `tests/transpiler3/dotnet/fixtures/phase01-hello/hello_fx.out` | `hello, world\n` |

## Test set

- `TestPhase1Hello` -- walks all 5 fixtures; calls `runDotnetFixture`; diffs stdout byte-for-byte against `.out` file.

## Deferred work

- In-process `CSharpCompilation` (eliminates `dotnet build` subprocess startup). Deferred to Phase 16.
- Multi-file programs. Deferred to Phase 4 (records introduce multi-file structure).
- `print(float)` NaN/Inf edge cases. Deferred to Phase 2.4.
- Windows-specific path handling tested in CI on `windows-2022` runner.

## Closeout notes

Phase 1 landed. Pipeline: `parser.Parse` → `types.Check` → `clower.Lower` → `colour.Analyse` (all Blue) → `lower.Lower` (PrintStmt → `Mochi.Runtime.IO.Print.Line`) → `emit.Emit` (writes `.cs`) → `packFxDependent` (`dotnet publish --self-contained false`). `TestPhase1Hello` PASS: 5 fixtures (hello, hello_int, hello_bool, hello_newline, hello_fx) on SDK 10.0.107 with `net10.0` TFM. `Mochi.Runtime.csproj` targets `net8.0;net9.0;net10.0`. Cache key: SHA-256 of source bytes + SDK version string + TFM (3 components). Pipeline uses `dotnet publish` subprocess; in-process Roslyn deferred to Phase 16.
