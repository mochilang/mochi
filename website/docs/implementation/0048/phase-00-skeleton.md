---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-48 Phase 0 — directory layout, dotnet SDK detection, csharpsrc AST node types, and Mochi.Runtime NuGet stub."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 0](/docs/mep/mep-0048#phase-0-spec-freeze-and-skeleton-trees) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase0Skeleton`: directory layout exists; `go build ./transpiler3/dotnet/...` clean; `dotnet build transpiler3/dotnet/runtime/Mochi.Runtime.csproj` produces an empty `Mochi.Runtime.0.10.0-alpha.nupkg`; dotnet SDK detection rejects SDK < 8.0.

## Goal-alignment audit

The user-facing goal of MEP-48 is "compile a Mochi program to a runnable .NET artefact". Phase 0 does not yet produce a runnable artefact. It plants the four structural anchors that make every later phase cheap to open: (1) the Go package tree tells a contributor which stage owns which concern without reading the MEP end-to-end, (2) the `Mochi.Runtime` project means every downstream phase can `dotnet build` its C# runtime pieces against a known namespace, (3) the toolchain detection step ensures every later phase can assume `dotnet` is available and at SDK 8+, and (4) the `csharpsrc` Go package provides the in-memory C# AST that every lowering pass writes to. The cost is one PR; without it every later phase repeats this orientation cost inline.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 0.0 | Directory layout + stub Go files (`doc.go` in each package); `go build ./transpiler3/dotnet/...` clean | NOT STARTED | — |
| 0.1 | Runtime NuGet stub: `Mochi.Runtime` C# project, `Runtime.cs` version constant, `dotnet build` exits 0, `dotnet pack` produces `.nupkg` | NOT STARTED | — |
| 0.2 | dotnet SDK toolchain detection: `build.go` resolves `dotnet` on `$PATH`; rejects SDK < 8.0; surfaces clear error | NOT STARTED | — |
| 0.3 | `csharpsrc` Go package: ~35 node types covering all C# constructs needed through Phase 6; each node implements `csString() string` | NOT STARTED | — |

## Sub-phase 0.0 -- Directory layout

### Goal-alignment audit (0.0)

The directory layout establishes the package boundaries. Without it, the first contributor to open a lowering bug would have to invent the split between `colour/`, `lower/`, `emit/`, `ilemit/`, `build/`, `runtime/`, and `csharpsrc/` themselves. The layout mirrors MEP-47's `transpiler3/jvm/` structure so that anyone familiar with the JVM transpiler can orient instantly. The `colour/` package is .NET-specific (async colouring pass) and has no JVM/BEAM equivalent.

### Decisions made (0.0)

**Directory structure**:

```
transpiler3/dotnet/
  colour/
    colour.go      # async colouring pass: propagates async/sync colour through call graph
    graph.go       # call-graph construction over aotir.Program
    fixpoint.go    # fixed-point iteration for colour propagation
  lower/
    lower.go       # aotir -> csharpsrc nodes (entry point)
    types.go       # Mochi type -> C# type mapping
    expr.go        # expression lowering
    stmt.go        # statement lowering
    decl.go        # top-level declarations (records, sum types, functions, modules)
    closure.go     # closure conversion; cell lifting into Func<> delegates
    match.go       # match -> C# switch expression (C# 8+)
    query.go       # query DSL + datalog lowering to LINQ
    agent.go       # agent class generation (Channel<TMessage> + async loop)
    stream.go      # stream class generation (IAsyncEnumerable<T>)
    mangle.go      # name mangling: reserved words, apostrophe, bang, mono-tag
  emit/
    emit.go        # csharpsrc -> C# source text (pretty printer)
    roslyn.go      # CSharpCompilation in-process invocation; .dll emission
    format.go      # deterministic source formatting (sorted usings, stable indent)
  ilemit/
    ilemit.go      # System.Reflection.Emit / PersistedAssemblyBuilder direct-IL path
  build/
    build.go       # Driver.Build() entry point + Target constants
    fxdep.go       # --target=dotnet-fx-dependent packaging
    singlefile.go  # --target=dotnet-singlefile packaging
    selfcontained.go # --target=dotnet-self-contained packaging
    aot.go         # --target=dotnet-aot: NativeAOT publish
    nuget.go       # --target=dotnet-nuget: NuGet package publish
    csproj.go      # csproj XML generation
    phase00_test.go
  csharpsrc/
    nodes.go       # all AST node types + csString() implementations
  runtime/
    Mochi.Runtime/
      Runtime.cs   # version constant + namespace anchors
      Mochi.Runtime.csproj
    Mochi.Analyzers/
      Analyzers.cs # MOCHI001..MOCHI006 Roslyn analyzer stubs
      Mochi.Analyzers.csproj
  testdata/
    phase00-skeleton/
      README.txt
```

Each package gets a `doc.go` with a one-paragraph package doc that states what the package owns, names the entry-point function, and cross-references adjacent packages.

**`colour/` package**: unique to the .NET transpiler. Absent in MEP-46 (BEAM) and MEP-47 (JVM) because Erlang processes are inherently async and JVM has Project Loom virtual threads. On .NET, async/await propagates through the call graph: any function that touches IO, channels, or streams must be `async Task<T>`; callers must be too. The colour pass runs immediately before lower, after aotir is complete.

## Sub-phase 0.1 -- Runtime NuGet stub

### Goal-alignment audit (0.1)

The NuGet project establishes the package identity (`Mochi.Runtime`) and namespace that all later phases build against. Without it, Phase 1 has nowhere to put the `Mochi.Runtime.IO.Print` helper that `print(str)` will call. Shipping the stub in Phase 0 means Phase 1 can add runtime classes without changing the project structure.

### Decisions made (0.1)

**`Mochi.Runtime.csproj`**:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFrameworks>net8.0;net10.0</TargetFrameworks>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <LangVersion>latest</LangVersion>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <Deterministic>true</Deterministic>
    <PackageId>Mochi.Runtime</PackageId>
    <Version>0.10.0-alpha</Version>
    <Authors>Mochi core</Authors>
    <License>Apache-2.0</License>
  </PropertyGroup>
</Project>
```

Multi-targeting (`net8.0;net10.0`) from day one. `TreatWarningsAsErrors=true` on the runtime itself enforces the Roslyn-clean secondary gate.

**`Runtime.cs`**:

```csharp
namespace Mochi.Runtime;

/// <summary>Mochi runtime for .NET. Apache-2.0.</summary>
public static class MochiRuntime
{
    public const string Version = "0.10.0-alpha";
}
```

This single file compiles cleanly. The resulting NuGet package is ~3 KB. It serves as the anchor for all later `Mochi.Runtime.*` sub-namespaces.

**`dotnet pack`**: `dotnet pack transpiler3/dotnet/runtime/Mochi.Runtime/Mochi.Runtime.csproj -c Release -o dist/` is the Phase 0 gate command. The `.nupkg` is written to `dist/Mochi.Runtime.0.10.0-alpha.nupkg`.

## Sub-phase 0.2 -- dotnet SDK detection

### Goal-alignment audit (0.2)

Every phase from 1 onwards invokes `dotnet build`. Surfacing a clear error at driver initialization ("dotnet SDK 8.0+ required; found 6.0 at /usr/bin/dotnet") is better than a cryptic build failure. Phase 0 builds the detector once so every later phase inherits it.

### Decisions made (0.2)

**`Toolchain` struct** in `transpiler3/dotnet/build/build.go`:

```go
type Toolchain struct {
    Dotnet string // absolute path to dotnet binary
    Major  int    // SDK major version (8, 9, 10, ...)
    Minor  int    // SDK minor version
    Patch  int    // SDK patch version
    RID    string // runtime identifier for current host (e.g., "linux-x64")
}
```

**`resolveToolchain()`** logic:
1. If `$DOTNET_ROOT` is set, check `$DOTNET_ROOT/dotnet`. Otherwise search `$PATH`.
2. Run `dotnet --version` and parse output `8.0.204` -> major=8, minor=0, patch=204.
3. If major < 8, return `fmt.Errorf("dotnet SDK 8.0+ required; found %d.%d at %s", major, minor, path)`.
4. Run `dotnet --info` and parse `RID:` line for the host RID.

**`TestPhase0Skeleton`** in `phase00_test.go`:
1. Calls `resolveToolchain()` -- passes if dotnet SDK 8+ is on PATH.
2. Verifies the runtime `.nupkg` exists after `dotnet pack`.
3. Runs `go build ./transpiler3/dotnet/...` via `exec.Command`.

## Sub-phase 0.3 -- csharpsrc AST node types

### Goal-alignment audit (0.3)

The `csharpsrc` package is the in-memory representation of emitted C# source. Every lowering pass in Phases 1-14 writes to `csharpsrc` nodes; the `emit` package serialises them to text, then passes the text to Roslyn. Defining the full node set in Phase 0 means no later phase needs to add new node types, keeping the schema stable across phases.

### Decisions made (0.3)

**`transpiler3/dotnet/csharpsrc/nodes.go`** defines ~35 node types:

Declaration nodes:
- `CompilationUnit` -- top-level file: using directives + namespace + type decls
- `NamespaceDecl` -- `namespace Mochi.User.PackageName { ... }`
- `ClassDecl` -- `public static class ModuleName { ... }` (module lowering target)
- `RecordDecl` -- `public sealed record Point(long X, long Y)` (positional record class)
- `RecordStructDecl` -- `public readonly record struct SmallPoint(long X, long Y)` (value type)
- `AbstractRecordDecl` -- `public abstract record Shape` (sum type base)
- `SealedRecordDecl` -- `public sealed record Circle(double R) : Shape` (sum type variant)
- `MethodDecl` -- static or instance method: modifiers, name, type params, params, return type, body
- `AsyncMethodDecl` -- async variant: return type is `Task<T>` or `ValueTask<T>`
- `PropertyDecl` -- `public T Prop { get; init; }` or with expression body
- `FieldDecl` -- field with modifiers, type, name, optional initialiser
- `InterfaceDecl` -- `public interface IAgent { ... }`
- `AttributeDecl` -- attribute applied to a declaration

Statement nodes:
- `Block` -- `{ stmt* }`
- `ReturnStmt` -- `return expr;`
- `IfStmt` -- `if (cond) thenBlock [else elseBlock]`
- `ForeachStmt` -- `foreach (var x in xs) body`
- `ForStmt` -- classic `for (init; cond; update) body`
- `WhileStmt` -- `while (cond) body`
- `BreakStmt`, `ContinueStmt`
- `ExprStmt` -- expression as statement
- `LocalDeclStmt` -- `var x = expr;` or `T x = expr;`
- `ReadonlyLocalDeclStmt` -- `readonly var x = expr;` (via `var` + flow analysis)
- `TryCatchStmt` -- `try { } catch (Type e) { }`
- `ThrowStmt` -- `throw expr;`
- `AwaitStmt` -- `await expr;` as statement
- `YieldReturnStmt` -- `yield return expr;` (for IAsyncEnumerable)

Expression nodes:
- `SwitchExpr` -- `expr switch { pattern => result, ... }` (C# 8+)
- `CallExpr` -- instance method call: receiver + name + type args + args
- `StaticCallExpr` -- static method call: class + name + type args + args
- `AwaitExpr` -- `await expr` (with optional `.ConfigureAwait(false)`)
- `LambdaExpr` -- `(params) => body` or `async (params) => { body }`
- `BinaryExpr` -- `left op right`
- `UnaryExpr` -- prefix/postfix unary
- `LiteralExpr` -- `42L`, `3.14`, `"str"`, `true`, `null`
- `NewExpr` -- `new T(args)` or `new T { Prop = val }`
- `FieldAccessExpr` -- `expr.Field`
- `IsExpr` -- `expr is Pattern var name`
- `CastExpr` -- `(T)expr`
- `CollectionExpr` -- `[a, b, c]` (C# 12 collection expression)
- `ThrowExpr` -- `throw new T(...)` as expression (C# 7+)
- `ConditionalExpr` -- ternary

Pattern nodes (used inside `SwitchExpr`):
- `TypePattern` -- `SomeType x`
- `RecordPattern` -- `Circle { R: var r }`
- `ConstantPattern` -- literal or enum member
- `WhenPattern` -- pattern with `when guard`
- `WildcardPattern` -- `_`
- `ListPattern` -- `[var h, .. var t]` (C# 11+)

Each node implements `csString(indent int) string`. The emitter in `emit/emit.go` calls `csString(0)` on the top-level `CompilationUnit` and indents nested nodes by threading the `indent` parameter.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/colour/doc.go` | Package doc: owns the async colouring pass |
| `transpiler3/dotnet/lower/doc.go` | Package doc: owns `Lower(prog *aotir.Program, colours ColourMap) (*csharpsrc.CompilationUnit, error)` |
| `transpiler3/dotnet/emit/doc.go` | Package doc: owns C# text emission and Roslyn CSharpCompilation invocation |
| `transpiler3/dotnet/ilemit/doc.go` | Package doc: owns System.Reflection.Emit direct-IL path |
| `transpiler3/dotnet/build/doc.go` | Package doc: owns `Driver.Build()` and all packaging targets |
| `transpiler3/dotnet/csharpsrc/nodes.go` | All ~35 AST node types + `csString()` implementations |
| `transpiler3/dotnet/build/build.go` | `Toolchain` struct, `resolveToolchain()`, `Target` constants |
| `transpiler3/dotnet/build/phase00_test.go` | `TestPhase0Skeleton`: toolchain detect, nupkg exists, `go build` clean |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Runtime.cs` | Version constant + namespace anchor |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Mochi.Runtime.csproj` | Multi-target net8.0/net10.0 project |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Analyzers.cs` | MOCHI001-MOCHI006 analyzer stubs |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Mochi.Analyzers.csproj` | Analyzer project referencing Roslyn |
| `transpiler3/dotnet/testdata/phase00-skeleton/README.txt` | Fixture directory placeholder |

## Test set

- `transpiler3/dotnet/build/phase00_test.go::TestPhase0Skeleton` -- three assertions: (1) `resolveToolchain()` returns SDK 8+, (2) runtime `.nupkg` exists after `dotnet pack`, (3) `go build ./transpiler3/dotnet/...` exits 0.
- `transpiler3/dotnet/csharpsrc/nodes_test.go::TestCSharpSrcNodes` -- round-trip: construct each node type and call `csString(0)`; verify the output is a valid C# fragment by running it through Roslyn's parser (`CSharpSyntaxTree.ParseText`). Covers all 35 node types.

## Deferred work

- `ilemit/ilemit.go` is a stub in Phase 0. `System.Reflection.Emit` / `PersistedAssemblyBuilder` direct-IL path for agent trampolines lands in Phase 9.
- `Mochi.Analyzers` is a stub in Phase 0. The six diagnostics (MOCHI001-MOCHI006) are implemented in Phase 5 (MOCHI001-MOCHI002), Phase 2 (MOCHI003), Phase 11 (MOCHI004-MOCHI005), and Phase 4 (MOCHI006).
- All build target files (`fxdep.go`, `singlefile.go`, etc.) are stubs in Phase 0.
- `format.go` deterministic formatting is a stub (returns raw `csString()` output). Proper sorted `using` directives and canonical blank lines land in Phase 16.

## Closeout notes

Phase 0 not yet started.
