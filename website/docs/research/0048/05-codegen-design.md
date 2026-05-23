# MEP-48 research note 05, Codegen design and IR layer choice

Author: research pass for MEP-48 (Mochi to .NET / CLR transpiler).
Date: 2026-05-23 02:10 (GMT+7).
Target runtime: .NET 8 LTS minimum (Nov 2023, support to Nov 2026),
.NET 10 LTS recommended (Nov 2025, LTS through Nov 2028),
with forward compatibility to .NET 11 (preview, ships Nov 2026).
Companion to MEP-45 (C, [[../0045/05-codegen-design]]), MEP-46
(BEAM, [[../0046/05-codegen-design]]), MEP-47 (JVM,
[[../0047/05-codegen-design]]).

## 1. Why the choice of IR matters on CLR

Unlike BEAM (see [[../0046/05-codegen-design]]) and like the JVM (see
[[../0047/05-codegen-design]]), the .NET runtime offers a marketplace
of layers rather than one canonical front door. The CLR's situation
differs from the JVM's in three meaningful ways.

1. **The official source compiler is also a library.** Roslyn (the
   C# compiler) is a NuGet package, not a separate tool that the
   transpiler has to shell out to. `CSharpCompilation.Create(...).Emit(stream)`
   runs in-process and writes a PE assembly to a stream in
   milliseconds. There is no `javac` startup cost analogue.

2. **The official IL emitter regressed.** When .NET Framework
   transitioned to .NET Core / .NET 5, the venerable
   `AssemblyBuilder.Save(...)` API was dropped. From .NET 5 through
   .NET 8, in-process IL emission could only produce a runnable
   in-memory assembly, never a persistable `.dll`. That regression
   was a major reason .NET-targeted transpilers (F# excluded) lean
   toward source emission. The .NET 9 release (Nov 2024) restored
   the API as `System.Reflection.Emit.PersistedAssemblyBuilder`.
   Lokad.ILPack covers older runtimes.

3. **NativeAOT is a first-class deployment mode.** Choosing an
   IR layer determines whether the produced binary can be passed
   through `dotnet publish -p:PublishAot=true`. Reflection-based
   factories that compile fine under JIT throw at AOT publish time
   if their `Type` arguments are not annotated with
   `[DynamicallyAccessedMembers]`. The IR layer choice gates these
   annotations, see §9.

The choice is load-bearing for the same six reasons listed in
[[../0047/05-codegen-design]] §1 plus two CLR-specific ones.

- **PDB shape.** Portable PDB is a standalone metadata file that
  ships alongside the assembly. Mapping back to Mochi source lines
  is done via `#line` directives in emitted C#, or via the
  `MethodDebugInformation` table when emitting IL directly.

- **NuGet packaging.** The output of a Mochi compile must be
  consumable by `dotnet add package`. The pack step (see §5.5)
  needs an assembly plus a PDB plus a `.nuspec`. Any IR layer that
  cannot persist a stable PE on disk in .NET 8 (i.e. anything
  except Roslyn or Lokad.ILPack) imposes a runtime upgrade on every
  Mochi user.

## 2. The candidate IR layers

Ten plausible front doors, highest abstraction first.

| #  | Candidate | Layer | Public entry point | NuGet |
|----|-----------|-------|--------------------|-------|
| 1  | C# source text (`.cs`) | source | `CSharpCompilation.Emit` | Microsoft.CodeAnalysis.CSharp 5.3.0 |
| 2  | F# source text (`.fs`) | source | `FSharpChecker.Compile` | FSharp.Compiler.Service |
| 3  | Roslyn SyntaxFactory AST | source builder | `SyntaxFactory.ClassDeclaration(...)` | Microsoft.CodeAnalysis.CSharp 5.3.0 |
| 4  | Roslyn Source Generators | source builder | `IIncrementalGenerator` | Microsoft.CodeAnalysis.CSharp 5.3.0 |
| 5  | `System.Reflection.Emit` (runnable) | IL | `AssemblyBuilder.DefineDynamicAssembly` | (BCL) |
| 6  | `Reflection.Emit` + Lokad.ILPack | IL | + `AssemblyGenerator.GenerateAssembly` | Lokad.ILPack 0.3.1 |
| 7  | `PersistedAssemblyBuilder` (.NET 9+) | IL (stdlib) | `new PersistedAssemblyBuilder(...).Save(...)` | (BCL .NET 9+) |
| 8  | Mono.Cecil | IL read/write | `AssemblyDefinition.Write` | Mono.Cecil 0.11.6 |
| 9  | dnlib | IL read/write | `ModuleDefMD.Write` | dnlib 4.5.0 |
| 10 | AsmResolver | IL read/write | `AssemblyDefinition.Write` | AsmResolver 6.0.0 |

A faint eleventh is "string templates plus Roslyn parse-and-emit", a
degenerate version of #1 where the transpiler skips the AST builder
and emits text. We list it as the fallback escape hatch in §11.

## 3. Layer-by-layer analysis

### 3.1 C# source text via Roslyn

The default choice. The flow is

```
Mochi AST -> Mochi MIR (aotir) -> C# source text (StringBuilder or SyntaxFactory)
          -> CSharpSyntaxTree.ParseText -> CSharpCompilation.Create
          -> CSharpCompilation.Emit(peStream, pdbStream)
          -> .dll + .pdb on disk
```

What we keep from Roslyn for free:

- **Static type checking.** Every transpiler bug that produces ill-typed
  C# is caught as a `CS****` diagnostic before any IL is emitted. This
  is the largest single source of confidence Roslyn provides over IL
  layers. Bad casts, missing members, accidental nullability
  violations, ambiguous overloads, all caught.
- **Closure lowering.** C# lambdas lower to `[CompilerGenerated]`
  display classes and `DelegateCache<,>` factories. We do not write
  any of that by hand.
- **Pattern lowering.** C# 11 list patterns, C# 12 collection
  expressions, C# 14 list slice patterns all desugar inside Roslyn.
- **Tail calls.** C# does not expose `.tail.`; Roslyn never emits it.
  We compensate with trampolines (§6.6), independent of layer choice.
- **`async` / `await` lowering.** The state machine that wraps a
  `Task<T>` returning method is generated by Roslyn. Mochi's async
  surface (see [[09-agent-streams]]) lowers to `async Task<T>` and
  inherits the work.
- **NRT (nullable reference type) flow analysis.** `#nullable enable`
  on the file header runs the flow analyser; if Mochi's type checker
  has a bug that produces a possibly-null assignment to a non-nullable
  field, Roslyn reports it.
- **Code Style analysers.** Optional `dotnet format` over the emitted
  output yields human-readable C#, useful for `--emit cs`.

**Maturity.** Microsoft.CodeAnalysis.CSharp 5.3.0 (10 March 2026).
Ships with every .NET SDK since 5.0. 1.5 B+ NuGet downloads. The
compiler that compiles every Roslyn-managed code base on earth.
**Doc quality.** Excellent. The dotnet/roslyn cookbook, the language
spec, the `#nullable` reference, the SyntaxFactory API reference.
**.NET 8 / .NET 10 support.** Both LTS targets supported. Roslyn
multi-targets `netstandard2.0`, so the transpiler runs on .NET 8
hosts and emits assemblies that target .NET 10.
**C# 14 support.** Roslyn 5.3.0 supports `LangVersion=14.0` which
covers extension members, partial constructors and events, the
`field` keyword, lambda parameter modifiers, implicit span
conversions, `nameof` unbound generics, null-conditional assignment.
**Size.** Microsoft.CodeAnalysis.CSharp.dll is ~22.8 MB; with the
workspaces package the total is ~30 MB. This is the only large cost.
**AOT.** Roslyn itself is not AOT-friendly (large reflection
surface), so the transpiler binary stays JIT. The emitted assemblies
are AOT-friendly modulo §9.

### 3.2 F# source text via FSharp.Compiler.Service

Rejected, for symmetric reasons to Kotlin / Scala on JVM
([[../0047/05-codegen-design]] §3.2-3.3).

- An extra compiler dependency next to Roslyn doubles the SDK
  surface (~50+ MB total).
- F#-specific runtime support (`FSharp.Core` ~5 MB) is required at
  load time of every emitted assembly.
- Mochi's surface looks like ML-flavoured Go; F# adds a syntactic
  layer Mochi developers do not read. Stack traces and IL inspection
  show F# names, not Mochi names.
- Roslyn already provides everything Mochi needs from a source
  layer; F# does not unlock a missing capability.

### 3.3 Roslyn SyntaxFactory AST

The structural form of choice 3.1. Instead of building a `StringBuilder`
and calling `CSharpSyntaxTree.ParseText`, we directly construct
`MemberDeclarationSyntax` nodes via `SyntaxFactory.ClassDeclaration(...)`,
`SyntaxFactory.MethodDeclaration(...)`, and so on.

```csharp
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

ClassDeclarationSyntax cls = ClassDeclaration("Foo")
    .AddModifiers(Token(SyntaxKind.PublicKeyword))
    .AddMembers(
        PropertyDeclaration(ParseTypeName("int"), "X")
            .AddModifiers(Token(SyntaxKind.PublicKeyword))
            .AddAccessorListAccessors(
                AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))));
```

**Pros.**
- No string formatting; no escape bugs around identifiers that
  collide with C# keywords.
- The resulting syntax tree can be fed directly to
  `CSharpCompilation.Create(syntaxTrees: [...])`, avoiding a parse
  round-trip.
- Token trivia (whitespace, comments) attaches structurally; we can
  emit Mochi-source comments next to lowered code.
- `NormalizeWhitespace().ToFullString()` produces pretty C# for
  `--emit cs`.

**Cons.**
- Verbose. A toy class can be ~30 API calls. Mochi's lower pass
  needs a SyntaxFactory facade.
- Cliffs around accessor lists, semicolons, modifiers. The well-known
  pitfall is forgetting `.WithSemicolonToken(...)` on an auto-property
  accessor.
- Helper discovery is hard. SyntaxFactory has hundreds of overloads.
  We mitigate via a thin Mochi-facing wrapper (`MochiC.ClassBuilder`)
  that exposes ~30 verbs Mochi codegen actually uses.

**Choice between 3.1 and 3.3.** Use **3.3 SyntaxFactory** as default,
because the rest of the lower pipeline already speaks an AST. Fall
back to string-template emission for two cases: (a) `#line`
directives, which are easier as text, and (b) the escape hatch when
the Roslyn dep is too heavy for a Mochi user (§11).

### 3.4 Roslyn Source Generators (IIncrementalGenerator)

These run inside the C# compiler driver during a user's `dotnet
build`. The generator implements `IIncrementalGenerator` and emits
additional C# files into the compilation.

This is the wrong shape for a from-scratch transpiler. Source
generators are designed for *additive* codegen within an existing
C# project; they assume there is already a host `.csproj`. Mochi
needs to produce an assembly from `.mochi` files only.

We may revisit source generators for a **secondary** integration
path, the "interop story": a C# project references
`Mochi.Interop.SourceGenerator`, the generator scans for `.mochi`
files in the project (via `additionalFiles`), and emits the
transpiled C# next to it. That is a different feature (Mochi
embedded in C#) from the primary CLI transpiler. Out of scope for
this note.

### 3.5 `System.Reflection.Emit` (runnable only)

The original BCL IL builder. `AssemblyBuilder.DefineDynamicAssembly`
gives back an in-memory assembly; we define modules, types, methods,
and IL via `ILGenerator.Emit(OpCodes.*)`.

**Pros.** Zero external deps. Compact. Fast emit (no parse, no
type check, no syntax tree). For Mochi's hot lowerings (numeric
loops, monomorphic Span<T> accessors, lambda call sites), the win
is real because we skip the C# lowering passes.

**Cons.**
- Cannot persist to disk on .NET 5 through .NET 8. Save was
  removed during the Core transition and only restored in .NET 9
  (see 3.7).
- No static type check. Mochi codegen bugs become InvalidProgramException
  at JIT time.
- Verbose; the developer maintains a stack-balance invariant manually.
- No PDB unless we use `MethodBuilder.SetSymbolGenerator` plus the
  legacy DiaSymReader path, which itself was deprecated.

Our use of Reflection.Emit is limited to **hot path lowerings** in
JIT mode (in-process compile-and-run), not the offline transpile
flow. See §11.

### 3.6 `Reflection.Emit` + Lokad.ILPack

Lokad.ILPack 0.3.1 (11 March 2026, MIT, 2.5 M downloads) is a
single-purpose library that takes a `System.Reflection.Emit`-built
dynamic assembly and serialises it to a `.dll` on disk under
.NET Standard 2.0. It is the canonical workaround for the
.NET 5-8 persistence regression and remains useful for transpilers
that have to ship on hosts older than .NET 9.

```csharp
var ab = AssemblyBuilder.DefineDynamicAssembly(name, AssemblyBuilderAccess.Run);
// ... define modules, types, IL ...
var gen = new Lokad.ILPack.AssemblyGenerator();
gen.GenerateAssembly(ab, "/tmp/out.dll");
```

**Maturity.** Stable; 0.3.0 was the only release between May 2022 and
March 2025, and 0.3.1 in March 2026. Low cadence reflects the small
API surface, not abandonment. The Lokad team maintains it for their
internal codegen needs.
**Doc.** Sparse but adequate. README + sample.
**Limitations.** Generic types with constraints involving generic
arguments occasionally serialise wrong; the codegen needs to keep IL
within a tested subset. Issue tracker has open generics-related
issues; Mochi's hot paths use only monomorphic IL so the limitation
does not bite.

### 3.7 `PersistedAssemblyBuilder` (.NET 9+)

In .NET 9, `System.Reflection.Emit.PersistedAssemblyBuilder` was
added to the BCL. It is a separate, fully-managed Reflection.Emit
implementation that supports saving. It sits next to (not on top of)
the runnable `AssemblyBuilder`; the persisted assembly cannot be
executed directly, you have to save then load.

```csharp
var ab = new PersistedAssemblyBuilder(name, typeof(object).Assembly);
var mod = ab.DefineDynamicModule("M");
// ... define types, IL ...
ab.Save("/tmp/out.dll");
```

For Mochi hosts that can require .NET 9+, this replaces Lokad.ILPack
with a BCL-native API. Some niche APIs (`GetCustomAttributes()` on
a `TypeBuilder` post-create) throw `NotSupportedException` in the
persisted variant, by design.

**Choice.** Use `PersistedAssemblyBuilder` on .NET 9+ hosts;
Lokad.ILPack on .NET 8 hosts; deprecate the Lokad path once Mochi's
minimum drops .NET 8.

### 3.8 Mono.Cecil

The veteran. Mono.Cecil 0.11.6 reads and writes .NET assemblies and
modules, with full generics support. Used by Unity IL2CPP, JetBrains
Rider tooling, NSubstitute's proxy generator, Fody, MonoMod. 74M+
NuGet downloads. The 0.11.6 release has been stable for years
(version cadence is approximately one bug-fix per 18 months, which
in this library indicates maturity rather than abandonment).

**Pros.** Stable API. Full read/write. Handles every IL pattern Mochi
might emit. Works on .NET Standard 2.0 so the transpiler runs on
.NET 8 hosts.
**Cons.** Third-party. Release cadence lags .NET runtime releases by
6-12 months; new metadata features (e.g. ref readonly fields, inline
arrays metadata format) arrive late. No PDB-portable writer
out of the box without extra effort.
**Comparison to dnlib and AsmResolver.** Cecil's API is the most
idiomatic and the most documented; the trade-off is that it is the
slowest to absorb new ECMA-335 amendments.

### 3.9 dnlib

dnlib 4.5.0 (15 May 2025, MIT, 2.4 M downloads) is the IL writer used
by de4dot, dnSpy, and a slice of the reverse-engineering ecosystem.
Reads and writes Windows PDB and Portable PDB. The API surface is
more sprawling than Cecil's and less curated.

**Pros.** Handles obscure metadata (mixed-mode assemblies, unusual
PDB shapes). Up-to-date with most ECMA-335 amendments.
**Cons.** API ergonomics are weaker. Documentation is README-only.
Less obvious choice for greenfield codegen; clearly aimed at
read-modify-write of existing assemblies.

### 3.10 AsmResolver

AsmResolver 6.0.0 (16 May 2026, MIT) is the newest of the three.
Released by `Washi1337`. Major-version cadence faster than Cecil.
1.07 K stars on GitHub; smaller user base than Cecil or dnlib but
better-curated public API.

**Pros.** Modern API design. Strong support for AppHost and
SingleFileHost bundle formats. Active maintainer. Good for tooling
that reads existing assemblies (the original use case is reverse
engineering).
**Cons.** Smaller ecosystem. Fewer Stack Overflow / blog posts to
crib from when stuck.

## 4. Decision criteria

We score on seven axes, each 0 (poor) to 5 (excellent).

- **stability**: how often a major version moves under us.
- **doc**: quality and findability of canonical docs.
- **AOT compatibility**: do emitted assemblies pass `PublishAot`?
- **debuggability**: PDB quality, IL inspection, stack-trace fidelity.
- **.NET 8 + .NET 10 support**: does the library cover both LTS?
- **size**: transpiler binary bloat (smaller is better, score
  inverted: small = 5).
- **transpiler complexity**: cost of the lower pass against this
  layer (less work = 5).

See the matrix in §10.

## 5. Five-pass pipeline

Mochi's transpile flow reuses `transpiler3/c/aotir` plumbing where
possible. The five passes:

### 5.1 Monomorphisation (reuse aotir)

Same logic as MEP-45 phase 3 ([[../0045/05-codegen-design]] §5.1).
Generic Mochi functions instantiate to monomorphic copies keyed on
their type arguments. Unlike C, we do **not** strictly need
monomorphisation for the CLR: the runtime has true generics and a
shared canonical instantiation for reference types. We still
monomorphise value-type instantiations (avoiding boxing) and reuse
the existing pass to keep IR symmetry.

The monomorphisation pass yields a `MonoIR` graph where every
function and type is concrete.

### 5.2 Closure conversion (reuse aotir)

Mochi closures lift to display structures with explicit captured
fields. The lowering target is a C# anonymous class (`record class
__Closure_1234(int x, string y) { ... }`) plus a delegate that
captures the closure. C# 12 primary constructors make this terse:

```csharp
public sealed record __Closure_1234(int X, string Y) {
    public int Apply(int z) => X + Y.Length + z;
}
```

The display structure is then wrapped at the call site:

```csharp
Func<int,int> f = new __Closure_1234(42, "hi").Apply;
```

Closure conversion is layer-agnostic. Reusing aotir's pass means
MEP-45, 46, 47 and 48 share the same shape; backends only differ in
the syntactic wrapper around the lifted record.

### 5.3 Name mangling

C# identifiers permit `_`, ASCII letters and digits, Unicode
letters, and (via `@`-quoted form) any reserved word. Mochi names
can collide with C# reserved words (`base`, `new`, `class`,
`record`, `event`, `field` after C# 14), so the mangler:

- Prepends `@` to identifiers that match a C# reserved word.
- Replaces `'` with `_prime`.
- Replaces `!` with `_bang`.
- Inserts a u4 hex tag for monomorphised instantiations:
  `List_Get<int>` becomes `List_Get_2a4b_Int32` after monomorphisation.

The mangler also encodes nesting: a Mochi nested function `f.g.h`
becomes the C# nested class `F.G.H`. This is preferable to flat
underscored names because Visual Studio's outline view groups
nested classes correctly.

### 5.4 Emit

The emit pass converts `MonoIR` into a Roslyn `SyntaxTree` via the
Mochi facade over SyntaxFactory. Each `MonoIR.Module` becomes a
`CompilationUnitSyntax` with one `#line` directive per Mochi source
location. The list of syntax trees feeds `CSharpCompilation.Create`
with options:

```csharp
var opts = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
    .WithOptimizationLevel(OptimizationLevel.Release)
    .WithNullableContextOptions(NullableContextOptions.Enable)
    .WithAllowUnsafe(true)
    .WithDeterministic(true);

var comp = CSharpCompilation.Create("Mochi.Generated.Foo",
    syntaxTrees: trees,
    references: refs,
    options: opts);

using var pe = File.Create("/tmp/out.dll");
using var pdb = File.Create("/tmp/out.pdb");
var emit = comp.Emit(peStream: pe, pdbStream: pdb,
    options: new EmitOptions(debugInformationFormat: DebugInformationFormat.PortablePdb));
```

`WithDeterministic(true)` produces byte-identical output across
runs, which Mochi's test harness ([[11-testing-gates]]) requires.
`NullableContextOptions.Enable` turns NRT on globally so transpiler
bugs that mishandle nullability fail loudly.

### 5.5 Postprocess (NuGet pack)

For the `--pack` flag, the emit pass is followed by:

1. Write `.nuspec` from MEP module metadata.
2. Run `nuget pack` or call `NuGet.Packaging` library directly.
3. Embed PDB into the assembly (`DebugInformationFormat.Embedded`)
   when `--single-file`.
4. Optionally sign the assembly via `sn` or the NuGet signing flow.

Step 2 has two implementations: shell out to `dotnet pack` (slow,
~3 s cold) or call `NuGet.Packaging` in-process. We use the
in-process path for incremental builds.

## 6. Lowering details

### 6.1 Closures

As §5.2. The closure record is `sealed record` with a primary
constructor. The delegate target is a method on the record. For
escaping closures we generate the record class; for non-escaping
ones (the common case in `for ... in ...` bodies, see
[[01-language-surface]]) C# 9 static lambdas and C# 12 primary
constructors let the lowering inline as a `static` delegate that
captures nothing, avoiding the allocation.

### 6.2 Sum types

Two strategies depending on the C# language version.

- **C# 12+ (default).** Sealed abstract record hierarchy.

  ```csharp
  public abstract record Shape;
  public sealed record Circle(double R) : Shape;
  public sealed record Square(double Side) : Shape;
  ```

  `match` over `Shape` lowers to a switch expression with property
  patterns (`shape switch { Circle { R: var r } => ... }`).

- **C# 14+ (preferred when minimum drops to .NET 10).** Use
  abstract record + list patterns where the Mochi match contains
  list-shaped subpatterns. C# 14 list slice patterns let Mochi's
  `[x, ..xs]` lower directly without manual head/tail decomposition.

For exhaustiveness, the lowered C# match adds a `_ => throw new
UnreachableException()` arm (System.Diagnostics.UnreachableException
in .NET 7+). Mochi's type checker has already proven exhaustiveness;
the throw exists only to satisfy CS8509 in case Roslyn cannot prove
it after lowering.

### 6.3 Match

Switch expressions with property patterns (C# 8) plus list patterns
(C# 11) plus collection expressions (C# 12) cover every Mochi
pattern. C# 14 adds list patterns to switch expressions over
collections, useful for Mochi's `[x, ..xs]` shape.

The lower pass emits `expr switch { Pattern => body, ... }` rather
than `switch (expr) { case Pattern: ... }` because the expression
form composes with the rest of Mochi's expression-oriented IR.

### 6.4 Generics

The CLR has true reified generics. Unlike the JVM
([[../0047/05-codegen-design]] §6.4 erasure section) and Mochi-on-C
(monomorphised), Mochi generic functions can map 1:1 to C# generic
methods:

```csharp
public static T Identity<T>(T x) => x;
```

`typeof(T)` inside the method body works as expected; reflection
sees the type argument; arrays of `T` are `T[]` and not
`object[]`. This is the **major win over JVM** for any Mochi code
that introspects type arguments (Mochi's `typeof` and runtime
dispatch).

Constraint clauses (`where T : ...`) lower from Mochi's bounded
generics. The mapping:

- Mochi `T: copy` (value semantics) → C# `where T : struct`.
- Mochi `T: ref` (reference semantics) → C# `where T : class`.
- Mochi `T: Eq` → C# `where T : IEquatable<T>`.
- Mochi `T: Ord` → C# `where T : IComparable<T>`.
- Mochi `T: numeric` (.NET 7+ generic math) → C# `where T :
  INumber<T>`. This unlocks Mochi numeric code as truly generic on
  CLR, no monomorphisation needed.

The `INumber<T>` route is closed on JVM (no equivalent), so MEP-48
emits faster numeric code than MEP-47 for polymorphic Mochi
numerics. Worth calling out in the docs.

### 6.5 Tail calls

C# does not expose `.tail.`. Roslyn never emits the tail prefix.
For self-recursive tail calls we emit a trampoline at the Mochi
lowering layer:

```csharp
while (true) {
    if (base_case) return result;
    // rebind parameters
    n = n - 1; acc = acc * n;
}
```

Mutual tail calls between Mochi functions lower to a Thunk-style
trampoline (`while ((next = next.Step()) != null)`). Performance is
roughly equal to a loop because the JIT inlines the `Step()` call
in the hot path.

For non-self tail calls, we **do** consider emitting raw IL with
the `.tail` prefix as a §11 hot-path optimisation; the JIT
honours it on x64 and arm64 in .NET 8+. This is one of the cases
where we drop to Reflection.Emit.

### 6.6 Result<T, E>

Mochi `Result<T, E>` lowers to a `readonly record struct` to avoid
heap allocation:

```csharp
public readonly record struct Result<T, E> {
    public readonly T Value;
    public readonly E Error;
    public readonly bool IsOk;
    public static Result<T, E> Ok(T v) => new(v, default!, true);
    public static Result<T, E> Err(E e) => new(default!, e, false);
}
```

The `?` operator (Mochi shorthand for "early-return on Err") lowers
to a switch pattern.

### 6.7 Async via Task<T> / ValueTask<T>

Mochi `async fn` lowers to `async Task<T>`; Mochi `agent` streams
([[09-agent-streams]]) lower to `IAsyncEnumerable<T>`. Roslyn
provides the state-machine rewrite for both.

For hot, frequently-awaited methods Mochi codegen emits
`ValueTask<T>` (no heap allocation when the synchronous fast path
hits). The annotation is driven by a Mochi attribute, not a runtime
heuristic.

## 7. Cross-version stability matrix

The minimum .NET target is 8 LTS; the recommended target is 10 LTS.
Feature usage by C# version, with the minimum runtime that supports
each:

| Feature | C# | Min .NET | Mochi usage |
|---------|----|----------|-------------|
| File-scoped namespaces | 10 | 6 | every emitted file |
| `record class` | 9 | 5 | sum-type variants |
| `record struct` | 10 | 6 | Result, Option, Either |
| `init` setters | 9 | 5 | immutable property init |
| `required` members | 11 | 7 | enforce field init in lowered records |
| Raw string literals | 11 | 7 | string literals with embedded quotes |
| List patterns | 11 | 7 | match over list-shaped Mochi values |
| Primary constructors (classes) | 12 | 8 | closure display classes |
| Collection expressions | 12 | 8 | array / list literals in lowered code |
| Inline arrays | 12 | 8 | small fixed buffers, perf hot paths |
| Generic math (`INumber<T>`) | 11+11 BCL | 7 | polymorphic Mochi numerics |
| Extension members (incl. properties) | 14 | 10 | rarely; for `--emit-style modern` |
| `field` keyword | 14 | 10 | only in user-readable emit mode |
| Partial constructors / events | 14 | 10 | source-generator interop only |
| `nameof` unbound generics | 14 | 10 | diagnostic messages |
| Implicit Span conversions | 14 | 10 | hot path numeric kernels |
| Lambda parameter modifiers | 14 | 10 | high-perf lambdas |

The lower pass switches features on/off via `--lang-version` (mirrors
`<LangVersion>` in csproj). Default is `LangVersion=12` so .NET 8 is
the minimum; `--lang-version=14` unlocks C# 14 features for .NET 10
users. The choice does not change semantics; it changes how readable
the emit looks.

## 8. Tooling and debugging

### 8.1 PDB

Portable PDB is the format. Roslyn emits portable PDB by default;
we pass `DebugInformationFormat.PortablePdb` explicitly anyway
because the legacy Windows PDB format does not survive `dotnet
publish` on Linux/macOS.

Mochi codegen emits `#line` directives in the C# source to map
back to `.mochi` line numbers. Example:

```csharp
public int Foo(int x) {
#line 17 "/abs/path/foo.mochi"
    return x + 1;
#line default
}
```

The portable PDB then carries Mochi line numbers, not C# line
numbers. Stack traces and breakpoints land in the `.mochi` file.

### 8.2 Source Link

Source Link embeds a JSON document in the PDB that lets debuggers
fetch the original source from a URL. Mochi codegen embeds a Source
Link document when given `--source-link <url>`, so a Mochi library
published as a NuGet package debuggable in any consumer's IDE without
shipping the `.mochi` files in the package.

### 8.3 SourceDebugExtension equivalent

The JVM has SourceDebugExtension (JSR 45) for mapping bytecode to
non-Java sources. The CLR's equivalent is the combination of `#line`
directives and the `MethodDebugInformation` table in portable PDB.
For Mochi this is enough; we do not need a separate stratum.

For `--emit-style trace` we also embed the original `.mochi` source
into a custom debug attribute (`MochiSourceAttribute`) so post-hoc
tooling can recover the source without disk access.

## 9. Reflection and NativeAOT guidance

NativeAOT (Mochi target: `dotnet publish -p:PublishAot=true`) closes
the world at compile time. Reflection-based factories that resolve
types or members by name at runtime break unless their reachability
is preserved via attributes.

**Allowed reflection patterns in Mochi codegen.** Mochi emits
reflection only in four shapes, each with a defending attribute:

1. **Lambda factory.** `Delegate.CreateDelegate(typeof(Func<...>),
   target, methodName)`. Used by the closure conversion fallback
   when an open delegate is needed. The `methodName` parameter is
   always a constant string of a generated method name; AOT analyser
   sees the constant and preserves the method.

2. **Generic method instantiation.** `MethodInfo.MakeGenericMethod`.
   Used only when Mochi's `T: any` (uniform representation) escape
   hatch hits at runtime. Decorated with
   `[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicMethods)]`
   on the `Type` parameter. We aim to make this dead in Mochi
   programs typed strictly.

3. **Property accessor lookup for record serialisation.**
   `typeof(T).GetProperties()`. Decorated with
   `[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicProperties)]`.
   Used by Mochi's JSON codec.

4. **Type construction for `typeof` runtime resolution.**
   `Type.GetType(string)`. We avoid this entirely; Mochi's compile
   time resolves type names ahead. The path remains as a debug
   helper, gated by `--debug`.

Any reflection beyond these four shapes is a transpiler bug that
the test harness ([[11-testing-gates]]) catches via the AOT smoke
test on every fixture.

Mochi codegen emits `[UnconditionalSuppressMessage("Trimming",
"IL2026")]` only where one of these four patterns is the cause and
the cause is provably safe. Blanket suppression is forbidden.

## 10. Decision matrix

Score 0 (poor) to 5 (excellent). Higher is better.

| Candidate | stability | doc | AOT | debug | .NET 8+10 | size (small=5) | low complexity |
|-----------|-----------|-----|-----|-------|-----------|----------------|----------------|
| 1. C# source via Roslyn (text) | 5 | 5 | 5 | 5 | 5 | 2 | 4 |
| 2. F# source via FSharp.Compiler.Service | 4 | 4 | 3 | 4 | 5 | 1 | 2 |
| 3. Roslyn SyntaxFactory | 5 | 5 | 5 | 5 | 5 | 2 | 3 |
| 4. Roslyn Source Generators | 5 | 4 | 5 | 5 | 5 | 3 | 1 (wrong shape) |
| 5. Reflection.Emit (runnable) | 5 | 4 | 4 | 2 | 5 | 5 | 2 |
| 6. Reflection.Emit + Lokad.ILPack | 4 | 3 | 4 | 3 | 5 | 5 | 2 |
| 7. PersistedAssemblyBuilder | 5 | 4 | 4 | 3 | 3 (.NET 9+) | 5 | 2 |
| 8. Mono.Cecil | 4 | 4 | 3 | 3 | 5 | 4 | 2 |
| 9. dnlib | 3 | 2 | 3 | 3 | 5 | 4 | 1 |
| 10. AsmResolver | 4 | 3 | 3 | 3 | 5 | 4 | 2 |

The leader is **Roslyn SyntaxFactory (#3)**, tied with **C# source
text via Roslyn (#1)** on every axis except complexity. #3 wins on
correctness (no string-format injection bugs) but loses on raw
typing speed for the implementation team. We adopt #3 by default
and keep #1 as the escape hatch.

For hot-path lowerings, **Reflection.Emit + Lokad.ILPack (#6)**
scores highest on size and complexity at the cost of debug and AOT
support. Combined with #7 once we drop .NET 8, this is the second
tier.

## 11. Final recommendation

**Default:** Roslyn SyntaxFactory plus `CSharpCompilation.Emit`.

- The Mochi lower pass produces a `SyntaxTree` via a thin facade
  over `Microsoft.CodeAnalysis.CSharp.SyntaxFactory`.
- The Mochi compiler invokes `CSharpCompilation.Create(...).Emit(...)`
  to produce a deterministic, portable-PDB-equipped `.dll`.
- `LangVersion` defaults to 12 (so .NET 8 LTS is the minimum target)
  and can be bumped to 14 with `--lang-version=14` for .NET 10 users.

**Hot path escape hatch:** `System.Reflection.Emit` plus Lokad.ILPack
(.NET 8) or `PersistedAssemblyBuilder` (.NET 9+).

- Numeric kernels (tight loops over `Span<T>` of primitives).
- Lambda call sites where the C# compiler would produce a heap
  allocation that we can prove unnecessary.
- Monomorphic `Span<T>` accessors where Roslyn's bounds-check
  elision is conservative.
- Tail calls between Mochi functions where we want to emit `.tail.`
  explicitly.

Mochi codegen tags hot-path methods with a Mochi attribute; the
lower pass routes them through the IL emitter instead of the
Roslyn emitter. The two emitters share the same monomorphisation
and closure-conversion passes; only the leaf-lowering differs.

**Backstop:** if the Roslyn NuGet dependency proves too heavy for
constrained Mochi users (sub-100 MB SDK requirement), fall back to
string-template emission. The fallback path drops SyntaxFactory and
emits C# as text via a `StringBuilder` plus `CSharpSyntaxTree.ParseText`,
then `Emit`. The size win is small (Roslyn dominates, not the
SyntaxFactory layer), so this is a last resort.

We expect the default path to ship for at least the first two MEP-48
phases; the hot-path escape hatch lands in phase 4 once basic
lowerings are stable; the string-template backstop ships only if a
Mochi user explicitly asks for it.

## 12. Implications for sibling notes

- [[01-language-surface]]: the C# 14 surface (collection
  expressions, list patterns) lets the Mochi → C# emit stay readable
  even for complex pattern code, with no loss of generality.
- [[02-design-philosophy]]: same rejection logic as JVM for F#
  source emission; we prefer the host language for transparency.
- [[04-runtime]]: `Mochi.Runtime` for .NET ships as a NuGet package
  referenced by every emitted assembly. Roslyn picks it up via the
  `MetadataReference.CreateFromFile(...)` list.
- [[06-type-lowering]]: details the Mochi → CLR type map; this note
  fixes the syntactic vehicle for that map (records + generics).
- [[07-dotnet-target-portability]]: details the .NET 8 vs .NET 10
  matrix; this note's §7 fixes the `LangVersion` story.
- [[08-dataset-pipeline]]: large-data emit reuses Span<T>; the hot
  path escape hatch in §11 is the IL emitter for those kernels.
- [[09-agent-streams]]: agent streams lower to
  `IAsyncEnumerable<T>`; Roslyn provides the state-machine rewrite,
  no extra work.
- [[10-build-system]]: incremental builds keep a long-running
  `MSBuildWorkspace` plus `CSharpCompilation` in-process to avoid
  cold-start cost per affected module.
- [[11-testing-gates]]: every emit fixture must (a) produce a
  byte-identical assembly under `Deterministic=true`, (b) pass
  ILVerify, (c) pass `PublishAot` smoke test.
- [[12-risks-and-alternatives]]: the Roslyn NuGet size is the
  single biggest risk; the §11 backstop is the mitigation.

## 13. Open questions

1. **NativeAOT for the transpiler itself.** Should `mochic` (the
   Mochi compiler binary) be NativeAOT-published? Roslyn is not
   trim-friendly, so the compiler binary stays JIT for at least
   phase 1; the emitted assemblies are independently AOT-capable.

2. **F# interop.** Mochi programs need to call into F#-defined
   libraries (FSharp.Core types, discriminated unions exposed as
   `FSharpUnion`). Lower pass surfaces this as a separate flag;
   does not affect the default codegen path.

3. **WASM / Blazor.** Mochi-targeting-Blazor would require AOT-
   only output and a more aggressive trimming pass. Out of scope
   for phase 1.

4. **MSIL inline assembly.** Should Mochi expose a `__il` escape
   for hand-written IL fragments, analogous to `__asm` in C? Tempting
   for the same reason as in MEP-45 (numeric kernels), but the IL
   verifier and AOT analyser make it dangerous. Defer to phase 3+.

5. **C# 14 `field` keyword.** Tempting to use in lowered property
   bodies to avoid emitting an explicit backing field. Held back
   until the minimum Mochi target moves to .NET 10 across the board.

## 14. Sources

Roslyn and C# language

- [Microsoft.CodeAnalysis.CSharp 5.3.0 on NuGet](https://www.nuget.org/packages/microsoft.codeanalysis.csharp/)
- [Microsoft.CodeAnalysis 5.3.0 on NuGet](https://www.nuget.org/packages/Microsoft.CodeAnalysis/)
- [Microsoft.CodeAnalysis.CSharp.Workspaces 5.3.0](https://www.nuget.org/packages/Microsoft.CodeAnalysis.CSharp.Workspaces/)
- [What's new in C# 14 (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14)
- [What's new in .NET 10 (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-10/overview)
- [What's new in C# 12 (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-12)
- [Collection expressions spec](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-12.0/collection-expressions)
- [Generating C# code with Roslyn APIs (Jeremy Davis)](https://blog.jermdavis.dev/posts/2024/csharp-code-with-roslyn)
- [Roslyn source generators cookbook](https://github.com/dotnet/roslyn/blob/main/docs/features/source-generators.cookbook.md)
- [Strumenta, Getting started with Roslyn](https://tomassetti.me/getting-started-with-roslyn-transforming-c-code/)

Reflection.Emit and IL persistence

- [System.Reflection.Emit.PersistedAssemblyBuilder class - Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/fundamentals/runtime-libraries/system-reflection-emit-persistedassemblybuilder)
- [PersistedAssemblyBuilder API reference (.NET 9)](https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.persistedassemblybuilder?view=net-9.0)
- [API Proposal: Add PersistedAssemblyBuilder type - dotnet/runtime #97015](https://github.com/dotnet/runtime/issues/97015)
- [System.Reflection.Emit.AssemblyBuilder class - Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/fundamentals/runtime-libraries/system-reflection-emit-assemblybuilder)
- [Lokad.ILPack 0.3.1 on NuGet](https://www.nuget.org/packages/Lokad.ILPack/)
- [Lokad/ILPack on GitHub](https://github.com/Lokad/ILPack)

Third-party IL writers

- [Mono.Cecil 0.11.6 on NuGet](https://www.nuget.org/packages/Mono.Cecil/)
- [Mono.Cecil documentation (Mono Project)](https://www.mono-project.com/docs/tools+libraries/libraries/Mono.Cecil/)
- [dnlib 4.5.0 on NuGet](https://www.nuget.org/packages/dnlib)
- [0xd4d/dnlib on GitHub](https://github.com/0xd4d/dnlib)
- [AsmResolver 5.5.1 on NuGet](https://www.nuget.org/packages/AsmResolver/)
- [AsmResolver.DotNet 5.5.1 on NuGet](https://www.nuget.org/packages/AsmResolver.DotNet/)
- [Washi1337/AsmResolver on GitHub](https://github.com/Washi1337/AsmResolver/blob/master/README.md)

NativeAOT, AOT analysis, and trimming

- [Trimming and AOT compatibility (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/)
- [DynamicallyAccessedMembersAttribute reference](https://learn.microsoft.com/en-us/dotnet/api/system.diagnostics.codeanalysis.dynamicallyaccessedmembersattribute)
- [Source Link specification](https://github.com/dotnet/sourcelink/blob/main/docs/README.md)
- [Portable PDB v1.0 spec](https://github.com/dotnet/runtime/blob/main/docs/design/specs/PortablePdb-Metadata.md)
