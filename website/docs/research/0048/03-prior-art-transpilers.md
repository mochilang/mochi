# MEP-48 research note 03, Prior art on .NET languages and toolchains

Companion note within the MEP-48 series (Mochi-to-.NET/CLR transpiler).
See also: [[01-language-surface]], [[02-design-philosophy]], [[04-runtime]],
[[05-codegen-design]], [[06-type-lowering]], [[07-dotnet-target-portability]],
[[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]],
[[11-testing-gates]], [[12-risks-and-alternatives]], and the MEP-47 sibling
[[../0047/03-prior-art-transpilers]].

This note surveys the .NET language and tooling ecosystem so the MEP-48
design decisions (C# source IR via Roslyn, with a System.Reflection.Emit /
Lokad.ILPack fallback for hot lowerings) rest on a defensible prior-art
base. Format per item is: name, one paragraph project description, key
insight for Mochi.

## 1. The host language family

### 1.1 C# (C# 12, C# 13, C# 14)

C# is the canonical CLR language and Mochi's chosen source IR. Recent
versions are:

- **C# 12** shipped with .NET 8 on November 14, 2023. Headline features:
  primary constructors on classes and structs, collection expressions
  (`[1, 2, ..rest]`), default lambda parameters, inline arrays, ref readonly
  parameters, the `[Experimental]` attribute, type aliases for any type.
- **C# 13** shipped with .NET 9 on November 12, 2024. Features: `params`
  collections (any countable type, not just arrays), the new `lock` object
  type using `System.Threading.Lock`, escape sequence `\e` for ESC, method
  group natural type improvements, implicit indexer access in object
  initializers, the `field` keyword in property accessors (preview).
- **C# 14** shipped with .NET 10 on November 11, 2025. Features promoted to
  GA: extension members (declarations including extension properties and
  static-on-type extensions), `field` keyword on auto-properties,
  null-conditional assignment (`x?.y = z`), modifiers on simple lambda
  parameters, `nameof` over unbound generic types (`nameof(List<>)`),
  user-defined compound assignment operators, partial events and partial
  constructors, more implicit conversions for `Span<T>` / `ReadOnlySpan<T>`.

The Roslyn compiler pipeline (parse, bind, lower, emit) is exposed as a
NuGet library, which is the foundation of MEP-48's codegen path. The
features that matter most for Mochi's lowering:

- **Records** (C# 9+) and **record structs** (C# 10+) give value-equality
  and `with` cloning for free. Mochi's record literals map naturally.
- **Primary constructors** (C# 12+) shrink generated boilerplate when
  lowering record / module init.
- **Pattern matching** (C# 8+, list patterns C# 11, with-expressions on
  structs C# 10) maps Mochi's pattern matcher directly.
- **Required members** (`required` keyword, C# 11) for non-null record
  fields when Nullable is on.
- **File-scoped namespaces** (C# 10) reduce one indent per emitted file.
- **Raw string literals** (`"""..."""`, C# 11) for embedding Mochi source
  in error messages without escaping.
- **Init-only properties** (C# 9) align with Mochi's "fields are immutable
  unless mutated locally" model.
- **Top-level statements** (C# 9) let single-file Mochi scripts compile to
  one `Program.cs` with no class scaffolding.
- **Source generators** (C# 9, incremental in Roslyn 4.0 / Nov 2021) are
  Mochi's escape hatch for compile-time codegen integrated with the
  consumer's IDE.
- **`ref readonly`**, **`scoped`**, **`ImmutableArray<T>` source generators**:
  signal that the runtime is moving toward stack-allocation discipline,
  which influences how Mochi lowers slices and views (see [[06-type-lowering]]).

**Insight**: Pin emitted C# at `LangVersion=12` for the LTS target (.NET 8)
and offer `LangVersion=14` opt-in for .NET 10. Avoid bleeding-edge C# 14
syntax in the default profile until VS 2022 17.10 fades from the field.
The "primary constructor on records + collection expressions" combo is the
sweet spot for compact emission.

### 1.2 F#

F# is Microsoft's ML-family CLR language. F# 8 shipped with .NET 8 (Nov
2023), F# 9 with .NET 9 (Nov 2024), F# 10 with .NET 10 (Nov 2025). It
brings discriminated unions, exhaustive match, immutable-default, units of
measure, type providers, and computation expressions. F# would lower
Mochi's algebraic data types and pure-by-default semantics cleanly. The
catch is that the F# compiler is a separate large dependency, and F#
interop with C#-centric libraries (System.Text.Json, EF Core, ASP.NET
Core) still requires occasional `[<CLIMutable>]` and reference equality
fiddling. Tooling support (Rider, Visual Studio, MSBuild) is good but
markedly thinner than C#.

**Insight**: Same call as MEP-47 made for Kotlin: F# is more elegant for
the lowering pattern but worse for ecosystem reach, debugger UX, and
package compatibility. Reject as primary IR. Keep a future "Mochi-to-F#"
escape hatch for the rare case where DU pattern matching has to round-trip
through a human-readable source.

### 1.3 VB.NET

Visual Basic .NET. Mode-of-life updates were frozen at .NET 5 and Microsoft
publicly stated VB.NET will be kept current with .NET runtime support but
will not gain new language features. Modern .NET 10 still includes VB.NET
in the SDK but few new projects start in it. Acknowledged for completeness;
not a target, not a model.

### 1.4 Boo

Boo is a Python-syntax-flavored CLR language created by Rodrigo B. de
Oliveira in 2003. It enjoyed niche adoption in early Unity scripting
(removed in Unity 5, 2014). The repo at github.com/boo-lang/boo has had
sporadic commits since 2017; effectively dormant. The interesting lesson
is its macro / AST-rewriting pipeline (`Boo.Lang.Compiler.Steps`), which
was an early demonstration of "user programs run inside the compiler" on
the CLR.

**Insight**: Macro pipelines are seductive but hard to maintain when
runtime support drifts. Keep Mochi's "macros" out of MEP-48 scope.

### 1.5 Nemerle

Nemerle was an ML-family / hybrid language with strong macro support, run
by JetBrains for a few years and then handed back to a small academic
community. Last meaningful release was around 2017. Compiles to CLR via
its own front end. Cited in compiler-design papers for being a good
teaching tool. Not active.

**Insight**: Echo of Boo. Strong macros tax the maintainer.

### 1.6 IronPython and IronRuby (DLR)

IronPython is a Python 3 implementation on the CLR built on the Dynamic
Language Runtime (DLR). Releases of interest:

- IronPython 3.4.0, December 12, 2022, targets .NET Framework 4.6.2, .NET
  Core 3.1, and .NET 6.
- IronPython 3.4.1, July 12, 2023, adds UTF-8 mode and `typing` module
  pieces.
- IronPython 3.4.2, December 20, 2024, fixes a blocker that prevented use
  on .NET 9. Targets .NET Framework 4.6.2, .NET 6, .NET 8.
- The `main` branch at github.com/IronLanguages/ironpython3 already targets
  .NET 10.0 and .NET Standard 2.0.

IronRuby has been dormant since 2010. Its codebase still ships as part of
the IronLanguages org but no releases are active in 2026.

The DLR remains the canonical "implement dynamic language on CLR"
substrate. Key DLR concepts:

- **`DynamicMetaObject`**: per-receiver behavior abstraction (think Smalltalk
  meta-objects).
- **`CallSite<T>`**: polymorphic inline caches for dynamic method calls.
- **Expression trees**: a hosted AST that the DLR compiles to IL via
  `Expression.Compile()`. The same expression-tree machinery now powers EF
  Core's query providers, MVC model binding, and a chunk of `System.Linq`.

**Insight**: Mochi is statically typed; the DLR is overkill for the steady
state. But if Mochi ever needs a `dynamic` escape hatch (it shouldn't in
v1), the DLR is the right answer. The lessons that *do* apply: (a)
polymorphic caching is more important than naive dispatch when the call
site has limited shape diversity, (b) expression trees are a great
"sub-IR" for hot kernels because they compose with `Expression.Compile()`
and skip the C# source layer.

### 1.7 ClojureCLR

ClojureCLR is David Miller's CLR port of Clojure, in active maintenance
since 2010. It compiles via `System.Reflection.Emit` and a custom IL
generator. Mature, but the community is roughly two orders of magnitude
smaller than Clojure-on-JVM. Targets .NET 6+ in 2026 releases.

**Insight**: One person can keep a CLR port of a serious language working
for fifteen years. The pain points reported repeatedly: (a) Reflection.Emit
APIs change subtly across .NET releases (notably `DynamicMethod` lifetime
and `AssemblyBuilder.Save` going away), (b) the JVM has better
mature-language tooling for AOT / records / value types because the CLR
side has been slower to land equivalents. MEP-48 should expect to track
the moving target.

### 1.8 Oxygene (RemObjects Pascal)

Oxygene is a commercial object-Pascal-derived language from RemObjects
that targets CLR, JVM, Cocoa native, and WebAssembly. It compiles by
re-using the platform-native object models, so its codegen has to do
considerable work to expose Pascal semantics over those substrates. The
"Elements" toolchain (Oxygene, Hydrogene, Iodine, Mercury, Silver, Gold)
demonstrates a multi-target transpiler that is funded as a niche
commercial product.

**Insight**: Multi-target transpilers can ship and sustain a business if
they pick consistent, conservative abstractions and lean on the host
language's standard library rather than reinventing collections. Mochi's
"emit idiomatic host code" choice is the same trade-off Oxygene makes.

## 2. Runtime history

### 2.1 Mono history

Mono began in 2001 as Miguel de Icaza's reimplementation of the CLR for
Linux. It became the basis for Xamarin's mobile runtimes (iOS, Android,
Mac) and after Microsoft's 2016 acquisition of Xamarin, was rolled into
Microsoft as a first-party project. On **November 10, 2020**, Microsoft
announced .NET 5.0 at .NET Conf 2020, the formal unification of .NET Core
and Mono into a single "one .NET" product (with full Xamarin integration
deferred to .NET 6 in November 2021). In 2026, Mono remains the iOS,
WebAssembly, and Android execution path for .NET (CoreCLR still being the
default for server, console, desktop). The Mono runtime sources live in
`dotnet/runtime/src/mono`.

**Insight**: Mochi's "one runtime story" cannot pretend that CoreCLR is
the only runtime. On iOS the AOT-only Mono variant is what actually
executes. See [[07-dotnet-target-portability]].

### 2.2 Xamarin and MAUI

Xamarin ended formal support on May 1, 2024. .NET MAUI is the official
successor, integrated into the unified .NET SDK starting with .NET 6 (Nov
2021). MAUI on .NET 10 LTS runs:

- Mono runtime on iOS, with full AOT.
- Mono runtime on Android (Android Runtime hosts it).
- CoreCLR on Windows (WinUI 3 / UWP shells).
- CoreCLR on macOS Catalyst.

Xamarin.Forms (the cross-platform UI atop classic Xamarin) is dead.
Existing Xamarin apps either migrate to MAUI or risk store delisting under
Apple's "latest SDK" requirements.

**Insight**: Mochi's "mobile" story (if any) is MAUI + Mono + AOT, not
some bespoke runtime. We do not need MAUI support in v1, but we must not
emit code that breaks under AOT trimming (no `MakeGenericMethod` on
arbitrary user types, no late-bound reflection of trimmed members).

### 2.3 Unity

Unity is the dominant CLR-hosted game engine. Historical context:

- Unity 4 through Unity 2017 used a forked Mono 2.x as scripting runtime,
  pinned at C# 4 effectively.
- Unity 2018 added an opt-in .NET 4.x-equivalent runtime (Mono with a
  newer C# compiler), generally settling at C# 7.3.
- Unity 2021 LTS bumped the C# baseline to C# 9.
- Unity 6 (released October 2024 LTS) keeps Mono at runtime but raises C#
  features to C# 9 by default. The CoreCLR migration is staged in
  follow-on releases: Unity has publicly committed to making CoreCLR the
  scripting runtime in **Unity 6.8** (target during 2026 to early 2027),
  with Mono fully removed at that point.
- IL2CPP is Unity's IL-to-C++ transpiler, used for iOS, WebGL, consoles,
  and any platform where JIT is forbidden. It is one of the most-deployed
  CLR transpilers in the world.

**Insight**: Two relevant lessons. First, "ship a stable runtime and hold
the C# version steady" is a viable strategy for a sprawling ecosystem
(Unity froze at C# 9 for five-plus years). Mochi can take the same stance
on the emitted C# version. Second, IL2CPP demonstrates that re-targeting
IL to a different lower-level form (C++) is technically tractable and
delivers a real shipping product. If Mochi ever needs a "no JIT" deploy
target beyond NativeAOT, the IL-to-C++ pattern is the prior art to study.

### 2.4 Godot Engine (.NET integration)

Godot 4.x supports C# scripting via the .NET 6+ runtime (Godot 4.0 needed
.NET 6, Godot 4.3 added support for .NET 8). Godot historically hosted
Mono and switched to the unified .NET when it became viable. The C#
binding is generated from Godot's native API.

**Insight**: Mochi is not in the game-engine space, but Godot's choice
("use Mono where mature, CoreCLR where supported") echoes our portability
plan in [[07-dotnet-target-portability]].

### 2.5 Blazor WebAssembly

Blazor is ASP.NET Core's WebAssembly target. Architecture:

- The Mono runtime is compiled to WASM (`dotnet.wasm`) and shipped as a
  small JS loader plus IL payloads.
- Two execution modes: **interpreted** (Mono interprets IL inside the
  browser, slower startup and steady state, smaller download) and **WASM
  AOT** (Mono AOT-compiles each IL method to a WASM function, large
  download, fast steady state).
- Blazor United, in .NET 8 (Nov 2023), unifies server-side rendering with
  client-side interactivity (Server and WASM modes selectable per
  component).
- .NET 10 ships major improvements to Blazor WASM size: the runtime is
  reported as up to 76% smaller download than .NET 8 for AOT scenarios.

**Insight**: If Mochi ever wants a "ship a Mochi program to a browser"
story, Blazor WASM is the cleanest path (emit a .NET assembly, let Mono
WASM execute it). It is also a useful counterexample: the runtime download
is non-trivial, so this is not the right target for "tiny demo
artifacts." See [[07-dotnet-target-portability]] and [[10-build-system]].

### 2.6 .NET Framework legacy (4.x)

.NET Framework 4.8 / 4.8.1 remains supported as a Windows component
through at least 2031 (security patches only). It is frozen at C# 7.3
for compiler defaults. Modern .NET targets ignore it.

**Insight**: Mochi targets .NET 8 LTS and later. We do not need to
generate code that runs on .NET Framework. The compatibility cost is
disproportionate to the benefit.

## 3. Compilation infrastructure

### 3.1 Roslyn (`Microsoft.CodeAnalysis.CSharp`)

Roslyn is the Microsoft-owned, open source C# (and VB) compiler exposed as
a library. The relevant types live in the
`Microsoft.CodeAnalysis.CSharp` NuGet package:

- `CSharpSyntaxTree.ParseText(string source)` to parse.
- `CSharpCompilation.Create(name, syntaxTrees, references, options)` to
  build a compilation.
- `Compilation.Emit(Stream peStream, Stream pdbStream)` to write a PE
  (DLL/EXE) and a portable PDB.
- `SemanticModel` to query types, symbols, conversions.

Roslyn's pipeline mirrors a textbook compiler: lex, parse, bind, lower
(several passes including async state machines, iterators, expression
trees, pattern matching), emit. Each stage is observable via the public
API.

Roslyn versions track the C# language version. Roslyn 4.0 (November 2021)
introduced incremental source generators (see 3.5 below).

**Insight**: Roslyn is the single most important dependency for MEP-48.
Emit C# source, hand it to `CSharpCompilation`, get a real PE out. We
inherit the optimizer for free. The drawback is JIT startup cost
(Roslyn's first-call latency is around 300-700 ms), so we cache compiled
artifacts aggressively (see [[10-build-system]]).

### 3.2 System.Reflection.Emit

`System.Reflection.Emit` is the in-process IL emission API:

- `AssemblyBuilder.DefineDynamicAssembly`
- `ModuleBuilder.DefineType`
- `TypeBuilder.DefineMethod`
- `MethodBuilder.GetILGenerator()`
- `ILGenerator.Emit(OpCode, operand)`

It is what IronPython, ClojureCLR, EF Core (when compiling queries),
ASP.NET Core's `DynamicMethodEmitter`, and most JIT-friendly tools use.
Limitations:

- In .NET Framework, `AssemblyBuilder` had a `Save` method that wrote a
  .dll. That method was removed in .NET Core 3.0 and never came back to
  `AssemblyBuilder` itself.
- In .NET 9 (Nov 2024), Microsoft reintroduced **`PersistedAssemblyBuilder`**
  in `System.Reflection.Emit`, which restores the persisted-to-disk
  workflow. This is great news for AOT-style toolchains.

**Insight**: Reflection.Emit is the right answer for hot lowerings where
we want to skip the C# round trip (e.g., tight numeric kernels for the
dataset pipeline, see [[08-dataset-pipeline]]). On .NET 8 we use
**Lokad.ILPack** to persist; on .NET 9+ we use the new
`PersistedAssemblyBuilder`. Either way, the consumer-facing API on the
Mochi side stays "emit a method-builder."

### 3.3 Lokad.ILPack

Lokad.ILPack is a small MIT-licensed library that takes an
`AssemblyBuilder` (or any `Assembly`) and serializes it to a .dll on disk.
Originally created to fill the `AssemblyBuilder.Save` gap on .NET Core 3.0.
Status in 2026: latest release March 11, 2026, ten-plus releases over six
years, active maintenance by Lokad. Used in ML.NET tooling and various
research compilers.

Usage:

```csharp
var generator = new Lokad.ILPack.AssemblyGenerator();
generator.GenerateAssembly(builderAssembly, "/path/Out.dll");
```

**Insight**: On .NET 8 LTS (Mochi's baseline target), Lokad.ILPack is the
required dependency for any "AOT-without-Roslyn" path. Add it to the
`mochi.NET.Emit` runtime helper, document the version pin. On .NET 9+ we
will gradually migrate to `PersistedAssemblyBuilder` but Lokad.ILPack
stays usable.

### 3.4 Mono.Cecil

Mono.Cecil is the canonical read/modify/write IL library, originally
written by Jb Evain inside the Mono project. Used by ILRepack (assembly
merging), JetBrains tooling, NSubstitute (mocking), Cake build addins,
Unity's IL2CPP toolchain, and a thousand other places. Mature, MIT
licensed.

**Insight**: For codegen that produces fresh assemblies (Mochi's normal
mode), Reflection.Emit + Lokad.ILPack is simpler. For post-processing
(weaving, AOP, trimming, instrumentation), Cecil is the tool. We mention
it because (a) we may need it for our test gates ([[11-testing-gates]]) to
verify what shipped, and (b) it shows that IL is a stable, tractable
target despite the runtime evolution.

### 3.5 dnlib

dnlib is another IL read/write library, used by **de4dot** (a deobfuscator)
and **dnSpy** (a popular .NET decompiler and debugger). Comparable to
Cecil; arguments about which is faster / more complete come up
periodically. Cecil is the more conservative pick.

**Insight**: Same as Cecil. We mention it for completeness and to flag
that if Mochi ever ships an IDE plugin that "decompiles" the user's Mochi
back from emitted IL for inspection, dnSpy / dnlib is the prior art.

### 3.6 Source Generators (IIncrementalGenerator)

C# source generators run inside the compilation as Roslyn analyzers,
producing additional `SyntaxTree`s that are compiled alongside user code.
Two API generations exist:

- `ISourceGenerator` (Roslyn 3.x, .NET 5) is the original API. Deprecated
  for new work because every IDE keystroke re-ran the whole generator.
- `IIncrementalGenerator` (Roslyn 4.0, Nov 2021, .NET 6+ tooling) is the
  current API. It uses a value-provider pipeline (`SyntaxProvider`,
  `CompilationProvider`, `Combine`, `Where`, `Select`) where each stage is
  cached on value equality. Only the changed inputs re-run.

In 2026, source generators are mainstream. Major users include:

- `System.Text.Json` (JSON serializer source generator, GA in .NET 6).
- `System.Text.RegularExpressions` (`[GeneratedRegex]`, .NET 7).
- `Microsoft.Extensions.Logging` (`[LoggerMessage]`).
- MVVM Toolkit (CommunityToolkit.Mvvm).
- MediatR-style request/response routing libraries.

**Insight**: Source generators are not the primary Mochi codegen path
(they only run inside *someone else's* C# compilation). But they are
relevant in two ways: (1) Mochi can ship a source generator that lifts
Mochi-fragment string literals or `*.mochi` AdditionalFiles into generated
C#, giving inline interop. (2) The incremental-pipeline model (value
providers, cache by structural equality) is exactly the design we want
for the Mochi compiler driver itself (see [[05-codegen-design]]).

## 4. AOT and deployment models

### 4.1 NativeAOT

NativeAOT compiles managed code ahead-of-time to a single native binary.
Introduced as preview in .NET 7 (Nov 2022), promoted to GA for console
apps in .NET 8 (Nov 2023), ASP.NET Core minimal-API support in .NET 8,
substantial EF Core support arriving in .NET 9 / .NET 10. Enabling is one
property in the `.csproj`: `<PublishAot>true</PublishAot>`.

Key restrictions (still applicable in .NET 10):

- No `MakeGenericMethod` / `MakeGenericType` on types not statically
  reachable.
- No `Assembly.Load`, no dynamically loaded plugins.
- Trimming removes anything not statically reachable; opt-in members via
  `[DynamicallyAccessedMembers]` or `<TrimmerRootDescriptor>`.
- Platform-specific output: cross-OS publish is unsupported.
- Reflection over closed-source types must be annotated; the trimmer warns
  on every potentially unsafe call.

**Insight**: Mochi should be NativeAOT-clean from day one. That means: no
`Type.GetType(string)` calls in generated code, no `Activator.CreateInstance`
of types we don't have a static reference to, generic code paths are
monomorphized at compile time (not at first call). See
[[12-risks-and-alternatives]].

### 4.2 ReadyToRun (R2R)

ReadyToRun is the older AOT approach: precompile IL to native code but
keep the IL in the assembly as fallback. The native code reduces JIT
startup latency without sacrificing late-binding flexibility. Used by
ASP.NET Core deployments, the .NET runtime itself ships R2R'd. Generated
by **crossgen2** (the modern R2R compiler, see 4.3). R2R artifacts are
larger than pure IL but smaller and more flexible than NativeAOT.

**Insight**: R2R is the safe default for server deployments where AOT
restrictions hurt. Mochi's "publish to server" target should emit
`<PublishReadyToRun>true</PublishReadyToRun>` unless the user explicitly
selects NativeAOT.

### 4.3 CrossGen2

CrossGen2 is the .NET Core 3.0+ replacement for the older CrossGen tool;
it is the R2R compiler. Lives in `dotnet/runtime` as `crossgen2`. Used
implicitly by `dotnet publish -p:PublishReadyToRun=true`. Worth knowing
because (a) it is a real IL-to-native compiler we could in principle
target directly, (b) bugs there cost real production hours.

**Insight**: We do not run CrossGen2 ourselves. We trust `dotnet publish`
to do the right thing.

### 4.4 IL2CPP (Unity)

IL2CPP, mentioned in 2.3 above, translates IL to C++ and then to native
code via the platform C++ toolchain. Used everywhere Unity needs JIT-free
execution. Closed source (parts), but the architecture is documented in
Unity blog posts and reverse-engineering writeups.

**Insight**: IL2CPP is what NativeAOT would look like if you wrote C++ as
your IR rather than going straight to native. Useful pattern if Mochi ever
needs "emit C as an alternate IR" (echoes MEP-45's choice; the C-target
work is referenced in [[12-risks-and-alternatives]] as one of the
fallbacks if .NET portability blows up).

## 5. Build, package, and project infrastructure

### 5.1 dotnet CLI

`dotnet` is the canonical command-line interface, shipped since .NET Core
1.0 (June 2016). Commands of interest:

- `dotnet new`, project templates.
- `dotnet build`, `dotnet run`, `dotnet test`.
- `dotnet publish` with `-r linux-x64 -c Release -p:PublishAot=true`.
- `dotnet pack`, NuGet package creation.
- `dotnet add package`, `dotnet add reference`.
- `dotnet tool install -g`, global tools (Mochi's CLI ships as a global
  tool: see [[10-build-system]]).

In .NET 10 (Nov 2025), `dotnet run app.cs` compiles and runs a single C#
file with no `.csproj` (file-based apps). A `#:package`
directive inside the file references a NuGet package. This is the closest
.NET has come to "script C# like Python."

**Insight**: `dotnet run mochi.cs` is the user-visible model. Mochi can
piggyback by emitting a single C# file in script mode and letting `dotnet`
do the rest. The MEP-48 build system should use `dotnet` rather than
inventing a parallel toolchain.

### 5.2 NuGet

NuGet is the .NET package registry, founded in 2010 by Outercurve
Foundation, now operated by Microsoft. As of 2026, nuget.org hosts over
400,000 unique packages (smaller than Maven Central at roughly 10 million,
larger than Hex.pm). Package format is a .zip with `.nupkg` extension
containing `.nuspec` (XML metadata) and lib/ + runtimes/ + ref/ folders.
Consumed via `<PackageReference Include="X" Version="Y" />` in `.csproj`.
Central package management lives in `Directory.Packages.props`.

**Insight**: Mochi ships a `Mochi.Runtime` NuGet package and at most one
or two helper packages. Use semantic versioning, pin to .NET 8 baseline,
provide `net8.0` and `net10.0` TFM-specific assets if needed. See
[[10-build-system]].

### 5.3 MSBuild

MSBuild is the .NET build system. SDK-style projects (`<Project Sdk="Microsoft.NET.Sdk">`)
hide most boilerplate. Common properties we will set in generated `.csproj`:

- `TargetFramework` (`net8.0`, `net10.0`).
- `Nullable` (`enable`).
- `ImplicitUsings` (`enable`).
- `LangVersion` (pin to `12` baseline).
- `TreatWarningsAsErrors` (`true` for our test gates).
- `PublishAot` / `PublishReadyToRun` for publish-time AOT.

**Insight**: MSBuild XML is verbose and stateful; do not invent custom
targets in the emitted project. Lean on the SDK defaults.

## 6. Standard library primitives that matter for Mochi

### 6.1 `Span<T>`, `Memory<T>`, `ReadOnlySpan<T>`

Stack-allocated references to contiguous memory. Used by `System.Text.Json`,
`Regex` source generation, `ArrayPool`, and most hot-path code in modern
.NET. `Span<T>` cannot be heap-allocated, captured by lambdas, or used in
async methods. `Memory<T>` is the heap-allocatable cousin that converts
back to `Span<T>` for synchronous regions.

**Insight**: Mochi's slice / view types lower to `ReadOnlySpan<T>` where
possible and `ReadOnlyMemory<T>` where lifetime crosses async boundaries.
See [[06-type-lowering]].

### 6.2 `System.Threading.Channels`

Bounded and unbounded MPMC queue primitives, added in .NET Core 3.0 (Sep
2019) and steadily refined since (notably in .NET 6 with
`ChannelReader<T>.ReadAllAsync()`). Used as the canonical concurrent queue
primitive in modern ASP.NET Core and dataflow libraries.

**Insight**: Mochi's mailbox / agent inbox lowers to
`Channel<T>` with bounded capacity by default. See [[09-agent-streams]].

### 6.3 `IAsyncEnumerable<T>`

Introduced in .NET Core 3.0 (Sep 2019) with C# 8. Allows
`await foreach (var x in source)` over async producers. The C# compiler
generates a state machine for `async IAsyncEnumerable<T>` methods.

**Insight**: Mochi streams lower directly to `IAsyncEnumerable<T>`. The
lowering is the path of least resistance and gets us cancellation
(`[EnumeratorCancellation] CancellationToken`) and back-pressure for
free. See [[09-agent-streams]].

### 6.4 `Task`, `ValueTask`, `async`/`await`

The async model. `Task<T>` is heap-allocated; `ValueTask<T>` is a struct
that avoids the allocation when the operation completes synchronously.
The C# compiler emits a state machine class (or struct in
`AsyncMethodBuilder` variations) per async method. The state machine
honours `ConfigureAwait(false)`, `SynchronizationContext`, and the
`TaskScheduler` model.

**Insight**: Mochi's async / spawn / await all lower to standard `Task` /
`ValueTask`. Avoid hand-rolled state machines unless we observe profiling
hot spots.

### 6.5 `System.Collections.Immutable`

`ImmutableArray<T>`, `ImmutableList<T>`, `ImmutableDictionary<K,V>`, and
friends. Used heavily by Roslyn (every `SyntaxNode.ChildNodes()` returns
`ImmutableArray<SyntaxNode>`). Value-equatable when wrapped properly.

**Insight**: Mochi's immutable collection literals lower to
`ImmutableArray<T>.CreateRange` or `ImmutableArray.Create`. See
[[06-type-lowering]] for the table.

### 6.6 `System.Text.Json`

The canonical JSON library on .NET 5+. Source-generator-based serialization
since .NET 6 (`[JsonSerializable]`, `JsonSerializerContext`). NativeAOT-clean
in the source-generator mode. Newtonsoft.Json is still common but no
longer recommended for new code.

**Insight**: Mochi's dataset pipeline ([[08-dataset-pipeline]]) uses
`System.Text.Json` + source generators for any JSON parsing. Never
Newtonsoft.

## 7. Cross-platform reality in 2026

.NET 8, .NET 9, .NET 10 ship with these RIDs available:

- `win-x64`, `win-arm64` (Windows 10 / Server 2016+).
- `linux-x64`, `linux-arm64`, `linux-musl-x64`, `linux-musl-arm64`.
- `osx-x64`, `osx-arm64`.
- `ios-arm64`, `iossimulator-arm64`, `iossimulator-x64`.
- `android-arm64`, `android-x64`, `android-arm`, `android-x86`.
- `browser-wasm` (Blazor WASM, WASI experimental).
- `wasi-wasm` (.NET 8 preview; .NET 9 added more support).

Platforms dropped in recent releases:

- .NET 9 dropped Windows 7 / Windows 8.x support; baseline is Windows 10
  1607 / Server 2016.
- .NET 10 keeps Windows 10+ baseline.
- 32-bit Linux (`linux-x86`, `linux-arm32`) is officially supported but
  on a thinner test matrix.

**Insight**: Mochi targets the eight major desktop / server RIDs in v1
(win-x64/arm64, linux-x64/arm64, osx-x64/arm64, plus musl variants).
Mobile and WASM are stretch targets for v2. See [[07-dotnet-target-portability]].

## 8. Synthesis: top lessons for MEP-48

Distilled to the most actionable items.

1. **C# source is the right primary IR.** Roslyn is the single biggest
   leverage point in the .NET ecosystem: free optimizer, free debugger
   support, free language evolution. Emit `LangVersion=12` for the .NET 8
   baseline. (MEP-47 reached the same conclusion for JVM via Java source.)
2. **Reject F# as primary IR.** Same call as MEP-47's Kotlin rejection.
   F# is elegant but a heavier dependency and worse ecosystem fit.
3. **Pin one runtime, one TFM as the baseline.** .NET 8 LTS, supported
   through Nov 2026 (standard term) or Nov 2028 (extended) is the
   conservative pick. Offer `net10.0` as an opt-in.
4. **Plan for Reflection.Emit fallback from day one.** Even if the steady
   state is "always emit C#," Mochi will eventually want hot kernels via
   IL. On .NET 8 we pair Reflection.Emit with **Lokad.ILPack** to persist;
   on .NET 9+ we migrate to `PersistedAssemblyBuilder`. Keep the surface
   API runtime-agnostic.
5. **Be NativeAOT-clean by default.** No `MakeGenericMethod` over user
   types, no `Type.GetType(string)`, no runtime assembly loading. The
   trimmer and the AOT compiler give Mochi a hard, mechanical correctness
   gate: if it doesn't trim, it doesn't ship. (Compare MEP-47's GraalVM
   native-image stance.)
6. **`IAsyncEnumerable<T>` is the canonical stream type.**
   `System.Threading.Channels.Channel<T>` is the canonical agent mailbox.
   Do not invent parallel abstractions.
7. **Use `System.Collections.Immutable` for Mochi's immutable collections.**
   The runtime helpers and Roslyn already lean on it heavily.
8. **Source generators (`IIncrementalGenerator`) are an escape hatch, not
   the main path.** Useful for inlining Mochi fragments into user C#
   projects, useful as a model for Mochi's own incremental driver.
9. **Track the Mono runtime explicitly.** iOS, Android, WASM all run Mono
   in 2026. Generated code must not assume CoreCLR-only intrinsics. (See
   [[07-dotnet-target-portability]].)
10. **Do not target Unity directly in v1.** Unity's C# baseline is C# 9
    and the CoreCLR migration is staged out to Unity 6.8 (during 2026).
    Mochi-on-Unity would have to back-port to C# 9 and Mono semantics;
    that is a sub-project, not a checkbox.
11. **NuGet is the only distribution channel.** No DLL drops, no custom
    package manager, no GitHub-tag-based fetch.
12. **`dotnet` CLI is the build entrypoint.** Mochi's compiler emits a
    `.csproj` + sources and calls `dotnet build`, `dotnet publish`,
    `dotnet test`. No custom MSBuild targets in user-facing emission.
13. **Lean on `System.Text.Json` source generators for the dataset
    pipeline.** Trim-safe, AOT-safe, fast.
14. **Document the runtime support window.** .NET 8 LTS through Nov 2026
    (standard) / Nov 2028 (extended), .NET 10 LTS through Nov 2028 / Nov
    2030. The Mochi MEP-48 spec must pin this in §1.

## 9. Open questions

For [[02-design-philosophy]]:

1. Do we ship a single "Mochi.NET" runtime NuGet, or split it into
   `Mochi.NET.Runtime` (always required) and `Mochi.NET.Emit` (only when
   the user needs Reflection.Emit / Lokad.ILPack)? The split costs an
   extra package but keeps the trimmer happy.
2. Do we ever expose dynamic features (the DLR / `dynamic`) in Mochi, or
   commit to "static only" v1? The DLR is heavy and incompatible with
   NativeAOT; staying static is the safer default.
3. Do we treat F# as a "non-goal forever" or as a "future escape hatch"?
   The decision shapes how much we constrain Mochi's surface to be
   C#-isomorphic.

For [[05-codegen-design]]:

4. Where do we draw the Roslyn-vs-Reflection.Emit boundary in v1? "Always
   Roslyn" is the simplest position. "Switch to Reflection.Emit for hot
   kernels measured via [[11-testing-gates]]" is more flexible but
   doubles the codegen surface area.
5. Do we use `PersistedAssemblyBuilder` (.NET 9+) or Lokad.ILPack (.NET
   8+) when we want a persisted assembly without Roslyn? Pick one or
   support both.
6. Should Mochi ship its own `IIncrementalGenerator` for embedding Mochi
   fragments into C# files, or push users to a separate `dotnet mochi
   build` step?
7. How do we expose Reflection.Emit's expression-tree path
   (`System.Linq.Expressions.Expression`) for users who want "compile
   this Mochi function to a delegate at runtime"? This is the EF Core
   pattern; could be valuable for the dataset DSL.

## 10. Sources

- Microsoft Learn, "The history of C#", https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history
- Microsoft Learn, "What's new in C# 14", https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14
- Visual Studio Magazine, ".NET 5 Arrives" (Nov 10, 2020), https://visualstudiomagazine.com/articles/2020/11/10/net-5-ga.aspx
- .NET Blog, "Announcing .NET 5.0", https://devblogs.microsoft.com/dotnet/announcing-net-5-0/
- .NET Blog, "Introducing .NET 5", https://devblogs.microsoft.com/dotnet/introducing-net-5/
- Visual Studio Magazine, "What's New for C# 14 and F# 10 in .NET 10", https://visualstudiomagazine.com/articles/2025/11/17/hats-new-for-c-14-and-f-10-in-net-10.aspx
- Roslyn project, "Incremental Generators design document", https://github.com/dotnet/roslyn/blob/main/docs/features/incremental-generators.md
- Andrew Lock, "Source generator updates: incremental generators", https://andrewlock.net/exploring-dotnet-6-part-9-source-generator-updates-incremental-generators/
- Lokad.ILPack repository, https://github.com/Lokad/ILPack
- NuGet Gallery, "Lokad.ILPack", https://www.nuget.org/packages/Lokad.ILPack/
- Libraries.io, "Lokad.ILPack", https://libraries.io/nuget/Lokad.ILPack
- Microsoft Learn, "Native AOT deployment overview", https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/
- Microsoft Learn, "ASP.NET Core support for Native AOT", https://learn.microsoft.com/en-us/aspnet/core/fundamentals/native-aot
- .NET Blog, "How to make libraries compatible with native AOT", https://devblogs.microsoft.com/dotnet/creating-aot-compatible-libraries/
- Microsoft .NET, "The official Xamarin support policy", https://dotnet.microsoft.com/en-us/platform/support/policy/xamarin
- Unity Discussions, "CoreCLR and .NET Modernization, Unite 2024", https://discussions.unity.com/t/coreclr-and-net-modernization-unite-2024/1519272
- Unity Discussions, "Path to CoreCLR, 2026: Upgrade Guide", https://discussions.unity.com/t/path-to-coreclr-2026-upgrade-guide/1714279
- IronPython project page, https://ironpython.net/
- IronPython releases, https://github.com/IronLanguages/ironpython3/releases
- IronPython on Wikipedia, https://en.wikipedia.org/wiki/IronPython
- ClojureCLR repository, https://github.com/clojure/clojure-clr
- Mono.Cecil repository, https://github.com/jbevain/cecil
- dnlib repository, https://github.com/0xd4d/dnlib
- dnSpy repository, https://github.com/dnSpyEx/dnSpy
- .NET runtime repository (Mono in `src/mono`), https://github.com/dotnet/runtime
- .NET 10 release notes (Nov 11, 2025), https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-10/overview
- Microsoft, "Microsoft .NET 8 release", https://devclass.com/2023/08/03/microsoft-net-8-gets-november-release-date-devs-hope-for-stable-maui/
- NDepend Blog, ".NET Native AOT Explained", https://blog.ndepend.com/net-native-aot-explained/
- RemObjects Oxygene, https://www.remobjects.com/elements/oxygene/
- Boo language repository, https://github.com/boo-lang/boo
- Nemerle repository, https://github.com/rsdn/nemerle
- Godot Engine documentation, "C# / Mono", https://docs.godotengine.org/en/stable/tutorials/scripting/c_sharp/index.html
- Blazor WebAssembly overview, https://learn.microsoft.com/en-us/aspnet/core/blazor/hosting-models
