# MEP-48 research note 02, Design philosophy

Author: research pass for MEP-48 (Mochi to .NET/CLR transpiler).
Date: 2026-05-23 (GMT+7).

This note records the *why*. It is the design-rationale charter for MEP-48
and explicitly contrasts the .NET target with the C target (MEP-45), the
Erlang/BEAM target (MEP-46), and the JVM target (MEP-47). All four backends
share a frontend (parser plus type checker), share a correctness gate
(byte-equal stdout against vm3), and share the fixture corpus, but their
runtime models, distribution shapes, and operational profiles are deeply
different. This note states the position MEP-48 takes on each axis.

The TL;DR position:

- **.NET is the right fourth target after C, BEAM, and JVM** because it
  complements each: C buys distribution shape (single-file native binary)
  and ceiling performance; BEAM buys fault tolerance, hot reload, and
  distribution-transparent actor semantics; JVM buys the largest
  third-party-library ecosystem in software history; .NET buys
  best-in-class Windows desktop integration, deep Microsoft tooling
  (Visual Studio, Rider, MSBuild), the cleanest reified-generics story
  in any mainstream managed runtime, the strongest LINQ-as-canonical-
  query story, NativeAOT for sub-50ms cold starts in a single binary,
  and access to the second-largest Fortune-500 corporate-IT install
  base after JVM.
- **.NET 8 LTS is the floor; .NET 10 LTS is fully supported.** .NET 8
  (Nov 2023) is the first LTS in the unified ".NET" era after Mono
  unification (announced 2020-11-10 at .NET Conf 2020), and is the
  first LTS to ship NativeAOT with library-mode support, `Channel<T>`
  maturity, `OrderedDictionary` (.NET 9 preview), `FrozenDictionary`,
  `System.Threading.Channels` with `BoundedChannelOptions`, full
  `IAsyncEnumerable` library surface, and C# 12 (primary constructors,
  collection expressions, inline arrays). .NET 10 LTS (released
  2025-11-11) ships C# 14 with extension types, refined NativeAOT,
  `OrderedDictionary<K,V>` in the BCL, and `dotnet run app.cs`
  file-based-app mode. The 8 floor is a hard requirement; the 10
  ceiling is recommended for new projects.
- **The IR-layer choice is deferred to note 05** (codegen design),
  which surveys C# source via Roslyn `SyntaxFactory`, IL emit via
  `System.Reflection.Emit`, Lokad.ILPack persistent assembly emit,
  Mono.Cecil, dnlib, and source generators (`IIncrementalGenerator`).
  The recommendation lands in note 05 §10 and is reflected in the
  spec body (MEP-48 §5). Position taken here: we will NOT emit F#
  source as the IR (extra compiler dependency, locks us to F#
  evolution, F# has its own type system that does not match Mochi).
  The choice is between C# source and direct IL emit, with a likely
  hybrid: C# source for ordinary code through Roslyn, direct IL via
  `System.Reflection.Emit` (or `PersistedAssemblyBuilder` since .NET 9)
  for tight-loop hotpaths and the Mochi runtime's invoke trampolines.
- **Reuse NuGet wholesale.** The Mochi runtime package
  (`Mochi.Runtime`) is a thin shim over the BCL plus a tiny number of
  vetted dependencies (`System.Text.Json` for JSON/CSV, `YamlDotNet`
  for YAML, optional `BouncyCastle.Cryptography` for crypto extras).
  The runtime is published to NuGet; users add one
  `<PackageReference>`.
- **Three deployment shapes:** framework-dependent dll/exe (default,
  runs on any installed .NET runtime), self-contained publish (target
  RID, no host .NET runtime requirement), and NativeAOT single-file
  binary (sub-50ms startup, no JIT). Different users, different
  tradeoffs; the build driver picks per `--target=` flag.
- **Differential testing against vm3 is the master gate.** Same as
  MEP-45, MEP-46, and MEP-47. vm3 is the recording oracle; the .NET
  artefact's stdout must diff byte-for-byte against `expect.txt` for
  every fixture, on .NET 8 and .NET 10, on x86_64-linux,
  aarch64-darwin, and x86_64-windows at minimum.

## 1. Why .NET is the right fourth target

Mochi already has three complementary first-class targets (C, BEAM,
JVM) landing through MEP-45, MEP-46, and MEP-47. .NET completes the
managed-runtime quadrant for several reasons that no prior target
covers:

Concretely, in 2026:

- NuGet, the canonical .NET package host, contains more than 6 million
  unique package versions across ~500,000 distinct package IDs. While
  it is smaller than Maven Central, the relevant subset for the Mochi
  workload (JSON, HTTP, crypto, datalog, observability, AI clients)
  is essentially complete.
- .NET is the dominant runtime in Windows enterprise IT, in many
  financial-services back-offices, in healthcare integration, in
  large segments of game development (Unity has ~50% of the
  professional indie market and dominant mobile-game share), and
  across Microsoft's first-party services (Azure, Office, Xbox
  Cloud, Bing, GitHub). The Fortune 500 install base is comparable
  to JVM in absolute terms, larger in some verticals.
- C# 12, 13, and 14 (Nov 2023, Nov 2024, Nov 2025) have added
  features Mochi can lower onto cleanly: primary constructors for
  records, collection expressions, inline arrays, list patterns
  (C# 11), required members, file-scoped types, extension types
  (C# 14 preview). The language has caught up with and in some
  places surpassed Kotlin and Scala on conciseness.
- CLR generics are *reified*, unlike JVM type erasure. A
  `List<int>` at runtime is an actual `System.Collections.Generic.List<int>`
  with full type information. This eliminates an entire class of
  workaround the JVM target had to ship (MEP-47 note 06 §3). For
  Mochi this is a substantial simplification: type-driven dispatch
  works at runtime without per-instance type tags.
- NativeAOT (Native Ahead-of-Time) is in GA since .NET 7 and matured
  significantly in .NET 8/9/10. It produces single static native
  binaries comparable in size and startup to GraalVM native-image,
  without the closed-world constraint penalty being quite as harsh
  (NativeAOT is closed-world too, but its trimming and ILC produce
  smaller binaries for typical .NET workloads).
- C# `async`/`await` is the canonical async story in the industry. It
  pre-dates Loom by years and shipped GA in 2012. The Mochi agent
  and stream model maps onto `System.Threading.Channels` plus
  `Task`/`ValueTask` natively. We pay the function-coloring cost (red
  vs blue functions) but inherit the entire BCL's async surface.

For Mochi specifically, .NET offers the following capabilities that
neither C, BEAM, nor JVM can match:

- **Reified generics.** Unlike Java's type erasure, CLR generics carry
  full type information at runtime. Mochi `list<int>` is a
  `List<long>` whose elements are unboxed `long` primitives, not
  boxed `Long` objects. Reflection-based dispatch in the agent
  supervisor works without per-instance carrier types.
- **Value types as first-class citizens.** C# `struct`, `record struct`,
  and `readonly struct` are stack-allocated value types with no GC
  pressure. Mochi small records (Option<int>, Point, ValueTuple-like
  records) can lower to value types, eliminating heap allocation for
  the common case. This is closer to MEP-45's C struct story than to
  JVM's reference-only records (until Valhalla GAs).
- **LINQ as canonical query target.** The Mochi query DSL is almost
  isomorphic to LINQ method-syntax. We get a battle-tested query
  engine, query-comprehension-to-method-call rewriting handled by
  Roslyn, and a parallelism story (PLINQ) for free.
- **System.Threading.Channels.** A first-class, BCL-supported
  back-pressure-aware channel implementation, used by Microsoft's own
  ASP.NET Core, Orleans actor framework, and the dotnet runtime. The
  Mochi agent mailbox lowers to one Channel<TMessage> per agent.
  Bounded vs unbounded is a one-flag choice.
- **`Span<T>` and `Memory<T>`.** Zero-copy slice types over arrays and
  unmanaged memory, with compiler-enforced lifetime rules
  (`ref struct`). The Mochi string runtime uses `ReadOnlySpan<byte>`
  for UTF-8 hot paths, eliminating allocations that JVM and BEAM
  cannot.
- **Visual Studio, Rider, VS Code.** Microsoft, JetBrains, and the
  C# Dev Kit team ship best-in-class IDE support, with refactoring,
  debugging, and roslyn-powered analyzers. Mochi-on-.NET users get a
  professional IDE story out of the box.
- **Cross-platform via dotnet CLI.** A single `dotnet publish -r linux-x64`
  or `-r win-arm64` or `-r osx-arm64` produces a self-contained
  artefact. The RID matrix is comprehensive and stable.

The MEP-45 C target solves the *distribution shape and ceiling
performance* problem. The MEP-46 BEAM target solves the *fault
tolerance and operational profile* problem. The MEP-47 JVM target
solves the *ecosystem and toolchain reach* problem for the JVM
quadrant. The MEP-48 .NET target solves the *Windows enterprise and
reified-generics-plus-value-types* problem for the .NET quadrant.
They are complementary; users pick by deployment context, library
access requirements, and team familiarity.

## 2. Why .NET 8 LTS as the floor

.NET 8 (Nov 2023) consolidated the language features Mochi needs:

- **Records (positional and nominal)** (C# 9, finalised C# 12): the
  natural lowering for Mochi records. Structural equality, hash,
  `ToString`, deconstruction, and `with` expressions for free. Both
  `record class` and `record struct`.
- **List patterns** (C# 11): the natural lowering for Mochi
  `let [a, b] = xs`. The `is [_, _, var x, ..]` syntax matches
  `ImmutableList<T>` via `IEnumerable<T>`. (For our default
  immutable type we ship a helper deconstructor since `ImmutableList`
  itself does not directly support C# list patterns without
  custom support.)
- **Sealed classes and `is` pattern matching** (C# 7+ enhanced
  through C# 12): the lowering for Mochi sum types. Mochi
  `type R = | A | B { x: int }` becomes a sealed abstract base
  record plus per-case record classes, with `match` lowered to a
  switch expression.
- **`System.Threading.Channels`** (.NET Core 3.0, matured in .NET 8):
  the natural lowering for Mochi agents and stream queues. Bounded
  channels with `BoundedChannelOptions` provide back-pressure;
  unbounded channels are equivalent to BEAM mailboxes.
- **NativeAOT** (.NET 7 GA, hardened in 8): closes the
  distribution-shape gap with MEP-45. A Mochi-on-.NET hello-world
  built with `dotnet publish -p:PublishAot=true` is a ~3-6MB static
  binary, comparable to MEP-45's C output and smaller than GraalVM
  native-image, with startup under 30ms.
- **`IAsyncEnumerable<T>`** (C# 8, library completion in .NET 8):
  the natural lowering for Mochi cold streams. `await foreach` is
  the consumer side.
- **Source generators** (Roslyn 4.0, refined in 4.x): allow Mochi
  build-time code injection without runtime reflection, which is
  load-bearing for NativeAOT compatibility.

.NET 6 LTS (Nov 2021) is end-of-support by November 2024 and is
not a target. .NET 7 was an STS release (18-month support) and
expired in May 2024. .NET 8 is the first LTS in the post-Mono-
unification era that is still in support during MEP-48's life.

.NET 10 LTS (Nov 2025) is fully supported and is the recommended
runtime for new Mochi-on-.NET projects. .NET 10 adds:

- **C# 14 with extension types** (preview): nominal extensions to
  existing types without inheritance, useful for the Mochi runtime
  to attach methods to BCL collections without wrappers.
- **`OrderedDictionary<K,V>`** in the BCL (added .NET 9): clean
  mapping for Mochi maps that need insertion-order iteration. On
  .NET 8 we ship a wrapper in `Mochi.Runtime.OrderedMap<K,V>`.
- **`dotnet run app.cs`** file-based app mode: a Mochi build can
  produce a single-file C# script that runs without an explicit
  csproj. Useful for the `mochi run` command's fast path.
- **NativeAOT improvements**: smaller binaries, faster ILC, better
  trim warning diagnostics.

.NET 11 STS (Nov 2026) and .NET 12 LTS (Nov 2027) are not yet
released; MEP-48 commits to forward compatibility but tests on the
developer-preview builds in CI.

The version-matrix policy is documented in [[07-dotnet-target-portability]] §1.

## 3. Why a survey-driven codegen IR choice

The BEAM target had a defensible default (Core Erlang via `cerl`).
The C target had a defensible default (custom `aotir` IR plus C
emission). The JVM target had a survey-driven hybrid (Java source
plus ClassFile API). The .NET target has at least nine plausible
codegen front doors, each with substantial real-world precedent:

1. C# source text via Roslyn `SyntaxFactory` lowered to `Compilation`.
2. C# source text written to disk and shelled out to `dotnet build`.
3. F# source. (Reject as default; locks us to F# evolution and
   forces a reconciliation of F# semantics with Mochi's.)
4. VB.NET source. (Reject as default; legacy, declining.)
5. IL emit via `System.Reflection.Emit` (in-memory `AssemblyBuilder`).
6. IL emit via `PersistedAssemblyBuilder` (added .NET 9, replaces
   the older `AssemblyBuilderAccess.Save` mode).
7. IL emit via Lokad.ILPack (third-party, persists in-memory
   `AssemblyBuilder` outputs; still actively maintained as of 2026).
8. IL emit via Mono.Cecil (read/write assembly editor).
9. IL emit via dnlib (alternative Cecil-replacement, broader feature
   support).
10. A custom IR (`aotir`-style) lowered to IL via path 5/6.
11. A Roslyn `IIncrementalGenerator` source generator (build-time only).

The choice is non-trivial: it changes which CLR features we can
exercise (DynamicMethod, CallSite caching, custom marshalling), how
much work the existing Roslyn pipeline does for us (analyzers, code
fixes, semantic refactoring), how much control we keep over IL
layout and PDB information, and whether the result is friendly to
NativeAOT's trimming.

[[05-codegen-design]] surveys all eleven, scores them on a decision
matrix, and recommends a hybrid: **Roslyn `SyntaxFactory` for the
ordinary lowering, plus `System.Reflection.Emit`
(`PersistedAssemblyBuilder` on .NET 9+) for invoke trampolines and
a small set of hot lowerings where skipping the C# round-trip is a
clear win.** The hybrid is the only choice that keeps debuggability
(line numbers come for free from Roslyn), inherits Roslyn's
optimisations (constant folding, dead-code elimination, async-state-
machine lowering, expression tree lowering), AND gives us direct
control where we need it (the agent supervisor's invoke trampoline
must call into arbitrary user-defined `OnXxx` handlers without
reflection at runtime, which is a closed-world cliff for NativeAOT
unless we emit the trampoline ourselves).

The position taken in note 02 is therefore: we do not pre-commit;
the codegen IR is a defensible engineering choice that note 05
makes with full data. The spec body (MEP-48 §5) cites note 05's
recommendation as normative.

## 4. Why reuse NuGet wholesale

The Mochi runtime package (`Mochi.Runtime`) is a *thin shim*, not a
re-implementation:

| Mochi concept             | .NET construct or library used                              |
|---------------------------|-------------------------------------------------------------|
| agent                     | `System.Threading.Channels.Channel<TMsg>` + `Task.Run`      |
| stream                    | `IAsyncEnumerable<T>` + `System.Linq.Async`                 |
| supervision               | `TaskScheduler.UnobservedTaskException` + user supervisor   |
| in-memory query state     | `ImmutableList<T>` / `ImmutableDictionary<K,V>` + LINQ      |
| Datalog fact tables       | `Dictionary<Predicate, HashSet<Tuple>>` + semi-naive runtime |
| persistent in-memory cfg  | `static readonly` fields                                    |
| HTTP client (fetch)       | `System.Net.Http.HttpClient` (BCL)                          |
| JSON                      | `System.Text.Json` (BCL, source-generator-friendly)         |
| YAML                      | `YamlDotNet` (NuGet, vetted)                                |
| CSV                       | `CsvHelper` or `Microsoft.VisualBasic.FileIO.TextFieldParser` |
| TLS, crypto               | `System.Security.Cryptography` (BCL)                        |
| logging                   | `Microsoft.Extensions.Logging` abstraction (BCL-adjacent)   |
| telemetry                 | `System.Diagnostics.DiagnosticSource` + OpenTelemetry SDK   |
| script-style packaging    | `dotnet run app.cs` (.NET 10) or single-file publish        |
| release packaging         | `dotnet publish --self-contained`                           |
| single binary             | NativeAOT (`dotnet publish -p:PublishAot=true`)             |
| FFI to C                  | `[DllImport]` / `LibraryImport` source generator            |
| async I/O                 | `Task` / `ValueTask` / `IAsyncEnumerable` + BCL async APIs  |

The Mochi runtime package adds, in `Mochi.Runtime.*` namespaces:

- `Mochi.Runtime.Core`: print formatters, panic, error conversion.
- `Mochi.Runtime.Str`: UTF-8 string ops, ReadOnlySpan<byte> helpers.
- `Mochi.Runtime.Coll`: OrderedMap<K,V>, OrderedSet<T>, structural
  equality helpers over ImmutableList/Dictionary/HashSet.
- `Mochi.Runtime.Query`: Mochi query DSL runtime helpers (group_by,
  hash_join, sort, set ops over LINQ).
- `Mochi.Runtime.Streams`: cold-stream operators not in
  System.Linq.Async; `window`, `throttle`.
- `Mochi.Runtime.Agents`: agent template, channel mailbox, intent
  dispatch, supervisor.
- `Mochi.Runtime.Datalog`: semi-naive evaluator over Dictionary-of-
  tuples.
- `Mochi.Runtime.Llm`: provider abstraction over HttpClient.
- `Mochi.Runtime.Fetch`: HttpClient wrapper with JSON decode shim.
- `Mochi.Runtime.Ffi`: P/Invoke marshalling helpers.
- `Mochi.Runtime.Test`: xUnit/MSTest-compatible expect/test driver.
- `Mochi.Runtime.Io`: variadic print, per-type formatter dispatch.

Total LOC target for v1: ~5000 lines of C#. This matches MEP-47's
JVM target estimate, since the runtime obligations are essentially
identical. The CLR's reified generics let us drop a few JVM-target
workarounds (no per-type-token threading at agent dispatch), but
we add a few .NET-specific concerns (NativeAOT trim hints, source
generator integration, async/await scope-capture rules).

## 5. Why three deployment shapes

Different users have different shipping needs. MEP-48 supports three:

### 5.1 `mochi build --target=dotnet-fx-dependent` (default)

Produces a framework-dependent .dll plus .exe wrapper containing the
user's compiled assemblies plus all transitive NuGet dependencies in
the publish directory. Runs on any installed .NET 8+ runtime via
`dotnet app.dll` or the platform-specific .exe wrapper.

Hello-world size: ~150 KB (just the user code; the runtime is on
the host). Realistic-app size: ~5 MB (user code plus
NuGet dependencies).

Use case: default. Most Mochi-on-.NET programs ship this way. User
installs a .NET runtime separately (analogous to "install JDK" for
MEP-47's uberjar).

### 5.2 `mochi build --target=dotnet-self-contained`

Produces a self-contained publish directory bundled with the user
code, the .NET runtime, and trimmed BCL assemblies. Targets a
specific RID (e.g., `linux-x64`, `osx-arm64`, `win-x64`). Self-
contained; no host .NET runtime required.

Hello-world size: ~50 MB (self-contained runtime plus user code).
With aggressive trimming (`<PublishTrimmed>true</PublishTrimmed>`):
~15 MB.

Use case: server deployments where "no .NET runtime on the host"
is a requirement; air-gapped environments; containerised services
where the base image does not include .NET.

### 5.3 `mochi build --target=dotnet-aot`

Produces a single static native binary via NativeAOT. Closed-world
AOT compilation through ILC (the .NET IL Compiler). Sub-30ms
startup. No JIT, no runtime reflection beyond what is declared in
trim/AOT roots.

Hello-world size: ~3-6 MB (with `-p:PublishAot=true`, default).
Per-arch. Reflection must be declared in source-generator-emitted
trim attributes or `ILLink.Substitutions.xml`. Mochi codegen emits
these automatically.

Use case: CLI tools, serverless functions (Azure Functions
Isolated, AWS Lambda), containerised services where startup time
and memory matter, embedded systems where a full .NET runtime is
too heavy.

### 5.4 `mochi build --target=dotnet-singlefile` (Phase 2)

Produces a single-file executable that extracts to a temp directory
on first run (non-AOT path). Useful when AOT-incompatible features
must be used but a single file is still desired.

### 5.5 `mochi build --target=dotnet-maui` (Phase 3, MEP-48.1)

Mobile / desktop GUI via .NET MAUI. Out of scope for v1; covered in
[[07-dotnet-target-portability]] §8 and [[12-risks-and-alternatives]].

### 5.6 `mochi build --target=dotnet-blazor` (Phase 3, MEP-48.2)

Browser WebAssembly via Blazor WASM. Out of scope for v1; covered
in [[07-dotnet-target-portability]] §9 and
[[12-risks-and-alternatives]].

## 6. Why differential testing against vm3 is the master gate

vm3 is the existing reference implementation. Byte-equal stdout from
the .NET artefact versus vm3, on every fixture, is the strictest
behaviour check available. vm3 is used here only as the recording
oracle for `expect.txt`; the transpiler does not consume any of vm3's
IR, runtime, or codegen. Property tests, fuzzing, and reproducibility
are secondary gates.

This is the same gate MEP-45, MEP-46, and MEP-47 use; sharing the
gate means we share the fixture corpus and the recorded goldens. A
change to a Mochi source file re-records `expect.txt` from vm3 in
one pass, and all four backends are validated against the same byte
sequence.

For .NET-specific test infrastructure we add:

- A per-TFM matrix: `net8.0` (LTS floor), `net10.0` (LTS ceiling),
  and the current developer-preview SDK (warning-only).
- A NativeAOT gate. The fixture corpus must publish with
  `PublishAot=true` and produce byte-equal stdout. Trim warning
  cleanliness is a load-bearing piece.
- A self-contained gate. The fixture corpus must publish with
  `--self-contained` on at least three RIDs and produce byte-equal
  stdout.
- An xUnit pass on the test functions emitted from Mochi `test`
  blocks.

[[11-testing-gates]] details the gates.

## 7. Why NOT compile to F# or VB.NET source

A reasonable alternative would be to lower Mochi to F# source and let
`dotnet build` do the rest. F# has clean discriminated unions,
records, pattern matching, and computation expressions. This was
considered and rejected:

- **F# has its own type system.** F#'s null-safety story (option
  types, fewer nullable references in idiomatic F#), units of
  measure, computation expressions, type providers, and active
  patterns add a layer of semantics Mochi does not have. Lowering
  Mochi to F# forces a reconciliation: how do Mochi sum types map
  to F# discriminated unions? Through a clean isomorphism, yes,
  but at the cost of inheriting F#'s naming conventions and
  surface-syntax constraints.
- **F# compiler as a build-time dependency.** `Fsc.dll` ships with
  the .NET SDK but adds startup cost; we lose the option of
  shelling out to a smaller `csc.exe` for fast script-mode builds.
- **F# evolution risk.** F# evolves on a different cadence than C#
  (F# follows the .NET SDK release cycle but with smaller feature
  drops). Mochi-on-F# would couple us to that cadence.
- **Ecosystem mismatch.** Most .NET enterprise code is C#. NuGet
  packages, ASP.NET integrations, and tooling assume C# as the
  primary host. F# is a healthy but smaller community. Mochi
  outputs that are C# are easier to read, debug, and integrate.
- **No structural-equality wins.** C# records since 9 have nearly
  all the structural-equality goodness of F# records, without the
  cost of switching languages.

VB.NET is similarly rejected on legacy/decline grounds. Visual Basic
remains supported but is in long-term maintenance mode, with no
significant new features since 2017.

The position: emit C# source (or IL directly, per note 05), not
source in another .NET language. C# is the lingua franca of .NET,
the most-used .NET language by an order of magnitude, and the only
one whose evolution is closely coupled with the runtime's.

## 8. Why .NET is *not* the right primary target

Symmetric to §1: things .NET cannot do that C, BEAM, or JVM can.

- **.NET cold-start cost outside NativeAOT.** A cold CoreCLR startup
  takes 50-200ms (faster than JVM but slower than a static C
  binary). With JIT warmup, a few hundred milliseconds more. CLI
  tools that must respond in <30ms need NativeAOT (which is
  supported) or MEP-45's C output (which is faster still).
- **Memory floor outside trimming/AOT.** A minimal .NET 8 self-
  contained resident set is 30-60 MB. NativeAOT cuts this to ~10
  MB. The C target's hello-world is ~3 MB resident; the BEAM
  release is ~30 MB. NativeAOT closes the gap with BEAM and
  approaches but doesn't beat C.
- **Hot reload.** .NET has Hot Reload (announced in .NET 6, refined
  through 8/9/10), but the model is heavy (every assembly must be
  reloadable; method-signature changes require restart). BEAM hot
  reload is first-class and supported by the language. .NET is not
  in the same league for production hot reload, though it's better
  than JVM for developer-loop hot reload.
- **Single-file ship without ANY runtime dependency.** NativeAOT
  closes this gap with the same closed-world constraints as
  GraalVM native-image. The C target's static binary has no
  constraints.
- **Embedded targets without a .NET runtime.** Most
  microcontrollers, most resource-constrained edge devices, lack a
  .NET runtime. NanoFramework targets some MCUs but is a separate
  runtime with its own constraints. The C target reaches further.
- **Library ecosystem size.** NuGet has ~500K packages; Maven
  Central has 10M+. For deep-cut library access (a specific Apache
  Spark connector, a niche cryptographic algorithm), the JVM
  ecosystem wins. NuGet covers the common case very well, but
  Maven Central covers the long tail better.

The four targets are complementary. Users with Windows enterprise
or Unity/Godot game requirements pick .NET. Users with mobile or
Android library-access requirements pick JVM. Users with operational
uptime requirements pick BEAM. Users with embedded or single-binary
requirements pick C. Many non-trivial Mochi programs will eventually
ship two, three, or all four.

## 9. Why this is not "just transpile to C#"

A common shorthand for the project is "transpile to C#." That's
broadly accurate but obscures three load-bearing design choices:

1. **The IR-layer choice matters** (note 05). C# source via Roslyn
   is one defensible choice; direct IL emit is another; agent
   trampolines and tight numeric loops force a hybrid. Calling
   MEP-48 "transpile to C#" pre-commits to source-only emission,
   which is wrong for NativeAOT-incompatible agent dispatch.
2. **The runtime layer is a Mochi-controlled package**, with about
   a dozen modules in the `Mochi.Runtime.*` namespace. Without it,
   Mochi's higher-level features (queries, streams, agents) have
   no place to land. See [[04-runtime]].
3. **The build driver owns the ship-format story** (framework-
   dependent, self-contained, NativeAOT, single-file, MAUI, Blazor),
   the NuGet publishing pipeline, the per-TFM matrix, and the
   reproducibility gate. See [[10-build-system]].

"Transpile to C#" without these three is a toy. MEP-48 specifies
all three.

## 10. Position on .NET ecosystem interoperability

.NET's defining strength after Windows-enterprise reach is library
access via NuGet. Mochi-on-.NET must make this strength reachable
from Mochi code without sacrificing Mochi's type-safety guarantees.
The position:

- **Mochi can `import "dotnet:System.Text.Json"`** to expose a .NET
  type to Mochi. The Mochi type checker reads the assembly's
  metadata (via `System.Reflection.Metadata` or Roslyn
  `MetadataReference`) and constructs Mochi-typed signatures. .NET
  nullable-reference-types are converted to Mochi `Option<T>` at
  the import boundary, with the user choosing the policy in a per-
  import annotation.
- **A Mochi module compiled to .NET IL is callable from C#** via
  standard interop: each Mochi public function becomes a public
  static method on a C# class named after the Mochi module
  (`Mochi.User.<ModuleName>`). Public Mochi types are public C#
  types in the same namespace.
- **The build system surfaces both directions.** `mochi build` can
  produce a .dll plus matching .csproj that references it; Mochi
  can include pre-compiled .dlls or NuGet packages via
  `--with-package=Foo.Bar` or `--with-dll=path/to/lib.dll`. Defer
  surface details to [[10-build-system]].
- **Annotations.** Mochi exposes a `@dotnet(...)` modifier on
  functions and types that controls how they appear from C#:
  visibility, naming, async-ness. Phase 2.

v1 aims for "C# can call Mochi cleanly". v2 aims for "Mochi can
import any NuGet dependency". The deeper integration is sequenced
through the phase plan in the spec body.

## 11. Position on NativeAOT

NativeAOT is .NET's distribution-shape escape hatch. Mochi-on-.NET
commits to making it a first-class supported target:

- **The fixture corpus must publish with `PublishAot=true`** as a
  CI gate ([[11-testing-gates]] §3).
- **The Mochi runtime package ships AOT-friendly source generators**
  and `[DynamicallyAccessedMembers]` attributes, so it works for
  downstream NativeAOT builds without user configuration.
- **Mochi codegen emits trim/AOT roots** as source-generated
  attributes based on the program's actual reflection use (only
  sum-type dispatch and System.Text.Json model classes need
  entries; Mochi has very little intrinsic reflection).
- **Build-time vs run-time class init.** Mochi defaults to run-time
  initialisation (avoids static-init traps for `Mochi.Fetch` and
  `Mochi.Llm`); module initializers are emitted lazily via the
  `ModuleInitializer` attribute only when statically detected as
  pure.

Trade-off: NativeAOT rules out runtime code generation via
`System.Reflection.Emit` and runtime plugin loading via `Assembly.LoadFrom`.
Mochi programs that rely on dynamic class loading (rare; mostly the
LLM provider plugin system) lose this when compiled to NativeAOT.
The build CLI warns when a feature is incompatible with the
selected target.

## 12. Position on Span<T> and Memory<T>

`Span<T>` and `Memory<T>` are .NET's zero-copy slice types. The
Mochi-on-.NET runtime uses them aggressively in the string and byte
hot paths:

- UTF-8 string scans use `ReadOnlySpan<byte>`, not `string`. This
  matches the spec (Mochi strings are UTF-8) and avoids
  encoding-conversion allocations.
- The query DSL's hash-join builders use `Span<T>` over array
  buffers, avoiding `List<T>` resize copies.
- The Datalog runtime's tuple comparison uses `ReadOnlySpan<long>`
  for primitive-keyed tuples.
- The agent mailbox does NOT use Span (channels need ownership
  semantics that `ref struct` cannot model).

`Span<T>` is `ref struct`, which means it cannot be stored in
heap-allocated objects, cannot be used in `async` methods (without
the `[UnscopedRef]` workaround), and cannot cross await boundaries.
The Mochi runtime respects these constraints; the surface language
does not expose Span semantics directly. See [[06-type-lowering]] §10.

## 13. Position on async/await and the function-coloring problem

Unlike JVM Loom, .NET does not have an implicit-blocking story. C#
async/await is the canonical concurrency primitive: a function is
either synchronous (`int Foo()`) or asynchronous (`Task<int> FooAsync()`),
and the two cannot be transparently mixed. This is the "function
coloring" problem.

Mochi does not surface async/await in the language; Mochi code is
written in a synchronous style. The .NET lowering must therefore:

- Compile every Mochi function as `async Task<T>` (red) when the
  function transitively calls any async operation (fetch, agent
  send/await, channel read/write, IO, sleep). The build pass that
  decides this is the "async colouring" pass; see
  [[05-codegen-design]] §12.
- Compile pure (sync-only) Mochi functions as ordinary synchronous
  methods (blue). This preserves performance for tight numeric
  loops and pure transformations.
- Insert `await` calls at every red-call site automatically. The
  user never types `await` in Mochi.
- Treat agent `on` handlers as async by default (since agents may
  await on mailbox reads). Pure handlers can be sync, decided per-
  handler by the colouring pass.

The "no async/await in surface" position is load-bearing.
Languages predating Loom (Kotlin, Rust, JavaScript, C#) had to
invent `suspend`/`async` to escape platform-thread costs. Mochi-on-
JVM with Loom obviates this. Mochi-on-.NET cannot (no Loom
equivalent), so the lowering injects the colouring transparently.

This is the largest *operational* delta relative to MEP-47. The
implementation is detailed in [[09-agent-streams]].

Two caveats:

- **`Task<T>` vs `ValueTask<T>`.** For hot paths where most
  invocations complete synchronously, `ValueTask<T>` avoids the
  allocation. The Mochi codegen uses `ValueTask` for any function
  whose async surface is entirely from `ValueTask`-returning
  primitives; otherwise `Task`.
- **Context-flowing in libraries.** ASP.NET Core, WPF, WinForms
  flow synchronization contexts. The Mochi runtime calls
  `.ConfigureAwait(false)` on every internal await to avoid
  context-pinning. Surface code does not need to know.

## 14. Position on observability and DiagnosticSource

.NET has a first-class observability story through
`System.Diagnostics.DiagnosticSource`, `Activity` (distributed
tracing primitive), and the OpenTelemetry SDK. Mochi runtime emits
diagnostic events for:

- Agent spawn / stop / panic.
- Stream subscribe / unsubscribe.
- Query execution (with input cardinality and duration).
- LLM provider calls.
- HTTP fetch with status code and latency.

These are exposed via `DiagnosticSource` listeners, consumable by
the OpenTelemetry SDK or by `EventSource`-based ETW/EventPipe
tooling. Mochi-on-.NET thus gets first-class observability without
mandating OpenTelemetry. (OpenTelemetry is supported as an opt-in
via a separate `Mochi.Runtime.Telemetry.Otel` NuGet package.)

## 15. Position on Mono vs CoreCLR vs Unity

The .NET runtime quadrant is fragmented across three flavours of
the same CLR:

- **CoreCLR.** The default modern runtime, the focus of MEP-48 v1.
  Maintained by Microsoft, Apache 2.0, ships in `dotnet` SDK.
- **Mono.** The cross-platform legacy CLR implementation, originally
  for Linux/macOS, now unified into the .NET project (announcement
  Nov 2020). Mono is still the JIT used inside Unity (through Unity
  6 LTS) and inside MAUI on iOS (AOT mode). Mono compatibility
  is a Phase-3 secondary gate.
- **Unity's IL2CPP.** Not a CLR at all; transpiles IL to C++ and
  compiles. Used for iOS, WebGL, and console game targets. Unity
  6.8 announced migration to CoreCLR (announced ~2025); MEP-48
  watches but does not target IL2CPP directly.

MEP-48 v1 targets CoreCLR only. Mono and IL2CPP are Phase-3
follow-ups documented in [[07-dotnet-target-portability]] §6 and
[[12-risks-and-alternatives]].

## 16. Summary of position

MEP-48 is a focused, complementary target that:

- Inherits the parser, type checker, and fixture corpus from the
  shared Mochi frontend.
- Targets .NET 8 LTS as the floor and .NET 10 LTS as the supported
  ceiling.
- Defers the codegen IR choice to [[05-codegen-design]], with a
  likely Roslyn-source-plus-Reflection.Emit hybrid.
- Reuses NuGet wholesale, with a thin `Mochi.Runtime.*` package.
- Ships via framework-dependent (default), self-contained, or
  NativeAOT, with MAUI and Blazor sequenced through later phases.
- Lowers Mochi agents to `System.Threading.Channels` plus
  async/await colouring; does not surface async to Mochi code.
- Lowers Mochi streams to `IAsyncEnumerable<T>`.
- Validates against vm3 byte-equal as the master gate, with TFM
  matrix, NativeAOT, and self-contained gates layered on top.
- Treats NativeAOT as a first-class shipping target.
- Defers Mono / IL2CPP / NanoFramework to Phase 3.
- Does not duplicate MEP-45's embedded story, MEP-46's hot-reload
  story, or MEP-47's Maven-Central reach; complements all three
  with Windows enterprise, reified generics, value types, LINQ,
  and NativeAOT distribution.

The next ten notes flesh out each axis of this position.
