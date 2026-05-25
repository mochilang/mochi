# MEP-48 research note 12, Risks and alternatives for the .NET target

Author: research pass for MEP-48 (Mochi to .NET/CLR transpiler).
Date: 2026-05-23 (GMT+7).

This note catalogues the risks of the design adopted in notes 01-11 and
the alternatives we considered and rejected. It is written so that a
future maintainer can see *why* a choice was made and which trade was
accepted. Mirrors MEP-45 / MEP-46 / MEP-47 note 12.

## 1. Top-level risks

### R1: NativeAOT trim warnings on Mochi runtime

NativeAOT requires every reflective access, every generic instantiation
visible only at runtime, every dynamic code path to be declared via
`[DynamicallyAccessedMembers]`, `[RequiresUnreferencedCode]`, or
`[RequiresDynamicCode]`. The trim analyser surfaces IL2026, IL2070,
IL2080, IL3050, and family warnings; the gate
([[11-testing-gates]] §6) requires zero.

The user-facing risk: a Mochi program that works with the JIT path
fails at runtime under NativeAOT because the trim analyser stripped
a method that the agent supervisor reflectively invokes.

Mitigation:

- The Mochi codegen emits trim attributes for every Mochi-public
  surface: sum-type variants, agent message records, JSON-mapped DTO
  records.
- The Mochi runtime ships its own trim attributes for its public
  surface and the `[ModuleInitializer]` reflection-roots.
- The CI gate runs every fixture through NativeAOT publish; missing
  attributes fail the gate.
- The agent supervisor avoids reflection entirely; dispatch is via
  source-generated `switch` over a closed sealed-record union.

Risk magnitude: medium. Most pure-Mochi programs work; programs heavy
on third-party reflective frameworks (Newtonsoft.Json non-source-
gen, Autofac, AutoMapper non-source-gen) may not. We document this
as a known limitation and recommend the framework-dependent or
self-contained path for those programs.

### R2: Async colouring as a transpiler responsibility

Mochi has no async/await in the surface language ([[02-design-philosophy]]
§13). The .NET lowering must split functions into sync and async
"colours" based on transitive call analysis. This is non-trivial:

- A function called from both sync and async contexts requires
  duplication (sync version plus async version), or the entire
  module becomes async (degrading sync-path performance).
- The colouring pass interacts with closures: an async closure
  captured by a sync function is a colour violation.
- Recursive cycles (mutual recursion) need fixed-point colouring.

Risk: the colouring pass is a new transpiler component with novel
correctness obligations. Bugs in it produce mysterious "cannot await
a non-async method" CS errors or worse silent deadlocks.

Mitigation:

- The colouring pass is one of the first phases implemented (Phase 1
  of the transpiler, before Phase 9 agents lights up).
- The pass has a property-based test: random Mochi modules are
  generated, coloured, and the result is checked for invariants
  (no sync-calls-async, no async-cycles, every awaited callee is
  red).
- The pass produces a colouring report in build verbose mode so the
  user can see which functions are async and why.
- The pass is deterministic: same input produces same colouring.

Risk magnitude: high during initial development, medium long-term.
The colouring pass is the largest novel piece of MEP-48; the JVM
target with Loom skipped this entirely.

### R3: NuGet is a supply-chain attack surface

Pulling deps from NuGet means trusting (a) the network, (b) the
NuGet feed (nuget.org or a private feed), (c) the dep authors. The
SolarWinds and event-stream precedents show that .NET-adjacent
ecosystems can carry malicious updates.

Mitigation:

- Every Mochi-side dep has a pinned version and a SHA-256 in
  `mochi.lock.json` (`packages.lock.json` is the NuGet equivalent;
  Mochi wraps both).
- The build verifies the lockfile before invoking the restore.
- A `mochi audit` subcommand checks pinned versions against the OSV
  vulnerability database (which mirrors NuGet CVEs).
- The runtime itself uses a deliberately small dep set (System.Text.Json,
  System.Threading.Channels, System.Linq.Async, optional
  YamlDotNet, optional BouncyCastle.Cryptography).
- The runtime's NuGet feed entry is signed by a Mochi-controlled key;
  `dotnet nuget verify` is part of the build.
- The runtime is published to nuget.org with two-factor enforcement
  on the owner account.

Risk magnitude: medium. Mitigation depth matches MEP-47's Maven
Central posture.

### R4: Mono and Unity IL2CPP divergence

Mono is the legacy CLR implementation, unified into the .NET project
in Nov 2020 but still maintained as a separate runtime for some
hosts (Unity until 6.8, MAUI iOS in AOT mode). Unity IL2CPP is a
distinct path that transpiles IL to C++ and uses a C++ compiler.

Risk: Mochi-on-.NET v1 targets CoreCLR; Mono and IL2CPP behaviour
diverges in subtle ways (different finalisation semantics, different
GC, IL2CPP can't handle some reflection patterns).

Mitigation:

- v1 explicitly does not target Mono or IL2CPP; the build CLI
  rejects `--target=mono` and `--target=il2cpp` until Phase 3 lands.
- Phase 3 (MEP-48.1 or MEP-48.2) will add Mono and IL2CPP gates
  separately, building on the lessons from CoreCLR.
- Unity's announced migration to CoreCLR (target Unity 6.8) reduces
  the urgency of IL2CPP support; we watch the schedule and may
  collapse the two follow-ups.
- The runtime avoids known IL2CPP problem patterns
  (`MakeGenericMethod`, `MakeGenericType` from reflection,
  `Activator.CreateInstance` with type params unknown at compile
  time).

Risk magnitude: low for v1 (out of scope), medium for Phase 3.

### R5: Roslyn API churn

Roslyn (`Microsoft.CodeAnalysis.CSharp`) is a stable API surface,
but it has had multiple major releases (3.x, 4.x). C# 12 (Roslyn
4.8), C# 13 (Roslyn 4.11), C# 14 (Roslyn 4.13/4.14) shipped with
new syntax forms. The transpiler's `SyntaxFactory` calls must
target a specific Roslyn version.

Risk: a Roslyn upgrade (e.g., 4.14 to 5.0) breaks the
`SyntaxFactory` API surface in an incompatible way; the transpiler
fails to build.

Mitigation:

- The transpiler pins its Roslyn version in `mochi/build/dotnet/Cargo.toml`
  -equivalent (we use Go's `go.mod` for the transpiler binary; the
  Roslyn calls are via a `dotnet`-side helper invoked over a JSON
  protocol).
- The Roslyn-using path is a separate binary (`mochi-dotnet-codegen`)
  built with the pinned Roslyn version; it is upgraded deliberately
  with a CI bisect.
- The hybrid `System.Reflection.Emit` path does not depend on
  Roslyn, so we have a fallback codepath.
- We test on Roslyn 4.8, 4.11, 4.13, and the developer-preview of
  the next version.

Risk magnitude: low. Roslyn has a strong backwards-compatibility
track record.

### R6: .NET version EOL cadence

Microsoft's .NET release cadence is annual: STS releases every
even-numbered year (e.g., .NET 7, .NET 9, .NET 11) with 18-month
support, LTS releases every odd-numbered year (e.g., .NET 6, .NET 8,
.NET 10, .NET 12) with 3-year support.

Risk: Mochi-on-.NET v1 targets .NET 8 LTS (EOL Nov 2026) and
.NET 10 LTS (EOL Nov 2028). Users on STS releases (.NET 7, .NET 9,
.NET 11) get less testing. EOL transitions force a forced upgrade
or a CI matrix expansion.

Mitigation:

- The MEP-48 spec commits to a 2-LTS support window (currently
  net8.0 and net10.0).
- When .NET 12 LTS ships (Nov 2027), we add it to the matrix; when
  .NET 8 reaches EOL (Nov 2026), we drop it from the required
  matrix but keep an advisory job.
- The transpiler emits TFM-conditional C# (e.g., `OrderedDictionary<K,V>`
  on net10.0+ vs the runtime wrapper on net8.0); the conditional
  compilation lives in source-generator output.
- STS releases get a "best effort" CI job; users on STS are
  warned that support is advisory.

Risk magnitude: low (well-understood Microsoft cadence).

### R7: Function colouring drift across .NET BCL versions

`ValueTask<T>` vs `Task<T>` is a per-API choice in the BCL, and the
.NET BCL has been moving toward `ValueTask` over time (e.g.,
`Channel<T>.Reader.ReadAsync` returns `ValueTask<T>` since .NET
Core 3.0). The transpiler must propagate `ValueTask` semantics
correctly (single-await, no `.Result` blocking).

Risk: A Mochi program that awaits a Channel read at multiple
sites silently triggers `InvalidOperationException` because
`ValueTask` was awaited twice.

Mitigation:

- The transpiler never duplicates an await of a `ValueTask`; the
  IR pass tracks awaitable values and forbids reuse.
- The Mochi runtime wraps `ValueTask` returns from BCL APIs in
  helpers that convert to `Task` for storage scenarios.
- The Roslyn analyzer `MOCHI004` flags missing `ConfigureAwait(false)`
  but does not flag `ValueTask` reuse (handled by the IR pass).

Risk magnitude: medium. The interaction is subtle and the failure
mode is a runtime exception.

### R8: Reified generics interact with IL2CPP and trimming

CLR reified generics let the transpiler avoid type-tag threading
(see [[06-type-lowering]] §3). However, NativeAOT and IL2CPP both
generate code for each generic instantiation seen at the call
graph; instantiations not seen are stripped (NativeAOT) or fail
at runtime (IL2CPP).

Risk: a Mochi function `fun map<T>(xs: list<T>): list<T>` called
only at `list<int>` in the static program but at `list<string>`
via a JSON deserialised callback will fail under NativeAOT or
IL2CPP because the `<string>` instantiation was not emitted.

Mitigation:

- Mochi has no runtime type-driven instantiation (no equivalent
  of `MakeGenericMethod` at the surface).
- JSON deserialisation goes through `System.Text.Json` source
  generators (`JsonSerializerContext`), which emit the necessary
  trim roots.
- The Mochi runtime's reflection-using helpers
  (`Mochi.Runtime.Eq.Equal<T>` etc.) are marked
  `[DynamicallyAccessedMembers]` or are rewritten to use
  source-generator-emitted dispatch tables.

Risk magnitude: low-medium for NativeAOT, medium for IL2CPP
(Phase 3).

### R9: Source generators slow incremental builds

Roslyn `IIncrementalGenerator` is incremental in the sense that
its output is cached and re-computed only on relevant input
changes. But large generators (e.g., `JsonSerializerContext` for
many DTOs) can still add significant build time.

Risk: Mochi-on-.NET projects with hundreds of records see slow
incremental builds due to the Mochi source-generator output.

Mitigation:

- The Mochi-shipped source generator is incremental, keyed by
  the Mochi AST hash per module.
- The generator emits per-module files, so changing one module
  invalidates only its slice.
- Benchmark gate ([[11-testing-gates]] §10) tracks generator
  throughput.

Risk magnitude: low. Roslyn 4.0+ incremental generators are well-
tuned for this workload.

### R10: Windows path and casing fragility

Mochi developer experience must work on Windows. Cross-platform
path handling, case-insensitive file systems, line-ending
conventions (CRLF vs LF), and Windows Defender / SmartScreen
warnings on un-signed binaries all contribute to friction.

Risk: Mochi-on-.NET works on Linux but produces broken builds on
Windows due to path-separator bugs, casing mismatches, or
Defender false positives.

Mitigation:

- CI runs the gate matrix on Windows in addition to Linux and
  macOS.
- The transpiler uses `System.IO.Path.Combine` exclusively (no
  hand-built path strings).
- The build CLI writes to `.bin/` not `bin/` to avoid case
  conflicts with .NET's `bin/` convention.
- The Mochi-published NuGet package and the build CLI binary are
  Authenticode-signed so Windows Defender does not flag them.

Risk magnitude: low-medium. Windows quirks are well-understood;
the CI matrix catches them.

## 2. Alternatives considered and rejected

### A1: Lower to F# source

Considered: emit F# source, let `fsc` compile it.

Rejected because:

- F# has its own type system (option types, units of measure,
  computation expressions, type providers) that does not map
  cleanly to Mochi's.
- F# evolution cadence differs from C# (smaller feature drops,
  longer release cycles).
- F# is a smaller community; tooling and library coverage are
  weaker than C#.
- C# 9+ records cover the structural-equality and discriminated-
  union story sufficiently for Mochi's needs.

See [[02-design-philosophy]] §7.

### A2: Lower to VB.NET source

Considered briefly: emit VB.NET source.

Rejected because:

- VB.NET is in long-term maintenance mode; no new features since
  2017.
- The VB.NET compiler is slower than `csc` and slower than Roslyn
  C# generation.
- VB.NET's case-insensitive identifiers conflict with Mochi's
  case-sensitive surface.

### A3: Skip Roslyn, emit IL only

Considered: bypass Roslyn entirely, emit IL via
`System.Reflection.Emit` for all code paths.

Rejected because:

- We lose Roslyn's optimisation passes (constant folding, dead-
  code elimination, async-state-machine lowering, lambda capture
  hoisting). Reimplementing these is a large undertaking.
- We lose Roslyn-generated PDBs (line mappings, local variable
  names); IL-emit-generated PDBs are sparser.
- We lose Roslyn analyzer interaction; the `Mochi.Analyzers`
  package can't see our IL directly.
- The async colouring lowering is much harder in IL (the C#
  compiler does the state-machine rewrite for us).

The hybrid path (Roslyn for ordinary code, `Reflection.Emit` for
agent trampolines and hot paths) is the chosen compromise.

### A4: Target IL2CPP for v1

Considered: target Unity's IL2CPP directly to get Unity game
development support out of the box.

Rejected because:

- IL2CPP has constraints (no `System.Reflection.Emit`, no
  `MakeGenericMethod`, restricted `Activator.CreateInstance`) that
  are tighter than NativeAOT.
- Unity 6.8 announced migration to CoreCLR; the IL2CPP target
  becomes legacy for the platform that drove it.
- Cross-platform IL2CPP requires Unity's build tooling, which is
  not freely redistributable.

Phase 3 (MEP-48.x) revisits if Unity 6.8 migration stalls.

### A5: Skip NativeAOT, target only JIT

Considered: target only the JIT path; skip NativeAOT entirely.

Rejected because:

- NativeAOT is the only way to match MEP-45 / MEP-47-GraalVM
  distribution-shape goals (single-file static binary, sub-50ms
  startup).
- Customer demand for AOT is rising (Azure Functions Isolated,
  serverless, container cold-start optimisation).
- NativeAOT skip would surrender the .NET target's best
  positioning vs JVM (where GraalVM is harder to ship).

### A6: Skip the runtime NuGet, inline everything

Considered: emit every runtime helper inline in user code, ship
no `Mochi.Runtime` package.

Rejected because:

- Duplicate code across user assemblies inflates binary size.
- A runtime bug fix requires rebuilding every user assembly.
- The agent supervisor needs a stable surface that multiple
  user assemblies share.
- Source-generator-based inlining is possible but loses the
  cross-program optimisation NuGet provides.

### A7: Skip the analyzer, rely only on runtime checks

Considered: don't ship `Mochi.Analyzers`; rely on runtime
exhaustiveness throws.

Rejected because:

- Runtime errors are worse UX than compile errors.
- The analyzer is cheap to ship (a small DLL bundled with the
  runtime NuGet).
- Mochi spec semantics treat non-exhaustive match as a compile-
  time error; surface code should never see the runtime throw.

### A8: Single TFM (net8.0 only) for v1

Considered: target only net8.0 for v1; add net10.0 in v2.

Rejected because:

- The .NET 8 LTS EOL is Nov 2026, only 6 months after MEP-48 v1
  ships (target). Single-TFM users would be stranded.
- The Mochi-Conf-2026 demo audience uses .NET 10 (the current LTS
  at demo time).
- Two-TFM matrix doubles CI throughput but is well within budget.

### A9: Use Mono.Cecil for IL editing

Considered: use Mono.Cecil to read existing assemblies and
re-emit modified versions.

Rejected because:

- We never modify existing assemblies; we generate new ones.
- Cecil is heavyweight (~1MB) vs `System.Reflection.Metadata`
  (~600KB, in the BCL).
- Cecil's write-back model duplicates the source assembly into
  memory; we don't need that.

We use `System.Reflection.Metadata` for read scenarios (importing
.NET types into the Mochi type checker) and `System.Reflection.Emit`
for write scenarios.

### A10: Use Newtonsoft.Json instead of System.Text.Json

Considered: use Newtonsoft.Json (Json.NET) as the canonical JSON
runtime.

Rejected because:

- Newtonsoft.Json's reflection-heavy design is hostile to
  NativeAOT and trimming.
- System.Text.Json's source-generator path (`JsonSerializerContext`)
  is trim-clean and AOT-friendly.
- System.Text.Json ships in the BCL (no separate NuGet for the
  common case); Newtonsoft is an extra dependency.
- System.Text.Json is consistently faster (1.5-2x in microbenchmarks).
- Newtonsoft.Json is in maintenance mode; James Newton-King moved
  to System.Text.Json development.

### A11: Bundle a private dotnet SDK

Considered: ship a private copy of the dotnet SDK with `mochi`,
so users don't need to install one.

Rejected because:

- The SDK is large (~500MB extracted).
- The SDK is platform-specific (3+ binaries to ship).
- Microsoft's redistribution policy allows it but recommends
  against bundling.
- The `dotnet` global tool model is the canonical install path.

Users install dotnet via Microsoft's installers or `apt`/`brew`/
`winget`; the build CLI requires `dotnet` on PATH.

### A12: Support .NET Framework 4.x

Considered: target .NET Framework 4.8 to reach legacy Windows
users.

Rejected because:

- .NET Framework is Windows-only.
- .NET Framework 4.8 is the final version (announced 2019).
- Microsoft Roslyn drops .NET Framework support over time.
- Modern features (records, nullable references, source
  generators, channels) require .NET Core / .NET 5+.

Users on .NET Framework can install .NET 8 alongside (side-by-side)
and use the Mochi target via the unified runtime.

## 3. Out-of-scope items (deferred to v2 or later)

- **MAUI desktop/mobile target.** Phase 3 (MEP-48.1).
- **Blazor WASM target.** Phase 3 (MEP-48.2).
- **Unity / IL2CPP target.** Phase 3 (MEP-48.3), pending Unity 6.8
  migration outcome.
- **Godot target.** Phase 3 (MEP-48.4), pending evaluation of
  Godot's C# integration.
- **NanoFramework / Meadow microcontroller target.** Out of v2.
- **F# interop (calling F# code from Mochi or vice versa).** v2.
- **Property-based testing infrastructure (FsCheck).** v2.
- **Mutation testing (Stryker.NET).** v2.
- **DLR / dynamic typing support.** Out of all foreseeable scope.
- **WinForms / WPF code generation.** Out of all foreseeable scope.

## 4. Open questions for the design

Q1. Should the runtime wrap `Channel<T>` directly, or expose a
Mochi-specific `MochiChannel<T>` that adds Mochi-side features
(panic propagation, supervisor hooks)? Current lean: wrap, because
the supervisor needs hooks. See [[09-agent-streams]] §6.

Q2. Should the lowering use `ValueTask<T>` or `Task<T>` by default?
Current lean: `Task<T>` for public surface, `ValueTask<T>` for
hot-path internals. See [[02-design-philosophy]] §13.

Q3. Should Mochi `Option<T>` lower to `T?` (nullable value type)
when `T` is a value type, or always to the `Option<T>` record
hierarchy? Current lean: per-call-site, with the flow analysis
pass making the decision. See [[06-type-lowering]] §5.4.

Q4. Should the build CLI default to `--target=dotnet-aot` or
`--target=dotnet-fx-dependent`? Current lean: framework-dependent,
because it has the smallest output and the highest compatibility
ceiling. AOT is opt-in.

Q5. Should the Mochi-shipped Roslyn analyzer be a hard dependency
of the Mochi runtime NuGet, or a separate optional package?
Current lean: hard dependency, because the exhaustiveness check is
load-bearing (the runtime throw is a backstop, not the primary
mechanism).

These are resolved during Phase 1 implementation; the spec body
records the final positions.

## 5. Comparison with sibling MEPs

| Risk axis                      | MEP-45 (C)     | MEP-46 (BEAM)   | MEP-47 (JVM)    | MEP-48 (.NET)        |
|--------------------------------|----------------|------------------|------------------|----------------------|
| Memory safety                  | manual         | GC               | GC               | GC                   |
| Distribution shape             | static binary  | escript / OCI    | uberjar / native | dll / SC / NativeAOT |
| Async story                    | manual (libuv) | OTP processes    | Loom (implicit)  | async/await colour   |
| Supply chain                   | apt/vcpkg      | hex.pm           | Maven Central    | NuGet                |
| Reflection support             | none           | full (eval)      | full (jvm)       | full (CLR)           |
| Reified generics               | n/a (no gen)   | n/a (no types)   | no (erased)      | yes                  |
| Value types                    | yes (manual)   | no               | no (until Valhalla) | yes (struct)      |
| Single-file ship               | yes (static)   | escript          | GraalVM          | NativeAOT            |
| Cold start                     | <5ms           | ~30ms            | ~300ms (50ms AOT)| ~50ms (30ms AOT)     |
| Library ecosystem size         | huge (C)       | medium (hex.pm)  | huge (10M+)      | medium (~6M)         |
| Hot reload                     | no             | yes (first-class)| limited          | limited (dev only)   |
| Windows desktop integration    | medium         | poor             | medium           | best                 |
| Mobile reach                   | via NDK        | poor             | Android JVM      | iOS/Android MAUI     |
| Game-engine integration        | Unreal C++     | no               | LWJGL            | Unity, Godot         |

The .NET target's specific niche: best Windows enterprise integration,
best value-types story among managed targets, best LINQ alignment,
best mobile and game-engine reach via MAUI/Unity/Godot.

## 6. Sunset / decommission criteria

If MEP-48 fails to ship within the spec window, the criteria for
decommission:

- NativeAOT trim cleanliness gate is unachievable across the
  fixture matrix (would require fundamental Mochi runtime
  redesign).
- Microsoft pivots .NET away from cross-platform support (highly
  unlikely; the Linux container hosting investment is too deep).
- The async colouring pass proves intractable (e.g., the property-
  test invariants cannot be satisfied; would force surfacing
  async/await to Mochi).

None of these are likely. The fallback in each case is documented:

- Trim cleanliness: ship Phase 15 as advisory, AOT is opt-in only.
- Microsoft pivot: target Mono LTS branch (community-maintained).
- Async intractability: surface a Mochi `await` keyword (spec
  amendment), follow JVM target's previous-generation patterns.

## 7. Summary of position

The .NET target is *complementary* to C, BEAM, and JVM, not a
replacement for any. The largest risks are:

- NativeAOT trim warnings (mitigated by source-generator-emitted
  attributes).
- Async colouring as a transpiler responsibility (mitigated by
  property-based testing of the colouring pass).
- NuGet supply chain (mitigated by lockfile pinning and audit).
- Roslyn API churn (mitigated by version pinning and a fallback
  IL-emit path).

The largest wins are:

- Reified generics (no type-tag threading, unlike JVM).
- Value types as first-class (no Valhalla wait, unlike JVM).
- LINQ as canonical query target (no runtime to write).
- NativeAOT as a first-class distribution shape.
- Best Windows enterprise and game-engine reach.

The spec body codifies these positions; the gate matrix
([[11-testing-gates]]) enforces them.
