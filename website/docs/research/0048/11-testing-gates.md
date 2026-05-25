# MEP-48 research note 11, Testing strategy and gates for MEP-48

Author: research pass for MEP-48 (Mochi to .NET/CLR transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies the test-as-spec gates that govern each MEP-48 phase. It
mirrors the MEP-45 `TestPhase*` and MEP-46 / MEP-47 `TestPhase*` patterns: a
single deterministic Go test per phase, plus a differential gate against vm3,
plus a Roslyn type-check gate, plus a TFM matrix, plus a NativeAOT gate.

## 1. Gate philosophy

A **gate** is a Go test (`Test...` in the Mochi repo, under
`tests/transpiler3/dotnet/`) that:

1. Iterates fixtures in `tests/transpiler3/dotnet/...`.
2. Compiles each fixture with the MEP-48 toolchain
   (`mochi build --target=dotnet`).
3. Runs the resulting `.dll`/`.exe` (via `dotnet <fixture>.dll` or the
   self-contained launcher).
4. Compares actual stdout against `<fixture>.out`.
5. Compares against the vm3 oracle (running the same Mochi source on the
   reference VM).

A gate **fails closed**: any fixture mismatch fails the test. Phases land
only when every fixture in the phase's matrix is green.

Mochi's tradition (from MEP-45, MEP-46, MEP-47) is one gate per phase,
gates remain in CI forever, and the fixture matrix only grows. We follow
the same pattern for MEP-48.

The .NET-specific addition is that **Roslyn itself is a gate**: every
emitted .cs file must compile under `csc /warnaserror /langversion:12`
(or `13` / `14` per TFM) with `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`.
A Mochi program that produces uncompilable C# is a transpiler bug, not
a runtime issue. The Mochi-shipped Roslyn analyzer (`Mochi.Analyzers`)
contributes additional diagnostics (exhaustiveness, etc.) and is also
gated to zero warnings.

## 2. Phase gates (planned matrix)

Following [[01-language-surface]] and the MEP body's phase plan:

| Phase | Gate                       | Fixture count target | Surface covered                                  |
|-------|----------------------------|----------------------|--------------------------------------------------|
| 1     | `TestPhase1Hello`          | 5                    | hello world, print, basic let, basic int         |
| 2     | `TestPhase2Scalars`        | 20                   | int/float/bool/string ops, comparisons           |
| 3.1   | `TestPhase3Lists`          | 25                   | list literal, index, len, for-each               |
| 3.2   | `TestPhase3Maps`           | 25                   | map literal, index, len, keys, values, has       |
| 3.3   | `TestPhase3Sets`           | 20                   | set literal, add, has, len                       |
| 3.4   | `TestPhase3ListOfRecord`   | 20                   | list[record], comprehensions over records        |
| 4     | `TestPhase4Records`        | 25                   | records, methods, equality, `with`               |
| 5     | `TestPhase5Sums`           | 25                   | sum types, pattern matching, exhaustiveness      |
| 6     | `TestPhase6Funs`           | 25                   | closures, higher-order, Func/Action delegates    |
| 7     | `TestPhase7Query`          | 30                   | from/where/select, group_by, order_by, joins     |
| 8     | `TestPhase8Datalog`        | 20                   | facts, rules, recursion                          |
| 9     | `TestPhase9Agents`         | 25                   | agent definitions, spawn, call, cast             |
| 10    | `TestPhase10Streams`       | 20                   | streams, IAsyncEnumerable, await foreach         |
| 11    | `TestPhase11Async`         | 15                   | async colouring, ValueTask hot paths             |
| 12    | `TestPhase12FFI`           | 25                   | extern .NET imports, NuGet deps                  |
| 13    | `TestPhase13LLM`           | 10                   | generate (mocked LLM provider)                   |
| 14    | `TestPhase14Fetch`         | 10                   | fetch (HttpClient, against local test server)    |
| 15    | `TestPhase15NativeAot`     | 30                   | NativeAOT publish, single-file binary            |
| 16    | `TestPhase16Reproducible`  | 10                   | reproducible build (byte-identical .dll/.exe)    |
| 17    | `TestPhase17SelfContained` | 20                   | self-contained publish across RIDs               |
| 18    | `TestPhase18TrimWarnings`  | 25                   | trim-clean (IL2026/IL2070/IL3050 all zero)       |

Total target by Phase 18: ~390 fixtures, all green on .NET 8 LTS and
.NET 10 LTS, on Linux x86-64, Linux aarch64, macOS aarch64, and Windows
x86-64.

## 3. Differential testing vs vm3

The vm3 oracle is **the reference Mochi interpreter** (the original
tree-walker in `interp/`). For each fixture:

1. Run vm3: `mochi run <fixture>.mochi > <fixture>.vm3.out`.
2. Run .NET build:
   `mochi build --target=dotnet-fx-dependent <fixture>.mochi -o /tmp/f.dll &&
    dotnet /tmp/f.dll > <fixture>.dotnet.out`.
3. `diff <fixture>.vm3.out <fixture>.dotnet.out` must be empty.

The fixture's checked-in `<fixture>.out` file is the **vm3 oracle output**;
CI verifies vm3 produces it (catching vm3 regressions) and then verifies
the .NET target matches.

For non-deterministic fixtures (random, time, streams, agents), the
fixture is excluded from differential testing and runs only the static
check (stdout vs checked-in `.out`).

The MEP-45 (C), MEP-46 (BEAM), and MEP-47 (JVM) targets run the same
fixtures. The `tests/transpiler3/` directory has a shared fixture pool
with per-target overrides for fixtures that exercise target-specific
behaviour (e.g., `*.dotnet.skip` to skip a fixture on .NET).

## 4. Roslyn warnings gate

`TestRoslynClean`:

1. Build all fixtures with `mochi build --target=dotnet-source`.
2. Run the C# compiler with `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`,
   `<Nullable>enable</Nullable>`, `<LangVersion>12</LangVersion>` (for
   net8.0) or `<LangVersion>14</LangVersion>` (for net10.0).
3. Verify zero warnings on Mochi-generated code.

This gate validates that the emitted C# is high-quality: no nullable-
reference warnings (CS86xx), no unused-using warnings (IDE0005), no
async-without-await (CS1998), no implicit conversions (CS0078), no
unreachable-code (CS0162), no obsolete-API uses (CS0612/CS0618). Vendor
code in `target/dotnet/vendor/` may have its own warnings.

False positives (Roslyn over-conservative on certain generic bounds)
are suppressed only via `<NoWarn>` per individual file with a tracked
suppression entry in `target/dotnet/SUPPRESSIONS.md` explaining why.

The Mochi-shipped `Mochi.Analyzers` package adds:

- `MOCHI001`: non-exhaustive match arms.
- `MOCHI002`: unreachable match arm (post-`_`).
- `MOCHI003`: Mochi `int` lowered to C# `int` (must be `long`).
- `MOCHI004`: missing `ConfigureAwait(false)` on Mochi-internal await.
- `MOCHI005`: `Span<T>` crossing an `await` boundary.
- `MOCHI006`: `record class` where `readonly record struct` would
  fit (informational, not error).

`MOCHI001` through `MOCHI005` are errors; `MOCHI006` is suggestion-only.

## 5. TFM matrix

| TFM         | .NET version | C# version | Required CI status         |
|-------------|--------------|------------|----------------------------|
| `net8.0`    | .NET 8 LTS   | C# 12      | required (LTS floor)       |
| `net10.0`   | .NET 10 LTS  | C# 14      | required (LTS ceiling)     |
| `net11.0`   | .NET 11 STS  | C# 15      | warning-only (preview SDK) |
| `net6.0`    | .NET 6 LTS   | C# 10      | not supported (EOL Nov 2024) |
| `net7.0`    | .NET 7 STS   | C# 11      | not supported (EOL May 2024) |

Every gate runs on `net8.0` and `net10.0` in parallel. A phase lands
only when both are green. The `net11.0` matrix runs in advisory mode;
a regression triggers a warning, not a block.

The TFM matrix is implemented in CI via a job-strategy matrix:

```yaml
strategy:
  matrix:
    tfm: [net8.0, net10.0]
    os: [ubuntu-latest, macos-latest, windows-latest]
```

Linux is the primary platform (CI throughput); macOS arm64 and Windows
x86-64 are the secondary platforms. AOT and self-contained gates are
RID-sensitive and run per OS.

## 6. NativeAOT gate

`TestPhase15NativeAot`:

1. For each phase-15-eligible fixture:
   - Publish with `dotnet publish -c Release -p:PublishAot=true -r linux-x64`.
   - Verify zero trim warnings (IL2026, IL2046, IL2070, IL2080, IL2090,
     IL2104, IL2200; IL3050 family for AOT).
   - Run the resulting binary.
   - Compare stdout against vm3.
2. Repeat for `osx-arm64` and `win-x64`.

Trim warnings are the load-bearing piece: NativeAOT will silently
strip reachable-but-uncalled methods, and the only way the runtime
catches these is at runtime via `MissingMethodException` or
`NotSupportedException`. The transpiler must emit
`[DynamicallyAccessedMembers]`, `[RequiresDynamicCode]`, and
`[UnconditionalSuppressMessage]` attributes correctly so that the
trim analysis is silent.

The Mochi runtime ships source-generator-emitted trim attributes for
its own surface; user-code attributes are emitted by the Mochi
codegen pass.

A fixture is eligible for Phase 15 unless it uses:

- Runtime `System.Reflection.Emit` (Mochi does not surface this).
- `Assembly.LoadFrom` (Mochi does not surface this).
- The Mochi LLM provider plugin loader (which uses dynamic loading;
  excluded from AOT gate).
- The XML serializer (`XmlSerializer`, which emits IL at runtime;
  Mochi prefers `System.Text.Json`).

Eligibility is declared per-fixture via a `<fixture>.aot.txt` file
that lists either `eligible` or `excluded: <reason>`.

## 7. Self-contained gate

`TestPhase17SelfContained`:

1. For each fixture:
   - Publish with `dotnet publish -c Release --self-contained -r linux-x64`.
   - Verify the publish directory contains no host-runtime dependency.
   - Run via the platform-specific launcher (e.g., `./fixture` on
     Linux).
   - Compare stdout against vm3.
2. Repeat for `osx-arm64`, `win-x64`, and `linux-arm64`.

Self-contained gates do NOT require trim cleanliness (trimming is
opt-in via `<PublishTrimmed>true</PublishTrimmed>`); a separate
`TestPhase17SelfContainedTrimmed` runs the same fixtures with
trimming enabled to verify the trim warnings stay clean.

Self-contained binaries are RID-sensitive; cross-RID publish (e.g.,
`linux-x64 -> win-x64`) is supported via the dotnet CLI but does
not produce a runnable binary on the host; the gate validates only
host-RID publishes.

## 8. Reproducible build gate

`TestPhase16Reproducible`:

1. For each fixture:
   - Build with `mochi build --target=dotnet --deterministic`.
   - Record the .dll's SHA-256.
   - Build again on a different machine (or with a different temp
     directory).
   - Verify the SHA-256 matches.

Reproducibility requires:

- Roslyn's `/deterministic` flag (on by default since C# 7.1).
- A canonical `<Deterministic>true</Deterministic>` in the
  generated .csproj.
- A pinned `PathMap` (`/_/=`) so source paths do not leak into PDBs.
- A pinned compiler version (the dotnet SDK in the build container).
- Source-generator outputs that do not depend on `DateTime.Now`,
  `Guid.NewGuid`, or any environmental input.

PDB files are reproduced separately (PDB SHA-256 must also match).

## 9. xUnit gate

`TestPhase18Xunit`:

For Mochi `test` blocks, the codegen emits xUnit-compatible methods
decorated with `[Fact]` (or `[Theory]` for parameterised tests). The
gate runs:

```
dotnet test --logger "console;verbosity=detailed" tests/transpiler3/dotnet/xunit/
```

Every `test` block produces a passing assertion. The xUnit framework
itself is a NuGet dependency of the Mochi.Runtime.Test package; it
is not bundled into the user's main artefact.

MSTest and NUnit are alternative test runners; the gate primarily
uses xUnit but the Mochi.Runtime.Test package supports all three
via attribute aliasing.

## 10. Benchmark gate (informational)

A small set of fixtures has companion `.bench.mochi` files that run
on BenchmarkDotNet. The gate captures throughput numbers and posts
them as a CI comment but does not fail on regressions; regression
thresholds are tuned manually as the fixture set evolves.

Benchmarks are run on the JIT path (default), the NativeAOT path,
and the self-contained-trimmed path. Cross-target deltas are
reported.

## 11. Cross-target differential gate

Beyond vm3-equality, the four backends (C, BEAM, JVM, .NET) should
all produce byte-equal stdout on every fixture in their shared
matrix. The cross-target gate:

```
TestCrossTargetDifferential
```

1. For each fixture:
   - Run on vm3, C, BEAM, JVM, .NET (whichever are eligible).
   - Verify every pair produces byte-equal stdout.
2. Report any divergence as a "target divergence" error.

A divergence is *always* a bug in at least one target (assuming the
fixture is deterministic). The cross-target gate catches these
early.

## 12. Memory and resource gates

`TestPhase18MemoryStable`:

For the streams and agent phases, a long-running fixture (e.g.,
1M agent messages) must not leak heap. The gate:

1. Run the fixture for N seconds.
2. Capture GC stats via `GC.GetTotalAllocatedBytes` and
   `GC.GetGCMemoryInfo`.
3. Verify the steady-state heap is bounded.

For NativeAOT this gate also runs `dotnet-trace` to capture
EventPipe traces; the agent supervisor must observe message
delivery without unbounded queue growth.

## 13. Security gates

Per the threat model and memory-safety spec:

- **TLS pinning gate.** The Mochi `fetch` runtime, when configured
  with a pinned certificate, must reject mismatched cert chains.
  The gate uses a local test server with a known-bad cert.
- **FFI sandbox gate.** Mochi P/Invoke calls require a manifest
  entry. The gate verifies that a P/Invoke not in the manifest is
  rejected at compile time (analyzer) and at runtime (when
  reflection bypasses the analyzer).
- **No `unsafe` code from user surface.** The Mochi codegen never
  emits `unsafe` blocks in user-facing code; the runtime may use
  `unsafe` for `Span<T>` pinning, but it is contained in
  `Mochi.Runtime.Unsafe` and not exposed.
- **No `Assembly.LoadFrom` from user surface.** Mochi imports go
  through `MetadataReference` at compile time; runtime loading is
  reserved for the LLM provider plugin system.

## 14. Performance gates

Soft gates (regression-warning, not block):

- Hello-world JIT cold-start: ≤ 200ms on net8.0, ≤ 150ms on net10.0.
- Hello-world NativeAOT cold-start: ≤ 30ms.
- Hello-world NativeAOT binary size: ≤ 8 MB.
- Hello-world self-contained publish size: ≤ 60 MB un-trimmed,
  ≤ 20 MB trimmed.
- 1M-element list iteration: ≤ 1.5x vm3.
- 1M-element map insert: ≤ 1.5x vm3.

Regressions of >10% fail the soft gate with a warning; >25% fail
hard.

## 15. Phase gate summary

For Phase N to land:

1. `TestPhaseN` passes on net8.0 and net10.0, Linux x64, macOS arm64,
   Windows x64.
2. `TestRoslynClean` passes on the Phase N fixtures (zero warnings).
3. Cross-target gate green where applicable.
4. NativeAOT gate green for Phase N fixtures (if N ≥ 15 or the
   fixtures are AOT-eligible).
5. xUnit gate green for any new test blocks.
6. The phase commit includes:
   - The new gate test under `tests/transpiler3/dotnet/`.
   - The new fixtures under
     `tests/transpiler3/dotnet/fixtures/phase<NN>/`.
   - The phase entry in the MEP-48 progress log.

A phase that misses any of these is "in-progress", not "landed".

## 16. Out of scope for the testing notes

- Property-based testing (FsCheck, Hedgehog): mentioned as future
  work in [[12-risks-and-alternatives]].
- Fuzzing (SharpFuzz, libFuzzer-binding): future work.
- Mutation testing (Stryker.NET): future work.
- IL verification (peverify-replacement on .NET 8+): handled
  implicitly by the C# compiler emit; no separate gate.

These are not v1 requirements but are documented in note 12 as
candidate v2 follow-ups.
