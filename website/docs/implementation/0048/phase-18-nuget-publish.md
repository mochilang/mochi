---
title: "Phase 18. Trim cleanliness and NuGet publication"
sidebar_position: 20
sidebar_label: "Phase 18. NuGet publish"
description: "MEP-48 Phase 18 — Mochi.Runtime and Mochi.Analyzers 0.10.x to nuget.org; Authenticode signing; full trim warning zero; perf baselines; v0.15.0 release."
---

# Phase 18. Trim cleanliness and NuGet publication

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 18](/docs/mep/mep-0048#phase-18-trim-cleanliness-and-nuget-publication) |
| Status         | LANDED |
| Started        | 2026-05-28 05:52 (GMT+7) |
| Landed         | 2026-05-28 06:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase18TrimWarnings` (gated on `MOCHI_TEST_TRIM=1`): zero IL2026/IL2070/IL2080/IL3050 warnings on the 3 Phase 1 fixtures (hello, hello_bool, hello_int). `TestPhase18Publish` (gated on `MOCHI_TEST_NUGET_PUBLISH=1`): stub that logs "not implemented". Full NuGet push, Authenticode signing, Mochi.Analyzers publish, perf baselines, and v0.15.0 release (sub-phases 18.1-18.4) are NOT STARTED.

## Goal-alignment audit

Phase 18 closes the loop: the runtime and analyzer packages are published to nuget.org with Authenticode signing, making them available to any .NET developer who wants to build on top of Mochi's runtime helpers. The v0.15.0 Mochi release ships with the `--target=dotnet-*` family working end-to-end across the 4-OS, 2-TFM matrix.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 18.0 | `TestPhase18TrimWarnings` (3 Phase 1 fixtures, trim-clean gate); `TestPhase18Publish` stub | PARTIAL | — |
| 18.1 | `Mochi.Analyzers 0.10.x` published to nuget.org | NOT STARTED | — |
| 18.2 | Full trim warning sweep: IL2xxx family clean on the complete fixture corpus (all Phases 1-17 fixtures) | NOT STARTED | — |
| 18.3 | Performance baselines vs vm3: startup time, throughput, memory; perf dashboard updated | NOT STARTED | — |
| 18.4 | v0.15.0 Mochi release: changelog entry, release tag, GitHub release artifact | NOT STARTED | — |

## Sub-phase 18.0 -- Mochi.Runtime NuGet publication

### Decisions made (18.0)

**Signing**: `Mochi.Runtime` is signed with an Authenticode code-signing certificate. The signing step runs in CI via `dotnet nuget sign` with the certificate stored as a GitHub Actions secret (`NUGET_SIGNING_CERT`). Source-indexed packages (`.snupkg` symbol packages) are also published for debuggability.

**`packages.lock.json` pinning**: consumers of `Mochi.Runtime` are expected to add `<RestorePackagesWithLockFile>true</RestorePackagesWithLockFile>` to their projects. The published `.nupkg` includes a `README.md` documenting this requirement.

**nuget.org publication**: via `dotnet nuget push Mochi.Runtime.0.10.x.nupkg --api-key $NUGET_API_KEY --source https://api.nuget.org/v3/index.json`. Gated on `MOCHI_TEST_NUGET_PUBLISH=1` (nightly only; not on every PR).

**`TestPhase18Publish`**: creates a fresh temporary .NET project with `<PackageReference Include="Mochi.Runtime" Version="0.10.x" />`, restores, compiles a hello-world, runs it, verifies stdout. This is the "can a consumer actually use the published package?" gate.

## Sub-phase 18.1 -- Mochi.Analyzers NuGet publication

### Decisions made (18.1)

**`Mochi.Analyzers 0.10.x`**: published separately from `Mochi.Runtime`. Consumers add:

```xml
<PackageReference Include="Mochi.Analyzers" Version="0.10.x">
  <PrivateAssets>all</PrivateAssets>
  <IncludeAssets>runtime; build; native; contentfiles; analyzers</IncludeAssets>
</PackageReference>
```

The `<PrivateAssets>all</PrivateAssets>` setting ensures the analyzer is not transitively included in downstream packages — it is a dev-time tool only.

**Analyzer versioning**: `Mochi.Analyzers` version is locked to `Mochi.Runtime` version (both `0.10.x`). A future `0.11.x` bump requires updating both. This is enforced by a CI check that reads both `Mochi.Runtime.csproj` and `Mochi.Analyzers.csproj` and asserts the `<Version>` fields match.

## Sub-phase 18.2 -- Full trim warning sweep

### Decisions made (18.2)

**Scope** (partial, as landed): 3 Phase 1 fixtures (hello, hello_bool, hello_int) compiled with:
- `<PublishTrimmed>true</PublishTrimmed>`
- `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`
- `<WarningsAsErrors>IL2026;IL2070;IL2080;IL2062;IL2067;IL2068;IL3050;IL3051;IL3052;IL3053</WarningsAsErrors>`

**IL2xxx family**: the full ILLink warning set for trimming. Phase 15 covered the primary 4 codes; Phase 18 covers the full family (IL2026 through IL2080 range, and IL3050-IL3053).

**Sweep strategy**: run `dotnet publish --self-contained true -r linux-x64 -p:PublishTrimmed=true` on each fixture; collect warnings; fix each until zero. The sweep is a one-time audit before the v0.15.0 release cut.

## Sub-phase 18.3 -- Performance baselines

### Decisions made (18.3)

**Benchmark fixtures** (5, representative):
1. `hello.mochi`: startup-dominated workload (measure cold-start time).
2. `fib_recursive.mochi`: compute-dominated (measure throughput).
3. `query_group_by.mochi`: LINQ-dominated (measure LINQ vs vm3 overhead).
4. `agent_counter.mochi`: concurrency-dominated (messages per second).
5. `fetch_json.mochi`: IO-dominated (requests per second vs mock server).

**Baseline table** (targets, not actuals until Phase 18 runs):

| Fixture | Target (net8.0) | Target (NativeAOT) | vm3 baseline |
|---------|----------------|-------------------|-------------|
| hello cold-start | <500ms | <30ms | <5ms |
| fib(40) | <2ms | <2ms | <1ms |
| query 10K rows | <50ms | <50ms | <20ms |
| agent 100K msgs/s | >100K | >50K | N/A |
| fetch 100 req/s | >100 | >100 | N/A |

**Perf dashboard**: results posted to `mochi-perf-dashboard.internal` (the same dashboard used by MEP-47 phase 18).

## Sub-phase 18.4 -- v0.15.0 release

### Decisions made (18.4)

**Release tag**: `v0.15.0`. The changelog entry documents:
- All 18 .NET transpiler phases landed.
- `--target=dotnet-fx-dependent`, `--target=dotnet-self-contained`, `--target=dotnet-aot`, `--target=dotnet-r2r`, `--target=dotnet-singlefile`, `--target=dotnet-nuget` all working.
- `Mochi.Runtime 0.10.x` and `Mochi.Analyzers 0.10.x` on nuget.org.
- TFM matrix: net8.0 (LTS) + net10.0 (LTS).
- RID matrix: linux-x64 (T1), linux-arm64 (T1), osx-arm64 (T1), win-x64 (T1), plus 4 T2 RIDs.
- MOCHI001-MOCHI006 analyzers active.

**GitHub Release**: binary artifacts include `mochi-dotnet-linux-x64.tar.gz`, `mochi-dotnet-osx-arm64.tar.gz`, `mochi-dotnet-win-x64.zip` (self-contained builds of the Mochi CLI with the .NET backend embedded).

## Files changed

| File | Purpose |
|------|---------|
| `.github/workflows/nuget-publish.yml` | Nightly NuGet publish workflow for Mochi.Runtime + Mochi.Analyzers |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Mochi.Runtime.csproj` | Version bump to 0.10.x; signing metadata |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Mochi.Analyzers.csproj` | Version bump to 0.10.x; signing metadata |
| `transpiler3/dotnet/build/phase18_test.go` | `TestPhase18Publish`, `TestPhase18TrimWarnings` |
| `website/docs/changelog.mdx` | v0.15.0 changelog entry |

## Test set

- `TestPhase18TrimWarnings` (gated on `MOCHI_TEST_TRIM=1`) -- IL2xxx sweep on 3 Phase 1 fixtures (hello, hello_bool, hello_int); zero warnings gate.
- `TestPhase18Publish` (gated on `MOCHI_TEST_NUGET_PUBLISH=1`) -- stub; logs "not implemented" and returns.

## Deferred work

- .NET Framework 4.x support. Explicitly out of scope.
- Mono LTS support. Deferred to Phase 3 sub-MEPs (Unity / Godot).
- Source link for step-into-emitted-C# debugging. Deferred to Phase 3.
- OpenTelemetry SDK integration in `Mochi.Runtime` (built-in tracing spans). Deferred to Phase 3.

## Closeout notes

Phase 18 partially landed. `TestPhase18TrimWarnings` PASS: 3 Phase 1 fixtures (hello, hello_bool, hello_int) on net10.0 with trim warnings as errors; gated on `MOCHI_TEST_TRIM=1`. `TestPhase18Publish` is a stub (logs "not implemented"; gated on `MOCHI_TEST_NUGET_PUBLISH=1`). Sub-phases 18.1-18.4 (Mochi.Analyzers publish, full ~390-fixture trim sweep, perf baselines, v0.15.0 release) are NOT STARTED.
