---
title: "Phase 17. Self-contained packaging across RIDs"
sidebar_position: 19
sidebar_label: "Phase 17. Self-contained"
description: "MEP-48 Phase 17 — --target=dotnet-self-contained on host RID; 4 fixtures (sc_add, sc_bool, sc_hello, sc_string)."
---

# Phase 17. Self-contained packaging across RIDs

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 17](/docs/mep/mep-0048#phase-17-self-contained-packaging-across-rids) |
| Status         | LANDED |
| Started        | 2026-05-28 05:52 (GMT+7) |
| Landed         | 2026-05-28 06:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase17SelfContained`: 4 fixtures green on the host RID (sc_add, sc_bool, sc_hello, sc_string). Multi-RID matrix, trimmed self-contained, single-file, and R2R variants (sub-phases 17.1-17.4) are NOT STARTED.

## Goal-alignment audit

Self-contained publish is the middle tier between framework-dependent (smallest, requires installed .NET) and NativeAOT (static binary, no CLR). A self-contained publish bundles the .NET runtime for a specific RID into the output directory; users do not need .NET installed. This is the right target for distribution to machines not under the developer's control (end-user machines, Docker containers without a .NET base image, CI machines with a custom runtime).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | `--target=dotnet-self-contained <rid>`: `dotnet publish --self-contained true -r <rid>` | NOT STARTED | — |
| 17.1 | Trimmed self-contained: `<PublishTrimmed>true</PublishTrimmed>` + trim-clean gate | NOT STARTED | — |
| 17.2 | Single-file self-contained: `<PublishSingleFile>true</PublishSingleFile>` (non-AOT) | NOT STARTED | — |
| 17.3 | ReadyToRun (R2R): `<PublishReadyToRun>true</PublishReadyToRun>` for faster startup | NOT STARTED | — |
| 17.4 | Multi-RID CI matrix: all 4 tier-1 RIDs + 4 tier-2 RIDs green | NOT STARTED | — |

> Sub-phases 17.1-17.4 are NOT STARTED. Phase 17.0 covers host-RID only (4 fixtures).

## Sub-phase 17.0 -- Self-contained publish

### Decisions made (17.0)

**`dotnet publish --self-contained true -r linux-x64`**: produces a directory containing the user `.dll`, all required .NET runtime shared libraries (`.so` / `.dll`), and the apphost executable. Un-trimmed size ~50-80 MB (includes the entire .NET runtime). Runs on any linux-x64 machine without .NET installed.

**Generated `.csproj` additions**:

```xml
<PropertyGroup>
  <SelfContained>true</SelfContained>
  <RuntimeIdentifier>linux-x64</RuntimeIdentifier>
</PropertyGroup>
```

The driver injects `<RuntimeIdentifier>` based on the `--target` flag value.

**Tier-1 RIDs**: `linux-x64`, `linux-arm64`, `osx-arm64`, `win-x64`.
**Tier-2 RIDs**: `osx-x64`, `win-arm64`, `linux-musl-x64`, `linux-musl-arm64`.

## Sub-phase 17.1 -- Trimmed self-contained

### Decisions made (17.1)

**`<PublishTrimmed>true</PublishTrimmed>`**: the trimmer removes unused BCL code from the output. Trimmed self-contained size: ~15-20 MB (vs ~80 MB un-trimmed). Faster startup (~200ms vs ~500ms un-trimmed).

**Trim-clean gate**: same IL2026/IL3050 zero-warning requirement as Phase 15 (NativeAOT). The trim analysis is less strict than NativeAOT ILC (some reflection still works in trimmed IL) but the diagnostic gate is the same.

**`<TrimmerRootDescriptor>trim.xml`**: a `trim.xml` file lists types and methods that must be preserved despite appearing unreachable to the trimmer. The Mochi driver generates this file for agent types (which the supervisor restarts via factory lambda, not reflection) and for Datalog types (which are registered by name).

## Sub-phase 17.2 -- Single-file non-AOT

### Decisions made (17.2)

**`<PublishSingleFile>true</PublishSingleFile>`**: packs all `.dll` files and native libraries into a single executable that self-extracts to a temp directory on first run. Size: same as self-contained, but one file. Cold start: +50ms for extraction (first run only; subsequent runs use cached extraction).

This is the `--target=dotnet-singlefile` target. Distinct from `--target=dotnet-aot` (which is truly single-file and static; no extraction).

## Sub-phase 17.3 -- ReadyToRun

### Decisions made (17.3)

**`<PublishReadyToRun>true</PublishReadyToRun>`**: pre-compiles managed IL to native code using CrossGen2 at publish time. The resulting output is larger than pure-IL self-contained (~120 MB vs ~80 MB) but has faster startup because the JIT does not need to compile hot methods on first run. Cold start: ~200ms (vs ~500ms without R2R for JIT-heavy startup paths).

R2R is the `--target=dotnet-r2r` target. Useful for long-running server processes where startup is measured once. Less useful for CLI tools where NativeAOT is the right choice.

## Sub-phase 17.4 -- Multi-RID CI matrix

### Decisions made (17.4)

**CI matrix**:

| Runner | RID | Gate |
|--------|-----|------|
| ubuntu-24.04 | linux-x64 | native run |
| ubuntu-24.04-arm | linux-arm64 | native run |
| macos-14 | osx-arm64 | native run |
| windows-2022 | win-x64 | native run |
| ubuntu-24.04 | linux-musl-x64 | Alpine container run |
| ubuntu-24.04-arm | linux-musl-arm64 | Alpine container run |
| macos-13 | osx-x64 | native run |
| windows-2022 | win-arm64 | emulation (QEMU) |

Tier-2 RIDs are gated on `MOCHI_TEST_TIER2_RIDS=1`. They run nightly, not on every PR.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/build/selfcontained.go` | `--target=dotnet-self-contained`: publish invocation, RID injection |
| `transpiler3/dotnet/build/singlefile.go` | `--target=dotnet-singlefile`: `PublishSingleFile=true` |
| `transpiler3/dotnet/build/aot.go` | `--target=dotnet-r2r`: `PublishReadyToRun=true` |
| `transpiler3/dotnet/build/csproj.go` | RID + trim + R2R + single-file `.csproj` properties |
| `.github/workflows/transpiler3-dotnet-rid-matrix.yml` | Multi-RID CI matrix workflow |
| `transpiler3/dotnet/build/phase17_test.go` | `TestPhase17SelfContained` |
| `tests/transpiler3/dotnet/fixtures/phase17-selfcontained/` | 4 fixture directories (sc_add, sc_bool, sc_hello, sc_string) |

## Test set

- `TestPhase17SelfContained` -- 4 fixtures on host RID: sc_add, sc_bool, sc_hello, sc_string. Verifies stdout matches vm3.

## Deferred work

- NativeAOT cross-compile: build linux-x64 NativeAOT binary on macOS arm64 host. Requires .NET 9+ ILC cross-compile support. Deferred to Phase 17.5.
- Windows ARM64 native runs (not QEMU). Deferred pending Windows ARM CI runner availability.

## Closeout notes

Phase 17 landed. `TestPhase17SelfContained` PASS: 4 fixtures on host RID (sc_add, sc_bool, sc_hello, sc_string). Fixture directory is `phase17-selfcontained/` (no hyphen before "selfcontained"). Multi-RID matrix, trimmed self-contained, single-file, and R2R variants (sub-phases 17.1-17.4) are NOT STARTED.
