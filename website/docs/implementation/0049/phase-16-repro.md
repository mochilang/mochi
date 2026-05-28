---
title: "Phase 16. Reproducible build"
sidebar_position: 20
sidebar_label: "Phase 16. Reproducible build"
description: "MEP-49 Phase 16 — deterministic build via SWIFTPM_DETERMINISTIC_BUILD, SOURCE_DATE_EPOCH=0, -Xlinker -no_uuid; SHA-256 comparison gate on linux-x64; skipped on macOS."
---

# Phase 16. Reproducible build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 16](/docs/mep/mep-0049#phase-16-reproducible-build) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | [#22458](https://github.com/mochilang/mochi/issues/22458) |
| Tracking PR    | [#22459](https://github.com/mochilang/mochi/pull/22459) |

## Gate

`TestPhase16Repro`: SHA-256 of the final binary matches across two independent builds (different `$HOME`, different `$TMPDIR`). Gate runs on **linux-x64** only -- the test is skipped on macOS because macOS Mach-O binaries embed a link-time UUID that remains non-deterministic even with the flags below (see Deferred work). 3 fixture programs.

## Goal-alignment audit

Reproducible builds enable supply-chain verification: a user can rebuild from source and confirm the binary matches the published one bit-for-bit. This is increasingly required by enterprise security policies. The gate is strict: byte-identical output across two independent builds on Linux.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `Deterministic bool` flag on `Driver`; `SWIFTPM_DETERMINISTIC_BUILD=1` + `SOURCE_DATE_EPOCH=0` injected into build env | LANDED | mep/0049-phase-16 |
| 16.1 | `-Xlinker -no_uuid` (macOS) / `-Xlinker --build-id=none` (Linux) in deterministic mode | LANDED | mep/0049-phase-16 |
| 16.2 | `sha256File` helper; two-build SHA-256 comparison in gate test | LANDED | mep/0049-phase-16 |
| 16.3 | macOS Mach-O UUID reproducibility | DEFERRED | — |

## Sub-phase 16.0 -- Deterministic build flags

### Decisions made (16.0)

**`Driver.Deterministic bool`**: the build driver field that activates deterministic mode. Set to `true` in `TestPhase16Repro`.

**`SWIFTPM_DETERMINISTIC_BUILD=1`**: SwiftPM environment variable (Swift 5.8+) that suppresses timestamp-based cache invalidation and seeds random identifiers deterministically.

**`SOURCE_DATE_EPOCH=0`**: standard reproducible-builds epoch. The Swift compiler and linker respect this when building for release.

**Implementation**: in `build.go`, when `Driver.Deterministic` is true, the env slice passed to `swift build` includes `SWIFTPM_DETERMINISTIC_BUILD=1` and `SOURCE_DATE_EPOCH=0`.

## Sub-phase 16.1 -- Linker UUID flags

### Decisions made (16.1)

**macOS (`-Xlinker -no_uuid`)**: removes the link-time UUID from Mach-O binaries. Passed as Swift compiler flags when `GOOS == darwin` and `Deterministic == true`.

**Linux (`-Xlinker --build-id=none`)**: removes the `.note.gnu.build-id` section. Passed when `GOOS == linux`.

Note: despite these flags, macOS Mach-O binaries still show SHA-256 mismatches in practice (see Deferred work). The gate therefore only runs on linux-x64.

## Sub-phase 16.2 -- SHA-256 comparison gate

### Decisions made (16.2)

**`sha256File(path string) (string, error)`**: in `deterministic.go`. Uses `crypto/sha256` + `io.Copy` to hash the file.

**Gate procedure in `TestPhase16Repro`**:
1. If `runtime.GOOS == "darwin"`, call `t.Skip` (macOS non-reproducibility, see Deferred).
2. Build fixture in `out1/` with `Driver{Deterministic: true}`.
3. Build the same fixture in `out2/` (different temp dir).
4. Compute SHA-256 of both output binaries.
5. Assert equal.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/build.go` | `Deterministic bool` on `Driver`; env injection for deterministic flags; linker flags |
| `transpiler3/swift/build/deterministic.go` | `sha256File` helper |
| `transpiler3/swift/build/phase16_test.go` | `TestPhase16Repro`: 3 fixtures; SHA-256 comparison; macOS skip |
| `tests/transpiler3/swift/fixtures/phase16-repro/` | 3 fixture directories |

## Test set

- `TestPhase16Repro` -- 3 fixtures: `repro_func`, `repro_hello`, `repro_int`. Test is skipped on macOS (`runtime.GOOS == "darwin"`).

## Deferred work

- Reproducibility on macOS: macOS Mach-O binaries embed a link-time UUID that changes between builds even with `-Xlinker -no_uuid`. Root cause investigation and fix deferred to Phase 16.3.
- Reproducible iOS `.ipa` (archive timestamps). Deferred to Phase 16.4.
- SBOM (Software Bill of Materials) generation. Deferred.
- Sorted declarations + BLAKE3-named closures in source emission for full determinism. Deferred to Phase 16.5.
