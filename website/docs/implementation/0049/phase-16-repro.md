---
title: "Phase 16. Reproducible build"
sidebar_position: 20
sidebar_label: "Phase 16. Reproducible build"
description: "MEP-49 Phase 16 — deterministic .o and binary output via SWIFTPM_DETERMINISTIC_BUILD, SOURCE_DATE_EPOCH, -Xlinker -no_uuid; SHA-256 comparison across machines."
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

`TestPhase16Repro`: SHA-256 of the final binary matches across two independent builds (different machine timestamps, different `$HOME`, different `$TMPDIR`). Gate runs on linux-x64. 10 fixture programs.

## Goal-alignment audit

Reproducible builds enable supply-chain verification: a user can rebuild from source and verify the binary matches the published one bit-for-bit. This is increasingly required by enterprise security policies. For Mochi's iOS target, reproducible archives enable App Store submission verification. The gate is strict: byte-identical output across two independent builds.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `SWIFTPM_DETERMINISTIC_BUILD=1` flag; `SOURCE_DATE_EPOCH` for file timestamps | LANDED | — |
| 16.1 | `-Xlinker -no_uuid` (macOS) / `--build-id=none` (Linux ld) to remove link-time UUIDs | LANDED | — |
| 16.2 | Sorted imports and declarations in emitted `.swift` source; deterministic sxtree traversal | LANDED | — |
| 16.3 | SHA-256 comparison gate: two parallel builds, compare binary hashes | LANDED | — |

## Sub-phase 16.0 -- SwiftPM deterministic build

### Decisions made (16.0)

**`SWIFTPM_DETERMINISTIC_BUILD=1`**: an environment variable recognised by SwiftPM (introduced in Swift 5.8). When set, SwiftPM:
- Uses a fixed seed for any random identifiers in generated code.
- Passes `-Xfrontend -enable-experimental-feature -Xfrontend StrictConcurrency` deterministically.
- Suppresses timestamp-based cache invalidation.

The Mochi build driver always sets `SWIFTPM_DETERMINISTIC_BUILD=1` when `--deterministic` flag is passed (or `MOCHI_DETERMINISTIC=1` env var is set). The `TestPhase16Repro` gate always sets it.

**`SOURCE_DATE_EPOCH`**: the Unix timestamp used for all file modification times in the build output. Set to a fixed value (e.g., `0` for epoch) when `--deterministic` is active:

```bash
export SOURCE_DATE_EPOCH=0
swift build -c release
```

The Swift compiler and linker respect `SOURCE_DATE_EPOCH` when building for release.

**Compiler flags**: the generated `Package.swift` passes these Swift settings in deterministic mode:

```swift
.unsafeFlags([
    "-Xfrontend", "-disable-reflection-metadata",
    "-whole-module-optimization",
])
```

`-whole-module-optimization` forces single-file compilation, which produces identical output regardless of file processing order.

## Sub-phase 16.1 -- Linker UUID removal

### Decisions made (16.1)

**macOS (`-Xlinker -no_uuid`)**: the macOS linker (`ld64`) embeds a UUID in each binary by default. This UUID is derived from the link timestamp and is non-deterministic. `-Xlinker -no_uuid` removes it. The generated `Package.swift` adds this flag in deterministic mode:

```swift
linkerSettings: [
    .unsafeFlags(["-Xlinker", "-no_uuid"], .when(platforms: [.macOS])),
]
```

**Linux (`-Xlinker --build-id=none`)**: the Linux linker (`lld` or `gold`) embeds a `.note.gnu.build-id` section derived from the binary content (content-addressed, not time-based). This is actually deterministic for the same input. For strict reproducibility, `--build-id=none` removes it:

```swift
linkerSettings: [
    .unsafeFlags(["-Xlinker", "--build-id=none"], .when(platforms: [.linux])),
]
```

**Object file timestamps**: Swift's `-c release` mode does not embed timestamps in `.o` files. No additional flag needed.

## Sub-phase 16.2 -- Deterministic source emission

### Decisions made (16.2)

**Sorted imports**: the emitter sorts `import` statements alphabetically. Swift imports in the sxtree `SourceFile` node are sorted before `Render()` is called. This prevents import order from depending on the order types were encountered during lowering.

**Sorted declarations**: within a `.swift` file, top-level declarations are emitted in a canonical order:
1. `import` statements (sorted alphabetically)
2. Type declarations (sorted by name)
3. Extension blocks (sorted by extended type name, then by method name)
4. Top-level functions (sorted by name)
5. `@main` entry struct (always last)

This ordering is enforced in the sxtree `SourceFile.Render()` method.

**Closure names**: anonymous closures (lifted functions) are named by a BLAKE3 hash of their source location (file + line + column in the original Mochi source). This makes closure names stable across refactors that don't touch the closure itself. BLAKE3 is chosen for speed (no crypto needed; just naming stability).

**Source maps**: the `.mochi.map` sidecar files (Phase 17.1, deferred) are written in deterministic order (sorted by source location).

## Sub-phase 16.3 -- SHA-256 comparison gate

### Decisions made (16.3)

**Gate procedure**:

1. Build `fixture.mochi` in `build_1/` with `MOCHI_DETERMINISTIC=1 SOURCE_DATE_EPOCH=0`.
2. Build the same `fixture.mochi` in `build_2/` with `MOCHI_DETERMINISTIC=1 SOURCE_DATE_EPOCH=0`, but with `TMPDIR=/tmp/build2` and `HOME=/tmp/home2` to simulate different machine environment.
3. SHA-256 both output binaries.
4. Assert SHA-256 hashes are equal.

**What is being compared**: the final linked binary (`.elf` on Linux, `Mach-O` on macOS). Not the intermediate `.o` files (which may contain debug paths that differ by build directory).

**Release mode**: only `-c release` builds are tested for reproducibility. `-c debug` builds embed debug info with absolute paths, making them inherently non-reproducible across machines.

**Known non-reproducibilities**: if any are discovered, they are logged and a flag `--accept-nondeterminism=<reason>` is added for that specific fixture until fixed. The gate fails by default if any nondeterminism is detected.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/deterministic.go` | `--deterministic` flag handling; `SWIFTPM_DETERMINISTIC_BUILD`, `SOURCE_DATE_EPOCH` injection |
| `transpiler3/swift/build/package.go` | `-Xlinker -no_uuid` / `--build-id=none` in deterministic mode |
| `transpiler3/swift/emit/emit.go` | Sorted imports, sorted declarations, BLAKE3-named closures |
| `transpiler3/swift/build/phase16_test.go` | `TestPhase16Repro`: 10 fixtures, two-build SHA-256 comparison |
| `tests/transpiler3/swift/fixtures/phase16-repro/` | 10 fixture directories |

## Test set

- `TestPhase16Repro` -- 10 fixtures: `repro_hello`, `repro_scalars`, `repro_list`, `repro_record`, `repro_sum`, `repro_closure`, `repro_agent`, `repro_query`, `repro_ffi`, `repro_multifile`.

## Deferred work

- Reproducible iOS `.ipa` (archive timestamps). Deferred to Phase 16.1.
- SBOM (Software Bill of Materials) generation. Deferred to a future tooling phase.
- Reproducibility on macOS (Mach-O UUID). Tested but macOS CI is slower; tracked separately.
