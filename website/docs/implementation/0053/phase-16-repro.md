---
title: "Phase 16. Reproducible build"
sidebar_position: 18
sidebar_label: "Phase 16. Repro"
description: "MEP-53 Phase 16, byte-identical binaries across two builds via SOURCE_DATE_EPOCH=0 + RUSTFLAGS=-C strip=symbols (macOS LC_UUID skip)."
---

# Phase 16. Reproducible build gate

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22613](https://github.com/mochilang/mochi/issues/22613) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 7e013a1d1a |

## Gate

`TestPhase16Repro` walks `tests/transpiler3/rust/fixtures/phase16-repro/` (7 fixtures), builds each fixture twice with `Driver{Deterministic: true, NoCache: true}`, and SHA-256 compares the two resulting binaries. On macOS, the gate platform-skips: the Mach-O `LC_UUID` load command is randomised per link by `ld64` and cannot be neutralised from rustc-side flags.

## Lowering decisions

`Driver.Deterministic=true` triggers three changes to the cargo invocation:

1. **`SOURCE_DATE_EPOCH=0`** in the env, so cargo respects a fixed timestamp for `CARGO_PKG_*` env vars and any build-time `chrono::Utc::now()` (cargo embeds the build timestamp into the binary in certain configurations).
2. **`RUSTFLAGS=-C strip=symbols`**, which strips symbol tables (carrying path-dependent debug info and build IDs) post-link.
3. **`--locked`**, which forces cargo to use `Cargo.lock` exactly and refuses to update any dependency.

Combined, these three flags produce SHA-256-byte-identical binaries on Linux across two builds with the same inputs.

On macOS, the `LC_UUID` load command in Mach-O binaries is computed from a hash of the file's contents at link time, then re-randomised across re-links. Apple's `ld64` (the system linker) does not expose a `--no-uuid` flag (unlike `lld`, which does). The MEP rejected using `lld` on darwin because that would require an extra toolchain install for users who otherwise want Apple's stock toolchain. The gate platform-skips on darwin and asserts only on Linux.

The fixture set (7 fixtures) is small to keep CI time reasonable — building 7 fixtures twice each is ~30s warm. Coverage: hello world, arithmetic, lists, records, sums, closures, and a 200-LOC composite test.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/build/build.go` | Wire `Deterministic` env / RUSTFLAGS / --locked |
| `transpiler3/rust/build/phase16_test.go` | 7-fixture double-build + SHA-256 compare gate, platform-skip on darwin |
| `tests/transpiler3/rust/fixtures/phase16-repro/*.mochi` + `.out` | 7 fixtures |

## Test set

- `TestPhase16Repro/<fixture>` for each `.mochi` in the fixture directory (7 fixtures).

## Closeout notes

Reproducibility under cargo is a moving target. Rust 1.84 (December 2024) made symbol-table emission deterministic by default; before that, RUSTFLAGS=-C strip=symbols was needed for symbols too. The MEP-53 implementation always sets the strip flag for safety against older rustc versions a user might pin.

A future sub-phase 16.1 could exercise the same gate under cargo-zigbuild on x86_64-unknown-linux-musl and aarch64-unknown-linux-musl. Currently the cross-build path is exercised in `Driver.Build` but not gated per-fixture in CI.

The macOS skip is documented in the test source for clarity:

```go
if runtime.GOOS == "darwin" {
    t.Skip("LC_UUID randomised per link; reproducibility cannot be enforced on darwin without lld")
}
```
