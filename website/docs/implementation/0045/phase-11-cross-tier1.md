---
title: "Phase 11. Cross-compile tier-1 matrix"
sidebar_position: 13
sidebar_label: "Phase 11. Tier-1 cross"
description: "MEP-45 Phase 11 tracking: every Phase 1-10 fixture cross-built and run-gated on each of the 8 tier-1 triples."
---

# Phase 11. Cross-compile tier-1 matrix

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 11](/docs/mep/mep-0045#phase-11-cross-compile-tier-1-matrix) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 23:00 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Every Phase 1-10 fixture compiles via `mochi build --target=<triple>` from every supported host and runs byte-equal vs vm3 on the target (native on macOS+Linux, qemu-user-static for cross-arch Linux, wasmtime for wasi).

## Goal-alignment audit

Tier-1 cross is the user-facing payoff of MEP-45: one source, every native target, without installing a separate toolchain. The zig cc path means users only need `mochi build --triple=<target>` and the runtime downloads zig automatically. Aligns directly with user-facing goal.

## Sub-phases (one per target)

| #    | Target                          | Status      | Commit | PR |
|------|---------------------------------|-------------|--------|----|
| 11.0 | x86_64-linux-gnu (native)       | LANDED 2026-05-25 23:00 (GMT+7) | — | — |
| 11.1 | x86_64-linux-musl (zig cc)      | LANDED 2026-05-25 23:00 (GMT+7) | — | — |
| 11.2 | aarch64-linux-gnu (zig + qemu)  | LANDED 2026-05-25 23:00 (GMT+7) | — | — |
| 11.3 | aarch64-linux-musl (zig + qemu) | LANDED 2026-05-25 23:00 (GMT+7) | — | — |
| 11.4 | aarch64-darwin (native macOS)   | LANDED 2026-05-25 23:00 (GMT+7) | — | — |
| 11.5 | x86_64-darwin (zig cc cross)    | NOT STARTED | —      | — |
| 11.6 | x86_64-windows-msvc (clang-cl)  | NOT STARTED | —      | — |
| 11.7 | x86_64-windows-gnu (zig+mingw)  | NOT STARTED | —      | — |

## Deliverables

`.github/workflows/transpiler3-c-cross-tier1.yml` extends `cross-aot.yml`'s pattern: Linux CI runner covers 11.0-11.3 (native + zig cc + qemu); macOS CI runner covers 11.4 (native aarch64-darwin).

## Decisions made

**`resolveCCForTarget` in driver.** Phase 11 adds `resolveCCForTarget(target string)` to `Driver`. When `target != ""` and no explicit CC is set, it calls `zig.Install()` to get the bundled zig binary and uses `zig cc` as the compiler. This matches the MEP-42 pattern in `compiler3/build/c/driver.go:resolveCC`. The `-target=<triple>` flag is then prepended to `ccArgs` before the standard compile flags.

**`NoZigFallback` skip for cross.** If `Driver.NoZigFallback` is true and `target != ""`, the cross-compile path still falls back to `resolveCC` (host discovery) rather than downloading zig. This preserves the existing test contract that `NoZigFallback` prevents network access.

**`MOCHI_TEST_ZIG_DOWNLOAD=1` opt-in.** Cross tests that require zig (11.1-11.3) skip unless `MOCHI_TEST_ZIG_DOWNLOAD=1` is set, preventing accidental network access in the default `go test` run. The CI workflow sets this env var unconditionally.

**Native gates (11.0, 11.4) are unconditional.** `TestPhase11NativeLinux` and `TestPhase11NativeDarwin` use the host cc and never download zig, so they do not need the opt-in guard.

**qemu-based run gates.** Tests for aarch64 targets check for `qemu-aarch64-static` or `qemu-aarch64` on PATH. If neither is found, the test is compile-only (verifies the binary is produced without running it). The CI workflow installs `qemu-user-static` via apt before these tests run.

## Deferred work

- 11.5: x86_64-darwin cross requires zig cc on arm64 macOS with `-target=x86_64-macos` and Rosetta 2 or a second macOS runner. Runner constraints deferred.
- 11.6/11.7: Windows CI. Tracked as Phase 11.6/11.7 once a Windows CI runner with clang/zig is available.
- Tier-2 (BSDs, riscv64, armv7, aarch64-windows): behind a `--tier=2` flag.

## Closeout notes

Sub-phases 11.0-11.4 are LANDED. Sub-phases 11.5-11.7 require additional CI runner configurations.
