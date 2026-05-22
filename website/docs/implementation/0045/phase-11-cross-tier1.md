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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Every Phase 1-10 fixture compiles via `mochi build --target=<triple>` from every supported host and runs byte-equal vs vm3 on the target (native on macOS+Linux, qemu-user-static for cross-arch Linux, wasmtime for wasi).

## Goal-alignment audit

_To be written before sub-phase 11.0 starts. Tier-1 cross is the user-facing payoff of MEP-45: one source, every native target. Aligns._

## Sub-phases (one per target)

| #    | Target                          | Status      | Commit | PR |
|------|---------------------------------|-------------|--------|----|
| 11.0 | x86_64-linux-gnu (native)       | NOT STARTED | —      | — |
| 11.1 | x86_64-linux-musl (zig cc)      | NOT STARTED | —      | — |
| 11.2 | aarch64-linux-gnu (zig + qemu)  | NOT STARTED | —      | — |
| 11.3 | aarch64-linux-musl (zig + qemu) | NOT STARTED | —      | — |
| 11.4 | aarch64-darwin (native macOS)   | NOT STARTED | —      | — |
| 11.5 | x86_64-darwin (zig cc cross)    | NOT STARTED | —      | — |
| 11.6 | x86_64-windows-msvc (clang-cl)  | NOT STARTED | —      | — |
| 11.7 | x86_64-windows-gnu (zig+mingw)  | NOT STARTED | —      | — |

## Deliverables

`.github/workflows/transpiler3-c-cross.yml` extends `cross-aot.yml`'s pattern: Linux CI runs Linux + cross-arch run-gates via qemu-user-static; macOS CI runs macOS run-gates; Windows CI runs Windows run-gates.

## Decisions made

_Fill in along the way._

## Deferred work

_Tier-2 (BSDs, riscv64, armv7, aarch64-windows): later, behind a `--tier=2` flag._

## Closeout notes

_Fill in after gate green on all 8 rows._
