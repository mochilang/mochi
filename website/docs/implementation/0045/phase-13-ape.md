---
title: "Phase 13. APE / Cosmopolitan"
sidebar_position: 15
sidebar_label: "Phase 13. APE"
description: "MEP-45 Phase 13 tracking: --apex build path via cosmocc; one APE binary that runs unmodified on linux+macOS+windows+BSDs."
---

# Phase 13. APE / Cosmopolitan

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 13](/docs/mep/mep-0045#phase-13-ape--cosmopolitan) |
| Status         | IN PROGRESS |
| Started        | 2026-05-26 00:30 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`mochi build --apex` produces one APE binary; the same binary runs and produces byte-equal output on Linux, macOS, Windows, FreeBSD, NetBSD, OpenBSD CI runners.

## Goal-alignment audit

APE is the most striking distribution story Mochi can tell: one file, every desktop OS, no install required. Phase 13.0 wires the `--apex` flag through the driver and CLI, resolves cosmocc from `MOCHI_COSMOCC_PATH` or PATH, skips all cosmocc-incompatible flags (`-target`, `-ffile-prefix-map`, `-Wl,-no_uuid`, `-static`, sanitisers), and gates the result with `TestPhase13APE`. The gate test skips gracefully when cosmocc is not installed so dev hosts without cosmocc are unaffected. Aligns directly with user-facing goal: one command produces a portable executable.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 13.0 | `Driver.Apex bool` + `resolveCosmoCC()` (MOCHI_COSMOCC_PATH then PATH); driver skips `-target`, `-ffile-prefix-map`, `-fdebug-prefix-map`, `-Wl,-no_uuid`, `-static`, sanitiser flags for Apex builds; `--apex` CLI flag wired to `Driver.Apex`; `TestPhase13APE` gate (add_ints compile + run, skips when cosmocc absent) | LANDED 2026-05-26 00:30 (GMT+7) | — | — |
| 13.1 | cosmocc vendored under `transpiler3/c/toolchain/cosmocc/` (eliminates MOCHI_COSMOCC_PATH requirement)             | NOT STARTED | —      | — |
| 13.2 | Runtime under Cosmopolitan: BDWGC compatibility, stream/agent surface preserved                                    | NOT STARTED | —      | — |
| 13.3 | Cross-OS CI runners: Linux + macOS + Windows + FreeBSD (cirrus-ci)                                                 | NOT STARTED | —      | — |

## Decisions made

**Phase 13.0: cosmocc not vendored (deferred to 13.1).** The spec originally called for cosmocc to be vendored under `transpiler3/c/toolchain/cosmocc/` in phase 13.0. However, the Cosmopolitan toolchain ships as a self-contained tarball (~100 MB) whose installation procedure differs from zig (no HTTP fetch with SHA-256 manifest). To keep 13.0 focused and shippable, vendoring is deferred to 13.1. Phase 13.0 resolves cosmocc from `MOCHI_COSMOCC_PATH` env var or PATH instead, and the gate test skips gracefully when neither is set. This means Phase 13.0 can land the entire driver + CLI + test infrastructure without requiring cosmocc on every dev host or CI runner.

**Phase 13.0: driver flag guards for Apex builds.** cosmocc does not accept several flags that the standard cc invocation passes:
1. `-target <triple>`: cosmocc targets its own "cosmopolitan" ABI internally; it does not use LLVM target triples.
2. `-ffile-prefix-map` / `-fdebug-prefix-map`: cosmocc uses its own DWARF path scheme; these flags cause build errors.
3. `-Wl,-no_uuid`: Apple linker flag; not applicable to cosmocc's linker.
4. `-static`: cosmocc always links statically with cosmopolitan libc by design.
5. `-fsanitize=address,undefined`: sanitisers require a platform libc that cosmocc replaces.

All five are suppressed by the existing `d.Apex` guards added to `build/driver.go`. The `isWasm` and `d.Apex` booleans are evaluated once at the top of the flag section for readability.

**Phase 13.0: MOCHI_COSMOCC_PATH takes priority over PATH.** This mirrors the pattern used by other mochi toolchain overrides (`MOCHI_CC`, `CC`). Users who install cosmocc to a non-standard path (e.g. a local build or CI cache) can set `MOCHI_COSMOCC_PATH` without polluting their `PATH`.

**Phase 13.0: gate test skips rather than fails when cosmocc absent.** `TestPhase13APE` checks `MOCHI_COSMOCC_PATH` and then `exec.LookPath("cosmocc")`. If neither is found, the test calls `t.Skip(...)` with a hint message. This mirrors the wasmtime pattern in Phase 12.0. CI runners that have cosmocc will exercise the full compile + run gate; all other environments pass without installing anything.

## Deferred work

- Phase 13.1: cosmocc vendored (eliminates MOCHI_COSMOCC_PATH requirement for end users and CI).
- Phase 13.2: streams/agents under Cosmopolitan (deferred; Phase 9 not yet landed).
- Phase 13.3: cross-OS CI matrix (Linux + macOS + Windows + FreeBSD).
- aarch64-APE: Cosmopolitan aarch64 support is still landing upstream; revisit later.

## Closeout notes

_Fill in after gate green._
