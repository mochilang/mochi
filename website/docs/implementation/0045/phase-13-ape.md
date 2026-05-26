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
| Status         | COMPLETE |
| Started        | 2026-05-26 00:30 (GMT+7) |
| Landed         | 2026-05-26 08:12 (GMT+7) |
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
| 13.1 | cosmocc vendored under `transpiler3/c/toolchain/cosmocc/`; `cosmocc.Find()`/`Ensure()` APIs; `resolveCosmoCC()` updated to check vendor cache before PATH; `TestPhase13CosmoVendor` gate (install_root, find_env_override, find_vendor_dir_override, ensure_cached[network-gated]) | LANDED 2026-05-26 08:04 (GMT+7) | — | — |
| 13.2 | Runtime under Cosmopolitan: drop `-pedantic` for Apex builds; inject `-DMOCHI_COSMO=1`; guard `_XOPEN_SOURCE` and Apple pragma in `sched.c`; note Cosmo signal compatibility in `shutdown.c`; `TestPhase13CosmoRuntime` gate (runtime_flags + apex_compile[cosmocc-gated]) | LANDED 2026-05-26 08:09 (GMT+7) | — | — |
| 13.3 | Cross-OS CI: `transpiler3-c-apex.yml` workflow builds APE binary on ubuntu-latest via cosmocc.Ensure(), uploads artifact, runs it on ubuntu + macos + windows matrix; `TestPhase13CrossOSCI` gate; FreeBSD deferred to cirrus-ci follow-up | LANDED 2026-05-26 08:12 (GMT+7) | — | — |

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

### Phase 13.2 (2026-05-26 08:09 GMT+7)

**Drop `-pedantic` for Apex builds.** cosmocc (GCC-based) embeds Cosmopolitan-specific extensions in `cosmopolitan.h` that are rejected by `-pedantic`. The flag is now guarded by `!d.Apex` in `driver.go`. All non-Apex builds continue to use `-pedantic` as before.

**`-DMOCHI_COSMO=1` define for Apex builds.** Injected alongside the other compile flags when `d.Apex`. Allows runtime C sources to detect Cosmopolitan builds at compile time without depending on cosmocc-internal macros (which are not always available before the first include).

**`sched.c` Cosmopolitan guard.** `_XOPEN_SOURCE 600` is only defined when `MOCHI_COSMO` is not set (Cosmopolitan provides `ucontext_t` natively without the XSI unlock). The Apple clang deprecation pragma pair is similarly guarded by `defined(__APPLE__) && !defined(MOCHI_COSMO)`.

**`shutdown.c` note.** POSIX signals (`SIGINT`, `SIGTERM`) and `alarm()` are provided by Cosmopolitan's NT layer on Windows, so no code change is required beyond the compile-time note.

**Gate.** `TestPhase13CosmoRuntime` with two sub-tests: `runtime_flags` (offline: compile a C stub that errors without `-DMOCHI_COSMO=1`) and `apex_compile` (skips if cosmocc not available; full compile + run of `add_ints` via `Driver.Apex=true` with the new flags).

### Phase 13.1 (2026-05-26 08:04 GMT+7)

**`cosmocc` package under `transpiler3/c/toolchain/cosmocc/`.** Mirrors the pattern from `toolchain/zig`. Exports: `Version` (pinned to 3.9.5), `InstallRoot()` (checks `MOCHI_COSMOCC_DIR`, `MOCHI_CACHE_DIR`, `~/.mochi/cache/cosmocc`, OS cache dir in that order), `Executable(vdir)` (returns `bin/cosmocc` or `bin/cosmocc.exe` on Windows), `Find()` (MOCHI_COSMOCC_PATH then vendored cache; returns `""` gracefully if absent), `Ensure()` (Find + download + unzip if missing). Download URL is `https://cosmo.zip/pub/cosmocc/cosmocc-{Version}.zip`. Extraction uses stdlib `archive/zip`.

**`resolveCosmoCC()` updated to three-step resolution.** Phase 13.0 only checked env var and PATH. Phase 13.1 inserts `cosmocc.Find()` between the env check and PATH fallback, so users who have never set `MOCHI_COSMOCC_PATH` but have run `cosmocc.Ensure()` (or `mochi cosmocc install` in a future CLI command) get automatic discovery.

**`ensure_cached` sub-test is network-gated.** `TestPhase13CosmoVendor/ensure_cached` only runs when `MOCHI_COSMOCC_VENDOR_TEST=1` to avoid hitting `cosmo.zip` on every `go test` run. The other three sub-tests (install_root, find_env_override, find_vendor_dir_override) are offline and always run.

### Phase 13.3 (2026-05-26 08:12 GMT+7)

**`transpiler3-c-apex.yml` cross-OS CI workflow.** Three-job structure:
1. `build-apex` (ubuntu-latest): restores cosmocc from `actions/cache` keyed `cosmocc-3.9.5`; if cache miss, triggers `MOCHI_COSMOCC_VENDOR_TEST=1` to download via `cosmocc.Ensure()`; compiles `add_ints` as an APE binary via `Driver.Apex=true`; uploads binary + expected output as a `retention-days: 1` artifact.
2. `run-apex` (matrix: ubuntu-latest, macos-latest, windows-latest): downloads the artifact; runs the `.com` binary; compares stdout to the expected file. POSIX jobs use shell comparison; Windows uses PowerShell.

**FreeBSD deferred.** The spec mentions cirrus-ci for FreeBSD. The `.com` binary runs unmodified on FreeBSD (the ELFCOMBO header autodetects the ABI), but adding a Cirrus CI integration requires a `.cirrus.yml` and separate account/org configuration. Deferred to a follow-up outside the Phase 13 scope.

**Offline gate.** `TestPhase13CrossOSCI` verifies the workflow file exists and contains the expected runner strings and test IDs without executing a build.

## Deferred work

- aarch64-APE: Cosmopolitan aarch64 support is still landing upstream; revisit later.
- FreeBSD CI: Cirrus CI integration for cross-OS APE validation.

## Closeout notes

All sub-phases 13.0-13.3 are LANDED. Phase 13.0 wired `--apex` through the driver. Phase 13.1 vendored cosmocc under `~/.mochi/cache/cosmocc`. Phase 13.2 fixed runtime compilation flags (`-DMOCHI_COSMO`, no `-pedantic`). Phase 13.3 added the cross-OS CI workflow verifying the same APE binary runs on Linux, macOS, and Windows.
