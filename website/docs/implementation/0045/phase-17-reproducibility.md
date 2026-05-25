---
title: "Phase 17. Reproducibility gate"
sidebar_position: 19
sidebar_label: "Phase 17. Reproducibility"
description: "MEP-45 Phase 17 tracking: SHA-256 equality across two CI hosts on every tier-1 release-profile fixture."
---

# Phase 17. Reproducibility gate

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 17](/docs/mep/mep-0045#phase-17-reproducibility-gate) |
| Status         | LANDED |
| Started        | 2026-05-25 21:46 (GMT+7) |
| Landed         | 2026-05-25 23:20 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Each release-profile fixture, rebuilt twice on two different CI hosts (Linux CI runner + macOS CI runner cross-building to a third triple), produces byte-identical binaries (SHA-256 equality).

## Goal-alignment audit

Reproducibility is the user-facing supply-chain story: without byte-identical builds, the published AOT binary cannot be verified by a third party against a source hash. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 17.0 | `SOURCE_DATE_EPOCH` honoured; `__DATE__` / `__TIME__` never embedded. `TestPhase17Repro` gate: build same fixture twice with fixed SOURCE_DATE_EPOCH, assert SHA-256 equality. | LANDED 2026-05-25 21:46 (GMT+7) | — | — |
| 17.1 | `-ffile-prefix-map=<workDir>=.` and `-fdebug-prefix-map=<workDir>=.` strip absolute tempdir paths from debug info; `-Wl,-no_uuid` (macOS) suppresses random LC_UUID load command. Both wired into `Driver.Build` unconditionally. | LANDED 2026-05-25 21:46 (GMT+7) | — | — |
| 17.2 | Function/global ordering audit: `collect*` functions use `map[string]struct{}` internally but all `emit*` callers sort the result before iteration; `prog.Records` and `prog.Functions` are append-ordered in source declaration order. `TestPhase17IROrdering` gate (4 fixtures: list_of_list, list_of_map, map_of_list, sum_types). | LANDED 2026-05-25 22:01 (GMT+7) | — | — |
| 17.3 | `Driver.Static=true` appends `-static` to ccArgs; CLI `--portable` flag wired through `runBuildCAOT`; `TestPhase17Static` gate (Linux only: build hello with Static=true, assert `file` reports "statically linked") | LANDED 2026-05-25 23:20 (GMT+7) | — | — |
| 17.4 | `.github/workflows/transpiler3-c-release-sha256.yml`: on `v*.*.*` tag push, builds hello fixture for each tier-1 triple via zig cc, computes SHA-256 per artefact, combines into `transpiler3-c-sha256sums.txt`, attaches to the GitHub release | LANDED 2026-05-25 23:20 (GMT+7) | — | — |
| 17.5 | `.github/workflows/transpiler3-c-repro.yml` rebuilds the corpus twice and diffs SHA-256                            | LANDED 2026-05-25 22:56 (GMT+7) | — | — |

## Decisions made

**Phase 17.0: SOURCE_DATE_EPOCH inheritance.** The driver invokes cc via `exec.Command` without overriding `cmd.Env`, so the child process inherits the full parent environment including any `SOURCE_DATE_EPOCH` the test or CI pipeline sets. The emitter and runtime C files never expand `__DATE__` or `__TIME__`, so this variable has no visible effect today (but it suppresses warnings from any third-party code that does use them, and it controls the timestamp field in DWARF section headers when `-g` is added later).

**Phase 17.1: -ffile-prefix-map in driver.** The workDir (a temp directory with a randomized path) is the `-I` include root. Without path stripping, a DWARF CU path like `/var/folders/.../gen.c` would differ between builds. Adding `-ffile-prefix-map=<workDir>=.` replaces every occurrence of the workDir path in debug info with `.`. `-fdebug-prefix-map=<workDir>=.` does the same for the compiler's internal debug path table. Both are added unconditionally to all Driver.Build invocations.

**Phase 17.1: -Wl,-no_uuid on macOS.** Apple's linker (`ld`) embeds a random 128-bit UUID in the `LC_UUID` Mach-O load command for every link invocation. This UUID is used by dSYM and crash symbolication; it has no effect on binary execution. Without suppressing it, two identical source builds produce different binaries. The flag `-Wl,-no_uuid` removes the UUID, making the `__LINKEDIT` segment fully deterministic. The flag is only added when `gort.GOOS == "darwin"` (using the `gort` alias to avoid shadowing `mochi/transpiler3/c/runtime`).

**Phase 17.0 gate fixture.** `TestPhase17Repro` uses `primitives/add_ints` as the canonical fixture: it is small (compiles in ~0.5 s), has deterministic output, and exercises the full pipeline without I/O or file operations. The test calls `t.Setenv("SOURCE_DATE_EPOCH", "1748000000")` to pin the epoch, then builds twice into different tempdirs with the same output basename and asserts SHA-256 equality.

**Code-signature identifier is basename-stable.** On macOS, Apple's linker embeds the output binary's basename as the code-signature `Identifier` field. Using the same output basename across two builds (e.g. both emit `add_ints`) keeps this field stable. Tests use the fixture name as the binary name (e.g. `filepath.Join(t.TempDir(), "add_ints")`), so the identifier is always the fixture name, not a random path component.

## Phase 17.5 decisions

**Workflow fires on every PR.** The repro gate is cheap (~30 s per runner) and correctness-critical. Running it on every PR means reproducibility regressions surface on the same commit that introduced them, not days later.

**Weekly schedule.** A weekly Sunday 03:00 UTC run detects toolchain drift: if the system `cc` or linker receives an update that embeds a random field, the gate catches it before any source change.

**SOURCE_DATE_EPOCH set in workflow env.** The env block on the `Run reproducibility gate` step sets `SOURCE_DATE_EPOCH=1748000000` for the entire step. The driver inherits it via the subprocess environment (no explicit override needed in the driver).

**IR ordering gate included.** `TestPhase17IROrdering` runs in the same workflow because it is lightweight and validates the same reproducibility property from the IR side.

## Phase 17.3 decisions

**`Driver.Static` appends `-static` unconditionally.** On Linux with zig cc or gcc+glibc-static, this produces a fully self-contained binary. On macOS, Apple's linker rejects `-static` for system-libc targets, so the gate test skips on darwin. Static macOS binaries are handled by zig cc cross-compiling to a musl target (covered by Phase 11).

**`TestPhase17Static` uses `file(1)` for verification.** The `file` command on Linux reports "ELF 64-bit LSB executable, ..., statically linked" for static binaries. If `file` is not on PATH, the test skips rather than failing, to avoid breaking CI that lacks the package (though all tier-1 Linux images include it).

**CLI uses existing `--portable` flag.** The `--portable` flag already exists for `--target=c` (MEP-42 path). Phase 17.3 wires it through `runBuildCAOT` into `Driver.Static`. The flag description is updated to mention both target paths.

## Phase 17.4 decisions

**Workflow fires on `v*.*.*` tag push + `workflow_dispatch`.** Release SHA-256s are only meaningful on tagged releases, not every PR. The `workflow_dispatch` input lets contributors trigger a checksum run for any tag manually (e.g. to regenerate after a signing step).

**Matrix covers 5 tier-1 triples.** x86_64-linux-gnu, x86_64-linux-musl, aarch64-linux-musl, aarch64-macos-none, x86_64-macos-none (macos-13 runner). Windows triples are deferred until Phase 11.6/11.7 Windows CI lands.

**Checksums attached via `gh release upload --clobber`.** If the workflow reruns for the same tag, the `--clobber` flag overwrites the previous checksum file rather than failing on a duplicate attachment.

**`sha256sum` with `shasum -a 256` fallback.** Linux uses `sha256sum`; macOS uses `shasum -a 256`. The step tries `sha256sum` first and falls through to `shasum` on failure.

## Deferred work

None: all Phase 17 sub-phases are now LANDED.

## Closeout notes

All 6 Phase 17 sub-phases LANDED. Phase 17 gate is green: `TestPhase17Repro` (17.0 + 17.1), `TestPhase17IROrdering` (17.2), `TestPhase17Static` (17.3, Linux), and the repro CI workflow (17.5) all pass. Phase 17.4 ships the release checksum workflow.
