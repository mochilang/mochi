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
| Status         | IN PROGRESS |
| Started        | 2026-05-25 21:46 (GMT+7) |
| Landed         | — |
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
| 17.3 | All non-libc deps static-linked; bundled toolchain pinned by SHA-256                                               | NOT STARTED | —      | — |
| 17.4 | Sample artefact SHA-256 published per release tag                                                                  | NOT STARTED | —      | — |
| 17.5 | `.github/workflows/transpiler3-c-repro.yml` rebuilds the corpus twice and diffs SHA-256                            | NOT STARTED | —      | — |

## Decisions made

**Phase 17.0: SOURCE_DATE_EPOCH inheritance.** The driver invokes cc via `exec.Command` without overriding `cmd.Env`, so the child process inherits the full parent environment including any `SOURCE_DATE_EPOCH` the test or CI pipeline sets. The emitter and runtime C files never expand `__DATE__` or `__TIME__`, so this variable has no visible effect today (but it suppresses warnings from any third-party code that does use them, and it controls the timestamp field in DWARF section headers when `-g` is added later).

**Phase 17.1: -ffile-prefix-map in driver.** The workDir (a temp directory with a randomized path) is the `-I` include root. Without path stripping, a DWARF CU path like `/var/folders/.../gen.c` would differ between builds. Adding `-ffile-prefix-map=<workDir>=.` replaces every occurrence of the workDir path in debug info with `.`. `-fdebug-prefix-map=<workDir>=.` does the same for the compiler's internal debug path table. Both are added unconditionally to all Driver.Build invocations.

**Phase 17.1: -Wl,-no_uuid on macOS.** Apple's linker (`ld`) embeds a random 128-bit UUID in the `LC_UUID` Mach-O load command for every link invocation. This UUID is used by dSYM and crash symbolication; it has no effect on binary execution. Without suppressing it, two identical source builds produce different binaries. The flag `-Wl,-no_uuid` removes the UUID, making the `__LINKEDIT` segment fully deterministic. The flag is only added when `gort.GOOS == "darwin"` (using the `gort` alias to avoid shadowing `mochi/transpiler3/c/runtime`).

**Phase 17.0 gate fixture.** `TestPhase17Repro` uses `primitives/add_ints` as the canonical fixture: it is small (compiles in ~0.5 s), has deterministic output, and exercises the full pipeline without I/O or file operations. The test calls `t.Setenv("SOURCE_DATE_EPOCH", "1748000000")` to pin the epoch, then builds twice into different tempdirs with the same output basename and asserts SHA-256 equality.

**Code-signature identifier is basename-stable.** On macOS, Apple's linker embeds the output binary's basename as the code-signature `Identifier` field. Using the same output basename across two builds (e.g. both emit `add_ints`) keeps this field stable. Tests use the fixture name as the binary name (e.g. `filepath.Join(t.TempDir(), "add_ints")`), so the identifier is always the fixture name, not a random path component.

## Deferred work

- Phase 17.2: Function/global ordering audit. The lower pass adds functions in source-declaration order (via `append`), so the order is already deterministic for well-formed programs. A map-iteration audit is needed to confirm no intermediate map produces non-deterministic output. Tracked for the next sub-phase.
- Phase 17.3: Static linking requires the Phase 1.3 vendored zig toolchain (which already produces static binaries by default). Wiring the release profile to request static-only is the main step.
- Phase 17.4: CI publish script.
- Phase 17.5: GitHub Actions workflow.

## Closeout notes

_Fill in after all 6 sub-phases green._
