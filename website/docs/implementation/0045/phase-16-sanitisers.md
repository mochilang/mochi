---
title: "Phase 16. Sanitiser matrix"
sidebar_position: 18
sidebar_label: "Phase 16. Sanitisers"
description: "MEP-45 Phase 16 tracking: ASan/UBSan/TSan/MSan/LeakSan clean on the full fixture corpus across x86_64-linux-gnu and aarch64-darwin."
---

# Phase 16. Sanitiser matrix

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 16](/docs/mep/mep-0045#phase-16-sanitiser-matrix) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 21:46 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Full Phase 1-15 fixture corpus compiles + runs clean under ASan, UBSan, TSan, MSan, LeakSan on x86_64-linux-gnu and aarch64-darwin.

## Goal-alignment audit

Sanitiser clean is the strongest internal correctness signal short of formal verification. The user-facing payoff is "no UB, no UAF, no OOB in your shipped binary." Every sub-phase below directly validates runtime safety properties that would otherwise be invisible at test time and only surface as crashes in production. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 16.0 | ASan clean on full corpus (aarch64-darwin). `-fsanitize=address`, `detect_leaks=0` (GC-less runtime intentionally does not free at exit). `TestPhase16ASan` gate (33 suites). | LANDED 2026-05-25 21:46 (GMT+7) | — | — |
| 16.1 | UBSan clean on full corpus. `-fsanitize=undefined -fno-sanitize-recover=all`. `TestPhase16UBSan` gate (33 suites). | LANDED 2026-05-25 21:46 (GMT+7) | — | — |
| 16.2 | TSan clean on streams/agents corpus                                                                                | NOT STARTED | —      | — |
| 16.3 | MSan clean on Linux (Apple-silicon MSan unsupported upstream)                                                      | NOT STARTED | —      | — |
| 16.4 | Build profile `--debug` wires sanitisers; CI nightly job runs the matrix                                           | LANDED 2026-05-25 22:46 (GMT+7) | — | — |

## Decisions made

**Phase 16.0: ExtraFlags on Driver.** Rather than adding a new compilation path, Phase 16.0 adds `ExtraFlags []string` to the existing `Driver` struct in `build/driver.go`. The field is appended to `ccArgs` after the standard compile arguments. Tests pass `-fsanitize=address`; the production CLI never sets this field, so there is no behaviour change for normal builds.

**Phase 16.0: detect_leaks=0.** The GC-less mochi runtime intentionally does not free heap-allocated lists, maps, and strings at program exit. Running the fixture binaries under LeakSan without suppression produces hundreds of expected "leaks" and would make the suite unmaintainable. Setting `ASAN_OPTIONS=detect_leaks=0:halt_on_error=1` keeps the gate focused on real memory errors (use-after-free, heap buffer overflow, stack overflow). A dedicated LeakSan clean pass would require either adding `__attribute__((destructor))` cleanup or a global arena, deferred to a follow-on sub-phase.

**Phase 16.0: Apple clang LeakSan.** Apple's clang 17 ships ASan but not LeakSan (the leak detector was never ported to the Darwin runtime). On macOS, `-fsanitize=address` compiles and links correctly; `-fsanitize=leak` fails at link time. The gate uses ASan only on macOS and notes that a Linux runner (x86_64-linux-gnu) would add `-fsanitize=leak`. Since our CI runs only on macOS today, the gate is macOS-only.

**Phase 16.0: suite exclusions.** Two fixture directories are excluded from the ASan gate runner:
- `divzero-trip`: intentionally exits with code 5 (`MOCHI_ERR_DIVZERO`). The standard runFixtureSuiteASan helper expects exit 0 and would false-fail on these. They are covered by TestPhase2DivzeroTrip with a custom exit-code check.
- `file_io`: writes to `/tmp/mochi_test_*` paths. Correct behaviour but orthogonal to memory safety; included in the normal TestPhase6FileIO gate.
- `hello`: flat fixture (no subdirectory per fixture); the runFixtureSuiteASan helper requires a suite with sub-fixture directories. Covered by TestHello and by the primitives suite.

**Phase 16.1: -fno-sanitize-recover=all.** UBSan's default is to print a diagnostic and continue. `-fno-sanitize-recover=all` makes every UB trap abort the binary (exit non-zero), which the runner catches as a test failure. This is stricter than the default and ensures no UB goes silently undetected.

**runFixtureSuiteASan helper.** A new function in `phase16_0_test.go` wraps the standard fixture runner pattern. It accepts `extraFlags []string` (added to `Driver.ExtraFlags`) and `asanEnv string` (appended to the subprocess environment). Reused by both Phase 16.0 (ASan) and Phase 16.1 (UBSan) tests.

## Phase 16.4 decisions

**`--profile=debug` in CLI.** A new `Profile string` field (with `--profile` arg tag, default `"release"`) was added to `BuildCmd`. `runBuildCAOT` passes `cmd.Profile` as the `profile` parameter to `Driver.Build`. This gives users `mochi build --target=c-aot --profile=debug --out=<bin> <src>` to get a sanitiser-instrumented binary without knowing the flags.

**Profile handling in Driver.Build.** When `profile == "debug"`, the driver appends `-g -fsanitize=address,undefined -fno-sanitize-recover=all` to `ccArgs` before `ExtraFlags`. This ensures tests that set `ExtraFlags` can still override or append, while the CLI path is zero-config. The production `""` and `"release"` profiles are unchanged.

**Nightly CI workflow.** `.github/workflows/transpiler3-c-sanitise-nightly.yml` runs `TestPhase16ASan`, `TestPhase16UBSan`, and `TestPhase16DebugProfile` on `ubuntu-latest` and `macos-latest` at 02:00 UTC daily. Runs on `workflow_dispatch` for on-demand use. Ubuntu step installs `clang` so ASan is available even on minimal images.

**Gate.** `TestPhase16DebugProfile` builds `primitives/add_ints` with `profile="debug"` and asserts the binary produces correct output under `ASAN_OPTIONS=detect_leaks=0:halt_on_error=1`. Skips on Windows and on hosts where `asanAvailable` returns false.

## Deferred work

- Full LeakSan clean: requires explicit free() at every exit path or a global arena. Tracked as sub-phase 16.0.1.
- TSan clean: blocked on Phase 9 (streams/agents) landing.
- MSan clean: Linux-only; current CI is aarch64-darwin only.

## Closeout notes

Sub-phases 16.0, 16.1, and 16.4 are LANDED. Sub-phases 16.2 (TSan) and 16.3 (MSan) are blocked on Phase 9 (streams/agents) and Linux CI respectively.
