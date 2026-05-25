---
title: "Phase 18. Performance gate"
sidebar_position: 20
sidebar_label: "Phase 18. Performance"
description: "MEP-45 Phase 18 tracking: median fixture wall-clock within 2x of Go backend on BG corpus; per-release static report; regression alerts."
---

# Phase 18. Performance gate

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 18](/docs/mep/mep-0045#phase-18-performance-gate) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 22:01 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Median fixture wall-clock time on the BG corpus is within 2x of the equivalent Go-backend build, on x86_64-linux-gnu and aarch64-darwin.

## Goal-alignment audit

Performance gate exists so a regression cannot ship silently. The user-facing payoff is "your native AOT build is at least as fast as the Go-embedded VM build." Without a gate, a slow code-generation path could ship undetected and erode the core AOT value proposition. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 18.0 | Benchmark harness: `tests/transpiler3/c/bench/` with 5 BG kernels; `TestPhase18BenchHarness` builds + runs each 5x, logs median wall-clock and binary size; asserts output correctness vs vm3-derived expected values. | LANDED 2026-05-25 22:01 (GMT+7) | — | — |
| 18.1 | Wall-clock, peak RSS, binary size (release/strip), compile time recorded per fixture                               | NOT STARTED | —      | — |
| 18.2 | Per-release report published to a static page                                                                      | NOT STARTED | —      | — |
| 18.3 | Regression alert: > 10% wall-clock regression vs previous main posts a comment on the PR                           | NOT STARTED | —      | — |

## Decisions made

**Phase 18.0: kernel corpus.** Five kernels representative of different workload shapes:
- `hello_world`: baseline startup + print overhead (binary size reference)
- `sum_loop`: tight integer arithmetic loop (sum 1..1_000_000, no allocation)
- `fib_iter`: iterative Fibonacci(50) (function call + simple loop)
- `fib_rec`: recursive Fibonacci(35) (deep function-call overhead; tests stack frame efficiency)
- `list_sum`: list append × 10_000 + for-in iteration (allocation-heavy path)

All five use only features present in the current AOT transpiler (integer arithmetic, while loops, `fun` declarations, `list<int>`, `append`, `for x in xs`).

**Phase 18.0: expected outputs from vm3 oracle.** Each kernel's expected stdout was produced by running `mochi run <kernel>.mochi` and hardcoded in the test. The test asserts byte-exact match so any regression in correctness also shows up in the bench gate.

**Phase 18.0: harness runs each kernel 5 times.** Five runs reduces jitter from OS scheduling while keeping the total gate time under 10 s. Median (index 2 of sorted 5) is the reported figure. Min and max are also logged for variance analysis.

**Phase 18.0: 2x gate deferred to Phase 18.1.** Phase 18.0 only asserts correctness + records timing; no vm3 comparison is performed. The 2x comparison requires running `mochi run` for each kernel and timing it, plus deciding how to handle JIT warm-up effects. That is Phase 18.1 scope.

**Phase 18.0 measured results (aarch64-darwin, Apple clang 17, 2026-05-25):**

| kernel      | binsize | min_ms | med_ms | max_ms |
|-------------|---------|--------|--------|--------|
| hello_world | 73.9KiB |   4.97 |  11.50 | 289.79 |
| sum_loop    | 73.9KiB |   4.19 |  11.70 | 311.95 |
| fib_iter    | 73.9KiB |   3.82 |   4.16 | 369.75 |
| fib_rec     | 73.9KiB |  86.09 |  97.83 | 342.03 |
| list_sum    | 73.9KiB |  65.87 |  77.71 | 318.81 |

The first-run latency (max_ms) is dominated by macOS dyld startup. The min_ms values (cold-start excluded from sorted tail) show the actual execution time: fib_rec(35) takes ~86 ms (expected for O(2^35) recursive calls) and list_sum takes ~66 ms.

## Deferred work

- Phase 18.1: Add vm3 baseline timing and enforce the 2x gate.
- Phase 18.2: CI-published static HTML report.
- Phase 18.3: PR regression alert (>10% wall-clock regression vs main).
- Tighter (1.5x) gate: revisit after Phase 19 with measured data.

## Closeout notes

_Fill in after all 4 sub-phases green._
