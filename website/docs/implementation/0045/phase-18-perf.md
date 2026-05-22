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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Median fixture wall-clock time on the BG corpus is within 2x of the equivalent Go-backend build, on x86_64-linux-gnu and aarch64-darwin.

## Goal-alignment audit

_To be written before sub-phase 18.0 starts. Performance gate exists so a regression cannot ship silently; the user-facing payoff is "your native build is at least as fast as the Go-embedded one". Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 18.0 | Benchmark harness: `tests/transpiler3/c/bench/` with BG kernels (sum_loop, fib_iter, hello_world, ...)             | NOT STARTED | —      | — |
| 18.1 | Wall-clock, peak RSS, binary size (release/strip), compile time recorded per fixture                               | NOT STARTED | —      | — |
| 18.2 | Per-release report published to a static page                                                                      | NOT STARTED | —      | — |
| 18.3 | Regression alert: > 10% wall-clock regression vs previous main posts a comment on the PR                           | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Tighter (1.5x) gate: revisit after Phase 19 with measured data._

## Closeout notes

_Fill in after gate green._
