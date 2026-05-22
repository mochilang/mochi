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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Full Phase 1-15 fixture corpus compiles + runs clean under ASan, UBSan, TSan, MSan, LeakSan on x86_64-linux-gnu and aarch64-darwin.

## Goal-alignment audit

_To be written before sub-phase 16.0 starts. Sanitiser clean is the strongest internal correctness signal short of formal verification; the user-facing payoff is "no UB in your shipped binary". Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 16.0 | ASan + LeakSan clean on full corpus, both hosts                                                                    | NOT STARTED | —      | — |
| 16.1 | UBSan clean (signed overflow, alignment, OOB shifts, null deref)                                                   | NOT STARTED | —      | — |
| 16.2 | TSan clean on streams/agents corpus                                                                                | NOT STARTED | —      | — |
| 16.3 | MSan clean on Linux (Apple-silicon MSan unsupported upstream)                                                      | NOT STARTED | —      | — |
| 16.4 | Build profile `--debug` wires sanitisers; CI nightly job runs the matrix                                           | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_KCFI / CFI sanitiser integration: shipped under `--secure` profile in `transpiler3/c/build/profile_secure.go`._

## Closeout notes

_Fill in after gate green._
