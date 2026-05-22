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
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Each release-profile fixture, rebuilt twice on two different CI hosts (Linux CI runner + macOS CI runner cross-building to a third triple), produces byte-identical binaries (SHA-256 equality).

## Goal-alignment audit

_To be written before sub-phase 17.0 starts. Reproducibility is the user-facing supply-chain story; without it, the AOT binary cannot ground a verifiable release. Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 17.0 | `SOURCE_DATE_EPOCH` honoured; `__DATE__` / `__TIME__` never embedded                                               | NOT STARTED | —      | — |
| 17.1 | `-ffile-prefix-map=$PWD=.` and `-fdebug-prefix-map=$PWD=.` strip absolute paths                                    | NOT STARTED | —      | — |
| 17.2 | Function/global ordering by sorted IR identifier (never hash-map iteration order)                                  | NOT STARTED | —      | — |
| 17.3 | All non-libc deps static-linked; bundled toolchain pinned by SHA-256                                               | NOT STARTED | —      | — |
| 17.4 | Sample artefact SHA-256 published per release tag                                                                  | NOT STARTED | —      | — |
| 17.5 | `.github/workflows/transpiler3-c-repro.yml` rebuilds the corpus twice and diffs SHA-256                            | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Bit-exact reproducibility under `--apex` (cosmocc): tracked in Phase 13.4 if upstream embed signature drifts._

## Closeout notes

_Fill in after gate green._
