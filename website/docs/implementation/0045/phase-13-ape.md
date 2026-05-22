---
title: "Phase 13. APE / Cosmopolitan"
sidebar_position: 15
sidebar_label: "Phase 13. APE"
description: "MEP-45 Phase 13 tracking: --apex build path via vendored cosmocc; one APE binary that runs unmodified on linux+macOS+windows+BSDs."
---

# Phase 13. APE / Cosmopolitan

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 13](/docs/mep/mep-0045#phase-13-ape--cosmopolitan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`mochi build --apex` produces one APE binary; the same binary runs and produces byte-equal output on Linux, macOS, Windows, FreeBSD, NetBSD, OpenBSD CI runners.

## Goal-alignment audit

_To be written before sub-phase 13.0 starts. APE is the most striking distribution story Mochi can tell: one file, every desktop OS. Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 13.0 | `cosmocc` vendored under `transpiler3/c/toolchain/cosmocc/`                                                        | NOT STARTED | —      | — |
| 13.1 | `--apex` build path: cosmocc replaces zig cc; output is `.com.dbg` + `.com` (stripped APE)                         | NOT STARTED | —      | — |
| 13.2 | Runtime under Cosmopolitan: BDWGC compatibility, stream/agent surface preserved                                    | NOT STARTED | —      | — |
| 13.3 | Cross-OS CI runners: Linux + macOS + Windows + FreeBSD (cirrus-ci)                                                 | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_aarch64-APE (Cosmopolitan aarch64 still landing upstream): later._

## Closeout notes

_Fill in after gate green._
