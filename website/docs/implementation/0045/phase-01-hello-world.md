---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-45 Phase 1 tracking: source-to-binary minimum viable pipeline that prints \"hello, mochi!\" on the host triple."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 1](/docs/mep/mep-0045#phase-1-hello-world) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`mochi build tests/transpiler3/c/fixtures/hello/hello.mochi -o /tmp/hello && /tmp/hello | diff - tests/transpiler3/c/fixtures/hello/expect.txt` exits 0 on host triple.

## Goal-alignment audit

_To be written before sub-phase 1.0 starts. The hello-world gate is the user-facing minimum: source on disk to native binary, no Go runtime, byte-equal stdout. Aligns._

## Sub-phases

| #   | Scope                                                                                                                                     | Status      | Commit | PR |
|-----|-------------------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 1.0 | Source-to-binary minimum: parser reused; lower; emit; build via host `cc` discovery; single integration test passes                       | NOT STARTED | —      | — |
| 1.1 | `--out PATH` and `--emit=c` CLI flags                                                                                                     | NOT STARTED | —      | — |
| 1.2 | `.mochi/cache/` BLAKE3 content-addressed cache; rebuild on unchanged source is no-op                                                      | NOT STARTED | —      | — |
| 1.3 | Vendored `zig cc` fallback under `transpiler3/c/toolchain/zig/install.go`                                                                 | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Cross matrix is Phase 11. Reproducibility of the hello binary across hosts is Phase 17._

## Closeout notes

_Fill in after gate green._
