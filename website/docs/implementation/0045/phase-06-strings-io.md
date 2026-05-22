---
title: "Phase 6. Strings and I/O"
sidebar_position: 8
sidebar_label: "Phase 6. Strings + I/O"
description: "MEP-45 Phase 6 tracking: mochi_str with SSO, utf8proc/simdutf, format strings, file I/O, stdin/stdout/stderr."
---

# Phase 6. Strings and I/O

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 6](/docs/mep/mep-0045#phase-6-strings-and-io) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Strings + stdlib I/O fixture suite (~40 cases: utf-8 iteration, slicing, concat, format, file read, file write, stdin read) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 6.0 starts. Strings + I/O is how a program talks to the outside world; without them most realistic Mochi programs do not run. Aligns._

## Sub-phases

| #   | Scope                                                                                                              | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 6.0 | `mochi_str`: immutable utf-8 slice; short-string optimisation (≤15 bytes inline); precomputed BLAKE3-trimmed hash  | NOT STARTED | —      | — |
| 6.1 | `+`, `len`, `[i]`, `contains`, `startsWith`, `endsWith`, `split`, `join`, `toUpper`/`toLower` via utf8proc          | NOT STARTED | —      | — |
| 6.2 | `print`, `println`, format-string interpolation (`"{name} is {age}"`)                                              | NOT STARTED | —      | — |
| 6.3 | File I/O: `readFile`, `writeFile`, `lines`, `appendFile`; `stdin`, `stdout`, `stderr` handles                       | NOT STARTED | —      | — |
| 6.4 | simdutf utf-8 validation on read; rejected input raises `MOCHI_ERR_PARSE`                                          | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Locale-aware collation: v2 (utf8proc default order is enough for v1)._

## Closeout notes

_Fill in after gate green._
