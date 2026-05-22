---
title: "Phase 7. Error model"
sidebar_position: 9
sidebar_label: "Phase 7. Error model"
description: "MEP-45 Phase 7 tracking: setjmp/longjmp try/catch, per-thread exception jump-buffer stack, built-in and user error codes."
---

# Phase 7. Error model

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 7](/docs/mep/mep-0045#phase-7-error-model) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Error-model fixture suite (~30 cases: `panic`, `try { ... } catch e { ... }`, deferred cleanup, finally, nested try, panic across closure boundary) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 7.0 starts. Error handling is core surface; without it long-running programs cannot recover from expected failures. Aligns._

## Sub-phases

| #   | Scope                                                                                                              | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 7.0 | Per-thread exception jump-buffer stack (TLS); `mochi_try_push` / `mochi_try_pop` / `mochi_raise(int, mochi_str)`   | NOT STARTED | —      | — |
| 7.1 | `try { ... } catch e { ... }` lowers to `if (setjmp(buf) == 0) { ... } else { ... }` with cleanup on longjmp path  | NOT STARTED | —      | — |
| 7.2 | Built-in error codes (`MOCHI_ERR_*`) wired through runtime calls (divzero, OOB, type mismatch, parse)              | NOT STARTED | —      | — |
| 7.3 | User error codes (positive integers); user `panic(code, msg)` lowers to `mochi_raise`                              | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Itanium-style table-driven unwind via libunwind: v2._

## Closeout notes

_Fill in after gate green._
