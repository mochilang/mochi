---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives + control flow"
description: "MEP-45 Phase 2 tracking: int/float/bool arithmetic, comparisons, short-circuit, if/while/for, functions, divide-by-zero panic."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 2](/docs/mep/mep-0045#phase-2-primitives-and-control-flow) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Arithmetic + control-flow suite (~50 fixtures: int/float ops, comparisons, if/else, while, for-in over int range, recursion) compiles and runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 2.0 starts. Primitives + control flow is the smallest set that gets a real (non-toy) Mochi program to compile. Aligns._

## Sub-phases

| #   | Scope                                                                                              | Status      | Commit | PR |
|-----|----------------------------------------------------------------------------------------------------|-------------|--------|----|
| 2.0 | `int` (`int64_t`), `float` (`double`), `bool`; arithmetic; comparisons; short-circuit `&&` / `||`  | NOT STARTED | —      | — |
| 2.1 | `if`/`else`, `while`, `return`, `break`, `continue`                                                | NOT STARTED | —      | — |
| 2.2 | `for x in start..end` (int range); user-defined multi-arg functions                                | NOT STARTED | —      | — |
| 2.3 | Integer divide-by-zero raises `MOCHI_ERR_DIVZERO` (checked profile); UB under `--fast-int`         | NOT STARTED | —      | — |
| 2.4 | Float NaN propagation matches vm3 byte-for-byte (IEEE 754 round-trip on `%.17g`)                   | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Tuple return values: Phase 3 alongside records. Big-int / fixed-width ints: not in v1._

## Closeout notes

_Fill in after gate green._
