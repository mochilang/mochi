---
title: "Phase 15. Datalog / logic"
sidebar_position: 17
sidebar_label: "Phase 15. Datalog"
description: "MEP-45 Phase 15 tracking: datalog lowering with semi-naive evaluation and magic-set transform."
---

# Phase 15. Datalog / logic

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 15](/docs/mep/mep-0045#phase-15-datalog--logic) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Logic fixture suite (~20 cases: ancestors, reachability, magic-set, stratified negation) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 15.0 starts. Datalog is a niche feature, but it is in the language and the AOT path must not be the place it stops working. Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 15.0 | Lower datalog rules to semi-naive evaluation: `transpiler3/c/lower/logic.go`                                       | NOT STARTED | —      | — |
| 15.1 | Magic-set transform for goal-directed evaluation                                                                   | NOT STARTED | —      | — |
| 15.2 | Stratified negation (sub-phase iff corpus demands)                                                                 | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Aggregates over recursive rules: v2._

## Closeout notes

_Fill in after gate green._
