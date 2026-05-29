---
title: "Phase 8. Datalog (compile-time semi-naive eval)"
sidebar_position: 10
sidebar_label: "Phase 8. Datalog (compile-time semi-naive eval)"
description: "MEP-54 Phase 8: Datalog (compile-time semi-naive eval). Gate: go test ./transpiler3/go/build/ -run TestPhase8."
---

# Phase 8. Datalog (compile-time semi-naive eval)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 8](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase8
```

## Description

Datalog (compile-time semi-naive eval) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
