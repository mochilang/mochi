---
title: "Phase 0. Skeleton: gotree / lower / emit / build / runtime package"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton: gotree / lower / emit / build / runtime package"
description: "MEP-54 Phase 0: Skeleton: gotree / lower / emit / build / runtime package. Gate: go test ./transpiler3/go/build/ -run TestPhase0."
---

# Phase 0. Skeleton: gotree / lower / emit / build / runtime package

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 0](/docs/mep/mep-0054#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | #22485 |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase0
```

## Description

Skeleton: gotree / lower / emit / build / runtime package for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
