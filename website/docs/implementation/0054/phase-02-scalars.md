---
title: "Phase 2. Scalars, arithmetic, control flow, string helpers"
sidebar_position: 4
sidebar_label: "Phase 2. Scalars, arithmetic, control flow, string helpers"
description: "MEP-54 Phase 2: Scalars, arithmetic, control flow, string helpers. Gate: go test ./transpiler3/go/build/ -run TestPhase2."
---

# Phase 2. Scalars, arithmetic, control flow, string helpers

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 2](/docs/mep/mep-0054#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | #22485 |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase2
```

## Description

Scalars, arithmetic, control flow, string helpers for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
