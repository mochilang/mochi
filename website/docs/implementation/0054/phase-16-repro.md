---
title: "Phase 16. Reproducible build (-trimpath SHA-256)"
sidebar_position: 18
sidebar_label: "Phase 16. Reproducible build (-trimpath SHA-256)"
description: "MEP-54 Phase 16: Reproducible build (-trimpath SHA-256). Gate: go test ./transpiler3/go/build/ -run TestPhase16."
---

# Phase 16. Reproducible build (-trimpath SHA-256)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 16](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase16
```

## Description

Reproducible build (-trimpath SHA-256) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
