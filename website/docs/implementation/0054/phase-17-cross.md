---
title: "Phase 17. Cross-compile matrix (5 GOOS/GOARCH)"
sidebar_position: 19
sidebar_label: "Phase 17. Cross-compile matrix (5 GOOS/GOARCH)"
description: "MEP-54 Phase 17: Cross-compile matrix (5 GOOS/GOARCH). Gate: go test ./transpiler3/go/build/ -run TestPhase17."
---

# Phase 17. Cross-compile matrix (5 GOOS/GOARCH)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 17](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase17
```

## Description

Cross-compile matrix (5 GOOS/GOARCH) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
