---
title: "Phase 15. go module publish (pkg.go.dev)"
sidebar_position: 17
sidebar_label: "Phase 15. go module publish (pkg.go.dev)"
description: "MEP-54 Phase 15: go module publish (pkg.go.dev). Gate: go test ./transpiler3/go/build/ -run TestPhase15."
---

# Phase 15. go module publish (pkg.go.dev)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 15](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase15
```

## Description

go module publish (pkg.go.dev) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
