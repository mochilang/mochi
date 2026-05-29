---
title: "Phase 4. Records (structs)"
sidebar_position: 6
sidebar_label: "Phase 4. Records (structs)"
description: "MEP-54 Phase 4: Records (structs). Gate: go test ./transpiler3/go/build/ -run TestPhase4."
---

# Phase 4. Records (structs)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 4](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase4
```

## Description

Records (structs) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
