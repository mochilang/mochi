---
title: "Phase 5. Sum types (tagged struct union)"
sidebar_position: 7
sidebar_label: "Phase 5. Sum types (tagged struct union)"
description: "MEP-54 Phase 5: Sum types (tagged struct union). Gate: go test ./transpiler3/go/build/ -run TestPhase5."
---

# Phase 5. Sum types (tagged struct union)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 5](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase5
```

## Description

Sum types (tagged struct union) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
