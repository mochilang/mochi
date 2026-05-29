---
title: "Phase 10. Streams (fan-out broadcast chan)"
sidebar_position: 12
sidebar_label: "Phase 10. Streams (fan-out broadcast chan)"
description: "MEP-54 Phase 10: Streams (fan-out broadcast chan). Gate: go test ./transpiler3/go/build/ -run TestPhase10."
---

# Phase 10. Streams (fan-out broadcast chan)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 10](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase10
```

## Description

Streams (fan-out broadcast chan) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
