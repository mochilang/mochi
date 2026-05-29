---
title: "Phase 14. HTTP fetch (net/http)"
sidebar_position: 16
sidebar_label: "Phase 14. HTTP fetch (net/http)"
description: "MEP-54 Phase 14: HTTP fetch (net/http). Gate: go test ./transpiler3/go/build/ -run TestPhase14."
---

# Phase 14. HTTP fetch (net/http)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 14](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase14
```

## Description

HTTP fetch (net/http) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
