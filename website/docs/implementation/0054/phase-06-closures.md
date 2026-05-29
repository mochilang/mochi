---
title: "Phase 6. Closures (function literals)"
sidebar_position: 8
sidebar_label: "Phase 6. Closures (function literals)"
description: "MEP-54 Phase 6: Closures (function literals). Gate: go test ./transpiler3/go/build/ -run TestPhase6."
---

# Phase 6. Closures (function literals)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 6](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase6
```

## Description

Closures (function literals) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
