---
title: "Phase 12. FFI via CGo"
sidebar_position: 14
sidebar_label: "Phase 12. FFI via CGo"
description: "MEP-54 Phase 12: FFI via CGo. Gate: go test ./transpiler3/go/build/ -run TestPhase12."
---

# Phase 12. FFI via CGo

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 12](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase12
```

## Description

FFI via CGo for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
