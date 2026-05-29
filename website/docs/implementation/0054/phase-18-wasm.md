---
title: "Phase 18. Wasm (GOOS=wasip1 GOARCH=wasm)"
sidebar_position: 20
sidebar_label: "Phase 18. Wasm (GOOS=wasip1 GOARCH=wasm)"
description: "MEP-54 Phase 18: Wasm (GOOS=wasip1 GOARCH=wasm). Gate: go test ./transpiler3/go/build/ -run TestPhase18."
---

# Phase 18. Wasm (GOOS=wasip1 GOARCH=wasm)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 18](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase18
```

## Description

Wasm (GOOS=wasip1 GOARCH=wasm) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
