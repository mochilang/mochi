---
title: "Phase 11. Async/await (goroutine + chan rendezvous)"
sidebar_position: 13
sidebar_label: "Phase 11. Async/await (goroutine + chan rendezvous)"
description: "MEP-54 Phase 11: Async/await (goroutine + chan rendezvous). Gate: go test ./transpiler3/go/build/ -run TestPhase11."
---

# Phase 11. Async/await (goroutine + chan rendezvous)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 11](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase11
```

## Description

Async/await (goroutine + chan rendezvous) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
