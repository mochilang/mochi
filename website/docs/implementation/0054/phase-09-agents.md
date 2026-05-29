---
title: "Phase 9. Agents (goroutine + channel mailbox)"
sidebar_position: 11
sidebar_label: "Phase 9. Agents (goroutine + channel mailbox)"
description: "MEP-54 Phase 9: Agents (goroutine + channel mailbox). Gate: go test ./transpiler3/go/build/ -run TestPhase9."
---

# Phase 9. Agents (goroutine + channel mailbox)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 9](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase9
```

## Description

Agents (goroutine + channel mailbox) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
