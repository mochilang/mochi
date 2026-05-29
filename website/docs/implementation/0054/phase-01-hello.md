---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-54 Phase 1: Hello world. Gate: go test ./transpiler3/go/build/ -run TestPhase1."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 1](/docs/mep/mep-0054#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | #22485 |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase1
```

## Description

Hello world for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
