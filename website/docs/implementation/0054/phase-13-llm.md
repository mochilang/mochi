---
title: "Phase 13. LLM (cassette replay)"
sidebar_position: 15
sidebar_label: "Phase 13. LLM (cassette replay)"
description: "MEP-54 Phase 13: LLM (cassette replay). Gate: go test ./transpiler3/go/build/ -run TestPhase13."
---

# Phase 13. LLM (cassette replay)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 13](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase13
```

## Description

LLM (cassette replay) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
