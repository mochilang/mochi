---
title: "Phase 3. Lists, maps, sets"
sidebar_position: 5
sidebar_label: "Phase 3. Lists, maps, sets"
description: "MEP-54 Phase 3: Lists, maps, sets. Gate: go test ./transpiler3/go/build/ -run TestPhase3."
---

# Phase 3. Lists, maps, sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 3](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase3
```

## Description

Lists, maps, sets for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
