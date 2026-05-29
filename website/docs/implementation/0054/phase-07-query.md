---
title: "Phase 7. Query DSL (range + filter + map pipeline)"
sidebar_position: 9
sidebar_label: "Phase 7. Query DSL (range + filter + map pipeline)"
description: "MEP-54 Phase 7: Query DSL (range + filter + map pipeline). Gate: go test ./transpiler3/go/build/ -run TestPhase7."
---

# Phase 7. Query DSL (range + filter + map pipeline)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-54 §Phase plan · Phase 7](/docs/mep/mep-0054#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking PR    | — |

## Gate

```
go test -v -count=1 ./transpiler3/go/build/... -run ^TestPhase7
```

## Description

Query DSL (range + filter + map pipeline) for the Mochi-to-Go transpiler. Gated against vm3 with byte-equal stdout comparison.
