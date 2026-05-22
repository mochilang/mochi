---
title: "Phase 8. Query DSL"
sidebar_position: 10
sidebar_label: "Phase 8. Query DSL"
description: "MEP-45 Phase 8 tracking: query algebra lowering with operator fusion, joins (inner/left/cross), group-by, order-by, distinct, set ops, arena allocation, load/save adapters."
---

# Phase 8. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 8](/docs/mep/mep-0045#phase-8-query-dsl) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Query fixture suite (~60 cases: filter, map, group-by, order-by, distinct, union, intersect, except, inner/left/cross join) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 8.0 starts. Query DSL is the highest-value language feature for the dataset/AI workflows the docs target; without it even simple ETL fixtures fail. Aligns._

## Sub-phases

| #   | Scope                                                                                                                        | Status      | Commit | PR |
|-----|------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 8.0 | Query algebra lowering: `transpiler3/c/lower/query.go` fuses filter+map+take into a single loop                              | NOT STARTED | —      | — |
| 8.1 | `group by` / `order by` / `distinct` / `union` / `intersect` / `except` operators                                            | NOT STARTED | —      | — |
| 8.2 | Joins: inner (hash-join via Swiss table), left (hash-join + right-side outer fill), cross (nested loop)                      | NOT STARTED | —      | — |
| 8.3 | Arena allocation: intermediates live in `mochi_arena` released at query boundary; surviving result copied to GC              | NOT STARTED | —      | — |
| 8.4 | `load`/`save` adapters: JSON (yyjson), YAML (libfyaml), CSV (home-grown)                                                     | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Cost-based join reordering: v2 (v1 honours source order)._

## Closeout notes

_Fill in after gate green._
