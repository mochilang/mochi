---
title: "Phase 3. Records, lists, maps, sets"
sidebar_position: 5
sidebar_label: "Phase 3. Records + collections"
description: "MEP-45 Phase 3 tracking: record types, list<T>, map<K,V>, set<T>, omap<K,V>, monomorphisation pass."
---

# Phase 3. Records, lists, maps, sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 3](/docs/mep/mep-0045#phase-3-records-lists-maps-sets) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Records / collections fixture suite (~80 cases) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 3.0 starts. Records + collections unlock realistic data-shaping code; without them no useful Mochi program compiles. Aligns._

## Sub-phases

| #   | Scope                                                                                                                          | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 3.0 | Record types: `struct pkg_R` in source field order; field access; record literals; equality                                    | NOT STARTED | —      | — |
| 3.1 | `list<T>`: `mochi_list__T` (growable dense vector); `[]`, `len`, `append`, `[i]`, slice                                        | NOT STARTED | —      | — |
| 3.2 | `map<K,V>`: cwisstable Swiss table per `(K,V)` instantiation; `m[k]`, `len`, `keys`, `values`                                  | NOT STARTED | —      | — |
| 3.3 | `set<T>`: Swiss table with elided value slot; `+`, `-`, `contains`, `len`                                                      | NOT STARTED | —      | — |
| 3.4 | Monomorphisation pass: `transpiler3/c/lower/mono.go` lowers each concrete instantiation once, deterministic ordering           | NOT STARTED | —      | — |
| 3.5 | `omap<K,V>` (insertion-order map): Swiss table + parallel insertion-order list (needed by Phase 8)                             | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Concurrent-safe maps: not in v1 (use a stream/agent boundary)._

## Closeout notes

_Fill in after gate green._
