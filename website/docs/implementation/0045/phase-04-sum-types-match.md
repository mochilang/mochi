---
title: "Phase 4. Sum types and Maranget pattern matching"
sidebar_position: 6
sidebar_label: "Phase 4. Sum types + match"
description: "MEP-45 Phase 4 tracking: tagged-union sum types with niche optimisation, Maranget decision-tree match lowering, exhaustiveness."
---

# Phase 4. Sum types and Maranget pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 4](/docs/mep/mep-0045#phase-4-sum-types-and-maranget-pattern-matching) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

ADT + match fixture suite (~40 cases including `option<T>`, `result<T,E>`, nested ADTs, exhaustive + non-exhaustive matches) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 4.0 starts. Sum types and pattern matching are core to idiomatic Mochi; without them the language is a stranger to its own programs. Aligns._

## Sub-phases

| #   | Scope                                                                                                                                       | Status      | Commit | PR |
|-----|---------------------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 4.0 | Sum-type lowering: `struct pkg_S { uint8_t tag; union { ... } u; }`; recursive variants box payload; niche optimisation for pointer-shaped `?T` | NOT STARTED | —      | — |
| 4.1 | Maranget decision-tree pass: `transpiler3/c/lower/match.go` lowers `match e { ... }` to chained `switch`/`if` tree                          | NOT STARTED | —      | — |
| 4.2 | Exhaustiveness check at type-check time (already in MEP-13); panic on non-exhaustive in `--debug`, UB in `--fast`                            | NOT STARTED | —      | — |
| 4.3 | Property test: `theft`-generated random pattern set decides identically to reference naive matcher (10000 cases per CI run)                  | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_GADT support: not in v1._

## Closeout notes

_Fill in after gate green._
