---
title: "Phase 5. Closures and higher-order functions"
sidebar_position: 7
sidebar_label: "Phase 5. Closures"
description: "MEP-45 Phase 5 tracking: closure-convert pass, fat-pointer (code+env) representation, free/method-as-closure shims."
---

# Phase 5. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 5](/docs/mep/mep-0045#phase-5-closures-and-higher-order-functions) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Higher-order fixture suite (~30 cases: `map`, `filter`, `fold`, `flatMap`, currying, captures by value + reference, closures returned from functions) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 5.0 starts. Higher-order combinators are how Mochi expresses data transformation; Phase 8's query DSL leans on this. Aligns._

## Sub-phases

| #   | Scope                                                                                                                                       | Status      | Commit | PR |
|-----|---------------------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 5.0 | Closure-convert pass: `transpiler3/c/lower/closure.go` rewrites every closure into explicit `(code, env_struct *)` fat pointer              | NOT STARTED | —      | — |
| 5.1 | Free function as closure: `env == NULL` shim auto-generated per arity                                                                       | NOT STARTED | —      | — |
| 5.2 | Method as closure: `env == self` shim auto-generated per method                                                                             | NOT STARTED | —      | — |
| 5.3 | Closures escaping return: env heap-allocated and GC-rooted; stack-keep escape-analysis deferred to v2                                       | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Escape analysis for stack-allocated env: v2._

## Closeout notes

_Fill in after gate green._
