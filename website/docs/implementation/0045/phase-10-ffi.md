---
title: "Phase 10. FFI shells"
sidebar_position: 12
sidebar_label: "Phase 10. FFI"
description: "MEP-45 Phase 10 tracking: C-direct FFI in v1, boxed mochi_value at boundary; Go/Python/TS via deferred sub-phases."
---

# Phase 10. FFI shells

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 10](/docs/mep/mep-0045#phase-10-ffi-shells) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

C-direct FFI fixture suite (~15 cases: call a vendored C function, pass scalars + strings + records, return scalars + records, error propagation) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

_To be written before sub-phase 10.0 starts. C-direct FFI is the natural FFI for a C-AOT target and the gateway for the embedding use case. Aligns._

## Sub-phases

| #    | Scope                                                                                                                       | Status      | Commit | PR |
|------|-----------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 10.0 | C direct: binding declarations in Mochi; emit header in build dir; user-provided `.c` neighbour compiled in                 | NOT STARTED | —      | — |
| 10.1 | Boxed `mochi_value` type for FFI-crossing values (sum of scalar + string + handle); marshalling helpers                     | NOT STARTED | —      | — |
| 10.2 | Go FFI via Unix-domain RPC (deferred sub-phase; ships after C-direct is green)                                              | NOT STARTED | —      | — |
| 10.3 | Python FFI via embedded libpython3 (deferred sub-phase)                                                                     | NOT STARTED | —      | — |
| 10.4 | TypeScript FFI via QuickJS-NG (deferred sub-phase)                                                                          | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_Go c-archive route (in-process, no RPC): v2, alongside 10.2 review._

## Closeout notes

_Fill in after gate green._
