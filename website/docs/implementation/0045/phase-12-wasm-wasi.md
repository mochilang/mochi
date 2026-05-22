---
title: "Phase 12. WASM / WASI"
sidebar_position: 14
sidebar_label: "Phase 12. WASM / WASI"
description: "MEP-45 Phase 12 tracking: wasm32-wasi target with vendored wasi-sdk, precise GC + shadow stack (no BDWGC on WASI), cooperative scheduler."
---

# Phase 12. WASM / WASI

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 12](/docs/mep/mep-0045#phase-12-wasm--wasi) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Every Phase 1-10 fixture compiles via `mochi build --target=wasm32-wasi` and runs byte-equal vs vm3 under `wasmtime`.

## Goal-alignment audit

_To be written before sub-phase 12.0 starts. WASM is the user-facing payoff for sandboxed/serverless deployment. Aligns._

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 12.0 | wasi-sdk vendored under `transpiler3/c/toolchain/wasi-sdk/`; build driver routes `--target=wasm32-wasi` through it | NOT STARTED | —      | — |
| 12.1 | Precise allocator + shadow-stack root scanning to replace BDWGC                                                    | NOT STARTED | —      | — |
| 12.2 | Stream/agent surface narrowed: no threading; M:N scheduler collapses to single-fibre cooperative loop              | NOT STARTED | —      | — |
| 12.3 | `wasmtime`-driven run-gate in CI; fixture corpus subset matching narrowed surface                                  | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_WasmGC: still drafting on common runtimes in 2026; revisit when WasmGC stabilises in wasmtime + wasmer._

## Closeout notes

_Fill in after gate green._
