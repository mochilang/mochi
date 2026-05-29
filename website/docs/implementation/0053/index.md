---
title: MEP-53 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 53. Mochi-to-Rust transpiler"
description: "Per-phase implementation tracking for MEP-53 (Mochi-to-Rust transpiler). Status + commit columns capture how each phase landed on main."
---

# MEP-53 implementation tracking

Per-phase tracking for [MEP-53 Mochi-to-Rust transpiler](/docs/mep/mep-0053). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR #22499, the in-branch commit on `mep/0053-rust`).

A phase is LANDED only when its gate is green on every Rust target listed for it in MEP-53 §6. Missing runtimes become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: rtree / lower / colour / emit / build / runtime crate | LANDED | aecbc2ddd7 | [phase-00](/docs/implementation/0053/phase-00-skeleton) |
| 1 | Hello world | LANDED | aecbc2ddd7 | [phase-01](/docs/implementation/0053/phase-01-hello) |
| 2 | Scalars, control flow, casts, string helpers | LANDED | 2b5904fada | [phase-02](/docs/implementation/0053/phase-02-scalars) |
| 3 | Lists, maps, sets | LANDED | f6d9c68bb3 | [phase-03](/docs/implementation/0053/phase-03-collections) |
| 4 | Records (structs) | LANDED | a7b47981ff | [phase-04](/docs/implementation/0053/phase-04-records) |
| 5 | Sum types (tagged enums) | LANDED | 26217cbcec | [phase-05](/docs/implementation/0053/phase-05-sums) |
| 6 | Closures (`Box<dyn Fn>`) | LANDED | ad43642b16 | [phase-06](/docs/implementation/0053/phase-06-closures) |
| 7 | Query DSL (from / where / select / order / skip / take) | LANDED | cf15291eb7 | [phase-07](/docs/implementation/0053/phase-07-query) |
| 8 | Datalog (compile-time semi-naive eval) | LANDED | b63d8118a9 | [phase-08](/docs/implementation/0053/phase-08-datalog) |
| 9 | Agents (Rust structs) | LANDED | abf0aeeef0 | [phase-09](/docs/implementation/0053/phase-09-agents) |
| 10 | Streams and channels (single-thread Rc) | LANDED | 9b6bd876ad | [phase-10](/docs/implementation/0053/phase-10-streams) |
| 11 | Try / catch / panic (panic::catch_unwind) | LANDED | 00d3ee0f28 | [phase-11](/docs/implementation/0053/phase-11-errors) |
| 12 | FFI via sidecar C + cc-rs | LANDED | 2b9ad7bd38 | [phase-12](/docs/implementation/0053/phase-12-ffi) |
| 13 | LLM generate with cassette replay | LANDED | 819cb38daa | [phase-13](/docs/implementation/0053/phase-13-llm) |
| 14 | Fetch + json_decode | LANDED | 0e74a0deea | [phase-14](/docs/implementation/0053/phase-14-fetch) |
| 15 | Publish-ready crate metadata + dry-run gate | LANDED | 458120b2d3 | [phase-15](/docs/implementation/0053/phase-15-publish) |
| 16 | Reproducible build gate | LANDED | 7e013a1d1a | [phase-16](/docs/implementation/0053/phase-16-repro) |
| 17 | wasm32-wasip1 target via wasmtime | LANDED | d3367069dc | [phase-17](/docs/implementation/0053/phase-17-wasm) |
| 18 | Embedded no_std + alloc variant | LANDED | f4c4cb32d3 | [phase-18](/docs/implementation/0053/phase-18-embedded) |

The umbrella PR is [#22499](https://github.com/mochilang/mochi/pull/22499) (merged 2026-05-29, merge commit fd92137ad1).

## Runtime matrix

A phase is fully LANDED only after its gate passes on every required target. Phases not yet exercised on a target become N.1, N.2, ... sub-phases.

| Target | Phases 0-14 (language + advanced runtime) | Phases 15-18 (packaging + cross-target) | Source |
|--------|--------------------------------------------|------------------------------------------|--------|
| Rust 1.95 stable, aarch64-apple-darwin | LANDED (local) | LANDED (local) | `transpiler3/rust/build/phase*_test.go` |
| Rust 1.95 stable, x86_64-unknown-linux-musl (cargo zigbuild) | NOT STARTED (16.1) | NOT STARTED (16.1) | sub-phase 16.1, blocked on `cargo-zigbuild` in CI image |
| Rust 1.95 stable, aarch64-unknown-linux-musl (cargo zigbuild) | NOT STARTED (16.1) | NOT STARTED (16.1) | sub-phase 16.1, blocked on `cargo-zigbuild` in CI image |
| wasm32-wasip1 (wasmtime 26) | LANDED (phase 17, 4 fixtures) | LANDED (phase 17) | `transpiler3/rust/build/phase17_test.go` |
| Embedded (`cargo check --no-default-features --features embedded`) | n/a (subset only) | LANDED (phase 18) | `transpiler3/rust/build/phase18_test.go` |

Sub-phase 16.1 (Linux musl cross-build coverage) tracks a future CI matrix expansion. Currently the cross-build path is exercised in `Driver.Build` for `TargetLinuxStaticX64` / `TargetLinuxStaticArm64` but not gated per-phase in a CI workflow.
