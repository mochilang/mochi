---
title: "Phase 8. Datalog"
sidebar_position: 10
sidebar_label: "Phase 8. Datalog"
description: "MEP-53 Phase 8, Datalog queries evaluated at compile-time via semi-naive fixpoint, emitted as frozen Vec literals."
---

# Phase 8. Datalog (compile-time semi-naive eval)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | b63d8118a9 |

## Gate

`TestPhase8Datalog` walks `tests/transpiler3/rust/fixtures/phase08-datalog/` (20 fixtures) and asserts byte-equal stdout. Coverage: ancestor closure, reachability, same-generation, transitive-closure, mutual-recursion (even / odd), aggregation queries.

## Lowering decisions

Mochi Datalog (`rule`, `query`) is evaluated **at compile time** in the Rust lower pass via semi-naive fixpoint. `transpiler3/rust/lower/datalog.go` walks the rule set, computes the least fixed point, and emits the result tuples as a frozen Rust `Vec` literal:

```rust
let ancestors: Vec<(String, String)> = vec![
    ("alice".to_string(), "bob".to_string()),
    ("bob".to_string(), "carol".to_string()),
    ("alice".to_string(), "carol".to_string()),
];
```

The trade-off: compile-time evaluation means the rule body and EDB facts must be statically known at lower time. Mochi forbids runtime EDB updates (no `assert` / `retract` statements), so this restriction is already enforced by the source language. The benefit: zero runtime cost (the query result is a baked-in constant), no need for a Datalog runtime in the emitted code, and no need for a Datalog crate dependency.

Semi-naive evaluation reuses the same `aotir.DatalogProgram` representation as the C target (MEP-45 phase 11); only the result emission differs.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/datalog.go` | Semi-naive evaluator + Vec literal emit |
| `transpiler3/rust/build/phase08_test.go` | 20-fixture gate |
| `tests/transpiler3/rust/fixtures/phase08-datalog/*.mochi` + `.out` | 20 fixtures |

## Test set

- `TestPhase8Datalog/<fixture>` for each `.mochi` in the fixture directory (20 fixtures).

## Closeout notes

Compile-time Datalog is a significant size-vs-speed win at the Rust target: each query becomes a frozen Vec, zero allocation at runtime past the initial `String::from` per cell. The cost is compile-time: large Datalog programs (thousands of facts) take noticeable lower-time. Fixture set caps facts at ~50 per program, which lowers in under 10ms.
