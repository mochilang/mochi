---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-53 Phase 1, the first end-to-end gate: a Mochi `print(\"hello\")` fixture compiles via cargo and produces matching stdout."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | aecbc2ddd7 |

## Gate

`TestPhase1Hello` in `transpiler3/rust/build/phase01_test.go` iterates every `.mochi` file under `tests/transpiler3/rust/fixtures/phase01-hello/`, builds it through `Driver.Build` with `TargetNativeExecutable`, runs the resulting binary, and diffs stdout against the paired `.out` file. The gate is byte-equal stdout match.

## Lowering decisions

The hello-world fixture lowers a single `print("hello")` statement. `print(s)` lowers to `mochi_runtime::io::print_str(s)` for string arguments. The emitted `main.rs` is:

```rust
#![allow(unused, non_snake_case, non_camel_case_types)]
extern crate mochi_runtime;

fn main() {
    mochi_runtime::io::print_str("hello");
}
```

Lifting print to a runtime helper (rather than inlining `println!`) is the key decision: it lets phase 2 swap in `print_i64` / `print_f64` / `print_bool` with deterministic float formatting without changing the lowering shape. `println!` would have forced phase 2 to introduce a separate emit path for non-string `print` calls.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/lower.go` | Wire `print(s)` → `mochi_runtime::io::print_str` |
| `transpiler3/rust/build/phase01_test.go` | Fixture-driven hello-world gate |
| `tests/transpiler3/rust/fixtures/phase01-hello/hello.mochi` | The fixture |
| `tests/transpiler3/rust/fixtures/phase01-hello/hello.out` | vm3-recorded expected stdout |

## Test set

- `TestPhase1Hello/hello` (single fixture).

## Closeout notes

Phase 1 confirmed the full pipeline: parse, typecheck, clower, rust/lower, colour (no-op), emit, cargo build (release), exec. cargo's first build was ~12s cold and ~0.5s warm with the on-disk cache hit. The cache key is SHA-256 of the workspace path + source content, so editing the `.mochi` file invalidates correctly.
