---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-53 Phase 0, cargo toolchain detection, rtree skeleton, mochi-runtime crate stub."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | aecbc2ddd7 (combined with phase 1) |

## Gate

`TestPhase0Skeleton` in `transpiler3/rust/build/phase00_test.go`: subtests `cargo_resolve`, `runtime_crate`, and `go_build`. The first calls `resolveCargo()` and rejects environments where no cargo is on PATH and `~/.cargo/bin/cargo` is absent. The second checks that `runtime3/rust/mochi-runtime/Cargo.toml` parses and that `src/lib.rs` exists, the minimum the emitter requires once `extern crate mochi_runtime;` is rendered. The third runs `go build ./transpiler3/rust/...` from the repo root and fails on any compile error in the Rust pipeline subtree.

## Lowering decisions

Phase 0 ships no lowering yet, only the scaffolding the later phases depend on. The driver under `transpiler3/rust/build/` exports `Driver.Build(src, out, target)`, the `Target` enum (initially `TargetNativeExecutable`, `TargetRustSource`, `TargetLinuxStaticX64`, `TargetLinuxStaticArm64`, `TargetWasm32WASI`), and `resolveCargo()`. Cargo resolution walks (1) `$MOCHI_CARGO`, (2) `~/.cargo/bin/cargo`, then (3) `exec.LookPath("cargo")`.

`transpiler3/rust/rtree/` lands the structural Rust AST: `SourceFile`, `UseDecl`, `ModDecl`, `StructDecl`, `EnumDecl`, `FnDecl`, `ImplDecl`, `LetStmt`, `IfStmt`, `MatchStmt`, `Return`, `BinaryOp`, `UnaryOp`, `Ident`, `StringLit`, `IntLit`, `FloatLit`, `BoolLit`, `RawDecl`, `RawStmt`, `RawExpr`. Each node exposes a `RustString(indent int)` or `RustExprString()` renderer using 4-space indent. The `SourceFile.RustSource()` entry point prepends `#![allow(unused, non_snake_case, non_camel_case_types)]`, the `extern crate` list, and the `use` list, then concatenates declarations.

`transpiler3/rust/lower/Lower(prog, fileBase, moduleName)` exists as a stub signature; it returns a `*rtree.SourceFile` wrapping an empty `fn main()` so the driver can round-trip parse, lower, and emit without crashing before any Mochi feature is wired. `runtime3/rust/mochi-runtime/src/lib.rs` ships the skeleton `io::print_*` and `conv` modules so the emitted `extern crate mochi_runtime;` resolves under cargo.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/build/build.go` | `Driver`, `Build`, `resolveCargo`, `Target` enum |
| `transpiler3/rust/rtree/` | Structural Rust AST nodes + renderer |
| `transpiler3/rust/lower/lower.go` | Stub `Lower` returning an empty `main` |
| `transpiler3/rust/colour/colour.go` | Stub colour pass (no-op until phase 6) |
| `transpiler3/rust/emit/emit.go` | `Emit(sf, workDir)` writes Cargo.toml + src/main.rs |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Runtime crate skeleton |
| `runtime3/rust/mochi-runtime/Cargo.toml` | Crate manifest |
| `transpiler3/rust/build/phase00_test.go` | `TestPhase0Skeleton` with 3 subtests |

## Test set

- `TestPhase0Skeleton/cargo_resolve`, `TestPhase0Skeleton/runtime_crate`, `TestPhase0Skeleton/go_build`.

## Closeout notes

Phase 0 landed on stable Rust 1.95 with rustup default install and `go build ./transpiler3/rust/...` clean. Key implementation insight: scaffolding the `rtree` renderer in the same commit as the driver avoided a Phase 1 dependency loop where the hello-world test would need both pieces at once. The runtime crate is single-file (`src/lib.rs`) to keep `cargo check --features embedded` simple in phase 18.
