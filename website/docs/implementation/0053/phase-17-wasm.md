---
title: "Phase 17. wasm32-wasip1"
sidebar_position: 19
sidebar_label: "Phase 17. wasm"
description: "MEP-53 Phase 17, wasm32-wasip1 target via cargo build --target wasm32-wasip1, runtime under wasmtime."
---

# Phase 17. wasm32-wasip1 target via wasmtime

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22614](https://github.com/mochilang/mochi/issues/22614) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | d3367069dc |

## Gate

`TestPhase17Wasm` walks `tests/transpiler3/rust/fixtures/phase17-wasm/` (4 fixtures), builds each to `TargetWasm32WASI`, runs the resulting `.wasm` under `wasmtime`, and diffs stdout. The gate skips if either the `wasm32-wasip1` rustup target is not installed (`rustup target list` filter) or `wasmtime` is not on PATH.

## Lowering decisions

The build target name was renamed in Rust 1.78 (May 2024): `wasm32-wasi` → `wasm32-wasip1`. MEP-53 uses the new name throughout (`TargetWasm32WASI` is the Go enum value, but the cargo target string and the binary subpath both use `wasm32-wasip1`). Phases 0-16 were initially written against `wasm32-wasi`; phase 17 renamed in `build.go` in two places:

```go
case TargetWasm32WASI:
    args = append(args, "--target", "wasm32-wasip1")
```

```go
case TargetWasm32WASI:
    subPath = filepath.Join("wasm32-wasip1", "release", binName+".wasm")
```

The runtime crate compiles unchanged for wasm32-wasip1; std::net::TcpStream is unavailable (wasm32-wasip1 has no TCP socket support in Preview 1), so any fixture using `fetch` would fail at link time. Phase 17 fixtures avoid fetch / json_decode entirely. Phase 14 fixtures are also not exercised under wasm.

The 4 fixtures cover: hello world, integer arithmetic, string operations, and a simple `for` loop. These exercise the runtime's `io::print_*`, `conv::int_to_str`, and `strings::*` modules, plus the basic control-flow lowering.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/build/build.go` | Rename wasm32-wasi → wasm32-wasip1 in cargo args + binary subpath |
| `transpiler3/rust/build/phase17_test.go` | 4-fixture gate with wasmtime, target-installed check |
| `tests/transpiler3/rust/fixtures/phase17-wasm/wasm_hello.mochi` + `.out` | hello fixture |
| `tests/transpiler3/rust/fixtures/phase17-wasm/wasm_int.mochi` + `.out` | int fixture |
| `tests/transpiler3/rust/fixtures/phase17-wasm/wasm_string.mochi` + `.out` | string fixture |
| `tests/transpiler3/rust/fixtures/phase17-wasm/wasm_loop.mochi` + `.out` | loop fixture |

## Test set

- `TestPhase17Wasm/wasm_hello`, `TestPhase17Wasm/wasm_int`, `TestPhase17Wasm/wasm_string`, `TestPhase17Wasm/wasm_loop`.

## Closeout notes

The `wasm_loop` fixture initially used Mochi's `1..=5` (inclusive range) syntax, which is **not** valid Mochi (Mochi ranges are exclusive `1..6`). The fix was to switch to `1..6`. This is a recurring subtle bug — Rust supports inclusive `..=` syntax, which is easy to reach for by reflex when writing a wasm test.

`wasmtime` was the easiest WASI runtime to integrate (vs wasmer or WasmEdge): a single binary on PATH, takes `wasmtime path/to/foo.wasm` with no extra flags, prints to stdout as expected. The test invokes wasmtime via `exec.Command("wasmtime", wasmPath)` and reads stdout directly.

The default `rustup target add wasm32-wasip1` is needed on the host; the per-phase gate detects this with `rustup target list | grep '^wasm32-wasip1 .installed'` (the trailing `(installed)` marker). When missing, the gate skips with a clear message.
