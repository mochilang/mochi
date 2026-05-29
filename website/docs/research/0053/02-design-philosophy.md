---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why Rust, why 1.78 floor, why single-thread runtime, why Box<dyn Fn>, why no tokio."
---

# 02. Design philosophy

This note frames the load-bearing decisions in MEP-53 against the 2024-2026 Rust ecosystem.

## Why Rust at all

The C target (MEP-45) already produces statically-linked single-file native binaries. Why a second native target?

Three reasons Rust delivers what C cannot:

1. **crates.io as a packaging system.** Rust's package manager is the most mature non-language-specific dep ecosystem outside npm (and arguably better than npm given Cargo's lockfile-by-default and reproducible-builds story). MEP-53's `TargetRustCrate` produces a publish-ready crate; the C target has no analogous endpoint.

2. **wasm32-wasip1 as a first-class target.** Rust 1.78 (May 2024) renamed wasm32-wasi to wasm32-wasip1 and made it stable. As of 2026-05, Rust is the canonical source language for WASI components, the Wasm Component Model, and the Bytecode Alliance toolchain. The C target's wasi support requires emscripten or wasi-libc, both of which are less ergonomic than Rust's first-class wasm32-wasip1.

3. **Embedded with no_std + alloc.** The Rust embedded WG has reached production maturity (embassy 1.0 May 2025, RTIC 2.0 October 2024). MEP-53's `embedded` feature gates the std-requiring modules so the conv + strings subset compiles into any bare-metal Rust program. C achieves this trivially (it has no std runtime to remove), but C also has no tooling to assert that a library is std-free.

## Why Rust 1.78 floor (1.95 production)

Rust 1.78 (May 2024) is the first stable to ship wasm32-wasip1 (the rename from wasm32-wasi). Any earlier floor would force MEP-53 to use the old name and break the wasm gate against modern wasmtime. 1.95 (the active stable as of 2026-05-29) is the recommended production version.

Rejected floors:

- **1.74 (November 2023)**: pre-rename; would force the wasm gate to use wasm32-wasi which is being phased out from cargo's target list.
- **1.85 (March 2025)**: adds stable async closures, which Mochi's design does not exercise (closures lower to `Box<dyn Fn>`, not async-fn). No gain.
- **1.84 (December 2024)**: makes symbol-table emission deterministic by default. Useful for phase 16 but not load-bearing — we set the strip flag explicitly anyway, so older rustc versions also reproduce.

## Why single-thread runtime

Mochi's source language does not have a thread-spawn primitive. The closest is `async expr`, which colours an expression but does not request concurrent evaluation. Mochi's streams and channels are concurrency-flavored but operationally single-producer-single-consumer under standard use.

Forcing every Mochi program through `Arc<Mutex<...>>` for these primitives would:

- Pay a synchronisation cost on every send / recv / emit.
- Force every captured value in a closure to be `Send + 'static`.
- Break the embedded gate (Arc requires `portable-atomic` or `alloc::sync` which needs atomic CAS, unavailable on some MCU targets).

The single-thread choice gives `Rc<RefCell<...>>` everywhere, which compiles under `embedded` (with alloc, no std), has zero CAS overhead, and matches the source-language operational model exactly.

Trade-off: users who write a Mochi program needing real concurrent threads must call into `std::thread::spawn` via FFI. This is documented as a limitation, not a deficiency: Mochi's source language does not promise threads.

## Why Box dyn Fn for closures

Three alternatives:

1. **Generic `impl Fn`**: cannot be stored in struct fields, returned from functions with multiple bodies, kept in homogeneous Vec. Mochi closures can be assigned to vars, returned from functions, stored in records. Rejected.

2. **`fn` pointer**: cannot capture state. Mochi closures can capture. Rejected.

3. **`Box<dyn Fn>`**: heap-allocated, indirect call, but supports all the things impl Fn can't. Accepted.

The colour pass mitigates the heap-allocation cost by eliding `.clone()` calls when a captured value is dead after the closure is constructed. For Copy types (i64, f64, bool), the clone is free anyway.

## Why no tokio

tokio is a heavyweight async runtime (~500K LOC across the runtime + ecosystem). Pulling it in for a transpiler whose source-language `async` is immediate-eval would be reverse subsidy. Mochi's `async expr` lowers to `expr` (no executor); `await fut` lowers to `fut` (identity). tokio is available as a user-imported dep for programs that need real async.

Same logic kills `reqwest` (~50 transitive deps, async-by-default), `serde` / `serde_json` (significant compile-time cost, monomorphisation explosion), and `sha2` (multiple sub-crates for one hash function). The runtime crate stays dep-free past `itertools` (which adds one thin sorted-by-key adapter; see [[dataset-pipeline]]).

## Why a structural rtree (not syn / quote)

`syn` is a procedural-macro IR with TokenStream-based rendering. Using `syn` from a Go program would require either a Rust-side helper binary (build complexity) or `quote`-style token emission from Go (no win over direct string emission). The structural `rtree` AST gives free indentation, supports the colour pass cleanly, and is reused from the Ruby target's `rtree` design. See [[codegen-design]].

## Why Apache-2.0 for the runtime crate

Matches the Mochi project license. Permissive enough for any user (including embedded firmware vendors who need patent-grant guarantees) to vendor the runtime into their program.

## Cross-references

- [[language-surface]] for the feature mapping.
- [[runtime]] for the building blocks chosen.
- [[type-lowering]] for the type system mapping.
- [[risks-and-alternatives]] for the rejected alternatives in detail.
