---
title: "03. Prior-art transpilers"
sidebar_position: 4
sidebar_label: "03. Prior art"
description: "Survey of source-to-Rust transpilers, Rust-as-target compilers, and related ecosystem tooling."
---

# 03. Prior-art transpilers

This note surveys the prior-art landscape that informed MEP-53's design choices. The scope is: tools that emit Rust source or consume Rust source for code-gen / lowering / FFI.

## Source-to-Rust transpilers

- **rust-bindgen (March 2017, hosted by rust-lang/rust-bindgen)**: emits Rust FFI bindings from C/C++ headers. Used by ~3,000 crates. Demonstrates that a structural representation of foreign-language types can be cleanly lowered to Rust struct + extern fn shape; MEP-53's cffi/ sidecar borrows the convention of putting C headers next to Rust source for cc-rs consumption.
- **cbindgen (Mozilla, 2017)**: the inverse — emits C headers from Rust source. Lower scope (just structs + extern fn) but the lesson is the same: structural emit beats string templates.
- **c2rust (Galois / Immunant, 2018, MIT-licensed)**: transpiles C source to Rust source. The relevant lesson: C-style pointer arithmetic doesn't lower cleanly without an `unsafe` heavy hand; MEP-53 avoids this entire class of issue by not lowering pointers (Mochi has no pointer type).
- **Corrode (Jamey Sharp, 2015, dormant since 2017)**: earlier C-to-Rust transpiler. Mostly historical interest; informed the design split between "translate semantics" (Corrode's goal) and "produce idiomatic Rust" (c2rust's goal). MEP-53 is on the "produce idiomatic Rust" side.

## Rust-as-target compilers (non-Rust-source frontends)

- **rune (rune-rs project, 2020+)**: a scripting language with Rust-source emit and a Rust-embedded interpreter. The "embedded interpreter" model is rejected by MEP-53 (we want native binaries, not embedded interpretation), but rune's Rust API for emitting types informed the rtree shape.
- **rapydscript-ng (Python-like → JavaScript, also experimented with Rust target)**: relevant for the closure-conversion approach. Python-like sources have rich closure semantics; the prior work on lowering them to fixed-shape Rust idioms informed MEP-53's `Box<dyn Fn>` decision.

## Rust → other targets (relevant for IR design)

- **cranelift (Bytecode Alliance, ongoing)**: the JIT / AOT codegen backend used by wasmtime, rustc (in nightly), and several Wasm compilers. Demonstrates that a Rust-side codegen IR can be small, well-defined, and reused across backends. MEP-53's `aotir` is a similar shape (target-agnostic, lowered per-target).
- **Rust-GPU (EmbarkStudios, 2020+)**: Rust source → SPIR-V for GPU shaders. The relevant insight: target-specific lowerings (no heap, no std, no panic) require careful feature-gating in the runtime crate. MEP-53's `embedded` feature gate is a smaller-scope version of the same idea.
- **gccrs (rust-lang/gccrs, ongoing as of 2026-05)**: an alternative Rust frontend for GCC. Not directly relevant to MEP-53, but the existence of multiple Rust frontends informs the [[testing-gates]] discussion: we don't need to support gccrs in CI because rustc is the canonical compiler.
- **mrustc (THiNotes / Mutabah, 2014+)**: an alternative Rust compiler implemented in C++. Used for bootstrapping. Out of scope; MEP-53 always uses cargo + rustc.
- **Polonius (rust-lang, ongoing)**: an experimental borrow-checker replacement. The relevant insight: borrow analysis is well-defined enough at the source level that an external pass can mirror it. MEP-53's colour pass does a small subset (clone-or-borrow at use sites) and trusts rustc to flag any miss.

## Wasm-related tooling

- **wasm-bindgen (rust-lang, 2017+)**: emits the host glue for wasm32-wasi modules. Not used by MEP-53 because Mochi-emitted wasm modules talk to wasmtime via WASI directly, not via JS host imports. Informed the decision to target wasm32-wasip1 (WASI Preview 1) rather than wasm32-unknown-unknown (no host imports at all).
- **wit-bindgen (Bytecode Alliance, 2022+)**: Wasm Component Model bindings. Out of scope for MEP-53; the wasm gate uses Preview 1, not Preview 2 components. A future sub-phase could add component-model support.
- **walrus, wasm-tools (Bytecode Alliance)**: low-level wasm manipulation. Not used by MEP-53 (cargo handles wasm linkage), but informative for future wasm sub-phases.

## Embedded Rust tooling

- **embedded-hal (rust-embedded WG, 1.0 May 2025)**: hardware-abstraction-layer traits for MCU peripherals. Out of scope; MEP-53's `embedded` feature gates the runtime crate to a "no I/O" subset that doesn't need HAL.
- **embassy (Embassy Project, 1.0 May 2025)**: an async embedded runtime. Out of scope for the same reason.
- **panic-halt, panic-abort (panic handler crates)**: required for any `no_std` binary. MEP-53 does not emit binaries for the embedded gate (cargo check is the gate, not cargo build), so a panic handler is not required.

## What MEP-53 borrows

- From rust-bindgen: structural representation of foreign types; co-locating headers with source.
- From cranelift: target-agnostic IR (aotir) with per-target lowering passes.
- From Rust-GPU: feature-gating to assert subset compilation.
- From the Mochi project's Ruby target (MEP-56): the structural `rtree` AST design.
- From the C target (MEP-45): the aotir + clower pipeline.

## What MEP-53 deliberately doesn't borrow

- No use of syn / quote for Rust-side emit (Go-side string emission via rtree is enough).
- No reliance on rustc plugins or proc-macros (the emitted code is plain Rust with derives only).
- No use of cargo as a library (we shell out to cargo as a subprocess).

## Cross-references

- [[design-philosophy]] for the rationale behind these choices.
- [[codegen-design]] for the rtree IR.
- [[runtime]] for the runtime crate composition.
- [[rust-target-portability]] for the target matrix.
