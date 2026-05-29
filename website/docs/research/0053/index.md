---
title: MEP-53 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-53 (Mochi-to-Rust transpiler). Twelve notes covering language surface, Rust design philosophy, prior art on Rust transpilers and ecosystem tooling, runtime crate building blocks, codegen via the structural rtree AST, type lowering, Rust portability matrix, dataset pipeline, agents on single-thread Rc<RefCell>, cargo build system, testing gates, and risks."
---

# MEP-53 research notes

These twelve notes are the deep research that fed MEP-53 (Mochi-to-Rust transpiler). They are informative; the MEP body at [/docs/mep/mep-0053](/docs/mep/mep-0053) is normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

Author: research pass for MEP-53 (Mochi-to-Rust transpiler).
Date: 2026-05-29 (GMT+7).

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0053/language-surface) | Mochi features mapped onto Rust 1.78+ lowering obligations |
| 02 | [Design philosophy](/docs/research/0053/design-philosophy) | Why Rust, why 1.78 floor, why single-thread runtime, why Box<dyn Fn>, why no tokio |
| 03 | [Prior-art transpilers](/docs/research/0053/prior-art-transpilers) | bindgen / cbindgen, rust-bindgen, rapydscript-ng, rune, cretonne / Cranelift, gccrs, Polonius, mrustc, Rust-GPU, IL2Rust analogues, Embedded Rust, SwiftWasm comparison |
| 04 | [Runtime building blocks](/docs/research/0053/runtime) | Rust stdlib, alloc crate, std::collections, std::net, panic::catch_unwind, no_std + alloc convention, the rejected tokio / reqwest / serde / sha2 alternatives |
| 05 | [Codegen design](/docs/research/0053/codegen-design) | Rust source via rtree IR (not syn / quote), aotir reuse, the colour pass for borrow-and-clone elision, 4-space indent, stable item ordering |
| 06 | [Type-system lowering](/docs/research/0053/type-lowering) | Mochi types onto i64 / f64 / String / Vec / HashMap / HashSet / struct / tagged enum / Box<dyn Fn>, the Rc<RefCell> single-thread choice |
| 07 | [Rust target and portability](/docs/research/0053/rust-target-portability) | Rust 1.78 / 1.95 matrix, host triples, cargo-zigbuild musl, wasm32-wasip1 (renamed from wasm32-wasi), no_std + alloc embedded, why no Polonius / gccrs / mrustc |
| 08 | [Dataset pipeline](/docs/research/0053/dataset-pipeline) | Query DSL via Iterator + itertools, BTreeMap for deterministic group-by, compile-time Datalog (semi-naive fixpoint) emitted as frozen Vec literals |
| 09 | [Agents and streams](/docs/research/0053/agent-streams) | Agents as plain structs (no threads), single-thread Rc<RefCell<VecDeque>> channels, single-thread broadcast streams, why Arc<Mutex> rejected, async colouring with no runtime effect |
| 10 | [Build system](/docs/research/0053/build-system) | Cargo as canonical driver, deterministic Cargo.toml, SOURCE_DATE_EPOCH=0 + RUSTFLAGS=-C strip=symbols, cargo zigbuild for musl, wasmtime for wasm, the cffi/ sidecar + cc-rs build script |
| 11 | [Testing gates](/docs/research/0053/testing-gates) | Per-phase Go test gates with vm3 oracle, byte-equal stdout diff, cargo check secondary gate, reproducibility SHA-256 gate, publish dry-run, embedded cargo check |
| 12 | [Risks and alternatives](/docs/research/0053/risks-and-alternatives) | Risk register (LC_UUID macOS, generic-closure storage, wasm32 no-TCP, cc-rs detection, cassette drift, embedded surface gap) + rejected alternatives (tokio, reqwest, serde, sha2, Arc/Mutex, impl Fn, syn / quote, inlined runtime, no published crate) |

Each note's filename uses the `NN-slug.md` convention; the leading `NN-` is stripped by Docusaurus for the URL path, so cross-links inside the notes use the unprefixed slug (e.g. `[[language-surface]]`).

The companion MEP body lives at [/docs/mep/mep-0053](/docs/mep/mep-0053). Implementation tracking lives at [/docs/implementation/0053/](/docs/implementation/0053/).
