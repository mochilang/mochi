---
title: "Phase 18. Embedded no_std + alloc"
sidebar_position: 20
sidebar_label: "Phase 18. Embedded"
description: "MEP-53 Phase 18, no_std + alloc embedded variant via the `embedded` feature, gating std-requiring modules behind cfg(feature = std)."
---

# Phase 18. Embedded no_std + alloc variant

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22617](https://github.com/mochilang/mochi/issues/22617) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | f4c4cb32d3 |

## Gate

`TestPhase18Embedded` in `transpiler3/rust/build/phase18_test.go` shells out to `cargo check --no-default-features --features embedded` against the `runtime3/rust/mochi-runtime` workspace. The gate is exit-zero from cargo. Skipped in `-short` mode and when cargo is not on PATH.

## Lowering decisions

The runtime crate declares two features:

```toml
[features]
default = ["std"]
std = []
embedded = []
```

Default features include `std` so non-embedded users see no behavior change. The `embedded` feature, when selected, disables `default` (via `--no-default-features`) and signals to the runtime that std is unavailable.

`src/lib.rs` is gated:

```rust
#![cfg_attr(feature = "embedded", no_std)]

#[cfg(feature = "embedded")]
extern crate alloc;

#[cfg(feature = "std")]
pub mod io { /* ... */ }

pub mod conv {
    #[cfg(feature = "embedded")]
    use alloc::string::{String, ToString};
    // ...
}

pub mod strings {
    #[cfg(feature = "embedded")]
    use alloc::string::{String, ToString};
    // ...
}

#[cfg(feature = "std")]
pub mod chan { /* ... */ }

#[cfg(feature = "std")]
pub mod stream { /* ... */ }

#[cfg(feature = "std")]
pub mod panic { /* ... */ }

#[cfg(feature = "std")]
pub mod fetch { /* ... */ }

#[cfg(feature = "std")]
pub mod json { /* ... */ }

#[cfg(feature = "std")]
pub mod llm { /* ... */ }

#[cfg(feature = "std")]
pub mod check { /* ... */ }
```

Modules that need only `alloc` (conv, strings) are always exposed but explicitly `use alloc::string::{String, ToString};` when the `embedded` feature is on (under no_std, `String` is not in the prelude). Modules that need std (io for `println!`, chan / stream for `Rc<RefCell>` indirectly via collections, panic for `std::panic::catch_unwind`, fetch for `std::net::TcpStream`, json for `std::collections::HashMap`, llm for `std::env` and `std::fs`, check for the panic dep) are gated behind `#[cfg(feature = "std")]`.

The embedded subset is intentionally narrow: only conv and strings work. A user emitting Mochi → Rust for embedded use would be restricted to scalar arithmetic, integer-to-string conversion, and char-aware string slicing. Collections, channels, streams, panic, fetch, JSON, LLM, and runtime-checked arithmetic are unavailable. This is documented as a "Mochi subset" target.

## Files changed

| File | Purpose |
|------|---------|
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `cfg(feature = "std")` gates and `cfg(feature = "embedded")` extern crate alloc |
| `runtime3/rust/mochi-runtime/Cargo.toml` | Declare `default`, `std`, `embedded` features |
| `transpiler3/rust/build/phase18_test.go` | `cargo check --no-default-features --features embedded` gate |

## Test set

- `TestPhase18Embedded` (single subtest, the cargo check itself).

## Closeout notes

The first attempt at the embedded feature failed with 72 errors because std:: paths were referenced everywhere unconditionally (println! in io, std::sync::Once in panic, std::net::TcpStream in fetch, std::collections::HashMap in json, etc). The fix was to gate every heavy module behind `#[cfg(feature = "std")]` rather than try to make them work under no_std + alloc. conv and strings were the only modules that could be made alloc-only without significant rework, and even those required adding `use alloc::string::{String, ToString};` at the top of each module (under embedded, these are not in the implicit no_std prelude).

The gate is `cargo check` rather than `cargo build` because building a `cdylib` or `staticlib` for a no_std target requires a `panic_handler` and `global_allocator`, which are application-level concerns. `cargo check` verifies the library compiles in isolation, which is enough to assert the embedded feature is well-formed.

Future sub-phase 18.1 could add a real bare-metal target (`thumbv7em-none-eabihf` for Cortex-M4F) and a tiny `no_main` test program that links against the embedded subset.
