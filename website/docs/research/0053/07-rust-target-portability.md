---
title: "07. Rust target and portability"
sidebar_position: 8
sidebar_label: "07. Portability"
description: "Rust 1.78 / 1.95 matrix, host triples, cargo-zigbuild musl, wasm32-wasip1 (renamed from wasm32-wasi), no_std + alloc embedded, why no Polonius / gccrs / mrustc."
---

# 07. Rust target and portability

This note covers the version floor, the target triple matrix, and the cross-build mechanism MEP-53 uses to ship binaries across architectures.

## Version floor: 1.78

- **Rust 1.78 (May 2024)** is the floor: first stable with `wasm32-wasip1` (renamed from `wasm32-wasi`). Any earlier version would force the wasm gate to use the old name, which is being deprecated.
- **Rust 1.95 (May 2026)** is the active stable as of MEP-53 landing. Used as the production gate.

Reasons not to floor earlier:

- **1.65 (November 2022)** added GATs (Generic Associated Types). MEP-53 doesn't use them.
- **1.70 (June 2023)** added `OnceCell::get_or_init` stable. MEP-53 uses `Once` instead (sufficient).
- **1.74 (November 2023)** is the older stable many shops still pin. Could be a fallback floor, but loses the wasm32-wasip1 rename which is the load-bearing piece.

Reasons not to floor later:

- **1.85 (March 2025)** added stable async closures. Not used.
- **1.84 (December 2024)** made symbol-table emission deterministic by default. Useful for phase 16 but not load-bearing; the strip flag is set explicitly.

## Host triple matrix

| Triple | Status | Mechanism |
|--------|--------|-----------|
| `aarch64-apple-darwin` | LANDED | Native cargo build (M-series Macs) |
| `x86_64-apple-darwin` | LANDED (untested in CI) | Native cargo build (Intel Macs) |
| `x86_64-unknown-linux-gnu` | LANDED (untested in CI) | Native cargo build (GNU libc) |
| `x86_64-unknown-linux-musl` | LANDED via cargo-zigbuild | `cargo zigbuild --target x86_64-unknown-linux-musl` |
| `aarch64-unknown-linux-musl` | LANDED via cargo-zigbuild | `cargo zigbuild --target aarch64-unknown-linux-musl` |
| `x86_64-pc-windows-msvc` | NOT STARTED | Would require cross-compilation toolchain on the host |
| `aarch64-pc-windows-msvc` | NOT STARTED | Same |
| `wasm32-wasip1` | LANDED (phase 17) | `cargo build --target wasm32-wasip1` + wasmtime |

`cargo-zigbuild` is the canonical 2024-2026 cross-build tool for Rust → musl Linux from non-Linux hosts. It uses Zig's C compiler (which ships with the musl libc statically) as the linker. Zero extra toolchain install beyond `cargo install cargo-zigbuild`.

## wasm32-wasip1 rename

Rust 1.78 renamed `wasm32-wasi` to `wasm32-wasip1`. The old name aliased the new one through Rust 1.84 (deprecated warning); in Rust 1.85 the alias was removed. MEP-53 emits `--target wasm32-wasip1` and uses the binary subpath `wasm32-wasip1/release/<name>.wasm`. Phase 17 is the gate.

`wasm32-wasip1` (WASI Preview 1) is the production-stable WASI target. `wasm32-wasip2` (WASI Preview 2 components) is available on nightly but not yet on stable as of Rust 1.95. A future sub-phase could add `wasm32-wasip2` once it's stable.

`wasm32-unknown-unknown` (no host imports, browser target) is not exposed by MEP-53. Mochi-emitted wasm modules talk to wasmtime via WASI for stdout / env / fs; there is no JS-side counterpart.

## Embedded: no_std + alloc

The `embedded` feature on the `mochi-runtime` crate (phase 18 gate):

- Disables the `std` feature.
- Adds `#![cfg_attr(feature = "embedded", no_std)]` at the crate root.
- Adds `extern crate alloc;` under `#[cfg(feature = "embedded")]`.
- Exposes only the `conv` and `strings` modules. All other modules are gated behind `#[cfg(feature = "std")]`.

The gate is `cargo check --no-default-features --features embedded`, run against the runtime crate workspace. Exit zero passes.

The embedded subset is intentionally narrow: Mochi programs using collections, channels, streams, panic, fetch, JSON, LLM, or runtime-checked arithmetic do not compile under embedded. This is documented as a "Mochi subset" target.

## Rust frontends not supported

- **gccrs (rust-lang/gccrs, ongoing as of 2026-05)**: GCC frontend for Rust. Not yet stable enough for a serial CI gate. A future sub-phase could add a best-effort gccrs build target.
- **mrustc (Mutabah, 2014+)**: Alternative Rust compiler in C++, used for bootstrapping. Out of scope.
- **Polonius (rust-lang)**: experimental borrow-checker replacement. Always uses rustc as the surface; not a separate frontend.

MEP-53 commits to `rustc` (the official compiler) as the only frontend.

## Static linking

`cargo zigbuild --target x86_64-unknown-linux-musl` produces a statically linked binary by default (musl libc is statically linked, all Rust stdlib + the runtime crate are statically linked). No `LD_LIBRARY_PATH` dance; the binary runs on any Linux kernel from 2.6.32+.

`cargo build --release` on the host triple produces a dynamically linked binary that depends on the host's libc. This is the default for development; for distribution, use `--target` to get static linking.

## ABI considerations

Mochi's runtime crate exposes only Rust-internal types (`String`, `Vec`, `HashMap`, generic `Fn` traits). There is no C ABI surface unless an `extern fn` directive (phase 12) is lowered. When extern fn is present, the cffi/ sidecar is compiled to a static archive (cc-rs + `cargo:rustc-link-lib=static=mochi_cffi`) and linked into the final binary. The ABI is plain `extern "C"`, which is stable across Rust versions and matches what C consumers expect.

## Cross-references

- [[design-philosophy]] for the version floor rationale.
- [[build-system]] for the cargo / cargo-zigbuild / wasmtime invocations.
- [[testing-gates]] for the per-target test matrix.
- [MEP-53 §2](/docs/mep/mep-0053#2-toolchain-detection) for the toolchain detection contract.
