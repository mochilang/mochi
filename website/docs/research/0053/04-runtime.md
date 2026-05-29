---
title: "04. Runtime building blocks"
sidebar_position: 5
sidebar_label: "04. Runtime"
description: "Rust stdlib, alloc crate, std::collections, std::net, panic::catch_unwind, no_std + alloc convention, and the rejected heavyweight dep alternatives."
---

# 04. Runtime building blocks

This note surveys the Rust building blocks MEP-53 leans on and explicitly names the heavyweight alternatives that were rejected.

## Stdlib surface used

- `std::println!` (via the runtime's `io::print_*` helpers): integer / float / string / bool formatting.
- `std::collections::{HashMap, HashSet, BTreeMap, BTreeSet, VecDeque}`: collection lowerings.
- `std::cell::RefCell` and `std::rc::Rc`: single-thread shared interior mutability for channels and streams.
- `std::panic::{catch_unwind, panic_any, set_hook, AssertUnwindSafe}`: try / catch / panic lowering.
- `std::sync::Once`: idempotent panic-hook silencing in `mochi_runtime::panic`.
- `std::net::TcpStream`: HTTP/1.1 fetch.
- `std::fs::{read_to_string, write, OpenOptions}`: file I/O.
- `std::env::var`: environment access for the LLM cassette dir.
- `std::ffi::CString`: FFI string round-trip.
- `std::path::PathBuf`: path manipulation in the LLM cassette lookup.
- `std::time::SystemTime` (rare; only when a Mochi program uses `now()`): wall-clock access.

## alloc crate (no_std + alloc)

Under the `embedded` feature, the runtime crate drops `std` and pulls in `alloc` only:

- `alloc::string::{String, ToString}`: explicit `use alloc::string::{...}` because under no_std these are not in the prelude.
- `alloc::vec::Vec`: implicit through `Vec::with_capacity` etc.

The conv and strings modules are the only modules that compile under embedded. They use alloc::String for their input/output types and rely on char iteration (no std::str::Chars-specific behavior beyond what alloc provides).

## Rejected heavyweight deps

- **tokio**: ~500K LOC across the runtime + ecosystem. Pulling it in for a transpiler whose source-language `async` is immediate-eval would be reverse subsidy. tokio is available as a user-imported dep.
- **reqwest**: pulls in tokio (or async-h1), rustls or native-tls, hyper, http, mime, and ~50 transitive deps. The 90-LOC hand-rolled `mochi_runtime::fetch` covers the Mochi contract (plain HTTP/1.1 GET, return body as String, panic code 98 on error) with zero deps.
- **serde / serde_json**: significant compile-time cost (monomorphisation explosion), and the runtime exposes only a "decode top-level object into `HashMap<String, String>`" API. The 90-LOC hand-rolled JSON decoder is faster to compile and easier to audit.
- **sha2**: multiple sub-crates (cpufeatures, block-buffer, crypto-common, digest) for one SHA-256 hash. The inlined ~80-LOC implementation in `mochi_runtime::llm` is dep-free and embeddable.
- **chrono**: time formatting and timezone DB. Mochi exposes only `now()` (a Unix epoch second count); `std::time::SystemTime` is enough.
- **anyhow / thiserror**: error-context types. Mochi's error model is panic-with-code, not Result-with-context.

## Accepted thin deps

- **itertools**: adds the `sorted_by_key` adapter used in the query pass. ~3K LOC, no transitive deps past Rust stdlib. The query DSL needs sorting and stable-collection-into-Vec; rolling our own would be ~40 LOC but with edge cases (descending order, stable-sort guarantees).
- **cc**: in the emitted Cargo.toml (build-dependency) for cffi/ phase 12. ~10K LOC, no transitive deps. Standard for any crate doing C interop.

## panic::catch_unwind details

The choice to use `panic::catch_unwind` for Mochi's try / catch rather than `Result<T, E>`:

- Mochi `try { stmt1; stmt2; stmt3; }` wraps arbitrary statements, not expression-level operations. Routing through `Result` would require either a full effect-system colouring pass or wrapping every fallible op site (every division, every index) in `?`.
- `panic::catch_unwind` is the standard Rust mechanism for converting unwinding panics into a Result-shape. Wrapped behind `mochi_runtime::panic::catch`, it gives Mochi user code a clean try-shape with zero per-statement overhead.
- `AssertUnwindSafe` is required because Mochi closures may capture `&mut` state; we assert safety because a Mochi panic always returns the code and never resumes with torn state.
- `silence_hook` is needed because the default panic hook prints to stderr on every panic. We install a no-op hook exactly once via `Once` so panic messages don't leak.

## Float formatting determinism

`io::print_f64` matches vm3 exactly:

- NaN → "NaN" (not "NaN" with quotes, just NaN).
- ±Inf → "+Inf" / "-Inf" (Rust default would be "inf" / "-inf").
- Integer-valued floats in [-2^53, 2^53] → integer rendering (no decimal point).
- Otherwise → Rust's default `{}` Display (`1.5` not `1.50000000000000`).

This list of special cases is the minimum needed to match vm3's byte-equal output; future fixtures may extend it.

## Cross-references

- [[design-philosophy]] for the "no tokio / no reqwest" rationale.
- [[type-lowering]] for HashMap vs BTreeMap selection.
- [[agent-streams]] for Rc/RefCell single-thread choice.
- [[testing-gates]] for the embedded cargo check gate.
- [MEP-53 §4](/docs/mep/mep-0053#4-runtime-crate) for the normative module list.
