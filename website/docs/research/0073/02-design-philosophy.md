---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional bridge, why rustdoc JSON over alternatives, why a synthesised extern C wrapper crate over direct FFI or cxx-style hand-written bridges, why the async bridge sits on a tokio singleton, why Sigstore-keyless is the only publish path, why the type-mapping table is closed not open."
---

# 02. Design philosophy

This note frames the six load-bearing design decisions in MEP-73 alongside the alternatives that were considered and rejected. Each section follows the same structure: the decision, the alternatives, the trade-offs.

## 1. Why bidirectional

Mochi could ship only the consume direction (`import rust "..."`) and leave Mochi-as-crates.io-publisher to a future MEP. Or only the publish direction, leaving consumption to a future MEP. The two directions are independent: consuming a crate uses rustdoc-JSON + wrapper synthesis; publishing a crate uses MEP-53's emit pipeline plus crates.io's upload API. They share infrastructure (the manifest tables, the lockfile sections, the trusted-publishing OIDC flow) but no code paths.

Shipping both directions in one MEP is the right scope because:

- **Symmetric distribution.** A library author writes Mochi, depends on Rust crates, publishes to crates.io. A library consumer either writes Mochi (uses MEP-73's `import rust`) or writes Rust (uses the cdylib via `cargo add`). A unidirectional bridge would leave one side of the symmetry broken: Mochi authors could not reach Rust users, or Rust authors could not reach Mochi users.

- **Shared OIDC flow.** Sigstore-keyless publishing requires a working OIDC token exchange in CI, a working Fulcio cert pull, a working Rekor log entry write. Implementing this once (for crates.io publish) and not at all (because consumers don't use it) would mean reimplementing it later. Doing both directions at once amortises the work.

- **Capability surface symmetry.** The `[rust.capabilities]` table that audits which capabilities the imported dep graph requires has its mirror in MEP-57's `[capabilities]` table that audits which capabilities the Mochi-published library requires. The same monotonicity rule applies to both, and they share the audit pass.

The trade-off is that MEP-73 is a larger spec than a strict single-direction bridge would be. The alternative (split into MEP-73 consume + MEP-74 publish) was rejected because the seam between them is artificial and would force two redundant lockfile-section migrations.

## 2. Why rustdoc JSON

The bridge needs a machine-readable description of every Rust crate's public surface. Four candidate sources existed:

- **Rustdoc JSON** (`cargo rustdoc --output-format=json`, currently nightly behind `-Z unstable-options`, schema pinned by the `rustdoc-types` crate). Produces the full ItemEnum tree for every public item: functions, structs, enums, traits, impls, modules, with full type signatures including generics, where clauses, and trait bounds.

- **cbindgen** (a tool that walks Rust source via the `syn` crate and emits a C header). Sees only items already annotated `extern "C"` and `#[no_mangle]`. Misses 95% of idiomatic crates.io content (most crates have zero `extern "C"` items).

- **Source parsing** (a Rust parser implemented in Go). Would have to handle the full Rust grammar including const generics, lifetimes, macros, attributes, and the ever-evolving stabilisation surface. Proc-macros would still defeat the source-level parse because the macro expands at compile time and the post-expansion surface is invisible to a source parser.

- **Manual interface description files** (uniffi's `.udl`, cxx's `#[cxx::bridge]`, diplomat's `#[diplomat::bridge]`). The user authors the interface, the tool generates the bindings. Violates the "no boilerplate" promise of MEP-73.

Rustdoc JSON wins on every axis except stability: the format is currently nightly-only. The compromise is to require a nightly toolchain at lock time (the user's normal `cargo build` runs on stable). The rust-lang/rust tracking issue #76578 for `--output-format=json` stabilisation is actively progressing; the `rustdoc-types` crate has been stable enough since v0.20 (October 2024) to serve as a versioned schema target.

The rustdoc-JSON ingest path also has the property that it sees the crate exactly as `cargo doc` would: any item invisible to `cargo doc` (private items, items behind disabled features, items behind `cfg` flags) is invisible to the bridge. This matches the user's mental model.

## 3. Why synthesised extern "C" wrapper crate

Given an ingested rustdoc-JSON surface, the bridge has three routes to making the items callable from Mochi:

- **Direct FFI**: Mochi's MEP-53 emit pass generates Rust code that directly invokes the source crate's API. This requires teaching the Mochi `aotir` IR about Rust lifetimes, generics, borrow rules, and async semantics, then teaching the Rust emit pass to render them. The cost is high (the IR is target-agnostic by design; the multi-backend invariant breaks). The benefit is zero wrapper compile time.

- **Synthesised extern "C" wrapper crate**: the bridge generates a sibling Rust crate that depends on the source crate and exposes a flat C-ABI surface. The Mochi side calls into the wrapper via a known, stable, lifetime-free, generic-free, borrow-free ABI. The wrapper crate is built via the same `cargo build --release` that builds the user's program.

- **cxx-style bridge**: the user authors a `#[cxx::bridge] mod ffi { ... }` block per imported crate, listing the items they want exposed and their bridge types. cxx generates both the Rust-side and the C++-side. Same boilerplate violation.

The synthesised wrapper crate path is the only one that delivers the "no boilerplate" promise without breaking the `aotir` IR's target-agnosticism. The wrapper-crate build is added to the user's `cargo build --release`; warm-cache wrapper compile is ~2-8 seconds per crate, cold-cache ~10-30 seconds. The wrappers are cached in `~/.cache/mochi/rust-deps/wrappers/<wrapper-sha256>/` and shared across workspaces.

The wrapper-vs-direct trade-off mirrors how PyO3 and napi-rs work: those frameworks also synthesise extern "C" wrappers (via `#[pyfunction]` and `#[napi]` proc-macros respectively). MEP-73 differs in that the synthesis is done by the Go side at lock time, not by Rust proc-macros at compile time, so the user does not edit Rust source at all.

## 4. Why a tokio runtime singleton for async

Idiomatic Rust crates expose a substantial `async fn` surface. Calling `async fn` from synchronous Mochi requires picking a host async runtime, constructing it once, and dispatching every async call through `block_on`. Three runtime choices existed:

- **tokio**: by far the most widely used (90%+ of async crates target tokio, May 2026 estimate from the crates.io top-5000 survey). Multi-thread and current-thread flavours. Hyper, reqwest, sqlx, axum, mio all build on tokio.

- **async-std**: an alternative runtime with stdlib-shaped APIs. Production-quality but second-place in adoption. As of 2026-05 the async-std team has slowed releases (the project is in maintenance mode since 2024-Q4).

- **smol**: a small async runtime designed for embedded and stand-alone use. Niche.

The bridge defaults to tokio with current-thread flavour. Multi-thread is opt-in via `[rust.runtime] flavor = "multi-thread"`. async-std and smol are not supported; users who need them must hand-author the wrapper.

The trade-off: a Mochi program that imports a single async function (say `tokio::time::sleep`) pulls in the full tokio runtime (~150K LOC across tokio + mio + bytes + pin-project-lite + ...). For programs that only need sync Rust, this cost is wasted. The bridge mitigates by constructing the runtime lazily on first async call; programs that import only sync items never construct the runtime and pay zero runtime cost (but the dep is still in the binary; ~5 MB of stripped object code).

The choice of singleton over per-call runtime is a clear win: per-call construction would be 100-1000x slower per async call.

## 5. Why Sigstore-keyless only for publish

crates.io has historically used long-lived API tokens (`CARGO_REGISTRY_TOKEN`) for `cargo publish` authentication. Cargo RFC #3724 (accepted Q4 2025, rolling GA through 2026) introduced Sigstore-keyless OIDC trusted publishing as a parallel path. Both paths work at publish time.

MEP-73 supports only the Sigstore-keyless path. Long-lived API tokens are rejected:

- **Supply-chain incident pattern.** Every major supply-chain incident from 2022 through 2026 (event-stream, ua-parser-js, xz-utils, retire.js, the ESLint plugin compromise, the cascading PyPI reflected-string flood of late 2025) traces to a compromised long-lived token or a stale credential left on a maintainer's dev laptop. The class of attack is structural; the mitigation is to remove the long-lived token from the trust boundary.

- **Industry direction.** Four ecosystems (npm, Maven Central, PyPI, Cargo) converged on Sigstore-keyless within 18 months (April 2024 through Q4 2025). A 2026 package system that ships without trusted publishing is shipping a decade-out-of-date supply-chain story.

- **Symmetry with MEP-57.** MEP-57 already mandates Sigstore-keyless for the Mochi central registry publish path. Mochi-to-crates.io publish using the same mechanism is the clean choice.

The transition risk: not all crates.io endpoints support OIDC at MEP-73 spec authoring time (May 2026). The bridge ships an `--allow-token-fallback` flag for the transition period, default off. The flag is removed once crates.io reaches full trusted-publishing GA (expected Q3 2026 per the rolling rollout).

## 6. Why a closed type-mapping table

The bridge translates Rust types to Mochi types via a fixed enumerated table. Items whose types fall outside the table are skipped with a `SkipReport`. The alternative would be an open table that synthesises a Mochi wrapper for every Rust type the bridge encounters (newtype-like wrapping for `Cow<T>`, `Box<dyn Trait>`, etc.).

The closed table wins because:

- **Predictable user surface.** A Mochi user can read the table and predict whether a given Rust item will translate. An open table would require the user to read the bridge's internal logic to predict outcomes.

- **Refusal is information.** When the bridge refuses to translate an item, the `SkipReport` tells the user precisely why. The user can then either hand-write an `extern fn` override (taking responsibility for the type at the FFI boundary) or skip the item entirely. An open table would silently translate non-trivial types in ways that may not match user expectations.

- **Generic explosion containment.** Generic items can be instantiated at infinitely many type arguments. An open table would have to either auto-instantiate (combinatorial explosion) or refuse generics (closed table by another name). The explicit `[rust.monomorphise]` declaration in `mochi.toml` makes the explosion bounded and user-visible.

- **Auditability.** The table fits in a single source file (~200 LOC of Go); changes to the table are reviewable as a unit. An open synthesis routine would be order-of-magnitude larger and harder to reason about.

The cost: the closed table refuses many items the user might want. The mitigation is the `extern fn ... custom` override path: the user can always escape the table by taking responsibility for the FFI boundary.

## Cross-references

- [[01-language-surface]] for the user-visible surface.
- [[03-prior-art-bridges]] for the PyO3 / neon / napi-rs / uniffi / cxx comparison the wrapper-vs-cxx decision draws on.
- [[04-rustdoc-json-ingest]] for the rustdoc-types schema choice.
- [[05-type-mapping]] for the closed-table contents.
- [[07-sigstore-cargo-rfc3724]] for the trusted-publishing flow detail.
- [[08-async-bridge]] for the tokio singleton detail.
- [MEP-73](/docs/mep/mep-0073) for the normative spec.
