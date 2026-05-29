---
title: "MEP-73 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-73: language surface, design philosophy, prior-art Rust bridges, rustdoc JSON ingest, the closed type-mapping table, the Cargo publish flow, Cargo RFC #3724 trusted publishing, the async bridge, ABI stability, lifetimes and ownership translation, embedded and no_std subset, plus the risks and rejected alternatives register."
---

# MEP-73 research bundle

This bundle is the informative companion to [MEP-73](/docs/mep/mep-0073). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import rust "..."` import shape, the `mochi.toml` `[rust-dependencies]` + `[rust]` tables, the CLI surface (`mochi pkg add rust`, `mochi pkg publish --to=crates.io`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why rustdoc JSON over alternatives, why a synthesised wrapper crate over direct FFI, why the async bridge sits on a tokio singleton, why Sigstore-keyless is the only publish path. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | PyO3, neon, napi-rs, uniffi, diplomat, swift-bridge, cxx, autocxx, wit-bindgen, JNI / JNR. What each gets right, what each requires the user to write, and what MEP-73 borrows. |
| [04. Rustdoc JSON ingest](04-rustdoc-json-ingest.md) | The rustdoc-types schema, the ItemEnum / Type discriminators, the stability story, why nightly is required at lock time, the Go-side parser shape. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic monomorphisation rule, the `&str` vs `String` parameter handling, the `Option` and `Result` desugar. |
| [06. Cargo publish flow](06-cargo-publish-flow.md) | The crates.io upload protocol, the per-package metadata requirements, the sparse-index format, the .crate tarball shape, the publish-side gate. |
| [07. Sigstore and Cargo RFC #3724](07-sigstore-cargo-rfc3724.md) | The OIDC token exchange, the Fulcio short-lived cert, the Rekor transparency log, the verification path at install time, the crates.io trusted-publishing GA timeline. |
| [08. Async bridge](08-async-bridge.md) | The tokio runtime singleton, the current-thread vs multi-thread choice, the `block_on` cost, the cancellation semantics, the cross-runtime mismatch when a crate uses async-std. |
| [09. ABI stability](09-abi-stability.md) | `extern "C"` guarantees, `repr(C)` requirements, opaque handle strategy for non-repr(C) types, the `String` and `Vec` round-trip, drop semantics across the wrapper boundary, the static link vs cdylib decision. |
| [10. Lifetimes and ownership](10-lifetimes-and-ownership.md) | How the bridge translates `&'a T`, `&'a mut T`, `Box<T>`, `Rc<T>`, `Arc<T>` into Mochi's ownership-free surface; the borrow-to-clone strategy; the move-to-handle strategy; the lifetime erasure trade-off. |
| [11. Embedded and no_std](11-embedded-and-no-std.md) | The `no_std` subset of crates.io, the `alloc` feature flag, the embedded MEP-53 target, what kind of Rust crates Mochi can consume on bare metal, the firmware story. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (rustdoc stability, wrapper compile time, generic explosion, tokio cost, capability drift, GA timing) and the rejected alternatives (cbindgen primary, cxx, uniffi, diplomat, WIT-only, dlopen pre-built, lifetime translation, long-lived token, multi-thread default, per-build cargo). |

## Cross-references

- [MEP-73 spec](/docs/mep/mep-0073) — the normative document.
- [MEP-53](/docs/mep/mep-0053) — the Rust transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [Implementation tracking](/docs/implementation/0073/) — the per-phase delivery status.
