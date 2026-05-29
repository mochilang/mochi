---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "The MEP-73 risk register (rustdoc JSON nightly-only, wrapper compile time, generic explosion, tokio runtime cost, capability drift, crates.io GA timing, Sigstore CA outage, schema version churn) and the alternatives considered and rejected (cbindgen primary, cxx, uniffi, diplomat, WIT-only, dlopen pre-built, lifetime translation, long-lived tokens, multi-thread default, per-build cargo)."
---

# 12. Risks and alternatives

This note collects the risks MEP-73 carries plus the alternative approaches that were considered and rejected. The risk register is a forward-looking inventory; the alternatives section documents the reasoning so future maintainers can understand why the chosen path was chosen.

## R1: rustdoc-types schema churn

**Risk**: the rustdoc JSON schema version bumps roughly every 2-3 months. Each bump may add new ItemKind / Type variants, rename fields, or change the discriminator scheme. A bridge built against schema v39 produces wrong results against v45.

**Likelihood**: high. The rust-lang/rustdoc team is actively evolving the schema.

**Impact**: medium. A schema mismatch produces a hard error at lock time, not silent miscompilation. Users get an actionable error message.

**Mitigation**:
- The bridge pins a supported schema range per release (`format_version >= 35 && <= 39` for 0.1.x).
- A `mochi pkg lock` against an unsupported schema fails with the message `rustdoc format_version=42 not supported; downgrade nightly to 2026-06-15 or upgrade mochi`.
- The user can pin the nightly toolchain via `[rust] nightly = "2026-06-15"`.
- The bridge ships a new minor version on every quarter to track schema updates.

**Residual**: between the upstream bump and the bridge release, users on a too-new nightly cannot lock. The CI workflow's `--toolchain=nightly-<date>` pin is the recommended workaround.

## R2: Wrapper crate compile time

**Risk**: every imported Rust crate triggers a wrapper crate build. For a Mochi package with 20 Rust deps, the build is 20 wrapper compiles plus the user code compile. Cold builds may take 5-15 minutes; warm builds (with cargo cache) take 30-60 seconds.

**Likelihood**: certain. The wrapper compile cost is intrinsic to the approach.

**Impact**: medium. Slow CI runs increase iteration cost.

**Mitigation**:
- Each wrapper crate has `codegen-units = 1` + `lto = "fat"` to minimise output size at the cost of compile time. The bridge offers `[rust.build] codegen-units = 16` to trade output size for compile speed.
- The content-addressed cache stores compiled wrapper artefacts keyed by `(crate-blake3, wrapper-version, rust-toolchain)`. A re-lock with the same inputs is a cache hit.
- The Mochi-cache layer (MEP-57 §6) shares wrapper artefacts across users on the same machine.

**Residual**: a fresh user (cache cold) pays the full compile cost. We accept this; the alternative (no wrapper layer, pre-built `.so` artefacts) carries supply-chain risk (R7).

## R3: Generic explosion

**Risk**: a user enumerating many monomorphisations (`[rust.monomorphise]` with 50 entries for `serde_json::from_str`) triggers 50 wrapper functions plus 50 Rust generic instantiations. Each generic instantiation reuses the same Rust template but produces a unique compiled function.

**Likelihood**: medium. Most users will enumerate 1-5 instantiations per generic; power users may want more.

**Impact**: low. The compile cost scales linearly. The binary size grows by ~2-10 KiB per instantiation.

**Mitigation**:
- The bridge warns when `[rust.monomorphise]` has more than 20 entries: `WARNING: 50 monomorphisations of serde_json::from_str will increase binary size by ~500 KiB. Consider whether all are needed.`
- The bridge uses `share-generics = true` in cargo's Cargo.toml so cross-crate generic instantiations are shared.

**Residual**: a deliberate power user can explode the binary. We accept this; the alternative (hide monomorphisation behind a wildcard) provides worse error messages when the generic does not type-check.

## R4: Tokio runtime cost

**Risk**: every Mochi binary that imports any async-fn from any Rust crate pays the tokio runtime startup + per-call block_on cost. A small CLI tool ends up with a multi-MB binary mostly composed of tokio.

**Likelihood**: certain. tokio's footprint is intrinsic.

**Impact**: medium. The binary size grows ~3 MB (compressed); the startup cost grows ~5 ms.

**Mitigation**:
- The wrapper enables only the tokio features it needs: `rt`, `time`, `sync`, `io-util`, `signal`. Missing default features include `tracing`, `parking_lot`. This drops ~800 KiB.
- The `[rust.runtime] flavor = "current-thread"` default avoids the multi-thread worker pool (saves ~200 KiB and ~3 ms startup).
- Users who do not import any async-fn pay zero tokio cost: the wrapper does not pull tokio in.

**Residual**: a Mochi tool that does import async-fn pays tokio's footprint. Users who care about footprint can opt out of async-fn imports.

## R5: Capability drift

**Risk**: an imported crate's capability footprint (which syscalls / network / fs it makes) changes between minor versions. A user who locks `reqwest@0.11.20` (no proc capability) may find that `reqwest@0.11.22` calls `Command::new` internally (a proc capability gain).

**Likelihood**: medium. Crates do gain capabilities silently.

**Impact**: high. A capability gain that the user did not authorise is a security boundary violation.

**Mitigation**:
- `mochi pkg lock --check` recomputes the static capability scan against the current lockfile. A drift fails with a diff.
- The CI workflow runs `mochi pkg lock --check` on every push.
- A capability gain triggers an explicit user acknowledgement: `mochi pkg lock --accept-capability=proc` to re-lock with the gained capability.

**Residual**: between the upstream release and the user's next `--check` run, the user is exposed. The mitigation is fast: any CI run catches the drift within ~24 hours.

## R6: crates.io GA timing

**Risk**: the Cargo RFC #3724 trusted-publishing GA rolls out through 2026. If MEP-73 ships before GA, users must use legacy tokens for the publish direction. If MEP-73 ships after GA but before universal adoption, some downstreams (cargo install with sigstore verify) may fail on crates published before the verifier supports the format.

**Likelihood**: high. The GA timeline is announced but historically slipped.

**Impact**: medium. The publish direction may be partially functional during the transition.

**Mitigation**:
- The `--allow-token-fallback` flag exists for the transition period.
- The bridge tracks the crates.io GA milestones and emits deprecation warnings when token mode is used.
- v1 of MEP-73 ships with a "Sigstore-keyless preferred, token fallback supported" mode. Once GA is universal, the token fallback is removed.

**Residual**: users on the transition period have a more confusing flow. The error messages explain the situation.

## R7: Sigstore CA outage

**Risk**: the public-good Sigstore instances (`fulcio.sigstore.dev`, `rekor.sigstore.dev`) are operated by the Linux Foundation as a public-good service. An outage of either prevents new publishes and (for users who fetch the Rekor log at install time) prevents verification.

**Likelihood**: low. The public-good infrastructure has had 99.9%+ uptime historically.

**Impact**: high. A Sigstore outage during a critical release window blocks publishing.

**Mitigation**:
- The bridge supports a private Sigstore-compatible signer via:
  ```toml
  [rust.publish.sigstore]
  fulcio-url = "https://fulcio.internal.example.com"
  rekor-url = "https://rekor.internal.example.com"
  ```
- Enterprise users can run their own Fulcio + Rekor instances.
- For verification, cached Rekor proofs reduce the dependency on a live Rekor query at every install.

**Residual**: public-good users during an outage cannot publish. We accept this; the outage is rare and brief, and falls back to the wider crates.io operational team to address.

## R8: Schema version churn (re-stated for completeness)

See R1.

## R9: Wrapper layer attack surface

**Risk**: the wrapper layer is auto-generated Rust code that bridges raw pointers and lifetimes. A bug in the generator could introduce a use-after-free, a double-free, or a Mochi-side type confusion.

**Likelihood**: low. The generator follows a small set of templates that are property-tested.

**Impact**: high. Memory safety violations are a security boundary failure.

**Mitigation**:
- The generator has property-tests covering 1000+ random crate-shapes (generated via cargo-fuzz against the type table).
- Miri runs on the test corpus (the wrapper code compiles under Miri's UB-detection mode).
- The wrapper crate uses `#[deny(unsafe_op_in_unsafe_fn)]` so every unsafe operation is explicitly annotated.

**Residual**: zero-day in the generator can leak. The mitigation is rapid: the bridge updates the generator and the user re-runs `mochi pkg lock`.

## R10: Imported crate license drift

**Risk**: a crate changes its license between minor versions (e.g., MIT/Apache-2.0 to AGPL). A user who locked under the old license may unknowingly redistribute under the new license.

**Likelihood**: low. Rust crates rarely change license; when they do, it is usually announced.

**Impact**: medium. License-compliance issue for downstream consumers.

**Mitigation**:
- `mochi pkg lock --check` recomputes the SPDX union and flags a drift.
- The publish-side licence-compatibility walk catches incompatibilities at publish time.
- The lockfile records the SPDX expression at the time of lock; a drift is detected against this record.

**Residual**: between releases, the user is bound by whatever SPDX expression was current at lock time. Reasonable: this is the standard semantics.

## R11: Cross-platform reproducibility

**Risk**: a Mochi-emitted Rust library crate may produce subtly different binaries on different platforms (e.g., the date format in the build metadata, the path embedded in PDB files). Downstreams that compare binary hashes across platforms fail.

**Likelihood**: medium. Cargo's reproducibility story is improving but not universal.

**Impact**: low. Most downstream consumers do not compare binary hashes across platforms.

**Mitigation**:
- The publish flow sets `SOURCE_DATE_EPOCH` to the commit timestamp.
- The .crate tarball is built deterministically (sorted file order, fixed timestamps).
- macOS-specific `LC_UUID` (see MEP-53 §R1) is skipped from the reproducibility gate.

**Residual**: the cdylib output for macOS is not byte-identical across machines because of `LC_UUID`. This is a known macOS limitation.

## R12: Future Rust edition

**Risk**: Rust ships a new edition (e.g., Rust 2027) that changes the surface in ways that affect rustdoc JSON output. The bridge's type-mapping table may not cover new shape variants.

**Likelihood**: certain on a multi-year horizon. Editions land every 3 years.

**Impact**: medium. New shapes produce more SkipReport entries until the bridge adds coverage.

**Mitigation**:
- The bridge follows the Rust release schedule and ships coverage updates with each edition.
- The user can pin the Rust edition for an imported crate via `[rust] edition = "2021"` if the crate has not yet migrated.

**Residual**: between Rust edition release and bridge coverage, the user gets SkipReport for new shapes. Reasonable.

## Alternatives considered

### A1: cbindgen as the primary surface

cbindgen generates C headers from Rust source. MEP-73 could have used cbindgen output instead of rustdoc JSON.

Why rejected:
- cbindgen processes Rust source via `syn` (the Rust source parser); it does not emit type-level information beyond the C-shape. Our bridge needs the full Rust type information (generic bounds, Option / Result shape, etc.).
- cbindgen runs on the upstream crate's source, requiring source-level access. rustdoc JSON requires only the compiled metadata.
- cbindgen's output is C, not a structured surface description. Parsing C back into a Rust type model is fragile.

Still used as a complementary tool for the publish-side cdylib header emission.

### A2: cxx-style hand-authored bridge

The cxx crate (Dropbox, used by chromium-ish projects) requires the user to declare a bridge module by hand in both Rust and C++. MEP-73 could have used a similar approach with Mochi-side declarations.

Why rejected:
- The hand-authoring requirement is exactly what we want to avoid. Our "zero boilerplate" goal is incompatible with mandatory bridge declarations.
- The cxx bridge model produces faster builds (no rustdoc JSON ingest, no Go-side synthesis), but at the cost of user-side declaration effort.

Cxx works well when the user controls both sides; our use case is "the user does not control the upstream crate", so the user cannot author a cxx bridge.

### A3: uniffi-rs (Mozilla) as the surface

uniffi generates language bindings from a UDL (Uniffi Definition Language) file. MEP-73 could have required a UDL file per imported crate.

Why rejected:
- UDL is hand-authored; same boilerplate problem as A2.
- uniffi's binding generator is a Rust-side macro that runs at the upstream crate's compile time. Imported crates do not include uniffi macros.
- The uniffi type system is its own surface (a subset of Rust + custom extensions); it does not match the Rust type system 1:1.

uniffi is great when the user controls the upstream library; our case does not match.

### A4: diplomat (Unicode Consortium) as the surface

diplomat is a Rust-to-many-languages bridge used by ICU4X. MEP-73 could have used diplomat's protocol.

Why rejected:
- diplomat requires the upstream library to be authored with diplomat-aware annotations. Imported crates do not have these.
- diplomat's language target list (Dart, JS, C++, Kotlin) does not include Mochi; adding Mochi is non-trivial.

Diplomat's design influenced MEP-73's lifetime erasure strategy.

### A5: WIT-only (Component Model) as the surface

WebAssembly Component Model with WIT (the WebAssembly Interface Type) is a candidate for cross-language bridging. MEP-73 could have required crates to be compiled to wasm components and bridged via WIT.

Why rejected:
- Most Rust crates do not target wasm. Requiring a wasm-component build breaks the "native Rust dep" promise.
- The Component Model is still stabilising as of 2026. The toolchain is fragile.
- Bridging WIT to Mochi requires a Mochi-side WIT runtime, which is itself a substantial undertaking.

WIT may become an alternative bridge for wasm-component-shaped crates in a future sub-phase, but it does not replace the native-Rust bridge.

### A6: dlopen pre-built .so artefacts from crates.io

crates.io could (hypothetically) host pre-built `.so` / `.dll` / `.dylib` artefacts per platform. Mochi could `dlopen` these and skip the Rust toolchain entirely.

Why rejected:
- crates.io does not host pre-built artefacts. Adding this would require ecosystem-wide buy-in.
- Pre-built artefacts pin the toolchain, the libc version, and the dependency versions in ways that are hard to reconcile with a user's specific build.
- The supply-chain risk increases: the pre-built artefact is not byte-for-byte verifiable against the source.

This is the cargo team's deliberate design choice; we follow it.

### A7: Full lifetime translation

The bridge could translate Rust lifetimes to a Mochi-side lifetime annotation (e.g., a `[scope=...]` attribute on Mochi references).

Why rejected:
- Mochi's surface language has no lifetime annotation. Adding one is a large language change.
- The borrow-to-clone strategy covers most cases without requiring a Mochi-side lifetime system.
- The cases where lifetime translation would help (returning a borrow tied to a parameter) are niche.

A future Mochi language with explicit lifetimes could lift this rejection.

### A8: Long-lived tokens (no Sigstore)

The publish flow could use long-lived crates.io API tokens stored in CI secrets. This is the legacy crates.io publish flow.

Why rejected:
- xz-utils backdoor (March 2024), event-stream NPM compromise (2018), and similar incidents demonstrate that long-lived tokens are the main attack vector against package registries.
- The crates.io team's RFC #3724 is the deliberate move away from tokens.
- Sigstore-keyless publishes are tied to specific CI workflows, which makes lateral movement much harder.

Long-lived tokens are not offered in v1 (only Sigstore-keyless trusted publishing). The transition flag `--allow-token-fallback` exists for the rollout period.

### A9: Multi-thread tokio default

The tokio runtime could default to multi-thread mode for parallelism out of the box.

Why rejected:
- Most Mochi-imported-Rust use cases are not parallelism-bound. The IO-bound default fits 90% of cases.
- Multi-thread mode increases the binary size by ~200 KiB and the startup cost by ~3 ms for every Mochi user.
- The opt-in is a single TOML key for the users who need it.

Default current-thread; opt-in multi-thread.

### A10: Per-build cargo

The bridge could invoke `cargo build` per imported crate (each as a separate `cargo build` process). This avoids the workspace setup complexity.

Why rejected:
- Each cargo invocation re-resolves the dependency graph, re-checks the toolchain, re-warms the registry index. The overhead is ~1 second per invocation; for 20 crates, that is 20 seconds of pure overhead.
- The workspace mode shares the registry index across crates and parallelises compilation. Build times are 3-5x faster.
- The workspace mode also enables shared generic instantiations and shared crate dependencies, reducing binary size.

Workspace mode is strictly better. Per-build cargo is reserved for `--dry-run` flows where the user wants to validate a single crate in isolation.

### A11: No published crate model (consume-only)

MEP-73 could have been consume-only: Mochi imports Rust crates but cannot publish Mochi packages as Rust crates.

Why rejected:
- The publish direction is necessary for the Mochi ecosystem to participate in crates.io distribution. Without it, downstream Rust users cannot consume Mochi libraries.
- The crates.io publish protocol is well-defined and matches the existing MEP-53 publish-dry-run gate; the marginal effort to ship both directions is small.

Both directions are in v1.

### A12: Manual ABI definition file

The bridge could require a per-crate `mochi-bindings.toml` file declaring the surface explicitly (instead of automatic ingest).

Why rejected:
- Hand-authored bindings re-introduce the boilerplate problem.
- The 24-crate fixture corpus shows that automatic ingest covers 70-85% of items per crate; the long tail is covered by per-item `[[rust.extern]]` overrides.

Manual bindings exist as a per-item override, not as the primary mode.

## Cross-references

- [[02-design-philosophy]] for the load-bearing decisions that drove these choices.
- [[03-prior-art-bridges]] for the broader landscape of language bridges.
- [[07-sigstore-cargo-rfc3724]] for the Sigstore-keyless rationale.
- [MEP-73 §Alternatives](/docs/mep/mep-0073#alternatives) for the normative alternatives list.
- [MEP-73 §Risks](/docs/mep/mep-0073#risks) for the normative risk register.
