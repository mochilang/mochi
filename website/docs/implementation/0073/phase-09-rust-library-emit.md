---
sidebar_label: "Phase 9: TargetRustLibrary emit"
sidebar_position: 10
---

# MEP-73 Phase 9: TargetRustLibrary emit

**Status:** LANDED (2026-05-29)
**Spec section:** [MEP-73 §3 Direction 2 — Rust library emit](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p9`

## Gate

Land the publish-direction emit pipeline: lower a Mochi package's
public surface (the things its `pub fn` / `pub struct` / `pub enum`
declarations expose) into a publishable Rust library crate whose
`Cargo.toml` declares `crate-type = ["rlib", "cdylib"]` and which
optionally ships a cbindgen-compatible C header so non-Rust consumers
can link against the cdylib.

## Why it matters

Phases 0-8 owned the consume direction (`import rust` brings a crates.io
crate into a Mochi program). Phase 9 opens the producer direction:
`mochi pkg publish --to=crates.io` ships a Mochi package outward as a
first-class Rust crate. Without this gate, Mochi code is a consumer of
the Rust ecosystem but not a contributor to it.

The lowered crate has three artefacts:

1. **`Cargo.toml`** — `[package]` metadata + `[lib]` configuration
   pinning both `rlib` (so downstream Cargo users see `extern crate
   <mochi_crate>;` semantics) and `cdylib` (so the same artefact
   links into C / C++ / Mochi / Python via FFI).
2. **`src/lib.rs`** — public Rust source rendered from the Mochi
   package's public surface. `pub fn` items lower to `pub fn` (or
   `#[no_mangle] pub extern "C" fn` when the surface is exported with
   the C ABI). `pub struct` / `pub enum` items lower to `pub struct` /
   `pub enum` with optional `#[repr(C)]` and `#[derive(...)]`
   attributes.
3. **`include/<crate>.h`** — optional cbindgen-style header listing
   every repr(C) type and every `extern "C"` function declaration,
   wrapped in the standard `#ifdef __cplusplus` extern-C block. Only
   emitted when `CHeader = true`.

The emit pipeline is byte-stable: given the same `PublicAPI` input,
`Render` returns byte-identical files. This is the contract the
publish flow (Phase 10) needs to produce reproducible source tarballs
that Sigstore-keyless OIDC trusted-publishing signs.

## What landed

### `package3/rust/library/library.go`

The core types and `Render` entry point.

- `PublicAPI{CrateName, Version, Items, Package, Dependencies,
  CHeader, NoStd}` — closed input shape callers construct from their
  IR state.
- `PackageMeta{Description, License, Repository, Documentation,
  Homepage, Keywords, Categories, Authors, Readme, Edition,
  RustVersion}` — the publish-side metadata block matching crates.io's
  schema.
- `Item` interface (sealed) with three concrete types:
  - `ItemFn{Name, Params, Return, Body, Extern, Doc}`
  - `ItemStruct{Name, Fields, ReprC, Derives, Doc}`
  - `ItemEnum{Name, Variants, ReprC, Derives, Doc}`
- `Render(api PublicAPI) (Files, error)` — validates structure
  (crate-name shape, duplicate item names, body presence on
  non-extern fns) and dispatches to the three renderers.
- `validCrateName` enforces the crates.io naming rule: ASCII letter
  start, followed by letters / digits / `_` / `-`.

### `package3/rust/library/manifest.go`

The `Cargo.toml` renderer.

- Byte-stable hand-rolled TOML (no third-party dep) matching the same
  style as Phase 7's `workspace.go`.
- `[package]` rows in fixed order: name, version, edition,
  rust-version, description, license, repository, documentation,
  homepage, readme, authors, keywords, categories.
- `[lib]` block always renders `crate-type = ["rlib", "cdylib"]`.
- `[dependencies]` block sorts by crate name; rows render as
  `name = "version-req"`.
- Empty metadata fields are omitted (no `key = ""` rows).
- Default edition is `2021`.

### `package3/rust/library/lib_rs.go`

The `src/lib.rs` renderer.

- Optional `#![no_std]` preamble + `extern crate alloc;` when
  `NoStd = true`.
- One Rust item per `Item` in declaration order, separated by blank
  lines.
- `ItemFn` renders to `pub fn` (default) or `#[no_mangle] pub extern
  "C" fn` (when `Extern`). Unit return `()` is elided in the signature.
- `ItemStruct` renders `#[repr(C)]` / `#[derive(...)]` attributes when
  set, followed by `pub struct Name { ... fields ... }`. Field-level
  `Pub bool` controls per-field visibility.
- `ItemEnum` renders attributes followed by `pub enum Name { ... }`;
  unit variants render as bare names, struct-style variants render as
  `Name { field: Type, ... }`.
- `///` doc-comments render preceding the item / field / variant.

### `package3/rust/library/cheader.go`

The cbindgen-compatible C header renderer.

- Standard layout: `#ifndef <CRATE>_H` / `#define` / `#include
  <stdint.h>` / `#include <stddef.h>` / `extern "C" {` wrapper / body
  / closing.
- Only `Extern` functions and `ReprC` types are emitted; non-extern
  fns and non-repr-C types are silently skipped (they are not
  ABI-stable, so they don't belong in a C header).
- `rustTypeToC` projects the closed set of repr(C)-safe Rust types
  into canonical C spellings: `i64` → `int64_t`, `f64` → `double`,
  `*const c_char` → `const char*`, `*mut T` → `<T_in_C>*`, and so on.
  Unknown types render verbatim (escape hatch for opaque-handle
  typedef names callers register at the Rust level).
- Tagged-union enums (variants with payloads) render a deferral
  comment pointing at Phase 12 — they require the
  monomorphisation-style tagged-union projection cbindgen produces.

### Tests

- `library_test.go` — 14 cases covering Render shape, validation,
  determinism, Files.Sorted, RenderError formatting, name helpers.
- `manifest_test.go` — 7 cases covering basic shape, custom edition,
  empty-metadata omission, full metadata, dep sorting, omitted
  `[dependencies]` section, `[lib]` block layout.
- `lib_rs_test.go` — 11 cases covering plain fn, extern fn, void
  return elision, struct render, unit-only and struct-variant enums,
  no_std preamble, doc comments, multi-item separation, body trimming,
  private fields.
- `cheader_test.go` — 13 cases covering include guard, stdint
  includes, extern-C wrap, struct / enum / fn renderers, skipping
  non-emit items, void / void params, the rustTypeToC table, and
  hyphenated crate-name handling.
- `phase09_test.go` (sentinel) with subtests:
  - `publishable_crate_shape`: full Cargo.toml + lib.rs + header
    end-to-end for a Point-distance example.
  - `determinism`: two Render calls produce byte-identical output.
  - `nostd_subset`: `NoStd = true` emits `#![no_std]` + `extern crate
    alloc`.
  - `c_header_omitted_by_default`: `CHeader = false` produces no
    `include/...` file.

## Target matrix

| Target           | Status   | Notes |
|------------------|----------|-------|
| Schema           | ✅       | PublicAPI / PackageMeta / Item closed shapes. |
| Cargo.toml       | ✅       | Deterministic, `crate-type = ["rlib", "cdylib"]`. |
| src/lib.rs       | ✅       | pub fn / pub struct / pub enum + `#[repr(C)]` + `#[derive]`. |
| Extern "C" fns   | ✅       | `#[no_mangle] pub extern "C" fn` + matching C decl. |
| cbindgen header  | ✅       | Include guard + stdint + extern-C wrap; unit-enum + repr(C) struct supported. |
| no_std subset    | ✅       | `#![no_std]` + `extern crate alloc;` preamble. |

## How this phase plugs in to the larger pipeline

```
  Mochi public surface                               Phase 9 emit
  (caller's IR)
                                                  ┌─────────────────────────┐
  pub fn ...      ───┐                            │ library.PublicAPI       │
  pub struct ...     │  caller projects via       │   CrateName, Version,   │
  pub enum ...       │  their IR walker           │   Items[...],           │
  pub type ...       │                            │   Package{...},         │
                     │                            │   Dependencies{...}     │
                     ▼                            └────────────┬────────────┘
                                                               │
                                                  library.Render(api)
                                                               │
                                              ┌────────────────┴────────────────┐
                                              ▼                                 ▼
                                       Cargo.toml                          src/lib.rs
                                       [lib] crate-type=                   pub fn / struct /
                                         ["rlib","cdylib"]                 enum (+repr(C))
                                                                              │
                                                                  optional: include/<crate>.h
```

The next phase (10. trusted publishing) consumes this output: writes
the rendered files to a source tarball, runs `cargo package`, and
ships through Sigstore-keyless OIDC to crates.io. Phase 9 is the
upstream half of that pipeline; the contract between phase 9 and
phase 10 is exactly the `Files` map `Render` returns.

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-29 23:35    | Worktree branch `mep/0073-phase-09` created off `origin/main`. |
| 2026-05-29 23:48    | `library.go`, `manifest.go`, `lib_rs.go`, `cheader.go` written. |
| 2026-05-29 23:50    | Test suite green (`go test ./package3/rust/...`). |
| 2026-05-29 23:53    | Tracking page + spec sync. |
