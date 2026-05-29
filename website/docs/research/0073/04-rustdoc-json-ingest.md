---
title: "04. Rustdoc JSON ingest"
sidebar_position: 5
sidebar_label: "04. Rustdoc ingest"
description: "The rustdoc-types schema, the ItemEnum and Type discriminators, the stability story (nightly-only as of May 2026, tracking rust-lang/rust #76578), the rustdoc-types versioning model, the Go-side parser strategy under package3/rust/rustdoc/, and the per-crate ingest fixtures."
---

# 04. Rustdoc JSON ingest

This note documents how MEP-73's ingest pipeline turns a Rust crate into a machine-readable surface description. The pipeline runs at `mochi pkg lock` time, not at build time.

## The rustdoc JSON output

Running `cargo +nightly rustdoc -- --output-format=json` (or, equivalently, `cargo +nightly doc --output-format=json`) produces a single JSON document per crate at `target/doc/<crate>.json`. The document conforms to the schema published by the `rustdoc-types` crate (rust-lang/rust-lang.github.io maintains the JSON Schema rendering at `https://rust-lang.github.io/rustdoc-types/`).

The document is a single JSON object:

```json
{
  "root": "0:0",
  "crate_version": "1.42.0",
  "includes_private": false,
  "index": {
    "0:0": { "name": "tokio", "inner": { "Module": { ... } }, "visibility": "Public", "id": "0:0", ... },
    "0:1": { "name": "spawn", "inner": { "Function": { ... } }, "visibility": "Public", "id": "0:1", ... },
    ...
  },
  "paths": {
    "0:0": { "kind": "Module", "path": ["tokio"], "crate_id": 0 },
    "0:1": { "kind": "Function", "path": ["tokio", "spawn"], "crate_id": 0 },
    ...
  },
  "external_crates": { ... },
  "format_version": 39
}
```

`format_version` is the rustdoc-types schema version. Schema version 39 is the May 2026 nightly. The bridge pins the supported schema version per release.

## The ItemEnum discriminator

Every entry in `index` has an `inner` field whose value is a single-variant JSON object discriminated by ItemKind. The variants relevant to MEP-73 are:

- `Function`: a free function or a method. Carries a `decl` field with parameter and return types, a `generics` field with type parameters and bounds, an optional `header.async_` flag, and an optional `header.unsafe_` flag.
- `Struct`: a struct definition. Carries a `kind` field (Plain / Tuple / Unit), a `fields` array of nested IDs (each pointing to a StructField entry), and a `generics` field.
- `StructField`: a field of a struct. Carries a `type` field of kind Type.
- `Enum`: an enum definition. Carries a `variants` array of nested IDs.
- `Variant`: a variant of an enum. Carries a `kind` field (Plain / Tuple / Struct) plus optional payload.
- `Trait`: a trait definition. Carries `items` (associated items) and `bounds`.
- `Impl`: an `impl` block. Carries `for_` (the type being implemented for), `trait_` (the trait being implemented, if any), and `items`.
- `Module`: a module. Carries `items` (an array of nested IDs).
- `Constant`: a `const` item. Carries `type_` and `expr`.
- `Static`: a `static` item.
- `TypeAlias`: a `type T = ...` alias.
- `Macro`: a `macro_rules!` definition.
- `ProcMacro`: a proc-macro (function-like, derive, or attribute).
- `Use`: a `use` re-export.

MEP-73 v1 supports Function, Struct (Plain only), StructField, Enum, Variant, TypeAlias, and Module ingest. Trait, Impl, Constant, Static, Macro, ProcMacro, and Use are skipped with `SkipReport` entries. A future sub-phase can add Trait dispatch (via dyn Trait opaque handles) and Constant binding.

## The Type discriminator

Type signatures are nested JSON objects discriminated by Type kind. The variants the closed table covers:

- `Primitive`: `i8` / `i16` / `i32` / `i64` / `u8` / `u16` / `u32` / `u64` / `f32` / `f64` / `bool` / `char` / `str` / `unit`.
- `ResolvedPath`: a path to a named type (`String`, `Vec<T>`, `HashMap<K, V>`, `Option<T>`, `Result<T, E>`, user struct, user enum). Carries `name` (`String`), `id` (the entry ID it resolves to in `index`), and `args` (a `GenericArgs` containing the type arguments).
- `Tuple`: a tuple type. Carries an array of nested Type values.
- `Slice`: `[T]`. Carries the element Type.
- `Array`: `[T; N]`. Carries the element Type and the length expression.
- `BorrowedRef`: `&T` or `&mut T`. Carries the lifetime, the `mutable` flag, and the referent Type.
- `FunctionPointer`: an `fn` pointer. Carries the parameter and return types.
- `Generic`: a generic type parameter reference (e.g., `T` in `fn foo<T>(x: T)`).

Variants the closed table refuses:

- `RawPointer`: `*const T`, `*mut T`. Requires user-side `extern fn ... unsafe` override.
- `ImplTrait`: `impl Trait` in return position. Requires explicit `[rust.monomorphise]` instantiation.
- `DynTrait`: `dyn Trait`. v1 has no opaque-trait-object surface.
- `Pat`: pattern types (a 2024+ unstable feature).
- `Infer`: inferred type (`_`). Should not appear in rustdoc output for public items.
- `QualifiedPath`: `<T as Trait>::Item`. Requires trait dispatch which v1 lacks.

Each refusal becomes a `SkipReport` entry naming the item and the offending Type variant.

## The Go-side parser

The bridge's `package3/rust/rustdoc/` package implements a Go-side parser:

```go
package rustdoc

type Document struct {
    Root         string `json:"root"`
    Index        map[string]Item `json:"index"`
    Paths        map[string]PathInfo `json:"paths"`
    FormatVersion int `json:"format_version"`
}

type Item struct {
    ID         string   `json:"id"`
    Name       string   `json:"name"`
    Visibility string   `json:"visibility"`
    Inner      ItemKind `json:"inner"`
}

type ItemKind struct {
    Function *FunctionItem `json:"Function,omitempty"`
    Struct   *StructItem   `json:"Struct,omitempty"`
    Enum     *EnumItem     `json:"Enum,omitempty"`
    ...
}

type FunctionItem struct {
    Decl     FunctionDecl `json:"decl"`
    Generics Generics     `json:"generics"`
    Header   FunctionHeader `json:"header"`
}

type FunctionDecl struct {
    Inputs []FunctionInput `json:"inputs"`
    Output *Type           `json:"output,omitempty"`
}
```

The parser unmarshals the document and constructs a normalised `ApiSurface` value:

```go
type ApiSurface struct {
    CrateName    string
    CrateVersion string
    FormatVersion int
    Functions    []FunctionSig
    Structs      []StructDef
    Enums        []EnumDef
    TypeAliases  []TypeAlias
    Skipped      []SkipReport
}
```

Walking the document is straightforward: a recursive descent from `Root`, following `Module.items` references, with cycle detection via the seen-ID set. The walk respects `visibility`: only `Public` items reach the surface.

## Schema version compatibility

The `rustdoc-types` crate publishes a new major version roughly every 2-3 months as new ItemKind / Type variants stabilise. The schema version at `format_version` in the document is the integer the bridge pins per release.

The bridge maintains a compatibility table:

| Bridge version | Supported format_version | Nightly date range |
|----------------|--------------------------|---------------------|
| 0.1.x | 35-39 | 2025-12 to 2026-06 (estimated) |
| 0.2.x | 40+ | 2026-06 onwards |

A document with an unsupported `format_version` produces a hard error at lock time: `mochi pkg lock` fails with `rustdoc format_version=42 not supported; downgrade nightly to 2026-06-15 or upgrade mochi`.

The user can pin the nightly toolchain via:

```toml
[rust]
nightly = "2026-06-15"
```

The bridge passes `--toolchain=nightly-2026-06-15` to cargo invocations.

## Stability story

As of May 2026, `--output-format=json` is behind `-Z unstable-options` on nightly. Stabilisation tracking issue: rust-lang/rust #76578. The rust-lang/rustdoc team has communicated a stabilisation intent for the 1.85-1.95 window, but the actual release date is unconfirmed.

MEP-73 ships with a nightly requirement at lock time. The user's normal `cargo build --release` (orchestrated by MEP-53) runs on stable. The nightly is needed only at lock time to produce the JSON output; it is not needed at build time.

A `--rust-nightly=auto` mode (default) auto-installs the supported nightly via `rustup install nightly-<date>` if not present. A `--rust-nightly=strict` mode fails if the supported nightly is not present (for hermetic CI environments).

When stabilisation lands (expected 2026-2027), the bridge will accept stable toolchains and `--rust-nightly` becomes a no-op.

## Ingest fixtures

The bridge's test corpus draws from the curated 24-crate fixture set (the April 2026 top-25-most-downloaded-on-crates.io snapshot, sans the long-deprecated `lazy_static`): anyhow, thiserror, serde, regex, rayon, itertools, once_cell, time, uuid, url, base64, hex, sha2, blake3, rand, rand_chacha, num_cpus, bytes, smallvec, indexmap, ahash, parking_lot, crossbeam, tokio.

For each crate, the test:

1. Materialises the crate at a known version from the content-addressed cache.
2. Runs `cargo +nightly rustdoc --output-format=json` against the materialised tree.
3. Parses the JSON via the Go-side `rustdoc` package.
4. Asserts that the parsed `ApiSurface` contains the expected number of public Function, Struct, Enum, TypeAlias entries (golden numbers checked into the test).
5. Asserts that the `Skipped` list contains the expected items (golden list).

The fixture set is regenerated quarterly to track crate API changes; the golden numbers are stored in `tests/package3/rust/rustdoc/<crate>-<version>.golden.json`.

## Cross-references

- [[01-language-surface]] for the user-visible surface this ingest feeds.
- [[02-design-philosophy]] §2 for the rejection of alternative ingest sources.
- [[05-type-mapping]] for what the bridge does with the parsed surface.
- [[09-abi-stability]] for the wrapper layer the surface drives.
- [MEP-73 §1](/docs/mep/mep-0073#1-pipeline-overview) for the normative pipeline.
