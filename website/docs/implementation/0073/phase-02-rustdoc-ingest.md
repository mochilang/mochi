---
title: "Phase 2. Rustdoc-JSON ingest"
sidebar_position: 4
sidebar_label: "Phase 2. Rustdoc-JSON ingest"
description: "MEP-73 Phase 2 lands the package3/rust/rustdoc/ Go parser: rustdoc-types schema (format_version 37-39), Document / Item / Type discriminated unions, walker that emits an ApiSurface with public Function / Struct / Enum / TypeAlias / Constant / Trait entries plus SkipReports for everything else."
---

# Phase 2. Rustdoc-JSON ingest

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:08 (GMT+7) |
| Landed         | 2026-05-29 21:17 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`package3/rust/rustdoc/` parses a rustdoc-json document produced by `cargo +nightly rustdoc --output-format=json` against the `rustdoc-types` schema (format_version 37-39, the May 2026 nightly range), walks the public-item tree from the crate root, and emits an `ApiSurface` carrying flat per-kind slices of `FunctionEntry`, `StructEntry`, `EnumEntry`, `TypeAliasEntry`, `ConstantEntry`, and `TraitEntry`, plus a `Skipped []errors.SkipReport` slice that classifies every refusal.

A hand-written fixture modelled after the `hex` crate (one of the 24 MEP-73 fixture crates) exercises the walker end to end. The fixture asserts:

- 2 functions (`encode`, `decode`) lifted into the surface.
- 1 unit struct (`FromHexError`).
- 1 enum (`FromHexErrorKind`) with three variants of three different kinds (plain / tuple / struct).
- 1 type alias (`Bytes`).
- 1 constant (`MAX_LENGTH`).
- 1 trait (`FromHex`) recorded with a `SkipTrait` skip.
- 1 `SkipGeneric` for a generic fn (`decode_to_slice<T>`).
- 1 `SkipExternFnUnsafe` for an `unsafe fn` (`as_str_unchecked`).
- A non-public helper fn (`internal_helper`, visibility="default") must NOT appear in the surface.

41 test functions across `parse_test.go`, `type_test.go`, and `walk_test.go` cover the schema and walker. Total package count after phase 2: 5 (errors, build, semver, sparse, rustdoc).

## Lowering decisions

### Schema-versioning policy

The bridge accepts `format_version` 37, 38, and 39. Versions outside the range fail at `Parse` time with `*ErrUnsupportedFormatVersion` (`errors.As`-friendly), so a nightly-toolchain bump that produces a newer schema is caught at lock time rather than silently producing wrong output. Cargo's rustdoc team bumps the schema roughly every 2-3 months as new `ItemEnum` / `Type` variants stabilise; the bridge release notes add the new version to `SupportedFormatVersions` only after the fixture corpus has been re-verified.

The choice of 37 as the lower bound matches the rust-1.85 nightly (Q1 2026), the same window MEP-73 §"Schema version compatibility" pins. Versions 36 and earlier renamed `function` to `fn` and `type_alias` to `typedef`, so backwards compatibility would require synonym handling. The bridge instead asks the user to upgrade their nightly toolchain (the `--rust-nightly=auto` mode in Phase 7 will install the supported nightly automatically).

### Discriminated-union shape

The `rustdoc-types` `ItemEnum` and `Type` are tagged unions of the shape `{"tag": payload}`. The Go parser models each as a single struct with one pointer field per variant (`Module *ModuleItem`, `Function *FunctionItem`, etc.) and a custom `UnmarshalJSON` that dispatches on the (alphabetically-first) tag key. Unknown variants do not error: they populate an `Unknown string` field so the walker can produce a precise `SkipReport` instead of failing the parse. This matches the design philosophy in research note 02 §3 ("Refuse, don't approximate") and §6 ("Schema drift must surface as a SkipReport, not a crash").

The same shape covers the `Type` variants: `resolved_path`, `dyn_trait`, `generic`, `primitive`, `function_pointer`, `tuple`, `slice`, `array`, `pat`, `impl_trait`, `infer`, `raw_pointer`, `borrowed_ref`, `qualified_path`, plus an `Unknown` catch-all. The 20-variant SkipReason enum from Phase 0 already covers every refusal class the walker can produce.

### Visibility handling

`Visibility` is a sum of `public`, `default` (crate-private), `crate` (the `pub(crate)` legacy form), and `restricted` (`pub(in path)`). The wire format is either a bare string `"public"`/`"default"` or a tagged object `{"crate": "..."}`/`{"restricted": {...}}`. The Go type carries a `Kind` enum + a `Path` field for the restricted/crate forms. The walker uses `IsPublic()` as the gating filter; only public items reach the surface (with the one exception that modules are walked regardless of their own visibility, because a `pub` item nested in a `pub(crate)` module still needs to be discoverable via re-export).

### Function input shape

Rustdoc emits function parameters as a 2-tuple `["name", Type]` rather than an object. The `FunctionInput` type has a custom `UnmarshalJSON` that splits the array and populates `Name` and `Type`. The test suite pins this with both the happy path (`["bytes", {"primitive":"u8"}]`) and the wrong-shape (object) negative case, so a future schema change to an object form will surface immediately.

### Walker classification

The walker classifies each public item by `Inner.Kind()`:

- `module`: recurse into `Module.Items`.
- `function`: emit unless generic, unsafe, or non-standard ABI; otherwise emit a `SkipReport` with the matching reason (`SkipGeneric`, `SkipExternFnUnsafe`, `SkipCustomAbi`).
- `struct`: emit unless generic; otherwise `SkipGeneric`. Plain / tuple / unit structs are distinguished via the `StructKind` discriminator.
- `enum`: emit unless generic. Variants are collected with their `plain`/`tuple`/`struct` shape preserved.
- `type_alias`: emit unless generic.
- `constant`: emit always (constants don't have generic params at the alias level).
- `trait`: record a `TraitEntry` but also emit a `SkipTrait` skip, because the v1 bridge does not generate trait bindings (concrete impls land in Phase 4).
- `use`: follow the re-export's target ID transparently.
- `extern_crate`, `primitive`, `static`, `macro`, `proc_macro`, `impl`, `assoc_const`, `assoc_type`, `variant`, `struct_field`: emit a skip.

Generic items always skip in v1; Phase 12 (monomorphisation) will later enable concrete instantiations declared in `mochi.toml`. Unsafe fns and non-`Rust`/`C` ABIs always skip because the bridge requires explicit capability opt-in.

### Determinism

The walker visits items in `Module.Items` order (rustdoc emits a deterministic order). The `seen` map prevents revisiting on a cycle; `SortSkipped()` provides a stable lexicographic order for golden-file diffs in later phases.

## Files changed

| File | Purpose |
|------|---------|
| `package3/rust/rustdoc/document.go` | `Document`, `Item`, `Visibility`, `PathInfo`, `Span`, `Deprecation`, `ExternalCrate`, `TargetInfo` |
| `package3/rust/rustdoc/item.go` | `ItemEnum` with 18 variants plus `Unknown`, payload types for each variant, `FunctionInput` custom unmarshal, `ABI` custom unmarshal |
| `package3/rust/rustdoc/type.go` | `Type` with 14 variants plus `Unknown`, `Generics`, `GenericParamDef`, `GenericArgs`, `GenericBound`, `WherePredicate`, `PolyTrait`, `QualifiedPathType`, `BorrowedRefType`, `RawPointerType`, `ArrayType`, `ConstValue` |
| `package3/rust/rustdoc/parse.go` | `Parse(io.Reader)`, `ParseBytes([]byte)`, `SupportedFormatVersions`, `ErrUnsupportedFormatVersion` |
| `package3/rust/rustdoc/surface.go` | `ApiSurface`, `FunctionEntry`, `StructEntry`, `EnumEntry`, `TypeAliasEntry`, `ConstantEntry`, `TraitEntry`, `FieldEntry`, `VariantEntry`, `ParamEntry`, internal `skip` helper |
| `package3/rust/rustdoc/walk.go` | `Walk(*Document) (*ApiSurface, error)`, internal walker state, per-kind emitters, `Snapshot` + `SortSkipped` |
| `package3/rust/rustdoc/parse_test.go` | minimal-doc parse, version-range rejection, visibility variants |
| `package3/rust/rustdoc/type_test.go` | 13 Type-variant unmarshal cases including `Unknown` catch-all, ABI variants, FunctionInput shape |
| `package3/rust/rustdoc/walk_test.go` | hex-like fixture: counts, skip reasons, function encode shape, enum variant kinds, path rendering, non-public exclusion, snapshot, sort determinism, nil/missing-root errors |

## Test set

- All `package3/rust/rustdoc/...` unit tests (41 functions).

Total: 41 test functions, all green via `go test ./package3/rust/rustdoc/...`.

## Closeout notes

Phase 2 introduces no new external runtime dependencies; only `encoding/json` from the stdlib plus the existing `mochi/package3/rust/errors` package. The schema-version compatibility range (37-39) is hard-coded as constants so a release-note review must accompany any change.

The `hex`-like fixture is a hand-written document, not a captured `cargo rustdoc` output, because Phase 1 (sparse-index client) has not yet been wired to a sparse-index fetch path in CI. Phase 7 will add a CI smoke test that runs `cargo +nightly rustdoc` against a real fixture crate and feeds the output through `Parse + Walk`; until then, the hand-written fixture pins the parser shape and the walker behaviour against the spec's documented variant set.

The `Unknown` catch-all on both `ItemEnum` and `Type` is intentional. When the May 2027 nightly adds a new variant (rust-lang/rust adds 1-2 per cycle), the bridge will not crash, will not silently drop the affected item, and will produce a `SkipReport` with `Reason=SkipUnknown` plus a `Detail` string naming the unknown tag. The user gets a clear "this rustdoc-types tag is not modelled yet" diagnostic and a directive to upgrade Mochi or add a bridge release that ranges over the new version.

Phase 3 (closed type-mapping table) consumes the `ApiSurface`'s `Type` values and emits Mochi `extern type` declarations; the `Type.Kind()` discriminator and the per-variant payload structs are the contract Phase 3 builds against.
