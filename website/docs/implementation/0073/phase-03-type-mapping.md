---
title: "Phase 3. Closed type-mapping table"
sidebar_position: 5
sidebar_label: "Phase 3. Type-mapping table"
description: "MEP-73 Phase 3 lands the package3/rust/typemap/ closed table: scalars, strings, collections, Option, Result, Tuple, Struct, Enum mappings between rustdoc.Type and Mochi types plus an explicit SkipReason for every refusal class."
---

# Phase 3. Closed type-mapping table

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:23 (GMT+7) |
| Landed         | 2026-05-29 21:30 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`package3/rust/typemap/` exposes a single `Map(t rustdoc.Type, dir Direction) (*Mapping, errors.SkipReason, string)` entry point. The function returns a structured `Mapping` describing the Mochi-side type and its FFI representation when a closed-table rule applies, or an `errors.SkipReason` plus a free-text detail when none exists. The contract is "closed": every rustdoc-types `Type` variant either has a documented rule or a documented refusal reason. There is no silent fallthrough that emits an approximated mapping.

The 53 test functions across `kind_test.go`, `mapping_test.go`, and `map_test.go` pin the closed table. The full `package3/rust/...` suite (6 packages: errors, build, semver, sparse, rustdoc, typemap) is green.

## Closed-table rules

### Primitives

| Rust | Mochi `Kind` | Mochi rendering | FFI repr |
|------|--------------|-----------------|----------|
| `bool` | `KindBool` | `bool` | `bool` |
| `i8`, `i16`, `i32` | `KindInt` | `int` | `int32_t` |
| `i64`, `i128`, `isize` | `KindInt64` | `int64` | `int64_t` |
| `u8` | `KindByte` | `byte` | `uint8_t` |
| `u16`, `u32` | `KindUInt` | `uint` | `uint32_t` |
| `u64`, `u128`, `usize` | `KindUInt64` | `uint64` | `uint64_t` |
| `f32` | `KindFloat` | `float` | `float` |
| `f64` | `KindFloat64` | `float64` | `double` |
| `char` | `KindChar` | `char` | `uint32_t` (codepoint) |
| `str` | `KindString` | `string` | `MochiString` |
| `()`, `never`, `!` | `KindUnit` | `unit` | `void` |

The `i128`/`u128` folding into 64-bit kinds is provisional, gated by a Phase 4 wrapper rule that emits a guard rejecting values outside `[i64::MIN, i64::MAX]` (resp. `u64`). Phase 4 may promote `i128`/`u128` to a `MochiBigInt` slot if the polish lands; this phase only commits to the lower-bound mapping so the table is closed.

### Strings

`std::string::String`, `alloc::string::String`, and any `&str` (immutable borrow of the `str` primitive) all map to `KindString` with `FFIRepr = "MochiString"`. The bridge always copies the bytes; nothing crosses the FFI as a borrow.

### Collections

| Rust | Mochi |
|------|-------|
| `Vec<u8>`, `&[u8]`, `[u8; N]` | `bytes` |
| `Vec<T>`, `&[T]`, `[T; N]` | `list[T]` |
| `HashMap<K, V>`, `BTreeMap<K, V>` | `map[K]V` |
| `HashSet<T>`, `BTreeSet<T>` | `list[T]` (Mochi has no set primitive) |

Path matching strips the `std::`, `alloc::`, `core::` qualifiers via `LastSegment`, so a `Vec<i64>` referenced as `std::vec::Vec<i64>` or `alloc::vec::Vec<i64>` produces the same `KindList`. The `Vec<u8>` and `[u8]` specialisations to `bytes` are detected after the element mapping resolves to `KindByte`; this keeps the rule local to `mapPath` / `mapSlice` / `mapArray` rather than a separate path table.

### Algebraic types

- `Option<T>` (any `::option::Option` path) → `KindOption` with `Elem = T`.
- `Result<T, E>` → `KindResult` with `OK = T`, `Err = E`.
- `(T1, T2, ...)` → `KindTuple` with `Fields = [T1, T2, ...]`.
- `()` (empty tuple) → `KindUnit`. The rustdoc encoding `{"tuple": []}` is detected by checking for a non-nil empty `Tuple` slice before the main dispatch, because `Type.Kind()` reports the empty-slice form as `"empty"`.

### Smart pointers

`Box<T>`, `Rc<T>`, and `Arc<T>` are transparent: their inner `T` is the result of `Map`. The wrapper layer (Phase 4) decides whether to clone (`Rc`/`Arc`) or deref-move (`Box`) at the FFI boundary; the surface mapping only sees the inner type.

### User-defined paths

Any `resolved_path` that does not match a known std container falls through to `KindStruct` with the `PathID` and `PathName` filled from the rustdoc `PathType`. Phase 4 (wrapper synth) resolves whether the path points at a struct, enum, or opaque type via the `ApiSurface.Structs` / `Enums` slices captured in Phase 2.

### Borrowed references

| Form | Direction `In` | Direction `Out` |
|------|----------------|-----------------|
| `&T` | recurse into `T` | recurse into `T` if lifetime is `'static`, else `SkipLifetime` |
| `&'static T` | recurse into `T` | recurse into `T` |
| `&mut T` | `SkipUnknown` (no v1 mapping) | `SkipUnknown` |

The `&mut T` refusal uses `SkipUnknown` because the enum has no dedicated `SkipMutBorrow` variant; the detail string carries the explanation. A future enum bump will promote this to a named reason without changing the rule.

### Refusal classes

| rustdoc kind | Reason | Detail |
|--------------|--------|--------|
| `raw_pointer` | `SkipRawPointer` | requires unsafe capability opt-in |
| `generic` | `SkipGeneric` | unresolved generic; declare under `[rust.monomorphise]` |
| `dyn_trait` | `SkipDynTrait` | dyn Trait has no Mochi surface |
| `impl_trait` | `SkipImplTrait` | impl Trait return position requires explicit monomorphisation |
| `qualified_path` | `SkipQualifiedPath` | `<T as Trait>::Item` not mappable |
| `function_pointer` | `SkipUnknown` | function pointer types are not mapped in v1 |
| `infer` | `SkipUnknown` | inference placeholder forbidden in public sig |
| `pat` | `SkipUnknown` | pattern types are unstable rustc feature |
| `Cow<T>` | `SkipCow` | not directly mappable; pass owned type |
| `OsString`/`OsStr`/`PathBuf`/`Path`/`CString`/`CStr` | `SkipOsString` | platform-specific encoding |
| `Pin<T>` | `SkipPin` | requires custom lifetime contracts |
| unknown rustdoc-types variant | `SkipUnknown` | detail names the variant tag |

### Composition propagation

Skip propagation is bottom-up: `Vec<dyn Trait>` returns `SkipDynTrait` from the inner mapping; `(i32, *const u8)` returns `SkipRawPointer`. Top-level callers see the most specific reason from the deepest unmappable subterm. The detail string is prefixed with `"tuple field N: "` etc. so the error surface tells the user which subterm failed.

## Files changed

| File | Purpose |
|------|---------|
| `package3/rust/typemap/kind.go` | `Kind` enum (21 variants + `KindInvalid`), `Direction` enum + `String()` methods |
| `package3/rust/typemap/mapping.go` | `Mapping` struct, `MochiType()`, `FFIRepr()`, `IsScalar()` |
| `package3/rust/typemap/map.go` | `Map(t, dir)` dispatch, primitive table, path table, `LastSegment`, all sub-maps |
| `package3/rust/typemap/kind_test.go` | `Kind` and `Direction` rendering |
| `package3/rust/typemap/mapping_test.go` | `MochiType` / `FFIRepr` / `IsScalar` coverage for every Kind |
| `package3/rust/typemap/map_test.go` | 42 dispatch tests covering every rule plus refusal classes and skip propagation |
| `website/docs/implementation/0073/phase-03-type-mapping.md` | this page |

## Test set

- All `package3/rust/typemap/...` unit tests (53 functions: 2 in kind_test, 9 in mapping_test, 42 in map_test).
- Full `go test ./package3/rust/...` regression across all 6 packages.

## Lowering decisions

### Why u8 maps to byte and not int

The Mochi byte type is a distinct primitive used by I/O, hashing, and binary protocols. Mapping every `u8` to `int` would erase semantics at the surface: callers passing `Vec<u8>` would lose the `bytes` literal form. The byte mapping survives into the `Vec<u8> → bytes` and `&[u8] → bytes` specialisations, which the wrapper layer relies on to skip the per-element FFI marshalling cost.

### Why Box, Rc, Arc are transparent at the surface

Smart pointers carry no value-type information at the Mochi surface; they only affect ownership and refcount handling, both of which are FFI-layer concerns. Folding them into the inner type lets Mochi callers see the underlying value type while the wrapper layer (Phase 4) emits the correct clone/move on each crossing. This matches research note 05 §"Smart pointer folding" and avoids exposing `Rc<T>` vs `Arc<T>` distinctions in import-site signatures.

### Why HashSet/BTreeSet map to list

Mochi has no first-class set type. Mapping `HashSet<T>` to `list[T]` preserves the element type and lets callers use list-style iteration. The wrapper layer materialises the set as a `Vec<T>` on entry and rebuilds the set on call; the order is not preserved, but `HashSet` callers do not expect order anyway. A future Mochi `set` primitive can promote this without changing the SkipReason taxonomy.

### Why empty tuple is unit, but Type.Kind() reports "empty"

The rustdoc-types JSON encoding for `()` is `{"tuple": []}`. `rustdoc.Type.Kind()` uses `len(t.Tuple) > 0` as the discriminator, so the empty form falls through to `"empty"`. The fix lives in `typemap.Map`: a pre-dispatch check for `t.Tuple != nil && len(t.Tuple) == 0` produces a `KindUnit` Mapping. The rustdoc parser is intentionally not changed; downstream consumers can rely on `Kind() == "empty"` continuing to mean "no payload at all" for genuine empty Type values.

### Why &mut T uses SkipUnknown

The `errors.SkipReason` enum (Phase 0) does not yet carry a `SkipMutBorrow` variant. Adding one mid-stream would break the fixture-count golden files in earlier phases. The Phase 4 wrapper synth will batch enum additions, at which point `&mut T` will get a named reason; today it surfaces as `SkipUnknown` with a clear Detail string.

## Closeout notes

Phase 3 introduces no new external dependencies; the package depends only on `mochi/package3/rust/errors` and `mochi/package3/rust/rustdoc`. The closed table is the single source of truth consumed by Phase 4 (wrapper synth), Phase 5 (extern-fn emitter), and Phase 12 (monomorphisation). A regression in any future rustdoc-types schema bump that adds a new `Type` variant will produce a `SkipUnknown` skip rather than a crash, exactly as Phase 2's `Type.Unknown` catch-all guarantees.

Phase 4 will consume `Mapping.FFIRepr()` to emit the extern-C wrapper function signatures, and `Mapping.MochiType()` to populate the SkipReport detail strings when a containing function fails to lower.
