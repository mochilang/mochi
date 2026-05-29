---
sidebar_label: "Phase 12: Monomorphisation"
sidebar_position: 13
---

# MEP-73 Phase 12: Monomorphisation (`[rust.monomorphise]` manifest + per-instantiation wrapper)

**Status:** LANDED (2026-05-30)
**Spec section:** [MEP-73 §3 — Generics and monomorphisation](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p12`

## Gate

Land the manifest-driven monomorphisation pipeline: when an upstream
crate exposes a generic fn like `fn from_str<T: DeserializeOwned>(s:
&str) -> Result<T, Error>`, Phase 3's typemap refuses it with
`SkipGeneric`. Phase 12 unlocks the case via an explicit
`[rust.monomorphise]` table in `mochi.toml`; the bridge synthesises one
extern "C" wrapper per `(item, type-args)` pair, with a mangled symbol
and a turbofish call-site path.

## Why it matters

Phases 4-5 produced wrappers only for fns whose every type was in the
closed type-mapping table. Generic fns (especially serde's
`from_str<T>`, `from_value<T>`, `to_string<T>` and the `Vec::new`
/ `HashMap::new` constructors) hit `SkipGeneric` and never produce a
SynthFn, so a Mochi user importing serde_json today cannot deserialise
into a concrete `MyStruct` type. The auto-monomorphisation alternative
(walk every Mochi call site and synthesise the matching instantiation)
risks the combinatorial-explosion failure mode MEP-73 §Risks calls out.

Phase 12 follows the spec's "closed surface" principle: the user
declares exactly which instantiations they need, the bridge emits
exactly those, and the manifest's length hard-bounds the combinatorial
risk. No inference; no auto-monomorphisation.

## What landed

### `package3/rust/monomorphise/monomorphise.go`

A new package owning the manifest's on-wire shape, the symbol mangler,
the turbofish renderer, the substitution lens, and the hand-rolled
TOML parser. No third-party TOML dep — the parser is narrow enough
that the layering-conservative path is the right tradeoff.

- `Spec` and `Entry` types. `Entry{Item, TypeArgs}` carries the
  upstream Rust path (e.g. `"serde_json::from_str"`) plus the
  per-type-parameter substitution map (e.g. `{"T": "MyStruct"}`).
- `Spec.Validate()` rejects empty item paths, empty type-args, empty
  parameter names, and empty substitutions. Bad entries would mangle
  to ambiguous symbols.
- `Spec.Lookup(item)` returns every Entry matching an upstream path
  (the same generic can be instantiated multiple times). Linear scan
  because the manifest is bounded at a few tens of entries.
- `ExternName(upstream, item, typeArgs)` mangles to a stable extern-C
  symbol with an `_of_<sorted-key>_<value>...` suffix. Sorting keeps
  the symbol byte-stable across runs even though `TypeArgs` is a map.
- `CallSite(item, typeArgs)` renders the turbofish call expression
  (e.g. `"serde_json::from_str::<MyStruct>"`). Multi-arg
  substitutions render as `<A, B>` with parameter names sorted for
  byte stability.
- `Substitute(t, subs)` recursively walks a `rustdoc.Type`, replacing
  every `Generic` node whose name is in `subs` with a concrete type
  (primitive when the substitution names a known Rust primitive,
  resolved_path otherwise). Tuples, slices, arrays, borrowed-refs and
  generic-arg lists are walked through.
- `ParseTOMLEntries(body)` parses the inline-array body of the
  `monomorphise` row. Accepts both bracket-wrapped and stripped
  forms. Rejects unquoted values and unbalanced braces.

### `package3/rust/wrapper/crate.go`

A new `SynthWithSpec(upstream, version, surface, spec)` entry point
extends `Synth` with a monomorphisation lens. For each fn whose path
matches a Spec entry, the wrapper emits one SynthFn per Entry with
substituted types, a mangled `ExternName`, and a turbofish
`UpstreamPath` from `monomorphise.CallSite`. Functions without
matching Spec entries take the default Synth path, which still
SkipGenerics for unconcretised parameters — the Spec is the only
escape hatch.

`Synth` is preserved as a thin wrapper over `SynthWithSpec(...,
monomorphise.Spec{})` so all existing callers compile unchanged.

### Tests

- `monomorphise_test.go` (24 cases): `Spec.Validate` (well-formed,
  empty-item, no-type-args, empty-key, empty-value); `Spec.Lookup`
  (multi-match preserves order, miss returns empty); `ExternName`
  (single arg, multi-arg sorted, non-alphanumeric sanitisation, no-
  type-args path, byte stable across 64 runs); `CallSite` (turbofish
  single, multi sorted by key, no-args returns plain, byte stable);
  `ParseTOMLEntries` (single entry, multiple entries, stripped
  brackets, rejects unquoted, rejects unbalanced, empty array);
  `Substitute` (primitive substitution, path substitution, tuple
  walk, Vec arg walk, borrowed-ref walk, unknown generic preserved).
- `phase12_test.go` (sentinel) with subtests:
  `parses_spec_from_manifest`, `lookup_returns_entries_for_item`,
  `extern_name_byte_stable`, `call_site_turbofish_rendering`,
  `validate_rejects_empty_item`, `validate_rejects_no_type_args`,
  `wrapper_emits_one_fn_per_entry`,
  `wrapper_skips_generic_when_no_spec`.

## Target matrix

| Target               | Status | Notes |
|----------------------|--------|-------|
| Manifest shape       | ✅     | Inline-array of inline-tables. `item` plus N capitalised type-param keys. |
| Parser               | ✅     | Hand-rolled; no third-party TOML dep. Rejects unquoted / unbalanced. |
| Spec validation      | ✅     | Empty item / type-args / key / value all rejected pre-emit. |
| Symbol mangling      | ✅     | `_of_<sorted-key>_<value>...` suffix; sanitises `<>:` to `_`. |
| Turbofish rendering  | ✅     | `::<A, B>` with parameter names sorted by key. |
| Substitution lens    | ✅     | Walks tuple / slice / array / borrowed-ref / Vec args. |
| Multi-instantiation  | ✅     | One SynthFn per Entry; no duplicate extern names. |
| Default closed-table | ✅     | Generic fns without Spec entries still SkipGeneric. |
| Async + generic combo| ✅     | `SynthFn.IsAsync` carries through; Phase 11 `block_on` body wraps the turbofish call. |
| Byte stability       | ✅     | ExternName + CallSite byte-stable across 64 runs (sort-by-key). |
| Embedded interaction | ⚠️     | Out of scope for Phase 12. Phase 13 may add an `embedded` profile that further restricts which substitutions resolve. |

## How this phase plugs in to the larger pipeline

```
  mochi.toml [rust.monomorphise]
    [
      { item = "serde_json::from_str", T = "MyStruct" },
      { item = "Vec::new", T = "i64" },
    ]
                │
                ▼
  monomorphise.ParseTOMLEntries
                │
                ▼
  monomorphise.Spec.Validate
                │
                ▼
  wrapper.SynthWithSpec(upstream, version, surface, spec)
                │
                ▼   (for each fn in surface.Functions)
                │
            entries := spec.Lookup(joinPath(fn.Path))
                │
   ┌────────────┴────────────┐
   │                         │
   ▼ entries empty           ▼ entries non-empty
  synthFn(...)              for each Entry e in entries:
   │                          synthFnMonomorphised(upstream, fn, e)
   │                          │
   │                          ▼
   │                        Inputs / Output run through
   │                        monomorphise.Substitute then typemap.Map
   │                          │
   │                          ▼
   │                        SynthFn{
   │                          ExternName: monomorphise.ExternName(...),
   │                          UpstreamPath: monomorphise.CallSite(...),
   │                        }
   ▼                          │
  Crate.Functions ◄───────────┘
```

The bridge sees only the user's enumerated set. Auto-monomorphisation
remains unsupported by design (MEP-73 §Risks).

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-30 00:20    | Worktree branch `mep/0073-phase-12` created off `origin/main`. |
| 2026-05-30 00:24    | `package3/rust/monomorphise/monomorphise.go` written (Spec, Entry, Validate, Lookup, ExternName, CallSite, ParseTOMLEntries). |
| 2026-05-30 00:27    | `Substitute` lens added to monomorphise; tests for primitive / path / tuple / Vec-arg / borrowed-ref walks. |
| 2026-05-30 00:29    | `wrapper.SynthWithSpec` + `synthFnMonomorphised` plumbed; `wrapper.Synth` preserved for callers. |
| 2026-05-30 00:31    | Phase 12 sentinel added (8 subtests). `go test ./package3/rust/...` green. |
| 2026-05-30 00:32    | Tracking page + spec sync. |
