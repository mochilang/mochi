---
sidebar_label: "Phase 8: mochi.lock integration"
sidebar_position: 9
---

# MEP-73 Phase 8: mochi.lock integration

**Status:** LANDED (2026-05-29)
**Spec section:** [MEP-73 §3 — Lockfile schema](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p8`

## Gate

Land the `[[rust-package]]` lockfile schema, byte-stable encoder/decoder,
and drift checker so `mochi pkg lock` and `mochi pkg lock --check` can
write and validate the rust-crate portion of `mochi.lock`.

## Why it matters

Phase 7 produced a deterministic Cargo workspace plus a portable
`cargo build` invocation. Determinism only buys safety if the lockfile
can detect when something downstream tries to slip a different value in.
This phase pins the seven sentinel fields per crate (registry source,
crate BLAKE3, crate SHA-256, rustdoc-types version, rustdoc SHA-256,
wrapper SHA-256, declared capabilities) and turns any divergence between
the recorded entry and a fresh resolve into a hard error.

The schema in this phase is the contract MEP-57 (`mochi pkg`) calls into
when it materialises `mochi.lock`, and the contract the bridge calls
into when it ingests it back. The drift checker is what
`mochi pkg lock --check` runs in CI to fail closed on any of the
following:

- An added or removed `[[rust-package]]` entry
- A version, source, or transitive-dependency move
- A `.crate` tarball whose dual hash no longer matches
- A `rustdoc-types` schema version bump
- A wrapper source whose rendered bytes drifted (the bridge would emit
  different code if it locked again right now)
- A new capability the lockfile did not previously acknowledge
  (per MEP-57 monotonicity)

## What landed

### `package3/rust/lockfile/lockfile.go`

The schema and codec.

- `Source{Kind, Registry, URL, Rev, Path}` covers the three sources the
  spec admits (registry, git, path) with `Kind` as the discriminator.
- `RustPackage{Name, Version, Source, CrateBlake3, CrateSHA256,
  RustdocTypesVersion, RustdocSHA256, WrapperSHA256,
  CapabilitiesDeclared, Dependencies, Features}` matches the field
  shape MEP-73 §3 documents, with kebab-case TOML key names rendered
  by the encoder.
- `Encode([]RustPackage) string` sorts by name then version and renders
  byte-stable TOML by hand (matching workspace.go's no-dependency
  style).
- `Decode(io.Reader)` / `DecodeString(string)` parses the same shape,
  tolerating comments, blank lines, surrounding preamble, and unknown
  keys (forward-compat tolerance).
- `splitTopLevel` is the helper that lets inline tables
  (`{ kind = "registry", registry = "..." }`) and arrays
  (`["a", "b"]`) tokenize without splitting on interior commas.

### `package3/rust/lockfile/wrapper_sha.go`

Two thin helpers that are the *only* points in the codebase that hash
wrapper or rustdoc content. Centralising the hash function means the
encoder, the bridge build pipeline, and the drift checker all observe
exactly the same digest.

- `WrapperSHA256(libRS string) string` — lowercase hex SHA-256 of the
  wrapper crate's rendered `src/lib.rs`.
- `RustdocSHA256(rustdocJSON []byte) string` — lowercase hex SHA-256 of
  a rustdoc JSON document.

SHA-256 (not BLAKE3) is used here to match the cargo / crates.io
ecosystem's preferred digest for source-level pins. The `.crate`
tarball itself still gets the MEP-57 dual BLAKE3 + SHA-256 pair.

### `package3/rust/lockfile/drift.go`

The `mochi pkg lock --check` engine.

- `DriftKind` is a closed enum covering every sentinel: added, removed,
  version, source, crate-blake3, crate-sha256, rustdoc-types-version,
  rustdoc-sha256, wrapper-sha256, capability-added, capability-removed,
  features, dependencies.
- `Drift{Crate, Kind, Want, Have, Detail}` is one observed difference
  with a human-readable rendering suitable for diagnostic output.
- `Check(want, have []RustPackage) []Drift` compares the two sets and
  returns every difference sorted by crate name then kind for stable
  output. A clean check returns nil.

Capability drift is split into `DriftCapabilityAdded` and
`DriftCapabilityRemoved` so a downstream policy can treat the two
directions differently (per MEP-57's monotonicity rule, an added
capability requires re-acknowledgement, while a removed capability is
not strictly an error).

### Tests

- `lockfile_test.go` (22 cases): Encode determinism, Decode malformed
  inputs, source-kind variants, string-array variants, roundtrip
  stability, reader-vs-string input, splitTopLevel edge cases.
- `drift_test.go` (~20 cases): every `DriftKind.String()`, every
  `Check`-detectable drift category, sort ordering, helper coverage
  (`sourceEqual`, `sourceString`, `stringSetEqual`, `stringSetDiff`,
  `joinSorted`).
- `wrapper_sha_test.go` (9 cases): pinned SHA-256 of empty string and
  "hello", single-byte change detection for both `WrapperSHA256` and
  `RustdocSHA256`, determinism.
- `phase08_test.go` (sentinel) with subtests `schema_roundtrip`,
  `wrapper_sha_pins_drift`, `check_clean_pair_returns_nil`,
  `check_detects_capability_addition`, `schema_matches_mep73_section_3`
  (hand-written exemplar based on MEP-73 §3 with surrounding preamble),
  `section_3_roundtrips_under_check` (Encode → Decode → Check returns
  nil).

## Target matrix

| Target           | Status   | Notes |
|------------------|----------|-------|
| Schema           | ✅       | RustPackage + Source closed shape per MEP-73 §3. |
| Encoder          | ✅       | Byte-stable TOML, sorted by name then version. |
| Decoder          | ✅       | Tolerant of comments / preamble / unknown keys. |
| Wrapper hash     | ✅       | Single canonical SHA-256 entry point. |
| Rustdoc hash     | ✅       | Sibling helper, byte input. |
| Drift checker    | ✅       | 13-variant `DriftKind` covers every schema field. |

## How this phase plugs in to the larger pipeline

```
                ┌────────────────────────────┐
                │ pipeline.Resolve(refs)     │   Phase 7
                │ → []ResolvedCrate          │
                └─────────┬──────────────────┘
                          │
                          │ + per-crate
                          │   { crate-blake3, crate-sha256,
                          │     rustdoc-types-version,
                          │     rustdoc-sha256,
                          │     wrapper-sha256,
                          │     capabilities, deps, features }
                          ▼
                ┌────────────────────────────┐
                │ []lockfile.RustPackage     │
                └─────────┬──────────────────┘
                          │
              ┌───────────┴─────────────┐
              ▼                         ▼
   lockfile.Encode(pkgs)      lockfile.Check(want, have)
       (mochi pkg lock)         (mochi pkg lock --check)
```

The bridge does not yet wire the hashes through (Phase 9+ owns
`TargetRustLibrary` emit and the integration call), but the schema is
landed and the drift contract is stable.

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-29 23:18    | Worktree branch `mep/0073-phase-08` created off `origin/main`. |
| 2026-05-29 23:20    | `lockfile.go`, `wrapper_sha.go`, `drift.go` written. |
| 2026-05-29 23:22    | Test suites green (`go test ./package3/rust/...`). |
| 2026-05-29 23:23    | Tracking page + spec sync. |
