---
title: "Phase 6. import rust grammar"
sidebar_position: 8
sidebar_label: "Phase 6. import rust grammar"
description: "MEP-73 Phase 6 lands the parser side of the bridge: `import rust \"<crate>@<semver>\" as <alias>` is admitted by the grammar, the path is validated at parse time against the `<crate>@<semver>` shape, and a `parser.RustImportRef` helper exposes the (crate, version) pair to downstream phases."
---

# Phase 6. import rust grammar

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 22:30 (GMT+7) |
| Landed         | 2026-05-29 22:42 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

The Mochi parser admits `import rust "<crate>@<semver>" as <alias>` as a first-class form, on par with `import python "..." as ...` and `import go "..." as ...`. Three pieces:

1. `"rust"` joins `knownImportLangs` in `parser/normalize.go`. The grammar already admits any identifier in the lang slot; the validator just stops rejecting `rust` as a typo.
2. A new validator `validateRustImport` (P066) rejects paths that are not in `<crate>@<semver>` form. Catching the typo at parse time turns `import rust "hex" as hex` (missing version) or `import rust "Hex@1.0" as hex` (upper-case crate name) into a positioned diagnostic instead of a far-later "crate not found" or wrapper-build failure.
3. A new helper `parser.RustImportRef(path) (crate, version string, ok bool)` exposes the parsed pair to downstream phases (the build driver in Phase 7 consumes it to look up the wrapper crate in the cache).

## Path validation

`RustImportRef` accepts strings of the shape `<crate>@<version>` with:

- Crate name: lower-case ASCII letters, digits, `_`, `-`; 1..64 chars; must start with a letter.
- Version: any non-empty token with no whitespace. Semver parsing itself lives in `package3/rust/semver` (Phase 1); the parser only enforces the `<crate>@<rest>` shape so a malformed path doesn't reach the bridge.

The crate-name rule is the conservative subset of Cargo's naming rules. Every crate published since 2018 fits this subset (upper-case names and dotted names were retired by Cargo before then). Staying strict keeps the parser diagnostic on the user's side rather than silently passing typos through.

Cargo accepts a range of version specifiers (`=1.0.0`, `1.0`, `^1.0`, `~1.0`, `1.0.0-rc.1`, `1.0.0+meta`); the parser admits all of them without trying to interpret them. The bridge's semver parser (`package3/rust/semver`) is the source of truth.

## Test surface

Parser unit tests in `parser/rust_import_test.go` cover every shape: plain `hex@0.4.3`, quoted strings, `once_cell` (underscore), `rand-chacha` (hyphen), `serde@^1.0` (caret), `serde@~1.0.195` (tilde), `tokio@1.40.0-rc.1` (pre-release), `tokio@1.40.0+meta` (build metadata), plus 8 negative cases (missing version, empty crate, empty version, uppercase, digit-start, dotted name, space in version, empty string).

Golden tests in `tests/parser/valid/import_rust.mochi` and `tests/parser/errors/import_rust_*.mochi` lock the AST shape and the P066 diagnostic against the rest of the parser corpus. The existing `import_unknown_lang.err` golden was updated to include `rust` in the supported-languages help line.

## What this does NOT do

- It does NOT fetch the crate. That is Phase 1 (sparse-index client, already landed) wired up by Phase 7 (build orchestration).
- It does NOT type-check the alias namespace. The downstream module resolver (Phase 8 will integrate with `mochi.lock`) ties the alias to the synthesised Phase 5 alias-module file.
- It does NOT consume the `package3/rust/semver` package directly. The parser stays free of bridge-implementation dependencies; structural shape validation here, semver semantics in the bridge.

## Files changed

| File | Purpose |
|------|---------|
| `parser/normalize.go` | adds `"rust"` to `knownImportLangs`, adds `errInvalidRustImportPath` (P066) template + `validateRustImport`, wires the validator into `normalizeStatement` |
| `parser/rust_import.go` | `RustImportRef(path) (crate, version string, ok bool)` + `isCargoCrateName` helper |
| `parser/rust_import_test.go` | 16 cases for `RustImportRef` + 13 cases for `isCargoCrateName` |
| `tests/parser/valid/import_rust.mochi` (+ `.golden`) | three valid forms: `hex@0.4.3`, `once_cell@1.19.0`, `serde@^1.0` |
| `tests/parser/errors/import_rust_missing_version.mochi` (+ `.err`) | rejects `import rust "hex" as hex` |
| `tests/parser/errors/import_rust_bad_crate.mochi` (+ `.err`) | rejects `import rust "Hex@1.0" as hex` |
| `tests/parser/errors/import_unknown_lang.err` | help text updated to include `rust` |
| `website/docs/implementation/0073/phase-06-import-grammar.md` | this page |
| `website/docs/implementation/0073/index.md` | mark Phase 6 LANDED, backfill Phase 5 SHA |
| `website/docs/mep/mep-0073.md` | mark Phase 6 LANDED in target matrix |

## Test set

- `go test ./parser/...` (all green)
- `go test ./package3/rust/...` (regression: 8 packages green)
- `go test ./types/...` (regression: green)

## Closeout notes

Phase 6 introduces no external dependencies and no Rust code. The parser stays free of `package3/rust/*` imports; the path validator only enforces the structural shape (`<crate>@<version>` with a non-empty crate name in the conservative subset and a non-empty version with no whitespace).

Phase 7 (build orchestration) consumes `RustImportRef` to dispatch each `import rust` to the sparse-index client (Phase 1) + rustdoc ingest (Phase 2) + typemap (Phase 3) + wrapper synth (Phase 4) + extern-emit (Phase 5) pipeline. The alias module file emitted by Phase 5 is loaded under the `<alias>` name from the import statement.

The version-required rule (`<crate>@<version>`, not just `<crate>`) is deliberate. MEP-73 binds the synthesised wrapper crate to an exact upstream version because the rustdoc-JSON it consumes is version-specific. A `mochi.lock`-style ranged constraint (Phase 8) layers on top: the lock file pins the exact version that satisfied the import constraint, and `mochi pkg lock --check` fails if the lock drifted.
