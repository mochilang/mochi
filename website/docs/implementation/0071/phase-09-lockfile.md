---
title: "MEP-71 Phase 9: lockfile integration"
sidebar_label: "Phase 9. Lockfile"
sidebar_position: 10
description: "Phase 9 of MEP-71. Adds the `[[python-package]]` table for mochi.lock plus `--check` drift detection plus a closed-vocabulary capability database."
---

# Phase 9: lockfile integration

Phase 9 introduces `package3/python/lockfile`, the package that produces and consumes the `[[python-package]]` array inside mochi.lock. The phase covers three responsibilities:

1. **Schema**: the on-disk TOML shape that each `import python` declaration pins to.
2. **`--check` mode**: a deterministic Diff between the on-disk lock and what `Plan` would currently produce, used by the `mochi pkg lock --check` CLI gate to refuse drift in CI.
3. **Capability database**: a sorted, closed-vocabulary list of capability tokens extracted from the Phase 5 `Wrapper.Items`, recorded on each entry so Phase 15 (attestation verification) can verify the surface against the attested set.

## Gate

`go test ./package3/python/lockfile/...` is green. The gate covers:

- `Source.Valid()` admits exactly the four MEP-71 §5 sources (`registry`, `index`, `git`, `path`) and rejects everything else.
- `Manifest.Sort` is `(Name, Alias)` ascending for deterministic round-trips.
- `FromBuildResult(req, res)` converts a Phase 8 Build result into a Manifest: every Target produces one Entry with the spec source, the original PEP 440 specifier, the resolved version when the spec is `==N.N.N`, the wrapper SHA-256, and the extracted capability list. Mismatched req / res counts and nil results are rejected.
- `RenderTOML(m)` produces a deterministic TOML document with one `[[python-package]]` table per entry. Optional fields (version, specifier, index-url, git-url, git-rev, local-path, wrapper-sha256, capabilities) are omitted when empty so the lock stays compact.
- `ParseTOML(src)` reads the document back: rejects an unknown `schema-version`, a missing `schema-version`, and any entry missing a required key (`name`, `alias`, `source`) or holding an unknown source token.
- Round-trip: `Check(FromBuildResult(req, res), ParseTOML(RenderTOML(...)))` returns nil.
- `CompareManifests` and `Check` classify every per-entry delta as `added` / `removed` / `changed`; `changed` reports the field set that diverged. The diff key is `<Name>:<Alias>` so the same distribution under two aliases shows up as two independent entries.
- `ExtractCapabilities(w)` walks `wrapper.Wrapper.Items` and the lowered `typemap.MochiType` tree, emitting tokens from the closed vocabulary: `async`, `dataclass`, `protocol`, `callable`, `stream`, `map`, `set`, `list`, `optional`, `sum`, `tuple`, `constant`, `typevar`. A wrapper whose surface is empty produces a nil list.
- Sentinel `TestPhase9LockfileIntegration` walks a two-target Build through `FromBuildResult` -> `RenderTOML` -> `ParseTOML` -> `Check`, asserts the pinned httpx version surfaces, the async + dataclass capabilities are recorded, and `Check` against an artificially drifted manifest surfaces both an added ghost entry and a flipped `wrapper-sha256`.

## Files

- `package3/python/lockfile/doc.go` — package overview pointing at MEP-71 §5.
- `package3/python/lockfile/entry.go` — `SchemaVersion`, `Source`, `Entry`, `Manifest`, `Manifest.Sort`.
- `package3/python/lockfile/capabilities.go` — capability token constants and `ExtractCapabilities`.
- `package3/python/lockfile/from_build.go` — `FromBuildResult` plus the `toSource` and `pinnedVersion` helpers.
- `package3/python/lockfile/render.go` — `RenderTOML` and `renderEntry`.
- `package3/python/lockfile/parse.go` — `ParseTOML` and `decodeEntry`, layered on `package3/python/toml`.
- `package3/python/lockfile/diff.go` — `DiffKind`, `DiffEntry`, `Diff`, `CompareManifests`, `Check`.
- `package3/python/lockfile/lockfile_test.go`, `phase09_test.go` — ~30 tests + the Phase 9 sentinel.

## Fixtures

The unit tests use synthetic `.pyi` fragments (the same minimal / record / async shapes Phase 8 uses) to exercise the gate. Corpus-wide validation against the 25-package fixture set lands with sub-phase 8.1 (wheel install) once the resolver populates the `version` field with real resolved versions; today the Phase 9 sentinel keys on pinned `==N.N.N` specs.

## Skip count

Phase 9 introduces no new refusal cases. The Phase 5 / Phase 4 `SkipReport`s already surface through `Result.Skipped` from Phase 8; Phase 9 keeps them out of the lockfile entirely (they belong to the per-wrapper `SKIPPED.txt`, not the lock).

## Notes

- The lockfile is keyed on `(distribution, alias)`. Two imports of the same PyPI distribution under different aliases produce two independent lockfile entries; each carries its own wrapper SHA-256 because the alias drives the wrapper module path. This matches Phase 8's per-alias `python_wrap/<alias>/` layout.
- The TOML encoder is hand-rolled (no third-party library) so the rendered bytes stay stable for hashing the workspace lock. The parser reuses `package3/python/toml`.
- Capability tokens are intentionally coarse. Phase 15 will gate attestation requirements per token (e.g. an attested wrapper must declare its `callable` surface); the closed vocabulary keeps the audit log human-readable.
- `--check` mode (`mochi pkg lock --check`) is implemented by the `Check` function plus the CLI plumbing that lives in MEP-57 (the polyglot workspace lock); the CLI gate calls `lockfile.Check(onDisk, FromBuildResult(req, res))` and surfaces the diff verbatim on non-zero exit.

## Timestamps

- 2026-05-30 00:11 (GMT+7): Phase 9 started.
- 2026-05-30 00:17 (GMT+7): Phase 9 LANDED.
