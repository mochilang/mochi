---
title: "Phase 9. mochi.lock integration"
sidebar_position: 11
sidebar_label: "Phase 9. Lockfile"
description: "MEP-72 Phase 9: mochi.lock `[[npm-package]]` + `[[jsr-package]]` schema integration; `mochi pkg lock --check` drift detection."
---

# Phase 9. mochi.lock integration

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase9Lockfile` in `package3/typescript/lockfile/phase09_test.go`: subtests `read_npm_section`, `read_jsr_section`, `write_npm_section`, `write_jsr_section`, `check_drift`, `regenerate`, `lockfile_v2_format`, `golden_corpus`. The first four exercise round-trip read/write of representative lockfile entries. The fifth runs `mochi pkg lock --check` against a workspace whose `mochi.toml` declares a different range than the lockfile pins and asserts a clear "lockfile out of date" diagnostic. The sixth runs `mochi pkg lock --regenerate` and asserts the rewritten lockfile matches the golden. The seventh asserts the lockfile format version (`version = 2`, per MEP-57 round 6) is honoured. The eighth runs the lockfile read/write against all 24 fixture packages plus the 6 JSR packages.

## Lowering decisions

The lockfile schema extends MEP-57's lockfile with two repeated table arrays:

```toml
version = 2

[[npm-package]]
name = "zod"
version = "3.22.4"
range-from-manifest = "^3.22.0"
registry = "https://registry.npmjs.org"
sri-hash = "sha512-Cb3xEZIliMnsf3jJpYRljDOj82uTrsi24kQqVLZ7L2zMquKL2HiVqRGirEEjP7iZK0bV1G4SDsRyf9j2yyTHaQ=="
blake3-tree-hash = "blake3:b3:9c8f...c4d2"
module-shape = "esm"
exports-map-resolved = "./dist/index.mjs"
sigstore-attested = true
sigstore-attestation-url = "https://registry.npmjs.org/-/npm/v1/attestations/zod@3.22.4"
capabilities-declared = []
engines-node = ">=18"
engines-workerd = "*"

[[jsr-package]]
scope = "std"
name = "encoding"
version = "1.0.5"
range-from-manifest = "^1"
registry = "https://jsr.io"
blake3-tree-hash = "blake3:b3:7a3e...91f0"
module-shape = "esm"
entry-module = "./mod.ts"
sigstore-attested = true
sigstore-attestation-url = "https://jsr.io/@std/encoding/1.0.5/_attestation.json"
capabilities-declared = []
```

The `[[npm-package]]` entries are ordered by `name` ascending; the `[[jsr-package]]` entries are ordered by `(scope, name)` ascending. Determinism is enforced by the renderer.

The `--check` mode reads `mochi.toml` and `mochi.lock`, recomputes the expected resolution, and exits with non-zero status if any of the following differ: pinned version, SRI / BLAKE3 hash, module shape, sigstore attestation status, capability declaration. The diagnostic names every drifted entry.

The `--regenerate` mode is destructive: it overwrites the lockfile with freshly resolved entries. The user opts into it explicitly; `mochi build` and `mochi run` never trigger it.

The lockfile-v2 format adds `version = 2` at the top of the file. The bridge refuses to operate on a v1 lockfile (the migration tool `mochi pkg lock --migrate-v1-to-v2` is invoked separately).

The phase exposes a `--cli-verb=lock` namespace per MEP-57 round 7's CLI-verb spec: `mochi pkg lock` (regenerate), `mochi pkg lock --check`, `mochi pkg lock --print` (read-only dump), `mochi pkg lock --migrate-v1-to-v2`.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/lockfile/schema.go` | `LockfileV2`, `NpmPackage`, `JsrPackage` types |
| `package3/typescript/lockfile/reader.go` | TOML parser for v2 lockfile |
| `package3/typescript/lockfile/writer.go` | deterministic TOML writer |
| `package3/typescript/lockfile/check.go` | drift detection |
| `package3/typescript/lockfile/regenerate.go` | regeneration entry |
| `package3/typescript/lockfile/migrate_v1_v2.go` | v1 → v2 migration helper |
| `package3/typescript/lockfile/phase09_test.go` | `TestPhase9Lockfile` sentinel |

## Test set

8 subtests as listed in the Gate section.

## Cross-references

- [MEP-72 §3 Lockfile extension](/docs/mep/mep-0072#3-lockfile-extension-npm-package-and-jsr-package) — the schema this phase implements.
- [MEP-57 round 6 lockfile v2](/docs/mep/mep-0057#round-6-lockfile-v2) — the underlying lockfile format.
- [MEP-74 phase 10 lockfile](/docs/implementation/0074/phase-10-lockfile) — the sister Go-side lockfile phase.
