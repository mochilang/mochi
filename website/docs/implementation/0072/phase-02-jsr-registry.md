---
title: "Phase 2. JSR registry client"
sidebar_position: 4
sidebar_label: "Phase 2. JSR registry"
description: "MEP-72 Phase 2: jsr.io packument + module-file reader + sigstore attestation status. Resolves @scope/pkg@semver-range to a concrete module-tree manifest + per-file BLAKE3 + sigstore-attested flag."
---

# Phase 2. JSR registry client

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase2JsrRegistry` in `package3/typescript/jsrregistry/phase02_test.go`: subtests `fetch_meta`, `resolve_range`, `download_module_tree`, `verify_blake3`, `read_attestation`. The first fetches `https://jsr.io/@std/encoding/meta.json` against a recorded golden and asserts the parsed `Meta` exposes all published versions. The second resolves `^1.0.0` against the meta and asserts the highest matching version. The third downloads every file listed in the version's manifest (`/@std/encoding/<version>_meta.json` then each `/@std/encoding/<version>/<path>`). The fourth verifies each file's BLAKE3 hash matches the manifest entry. The fifth reads the attestation pointer (when present) and verifies the Sigstore signature.

The phase gates against a fixture replay of 6 JSR-native packages (April 2026): `@std/encoding`, `@std/fmt`, `@std/path`, `@std/io`, `@hono/hono`, `@oak/oak`. The replay uses recorded HTTP cassettes (`testdata/cassettes/*.json`).

## Lowering decisions

The JSR client differs from the npm client in three substantial ways:

1. **Module tree, not tarball.** JSR serves individual `.ts` files plus a manifest, not a gzipped tarball. The client downloads the manifest first (`<base>/<scope>/<pkg>/<version>_meta.json`), then iterates the manifest's file list and fetches each file. The file count for a typical JSR package is 5-50 (vs npm's 100s-1000s after dependency tree expansion).
2. **BLAKE3 native.** JSR uses BLAKE3 as the canonical hash from day one. The MEP-57 dual-hash scheme treats BLAKE3 as primary; the JSR-side hash IS the lockfile-recorded primary hash without a secondary computation.
3. **Sigstore enforced server-side.** JSR's publish flow (since mid-2024) requires Trusted Publishing for new packages; many packages predating that requirement have no attestation. The client reads `meta.json`'s `attestations` array; when empty, the lockfile records `sigstore-attested = false`.

The client uses the same `net/http` + TLS 1.3 + custom transport as the npm client. The registry endpoint is `https://jsr.io` (configurable via `Options.RegistryURL`).

JSR's `npm:` compatibility mode (where a JSR package re-exports an npm package) is acknowledged but not specially handled at phase 2; the import resolves via the JSR side, the underlying npm tarball flows through the npm-registry phase 1 client.

Source-not-dist invariant: JSR publishes source `.ts` files, not transpiled `.js` + `.d.ts`. The JSR runtime (Deno) transpiles on-demand; the bridge's `.d.ts` ingest helper (phase 3) is fed the source `.ts` directly, and the TypeScript compiler API processes it the same way it would a pre-shipped `.d.ts`. This means JSR packages do NOT need a `.d.ts` build step on the publish side (phase 13 is significantly simpler than phase 12).

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/jsrregistry/client.go` | `Client`, `Options`, `NewClient`, `FetchMeta`, `DownloadModuleTree`, `FetchAttestation` |
| `package3/typescript/jsrregistry/meta.go` | `Meta`, `MetaVersion`, `ManifestEntry`, `ResolveRange` |
| `package3/typescript/jsrregistry/attestation.go` | `Attestation`, `VerifySigstore` (re-uses bulk of npmregistry's verifier) |
| `package3/typescript/jsrregistry/blake3.go` | per-file BLAKE3 verification against manifest |
| `package3/typescript/jsrregistry/phase02_test.go` | `TestPhase2JsrRegistry` sentinel |
| `package3/typescript/jsrregistry/testdata/cassettes/*.json` | recorded HTTP responses for the 6 JSR fixture packages |

## Test set

- `TestPhase2JsrRegistry/fetch_meta`
- `TestPhase2JsrRegistry/resolve_range`
- `TestPhase2JsrRegistry/download_module_tree`
- `TestPhase2JsrRegistry/verify_blake3`
- `TestPhase2JsrRegistry/read_attestation`
- Fixture-corpus replay for the 6 JSR packages.

## Cross-references

- [MEP-72 §3 Lockfile extension](/docs/mep/mep-0072#3-lockfile-extension-npm-package-and-jsr-package) — the `[[jsr-package]]` fields this phase populates.
- [Research note 06 §3 JSR publish flow](/docs/research/0072/06-npm-jsr-publish-flow#3-jsr-publish-flow) — the source-not-dist invariant this phase relies on.
- [Research note 07 §2 JSR Trusted Publishing](/docs/research/0072/07-sigstore-npm-jsr-trusted-publishing#2-jsr-trusted-publishing) — the attestation format this phase reads.
