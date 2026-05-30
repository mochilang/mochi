---
title: "Phase 1. npm registry client"
sidebar_position: 3
sidebar_label: "Phase 1. npm registry"
description: "MEP-72 Phase 1: registry.npmjs.org packument + tarball + provenance reader. Resolves package@semver-range to a concrete tarball URL + integrity hash + sigstore-attested flag."
---

# Phase 1. npm registry client

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase1NpmRegistry` in `package3/typescript/npmregistry/phase01_test.go`: subtests `fetch_packument`, `resolve_range`, `download_tarball`, `verify_integrity`, `read_provenance`. The first fetches `https://registry.npmjs.org/zod` against a recorded golden response and asserts the parsed `Packument` exposes all published versions plus the `dist-tags` map. The second resolves `^3.22.0` against the packument and asserts the highest matching version is selected. The third downloads the tarball at `dist.tarball`, verifies the SHA-512 in `dist.integrity` (subresource-integrity format: `sha512-<base64>`), and unpacks to a scratch directory. The fourth verifies the unpacked tree's content-addressed hash matches a recorded golden. The fifth reads the `_attestations` array on the packument and exposes the `predicateType = "https://slsa.dev/provenance/v1"` entries.

The phase also gates against a fixture replay of the May 2026 packument for all 24 fixture-corpus packages. The replay uses a recorded HTTP cassette (`testdata/cassettes/*.json`) so the gate runs hermetically without network access.

## Lowering decisions

The client is a pure-Go HTTP/2 client using `net/http` with a custom `http.Transport` that pins TLS to TLS 1.3 minimum. The registry endpoint is configurable via `Options.RegistryURL` (default `https://registry.npmjs.org`); private registries (Verdaccio, GitHub Packages, Artifactory) are supported by overriding the base URL.

Packument fetches use `Accept: application/vnd.npm.install-v1+json` to get the abbreviated packument (deps-only, no readme, no author bios). Full packument is available via `Accept: application/json` but is ~10x the size and only needed when the bridge surfaces a deprecation reason or repository URL; phase 1 ships only the abbreviated path.

The `dist.integrity` field is the canonical hash (subresource-integrity format introduced in npm 5.0, December 2017). The bridge verifies `sha512` (mandatory; `sha1` legacy entries are rejected with a clear diagnostic). Phase 9's lockfile records both the SRI hash AND the MEP-57 BLAKE3 secondary hash of the unpacked tree.

The `_attestations` field is the Sigstore-attestation pointer added by npm Trusted Publishing (GA April 2024). When present, the bridge fetches the attestation bundle from `https://registry.npmjs.org/-/npm/v1/attestations/<pkg>@<version>` and verifies the Sigstore signature against the Fulcio CA bundle. When absent, the bridge sets `sigstore-attested = false` in the lockfile and emits a build-time warning (configurable to error via `[ts.capabilities] sigstore-required = true`).

Tarball download uses streaming gunzip + tar extraction; the helper allocates a scratch directory under the workspace's `WorkDir/tarballs/<pkg>@<version>/` and writes the extracted tree there. Symlinks in tarballs are refused (some npm packages have CVE-grade symlink-traversal payloads); the extractor enforces the no-parent-traversal rule and rejects any entry with `..` in its path.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/npmregistry/client.go` | `Client`, `Options`, `NewClient`, `FetchPackument`, `DownloadTarball`, `FetchAttestation` |
| `package3/typescript/npmregistry/packument.go` | `Packument`, `PackumentVersion`, `Dist`, `DistTags`, `ResolveRange` |
| `package3/typescript/npmregistry/attestation.go` | `Attestation`, `PredicateType`, `VerifySigstore` |
| `package3/typescript/npmregistry/tarball.go` | streaming gunzip + tar extraction with symlink + parent-traversal refusal |
| `package3/typescript/npmregistry/phase01_test.go` | `TestPhase1NpmRegistry` sentinel |
| `package3/typescript/npmregistry/testdata/cassettes/*.json` | recorded HTTP responses for all 24 fixture packages |

## Test set

- `TestPhase1NpmRegistry/fetch_packument`
- `TestPhase1NpmRegistry/resolve_range`
- `TestPhase1NpmRegistry/download_tarball`
- `TestPhase1NpmRegistry/verify_integrity`
- `TestPhase1NpmRegistry/read_provenance`
- Fixture-corpus replay for all 24 packages.

## Cross-references

- [MEP-72 §3 Lockfile extension](/docs/mep/mep-0072#3-lockfile-extension-npm-package-and-jsr-package) — the `[[npm-package]]` fields this phase populates.
- [Research note 07 §1 npm Trusted Publishing](/docs/research/0072/07-sigstore-npm-jsr-trusted-publishing#1-npm-trusted-publishing) — the attestation format this phase reads.
- [MEP-74 phase 1 module-proxy](/docs/implementation/0074/phase-01-module-proxy) — the parallel Go-side registry client.
