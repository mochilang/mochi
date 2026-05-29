---
title: "04. Packagist ingest"
sidebar_position: 5
sidebar_label: "04. Packagist ingest"
description: "The Packagist v2 sparse API in depth: `p2/<vendor>/<package>.json` format, dist/source URL resolution, SHA-256 checksums in Packagist responses, version constraint resolution, comparison with Cargo sparse index and Go module proxy, the Packagist webhook model, and the Packagist mirror protocol."
---

# 04. Packagist ingest

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note describes how the bridge fetches PHP package metadata and dist zips from Packagist, how that compares to the Cargo sparse index (MEP-73) and the Go module proxy (MEP-74), and how the content-addressed cache is structured.

## 1. Packagist v2 sparse API

Packagist (packagist.org) is the primary Composer package registry. It hosts metadata and dist URL pointers for over 400,000 packages (May 2026). The v2 API endpoint for per-package metadata is:

```
GET https://packagist.org/p2/<vendor>/<package>.json
```

Example:

```
GET https://packagist.org/p2/guzzlehttp/guzzle.json
```

The response is a JSON document with the following structure (simplified):

```json
{
  "packages": {
    "guzzlehttp/guzzle": [
      {
        "name": "guzzlehttp/guzzle",
        "version": "7.8.1",
        "version_normalized": "7.8.1.0",
        "description": "Guzzle is a PHP HTTP client library",
        "keywords": ["framework", "http", "rest", "web service"],
        "license": ["MIT"],
        "authors": [{"name": "Michael Dowling", "email": "mtdowling@gmail.com"}],
        "require": {
          "php": "^7.2.5 || ^8.0",
          "ext-json": "*",
          "guzzlehttp/promises": "^1.5.3 || ^2.0.1",
          "guzzlehttp/psr7": "^1.9.1 || ^2.4.5",
          "psr/http-client": "^1.0",
          "symfony/deprecation-contracts": "^2.2 || ^3.0"
        },
        "autoload": {
          "psr-4": {"GuzzleHttp\\": "src/"}
        },
        "dist": {
          "type": "zip",
          "url": "https://api.github.com/repos/guzzle/guzzle/zipball/a52f0440...",
          "reference": "a52f0440...",
          "shasum": "fedcba9876543210..."
        },
        "source": {
          "type": "git",
          "url": "https://github.com/guzzle/guzzle.git",
          "reference": "a52f0440..."
        },
        "time": "2023-12-03T20:53:26+00:00"
      }
      ...
    ]
  }
}
```

Key fields:

- `dist.url`: the direct download URL for the dist zip. Usually a GitHub or GitLab tarball URL.
- `dist.shasum`: SHA-1 (historical) or SHA-256 (newer entries) of the dist zip. The bridge verifies SHA-256; where Packagist only provides SHA-1, the bridge computes and stores SHA-256 independently.
- `dist.reference`: the git commit SHA the dist was built from.
- `require`: the Composer dependency constraints for this version.
- `autoload.psr-4`: the PSR-4 namespace-to-directory mapping. Used by the bridge to synthesise the `vendor/autoload.php` map.

The v2 API returns all versions in a single response per package (unlike the v1 API which required paginated fetches or the full `packages.json` catalog). For packages with hundreds of versions (e.g., `symfony/console` with 100+ releases), the response can be 1-2MB; the bridge caches the response with an HTTP `If-Modified-Since` header.

## 2. Packagist v2 vs Cargo sparse index vs Go module proxy

All three are "lazy" metadata sources: they serve per-package metadata on demand rather than requiring the client to clone or download a full catalog. The comparisons:

| Feature | Packagist v2 | Cargo sparse index | Go module proxy |
|---------|-------------|-------------------|-----------------|
| Endpoint format | `/p2/<vendor>/<pkg>.json` (all versions in one JSON doc) | `/<crate-name>` (one entry per version, NDJSON) | `/<module>/@v/<version>.info`, `/@v/<version>.mod`, `/@v/<version>.zip` |
| Checksum field | SHA-1 (historical) + SHA-256 (newer) in response | SHA-256 in index entry | `h1:` (base64-sha256-of-zip) in sum.golang.org |
| Transparency log | None (Packagist has no checksum DB) | None (Cargo has no checksum log; RFC #3724 adds Rekor for signatures) | sum.golang.org (Merkle tree) |
| Auth for private packages | GitHub token for private dist URLs | `CARGO_REGISTRIES_*` env vars | `GOPROXY` + git credentials / `GOAUTH` |
| Immutability | Dist zip URLs can change (GitHub renames); shasum is the integrity anchor | .crate tarballs are immutable; content-addressed on crates.io | module .zip is immutable on proxy.golang.org once uploaded |
| Mirror protocol | `/packages.json` + metadata-changes endpoint | Sparse index mirrors via HTTP ETag | Athens-style GOPROXY mirrors |

Key difference from MEP-73 and MEP-74: Packagist has no transparency log. The SHA-256 recorded in `mochi.lock` is the bridge's own integrity anchor; there is no third-party cross-check equivalent to sum.golang.org or Cargo's Rekor attestation. This is the Packagist supply-chain gap documented in [[07-packagist-trusted-publishing-gap]].

## 3. Version constraint resolution

Composer uses a version constraint language slightly different from Cargo's:

| Constraint | Meaning |
|-----------|---------|
| `^7.8` | `>=7.8.0 <8.0.0` (same as Cargo) |
| `~7.8.0` | `>=7.8.0 <7.9.0` (patch-level only; differs from Cargo where `~7.8` means `>=7.8.0 <8.0.0`) |
| `7.8.1` | exact version |
| `>=7.0 <8.0` | range |
| `7.8.*` | `>=7.8.0 <7.9.0` |
| `*` | any version |

The `~` operator differs between Composer and Cargo: Composer's `~7.8.0` locks the patch, Cargo's `~7.8` allows any patch in minor 7.8. The bridge implements Composer's semantics for `[php-dependencies]` constraints.

For transitive dependency resolution, the bridge delegates to Composer's embedded solver:

```
composer update --no-install --dry-run --format=json
```

run in a temp workspace seeded with the user's `[php-dependencies]` constraints. The `--dry-run --format=json` output is a machine-readable resolution plan that the bridge parses and writes into `mochi.lock`. MEP-57's PubGrub solver is not used for PHP dependencies (Composer's solver understands PHP-specific semantics like `platform` packages and `ext-*` requirements); MEP-57's solver records the result from Composer as authoritative.

## 4. Content-addressed cache layout

Downloaded dist zips are stored in a content-addressed cache:

```
~/.cache/mochi/php-deps/
  fe/                         # first 2 hex chars of SHA-256
    fedcba9876543210.../      # SHA-256 hex prefix
      fedcba9876543210...zip  # the dist zip
      fedcba9876543210...json # the reflection CLI output for this zip
```

The key is the SHA-256 of the dist zip. Two identical packages at different URLs that happen to produce the same zip (e.g., a mirror of the same release) share a cache entry.

The reflection CLI output is cached alongside the zip. The SHA-256 of the reflection JSON is recorded in `mochi.lock` as `reflection-sha256`. A re-run of the reflection CLI on a cached zip (without re-downloading) is a cache hit.

## 5. Packagist dist URL stability

Packagist's `dist.url` points to GitHub (or GitLab, Bitbucket, etc.) archive URLs. These URLs are stable as long as the repository exists and the release tag is not deleted. However:

- Repository renames change the URL (`github.com/guzzle/guzzle` → `github.com/new-owner/guzzle`).
- Repository deletions break all dist URLs.
- GitHub API rate limits apply to unauthenticated dist downloads (`api.github.com` rate limit: 60 requests/hour without a token).

The bridge mitigates by caching the dist zip locally (content-addressed, so a URL change does not invalidate the cache if the content is the same). The bridge records the `dist.reference` (git commit SHA) in the lockfile alongside `dist-sha256`; a URL change with the same `dist.reference` and matching `dist-sha256` is treated as a clean rename.

For GitHub API rate limits, the bridge sets `Authorization: token $GITHUB_TOKEN` if the environment variable is set (standard CI practice). Unauthenticated downloads are limited but sufficient for CI in most cases.

## 6. Packagist webhook and GitHub App

Packagist discovers new package versions via two mechanisms:

**GitHub App**: if the repository owner installs the Packagist GitHub App, Packagist receives a webhook payload on every push and tag creation. The App automatically crawls the repository for new tags. This is the recommended integration for new packages.

**Update API**: a manual POST to `https://packagist.org/api/update-package?username=<user>&apiToken=<token>` triggers an immediate crawl of the package's registered repository URL. Used for CI pipelines that do not use the GitHub App.

MEP-75's publish flow uses the Update API as a fallback and recommends the GitHub App for registered repositories. See [[06-composer-publish-flow]] for the full publish flow.

## 7. Packagist mirror protocol

Packagist supports a mirror protocol for organisations that want a local or geo-distributed copy:

- The primary catalog endpoint (`/packages.json`) lists all packages and their metadata checksums.
- The metadata-changes endpoint streams incremental updates since a given timestamp.
- Dist zips are fetched on demand (mirrors do not pre-cache all dist zips; they cache on first request).

A local Packagist mirror can be run with the open-source `composer/satis` tool (a static Packagist mirror generator) or with `Private Packagist` (Packagist's commercial offering). The bridge supports custom mirrors via `[registry]` in the user profile (analogous to MEP-57's mirror support):

```toml
[registry.php]
packagist-url = "https://packagist.mirror.example.com"
```

## Cross-references

- [[05-type-mapping]] for what the bridge does with the reflection surface extracted from the downloaded package.
- [[10-build-system]] for the cache layout detail and the reflection CLI invocation.
- [[07-packagist-trusted-publishing-gap]] for the supply-chain implications of the missing transparency log.
- [MEP-75 §8](/docs/mep/mep-0075#8-packagist-v2-sparse-api) for the normative spec.
- [MEP-57](/docs/mep/mep-0057) for the broader content-addressed object store model.
