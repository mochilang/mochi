---
title: "Registry index: sparse HTTPS protocol"
description: "Sparse index URL scheme, JSONL entry format, HTTP semantics, retry strategy, mirror discovery, yank flow, search and metadata, authentication model, rate limiting, error codes."
sidebar_position: 7
---

# 07. Registry index: sparse HTTPS protocol

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/registry-index`.

This note specifies the sparse HTTPS registry index protocol Mochi-57 ships in `pkg/pkgregistry/sparse/` (the working name in early drafts was `pkg/pkgindex/`; phase docs 0, 2, 7, 8, 11, 18 canonicalize the implementation under `pkg/pkgregistry/` and there is no `pkg/pkgindex/` directory at v1). The choice of sparse-over-git is in [02-design-philosophy](./02-design-philosophy) §4; the comparison with Cargo's 2023 GA migration is in [03-prior-art-registries](./03-prior-art-registries) §1.

## 1. Why a sparse index

Two surface choices for a registry index:

1. **Git-based** (Cargo legacy, Hex's mirror): the index is a git repo. Clients clone it on first use and pull updates. As the index grows, clone cost grows; Cargo's legacy index reached ~1.5GB by 2023 with O(30s) cold-clone latency on residential connections.
2. **Sparse HTTPS** (Cargo since 1.68, npm always, JSR, uv): the index is a per-package endpoint served over HTTPS. Clients fetch only what they need.

Sparse wins on:

- **Cold-cache latency**: a manifest with 20 deps issues 20 HTTPS GETs in parallel; pre-HTTP/2, that was a problem. With HTTP/2 multiplexing (universal since 2018), parallel GETs over a single TCP connection are ~constant cost. Cargo's published benchmark shows 5-20x improvement.
- **Bandwidth**: a user with 20 deps does not download the index entries for 250k other crates.
- **Cache invalidation**: ETag and `If-None-Match` work on per-package endpoints with no protocol gymnastics.
- **Mirror operation**: a sparse index mirror is a transparent HTTPS reverse proxy, not a git proxy.
- **Federation**: distinct sparse-index hosts compose via URL prefixes; no git remote choreography.

Sparse loses on:

- **Discoverability**: there is no single git log of "new versions published this week". The registry exposes a sidecar firehose endpoint (`/feed.jsonl`) for that.

MEP-57 ships sparse from v1. There is no legacy git index to migrate from.

## 2. URL scheme

```
https://index.mochi.dev/<bucket>/<scope>/<name>
```

Where `<bucket>` is the first two characters of `<name>` (lowercase). This sharding matches Cargo's `index.crates.io/<bb>/<bb>/<name>` shape and lets a CDN front the index efficiently.

For unscoped packages: `<scope>` is the literal `-` (a sentinel meaning "no scope"). Full path: `https://index.mochi.dev/da/-/datalog`.

For scoped packages: `<scope>` is the scope without the `@` prefix. Full path: `https://index.mochi.dev/st/mochi/strings`.

### 2.1 Bucket determination

```go
func bucket(name string) string {
    n := strings.ToLower(name)
    switch {
    case len(n) >= 4: return n[:2] + "/" + n[2:4]
    case len(n) >= 2: return n[:2] + "/" + n[:2]   // pad: same prefix
    case len(n) == 1: return n + "/-"
    default:          return "-/-"
    }
}
```

This is two-character pairs deep matching Cargo's `/aa/bb/<name>` layout: spreads packages across a fanout of up to 65,536 directories so any single directory's listing stays bounded.

## 3. Index entry format

Each per-package endpoint returns line-delimited JSON, one line per version, in semver descending order:

```jsonl
{"v":"1.2.5","r":"2026-05-20T12:00:00Z","b3":"e2d1...","s2":"abf3...","y":false,"c":["fs.read"],"d":{"@mochi/strings":"^0.4"},"t":["typescript","python","jvm"]}
{"v":"1.2.4","r":"2026-05-18T09:00:00Z","b3":"71f0...","s2":"82a1...","y":false,"c":["fs.read"],"d":{"@mochi/strings":"^0.4"},"t":["typescript","python","jvm"]}
{"v":"1.2.3","r":"2026-05-10T18:00:00Z","b3":"5c2a...","s2":"4d31...","y":true,"yr":"security: see GHSA-xxxx","c":["fs.read"],"d":{"@mochi/strings":"^0.4"},"t":["typescript","python","jvm"]}
```

Field abbreviations:

| Key   | Meaning                                                     |
|-------|-------------------------------------------------------------|
| `v`   | Version (semver)                                            |
| `r`   | Release timestamp (RFC 3339)                                |
| `b3`  | BLAKE3-256 hex of the tarball                               |
| `s2`  | SHA-256 hex of the tarball                                  |
| `y`   | Yanked flag                                                 |
| `yr`  | Yank reason (present if `y == true`)                        |
| `c`   | Capabilities required                                       |
| `d`   | Dependencies map: name -> version range                     |
| `t`   | Targets supported                                           |
| `dv`  | dev-dependencies (only present for full-mode endpoints; see §6) |
| `cf`  | conditional features (only present for full-mode endpoints)    |
| `mp`  | Mochi compiler range (e.g. `">=0.7, <1.0"`)                  |
| `ed`  | Edition (e.g. `"2026"`)                                      |
| `pr`  | Provenance: `{"sig":"<sigstore bundle url>"}`               |
| `lk`  | License (SPDX)                                               |

Line-delimited JSON matches Cargo's sparse index format. The abbreviations keep entries small (~300 bytes typical) so a heavily-versioned package's full history fits in <100KB.

## 4. HTTP semantics

### 4.1 GET

`GET https://index.mochi.dev/<bucket>/<scope>/<name>` returns:

- **200 OK** with the JSONL body if the package exists.
- **404 Not Found** if the package does not exist.
- **410 Gone** if the package has been removed (rare; reserved for legal takedowns).

Headers:

- `Content-Type: application/x-mochi-index+jsonl; charset=utf-8`
- `ETag: "<hash>"` (strong; weak ETags rejected by client)
- `Last-Modified: <RFC 7231 date>`
- `Cache-Control: public, max-age=300, stale-while-revalidate=86400`

### 4.2 Conditional GET

`If-None-Match: "<etag>"` returns **304 Not Modified** if the index has not changed. This is the common case for incremental work; clients store the ETag in `~/.cache/mochi/registry/etags/<bucket>/<scope>/<name>`.

### 4.3 HEAD

`HEAD https://index.mochi.dev/<bucket>/<scope>/<name>` returns headers only. Used by `mochi lock --check` to verify the ETag matches without fetching the body.

### 4.4 Retries

Clients implement exponential backoff with jitter:

```
base = 250ms; cap = 30s
on attempt N (1..6): sleep min(cap, base * 2^(N-1)) +- 25% jitter
```

A `503 Service Unavailable` or `429 Too Many Requests` triggers retry. A `Retry-After` header overrides the backoff calculation.

After 6 attempts, the resolver records an incompatibility ("could not fetch metadata for `<pkg>`") and reports it as a solver failure with context.

### 4.5 Connection budget

Per-host connection budget defaults to 8 parallel HTTP/2 streams. Configurable via `MOCHI_INDEX_PARALLELISM`. Concurrent fetches across packages reuse a single HTTP/2 connection per host.

## 5. Object store

The tarballs themselves live at a separate host:

```
https://blobs.mochi.dev/<blake3-hex>
```

A blob is fetched by its BLAKE3-256 hex. The body is the `.mochi.tar.zst` archive. Headers:

- `Content-Type: application/vnd.mochi.tarball+zstd`
- `ETag: "<blake3-hex>"` (the hash IS the etag)
- `Cache-Control: public, max-age=31536000, immutable` (content-addressed; cache forever)
- `X-Mochi-Sha256: <hex>` (cross-ecosystem hash for SLSA / Sigstore)
- `X-Mochi-Sigstore-Bundle: <url>` (pointer to the Sigstore bundle)

The client verifies the BLAKE3 matches after download. A mismatch fails the install with `M057_BLOB_E001` (hash mismatch).

See [08-content-addressed-store](./08-content-addressed-store) for the integrity model.

## 6. Optional `?full=1` mode

For tooling that wants the full per-version manifest (dev-deps, optional features, etc.), the endpoint supports `?full=1`:

```
GET https://index.mochi.dev/<bucket>/<scope>/<name>?full=1
```

The body is JSONL with full fields populated. `mochi info <pkg>` uses this; the solver does not (it only needs the prod-deps + capabilities + targets).

The `?full=1` body can be ~5x larger; the solver path is bandwidth-aware and skips it. Mirrors are encouraged to cache the full mode separately.

## 7. Filesystem-backed registry

For local development and offline use, MEP-57 supports `file://` registries:

```
file:///opt/mochi-cache/<bucket>/<scope>/<name>
```

The filesystem layout mirrors the URL scheme. Each per-package file is the same JSONL content. Blobs live at `file:///opt/mochi-cache/blobs/<blake3-hex>`.

The filesystem-backed implementation is the test backend for the solver and the implementation for `mochi vendor`'s offline cache. See [12-risks-and-alternatives](./12-risks-and-alternatives) §14 for the network-degradation discussion.

## 8. Mirrors

The registry mirror protocol allows a third party to operate `https://mirror.example/`. A mirror serves the same URL scheme; clients consult mirrors via `[[registry.alternate]]` in `mochi.toml` (see [04-manifest-format](./04-manifest-format) §9).

A mirror's responsibility:

1. Periodically replicate the upstream index.
2. Serve identical JSONL bytes.
3. Serve identical blobs (by content hash, dedup trivial).
4. Forward Sigstore bundle URLs unchanged.

Mirror integrity: the consumer verifies the Sigstore bundle against the published OIDC identity regardless of mirror. A malicious mirror cannot forge a signed bundle for a different artifact because Sigstore's transparency log (Rekor) records the upstream publish event.

The mirror discovery flow uses the `[[registry.alternate]]` config; there is no DNS-based auto-discovery in v1.

## 9. Yanking

Yank: a version remains downloadable but is excluded from new resolutions:

```
POST https://index.mochi.dev/<pkg>/yank
Authorization: Bearer <Sigstore-OIDC-bound publish credential>
Body: { "version": "1.2.3", "reason": "security: see GHSA-xxxx" }
```

The index entry's `y` flag flips to `true` and `yr` is populated. Existing lockfiles continue to install (yanked artifacts remain in the blob store) but new resolutions skip yanked versions.

Cargo's yank model is the prior art; npm calls this "deprecate" (different semantics: deprecate is informational, never blocks resolution).

## 10. Search and metadata

`mochi search <term>` queries a separate metadata service at `https://index.mochi.dev/_search?q=<term>`. Returns a list of `{name, version, description, score}` entries.

`mochi info <pkg>` queries `https://index.mochi.dev/_info/<scope>/<name>` for a richer metadata view: README, authors, downloads, ranked deps. This is the human-facing endpoint; the solver does not use it.

Both are out-of-band of the sparse index proper. Mirrors can implement or skip these endpoints; the solver does not depend on them.

## 11. Authentication

For public reads: anonymous HTTPS. No auth required.

For publish (POST endpoints): Sigstore-OIDC bound credential. See [09-trusted-publishing](./09-trusted-publishing).

For private registries: HTTP Basic + token, or mTLS, per mirror operator policy. Mochi's client supports both via `mochi config registry <name> token=...` or `mochi config registry <name> cert=...`. Long-lived tokens for *publish* are forbidden by central registry policy; private registries may set their own policy.

## 12. Rate limiting

Anonymous reads: 60 req/sec per IP, 1000 req/min sustained. ETag traffic does not count (304 responses are free). A `429 Too Many Requests` with `Retry-After` is returned on excess.

Publish: 10 publishes per OIDC identity per minute. Yanks: 10 per identity per minute.

These limits are policy, not protocol; mirrors set their own.

## 13. Operational considerations

A typical resolution: ~30 packages, each 1 ETag-conditional GET. Steady-state: 30x 304 responses, ~1KB headers each, ~30KB total. Cold cache: 30x 200 responses, ~5KB body each, ~150KB total. With HTTP/2 multiplexing this completes in 1-2 round trips.

For comparison, Cargo's sparse index migration documented similar numbers in the original 1.68 announcement; the protocol is well-validated at scale.

CDN-frontable: the URL scheme is cache-friendly (per-package paths, per-blob immutable URLs), so a CloudFront / Cloudflare / Fastly front-end works without modification.

## 14. Error codes

| Code              | Meaning                                                   |
|-------------------|-----------------------------------------------------------|
| `M057_INDEX_E001` | Index fetch failed after retries.                         |
| `M057_INDEX_E002` | Index entry malformed.                                    |
| `M057_INDEX_E003` | Index entry rejects unknown field (forward-compat issue). |
| `M057_INDEX_E004` | ETag mismatch on conditional re-fetch.                    |
| `M057_INDEX_E005` | Blob hash mismatch.                                       |
| `M057_INDEX_E006` | Mirror returned diverging content (consumer detects via Sigstore). |

## 15. Cross-references

- Sparse-over-git rationale: [02-design-philosophy](./02-design-philosophy) §4.
- Cargo 2023 GA as prior art: [03-prior-art-registries](./03-prior-art-registries) §1.
- Solver fetching this index: [05-solver-design](./05-solver-design) §8.
- Content-addressed blob protocol: [08-content-addressed-store](./08-content-addressed-store).
- Sigstore verification flow: [09-trusted-publishing](./09-trusted-publishing).
- Mirror federation discussion: [12-risks-and-alternatives](./12-risks-and-alternatives) §14.
