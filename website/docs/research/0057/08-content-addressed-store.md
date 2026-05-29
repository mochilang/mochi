---
title: "Content-addressed object store: BLAKE3 plus SHA-256"
description: "Dual-hash rationale, blob tarball format, publish-time hashing, consumer-side verification, cache layout, capacity and economics, garbage collection, backup and replication, integrity-only fallback, cryptographic agility, IPFS rejected, benchmarks."
sidebar_position: 8
---

# 08. Content-addressed object store: BLAKE3 + SHA-256

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/content-addressed-store`.

This note specifies the content-addressed blob store Mochi-57 ships in `pkg/pkgblob/`. The dual-hash rationale is in [02-design-philosophy](./02-design-philosophy) §4; the registry HTTP protocol is in [07-registry-index](./07-registry-index).

## 1. What "content-addressed" means here

Every published artifact is keyed by a hash of its bytes. The URL `https://blobs.mochi.dev/<blake3-hex>` is "the blob whose BLAKE3-256 hash is `<blake3-hex>`". Given an address, the client downloads the blob and recomputes the hash; if it does not match, the blob is rejected.

This is the model of Nix's `/nix/store/<hash>-<name>` (2003), git's object database (2005), Cargo's `.crate` cache (2015), and IPFS (2015). The 20+ years of prior art has earned the model unambiguous adoption.

Two properties follow:

1. **Tamper-evident**: any in-flight modification (CDN cache poisoning, malicious mirror, ISP injection) changes the hash and is detected at install time.
2. **Dedup-friendly**: a published artifact's hash is its identity; two packages embedding the same dependency tarball share storage.

## 2. Why BLAKE3 primary + SHA-256 secondary

A *primary* hash is what the URL is keyed by; a *secondary* hash is what cross-ecosystem tooling speaks. Cross-ecosystem tools need SHA-256 because:

- **Sigstore / Rekor** logs the SHA-256 of the signed artifact.
- **SLSA** provenance v1 specifies SHA-256 as the canonical content hash.
- **npm Trusted Publishing** publishes provenance with SHA-256.
- **PyPI PEP 740** records SHA-256.
- **Maven Central** records SHA-256 (plus SHA-1 legacy).
- **GitHub artifact attestations** use SHA-256.

If we keyed only by BLAKE3, every cross-ecosystem integration would have to recompute SHA-256 client-side, and our supply-chain story would diverge from the SLSA / Sigstore canonical hash. So SHA-256 is mandatory as a secondary.

We chose BLAKE3 as the primary because:

- **Speed**: BLAKE3 is several times faster than SHA-256 on modern CPUs (Intel/AMD with SHA extensions are roughly comparable but ARM and x86 without SHA extensions favour BLAKE3 by 3-5x).
- **Parallelism**: BLAKE3 is internally parallel (tree hash). Large artifacts hash on multiple cores.
- **Secure**: SHA-3 / Keccak family with modern construction; no known weaknesses 2026.
- **Forward-looking**: Cargo migrated to BLAKE3 for its internal cache in 2024; the precedent is set.

Two-hash overhead is small: BLAKE3 of a 1MB artifact on a 2024-vintage laptop is ~3ms; SHA-256 ~10ms. Total dual-hash cost ~13ms, dominated by the SHA-256, which we cannot drop for the reasons above.

If BLAKE3 is later found weak, the URL scheme migrates via a one-time rewrite (blobs are content-addressed; the rewrite is a hash recomputation, not a re-publish). SHA-256 stays as the secondary across migrations.

## 3. Blob format

The blob is a `.tar.zst` archive: tar (POSIX 1003.1-2008 pax format) compressed with zstd level 19.

Why tar.zst:

- **Tar**: simplest archive format with mature tooling; reproducible builds in tar with `mtime=0` and sorted entries are well-understood (see [implementation Phase 17](/docs/implementation/0057/phase-17-repro)).
- **Zstd**: 2-3x faster decompression than gzip at comparable ratios; the entire 2024-2026 trend has been zstd adoption (Cargo's `.crate` migrated 2023; Docker images native zstd 2024; npm RFC 752 proposes optional zstd 2025). Level 19 is the publish-side compression level (high ratio, slow); decompression speed is unaffected by level.

The tar contents are deterministic:

- Entries sorted by path (UTF-8 NFC, lexicographic).
- Every entry's `mtime = 0` (or `SOURCE_DATE_EPOCH` if set, for trust-preserving rebuilds).
- Every entry's `uid = 0`, `gid = 0`, `uname = ""`, `gname = ""`.
- Every entry's `mode` masked to `0644` for files, `0755` for directories.
- No symlinks or device files.

These rules make `mochi pack` byte-deterministic given the same inputs. Reproducible builds (Phase 17) verify this with two CI hosts producing byte-identical output.

## 4. Blob contents

A published `.mochi.tar.zst` contains:

```
mochi.toml                          # the manifest, with [provenance] populated
mochi.lock                          # the producer's lockfile, advisory only
src/...                             # Mochi source tree
README.md                           # if present in [package].readme
LICENSE                             # if present
CHANGELOG.md                        # if present
.mochi-pkg/
  metadata.json                     # canonical metadata digest (matches index entry)
  capabilities.json                 # declared capability set + justifications
  sbom.cdx.json                     # CycloneDX 1.6 SBOM (Phase 15)
  sbom.spdx.json                    # SPDX 3.0 SBOM (Phase 15)
  attestation.intoto.jsonl          # in-toto attestation (Phase 15)
```

Excluded by default:

- `.git/`, `.svn/`, `.hg/`, `.bzr/`
- `node_modules/`, `__pycache__/`, `target/`, `build/`, `dist/`
- `*.pyc`, `*.pyo`, `*.class`, `*.o`, `*.so`, `*.dll`
- Anything in `.mochiignore` (gitignore-shaped)
- Any file whose path matches a security-sensitive pattern (`.env`, `*.pem`, `id_rsa`, etc.)

Inclusion overrides via `[package.include]` in `mochi.toml`; exclusions via `[package.exclude]`.

## 5. Publish-time hashing

The publish pipeline:

1. Build the tarball deterministically (sorted, mtime=0, etc.).
2. Compress with zstd level 19.
3. Compute BLAKE3-256 → `<b3>`.
4. Compute SHA-256 → `<s2>`.
5. Build the Sigstore bundle: sign the SHA-256 with the OIDC-bound Fulcio certificate.
6. POST the bundle and the tarball to the registry's publish endpoint.

The registry verifies:

1. BLAKE3 of received body matches the URL hex (catches transport corruption).
2. SHA-256 matches the bundle's signed claim.
3. Sigstore certificate chain is valid against Fulcio's roots.
4. OIDC identity in the certificate maps to a registered publisher for the package.

If all pass, the blob is stored at `<b3>` and the index entry is appended.

## 6. Consumer-side verification

On `mochi fetch`:

1. The lockfile lists `<b3>` and `<s2>` for each package.
2. Fetch `https://blobs.mochi.dev/<b3>`.
3. Stream-hash the body with BLAKE3 and SHA-256 simultaneously.
4. On end-of-stream, compare both hashes to the lockfile pins. Mismatch → reject with `M057_BLOB_E001`.
5. Verify the Sigstore bundle against the SHA-256 → `M057_BLOB_E002` on failure.
6. Decompress and extract.

Streaming hash + decompression run concurrently; the consumer never holds the full tarball in memory.

## 7. Cache layout

```
~/.cache/mochi/
├── registry/
│   ├── index/
│   │   └── <bucket>/<scope>/<name>            # cached JSONL index entry
│   │   └── <bucket>/<scope>/<name>.etag       # ETag for conditional fetch
│   └── blobs/
│       └── <b3-first-2-hex>/<b3-hex>          # cached blob, content-addressed
├── extracted/
│   └── <b3-hex>/                              # extracted tree, eagerly created on first use
└── locks/
    └── <name>-<version>.lock                  # OS-level fcntl lock for concurrent installs
```

The blob layout shards by the first two hex chars to keep any directory's listing bounded. Deduplication is automatic: two packages including the same blob share the cached file. Extraction is per-blob; multiple instances of the same blob extracted under distinct workspace paths share the extracted tree via symlink or hard link (configurable per-OS).

Cache lifetime: blobs and extracted trees are kept indefinitely. `mochi cache prune` GCs unused entries; `mochi cache clean` wipes everything.

## 8. Capacity and economics

Typical artifact sizes (tarball post-zstd):

- Small library (10 source files, no deps): ~5KB.
- Medium library (100 files, one transitive dep): ~50KB.
- Large library (1000 files, framework-scale): ~500KB to ~5MB.

A long-tail of all-historical-versions for the registry at 100k packages with median 20 versions each: 2M blobs averaging 50KB = 100GB. Operationally trivial for an S3-class store.

CDN economics: blobs are immutable (the URL is content-addressed), so cache hit rates are extremely high. A 90%+ CDN hit rate is achievable on free-tier providers; egress costs scale with the active mirror population, not raw downloads.

## 9. Garbage collection at the registry side

The registry never deletes a blob a current lockfile may reference. Yanked versions remain downloadable. The deletion policy:

- A yanked version's blob remains for 5 years past yank.
- A legal takedown (DMCA, court order) is the exception; the blob is replaced with a tombstone (`410 Gone` response). The yank trail records the takedown.
- A blob's `Last-Verified-At` timestamp updates on every Sigstore re-verification (operational integrity check, not consumer-visible).

This matches Cargo's policy (yanked crates never deleted) and npm's (deprecated packages never deleted).

## 10. Backup and replication

The blob store is replicated:

- Primary: `blobs.mochi.dev` (S3-class object store with CDN front).
- Secondary: a daily-synced backup to a second cloud provider.
- Tertiary: opt-in mirrors run by users, federation TBD in v2.

Consumers can ask `mochi audit blobs` to verify that the BLAKE3 in their cache matches the registry's current BLAKE3. A mismatch is a forensic signal (cache poisoning or registry compromise); the audit command writes a report and exits non-zero.

## 11. Integrity-only fallback (offline mode)

`mochi fetch --offline` resolves only against the cache: no network calls. Each lookup verifies the BLAKE3 in the cache matches the lockfile pin. A miss fails the install with `M057_OFFLINE_E001` listing the missing blobs.

`mochi vendor <dir>` copies the entire resolved tree's blobs and extracted trees under `<dir>/.mochi-vendor/`. The vendored directory is consumed by `mochi build --vendor=<dir>`, again offline.

## 12. Cryptographic agility

A future Mochi version may add SHA-3 or BLAKE-NG as a tertiary hash. The index entry format reserves `h3` for this. v1 ships with `b3` + `s2` only; consumers that see unknown hash fields ignore them.

The transition path for a primary-hash migration: re-key the blob URL scheme (e.g. add `https://blobs.mochi.dev/h3/<hex>`), keep the old URL scheme accessible for legacy lockfiles, and require new lockfiles to record both. The current lockfile schema supports this via the optional `h3` field.

## 13. Why not Merkle DAG / IPFS

IPFS is a content-addressed network with built-in federation. We considered it and rejected for v1:

- **Operational complexity**: running a registry on IPFS requires IPFS node infrastructure; the centralised HTTPS path is much simpler.
- **Latency**: IPFS lookups have multi-second p95s without pinning; the user experience is worse than HTTPS.
- **CDN**: HTTPS over a major CDN is universally fast; IPFS HTTPS gateways are slower and inconsistent.
- **Auditability**: a centralised registry has a clear write log (Rekor + index ETag); IPFS's distributed write log is harder to audit.

A future v2 candidate is *export to IPFS as a mirror*: the blob URL contract makes this trivial because the URL is already content-addressed.

## 14. Performance benchmarks (target)

A 50-dep cold install on a residential gigabit connection:

- Index fetch: 50x conditional GETs over HTTP/2, ~200ms.
- Blob fetch: 50x GETs in parallel, ~2s for ~10MB total.
- BLAKE3 + SHA-256 on each: ~5ms x 50 = 250ms total (parallel).
- Sigstore bundle verify: ~10ms each, ~500ms total.
- Decompress + extract: ~2s.

End-to-end cold install: ~5s for 50 deps, ~10MB of source. Warm install (lockfile + cache hit): <100ms.

These numbers match Cargo's published sparse-index performance for comparable graphs.

## 15. Cross-references

- Dual-hash rationale: [02-design-philosophy](./02-design-philosophy) §4.
- Comparable stores in survey: [03-prior-art-registries](./03-prior-art-registries) §1 (Cargo), §8 (Nix).
- Registry URL scheme: [07-registry-index](./07-registry-index).
- Sigstore bundle format: [09-trusted-publishing](./09-trusted-publishing).
- Reproducible build details (mtime=0, sorting): [02-design-philosophy](./02-design-philosophy) §4 + MEP-57 Phase 17.
- Cache-related risks (poisoning, eviction): [12-risks-and-alternatives](./12-risks-and-alternatives) §8.
