---
title: "Phase 1. Sparse-index client"
sidebar_position: 3
sidebar_label: "Phase 1. Sparse-index client"
description: "MEP-73 Phase 1 lands the cargo sparse-index client: bucket-path URL builder, NDJSON entry parser, HTTP fetch, SHA-256-verified content-addressed cache with BLAKE3-256 alias, plus a from-scratch cargo-flavoured semver parser."
---

# Phase 1. Sparse-index client

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 20:50 (GMT+7) |
| Landed         | 2026-05-29 21:06 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

Two packages land under `package3/rust/`:

- `package3/rust/semver/`: a cargo-flavoured semver parser. Implements semver 2.0.0 ordering (§11), the cargo leading-zero carve-out for the caret operator (`^0.x.y` and `^0.0.z`), tilde, wildcard, intersection (`>=1.2.3, <1.3.0`), and the cargo "pre-releases are opt-in" rule.
- `package3/rust/sparse/`: a sparse-index client. `BucketPath` computes the cargo path scheme (`1/`, `2/`, `3/<first>/`, `<first2>/<chars-3-4>/`), `CrateEntry` mirrors cargo's `RegistryPackage` JSON shape with `name`, `vers`, `deps`, `cksum`, `features`, `features2`, `yanked`, `links`, `v`, `rust_version`. `ParseIndex` reads NDJSON streams. `Client` issues HTTP requests against a configurable base URL (default `https://index.crates.io/`) with a `User-Agent` of `mochi-rust-bridge/0.1`. `Cache` is the on-disk content-addressed store: index files land under `<root>/registry/index/<host>/<bucket>/<name>`, `.crate` tarballs land under `<root>/registry/cache/<host>/<name>-<version>.crate` (cargo-compatible layout), and a BLAKE3-256 alias is hard-linked under `<root>/registry/blake3/<first2>/<rest>.crate`. `StoreCrate` streams through SHA-256 and BLAKE3 hashers simultaneously, verifying the SHA-256 against the index `cksum` and recording the BLAKE3 digest for Mochi's own lockfile schema.

## Lowering decisions

### Cargo-flavoured semver, hand-rolled

Mochi already ships a `semver` package for Mochi modules, but it follows semver.org strictly. Cargo deviates in three load-bearing ways:

- Bare requirements (`"1.2.3"`) are caret operators, not exact pins.
- The caret operator has a leading-zero carve-out: `^0.2.3 => >=0.2.3, <0.3.0`, `^0.0.3 => >=0.0.3, <0.0.4`.
- Pre-releases are opt-in: `^1.0.0` does not match `1.0.0-alpha` unless the requirement itself names a pre-release on the same major/minor/patch.

Reusing the existing Mochi-flavoured semver would have meant translating both ways for every resolver call, and the translation surface (caret carve-outs, pre-release matching, intersection semantics) is exactly where bugs hide. The cost of a dedicated package is one file each for parsing and matching. The tests cover the semver.org §11 ordering chain (alpha < alpha.1 < alpha.beta < beta < beta.2 < beta.11 < rc.1 < 1.0.0), build metadata is ignored for equality, and the cargo carve-outs are pinned with explicit case lists.

### Bucket-path scheme

The path scheme is `1/<n>` (1-char), `2/<n>` (2-char), `3/<first>/<n>` (3-char), `<first2>/<chars-3-4>/<n>` (4+ char). The crate name is lowercased before bucketisation (cargo is case-insensitive for index lookup) but the hyphen-vs-underscore choice is preserved literally in the path: `foo-bar` and `foo_bar` are separate index entries because the registry rejects collisions at publish time. `CrateNameEqual` reports whether two names resolve to the same crate under cargo's lookup rules (fold case, fold `-`/`_`); use it whenever a user-supplied name needs to be matched against an index entry.

### Entry parser

`ParseIndex` reads NDJSON via `bufio.Scanner` with a 2 MiB max line size. Blank lines are tolerated (some mirrors trail with a newline). Each `CrateEntry` includes the cargo schema version `V`: a v2 entry adds `features2`, which `MergedFeatures` merges over `features` (Features2 wins on collision, per cargo's `cargo/src/cargo/sources/registry/index.rs`). `SortEntriesByVersion` is the deterministic sort, with a non-yanked-first tiebreaker at equal versions. `LatestMatching(entries, req)` skips yanked entries, parses each `vers`, and returns the highest semver match.

### Client

`Client` is a thin HTTP wrapper. `BaseURL` is normalised to end in `/`. `IndexURL` resolves the bucket path against the base URL with `net/url.ResolveReference` so that mirrors hosted on sub-paths (e.g. an internal proxy at `https://mirror.corp/cargo/`) work without extra plumbing. `FetchIndex` returns `ErrCrateNotFound` (errors.Is-friendly) on HTTP 404 and a generic error on other non-200 responses. `FetchCrate` writes the tarball bytes to a caller-supplied `io.Writer` without verifying; verification happens in `Cache.StoreCrate` so the bytes never leave the hashing pipeline.

### Cache layout

The on-disk layout intentionally mirrors `$CARGO_HOME/registry/cache/<host>/<name>-<version>.crate` so that Phase 7's build orchestration can hard-link from Mochi's cache into `$CARGO_HOME` and let cargo find the tarballs without re-downloading. The BLAKE3 alias under `<root>/registry/blake3/` lets Phase 8's `mochi.lock` integrity field be either `sha256-<hex>` or `blake3-<hex>` without an extra download round trip, because both digests are computed in a single `io.MultiWriter` pass during `StoreCrate`. The alias is hard-linked best-effort; on cross-device filesystems it falls back to a byte copy, and an alias failure is non-fatal because the canonical path is the cargo-compatible one.

`StoreCrate` writes to a sibling `.crate-*.tmp` file, hashes as it goes, verifies the SHA-256 against the expected hex from the index, and only renames into place on success. On checksum mismatch the partial file is removed and `ErrChecksumMismatch` is returned (errors.Is-friendly), so a single bad fetch cannot poison the cache. `HasCrate` reports presence of the final renamed path, which is sufficient because rename-after-verify is atomic on POSIX file systems.

`StoreIndex` similarly uses tmp-then-rename so a partially written index file is never visible. `LoadIndex` returns `os.ErrNotExist` (errors.Is-friendly) when no cached copy exists. `Cache.FetchIndex(ctx, client, name)` is the convenience method that fetches via the client, persists the bytes, and returns the parsed entries in one call.

## Files changed

| File | Purpose |
|------|---------|
| `package3/rust/semver/version.go` | `Version`, `Parse`, `ParseRelaxed`, `MustParse`, `Compare`, `Equal`, `IsPrerelease` |
| `package3/rust/semver/req.go` | `Req`, `ParseReq`, `MustParseReq`, `Matches`, `MaxSatisfying`, caret/tilde/wildcard/inequality expanders |
| `package3/rust/semver/version_test.go` | semver.org §11 ordering, build-metadata ignored for equality, invalid-input rejection |
| `package3/rust/semver/req_test.go` | caret carve-out cases, tilde, wildcard, exact, inequality, intersection, pre-release opt-in, `MaxSatisfying` |
| `package3/rust/sparse/path.go` | `BucketPath`, `ValidateCrateName`, `CrateNameEqual` |
| `package3/rust/sparse/entry.go` | `CrateEntry`, `CrateDep`, `ParseIndex`, `MergedFeatures`, `SortEntriesByVersion`, `LatestMatching` |
| `package3/rust/sparse/client.go` | `Client`, `NewClient`, `IndexURL`, `FetchIndex`, `DownloadURLFor`, `FetchCrate`, error sentinels |
| `package3/rust/sparse/cache.go` | `Cache`, `NewCache`, `IndexPath`, `CratePath`, `Blake3Path`, `StoreIndex`, `LoadIndex`, `StoreCrate`, `HasCrate`, `FetchIndex` |
| `package3/rust/sparse/http.go` | shared `getWithUA` helper and the `bytesReader` shim |
| `package3/rust/sparse/path_test.go` | bucket scheme + name validation + case/separator folding |
| `package3/rust/sparse/entry_test.go` | NDJSON fixture, blank-line tolerance, schema-v2 merge, `LatestMatching` w/ yanked skip |
| `package3/rust/sparse/client_test.go` | httptest server, 404 → `ErrCrateNotFound`, 500 → generic error, download URL templating, `ErrCrateVersionNotFound` |
| `package3/rust/sparse/cache_test.go` | path layout, SHA-256 verification, mismatch removes partial, BLAKE3 alias exists, overwrite on re-store |

## Test set

- All `package3/rust/semver/...` unit tests.
- All `package3/rust/sparse/...` unit tests.

Total: 39 test functions, all green via `go test ./package3/rust/...`.

## Closeout notes

Phase 1 introduces one new runtime dependency: `lukechampine.com/blake3` (already an indirect transitive dep in the repo's `go.sum`, now used directly). No other external dependencies are needed; the HTTP client is `net/http` and the cache uses only `os`/`io`/`crypto`.

The bridge between Mochi semver requirements and cargo semver requirements is now closed: Phase 2 (rustdoc-JSON ingest) and Phase 6 (`import rust "..."` grammar) will both translate user requirements to cargo `Req` values before resolving against `LatestMatching`. The cache layout is fixed early so Phase 7 (build orchestration) can rely on the `<root>/registry/cache/...` path without renegotiation, and Phase 8 (`mochi.lock` integration) can rely on the `<root>/registry/blake3/...` alias being populated whenever `StoreCrate` succeeds.

Pre-release matching is the most subtle piece: `^1.0.0` does not match `1.0.0-alpha`, but `>=1.0.0-alpha` does. The implementation treats a pre-release as a satisfier only when at least one comparator in the requirement names the same (major, minor, patch) with a pre-release identifier, which is what cargo's `semver` crate does. The test suite pins this with both positive (`>=1.0.0-alpha` matches `1.0.0-alpha`) and negative (`^1.0.0` does not match `2.0.0-alpha`) cases.
