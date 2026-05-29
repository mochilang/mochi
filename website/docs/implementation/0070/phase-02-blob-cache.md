---
title: "Phase 02: Blob cache"
sidebar_position: 4
sidebar_label: "Phase 02: Blob cache"
description: "Content-addressed JAR/POM/AAR fetch with SHA-256 + BLAKE3 verification and atomic writes under ~/.cache/mochi/kotlin-deps/."
---

# Phase 02: Blob cache

**Status:** Planned

## Deliverables

1. `package3/kotlin/blob/store.go` — content-addressed object store.
   - `Fetch(coord Coordinate, url string) (path string, sha256 [32]byte, blake3 [32]byte, err error)` — download the artifact if not cached, verify hashes, return the local path.
   - `Get(sha256 [32]byte) (path string, ok bool)` — lookup by SHA-256.
   - `CachePath(sha256 [32]byte) string` — return the canonical path for a given hash.
2. `package3/kotlin/blob/verify.go` — streaming SHA-256 + BLAKE3 dual-hash verification (matches MEP-73 Rust bridge's pattern).
3. `package3/kotlin/blob/fetch.go` — HTTP download with retry (3 attempts, exponential backoff), `Content-Length` + `Content-Type` validation, partial download detection.

## Cache layout

```
~/.cache/mochi/kotlin-deps/
  blobs/
    sha256/
      ab/
        ab12cd34ef56...  (first 2 hex chars = directory shard)
  meta/
    org.jetbrains.kotlinx/
      kotlinx-coroutines-core/
        1.7.3.json       (records sha256, blake3, jar-path, pom-path, fetched-at)
```

The blob store is shared across all Mochi projects on the machine (identical to the Rust bridge's blob store under `~/.cache/mochi/rust-deps/`).

## Gate

Round-trip all 20 corpus artifacts:
1. Cold fetch: download each artifact from Maven Central, verify SHA-256 and BLAKE3.
2. Warm fetch: second call returns cached path without HTTP request.
3. Tamper detection: modify one byte in a cached blob; verify the next `Fetch` call detects the mismatch and re-downloads.
4. Network error: simulate 500 response; verify retry logic and final failure with `ErrFetchFailed`.
