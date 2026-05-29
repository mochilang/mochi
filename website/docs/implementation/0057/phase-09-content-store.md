---
title: "Phase 9. Content-addressed object store"
sidebar_position: 10
sidebar_label: "Phase 9. Content store"
description: "MEP-57 Phase 9 — BLAKE3-256 primary + SHA-256 secondary blob fetch and verification, streaming dual-hash, cache layout, content-addressed dedup."
---

# Phase 9. Content-addressed object store

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 9](/docs/mep/mep-0057#phase-9-content-store) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase9ContentStore`: every blob fetch computes BLAKE3 and SHA-256 in one streaming pass; cache hit on second fetch is O(1) ms; a poisoned blob is rejected with `M057_BLOB_E001`; a SHA-256 mismatch with the index-declared value is rejected with `M057_BLOB_E002`.

Pass criteria:

1. Streaming dual-hash. A 100MB blob fetch on a single-core CI runner does not exceed steady-state memory of ~16MB; BLAKE3 and SHA-256 are computed in one pass via a `io.MultiWriter`. Memory is measured with `runtime.ReadMemStats` before and after.
2. Cache hit O(1). A second fetch of the same blob (already on disk) returns within 5ms wall-clock; the verification re-hashes from the cached file but does not re-download.
3. Poison detection. A blob whose downloaded bytes hash to a different BLAKE3 than the URL hex raises `M057_BLOB_E001` with both expected and actual hashes in the error.
4. SHA-256 mismatch. The blob's bytes hash to a SHA-256 different from the index entry's `s2` field raises `M057_BLOB_E002`.
5. Concurrent install. Two parallel `mochi install` runs of the same blob never produce a half-extracted tree. The fcntl-based per-blob lock serialises extraction; one process extracts, the other waits and reuses.
6. Extraction safety. A tarball containing `../../../../etc/passwd`, a symlink to `/etc/shadow`, a device file, or a hard link out of the extract root is rejected with `M057_BLOB_E003` (extraction-safety).

## Goal-alignment audit

The blob store is where source materialises. Without verified content addressing, every other layer is gated on an unverified transfer. The user-facing goal moved: "I cannot install a tampered tarball, even if a malicious mirror tries".

Dual-hash design (BLAKE3 primary, SHA-256 secondary) is from research note 08 §2: cross-ecosystem interoperability (SLSA, Sigstore, npm, PyPI, Maven, GitHub) requires SHA-256, performance wants BLAKE3. The cost of computing both is ~13ms per MB on a 2024-era laptop. The benefit is one wire format compatible with every supply-chain ecosystem.

Tarball extraction safety is a 30-year-old CVE class (tar traversal, symlink trickery). Phase 9 implements the well-known defences (canonicalise paths, reject `..`, reject absolute paths, reject non-regular non-directory entries).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | Streaming dual-hash (BLAKE3 + SHA-256 in one pass) | NOT STARTED | — |
| 9.1 | Blob URL fetch via Phase 8's HTTP client | NOT STARTED | — |
| 9.2 | Cache layout `$MOCHI_HOME/store/blobs/<bb>/<aa>/<hex>` | NOT STARTED | — |
| 9.3 | Cache concurrency: fcntl per-blob lock during extract | NOT STARTED | — |
| 9.4 | zstd decompression streaming into tar extractor | NOT STARTED | — |
| 9.5 | Tar extraction with reproducibility guards | NOT STARTED | — |
| 9.6 | Extracted-tree caching at `$MOCHI_HOME/store/extracted/<hex>/` | NOT STARTED | — |
| 9.7 | `mochi pkg cache prune / clean` commands | NOT STARTED | — |
| 9.8 | `mochi pkg audit blobs` — re-hash cache, compare against lockfile | NOT STARTED | — |

## Sub-phase 9.0 — Streaming dual-hash

```go
// pkg/pkgblob/hash.go
package pkgblob

import (
    "crypto/sha256"
    "encoding/hex"
    "hash"
    "io"

    "lukechampine.com/blake3"
)

type DualHash struct {
    BLAKE3 hash.Hash
    SHA256 hash.Hash
}

func NewDual() *DualHash {
    return &DualHash{
        BLAKE3: blake3.New(32, nil),
        SHA256: sha256.New(),
    }
}

func (d *DualHash) Write(p []byte) (int, error) {
    d.BLAKE3.Write(p)
    d.SHA256.Write(p)
    return len(p), nil
}

func (d *DualHash) Sum() (b3, s2 string) {
    return hex.EncodeToString(d.BLAKE3.Sum(nil)),
           hex.EncodeToString(d.SHA256.Sum(nil))
}
```

Usage:

```go
h := NewDual()
tr := io.TeeReader(resp.Body, h)
// stream tr through zstd -> tar extractor; bytes flow through h on the way.
b3, s2 := h.Sum()
```

The hash runs in the same goroutine as decompression; back-pressure from extraction throttles the reader, no buffer growth.

## Sub-phase 9.1 — Blob fetch

```go
// pkg/pkgblob/fetch.go
type Store interface {
    Fetch(blake3 string) (io.ReadCloser, error)
}

type HTTPBlobStore struct {
    Endpoint string
    client   *http.Client
}

func (s *HTTPBlobStore) Fetch(blake3 string) (io.ReadCloser, error) {
    url := fmt.Sprintf("%s/%s", s.Endpoint, blake3)
    req, _ := http.NewRequest("GET", url, nil)
    resp, err := s.client.Do(req)
    if err != nil { return nil, err }
    if resp.StatusCode != 200 {
        resp.Body.Close()
        return nil, fmt.Errorf("%w: %s -> %d", ErrBlobE001, url, resp.StatusCode)
    }
    return resp.Body, nil
}
```

The fetch path reuses Phase 8's HTTP client (HTTP/2 multiplexing, retries, backoff). The blob endpoint sets `Cache-Control: public, max-age=31536000, immutable` (research note 08 §6), so CDN cache hit rates are very high.

## Sub-phase 9.2 — Cache layout

Canonical root `$MOCHI_HOME` (see [phase 0
§conventions](./phase-00-skeleton#files-changed)); research note 08 §7
established the two-character sharding scheme:

```
$MOCHI_HOME/store/
  blobs/
    <bb>/<aa>/<hex>        # bb = blake3[:2], aa = blake3[2:4]; full hex is filename
  extracted/
    <hex>/
      manifest.toml
      src/...
      .integrity           # the verified BLAKE3 + SHA-256
  locks/
    <hex>.lock             # fcntl, keyed by blob hash (not name+version);
                           # see Phase 9.3 for the rationale
```

(The Phase 8 sparse-index cache lives at `$MOCHI_HOME/index/`; it is a
sibling of `store/`, not nested.)

The two-character pair sharding caps per-directory entries to a few hundred
even for very popular packages.

## Sub-phase 9.3 — Concurrency

Two `mochi install` processes for the same blob must not produce a half-extracted tree. Use OS fcntl locks:

```go
// pkg/pkgblob/lock.go
type BlobLock struct {
    path string
    f    *os.File
}

func AcquireExclusive(cachedir, name, version string) (*BlobLock, error) {
    path := filepath.Join(cachedir, "locks", name+"-"+version+".lock")
    if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil { return nil, err }
    f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0644)
    if err != nil { return nil, err }
    if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
        f.Close()
        return nil, err
    }
    return &BlobLock{path: path, f: f}, nil
}

func (l *BlobLock) Release() error {
    syscall.Flock(int(l.f.Fd()), syscall.LOCK_UN)
    return l.f.Close()
}
```

The lock path is per `(name, version)` because two concurrent installs of distinct blobs should not serialise on each other. Windows uses `LockFileEx`; the abstraction lives behind `pkg/pkgblob/lock_unix.go` and `lock_windows.go`.

This lock is internal to the `mochi` CLI process group sharing one `$MOCHI_HOME`; it is not a capability that downstream packages can request or grant. The Phase 10 capability whitelist does not list `fs.lock`; packages have no syscall surface to acquire OS-level file locks. The `fcntl` / `LockFileEx` calls happen inside the CLI binary before any package code runs.

The flow:

1. Acquire shared lock to check extracted dir exists.
2. If yes, release shared lock, use extracted tree.
3. If no, upgrade to exclusive lock, fetch + extract under temp dir, atomic rename.
4. Release exclusive lock.

## Sub-phase 9.4 — Zstd decompression

```go
import "github.com/klauspost/compress/zstd"

func decompress(in io.Reader) (io.Reader, error) {
    d, err := zstd.NewReader(in, zstd.WithDecoderConcurrency(1))
    if err != nil { return nil, err }
    return d, nil
}
```

Single-threaded decode is fine; the bottleneck is network, not CPU. Memory bound ~1MB working set.

## Sub-phase 9.5 — Tar extraction with reproducibility guards

```go
func extractTar(in io.Reader, dst string) error {
    tr := tar.NewReader(in)
    for {
        h, err := tr.Next()
        if err == io.EOF { break }
        if err != nil { return err }
        if err := validateEntry(h); err != nil { return err }
        target := filepath.Join(dst, h.Name)
        if !strings.HasPrefix(target, dst+string(filepath.Separator)) {
            return fmt.Errorf("%w: path escapes root: %s", ErrBlobE003, h.Name)
        }
        switch h.Typeflag {
        case tar.TypeDir:
            os.MkdirAll(target, 0755)
        case tar.TypeReg:
            if err := writeRegular(target, tr, h); err != nil { return err }
        default:
            return fmt.Errorf("%w: unsupported entry type %v: %s",
                ErrBlobE003, h.Typeflag, h.Name)
        }
    }
    return nil
}

func validateEntry(h *tar.Header) error {
    if strings.Contains(h.Name, "..") {
        return fmt.Errorf("%w: '..' in path: %s", ErrBlobE003, h.Name)
    }
    if filepath.IsAbs(h.Name) {
        return fmt.Errorf("%w: absolute path: %s", ErrBlobE003, h.Name)
    }
    if h.Linkname != "" {
        return fmt.Errorf("%w: symlinks not allowed: %s", ErrBlobE003, h.Name)
    }
    if h.Mode&^0777 != 0 {
        return fmt.Errorf("%w: setuid/setgid/sticky not allowed: %s", ErrBlobE003, h.Name)
    }
    return nil
}
```

Entries accepted: directories (0755) and regular files (0644). Everything else (symlinks, devices, fifos, hard links) is rejected. This matches the strict reproducible-tar policy from research note 08 §3.

## Sub-phase 9.6 — Extracted-tree caching

After successful verify + extract, the tree is rooted at
`$MOCHI_HOME/store/extracted/<hex>/`:

```
$MOCHI_HOME/store/extracted/<hex>/
  manifest.toml
  src/
  LICENSE
  .integrity           # BLAKE3 + SHA-256 of the tarball, verified
```

`.integrity` is a two-line text file:

```
blake3=e2d1...
sha256=abf3...
```

Subsequent installs read this file and skip re-hashing if the lockfile pin matches.

## Sub-phase 9.7 — Cache management CLI

```
mochi pkg cache size                      # show disk usage
mochi pkg cache prune --older-than=30d    # GC unused entries
mochi pkg cache prune --dry-run           # preview
mochi pkg cache clean                     # wipe everything
mochi pkg cache verify                    # re-hash every entry, report poison
```

`prune` reads access times from the OS where supported; on filesystems with `noatime` it falls back to a per-blob `.lastused` timestamp file written on cache hit.

## Sub-phase 9.8 — `mochi pkg audit blobs`

For supply-chain forensics (research note 08 §10):

```go
func cmdAuditBlobs(c *cli.Context) error {
    lock, _ := pkglock.ParseFile("mochi.lock")
    var problems []string
    for _, p := range lock.Packages {
        if p.BLAKE3 == "" { continue }       // workspace / path source
        path := blobPath(p.BLAKE3)
        if !exists(path) {
            problems = append(problems, fmt.Sprintf("%s: blob missing", p.Name))
            continue
        }
        actual, _ := hashFile(path)
        if actual != p.BLAKE3 {
            problems = append(problems, fmt.Sprintf("%s: blob hash %s != lock %s",
                p.Name, actual, p.BLAKE3))
        }
    }
    if len(problems) > 0 {
        fmt.Println(strings.Join(problems, "\n"))
        return cli.Exit("", 1)
    }
    fmt.Println("OK")
    return nil
}
```

A mismatch is a P0 forensic signal.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgblob/hash.go` | Dual-hash streaming writer | Owner |
| `pkg/pkgblob/fetch.go` | HTTP blob fetch | Owner |
| `pkg/pkgblob/extract.go` | Tar + zstd extractor with safety guards | Owner |
| `pkg/pkgblob/cache.go` | Cache layout + path helpers | Owner |
| `pkg/pkgblob/lock.go` | fcntl per-blob lock | Owner |
| `pkg/pkgblob/lock_unix.go` | POSIX implementation | Owner |
| `pkg/pkgblob/lock_windows.go` | Windows implementation | Owner |
| `pkg/pkgblob/audit.go` | `mochi pkg audit blobs` core | Owner |
| `cmd/mochi/cache.go` | `mochi pkg cache size / prune / clean / verify` (extended by Phase 19 `gc`) | Owner |
| `cmd/mochi/audit.go` | `mochi pkg audit blobs` handler (extended by Phase 10 `capabilities`, Phase 13 `signatures`, Phase 16 advisory feed) | Owner |
| `tests/pkgsystem/content-store/normal/*` | Happy path | Owner |
| `tests/pkgsystem/content-store/poisoned/*` | Reject mismatched bytes | Owner |
| `tests/pkgsystem/content-store/path-escape/*` | Reject `..` in tar | Owner |
| `tests/pkgsystem/content-store/concurrent-install/*` | Two processes, one blob | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
Sources (see [error registry](./errors)). The Phase 9 BLOB_E002
mapping is registry-name reuse: the registry's `M057_BLOB_E002` is the
generic "fetch partial / connection reset" sentinel. SHA-256 disagreement
falls under `M057_BLOB_E001` (dual-hash family) plus the integrity
sidecar. Verbal aliases used in early drafts:

| Code | Trigger |
|------|---------|
| `M057_BLOB_E001` | BLAKE3 (or SHA-256 sidecar) of downloaded bytes does not match URL hex / index `s2`. |
| `M057_BLOB_E002` | Blob fetch partial / connection reset (auto-retried). |
| `M057_BLOB_E003` | Tar entry violates safety policy (path escape, absolute path). |
| `M057_BLOB_E007` | 404 from local registry blob endpoint. |
| `M057_RESOLVE_E008` | Could not acquire per-blob lock (timeout). |

## Test set

- `TestPhase9DualHash` — streaming hash result matches reference for fixtures of 1KB / 1MB / 100MB.
- `TestPhase9Fetch` — happy path.
- `TestPhase9Poison` — flipped byte raises E001.
- `TestPhase9Sha256Mismatch` — index `s2` disagreement raises E002.
- `TestPhase9PathEscape` — `..` in tar raises E003.
- `TestPhase9Concurrent` — two install goroutines, single extract.
- `TestPhase9CachePrune` — `prune --older-than=Nd` removes entries.

## Performance targets

From research note 08 §14:

- Index fetch (50 deps): ~200ms over HTTP/2.
- Blob fetch (50 deps, ~10MB total): ~2s parallel.
- Dual-hash on each (~5ms x 50 parallel): ~250ms total.
- Sigstore verify (Phase 13, ~10ms each): ~500ms total.
- Decompress + extract: ~2s.
- Cold install end-to-end: ~5s for 50 deps / ~10MB.
- Warm install (lockfile + cache hit): less than 100ms.

## Open questions

- Whether to allow `Content-Encoding: zstd` over HTTP (compress on the wire) in addition to the file format `.tar.zst`; current plan: no, the file format already compresses and double-compression wastes CPU.
- Whether to support optional `h3` (third hash, SHA-3 / Keccak) for future agility; reserved in the schema, not implemented at v1 (research note 08 §12).

## Cross-references

- Store details: [research note 08](/docs/research/0057/content-addressed-store).
- Dual-hash rationale: [research note 02 §4](/docs/research/0057/design-philosophy).
- Tarball format: [research note 08 §3](/docs/research/0057/content-addressed-store).
- Cache layout: [research note 08 §7](/docs/research/0057/content-addressed-store).
- Forensic audit pattern: [research note 08 §10](/docs/research/0057/content-addressed-store).
