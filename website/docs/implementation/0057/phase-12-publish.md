---
title: "Phase 12. Publish pipeline"
sidebar_position: 13
sidebar_label: "Phase 12. Publish pipeline"
description: "MEP-57 Phase 12 — `mochi pkg publish` tarball build, manifest pin, dry-run, deterministic archive, BLAKE3 + SHA-256 computation, pre-flight validation, registry POST."
---

# Phase 12. Publish pipeline (no signing yet)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 12](/docs/mep/mep-0057#phase-12-publish) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase12Publish`: every fixture under `tests/pkgsystem/publish/` builds a byte-deterministic tarball matching its golden; pre-flight validation catches missing metadata; `--dry-run` prints intended actions without contacting the registry.

Pass criteria:

1. Byte-deterministic tarball. Building the same fixture twice produces byte-identical `.tar.zst`. Tested via `mochi pkg publish --no-upload --out=tmp.tar.zst` x2, then `cmp`.
2. Pre-flight rejection. A manifest missing `license`, `description`, or `readme` is rejected with `M057_PUB_E001` listing every missing field. The list is alphabetised.
3. Include / exclude rules. A repo containing `.git/`, `node_modules/`, `*.log`, and `dist/` produces a tarball that excludes all four by default. A `[package.exclude] = ["src/internal/*"]` rule additionally drops the listed paths.
4. Streaming hash. The publish path computes BLAKE3 and SHA-256 over the compressed output in a single pass via `io.MultiWriter`; memory stays bounded for a 50MB tarball (research note 09 §3).
5. Dry-run completeness. `mochi pkg publish --dry-run` prints the tarball contents (sorted), the computed `b3` and `s2`, the planned POST URL, and the index-entry JSON line; it does NOT touch the network.
6. Registry POST. A successful POST receives `201 Created` with a JSON body `{ "version_url": "...", "blob_url": "..." }`; the client validates the returned URL ends in the same `b3` it computed.

## Goal-alignment audit

Publishing without signing is the foundation Phase 13 layers Sigstore on. Splitting the phases isolates the deterministic-tarball work from the OIDC + Sigstore plumbing, both of which need their own gates.

Byte-determinism is the load-bearing property. Without it, two CI runs of the same source produce two different hashes, which breaks every downstream reproducibility check (Phase 17), every supply-chain audit, and every `mochi pkg audit blobs --since` query. The cost of determinism is small (sort entries, zero mtimes, normalise mode bits), the benefit is permanent.

The deliberate decision to *omit* signing here: Phase 12 ships a working publish pipeline that just doesn't sign. Early adopters get a usable command; Phase 13 turns on Sigstore once Fulcio + Rekor + OIDC wiring lands. This is also the order GitHub used for the npm provenance rollout (2023): publish first, sign later, gate on TUF root distribution.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | Pre-flight: manifest validation, license required, README required | NOT STARTED | — |
| 12.1 | Tarball builder: sorted entries, mtime=0, uid/gid=0, mode mask | NOT STARTED | — |
| 12.2 | Default include / exclude rules (`.git`, `node_modules`, etc.) | NOT STARTED | — |
| 12.3 | `[package.include]` / `[package.exclude]` override paths | NOT STARTED | — |
| 12.4 | zstd level 19 compression | NOT STARTED | — |
| 12.5 | Streaming BLAKE3 + SHA-256 over the compressed output | NOT STARTED | — |
| 12.6 | Registry POST endpoint client (no signing in this phase) | NOT STARTED | — |
| 12.7 | `mochi pkg publish --dry-run` | NOT STARTED | — |
| 12.8 | Index-entry JSON line generation (forwarded to registry) | NOT STARTED | — |

## Sub-phase 12.0 — Pre-flight validation

Before any tarball work, validate the manifest. Required fields (research note 09 §1):

| Field | Required for publish? | Why |
|-------|----------------------|-----|
| `[package].name` | yes | identity |
| `[package].version` | yes | identity |
| `[package].license` | yes | legal / SBOM |
| `[package].description` | yes | search / display |
| `[package].readme` | yes | (path; defaults to `README.md`) |
| `[package].repository` | yes | reproducibility / SLSA |
| `[package].homepage` | optional | display |
| `[package].authors` | optional | display |
| `[capabilities]` | optional but present implies validated | Phase 10 |
| `[targets]` | yes | drives polyglot fan-out |

```go
// pkg/pkgpublish/preflight.go
type Missing struct {
    Field string
    Why   string
}

func Preflight(m *pkgmanifest.Manifest, root string) ([]Missing, error) {
    var miss []Missing
    if m.Package.License == "" {
        miss = append(miss, Missing{"license", "SPDX identifier required"})
    }
    if m.Package.Description == "" {
        miss = append(miss, Missing{"description", "one-sentence package summary"})
    }
    if !fileExists(filepath.Join(root, m.Package.Readme)) {
        miss = append(miss, Missing{"readme", fmt.Sprintf("file %q not found", m.Package.Readme)})
    }
    if m.Package.Repository == "" {
        miss = append(miss, Missing{"repository", "URL of source repo"})
    }
    if len(m.Targets) == 0 {
        miss = append(miss, Missing{"targets", "at least one target entrypoint"})
    }
    sort.Slice(miss, func(i, j int) bool { return miss[i].Field < miss[j].Field })
    return miss, nil
}
```

Other pre-flight checks (non-field):

- License string is a valid SPDX identifier (parse via existing `pkg/spdx`).
- Version is not yanked in the local registry view (would conflict).
- Version is monotonically greater than the latest published (warn, not error; allows backfill).
- Capability declarations satisfy monotonicity vs the published history (Phase 10.5 in `--strict` mode).

## Sub-phase 12.1 — Tarball builder

Determinism rules (research note 08 §3, research note 09 §2):

| Property | Value |
|----------|-------|
| Entry order | Lexicographic by path (POSIX byte order). |
| `mtime` | `0` (Unix epoch). |
| `uid` / `gid` | `0`. |
| `uname` / `gname` | empty string. |
| Mode | masked to `0644` for files, `0755` for directories. |
| Format | USTAR; PAX records only for paths > 100 chars. |
| `linkname` | rejected (no symlinks). |
| File data | unchanged. |

```go
// pkg/pkgpublish/tar.go
func WriteTarball(w io.Writer, root string, files []string) error {
    tw := tar.NewWriter(w)
    sort.Strings(files)
    for _, f := range files {
        info, err := os.Stat(filepath.Join(root, f))
        if err != nil { return err }
        hdr := &tar.Header{
            Name:    filepath.ToSlash(f),
            Size:    info.Size(),
            Mode:    int64(maskMode(info.Mode())),
            Format:  tar.FormatUSTAR,
            ModTime: time.Unix(0, 0),
        }
        if info.IsDir() {
            hdr.Typeflag = tar.TypeDir
            hdr.Mode = 0755
            hdr.Size = 0
        } else {
            hdr.Typeflag = tar.TypeReg
            hdr.Mode = 0644
        }
        if err := tw.WriteHeader(hdr); err != nil { return err }
        if !info.IsDir() {
            r, _ := os.Open(filepath.Join(root, f))
            io.Copy(tw, r)
            r.Close()
        }
    }
    return tw.Close()
}

func maskMode(m os.FileMode) os.FileMode {
    if m&0111 != 0 { return 0755 }
    return 0644
}
```

Symlinks: rejected at file-enumeration time with `M057_PUB_E002` ("symlinks not allowed in published tarballs"). This matches the extraction-time rejection in Phase 9.5; the publish side enforces the same invariant at creation.

## Sub-phase 12.2 — Default include / exclude

Default excludes (always applied unless overridden):

```
.git/
.svn/
.hg/
node_modules/
target/
dist/
build/
*.log
*.tmp
*.swp
.DS_Store
.idea/
.vscode/
.env
.env.*
```

Default includes (relative to manifest root):

```
mochi.toml
README*                       (any case, .md / .rst / no ext)
LICENSE*                      (any case)
CHANGELOG*                    (any case)
src/**
```

Order: excludes evaluated before includes; a path matched by an exclude is dropped regardless. Manifest overrides via `[package.include]` and `[package.exclude]` reset both lists (not append).

## Sub-phase 12.3 — Manifest overrides

```toml
[package]
include = ["src/**", "examples/**", "LICENSE", "README.md"]
exclude = ["src/internal/**", "src/**.test.mochi"]
```

Glob semantics: `**` recursive, `*` non-slash. Implemented via `pkg/pkgglob` (existing). Resolved relative to the manifest directory; absolute paths rejected with `M057_PUB_E003`.

## Sub-phase 12.4 — zstd level 19

```go
import "github.com/klauspost/compress/zstd"

func newCompressor(w io.Writer) (*zstd.Encoder, error) {
    return zstd.NewWriter(w,
        zstd.WithEncoderLevel(zstd.SpeedBestCompression),  // level 19
        zstd.WithEncoderConcurrency(1),                    // determinism
    )
}
```

Level 19 chosen because:

- Better ratio than gzip (typical 30-40% smaller).
- Determinism is single-threaded only (multi-threaded zstd is non-deterministic).
- Compression time dominated by network for typical packages anyway.

The decoder (Phase 9.4) is single-threaded too.

## Sub-phase 12.5 — Streaming dual-hash

Reuse `pkgblob.NewDual()` from Phase 9.0; pipe the compressed output through it:

```go
func BuildPublishArtefact(root string, m *Manifest) (*Artefact, error) {
    files := walkFiles(root, m.Includes, m.Excludes)
    var buf bytes.Buffer
    h := pkgblob.NewDual()
    w := io.MultiWriter(&buf, h)
    enc, _ := newCompressor(w)
    if err := WriteTarball(enc, root, files); err != nil { return nil, err }
    if err := enc.Close(); err != nil { return nil, err }
    b3, s2 := h.Sum()
    return &Artefact{
        Bytes:  buf.Bytes(),
        BLAKE3: b3,
        SHA256: s2,
        Size:   int64(buf.Len()),
    }, nil
}
```

The `bytes.Buffer` is acceptable up to ~50MB; for larger sources the path streams directly to a temp file and dual-hashes on the way.

## Sub-phase 12.6 — Registry POST

Endpoint (research note 09 §4):

```
POST https://upload.mochi.dev/packages
Content-Type: application/vnd.mochi.tarball+zstd
Authorization: Bearer <oidc-token-or-api-key>
X-Mochi-Manifest: <base64 manifest TOML>
X-Mochi-Blake3:   <hex>
X-Mochi-Sha256:   <hex>

<tarball bytes>
```

Response:

```
201 Created
Content-Type: application/json

{
  "version_url": "https://index.mochi.dev/@mochi/strings",
  "blob_url":    "https://blobs.mochi.dev/<bb>/<aa>/<hex>.tar.zst",
  "rekor_url":   null
}
```

`rekor_url` populated only when Phase 13 lands; at v12 it is always null.

Client:

```go
func (c *Client) Upload(art *Artefact, m *Manifest) (*PublishResult, error) {
    req, _ := http.NewRequest("POST", c.URL+"/packages", bytes.NewReader(art.Bytes))
    req.Header.Set("Content-Type", "application/vnd.mochi.tarball+zstd")
    req.Header.Set("Authorization", "Bearer "+c.Token)
    req.Header.Set("X-Mochi-Manifest", base64.StdEncoding.EncodeToString(manifestBytes(m)))
    req.Header.Set("X-Mochi-Blake3", art.BLAKE3)
    req.Header.Set("X-Mochi-Sha256", art.SHA256)
    resp, err := c.HTTP.Do(req)
    if err != nil { return nil, err }
    defer resp.Body.Close()
    switch resp.StatusCode {
    case 201: return parseSuccess(resp)
    case 409: return nil, fmt.Errorf("%w: version already exists", ErrPubE004)
    case 422: return nil, fmt.Errorf("%w: registry rejected manifest: %s", ErrPubE005, readBody(resp))
    case 401: return nil, fmt.Errorf("%w: auth failed", ErrPubE006)
    default:  return nil, fmt.Errorf("%w: status %d", ErrPubE008, resp.StatusCode)
    }
}
```

Idempotency: re-uploading the same `(name, version, b3)` is idempotent (`201` again, registry server-side). A different `b3` for the same `(name, version)` is `409 Conflict` (versions are immutable).

## Sub-phase 12.7 — `mochi pkg publish --dry-run`

Output spec:

```
$ mochi pkg publish --dry-run
package:     @mochi/strings 0.4.7
license:     MIT
target dir:  /home/user/projects/strings
files (12):
  LICENSE                                  (1.1 KB)
  README.md                                (2.4 KB)
  mochi.toml                               (412 B)
  src/lib.mochi                            (3.2 KB)
  ...
tarball:    12 files, 31.4 KB compressed
blake3:     e2d1a4...
sha256:     abf3e1...
endpoint:   https://upload.mochi.dev/packages
index entry (would write):
  {"v":"0.4.7","r":"2026-05-29T...","b3":"...","s2":"...","c":[],"d":{}}

(dry-run; nothing uploaded)
```

The output is deterministic; the test fixtures golden-match this text.

## Sub-phase 12.8 — Index entry generation

The client constructs the JSONL entry the registry will append to the package's index file (the registry may also derive it server-side; sending it as a hint speeds the round-trip):

```go
func BuildIndexEntry(m *Manifest, art *Artefact, sde time.Time) IndexEntry {
    // sde is pkgrepro.SourceDateEpoch() (Phase 17.0). When SOURCE_DATE_EPOCH
    // is unset the publish CLI passes wall time; when set, the value flows
    // through unchanged so two publishes of the same tag yield byte-identical
    // entries.
    return IndexEntry{
        V:  m.Package.Version,
        R:  sde.UTC().Truncate(time.Second),
        B3: art.BLAKE3,
        S2: art.SHA256,
        C:  m.Capabilities.Required.Sorted(),
        D:  shortenDeps(m.Dependencies),
        T:  sortedTargetNames(m.Targets),
        MP: compactRange(m.Package.MochiRange),
        ED: m.Package.Edition,
        LK: m.Package.License,
    }
}
```

`shortenDeps` writes each dep as `name = "range"`, sorted by name.

Field names map to the abbreviations in Phase 8.3's parser.

Determinism: `R` is the only timestamp the publisher controls in the index
entry. The registry may stamp its own `received_at` server-side for audit,
but the JSONL line stored in the bucket is computed client-side and is
required to be byte-identical across reproducing publishes. See
[phase 17 §17.0](./phase-17-repro#sub-phase-170--source_date_epoch) for the
SDE-flow contract.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgpublish/preflight.go` | Manifest validation | Owner |
| `pkg/pkgpublish/tar.go` | Deterministic tar writer (consumes Phase 17 SDE) | Owner |
| `pkg/pkgpublish/glob.go` | Include/exclude resolution | Owner |
| `pkg/pkgpublish/compress.go` | zstd encoder | Owner |
| `pkg/pkgpublish/artefact.go` | Build pipeline + dual-hash | Owner |
| `pkg/pkgpublish/client.go` | Registry HTTPS client | Owner |
| `pkg/pkgpublish/dryrun.go` | Dry-run renderer | Owner |
| `pkg/pkgpublish/index_entry.go` | JSONL line builder | Owner |
| `cmd/mochi/publish.go` | `mochi pkg publish` handler (extended by Phase 14 `--target`) | Owner |
| `tests/pkgsystem/publish/tiny-lib/*` | Golden tarball | Owner |
| `tests/pkgsystem/publish/with-excludes/*` | Exclude rules | Owner |
| `tests/pkgsystem/publish/deterministic/*` | Twice-build byte-compare | Owner |
| `tests/pkgsystem/publish/missing-license/*` | Preflight rejection | Owner |
| `tests/pkgsystem/publish/dryrun/*` | Dry-run output golden | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_PUB_E001` | Pre-flight: required field missing. |
| `M057_PUB_E002` | Tarball contains a symlink, device, or hardlink. |
| `M057_PUB_E003` | Glob expansion produced an absolute or `..` path. |
| `M057_PUB_E004` | Registry returned 409: version already published with different hash. |
| `M057_PUB_E005` | Registry rejected manifest with 422. |
| `M057_PUB_E006` | Registry returned 401: auth failed. |
| `M057_PUB_E008` | Registry returned unexpected status. |

(Phase 13 owns the Sigstore failures under the `M057_SIG_*` namespace; see the [error registry](./errors). Phase 12 no longer pre-reserves rows for them.)

## Test set

- `TestPhase12Preflight` — missing license, description, readme each reported.
- `TestPhase12TarballGolden` — `tiny-lib/` matches `expected.tar.zst`.
- `TestPhase12Determinism` — two runs produce identical bytes.
- `TestPhase12Excludes` — `.git/` and `*.log` filtered.
- `TestPhase12ManifestOverrides` — `[package.exclude]` overrides default include.
- `TestPhase12DryRun` — output matches `expected.txt`.
- `TestPhase12UploadHappy` — mock registry returns 201, client parses success.
- `TestPhase12Upload409` — registry returns 409, client raises E004.
- `TestPhase12StreamHash` — BLAKE3+SHA256 computed in single pass; memory bound test.

## Open questions

- Whether to support per-target source bundles (e.g., publish only `src/python` for the python-only consumer); current plan: no, single tarball with all targets, consumer-side selection at install.
- Whether the dry-run should also call out to the registry for a `409` pre-check; current plan: yes, optional `--check-server` flag.
- Whether to support API-key auth at v12 or wait for OIDC at v13; current plan: API key now (`MOCHI_TOKEN` env), OIDC at v13.

## Cross-references

- Blob format: [research note 08 §3](/docs/research/0057/content-addressed-store).
- Publish pipeline overview: [research note 09](/docs/research/0057/publishing-pipeline).
- Reproducible tar policy: [research note 08 §3](/docs/research/0057/content-addressed-store).
- Sigstore enabling (next phase): [phase 13](./phase-13-sigstore).
