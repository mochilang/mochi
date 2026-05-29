---
title: "Phase 17. Reproducible package build"
sidebar_position: 18
sidebar_label: "Phase 17. Reproducible build"
description: "MEP-57 Phase 17 — SOURCE_DATE_EPOCH respect, sorted tar entries, mtime=0, fixed uid/gid, deterministic zstd compression level, byte-equal rebuild verification."
---

# Phase 17. Reproducible package build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 17](/docs/mep/mep-0057#phase-17-repro) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase17Reproducible`: for every fixture under `tests/pkgsystem/repro/`, building the package twice in different temp directories with different umasks and TZ produces byte-identical tarballs and identical BLAKE3 + SHA-256 hashes.

The reproducibility gate runs on linux-x86_64 and macos-arm64 (the two
hosts that share POSIX tar + zstd semantics). Windows-x86_64 runs the
gate in *consume* mode only: it verifies that a tarball built upstream
on linux/mac extracts and hashes identically, but it does not act as a
build origin because Windows file-attribute and CRLF handling would
require a separate normalization pass. This is a deliberate scope cut
documented as Phase 17 open question 4; consumer rebuilds (criterion 6)
on Windows still pass because the source tag plus a linux/mac builder
produces the registry's `<b3>`.

Pass criteria:

1. Cross-TZ. Building in `TZ=UTC` and `TZ=Asia/Ho_Chi_Minh` produces byte-identical tarballs.
2. Cross-umask. `umask 022` vs `umask 077` produces byte-identical tarballs.
3. Cross-locale. `LC_ALL=C` vs `LC_ALL=en_US.UTF-8` vs `LC_ALL=ja_JP.UTF-8` produces byte-identical tarballs.
4. Cross-filesystem. ext4 vs APFS source mounts produce byte-identical tarballs (case-sensitive; case-folding fs is out of scope; NTFS deferred per the Windows scope cut above).
5. SOURCE_DATE_EPOCH. With `SOURCE_DATE_EPOCH=1700000000`, every tar entry has `mtime=1700000000`; every zstd frame is independent of wall clock.
6. Rebuild script. `mochi pkg rebuild --from-source <repo>@<tag>` reproduces the registry's `<b3>` from a fresh git clone with no machine-specific environment.
7. Diffoscope-clean. For a fixture where reproducibility fails, `diffoscope` output is the test diagnostic; the harness asserts no diff at the byte level.

## Goal-alignment audit

Reproducibility lets a third party (or a consumer's CI) re-derive the published artifact from source and prove equality. This is the audit surface that backs SLSA L3 claims from Phase 15. The user-facing goal moved: "I can clone the source tag and rebuild the exact tarball that's on the registry. Bit-for-bit".

The reproducible-builds.org playbook (research note 12 §A.14) is the input here. Every source of non-determinism is enumerated and pinned: timestamps, file order, mode bits, locale, umask, line endings, hash map iteration, compression dictionaries. The cost is small (sort, mask, pin), the benefit is independent verifiability.

This phase intentionally does NOT require deterministic compiler output (the polyglot emitters in Phase 14 already aim for byte-determinism, but for source packages the Mochi compiler is not invoked). Source packages contain `.mochi` files plus a manifest; reproducibility of these is straightforward.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | Honour `SOURCE_DATE_EPOCH` for all timestamps | NOT STARTED | — |
| 17.1 | Sorted directory walk (NFC + byte order) | NOT STARTED | — |
| 17.2 | tar entries: mtime=epoch, uid=gid=0, mode mask, no xattrs | NOT STARTED | — |
| 17.3 | zstd: explicit level 19, dict=none, no timestamp in frame | NOT STARTED | — |
| 17.4 | `LC_ALL=C` enforcement during build | NOT STARTED | — |
| 17.5 | `mochi pkg pack --verify-reproducible` double-build + compare | NOT STARTED | — |
| 17.6 | Rebuilder script publishable alongside source | NOT STARTED | — |
| 17.7 | Line-ending normalisation (CRLF -> LF) for text files | NOT STARTED | — |
| 17.8 | `mochi pkg rebuild` consumer-side verification | NOT STARTED | — |

## Sub-phase 17.0 — SOURCE_DATE_EPOCH

The reproducible-builds.org convention: every timestamp the build writes equals `min(file_mtime, SOURCE_DATE_EPOCH)`. If unset, the build uses `0` (Unix epoch).

```go
// pkg/pkgrepro/clock.go
func SourceDateEpoch() time.Time {
    if v := os.Getenv("SOURCE_DATE_EPOCH"); v != "" {
        if secs, err := strconv.ParseInt(v, 10, 64); err == nil {
            return time.Unix(secs, 0).UTC()
        }
    }
    return time.Unix(0, 0).UTC()
}
```

Used by:

- Phase 12.1 tar mtimes.
- Phase 15.0 CycloneDX `metadata.timestamp`.
- Phase 13.2 in-toto `runDetails.metadata.startedOn` (only when SOURCE_DATE_EPOCH set; otherwise the actual build timestamp is used because the in-toto Statement is signed and the timestamp is forensic).

When SOURCE_DATE_EPOCH is set, `mochi pkg publish` warns "reproducible build mode; in-toto runDetails.startedOn will be SOURCE_DATE_EPOCH, not actual wall time".

## Sub-phase 17.1 — Sorted directory walk

```go
// pkg/pkgrepro/walk.go
func WalkSorted(root string, accept func(string) bool) ([]string, error) {
    var paths []string
    err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
        if err != nil { return err }
        rel, _ := filepath.Rel(root, p)
        rel = filepath.ToSlash(rel)
        rel = norm.NFC.String(rel)            // Unicode NFC normalisation
        if accept(rel) { paths = append(paths, rel) }
        return nil
    })
    if err != nil { return nil, err }
    sort.Slice(paths, func(i, j int) bool { return paths[i] < paths[j] })  // byte order
    return paths, nil
}
```

NFC normalisation matters because macOS HFS+ stores filenames as NFD; the same file appears under a different byte sequence on Linux. NFC is the canonical form in the tarball regardless of source filesystem.

Sort by UTF-8 byte order (not locale-aware): identical to `LC_ALL=C` sort.

## Sub-phase 17.2 — Tar deterministic entries

Already covered in Phase 12.1; Phase 17 adds the *extended attribute* rejection:

```go
func validateEntry(hdr *tar.Header) error {
    if len(hdr.Xattrs) > 0 {
        return fmt.Errorf("%w: xattrs not allowed", ErrReproE001)
    }
    if hdr.PAXRecords != nil {
        for k := range hdr.PAXRecords {
            if !isAllowedPAX(k) {
                return fmt.Errorf("%w: PAX record %q not allowed", ErrReproE001, k)
            }
        }
    }
    return nil
}

func isAllowedPAX(k string) bool {
    switch k {
    case "path":     return true  // long-path workaround
    case "linkpath": return false
    case "size":     return true  // large file
    default:         return false
    }
}
```

xattrs and most PAX records leak host-fs metadata; rejected.

## Sub-phase 17.3 — Deterministic zstd

```go
import "github.com/klauspost/compress/zstd"

func newDeterministicCompressor(w io.Writer) (*zstd.Encoder, error) {
    return zstd.NewWriter(w,
        zstd.WithEncoderLevel(zstd.SpeedBestCompression),  // 19
        zstd.WithEncoderConcurrency(1),
        zstd.WithEncoderCRC(false),                        // CRC32 is not deterministic across libs
        zstd.WithNoEntropyCompression(false),
    )
}
```

zstd frames carry no timestamp; with concurrency=1 and a fixed level, the output is deterministic across builds of klauspost/compress (which we vendor at a pinned version).

Risk: a future zstd library version may produce different bytes for the same level. Mitigation: vendor zstd at a fixed version with reproducibility-tested SHA pinned in `go.sum`; bump the Mochi major when zstd changes substantially.

## Sub-phase 17.4 — Locale enforcement

```go
// pkg/pkgrepro/locale.go
func EnforceCLocale() {
    os.Setenv("LC_ALL", "C")
    os.Setenv("LANG", "C")
    os.Setenv("TZ", "UTC")
}
```

Called at the top of `cmdPublish`, `cmdPack`. Any subprocess (none in the publish path, but safe) inherits the env.

Test: a fixture that intentionally embeds locale-sensitive data (e.g., comma-as-decimal separator) is built under three locales and asserted byte-identical.

## Sub-phase 17.5 — `mochi pkg pack --verify-reproducible`

```
mochi pkg pack --verify-reproducible --out tarball.tar.zst
```

Builds the tarball twice:

```go
func cmdPackVerifyRepro(c *cli.Context) error {
    art1, err := buildArtefact(c)
    if err != nil { return err }
    art2, err := buildArtefact(c)   // second invocation, fresh tmp dirs
    if err != nil { return err }
    if !bytes.Equal(art1.Bytes, art2.Bytes) {
        return diagnoseRepro(art1, art2)   // diffoscope-style report
    }
    if art1.BLAKE3 != art2.BLAKE3 {
        return fmt.Errorf("%w: hash differs", ErrReproE002)
    }
    fmt.Printf("reproducible: %s\n", art1.BLAKE3)
    return nil
}
```

Diagnose: a per-byte diff with a hex-window context; calls out which tar entry differs and which field (mode, mtime, content).

## Sub-phase 17.6 — Rebuilder script

A `rebuild.sh` script is shipped alongside the source package on the registry:

```bash
#!/usr/bin/env bash
set -euo pipefail
REPO="https://github.com/mochilang/strings.git"
TAG="v0.4.7"
EXPECTED_B3="e2d1a4..."
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT
git clone --depth=1 --branch="$TAG" "$REPO" "$TMPDIR/src"
cd "$TMPDIR/src"
SOURCE_DATE_EPOCH=$(git log -1 --format=%ct "$TAG")
export SOURCE_DATE_EPOCH LC_ALL=C TZ=UTC
mochi pkg pack --out "$TMPDIR/build.tar.zst"
ACTUAL_B3=$(mochi hash blake3 "$TMPDIR/build.tar.zst")
if [ "$ACTUAL_B3" != "$EXPECTED_B3" ]; then
    echo "MISMATCH: expected $EXPECTED_B3 got $ACTUAL_B3" >&2
    exit 1
fi
echo "REPRODUCIBLE: $EXPECTED_B3"
```

`SOURCE_DATE_EPOCH` derives from the tag's commit timestamp, so anyone can re-derive without knowing the original CI's wall clock.

The script is auto-generated and pushed to the registry; `mochi pkg audit blobs --rebuild` runs it against the locked tree.

## Sub-phase 17.7 — Line-ending normalisation

A `.mochi` source file checked out on Windows may have CRLF; on Linux it has LF. Reproducibility demands LF in the tarball.

```go
func normaliseFileContent(data []byte, path string) []byte {
    if !isTextFile(path) { return data }
    return bytes.ReplaceAll(data, []byte("\r\n"), []byte("\n"))
}

func isTextFile(path string) bool {
    switch filepath.Ext(path) {
    case ".mochi", ".toml", ".md", ".txt", ".json", ".yaml", ".yml": return true
    }
    return false
}
```

Binary files (images, fonts) pass through unchanged.

## Sub-phase 17.8 — Consumer-side `mochi pkg rebuild`

```
mochi pkg rebuild @mochi/strings@0.4.7                 # download source, rebuild, compare
mochi pkg rebuild --all                                # for every locked package
mochi pkg rebuild --all --fail-fast                    # first non-reproducible aborts
```

```go
func cmdRebuild(c *cli.Context) error {
    pkgs := selectPackages(c)
    for _, p := range pkgs {
        srcURL := lookupRepository(p.Name, p.Version)   // from index entry
        sde    := lookupCommitTimestamp(srcURL, p.Tag)
        artefact, err := rebuildOneInIsolation(srcURL, sde)
        if err != nil { return err }
        if artefact.BLAKE3 != p.BLAKE3 {
            return fmt.Errorf("%w: %s rebuild mismatch: expected %s got %s",
                ErrReproE002, p.Name, p.BLAKE3, artefact.BLAKE3)
        }
    }
    fmt.Println("OK: every package rebuilt to identical hash")
    return nil
}
```

Network access during rebuild: git clone of source repo. No registry access (the goal is to prove independence from the registry's bytes).

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgrepro/clock.go` | `SOURCE_DATE_EPOCH` parsing | Owner |
| `pkg/pkgrepro/walk.go` | Sorted, NFC walker | Owner |
| `pkg/pkgrepro/locale.go` | Locale enforcement | Owner |
| `pkg/pkgrepro/normalise.go` | Line-ending normalisation | Owner |
| `pkg/pkgrepro/verify.go` | Twice-build + compare | Owner |
| `pkg/pkgrepro/diagnose.go` | Diffoscope-style report | Owner |
| `pkg/pkgrepro/rebuild.go` | Consumer-side rebuild | Owner |
| `pkg/pkgpublish/sign.go` | Honour SDE in in-toto | Extends (Phase 13) |
| `cmd/mochi/pack.go` | `--verify-reproducible` flag | Owner |
| `cmd/mochi/rebuild.go` | `mochi pkg rebuild` handler | Owner |
| `tests/pkgsystem/repro/cross-tz/*` | TZ-invariance | Owner |
| `tests/pkgsystem/repro/cross-umask/*` | umask-invariance | Owner |
| `tests/pkgsystem/repro/cross-locale/*` | LC-invariance | Owner |
| `tests/pkgsystem/repro/cross-fs/*` | filesystem-invariance | Owner |
| `tests/pkgsystem/repro/sde/*` | SOURCE_DATE_EPOCH respected | Owner |
| `tests/pkgsystem/repro/diagnose/*` | Diagnostic output golden | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_REPRO_E001` | Tarball contains non-deterministic metadata (xattr, PAX record). |
| `M057_REPRO_E002` | Twice-built tarball hashes differ. |
| `M057_REPRO_E003` | Consumer rebuild does not match registry hash. |
| `M057_REPRO_E004` | Source repository tag missing or moved. |
| `M057_REPRO_E005` | SOURCE_DATE_EPOCH invalid integer. |

## Test set

- `TestPhase17SDE` — env var honoured.
- `TestPhase17WalkSorted` — paths NFC-normalised + byte-sorted.
- `TestPhase17NoXattr` — xattr entry rejected.
- `TestPhase17CrossTZ` — UTC vs +07 builds identical.
- `TestPhase17CrossUmask` — 022 vs 077 builds identical.
- `TestPhase17CrossLocale` — three locales build identical.
- `TestPhase17VerifyRepro` — `mochi pkg pack --verify-reproducible` exits 0.
- `TestPhase17Diagnose` — when broken, diagnostic identifies the differing entry.
- `TestPhase17Rebuild` — consumer rebuild matches registry hash.

## Open questions

- Whether to require reproducibility for publishes (refuse to publish if `--verify-reproducible` fails); current plan: warn at v1, require at v2.
- Whether to support a wider rebuild matrix (e.g., across mochi compiler versions); deferred to v1.1 with explicit compatibility windows.
- Whether to publish rebuild attestations from third parties (cross-rebuilders like Debian's); current plan: yes, post v1.0, accept rebuild signatures from a configured trust list.
- Windows-x86_64 as a build origin (currently consumer-only per Gate). Adding it requires a tar-builder pass that normalizes CRLF line endings, strips Windows file attributes, and forces case-sensitive sort order for paths that case-fold on NTFS. Tracked for v1.1.

## Cross-references

- Tar determinism rules: [research note 08 §3](/docs/research/0057/content-addressed-store).
- Rebuilder pattern (reproducible-builds.org): [research note 12 §A.14](/docs/research/0057/risks-and-alternatives).
- Publish-time determinism: [phase 12 §12.1](./phase-12-publish#sub-phase-121--tarball-builder).
- In-toto integration: [phase 13 §13.2](./phase-13-sigstore#sub-phase-132--in-toto-statement-v1--slsa-build-l3).
