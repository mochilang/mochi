---
title: "Phase 11. Registry mirror protocol"
sidebar_position: 12
sidebar_label: "Phase 11. Mirror protocol"
description: "MEP-57 Phase 11 — mirror discovery via `[[registry.alternate]]`, replication semantics, Sigstore-verified integrity preserving against malicious mirrors, `mochi pkg mirror serve / sync` commands."
---

# Phase 11. Registry mirror protocol

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 11](/docs/mep/mep-0057#phase-11-mirror) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase11Mirror`: a mock mirror serves the same content as the upstream and resolves identically; a mock mirror that serves modified content fails Sigstore verification; failover from default to alternate works on upstream 503.

Pass criteria:

1. Mirror parity. A mirror populated by `mochi pkg mirror sync` from a known upstream root resolves every fixture identically to direct upstream resolution; the produced lockfile is byte-identical.
2. Tamper resistance. A mirror that returns a tarball whose BLAKE3 does not match the index entry's `b3` raises `M057_BLOB_E001` and the client moves to the next alternate.
3. Sigstore preservation. A tarball with a hash that matches but whose Sigstore bundle does not verify against the trust root raises `M057_SIG_E004` and is also rejected (proves a mirror cannot substitute a "validly hashed" tarball that lacks signature, once Phase 13 is enabled).
4. Failover. With `upstream = "https://index.mochi.dev"` and `[[registry.alternate]] url = "https://mirror.corp/internal"`, a primary 503 routes to the alternate; on alternate 200 the resolution succeeds and the lockfile records the source mirror in `[provenance]`.
5. Divergence detection. `mochi pkg audit mirror corp` compares a sample of upstream index entries against the mirror and reports any divergence with `M057_INDEX_E006`.
6. Sync efficiency. A second `mochi pkg mirror sync` against an already-populated dest skips entries whose ETag is unchanged; the test asserts only changed entries trigger blob copies.

## Goal-alignment audit

Mirrors are the operational answer to "what if the central registry goes down". Without them, an outage at `index.mochi.dev` blocks every Mochi build globally. The user-facing goal moved: "My corporate proxy hosts a Mochi mirror; my builds resolve against it transparently and Sigstore still verifies".

The threat model that justifies Phase 11 explicitly: a corporate mirror is not a trust boundary. The mirror operator may be compromised; the upstream signer is the only trusted party. Therefore every blob fetched via a mirror must verify identically to one fetched from upstream (same BLAKE3, same SHA-256, same Sigstore bundle). The mirror is an availability mechanism, not an authorisation mechanism.

This contrasts with corporate package proxies in the Java/Python world (Artifactory, Sonatype Nexus), where the proxy can re-sign or rewrite. Mochi's mirrors are content-addressed pass-throughs; rewriting would invalidate the signature chain.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Mirror discovery: `[[registry.alternate]]` parsed, fallback chain | NOT STARTED | — |
| 11.1 | Failover semantics on upstream error | NOT STARTED | — |
| 11.2 | Sigstore + dual-hash verification preserves against mirror substitution | NOT STARTED | — |
| 11.3 | `mochi pkg mirror sync --upstream=<url> --dest=<dir>` | NOT STARTED | — |
| 11.4 | `mochi pkg mirror serve --root=<dir>` | NOT STARTED | — |
| 11.5 | `mochi pkg audit mirror <name>` cross-check | NOT STARTED | — |
| 11.6 | Mirror source recorded in lockfile `[provenance]` | NOT STARTED | — |

## Sub-phase 11.0 — Mirror discovery

Configuration lives at `$MOCHI_HOME/config/registries.toml` (introduced in
Phase 8.8; canonical layout: [phase 0
§conventions](./phase-00-skeleton#files-changed)):

```toml
[default]
url   = "https://index.mochi.dev"
blobs = "https://blobs.mochi.dev"

[[alternate]]
name  = "corp"
url   = "https://mirror.corp.example/mochi-registry"
blobs = "https://mirror.corp.example/mochi-blobs"
priority = 10                # lower wins; default 100

[[alternate]]
name  = "cloudflare"
url   = "https://mochi-mirror.cloudflarestorage.com"
blobs = "https://mochi-mirror.cloudflarestorage.com/blobs"
priority = 50
```

Parser:

```go
// pkg/pkgregistry/config.go
type Config struct {
    Default     Registry
    Alternates  []Registry
}

type Registry struct {
    Name     string  // empty for default
    URL      string
    Blobs    string  // optional blob endpoint; defaults to <URL>/blobs
    Token    string  // optional bearer for private mirrors
    Priority int     // sort order for alternates
}

func LoadConfig(path string) (*Config, error) { /* TOML decode + validate */ }
```

Validation:

- `URL` and `Blobs` must parse to `https://` URLs (or `http://` only for `localhost`).
- `Name` must be unique among alternates.
- Priority sort is stable; ties broken by declared order.

## Sub-phase 11.1 — Failover semantics

`FailoverRegistry` from Phase 8.7 is generalised to wrap arbitrarily many `Registry` instances:

```go
// pkg/pkgregistry/failover.go
type FailoverChain struct {
    Primary  Registry
    Fallback []Registry  // sorted by priority
}

func (c *FailoverChain) Versions(pkg string) ([]VersionEntry, error) {
    if entries, err := c.Primary.Versions(pkg); err == nil {
        return entries, nil
    } else if !isRetryable(err) {
        return nil, err  // 404 from primary is final
    }
    for _, alt := range c.Fallback {
        entries, err := alt.Versions(pkg)
        if err == nil {
            return entries, nil
        }
    }
    return nil, ErrAllRegistriesFailed
}
```

Retryable categorisation (research note 07 §8):

| Error from primary | Retryable? | Reason |
|--------------------|-----------|--------|
| Network timeout / DNS fail | yes | transient |
| `503 Service Unavailable` | yes | known transient |
| `429 Too Many Requests` | yes (after `Retry-After`) | rate limit |
| `404 Not Found` | no | authoritative absence |
| `410 Gone` | no | authoritative deletion |
| TLS verification failure | no | suspicious |
| `200` with malformed body | yes | could be transient corruption |

The blob endpoint follows the same failover chain independently: a blob may be served by `cloudflare` even if the index came from `corp` (both verify against the same hash anyway).

## Sub-phase 11.2 — Sigstore + dual-hash preservation

The integrity-preservation invariant (research note 12 §A.10):

> A mirror is just a CDN. Every byte the client fetches MUST be verified against either:
> 1. The BLAKE3 hex embedded in the URL or index entry (Phase 9), AND
> 2. The Sigstore bundle (Phase 13) chained to a trust root the client controls.

The mirror has no key material that the client trusts. A mirror that substitutes the index entry must also substitute the Sigstore bundle and forge a Rekor inclusion proof. The Rekor cross-check (Phase 13.5) makes this detectable: the client queries the upstream Rekor log (not the mirror's copy) to verify inclusion.

Test fixture: `tests/pkgsystem/mirror/tampered/`:

- Mirror responds 200 to `/blobs/<correct-blake3>` with bytes that hash to `<other-blake3>`.
- Client computes BLAKE3 streaming (Phase 9.0); detects mismatch.
- Client raises `M057_BLOB_E001`, removes the mirror from candidate set for the rest of the run (cooldown 5 minutes), retries next alternate.

## Sub-phase 11.3 — `mochi pkg mirror sync`

```go
// cmd/mochi/mirror_sync.go
func cmdMirrorSync(c *cli.Context) error {
    upstream := c.String("upstream")
    dest    := c.String("dest")
    since   := c.String("since")    // optional timestamp
    src, _ := sparse.New(upstream)
    dst, _ := local.New(dest)
    pkgs, _ := src.ListPackages(since)
    sem := make(chan struct{}, c.Int("concurrency"))
    var wg sync.WaitGroup
    for _, pkg := range pkgs {
        wg.Add(1)
        sem <- struct{}{}
        go func(p string) {
            defer wg.Done(); defer func() { <-sem }()
            mirrorOne(src, dst, p)
        }(pkg)
    }
    wg.Wait()
    return nil
}
```

The `mirrorOne` routine:

1. Fetch `Versions(pkg)` from upstream with ETag.
2. If 304, skip (mirror entry is current).
3. Else write the JSONL atomically to `dest/<bucket>/<scope>/<name>`.
4. For each new entry's `b3`, fetch the blob; verify hash; copy to `dest/blobs/<bb>/<aa>/<hex>`.
5. Fetch the Sigstore bundle (Phase 13); copy alongside.

Atomic write: write to `<path>.tmp.<random>`, fsync, rename. Survives crash.

Flags:

- `--concurrency=N` (default 8 by `MOCHI_INDEX_PARALLELISM`).
- `--since=2026-01-01T00:00:00Z` mirrors only entries with `r` >= cutoff.
- `--bucket=ab/cd` mirrors only one bucket (useful for sharded mirrors).
- `--dry-run` prints what would change.

A successful sync writes a `dest/.last-sync.json`:

```json
{
  "upstream": "https://index.mochi.dev",
  "finished_at": "2026-05-29T14:02:33+07:00",
  "packages_synced": 1247,
  "blobs_copied": 19,
  "bytes_copied": 41271819
}
```

`finished_at` is operational metadata and intentionally records the actual
wall-clock end of the sync (not `SOURCE_DATE_EPOCH`); `/healthz` and
mirror-staleness audits depend on it being real time. The mirror's served
*content* (index entries, blobs, bundles) is byte-identical to upstream
and reproducible; `.last-sync.json` is metadata that lives next to it, not
part of the registry surface.

## Sub-phase 11.4 — `mochi pkg mirror serve`

A wrapper around Phase 7.3's `mochi registry serve` that adds:

- Authentication: `--auth=bearer:<token>` for private mirrors.
- Read-only enforcement: rejects `PUT` / `POST` with 405.
- Metrics: `/metrics` Prometheus endpoint exposing fetch latency p50/p95/p99 per package.
- Healthcheck: `/healthz` returns 200 if `.last-sync.json` is fresh (within 24h).

```go
func cmdMirrorServe(c *cli.Context) error {
    root := c.String("root")
    reg, _ := local.New(root)
    mux := http.NewServeMux()
    mux.HandleFunc("/healthz", healthHandler(root))
    mux.HandleFunc("/metrics", promhttp.Handler().ServeHTTP)
    mux.HandleFunc("/", authMiddleware(
        readOnlyMiddleware(
            registry.NewHTTPHandler(reg))))
    return http.ListenAndServeTLS(c.String("addr"), c.String("cert"), c.String("key"), mux)
}
```

## Sub-phase 11.5 — `mochi pkg audit mirror <name>`

Cross-check: sample the upstream index and compare to the mirror's view. Reports divergence:

```go
func cmdAuditMirror(c *cli.Context) error {
    cfg, _ := LoadConfig("")
    mirror := cfg.FindAlternate(c.Args().First())
    upstream := cfg.Default
    sample := sampleStrategy(c.Int("samples"))   // top 100 packages by default
    var diffs []Divergence
    for _, pkg := range sample {
        u, _ := upstream.Versions(pkg)
        m, _ := mirror.Versions(pkg)
        if d := compareEntries(u, m); d != nil { diffs = append(diffs, *d) }
    }
    if len(diffs) > 0 {
        printDivergence(diffs)
        return cli.Exit("M057_INDEX_E006: mirror diverged", 1)
    }
    fmt.Println("OK: mirror matches upstream for sampled packages")
    return nil
}
```

Compare strategy: ETag first (cheap); if etags differ, parse and field-diff per version. Diff output:

```
@mochi/json: 2 versions diverge
  1.2.5: upstream b3=abc.. mirror b3=def..  <-- HASH DIFFER
  1.2.4: upstream yanked=true mirror yanked=false
```

A hash divergence is a P0 forensic signal (the mirror is serving a different artefact for the same name+version).

## Sub-phase 11.6 — Mirror source in lockfile

When a package is resolved through an alternate, the lockfile's per-package `[provenance]` records which registry served the bytes:

```toml
[[package]]
name = "@mochi/json"
version = "1.2.5"
source = "registry"
registry = "https://mirror.corp.example/mochi-registry"   # not the default
blake3 = "..."
sha256 = "..."
```

This makes "did this build hit the corporate mirror?" auditable by reading the lockfile, with no need to re-run the resolution. Also feeds the SBOM (Phase 15).

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgregistry/config.go` | `[default]` + `[[alternate]]` parsing | Owner |
| `pkg/pkgregistry/failover.go` | Chained `Versions` / `Blob` (rewrites Phase 8 stub) | Rewrites (Phase 8) |
| `pkg/pkgregistry/mirror.go` | Sync engine | Owner |
| `cmd/mochi/mirror.go` | `mochi pkg mirror sync / serve` handler | Owner |
| `cmd/mochi/audit_mirror.go` | `mochi pkg audit mirror` handler | Owner |
| `pkg/pkglock/lock.go` | `registry` field per package | Extends (Phase 4) |
| `tests/pkgsystem/mirror/sync-roundtrip/*` | Sync then resolve through mirror | Owner |
| `tests/pkgsystem/mirror/tampered/*` | Hash mismatch rejection | Owner |
| `tests/pkgsystem/mirror/failover/*` | Upstream 503, alternate 200 | Owner |
| `tests/pkgsystem/mirror/divergence/*` | Audit detects divergence | Owner |

## Error code surface

Phase 11 owns `M057_INDEX_E006` (mirror divergence) and the `M057_MIRROR_*`
codes listed in the [error registry](./errors). The hash-mismatch case
(`M057_BLOB_E001`) is sourced from Phase 9; the Sigstore failure
(`M057_SIG_E004`) is sourced from Phase 13. Phase 11 re-raises with `%w`
rather than redeclaring.

## Test set

- `TestPhase11Discovery` — `[[registry.alternate]]` parsed in declared+priority order.
- `TestPhase11Failover` — primary 503, alternate 200 succeeds.
- `TestPhase11Tampered` — mismatched bytes raise E001.
- `TestPhase11SyncIncremental` — second sync skips unchanged entries.
- `TestPhase11Divergence` — `audit mirror` reports mismatched hashes.
- `TestPhase11LockfileSource` — lockfile records the serving registry.

## Open questions

- Whether to support pull-through-cache mirrors that lazily fetch from upstream on miss; current plan: no, mirrors are pre-populated.
- Whether to allow a mirror with a different trust root for Sigstore (e.g., corporate intermediate CA on top of Sigstore Fulcio); current plan: no at v1, Fulcio public root only.
- Whether to expose mirror health in `mochi doctor`; current plan: yes, `mochi doctor` pings every configured mirror.

## Cross-references

- Mirror discussion: [research note 07 §8](/docs/research/0057/registry-index).
- Mirror divergence risk: [research note 12 §A.10](/docs/research/0057/risks-and-alternatives).
- Sigstore verification: Phase 13.
- Failover from Phase 8.7: [phase 8 §8.7](./phase-08-sparse-index#sub-phase-87--registry-failover).
