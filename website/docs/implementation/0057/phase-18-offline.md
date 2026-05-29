---
title: "Phase 18. Offline + vendor mode"
sidebar_position: 19
sidebar_label: "Phase 18. Offline + vendor"
description: "MEP-57 Phase 18 — `mochi pkg vendor` materialises all deps to `vendor/`, `mochi build --offline` refuses any network IO, deterministic hits-only resolution against local cache."
---

# Phase 18. Offline + vendor mode

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 18](/docs/mep/mep-0057#phase-18-offline) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase18Offline`: `mochi pkg vendor` writes a complete dep tree under `vendor/`; subsequent `mochi build --offline` resolves and builds with `network = off` capability denied at the syscall layer; any cache miss aborts with `M057_OFFLINE_E001`.

Pass criteria:

1. Vendor materialisation. `mochi pkg vendor` populates `vendor/packages/<scope>/<name>/<version>/` for every locked package with a complete extracted source tree plus index entry and bundle.
2. Network deny. `mochi build --offline` denies every outbound network call at process entry (`net.Dial` returns an immediate error); the test verifies via a `net.Listen` mock that intercepts `Dial`.
3. Cache hits-only. A missing blob in cache + vendor aborts with `M057_OFFLINE_E001` naming the missing package. No fallback to network.
4. `--frozen` mode. `mochi build --frozen --offline` rejects any lockfile drift relative to the manifest (would require resolution).
5. Vendor integrity. The vendor tree's BLAKE3 of each blob matches the lockfile; `mochi pkg vendor verify` re-hashes and reports mismatches with `M057_BLOB_E006` (vendor verify hash mismatch; see [error registry](./errors)).
6. Audit offline. `mochi pkg audit --offline` uses a snapshot of the advisory feed shipped under `vendor/advisories/`; missing snapshot warns but does not fail audit.
7. Air-gap mode. With env `MOCHI_OFFLINE=hard`, every command behaves as if `--offline` was passed, and `mochi config` refuses to change registry URLs (defense against accidental network in compliance contexts).

## Goal-alignment audit

Offline + vendor is the *air-gap* surface for regulated industries, classified networks, and reproducible CI runners. Without it, a network blip blocks a build. The user-facing goal moved: "I commit `vendor/`; my CI runs with the network disabled and the build still succeeds".

This is also the operational answer to long-tail registry availability concerns: a project that has vendored its deps survives even total upstream registry loss. The cost is repo size (vendored deps add MB to the source tree); the benefit is hermeticity.

Cargo's `--offline` flag is the closest prior art (research note 03 §1); Mochi inherits the semantics and adds the syscall-layer deny so that the cache-hits-only invariant cannot be silently bypassed by a misconfigured fallback.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 18.0 | `mochi pkg vendor` materialises blobs + manifests + lock | NOT STARTED | — |
| 18.1 | `vendor/` layout: content-addressed, mirrors store schema | NOT STARTED | — |
| 18.2 | `mochi build --offline` flag: deny all outbound network | NOT STARTED | — |
| 18.3 | Cache hits-only resolution path | NOT STARTED | — |
| 18.4 | `mochi pkg audit --offline` against last-synced advisory snapshot | NOT STARTED | — |
| 18.5 | `mochi pkg vendor --frozen` rejects lockfile drift | NOT STARTED | — |
| 18.6 | `mochi pkg vendor verify` re-hash check | NOT STARTED | — |
| 18.7 | Hard offline (`MOCHI_OFFLINE=hard`) | NOT STARTED | — |

## Sub-phase 18.0 — `mochi pkg vendor`

```go
// pkg/pkgvendor/vendor.go
func Vendor(lock *Lockfile, dest string, opts Options) error {
    if err := os.MkdirAll(dest, 0755); err != nil { return err }
    for _, p := range lock.Packages {
        if p.Source == "workspace" { continue }      // skip workspace members
        if err := vendorOne(p, dest); err != nil { return err }
    }
    return writeManifest(dest, lock)
}

func vendorOne(p LockedPackage, dest string) error {
    pkgDir := filepath.Join(dest, "packages", vendorPath(p.Name), p.Version)
    if exists(pkgDir) { return nil }                 // already vendored
    src, err := store.OpenExtracted(p.BLAKE3)
    if err != nil { return err }
    if err := copyTree(src, pkgDir); err != nil { return err }
    /* also copy index entry, sigstore bundle, .caps.json */
    return nil
}

// vendorPath splits "@scope/name" or bare "name" into the on-disk layout
// (see Phase 7 §7.1 bucket scheme). Defined in pkg/pkgvendor/layout.go.
func vendorPath(name string) string { /* "@a/b" -> "@a/b" with bucket prefix */ }

func writeManifest(dest string, lock *Lockfile) error {
    // vendor/index.json: maps PackageKey -> path under vendor/
    return os.WriteFile(filepath.Join(dest, "index.json"), jsonMarshal(buildIndex(lock)), 0644)
}
```

Manifest:

```json
{
  "version": 1,
  "generated_at": "2026-05-29T07:00:01Z",
  "lockfile_sha": "abc123...",
  "packages": {
    "@mochi/json@1.2.5": {
      "path": "packages/-/json/1.2.5",
      "blake3": "..."
    }
  }
}
```

`generated_at` uses `pkgrepro.SourceDateEpoch()`
([phase 17 §17.0](./phase-17-repro#sub-phase-170--source_date_epoch)) so
two `mochi pkg vendor` runs from the same lockfile produce byte-identical
`vendor/index.json`. `packages` is sorted by `<PackageKey>@<Version>`;
inner objects emit fields in declared order. Reproducibility is asserted
by `TestPhase18VendorReproducible`: vendor twice, byte-compare.

`lockfile_sha` lets `mochi build --offline` detect lockfile drift without reading every package.

## Sub-phase 18.1 — `vendor/` layout

```
vendor/
  index.json
  packages/
    -/json/1.2.5/
      mochi.toml
      src/
      LICENSE
      .integrity                            # BLAKE3 + SHA-256
      .sigstore.bundle                      # Phase 13
      .caps.json                            # Phase 10
    -/json/1.2.5.tar.zst                    # original tarball (optional, for re-verify)
  advisories/                                # snapshot of advisory feed (optional)
    index.jsonl
    by-id/MCHI-2026-0001.yaml
```

Tarballs are optional (`mochi pkg vendor --tarballs` opts in); useful for `mochi pkg vendor verify` to re-hash without re-extraction.

## Sub-phase 18.2 — `--offline` syscall deny

The simplest deny: install a custom `Dialer` in `http.DefaultClient` that always returns an error:

```go
// pkg/pkgnet/offline.go
func EnforceOffline() {
    http.DefaultTransport = &http.Transport{
        DialContext: func(ctx context.Context, network, addr string) (net.Conn, error) {
            return nil, fmt.Errorf("%w: network access denied in offline mode", ErrOfflineE001)
        },
    }
}
```

But that only covers HTTP via `http.DefaultClient`. To cover all `net.Dial` paths, the Mochi binary applies the deny to every `Dial` call site we control (`net/http`, `net.DialContext`, etc.). For the truly hermetic case, the user runs the build under a sandbox (`bwrap --unshare-net` on Linux, `sandbox-exec` on macOS).

## Sub-phase 18.3 — Cache hits-only

The `Registry` interface is wrapped with an offline adapter:

```go
// pkg/pkgregistry/offline.go
type OfflineRegistry struct {
    Local *local.FilesystemRegistry   // points at vendor/ or $MOCHI_HOME/store
}

func (r *OfflineRegistry) Versions(pkg string) ([]VersionEntry, error) {
    entries, err := r.Local.Versions(pkg)
    if errors.Is(err, fs.ErrNotExist) {
        return nil, fmt.Errorf("%w: package %q not in vendor/ or cache", ErrOfflineE001, pkg)
    }
    return entries, err
}

func (r *OfflineRegistry) Blob(blake3 string) (io.ReadCloser, error) {
    /* same; fallback to ErrOfflineE001 on miss */
}
```

The solver runs unchanged; the only difference is the registry's miss semantics.

## Sub-phase 18.4 — `mochi pkg audit --offline`

Reuses `vendor/advisories/` (or `$MOCHI_HOME/advisories/`, canonical layout
[phase 0 §conventions](./phase-00-skeleton#files-changed)) and runs the
audit (Phase 16.3) without network refresh:

```go
func cmdAuditOffline(c *cli.Context) error {
    cacheDir := c.String("cache-dir")
    if !exists(filepath.Join(cacheDir, "index.jsonl")) {
        warnf("no advisory snapshot at %s; results may be stale", cacheDir)
    }
    feed, _ := pkgadvisory.NewLocal(cacheDir)
    hits := runAudit(feed, lock)
    return renderAndExit(hits, parseFailOn(c.String("fail-on")))
}
```

Snapshot freshness shown:

```
WARN: advisory snapshot last refreshed 2026-04-12 (47 days ago)
       run `mochi pkg audit --refresh` online to update
```

## Sub-phase 18.5 — `mochi pkg vendor --frozen`

Refuses if the lockfile is not consistent with the manifest:

```go
func VendorFrozen(m *Manifest, lock *Lockfile, dest string) error {
    if err := pkglock.CheckLock(m, lock); err != nil {
        return fmt.Errorf("%w: lockfile not in sync with manifest", ErrOfflineE002)
    }
    return Vendor(lock, dest, Options{})
}
```

This is the CI mode: a PR that bumps a dep but forgets to commit the lockfile fails the vendor step.

## Sub-phase 18.6 — `mochi pkg vendor verify`

Walks `vendor/` and re-hashes every package; reports mismatches:

```go
func VendorVerify(dest string, lock *Lockfile) error {
    var problems []string
    for _, p := range lock.Packages {
        actual, err := hashVendorTree(filepath.Join(dest, "packages", vendorPath(p.Name), p.Version))
        if err != nil { problems = append(problems, fmt.Sprintf("%s: %v", p.Name, err)); continue }
        if actual != p.BLAKE3 {
            problems = append(problems, fmt.Sprintf("%s: vendor hash %s != lock %s", p.Name, actual, p.BLAKE3))
        }
    }
    if len(problems) > 0 { return cli.Exit(strings.Join(problems, "\n"), 1) }
    return nil
}
```

This catches tampering with `vendor/` (someone edited a source file under vendor/, hoping the build would pick it up). The hash now fails.

## Sub-phase 18.7 — Hard offline

```go
// init time
func init() {
    switch os.Getenv("MOCHI_OFFLINE") {
    case "hard":
        EnforceOffline()
        forceOfflineAllCommands()
    case "soft", "":
        // honour --offline flag
    }
}
```

In `MOCHI_OFFLINE=hard`:

- All commands behave as if `--offline` was passed.
- `mochi config registry default <url>` refuses (returns `M057_OFFLINE_E003`).
- `mochi pkg publish` refuses (publishing is online by definition).
- `mochi pkg audit` uses cached feed only.
- `mochi pkg vendor` refuses (no source to vendor from).

Use case: compliance-controlled CI that must demonstrate it cannot exfiltrate.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgvendor/vendor.go` | `mochi pkg vendor` | Owner |
| `pkg/pkgvendor/verify.go` | `mochi pkg vendor verify` | Owner |
| `pkg/pkgvendor/layout.go` | `vendor/` schema | Owner |
| `pkg/pkgnet/offline.go` | Network deny | Owner |
| `pkg/pkgregistry/offline.go` | Cache-only registry | Extends (Phase 7) |
| `pkg/pkgadvisory/offline.go` | Offline audit | Extends (Phase 16) |
| `cmd/mochi/vendor.go` | `mochi pkg vendor` handler | Owner |
| `cmd/mochi/build.go` | `--offline` flag | Extends (compiler driver) |
| `tests/pkgsystem/offline/vendor-roundtrip/*` | Vendor + build | Owner |
| `tests/pkgsystem/offline/cache-miss/*` | Miss raises E001 | Owner |
| `tests/pkgsystem/offline/frozen-drift/*` | Frozen rejection | Owner |
| `tests/pkgsystem/offline/verify-tamper/*` | Tampered vendor caught | Owner |
| `tests/pkgsystem/offline/hard/*` | MOCHI_OFFLINE=hard behaviour | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_OFFLINE_E001` | Required package or blob not in vendor/ or cache. |
| `M057_OFFLINE_E002` | Frozen mode: lockfile / manifest drift. |
| `M057_OFFLINE_E003` | Hard offline: configuration change refused. |
| `M057_BLOB_E006` | Vendor verify: extracted hash does not match lock. See [error registry](./errors). |

## Test set

- `TestPhase18Vendor` — produces complete tree.
- `TestPhase18Offline` — build succeeds with no network.
- `TestPhase18CacheMiss` — missing blob aborts.
- `TestPhase18Frozen` — drift rejected.
- `TestPhase18Verify` — tampered file caught.
- `TestPhase18AuditOffline` — uses snapshot, warns if stale.
- `TestPhase18Hard` — `MOCHI_OFFLINE=hard` denies config change.

## Open questions

- Whether to support partial vendoring (only some deps vendored, rest from cache); current plan: no, all-or-nothing.
- Whether the vendor tree is committed to git (large repos) or fetched as a CI step from a shared artefact; left to the user; both flows work.
- Whether to also vendor compiler toolchains (so the build is truly hermetic including compiler version); deferred to v1.1.

## Cross-references

- Cache layout: [research note 08 §5](/docs/research/0057/content-addressed-store).
- Cargo `--offline` inheritance: [research note 03 §1](/docs/research/0057/prior-art-registries).
- Frozen check: [phase 4 §4.5](./phase-04-lockfile).
- Vendor verify reuses Phase 9 dual-hash: [phase 9 §9.0](./phase-09-content-store).
