---
title: "Phase 4. Lockfile format"
sidebar_position: 5
sidebar_label: "Phase 4. Lockfile format"
description: "MEP-57 Phase 4 — `mochi.lock` writer and reader: canonical TOML serialisation, version envelope, per-platform sections, manifest hash gating, `mochi pkg lock --check` semantics."
---

# Phase 4. Lockfile format

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 4](/docs/mep/mep-0057#phase-4-lockfile) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase4Lockfile`: byte-identical lockfile output across two CI hosts for the same manifest and pinned mock registry. `mochi pkg lock --check` correctly distinguishes stale (manifest changed), drift (resolution changed), and current.

Pass criteria:

1. Byte identity. The matrix `(linux x86_64, linux arm64, macos arm64, windows x86_64)` builds the same lockfile from a fixed fixture set. Hashing the bytes (SHA-256) on each platform yields the same digest. The matrix runs in CI via `.github/workflows/pkgsystem-test.yml`.
2. Round-trip. `pkglock.ParseBytes(pkglock.WriteCanonical(lock)) == lock` (DeepEqual) for every fixture.
3. Version envelope. A fixture with `version = 99` raises `M057_LOCK_E003` with a "lockfile too new" message that names the running Mochi version.
4. Manifest hash gating. `mochi pkg lock --check` against a manifest whose BLAKE3 differs from `manifest_hash` raises `M057_LOCK_E001`. The fixture corpus includes a "manifest reordered keys" case where the parsed manifest is identical but the canonical hash matches; the harness verifies the writer's canonical form ensures the hash is stable.
5. Resolution drift. With a pinned mock registry, removing a package from the lockfile triggers `M057_LOCK_E002` on `--check`.
6. Capability delta. A fixture upgrades a dep version that newly requires `net.dial`; without `--accept-capabilities`, `mochi pkg update` raises `M057_LOCK_E006` and prints the previously-seen capability set.

## Goal-alignment audit

The lockfile is the reproducibility contract. Without it, two `mochi install` runs against a moving registry produce different trees, defeating Sigstore verification and the whole supply-chain story. The user-facing goal moved: "Two engineers checking out my repo, on different OSes, produce the same dependency tree" and "Reviewing a PR that bumps a dep tells me exactly which lines moved".

Lessons explicitly inherited from prior art (research note 06 §1):

- Cargo.lock: text TOML, sorted keys, manifest_hash as the cache key (mochi follows the spirit).
- uv.lock: universal across platforms via `[[platform]]` records and per-package `[[package.platform]]` markers (mochi adopts this design directly; see §3.3 of research note 06).
- bun.lockb: binary lockfile, reversed 2025 (anti-pattern, do not repeat).
- package-lock.json v3: text JSON, deterministic; we prefer TOML for editing parity with manifest.
- go.sum: line-oriented text with one hash per line; we want richer structure.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | Lockfile struct schema; v1 envelope | NOT STARTED | — |
| 4.1 | Canonical writer (sorted keys, lowercase hex, LF, sorted package array) | NOT STARTED | — |
| 4.2 | Reader with `version = N` dispatch and forward-compat error | NOT STARTED | — |
| 4.3 | Per-platform `[[platform]]` array; `[[package.platform]]` index references | NOT STARTED | — |
| 4.4 | `manifest_hash` gating + `mochi pkg lock --check` | NOT STARTED | — |
| 4.5 | Workspace lock semantics (`source = "workspace"`) | NOT STARTED | — |
| 4.6 | Cross-platform byte-identity gate | NOT STARTED | — |
| 4.7 | `capabilities-seen` + `--accept-capabilities` | NOT STARTED | — |
| 4.8 | `mochi pkg lock --refresh` (merge-conflict path) | NOT STARTED | — |

## Sub-phase 4.0 — Struct schema

```go
// pkg/pkglock/lock.go (expands Phase 0 stub)
package pkglock

type Lockfile struct {
    Version       int                  `toml:"version"`         // = 1
    Mochi         string               `toml:"mochi"`           // compiler version
    Manifest      string               `toml:"manifest"`        // relative path
    ManifestHash  string               `toml:"manifest_hash"`   // "blake3-256:<hex>"
    Platforms     []Platform           `toml:"platform"`
    Packages      []LockedPackage      `toml:"package"`
    CapsSeen      map[string][]string  `toml:"capabilities-seen"`
    Provenance    LockProvenance       `toml:"provenance"`
}

type Platform struct {
    OS     string `toml:"os"`     // linux, macos, windows, freebsd
    Arch   string `toml:"arch"`   // x86_64, aarch64, wasm32
    Target string `toml:"target"` // typescript, python, jvm, c, beam, dotnet, swift, kotlin, rust
}

type LockedPackage struct {
    Name         string                  `toml:"name"`
    Version      string                  `toml:"version"`
    Source       string                  `toml:"source"`        // "registry:host", "workspace", "path:rel", "git:url@rev"
    BLAKE3       string                  `toml:"blake3,omitempty"`
    SHA256       string                  `toml:"sha256,omitempty"`
    Yanked       bool                    `toml:"yanked,omitempty"`
    Capabilities []string                `toml:"capabilities,omitempty"`
    Path         string                  `toml:"path,omitempty"`     // workspace / path source
    GitRev       string                  `toml:"git_rev,omitempty"`  // git source
    Dependencies map[string]string       `toml:"dependencies"`       // name -> concrete version
    Platforms    []PlatformRef           `toml:"platform"`
}

type PlatformRef struct {
    Index int `toml:"index"`
}

type LockProvenance struct {
    SolverSeed             string   `toml:"solver_seed"`
    RegistryETag           string   `toml:"registry_etag"`
    SigstoreVerifiedCount  int      `toml:"sigstore_verified_count"`
    SigstoreUnverified     []string `toml:"sigstore_unverified"`
}
```

The schema mirrors research note 06 §3 exactly. Hash strings carry the algorithm prefix (`blake3-256:<hex>`); the bare hex form is for blob filenames.

## Sub-phase 4.1 — Canonical writer

From research note 06 §4, the rules for byte identity:

1. Sorted keys: every TOML table writes keys in lexicographic order.
2. Sorted array order: `[[package]]` blocks sorted by `(name, version)`.
3. Lowercase hex for hashes.
4. Decimal integers, no leading zeros.
5. No trailing whitespace.
6. LF line endings (no CR).
7. UTF-8 NFC normalised strings.
8. Specific quoting: bare keys where allowed; basic strings for values; no literal strings; no multi-line strings.
9. Footer newline: file ends with exactly one LF.

```go
// pkg/pkglock/canonical.go
func WriteCanonical(buf *bytes.Buffer, lock *Lockfile) error {
    w := &canonicalWriter{buf: buf}
    w.kv("version", lock.Version)
    w.kv("mochi", lock.Mochi)
    w.kv("manifest", lock.Manifest)
    w.kv("manifest_hash", lock.ManifestHash)
    w.blank()
    for _, p := range lock.Platforms { // already sorted
        w.tableArray("platform")
        w.kv("os", p.OS); w.kv("arch", p.Arch); w.kv("target", p.Target)
        w.blank()
    }
    pkgs := append([]LockedPackage(nil), lock.Packages...)
    sort.Slice(pkgs, func(i, j int) bool {
        if pkgs[i].Name != pkgs[j].Name { return pkgs[i].Name < pkgs[j].Name }
        return pkgs[i].Version < pkgs[j].Version
    })
    for _, p := range pkgs {
        writePackage(w, p)
    }
    writeCapsSeen(w, lock.CapsSeen)
    writeProvenance(w, lock.Provenance)
    return w.err
}
```

Direct TOML encoding via `pelletier/go-toml/v2` is forbidden for writing. Library encoders don't guarantee canonical output. The reader is allowed to use the library; the writer must be hand-rolled.

The writer also emits a fixed header comment: `# This file is generated by mochi pkg lock. Do not edit by hand.`

Output is written via temp file + fsync + atomic rename to avoid half-written lockfiles after a crash.

## Sub-phase 4.2 — Reader with version dispatch

```go
// pkg/pkglock/parse.go
func ParseFile(path string) (*Lockfile, error) {
    buf, err := os.ReadFile(path)
    if err != nil { return nil, fmt.Errorf("%w: %s: %v", ErrLockE004, path, err) }
    return ParseBytes(buf, path)
}

func ParseBytes(buf []byte, originPath string) (*Lockfile, error) {
    var envelope struct{ Version int `toml:"version"` }
    if err := toml.Unmarshal(buf, &envelope); err != nil {
        return nil, fmt.Errorf("%w: %v", ErrLockE004, err)
    }
    switch envelope.Version {
    case 0:
        return nil, fmt.Errorf("%w: missing version envelope", ErrLockE004)
    case 1:
        return parseV1(buf)
    default:
        return nil, fmt.Errorf("%w: lock written by newer mochi (version %d > 1); upgrade",
                               ErrLockE003, envelope.Version)
    }
}
```

A v2 reader can read v1; a v1 reader cannot read v2 (research note 06 §5). Never silently downgrade.

## Sub-phase 4.3 — Platform array and per-package indices

The `[[platform]]` array deduplicates platform records across the lockfile. Each `[[package.platform]]` references one by index.

The platform set is built from the workspace's effective target set crossed with the supported `(os, arch)` matrix:

```go
func BuildPlatformSet(ws *WorkspaceState) []Platform {
    targets := union(allMemberTargets(ws))
    osArchs := []OsArch{
        {"linux", "x86_64"}, {"linux", "aarch64"},
        {"macos", "aarch64"}, {"macos", "x86_64"},
        {"windows", "x86_64"},
    }
    var out []Platform
    for _, t := range targets {
        for _, oa := range osArchs {
            if isSupported(t, oa) {
                out = append(out, Platform{OS: oa.OS, Arch: oa.Arch, Target: t})
            }
        }
    }
    sort.Sort(byPlatform(out))
    return out
}
```

Per-package presence (research note 06 §3.3) is recorded as `[[package.platform]]` index refs. A package present on every platform records every index; one present only on a subset records just that subset (uv's "marker simplification" pattern).

## Sub-phase 4.4 — Manifest hash gating

```go
func ManifestHash(manifestPath string) (string, error) {
    canonical, err := readAndCanonicaliseManifest(manifestPath)
    if err != nil { return "", err }
    h := blake3.Sum256(canonical)
    return "blake3-256:" + hex.EncodeToString(h[:]), nil
}
```

The hash is computed over the *canonical* manifest bytes (re-emit through the Phase 1 canonical writer first). This way, reordering keys or whitespace edits in `mochi.toml` do not invalidate the lock; only semantic changes do.

`mochi pkg lock --check` flow (research note 06 §8):

```go
func CheckLock(m *pkgmanifest.Manifest, lock *Lockfile) error {
    manifestHash, err := ManifestHashStruct(m)
    if err != nil { return err }
    if lock.ManifestHash != manifestHash {
        return fmt.Errorf("%w: manifest changed since lock", ErrLockE001)
    }
    // Re-resolve and compare; difference is M057_LOCK_E002.
    fresh, err := ResolveFresh(m)
    if err != nil { return err }
    if !equalLockfiles(lock, fresh) {
        return fmt.Errorf("%w: resolution drifted", ErrLockE002)
    }
    return nil
}

// CLI wrappers in `cmd/mochi/lock.go` accept paths and parse first; the core
// signature takes parsed values so Phase 18 §18.5 (frozen vendor) can reuse it
// against an already-loaded manifest/lock pair without re-reading from disk.
```

Stale (E001) is "manifest changed without re-locking". Drift (E002) is "manifest unchanged but the resolution would produce a different lockfile" (usually a mirror has yanked a version or a registry rolled forward).

## Sub-phase 4.5 — Workspace lock semantics

For each workspace member, emit one `[[package]]` block with `source = "workspace"`:

```toml
[[package]]
name = "@my/parser"
version = "0.1.0"
source = "workspace"
path = "packages/parser"

  [package.dependencies]
  "@mochi/strings" = "0.4.7"
```

No `blake3` / `sha256` for workspace sources (the integrity is the working tree itself).

When a workspace member is published, the consumer downloads only that member; the consumer's solver re-resolves transitive deps (research note 06 §7). The producer lockfile is the producer's contract, not the consumer's.

## Sub-phase 4.6 — Cross-platform byte-identity gate

`.github/workflows/pkgsystem-test.yml` runs:

```yaml
strategy:
  matrix:
    os: [ ubuntu-24.04, ubuntu-24.04-arm, macos-15, windows-2022 ]
steps:
  - run: go test -run TestPhase4ByteIdentity ./pkg/pkglock/...
  - run: sha256sum tests/pkgsystem/lockfile/byte-identity/expected.lock
```

The test:

```go
func TestPhase4ByteIdentity(t *testing.T) {
    fixture := "tests/pkgsystem/lockfile/byte-identity/manifest.toml"
    expected, _ := os.ReadFile("tests/pkgsystem/lockfile/byte-identity/expected.lock")
    lock, _ := pkglock.ResolveAndLock(fixture, mockRegistry)
    var got bytes.Buffer
    pkglock.WriteCanonical(&got, lock)
    if !bytes.Equal(got.Bytes(), expected) {
        t.Fatalf("byte identity broken; diff:\n%s", unifiedDiff(expected, got.Bytes()))
    }
}
```

The matrix posts each platform's hash to the same CI summary; a mismatch is a P0 bug.

## Sub-phase 4.7 — `capabilities-seen` and audit prompt

On `mochi pkg lock`, the lockfile records each transitive dep's declared capabilities under `[capabilities-seen]`. On `mochi pkg update`, if a new dep version newly requires a capability not in the seen set, the user is shown a warning and must opt in:

```
warning: @mochi/json 1.3.0 newly requires capability "net.dial"
  Previously seen capabilities: ["fs.read"]
  Audit and accept with: mochi pkg lock --accept-capabilities
```

```go
func DetectNewCapabilities(old, fresh *Lockfile) []CapabilityDelta {
    var out []CapabilityDelta
    for name, freshPkg := range index(fresh.Packages) {
        seen := old.CapsSeen[name]
        for _, cap := range freshPkg.Capabilities {
            if !contains(seen, cap) {
                out = append(out, CapabilityDelta{Pkg: name, NewCap: cap, Seen: seen})
            }
        }
    }
    return out
}
```

`--accept-capabilities` overrides the prompt and rolls the new capability into `capabilities-seen`. Without it, the resolver raises `M057_LOCK_E006`.

This is the supply-chain delta signal from research note 02 §6. It is the closest practical defence against the xz-utils 2024 pattern within the constraints of a v1 capability vocabulary.

## Sub-phase 4.8 — `mochi pkg lock --refresh`

For merge conflicts (research note 06 §9):

```go
func LockRefresh(manifestPath string) error {
    fresh, err := ResolveFresh(manifestPath)
    if err != nil { return err }
    return WriteCanonical(os.Stdout, fresh)
}
```

The flow is:

1. Read `mochi.toml` (the manifest is the source of truth).
2. Ignore the conflicting lockfile.
3. Resolve fresh.
4. Write a canonical lockfile.

No hand-merging. Most conflicts touch separate package blocks, but `--refresh` is always the safe path.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkglock/lock.go` | Schema (extended by Phase 10 caps, Phase 11 registry, Phase 13 provenance) | Owner |
| `pkg/pkglock/canonical.go` | Canonical writer | Owner |
| `pkg/pkglock/parse.go` | Version-dispatched reader | Owner |
| `pkg/pkglock/manifest_hash.go` | `ManifestHash` | Owner |
| `pkg/pkglock/check.go` | `CheckLock` (stale + drift) | Owner |
| `pkg/pkglock/capsdelta.go` | `DetectNewCapabilities` | Owner |
| `pkg/pkglock/refresh.go` | `LockRefresh` | Owner |
| `cmd/mochi/lock.go` | `mochi pkg lock`, `--check`, `--refresh`, `--accept-capabilities` | Owner |
| `tests/pkgsystem/lockfile/roundtrip/*` | Round-trip corpus | Owner |
| `tests/pkgsystem/lockfile/byte-identity/*` | Cross-platform identity fixture | Owner |
| `tests/pkgsystem/lockfile/stale-check/*` | E001 fixture | Owner |
| `tests/pkgsystem/lockfile/drift-check/*` | E002 fixture | Owner |
| `tests/pkgsystem/lockfile/version-envelope/*` | E003 fixture | Owner |
| `tests/pkgsystem/lockfile/caps-delta/*` | E006 fixture | Owner |
| `tests/pkgsystem/lockfile/workspace/*` | Workspace source kind | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_LOCK_E001` | Manifest hash mismatch (stale lock). |
| `M057_LOCK_E002` | Resolution mismatch (drift). |
| `M057_LOCK_E003` | Lock version too new. |
| `M057_LOCK_E004` | Invalid lockfile syntax. |
| `M057_LOCK_E005` | Missing required lock field. |
| `M057_LOCK_E006` | New capability without `--accept-capabilities`. |

Hash mismatch on cached blob surfaces as `M057_BLOB_E001` (BLAKE3
mismatch, owner Phase 9.4) rather than a Phase 4 sentinel; the lockfile
contains the expected hash but the mismatch is detected by the content
store. Phase 4 re-raises with `%w` rather than redeclaring a code.

## Test set

- `TestPhase4Roundtrip` — write -> read -> DeepEqual.
- `TestPhase4ByteIdentity` — same bytes across platforms.
- `TestPhase4StaleCheck` — E001 fires.
- `TestPhase4DriftCheck` — E002 fires.
- `TestPhase4VersionEnvelope` — E003 fires for `version = 99`.
- `TestPhase4CapsDelta` — E006 fires for new cap without flag; passes with flag.
- `TestPhase4Refresh` — `--refresh` produces a clean lock.

## Open questions

- Whether to expose `provenance.registry_etag` to `mochi pkg why` for debug; current plan: yes, but only when `--verbose`.
- Whether to record `[provenance].solver_decision_count` as a perf metric; landed in [Phase 19 §19.3](./phase-19-cache-perf) (solver memo), which is the natural home for solver-timing counters.
- Whether `capabilities-seen` should be per-version or per-package; current plan: per-package, taking the union (keeps the delta noise low).

## Cross-references

- Lockfile format: [research note 06](/docs/research/0057/lockfile-format).
- Bun.lockb reversal lesson: [research note 02 §3](/docs/research/0057/design-philosophy).
- Capability delta semantics: [research note 02 §6](/docs/research/0057/design-philosophy).
- Manifest hashing: [research note 04 §11](/docs/research/0057/manifest-format).
- Per-platform sections: [research note 06 §3.3](/docs/research/0057/lockfile-format).
