---
title: "Phase 0. Skeleton"
sidebar_position: 1
sidebar_label: "Phase 0. Skeleton"
description: "MEP-57 Phase 0 — Go package stubs for the Mochi module and package system: pkg/pkgmanifest, pkg/pkglock, pkg/pkgsolver/pubgrub, pkg/pkgregistry, pkg/pkgblob, pkg/pkgsign, pkg/pkgcap, pkg/pkgfanout. CI workflow."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 0](/docs/mep/mep-0057#phase-0-skeleton) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase0Skeleton`: `go build ./pkg/pkgmanifest/... ./pkg/pkglock/... ./pkg/pkgsolver/... ./pkg/pkgregistry/... ./pkg/pkgblob/... ./pkg/pkgsign/... ./pkg/pkgcap/... ./pkg/pkgfanout/...` exits 0 and `go vet ./pkg/...` passes clean. No fixture execution in Phase 0; the gate is structural.

## Goal-alignment audit

Phase 0 has no user-facing output. Its value is that every subsequent phase starts from a compilable, structurally correct Go skeleton with the package boundaries fixed. The package layout below is the contract every later phase implements against; getting it wrong here multiplies churn over 19 follow-on phases.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 0.0 | `pkg/pkgmanifest` package stubs | NOT STARTED | — |
| 0.1 | `pkg/pkglock` package stubs | NOT STARTED | — |
| 0.2 | `pkg/pkgsolver/pubgrub` package stubs | NOT STARTED | — |
| 0.3 | `pkg/pkgregistry` and `pkg/pkgblob` package stubs | NOT STARTED | — |
| 0.4 | `pkg/pkgsign`, `pkg/pkgcap`, `pkg/pkgfanout` package stubs | NOT STARTED | — |
| 0.5 | `cmd/mochi pkg ...` subcommand wiring | NOT STARTED | — |
| 0.6 | CI workflow skeleton (`.github/workflows/pkgsystem-test.yml`) | NOT STARTED | — |

## Sub-phase 0.0 — pkg/pkgmanifest stubs

### Package layout

```
pkg/pkgmanifest/
  manifest.go        # Manifest, Package, Dep, Capabilities, Targets, Provenance structs
  parse.go           # ParseFile, ParseBytes; uses pelletier/go-toml/v2
  validate.go        # Validate(): schema check, reserved-name check, capability whitelist
  resolve.go         # ResolveImport(path string) (Source, error) for "scope/name@req" form
  write.go           # WriteFile: canonical TOML emit; sorted keys for round-trip stability
  semver.go          # SemverReq parse + match; Caret, Tilde, Exact, Range
  manifest_test.go   # TestPhase0Skeleton stub
```

### Manifest struct shape

```go
// pkg/pkgmanifest/manifest.go
package pkgmanifest

import "time"

type Manifest struct {
    Schema       int           `toml:"mochi-manifest"`     // = 1 for v1
    Package      Package       `toml:"package"`
    Dependencies map[string]Dep `toml:"dependencies,omitempty"`
    DevDependencies map[string]Dep `toml:"dev-dependencies,omitempty"`
    Capabilities Capabilities  `toml:"capabilities,omitempty"`
    Targets      Targets       `toml:"targets,omitempty"`
    Provenance   *Provenance   `toml:"provenance,omitempty"`
    Workspace    *Workspace    `toml:"workspace,omitempty"`
}

type Package struct {
    Name        string   `toml:"name"`
    Version     string   `toml:"version"`
    Edition     string   `toml:"edition"`               // mochi language edition, e.g. "2026"
    MinMochi    string   `toml:"min-mochi-version,omitempty"`
    Authors     []string `toml:"authors,omitempty"`
    License     string   `toml:"license,omitempty"`
    Description string   `toml:"description,omitempty"`
    Homepage    string   `toml:"homepage,omitempty"`
    Repository  string   `toml:"repository,omitempty"`
    Keywords    []string `toml:"keywords,omitempty"`
    Categories  []string `toml:"categories,omitempty"`
    Readme      string   `toml:"readme,omitempty"`      // path relative to manifest
    Include     []string `toml:"include,omitempty"`     // file globs to publish
    Exclude     []string `toml:"exclude,omitempty"`
}

type Dep struct {
    Version  string   `toml:"version,omitempty"`
    Path     string   `toml:"path,omitempty"`           // local-path dep
    Git      string   `toml:"git,omitempty"`            // git URL
    Rev      string   `toml:"rev,omitempty"`            // git commit-ish
    Tag      string   `toml:"tag,omitempty"`            // git tag
    Branch   string   `toml:"branch,omitempty"`         // git branch
    Optional bool     `toml:"optional,omitempty"`
    Features []string `toml:"features,omitempty"`       // pkg feature set
    Default  *bool    `toml:"default-features,omitempty"`
    Registry string   `toml:"registry,omitempty"`       // mirror override
    Targets  []string `toml:"targets,omitempty"`        // limit to listed transpiler targets
}

type Capabilities struct {
    Required []string  `toml:"required,omitempty"`  // capability whitelist required by this pkg
    Optional []string  `toml:"optional,omitempty"`  // capabilities enabled by feature flags
}

type Targets struct {
    Supports     []string            `toml:"supports,omitempty"`     // c, beam, jvm, dotnet, swift, kotlin, python, typescript, rust
    Defaults     []string            `toml:"defaults,omitempty"`
    Overrides    map[string]Override `toml:"overrides,omitempty"`    // per-target manifest overlay
}

type Override struct {
    Dependencies map[string]Dep `toml:"dependencies,omitempty"`
    NativeLibs   []string       `toml:"native-libs,omitempty"`      // C / system libs
    PostBuild    []string       `toml:"post-build,omitempty"`       // commands run after target build
}

// Provenance is the declarative `[provenance]` table authors put in mochi.toml.
// Phase 12-13 read it to constrain who may publish. The signed attestation
// emitted by the publish pipeline is a separate type (`pkgmanifest.Attestation`,
// declared in Phase 1 §1.3) carrying Sigstore bundle bytes and OIDC claims.
type Provenance struct {
    Publisher   string `toml:"publisher,omitempty"`     // OIDC subject expected on publish
    Repository  string `toml:"repository,omitempty"`    // GitHub/GitLab URL
    Workflow    string `toml:"workflow,omitempty"`      // workflow file path
    SourceDate  *time.Time `toml:"source-date,omitempty"`
}

type Workspace struct {
    Members  []string `toml:"members,omitempty"`
    Exclude  []string `toml:"exclude,omitempty"`
    Resolver string   `toml:"resolver,omitempty"`       // "pubgrub" (default) or "mvs"
}
```

## Sub-phase 0.1 — pkg/pkglock stubs

```
pkg/pkglock/
  lock.go            # Lockfile, LockedPkg, Source, Cap struct types
  parse.go           # ParseFile / ParseBytes
  write.go           # WriteFile: canonical TOML emit; sorted keys
  diff.go            # Diff(oldLock, newLock) (Diff, error) for `mochi pkg update`
  verify.go          # Verify(manifest, lock) (bool, error): checks consistency
  lock_test.go       # TestPhase0Skeleton stub
```

Phase 0 ships forward-declared stubs only. Phase 4 §4.0 is the canonical
source for the Lockfile, LockedPackage, Platform, and LockProvenance schemas.
The skeleton here exists so the package compiles and other phases can import
the type names; the field set lands in Phase 4.

```go
// pkg/pkglock/lock.go
package pkglock

// Lockfile is fully specified in Phase 4 §4.0. The stub below carries the
// minimum field set required by Phase 0's compile-only smoke test.
type Lockfile struct {
    Version  int             `toml:"version"`           // = 1 for v1 format
    Mochi    string          `toml:"mochi"`             // language version pin
    Packages []LockedPackage `toml:"package"`           // expanded in Phase 4
}

// LockedPackage is fully specified in Phase 4 §4.0 (LockedPackage).
type LockedPackage struct {
    Name    string `toml:"name"`
    Version string `toml:"version"`
    Source  string `toml:"source"`     // canonical encoding in Phase 4 §4.1
    BLAKE3  string `toml:"blake3,omitempty"`
    SHA256  string `toml:"sha256,omitempty"`
}
```

## Sub-phase 0.2 — pkg/pkgsolver/pubgrub stubs

```
pkg/pkgsolver/
  solver.go          # Solver interface
  pubgrub/
    solver.go        # the PubGrub algorithm
    incompat.go      # Incompatibility, Term, Set types
    decision.go      # decision stack, backtracking
    derive.go        # incompatibility derivation
    explain.go       # human-readable conflict explanations
    package_source.go # PackageProvider abstraction (registry / git / path)
    solver_test.go   # TestPhase0Skeleton stub
```

```go
// pkg/pkgsolver/pubgrub/solver.go
package pubgrub

type Solver struct {
    root     PackageKey
    provider PackageProvider
    incompat []*Incompatibility
    decisions []*Decision
    // ...
}

func (s *Solver) Solve(ctx context.Context) (*Solution, error) {
    // 1. start with the root package
    // 2. loop: pick a decision, propagate via unit propagation
    // 3. on conflict, derive a new incompatibility and backtrack
    // 4. terminate when all packages are decided or root is in conflict
    // ctx is honoured for cancellation; Phase 5.11 wires a 60s watchdog.
    return nil, nil
}
```

## Sub-phase 0.3 — pkg/pkgregistry + pkg/pkgblob stubs

```
pkg/pkgregistry/
  registry.go        # Registry interface (Versions, Manifest, Blob)
  sparse/sparse.go   # SparseRegistry (HTTPS) stub
  local/local.go     # FilesystemRegistry stub
  sparse/cache.go    # HTTP cache with ETag + If-Modified-Since stub
  registry_test.go

pkg/pkgblob/
  blob.go            # Blob fetch / store interface
  content.go         # content-addressed paths: blobs/<bl/ak/blake3>.tar.zst
  http.go            # HTTPBlobStore against blobs.mochi.dev
  fs.go              # FilesystemBlobStore
  verify.go          # blake3 + sha256 verification on read
  blob_test.go
```

## Sub-phase 0.4 — pkg/pkgsign, pkg/pkgcap, pkg/pkgfanout stubs

```
pkg/pkgsign/
  bundle.go          # Sigstore bundle struct (cert, signature, log entry)
  fulcio.go          # Fulcio cert request via OIDC token exchange
  rekor.go           # Rekor log entry submission + retrieval
  verify.go          # verify a bundle against the Sigstore root of trust
  oidc.go            # GitHub Actions / GitLab CI OIDC token fetch

pkg/pkgcap/
  caps.go            # Capability whitelist (fs.read, fs.write, net.dial, ...)
  audit.go           # diff capability sets between two lockfiles
  enforce.go         # runtime / build-time enforcement hooks

pkg/pkgfanout/
  fanout.go          # one mochi.toml -> per-ecosystem artifact builder
  npm.go             # npm package emitter (delegates to MEP-52)
  pypi.go            # PyPI wheel + sdist emitter (delegates to MEP-51)
  maven.go           # Maven Central jar emitter (delegates to MEP-47)
  nuget.go           # NuGet nupkg emitter (delegates to MEP-48)
  jsr.go             # JSR scope emitter
  crates.go          # crates.io emitter (delegates to MEP-53)
  hex.go             # hex.pm emitter (delegates to MEP-46)
  swiftpm.go         # Swift Package Index emitter (delegates to MEP-49)
```

## Sub-phase 0.5 — `mochi pkg ...` subcommand wiring

`cmd/mochi/pkg.go` adds the `pkg` subcommand tree:

```
mochi pkg new <name> [--lib | --bin]
mochi pkg init
mochi pkg add <name>[@<req>]
mochi pkg remove <name>
mochi pkg update [<name>]
mochi pkg tree
mochi pkg lock [--check]
mochi pkg fetch
mochi pkg vendor [<dir>]
mochi pkg publish [--target=<eco>] [--dry-run]
mochi pkg audit [signatures]
mochi pkg why <name>
mochi pkg search <term>
mochi pkg info <name>
mochi pkg mirror serve|sync
mochi pkg workspace ls|add|remove
```

All subcommands stubbed to return `errors.New("not implemented; tracked in phase N")` in Phase 0; later phases wire them up.

## Sub-phase 0.6 — CI workflow

`.github/workflows/pkgsystem-test.yml`:

```yaml
name: Package system tests

on:
  push:
    branches: [ main ]
  pull_request:
    paths:
      - 'pkg/**'
      - 'cmd/mochi/pkg.go'
      - 'tests/pkgsystem/**'
      - '.github/workflows/pkgsystem-test.yml'

# Default permissions: read-only on all scopes. Individual jobs widen as
# needed (Phase 13 publish-mock needs id-token: write). PRs from forks
# never receive elevated tokens; the bench job (Phase 19) refuses to run
# on a fork-PR via the conditional in §19's workflow.
permissions:
  contents: read

# Cancel superseded runs for the same PR; main-branch runs are never
# cancelled so historical builds remain reproducible.
concurrency:
  group: pkgsystem-${{ github.workflow }}-${{ github.event_name == 'pull_request' && github.head_ref || github.run_id }}
  cancel-in-progress: ${{ github.event_name == 'pull_request' }}

jobs:
  pkgsystem-test:
    runs-on: ${{ matrix.os }}
    timeout-minutes: 25
    strategy:
      fail-fast: false
      matrix:
        os: [ ubuntu-24.04, macos-15, windows-2022 ]
    steps:
      - uses: actions/checkout@v6
        with:
          # full history so SOURCE_DATE_EPOCH derivation from commit
          # timestamps works in reproducibility tests (Phase 17)
          fetch-depth: 0
      - uses: actions/setup-go@v6
        with:
          go-version-file: go.mod
          # cache key is hash(go.sum) + OS + Go version; setup-go@v6
          # composes this automatically when cache-dependency-path is set.
          cache-dependency-path: go.sum
      # Reproducibility tests need a pinned timezone and locale.
      - name: Pin environment
        run: |
          echo "TZ=UTC" >> "$GITHUB_ENV"
          echo "LC_ALL=C.UTF-8" >> "$GITHUB_ENV"
          echo "SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)" >> "$GITHUB_ENV"
        shell: bash
      - run: go test -v -timeout 900s ./pkg/...
      # Soft step: lint runs only on Linux to avoid triple cost.
      - if: matrix.os == 'ubuntu-24.04'
        run: go vet ./pkg/...
```

Action pinning policy: every `uses:` line references a tagged major
(`@v6` form). Renovate updates them as a single coordinated PR;
SHA pinning is deferred to Phase 13 (the publish workflow that signs
artefacts needs immutable references; the test workflow does not).

Secrets surface: this workflow uses no secrets. The `GITHUB_TOKEN`
default token is read-only for `contents` and unused. Phase 13.7's
`publish-test.yml` is the first workflow that needs OIDC; it explicitly
sets `permissions: { id-token: write, contents: read }`.

Fork PR behaviour: this workflow runs against fork PRs. Because there is
no secret access and no write permission, fork code cannot exfiltrate
anything. Phase 13's publish-test refuses fork PRs explicitly with `if:
github.event.pull_request.head.repo.full_name == github.repository`.

## Files changed

Conventions used across the phase tracking pages:

- **Owner** column: `Owner` = phase introduces the file or directory.
  `Extends` = phase adds new exported symbols / new responsibilities.
  `Rewrites` = phase replaces previous implementation wholesale.
- **Package layout**: there is one Go package per registry-side concern.
  Phase 0 reserves `pkg/pkgregistry/` (was earlier called `pkg/pkgindex/` in
  research notes); all index, sparse, mirror, and offline code lives under
  that root. There is no `pkg/pkgindex/` directory at v1.
- **CLI layout**: every package-system verb is a subcommand of `mochi pkg`.
  `cmd/mochi/pkg.go` (this phase) wires the verb tree; later phases add files
  named after the verb they implement (`cmd/mochi/lock.go`, etc.) but only
  register handlers, never a top-level command.
- **Cache layout**: every on-disk cache subtree is rooted at `$MOCHI_HOME/`.
  The CLI resolves the root in this order:
  1. `MOCHI_HOME` environment variable (any platform);
  2. `$XDG_CACHE_HOME/mochi` (Linux, macOS when set);
  3. `~/.cache/mochi` (Linux, macOS fallback);
  4. `%LOCALAPPDATA%\mochi` (Windows).

  Subtrees, all owned by phases that introduce the artefact stored there:

  | Path                              | Phase owner | Contents                                               |
  |-----------------------------------|-------------|--------------------------------------------------------|
  | `$MOCHI_HOME/index/`              | Phase 8     | Sparse index JSONL files + ETag cache                  |
  | `$MOCHI_HOME/store/blobs/<bb>/<aa>/<hex>` | Phase 9 | Content-addressed `.tar.zst` blobs (BLAKE3 hex)        |
  | `$MOCHI_HOME/store/extracted/<hex>/` | Phase 9  | Verified extracted trees, hardlink-installed by Phase 19 |
  | `$MOCHI_HOME/store/locks/<hex>.lock` | Phase 9  | Per-blob fcntl locks (POSIX) / LockFileEx (Windows)    |
  | `$MOCHI_HOME/advisories/`         | Phase 16    | Cached advisory feed (JSONL + by-id YAML)              |
  | `$MOCHI_HOME/fanout/<target>/<version>/` | Phase 14 | Polyglot driver staging area                        |
  | `$MOCHI_HOME/mirrors/<name>/`     | Phase 11    | Local mirror sync trees                                |
  | `$MOCHI_HOME/config/registries.toml` | Phase 8 | Per-user registry config (was `~/.config/mochi/`)      |
  | `$MOCHI_HOME/config/auth.toml`    | Phase 11    | Per-mirror bearer tokens (mode 0600)                   |

  Older phase drafts referred to `~/.cache/mochi/registry/`, `~/.config/mochi/`
  and similar paths; all are rewritten to the `$MOCHI_HOME` form. There is
  no separate config root; `config/` is a subtree of `$MOCHI_HOME` because
  registries.toml is read every command and the OS-defined config root adds
  a second lookup path that is rarely useful in CI containers.
- **Schema key naming**: two conventions coexist by design.
  Index JSONL entries (Phase 8) use compact short keys (`v`, `r`, `b3`,
  `s2`, `deps`, `y`, `cap`, `tgt`) because each line is shipped over the
  wire on every solve and the bytes add up. Manifests (`mochi.toml`,
  Phase 1) and lockfiles (`mochi.lock`, Phase 4) use long keys (`version`,
  `released`, `blake3`, `sha256`, `dependencies`, `yanked`, `capabilities`,
  `targets`) because both are human-edited and reviewed in PRs. The
  long-key form is canonical; the JSONL form is a wire encoding. Phase 8
  documents the bidirectional mapping table.

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgmanifest/*.go` | Manifest parse / validate / write stubs | Owner |
| `pkg/pkglock/*.go` | Lockfile parse / write / diff stubs | Owner |
| `pkg/pkgsolver/pubgrub/*.go` | PubGrub solver stubs | Owner |
| `pkg/pkgregistry/*.go` | Registry interface + sparse index stubs | Owner |
| `pkg/pkgblob/*.go` | Content-addressed blob store stubs | Owner |
| `pkg/pkgsign/*.go` | Sigstore bundle stubs | Owner |
| `pkg/pkgcap/*.go` | Capability whitelist stubs | Owner |
| `pkg/pkgfanout/*.go` | Polyglot fan-out stubs | Owner |
| `pkg/pkgresolve/*.go` | Local resolution stubs (extended by Phase 2) | Owner |
| `pkg/pkgworkspace/*.go` | Workspace stubs (extended by Phase 3) | Owner |
| `pkg/pkgpublish/*.go` | Publish-pipeline stubs (extended by Phase 12) | Owner |
| `pkg/pkgsbom/*.go` | SBOM stubs (extended by Phase 15) | Owner |
| `pkg/pkgadvisory/*.go` | Advisory feed stubs (extended by Phase 16) | Owner |
| `pkg/pkgrepro/*.go` | Reproducibility stubs (extended by Phase 17) | Owner |
| `pkg/pkgvendor/*.go` | Vendor stubs (extended by Phase 18) | Owner |
| `pkg/pkgnet/*.go` | Network-policy stubs (extended by Phase 18) | Owner |
| `pkg/pkgstore/*.go` | Shared cache stubs (extended by Phase 19) | Owner |
| `pkg/pkgtrace/*.go` | Tracing stubs (extended by Phase 19) | Owner |
| `pkg/pkgwhy/*.go` | Explanation stubs (extended by Phase 6) | Owner |
| `pkg/pkgemit/*.go` | Sidecar emitter stubs (extended by Phase 10) | Owner |
| `cmd/mochi/pkg.go` | `mochi pkg ...` subcommand tree | Owner |
| `.github/workflows/pkgsystem-test.yml` | CI workflow skeleton | Owner |

## Test set

- `TestPhase0Skeleton` — `go build ./pkg/...` + `go vet ./pkg/...` exit 0.
- `TestPhase0CLIWiring` — `mochi pkg --help` returns the full subcommand tree (synthetic check).

## Deferred work

- Schema versioning of `mochi.toml` past v1 (`mochi-manifest = 2`). Deferred to v2 design.
- Federated / non-central index discovery. Deferred to v2.
- Transparency log (`sum.mochi.dev`). Deferred to v2.
