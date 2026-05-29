---
title: "Phase 2. Local resolution"
sidebar_position: 3
sidebar_label: "Phase 2. Local resolution"
description: "MEP-57 Phase 2 — Manifest activates resolver; path-form imports keep working; scoped imports resolve to a local content-addressed cache; manifest-less mode for single-file scripts continues unchanged."
---

# Phase 2. Local resolution

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 2](/docs/mep/mep-0057#phase-2-local-resolution) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase2LocalResolution`: every fixture program under `tests/pkgsystem/local-resolve/` either parses and resolves (and writes a deterministic resolved-tree JSON identical to its golden) or fails with the exact expected error code.

Pass criteria:

1. Specifier classifier. `pkgresolve.Classify(spec)` returns the right kind for every entry in `tests/pkgsystem/local-resolve/classify/cases.json` (250+ rows covering path, scoped, unscoped, FFI-tagged, mixed-shape, and degenerate inputs).
2. Manifest discovery. Given a working directory, `pkgresolve.DiscoverManifest(cwd)` walks parents and returns either a manifest path (preferring `mochi.workspace.toml` over `mochi.toml`) or a "manifest-less" verdict. Both outcomes are covered by separate fixtures.
3. Cache lookup. For every scoped import in `tests/pkgsystem/local-resolve/scoped-cached/`, the resolver returns a `ResolvedModule` whose source path lives under the test cache root (`tests/pkgsystem/local-resolve/scoped-cached/cache/`). The resolved tree is dumped as canonical JSON and compared against `golden.json`.
4. Manifest-less mode. Path-form and FFI imports work without a manifest. Any scoped import in manifest-less mode raises `M057_RESOLVE_E002` and the error message contains the literal hint `did you forget mochi init?`.
5. Version mismatch. When the in-source `@req` disagrees with the manifest pin, `pkgresolve.Resolve` raises `M057_RESOLVE_E003` and the error message shows both the in-source and in-manifest specs.
6. Regression. Every `examples/v0.7/**/*.mochi` still resolves without changes; the test harness reuses `tests/examples_test.go` for the regression sweep.

## Goal-alignment audit

Phase 2 is where existing path-form imports stay green AND new scoped imports start resolving against a pre-populated local cache. The user-facing goal moved: "I can write `import "@mochi/strings@^0.4" as str` in a file inside a manifest-rooted directory, and the compiler finds the cached source".

The scoped-import surface is purely additive: no existing token is repurposed, no existing AST node changes. The classifier is a 12-line function over the first character class (see research note 01 §2.1). Path-form regression is the highest-cost failure mode if the dispatch is wrong, because every Mochi example today uses path form. The fixture corpus therefore weighs path-form regression heavily.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.0 | Specifier classifier (path / scoped / unscoped / FFI / ambiguous) | NOT STARTED | — |
| 2.1 | Manifest discovery (walk parents for `mochi.workspace.toml` / `mochi.toml`) | NOT STARTED | — |
| 2.2 | Cache lookup at `$MOCHI_HOME/store/extracted/<blake3>/` (see [layout](./phase-00-skeleton#sub-phase-06--ci-workflow)) | NOT STARTED | — |
| 2.3 | Manifest-less mode: path + FFI work; scoped fails with M057_RESOLVE_E002 | NOT STARTED | — |
| 2.4 | In-source `@req` vs manifest `version` mismatch detection | NOT STARTED | — |
| 2.5 | Resolver dispatch wired into `parser/import.go` | NOT STARTED | — |
| 2.6 | Regression: every existing example under `examples/v0.7/` still resolves | NOT STARTED | — |
| 2.7 | Resolved-tree canonical JSON dump | NOT STARTED | — |

## Sub-phase 2.0 — Specifier classifier

The classifier is a single function over the first character class plus FFI tag presence. From research note 01 §2:

| First character | Specifier shape                               | Resolver path                                 |
|-----------------|-----------------------------------------------|-----------------------------------------------|
| `.` or `/`      | file-relative path (existing)                 | `runtime/mod/mod.go` walk-and-join (unchanged)|
| `@`             | scoped package: `@scope/name[@req]`           | `pkg/pkgmanifest` + `pkg/pkgsolver` + `pkg/pkgregistry` |
| `[a-z]`         | unscoped package: `name[@req]`                | same as scoped, scope inferred from registry  |
| (lang tag)      | FFI: `go|python|typescript|rust "..."`        | per-target FFI driver (unchanged)             |

```go
// pkg/pkgresolve/classify.go
package pkgresolve

type SpecifierKind int

const (
    KindUnknown SpecifierKind = iota
    KindPathRelative
    KindPathAbsolute
    KindScopedPackage
    KindUnscopedPackage
    KindFFIGo
    KindFFIPython
    KindFFITypeScript
    KindFFIRust
    KindFFISwift
    KindAmbiguous
)

type Classification struct {
    Kind    SpecifierKind
    Scope   string // for scoped packages
    Name    string
    Req     string // optional in-source version spec
    Lang    string // for FFI tags
}

func Classify(stmt parser.ImportStmt) (Classification, error) {
    if stmt.Lang != "" {
        return classifyFFI(stmt)
    }
    spec := stmt.Path
    switch {
    case len(spec) == 0:
        return Classification{}, fmt.Errorf("%w: empty specifier", ErrAmbiguousSpec)
    case spec[0] == '.':
        if containsAtVersion(spec) {
            return Classification{Kind: KindAmbiguous}, fmt.Errorf("%w: path form cannot carry @req", ErrAmbiguousSpec)
        }
        return Classification{Kind: KindPathRelative}, nil
    case spec[0] == '/':
        return Classification{Kind: KindPathAbsolute}, nil
    case spec[0] == '@':
        return parseScopedSpec(spec)
    default:
        return parseUnscopedSpec(spec)
    }
}
```

The mixed-shape case (`import "./util@^1" as x`) raises `M057_RESOLVE_E001`. The classifier does no I/O.

## Sub-phase 2.1 — Manifest discovery

From research note 01 §3:

1. Start at the entry point's directory.
2. Walk up looking for `mochi.workspace.toml`. If found, that directory is the workspace root.
3. Otherwise walk up looking for `mochi.toml`. The first directory containing it is the package root.
4. If neither is found, the entry point runs in *manifest-less mode*.

```go
// pkg/pkgresolve/discover.go
type DiscoveryResult struct {
    Workspace    *pkgmanifest.Manifest   // workspace root manifest if found
    WorkspacePath string
    Package      *pkgmanifest.Manifest   // nearest package manifest if found
    PackagePath  string
    ManifestLess bool
}

func DiscoverManifest(startDir string) (DiscoveryResult, error) {
    // First walk: look for mochi.workspace.toml all the way to /
    workspacePath, workspaceManifest := walkUpFor(startDir, "mochi.workspace.toml")

    // Second walk: stop at the first mochi.toml
    packagePath, packageManifest := walkUpFor(startDir, "mochi.toml")

    if workspacePath == "" && packagePath == "" {
        return DiscoveryResult{ManifestLess: true}, nil
    }
    return DiscoveryResult{
        Workspace: workspaceManifest, WorkspacePath: workspacePath,
        Package: packageManifest, PackagePath: packagePath,
    }, nil
}
```

Filesystem stop conditions: filesystem root (`/`), volume root on Windows, or the user home directory (configurable via `MOCHI_DISCOVERY_STOP_AT`). Symlinks are resolved but not followed across volume boundaries.

A directory containing both `mochi.toml` and `mochi.workspace.toml` is rejected with `M057_RESOLVE_E010` to avoid ambiguity (research note 04 §8).

## Sub-phase 2.2 — Cache lookup

The local content-addressed cache (research note 08 §7) is rooted at
`$MOCHI_HOME` (canonical layout: [phase 0
§conventions](./phase-00-skeleton#files-changed)):

```
$MOCHI_HOME/                       # ~/.cache/mochi by default
  store/
    blobs/<bb>/<aa>/<hex>.tar.zst  # content-addressed (Phase 9)
    extracted/<hex>/               # verified extracted trees (Phase 9)
      manifest.toml                # byte-identical to publish-time
      src/
      LICENSE
      provenance.json              # Sigstore bundle if verified
      .integrity                   # blake3 + sha256 lines
    locks/<hex>.lock               # fcntl per-blob lock (Phase 9)
  index/<bucket>/<scope>/<name>    # cached sparse-index responses (Phase 8)
  tmp/                             # download staging
```

Phase 2 reads from this layout but does not own any of the subtrees;
ownership is documented in the canonical layout table.

Resolver flow for a scoped import (research note 01 §7):

```go
func (r *PkgResolver) Resolve(c Classification, ctx *Context) (ResolvedModule, error) {
    // 1. lockfile pin
    locked, ok := ctx.Lock.Lookup(c.Scope, c.Name)
    if !ok {
        return ResolvedModule{}, errLockedPkgMissing(c)
    }
    if c.Req != "" {
        if err := assertReqMatches(c.Req, locked.Version); err != nil {
            return ResolvedModule{}, err  // M057_RESOLVE_E003
        }
    }

    // 2. cache lookup
    cachePath := ctx.Cache.PackageDir(c.Scope, c.Name, locked.Version)
    if exists(cachePath) {
        if err := verifyIntegrity(cachePath, locked.BLAKE3, locked.SHA256); err != nil {
            // poisoned cache; corrupt entry and refetch in later phase
            return ResolvedModule{}, fmt.Errorf("%w: %s", ErrCachePoisoned, cachePath)
        }
        return ResolvedModule{SourceRoot: cachePath, Version: locked.Version}, nil
    }

    // Phase 2 stops here: cold-cache fetch is Phase 8 (sparse index)
    return ResolvedModule{}, ErrColdCache
}
```

The `ErrColdCache` sentinel is intentional in Phase 2: this phase only resolves against a pre-populated cache. The fetch path is Phase 8.

Integrity verification computes BLAKE3 of the extracted tree as defined in research note 08 §3, walking files in sorted-by-path order. SHA-256 is the secondary hash for SLSA / Sigstore.

## Sub-phase 2.3 — Manifest-less mode

When `DiscoverManifest` returns `ManifestLess: true`:

- Path imports work. The existing `runtime/mod/mod.go` resolver is used unchanged.
- FFI imports work. The per-language driver is dispatched unchanged.
- Scoped imports raise `M057_RESOLVE_E002`:

```
error: scoped import "@mochi/strings@^0.4" requires a manifest
  --> hello.mochi:3:8
   |
 3 | import "@mochi/strings@^0.4" as str
   |        ^^^^^^^^^^^^^^^^^^^^^
   = note: no mochi.toml or mochi.workspace.toml found in this directory or any parent
   = help: did you forget mochi init?
```

The literal `did you forget mochi init?` is required by the test harness.

## Sub-phase 2.4 — Version mismatch detection

If `@req` appears in source and the manifest pins a version, the parser checks them:

```go
func assertReqMatches(sourceReq, lockedVersion string) error {
    r, err := pkgmanifest.ParseRange(sourceReq)
    if err != nil { return err }
    v, _ := pkgmanifest.ParseVersion(lockedVersion)
    if !r.Matches(v) {
        return fmt.Errorf("%w: source spec %q does not match locked %q",
            ErrVersionMismatch, sourceReq, lockedVersion)
    }
    return nil
}
```

If the in-source `@req` is absent, the locked version wins (research note 01 §2).

If the in-manifest version range and the in-source `@req` disagree (e.g. manifest says `^0.4`, source says `@^0.5`), the parser raises `M057_RESOLVE_E003` regardless of which one matches the locked version.

## Sub-phase 2.5 — Resolver dispatch wired in

`parser/import.go` already produces `ast.ImportStmt`. Phase 2 routes resolution:

```go
// parser/import.go (existing, extended)
func (p *Parser) resolveImport(stmt *ast.ImportStmt) (*ast.Module, error) {
    c, err := pkgresolve.Classify(stmt)
    if err != nil { return nil, err }
    switch c.Kind {
    case pkgresolve.KindPathRelative, pkgresolve.KindPathAbsolute:
        return p.pathResolver.Resolve(stmt)            // existing code
    case pkgresolve.KindScopedPackage, pkgresolve.KindUnscopedPackage:
        return p.pkgResolver.Resolve(c, p.ctx)         // new
    case pkgresolve.KindFFIGo, pkgresolve.KindFFIPython,
         pkgresolve.KindFFITypeScript, pkgresolve.KindFFIRust,
         pkgresolve.KindFFISwift:
        return p.ffiResolver.Resolve(stmt)             // existing code
    case pkgresolve.KindAmbiguous:
        return nil, errAmbiguousSpec(stmt)
    }
    return nil, fmt.Errorf("unknown specifier kind: %v", c.Kind)
}
```

The path resolver and FFI resolver branches use the existing implementations; no change required.

## Sub-phase 2.6 — Regression

Every fixture under `examples/v0.7/` must still resolve after Phase 2 lands. The harness:

```go
// tests/pkgsystem/regression_test.go
func TestPhase2Regression(t *testing.T) {
    for _, mochi := range glob(t, "examples/v0.7/**/*.mochi") {
        t.Run(mochi, func(t *testing.T) {
            if _, err := parser.ParseFile(mochi); err != nil {
                t.Fatalf("regression: %v", err)
            }
        })
    }
}
```

If the regression sweep finds a file that previously parsed and now fails, the phase is not done.

## Sub-phase 2.7 — Resolved-tree canonical JSON dump

For fixture stability the resolver emits a canonical JSON of the resolved tree:

```json
{
  "root": {"scope": "", "name": "demo", "version": "0.1.0"},
  "modules": [
    {
      "scope": "", "name": "demo", "version": "0.1.0",
      "source": "file://./packages/demo",
      "imports": [
        {"alias": "str", "scope": "mochi", "name": "strings", "version": "0.4.7"}
      ]
    },
    {
      "scope": "mochi", "name": "strings", "version": "0.4.7",
      "source": "cache:///mochi/strings/0.4.7",
      "imports": []
    }
  ]
}
```

Sorted by `(scope, name, version)`. Compared against `golden.json` byte-for-byte after a `json.Marshal` round-trip with sorted-keys output.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgresolve/classify.go` | Specifier classifier | Owner |
| `pkg/pkgresolve/discover.go` | Manifest discovery | Owner |
| `pkg/pkgresolve/resolve.go` | Top-level `Resolve` dispatch (extended by Phase 3) | Owner |
| `pkg/pkgresolve/cache.go` | Cache lookup + integrity verification | Owner |
| `pkg/pkgresolve/dump.go` | Canonical JSON dump | Owner |
| `pkg/pkgresolve/errors.go` | Sentinel errors (M057_RESOLVE_E001, etc.) | Owner |
| `parser/import.go` | Dispatch hook | Extends |
| `tests/pkgsystem/local-resolve/path-only/*.mochi` | Path regression corpus | Owner |
| `tests/pkgsystem/local-resolve/scoped-cached/*.mochi` | Scoped cache-hit corpus | Owner |
| `tests/pkgsystem/local-resolve/version-mismatch/*.mochi` | Mismatch corpus | Owner |
| `tests/pkgsystem/local-resolve/classify/cases.json` | Classifier coverage | Owner |
| `tests/pkgsystem/local-resolve/discover/*` | Discovery test trees | Owner |

## Error code surface

Sources (see [error registry](./errors)). Verbal aliases used in early
drafts are renamed to the canonical `M057_RESOLVE_E<NNN>` form:

| Code | Trigger |
|------|---------|
| `M057_RESOLVE_E002` | Scoped import without a discoverable manifest. |
| `M057_RESOLVE_E001` | Mixed-shape specifier (e.g. `./foo@^1`). |
| `M057_RESOLVE_E010` | Both `mochi.toml` and `mochi.workspace.toml` in the same directory. |
| `M057_RESOLVE_E003` | In-source `@req` disagrees with manifest or lockfile. |
| `M057_RESOLVE_E004` | Resolved import is not in the lockfile (or 404 at registry). |
| `M057_RESOLVE_E009` | Cache entry exists but BLAKE3 does not match the lockfile. |
| `M057_RESOLVE_E007` | Phase 2 only: cold-cache sentinel for a scoped import; Phase 8 resolves it. |

## Fixtures

- `tests/pkgsystem/local-resolve/path-only/`: programs using only path imports; no manifest required.
- `tests/pkgsystem/local-resolve/scoped-cached/`: programs importing scoped packages whose tarballs are pre-extracted under a test cache root.
- `tests/pkgsystem/local-resolve/version-mismatch/`: positive failure cases.
- `tests/pkgsystem/local-resolve/manifest-less/`: every existing example as a regression sweep.
- `tests/pkgsystem/local-resolve/classify/cases.json`: 250+ classifier rows.
- `tests/pkgsystem/local-resolve/discover/{a,b,c}/`: manifest discovery trees.

## Test set

- `TestPhase2Classify` — classifier `cases.json`.
- `TestPhase2Discover` — manifest discovery.
- `TestPhase2CacheHit` — scoped imports against pre-populated cache.
- `TestPhase2ManifestLess` — scoped raises `M057_RESOLVE_E002`.
- `TestPhase2VersionMismatch` — `M057_RESOLVE_E003`.
- `TestPhase2Regression` — `examples/v0.7/**/*.mochi` still resolves.

## Open questions

- Whether to surface ambiguous-spec errors at the parser or resolver layer; current plan: parser, so editor squigglies show even before the resolver runs.
- Whether absolute path imports (`/abs/path`) should be supported or rejected; current plan: supported as `KindPathAbsolute`, since shell scripts use them today.

## Cross-references

- Language surface details: [research note 01](/docs/research/0057/language-surface).
- Cache layout: [research note 08 §7](/docs/research/0057/content-addressed-store).
- Backwards-compatibility matrix: [research note 01 §8](/docs/research/0057/language-surface).
- Manifest discovery algorithm: [research note 04 §8](/docs/research/0057/manifest-format).
