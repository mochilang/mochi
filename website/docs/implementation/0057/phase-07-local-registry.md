---
title: "Phase 7. Local filesystem-backed registry"
sidebar_position: 8
sidebar_label: "Phase 7. Local registry"
description: "MEP-57 Phase 7 — `file://` registry backend mirroring the sparse index URL scheme on disk. Foundation for offline use, vendoring, and the network registry's integration tests."
---

# Phase 7. Local filesystem-backed registry

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 7](/docs/mep/mep-0057#phase-7-local-registry) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase7LocalRegistry`: solver and resolver hit a `file://` registry transparently; output matches a network-fronted equivalent run on identical content.

Pass criteria:

1. URL parity. The `Registry` interface implemented by `pkg/pkgregistry/local` returns identical bytes to the planned `pkg/pkgregistry/sparse` (HTTP) backend, given identical on-disk content. The harness compares `Versions(name)` and `Manifest(name, ver)` outputs across both backends.
2. Solver parity. Running `mochi pkg lock` with `[registry] default = "file:///opt/mochi-cache"` produces a lockfile byte-identical to the same lock against the HTTP backend (`https://index.mochi.dev`) seeded with the same fixture content.
3. Blob retrieval. `Blob(blake3)` returns the byte-identical tarball whether sourced from disk or HTTPS.
4. Init helper. `mochi pkg registry init <root>` populates a usable filesystem registry from a set of input manifests + tarballs.
5. Serve helper. `mochi pkg registry serve --local=<root> --port=N` opens an HTTPS reverse proxy that fronts a local root with the sparse URL scheme; the test harness uses this to verify the local backend matches the HTTP shape.

## Goal-alignment audit

A local registry is the test harness for Phase 8 (network) and the production backend for offline / vendor / corporate-mirror cases. Without it, every test of the solver requires a live network endpoint, which is slow and fragile.

The filesystem layout is identical to the URL scheme (research note 07 §7): the same path under `<root>/` that would appear after `https://index.mochi.dev/`. This means a corporate mirror that runs `rsync` from upstream produces a directly servable tree without rewriting anything; just point `[[registry.alternate]]` at the result.

Phase 7 deliberately does *not* implement HTTP semantics (ETag, conditional GET, retries). Those are Phase 8. Phase 7 is the simplest possible backend that satisfies the `Registry` interface and feeds the solver fixture corpus.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 7.0 | `pkg/pkgregistry/local` package: parse `file://` URL, walk filesystem | NOT STARTED | — |
| 7.1 | URL scheme on disk: `<root>/<bucket>/<scope>/<name>` | NOT STARTED | — |
| 7.2 | Blob lookup: `<root>/blobs/<blake3-hex>` | NOT STARTED | — |
| 7.3 | `mochi pkg registry serve --local=<root>` debug command | NOT STARTED | — |
| 7.4 | `mochi pkg registry init <root>` for populating a test root from manifests | NOT STARTED | — |
| 7.5 | Cross-backend parity tests | NOT STARTED | — |
| 7.6 | Negative paths (missing pkg -> 404 analog, malformed JSONL -> M057_INDEX_E002) | NOT STARTED | — |

## Sub-phase 7.0 — `pkg/pkgregistry/local` package

The shared `Registry` interface:

```go
// pkg/pkgregistry/registry.go
type Registry interface {
    Versions(pkg string) ([]VersionEntry, error)
    Manifest(pkg, version string) (*pkgmanifest.Manifest, error)
    Blob(blake3 string) (io.ReadCloser, error)
}

type VersionEntry struct {
    Version      string
    Released     time.Time
    BLAKE3       string
    SHA256       string
    Yanked       bool
    YankReason   string
    Capabilities []string
    Dependencies map[string]string
    Targets      []string
    MochiRange   string
    Edition      string
    License      string
    Provenance   ProvenanceRef
}
```

`pkg/pkgregistry/local`:

```go
// pkg/pkgregistry/local/local.go
type FilesystemRegistry struct {
    Root string // absolute path
}

func New(root string) (*FilesystemRegistry, error) {
    abs, err := filepath.Abs(root)
    if err != nil { return nil, err }
    if !isDir(abs) {
        return nil, fmt.Errorf("%w: %q is not a directory", ErrIndexE001, abs)
    }
    return &FilesystemRegistry{Root: abs}, nil
}

func (r *FilesystemRegistry) Versions(pkg string) ([]VersionEntry, error) {
    path := r.indexPath(pkg)
    f, err := os.Open(path)
    if err != nil {
        if errors.Is(err, fs.ErrNotExist) {
            return nil, fmt.Errorf("%w: package %q not found", ErrPkgNotFound, pkg)
        }
        return nil, fmt.Errorf("%w: %v", ErrIndexE001, err)
    }
    defer f.Close()
    return parseJSONL(f)
}
```

## Sub-phase 7.1 — URL scheme on disk

The bucket calculation matches the sparse-index URL scheme (research note 07 §2.1):

```go
func bucket(name string) string {
    n := strings.ToLower(name)
    switch {
    case len(n) >= 4: return n[:2] + "/" + n[2:4]
    case len(n) >= 2: return n[:2] + "/" + n[:2]
    case len(n) == 1: return n + "/-"
    default:          return "-/-"
    }
}

func (r *FilesystemRegistry) indexPath(pkg string) string {
    scope, name := pkgmanifest.SplitName(pkg)
    if scope == "" { scope = "-" } // sentinel
    return filepath.Join(r.Root, bucket(name), scope, name)
}
```

Examples:

| Package         | Disk path                                                  |
|-----------------|------------------------------------------------------------|
| `@mochi/strings`| `<root>/st/ri/mochi/strings`                               |
| `datalog`       | `<root>/da/ta/-/datalog`                                   |
| `x`             | `<root>/x/-/-/x`                                           |

(Single-character names use the padding rule.)

## Sub-phase 7.2 — Blob lookup

```go
func (r *FilesystemRegistry) Blob(blake3 string) (io.ReadCloser, error) {
    if !isHex64(blake3) {
        return nil, fmt.Errorf("%w: not a 256-bit hex string", ErrBlobE001)
    }
    path := r.blobPath(blake3)
    f, err := os.Open(path)
    if err != nil {
        if errors.Is(err, fs.ErrNotExist) {
            return nil, fmt.Errorf("%w: %s", ErrBlobNotFound, blake3)
        }
        return nil, err
    }
    return f, nil
}

func (r *FilesystemRegistry) blobPath(blake3 string) string {
    // matches blobs.mochi.dev URL: blobs/<bl>/<ak>/<rest>
    return filepath.Join(r.Root, "blobs", blake3[:2], blake3[2:4], blake3)
}
```

Two-character pair sharding keeps any single directory bounded for popular projects.

## Sub-phase 7.3 — `mochi pkg registry serve`

A debug HTTPS server that fronts a local root with the URL scheme. Used to integration-test Phase 8 (sparse HTTPS) against a local backend:

```go
// cmd/mochi/registry.go
func cmdRegistryServe(c *cli.Context) error {
    root := c.String("local")
    addr := c.String("addr")
    reg, err := local.New(root)
    if err != nil { return err }
    mux := http.NewServeMux()
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        switch {
        case strings.HasPrefix(r.URL.Path, "/blobs/"):
            serveBlob(w, r, reg)
        default:
            serveIndex(w, r, reg)
        }
    })
    return http.ListenAndServeTLS(addr, c.String("cert"), c.String("key"), mux)
}
```

`serveIndex` reads the JSONL file from disk and writes it back with `Content-Type: application/x-mochi-index+jsonl; charset=utf-8`, an `ETag` (computed from file mtime + size), and `Cache-Control: public, max-age=300`.

`serveBlob` streams the tarball with `Content-Type: application/vnd.mochi.tarball+zstd` and `ETag` equal to the BLAKE3 hex (matches blobs.mochi.dev convention).

This server is the test-time alternative to mocking `net/http` in unit tests; integration tests against `mochi pkg registry serve` exercise the same code path used in production HTTPS.

## Sub-phase 7.4 — `mochi pkg registry init`

Populates a registry root from a set of manifests + tarballs:

```go
func cmdRegistryInit(c *cli.Context) error {
    root := c.String("root")
    inputs := c.StringSlice("from")  // paths to .tar.zst tarballs
    for _, in := range inputs {
        m, err := readManifestFromTarball(in)
        if err != nil { return err }
        bl, sh, err := hashTarball(in)
        if err != nil { return err }
        if err := writeIndexEntry(root, m, bl, sh); err != nil { return err }
        if err := copyBlob(root, in, bl); err != nil { return err }
    }
    return nil
}
```

The output tree is a complete sparse index ready to be served.

## Sub-phase 7.5 — Cross-backend parity tests

The test harness runs each solver fixture twice: once against the local filesystem registry and once against `mochi pkg registry serve` over HTTPS:

```go
// tests/pkgsystem/local-registry/parity_test.go
func TestPhase7Parity(t *testing.T) {
    for _, fix := range fixtureSet {
        local, _ := local.New(fix.RegistryRoot)
        srv := startTestServer(t, fix.RegistryRoot)
        defer srv.Close()
        http := sparse.New(srv.URL)
        for _, pkg := range fix.Packages {
            localV, _ := local.Versions(pkg)
            httpV, _ := http.Versions(pkg)
            assertEqual(t, localV, httpV)
            // Compare manifests, blobs, error paths.
        }
    }
}
```

This is the cheap insurance against Phase 8 protocol drift.

## Sub-phase 7.6 — Negative paths

Cases:

- `Versions("@nonexistent/pkg")` returns `M057_INDEX_E008` (package not found in local index).
- A JSONL file with a malformed line raises `M057_INDEX_E002` and the error mentions the line number.
- A file whose JSONL contains an unknown field (forward-compat) is *accepted* with a warning under `Manifest.Warnings`; old clients must not refuse to parse new fields (research note 07 §14, E003 reserved but not used at v1).
- A `Blob(notfound)` returns `M057_BLOB_E007` (local registry 404). The
  hash-mismatch case `M057_BLOB_E001` is owned by Phase 9; see the
  [error registry](./errors).

## Files changed

All registry-side code lives under `pkg/pkgregistry/` (canonical name). The
research notes that pre-date this decision use `pkg/pkgindex/` as a working
name; that directory does not exist at v1, every reference is rewritten to
`pkg/pkgregistry/`.

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgregistry/registry.go` | `Registry` interface (extended by Phase 8, 11, 18) | Owner |
| `pkg/pkgregistry/local/local.go` | Filesystem backend | Owner |
| `pkg/pkgregistry/local/jsonl.go` | JSONL parser | Owner |
| `pkg/pkgregistry/local/init.go` | `mochi pkg registry init` core | Owner |
| `pkg/pkgregistry/local/serve.go` | HTTPS reverse-proxy front-end | Owner |
| `cmd/mochi/registry.go` | `mochi pkg registry ...` handlers | Owner |
| `tests/pkgsystem/local-registry/fixture-tree/` | Sample registry root | Owner |
| `tests/pkgsystem/local-registry/parity_test.go` | Cross-backend parity | Owner |

## Error code surface

Phase 7 owns `M057_INDEX_E001` (FS read fail) and `M057_BLOB_E007` (local
registry 404). All other codes referenced here are documented in the
canonical [error registry](./errors). The previous local `M057_BLOB_E001`
row was a collision with Phase 9's hash-mismatch code and is removed.

## Test set

- `TestPhase7VersionsList` — `Versions` reads JSONL and returns entries.
- `TestPhase7ManifestFetch` — `Manifest` returns the parsed manifest for a version.
- `TestPhase7Blob` — `Blob` streams tarball.
- `TestPhase7Parity` — local vs HTTPS-serve parity.
- `TestPhase7Init` — `mochi pkg registry init` produces a usable tree.

## Open questions

- Whether to support a writable `file://` registry for publish flows; current plan: no, publish always pushes to a real registry. The `file://` backend is read-only.
- Whether to add a memory-only backend for unit tests that don't want to touch disk; deferred to Phase 7.5 if useful.

## Cross-references

- Filesystem-backed registry: [research note 07 §7](/docs/research/0057/registry-index).
- URL scheme: [research note 07 §2](/docs/research/0057/registry-index).
- Solver consuming the interface: [research note 05 §8](/docs/research/0057/solver-design).
- Blob content-addressing: [research note 08 §5](/docs/research/0057/content-addressed-store).
