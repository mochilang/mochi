---
title: "10. Build system"
sidebar_position: 11
sidebar_label: "10. Build system"
description: "go build driver, the Driver.Build cache + sandbox, cross-compile invocation, deterministic flags (-trimpath, -buildvcs=false, -ldflags=-buildid=, SOURCE_DATE_EPOCH=0), pkg.go.dev publication."
---

# 10. Build system

This note describes the Go build driver, the per-build sandbox, the cache, the deterministic flags, and the pkg.go.dev publication story.

## `Driver.Build` shape

```go
type Driver struct {
    Toolchain       Toolchain   // resolved go binary
    Deterministic   bool        // strip build IDs and timestamps
    NoVendor        bool        // use proxy mode instead of vendor
    NoCache         bool        // force -a rebuild
    AdditionalFlags []string    // appended to `go build`
}

func (d *Driver) Build(src, out string, target Target) (BuildResult, error)
```

`Build` walks: parse → typecheck → clower.Lower → go/lower.Lower → emit → write workspace → invoke `go build`. The workspace is a per-build temp directory containing `go.mod`, `go.sum`, `main.go`, and `vendor/` (if vendor mode). After `go build` succeeds, the artefact is copied to `out`.

## Workspace layout

```
<workdir>/
├── go.mod                       // module mochi-build-<sha256>
├── go.sum                       // pinned runtime hash
├── main.go                      // the emitted source
└── vendor/                      // if vendor mode (default)
    ├── modules.txt
    └── dev.mochilang/
        └── runtime/
            └── go/
                ├── go.mod
                ├── doc.go
                ├── collections/...
                └── ...
```

The `go.mod` uses a synthesised module name (`mochi-build-<sha256-of-src-path>`) so cached `go.sum` entries are keyed by source workspace. This means two parallel builds of different Mochi sources do not collide in `~/go/pkg/mod`.

## Cache

`~/.cache/mochi/go/` is content-addressed by SHA-256 of the workspace path + target tuple. A cached build holds:

- The emitted `main.go` (input).
- The compiled binary (output).
- The `go build` invocation metadata (input hash, output hash, build time).

On a cache hit the driver compares the input hash; if equal, it copies the cached binary and skips `go build`. `Driver.NoCache=true` skips the lookup and forces `go build -a`.

## Cross-compile invocation

```go
cmd := exec.Command(d.Toolchain.Go, "build", "-trimpath", "-buildvcs=false", "-o", outPath, ".")
cmd.Dir = workdir
cmd.Env = append(os.Environ(),
    fmt.Sprintf("GOOS=%s", target.GOOS()),
    fmt.Sprintf("GOARCH=%s", target.GOARCH()),
    "GOPATH="+gopath,
    "GOCACHE="+gocache,
)
```

The env is built explicitly, not inherited fully. We explicitly set `GOPATH` and `GOCACHE` to per-build directories so a hostile `~/.netrc` or `~/.gitconfig` cannot poison the build.

For wasm targets:

```go
case TargetGoWasmJS:
    env["GOOS"] = "js"
    env["GOARCH"] = "wasm"
case TargetGoWasiP1:
    env["GOOS"] = "wasip1"
    env["GOARCH"] = "wasm"
```

For darwin targets on Linux, the driver tests for the presence of an external linker (`MOCHI_DARWIN_LINKER` env var pointing at osxcross's `o64-clang`); if absent, the target is rejected at `Build` start with a clear error.

## Deterministic flags

When `Driver.Deterministic=true`:

| Flag | Purpose |
|------|---------|
| `-trimpath` | Strip absolute filesystem paths from the binary. |
| `-buildvcs=false` | Disable git-derived stamping (commit hash, dirty flag). |
| `-ldflags="-buildid="` | Strip the build ID (a content-addressed hash of input files; would change if temp paths differ). |
| `-ldflags="-s -w"` | Strip the symbol table and DWARF debug info. |
| `SOURCE_DATE_EPOCH=0` (env) | Neutralise any remaining time-derived metadata. |

These give bit-identical output on Linux and Windows. On macOS the Mach-O `LC_UUID` load command is randomised per link by `ld64` and cannot be controlled; Phase 16 platform-skips the macOS reproducibility gate.

## pkg.go.dev publication

The runtime module `dev.mochilang/runtime/go` publishes via the standard Go module proxy flow:

1. Tag a release: `git tag v1.2.3 && git push --tags`.
2. The Go module proxy at `proxy.golang.org` fetches the module the first time anyone references it (or via `GOPROXY=https://proxy.golang.org go install dev.mochilang/runtime/go@v1.2.3`).
3. pkg.go.dev indexes the module and renders its documentation.

No `npm publish`-equivalent step is needed. The git tag is the entire publication action.

Publication metadata (license, README, documentation) is read from the module by pkg.go.dev. We include:

- `LICENSE` (Apache-2.0).
- `README.md` at the module root.
- Per-package `doc.go` files with package overview docstrings.
- `// CODEOWNERS` and `SECURITY.md` for contribution / vulnerability reporting.

## Signed releases

Phase 18 wires signed release tags via [`cosign`](https://github.com/sigstore/cosign) and GitHub Actions OIDC. The release workflow:

1. On `git tag vX.Y.Z` push, the workflow checks out the tag, runs `go test ./...`, and produces SBOM via `syft`.
2. `cosign sign-blob --yes --output-signature ... main.go` signs the emitted source with the workflow's OIDC token.
3. The signature + SBOM are attached as release assets on GitHub.

This is consistent with the MEP-53 (Rust) and MEP-55 (PHP) signed-release flows.

## Hermetic build verification

A separate `TestPhase16Hermetic` runs the same Mochi source through `Driver.Build` twice in independent temp directories with `Deterministic=true`; the resulting binaries are SHA-256 compared. The gate fails if they differ.

The gate is Linux-only by default. macOS runs as `allow_failure: true` so the LC_UUID skip is visible but not blocking. Windows is gated identically to Linux.

## `go mod tidy` discipline

The emitted `go.mod` is generated by the lowerer with the exact set of imports needed; we do not run `go mod tidy` during the build because tidy walks the entire workspace and can introduce indirect deps. The lowerer's `runtimeFlags` struct is the source of truth for what imports to emit.

A Phase 18 sub-gate (`TestPhase18ModTidyIdempotent`) runs `go mod tidy` against the emitted module and asserts no changes — this catches lowerer bugs where the import list drifts from the actual usage.

## Build observability

`Driver.Build` returns a `BuildResult` with:

- `Workdir` — the temp directory used.
- `ArtifactPath` — where the binary was written.
- `Duration` — wall-clock time for the build.
- `GoBuildOutput` — stdout+stderr from `go build`.
- `CacheHit` — whether the cache was hit.

The test harness logs these to a per-fixture file so build regressions (sudden duration spikes, sudden cache misses) are visible without re-running.
