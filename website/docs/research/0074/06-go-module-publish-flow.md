---
title: "06. Go module publish flow"
sidebar_position: 7
sidebar_label: "06. Module publish flow"
description: "The git-tag upload protocol, the canonical-import-path requirement, the module proxy's caching behaviour, the per-module metadata requirements, the publish-side gate against a synthetic downstream consumer."
---

# 06. Go module publish flow

This note documents the publish direction (Mochi → Go module) end-to-end. The Go ecosystem's publish model is fundamentally different from Rust's crates.io / npm's registry / PyPI's upload endpoint: there is no central registry. Module publication is a git operation that the module proxy at `proxy.golang.org` picks up asynchronously.

## The git-tag-as-publish model

To publish a Go module at version `v1.2.3`:

1. Have a public git repository (GitHub, GitLab, Gitea, self-hosted) at a canonical-import-path URL like `github.com/example/my-mochi-lib`.
2. The repo contains a `go.mod` with `module github.com/example/my-mochi-lib` at the root (or under a `vN/` subdirectory for v2+).
3. Tag a commit with the semver tag: `git tag v1.2.3 && git push origin v1.2.3`.
4. The first time anyone `go get github.com/example/my-mochi-lib@v1.2.3` runs, the Go module proxy fetches the tag, packages the source tree as a `.zip`, computes the `h1:` checksum, and serves it.
5. The proxy submits the `(module, version, h1:hash)` triple to sum.golang.org's checksum DB. The DB appends the triple to its Merkle tree and signs the new tree head.
6. From this point forward, every `go get` of `v1.2.3` is served from the proxy and verified against the checksum DB.

There is no upload step. There is no central registry POST. There is no API token. The entire publish protocol is git push.

## What MEP-74's `mochi pkg publish --to=go+git+<repo>@<tag>` does

The CLI orchestrates:

1. **Build the library module.** `Driver.Build` with `target = TargetGoLibrary` writes a publish-ready Go module to `<workdir>/publish/`:
   - `go.mod` with `module <canonical-import-path>` and `go <go-version-floor>`.
   - `doc.go` with the `// Package foo provides ...` first-sentence godoc rule plus the SPDX licence comment.
   - `LICENSE` file copied from the SPDX template for the declared licence.
   - `README.md` carried over from the Mochi package's `README.md` (if present).
   - `<pkg-name>.go` source files mirroring the Mochi package's public surface.
   - `go.sum` pinning every transitive dependency from `[go-dependencies]`.

2. **Validate.** Run inside `<workdir>/publish/`:
   - `gofmt -l .` must return zero files (the emit pass is already gofmt-clean, but the validation is the safety net).
   - `go vet ./...` must return zero diagnostics.
   - `go build ./...` must succeed.
   - `go test ./...` is NOT run (test files don't ship in the published module by Go convention).
   - `golint`-style godoc rule checks (every exported identifier has a doc comment).

3. **Stage the publish commit.** Either:
   - `<repo-url>` points to an existing local clone: stage the publish-tree files in the clone, commit with a message like `Release v1.2.3 (mochi pkg publish)`.
   - `<repo-url>` is a remote URL: clone the repo to `<workdir>/publish-repo/`, copy the publish-tree files in, commit, push the branch.

4. **Tag.** Run `git tag <tag>` at the publish commit.

5. **Optionally sign.** With `--cosign-sign`, the bridge:
   - Acquires an OIDC token from the CI environment (GitHub Actions `id-token: write` claim, GitLab CI ID token, etc.).
   - Calls `cosign sign-blob --identity-token <oidc> --bundle <tag>.sig` against the commit SHA.
   - Creates a sibling git tag `<tag>.sig` whose annotated message is the cosign bundle (base64-encoded JSON).
   - The signing flow is opt-in v1; the Go team has not committed to a canonical signing format yet.

6. **Push.** `git push origin <tag>` (and optionally `<tag>.sig`).

7. **Warm the proxy cache** (optional). HTTP GET `https://proxy.golang.org/<canonical-import-path>/@v/<tag>.info`. This triggers the proxy to fetch the new tag and submit it to sum.golang.org. Without this, the first consumer to `go get` the new version pays the cold-fetch latency.

## Layout of the emitted publish-tree

For a Mochi package `my-mochi-lib` at canonical-import-path `github.com/example/my-mochi-lib`:

```
publish/
  go.mod                  # module github.com/example/my-mochi-lib + go 1.21 + require ...
  go.sum                  # transitive hashes
  doc.go                  # // Package mymochilib provides ...
  LICENSE                 # Apache-2.0 OR MIT
  README.md               # carried over from Mochi
  user.go                 # func User(...) ...
  user_test.go            # NOT emitted; test files do not ship
  internal/               # NOT emitted; internal helpers stay in the Mochi package
```

The package name comes from the canonical-import-path's last segment (with hyphens stripped per Go convention: `my-mochi-lib` → package `mymochilib`).

## The canonical-import-path requirement

`[go.publish] canonical-import-path` is REQUIRED. The bridge writes this value into `go.mod`'s `module` directive. A mismatch between the declared path and the git remote URL is a fatal publish error: the module proxy would refuse to serve a module whose `go.mod` declares a different canonical path than the proxy's URL.

For vanity import paths (`go.uber.org/zap` → `github.com/uber-go/zap`), the bridge supports the vanity-redirect protocol per phase 17. The canonical-import-path is the vanity URL; the git remote is the actual host. The Mochi side configures the vanity redirect via a `[go.publish.vanity]` table.

## Pre-existing repo state

The bridge does not erase the repo's prior state. If the repo already has commits, the publish-tree files are layered on top of HEAD. This means:

- A user can run `mochi pkg publish` multiple times against the same repo with different tags; each run adds a new commit.
- A user can intermix Mochi-published commits with hand-authored Go commits in the same repo (e.g., editing the README between Mochi releases).
- Tag immutability is enforced by git; the bridge refuses to re-tag an existing tag.

## Semver discipline

The bridge does not pick the next semver tag automatically. The user provides the tag explicitly via the `@<tag>` suffix:

```
mochi pkg publish --to=go+git+github.com/example/my-mochi-lib@v1.2.3
```

The bridge validates:

- The tag is a valid semver (per `golang.org/x/mod/semver.IsValid`).
- The tag is not already present in the remote.
- For v2+ modules, the canonical-import-path includes the `vN/` suffix (Go's "semver major in path" rule: `github.com/foo/bar/v2`).

## The major-version rule

Go's module system embeds the major version in the import path for v2+:

```
v0.x.y, v1.x.y  →  github.com/foo/bar
v2.x.y          →  github.com/foo/bar/v2
v3.x.y          →  github.com/foo/bar/v3
```

The bridge enforces this rule: when publishing v2.0.0 or higher, `[go.publish] canonical-import-path` must include the `/vN` suffix. The published `go.mod` accordingly declares `module github.com/foo/bar/v2`.

## The publish-side gate

`mochi pkg publish --dry-run` exercises the full flow except the push step. This is the CI-gated reproducibility check:

- Build, validate, stage, tag locally, optionally sign.
- Diff the staged publish-tree against the prior `v.<N-1>` tag's tree (if a prior tag exists). Report changed files for the publisher's review.
- Run a synthetic downstream consumer: in a sibling temp directory, write a tiny Go program that imports the staged module and calls a representative exported function. Run `go build` to confirm the API contract.

The synthetic downstream consumer is the bridge's catch-net for API regressions: if the publish would break downstream `go get`s of the new tag, the dry-run catches it.

## Private GOPROXY alternative

For private modules hosted on Athens / JFrog Go / a self-hosted Go module proxy, the bridge supports:

```
mochi pkg publish --to=go+goproxy+https://goproxy.corp.example.com@v1.2.3
```

This path:

- Builds and validates the same publish-tree.
- Packages it as a `.zip` per the Go module proxy spec: zip-of-`<canonical-path>@<version>/` directory.
- Computes the `h1:` hash.
- POSTs the .zip to `<proxy-url>/<canonical-path>/@v/<version>.zip` plus the `.mod` and `.info` files per the GOPROXY-compatible upload spec.

This bypasses the git-tag flow entirely and is useful for corporate proxies. Authentication is `.netrc` or HTTP basic; there is no Mochi-managed token.

## Verification of the published module

After publish, the user can verify the result by running `go get` against the canonical-import-path from a clean GOPATH:

```
$ GOPATH=$(mktemp -d) go get github.com/example/my-mochi-lib@v1.2.3
$ ls $GOPATH/pkg/mod/github.com/example/my-mochi-lib@v1.2.3/
go.mod  LICENSE  README.md  doc.go  user.go
```

If the module proxy has cached the new tag (within ~30s of the push under normal conditions), this works immediately. If not, `GOPROXY=direct go get` bypasses the proxy and fetches from the git remote directly.

## Cross-references

- [[01-language-surface]] for the `[go.publish]` manifest table.
- [[07-sigstore-go-checksumdb]] for the optional cosign signing and the sum.golang.org integration.
- [[09-abi-stability]] for the cgo export contract when `cgo-export = true`.
- [MEP-74 §6](/docs/mep/mep-0074#6-build-orchestration) for the build orchestration this publish flow extends.
- [The Go module reference](https://go.dev/ref/mod) for the canonical Go-side documentation.
