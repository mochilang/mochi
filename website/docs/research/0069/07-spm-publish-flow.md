---
title: "07. SPM publish flow"
sidebar_position: 8
sidebar_label: "07. SPM publish flow"
description: "The git-tag publish path (tag creation, signing, push, GitHub Release), the SE-0292 registry API (POST /packages, source archive format, signed manifest), GitHub Actions OIDC token exchange, XCFramework binary distribution, and the Swift Package Index indexing pipeline."
---

# 07. SPM publish flow

MEP-69 supports two publish paths: `--to=swift-git` (classical git-tag model) and `--to=swift-registry` (SE-0292 structured registry). They are complementary; the `--emit-ci` workflow runs both in sequence.

## §1. `--to=swift-git`: git-tag publish

### What it does

The git-tag publish path releases a Mochi package as a Swift package that any Swift developer can add via `swift package add <git-url>` or by copying a `.package(url:from:)` line into their `Package.swift`.

### Steps

1. **Build the Swift library.** Invokes `Driver.Build` with `target = TargetSwiftLibrary`, producing a Swift package directory at `<outdir>/<package-name>/` containing:
   - `Package.swift` with the correct `products:`, `targets:`, and `dependencies:` (including `MochiRuntime`).
   - `Sources/<module>/<module>.swift` with `public` API items.
   - `Sources/<module>/MochiBridge.swift` with `@_cdecl` exports for C ABI consumers.
   - (Optional) `<package-name>.xcframework/` if `[swift] xcframework = true`.
   - `README.md` generated from Mochi doc comments.

2. **Compute the version tag.** Reads `[package].version` from `mochi.toml`. Must be a valid SemVer string. Creates a tag name of `v<version>` (with the `v` prefix) following the SPM convention.

3. **Create an annotated git tag.** Runs `git tag --annotate v<version> -m "Release v<version>"` at the current commit in `<outdir>`. If `[package].gpg-sign = true` or `commit.gpgsign = true` in the user's git config, adds `--sign` to create a signed tag.

4. **Push the tag to the remote.** Reads `[swift.publish] github-repo` from `mochi.toml`. Runs `git push origin v<version>`.

5. **Create a GitHub Release (optional).** If the remote is a GitHub URL and `GITHUB_TOKEN` is set (or the CI `id-token: write` permission is active), calls the GitHub Releases API to create a Release for the tag with:
   - The body set to the changelog section for this version (auto-extracted from `CHANGELOG.md` if present).
   - The XCFramework bundle attached as a release asset (if `xcframework = true`).
   - The `Package.swift` attached as a release asset for audit.

6. **Output.** Prints the tag URL, the GitHub Release URL (if created), and the `swift package add` command the user can share.

```
Released: github.com/myorg/MyMochiLib @ v1.2.0
  Tag: https://github.com/myorg/MyMochiLib/releases/tag/v1.2.0
  Add: swift package add https://github.com/myorg/MyMochiLib.git --from 1.2.0
```

### Tag signing

The bridge supports three tag signing modes:

| Mode | Config | Mechanism |
|------|--------|-----------|
| Unsigned | (default) | Annotated tag only. |
| GPG-signed | `commit.gpgsign = true` in git config | `git tag --sign` |
| SSH-signed | `gpg.format = ssh` in git config | `git tag --sign` via SSH key |

Unsigned tags are accepted; SPM does not require tag signatures. Tag signing is recommended for packages published to the Swift Package Index registry.

## §2. `--to=swift-registry`: SE-0292 registry publish

### What SE-0292 is

SE-0292 (Package Registry Service, accepted January 2022) defines a REST API for Swift package registries. A compliant registry exposes endpoints for:

- `GET /packages/{scope}/{name}/releases` — list releases.
- `GET /packages/{scope}/{name}/{version}.zip` — download release archive.
- `POST /packages/{scope}/{name}/{version}` — publish a release.
- `GET /identifiers?url=<git-url>` — reverse-lookup a package identity from its git URL.

The Swift Package Index operates a pilot registry at `packages.swift.org` that accepts SE-0292 publication for indexed packages.

### Steps

1. **Build the Swift library** (same as git-tag path above).

2. **Create the source archive.** Runs `git archive --format=zip --prefix=<package-name>-<version>/ HEAD` in the emitted Swift package directory, producing a `<package-name>-<version>.zip`. This is the SE-0292 release archive format (SE-0292 specifies `.zip`, not `.tar.gz`).

3. **Obtain an OIDC token.** From the CI environment:
   - GitHub Actions: requests a token via the `ACTIONS_ID_TOKEN_REQUEST_URL` + `ACTIONS_ID_TOKEN_REQUEST_TOKEN` environment variables. Requires the workflow to declare `permissions: id-token: write`.
   - GitLab CI: reads `CI_JOB_JWT_V2`.
   - Fallback: reads `SWIFT_REGISTRY_TOKEN` environment variable (a static registry token, the only acceptable long-lived token use case in MEP-69, logged as a warning).

4. **Upload to the SE-0292 endpoint.** Issues a `multipart/form-data` POST to `<registry-base>/packages/<scope>/<name>/<version>` with:
   - The `.zip` archive as the `source-archive` part.
   - A JSON `metadata` part containing the package description, license, keywords, and repository URL from `mochi.toml`.
   - An `Authorization: Bearer <oidc-token>` header.

5. **Record the registry release.** Reads the registry's response and records the registry release URL in the build output.

### Manifest requirements for SE-0292

The SE-0292 POST body's `metadata` JSON must include:

```json
{
  "description": "A Mochi library published as a Swift package",
  "keywords": ["mochi", "example"],
  "licenseURL": "https://github.com/myorg/MyMochiLib/blob/main/LICENSE",
  "repositoryURL": "https://github.com/myorg/MyMochiLib.git",
  "readmeURL": "https://github.com/myorg/MyMochiLib/blob/main/README.md"
}
```

These fields are sourced from `mochi.toml`'s `[package]` section and `[swift.publish]`.

## §3. The `Package.swift` the bridge emits

The `TargetSwiftLibrary` emit pass produces a `Package.swift` with the following shape:

```swift
// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "MyMochiLib",
    platforms: [
        .macOS(.v13),
        .iOS(.v16),
    ],
    products: [
        .library(name: "MyMochiLib", targets: ["MyMochiLib"]),
    ],
    dependencies: [
        .package(url: "https://github.com/mochilang/mochi-swift-runtime.git", from: "0.6.0"),
    ],
    targets: [
        .target(
            name: "MyMochiLib",
            dependencies: [
                .product(name: "MochiRuntime", package: "mochi-swift-runtime"),
            ],
            path: "Sources/MyMochiLib"
        ),
    ]
)
```

When `xcframework = true` and the XCFramework is pre-built and attached as a GitHub Release asset, the `Package.swift` instead uses a `binaryTarget`:

```swift
.binaryTarget(
    name: "MyMochiLib",
    url: "https://github.com/myorg/MyMochiLib/releases/download/v1.2.0/MyMochiLib.xcframework.zip",
    checksum: "sha256-hex-of-the-xcframework-zip"
)
```

The `checksum` is the SHA-256 of the `.xcframework.zip` file, which is the standard SPM binary target checksum format.

## §4. The `--emit-ci` GitHub Actions workflow

`mochi pkg publish --to=swift-registry --emit-ci` generates `.github/workflows/swift-release.yml`:

```yaml
name: Swift Release
on:
  push:
    tags: ["v*.*.*"]
permissions:
  contents: write
  id-token: write
jobs:
  release:
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      - uses: swift-actions/setup-swift@v2
        with: { swift-version: "5.10" }
      - name: Build and publish (git tag)
        run: mochi pkg publish --to=swift-git --remote=origin
      - name: Publish to Swift Package Index registry
        run: mochi pkg publish --to=swift-registry --registry=https://packages.swift.org
        if: success()
```

## §5. Swift Package Index (SPI) indexing

The Swift Package Index crawls public GitHub repositories for `Package.swift` manifests and indexes them at `swiftpackageindex.com`. Packages published via the git-tag path are automatically discovered by SPI's crawler if the repository is public. There is no manual submission step for SPI.

SPI's `spi-manifest.yml` format (an optional file in the package repository) allows declaring supported platforms, Swift versions, and categories:

```yaml
# .spi.yml
version: 1
builder:
  configs:
    - platform: macosSpm
      swift_version: "5.10"
    - platform: linuxSpm
      swift_version: "5.10"
    - platform: iosSpm
      swift_version: "5.10"
```

MEP-69's `--emit-ci` flag optionally emits a `.spi.yml` alongside the `Package.swift` when `[swift.publish] spi-manifest = true` is set in `mochi.toml`.

## Cross-references

- [[01-language-surface]] for the `mochi pkg publish` CLI surface.
- [[11-xcframework-and-multiplatform]] for the XCFramework bundle that is attached as a release asset.
- [[06-spm-git-resolution]] for the inverse operation (consuming packages from git).
- [SE-0292](https://github.com/apple/swift-evolution/blob/main/proposals/SE-0292-package-registry-service.md) — the normative SE-0292 registry API specification.
