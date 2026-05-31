---
title: "06. SPM git resolution"
sidebar_position: 7
sidebar_label: "06. SPM git resolution"
description: "The SPM git dependency model (tags, branches, revisions), the shallow-clone strategy, Package.swift dump-package JSON extraction, the dependency graph walk, the version resolution algorithm, and the content-addressed archive format."
---

# 06. SPM git resolution

## SPM's dependency model

Swift Package Manager uses git as its primary distribution mechanism. A Swift package is defined by a `Package.swift` manifest at the root of a git repository. Package versions correspond to git tags matching the SemVer pattern `Major.Minor.Patch` (with or without a `v` prefix: both `5.9.1` and `v5.9.1` are valid SPM tags).

A `Package.swift` declares its dependencies as:

```swift
// Package.swift
dependencies: [
    .package(url: "https://github.com/Alamofire/Alamofire.git", from: "5.9.0"),
    .package(url: "https://github.com/apple/swift-nio.git", .upToNextMajor(from: "2.65.0")),
    .package(url: "https://github.com/vapor/vapor.git", exact: "4.99.1"),
    .package(path: "../local-package"),
]
```

The version requirement forms are:
- `.upToNextMajor(from: "X.Y.Z")` — resolves to the highest version >= X.Y.Z and < (X+1).0.0.
- `.upToNextMinor(from: "X.Y.Z")` — resolves to the highest version >= X.Y.Z and < X.(Y+1).0.
- `.exact("X.Y.Z")` — resolves to exactly X.Y.Z.
- `.range("X.Y.Z"..<"A.B.C")` — explicit range.
- `.branch("main")` — HEAD of the named branch (not reproducible across lock runs).
- `.revision("abc1234")` — pinned commit SHA.

## MEP-69 resolution algorithm

The bridge's `spmresolver.Resolve()` function re-implements the SPM resolution algorithm in Go. SPM uses a variant of the "minimum versions satisfying all constraints" algorithm (analogous to Go modules' MVS), but with the "highest version in range" semantics that Cargo and npm use for `^` constraints.

**Algorithm (simplified):**

1. Start with the user's `[swift-dependencies]` constraints.
2. For each declared dependency, query the git remote for its available tags (via `git ls-remote --tags`). Cache tag lists in the content-addressed store keyed by `<remote-url-hash>/<fetch-timestamp-rounded-to-hour>`.
3. Select the highest version tag that satisfies the constraint (`.upToNextMajor` → highest version with same major; `.exact` → that exact version).
4. Shallow-clone the package source at the selected tag: `git clone --depth=1 --branch=<tag> <url> <cache-dir>`.
5. Parse the cloned `Package.swift` via `swift package dump-package --package-path <dir>` (produces a JSON representation). See §dump-package below.
6. Walk the JSON dependency graph recursively, applying steps 2-5 for each transitive dependency.
7. Detect version conflicts (two requirements on the same package that cannot be satisfied simultaneously) and emit a lock error with a diagnostic.
8. Write the resolved set as `[[swift-package]]` entries in `mochi.lock`, recording the tag, the commit SHA at the tag, and the BLAKE3-256 + SHA-256 of the source archive.

### `swift package dump-package`

The `dump-package` subcommand outputs the `Package.swift` manifest as JSON without compiling the package:

```json
{
  "name": "Alamofire",
  "defaultLocalization": null,
  "platforms": [
    { "platformName": "macos", "version": "10.15" },
    { "platformName": "ios", "version": "13.0" }
  ],
  "products": [
    { "name": "Alamofire", "type": { "library": ["automatic"] }, "targets": ["Alamofire"] }
  ],
  "dependencies": [
    {
      "identity": "swift-log",
      "requirement": { "range": [{ "lowerBound": "1.5.0", "upperBound": "2.0.0" }] },
      "url": "https://github.com/apple/swift-log.git"
    }
  ],
  "targets": [
    { "name": "Alamofire", "type": "regular", "dependencies": ["swift-log"] }
  ]
}
```

The bridge parses this JSON (not the Swift source) to extract the dependency graph. This is safe because `dump-package` is run at the shallow-cloned tag; it reflects the exact manifest at the pinned version.

Packages that use Swift 5.9+ `#if swift(...)` conditional compilation in `Package.swift` may emit different dependency lists for different Swift versions. The bridge passes `--swift-version <version>` to `dump-package` when the `[swift] tools-version` is specified, ensuring the resolved graph matches the bridge's target toolchain.

## Shallow-clone strategy

Full git clones of large packages (swift-nio, vapor, Alamofire) download 50-200 MB of history. The bridge uses `--depth=1` shallow clones for all tagged resolution steps (90% of production use cases). The trade-off:

- `--depth=1` at a tag: downloads only the tree at the tag commit. Fast (~2-10 seconds per package on a typical internet connection). Archive SHA is deterministic: `git archive --format=tar.gz HEAD` produces the same bytes regardless of history depth.
- Full clone: required only for `.branch()` resolution (branch HEAD is a moving target; a full clone enables `git log` history inspection). Not cached by the bridge (branch dependencies are not reproducible and are flagged as warnings in `mochi pkg lock` output).
- Pinned revision: `git clone --depth=1 --no-single-branch <url>` followed by `git fetch --depth=1 origin <sha>`. Required when the `revision:` field specifies a commit SHA that does not correspond to a tag.

The content-addressed archive is a `git archive --format=tar.gz --prefix=<pkg>-<version>/` of the shallow-cloned directory, compressed with gzip at level 9. The BLAKE3-256 of this archive is the `archive-blake3` lockfile field. SHA-256 is computed in parallel for `archive-sha256`.

## Content-addressed store layout

```
~/.cache/mochi/swift-deps/
  blobs/
    <blake3-hex>/         # one directory per unique archive
      archive.tar.gz      # the compressed source archive
      source/             # extracted source tree (on-demand)
      .build/             # swift build output (on-demand)
      modules/
        <ModuleName>.swiftmodule/
          <triple>.swiftinterface  # the extracted interface file
  wrappers/
    <wrapper-sha256>/
      swift_wrap_<pkg>/   # the synthesised SwiftPM package
      shim.mochi           # the synthesised Mochi shim
  registries/
    <registry-host>/      # SE-0292 registry cache (see §07)
```

The `source/` and `.build/` subdirectories are created lazily: `source/` when the archive is first extracted, `.build/` when `swiftinterface.Generate()` is first called for this archive. The `modules/` directory is populated after the `swift build` step.

## Version conflict detection

The bridge detects version conflicts at lock time and emits diagnostics:

```
ERROR: version conflict: swift-log
  Alamofire@5.9.1 requires swift-log ^1.5 (resolves to 1.6.2)
  vapor@4.99.1    requires swift-log ^1.6 (resolves to 1.6.2)
  swift-nio@2.75.0 requires swift-log ^1.4 (resolves to 1.6.2)
  → Resolved to swift-log 1.6.2 (highest satisfying all constraints) ✓

ERROR: irreconcilable conflict: MyPackage
  LibA@1.0 requires MyPackage exact: 2.0.0
  LibB@3.0 requires MyPackage exact: 3.0.0
  → Cannot satisfy both constraints. Resolution: pin LibA or LibB to a version
    that shares a compatible MyPackage constraint.
    Run: mochi pkg add swift LibA@<version> to explore alternatives.
```

The irreconcilable conflict diagnostic includes the full dependency chain from each conflicting requirement back to the user's `[swift-dependencies]` table, so the user knows which top-level dependency is causing the conflict.

## Cross-references

- [[07-spm-publish-flow]] for the publish side (git tag push and SE-0292 registry).
- [[04-swiftinterface-ingest]] for what happens after the source is fetched and built.
- [[12-risks-and-alternatives]] §R4 for the SPM git resolution speed risk and mitigation.
