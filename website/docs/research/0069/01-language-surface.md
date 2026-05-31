---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import swift \"<package>@<semver>\" as <alias>` import form, the `[swift-dependencies]` / `[swift]` / `[swift.publish]` / `[swift.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add swift`, `mochi pkg lock`, `mochi pkg publish --to=swift-git`, `mochi pkg publish --to=swift-registry`, `mochi pkg sync swift`), and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-69 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "dotnet" | "swift"
```

MEP-69 adds `swift` as a new alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<package-name>` | Bare name. Resolved via `[swift.registries]` or the Swift Package Index API at lock time. |
| `<package-name>@<semver-req>` | Explicit version (`^5.9`, `>=5.9.0, <6.0.0`, `exact: 5.9.1`). Must match `[swift-dependencies]`. |
| `<owner>/<repo>@<semver-req>` | GitHub/GitLab shorthand. Expands to `https://github.com/<owner>/<repo>.git`. |
| `github.com/<owner>/<repo>@<semver-req>` | Full git shorthand. |
| `<name>@git+<url>` | Git source, default branch. |
| `<name>@git+<url>#<rev>` | Git source, pinned to a commit SHA or tag. |
| `<name>@git+<url>#branch:<branch>` | Git source, pinned to a branch. |
| `<name>@path+<rel-path>` | Local path, relative to the manifest. |

Example surface:

```mochi
import swift "Alamofire@^5.9" as http
import swift "vapor/vapor@^4.99" as vapor
import swift "swift-argument-parser" as args
import swift "apple/swift-collections@^1.1" as collections

fn fetch_user(id: string): string {
    let resp = http.AF.request("https://api.example.com/users/\(id)").response()
    return resp.data?.utf8String ?? ""
}

fn run_server() {
    let app = vapor.Application(.production)
    defer { app.shutdown() }
    try! app.run()
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<Type>.<method>` resolves to the synthesised `extern fn` declaration the bridge generated for the Swift module's public item. Swift type names (PascalCase) and function names (camelCase) are preserved verbatim in the shim; the bridge does not rename.

The `auto` modifier is admitted for `import swift ... auto`, opting into flat namespace binding where every public top-level item is bound at file scope instead of under the alias prefix.

## Manifest: `[swift-dependencies]`

```toml
[swift-dependencies]
Alamofire = { version = "^5.9", url = "https://github.com/Alamofire/Alamofire.git" }
"swift-argument-parser" = { version = "^1.5", url = "https://github.com/apple/swift-argument-parser.git" }
"swift-nio" = { version = "^2.75", url = "https://github.com/apple/swift-nio.git" }
Vapor = { version = "^4.99", url = "https://github.com/vapor/vapor.git" }
"swift-collections" = { version = "^1.1", url = "https://github.com/apple/swift-collections.git" }
MyLocalPackage = { path = "../MySwiftPackage" }
```

The grammar:

- A table entry requires either `url` + `version` (remote git), `path` (local), or a bare name resolvable through `[swift.registries]`.
- `version` uses SPM version requirement syntax: `"^5.9"` (up to next major), `"~5.9.0"` (up to next minor), `"5.9.1"` (exact), `">=5.8, <6.0"` (range). The bridge maps these to SPM's `.upToNextMajor(from:)`, `.upToNextMinor(from:)`, `.exact()`, and custom range requirement forms in the synthesised `Package.swift`.
- `branch` and `revision` are alternatives to `version` for development checkouts.
- `url` is the canonical git remote URL; the user may omit it if the package name is registered in `[swift.registries]`.

The user does not write a separate `Package.swift`. The bridge synthesises the workspace `Package.swift` at build time, populating `.package(url:, from:)` dependency entries from `[swift-dependencies]` and pinning the exact resolved version from `mochi.lock`.

## Manifest: `[swift]`

```toml
[swift]
tools-version = "5.9"
platforms = [
    { name = "macOS", version = "13.0" },
    { name = "iOS", version = "16.0" },
    { name = "linux", version = "" },
]
runtime = { async-mode = "dispatch-group" }
xcframework = false
monomorphise = [
    { item = "OrderedDictionary", K = "String", V = "Int" },
    { item = "Heap", T = "Double" },
]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `tools-version` | `"5.9"` | Swift tools version for the synthesised `Package.swift`. Minimum `"5.5"` (structured concurrency). |
| `platforms` | `[{ name: "macOS", version: "13.0" }]` | Platform + minimum version array. Add `{ name: "linux", version: "" }` for Linux. |
| `runtime.async-mode` | `"dispatch-group"` | Async bridge strategy. `"dispatch-group"` blocks synchronously; `"actor"` surfaces as Mochi `async fun`. |
| `xcframework` | `false` | Whether to produce an XCFramework bundle on Apple platform builds. |
| `monomorphise` | `[]` | Explicit generic instantiations. Each entry binds one `<item>` at one set of type arguments. |

## Manifest: `[swift.registries]`

```toml
[swift.registries]
default = "https://packages.swift.org"
myorg = "https://registry.mycompany.com/swift"
```

Maps registry names to SE-0292 base URLs. `default` is queried when a bare package name has no explicit `url` in `[swift-dependencies]` and no other named registry applies. The `myorg` registry is queried when the package name is prefixed with `myorg:` in the import string (e.g., `import swift "myorg:MyPrivateLib@^1.0" as lib`).

## Manifest: `[swift.publish]`

```toml
[swift.publish]
package-name = "MyMochiLib"
github-repo = "github.com/myorg/MyMochiLib"
swift-tools-version = "5.9"
platforms = [
    { name = "macOS", version = "13.0" },
    { name = "iOS", version = "16.0" },
    { name = "linux", version = "" },
]
registry = "https://packages.swift.org"
description = "A Mochi library published as a Swift package."
license = "Apache-2.0"
readme = "README.md"
```

| Key | Default | Meaning |
|-----|---------|---------|
| `package-name` | `[package].name` from `mochi.toml` | Swift package identity name. |
| `github-repo` | — | Remote for git-tag publish path (`owner/repo` or full URL). Required for `--to=swift-git`. |
| `swift-tools-version` | `"5.9"` | `// swift-tools-version:` header in the emitted `Package.swift`. |
| `platforms` | Inherits from `[swift].platforms` | Per-platform declarations in the emitted `Package.swift`. |
| `registry` | `https://packages.swift.org` | SE-0292 registry endpoint for `--to=swift-registry`. |
| `description` | `[package].description` | Package description for registry metadata. |
| `license` | `[package].license` | SPDX license identifier. |
| `readme` | `"README.md"` | Path to the README file included in the registry release. |

## Manifest: `[swift.capabilities]`

```toml
[swift.capabilities]
net = true
fs = false
proc = false
objc = false
unsafe = false
main-actor = false
```

| Key | Default | Meaning |
|-----|---------|---------|
| `net` | `false` | Package graph opens network connections (Alamofire, swift-nio, Vapor). |
| `fs` | `false` | Package graph reads or writes files. |
| `proc` | `false` | Package graph spawns subprocesses via `Foundation.Process`. |
| `objc` | `false` | Package graph uses Objective-C-bridged types. Requires an Apple platform target. |
| `unsafe` | `false` | Package graph uses `withUnsafePointer` items the user has hand-overridden. |
| `main-actor` | `false` | Package graph has `@MainActor`-isolated functions the user explicitly opts in to wrapping. |

The bridge walks the `.swiftinterface` surface at lock time, infers capability marks from API patterns (e.g., `URLSession` → `net`, `FileManager` → `fs`, `@objc` protocol → `objc`), and validates that the union is a subset of the user's `[swift.capabilities]` declaration.

## CLI surface

### `mochi pkg add swift <package>[@<semver>] [--url=<git-url>]`

```
$ mochi pkg add swift Alamofire@^5.9 --url=https://github.com/Alamofire/Alamofire.git
Added Alamofire = { version = "^5.9", url = "..." } to [swift-dependencies]
Running mochi pkg lock ...
Resolved 3 Swift packages (Alamofire + 2 transitive)
Wrote mochi.lock (+3 [[swift-package]] entries)
```

### `mochi pkg lock`

Walks `[swift-dependencies]`, shallow-clones each package at the resolved tag, generates `.swiftinterface` files, synthesises the `@_cdecl` wrapper, and writes `[[swift-package]]` entries into `mochi.lock`.

### `mochi pkg lock --check`

Recomputes `archive-blake3`, `swiftinterface-sha256`, `wrapper-sha256`, `swift-tools-version`, and `capabilities-declared` for every `[[swift-package]]` entry and exits non-zero on any mismatch. This is the CI reproducibility gate.

### `mochi pkg publish --to=swift-git [--remote=<url>] [--tag=<v>] [--dry-run]`

1. Builds the Mochi package as a Swift library via `TargetSwiftLibrary`.
2. Creates a semver-compliant annotated git tag (`v<version>` by default) on the current commit.
3. Optionally signs the tag (GPG or SSH via `commit.gpgsign`).
4. Pushes the tag to the configured `[swift.publish] github-repo` remote.
5. If the remote is GitHub, optionally creates a GitHub Release with the generated `Package.swift` and (when `xcframework = true`) the XCFramework as a release asset.
6. `--dry-run` creates the tag locally without pushing.

### `mochi pkg publish --to=swift-registry [--registry=<url>] [--dry-run]`

1. Builds the Swift library package.
2. Creates a source archive (a `git archive --format=tar.gz` at the locked tag).
3. Obtains an OIDC token from the CI environment (GitHub Actions `id-token: write`, GitLab CI).
4. POSTs the archive plus a signed release manifest to the SE-0292 registry endpoint.
5. Records the registry release URL alongside the pushed tag.
6. `--dry-run` validates the manifest without uploading.

### `mochi pkg publish --to=swift-registry --emit-ci`

Generates `.github/workflows/release.yml` for automated git-tag + registry publish on semver tags. The generated workflow:
- Runs on `push` events matching `v*.*.*` tag patterns.
- Calls `mochi pkg publish --to=swift-git` to create the GitHub Release.
- Calls `mochi pkg publish --to=swift-registry` to register with the SE-0292 registry.

### `mochi pkg sync swift`

Re-runs the `.swiftinterface` ingest and wrapper synthesiser from the existing `mochi.lock` without re-resolving versions. Used after manual edits to the synthesised shim or after a bridge upgrade.

## Per-import alias resolution

The alias `<alias>` introduced by `import swift "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/swift_wrap/<pkg>/shim.mochi` containing a corpus of `extern fn` declarations such as:

```mochi
extern type AFSession
extern fn af_request(url: string, method: string): AFSession from swift "Alamofire.AF.request(_:method:)"
extern fn af_response_data(session: AFSession, out_ptr: ptr, out_len: ptr): int from swift "Alamofire.DataRequest.response()"
extern fn af_session_free(session: AFSession) from swift "mochi_Alamofire_AFSession_free"
```

The import `import swift "Alamofire@^5.9" as http` becomes (post-resolution) `import "./swift_wrap/Alamofire/shim.mochi" as http`. The synthesised shim is read by the parser exactly as a hand-written `.mochi` file.

The shim is regenerated on every `mochi pkg lock` and is gitignored by default. Users who need to override a synthesised binding use:

```mochi
import swift "Alamofire@^5.9" as http_auto
extern fn af_upload(url: string, data: bytes): int from swift "mochi_Alamofire_upload" custom
```

The `custom` modifier keeps the override stable across `mochi pkg sync swift` runs.

## Cross-references

- [[02-design-philosophy]] for the rationale behind each language-surface choice.
- [[04-swiftinterface-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[07-spm-publish-flow]] for the `mochi pkg publish` paths.
- [MEP-69 §4](/docs/mep/mep-0069#4-surface-syntax-import-swift) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
