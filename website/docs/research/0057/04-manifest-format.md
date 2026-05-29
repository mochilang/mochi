---
title: "Manifest format: mochi.toml schema deep dive"
description: "Full schema of Mochi's mochi.toml manifest. Top-level structure, package keys, dependencies forms, features, targets, capabilities, provenance, workspace, and registry tables, validation rules, error codes, manifest evolution."
sidebar_position: 4
---

# 04. Manifest format: `mochi.toml` schema deep dive

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/manifest-format`.

This note specifies the full `mochi.toml` schema. The format choice rationale is in [02-design-philosophy](./02-design-philosophy) §1; the survey of comparable manifests is in [03-prior-art-registries](./03-prior-art-registries).

## 1. Top-level structure

```toml
# mochi.toml
[package]
name        = "@scope/name"          # required, scoped or unscoped
version     = "0.1.0"                # required, semver (PEP 440 / npm style)
description = "One line."            # optional, <= 200 chars
license     = "Apache-2.0"           # SPDX identifier; required for publish
authors     = ["Jane <jane@example.com>"]
repository  = "https://github.com/scope/name"
homepage    = "https://example.com"
keywords    = ["mochi", "datalog"]   # max 5
readme      = "README.md"            # path relative to manifest
edition     = "2026"                 # Mochi language edition
mochi       = ">=0.7, <1.0"          # Mochi compiler version range

[dependencies]
"@mochi/strings" = "^0.4"
"@mochi/json"    = { version = "^1.2", capabilities = ["fs.read"] }
"datalog"        = { version = "^1.0", optional = true }
"@my/internal"   = { workspace = true }

[dev-dependencies]
"@mochi/test" = "^0.3"

[features]
default   = ["json"]
json      = ["@mochi/json"]
datalog   = ["datalog"]

[targets]
typescript = { entrypoint = "src/main.mochi" }
python     = { entrypoint = "src/main.mochi" }
jvm        = { entrypoint = "src/main.mochi", ffi = ["java.util"] }
c          = { entrypoint = "src/main.mochi" }

[targets.python.overrides]
"src/clock.mochi" = "src/clock_python.mochi"

[capabilities]
required = ["fs.read", "net.dial"]
optional = ["proc.spawn"]

[provenance]
# Populated by the publish pipeline. Do not edit by hand.
sigstore_bundle = "<sigstore bundle reference, set at publish>"
oidc_issuer     = "https://token.actions.githubusercontent.com"

[workspace]
# Present only in the workspace root; sets default for members.
members = ["packages/*"]
exclude = ["packages/vendor"]

[workspace.dependencies]
"@mochi/strings" = "^0.4"

[workspace.targets]
default = ["typescript", "python"]

[registry]
default = "https://index.mochi.dev"

[[registry.alternate]]
name = "corp"
url  = "https://mirror.corp.example/mochi-registry"
```

## 2. `[package]` keys

| Key           | Type             | Required        | Notes                                                                     |
|---------------|------------------|-----------------|---------------------------------------------------------------------------|
| `name`        | string           | yes             | `[a-z][a-z0-9_-]*` or `@scope/[a-z][a-z0-9_-]*`. Scope max 39 chars; name max 64. |
| `version`     | string           | yes             | semver 2.0.0. Pre-release: `1.0.0-rc.1`. Build: `1.0.0+build.42`.        |
| `description` | string           | for publish     | One line, <= 200 UTF-8 chars. No control characters except space.         |
| `license`     | string           | for publish     | SPDX expression. Compound: `Apache-2.0 OR MIT`. `LicenseRef-` for custom.|
| `authors`     | string[]         | optional        | Free-form. Email parens convention.                                       |
| `repository`  | string (URL)     | optional        | https://, git+https://, or ssh://                                         |
| `homepage`    | string (URL)     | optional        |                                                                           |
| `keywords`    | string[] (max 5) | optional        | Each <= 32 chars. Lowercase recommended.                                  |
| `readme`      | string (path)    | optional        | Path to README file. Embedded in tarball.                                 |
| `edition`     | string           | yes             | Mochi language edition. `"2026"` initial.                                 |
| `mochi`       | string           | yes             | Compiler version range. `">=0.7, <1.0"`.                                  |

Scope namespace rules:
- `@mochi/` is reserved for the Mochi project itself.
- `@mochi-lang/` is reserved for language tooling.
- Other scopes are first-come-first-served subject to anti-typosquatting checks (see [12-risks-and-alternatives](./12-risks-and-alternatives) §1).
- Unscoped names share a flat global namespace; PRs to claim must demonstrate ownership of the homepage or git repo.

## 3. `[dependencies]` and friends

Three forms accepted per entry:

```toml
[dependencies]
shorthand = "^1.2"

table = { version = "^1.2", optional = true, capabilities = ["fs.read"], features = ["json"] }

workspace = { workspace = true }
```

Per-entry fields:

| Field          | Type              | Notes                                                                  |
|----------------|-------------------|------------------------------------------------------------------------|
| `version`      | string            | semver range.                                                          |
| `optional`     | bool              | Only included when the feature gating it is enabled.                   |
| `capabilities` | string[]          | Subset of declared capabilities the dep is permitted to use.           |
| `features`     | string[]          | Features to enable in the dep.                                         |
| `default-features` | bool          | Disable default features. Default `true`.                              |
| `workspace`    | bool              | Use workspace pin. Must be set in workspace root's `[workspace.dependencies]`. |
| `path`         | string            | Path to a local checkout. For dev only; excluded on publish.            |
| `git`          | string (URL)      | Git URL. For dev only; excluded on publish.                            |
| `branch`/`tag`/`rev` | string      | Used with `git`.                                                       |
| `registry`     | string            | Use a non-default registry by name (from `[[registry.alternate]]`).    |

Version range grammar (npm + Cargo intersect):

```
range = comparator (',' comparator)*
comparator = ('^' | '~' | '<' | '<=' | '>' | '>=' | '=' )? version
           | '*'                                                       ; any
version = digits '.' digits '.' digits ('-' prerelease)? ('+' build)?
```

Default operator if none given: `^`. So `"1.2.3"` means `"^1.2.3"`. This matches Cargo and npm; uv adopted the same.

## 4. `[features]`

Cargo-style optional features. Default feature set in `default = [...]`. Each feature toggles a list of dependencies and/or other features:

```toml
[features]
default = ["json"]
json    = ["@mochi/json"]
xml     = ["@mochi/xml"]
parsers = ["json", "xml"]
```

Features intersect with `optional = true` deps: a dep marked optional is only resolved when a feature listing it is enabled. The solver expands the feature graph before propagating constraints.

A feature can transitively enable a dep's feature: `"@mochi/json/streaming"` enables the `streaming` feature of `@mochi/json`. This matches Cargo's exact syntax.

## 5. `[targets]`

Declares which transpiler targets the package supports. Each target table:

```toml
[targets.<target>]
entrypoint = "src/main.mochi"        # required
ffi        = ["string", ...]         # optional, target-specific FFI deps
```

The target name is one of: `c`, `beam`, `jvm`, `dotnet`, `swift`, `kotlin`, `python`, `typescript`, `rust`. Adding a target requires a MEP (e.g. MEP-55 for Wasm).

`ffi` is a list of target-specific dependency identifiers. For the `jvm` target, FFI entries are Maven coordinates (`groupId:artifactId:version`). For `python`, PyPI distribution names plus version specifiers (`httpx>=0.27,<1`). For `typescript`, npm package names. The publish pipeline declares these as transitive deps in the target artifact.

### 5.1 Per-target overrides

```toml
[targets.python.overrides]
"src/clock.mochi" = "src/clock_python.mochi"
```

At compile time, when the target is `python`, the resolver substitutes `src/clock.mochi` with `src/clock_python.mochi` everywhere in the module graph. The override file must declare the same module-level interface (same exported symbols with compatible types) as the file it overrides; the compiler verifies.

### 5.2 Per-target dependency overrides

```toml
[targets.python.dependencies]
"@scope/fs" = { version = "^0.5" }  # use a Python-friendly fs lib
```

These deps replace the default `[dependencies]` entry for that target. The solver runs once per target with the merged dep set.

## 6. `[capabilities]`

Closed set: `fs.read`, `fs.write`, `net.dial`, `net.listen`, `env`, `ffi`, `clock`, `random`, `proc.spawn`.

```toml
[capabilities]
required = ["fs.read", "net.dial"]
optional = ["proc.spawn"]
```

`required` capabilities must be available; the consumer's manifest pins them. `optional` capabilities are documented but not asserted; their absence triggers a `capability_missing` runtime fault if the dep tries to use one.

See [10-capability-model](./10-capability-model) for enforcement semantics.

## 7. `[provenance]`

Populated by the publish pipeline. Manifest carries empty/placeholder values; the published artifact's manifest is rewritten by the publisher with:

```toml
[provenance]
sigstore_bundle = "<base64 of bundle>"   # full Sigstore signature bundle
oidc_issuer     = "https://token.actions.githubusercontent.com"
oidc_subject    = "repo:scope/name:ref:refs/tags/v0.1.0"
build_timestamp = "2026-05-29T06:35:00Z"
source_commit   = "<git sha-256>"
build_tool      = "mochi-publish/0.7.0"
```

The consumer never edits this. `mochi publish --dry-run` shows what will be set. See [09-trusted-publishing](./09-trusted-publishing) for the full flow.

## 8. `[workspace]` (workspace root only)

```toml
[workspace]
members = ["packages/*", "tools/cli"]
exclude = ["tools/cli/vendor"]
default-target = ["typescript"]

[workspace.dependencies]
"@mochi/strings" = "^0.4"
"@mochi/json"    = "^1.2"

[workspace.targets]
default = ["typescript", "python", "jvm"]
```

Members can be globs. Excludes apply after globs. Members must each carry their own `mochi.toml`.

Workspace dependencies are pinned in `[workspace.dependencies]`; members reference them with `{ workspace = true }`. This is the Cargo inheritance pattern.

The companion file `mochi.workspace.toml` is a synonym for an empty package with `[workspace]` filled. A directory may have either:

- `mochi.toml` with `[workspace]` block (member-and-workspace-root combined; the directory is itself a package).
- `mochi.workspace.toml` (workspace umbrella with no package; members are required).

The detection rule prefers `mochi.workspace.toml` for the umbrella case to avoid ambiguity.

## 9. `[registry]` and `[[registry.alternate]]`

```toml
[registry]
default = "https://index.mochi.dev"

[[registry.alternate]]
name = "corp"
url  = "https://mirror.corp.example/mochi-registry"

[[registry.alternate]]
name = "local"
url  = "file:///opt/mochi-cache"
```

Per-dependency override:

```toml
"@my/internal" = { version = "^1.0", registry = "corp" }
```

The default registry is the central `index.mochi.dev`. Alternates are typed; `mochi publish --target=mochi-central` always pushes to the default registry, `--target=corp` to the `corp` alternate.

A `file://` registry is supported for offline / vendor use; see [07-registry-index](./07-registry-index) §7.

## 10. Manifest validation

The parser:

1. Reads TOML using a strict TOML 1.0.0 parser. Mochi's TOML parser rejects unknown top-level keys (closed schema) but warns rather than errors on unknown sub-keys to allow forward-compatible additions.
2. Validates `package.name` against the scope/name regex.
3. Validates `package.version` against semver 2.0.0.
4. Validates `package.license` against the SPDX expression grammar (3.x).
5. Validates each dep version against the version range grammar.
6. Validates each capability against the closed set.
7. Validates each target against the closed set.
8. Resolves workspace inheritance and computes the effective dependency set.
9. Cross-checks: every feature lists deps or features that exist; every override path exists.

Errors are reported with `M057_MANIFEST_E<NNN>` codes:

- `M057_MANIFEST_E001`: invalid TOML.
- `M057_MANIFEST_E002`: missing required key.
- `M057_MANIFEST_E003`: invalid package name.
- `M057_MANIFEST_E004`: invalid version.
- `M057_MANIFEST_E005`: invalid license expression.
- `M057_MANIFEST_E006`: invalid version range.
- `M057_MANIFEST_E007`: unknown capability.
- `M057_MANIFEST_E008`: unknown target.
- `M057_MANIFEST_E009`: workspace inheritance unresolved.
- `M057_MANIFEST_E010`: feature lists undeclared dep.
- `M057_MANIFEST_E011`: override path does not exist.
- `M057_MANIFEST_E012`: unknown top-level key.

## 11. Manifest evolution

The manifest carries no explicit version field; the schema is identified by the Mochi edition pinned in `package.edition`. The parser dispatches on edition:

```go
switch m.Package.Edition {
case "2026":
    return parseEdition2026(buf)
default:
    return errEditionNotSupported(m.Package.Edition)
}
```

Future editions can add or remove keys; the parser keeps editions for at least three years per the Mochi compatibility window.

Forward compatibility: unknown sub-keys (inside known tables) generate warnings, not errors, so a v1 parser can read a v2-shaped manifest that adds new sub-keys without breaking. Unknown top-level tables are errors (closed schema at the root).

## 12. Cross-MEP cross-walks

A Mochi package that publishes to a downstream ecosystem maps `[package]` to that ecosystem's metadata. The mapping table is in [11-polyglot-fanout](./11-polyglot-fanout) §4; here is the abbreviated form for the npm and PyPI cases:

| `[package]` field | npm `package.json`     | PyPI `pyproject.toml` `[project]` |
|-------------------|------------------------|-----------------------------------|
| `name`            | `name`                 | `name`                            |
| `version`         | `version`              | `version`                         |
| `description`     | `description`          | `description`                     |
| `license`         | `license`              | `license`                         |
| `authors`         | `author`               | `authors`                         |
| `repository`      | `repository.url`       | `[project.urls].repository`       |
| `homepage`        | `homepage`             | `[project.urls].homepage`         |
| `keywords`        | `keywords`             | `keywords`                        |
| `readme`          | `readme`               | `readme`                          |

The publish pipeline emits target-correct metadata; the source of truth stays `mochi.toml`.

## 13. Worked manifest examples

### 13.1 Tiny library

```toml
[package]
name        = "@mochi/strings"
version     = "0.4.7"
description = "String manipulation utilities for Mochi."
license     = "Apache-2.0"
authors     = ["Mochi authors"]
repository  = "https://github.com/mochilang/strings"
edition     = "2026"
mochi       = ">=0.7, <1.0"

[targets]
typescript = { entrypoint = "src/main.mochi" }
python     = { entrypoint = "src/main.mochi" }
jvm        = { entrypoint = "src/main.mochi" }
c          = { entrypoint = "src/main.mochi" }
beam       = { entrypoint = "src/main.mochi" }
dotnet     = { entrypoint = "src/main.mochi" }
kotlin     = { entrypoint = "src/main.mochi" }
swift      = { entrypoint = "src/main.mochi" }
rust       = { entrypoint = "src/main.mochi" }
```

No deps, no capabilities (string ops are pure). Fans out to all nine targets.

### 13.2 CLI tool with FFI

```toml
[package]
name        = "@example/migrate"
version     = "1.5.0"
description = "Database migration tool."
license     = "MIT"
edition     = "2026"
mochi       = ">=0.7, <1.0"

[dependencies]
"@mochi/strings" = "^0.4"
"@mochi/json"    = "^1.2"

[targets.python]
entrypoint = "src/main.mochi"
ffi = ["psycopg[binary]>=3.2"]

[targets.jvm]
entrypoint = "src/main.mochi"
ffi = ["org.postgresql:postgresql:42.7.4"]

[capabilities]
required = ["fs.read", "fs.write", "net.dial", "env"]
```

Targets two ecosystems with target-specific Postgres drivers. Capabilities declare DB access (network), config access (env), and local file work.

### 13.3 Workspace

```toml
# my-workspace/mochi.workspace.toml
[workspace]
members = ["packages/parser", "packages/codegen", "packages/cli"]

[workspace.dependencies]
"@mochi/strings" = "^0.4"
"@mochi/json"    = "^1.2"

[workspace.targets]
default = ["typescript", "python"]
```

```toml
# my-workspace/packages/parser/mochi.toml
[package]
name    = "@my/parser"
version = "0.1.0"
license = "Apache-2.0"
edition = "2026"
mochi   = ">=0.7, <1.0"

[dependencies]
"@mochi/strings" = { workspace = true }
```

```toml
# my-workspace/packages/codegen/mochi.toml
[package]
name    = "@my/codegen"
version = "0.1.0"
license = "Apache-2.0"
edition = "2026"
mochi   = ">=0.7, <1.0"

[dependencies]
"@my/parser"     = { workspace = true }
"@mochi/strings" = { workspace = true }
```

The `[workspace.dependencies]` pin propagates; the members declare `workspace = true` and inherit.

## 14. Anti-patterns and explicit rejections

- **No `[scripts]` section**: npm-style scripts (`"build": "tsc"`) couple manifests to a runtime concept. Mochi's build is implicit (`mochi build` figures it out). Users can author scripts via shell scripts; the manifest stays declarative.
- **No `[devDependencies]` for "tools you need to compile this"**: those go in `[dev-dependencies]` per Cargo. The dev set is included in lockfile but excluded from published tarball.
- **No `[engines]` field**: the `mochi = ">=0.7, <1.0"` compiler version replaces npm's `engines.node`.
- **No environment-variable references in the manifest**: TOML allows literal `"${HOME}/path"` strings, but the parser does not expand them. Reproducibility requires the manifest to be hermetic.
- **No `git+https://github.com/...` URL paths for *published* artifacts**: a published `[provenance]` block must point at a Sigstore bundle, not a git URL. The `git = "..."` form is only valid in `[dependencies]` and only for dev paths.

## 15. Cross-references

- Why TOML: [02-design-philosophy](./02-design-philosophy) §1.
- Manifest in survey context: [03-prior-art-registries](./03-prior-art-registries).
- Lockfile that lock manifests: [06-lockfile-format](./06-lockfile-format).
- Capability vocabulary: [10-capability-model](./10-capability-model).
- Polyglot field mapping: [11-polyglot-fanout](./11-polyglot-fanout) §4.
- Rejected alternative formats: [12-risks-and-alternatives](./12-risks-and-alternatives) §6.
