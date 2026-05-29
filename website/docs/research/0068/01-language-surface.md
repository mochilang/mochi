---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import dotnet \"<package>@<semver>\" as <alias>` import form, the `[dotnet-dependencies]` / `[dotnet]` / `[dotnet.publish]` / `[dotnet.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add dotnet`, `mochi pkg lock`, `mochi pkg publish --to=nuget.org`, `mochi pkg sync dotnet`), and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-68 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "dotnet"
```

MEP-68 adds `dotnet` as the fifth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<PackageName>` | Bare name. Resolves through `[dotnet-dependencies]` plus `mochi.lock`. The lockfile records the picked version. |
| `<PackageName>@<semver-req>` | Explicit constraint (`^13.0`, `~3.1`, `>=8.0`). Must be compatible with `[dotnet-dependencies]`. |
| `<PackageName>@path+<rel-path>` | Path source, relative to the manifest directory. |

Example surface:

```mochi
import dotnet "Newtonsoft.Json@^13.0" as json
import dotnet "Serilog" as log
import dotnet "Dapper" as dapper

fn fetch_users(conn: any): list<string> {
    let users = dapper.SqlMapper.Query(conn, "SELECT Name FROM Users")
    log.Log.Information("Found {Count} users", users.Count)
    return users.Select(u => json.JsonConvert.SerializeObject(u))
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` declaration the bridge generated for the package's public API members. Namespace hierarchy from the CLR is preserved: `json.Newtonsoft.Json.JsonConvert` for the fully-qualified form, or with `auto`, `JsonConvert` directly.

The `auto` modifier (already accepted for `import go ... auto` and `import rust ... auto`) is admitted for `import dotnet ... auto`. With `auto`, every public top-level type of the package is bound at file scope rather than under the alias namespace. Default is namespaced.

## Manifest: `[dotnet-dependencies]`

This table is the user-facing dependency declaration:

```toml
[dotnet-dependencies]
Newtonsoft.Json = "^13.0"
Serilog = "^3.1"
Microsoft.Extensions.DependencyInjection = "^8.0"
System.Text.Json = "^8.0"
Dapper = "^2.1"
Microsoft.Extensions.Http = { version = "^8.0", framework = "net8.0" }
MyLocalPackage = { path = "../MyPackage" }
```

The grammar mirrors NuGet's `<PackageReference>` conventions:

- A bare string is shorthand for `{ version = "..." }`.
- The table form admits `version`, `framework` (TFM override for this dep), `path` (local path source), and `prerelease` (whether to allow prerelease versions).
- Package names are case-insensitive on nuget.org; the bridge normalises to the canonical casing from the NuGet index.

The user does not write a `.csproj` file. The bridge synthesises the `<PackageReference>` entries in each shim's `.csproj` at build time, pinning exact versions from `mochi.lock`.

## Manifest: `[dotnet]`

```toml
[dotnet]
framework = "net8.0"
runtime = "osx-arm64"
bridge = "clr-hosting"
monomorphise = [
    { item = "System.Linq.Enumerable.Select", T = "string" },
    { item = "System.Collections.Generic.List`1.Add", T = "int" },
]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `framework` | `"net8.0"` | Target framework moniker (TFM) for resolution. |
| `runtime` | (auto-detected) | Runtime identifier (RID) for NativeAOT and platform-specific packages. |
| `bridge` | `"clr-hosting"` | How the shim is loaded at runtime. Alt: `"nativeaot"`. |
| `monomorphise` | `[]` | Explicit generic instantiations for the type-mapping pass. |

The `framework` key accepts any recognised .NET TFM: `"net6.0"`, `"net8.0"` (LTS), `"net9.0"`. The bridge uses this TFM when selecting which dependency group from a package's `.nuspec` to follow.

The `runtime` key is used only for NativeAOT builds and for packages that ship platform-specific native libraries (e.g., packages with `runtimes/<rid>/native/*.so` assets). For CLR hosting builds, the runtime is determined by the installed .NET SDK.

The `monomorphise` table is the only path to import a generic-parameter CLR type. The bridge does not auto-monomorphise (see [[10-generics-and-reification]] §2).

## Manifest: `[dotnet.publish]`

```toml
[dotnet.publish]
package-id = "MyMochiLib"
version = "1.0.0"
authors = ["tamnd"]
description = "A Mochi library as a NuGet package."
target-framework = "net8.0"
readme = "README.md"
project-url = "https://github.com/example/mymochi"
license = "Apache-2.0"
aot = false
```

| Key | Default | Meaning |
|-----|---------|---------|
| `package-id` | (from `[package].name`) | The NuGet package identifier. |
| `version` | (from `[package].version`) | The NuGet package version. |
| `authors` | (from `[package].authors`) | Comma-separated author names for the `.nuspec`. |
| `description` | (from `[package].description`) | Package description for nuget.org. Max 4000 characters. |
| `target-framework` | `"net8.0"` | The `<TargetFramework>` in the emitted `.csproj`. |
| `readme` | `"README.md"` | Path to the README file to embed in the package. |
| `license` | (from `[package].license`) | SPDX expression. Required for nuget.org publish. |
| `aot` | `false` | Whether to also publish an AOT-compiled variant alongside the managed assembly. |

This table is only consulted when the user runs `mochi pkg publish --to=nuget.org`. Mochi packages that do not publish to nuget.org can omit it.

## Manifest: `[dotnet.capabilities]`

```toml
[dotnet.capabilities]
net = true
fs = false
```

These capability flags are a strict refinement of MEP-57's `[capabilities]` table. The bridge walks the .NET dep graph at lock time and computes the union of capability marks across every reachable package. If the union exceeds the declaration, lock fails with a diagnostic.

The two canonical capabilities for .NET deps are:

- `net`: any reachable package opens TCP, UDP, or network sockets (`System.Net.Http.HttpClient`, `System.Net.Sockets.*`).
- `fs`: any reachable package reads or writes files (`System.IO.File`, `System.IO.Stream` to disk-backed streams).

Capabilities outside this set (clock, env, random) are inherited from MEP-57's broader `[capabilities]` table.

## CLI surface

The `mochi pkg` subcommand gains four new operations:

### `mochi pkg add dotnet <Package>[@<version>]`

```
$ mochi pkg add dotnet Newtonsoft.Json@^13.0
Added Newtonsoft.Json = "^13.0" to [dotnet-dependencies]
Running mochi pkg lock ...
Resolved 3 .NET packages (Newtonsoft.Json + 2 transitive)
Wrote mochi.lock (+3 [[dotnet-package]] entries)
```

Equivalent to manually editing `mochi.toml` plus running `mochi pkg lock`. Idempotent if the entry already exists at a compatible version.

### `mochi pkg lock`

Walks `[dotnet-dependencies]`, runs the resolver against the NuGet v3 API, downloads each package to the content-addressed cache, runs `mochi-dotnet-meta` on each, synthesises the C# shim for each, and writes a `[[dotnet-package]]` entry per dep into `mochi.lock`.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `nupkg-sha512`, `metadata-sha256`, `shim-sha256`, and `capabilities-declared`, and exits non-zero on any mismatch. This is the CI-enforced reproducibility gate.

### `mochi pkg publish --to=nuget.org [--dry-run]`

- Builds the package via `Driver.Build` with `target = TargetDotNetLibrary`.
- Runs `dotnet pack -c Release --no-build` to produce the `.nupkg`.
- Validates the `.nuspec` metadata.
- Obtains a GitHub Actions OIDC token from the CI environment.
- Presents the token plus the `.nupkg` to nuget.org's trusted-publishing endpoint.
- Records the publish confirmation.

The `--dry-run` flag skips upload; the metadata validation and OIDC token acquisition are still exercised.

### `mochi pkg sync dotnet`

Re-runs the shim generator from the existing `mochi.lock`, without re-resolving versions or re-downloading packages. Used after manual edits to a synthesised shim file or after a bridge version upgrade that changes the shim format.

## Per-import alias resolution

The alias `<alias>` introduced by `import dotnet "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/dotnet_shim/<pkg>/shim.mochi` containing a corpus of `extern fn` declarations like:

```mochi
extern type JsonConvert
extern fn deserialize_object(json: string): any from dotnet "Newtonsoft.Json.JsonConvert.DeserializeObject"
extern fn serialize_object(value: any): string from dotnet "Newtonsoft.Json.JsonConvert.SerializeObject"
```

The import `import dotnet "Newtonsoft.Json" as json` becomes (post-resolution) `import "./dotnet_shim/Newtonsoft.Json/shim.mochi" as json`. The synthesised shim is read by the parser exactly as a hand-written `.mochi` file would be.

The shim file is regenerated on every `mochi pkg lock` and is gitignored by default. Users who need to customise a synthesised binding can override individual items:

```mochi
import dotnet "Newtonsoft.Json" as json_auto
extern fn json_deserialize_typed(json: string): MyType from dotnet "Newtonsoft.Json.JsonConvert.DeserializeObject`1[MyType]" custom
```

The `custom` modifier keeps the override stable across `mochi pkg sync dotnet` runs.

## Cross-references

- [[02-design-philosophy]] for the rationale.
- [[04-assembly-metadata-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-nuget-publish-flow]] for the `mochi pkg publish` path.
- [MEP-68 §4](/docs/mep/mep-0068#4-surface-syntax-import-dotnet) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
