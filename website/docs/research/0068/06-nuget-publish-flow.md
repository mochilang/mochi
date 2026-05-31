---
title: "06. NuGet publish flow"
sidebar_position: 7
sidebar_label: "06. NuGet publish flow"
description: "The NuGet V3 protocol (registration, flat container, search, publish endpoints), the .nupkg archive format, dotnet pack invocation, per-package metadata requirements, and the publish-side gate."
---

# 06. NuGet publish flow

This note covers both directions of the NuGet protocol: Direction 1 (fetching packages from nuget.org for the consumer path) and Direction 2 (publishing a Mochi package as a `.nupkg` to nuget.org for the producer path).

## NuGet V3 protocol overview

NuGet V3 is a JSON-based REST protocol whose entry point is the **service index** at `https://api.nuget.org/v3/index.json`. The service index lists resource URLs by `@type`:

| Resource type | Example URL | Purpose |
|--------------|-------------|---------|
| `RegistrationsBaseUrl/3.6.0` | `https://api.nuget.org/v3/registration5-gz-semver2/` | Package registration blobs (version lists, metadata) |
| `PackageBaseAddress/3.0.0` | `https://api.nuget.org/v3-flatcontainer/` | Content download (`.nupkg`, `.nuspec`, `.dll`) |
| `SearchQueryService/3.5.0` | `https://azuresearch-usnc.nuget.org/query` | Full-text package search |
| `PackagePublish/2.0.0` | `https://www.nuget.org/api/v2/package` | Upload endpoint for push |
| `SymbolPackagePublish/4.9.0` | `https://nuget.smbsrc.net/api/v2/symbolpackage` | Symbol package upload |
| `TrustedPackagePublish/1.0.0` | `https://api.nuget.org/v3/trustedpublish/` | OIDC trusted publish endpoint (GA November 2024) |

The bridge uses the service index to discover the current resource URLs at lock time, caching the index for 24 hours.

## Direction 1: fetching packages

### Version resolution

For each `[dotnet-dependencies]` entry, the bridge queries the registration endpoint:

```
GET https://api.nuget.org/v3/registration5-gz-semver2/<id>/index.json
```

The response is a registration index whose `items` array contains registration page blobs. Each page covers a version range. The bridge walks the pages (requesting individual page URLs when `@id` is present) to collect all available versions and their metadata.

Version resolution follows NuGet's SemVer 2.0 logic:
- Four-part versions (`13.0.3.0`) are normalised to `13.0.3` (dropping trailing zero).
- Prerelease versions are excluded unless `prerelease = true` is set in the dependency entry.
- Version range syntax: `[13.0.3]` (exact), `(13.0.0,)` (exclusive lower), `[12.0.0,14.0.0)` (range), `^13.0` (MEP-68 shorthand for `[13.0.0,14.0.0)`).
- Dependency conflicts are resolved via the "nearest wins" rule (the directly-declared version wins over transitive), then "lowest applicable" (within a range, pick the lowest version that satisfies all constraints).

### Content download

Once the version is resolved, the bridge downloads the `.nupkg` archive:

```
GET https://api.nuget.org/v3-flatcontainer/<id>/<version>/<id>.<version>.nupkg
```

The `.nupkg` is a ZIP archive with the structure:

```
Newtonsoft.Json.13.0.3.nupkg
├── [Content_Types].xml
├── _rels/.rels
├── Newtonsoft.Json.nuspec           # package metadata
├── lib/
│   ├── net20/Newtonsoft.Json.dll    # .NET 2.0 target (legacy)
│   ├── netstandard2.0/
│   │   ├── Newtonsoft.Json.dll      # NS2.0 target (primary for bridge)
│   │   └── Newtonsoft.Json.xml      # XML documentation
│   └── net6.0/Newtonsoft.Json.dll   # .NET 6 target
├── build/
├── analyzers/
└── icon.png
```

The bridge selects the DLL from the highest `lib/<tfm>/` folder that is compatible with the configured `[dotnet] framework` (following NuGet's TFM compatibility graph: `net9.0` is compatible with `net8.0`, `net6.0`, `netstandard2.0`, `netstandard2.1`, `netcoreapp3.1`, etc.). The XML documentation file from the same folder is also extracted.

Hash verification uses the `nupkg-blake3` (computed by the bridge) and `nupkg-sha512` (published by nuget.org in the registration blob's `packageHash` field with `packageHashAlgorithm: "SHA512"`). Both hashes must match the lockfile entries at `mochi pkg lock --check` time.

### Content-addressed cache

Downloaded `.nupkg` archives are stored at:

```
~/.cache/mochi/dotnet-deps/<blake3-hex>/<id>.<version>.nupkg
```

and extracted to:

```
~/.cache/mochi/dotnet-deps/<blake3-hex>/extracted/
├── <id>.dll
├── <id>.xml
└── <id>.nuspec
```

The extracted DLL is what the ECMA-335 parser reads. The cache is write-once (the BLAKE3 hash is the key); no update is ever needed unless the lockfile hash changes.

### NuGet.Config for wrapper project restore

The wrapper project's `dotnet restore` must resolve the NuGet package from the local cache, not the internet, to ensure reproducibility and offline builds. The bridge writes a `NuGet.Config` alongside the wrapper project:

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="mochi-local" value="<workdir>/dotnet_deps/" />
  </packageSources>
  <fallbackPackageFolders>
    <add key="mochi-cache" value="~/.cache/mochi/dotnet-deps/" />
  </fallbackPackageFolders>
</configuration>
```

This ensures `dotnet restore` uses only the pre-downloaded packages.

## Direction 2: publishing to nuget.org

### `mochi pkg publish --to=nuget.org` flow

1. **Emit `TargetDotnetLibrary`.** The MEP-53 driver emits the `.csproj`, `Src/*.cs`, XML documentation, and `.nuspec`.

2. **Run `dotnet build`.** Compiles the emitted C# source to a DLL.

3. **Run `dotnet pack`.** Produces `<id>.<version>.nupkg` in the configured output directory. The bridge specifies `--no-build` (the DLL is already compiled) and `--output <outdir>`. `dotnet pack` reads the `.nuspec` or the `<PackageId>`, `<Version>`, `<Description>`, `<Authors>` properties from `.csproj` to populate the `.nupkg` metadata.

4. **Obtain OIDC token.** See [[07-oidc-nuget-trusted-publishing]].

5. **Upload via `TrustedPackagePublish` endpoint.** `PUT https://api.nuget.org/v3/trustedpublish/<id>/<version>` with the OIDC token in the `X-NuGet-OIDC-Token` header and the `.nupkg` bytes in the request body.

6. **Record Rekor log entry.** If the upload succeeds, nuget.org returns a Rekor log entry URL in the `X-NuGet-Rekor-Log-Entry` response header. The bridge records this in `mochi.lock` under `[published.nuget]`.

### The `.nuspec` metadata fields

The bridge generates a `.nuspec` from `mochi.toml`'s `[package]` section:

```xml
<?xml version="1.0" encoding="utf-8"?>
<package>
  <metadata>
    <id>MyPackage</id>
    <version>1.0.0</version>
    <description>A Mochi-authored .NET library for...</description>
    <authors>Alice Example</authors>
    <license type="expression">MIT</license>
    <repository type="git" url="https://github.com/example/mypackage" />
    <projectUrl>https://mypackage.dev</projectUrl>
    <readme>README.md</readme>
    <icon>assets/icon.png</icon>
    <tags>mochi dotnet example</tags>
    <requireLicenseAcceptance>false</requireLicenseAcceptance>
    <dependencies>
      <group targetFramework="net9.0">
        <!-- [dotnet-dependencies] entries that are public dependencies -->
      </group>
    </dependencies>
  </metadata>
</package>
```

Required fields (nuget.org rejects packages missing these): `id`, `version`, `description`, `authors`. If `mochi.toml`'s `[package]` section lacks any required field, `mochi pkg publish` exits with an error listing the missing fields.

The `license` field uses the SPDX expression format (`MIT`, `Apache-2.0`, `MIT OR Apache-2.0`). If the `[package] license` field is absent, `mochi pkg publish` warns and defaults to `proprietary` (not an SPDX expression; nuget.org accepts it but marks the package as unlicensed in its UI).

### Symbol packages

Mochi does not emit `.pdb` symbol files (the emitted C# source is the "source", and the `mochi-dotnet-runtime` package provides the helpers; there is no Mochi-authored .pdb). The bridge skips `SymbolPackagePublish`. A future sub-phase may emit a source-link-enabled `.pdb` that points to the Mochi source repository.

### Dry-run mode

`mochi pkg publish --to=nuget.org --dry-run` runs steps 1-3, skips steps 4-6, and instead:

- Runs `dotnet pack --output <outdir>` and validates the resulting `.nupkg` (all required metadata present, no duplicate files, assembly version matches package version).
- Runs `nuget verify` (if the `nuget` CLI is available) to assert the package is well-formed.
- Emits a `DRY_RUN: upload would have PUT <nupkg-size> bytes to https://api.nuget.org/v3/trustedpublish/...` log line.

This mode is used in CI to validate the publish path without actually uploading (and without needing a trusted-publisher configuration on nuget.org).

## NuGet package validation gates

Before upload, the bridge validates:

1. **No duplicate assembly names.** The `.nupkg` must not contain two DLLs with the same `AssemblyName` attribute.
2. **Version matches manifest.** The `<version>` in `.nuspec` must match `mochi.toml [package] version`.
3. **Licence compatibility.** The transitive dep graph's SPDX licence union must be compatible with the declared `[package] license` (same check as MEP-73 §Risks §10 but for NuGet SPDX metadata).
4. **No pre-release transitive deps unless pre-release version.** A `1.0.0` package must not depend on `1.0.0-alpha.1` packages.
5. **Assembly strong-name consistency.** If the package is strong-named (sign key in `mochi.toml`), the generated assembly must be signed with the matching key.
