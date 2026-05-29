---
title: "Phase 14. Polyglot fan-out"
sidebar_position: 15
sidebar_label: "Phase 14. Polyglot fan-out"
description: "MEP-57 Phase 14 — `mochi pkg publish --target=<eco>` for npm + JSR, PyPI, Maven Central, NuGet, crates.io, Hex, Swift Package Index, Kotlin Multiplatform. Per-target packaging plus metadata mapping."
---

# Phase 14. Polyglot fan-out

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 14](/docs/mep/mep-0057#phase-14-polyglot) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

This is an umbrella phase. Per [the umbrella phase coverage rule](/docs/research/0057/risks-and-alternatives), it is not LANDED until every target gate is green. Missing targets become 14.1, 14.2, etc. sub-phases.

## Gate

`TestPhase14Polyglot`: for each opted-in target, `mochi pkg publish --target=<x> --dry-run` produces a target-correct artifact (e.g. valid `package.json`, valid `pyproject.toml`, valid `pom.xml`); round-trip through a mock target registry succeeds.

Pass criteria:

1. Dispatcher correctness. `mochi pkg publish --target=all` invokes every target driver in dependency-free parallel; per-target failures collected and reported as a single error tree.
2. Per-target artefact validity. Each target's emitted manifest validates against its ecosystem's spec (e.g., `package.json` against npm schema, `pyproject.toml` against PEP 621, `pom.xml` against Maven 4 XSD).
3. Field mapping completeness. Every `[package]` field that has an analogue in the target ecosystem is mapped; the matrix in research note 11 §3 is the spec.
4. Round-trip through mock registries. Each driver tested against a local mock of its target registry (`verdaccio`, `devpi`, `nginx-as-maven`, `local-nuget`, etc.) and a publish succeeds.
5. `--strict` mode behaviour. With `--strict`, the first target failure aborts the run and rolls back any per-target staging directories; without `--strict`, other targets continue.
6. Capability translation. The `.caps.json` sidecar from Phase 10 maps to target-specific runtime gates (Deno permissions for npm, Python seatbelt config for PyPI, Wasm component imports for the wasm target).
7. Determinism per target. Each target's artefact is byte-deterministic (same source → same hash); same property as Phase 12 main tarball.

## Goal-alignment audit

Polyglot fan-out is the *adoption surface*. Without it, Mochi libraries are useless to non-Mochi consumers. The user-facing goal moved: "One `mochi pkg publish --target=all` publishes my library to npm + PyPI + Maven Central + ... in one tag push".

The "one push, many ecosystems" workflow is the empirical answer to language-lock-in. Library authors today either pick one ecosystem (and lose the others), maintain N parallel codebases, or write thin wrappers in each language (and accept the maintenance burden). Mochi compiles once to each target's idiomatic shape; the fan-out automates the distribution side.

The umbrella structure (sub-phases 14.1 to 14.8 per target) lets each target ship independently. The `mochi pkg publish` command refuses targets that have not yet landed at v1; `--target=python` either works fully or errors with `M057_FAN_E002` (see [error registry](./errors)). There is no "best-effort" partial support.

## Sub-phases

| # | Scope | MEP driver | Status | Commit |
|---|-------|-----------|--------|--------|
| 14.0 | Per-target packaging dispatcher | (this MEP) | NOT STARTED | — |
| 14.1 | npm + JSR target | MEP-52 | NOT STARTED | — |
| 14.2 | PyPI target | MEP-51 | NOT STARTED | — |
| 14.3 | Maven Central target | MEP-47 | NOT STARTED | — |
| 14.4 | NuGet target | MEP-48 | NOT STARTED | — |
| 14.5 | Swift Package Index target | MEP-49 | NOT STARTED | — |
| 14.6 | Kotlin Multiplatform target | MEP-50 | NOT STARTED | — |
| 14.7 | crates.io target | MEP-53 | NOT STARTED | — |
| 14.8 | Hex target | MEP-46 | NOT STARTED | — |
| 14.9 | Field mapping table per target | (this MEP) | NOT STARTED | — |
| 14.10 | `--strict` and per-target rollback | (this MEP) | NOT STARTED | — |

## Sub-phase 14.0 — Dispatcher

```go
// pkg/pkgfanout/dispatcher.go
type Driver interface {
    Name() string                                             // "npm", "pypi", ...
    Build(ctx context.Context, src *Source) (*Artefact, error)
    Validate(art *Artefact) error
    Upload(ctx context.Context, art *Artefact, creds Creds) error
    DryRunPrint(w io.Writer, art *Artefact) error
}

type Dispatcher struct {
    Drivers map[string]Driver
}

func (d *Dispatcher) Run(ctx context.Context, src *Source, targets []string, strict bool) error {
    var wg sync.WaitGroup
    errs := make(chan error, len(targets))
    for _, t := range targets {
        drv, ok := d.Drivers[t]
        if !ok { errs <- fmt.Errorf("%w: %s", ErrTargetUnsupported, t); continue }
        wg.Add(1)
        go func(drv Driver) {
            defer wg.Done()
            art, err := drv.Build(ctx, src)
            if err == nil { err = drv.Validate(art) }
            if err == nil { err = drv.Upload(ctx, art, src.Creds[drv.Name()]) }
            if err != nil { errs <- fmt.Errorf("%s: %w", drv.Name(), err) }
        }(drv)
    }
    wg.Wait()
    close(errs)
    return collectErrors(errs, strict)
}
```

`strict=true` cancels the `ctx` on first error, propagating cancellation to all in-flight drivers.

## Sub-phase 14.1 — npm + JSR target

Driver: MEP-52. The compiler emits TypeScript via the `typescript` target; the packaging layer wraps in a `package.json`:

```json
{
  "name":    "@mochilang/strings",
  "version": "0.4.7",
  "description": "...",
  "type":    "module",
  "main":    "dist/index.js",
  "types":   "dist/index.d.ts",
  "license": "MIT",
  "repository": {"type": "git", "url": "https://github.com/mochilang/strings"},
  "keywords": ["mochi"],
  "engines": {"node": ">=20"},
  "exports": {
    ".": {"import": "./dist/index.js", "types": "./dist/index.d.ts"}
  },
  "files": ["dist", "README.md", "LICENSE"]
}
```

JSR variant: writes `jsr.json` in addition (research note 11 §4.1). Submodule structure mirrors Mochi modules.

Provenance: the npm `provenance` field is populated from the in-toto Statement (Phase 13.2); npm registry verifies against Sigstore upstream (npm trusted publishing is GA as of 2024).

Capability mapping: `.caps.json` is emitted as `dist/mochi.caps.json` and consumed by a thin `mochi-runtime-node` shim that wires Deno `--allow-*` analogues into the Node runtime.

## Sub-phase 14.2 — PyPI target

Driver: MEP-51. Emits Python source via the `python` target, packages as a `pyproject.toml`-based sdist + wheel:

```toml
[project]
name        = "mochi-strings"
version     = "0.4.7"
description = "..."
readme      = "README.md"
license     = {text = "MIT"}
requires-python = ">=3.12"
authors     = [{name = "...", email = "..."}]

[build-system]
requires    = ["hatchling"]
build-backend = "hatchling.build"

[tool.hatch.build]
include = ["mochi_strings/**"]
```

Wheel build: pure-Python wheel (`tag=py3-none-any`); native deps deferred to v1.1.

PyPI trusted publishing: OIDC token from GitHub Actions (Phase 13.0) plus PyPI `trusted-publishers` config; no API token needed.

Capability mapping: Python runtime shim (`mochi-runtime-python` package) reads `mochi.caps.json` and wires syscall denial via `seccomp` (Linux) or `sandbox-exec` (macOS).

## Sub-phase 14.3 — Maven Central target

Driver: MEP-47. Emits JVM bytecode via the `jvm` target, packages as a Maven artefact:

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>org.mochilang</groupId>
  <artifactId>strings</artifactId>
  <version>0.4.7</version>
  <packaging>jar</packaging>
  <name>strings</name>
  <description>...</description>
  <licenses>
    <license><name>MIT</name><url>https://opensource.org/licenses/MIT</url></license>
  </licenses>
  <scm><url>https://github.com/mochilang/strings</url></scm>
  <developers><developer><id>tamnd</id></developer></developers>
  <dependencies>
    <dependency>
      <groupId>org.mochilang</groupId>
      <artifactId>runtime</artifactId>
      <version>0.7.0</version>
    </dependency>
  </dependencies>
</project>
```

Sonatype Central Portal API used for upload (no more OSSRH for new namespaces, post-2024). Verification: GPG signatures (still required by Maven Central) generated from a Sigstore-pinned identity; research note 09 §11 explains the dual-signing approach.

Capability mapping: emitted as Java `RuntimePermission` declarations in the jar manifest; consumed by a Mochi `SecurityManager` shim.

## Sub-phase 14.4 — NuGet target

Driver: MEP-48. Emits .NET MSIL via the `dotnet` target, packages as `.nupkg`:

```xml
<?xml version="1.0"?>
<package>
  <metadata>
    <id>Mochilang.Strings</id>
    <version>0.4.7</version>
    <authors>...</authors>
    <license type="expression">MIT</license>
    <projectUrl>https://github.com/mochilang/strings</projectUrl>
    <description>...</description>
    <readme>README.md</readme>
    <tags>mochi</tags>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="Mochilang.Runtime" version="0.7.0" />
      </group>
    </dependencies>
  </metadata>
</package>
```

NuGet trusted publishing landed in 2025 (research note 11 §4.4); OIDC-only upload supported.

Capability mapping: emitted as .NET `SecurityPermission` attributes via the runtime shim.

## Sub-phase 14.5 — Swift Package Index target

Driver: MEP-49. Emits Swift via the `swift` target. SPI does not have a central registry; publication is "tag a Git release and Index picks it up":

```swift
// Package.swift
// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "Strings",
    products: [.library(name: "Strings", targets: ["Strings"])],
    dependencies: [
        .package(url: "https://github.com/mochilang/runtime", from: "0.7.0"),
    ],
    targets: [
        .target(name: "Strings", dependencies: ["Runtime"]),
    ]
)
```

The driver writes `Package.swift` to the source tree and creates/updates a `swift/` branch. A `mochi pkg publish --target=swift` action pushes that branch and tags it.

Capability mapping: Swift's `Sandbox` (via Hardened Runtime entitlements) for macOS; emitted as `.entitlements` file in the package.

## Sub-phase 14.6 — Kotlin Multiplatform target

Driver: MEP-50. Emits Kotlin via the `kotlin` target, packages as a multiplatform Maven artefact:

```kotlin
// build.gradle.kts
plugins {
    kotlin("multiplatform") version "2.0.0"
    id("maven-publish")
    id("org.jetbrains.dokka") version "1.9.0"
}

kotlin {
    jvm()
    js(IR) { browser() }
    iosX64(); iosArm64(); iosSimulatorArm64()
    linuxX64()

    sourceSets {
        commonMain.dependencies { implementation("org.mochilang:runtime:0.7.0") }
    }
}
```

Same Sonatype Central Portal as Maven; the multiplatform aspect means N×M artefacts (compiler target × Kotlin platform).

## Sub-phase 14.7 — crates.io target

Driver: MEP-53. Emits Rust source via the `rust` target, packages as a `Cargo.toml` crate:

```toml
[package]
name        = "mochilang-strings"
version     = "0.4.7"
edition     = "2021"
description = "..."
license     = "MIT"
repository  = "https://github.com/mochilang/strings"
readme      = "README.md"

[dependencies]
mochilang-runtime = "0.7.0"
```

crates.io trusted publishing: OIDC GA in 2025 (research note 11 §4.7). Upload via `cargo publish` analogue.

Capability mapping: not enforced by the Rust ecosystem; emitted as a `[mochi.capabilities]` table in `Cargo.toml.metadata` for tooling consumption.

## Sub-phase 14.8 — Hex target

Driver: MEP-46. Emits BEAM bytecode via the `beam` target, packages as a Hex tarball:

```elixir
# mix.exs
defmodule Strings.MixProject do
  use Mix.Project

  def project do
    [
      app: :mochilang_strings,
      version: "0.4.7",
      description: "...",
      package: package(),
      deps: [{:mochilang_runtime, "~> 0.7"}],
    ]
  end

  defp package, do: [
    name: "mochilang_strings",
    licenses: ["MIT"],
    links: %{"GitHub" => "https://github.com/mochilang/strings"},
  ]
end
```

Hex.pm OIDC publishing landed in 2024 (research note 11 §4.8).

## Sub-phase 14.9 — Field mapping

| Mochi `[package]` | npm `package.json` | PyPI `pyproject.toml` | Maven `pom.xml` | NuGet `.nuspec` | crates.io `Cargo.toml` | Hex `mix.exs` |
|-------------------|--------------------|----------------------|------------------|------------------|------------------------|---------------|
| `name` (`@scope/name`) | `name` (`@scope/name`) | `name` (`mochi_name`) | `groupId` + `artifactId` | `id` (`Scope.Name`) | `name` (`mochilang-name`) | `app:` (`name`) |
| `version` | `version` | `version` | `version` | `version` | `version` | `version` |
| `description` | `description` | `description` | `<description>` | `description` | `description` | `description` |
| `license` | `license` (SPDX) | `license = {text=...}` | `<licenses>` | `<license expression="SPDX">` | `license` | `licenses: ["..."]` |
| `repository` | `repository.url` | `urls.Repository` | `<scm><url>` | `<repository>` | `repository` | `links: %{...}` |
| `homepage` | `homepage` | `urls.Homepage` | `<url>` | `projectUrl` | `homepage` | (links map) |
| `keywords` | `keywords` | `keywords` | (none direct) | `<tags>` | `keywords` | (none direct) |
| `authors` | `author` | `authors` | `<developers>` | `<authors>` | `authors` | `maintainers` |
| `readme` | `readme` | `readme` | (auto-included) | `<readme>` | `readme` | `description` |
| `dependencies` | `dependencies` | `dependencies` | `<dependencies>` | `<dependencies>` | `[dependencies]` | `deps:` |

Naming-collision rules (research note 11 §3.2): for each ecosystem, the mapped name passes through a normaliser. `@scope/name` becomes `@scope/name` in npm/JSR (which support scopes), `scope-name` in PyPI/crates.io (no scopes; flatten), `Scope.Name` in NuGet (dot convention), `org.scope:name` in Maven (group:artifact).

## Sub-phase 14.10 — `--strict` and rollback

```go
func (d *Dispatcher) RunStrict(ctx context.Context, ...) error {
    childCtx, cancel := context.WithCancel(ctx)
    defer cancel()
    /* fail-fast variant: first error -> cancel; collect; report */
    /* per-target staging: build to tmp dir, only upload on full success */
}
```

Staging strategy: each driver builds to `$MOCHI_HOME/fanout/<target>/<version>/`
(canonical layout: [phase 0 §conventions](./phase-00-skeleton#files-changed)).
Upload only when all builds succeed. On strict failure, staging tree is removed;
on non-strict failure, kept for the user to inspect.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgfanout/dispatcher.go` | Driver dispatch | Owner |
| `pkg/pkgfanout/driver.go` | `Driver` interface | Owner |
| `pkg/pkgfanout/npm/*` | npm + JSR driver | Owner |
| `pkg/pkgfanout/pypi/*` | PyPI driver | Owner |
| `pkg/pkgfanout/maven/*` | Maven Central driver | Owner |
| `pkg/pkgfanout/nuget/*` | NuGet driver | Owner |
| `pkg/pkgfanout/spi/*` | Swift Package Index driver | Owner |
| `pkg/pkgfanout/kotlinmp/*` | Kotlin MP driver | Owner |
| `pkg/pkgfanout/cratesio/*` | crates.io driver | Owner |
| `pkg/pkgfanout/hex/*` | Hex driver | Owner |
| `pkg/pkgfanout/mapping/mapping.go` | Field mapping table | Owner |
| `cmd/mochi/publish.go` | `--target=<list>`, `--strict` flags | Extends (Phase 12) |
| `tests/pkgsystem/fanout/<target>/dry-run/*` | Per-target golden artefacts | Owner |
| `tests/pkgsystem/fanout/<target>/upload/*` | Mock registry round-trip | Owner |

## Error code surface

Phase 14 owns the `M057_FAN_*` codes listed in the [error registry](./errors).
`M057_TARGET_UNSUPPORTED` (the old CLI rejection code) is renamed `M057_FAN_E002`
so it no longer collides with the Phase 5 solver code of the same English name
(now `M057_SOLVER_E004`).

## Test set

- `TestPhase14Dispatcher` — parallel runs of mock drivers complete.
- `TestPhase14Strict` — first failure cancels rest.
- `TestPhase14NPM` — `package.json` validates.
- `TestPhase14PyPI` — `pyproject.toml` validates against PEP 621.
- `TestPhase14Maven` — `pom.xml` validates against Maven 4 XSD.
- `TestPhase14NuGet` — `.nuspec` validates.
- `TestPhase14SPI` — `Package.swift` parses with `swift package describe`.
- `TestPhase14KotlinMP` — `build.gradle.kts` parses.
- `TestPhase14Crates` — `Cargo.toml` parses.
- `TestPhase14Hex` — `mix.exs` parses.
- `TestPhase14FieldMapping` — every row of the mapping table tested.
- `TestPhase14Determinism` — twice-built target artefacts byte-identical.

## Open questions

- Whether targets that lack OIDC trusted publishing (Hex prior to mid-2024) should fall back to API tokens with a warning; current plan: yes, opt-in via `--allow-api-token`.
- Whether to emit Sigstore bundles for npm + PyPI provenance fields automatically; current plan: yes, both surfaces are GA.
- Whether to support a "private" target that uploads to a self-hosted Verdaccio / devpi instead of public registries; current plan: yes, configured via `[fanout.npm] registry = "https://...".`.

## Cross-references

- Fan-out details: [research note 11](/docs/research/0057/polyglot-fanout).
- Per-target rationale: [research note 02 §7](/docs/research/0057/design-philosophy).
- Capability mapping table: [phase 10 §10.8](./phase-10-capabilities#sub-phase-108--capsjson-sidecar-for-emitters).
- Dual-signing (Sigstore + GPG): [research note 09 §11](/docs/research/0057/trusted-publishing).
