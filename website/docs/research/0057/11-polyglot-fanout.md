---
title: "Polyglot fan-out: one source, eight ecosystem artifacts"
description: "Fan-out pipeline, per-target packaging for mochi central, npm plus JSR, PyPI, Maven Central, NuGet, Swift Package Index, Kotlin Multiplatform, crates.io, Hex, field mapping, per-target overrides, capability fan-out, version coupling, workflow integration."
sidebar_position: 11
---

# 11. Polyglot fan-out: one source, eight ecosystem artifacts

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/polyglot-fanout`.

This note specifies the polyglot publishing pipeline: how one `mochi.toml` becomes per-ecosystem artifacts. The fan-out rationale is in [02-design-philosophy](./02-design-philosophy) §7; per-ecosystem prior art is in [03-prior-art-registries](./03-prior-art-registries).

## 1. The eight target ecosystems

| Target name   | Downstream registry             | Artifact form                       | Driver MEP |
|---------------|----------------------------------|-------------------------------------|------------|
| `mochi`       | index.mochi.dev                 | `.mochi.tar.zst` + Sigstore bundle  | MEP-57     |
| `typescript`  | registry.npmjs.org + jsr.io     | npm package (CJS + ESM) + JSR scope | MEP-52     |
| `python`      | pypi.org                        | wheel + sdist (PEP 517 / 660)       | MEP-51     |
| `jvm`         | repo.maven.apache.org           | jar + pom + sources jar             | MEP-47     |
| `dotnet`      | nuget.org                       | nupkg                                | MEP-48     |
| `swift`       | swiftpackageindex.com (git tag) | Swift Package + Package.swift       | MEP-49     |
| `kotlin`      | Maven Central (KMP) + Play Console | jar + KMP klib + Android AAB       | MEP-50     |
| `rust`        | crates.io                       | crate + Cargo.toml                  | MEP-53     |
| `beam`        | hex.pm                          | OTP application package             | MEP-46     |

Nine targets in total (eight downstream plus mochi central). The mochi-central artifact is the source of truth; the others are lowered for ecosystem consumers.

A package's `[targets]` block opts in per target ([04-manifest-format](./04-manifest-format) §5).

## 2. The fan-out pipeline

```
              ┌────────────────────┐
              │   mochi.toml       │
              │   src/...          │
              │   mochi.lock       │
              └─────────┬──────────┘
                        │
                        ▼
              ┌────────────────────┐
              │  mochi publish     │
              │    --target=*      │
              └─────────┬──────────┘
                        │
            ┌───────────┼───────────┬──────────┬─────────┐
            ▼           ▼           ▼          ▼         ▼
       central       npm        PyPI       Maven      others...
         │            │           │           │
         ▼            ▼           ▼           ▼
   Sigstore     npm provenance  PEP 740  Sigstore (Oct 2024)
```

`mochi publish` without `--target` defaults to publishing the mochi-central artifact only. `mochi publish --target=npm` publishes to npm and JSR. `mochi publish --target=all` publishes to every target the package opts into.

Per target, the publish step:

1. Lowers the Mochi source through the target's transpiler (MEP-45 to MEP-53).
2. Constructs the target's artifact form (npm pkg, wheel, jar, ...).
3. Maps `[package]` metadata to the target's metadata format ([04-manifest-format](./04-manifest-format) §12).
4. Signs with Sigstore + OIDC where the target supports it.
5. Pushes to the target registry.

## 3. Per-target packaging

### 3.1 mochi central

Produces `.mochi.tar.zst` + Sigstore bundle. The canonical case ([08-content-addressed-store](./08-content-addressed-store) §3-4).

### 3.2 typescript (npm + JSR)

The MEP-52 transpiler lowers Mochi to TypeScript. The publish step:

1. Run `mochi build --target=typescript` to emit `dist/`.
2. Generate `package.json` from `mochi.toml`:
   - `name`: from `[package].name`, with scope handling (`@scope/name` works unchanged).
   - `version`: from `[package].version` (semver maps directly).
   - `exports`: per Node / Deno / Bun / browser conditional exports map.
   - `dependencies`: each Mochi dep that publishes to npm is referenced by its npm name.
   - `peerDependencies`: for runtime-injected deps like the Mochi runtime.
   - `engines.node`: derived from the Mochi compiler version pin.
3. Run `tsc --emitDeclarationOnly` to emit `.d.ts` files.
4. `npm publish --provenance` with GitHub OIDC trusted publishing (GA April 2024).
5. `deno publish` to JSR (parallel; same TypeScript source, different distribution).

### 3.3 python (PyPI)

The MEP-51 transpiler lowers to Python. The publish step:

1. Run `mochi build --target=python` to emit `python/`.
2. Generate `pyproject.toml` from `mochi.toml` ([04-manifest-format](./04-manifest-format) §12):
   - `[project]` block populated from `[package]`.
   - `dependencies`: each Mochi dep that publishes to PyPI is referenced.
   - `requires-python`: from the Mochi runtime support matrix.
3. Run `hatch build` (or equivalent PEP 517 frontend) to produce wheel + sdist.
4. `uv publish --trusted-publisher` (PEP 740, GA Nov 2024).

### 3.4 jvm (Maven Central)

The MEP-47 transpiler lowers to JVM bytecode directly. The publish step:

1. Run `mochi build --target=jvm` to emit `jvm-classes/`.
2. Generate `pom.xml` from `mochi.toml`:
   - `groupId` from `[package].name` scope (e.g. `@mochi/strings` → `dev.mochi.strings`).
   - `artifactId` from the name segment.
   - `version` from `[package].version`.
   - `dependencies`: each Mochi dep that publishes to Maven Central is referenced.
3. Build jar (classes + resources), sources jar, javadoc jar (stubbed).
4. Sign with Sigstore (Maven Central Sigstore GA Oct 2024) plus PGP fallback for older mirrors.
5. Push to OSSRH staging; auto-release via the central UI.

### 3.5 dotnet (NuGet)

The MEP-48 transpiler lowers to MSIL. The publish step:

1. Run `mochi build --target=dotnet`.
2. Generate `<package>.nuspec` from `mochi.toml`.
3. Pack as `.nupkg`.
4. Sign with NuGet's X.509 chain (Sigstore integration is on NuGet's roadmap; v1 uses X.509).
5. `dotnet nuget push`.

### 3.6 swift (Swift Package Index)

The MEP-49 transpiler lowers to Swift. The publish step:

1. Run `mochi build --target=swift` to emit `Sources/<name>/`.
2. Generate `Package.swift` from `mochi.toml`.
3. Commit and tag a git release.
4. Push the tag; SwiftPM and Swift Package Index pull from the git tag.
5. Sign with Apple notarisation (for binary outputs; source distribution is git tag + trust-on-first-use).

### 3.7 kotlin (Maven Central KMP / Play Console)

The MEP-50 transpiler lowers to Kotlin. The publish step diverges:

- **Library**: same as JVM (jar + pom to Maven Central) with KMP klib added.
- **App for Play**: bundles Android App Bundle (AAB) + signing key + uploads to Play Console.

### 3.8 rust (crates.io)

The MEP-53 transpiler lowers to Rust. The publish step:

1. Run `mochi build --target=rust` to emit `rust-src/`.
2. Generate `Cargo.toml` from `mochi.toml`.
3. `cargo package` to produce the crate.
4. Sign with Sigstore (Cargo RFC #3724, impl 2025).
5. `cargo publish`.

### 3.9 beam (hex.pm)

The MEP-46 transpiler lowers to Erlang. The publish step:

1. Run `mochi build --target=beam`.
2. Generate `mix.exs` from `mochi.toml`.
3. `mix hex.build` then `mix hex.publish`.

## 4. Field mapping

The full per-target field mapping (Mochi side → target side):

| Mochi `[package]` | npm `package.json`     | PyPI `[project]`     | Maven `pom.xml`            | NuGet `.nuspec`    |
|-------------------|------------------------|----------------------|----------------------------|--------------------|
| `name`            | `name`                 | `name`               | `groupId:artifactId`       | `id`               |
| `version`         | `version`              | `version`            | `version`                  | `version`          |
| `description`     | `description`          | `description`        | `description`              | `description`      |
| `license`         | `license`              | `license`            | `licenses[0].name`         | `license`          |
| `authors`         | `author`               | `authors`            | `developers`               | `authors`          |
| `repository`      | `repository.url`       | `urls.repository`    | `scm.url`                  | `repository`       |
| `homepage`        | `homepage`             | `urls.homepage`      | `url`                      | `projectUrl`       |
| `keywords`        | `keywords`             | `keywords`           | (no equivalent)            | `tags`             |
| `readme`          | `readme`               | `readme`             | (embed in jar)             | `readme`           |

Per-target dependency lookup:

- The Mochi dep `@mochi/strings` publishes to npm as `@mochi/strings` (scope preserved).
- The same dep publishes to PyPI as `mochi-strings` (PyPI does not allow `@`).
- The same dep publishes to Maven Central as `dev.mochi:strings`.
- The version pin in each downstream artifact references the corresponding downstream version (Mochi versions are 1:1 mapped per target).

Version mapping rules:

- Mochi semver `1.2.3` maps to npm `1.2.3`, PyPI `1.2.3`, Maven `1.2.3`, NuGet `1.2.3`, crates.io `1.2.3`.
- Pre-release `1.0.0-rc.1` maps to npm `1.0.0-rc.1`, PyPI `1.0.0rc1` (PEP 440 normalisation), Maven `1.0.0-rc.1`, crates.io `1.0.0-rc.1`.
- Build metadata `1.0.0+build.42` is dropped in most downstream registries (informational); preserved in mochi central.

## 5. Per-target overrides

A package can override source files per target ([04-manifest-format](./04-manifest-format) §5.1):

```toml
[targets.python.overrides]
"src/clock.mochi" = "src/clock_python.mochi"
```

At lowering time, the transpiler substitutes the override before lowering. The override must declare the same exports with compatible types. The compiler verifies.

Per-target FFI declarations:

```toml
[targets.python]
ffi = ["psycopg[binary]>=3.2"]

[targets.jvm]
ffi = ["org.postgresql:postgresql:42.7.4"]
```

The publish pipeline propagates these to the target's dependency list (PyPI requirements, Maven dependencies).

## 6. Capability fan-out

Each target enforces capabilities differently ([10-capability-model](./10-capability-model) §5):

- TypeScript / Deno: emits `deno.json` with permission block; Node/Bun documents.
- Python: emits `mochi_caps.json` sidecar consumed by the runtime shim.
- JVM: emits `META-INF/mochi-caps.json`; runtime check at agent attach.
- .NET: emits `mochi-caps.json` consumed by the .NET runtime shim.
- Swift: documented; Apple sandbox profile derived for app submissions.
- Kotlin: jvm path same as JVM; Android path emits Manifest permissions.
- Rust: emits `mochi_caps` static const consumed by the rust-runtime crate.
- BEAM: documented in mix; OTP application has no equivalent runtime check.

The publish step verifies the declared capability set is compatible with the target. E.g. if a target does not support `proc.spawn` in its runtime, the publish fails with `M057_CAP_E006`.

## 7. Version coupling across targets

A breaking change in the Mochi source is a major bump everywhere. A patch-level Mochi change is a patch bump in every downstream artifact. The version space is unified.

A target-specific change (an FFI fix in the Python override only) is still a patch bump in every target. The publish pipeline rebuilds and re-pushes every target on every release.

For partial publishes (`mochi publish --target=python` only): the version bumps in PyPI; the other registries lag. This is allowed but discouraged; it produces version skew that confuses consumers.

The recommended pattern is `mochi publish --target=all` from CI on every tag.

## 8. Workflow integration

A package opting in to all nine ecosystems has a CI workflow that:

1. Runs the test matrix (`mochi test --target=<each>`).
2. Runs the reproducibility gate (`mochi build --target=<each>` twice, byte-compare).
3. Runs Sigstore-required publish on every target that supports it.

Sample workflow (`.github/workflows/publish.yml`):

```yaml
name: Publish
on:
  push:
    tags: ['v*']

jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write
      contents: write
    steps:
      - uses: actions/checkout@v4
      - uses: mochilang/setup-mochi@v1
      - run: mochi test --target=mochi,typescript,python,jvm,dotnet,swift,kotlin,rust,beam
      - run: mochi publish --target=all
```

A single tag push fans out to every supported registry. Each fan-out leg has its own retry policy; partial failures are reported but do not block subsequent targets unless `--strict` is set.

## 9. Cross-ecosystem dep resolution

When a Mochi consumer's `[targets]` includes both `typescript` and `python`, and a Mochi dep declares both targets, the consumer's resolver:

1. Runs the solver against `[dependencies]` for the consumer's target set.
2. Per target, looks up the downstream package in the target's registry to verify availability.
3. Per target, records the downstream version in the lockfile's per-platform section ([06-lockfile-format](./06-lockfile-format) §3.2).

If the downstream registry is unreachable, the resolver uses the Mochi-central pin (since the downstream version is mechanically derived from the Mochi version).

## 10. Risks specific to fan-out

| Risk                                                | Mitigation                                                |
|-----------------------------------------------------|-----------------------------------------------------------|
| Target registry rate-limits batch publishes         | Exponential backoff + per-target retry budget             |
| Downstream registry rejects metadata               | Lint pre-flight (`mochi publish --dry-run --target=<x>`)  |
| Version skew when one target's publish fails       | `--strict` mode aborts at first failure                   |
| Per-target license incompatibility                 | License-mapping validation in pre-flight                  |
| FFI version conflict across targets                | Solver runs per target; conflicts reported per-target     |
| Maintainer error: forgot to opt in a target        | `mochi publish` shows opt-in summary before pushing       |
| Downstream namespace collision (e.g. `@mochi/strings` already exists on npm) | Registry-side collision check at first publish, blocking |

Mochi-central is the source-of-truth; if a downstream registry collision blocks publish, the maintainer renames in mochi-central too, keeping consistency.

## 11. Why fan out, not require consumers to install Mochi

The fundamental decision: do we ship Mochi source-only and require every consumer to install the Mochi toolchain, or do we lower to native artifacts?

Fan-out wins because:

- A Python service consuming a Mochi-authored utility should not have to install the Mochi toolchain.
- A Java app pulling a Mochi-authored library wants a normal Maven dep.
- A Rust crate consumer wants a normal `cargo add`.

The cost is the fan-out pipeline complexity (eight targets, eight signing flows, eight metadata mappings). The benefit is adoption: Mochi can be a producer for ecosystems Mochi is not (yet) the consumer for.

This is the Kotlin Multiplatform 2.x lesson learned the hard way ([03-prior-art-registries](./03-prior-art-registries) briefly; Kotlin's 2018-2022 attempts at single-source-multi-target failed because the lowered JS surface was non-idiomatic; KMP 2.x corrected by lowering to idiomatic per-target output).

## 12. Cross-references

- Mandatory polyglot motivation: [02-design-philosophy](./02-design-philosophy) §7.
- Per-ecosystem details: [03-prior-art-registries](./03-prior-art-registries).
- Manifest fields: [04-manifest-format](./04-manifest-format) §5 and §12.
- Lockfile per-platform: [06-lockfile-format](./06-lockfile-format) §3.2.
- Capability target enforcement: [10-capability-model](./10-capability-model) §5.
- Fan-out risks: [12-risks-and-alternatives](./12-risks-and-alternatives) §11.
