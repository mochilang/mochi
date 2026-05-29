---
title: "Prior art: registries and package managers (2024-2026 survey)"
description: "Survey of modern (2024-2026) package management systems: Cargo, Go modules, npm, JSR, Bun, uv plus PEP 751 plus 723 plus 735, Bazel bzlmod, Nix flakes, SwiftPM, Hex, NuGet, Pixi, Spack, Pkl, with specific lessons MEP-57 inherits or rejects."
sidebar_position: 3
---

# 03. Prior art: registries and package managers (2024-2026 survey)

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/prior-art-registries`.

This is the survey companion to [02-design-philosophy](./02-design-philosophy). It walks each modern (2024-2026) package manager Mochi-57 borrows from or rejects, and ends each section with the one specific lesson MEP-57 inherits.

## 1. Cargo (Rust)

**Status as of 2026-Q2**: Cargo 1.85 (Feb 2026). Sparse HTTPS index GA since 1.68 (March 2023); PubGrub-based resolver RFC #3796 accepted Oct 2024, implementation rolling out across 1.83 to 1.86; Sigstore + OIDC trusted publishing RFC #3724 accepted Aug 2024, partial implementation in 1.84.

**Manifest**: `Cargo.toml`. TOML since 1.0 (May 2015). Schema covers `[package]`, `[dependencies]`, `[dev-dependencies]`, `[build-dependencies]`, `[features]`, `[target.'cfg(...)']`, `[workspace]`, `[patch]`, `[replace]`, `[profile]`. Workspace inheritance via `workspace = true`. Optional features via `[features]`. Path / git / registry / workspace source types.

**Lockfile**: `Cargo.lock`. TOML. Per-package entry has `name`, `version`, `source`, `checksum`, `dependencies`. Version envelope (`version = 3` as of 2024).

**Solver**: classic SAT-derived through 2024; PubGrub transition through 2025-2026 per RFC #3796.

**Registry**: crates.io (central). Sparse index at `https://index.crates.io/<two-char-prefix>/<two-char-prefix>/<name>`. Tarballs at `https://crates.io/api/v1/crates/<name>/<version>/download`. SHA-256 content hash recorded in lockfile.

**Publishing**: `cargo publish`. Currently long-lived tokens via `cargo login`; RFC #3724 adds trusted publishing via GitHub OIDC.

**Workspaces**: first-class. Members declared in `[workspace]`. Per-member `Cargo.toml` can inherit dependencies, package metadata, lints via `workspace = true`.

**Lesson for MEP-57**: the *sparse-index protocol* (Phase 8) is direct prior art; we follow Cargo's URL scheme almost verbatim, just under `index.mochi.dev`. The *workspace inheritance pattern* (`workspace = true`) is direct prior art for MEP-57's `[workspace.dependencies]` block.

## 2. Go modules

**Status as of 2026-Q2**: Go 1.24 (Feb 2026). Modules GA since Go 1.14 (2020). MVS solver unchanged.

**Manifest**: `go.mod`. Custom Go-DSL (not TOML, not JSON). Schema: `module`, `go`, `require`, `replace`, `exclude`, `retract`.

**Lockfile**: `go.sum`. Plain-text per-line entries: `<module> <version>/<go.mod or directory> <hash>`.

**Solver**: Minimum Version Selection (MVS). Deterministic. Picks the *lowest* version satisfying each constraint. Relies on the "v1 forever" semver-major rule (module path includes `/v2`, `/v3` etc).

**Registry**: federated. The module path is a URL (e.g. `github.com/foo/bar`). Module proxies (`proxy.golang.org` by default) cache and serve content-addressed archives. Module checksum DB (`sum.golang.org`) is a transparency log of `(module, version, hash)` tuples. Independent of the proxy, the checksum DB verifies that everyone sees the same bytes.

**Publishing**: there is no `go publish`. A module is "published" by tagging a git commit and pushing it. The proxy fetches on first request.

**Workspaces**: Go workspaces via `go.work` (Go 1.18, 2022). Workspace mode overrides individual module replacements.

**Lessons for MEP-57**:
- The *transparency log* (`sum.golang.org`) is the v2 candidate (MEP-57 §Open questions). v1 does without it.
- *MVS is rejected* because Mochi spans nine ecosystems with incompatible semver semantics ([02-design-philosophy](./02-design-philosophy) §2).
- The *module proxy* model (intermediate cache that fronts the upstream) is referenced for [07-registry-index](./07-registry-index) §6 mirror discussion.
- The *no-publish* model (publish-by-tag) is rejected for Mochi: Mochi packages need Sigstore bundles, which require an explicit publish event.

## 3. npm (JavaScript / TypeScript)

**Status as of 2026-Q2**: npm 11 (2026). Trusted Publishing GA April 2024 (npm provenance). `package-lock.json` v3 (since npm 7).

**Manifest**: `package.json`. JSON (no comments officially; many users use `//`-prefixed faux-comment keys which npm tolerates but warns on). Schema enormous: `name`, `version`, `dependencies`, `devDependencies`, `peerDependencies`, `optionalDependencies`, `bundleDependencies`, `engines`, `os`, `cpu`, `exports`, `imports`, `bin`, `scripts`, `workspaces`, ... npm 8 deprecated several legacy fields; npm 11 has cleaned up most.

**Lockfile**: `package-lock.json`. JSON. `packages` map keyed by install path. Each entry: `version`, `resolved` (tarball URL), `integrity` (SRI hash), `dependencies`.

**Solver**: SAT-based historically. Through 2025 npm's resolver remains SAT-derived; npm RFC 754 (2025) proposes PubGrub for explanation quality.

**Registry**: registry.npmjs.org (central). Per-package metadata at `registry.npmjs.org/<scope>/<pkg>` (sparse-style since 2010s). Tarballs at `registry.npmjs.org/<scope>/<pkg>/-/<pkg>-<version>.tgz`.

**Publishing**: `npm publish`. Trusted Publishing via GitHub OIDC GA April 2024 (npm provenance). Long-lived tokens still supported but deprecated for high-trust paths.

**Workspaces**: `workspaces` field in `package.json` (since npm 7). Members are referenced by glob. Cross-member symlinking via `node_modules/<name>` redirects.

**Lessons for MEP-57**:
- The *Trusted Publishing model* (npm provenance, April 2024 GA) is direct prior art for [09-trusted-publishing](./09-trusted-publishing).
- The *scoped package namespace* (`@org/pkg`) is borrowed verbatim for MEP-57's `[a-zA-Z]` scope.
- The *peer dependency model* is rejected: it solves a problem (singleton libraries like React) that Mochi does not have because Mochi's solver enforces single-version-per-name by default. Peer deps add resolver complexity for limited benefit at Mochi's stage.
- The *JSON manifest* is rejected (see [02-design-philosophy](./02-design-philosophy) §1).
- The *SAT-based solver* is rejected (same reference).

## 4. JSR (Deno team, 2024)

**Status as of 2026-Q2**: JSR GA March 2024. Source-only registry for TypeScript / JavaScript. Free, open-source, run by the Deno team.

**Manifest**: `jsr.json` or `deno.json`. JSON / JSONC.

**Lockfile**: `deno.lock` (when used from Deno) or `package-lock.json` (when used from Node via `npm install` of a JSR mirror).

**Solver**: derived from Deno's resolver; sparse HTTPS index.

**Registry**: `jsr.io` (central). Sparse index. Tarballs are source TypeScript, not compiled JS. Consumers compile themselves (or via npm mirror at `npm:@jsr/...`).

**Publishing**: `deno publish` or `jsr publish`. Trusted publishing via GitHub OIDC. Mandatory: no long-lived tokens supported.

**Provenance**: every published package gets a Sigstore bundle by default (since 2024).

**Lessons for MEP-57**:
- *Source-only publishing* is direct prior art: Mochi's central registry ships Mochi source plus the lockfile, not lowered artifacts. The polyglot fan-out (Phase 14) handles target-specific artifacts in target-specific registries.
- *Mandatory Sigstore on every publish* is the model we follow.
- *Lowering at install time* on the consumer side matches the Mochi compile-at-consume model. Lockfile reproducibility makes this safe.

## 5. Bun (JavaScript / TypeScript, alt runtime)

**Status as of 2026-Q2**: Bun 1.5 (March 2026). `bun install` is the package manager.

**Manifest**: `package.json` (npm-compatible).

**Lockfile**: `bun.lock` (text TOML-shaped) since 2025; previously `bun.lockb` (binary) which Bun reversed under community pressure.

**Solver**: npm-compatible (SAT-derived).

**Registry**: registry.npmjs.org plus optional Bun-specific endpoints for bun-native packages.

**Publishing**: `bun publish` (npm-compatible).

**Workspaces**: `workspaces` field in `package.json`.

**Lessons for MEP-57**:
- The *bun.lockb reversal* is the canonical lesson: do not ship a binary lockfile. ([06-lockfile-format](./06-lockfile-format) §1 and [02-design-philosophy](./02-design-philosophy) §3.)
- Bun's lockfile format (`bun.lock` is text TOML, version envelope) is one of the templates we follow.

## 6. uv (Python, Astral 2024)

**Status as of 2026-Q2**: uv 0.7.x (May 2026). Rust-implemented Python package manager. Replaces pip + pip-tools + venv + pyenv. PubGrub-based resolver.

**Manifest**: `pyproject.toml` (PEP 621 standardised). TOML. Schema: `[project]` (name, version, dependencies, ...), `[project.optional-dependencies]`, `[build-system]`, `[tool.uv]` (uv-specific), `[tool.uv.workspace]`.

**Lockfile**: `uv.lock` (text TOML). Per-platform sections. Version envelope.

**Solver**: PubGrub. Universal lockfile (one lockfile resolves all platforms).

**Registry**: PyPI (federated; multiple indexes supported, including private). Sparse-style HTTPS at `pypi.org/simple/<pkg>/`. Wheels at `files.pythonhosted.org`.

**Publishing**: `uv publish`. Trusted publishing via PEP 740 (Sigstore + OIDC, GA Nov 2024).

**Workspaces**: `[tool.uv.workspace]` declares members. Workspace dependencies via `tool.uv.workspace = true`.

**Inline script dependencies**: PEP 723 (`# /// script` block at the top of a `.py` file declaring deps for `uv run`).

**PEP 751 lockfile format**: accepted Nov 2024. Cross-tool Python lockfile. uv and pip both target compatibility through 2026.

**Lessons for MEP-57**:
- *Universal lockfile with per-platform sections* is direct prior art for MEP-57's lockfile shape ([06-lockfile-format](./06-lockfile-format) §3).
- *PubGrub solver* with conflict explanations matches MEP-57 §2 ([05-solver-design](./05-solver-design)).
- *PEP 751* is the canonical reference for the text lockfile envelope.
- *Workspace pattern* (members declared in root, members reference via `workspace = true`) is borrowed.
- *Inline script dependencies* (PEP 723) are deferred to a future Mochi MEP; v1 requires a manifest in a directory.

## 7. Bazel (bzlmod, GA April 2024)

**Status as of 2026-Q2**: Bazel 8 (Jan 2026). bzlmod (Bazel modules) GA since Bazel 7 (April 2024); legacy WORKSPACE files removed in Bazel 8.

**Manifest**: `MODULE.bazel`. Starlark with TOML-shaped key=value at the top. Declares `module(name, version, ...)`, `bazel_dep(name, version, ...)`.

**Lockfile**: `MODULE.bazel.lock`. JSON.

**Solver**: MVS (Minimum Version Selection). Same as Go, by design.

**Registry**: Bazel Central Registry (BCR, registry.bazel.build) plus federated mirrors. Sparse JSON metadata.

**Publishing**: PR against `bazelbuild/bazel-central-registry`. No automated publish flow. Future Sigstore integration tracked but not yet GA.

**Workspaces**: Bazel itself is a build system, so the workspace concept is the build graph.

**Lessons for MEP-57**:
- *MVS in Bazel context* works because Bazel's ecosystem is mostly closed-set rules and tools where strict semver discipline is enforced socially. Mochi's polyglot fan-out cannot rely on this.
- *PR-against-central-registry* is a low-tech publish model. Mochi rejects it because Sigstore + OIDC must be enforced at registry-write time.

## 8. Nix flakes

**Status as of 2026-Q2**: Nix 2.24 (April 2026). Flakes still experimental but de facto stable; flake-based ecosystem mature.

**Manifest**: `flake.nix`. Nix-DSL.

**Lockfile**: `flake.lock`. JSON. Per-input lock with revision hash.

**Solver**: there is no "solver" in the Cargo sense; flake inputs are exact pins, and `nix flake update` rewrites them.

**Registry**: federated. Inputs are git URLs, tarballs, or registries.

**Publishing**: no formal publish; users push to a git repo and consumers reference it.

**Lessons for MEP-57**:
- *Content-addressed store* (`/nix/store/<hash>-<name>`) is direct prior art for [08-content-addressed-store](./08-content-addressed-store).
- The *exact-pin model* is more rigid than Mochi wants; we keep semver ranges for ergonomics.
- *Flake-DSL manifest* is rejected (Nix-DSL is hard to parse outside Nix; see [02-design-philosophy](./02-design-philosophy) §1).

## 9. SwiftPM

**Status as of 2026-Q2**: Swift 6.1 (March 2026). Swift Package Manager unchanged at core; macros and async surface are evolving.

**Manifest**: `Package.swift`. Swift-DSL.

**Lockfile**: `Package.resolved`. JSON. Per-package version pin with revision.

**Solver**: SAT-derived. Resolves git tags directly.

**Registry**: federated. SwiftPM resolves directly against git repos by default. Swift Package Index (`swiftpackageindex.com`) is a discovery layer, not a registry; the source of truth is the git tag.

**Publishing**: tag a git commit. The Swift Package Registry (formal protocol since Swift 5.5, 2021) adds an optional centralised flow but most packages still publish by git tag.

**Lessons for MEP-57**:
- *Git-tag publishing* is the path for the Swift target in MEP-57's fan-out: the Swift artifact is a tagged git commit at a Mochi-owned source repo.
- *Swift-DSL manifest* is rejected for Mochi's central manifest (same chicken-and-egg reason as Nix), but for Swift target output we generate `Package.swift` because that's what consumers expect.

## 10. Hex (Elixir)

**Status as of 2026-Q2**: Hex 2.2 (2026). Mature centralised package manager for the BEAM ecosystem.

**Manifest**: `mix.exs`. Elixir-DSL (functions defining the project).

**Lockfile**: `mix.lock`. Erlang term file.

**Solver**: SAT-derived (`Hex.Solver`). Backtracking with conflict reporting.

**Registry**: hex.pm (central). HTTP-based with tarball downloads.

**Publishing**: `mix hex.publish`. Long-lived tokens; trusted publishing on roadmap.

**Hex Diff**: Hex provides per-package `mix hex.outdated` and per-version `mix hex.audit` flows that surface advisory data.

**Lessons for MEP-57**:
- *Hex Diff* (showing what a version bump changes) inspires MEP-57's `mochi audit signatures` and `mochi why` commands.
- The *Elixir-DSL manifest* is rejected for Mochi but accepted for BEAM target output.

## 11. NuGet (.NET)

**Status as of 2026-Q2**: NuGet 6.12 (2026). Centralised .NET package registry.

**Manifest**: `<project>.csproj` (XML).

**Lockfile**: `packages.lock.json` (JSON), optional.

**Solver**: SAT-derived.

**Registry**: nuget.org (central) plus private NuGet servers.

**Publishing**: `dotnet nuget push`. API key. NuGet package signing supported (X.509 chain) since 2018; Sigstore integration on roadmap.

**Lessons for MEP-57**:
- *XML manifest* is rejected (no comments-cleanness gain over TOML, plus XML's verbosity).
- *X.509 signing* is the existing surface MEP-57 layers Sigstore over for the NuGet target.

## 12. Pixi (prefix.dev, 2024)

**Status as of 2026-Q2**: Pixi 0.30+. Conda-compatible package manager built on Rattler. TOML manifest.

**Manifest**: `pixi.toml`. TOML.

**Lockfile**: `pixi.lock`. YAML. (Note: pixi is one of the few text lockfiles in YAML; the project has discussed migration to TOML.)

**Solver**: SAT (rattler-solve via libsolv).

**Registry**: conda channels (conda-forge, defaults, private). Federated.

**Publishing**: not a primary use case; conda-forge is the publish path.

**Workspaces**: `[feature]` and `[environments]` blocks function as workspaces.

**Lessons for MEP-57**:
- *TOML manifest in a 2024-vintage tool* validates the format choice.
- *YAML lockfile* is rejected (Pixi's own discussion echoes Norway-problem concerns).

## 13. Spack (HPC)

**Status as of 2026-Q2**: Spack 0.22.x (2026). HPC-focused package manager. ASP / Clingo solver.

**Manifest**: `spack.yaml` (environment) plus per-package `package.py` (Python).

**Lockfile**: `spack.lock`. JSON.

**Solver**: ASP / Clingo. Handles concretisation of complex dependency graphs with hardware-aware constraints (compilers, MPI flavours).

**Registry**: federated builds; binaries via mirrors.

**Lessons for MEP-57**:
- *ASP / Clingo* is rejected for Mochi's solver due to runtime weight ([02-design-philosophy](./02-design-philosophy) §2).
- *Hardware-aware constraints* (architecture, compiler) inspire MEP-57's per-platform lockfile sections (a much narrower analogue).

## 14. Pkl (Apple, 2024)

**Status as of 2026-Q2**: Pkl 0.27 (2026). Configuration language with first-class package management.

**Manifest**: `PklProject`. Pkl-DSL.

**Lockfile**: `PklProject.deps.json`.

**Solver**: PubGrub-derived (per Apple's 2024 blog post).

**Registry**: federated. Pkl supports both centralised and direct-import-from-URL.

**Publishing**: tag a git commit; Pkl resolves via HTTPS.

**Lessons for MEP-57**:
- *PubGrub adoption* in a 2024-vintage system from a major vendor validates the algorithmic choice.
- *Pkl-DSL manifest* is acceptable in Pkl's context (Pkl is itself a config language) but inappropriate for Mochi.

## 15. Cross-system summary table

| System          | Manifest               | Lockfile             | Solver       | Registry              | Publish auth          |
|-----------------|------------------------|----------------------|--------------|-----------------------|-----------------------|
| Cargo           | Cargo.toml (TOML)      | Cargo.lock (TOML v3) | SAT→PubGrub  | crates.io (sparse)    | Token→Sigstore (RFC 3724) |
| Go              | go.mod (Go-DSL)        | go.sum (text)        | MVS          | Federated + proxy     | Tag git commit        |
| npm             | package.json (JSON)    | package-lock.json    | SAT          | npmjs.org             | Token + Trusted Pub.  |
| JSR             | jsr.json / deno.json   | deno.lock            | PubGrub-derived | jsr.io (sparse)    | Trusted Publishing    |
| Bun             | package.json (JSON)    | bun.lock (TOML)      | SAT          | npmjs.org             | Token                 |
| uv              | pyproject.toml (TOML)  | uv.lock (TOML)       | PubGrub      | PyPI (federated)      | PEP 740 (Sigstore)    |
| Bazel bzlmod    | MODULE.bazel (Starlark) | MODULE.bazel.lock (JSON) | MVS    | BCR + mirrors         | PR to central         |
| Nix flakes      | flake.nix (Nix-DSL)    | flake.lock (JSON)    | exact-pin    | git URLs              | git push              |
| SwiftPM         | Package.swift (Swift)  | Package.resolved     | SAT          | git tag + index       | git tag               |
| Hex             | mix.exs (Elixir)       | mix.lock (Erlang)    | SAT          | hex.pm                | Token                 |
| NuGet           | *.csproj (XML)         | packages.lock.json   | SAT          | nuget.org             | API key + X.509       |
| Pixi            | pixi.toml (TOML)       | pixi.lock (YAML)     | SAT (libsolv) | conda channels       | n/a (conda-forge)     |
| Spack           | spack.yaml + package.py | spack.lock (JSON)   | ASP/Clingo   | federated mirrors     | n/a                   |
| Pkl             | PklProject (Pkl)       | PklProject.deps.json | PubGrub      | git URLs / central    | git tag               |
| **MEP-57**      | **mochi.toml (TOML)**  | **mochi.lock (TOML)** | **PubGrub** | **index.mochi.dev (sparse) + content-addressed** | **Sigstore + OIDC only** |

## 16. Specific decisions inherited

- TOML manifest: from Cargo, uv, Pixi, Pkl. ([04-manifest-format](./04-manifest-format))
- Sparse HTTPS index: from Cargo 1.68 GA, JSR. ([07-registry-index](./07-registry-index))
- Universal lockfile with per-platform sections: from uv. ([06-lockfile-format](./06-lockfile-format))
- PubGrub solver: from Dart, uv, Cargo RFC 3796, Pkl. ([05-solver-design](./05-solver-design))
- Trusted Publishing via Sigstore + OIDC: from npm (April 2024), Maven Central (Oct 2024), PEP 740 (Nov 2024), Cargo RFC 3724. ([09-trusted-publishing](./09-trusted-publishing))
- Content-addressed store: from Nix, Cargo `.crate` cache, git. ([08-content-addressed-store](./08-content-addressed-store))
- Workspace inheritance: from Cargo `workspace = true`, uv. ([01-language-surface](./01-language-surface) §4)

## 17. Specific decisions rejected

- JSON manifest (npm): no comments, no trailing commas, bad for human editing.
- YAML manifest / lockfile (Pixi lockfile is the unique YAML holdout): Norway-problem, whitespace traps.
- Nix-DSL / Pkl-DSL / Starlark / Elixir-DSL manifest: chicken-and-egg with the compiler we're configuring.
- MVS (Go, Bazel): does not cross polyglot semver semantics.
- SAT-based (Cargo legacy, npm, Bun, Hex, NuGet, Spack via SAT): weak explanations.
- ASP / Clingo (Spack): too heavy for a CLI single-binary.
- Git-tag publishing (Go, SwiftPM, Nix): no Sigstore enforcement window.
- Long-lived API tokens (Cargo legacy, Hex, NuGet, Bun): every major supply-chain incident traces to one.
- Binary lockfile (Bun's reversed `bun.lockb`): not diffable.

## 18. Cross-references

- The "why" for each inheritance: [02-design-philosophy](./02-design-philosophy).
- Manifest schema details: [04-manifest-format](./04-manifest-format).
- Solver implementation: [05-solver-design](./05-solver-design).
- Lockfile format: [06-lockfile-format](./06-lockfile-format).
- Sparse index protocol: [07-registry-index](./07-registry-index).
- Content-addressed store: [08-content-addressed-store](./08-content-addressed-store).
- Trusted publishing: [09-trusted-publishing](./09-trusted-publishing).
- Capability model: [10-capability-model](./10-capability-model).
- Polyglot fan-out: [11-polyglot-fanout](./11-polyglot-fanout).
- Rejected alternatives: [12-risks-and-alternatives](./12-risks-and-alternatives).
