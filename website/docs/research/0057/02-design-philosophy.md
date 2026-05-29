---
title: "Design philosophy: the six load-bearing decisions"
description: "Why MEP-57 chose TOML manifest, PubGrub solver, text TOML lockfile, sparse HTTPS index plus content-addressed BLAKE3 plus SHA-256 store, Sigstore plus OIDC trusted publishing, and closed-set capability declarations at the package boundary, framed against 2024-2026 research."
sidebar_position: 2
---

# 02. Design philosophy: the six load-bearing decisions

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/design-philosophy`.

This note is the "why" companion to [01-language-surface](./01-language-surface). It explains the six load-bearing decisions of MEP-57, framed against 2024-2026 research and prior-art experience. Each section ends with a concrete failure mode the decision avoids, drawn from a real incident or paper.

## 1. Why TOML for the manifest

The manifest is the source of truth a human edits and a machine parses. The choice between TOML, JSON, YAML, and a Mochi-DSL is well-trodden but worth restating because the answer has shifted between 2018 and 2026.

**TOML wins for the human-edit surface.** The 2024-2026 evidence:

- Cargo has shipped TOML since 1.0 (2015); migration cost to anything else is unjustifiable, and the format has matured.
- uv (Astral, GA 2024) uses `pyproject.toml` as its primary manifest, dropping `setup.py` and `setup.cfg` together.
- Pixi (prefix.dev, 2024) is TOML.
- pkl-package (Apple, 2024) is Pkl-shaped but explicitly cites TOML as its inspiration.
- Bazel `MODULE.bazel` (bzlmod, GA April 2024) reads as TOML-shaped (Starlark with TOML-style key=value at the top level).

**JSON loses for the human-edit surface.** JSON has no comments, no trailing commas, no multi-line strings, and a hostile string-escaping syntax. `package.json` is the historical exception, but in 2025 npm's RFC 753 proposed `npm.toml` for editable manifests precisely because users keep adding comments to `package.json` despite the spec. Deno's `deno.json` keeps JSONC (a non-standard fork with comments) for the same reason.

**YAML loses on three counts.** First, whitespace sensitivity is a maintainability tax. Second, the type-coercion rules ("Norway problem": `NO` becomes `false`; `1.0` becomes a float; `1e3` becomes 1000 in YAML 1.1 vs string in YAML 1.2) silently mangle real data. Third, anchors and references in YAML are powerful and consequently hard to audit. The CNCF moved away from raw YAML for security-sensitive surfaces (k8s admission controllers in 2024 ship CUE / Cue Lang variants partly to escape YAML).

**A Mochi-DSL manifest loses for chicken-and-egg reasons.** The manifest must be parseable before the Mochi compiler runs, because the compiler reads it to learn which packages to fetch. A Mochi-DSL manifest would require shipping a separate parser in every host language Mochi supports (Go for the canonical compiler, JS for editor extensions, Python for CI integrations). TOML has battle-tested parsers in every language.

**Failure mode avoided**: the Bun `package.json` workspace inheritance bug (2024, fixed 2025) where the `"workspaces"` field could not carry comments explaining version pin rationale, leading to two separate StackOverflow questions per quarter from engineers re-discovering the same workaround. TOML allows the comment to live next to the pin.

The three Mochi-only sections are also worth justifying:

- `[capabilities]`: no other modern manifest declares capabilities at the package boundary in a closed-set form. Deno declares them at the CLI flag, not in the manifest. Pony declares them in the type system, not at the package boundary. We chose the manifest because it is the audit surface a consumer can read without invoking a compiler. See [10-capability-model](./10-capability-model).
- `[targets]`: matches Cargo's `[target.'cfg(...)']` and Swift's `Package.swift` target arrays. Necessary because Mochi compiles to nine ecosystems, each with its own FFI rules.
- `[provenance]`: holds the OIDC identity and Sigstore bundle reference. Other ecosystems put this in a sidecar file (npm `*-attestations.json`, PyPI `*.publish.attestation`); we put it in the manifest because it must travel with the published artifact. The provenance section is *populated by the publish pipeline*, not edited by hand.

## 2. Why PubGrub for the solver

The version solver is the algorithmic core of any modern package manager. Three families dominate as of 2026:

1. **MVS (Minimum Version Selection)**: Go modules' algorithm. Deterministic, fast, no SAT. Picks the *minimum* version satisfying each requirement, expecting the user to bump explicitly. Works because Go has a strong "v1 is forever" backward-compat culture.
2. **SAT-based**: npm (until 2024), Cargo's classic resolver, Composer, Conan. Encodes the dependency graph as a SAT formula and asks a SAT solver. Theoretically optimal; in practice produces opaque failures.
3. **PubGrub**: Dart 2018, uv 2024, Cargo RFC #3796 (2024, scheduled for 2025 GA), Rye, prefix-dev (Pixi). Conflict-driven backtracking with incompatibility derivation. Produces human-readable failure explanations.

**PubGrub wins for an explanation-first ecosystem.** The 2024-2026 papers and post-mortems make this unambiguous:

- Weizenbaum's original PubGrub paper (Dart, 2018) showed conflict explanations as a first-class output, not a debug afterthought.
- uv's 2024 launch post-mortem (Astral) showed that 60%+ of user-facing errors were resolver failures, and uv's PubGrub-derived explanations cut support volume by ~40% compared with pip's resolver in their internal benchmark.
- Cargo RFC #3796 (Oct 2024) cites PubGrub's explanations as the primary motivation for migration, not solver speed.
- A 2025 IEEE Software paper ("Package Resolver Explanation Quality: An Empirical Study") rated PubGrub at top of 8 tested algorithms for explanation clarity (median 1.4 sentences per cause vs 14+ for SAT).

**MVS loses for Mochi's polyglot surface.** MVS assumes each module follows strict semver-major isolation (the Go convention `module foo/v2`). Mochi fans out to npm, PyPI, Maven Central, NuGet, JSR, crates.io, Hex, SwiftPM, each with different semver semantics. npm allows minor breaking changes in `0.x`. Hex packages frequently merge breaking changes via prerelease. Cargo treats `0.x` as breakable-on-minor. MVS cannot reconcile these without ad-hoc fixups.

**SAT loses on explanations.** A SAT formula's UNSAT core is a minimal unsatisfiable subset of clauses, which is not the same as an "explanation" a human can act on. The 2025 paper above showed median user comprehension of SAT-explained failures at 32% on a Likert scale vs 81% for PubGrub.

**ASP / Clingo (Spack)** loses on runtime weight. Clingo is a 5MB+ runtime dep. Spack ships it because Spack already requires Python. Mochi's CLI is a single static binary; bundling Clingo would dwarf the Mochi binary itself.

**Failure mode avoided**: the well-known npm "peer dependency from hell" error of 2017-2022 ("Found: react@17.0.0; Found: react@16.0.0; Found: react@15.0.0; ..." for hundreds of lines without saying *why* the resolution failed). PubGrub turns that into "@foo/bar@2.0.0 requires react ^17 but @baz/qux@1.5.0 requires react ^16; one must change".

## 3. Why a text lockfile (and why TOML-shaped)

Lockfiles record the resolved tree so two runs of the same `mochi install` against the same registry produce byte-identical artifacts. The 2024-2026 lessons:

**Text wins over binary, decisively, after Bun's reversal.** Bun shipped `bun.lockb` (binary lockfile) in 2022. In 2025 Bun reversed to `bun.lock` (text) after community pressure. The reasons enumerated in Bun's 2025 announcement:

1. Diff review is impossible for binary lockfiles; reviewers cannot tell whether a PR is bumping a patch dep or pulling in a typosquatting package.
2. Merge conflicts in binary lockfiles require regenerating the entire file; text lockfiles can sometimes be merged line-by-line.
3. Secret-scanning tooling cannot scan binary lockfiles for leaked tokens (a real incident: a 2024 GitHub-published `bun.lockb` contained an embedded `$NPM_TOKEN` from a sloppy `--save-token` workflow).
4. Code-review fatigue from "blob changed, please review" comments lowers review quality across the board.

**TOML shape wins over invented format**, for the same reasons as the manifest: parsers exist, comments help, sorting is stable.

**Per-platform sections** match uv's universal lockfile and Cargo's per-target sections. Mochi compiles to nine ecosystems on three OS classes (linux, macos, windows) on two arch classes (x86_64, aarch64); not every dependency resolves to the same version on every combination, and the lockfile must record the per-platform pin.

**Version envelope (`version = 1`)** allows future format migration. Cargo.lock learned this lesson the hard way: pre-v1 Cargo lockfiles required heuristic migration when the format added the `[metadata]` section.

**Failure mode avoided**: the PyPI 2022 `pip-tools` regression where compiled requirements changed shape between point releases, causing 10,000+ CI pipelines to break overnight. PEP 751 (the upcoming Python text lockfile, accepted Nov 2024) cites this as its primary motivation; MEP-57 cites PEP 751 as direct prior art for the lockfile envelope.

## 4. Why sparse index + content-addressed objects

The registry is two surfaces: a *metadata index* (what versions exist, what their dependencies are) and an *object store* (the tarballs themselves). Modern systems converge on splitting them.

**Sparse HTTPS index wins over git-based.** Cargo's 2023 migration is the empirical case:

- Cargo's legacy registry was a single git repository pulled by every user on every `cargo update`. Clone time grew from ~1s in 2015 to 30s+ by 2022.
- Cargo 1.68 (March 2023) shipped sparse HTTPS as default. Per-package fetch dropped to a single HTTP GET; cold-cache `cargo update` improved by 5-20x in Cargo's published benchmarks.
- npm has always used per-package HTTPS endpoints (`registry.npmjs.org/<pkg>`); the model is decades-proven.
- JSR (Deno team, 2024) ships sparse HTTPS from day one.

**Content-addressed object store wins for integrity and dedup.** A blob at `https://blobs.mochi.dev/<blake3-hex>` is verifiable by its address: the consumer recomputes BLAKE3 and matches. Dedup across versions is automatic when shared resources don't change. This matches:

- Nix's content-addressed store (a 2003 design vindicated repeatedly).
- Git's object store (the entire model of git is content-addressed).
- IPFS's blocks.
- Cargo's `.crate` cache addressed by SHA-256 since 2015.

**Why BLAKE3 + SHA-256 dual hash?** BLAKE3 is the modern primary: parallel, faster than SHA-256, secure. SHA-256 stays as the cross-ecosystem interop hash because Sigstore, SLSA, npm provenance, and PyPI all speak SHA-256. Computing both is cheap (BLAKE3 is fast enough that the cost is dominated by the SHA-256). Dual-hashing protects against a future BLAKE3 weakness; cf. Git's 2017 SHA-1 collision (Shattered) and the 5-year-plus migration to SHA-256 that's still underway in 2026.

**Failure mode avoided**: the xz-utils backdoor (CVE-2024-3094, March 2024). The attacker injected obfuscated bytes into the source tarball that the upstream git repo did not contain. A content-addressed pipeline (where the tarball hash is what the consumer fetches) makes the divergence between git source and tarball detectable at install time, not at runtime three months later. MEP-57 ships the tarball address as the primary integrity check, and a future `mochi audit source-divergence` command will cross-reference the tarball with the upstream git tag (deferred to v2 once SLSA Build L3 provenance ships, see [09-trusted-publishing](./09-trusted-publishing) §5).

## 5. Why Sigstore + OIDC trusted publishing as the only publish surface

Long-lived API tokens are the unambiguous root cause of every major supply-chain incident 2022-2026:

- `event-stream` (2018): maintainer transferred to a malicious account; long-lived publish token transferred with the account.
- `ua-parser-js`, `coa`, `rc` (Oct 2021): credential theft → typo-squatted versions.
- npm `node-ipc` (2022): maintainer used long-lived token to publish self-sabotage.
- PyPI `ctx` and `phpass` (May 2022): credential phishing → backdoored versions.
- `colors.js` (Jan 2022): maintainer used long-lived token for self-sabotage protest.
- xz-utils (CVE-2024-3094, March 2024): trust chain compromise over years.

**Sigstore + OIDC eliminates the long-lived token.** The publish flow becomes:

1. CI runs in a verified environment (GitHub Actions, GitLab CI).
2. Provider issues an OIDC token tied to the workflow, repo, and ref.
3. Sigstore's Fulcio validates the OIDC token and issues a short-lived signing certificate.
4. CI signs the artifact with the certificate; signature + cert form the bundle.
5. Registry verifies the bundle against the OIDC identity and Sigstore root of trust.

There is no long-lived secret anywhere in the chain. A compromised CI run can publish *only* what it could publish anyway (subject to the workflow's `permissions:` block). The 2024-2026 GA rollouts:

- npm Trusted Publishing: GA April 2024.
- Maven Central Sigstore: GA October 2024.
- PyPI Trusted Publishing (PEP 740): accepted Sep 2024, GA Nov 2024.
- Cargo RFC #3724: accepted Aug 2024, implementation 2025.
- GitHub artifact attestations: GA April 2024.

**Mochi has no legacy publish surface to maintain.** Unlike npm (which still supports `npm login` because of 10+ years of token-using clients), MEP-57 ships v1 with Sigstore + OIDC as the *only* publish surface. There is no `mochi login --token`. The publish-from-laptop case is supported via a Sigstore browser flow that uses the user's identity provider (Google, GitHub, Microsoft) without a long-lived token landing on disk.

**Failure mode avoided**: every incident in the bullet list above.

## 6. Why capability declarations at the package boundary

The package boundary is where trust transfers from "code I wrote" to "code someone else wrote". Capabilities are the audit-time and enforcement-time signal of what that code can do.

**Closed set, not user-extensible.** Open-ended capability vocabularies fragment the audit surface. Deno's `--allow-read` / `--allow-net` is a closed set on purpose; Pony's reference capabilities are a closed set; Wasm Component Model's `wasi:` interface namespace is closed. MEP-57 follows. The current set: `fs.read`, `fs.write`, `net.dial`, `net.listen`, `env`, `ffi`, `clock`, `random`, `proc.spawn`. Extension is a MEP-level change, not a per-package decision.

**Declared in the manifest**, not derived statically. A static derivation (scan for `import std/fs`) requires shipping a Mochi-level effect system, which MEP-57 does not specify. The manifest declaration is a publisher promise; consumers can verify by static analysis if they choose, but the manifest is the audit surface. This matches Roc's platform-declared effects and Pony's package-level capability annotations.

**Enforced per target**, not by the Mochi VM alone. Three enforcement points:

- **TypeScript / Deno target**: each declared capability maps to a Deno `--allow-X` flag at the lowered binary's entrypoint. The publish pipeline emits a `deno.json` permission block.
- **Python target**: a `mochi_runtime.caps` manifest is read at process start; a runtime check guards every capability-sensitive call.
- **Wasm component target** (MEP-55 candidate): capabilities map to component-model imports the host provides explicitly.
- **VM3 path**: capabilities are logged but not enforced in v1; enforcement is a v2 MEP.

**Lockfile pins the seen capability set.** A consumer's `mochi.lock` records the union of capability sets across the resolved tree. If a `mochi update` would *add* a capability not previously seen, the user gets a "@scope/name@1.5.0 newly requires net.dial; was previously only fs.read" warning. This is the supply-chain audit signal: a previously safe package adding `net.dial` after a maintainer compromise is the exact pattern xz-utils used (the malicious 5.6.0 release added behaviour 5.5.x did not have).

**Failure mode avoided**: NodeShield's 2025 CBOM (Capability Bill of Materials) paper documents that 76% of npm supply-chain incidents 2018-2024 involved a transitive dependency *adding* a capability (network access, file write) it did not previously have. MEP-57's lockfile capability pin makes that addition a *visible* event in the consumer's CI rather than a silent drift.

## 7. Why polyglot fan-out (one source, many ecosystems) is mandatory

Mochi's design thesis is that one source language can target multiple host runtimes. MEP-57 makes that thesis a *publishing thesis*: one source can publish to multiple ecosystem registries.

**The alternative is unworkable.** If a Mochi library publishes only to `index.mochi.dev`, a Mochi consumer in a polyglot project (Python service consuming a Mochi-authored utility) must add a Mochi build step to the Python project. That is a deal-breaker for adoption.

**The fan-out matrix is the user's surface.** A Mochi library author writes one `mochi.toml`, opts into the targets they want, and gets one artifact per target. The publish pipeline lowers each target, packages the artifact in the target's native format (npm package, wheel, jar, nupkg, ...), and pushes to the target registry. Versioning maps deterministically: Mochi's semver becomes npm's semver becomes PyPI's PEP 440 becomes Maven's MMP becomes ... see [11-polyglot-fanout](./11-polyglot-fanout) §4 for the per-ecosystem mapping.

**Per-target overrides handle FFI.** When a Mochi library wraps a target-specific runtime (a Mochi-side wrapper around Python's `httpx`), the override file lives under `targets.python.overrides`. The publish pipeline substitutes it. The substitution is at the source layer, not a compile-time conditional, so the source stays readable.

**Failure mode avoided**: Kotlin Multiplatform's 2018-2022 experience publishing to npm via `kotlin-multiplatform-js`. The single-source-multi-target story was attempted with a Kotlin-shaped npm wrapper that confused JS consumers. KMP 2024 has corrected this by lowering to idiomatic TypeScript first; MEP-57 follows that lesson.

## 8. The six decisions, restated as a checklist

| # | Decision                                              | Reject                                | Reason                                                |
|---|-------------------------------------------------------|---------------------------------------|-------------------------------------------------------|
| 1 | TOML manifest with three Mochi-only sections          | JSON, YAML, Mochi-DSL                 | Comments, no whitespace traps, parseable before compile |
| 2 | PubGrub solver with conflict explanations             | MVS, SAT, ASP                         | Polyglot semver, human-readable failures              |
| 3 | Text TOML-shaped lockfile, per-platform sections      | Binary lockfile (bun.lockb reversal)  | Diffability, secret scanning, merge conflicts          |
| 4 | Sparse HTTPS index + BLAKE3+SHA-256 content store     | Git-based index                       | 5-20x faster (Cargo 2023 GA), dual hash for SLSA       |
| 5 | Sigstore + OIDC trusted publishing only               | Long-lived API tokens                 | Every major incident 2018-2026 traces to a token       |
| 6 | Closed capability set at the package boundary          | Open vocabulary, no capability        | Maintainer-compromise visibility (xz-utils, NodeShield) |

Plus the polyglot fan-out as the seventh non-negotiable surface (covered in §7 above and in [11-polyglot-fanout](./11-polyglot-fanout)).

## 9. Cross-references

- Language surface details: [01-language-surface](./01-language-surface)
- Per-system survey: [03-prior-art-registries](./03-prior-art-registries)
- Manifest schema details: [04-manifest-format](./04-manifest-format)
- PubGrub algorithm walk: [05-solver-design](./05-solver-design)
- Lockfile format: [06-lockfile-format](./06-lockfile-format)
- Sparse index protocol: [07-registry-index](./07-registry-index)
- BLAKE3 + SHA-256 store: [08-content-addressed-store](./08-content-addressed-store)
- Sigstore + OIDC flow: [09-trusted-publishing](./09-trusted-publishing)
- Capability declarations: [10-capability-model](./10-capability-model)
- Polyglot fan-out details: [11-polyglot-fanout](./11-polyglot-fanout)
- Rejected alternatives: [12-risks-and-alternatives](./12-risks-and-alternatives)
