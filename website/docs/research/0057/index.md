---
title: "MEP-57 research bundle: Mochi module and package system"
description: "Twelve research notes covering language surface, design philosophy, prior art across modern package managers, manifest format, PubGrub solver design, lockfile format, sparse HTTPS registry index, content-addressed object store, Sigstore-backed trusted publishing, capability model, polyglot fan-out matrix, and risks for the Mochi module and package system proposed in MEP-57."
sidebar_position: 57
sidebar_label: "MEP-57"
---

# MEP-57 research bundle: Mochi module and package system

Author: research pass for MEP-57 (Mochi module and package system).
Date: 2026-05-29 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-57, the Mochi module and package system](/docs/mep/mep-0057). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0057/language-surface) | How the existing `import` statement extends to manifest-resolved packages, workspaces, capabilities, polyglot target opt-in, while keeping path-form imports intact |
| 02 | [Design philosophy](/docs/research/0057/design-philosophy) | Why TOML, why PubGrub, why sparse index, why content-addressed objects, why Sigstore + OIDC, why capabilities at the package boundary, why polyglot fan-out is mandatory, framed against 2024-2026 research |
| 03 | [Prior art: registries and package managers](/docs/research/0057/prior-art-registries) | Survey of modern (2024-2026) systems: Cargo, Go modules, npm, JSR, Bun, uv + PEP 751 / 723 / 735, Bazel bzlmod, Nix flakes, SwiftPM, Hex, NuGet, Pixi, Spack, Pkl |
| 04 | [Manifest format](/docs/research/0057/manifest-format) | `mochi.toml` schema deep dive, key set comparison with Cargo / uv / pyproject / package.json, validation rules, three Mochi-only sections, manifest evolution |
| 05 | [Solver design](/docs/research/0057/solver-design) | PubGrub algorithm walk, derivation of incompatibilities, decision levels, conflict-driven backtracking, "why" explanation generation, rejection of MVS / SAT / ASP for Mochi's polyglot surface |
| 06 | [Lockfile format](/docs/research/0057/lockfile-format) | `mochi.lock` canonical TOML serialisation, per-platform sections, version envelope, lessons from Bun's `bun.lockb` reversal, PEP 751, Cargo.lock, package-lock.json v3 |
| 07 | [Registry index](/docs/research/0057/registry-index) | Sparse HTTPS index protocol, ETag and cache semantics, retry strategy, mirror discovery, version yank flow, deletion policy, comparison with Cargo's GA March 2023 migration |
| 08 | [Content-addressed object store](/docs/research/0057/content-addressed-store) | BLAKE3-256 primary + SHA-256 secondary, blob fetch protocol, cache layout, deduplication, integrity verification, why dual hashing matches SLSA + Sigstore + npm requirements |
| 09 | [Trusted publishing](/docs/research/0057/trusted-publishing) | Sigstore + OIDC keyless flow, Fulcio certificate request, signature bundle, Rekor transparency log, verification, comparison with npm Trusted Publishing (April 2024), Maven Central (October 2024), PyPI PEP 740 (Nov 2024), Cargo RFC #3724 |
| 10 | [Capability model](/docs/research/0057/capability-model) | Closed capability set, declaration syntax, lockfile annotation, enforcement per target (Deno permissions, Python runtime checks, Wasm component imports), lessons from Roc platforms, Pony reference capabilities, Wasm Component Model, Lavamoat / NodeShield |
| 11 | [Polyglot fan-out](/docs/research/0057/polyglot-fanout) | One source manifest, eight artifact pipelines, per-target overrides, FFI lowering hand-off, target-specific gates, version mapping across ecosystems with incompatible semver semantics |
| 12 | [Risks and alternatives](/docs/research/0057/risks-and-alternatives) | 16 risks (typosquatting, registry compromise, solver loops, capability creep, sigstore root rotation, polyglot drift, ...) + 12 rejected alternatives (Unison hash-as-name, git-based registry, Mochi-DSL manifest, monorepo-only, vendoring-only, MVS, SAT, ASP, binary lockfile, long-lived tokens, open capability vocabulary, no capabilities at all) + 7 v2 candidates (federated discovery, index transparency log, cargo-vet attestations, effect-system capabilities, path-scoped capabilities, PEP 723 inline-script deps, Wasm component model) |

## Cross-references

- [MEP-45 (C target)](/docs/mep/mep-0045)
- [MEP-46 (BEAM target)](/docs/mep/mep-0046)
- [MEP-47 (JVM target)](/docs/mep/mep-0047)
- [MEP-48 (.NET target)](/docs/mep/mep-0048)
- [MEP-49 (Swift target)](/docs/mep/mep-0049)
- [MEP-50 (Kotlin target)](/docs/mep/mep-0050)
- [MEP-51 (Python target)](/docs/mep/mep-0051)
- [MEP-52 (TypeScript / JavaScript target)](/docs/mep/mep-0052)
- [MEP-53 (Rust target)](/docs/mep/mep-0053)
