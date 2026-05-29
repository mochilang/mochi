---
title: MEP-57 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 57. Mochi module and package system"
description: "Per-phase implementation tracking for MEP-57 (Mochi module and package system: TOML manifest, PubGrub-derived solver, text lockfile, sparse HTTPS registry index, content-addressed BLAKE3 object store, Sigstore + OIDC trusted publishing, capability declarations, polyglot fan-out to npm / PyPI / Maven Central / NuGet / JSR / crates.io / Hex / Swift Package Index). Status and commit columns get filled in along the way as sub-PRs land."
---

# MEP-57 implementation tracking

Per-phase tracking for [MEP-57 Mochi module and package system](/docs/mep/mep-0057). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-57 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase                                                       | Title                                                                          | Status      | Commit |
|-------------------------------------------------------------|--------------------------------------------------------------------------------|-------------|--------|
| [0](./phase-00-skeleton)                                    | Skeleton (pkg/ package stubs)                                                  | NOT STARTED | n/a    |
| [1](./phase-01-manifest)                                    | Manifest format (`mochi.toml` parser + schema + round-trip)                    | NOT STARTED | n/a    |
| [2](./phase-02-local-resolution)                            | Local resolution (manifest activates resolver, path-form imports preserved)    | NOT STARTED | n/a    |
| [3](./phase-03-workspaces)                                  | Workspaces (multi-package monorepo, `mochi.workspace.toml` umbrella)           | NOT STARTED | n/a    |
| [4](./phase-04-lockfile)                                    | Lockfile format (`mochi.lock` writer / reader)                                 | NOT STARTED | n/a    |
| [5](./phase-05-pubgrub)                                     | PubGrub solver core (conflict-driven backtracking, incompatibility derivation) | NOT STARTED | n/a    |
| [6](./phase-06-explanations)                                | Solver explanations + `mochi why`                                              | NOT STARTED | n/a    |
| [7](./phase-07-local-registry)                              | Local filesystem-backed registry (sparse index over disk)                      | NOT STARTED | n/a    |
| [8](./phase-08-sparse-index)                                | Network sparse index over HTTPS (`index.mochi.dev`)                            | NOT STARTED | n/a    |
| [9](./phase-09-content-store)                               | Content-addressed object store (BLAKE3 + SHA-256 dual hash)                    | NOT STARTED | n/a    |
| [10](./phase-10-capabilities)                               | Capability declarations + audit warning surface                                | NOT STARTED | n/a    |
| [11](./phase-11-mirror)                                     | Registry mirror protocol                                                       | NOT STARTED | n/a    |
| [12](./phase-12-publish)                                    | Publish pipeline (tarball build, manifest pin, dry-run)                        | NOT STARTED | n/a    |
| [13](./phase-13-sigstore)                                   | Sigstore + OIDC trusted publishing                                             | NOT STARTED | n/a    |
| [14](./phase-14-polyglot)                                   | Polyglot fan-out (`mochi publish --target=<eco>`)                              | NOT STARTED | n/a    |
| [15](./phase-15-sbom)                                       | SBOM + provenance (CycloneDX 1.6, SPDX 3.0, in-toto attestation)               | NOT STARTED | n/a    |
| [16](./phase-16-advisory)                                   | Advisory database + `mochi audit`                                              | NOT STARTED | n/a    |
| [17](./phase-17-repro)                                      | Reproducible package build (`SOURCE_DATE_EPOCH`, sorted tar)                   | NOT STARTED | n/a    |
| [18](./phase-18-offline)                                    | Offline + vendor (`mochi vendor`, offline cache hits-only mode)                | NOT STARTED | n/a    |
| [19](./phase-19-cache-perf)                                 | Workspace cache + perf (parallel fetch, content-deduped global cache)          | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
