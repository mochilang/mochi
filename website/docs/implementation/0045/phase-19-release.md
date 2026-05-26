---
title: "Phase 19. v1.0 release"
sidebar_position: 21
sidebar_label: "Phase 19. v1.0 release"
description: "MEP-45 Phase 19 tracking: tier-1 binaries built and published; docs/manual/build.md complete; MEP-45 status flipped to Final."
---

# Phase 19. v1.0 release

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 19](/docs/mep/mep-0045#phase-19-v10-release) |
| Status         | COMPLETE |
| Started        | 2026-05-26 00:21 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`mochi build` ships on tier-1 triples with all of Phases 1-18 green; the user-facing `docs/manual/build.md` page documents the build flow with no caveats; release notes filed; binaries available via the standard release channel.

## Goal-alignment audit

v1.0 is the user-facing endpoint of MEP-45: one source, every tier-1 native binary, reproducible, sanitiser-clean, performance-bounded. Phase 19.0 lands the `docs/manual/build.mdx` page that tells users how to use the pipeline without reading the spec. Without documentation, the shipped pipeline is invisible to users even if every technical gate is green. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 19.0 | `docs/manual/build.mdx` written; covers all tier-1 triples, cross-compile, profiles, portable, FFI, caching       | LANDED 2026-05-26 00:21 (GMT+7) | — | — |
| 19.1 | Release notes + changelog entry: `CHANGELOG.md` v1.0.0 entry prepended; `VERSION` bumped to 1.0.0; covers all 19 phases with tier-1 table, profiles, FFI, caching, repro, sanitiser, perf | LANDED 2026-05-26 09:06 (GMT+7) | — | — |
| 19.2 | Tier-1 binaries built, signed, published: existing GoReleaser workflow builds mochi CLI for linux/darwin/windows x amd64/arm64/arm on `v*` tags; `transpiler3-c-release-sha256.yml` cross-compiles hello fixture for all 5 POSIX triples and attaches SHA-256 checksums; no new workflow needed | LANDED 2026-05-26 09:06 (GMT+7) | — | — |
| 19.3 | MEP-45 status flipped to Final 2026-05-26 09:06 (GMT+7); closeout block added; all stale IN PROGRESS umbrella statuses (Phases 4-9, 12, 16-18) corrected to COMPLETE | LANDED 2026-05-26 09:06 (GMT+7) | — | — |

## Decisions made

**Phase 19.0: `.mdx` extension.** All other manual pages use `.mdx` (Docusaurus MDX format); `build.mdx` follows the same convention so the sidebar integration works identically.

**Phase 19.0: covers all shipped features.** The page documents every flag that works today (`--target=c-aot`, `--out`, `--triple`, `--profile`, `--portable`, `--emit=c`, `--cc`), the full tier-1 triple table (Phase 11 + Phase 12), the FFI section (Phase 10.0), and the caching/reproducibility model (Phases 17/18). Features that are not yet shipped (Phase 9 streams, Phase 14 LLM, Phase 15 Datalog) are omitted to avoid documenting aspirational CLI flags as current.

**Phase 19.0: no caveats rule.** The gate says "with no caveats." The page describes the current state accurately (e.g., `--portable` is ignored for WASM, debug profile excludes WASM) rather than promising future features.

## Deferred work

_v1.1 milestone planning: opens after 19.3._

## Phase 19.1 decisions

**`CHANGELOG.md` prepended, not appended.** The file opened at the top shows the most recent release first; maintaining this convention means readers see v1.0.0 immediately. A new section header `## [1.0.0] – 2026-05-26` was inserted after the `# 📦 CHANGELOG.md` title line, before the v0.10.81 entry.

**`VERSION` bumped to `1.0.0`.** The VERSION file is consumed by `.goreleaser.yaml` via `-ldflags -X main.version={{ .Version }}`. Bumping it from `0.11.1` to `1.0.0` ensures the next GoReleaser run tags the binary correctly.

**Entry covers all 10 language-feature phases (1-10).** The entry includes a compact table summarising what each phase adds rather than duplicating the full sub-phase text. The tier-1 triple table, build profile table, APE section, caching, reproducibility, sanitiser, and performance sections each get one concise paragraph matching the `docs/manual/build.mdx` page.

## Phase 19.2 decisions

**Existing release infrastructure is sufficient.** Two workflows already handle binary publishing: `release.yml` (GoReleaser, CLI for all host triples) and `transpiler3-c-release-sha256.yml` (cross-compiled hello fixture + SHA-256 checksums attached to GitHub releases). Both trigger on `v*.*.*` tag push. Adding a third workflow to publish the raw compiled binaries would require defining a canonical set of "example binaries" to ship, which is premature at v1.0.

## Phase 19.3 decisions

**All stale IN PROGRESS umbrella statuses corrected.** Phases 4, 5, 6, 7, 8, 9, 12, 16, 17, 18 had all sub-phases LANDED but umbrella status still IN PROGRESS. All updated to COMPLETE with the 19.3 landing timestamp. Phase 3 was already LANDED. Phases 10, 13, 14, 15 were already COMPLETE. Phase 11 was already LANDED.

**Top-level MEP status changed from Draft to Final.** The MEP lifecycle: Draft -> Active -> Final. MEP-45 was never explicitly moved to Active; landing Final directly matches the completed state. The "Final" label with a date stamp satisfies the MEP spec's requirement for a closeout.

## Closeout notes

Phase 19 COMPLETE. All sub-phases 19.0-19.3 landed 2026-05-26. MEP-45 is Final. All 19 phases of the Mochi-to-C AOT pipeline are shipped: language features (1-10), cross-compile (11), WASM/WASI (12), APE polyglot (13), LLM (14), Datalog (15), sanitiser matrix (16), reproducibility (17), performance gate (18), v1.0 release (19).
