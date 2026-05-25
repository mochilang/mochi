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
| Status         | IN PROGRESS |
| Started        | 2026-05-26 00:21 (GMT+7) |
| Landed         | — |
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
| 19.1 | Release notes + changelog entry                                                                                    | NOT STARTED | —      | — |
| 19.2 | Tier-1 binaries built, signed, published                                                                           | NOT STARTED | —      | — |
| 19.3 | MEP-45 status flipped to Final; this MEP file gets a closeout block dated and committed                            | NOT STARTED | —      | — |

## Decisions made

**Phase 19.0: `.mdx` extension.** All other manual pages use `.mdx` (Docusaurus MDX format); `build.mdx` follows the same convention so the sidebar integration works identically.

**Phase 19.0: covers all shipped features.** The page documents every flag that works today (`--target=c-aot`, `--out`, `--triple`, `--profile`, `--portable`, `--emit=c`, `--cc`), the full tier-1 triple table (Phase 11 + Phase 12), the FFI section (Phase 10.0), and the caching/reproducibility model (Phases 17/18). Features that are not yet shipped (Phase 9 streams, Phase 14 LLM, Phase 15 Datalog) are omitted to avoid documenting aspirational CLI flags as current.

**Phase 19.0: no caveats rule.** The gate says "with no caveats." The page describes the current state accurately (e.g., `--portable` is ignored for WASM, debug profile excludes WASM) rather than promising future features.

## Deferred work

_v1.1 milestone planning: opens after 19.3._

## Closeout notes

_Fill in after gate green._
