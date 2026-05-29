---
title: MEP-55 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 55. Mochi-to-PHP transpiler"
description: "Per-phase implementation tracking for MEP-55 (Mochi-to-PHP 8.4 transpiler for Composer + Packagist, Phar + FrankenPHP + RoadRunner packaging, GPG-signed releases with Sigstore attestation). All 18 phases shipped via umbrella PR #22481 merged 2026-05-29."
---

# MEP-55 implementation tracking

Per-phase tracking for [MEP-55 Mochi-to-PHP transpiler](/docs/mep/mep-0055). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-55 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

All 18 phases shipped together on `impl/mep-0055-php`, merged to `main` via [PR #22481](https://github.com/mochilang/mochi/pull/22481) on 2026-05-29 at 07:35 UTC (merge commit `7c72ebaae9`).

## Phase status

| Phase | Title                                                                 | Status | Commit     |
|-------|-----------------------------------------------------------------------|--------|------------|
| 0     | Skeleton, scaffold, matrix CI                                         | LANDED | `7c72ebaae9` |
| 1     | Hello world (print int/float/bool/string + newline shape)             | LANDED | `7c72ebaae9` |
| 2     | Scalars (int, float, bool, string, compare, arith, casts, control)   | LANDED | `7c72ebaae9` |
| 3     | Collections (lists, maps, sets, list-of-records)                      | LANDED | `7c72ebaae9` |
| 4     | Records (final readonly class with constructor promotion)             | LANDED | `7c72ebaae9` |
| 5     | Sum types (abstract readonly base + final readonly variants)          | LANDED | `7c72ebaae9` |
| 6     | Closures and higher-order functions                                   | LANDED | `7c72ebaae9` |
| 7     | Query DSL                                                             | LANDED | `7c72ebaae9` |
| 8     | Datalog (compile-time semi-naive evaluation)                          | LANDED | `7c72ebaae9` |
| 9     | Agents (userland Channel + final class)                               | LANDED | `7c72ebaae9` |
| 10    | Streams (IteratorAggregate + subscribe_limit + drop branch)          | LANDED | `7c72ebaae9` |
| 11    | Async coloring (sync wrappers, no Amp/Revolt dependency)              | LANDED | `7c72ebaae9` |
| 12    | FFI (FFI extension + sodium dispatch)                                 | LANDED | `7c72ebaae9` |
| 13    | LLM (cassette dispatch with DJB2/GMP hash)                            | LANDED | `7c72ebaae9` |
| 14    | fetch (curl, byte-equal against vm3)                                  | LANDED | `7c72ebaae9` |
| 15    | Composer runtime + PHPStan level 9 + Psalm 6 level 1 + php-cs-fixer  | LANDED | `7c72ebaae9` |
| 16    | Reproducibility (byte-equal by construction)                          | LANDED | `7c72ebaae9` |
| 17    | Packaging (Phar + FrankenPHP + RoadRunner)                            | LANDED | `7c72ebaae9` |
| 18    | Signed releases (GPG tag + Sigstore attestation + optional php-signify) | LANDED | `7c72ebaae9` |

## Audit follow-up PRs

Eleven post-merge audits caught soft spots that the umbrella PR's CI did not. Each landed as its own PR on `main`:

| Round | PR     | Issue   | Fix                                                                                           |
|-------|--------|---------|-----------------------------------------------------------------------------------------------|
| 1     | #22568 | n/a     | Strengthen test assertions across phases 4/7/8/13 (DJB2 reimpl, fixture filename pinning)     |
| 1     | #22571 | n/a     | Phase 10.2 `stream_backpressure` fixture exercising `subscribe_limit` + drop branch           |
| 1     | #22575 | n/a     | Phase 9.1 `spawn AgentType()` lowering + `agent_spawn` fixture                                |
| 1     | #22577 | n/a     | Drop unused `amphp/revolt` deps from runtime composer.json (Phase 11 is sync wrappers)        |
| 1     | #22579 | n/a     | Colour pass docs honesty (no Phase 11 replacement; map stays all-Blue)                        |
| 2     | #22594 | n/a     | -Inf branch fixture, empty-needle `str_contains`, empty-list sort, drop dead strCat flag      |
| 3     | #22596 | n/a     | Lock Phase 1 emit shapes for int/float/bool/newline                                           |
| 3     | #22601 | n/a     | Lock Phase 13 stderr-diagnostic branches (env-unset + cassette-missing)                       |
| 3     | #22603 | n/a     | Pin Phase 17 packaging config defaults (rr.yaml workers/timeouts, Caddyfile worker count)     |
| 4     | #22606 | n/a     | Pin Phase 2 float-compare strict-equality shape (`compare_float` fragment row)                |
| 5     | #22616 | n/a     | Honest `Driver.Deterministic` doc (flag is a no-op; pipeline is deterministic by construction)|
| 6     | #22625 | #22624  | Phase 13 non-empty `model:` field cassette-key fixture                                        |
| 6     | #22627 | #22626  | Phase 18 publish trust-chain: gate publish steps on GPG; audit-against-tarball; pin signify   |
| 6     | #22629 | #22628  | Honest fallthrough error messages in PHP `lower.go` (drop misattributed phase labels)         |

## Known residuals

- `Mochi\\Runtime\\IO` is dead from the lowerer's POV; emitted programs inline `mochi_print_*` helpers. The runtime IO class is kept as a downstream public-API surface and is annotated `@api` for Psalm's `findUnusedCode` rule.
- `php-runtime (8.5)` matrix entry is `allow_failure: true`. A pass there does NOT mean Psalm + PHPStan passed; check step logs.
- Live LLM providers (OpenAI, Anthropic, Google, llama.cpp) are deferred. Phase 13 ships cassette-only dispatch.
