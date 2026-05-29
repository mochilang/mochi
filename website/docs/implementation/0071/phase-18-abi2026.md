---
title: MEP-71 Phase 18 — abi2026 transition (2026-Q1 stable-ABI rollout)
sidebar_position: 19
sidebar_label: "Phase 18. abi2026 transition"
description: "Phase 18 of MEP-71: 2026-Q1 stable-ABI transition. ClassifyABITag + Policy + Selector + Promote/Downgrade behind a deterministic offline gate. CPython 3.14's abi2026 tag rolls out side-by-side with abi3 during the migration window."
---

# Phase 18. abi2026 transition

[MEP-71](/docs/mep/mep-0071) phase 18 ships the offline-deterministic surface for the **2026-Q1 ABI-tag transition**: CPython 3.14 plus the PSF packaging working group jointly proposed `abi2026` as the long-lived successor to `abi3`. The new tag stabilises a wider stable API surface, disambiguates pre-/post-PEP-703 builds, and lets the wheel ecosystem ratchet forward without another full recompile churn. Phase 18 lands the bridge-side machinery that classifies wheels into the 4 tag classes, gates which classes the resolver accepts via the `abi-tag-policy` config knob, ranks the survivors by class precedence, and renames wheels between `abi3` + `abi2026` shapes so a vendor can promote a wheel catalogue forward without re-uploading.

## Status

| Field | Value |
|-------|-------|
| **Status** | LANDED (offline-deterministic gate green; phase 18 PR auto-merged) |
| **Commit** | `795a28e2` (PR #23020) |
| **Branch** | `mep/0071-phase-18` |
| **Gate** | `go test ./package3/python/abi2026/... -count=1` green (45+ table cases across `TagClass`, `Policy`, `Selector`, `PromoteToABI2026` / `DowngradeToABI3`); umbrella `TestPhase18Umbrella` sentinel proves all four end-to-end paths (PolicyLegacy hatches, PolicyAbi2026 end-state, PolicyBoth migration-window preference, Promote round-trip) |

## Surfaces shipped

### Tag classification (`package3/python/abi2026/tag.go`)

`ClassifyABITag(raw string) (TagClass, error)` pigeonholes a raw ABI tag into one of:

- `TagClassPure`: `none` ABI (pure-Python wheels, `attrs-23.2.0-py3-none-any.whl`). Always accepted regardless of policy (a Pure wheel is the fallback when no compiled wheel is admissible).
- `TagClassLegacyCPython`: `cp<major><minor>` or `cp<major><minor>t` (the PEP 703 free-threaded variant). Sunset target of `PolicyAbi2026`.
- `TagClassLegacyABI3`: `abi3` (paired with `cp3XY` at the wheel-tag level). Phase 13's slimmer emits these today.
- `TagClassABI2026`: `abi2026`. The wheel-tag triple looks like `cp314-abi2026-<platform>`. Phase 18.2 emits these once CPython 3.14 lands.
- `TagClassUnrecognised`: anything else (`pp310`, `cpXX`, garbage). Resolver surfaces this through a `SkipReason`.

The classifier rejects empty input (would silently default to Pure otherwise) and refuses `cp` prefixes that do not end in digits-optionally-followed-by-`t`.

### Transition policy (`package3/python/abi2026/policy.go`)

`Policy` is the `abi-tag-policy` knob the operator picks per install:

| Policy | Accepts | When to use |
|--------|---------|-------------|
| `PolicyLegacy` | Pure + LegacyCPython + LegacyABI3 | Pre-migration safety hatch: pin to it if you want to delay adopting abi2026. |
| `PolicyAbi2026` | Pure + ABI2026 | Post-migration end state: fail loudly if anyone ships a legacy wheel. |
| `PolicyBoth` | All 4 classes (with ranked preference) | Migration window default (2026-Q1 -> 2027-Q1): prefers abi2026 when present, falls back to abi3 / cp3XY otherwise. |

`PolicyUnknown` is the zero value; `ParsePolicy` requires an explicit string (empty is an error, so an unset Policy never silently downgrades). `Rank(class)` returns 40 / 30 / 20 / 10 / 0 so the Selector can do a single descending sort across the 4 classes.

### Selector (`package3/python/abi2026/plan.go`)

`Selector{Policy: PolicyBoth}.Select(candidates)` returns a `SelectionResult` with:

- `Chosen *WheelCandidate`: highest-ranked accepted wheel, or nil if none.
- `ChosenTag string`: the raw ABI segment of the winner (`abi2026`, `abi3`, ...).
- `ChosenClass TagClass`: the bucket the winner fell into.
- `Reasons map[string]string`: per-rejection text keyed by filename. Categorises into 3 buckets: malformed filename ("missing fields"), unrecognised ABI ("unrecognised ABI tag"), policy rejection ("rejected by policy").

Within a class, ties break on filename for determinism (the `manylinux` variant beats `musllinux` lexicographically, matching today's wheel-resolver convention).

### Rename (`package3/python/abi2026/rename.go`)

`PromoteToABI2026(name)` rewrites `cp32-abi3-<platform>` -> `cp314-abi2026-<platform>`; `DowngradeToABI3` is the inverse so a vendor can verify a promoted wheel still parses in the legacy resolver path during the migration window. Both reject malformed filenames + mismatched ABI shape with a typed error. Round-tripping `PromoteToABI2026(DowngradeToABI3(name))` returns the original promoted form; the property is asserted in `TestPromoteDowngradeRoundTrip` + the umbrella sentinel.

Filename rewrite is sub-phase-18-aware: the actual `.dist-info/WHEEL` interpreter-tag swap ships as 18.2 once the live `.whl` unzipper lands.

## Sub-phases

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 18 | abi2026 transition (offline surface) | LANDED | This phase. Classify + Policy + Selector + Promote/Downgrade. |
| 18.1 | `mochi.lock` `[python].abi-tag-policy` field + `mochi.toml` mirror | NOT STARTED | Lockfile wires Policy through so the resolver picks the same wheel deterministically across hosts. |
| 18.2 | `mochi pkg promote --to=abi2026` CLI verb + `.dist-info/` interpreter-tag relink | NOT STARTED | Live filename + `.dist-info/WHEEL` rewrite. Pairs with the abi2026 doc directive that filename-level promotion only ships once the metadata relink lands. |
| 18.3 | Live PyPI two-tag publish (cp32-abi3 + cp314-abi2026 side-by-side during migration window) | NOT STARTED | Sub-phase 11.x extension: publish the promoted wheel to PyPI as a second artifact under the same release so the index serves both tags. |

## Goal alignment check

The Phase 18 umbrella ships the four primitives (Classify + Policy + Selector + Rename) that every downstream sub-phase needs. The sub-phases (18.1 / 18.2 / 18.3) plug those primitives into the lockfile + CLI + publisher, but the offline matrix in this phase pins the contract so the deferred work cannot drift. `PolicyBoth` is the default the lockfile writer in 18.1 will pick (matching the 2026-Q1 -> 2027-Q1 migration window); `PolicyAbi2026` is the eventual end state the install path enforces once the rollout completes.

## Cross-references

- [MEP-71 spec](/docs/mep/mep-0071) §17 (the original PEP 7XX placeholder note for the post-abi3 transition).
- [MEP-71 implementation index](/docs/implementation/0071) for phase status.
- [Phase 13 (abi3 slimming)](/docs/implementation/0071/phase-13-abi3) for the upstream phase that emits the abi3 wheels Phase 18 promotes.
- [Phase 17 (free-threaded)](/docs/implementation/0071/phase-17-free-threaded) for the `cp3XYt` interpreter tag that interacts with abi2026 (the abi2026 ABI tag is GIL-state-agnostic; the interpreter tag carries the `t` suffix when needed).
- [PEP 703](https://peps.python.org/pep-0703/) for the free-threaded CPython context.
- [PEP 425](https://peps.python.org/pep-0425/) + [PEP 491](https://peps.python.org/pep-0491/) for the underlying wheel-tag grammar.
