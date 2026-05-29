---
title: "Phase 31. Reproducibility test + dead path filter cull (audit-6)"
sidebar_position: 35
sidebar_label: "Phase 31. Reproducibility test + dead path filter cull (audit-6)"
description: "MEP-56 Phase 31, lock in byte-equal reproducible emission across runs and remove a dead tests/transpiler3/ruby paths filter line that referenced a directory that never existed."
---

# Phase 31. Reproducibility test + dead path filter cull (audit-6)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 14:20 (GMT+7) |
| Landed         | 2026-05-29 14:20 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 13e42c2a33 |

## Gate

Two surface changes, both audit-driven:

1. **`TestPhase32Reproducibility` locks in byte-equal emission across two Driver.Build invocations.** Three subtests confirm the same Mochi source produces byte-identical output files each run:
   - `hello_source` (TargetRubySource, `print("hi")`): trivial fixture catches single-file emission drift.
   - `records_and_query` (TargetRubySource, list of records walked by a `from ... where ... select` query): non-trivial fixture catches Data.define ordering, Enumerable chain construction, and list-literal element ordering drift.
   - `hello_gem` (TargetRubyGem, multi-file gem build): catches non-determinism in the gemspec or `lib/*.rb` emission, the most exposed surface for repeat-build divergence.
   
   Each subtest builds the same source twice into two separate temp dirs (`first` and `second` snapshots), walks every emitted file, and asserts byte equality. A regression that introduced a timestamp, a tmpdir leak, or map-iteration non-determinism would fail the gate.

2. **The dead `tests/transpiler3/ruby/**` paths filter in `.github/workflows/transpiler3-ruby-test.yml` is removed.** The directory does not exist (other transpilers use `tests/transpiler3/jvm`, `/c`, `/beam`, `/dotnet`, `/swift` but Ruby never had one), so the filter was unreachable and the line was dead. Per the project rule "don't design for hypothetical future requirements", the filter is dropped now and can be re-added when a fixture corpus is actually added under that path.

## Audit findings not auto-fixed

Two findings surfaced during audit-6 that are out of scope for this phase but worth recording:

1. **Apache-2.0 vs MIT license divergence in emitted gems.** `transpiler3/ruby/build/build.go:246` hardcodes `s.license = "Apache-2.0"` in the gemspec template that `buildGem` emits for the user's program. The repo root `LICENSE` is MIT (`Copyright (c) 2025 Mochi`) and `mochi-runtime/mochi-runtime.gemspec` declares `s.licenses = ["MIT"]`. The discrepancy is not a Ruby-transpiler bug per se: the emitted gem's license should arguably be configurable by the user (their own program is not necessarily Apache or MIT). Recommended follow-up: either change the hardcoded default to MIT to match the rest of the repo, or thread a `License` field through `Driver.Build` options so users can pick. This phase does not change `build.go`; the divergence is locked in by `phase22_test.go:61` (asserts `Apache-2.0` exactly) so changing the default also requires a test update.

2. **C lowerer rejects sum types as record fields.** While drafting the reproducibility fixture, the obvious "list of pixels where Pixel has a Color sum field" form returned a C-lower error: `transpiler3/c/lower: type "Pixel": field "c": type "Color" not supported in Phase 4.0`. This is upstream of the Ruby lowerer (the C lowerer is the shared AOT IR entry point for MEP-45/46/47/48/56) and so out of scope here, but it is worth surfacing: Mochi-on-Ruby can express the Ruby side of this fixture, but the shared lowering pipeline blocks it. The fixture was reshaped to `records_and_query` (a list of `User { name, age }`) which exercises the same Data.define + query paths without hitting the sum-as-field limitation.

## Build target / audit decisions

Three decisions are baked into this phase:

1. **Reproducibility is a public guarantee, not an internal nicety.** Downstream consumers of the emitted gem (auditors, supply-chain scanners, deterministic-build CI) need byte-equal artefacts across rebuilds. The new gate locks that in for `TargetRubySource` and `TargetRubyGem`; extending the same `snapshotEmittedFiles` helper to every build target is a low-cost follow-up but not done here because phases 22 to 27 each have an artefact-existence gate that would already fail loudly on a major regression.
2. **Drop dead config rather than wire it up.** The `tests/transpiler3/ruby/**` filter was a TODO marker disguised as live config. Removing it is the same call as removing dead `Lambda` / `BlockLit` in phase 29, applied to YAML.
3. **Surface, don't auto-fix, contested defaults.** The Apache-2.0 hardcoding could be a deliberate choice (Mochi's preferred default for derived works) or an oversight. Phase 31 surfaces the divergence in the closeout notes and leaves the call to the next maintainer.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/phase32_reproducibility_test.go` | `TestPhase32Reproducibility` (3 subtests): hello / records+query / gem build, each built twice and byte-compared via `snapshotEmittedFiles` |
| `.github/workflows/transpiler3-ruby-test.yml` | Dropped dead `tests/transpiler3/ruby/**` paths filter line |
| `website/docs/implementation/0056/index.md` | Phase 31 row appended |
| `website/docs/implementation/0056/phase-31-audit-6.md` | This tracking page |
| `website/sidebars.js` | Phase 31 entry appended after phase 30 |

## Test set

`phase32_reproducibility_test.go`:

- `TestPhase32Reproducibility/hello_source`: 1-file `.rb` emitted; byte-equal across two builds.
- `TestPhase32Reproducibility/records_and_query`: cross-cutting fixture (records + query + sum-free record fields); byte-equal across two builds.
- `TestPhase32Reproducibility/hello_gem`: multi-file gem (`.gemspec`, `lib/*.rb`, etc.); every file byte-equal across two builds.

## Closeout notes

Phase 31 is audit-6, the smallest gap-closure pass. The reproducibility lock-in is the main user-facing payoff. The Apache-2.0 vs MIT divergence in `build.go` is the most actionable follow-up surfaced; the sum-as-record-field block in the C lowerer is the most actionable cross-MEP follow-up. Neither blocks MEP-56 closure since both are upstream of (license) or orthogonal to (sum-as-field) the Ruby transpiler's mandate, but they are tracked here so a future maintainer does not rediscover them.
