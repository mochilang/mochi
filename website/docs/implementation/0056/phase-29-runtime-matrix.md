---
title: "Phase 29. Runtime matrix CI + dead-code cull (audit-4)"
sidebar_position: 33
sidebar_label: "Phase 29. Runtime matrix CI + dead-code cull (audit-4)"
description: "MEP-56 Phase 29, add the transpiler3-ruby-test.yml CI matrix for CRuby 3.2/3.4 on ubuntu and macos, and remove dead rtree nodes (Lambda, BlockLit) that the lower pass never instantiates."
---

# Phase 29. Runtime matrix CI + dead-code cull (audit-4)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 14:01 (GMT+7) |
| Landed         | 2026-05-29 14:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | e0956a4238 |

## Gate

Two orthogonal gates, both audit-driven (no new user-visible Mochi feature, but the CI matrix gate moves the umbrella phase coverage rule forward):

1. **`transpiler3-ruby-test.yml` exists and is green on the required CRuby versions.** The workflow runs `go test -v -timeout 600s ./transpiler3/ruby/...` on a three-entry matrix: CRuby 3.2 on ubuntu (blocking), CRuby 3.4 on ubuntu (blocking), CRuby 3.4 on macos (non-blocking). Each job uses `ruby/setup-ruby@v1` to pin the toolchain, exports `MOCHI_RUBY=$(which ruby)` so `resolveToolchain` picks up the matrix-installed binary instead of falling through to Homebrew slots, and runs the full Ruby transpiler test suite against that interpreter.
2. **`go build ./transpiler3/ruby/...` and `go test ./transpiler3/ruby/...` still pass after removing `Lambda` and `BlockLit` from `rtree/nodes.go`.** The audit-4 dead-code sweep removed two `rubyExpr`-implementing types that `lower.go` never instantiates: `Lambda` (intended `->(p) { … }` lambda emission, never reached because `MethodDecl` is preferred) and `BlockLit` plus its `MethodCall.Block` field (block syntax for HOF calls is emitted via `RawExpr` strings instead, as documented in the new MethodCall comment). Build- and vet-clean confirms no caller drift.

The MEP-56 umbrella phase coverage rule (`feedback_umbrella_phase_targets.md`) requires every target in §6's matrix to be exercised before the MEP can close. CRuby 3.2 / 3.4 are now exercised on every PR; JRuby 10, TruffleRuby 33, and mruby 4 remain unexercised and are tracked as sub-phases 29.1, 29.2, 29.3 below.

## Build target / audit decisions

This phase does not change the transpiler pipeline, the runtime gem, or any build target. Two surface changes:

1. **`.github/workflows/transpiler3-ruby-test.yml` added.** Modeled after `transpiler3-jvm-test.yml`: same `paths:` filter shape (the new workflow only runs when `transpiler3/ruby/**`, `mochi-runtime/**`, `tests/transpiler3/ruby/**`, or the workflow itself changes), same Go setup, same `continue-on-error: ${{ !matrix.blocking }}` semantics. The matrix entries are deliberately narrow at first: CRuby 3.2 / 3.4 on ubuntu blocking, 3.4 on macos non-blocking. CRuby 4.0 is not in the matrix because `ruby/setup-ruby@v1` does not publish a 4.0 image yet; 4.0 coverage lives in the locally-run developer suite via the existing Homebrew slot detection in `build.go`.
2. **`transpiler3/ruby/rtree/nodes.go` shrunk.** Removed `Lambda` (lines 509 to 519 of the pre-change file) and `BlockLit` (lines 433 to 438 of the pre-change file). Removed the `Block *BlockLit` field on `MethodCall` and the block-rendering branch in `MethodCall.RubyExprString()` (lines 467 to 480 of the pre-change file). The MethodCall comment now explicitly says block-using call sites are emitted via `RawExpr` (and that is true: every `.map { … }` / `.select { … }` / `.inject { … }` call site is built as a `RawExpr` in `lower.go` lines 911 / 922 / 937).

Three audit decisions are baked into this phase:

1. **Delete unused code rather than wire it up.** The project rule "Avoid backwards-compatibility hacks. If you are certain that something is unused, you can delete it completely" applies here. Both `Lambda` and `BlockLit` had complete `RubyExprString` implementations but zero call sites in `lower.go`. Wiring them up would mean reshaping working `RawExpr`-based block emission for no user-visible delta. Deleting them is the cheaper choice and removes the risk of a future contributor mistakenly believing the rtree supports first-class lambdas when in fact every lambda flows through `RawExpr`.
2. **Block emission stays in `RawExpr`.** A future audit may want to re-introduce a typed `Block` node so that block bodies can be lowered structurally instead of by string formatting. That is deliberately out of scope for phase 29: the spec change would have to land first (MEP-56 §3 would need a "block syntax" row), and the lower pass would have to migrate `ListMapExpr` / `ListFilterExpr` / `ListFoldlExpr` / `AsyncExpr` lowering. None of that is justified by an existing bug.
3. **Non-CRuby runtimes are explicit sub-phases.** `index.md` now lists JRuby 10 / TruffleRuby 33 / mruby 4 as sub-phases 29.1 / 29.2 / 29.3 with a note that container-based toolchain detection in `build.go` is the prerequisite. This makes the umbrella phase rule operational: future contributors can pick up 29.1 (JRuby) without re-reading the audit thread that surfaced the gap.

## Files changed

| File | Purpose |
|------|---------|
| `.github/workflows/transpiler3-ruby-test.yml` | New CI workflow, matrix CRuby 3.2 / 3.4 on ubuntu (blocking) and CRuby 3.4 on macos (non-blocking) |
| `transpiler3/ruby/rtree/nodes.go` | Removed dead `Lambda` and `BlockLit` types plus `MethodCall.Block` field and block-rendering branch |
| `website/docs/implementation/0056/index.md` | Runtime matrix updated to mark CRuby 3.2 / 3.4 as LANDED via CI; JRuby / TruffleRuby / mruby renamed as sub-phases 29.1 / 29.2 / 29.3 |
| `website/docs/implementation/0056/phase-29-runtime-matrix.md` | This tracking page |
| `website/sidebars.js` | Phase 29 entry appended after phase 28 |

## Sub-phases (not yet landed)

| Sub-phase | Target | Status | Prerequisite |
|-----------|--------|--------|--------------|
| 29.1 | JRuby 10 (CRuby-compatible subset) | NOT STARTED | Container image entry in `transpiler3-ruby-test.yml` plus `$MOCHI_JRUBY` env var detection in `build.go` `resolveToolchain` |
| 29.2 | TruffleRuby 33 | NOT STARTED | Container image entry plus `$MOCHI_TRUFFLERUBY` env var detection |
| 29.3 | mruby 4 (language subset only, no threads / agents / streams) | NOT STARTED | `MRUBY_HOME` and `MOCHI_MRBC` env var detection plus a sub-corpus marker on phases 10 / 11 / 12 / 13 / 21 to opt out |

Each sub-phase keeps the same gate template: the new runtime appears as a `continue-on-error` matrix entry first (non-blocking), and only promotes to blocking once all per-phase tests pass on it.

## Test set

No new Go test functions are added. The phase is validated by:

- `go build ./transpiler3/ruby/...` succeeds after dead-code removal.
- `go vet ./transpiler3/ruby/...` succeeds.
- `go test -v ./transpiler3/ruby/...` succeeds locally and in CI on CRuby 3.2 / 3.4 / 3.4-macos via `transpiler3-ruby-test.yml`.

## Closeout notes

Phase 29 is the audit-4 follow-up to phase 28's gap closure. Phase 28 added tests for `Driver.Build` error paths, edge-case coverage, and emitted-syntax checks; phase 29 closes the umbrella phase coverage rule (CRuby 3.2 / 3.4 in CI) and the dead-code residue (`Lambda`, `BlockLit`) that phase 28's emit audit surfaced. The MEP-56 §6 runtime matrix now matches reality: CRuby 3.2 / 3.4 are CI-green; JRuby / TruffleRuby / mruby are sub-phases waiting on a toolchain detection refactor.
