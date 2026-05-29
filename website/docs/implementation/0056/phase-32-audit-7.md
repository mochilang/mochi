---
title: "Phase 32. License + dead-code + reproducibility expansion (audit-7)"
sidebar_position: 36
sidebar_label: "Phase 32. License + dead-code + repro expansion (audit-7)"
description: "MEP-56 Phase 32, align mochi-runtime gem license with the spec, expand TestPhase32Reproducibility to all 7 build targets, harden findRuby, gem-install smoke probe in CI, and cull dead Toolchain.Bundle / Driver.CacheDir / MethodParam.Default / dead test imports."
---

# Phase 32. License + dead-code + reproducibility expansion (audit-7)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 14:54 (GMT+7) |
| Landed         | 2026-05-29 14:54 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) follow-up |
| Commit         | 0a753708ad |

## Gate

Audit-7 lands seven surface changes driven by a fresh sweep against MEP-56 §2 to §6:

1. **`mochi-runtime.gemspec` license: MIT → Apache-2.0.** MEP-56 §1 and §4 declare `mochi-runtime` as Apache-2.0. The gemspec at line 6 declared MIT, contradicting both the spec and the emitted user-gem (`build.go:246` hardcodes Apache-2.0, asserted by `phase22_test.go:61`). Installed `mochi-runtime` and a Mochi-emitted gem would have ended up under different licenses. Repo root LICENSE remains MIT (governs the rest of the repo); the runtime gem is the spec-carved Apache-2.0 exception. `phase31_audit_6` surfaced this as a finding; audit-7 picks the side that matches the spec.

2. **`TestPhase32Reproducibility` expanded from 3 to 8 cases, covering all 7 build targets.** Bundle, IRubyKernel, Tebako, TruffleNative, and MRuby targets share the same `RubySource()`+`fmt.Sprintf` emission path as Source/Gem but were not previously byte-equality-gated. A future regression introducing a timestamp into `press.sh`, a tmpdir leak in the IRuby notebook JSON, or a map-iter non-determinism in `mruby_build.sh` will now fail the gate. All 5 added cases pass byte-equal across two builds locally.

3. **`findRuby` errors when `$MOCHI_RUBY` is set but points to an inaccessible binary.** Before, a typo or stale value silently fell through to Homebrew/PATH, masking the user's intent. With sub-phases 29.1/29.2/29.3 (JRuby/TruffleRuby/mruby) planned to pin specific interpreters via `MOCHI_RUBY`, a silent fall-through would route the runtime matrix back to CRuby and the regression would only surface as "tests look fine but the wrong runtime was tested". Now `MOCHI_RUBY` set + `os.Stat` failure returns `MOCHI_RUBY points to %q which is not accessible: ...`.

4. **CI gains a `gem install` + `require` smoke probe.** Previous workflow ran `gem build` and stopped there; a `mochi-runtime.gemspec` regression that dropped a required `lib/mochi/runtime/*.rb` file (e.g., from a wrong `Dir["lib/**/*.rb"]` glob) would still pass because unit tests ran against the source tree with `-Ilib`. Now CI installs the packed gem (`gem install --no-document`), `cd /` to escape the source tree, and `require`s each runtime sub-module. A regression in the gem's file list now fails CI with a `LoadError`.

5. **Dead `Toolchain.Bundle`, `Driver.CacheDir`, and `effectiveCacheDir()` culled.** `Toolchain.Bundle` was set at `build.go:77` from `exec.LookPath("bundle")` and never read. `Driver.CacheDir` was set by ~30 tests (`&Driver{CacheDir: t.TempDir()}`) and read only by `effectiveCacheDir()`, which was itself unreferenced. The cache-dir-as-tempdir test pattern was inherited from MEP-46 / MEP-47 boilerplate and is meaningless here. All 30+ test sites converted to plain `&Driver{}`.

6. **Dead `MethodParam.Default` (rtree) culled.** Field was never set anywhere in lower or emit; the `RubyString` branch that consumed it was unreachable. Dropped both the field and the branch.

7. **Dead `_ = strings.TrimSpace` + `strings` import in `phase18_test.go` removed.** Leftover unused-import suppressor from a prior refactor; `strings` was no longer used in that file.

## Audit findings not auto-fixed

Two findings from the audit-7 sweep are deferred to a future phase:

1. **Package-level mutable `agentsByName` in `transpiler3/ruby/lower/lower.go:16`.** Each `Lower()` call writes a global `map[string]*aotir.AgentDecl` then reads it from `lowerExpr`. Two concurrent `Driver.Build` invocations would race. No other transpiler3 backend ships state this way, and the Ruby driver does not document concurrency one way or the other. The proper fix is to thread a `lowerCtx` struct through the 29 `lower*` functions, which is a non-trivial refactor and out of scope for an audit-pass. No current code path exercises concurrent builds (no `t.Parallel()`, no `go func` in driver), so this is a latent-only hazard.

2. **`Mochi::Runtime::Stream` MPMC behavior is single-threaded-only in tests.** Spec §1 calls streams a "bounded MPMC broadcast channel". `test_stream.rb` exercises subscribe / subscribe_limit / late-subscriber-miss / multi-subscriber-broadcast but all from one thread. A multi-producer / multi-consumer test (`Thread.new` producers + consumers + `pop`) would catch a deadlock or lost message under contention. Out of scope here because it is additive coverage, not a known regression.

## Build target / audit decisions

Four decisions are baked into this phase:

1. **Pick the side that matches the spec when the divergence is documented there.** MEP-56 §1 explicitly carves out the runtime as Apache-2.0; the gemspec is the surface that was wrong, not the spec. Phase 31 left the call open; phase 32 closes it spec-side.
2. **Test every target in the same harness rather than carve sub-tests by target type.** The `snapshotEmittedFiles` walk is generic; adding 5 table rows is cheaper than maintaining 5 parallel tests and gives a uniform failure signal.
3. **CI smoke probe runs `gem install`, not just `gem build`.** A gem can pack without being installable (missing dependency, wrong required ruby version, broken `Dir[]` glob). The probe catches the install + load case in addition to the pack case.
4. **`MOCHI_RUBY` is a hard pin, not a hint.** When set, a missing binary is an error, not a "try elsewhere". The runtime matrix needs this guarantee.

## Files changed

| File | Purpose |
|------|---------|
| `mochi-runtime/mochi-runtime.gemspec` | License MIT → Apache-2.0 |
| `transpiler3/ruby/build/build.go` | Dropped `Toolchain.Bundle`, `Driver.CacheDir`, `effectiveCacheDir()`; `findRuby` errors on bad `MOCHI_RUBY` |
| `transpiler3/ruby/build/phase32_reproducibility_test.go` | Added 5 cases for Bundle, IRubyKernel, Tebako, TruffleNative, MRuby targets |
| `transpiler3/ruby/build/phase*_test.go` (30+ files) | Converted `&Driver{CacheDir: t.TempDir()}` to `&Driver{}` |
| `transpiler3/ruby/build/phase18_test.go` | Dropped dead `_ = strings.TrimSpace` and `strings` import |
| `transpiler3/ruby/rtree/nodes.go` | Dropped dead `MethodParam.Default` field + RubyString branch |
| `.github/workflows/transpiler3-ruby-test.yml` | Added `gem install` + `require` smoke probe step |
| `website/docs/implementation/0056/index.md` | Phase 32 row appended |
| `website/docs/implementation/0056/phase-32-audit-7.md` | This tracking page |

## Test set

- `TestPhase32Reproducibility` now 8 subtests, all PASS:
  - `hello_source`, `records_and_query`, `hello_gem` (existing), plus `hello_bundle`, `hello_iruby`, `hello_tebako`, `hello_truffle`, `hello_mruby`.
- Full `go test -timeout 600s ./transpiler3/ruby/...`: PASS (build 12.8s, rtree 0.5s).
- `mochi-runtime` minitest suite: 21 runs / 42 assertions / 0 failures.
- Local `gem install --user-install` + `require` smoke probe: PASS on CRuby 4.0.

## Closeout notes

Audit-7 is a small gap-closure pass driven by a fresh §1 to §6 read of MEP-56. The two scope-stretch findings (concurrency in `lower/lower.go`, MPMC stream test) are surfaced as known gaps; both would be appropriate as audit-8 or as standalone follow-up PRs. The license fix completes the audit-6 outstanding finding. The reproducibility expansion to 7 targets is the highest-leverage change: it closes the matrix coverage gap that phase 31 explicitly carved out as "low cost follow-up".
