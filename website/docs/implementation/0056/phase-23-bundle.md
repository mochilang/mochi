---
title: "Phase 23. TargetRubyBundle"
sidebar_position: 27
sidebar_label: "Phase 23. TargetRubyBundle"
description: "MEP-56 Phase 23, emit a Bundler-managed script layout (Gemfile + script) for bundle exec ruby."
---

# Phase 23. TargetRubyBundle

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 15293215af |

## Gate

`TestPhase23TargetRubyBundle` in `transpiler3/ruby/build/phase23_test.go`: builds `hello_bundle.mochi` (`print("hi from bundle")`) via `Driver.Build` with `TargetRubyBundle`, asserts both `hello_bundle.rb` and `Gemfile` exist under `out`, asserts the script contains the `hi from bundle` literal, asserts the Gemfile contains `source "https://rubygems.org"`, `gem "mochi-runtime"`, and `ruby ">= 3.2"`, then runs a Ruby probe that calls `Bundler::Definition.build(Pathname.new(ARGV[0]), nil, false)` on the Gemfile and asserts the resolved dependency list contains `mochi-runtime`. The probe runs offline (`nil` lockfile, `false` for ignoring lockfile updates) so no rubygems.org traffic happens during the test.

## Build target / audit decisions

`buildBundle` in `transpiler3/ruby/build/build.go` (lines 270 to 290) writes `<name>.rb` (the lowered Ruby) and a fixed `Gemfile` whose body is `source "https://rubygems.org"`, `ruby ">= 3.2"`, and `gem "mochi-runtime", ">= 0.1"`. Bundler is in stdlib for Ruby 3.2+, so the user only needs `bundle install` plus `bundle exec ruby <name>.rb` to run. The source URL is hard-coded to the canonical rubygems index because Bundler refuses to resolve gems without an explicit source line, and any private gem server can be substituted after generation.

The script lives at the package root (not under `lib/`) because TargetRubyBundle is meant to be a runnable script layout, not a gem. A gem layout is the job of `TargetRubyGem` (Phase 22). Splitting the targets keeps each output minimal: `TargetRubyBundle` is the dev-iteration target (one Gemfile, one script, `bundle exec`), `TargetRubyGem` is the publish target (gemspec, `lib/`, `gem build`).

The `ruby ">= 3.2"` constraint matches the `s.required_ruby_version` in `buildGem`. Both targets share the same floor because the lowered Ruby uses `Data.define` and pattern matching with `case/in`, neither of which is available pre-3.2.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildBundle` writes `<name>.rb` + `Gemfile` (lines 270 to 290); `Driver.Build` dispatches `TargetRubyBundle` to it (line 207) |
| `transpiler3/ruby/build/phase23_test.go` | `TestPhase23TargetRubyBundle` build + Gemfile substring + Bundler probe |

## Test set

- `TestPhase23TargetRubyBundle` (single test, no subtests): asserts files exist, Gemfile contains expected pins, and `Bundler::Definition.build` resolves `mochi-runtime` in the dependency set.

## Closeout notes

Phase 23 deliberately validates the Gemfile by parsing it through real Bundler rather than by string matching alone. Bundler's parser catches malformed source lines, ambiguous Ruby constraints, and gem-name typos that a regex would miss. The `Bundler::Definition.build(path, lockfile=nil, unlock=false)` form bypasses lockfile resolution so the test runs without network access and without writing `Gemfile.lock` to the temp dir. The shared Gemfile body between this target and `buildTebakoPackage` (Phase 25) was a conscious choice: both targets need the mochi-runtime resolution path, so duplicating the literal keeps each function self-contained and avoids a helper that would only be called twice.
