---
title: "Phase 22. TargetRubyGem"
sidebar_position: 26
sidebar_label: "Phase 22. TargetRubyGem"
description: "MEP-56 Phase 22, emit a RubyGems layout (gemspec + lib) so gem build produces a publishable .gem artefact."
---

# Phase 22. TargetRubyGem

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 59c6cb5f0a |

## Gate

`TestPhase22TargetRubyGem` in `transpiler3/ruby/build/phase22_test.go`: builds `hello_gem.mochi` (`print("hi from gem")`) via `Driver.Build` with `TargetRubyGem`, asserts both `lib/hello_gem.rb` and `hello_gem.gemspec` exist, asserts `lib/hello_gem.rb` contains the `hi from gem` literal, asserts the gemspec contains the substrings `s.name`, `"hello_gem"`, `lib/hello_gem.rb`, and `mochi-runtime`, then shells out to `gem build hello_gem.gemspec` and asserts a `hello_gem-0.1.0.gem` artefact lands beside the spec. The test resolves `gem` from `dirname(tc.Ruby)` first and falls back to `$PATH`; if neither resolves the test skips rather than fails so contributors without a full Ruby toolchain are not blocked.

## Build target / audit decisions

`buildGem` in `transpiler3/ruby/build/build.go` (lines 229 to 258) writes exactly two files under `out`: `lib/<name>.rb` (the lowered Ruby straight out of `sf.RubySource()`) and `<name>.gemspec`. The gemspec is a here-printed `Gem::Specification.new` block with `s.name` set to the file base, `s.version = "0.1.0"`, `s.required_ruby_version = ">= 3.2"`, `s.files = ["lib/<name>.rb"]`, `s.require_paths = ["lib"]`, and `s.add_runtime_dependency "mochi-runtime", ">= 0.1"`. The 0.1.0 version is fixed because the build target does not know what release the caller wants. Callers are expected to rewrite the version line (or pass `--version` to `gem build`) before publishing. The runtime dependency is pinned to `>= 0.1` rather than an exact version so a downstream gem can resolve against any compatible mochi-runtime once published to rubygems.org. Ruby 3.2 is the floor because `Data.define` (used by every record lowering) does not exist in 3.1 or earlier.

The layout deliberately mirrors what `bundle gem` produces, so a user can run `gem build` immediately and `gem push` once they have credentials. Nothing in the layout is gem-internal scaffolding (no `Rakefile`, no `bin/`, no `README.md`) because the emitted code is library code, not a CLI; users adding a CLI entry point edit the gemspec after generation.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildGem` writes `lib/<name>.rb` + `<name>.gemspec` (lines 229 to 258); `Driver.Build` dispatches `TargetRubyGem` to it (line 205) |
| `transpiler3/ruby/build/phase22_test.go` | `TestPhase22TargetRubyGem` build + spec assertions + live `gem build` |

## Test set

- `TestPhase22TargetRubyGem` (single test, no subtests): asserts files exist, gemspec contains expected substrings, and `gem build hello_gem.gemspec` produces `hello_gem-0.1.0.gem`.

## Closeout notes

Phase 22 landed with the live `gem build` invocation rather than a static gemspec lint. The reason is that `gem build` performs its own internal validation (required fields, file presence, license SPDX validity) and emits a real `.gem`, which proves the layout is publishable, not just syntactically plausible. The fallback path to `$PATH` for `gem` was added after the first CI run on a host where `gem` lived in `/usr/bin` rather than next to `ruby`. The runtime dependency string locks future contributors out of accidentally dropping `mochi-runtime`, which would silently break every emitted gem since the lowered code calls `Mochi::Runtime` helpers.
