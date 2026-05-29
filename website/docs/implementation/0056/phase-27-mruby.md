---
title: "Phase 27. TargetMRuby"
sidebar_position: 31
sidebar_label: "Phase 27. TargetMRuby"
description: "MEP-56 Phase 27, emit an mruby compile layout (build_config.rb + mruby_build.sh) for embedded bytecode and binary builds."
---

# Phase 27. TargetMRuby

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 64bccce8c5 |

## Gate

`TestPhase27TargetMRuby` in `transpiler3/ruby/build/phase27_test.go`: builds `hello_mruby.mochi` (`print("hi from mruby")`) via `Driver.Build` with `TargetMRuby`, asserts `hello_mruby.rb`, `build_config.rb`, and `mruby_build.sh` exist under `out`, asserts the script contains the `hi from mruby` literal, asserts `build_config.rb` contains `MRuby::Build.new`, `toolchain :gcc`, `gembox 'default'`, and `conf.bins = ['hello_mruby']`, asserts `mruby_build.sh` contains `#!/usr/bin/env bash`, `set -euo pipefail`, `"$MRBC" -o "hello_mruby.mrb" "hello_mruby.rb"`, `MRUBY_HOME`, `MOCHI_MRBC`, and `rake`, asserts the executable bit on the build script, and runs `bash -n mruby_build.sh` via `checkBashSyntax`. mruby itself is not invoked.

## Build target / audit decisions

`buildMRuby` in `transpiler3/ruby/build/build.go` (lines 450 to 494) writes three artefacts: `<name>.rb` (the lowered Ruby), `build_config.rb` (the mruby build descriptor), and `mruby_build.sh` (mode `0o755`). The build config wraps an `MRuby::Build.new do |conf| ... end` block with `toolchain :gcc`, `conf.gembox 'default'` (pulls in the standard mruby gembox), `conf.gem core: 'mruby-bin-mrbc'` and `core: 'mruby-bin-mruby'` (the bytecode compiler and runtime binaries), `conf.gem '#{__dir__}'` (drops the emitted script into the build as a gem entry), and `conf.bins = ['<name>']` (names the produced binary).

`mruby_build.sh` first runs `"$MRBC" -o "<name>.mrb" "<name>.rb"` to produce mruby bytecode, which is the minimum useful artefact for embedded use. If `MRUBY_HOME` points at an mruby source checkout, the script additionally copies `build_config.rb` into `$MRUBY_HOME`, runs `rake` to drive a full mruby build, and copies the resulting binary out to the user's working directory. The `MOCHI_MRBC` override lets the user point at a `mrbc` outside `$PATH` without disturbing other tooling.

The split between "always produce `.mrb`" and "optionally produce a binary" matches how mruby is actually used: embedded developers want bytecode to link into a host application, while standalone CLI users want a binary. Forcing a full binary build would require an mruby checkout for every invocation, which is a heavy dependency for the embedded path.

mruby targets the supported language subset only. mruby does not implement `Data.define`, `case/in` pattern matching, refinements, or several other features the full CRuby lowering uses. Sources that depend on records, sum-type matches, or query expressions will compile to `.rb` and to bytecode but will not run; users who need those features must use one of the CRuby-targeted build targets. The Mochi compile pipeline does not currently emit a diagnostic for unsupported features when `TargetMRuby` is selected; the failure surfaces at `mrbc` time or at `mruby` runtime.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildMRuby` writes `<name>.rb` + `build_config.rb` + executable `mruby_build.sh` (lines 450 to 494); `Driver.Build` dispatches `TargetMRuby` (line 215) |
| `transpiler3/ruby/build/phase27_test.go` | `TestPhase27TargetMRuby` file existence + content + mode + `bash -n` assertions |

## Test set

- `TestPhase27TargetMRuby` (single test, no subtests): asserts all three artefacts exist, the script contains the print literal, `build_config.rb` contains the four required MRuby::Build directives, `mruby_build.sh` contains the mrbc invocation plus env-var hooks, the executable bit is set, and `bash -n mruby_build.sh` parses.

## Closeout notes

Phase 27 ships without an integration test against a real mruby checkout. The reason: mruby builds need `rake`, a C toolchain, and at least 200 MB of checkout, none of which is available in the lightweight CI image. The bash-syntax + content assertions catch template regressions, which is the failure mode the test is designed to surface. Users who want bytecode-only output (no binary) can ignore `MRUBY_HOME` entirely; the script's `if [[ -n "$MRUBY_HOME" && -d "$MRUBY_HOME" ]]` guard means the `.mrb` is always produced and the `rake` step only runs when feasible. The mruby language-subset caveat is documented here rather than enforced in the lowerer because the supported subset is a moving target (mruby gembox configuration affects what's available), and a stale compile-time check would block valid programs.
