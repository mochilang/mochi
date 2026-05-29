---
title: "Phase 26. TargetTruffleNative"
sidebar_position: 30
sidebar_label: "Phase 26. TargetTruffleNative"
description: "MEP-56 Phase 26, emit a TruffleRuby native-image build layout for AOT binary production via GraalVM."
---

# Phase 26. TargetTruffleNative

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

`TestPhase26TargetTruffleNative` in `transpiler3/ruby/build/phase26_test.go`: builds `hello_truffle.mochi` (`print("hi from truffle")`) via `Driver.Build` with `TargetTruffleNative`, asserts both `hello_truffle.rb` and `native_build.sh` exist under `out`, asserts the script contains the `hi from truffle` literal, asserts `native_build.sh` contains every load-bearing fragment (`#!/usr/bin/env bash`, `set -euo pipefail`, `--language:ruby`, `--no-fallback`, `-o "hello_truffle"`, `"hello_truffle.rb"`, `MOCHI_GRAALVM_HOME`, `GRAALVM_HOME`, `native-image`), asserts the executable bit is set, and runs `bash -n native_build.sh` via `checkBashSyntax`. GraalVM is not invoked since the binary toolchain is typically absent from CI hosts.

## Build target / audit decisions

`buildTruffleNative` in `transpiler3/ruby/build/build.go` (lines 400 to 438) writes `<name>.rb` (the lowered Ruby) at the package root and `native_build.sh` (mode `0o755`) beside it. The shell script resolves `GRAAL_HOME` from `${MOCHI_GRAALVM_HOME:-$GRAALVM_HOME}`, aborts with an explicit `>&2` error if neither is set, verifies `$GRAAL_HOME/bin/native-image` exists and is executable, then runs `"$NATIVE_IMAGE" --language:ruby --no-fallback --initialize-at-build-time -o "<name>" "<name>.rb"`.

`--no-fallback` is mandatory because without it native-image falls back to a regular JVM when AOT analysis cannot complete, producing a binary that still needs the JDK at runtime (defeating the point of single-file distribution). `--initialize-at-build-time` runs class initialisers during AOT compilation, which speeds startup and reduces image size; for pure-Ruby code without native extensions this is safe. `--language:ruby` activates the polyglot Ruby component, which must have been installed into GraalVM beforehand (`gu install ruby` on GraalVM 22.x, or shipped as part of the Oracle GraalVM distribution on 23.x+).

The two env-var precedence (`MOCHI_GRAALVM_HOME` falling back to `GRAALVM_HOME`) lets the user override the system-wide GraalVM with a Mochi-specific install without unsetting the platform variable. A single `GRAALVM_HOME` lookup would have been simpler but would not survive the common case of "I have one GraalVM for my Java work and a different one for TruffleRuby experiments".

The script intentionally lacks `bundle install`. TruffleRuby is a complete Ruby implementation but the native-image pipeline doesn't load gems from a Bundler-managed `vendor/` tree the same way CRuby does, so users with gem dependencies need to do additional work (compile the gem with `truffleruby`, then point native-image at the result). Emitting a one-shot `bundle install` line would silently produce broken binaries.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildTruffleNative` writes `<name>.rb` + executable `native_build.sh` (lines 400 to 438); `Driver.Build` dispatches `TargetTruffleNative` (line 213) |
| `transpiler3/ruby/build/phase26_test.go` | `TestPhase26TargetTruffleNative` file existence + content + mode + `bash -n` assertions |

## Test set

- `TestPhase26TargetTruffleNative` (single test, no subtests): asserts both artefacts exist, the script contains the print literal, `native_build.sh` contains every required native-image flag and env-var fragment, the executable bit is set, and `bash -n native_build.sh` parses.

## Closeout notes

Phase 26 cannot run a real native-image build in CI (GraalVM is too heavy and the Ruby component is only available on Linux x86_64 and macOS arm64 channels), so the gate stops at "the bash script is syntactically valid and contains the documented flags". The flag list in the test was chosen by walking the GraalVM 23.0 Ruby docs and pulling out the minimum set needed for a working AOT binary: `--language:ruby` selects the polyglot, `--no-fallback` forbids the JVM fallback, the `-o` flag names the output binary, and the positional argument names the source. Drop any of those and the test fails before a developer commits a broken script template.
