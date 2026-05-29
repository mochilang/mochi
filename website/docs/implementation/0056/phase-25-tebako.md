---
title: "Phase 25. TargetTebako"
sidebar_position: 29
sidebar_label: "Phase 25. TargetTebako"
description: "MEP-56 Phase 25, emit a Tebako packing layout (root tree + executable press.sh) for single-file binary production."
---

# Phase 25. TargetTebako

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 89202f961a |

## Gate

`TestPhase25TargetTebako` in `transpiler3/ruby/build/phase25_test.go`: builds `hello_tebako.mochi` (`print("hi from tebako")`) via `Driver.Build` with `TargetTebako`, asserts `root/hello_tebako.rb`, `root/Gemfile`, and `press.sh` all exist, asserts the script contains the `hi from tebako` literal, asserts the Gemfile contains `gem "mochi-runtime"`, asserts `press.sh` contains every load-bearing fragment (`#!/usr/bin/env bash`, `set -euo pipefail`, `tebako`, `--entry-point=hello_tebako.rb`, `--output=/mnt/w/hello_tebako`, `--root=/mnt/w/root`, `MOCHI_TEBAKO_IMAGE`, `MOCHI_TEBAKO_RUBY`), asserts `press.sh` has the executable bit set (`mode.Perm() & 0o111 != 0`), and finally runs `bash -n press.sh` via the `checkBashSyntax` helper to confirm the script parses. Docker and Tebako itself are not invoked; the gate is "layout is correct and the press script is syntactically valid bash".

## Build target / audit decisions

`buildTebakoPackage` in `transpiler3/ruby/build/build.go` (lines 347 to 389) writes three artefacts: `root/<name>.rb` (the lowered Ruby), `root/Gemfile` (identical body to the bundle target: `source "https://rubygems.org"`, `ruby ">= 3.2"`, `gem "mochi-runtime", ">= 0.1"`), and `press.sh` (mode `0o755`) at the package root. The split between `root/` (the Tebako input tree) and `press.sh` (the host-side driver) matches Tebako's expected layout: `tebako press` runs against a `--root` directory and writes the output binary elsewhere.

The press script docker-runs `${MOCHI_TEBAKO_IMAGE:-ghcr.io/tamatebako/tebako-ubuntu-20.04:latest}` with `-v "$HERE":/mnt/w`, invoking `press --root=/mnt/w/root --entry-point=<name>.rb --output=/mnt/w/<name> --Ruby="$RUBY_VERSION"` where `$RUBY_VERSION` defaults to `3.3.7` via `${MOCHI_TEBAKO_RUBY:-3.3.7}`. Both env-var overrides exist so users on air-gapped networks can mirror the container image and pin a specific Ruby release independently of the upstream Tebako image's default. `set -euo pipefail` is mandatory: without `-e` the script silently ignores Docker failures, and without `pipefail` a Docker exit code can be masked by a successful `tee` later if a user pipes the output.

The mode `0o755` is intentional. Without the executable bit, the user has to `chmod +x press.sh` before running, which is friction; with it, `./press.sh` works immediately. The test asserts the bit because Go file modes are notoriously easy to lose across `cp` and tar round-trips.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `buildTebakoPackage` writes `root/<name>.rb`, `root/Gemfile`, executable `press.sh` (lines 347 to 389); `Driver.Build` dispatches `TargetTebako` (line 211) |
| `transpiler3/ruby/build/phase25_test.go` | `TestPhase25TargetTebako` file existence + content + mode + `bash -n` assertions; `checkBashSyntax` helper |

## Test set

- `TestPhase25TargetTebako` (single test, no subtests): asserts artefact files exist, script and Gemfile contain expected literals, `press.sh` contains every required Docker / Tebako fragment, the executable bit is set, and `bash -n press.sh` parses.

## Closeout notes

Phase 25 ships with `bash -n` as the only "execution" of the press script; running real Tebako requires Docker plus the tamatebako image, neither of which is part of the test harness. The split was deliberate: layout + bash syntax can be validated everywhere, end-to-end binary production cannot. The two `MOCHI_TEBAKO_*` env vars exist so the test harness could in principle stub the image, but in practice the test skips the docker step entirely. The `checkBashSyntax` helper (defined at the top of the test file) was extracted because `buildTruffleNative` (Phase 26) and `buildMRuby` (Phase 27) both also emit `bash -n`-validated shell scripts, and centralising the lookup logic avoids three near-identical helpers.
