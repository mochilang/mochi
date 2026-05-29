---
title: "Phase 30. mochi-runtime gem unit tests + gem-build CI (audit-5)"
sidebar_position: 34
sidebar_label: "Phase 30. mochi-runtime gem unit tests + gem-build CI (audit-5)"
description: "MEP-56 Phase 30, add the mochi-runtime gem's own minitest suite (Version, IO, Panic, Stream / LimitedQueue) and wire gem build mochi-runtime.gemspec into transpiler3-ruby-test.yml."
---

# Phase 30. mochi-runtime gem unit tests + gem-build CI (audit-5)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 14:13 (GMT+7) |
| Landed         | 2026-05-29 14:13 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | c1fe1b55ef |

## Gate

Two new CI steps land alongside four new minitest files. The phase is green when all four steps below pass on each Ruby matrix entry (CRuby 3.2 / 3.4 on ubuntu, CRuby 3.4 on macos):

1. `ruby -Ilib -Itest test/test_version.rb` (3 runs / 5 assertions): VERSION defined, SemVer-shaped (`/\A\d+\.\d+\.\d+\z/`), frozen (matches `frozen_string_literal: true`).
2. `ruby -Ilib -Itest test/test_io.rb` (7 runs / 16 assertions): `format_value` of int / fractional float / integer-valued float / nil / bool / string / Unicode; `putln` writes formatted value + newline to `$stdout`.
3. `ruby -Ilib -Itest test/test_panic.rb` (5 runs / 7 assertions): `Panic` inherits from `StandardError`; carries `code` and `message`; round-trips through `rescue Mochi::Runtime::Panic => e`; does not capture unrelated errors; `code` is read-only.
4. `ruby -Ilib -Itest test/test_stream.rb` (6 runs / 14 assertions): `subscribe` receives emitted values in order; late subscribers miss prior emits; multiple subscribers each get the full stream; `subscribe_limit(N)` silently drops past N; `LimitedQueue.pop` blocks until push; `LimitedQueue` silently drops above limit.
5. `gem build mochi-runtime.gemspec` succeeds and produces `mochi-runtime-<version>.gem`.

Local run before commit: **21 runs / 42 assertions / 0 failures**. Each `.rb` test file uses only stdlib `minitest/autorun` and the targeted lib file via `require_relative`, so the suite has zero external dependencies.

## Build target / audit decisions

The phase does not change `build.go`, the lower pass, or the runtime gem code. Three audit decisions are baked into the test design:

1. **Stdlib minitest, no Rakefile, no Gemfile.** The runtime gem stays a flat-file project: tests live under `mochi-runtime/test/`, each is runnable as a standalone script via `ruby -Ilib -Itest test/test_X.rb`. This keeps the gem trivial to build from a release artefact (no `bundle install` step in downstream consumers) and keeps the CI step explicit (one line per test file, no test-discovery magic).
2. **`gem build` validation belongs in CI, not in the transpiler test suite.** Phase 22 (`TargetRubyGem`) only validates the gemspec we *generate for the user* via `Driver.Build`; the *runtime gem's own* `mochi-runtime.gemspec` is consumed via `require_relative` in every test and is implicitly trusted. Phase 30 adds an explicit `gem build mochi-runtime.gemspec` so a future regression (missing files in `files`, bad metadata, dropped dependency) fails fast in CI rather than at publish time.
3. **Test the public contract that emitted Mochi code depends on.** Each test covers a behaviour that the transpiler's emit layer assumes: `format_value`'s float formatting (used by `print(float)` in phase 02), `Panic#code` round-trip (used by `try/catch` in phase 19), `Stream` ordering (used by phase 13), `subscribe_limit` drop semantics (used by phase 21). A regression in any of these would silently break the corresponding transpiler phase output.

## Files changed

| File | Purpose |
|------|---------|
| `mochi-runtime/test/test_version.rb` | 3 runs / 5 assertions: VERSION defined, SemVer-shaped, frozen |
| `mochi-runtime/test/test_io.rb` | 7 runs / 16 assertions: `format_value` over scalars + Unicode; `putln` to `$stdout` |
| `mochi-runtime/test/test_panic.rb` | 5 runs / 7 assertions: Panic is StandardError, carries code+message, rescuable, read-only code |
| `mochi-runtime/test/test_stream.rb` | 6 runs / 14 assertions: Stream ordering, late-subscriber miss, multi-sub, `subscribe_limit` drop, LimitedQueue push/pop semantics |
| `.github/workflows/transpiler3-ruby-test.yml` | Two new steps: "Run mochi-runtime gem unit tests" runs all four minitest files; "Validate mochi-runtime.gemspec packs cleanly" runs `gem build` and lists the artefact |
| `website/docs/implementation/0056/index.md` | Phase 30 row appended to phases table |
| `website/docs/implementation/0056/phase-30-runtime-gem-tests.md` | This tracking page |
| `website/sidebars.js` | Phase 30 entry appended after phase 29 |

## Test set

`mochi-runtime/test/`:

- `test_version.rb`: `test_version_constant_is_defined`, `test_version_is_semver_shaped`, `test_version_is_frozen`.
- `test_io.rb`: `test_format_int`, `test_format_float_integer_valued_gets_dot_zero`, `test_format_float_fractional`, `test_format_nil_renders_empty`, `test_format_bool`, `test_format_string_round_trips`, `test_putln_writes_to_stdout_with_newline`.
- `test_panic.rb`: `test_panic_is_standard_error`, `test_panic_carries_code_and_message`, `test_panic_round_trip_through_rescue`, `test_panic_does_not_rescue_other_errors`, `test_panic_code_is_read_only`.
- `test_stream.rb`: `test_subscribe_receives_emitted_values_in_order`, `test_late_subscriber_misses_prior_values`, `test_multiple_subscribers_each_get_full_stream`, `test_subscribe_limit_drops_after_threshold`, `test_limited_queue_pop_blocks_until_push`, `test_limited_queue_silently_drops_above_limit`.

## Closeout notes

Phase 30 was the audit-5 follow-up to phase 29's CI matrix. Phase 29 surfaced that the runtime gem had zero direct unit tests and zero `gem build` validation in CI; phase 30 fixes both. The 21 minitest runs use only stdlib `minitest/autorun` so no `Gemfile`, `bundler-cache`, or `rake` setup is required, keeping the CI step diff under 20 lines. `LimitedQueue.pop` is intentionally tested with a real `Thread.new` + 10ms `sleep` rather than mocked, because the underlying contract is that pop *blocks*, and a mocked queue would not catch a regression that replaced the blocking `Thread::Queue` with a polling alternative.
