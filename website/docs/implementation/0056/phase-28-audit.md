---
title: "Phase 28. Audit gap closure (driver errors, edge cases, emitted-syntax checks)"
sidebar_position: 32
sidebar_label: "Phase 28. Audit gap closure (driver errors, edge cases, emitted-syntax checks)"
description: "MEP-56 Phase 28, audit sweep covering Driver.Build failure paths, scalar/collection/query edge cases, and ruby -c on every emitted .rb."
---

# Phase 28. Audit gap closure (driver errors, edge cases, emitted-syntax checks)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | d43705737a |

## Gate

Three top-level Go tests close the audit gaps left by the per-feature phases:

- `TestPhase28DriverErrorPaths` (`phase28_driver_errors_test.go`): five subtests cover `Driver.Build` failure branches (parse error, typecheck error, missing source file, unknown target, output-dir is a file). Each subtest asserts a non-nil error and (for the four with stable wording) asserts the message names the failing stage (`parse`, `typecheck`, `read`/`no such file`/`parse`, `not implemented`).
- `TestPhase29EdgeCases` (`phase29_edge_cases_test.go`): 30 subtests build and execute Mochi sources covering collection-boundary, scalar, string-Unicode, query, closure-capture, and previously-uncovered IR-node scenarios (`BreakStmt`, `ContinueStmt`, `MapValuesExpr`, `UnNotBool`, `Panic#code` round-trip, `ListSortAscExpr`, `ListSliceExpr`, `MathCallExpr "ceil"`, `StrConvertExpr`, `NumCastExpr`, `MatchStmt Default arm`, chained `elsif` folding, integer-valued float print, `Mochi::Runtime::VERSION` constant load, `Panic#message` channel, locked-in `BinDivI64` floor-div behaviour) that the per-feature phase tests intentionally kept happy-path only.
- `TestPhase30TargetEmittedSyntax` (`phase30_target_syntax_test.go`): seven subtests run `ruby -c` against the `.rb` every non-source build target emits, plus a JSON.parse round-trip against the IRuby notebook and a `.gem` archive walk that asserts `lib/<name>.rb` is present byte for byte.
- `TestPhase31Integration` (`phase31_integration_test.go`): five cross-cutting subtests exercise multi-feature programs (`closure_over_query_result`, `nested_hof_filter_then_map_then_reduce`, `try_catch_around_record_field_access`, `list_of_records_query`, `match_arm_returns_distinct_per_variant`) that touch two or more phases at once, catching regressions per-feature tests would miss.

Every subtest skips if `resolveToolchain` cannot find a Ruby; the `.gem` subtest additionally falls back to `$PATH` for `gem` or skips.

## Build target / audit decisions

The audit phase does not change `build.go`. The audit tests exercise the existing `Driver.Build` switch (lines 195 to 217) and the per-target functions (`buildGem` lines 229 to 258, `buildBundle` lines 270 to 290, `buildIRubyNotebook` lines 296 to 335, `buildTebakoPackage` lines 347 to 389, `buildTruffleNative` lines 400 to 438, `buildMRuby` lines 450 to 494) and validate their outputs end to end.

Three audit decisions are baked into the test design:

1. **Error-message contracts as a public surface.** `TestPhase28DriverErrorPaths` asserts not just that an error is returned but that the message contains a stage-naming substring (`parse`, `typecheck`, `not implemented`). This locks the error wording into the public contract, so a refactor that renames the stages must update the user-facing message at the same time.
2. **Edge cases through the same execution harness as the happy-path tests.** `TestPhase29EdgeCases` uses the existing `runRubyFixture(t, tc, runtimeLib, name, src, want)` helper, so an edge-case regression looks identical to a per-feature regression and uses the same `-I mochi-runtime/lib` invocation. The fixtures cover empty `list<int>`, empty `string`, negative arithmetic, a near-int63 integer, Unicode (`héllo` round-tripping through `len` and `upper`), single-element and nested lists, multi-key map and omap round-trips, `keys()` cardinality, zero-iteration `for i in 0..0` and zero-iteration `while`, a sum-type with three arms exercised through all three (`Pos`, `Neg`, `Zero`), and capture-by-value semantics for a closure over a `let`.
3. **Syntactic validation that does not require the heavy toolchains.** `TestPhase30TargetEmittedSyntax` runs `ruby -c` against the emitted `.rb` for `TargetRubyGem`, `TargetRubyBundle`, `TargetTebako`, `TargetTruffleNative`, and `TargetMRuby`. Notebook JSON validity is exercised via a `JSON.parse` probe. The `.gem` archive is unpacked via `inspectGemContainsLib` (defined at the bottom of `phase30_target_syntax_test.go`, lines 182 to 219), which uses `archive/tar` to walk the outer tar, identifies `data.tar.gz`, wraps it in `compress/gzip`, and walks the inner tar looking for `lib/<name>.rb`. The two-layer tar (outer plain + inner gzipped) is the documented RubyGems `.gem` format (the outer also contains `metadata.gz` and `checksums.yaml.gz`).

The shared exercise source in `TestPhase30TargetEmittedSyntax` (a `Sign` sum type, `classify` function, `name` matcher, and a `from x in xs select` query) is non-trivial on purpose: any syntactic glitch in record / sum / query lowering shows up under `ruby -c`. A trivial `print("hi")` source would pass `ruby -c` even with broken record lowering since no records would be emitted.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/phase28_driver_errors_test.go` | `TestPhase28DriverErrorPaths` (5 subtests): parse / typecheck / missing-file / unknown-target / out-dir-is-file |
| `transpiler3/ruby/build/phase29_edge_cases_test.go` | `TestPhase29EdgeCases` (30 subtests): collection, scalar, string, query, closure boundary scenarios plus uncovered IR nodes + runtime gem assertions (VERSION, Panic#message) + locked floor-div divergence |
| `transpiler3/ruby/build/phase22_test.go` | Expanded gemspec substring assertions for `s.version`, `s.license`, `required_ruby_version`, `summary`, `authors`, `add_runtime_dependency` constraint, `require_paths` |
| `transpiler3/ruby/build/phase23_test.go` | Gemfile assertions for `mochi-runtime ">= 0.1"` version pin and `# frozen_string_literal: true` header |
| `transpiler3/ruby/build/phase24_test.go` | Notebook assertions for `nbformat_minor` (5), `kernelspec.display_name`, `language_info.file_extension`, cell `execution_count == nil` |
| `transpiler3/ruby/build/phase25_test.go` | Tebako press.sh assertions for default image (`ghcr.io/tamatebako/...`), default Ruby version (`3.3.7`), `--Ruby=` flag, full Gemfile |
| `transpiler3/ruby/build/phase26_test.go` | TruffleNative assertions for `--initialize-at-build-time` flag and both error-message branches when `GRAAL_HOME` / `native-image` are missing |
| `transpiler3/ruby/build/phase27_test.go` | MRuby build_config.rb assertions for `conf.gem core: 'mruby-bin-mrbc'`, `mruby-bin-mruby`, and `conf.gem '#{__dir__}'` directives |
| `transpiler3/ruby/lower/lower.go` | Floor-div divergence flagged inline at line 1187 (no code change); covered by lock-in test |
| `website/docs/mep/mep-0056.md` | §3 surface-syntax table extended with anonymous record `type Pair = { … }`, struct-literal agent constructor, `set{…}` and `omap<K,V>` annotation forms, `fetch` keyword form, `json_decode` snake_case, `int(x)` cast form, BinDivI64 known-divergence row |
| `website/docs/implementation/0056/phase-02-scalars.md` | Floor-div divergence documented with the lock-in test name |
| `website/docs/implementation/0056/phase-09-datalog.md` | Subtest count corrected from 3 to 5 |
| `transpiler3/ruby/build/phase30_target_syntax_test.go` | `TestPhase30TargetEmittedSyntax` (7 subtests): `ruby -c` on every emitted `.rb`, notebook JSON probe, `.gem` archive walk via `inspectGemContainsLib` helper |
| `transpiler3/ruby/build/phase31_integration_test.go` | `TestPhase31Integration` (5 subtests): cross-cutting programs touching closures + queries + records + try/catch + match together |
| `transpiler3/ruby/build/phase09_test.go` | `dl_neq_constraint` and `dl_not_negation` subtests added for previously-untested `IsNeq` and `IsNot` paths in `datalog.go` |

## Test set

`phase28_driver_errors_test.go`:

- `TestPhase28DriverErrorPaths/parse_error`, `typecheck_error`, `missing_file`, `unknown_target`, `out_dir_is_file`.

`phase29_edge_cases_test.go`:

- `TestPhase29EdgeCases/empty_list_len`, `empty_string_len`, `negative_arithmetic`, `large_integer`, `unicode_string_ops`, `list_with_one_element`, `nested_list`, `map_get_missing_default`, `omap_round_trip_multi_key`, `map_keys_iter_yields_all`, `for_range_zero_iterations`, `while_loop_zero_iterations`, `sum_neg_zero_pos_all_arms`, `closure_capture_by_value`, `break_exits_for_range`, `continue_skips_iteration`, `map_values_len`, `not_bool`, `panic_code_carries_through_catch`, `list_sort_via_query`, `list_slice_subscript`, `math_ceil`, `str_convert_float`, `int_cast_from_float`, `match_default_arm`, `if_else_if_else_chain`, `integer_valued_float_print`, `runtime_version_constant_loads`, `panic_message_arg_surfaces`, `negative_int_floor_div_known_divergence`.

`phase30_target_syntax_test.go`:

- `TestPhase30TargetEmittedSyntax/gem`, `bundle`, `tebako`, `truffle_native`, `mruby`, `notebook_json_round_trip`, `gem_unpack_round_trip`.

`phase31_integration_test.go`:

- `TestPhase31Integration/closure_over_query_result`, `nested_hof_filter_then_map_then_reduce`, `try_catch_around_record_field_access`, `list_of_records_query`, `match_arm_returns_distinct_per_variant`.

`phase09_test.go` (additions):

- `TestPhase9Datalog/dl_neq_constraint`, `dl_not_negation`.

## Closeout notes

Phase 28 was the gap-closure sweep before the MEP closed. The three test files are intentionally separated by audit theme so future contributors can find the right slot for a new regression. Driver errors go in `phase28_driver_errors_test.go`, language-edge cases go in `phase29_edge_cases_test.go`, build-target output checks go in `phase30_target_syntax_test.go`. The `.gem` walk via `archive/tar` + `compress/gzip` was chosen over shelling out to `gem unpack` because the latter writes to disk and races against the build temp dir, while `inspectGemContainsLib` runs entirely in memory and is deterministic. The `large_integer` literal (`9223372036854775000`) sits just inside int63 to confirm the parser does not truncate near the boundary, and `unicode_string_ops` covers `len` on a 5-character string with one non-ASCII codepoint to confirm Ruby's `String#length` is character-based (which is true on 3.2+ but was not always the case historically).
