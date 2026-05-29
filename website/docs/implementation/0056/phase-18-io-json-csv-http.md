---
title: "Phase 18. File I/O, JSON, CSV, HTTP"
sidebar_position: 22
sidebar_label: "Phase 18. File I/O, JSON, CSV, HTTP"
description: "MEP-56 Phase 18, file / JSON / CSV / HTTP builtins lowered to Ruby stdlib calls."
---

# Phase 18. File I/O, JSON, CSV, HTTP

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 3890f4bd77 |

## Gate

`TestPhase18IO` in `transpiler3/ruby/build/phase18_test.go`: seven subtests (`read_file`, `write_file`, `append_file`, `lines`, `json_decode`, `load_csv`, `http_get`). Each subtest seeds a temp data file (or stands up a local `httptest.Server`), compiles a Mochi program that exercises one I/O builtin against the seeded path, runs the emitted `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against a recorded expectation (e.g. `load_csv` reads `a,b,c\n1,2,3\n` and prints `a\n3\n`). The `http_get` subtest uses a real `httptest.NewServer` so the lowered Ruby is exercised end-to-end against an actual HTTP socket, not a mock.

## Lowering decisions

File I/O lowers to direct `File` class methods, JSON / CSV / HTTP lower to inline `require ...; ...` expressions so the relevant stdlib is loaded lazily only when the program actually uses it (`transpiler3/ruby/lower/lower.go` lines 287 to 317 and 1054 to 1083):

- `aotir.WriteFileStmt` to `RawStmt` rendering `File.write(path, content)` (lines 287 to 295). `File.write` opens, writes, and closes in one call, matching Mochi's atomic-write semantics.
- `aotir.AppendFileStmt` to `RawStmt` rendering `File.open(path, 'a') { |__f| __f.write(content) }` (lines 297 to 305). The block form auto-closes the handle on exit; `'a'` is the append mode flag.
- `aotir.SaveCSVStmt` to `RawStmt` rendering `(require 'csv'; CSV.open(path, 'w') { |__c| (data).each { |__row| __c << __row } })` (lines 306 to 317); covered more deeply by Phase 21.
- `aotir.ReadFileExpr` to `RawExpr` rendering `File.read(path)` (lines 1054 to 1059). Returns the file's full content as a single Ruby `String`.
- `aotir.LinesExpr` to `RawExpr` rendering `File.readlines(path, chomp: true)` (lines 1060 to 1065). The `chomp: true` keyword (Ruby 2.4+) strips trailing newlines so each list element is the bare line, matching Mochi's `lines()` contract.
- `aotir.HttpGetExpr` to `RawExpr` rendering `(require 'open-uri'; URI.parse(url).open.read)` (lines 1066 to 1071). `open-uri` patches `URI` so `URI#open` follows HTTP redirects and returns an `IO`; `.read` then drains the body. This is simpler than `Net::HTTP.get` for plain GET requests and handles `https` URLs out of the box via the patched URI.
- `aotir.LoadCSVExpr` to `RawExpr` rendering `(require 'csv'; CSV.read(path))` (lines 1072 to 1077). `CSV.read` returns `Array<Array<String>>`, matching Mochi's `list<list<string>>` from `loadCSV`.
- `aotir.JsonDecodeExpr` to `RawExpr` rendering `(require 'json'; JSON.parse(input))` (lines 1078 to 1083). Returns Ruby `Hash`/`Array`/scalar trees aligned with Mochi's `any`-typed JSON decode result.

The inline `(require 'x'; expr)` pattern is deliberate: it pushes the `require` cost to the first use of each subsystem at runtime, keeping the static prelude minimal (`require "mochi/runtime"` only). Ruby's `require` is idempotent so a hot loop pays the cost once.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | File I/O statements (lines 287 to 317) and `ReadFile`/`Lines`/`HttpGet`/`LoadCSV`/`JsonDecode` expressions (lines 1054 to 1083) lower to Ruby stdlib calls |
| `transpiler3/ruby/build/phase18_test.go` | `TestPhase18IO` with 7 subtests including a live `httptest.Server` for the HTTP path |

## Test set

- `TestPhase18IO/read_file`, `write_file`, `append_file`, `lines`, `json_decode`, `load_csv`, `http_get`.

## Closeout notes

Phase 18 landed on CRuby 4.0 (Homebrew) with all seven subtests green. `open-uri` (not `Net::HTTP.get`) is the deliberate choice for `fetch URL`: `open-uri` handles redirects, HTTPS, and basic auth headers without extra glue and is in the stdlib on every supported runtime. `chomp: true` in `File.readlines` is the one place where Mochi diverges from Ruby's default (which keeps trailing `\n`); without it, `lines("first\nsecond\n")` would return `["first\n", "second\n"]` instead of the expected `["first", "second"]`.
