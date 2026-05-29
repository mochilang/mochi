---
title: "Phase 14. Deep string ops (index, contains, substring, reverse, upper, lower, split, join, str)"
sidebar_position: 18
sidebar_label: "Phase 14. Deep string ops"
description: "MEP-56 Phase 14, deep string builtins lowered to Ruby String / Array methods."
---

# Phase 14. Deep string ops (index, contains, substring, reverse, upper, lower, split, join, str)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 07eb070db3 |

## Gate

`TestPhase14StringDeep` in `transpiler3/ruby/build/phase14_test.go`: seven subtests (`str_index`, `str_contains`, `str_substring`, `str_reverse`, `str_upper_lower`, `str_split_join`, `str_convert`). Each subtest writes a Mochi source that exercises one string builtin, builds it via `Driver.Build` to a `.rb`, runs the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs combined stdout against the recorded expectation (e.g. `str_reverse` expects `ihcom\n` for `reverse("mochi")`).

## Lowering decisions

Each string IR node maps to the closest Ruby `String` / `Array` method so the runtime cost is one C-level call, not a wrapper. The mapping (`transpiler3/ruby/lower/lower.go` lines 568 to 652):

- `aotir.StrLenExpr` to `MethodCall{Method: "length"}` (lines 568 to 573); Ruby `String#length` counts characters under CRuby's encoding-aware default.
- `aotir.StrIndexExpr` to `RawExpr` rendering `recv[idx]` (lines 574 to 583); Ruby string indexing returns a 1-character substring (not a Fixnum), which matches Mochi's `string` element type.
- `aotir.StrContainsExpr` to `MethodCall{Method: "include?"}` with `UseParens: true` (lines 584 to 593).
- `aotir.StrSubstringExpr` to `RawExpr` rendering `(recv[start...end] || "")` (lines 594 to 608); the `|| ""` guards against `nil` when the start index is past end-of-string, matching Mochi's "empty slice" semantics.
- `aotir.StrReverseExpr` to `MethodCall{Method: "reverse"}` (lines 609 to 614).
- `aotir.StrConvertExpr` to `MethodCall{Method: "to_s"}` (lines 615 to 620); covers `str(n)` for both `int` and `float`, since every Ruby object responds to `to_s`.
- `aotir.StrUpperExpr` to `MethodCall{Method: "upcase"}` (lines 621 to 626).
- `aotir.StrLowerExpr` to `MethodCall{Method: "downcase"}` (lines 627 to 632).
- `aotir.StrSplitExpr` to `MethodCall{Method: "split", Args: [sep], UseParens: true}` (lines 633 to 642); Ruby `String#split(sep)` returns `Array<String>`, matching Mochi's `list<string>`.
- `aotir.StrJoinExpr` to `MethodCall{Receiver: list, Method: "join", Args: [sep], UseParens: true}` (lines 643 to 652); the receiver is the list, not the separator, because Ruby exposes `join` as an `Array` method, not a `String` one.

Every node is a pure `MethodCall` or single-line `RawExpr` (no temp variables, no closures), so the emitted Ruby reads naturally and a Ruby-fluent reader can audit it without consulting the lowerer.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | Nine `aotir.Str*Expr` cases lower to Ruby `String` / `Array` methods, lines 568 to 652 |
| `transpiler3/ruby/build/phase14_test.go` | `TestPhase14StringDeep` with 7 subtests |

## Test set

- `TestPhase14StringDeep/str_index`, `str_contains`, `str_substring`, `str_reverse`, `str_upper_lower`, `str_split_join`, `str_convert`.

## Closeout notes

Phase 14 landed on CRuby 4.0 (Homebrew) with all seven subtests green. The substring lowering's `|| ""` guard is the only non-obvious choice: Ruby's `String#[]` slice returns `nil` (not `""`) when start is past the end, which would crash `print` downstream; the fallback aligns behaviour with Mochi's "empty slice past the edge" rule. `split` and `join` are intentionally direct method calls instead of regex paths, since Mochi's separator is always a literal string and Ruby's `split(String)` skips the regex compile step.
