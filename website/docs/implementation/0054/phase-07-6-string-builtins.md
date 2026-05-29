---
title: "Phase 7.6. String builtins"
sidebar_position: 18
sidebar_label: "Phase 7.6. String builtins"
description: "MEP-54 Phase 7.6, StrIndex/StrSubstring/StrReverse/StrSplit/StrJoin via mochiStr* helpers + strings stdlib delegations."
---

# Phase 7.6. String builtins

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-28 (GMT+7) |
| Tracking PR    | [#22573](https://github.com/mochilang/mochi/pull/22573) |
| Commit         | 9e73065a33 |

## Gate

24 fixtures across three trios: 8 `string_methods` (`str_contains_false`, `str_contains_true`, `str_index_simple`, `str_index_concat`, `str_index_in_function`, `str_methods_combined`, `str_reverse`, `str_substring`), 8 `string_extra` (`str_join_basic`, `str_join_single`, `str_lower`, `str_split_basic`, `str_split_join`, `str_split_spaces`, `str_upper`, `str_upper_lower`), 8 `str_convert` (`str_bool`, `str_concat`, `str_float`, `str_in_condition`, `str_in_function`, `str_int`, `str_list_values`, `str_string_identity`). All 217 transpiler3/go fixtures green.

## Lowering decisions

- `StrIndexExpr` -> `mochiStrIndex` helper (rune-based index; negative bounds wrap to `len + i`, OOB panics with the runtime panic code, matching the C runtime). Naive `s[i]` would index UTF-8 bytes and diverge for any non-ASCII string.
- `StrSubstringExpr` -> `mochiStrSubstring` helper (rune-based half-open slice with clamping on both bounds). The clamp catches both `s[-1:5]` and `s[100:200]` rather than panicking, matching the C runtime's permissive semantics.
- `StrReverseExpr` -> `mochiStrReverse` helper (reverse by runes so multibyte characters survive the round-trip; reversing by bytes would mangle UTF-8 surrogates).
- `StrSplitExpr` -> `strings.Split(s, sep)`. The Go stdlib version's empty-separator behaviour (split into runes) matches Mochi.
- `StrJoinExpr` -> `strings.Join(xs, sep)`. Direct delegation; `xs` is already a `[]string` from the Phase 3.1 list lowerer.

`addImport("strings")` is registered for split / join. The three `mochiStr*` helpers are introduced via `addHelper` and rendered into the `emittedHelpers` block before `main`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/expr.go` | `StrIndexExpr`, `StrSubstringExpr`, `StrReverseExpr`, `StrSplitExpr`, `StrJoinExpr` |
| `transpiler3/go/lower/lower.go` | `mochiStrIndex`, `mochiStrSubstring`, `mochiStrReverse` helper texts |
| `tests/transpiler3/go/fixtures/str_*/` | 24 fixtures |

## Test set

- 24 `TestPhase1Hello/str_*` subtests.

## Closeout notes

The rune-based pattern was set up in Phase 7.3 (`StrLenExpr`); Phase 7.6 reuses the rune-conversion trick for every method that touches a character. The cost is a `[]rune(s)` conversion per call (linear in the string length); this is fine for the fixture goldens but a Phase 7.6.1 optimisation pass could memoise the rune slice for repeated method calls on the same string. No fixture motivates it yet.
