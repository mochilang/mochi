---
title: "Phase 6. Strings and I/O"
sidebar_position: 8
sidebar_label: "Phase 6. Strings + I/O"
description: "MEP-45 Phase 6 tracking: string concatenation, len(s), mochi_str_cat runtime, string ops gate."
---

# Phase 6. Strings and I/O

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 6](/docs/mep/mep-0045#phase-6-strings-and-io) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 16:43 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

8-fixture suite under `tests/transpiler3/c/fixtures/string_ops/`: literal concat, variable concat, chained concat, `len` on literal, `len` on variable, concat inside a named function, concat+len, concat+equality. All 8 fixtures compile + run byte-equal vs expected output. `TestPhase6StringOps` gate is green.

## Goal-alignment audit

String concatenation and `len` on strings are used in nearly every Mochi program that produces textual output. Without them the transpiler cannot process the majority of user-facing examples. Phase 6.0 lands the `mochi_str_cat` runtime function and wires `+` on strings end-to-end (IR, lower, verifier, emit). This unblocks the closure-string fixture (which previously failed with "operator + wants both int or both float") and allows programs that build strings from parts. Aligns directly with user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 6.0 | String concatenation (`+`) and `len(s)` on strings: `BinStrCat` IR op; `StrLenExpr` IR node; `mochi_str_cat` C runtime (`runtime/src/strings.c` + `runtime/include/mochi/strings.h`); lower pass: `opForTypes` returns `BinStrCat` for `+` on TypeString, `lowerLenCall` returns `StrLenExpr` for TypeString; emit: `mochi_str_cat(a,b)` and `(int64_t)strlen(s)`; verifier: `BinStrCat` validated + `StrLenExpr` validated; `TestPhase6StringOps` gate (8 fixtures) | LANDED 2026-05-25 16:43 (GMT+7) | — | — |
| 6.1 | `[i]` (string indexing, returns one-codepoint string), `contains`, `startsWith`, `endsWith` | NOT STARTED | — | — |
| 6.2 | `split`, `join`, `toUpper`, `toLower` via utf8proc | NOT STARTED | — | — |
| 6.3 | Format-string interpolation (`"{name} is {age}"` lowers to a printf-style sequence) | NOT STARTED | — | — |
| 6.4 | File I/O: `readFile`, `writeFile`, `lines`, `appendFile`; `stdin`, `stdout`, `stderr` handles | NOT STARTED | — | — |
| 6.5 | simdutf utf-8 validation on read; rejected input raises `MOCHI_ERR_PARSE` | NOT STARTED | — | — |

## Decisions made

**`mochi_str_cat` memory model.** In Phase 6.0, `mochi_str_cat(a, b)` calls `malloc(len(a) + len(b) + 1)` and returns the freshly allocated string. The caller never frees it (no GC in Phase 6.0). This leaks memory for programs that concatenate in loops, but is correct for straight-line programs and deferred to the Phase 17 GC integration.

**`len(s)` emits `strlen`.** `StrLenExpr` lowers to `(int64_t)strlen(s)`. This counts bytes, not Unicode codepoints. For ASCII strings (the majority of the fixture corpus) this is correct. Full Unicode codepoint counting via `utf8proc_strlen` is deferred to Phase 6.2.

**Header included unconditionally.** `#include "mochi/strings.h"` is emitted in the prologue of every generated C file, matching the pattern of `print.h`, `list.h`, and `map.h`. This avoids conditional inclusion logic and has zero cost if the functions are unused (linker strips them).

**`BinStrCat` is a first-class `BinOp`.** Unlike the approach of lowering to a `CallExpr` calling `mochi_str_cat`, adding `BinStrCat` to the `BinOp` enum keeps the IR typed and lets the verifier enforce that both operands are `TypeString` using the existing `scalarBinOpTypes` table. The emit pass maps `BinStrCat` to the `mochi_str_cat(left, right)` call.

## Bug fixes in this phase

- `opForTypes` rejected `+` on strings with "operator + wants both int or both float, got string and string". Fixed by adding a `BinStrCat` case before the catch-all error return.
- `lowerLenCall` rejected string receivers with "len() argument must be a list or map in Phase 3.2". Fixed by adding `case aotir.TypeString: return &StrLenExpr{...}` before the list case.

## Deferred work

- `mochi_str_cat` leaks memory. Full GC integration deferred to Phase 17.
- `len(s)` counts bytes, not Unicode codepoints. utf8proc-based codepoint count deferred to Phase 6.2.
- String indexing `s[i]` (returns one-codepoint string): Phase 6.1.
- String methods (`contains`, `startsWith`, `endsWith`, `split`, `join`, `toUpper`, `toLower`): Phases 6.1-6.2.
- Format strings: Phase 6.3.
- File I/O: Phase 6.4.
- simdutf validation: Phase 6.5.
- Short-string optimisation (SSO, inline ≤15 bytes): deferred to after the `mochi_str` struct replaces `const char *` (Phase 6.x).

## Closeout notes

_Fill in after gate fully green (all 6 sub-phases)._
