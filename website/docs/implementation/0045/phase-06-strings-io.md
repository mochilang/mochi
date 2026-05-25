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
| 6.1 | `s[i]` (string indexing, returns one-byte-char string), `s.contains(sub)`, `substring(s, start, end)`, `reverse(s)`; `StrIndexExpr`, `StrContainsExpr`, `StrSubstringExpr`, `StrReverseExpr` IR nodes; `StrMethodRef` transient node for postfix call dispatch; `mochi_str_index`, `mochi_str_contains`, `mochi_str_substring`, `mochi_str_reverse` runtime functions; `TestPhase6StringMethods` gate (8 fixtures) | LANDED 2026-05-25 17:04 (GMT+7) | — | — |
| 6.2 | `split`, `join`, `toUpper`, `toLower` via utf8proc | NOT STARTED | — | — |
| 6.3 | Format-string interpolation (`"{name} is {age}"` lowers to a printf-style sequence) | NOT STARTED | — | — |
| 6.4 | File I/O: `readFile`, `writeFile`, `lines`, `appendFile`; `stdin`, `stdout`, `stderr` handles | NOT STARTED | — | — |
| 6.5 | simdutf utf-8 validation on read; rejected input raises `MOCHI_ERR_PARSE` | NOT STARTED | — | — |

## Decisions made

**`mochi_str_cat` memory model.** In Phase 6.0, `mochi_str_cat(a, b)` calls `malloc(len(a) + len(b) + 1)` and returns the freshly allocated string. The caller never frees it (no GC in Phase 6.0). This leaks memory for programs that concatenate in loops, but is correct for straight-line programs and deferred to the Phase 17 GC integration.

**`StrMethodRef` transient IR node.** Phase 6.1 needs to handle `s.contains("sub")` which in the parser AST becomes `PostfixExpr { Target: Selector{Root:"s", Tail:["contains"]}, Ops: [CallOp{Args:["sub"]}] }`. The `lowerPrimary` step processes `s.contains` as a field access on a string; rather than failing, `lowerFieldOp` returns a `StrMethodRef{Receiver, MethodName}`. Then `lowerPostfix` sees the following `CallOp` and converts the `StrMethodRef` into the concrete `StrContainsExpr`. `StrMethodRef` is never emitted; the verifier rejects it if it reaches the output.

**Phase 6.1 is byte-based, not rune-based.** `mochi_str_index`, `mochi_str_substring`, and `mochi_str_reverse` operate on bytes (treating the string as ASCII). This matches vm3 behavior for the ASCII fixture corpus. Full UTF-8 codepoint support via utf8proc is Phase 6.2.

**`reverse` is a builtin function in Phase 6.1.** vm3 implements `reverse(s)` as a global builtin. In the AOT lower pass, `lowerUserCallExpr` detects `"reverse"` and routes to `lowerReverseCall`, which requires a string argument. If list reverse is needed in a future phase, it will be dispatched based on the argument type.

**`len(s)` emits `strlen`.** `StrLenExpr` lowers to `(int64_t)strlen(s)`. This counts bytes, not Unicode codepoints. For ASCII strings (the majority of the fixture corpus) this is correct. Full Unicode codepoint counting via `utf8proc_strlen` is deferred to Phase 6.2.

**Header included unconditionally.** `#include "mochi/strings.h"` is emitted in the prologue of every generated C file, matching the pattern of `print.h`, `list.h`, and `map.h`. This avoids conditional inclusion logic and has zero cost if the functions are unused (linker strips them).

**`BinStrCat` is a first-class `BinOp`.** Unlike the approach of lowering to a `CallExpr` calling `mochi_str_cat`, adding `BinStrCat` to the `BinOp` enum keeps the IR typed and lets the verifier enforce that both operands are `TypeString` using the existing `scalarBinOpTypes` table. The emit pass maps `BinStrCat` to the `mochi_str_cat(left, right)` call.

## Bug fixes in this phase

- `opForTypes` rejected `+` on strings with "operator + wants both int or both float, got string and string". Fixed by adding a `BinStrCat` case before the catch-all error return.
- `lowerLenCall` rejected string receivers with "len() argument must be a list or map in Phase 3.2". Fixed by adding `case aotir.TypeString: return &StrLenExpr{...}` before the list case.
- `lowerIndexOp` rejected string receivers with "index access [k]: receiver is string, expected a list or map". Fixed by adding a `case aotir.TypeString:` branch that returns `StrIndexExpr`.
- `lowerFieldOp` rejected TypeString receivers with "field access .X: receiver is string, expected a record". Fixed by returning a `StrMethodRef` for known string method names ("contains") before the TypeRecord check.
- `first` is a vm3 builtin expecting a list; renamed the function `head` in `str_index_in_function` fixture to avoid collision.

## Deferred work

- All Phase 6.x string functions leak memory. Full GC integration deferred to Phase 17.
- `len(s)` counts bytes, not Unicode codepoints. utf8proc-based codepoint count deferred to Phase 6.2.
- `mochi_str_index`, `mochi_str_substring`, `mochi_str_reverse` operate on bytes (ASCII). Full UTF-8 codepoint support via utf8proc: Phase 6.2.
- `startsWith`, `endsWith`: not in vm3 either; deferred to a later sub-phase when vm3 grows these methods.
- `split`, `join`: vm3 builtins return nil (no implementation) so no oracle; deferred.
- `toUpper`, `toLower` via utf8proc: Phase 6.2.
- Format-string interpolation: Phase 6.3.
- File I/O: Phase 6.4.
- simdutf validation: Phase 6.5.
- Short-string optimisation (SSO, inline 15 bytes): deferred to after the `mochi_str` struct replaces `const char *` (Phase 6.x).
- `StrMethodRef` should be caught by verifier if it leaks into the IR output (defensive check added to verifier).

## Closeout notes

_Fill in after gate fully green (all 6 sub-phases)._
