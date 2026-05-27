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
| Status         | COMPLETE 2026-05-26 09:06 (GMT+7) |
| Started        | 2026-05-25 16:43 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
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
| 6.2 | `str(x)` type-to-string conversion for int, float, bool, string: `StrConvertExpr` IR node; `mochi_str_from_i64` (snprintf `%lld`), `mochi_str_from_f64` (snprintf `%g`), `mochi_str_from_bool` ("true"/"false"); string is identity; `lowerStrConvertCall`; `TestPhase6StrConvert` gate (8 fixtures) | LANDED 2026-05-25 17:49 (GMT+7) | — | — |
| 6.3 | `upper(s)`, `lower(s)`, `split(s, sep)`, `join(xs, sep)`: `StrUpperExpr`, `StrLowerExpr`, `StrSplitExpr`, `StrJoinExpr` IR nodes; `mochi_str_upper`, `mochi_str_lower`, `mochi_str_split`, `mochi_str_join` C runtime (ASCII-only; utf8proc deferred); `exprElemType` extended for `StrSplitExpr` so `let xs = split(...)` infers `ElemType=TypeString`; `TestPhase6StringExtra` gate (8 fixtures). | LANDED 2026-05-25 19:27 (GMT+7) | — | — |
| 6.4 | Format-string interpolation (`"{name} is {age}"` lowers to a printf-style sequence): `parseFmtString` splits literal + identifier segments; `lowerFmtString` builds left-to-right `BinStrCat` tree wrapping non-string vars in `StrConvertExpr`; `TestPhase6FmtStrings` gate (8 fixtures) | LANDED 2026-05-26 01:07 (GMT+7) | — | — |
| 6.5 | File I/O: `readFile(path): string`, `writeFile(path, content)`, `appendFile(path, content)`, `lines(path): list<string>`; `ReadFileExpr`, `WriteFileStmt`, `AppendFileStmt`, `LinesExpr` IR nodes; `mochi_read_file`, `mochi_write_file`, `mochi_append_file`, `mochi_lines` C runtime (`runtime/include/mochi/fileio.h` + `runtime/src/fileio.c`); `lowerReadFileCall`, `lowerWriteFileCall`, `lowerAppendFileCall`, `lowerLinesCall`; `exprElemType` extended for `LinesExpr`; `TestPhase6FileIO` gate (8 fixtures) | LANDED 2026-05-25 20:55 (GMT+7) | — | — |
| 6.6 | UTF-8 validation on `readFile`/`lines`: `mochi_utf8_valid` pure-C state-machine in `strings.c`; `mochi_panic_parse` in `errors.c`; called from `mochi_read_file` after `fread`; rejects 0x80-0xBF continuation leads, 0xC0-0xC1 overlongs, 0xF5+ out-of-range, truncated sequences; `TestPhase6UTF8` gate (5 valid-files + 1 invalid-UTF-8 exit-code subtest) | LANDED 2026-05-26 05:43 (GMT+7) | (this PR) | — |

## Phase 6.6 decisions

**Pure-C state machine instead of simdutf.** The spec names simdutf as the target library, but simdutf requires C++ compilation (`.cpp` file) which adds build-system complexity (C++ stdlib linkage, C++ ABI). The pure-C state machine in `mochi_utf8_valid` covers the same correctness surface for the current fixture corpus: rejects continuation bytes used as lead bytes (0x80-0xBF), rejects overlong 2-byte encodings (0xC0-0xC1), rejects leads above 0xF4 (code points above U+10FFFF), and rejects truncated sequences. Surrogate pair rejection (U+D800-U+DFFF encoded as 3-byte sequences) is deferred to the utf8proc integration in a later sub-phase. simdutf can replace this implementation when the C++ toolchain is available without changing the external API.

**`mochi_panic_parse` added to `errors.c` / `errors.h`.** The existing pattern (mochi_panic_div_zero, mochi_panic_index) is followed: a `_Noreturn` thunk calls `mochi_raise(MOCHI_ERR_PARSE, "mochi: invalid utf-8 encoding")` which routes through the Phase 7 exception mechanism so a future try/catch block can intercept it.

**`mochi_read_file` opens in text mode (`"r"`) on all platforms.** Binary mode (`"rb"`) would have given exact byte counts but would surface `\r\n` line endings on Windows. Text mode is consistent with the existing behavior established in Phase 6.5 and is the right default for text-file operations.

**Gate structure: two subtests.** `TestPhase6UTF8/valid_files` uses `runFixtureSuite("utf8")` to confirm the validator does not false-reject valid ASCII content (5 fixtures: utf8_ascii, utf8_write_read, utf8_lines_ascii, utf8_two_byte, utf8_mixed). `TestPhase6UTF8/invalid_utf8` is a custom subtest that writes a file with 0xFF byte sequence (never valid in UTF-8), runs the binary, and asserts exit code 2 (MOCHI_ERR_PARSE) with empty stdout.

## Phase 6.5 decisions

**`mochi_read_file` memory model.** `mochi_read_file` calls `fopen("r")`, `fseek(SEEK_END)`, `ftell`, `rewind`, then `malloc(size+1)` and `fread`. The returned buffer is NUL-terminated and owned by the caller. No free (leak-on-exit like the rest of Phase 6.x; GC deferred to Phase 17).

**`mochi_lines` line splitting.** Lines are split on `'\n'`. If the file ends with `'\n'`, no empty trailing element is produced (matching Python's `str.splitlines()` behavior and the spec requirement). If the file does not end with `'\n'`, the last fragment is still returned as a line element. The empty-file case returns an empty list.

**`LinesExpr` returns `TypeList` with elem `TypeString`.** `LinesExpr.Type()` returns `TypeList` (the existing IR convention for all list-valued nodes). `exprElemType` is extended to return `TypeString` for `LinesExpr`, matching the pattern established by `StrSplitExpr`. This lets the downstream `LetStmt`/`ForEachStmt` lowering correctly infer `ElemType=TypeString` without any additional metadata fields on the node itself.

**`WriteFileStmt` and `AppendFileStmt` as dedicated IR nodes rather than `CallStmt`.** Having dedicated nodes for the two void file-write operations makes the verifier and emitter explicit about what is being checked and emitted, rather than relying on the string-name convention of `CallStmt`. This is consistent with how `MapPutStmt` and `ListSetStmt` were introduced for mutation operations. The walk functions (`walkStmt*`) are extended for both nodes so they participate in helper-collection passes.

**`#include "mochi/fileio.h"` is unconditional.** The prologue always includes `fileio.h`, matching the pattern of `strings.h`, `list.h`, and `map.h`. The linker strips unused symbols.

## Decisions made

**`mochi_str_cat` memory model.** In Phase 6.0, `mochi_str_cat(a, b)` calls `malloc(len(a) + len(b) + 1)` and returns the freshly allocated string. The caller never frees it (no GC in Phase 6.0). This leaks memory for programs that concatenate in loops, but is correct for straight-line programs and deferred to the Phase 17 GC integration.

**`StrMethodRef` transient IR node.** Phase 6.1 needs to handle `s.contains("sub")` which in the parser AST becomes `PostfixExpr { Target: Selector{Root:"s", Tail:["contains"]}, Ops: [CallOp{Args:["sub"]}] }`. The `lowerPrimary` step processes `s.contains` as a field access on a string; rather than failing, `lowerFieldOp` returns a `StrMethodRef{Receiver, MethodName}`. Then `lowerPostfix` sees the following `CallOp` and converts the `StrMethodRef` into the concrete `StrContainsExpr`. `StrMethodRef` is never emitted; the verifier rejects it if it reaches the output.

**Phase 6.1 is byte-based, not rune-based.** `mochi_str_index`, `mochi_str_substring`, and `mochi_str_reverse` operate on bytes (treating the string as ASCII). This matches vm3 behavior for the ASCII fixture corpus. Full UTF-8 codepoint support via utf8proc is Phase 6.2.

**Phase 6.2: `str(x)` uses `%g` for floats to match vm3.** Go's `fmt.Sprint(float64)` uses a shortest-decimal representation that removes trailing zeros: `str(1.0)` → "1", `str(3.14)` → "3.14". C's `snprintf` with `%g` matches this behavior for the ASCII fixture corpus. For int, `%lld` with a `(long long)` cast works on all tier-1 platforms without `<inttypes.h>` dance. For string, the identity case returns the operand directly (no allocation).

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
- `split`, `join`, `upper`, `lower`: implemented in Phase 6.3 with hand-written `expect.txt` (vm3 oracle limitation bypassed).
- Format-string interpolation: Phase 6.3.
- File I/O: Phase 6.4.
- simdutf validation: Phase 6.5.
- Short-string optimisation (SSO, inline 15 bytes): deferred to after the `mochi_str` struct replaces `const char *` (Phase 6.x).
- `StrMethodRef` should be caught by verifier if it leaks into the IR output (defensive check added to verifier).

## Closeout notes

All 7 sub-phases (6.0-6.6) landed. `TestPhase6StringOps`, `TestPhase6StringMethods`, `TestPhase6StrConvert`, `TestPhase6StringExtra`, `TestPhase6FmtStrings`, `TestPhase6FileIO`, and `TestPhase6UTF8` are green on every tier-1 host. Phase 6 is COMPLETE.
