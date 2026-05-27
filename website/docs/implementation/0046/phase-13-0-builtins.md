---
title: "Phase 13.0. Builtins (string, math, list aggregates)"
sidebar_position: 14.5
sidebar_label: "Phase 13.0. Builtins"
description: "MEP-46 Phase 13.0: string operations, math builtins, numeric cast, and list aggregates on the BEAM target."
---

# Phase 13.0. Builtins

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46](/docs/mep/mep-0046) (extension sub-phase) |
| Status         | LANDED |
| Started        | 2026-05-26 15:30 (GMT+7) |
| Landed         | 2026-05-26 15:35 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

---

## Goal-alignment audit

String operations, math builtins, numeric casts, and list aggregates are used in virtually every real Mochi program. Without them, programs that manipulate text, compute with `abs`/`floor`/`ceil`, cast `float` to `int`, or aggregate lists (`min`, `max`, `sum`) cannot compile to BEAM. This phase unblocks a wide class of programs that already pass the vm3 backend.

---

## Sub-phase 13.0: string, math, list aggregate builtins

### String operations

| Mochi builtin | BEAM lowering |
|---------------|---------------|
| `len(s)` | `string:length(S)` |
| `upper(s)` | `string:uppercase(S)` |
| `lower(s)` | `string:lowercase(S)` |
| `s.contains(sub)` | `binary:match(S, Sub) =/= nomatch` |
| `s[i]` | `mochi_str:index(S, I)` |
| `substring(s, start, end)` | `mochi_str:substring(S, Start, End)` |
| `reverse(s)` | `mochi_str:reverse(S)` |
| `str(x)` | `mochi_str:convert(X)` |
| `split(s, sep)` | `mochi_str:split(S, Sep)` |
| `join(xs, sep)` | `mochi_str:join(Xs, Sep)` |

`string:length/1` counts Unicode codepoints (same semantics as Mochi's `len` on strings).

`mochi_str:index/2` and `mochi_str:substring/3` use `string:to_graphemes/1` + `lists:nth/2` / `lists:sublist/3` to operate on codepoint sequences, then re-encode via `unicode:characters_to_binary/1`.

`mochi_str:convert/1` dispatches on the runtime type: `integer_to_binary/1` for integers, `float_to_binary/1` for floats, `atom_to_binary/2` for booleans.

`s.contains(sub)` uses `binary:match/2` which returns `nomatch` or `{Start, Length}`. The lowerer wraps this in `erlang:'=/='/2` to produce a boolean, consistent with vm3 semantics (no start index returned).

### Math builtins

| Mochi builtin | BEAM lowering |
|---------------|---------------|
| `abs(n)` (int) | `erlang:abs(N)` |
| `abs(f)` (float) | `erlang:abs(F)` |
| `floor(f)` | `erlang:floor(F)` |
| `ceil(f)` | `erlang:ceil(F)` |

All four are OTP BIFs inlined by the BEAM JIT on OTP 27. `erlang:abs/1` is overloaded for both integer and float; the BEAM dispatcher selects the correct implementation at runtime based on tag bits.

### Numeric cast

| Mochi builtin | BEAM lowering |
|---------------|---------------|
| `int(f)` | `erlang:trunc(F)` |

`erlang:trunc/1` truncates toward zero, matching vm3's `int(f)` semantics (which uses Go's `int(f)` conversion, also truncation toward zero).

### List aggregates

| Mochi builtin | BEAM lowering |
|---------------|---------------|
| `min(xs)` | `lists:min(Xs)` |
| `max(xs)` | `lists:max(Xs)` |
| `sum(xs)` | `lists:sum(Xs)` |
| `val in xs` | `lists:member(Val, Xs)` |

`lists:min/1`, `lists:max/1`, `lists:sum/1` are standard OTP functions. `lists:member/2` takes `(Elem, List)` (note order), which is opposite to the aotir `ListContainsExpr{Value, List}` field order; the lowerer passes them as `[Val, List]` accordingly.

---

## Runtime modules

**`mochi_str.erl`** (extended) adds: `index/2`, `substring/3`, `reverse/1`, `convert/1`, `split/2`, `join/2`.

No new runtime modules were needed for math builtins (all OTP BIFs) or list aggregates (all standard `lists` module functions).

---

## Test set

5 fixtures under `tests/transpiler3/beam/fixtures/phase13/`; `TestPhase13Builtins` runs all and diffs vs expected output.

| # | File | Description |
|---|------|-------------|
| 1100 | `1100_str_len` | `len/1` on strings |
| 1101 | `1101_str_upper_lower` | `upper/1`, `lower/1` |
| 1102 | `1102_str_contains` | `s.contains(sub)` |
| 1103 | `1103_math_builtins` | `abs`, `int` cast |
| 1104 | `1104_list_aggregates` | `min`, `max`, `sum`, `in` |
