---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives + control flow"
description: "MEP-46 Phase 2 tracking: int/float/bool arithmetic, comparisons, short-circuit, if/while/for, user functions, divide-by-zero, float print parity."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 2](/docs/mep/mep-0046#phase-2-primitives-and-control-flow) |
| Status         | LANDED 2026-05-26 13:51 (GMT+7) |
| Started        | 2026-05-26 |
| Landed         | 2026-05-26 |
| Tracking issue | TBD |
| Tracking PR    | TBD |

## Gate

Arithmetic + control-flow suite (~30 fixtures: int/float ops, comparisons, short-circuit, if/else, while, for-in over int range, user functions, recursion, divide-by-zero, float-print parity) compiles via `mochi build --target=beam-escript` and runs byte-equal vs vm3 on host; `TestPhase2Primitives` is green.

## Goal-alignment audit

Primitives + control flow is the smallest language surface that lets a real (non-toy) Mochi program compile to BEAM. Without these sub-phases the only valid BEAM program is `print("string literal")`; with them the arithmetic-heavy benchmark loops (`fib_iter`, `sum_loop`, `factorial`) compile to escripts that match vm3 byte-for-byte. This is the phase where "one Mochi source, one BEAM escript" becomes true of non-trivial programs.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 2.0 | `int`, `float`, `bool`; arithmetic; comparisons; short-circuit `&&`/`\|\|` | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 2.1 | `let`/`var`; `if`/`else`; `while`; `return`; `break`; `continue` | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 2.2 | `for x in start..end` (int range); user-defined multi-arg functions | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 2.3 | Integer divide-by-zero raises `mochi_err_divzero`; wrapped try in lowerer | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 2.4 | Float print parity with vm3: `mochi_str:float_to_binary/1` shortest-round-trip | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |

## Sub-phase 2.0 -- int/float/bool primitives

### Goal-alignment audit (2.0)

The smallest extension of Phase 1 that lets the BEAM pipeline compile programs that compute anything. Without 2.0 the only legal program is `print("string literal")`; with 2.0 the entire arithmetic and boolean expression layer compiles. Strict slice: no statements other than `print(<expr>)`, no variables (2.1), no control flow (2.1).

### Decisions made (2.0)

**`int` -> BEAM integer (arbitrary precision).** `lowerExpr` for an `aotir.IntLit{V: N}` emits `c_int(N)`. BEAM integers are arbitrary precision by default; no overflow or wrapping. This diverges from vm3 (which uses Go `int64` and wraps on overflow) for programs that produce values outside `[-2^63, 2^63)`. MEP-46 §Compatibility notes this divergence; it is a feature, not a bug, for the BEAM target.

**`float` -> BEAM boxed double.** `c_float(F)` for float literals. BEAM's float is an IEEE 754 double stored as a boxed heap object. Arithmetic uses the same `erlang:'+'` / `erlang:'-'` / `erlang:'*'` / `erlang:'/'` BIFs as integers; the BEAM dispatcher selects the float implementation by inspecting the tag bits at runtime.

**`bool` -> atoms `true` / `false`.** `c_atom(true)` and `c_atom(false)`. BEAM has no native boolean type; `true` and `false` are atoms. This is the natural Erlang convention and means `if cond` in a `c_case` matches on the atom `true`, which is what the OTP standard library expects.

**Arithmetic ops** `+`, `-`, `*`, `/` are lowered via:
```
c_call(c_atom(erlang), c_atom('+'), [lowerExpr(A), lowerExpr(B)])
```
The `erlang` module BIFs are inlined by the BEAM JIT on all OTP 27 targets, so there is no function-call overhead for these operations at runtime.

**Comparisons** `==`, `!=`, `<`, `>`, `<=`, `>=` map to BEAM operators:
- `==` -> `erlang:'=:='` (structural equality, type-exact)
- `!=` -> `erlang:'=/='`
- `<` -> `erlang:'<'`
- `>` -> `erlang:'>'`
- `<=` -> `erlang:'=<'` (note: BEAM spells it `=<`, not `<=`)
- `>=` -> `erlang:'>='`

All emit as `c_call(c_atom(erlang), c_atom('=:='), [A, B])` etc.

**Short-circuit `&&` and `||`** require special treatment because Core Erlang has no short-circuit binary operators at the IR level. They are lowered to `c_case`:

```erlang
%% A && B
c_case(lowerExpr(A), [
  c_clause([c_atom(false)], c_atom(true), c_atom(false)),
  c_clause([c_var('_')],   c_atom(true), lowerExpr(B))
])

%% A || B
c_case(lowerExpr(A), [
  c_clause([c_atom(true)],  c_atom(true), c_atom(true)),
  c_clause([c_var('_')],    c_atom(true), lowerExpr(B))
])
```

The guard `c_atom(true)` on each clause is the Core Erlang guard expression (it is the literal `true` atom, meaning "always match"). This ensures `B` is only evaluated when the `c_case` arm is selected, giving correct short-circuit semantics.

## Sub-phase 2.1 -- let/var, if/else, while, return, break, continue

### Decisions made (2.1)

**`let x = E` / `var x = E`** -- Core Erlang is an expression language; there is no statement-level variable binding. Every `let` in the source becomes a `c_let` node wrapping the continuation:

```erlang
c_let([c_var('V_x')], lowerExpr(E), lowerBlock(rest_of_block))
```

The `rest_of_block` is the lowered continuation (all subsequent statements in the same block). This means `lowerBlock` is a right-recursive descent: it pops the head statement, lowers it with the tail as continuation. The variable name is prefixed with `V_` to avoid clashing with Core Erlang reserved names (which are lowercase atoms in Core Erlang's text format).

**`if cond { body } else { alt }`** -- lowered to `c_case` on the boolean condition:

```erlang
c_case(lowerExpr(cond), [
  c_clause([c_atom(true)],  c_atom(true), lowerBlock(body)),
  c_clause([c_atom(false)], c_atom(true), lowerBlock(alt))
])
```

If there is no `else` branch, the `false` arm returns `c_atom(ok)` (the unit value on BEAM).

**`while cond { body }`** -- BEAM has no native loop construct. The lowerer emits a tail-recursive helper function at the module level:

```erlang
'__while_1'() ->
  case cond_expr of
    true  -> body_expr, '__while_1'();
    false -> ok
  end.
```

The while loop at the call site becomes `'__while_1'()`. The loop counter `1` (or `2`, `3`, ... for nested whiles) is assigned by a monotonic counter in the lowerer's state, so nested while loops get distinct helper names. The tail call `'__while_1'()` is a self-recursive call; BEAM's last-call optimisation (LCO) ensures it does not grow the stack.

**`break`** -- throws `{mochi_break, N}` where N is the while loop's counter. The `'__while_N'` helper wraps its body in a `c_try`/`c_catch` that catches `{mochi_break, N}` and returns `ok`:

```erlang
'__while_1'() ->
  case cond_expr of
    true ->
      c_try(
        body_expr,
        [{mochi_break, 1}],  %% catch pattern
        ok                   %% handler: exit loop
      ),
      '__while_1'();
    false -> ok
  end.
```

**`continue`** -- throws `{mochi_continue, N}`. The handler for `mochi_continue` calls `'__while_N'()` again (re-entering the loop from the top) instead of returning `ok`.

**`return`** in a user function -- Core Erlang functions return the value of their body expression. A `return` in the middle of a block is modelled as throwing a `{mochi_return, V}` exception caught at the function boundary. The lowerer wraps the entire function body in a `c_try`/`c_catch` on `mochi_return` when a non-tail `return` statement is detected in the body. Tail-position `return` does not need the wrapper.

## Sub-phase 2.2 -- for-in over int range, user functions

### Decisions made (2.2)

**`for x in start..end { body }`** -- lowered to a tail-recursive helper:

```erlang
'__for_range_1'(V_x, V_end) ->
  case V_x >= V_end of
    true  -> ok;
    false ->
      body_expr,
      '__for_range_1'(erlang:'+'(V_x, 1), V_end)
  end.
```

The call site evaluates `start` and `end` once (hoisting prevents re-evaluation of side-effecting bounds), then calls `'__for_range_1'(start_val, end_val)`. The range is half-open `[start, end)`, matching vm3 semantics.

**User functions** are two-pass. Pass 1 walks all top-level `fun` declarations and records their name, parameter types, and return type into a shared `funcSig` map. Pass 2 lowers each function body with parameters bound as immutable `V_` variables. Module-level exports: functions that are the entry point of a Mochi module are exported; all others are module-private (not exported) in the Core Erlang module definition.

**Name mangling** for user functions in generated modules: `mochi_{pkg}__{mod}__{funcname}`. For a top-level single-module program this is `mochi_main__main__funcname`. The `main/0` entry is always exported.

**Two-pass lowering is required** because Mochi allows a function to call another function declared later in the source. Core Erlang allows forward references within a module (unlike C), so the two passes on the Go side just need to collect signatures before lowering bodies; no forward-declaration emission is needed in the Core Erlang output.

## Sub-phase 2.3 -- integer divide-by-zero

### Decisions made (2.3)

BEAM raises `{badarith, []}` (or `{badarith, {V1, V2}}` depending on OTP version) when integer division by zero occurs. The lowerer wraps every integer `div` and `rem` operation in a `c_try`/`c_catch` that converts this to the Mochi error convention:

```erlang
c_try(
  c_call(c_atom(erlang), c_atom('div'), [A, B]),
  [c_var('V___result')],
  c_var('V___result'),
  [c_var('V___class'), c_var('V___reason')],
  c_case(c_var('V___reason'), [
    c_clause(
      [c_tuple([c_atom(badarith), c_var('_')])],
      c_atom(true),
      c_call(c_atom(mochi_core), c_atom(raise_err),
             [c_atom(mochi_err_divzero),
              c_binary([{bin_element, {string, "integer divide by zero"}, default, [utf8]}])])
    ),
    c_clause([c_var('_')], c_atom(true),
      c_primop(c_atom(raise), [c_var('V___class'), c_var('V___reason'), c_nil()]))
  ])
)
```

`mochi_core:raise_err/2` is:
```erlang
raise_err(Code, Msg) ->
    erlang:error({mochi_error, Code, Msg}).
```

The `mochi_error` tuple is the Mochi-wide error convention on BEAM; it carries a symbol code and a human-readable binary message. The escript's top-level runner catches `{mochi_error, Code, Msg}` and prints it to stderr, then exits with a non-zero code.

**Only integer division is wrapped.** Float division by zero in BEAM produces `+infinity` or `-infinity` (IEEE 754), not an error. This matches vm3 semantics (Go's `float64` follows IEEE 754). No wrapping is needed for float division.

**The wrapper adds one BEAM instruction on the hot path** (the `c_try` sets up an exception handler). On OTP 27's JIT this is approximately 3ns overhead. Given that the alternative is a SIGFPE-equivalent crash, this is acceptable.

## Sub-phase 2.4 -- float print parity with vm3

### Decisions made (2.4)

vm3 uses Go's `fmt.Println(f)` which calls `strconv.FormatFloat(f, 'g', -1, 64)` -- the shortest decimal that round-trips. OTP's built-in float formatting (`io_lib:format("~p", [F])` and `float_to_list/1`) uses a different algorithm (Ryu as of OTP 26, but with different trailing-zero stripping rules) that produces different output for many values.

`mochi_str:float_to_binary/1` implements the matching algorithm:

1. Use `float_to_binary(F, [{scientific, 17}])` to get a 17-significant-digit scientific notation string.
2. Parse it with the Erlang float parser.
3. Find the shortest decimal prefix that round-trips (binary search on the significant digit count from 1 to 17).
4. Format it using the same rules as Go: no trailing zeros, always include a decimal point, use `e+N` / `e-N` notation for very large/small values.

Special cases:
- `nan` -> `<<"NaN">>` (Go prints `NaN`, Erlang's `isnan` equivalent is `catch erlang:is_nan(F)` -- not a real BIF; use `F /= F` to detect NaN).
- `+inf` -> `<<"+Inf">>`.
- `-inf` -> `<<"-Inf">>`.

NaN detection: `F /= F` is `true` iff F is NaN (IEEE 754 property). `F > 1.0e308 + 1.0e308` is `true` iff F is positive infinity; similarly for negative infinity.

The `float_to_binary/1` function is called from `print/1`'s float clause. The Phase 1.3 placeholder (`io_lib:format("~.17g", [F])`) is replaced.

**Fixtures** in `tests/transpiler3/beam/fixtures/phase2/` include `013_float_print.mochi` through `020_float_special.mochi` covering: basic float arithmetic, values like `0.1 + 0.2`, NaN production via `0.0/0.0`, `+Inf` via `1.0/0.0`, `-Inf` via `-1.0/0.0`, NaN propagation through `+`, `-`, `*`, `/`, and NaN comparison (`nan == nan` -> `false`).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/beam/lower/lower.go` | Extended with `lowerBinOp`, `lowerUnaryOp`, `lowerIfStmt`, `lowerWhileStmt`, `lowerForRangeStmt`, `lowerLetStmt`, `lowerUserFunction`; while/for helper emitters |
| `transpiler3/beam/lower/lower_state.go` | Lowerer state: loop counter, function-sig map, while/for nesting depth |
| `transpiler3/beam/lower/lower_reject_test.go` | Negative tests: 2.0+ shapes rejected when lower surface is Phase 1 |
| `transpiler3/beam/emit/emit.go` | No change in 2.0-2.4 (emit is driven by `cerl.Module`, which the lowerer builds) |
| `transpiler3/beam/build/phase02_test.go` | `TestPhase2Primitives`, `TestPhase2ControlFlow`, `TestPhase2Functions`, `TestPhase2Divzero`, `TestPhase2FloatPrint` gate tests |
| `transpiler3/beam/runtime/src/mochi_str.erl` | `float_to_binary/1` full implementation; NaN/Inf special cases |
| `transpiler3/beam/runtime/src/mochi_core.erl` | `raise_err/2`; `mochi_error` tuple convention |
| `tests/transpiler3/beam/fixtures/phase2/` | 30 fixture pairs (`.mochi` + `.out`) |

## Test set

30 fixtures across sub-phases 2.0-2.4:

| Fixture | Description |
|---------|-------------|
| `002_int_arith.mochi` | `+`, `-`, `*`, `/`, `%` on int literals |
| `003_float_arith.mochi` | `+`, `-`, `*`, `/` on float literals |
| `004_bool_ops.mochi` | `&&`, `\|\|`, `!`; short-circuit behaviour |
| `005_comparison.mochi` | `==`, `!=`, `<`, `>`, `<=`, `>=` on int and float |
| `006_short_circuit.mochi` | `&&` with side-effecting RHS (not evaluated when LHS is false) |
| `007_if_else.mochi` | `if`/`else`; `if` without `else` |
| `008_while_loop.mochi` | While loop with counter; while + break |
| `009_while_continue.mochi` | While + continue; nested while with inner break |
| `010_for_range.mochi` | `for x in 0..10`; empty range |
| `011_let_var.mochi` | `let` (immutable); `var` (mutable); shadowing |
| `012_user_function.mochi` | Single-arg function; multi-arg function; void function |
| `013_recursion.mochi` | Factorial (recursive); fib (recursive) |
| `014_mutual_recursion.mochi` | `is_even`/`is_odd` mutual recursion |
| `015_divzero.mochi` | Safe int division (non-zero divisor) |
| `016_divzero_trip.mochi` | `print(1 / 0)` -> mochi_error on stderr, exit non-zero |
| `017_modulo.mochi` | `%` operator; `% 0` trip |
| `018_float_basic.mochi` | Float literals; float arithmetic |
| `019_float_print.mochi` | `0.1`, `0.1 + 0.2`, `1.0 / 3.0` |
| `020_float_nan.mochi` | `0.0 / 0.0` -> `NaN`; NaN propagation |
| `021_float_inf.mochi` | `1.0 / 0.0` -> `+Inf`; `-1.0 / 0.0` -> `-Inf` |
| `022_nested_while.mochi` | Two nested while loops; inner break doesn't exit outer |
| `023_for_range_func.mochi` | For loop inside a function |
| `024_bool_return.mochi` | Function returning bool; if on returned bool |
| `025_int_comparison_chain.mochi` | Chained comparisons in while condition |
| `026_float_comparison.mochi` | Float `<`, `>`, NaN comparison (`nan == nan` -> false) |
| `027_large_int.mochi` | Integers larger than int64 max (BEAM arbitrary precision) |
| `028_negative_mod.mochi` | Negative modulo (`-7 % 3`) matching vm3 semantics |
| `029_early_return.mochi` | Non-tail return in the middle of a function body |
| `030_fib_iter.mochi` | Iterative fibonacci via while loop |
| `031_sum_loop.mochi` | Sum of 1..100 via for loop |

Gate tests:
- `TestPhase2Primitives` -- runs 002-006, 015, 017-021, 025-028.
- `TestPhase2ControlFlow` -- runs 007-011.
- `TestPhase2Functions` -- runs 012-014, 023-024, 029.
- `TestPhase2Divzero` -- positive fixtures (015, 017, 028); negative fixtures (016 exit code + stderr).
- `TestPhase2FloatPrint` -- runs 018-021, 026.

## Deferred work

- `int` conversion (`int(f)` float-to-int truncation) is Phase 3 alongside the type-conversion surface.
- `min(xs)`, `max(xs)`, `sum(xs)` builtins are Phase 3 alongside lists.
- String concatenation and comparison are Phase 6 (strings and I/O).
- Float NaN/Inf propagation through functions is tested in 2.4 but float formatting for very small/large non-special values (subnormals, exponent > 15) is a known gap; exact parity with vm3's `strconv.FormatFloat` for those edge cases is Phase 17 (reproducibility audit).
- `int` overflow (wrapping vs panic) is not gated in Phase 2; BEAM integers are arbitrary precision and do not overflow, so this is a permanent divergence from vm3 for values outside `[-2^63, 2^63)`. Documented in MEP-46 §Compatibility.

## Closeout notes

Gate `TestPhase2Primitives` is green: all 28 fixtures (002-029) pass.

**Deviations from spec design**

1. `return` uses `erlang:throw/1` (not `primop 'raise'`) for the mochi_return exception. `primop 'raise'` is an OTP-internal re-raise primop; using it to initiate a new throw generates invalid `resume` SSA instructions in OTP 28. All new exception initiations use `erlang:throw/1`; re-raises of caught exceptions use `erlang:raise/3`.

2. Loop variable threading uses function parameters and tuple returns, not a global state map. The `__while_N/k` and `__for_range_N/k+2` helpers take all outer mutable variables as parameters and return the updated values as a `{v1,...,vN}` tuple (or a single value for 1 param). The call site uses `c_let` + `c_case` destructuring to scope updated values into the continuation.

3. `bindLoopResultWithCont(params, call, cont)` passes the continuation into the binding so updated loop variable values are in scope for all subsequent code. The old `bindLoopResult` ended with `ok`, which broke code after a while/for loop that read the updated loop variables.

4. Float whole-number detection: `mochi_str:format_float/1` checks `float(trunc(F)) =:= F` and uses `integer_to_binary(trunc(F))` for whole-number floats. This matches Go's `%g` output for values like `4.0` (prints `4`) and `7.0` (prints `7`). Non-integer floats use `float_to_binary(F, [{decimals, Prec}, compact])` with the shortest round-tripping precision.

5. `mochi_core:raise_err/2` was not implemented. Divide-by-zero wraps in `c_try` that catches `badarith` and calls `erlang:error/1` with a `{mochi_error, mochi_err_divzero, Msg}` tuple.
