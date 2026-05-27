---
title: "Phase 13.1: Panic and try-catch"
sidebar_label: "13.1 Panic / try-catch"
---

# Phase 13.1: Panic and try-catch

**Status:** LANDED  
**Gate:** `TestPhase13_1PanicTryCatch` green (5 fixtures)  
<<<<<<< HEAD
**Landed:** 2026-05-26 16:06 (GMT+7)
=======
**Landed:** 2026-05-26 16:06 (GMT+7)  
**Commit:** `924dfd9901`
>>>>>>> 5c9a10c53e (docs: audit MEP-46 implementation tracking pages — all phases LANDED)

## Goal alignment

Panic and try-catch are load-bearing error-handling primitives. Without them,
integer division by zero and explicit `panic()` calls can only crash the whole
program. With them, Mochi programs can recover from numeric errors and route
error codes through structured catch blocks. This unblocks division-safe math,
user-facing error handling, and the broader error propagation model.

## Design

### PanicStmt lowering

`panic(code, msg)` lowers to `erlang:error({mochi_panic, Code, Msg})`. The
`mochi_panic` tag lets try-catch handlers distinguish Mochi panics from
internal Erlang exceptions.

### TryCatchStmt lowering

```
try { TryBody } catch e { CatchBody }
```

lowers to a Core Erlang `c_try` node:

```erlang
try <try_body_cps>
of [V___tryvalN] -> V___tryvalN
catch V___clsN, V___excN, V___stkN ->
  case V___excN of
    {mochi_panic, V_e, _} -> <catch_body_cps>
    V___otherN -> erlang:throw(V___otherN)
  end
end
```

Key points:
- Both try body and catch body use the **same continuation** (statements after
  the try-catch block). This is standard CPS lowering.
- The re-raise clause uses `erlang:throw/1` rather than `erlang:raise/3`
  because `{mochi_return,...}` exceptions (from `return` statements) use class
  `throw`, and their stacktrace may be empty, which `erlang:raise/3` rejects.
- The catch variable (e.g., `e`) is bound to the panic code (the second element
  of the `{mochi_panic, Code, Msg}` tuple).

### wrapArithErr

Integer division and modulo are wrapped in a `c_try` that catches `badarith`
and re-throws as `erlang:error({mochi_panic, 5, "integer divide by zero"})`.
This makes division-by-zero catchable by `TryCatchStmt` uniformly.

### Unique CTry variable names

Core Erlang forbids duplicate `evar` names across nested `c_try` nodes. The
`lowerer` struct carries a `tryNum int` counter. Every `c_try` emission
(`lowerFunctionBody`, `TryCatchStmt`, `wrapArithErr`) calls `l.nextTryNum()`
to generate unique names like `V___cls0`, `V___rsn0`, `V___stk0`.

## Lowering table

| Mochi construct | Core Erlang output |
|---|---|
| `panic(code, msg)` | `erlang:error({mochi_panic, Code, Msg})` |
| `try { B } catch e { C }` | `c_try <B_cps> of [T] -> T catch Cls,Exc,Stk -> case Exc of {mochi_panic,E,_} -> <C_cps>; Other -> erlang:throw(Other) end` |
| `10 / 0` | `c_try erlang:div(10,0) of [R] -> R catch _,badarith,_ -> erlang:error({mochi_panic,5,"integer divide by zero"}); _,Other,_ -> erlang:raise(...)` |

## Test set

| Fixture | What it tests |
|---|---|
| `1110_panic_basic` | Bare `panic()` caught by top-level try-catch; catch var holds code |
| `1111_try_catch_no_raise` | try block completes normally; catch not executed |
| `1112_try_catch_panic` | Explicit `panic(99, ...)` caught; catch var = 99 |
| `1113_try_catch_divzero` | `10 / 0` inside try caught as `MOCHI_ERR_DIVZERO` (code 5) |
| `1114_try_catch_in_fun` | try-catch inside a user function; both success and failure paths return correct value |

## Decisions

**erlang:throw vs erlang:raise for re-raise.** The original implementation used
`erlang:raise(Class, Other, Stk)` in the re-raise clause. This silently swallowed
`{mochi_return,...}` exceptions inside functions because `erlang:throw`'s stacktrace
is sometimes empty, and `erlang:raise/3` with an empty stacktrace fails
non-obviously. Switching to `erlang:throw(Other)` (which always succeeds and
preserves the reason) fixed the issue. Since `{mochi_return,...}` is always
`throw`-class, this is semantically equivalent.

**badarith pattern.** The original `wrapArithErr` matched on
`{badarith, _}` (a 2-tuple). Erlang actually throws plain atom `badarith` for
integer divide-by-zero, not a tuple. Fixed to match `badarith` directly.
