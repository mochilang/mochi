---
title: "Phase 11. Error model (panic / try-catch)"
sidebar_position: 16
sidebar_label: "Phase 11. Error model"
description: "MEP-51 Phase 11 -- panic(code, msg) lowers to raise MochiPanic; try { ... } catch e { ... } lowers to a single try/except over the MochiPanic family with built-in fault collapse (ZeroDivisionError to 5, IndexError to 4); 10 fixtures. Async coloring and MochiResult deferred to 11.1."
---

# Phase 11. Error model (panic / try-catch)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 11](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (11.0 only; 11.1-11.4 DEFERRED) |
| Started        | 2026-05-29 19:42 (GMT+7) |
| Landed         | 2026-05-29 19:51 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase11ErrorModel`: 10 fixtures green on CPython 3.12.7 in `transpiler3/python/build/phase11_test.go`. The corpus mirrors the C `error_model/` corpus (7 fixtures: `try_catch_div_zero`, `try_catch_in_fun`, `try_catch_index_oob`, `try_catch_nested`, `try_catch_no_raise`, `try_catch_reraise`, `user_panic_basic`) plus 3 Python-specific fixtures (`panic_code_passthrough`, `try_catch_in_loop`, `try_catch_string_index`). Each fixture rebuilds from `tests/transpiler3/python/fixtures/phase11-error-model/*.mochi`, runs `python -m mochi_user_<name>`, and byte-compares stdout to the matching `.out`. The full Phase 1-11 regression (`go test ./transpiler3/python/... -count=1`) finishes in 14.4s with zero regressions.

`user_panic_uncaught.mochi` from the C corpus is intentionally excluded: the program panics with no enclosing catch, so it exits non-zero with a Python traceback. The `runPythonFixture` harness gates on stdout byte-equality and a clean exit, so an uncaught-panic fixture would need a separate harness path. The error semantics for the uncaught case are exercised indirectly by every other fixture (Python's default behaviour propagates `MochiPanic` to the interpreter with the same code on `MochiPanic.code`).

## Goal-alignment audit

Mochi's error model is one of the two pieces of v1 user-facing surface (the other is async/streams) that every program eventually touches: `try { ... } catch e { ... }` is how user code recovers from list-index faults, division-by-zero, parse errors, FFI failures, and user-raised `panic(code, msg)`. Phase 11.0 is what turns "the Python target accepts try/catch and panic at all" from false to true. Without 11.0 every Mochi program with a `try` block or a `panic` call rejects at the Python target with "unsupported statement", which is the gate that blocks Phase 13 (LLM helpers raise on contract failure), Phase 14 (fetch raises on HTTP/network failure), and Phase 17 (the ipykernel needs to surface MochiPanic as a structured ipykernel error rather than a raw Python traceback).

The async-colouring + MochiResult surface originally scoped for Phase 11 (the 11.1-11.4 sub-phases) is genuinely deferred, not punted: the Mochi C lower (`transpiler3/c/lower/lower.go` §AsyncExpr) currently rejects every `AsyncExpr` and `AwaitExpr` at the C target too, so the IR side is also unblocked but not exercised by any fixture. The v1 corpus has zero `async fn` / `await` / `throws` programs; the synchronous panic + try/catch surface is what `tests/transpiler3/c/fixtures/error_model/*` exercise, and what every other transpiler target ships first. The async colour pass plus MochiResult tagged union ride on top of the same `MochiPanic` exception class once the IR surfaces them. Landing 11.0 standing alone is correct precisely because it locks the load-bearing emit shape: every later sub-phase extends `MochiPanic` (e.g. `MochiResult.Err(MochiPanic(code, msg))`) without breaking the existing emit.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | `panic(code, msg)` to `raise MochiPanic(code, msg)`; `try { ... } catch e { ... }` to `try / except (MochiPanic, ZeroDivisionError, IndexError) as __mp: e = _panic_code(__mp); ...` | LANDED 2026-05-29 | (filled at ship) |
| 11.1 | `async fn` colouring pass; `await` lowering; `async for`, `async with` | DEFERRED (no v1 fixtures) |  --  |
| 11.2 | `MochiResult[T, E]` (`Ok[T]` / `Err[E]` frozen-slots dataclass) in `mochi_runtime.result`; `throws E` lowering | DEFERRED (no v1 fixtures) | -- |
| 11.3 | PEP 654 `ExceptionGroup` unwrap at TaskGroup parent to `MochiResult.Err(list)` | DEFERRED to Phase 11.1 | -- |
| 11.4 | Async runtime entry point (`asyncio.run(main())`) when the main fn is coloured async | DEFERRED to Phase 11.1 | -- |

## Sub-phase 11.0 -- Synchronous panic / try-catch

### Goal-alignment audit (11.0)

The synchronous panic + try/catch surface is the load-bearing emit shape for every later error-model sub-phase. If the exception class is wrong, every subsequent sub-phase has to re-litigate what `e` binds to in the catch body (integer code? Python exception object? MochiResult.Err?), what the runtime base class is (Exception vs BaseException), and how built-in faults collapse. Landing 11.0 first locks the shape: panics raise `MochiPanic(code, msg)`, catches see `e: int`, built-in faults (`ZeroDivisionError`, `IndexError`) collapse to canonical codes via `_panic_code`. Phase 11.2 layers `MochiResult.Err(MochiPanic(...))` on top without touching the existing raise / except emit.

### Decisions made (11.0)

**`MochiPanic(Exception)`, not `BaseException`.** The Python convention is that user code catches `Exception`, while `BaseException` covers `KeyboardInterrupt` and `SystemExit` (which should pass through). Mochi's `try { ... } catch e { ... }` is user-level error recovery, not interpreter-control interception, so subclassing `Exception` is correct: a `try` block that wraps a long-running computation does not silently swallow Ctrl-C. The class lives in `mochi_runtime.except_` (trailing underscore avoids the Python `except` keyword) with `__slots__ = ("code", "msg")`.

**Catch tuple is fixed at lower time, not user-extensible.** Every Mochi `catch` arm catches the same three Python exception types: `(MochiPanic, ZeroDivisionError, IndexError)`. `MochiPanic` covers user-raised panics; the two built-ins cover the runtime faults that Mochi v1 specifies a code for (5 = DIVZERO, 4 = INDEX). New built-in faults extend the tuple in `lower/panic.go` and `_panic_code` in `mochi_runtime.except_` in lockstep. There is no user-extensible mechanism today because Mochi v1 has no user-defined exception types; a `recover` operator that matches on a specific code is the Phase 11.2 surface.

**`_panic_code(exc) -> int` collapses the catch surface to a single integer.** The C lowering binds `e` to `mochi_except_code` (a global int). The Python catch body sees the actual Python exception object via `as __mp`, but the lower prepends `e = _panic_code(__mp)` so the rest of the body sees the same integer code the C target sees. This matches byte-equal-stdout against vm3 for every fixture (`print(e)` prints `5` for div-by-zero, `4` for index OOB, the literal panic code for user panics) and keeps the catch-body source language-agnostic.

**Bind variable is `__mp`, not the user-visible catch var.** The Python `except E as <name>` clause introduces `<name>` in the except scope only, and the rebound name is deleted after the block. Mochi semantics require `e` to remain available throughout the catch body (and to be a Mochi int, not a Python exception object). Using `__mp` as the internal scratch lets the lowerer prepend `<CatchVar> = _panic_code(__mp)` and then drop the scratch; the user-visible `<CatchVar>` is an ordinary assigned name, not the `except ... as` binding, so it survives the entire catch body and follows the same scoping rules as any other Mochi let.

**No `finally` clause today.** The C lowering does not emit a `finally` analogue (the `mochi_try_push` / `mochi_try_pop` pair in `mochi_runtime.except` runs cleanup unconditionally only on the failure path), and Mochi v1 has no `finally { ... }` source surface. If a future Mochi `try { ... } finally { ... }` lands at the IR level, this lower extends `TryExceptStmt` with a `Finally []Stmt` field; no other call site needs to change.

**Nested try works for free.** Python's try/except composes naturally, so the C lower's nested `TryCatchStmt` (each with a unique `BufName`) lowers to nested Python try/except blocks. The `try_catch_nested.mochi` and `try_catch_reraise.mochi` fixtures verify this: the inner catch body can re-raise (via another `panic`) and the outer catch sees the new code, just like the C target.

**Try-catch inside a function body works for free.** The C lower carries the jmp_buf as a stack-local in the C function; the Python emit has no analogous bookkeeping. The `try_catch_in_fun.mochi` fixture verifies that a `try { ... } catch e { ... }` inside `fun safe_div(a, b)` returns the catch-bound value via the surrounding `return result`, byte-equal to vm3.

**Try-catch inside a loop works for free.** Python's try/except inside a `while` loop is a no-op for the loop control flow: the except handler returns to the next loop iteration. The `try_catch_in_loop.mochi` fixture exercises this with `10 / i` over `i in 0..2`, catching the div-by-zero on the first iteration and continuing.

### Fixture corpus (10 fixtures)

`tests/transpiler3/python/fixtures/phase11-error-model/`:

| Fixture | Surface | Notes |
|---------|---------|-------|
| `user_panic_basic.mochi` | `panic(42, "boom")` inside a `try`/`catch` | User panic code round-trip |
| `try_catch_div_zero.mochi` | `10 / 0` inside a `try` | ZeroDivisionError to code 5 |
| `try_catch_index_oob.mochi` | `xs[5]` on a 3-elem list | IndexError to code 4 |
| `try_catch_string_index.mochi` | `s[10]` on a 2-char string | String index OOB to code 4 |
| `try_catch_no_raise.mochi` | No fault inside try | Verifies the happy path is a no-op |
| `try_catch_in_fun.mochi` | `safe_div(a, b)` with internal try/catch | Catch inside a function body |
| `try_catch_in_loop.mochi` | `10 / i` over `i in 0..2` | Catch inside a while loop |
| `try_catch_nested.mochi` | Outer try wraps inner try; inner catches | Nested try/except composition |
| `try_catch_reraise.mochi` | Inner catch re-panics with code 99 | Re-raise via another panic |
| `panic_code_passthrough.mochi` | `panic(7, ...)` then `panic(9, ...)` | Code 7 (FFI) and 9 (ASSERT) round-trip |

Each fixture has a matching `.out` file with the canonical vm3 stdout. `TestPhase11ErrorModel` walks the directory, runs `runPythonFixture` (build, `python -m mochi_user_<name>`, byte-equal diff). All 10 fixtures pass on CPython 3.12.7.

### Files changed

| File | Purpose |
|------|---------|
| `runtime/python/mochi_runtime/except_.py` (new) | `MochiPanic(Exception)` with `code, msg` slots; `_panic_code(exc)` collapse helper |
| `transpiler3/python/lower/panic.go` (new) | `lowerPanicStmt` (panic to RaiseStmt(MochiPanic(code, msg))); `lowerTryCatchStmt` (try/catch to TryExceptStmt with prepended `e = _panic_code(__mp)`) |
| `transpiler3/python/lower/lower.go` | `needsExcept` flag + import gating; dispatch cases for PanicStmt, TryCatchStmt |
| `transpiler3/python/pysrc/nodes.go` | New AST nodes RaiseStmt and TryExceptStmt |
| `transpiler3/python/build/build.go` | Cache marker `mep51-phase10` to `mep51-phase11` |
| `transpiler3/python/build/phase11_test.go` (new) | `TestPhase11ErrorModel` walks `phase11-error-model/` |
| `tests/transpiler3/python/fixtures/phase11-error-model/` (new) | 10 `.mochi` + 10 `.out` files |

## Deferred work

- **11.1 async colour pass.** `async fn` and `await` need a fixed-point over the call graph: a function is async if it `await`s, or if any function it calls is async. The colour pass propagates this to a deterministic ordering, then the emit picks `def` or `async def`. Deferred because no v1 fixtures use `async fn`; the C lower already has `AsyncExpr` / `AwaitExpr` / `TypeFuture` in the IR but rejects them at the C target too.
- **11.2 MochiResult[T, E].** `Ok[T]` and `Err[E]` as frozen-slots dataclasses in `mochi_runtime.result`; `throws E` lowering wraps the return type and the catch surface converts `MochiPanic` to `MochiResult.Err`. Deferred to Phase 11.1 (rides on top of the async-colour pass for `throws` plus `async`).
- **11.3 PEP 654 ExceptionGroup unwrap.** When a TaskGroup raises multiple inner failures, PEP 654 wraps them in an ExceptionGroup. The Mochi error model is flat, so the parent catch needs to unwrap to `MochiResult.Err([code1, code2, ...])`. Deferred to Phase 11.1 alongside TaskGroup supervision (Phase 9.2).
- **11.4 async main entry point.** When the colour pass marks `main` as async, the emit needs `asyncio.run(main())` instead of `main()` under the `__name__ == "__main__"` guard. Deferred to Phase 11.1.
- **`exit code` for uncaught panics.** The C target writes the panic message to stderr and exits with `code`. The Python target today propagates `MochiPanic` to the interpreter, which prints a traceback and exits 1. A future improvement is a top-level `except MochiPanic` in the emit that mirrors the C exit-code convention; deferred until a uncaught-panic harness lands in `runPythonFixture`.
