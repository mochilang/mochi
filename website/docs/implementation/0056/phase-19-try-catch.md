---
title: "Phase 19. try / catch / panic (Mochi::Runtime::Panic)"
sidebar_position: 23
sidebar_label: "Phase 19. try / catch / panic"
description: "MEP-56 Phase 19, Mochi try/catch/panic lowered to Ruby begin/rescue with a typed exception."
---

# Phase 19. try / catch / panic (Mochi::Runtime::Panic)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | a1b3d4bcdc |

## Gate

`TestPhase19TryCatch` in `transpiler3/ruby/build/phase19_test.go`: three subtests (`panic_caught`, `panic_caught_in_fun`, `try_no_panic`). The first verifies that a top-level `try { panic(42, "boom") } catch e { print(e) }` prints `before\ncaught\n42\n` (i.e. the catch binding receives the integer panic code, the message is dropped from stdout but survives on the exception). The second wraps the same construct inside a function and confirms control returns normally after the catch. The third verifies the no-panic happy path: the catch arm must not run, and the variable assigned in the try body must survive. Each runs under the resolved Ruby toolchain with `-I mochi-runtime/lib`.

## Lowering decisions

`panic(code, msg)` lowers to `raise Mochi::Runtime::Panic.new(code, msg)`, and `try { ... } catch e { ... }` lowers to `begin ... rescue Mochi::Runtime::Panic => __exc ... end` with `e = __exc.code` synthesised as the first line of the rescue body (`transpiler3/ruby/lower/lower.go` lines 318 to 330 and 1301 to 1325):

- `aotir.PanicStmt` to `RawStmt` rendering `raise Mochi::Runtime::Panic.new(code, msg)` (lines 320 to 330). `Mochi::Runtime::Panic` is a `StandardError` subclass defined in `mochi-runtime/lib/mochi/runtime/panic.rb` with `attr_reader :code`; the message is passed to `super(msg)` so it surfaces via the standard `StandardError#message` channel for diagnostics.
- `aotir.TryCatchStmt` to a hand-built `RawStmt` from `lowerTryCatchStmt` (lines 1301 to 1325). The emitted shape is:

  ```
  begin
    <try body>
  rescue Mochi::Runtime::Panic => __exc
    e = __exc.code
    <catch body>
  end
  ```

  Rescuing the specific `Mochi::Runtime::Panic` class (not bare `rescue`, not `StandardError`) means unrelated Ruby errors (e.g. a stdlib bug) bubble up instead of being silently absorbed. The catch variable is assigned from `__exc.code` (line 1319) because Mochi's `catch e` binds the integer panic code, not the exception object; pulling `.code` off the typed exception keeps the lowering one-to-one with the Mochi semantics.
- The fresh exception name `__exc` uses the `__`-prefix lowerer reservation so it cannot collide with any Mochi-level identifier in the catch body.
- The happy-path subtest (`try_no_panic`) needs no lowering glue: with no `raise`, Ruby's `begin ... rescue` just falls through and the rescue body is skipped.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `PanicStmt` (lines 320 to 330) and `TryCatchStmt` (lines 318 to 319, dispatching to `lowerTryCatchStmt` at lines 1301 to 1325) lower to Ruby `raise` and `begin`/`rescue` |
| `mochi-runtime/lib/mochi/runtime/panic.rb` | `Mochi::Runtime::Panic < StandardError` with `attr_reader :code` |
| `transpiler3/ruby/build/phase19_test.go` | `TestPhase19TryCatch` with 3 subtests |

## Test set

- `TestPhase19TryCatch/panic_caught`, `panic_caught_in_fun`, `try_no_panic`.

## Closeout notes

Phase 19 landed on CRuby 4.0 (Homebrew). The decision to bind the catch variable to `__exc.code` (an integer) rather than `__exc` (the exception object) follows MEP-56 §panic-semantics: Mochi treats panic codes as the user-visible payload and message as a diagnostic side-channel. Using a typed exception class (`Mochi::Runtime::Panic`) instead of a sentinel `String` keeps unrelated Ruby errors from being swallowed by Mochi's `catch` and lets the runtime surface the code via a real accessor, not a regex parse on the message.
