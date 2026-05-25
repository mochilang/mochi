---
title: "Phase 7. Error model"
sidebar_position: 9
sidebar_label: "Phase 7. Error model"
description: "MEP-45 Phase 7 tracking: setjmp/longjmp try/catch, per-thread exception jump-buffer stack, built-in and user error codes."
---

# Phase 7. Error model

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 7](/docs/mep/mep-0045#phase-7-error-model) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 23:07 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Error-model fixture suite (~30 cases: `panic`, `try { ... } catch e { ... }`, deferred cleanup, finally, nested try, panic across closure boundary) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Error handling is core surface; without it long-running programs cannot recover from expected failures. Phase 7.0 lands the C runtime infrastructure (jump-buffer stack) that makes try/catch possible; Phase 7.2 wires existing panics through `mochi_raise` so they become catchable once Phase 7.1 lands. Both phases have zero impact on programs that don't use try/catch (fallback path is identical to pre-7.0 behaviour). Aligns directly with user-facing goal.

## Sub-phases

| #   | Scope                                                                                                              | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 7.0 | Per-thread exception jump-buffer stack (TLS); `mochi_try_push` / `mochi_try_pop` / `mochi_raise(int, mochi_str)` in `runtime/{include/mochi/except.h,src/except.c}`; `TestPhase7ErrorModel` gate reuses divzero suite | LANDED 2026-05-25 23:07 (GMT+7) | — | — |
| 7.1 | `try { ... } catch e { ... }` lowers to `if (setjmp(buf) == 0) { ... } else { ... }` with cleanup on longjmp path  | NOT STARTED (parser blocked) | —      | — |
| 7.2 | Built-in error codes (`MOCHI_ERR_*`) wired through runtime calls (divzero, OOB, type mismatch, parse)              | LANDED 2026-05-25 23:07 (GMT+7) | — | — |
| 7.3 | User error codes (positive integers); user `panic(code, msg)` lowers to `mochi_raise`                              | NOT STARTED (parser blocked) | —      | — |

## Decisions made

**Phase 7.0: jump-buffer stack in `except.h/c`.** Rather than adding a new runtime module, the exception infrastructure lives in a dedicated `except.h` / `except.c` pair. The stack is a TU-local `static jmp_buf *` array of depth `MOCHI_TRY_MAX_DEPTH=64`. A single-threaded stack is correct for Phase 7; when Phase 9 (streams/agents) adds threads, the storage class will be upgraded to `__thread`.

**Phase 7.0: `mochi_except_code` and `mochi_except_msg` globals.** After a longjmp the catch-block prologue needs the error code and message. These are stored in TU-local globals (`mochi_except_code`, `mochi_except_msg`) before the longjmp. The catch-block emitter (Phase 7.1) will read them to populate the catch variable.

**Phase 7.0: `except.h` included unconditionally in prologue.** `emit.go` adds `#include "mochi/except.h"` to the generated C TU prologue alongside `errors.h`. When the program does not use try/catch the header is included but the functions are never referenced; the linker discards unreferenced symbols at `-O2`.

**Phase 7.2: `mochi_panic_div_zero/index` route through `mochi_raise`.** The only change to `errors.c` is replacing `fputs + exit` with `mochi_raise(MOCHI_ERR_*, msg)`. When no try block is on the stack, `mochi_raise` falls through to `fputs + exit`, producing identical observable behaviour. The full divzero suite (12 fixtures) passes unchanged.

**Phase 7.1 + 7.3 parser blocker.** `try { } catch e { }` syntax and `panic(code, msg)` builtin are not yet in the Mochi parser. These sub-phases are unblocked when the parser adds those AST nodes.

## Deferred work

- Phase 7.1: `try/catch` lowering (parser blocked).
- Phase 7.3: user `panic(code, msg)` (parser blocked).
- Thread-local jump-buffer stack: upgrade `__mochi_try_stack` to `__thread` when Phase 9 lands.
- Itanium-style table-driven unwind via libunwind: v2.

## Closeout notes

Sub-phases 7.0 and 7.2 are LANDED. Sub-phases 7.1 and 7.3 are blocked on parser changes.
