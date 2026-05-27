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
| Status         | COMPLETE 2026-05-26 09:06 (GMT+7) |
| Started        | 2026-05-25 23:07 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
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
| 7.1 | `try { ... } catch e { ... }` lowers to `if (setjmp(buf) == 0) { ... } else { ... }` with mochi_try_pop on normal exit; `TryCatchStmt` IR node; `lowerTryCatch` + `tryCounter` in lowerer; `TryCatch` field added to parser `Statement`; type checker introduces catch var as `int` in catch scope; 8 fixtures + `TestPhase7TryCatch` gate | LANDED 2026-05-26 01:07 (GMT+7) | — | — |
| 7.2 | Built-in error codes (`MOCHI_ERR_*`) wired through runtime calls (divzero, OOB, type mismatch, parse)              | LANDED 2026-05-25 23:07 (GMT+7) | — | — |
| 7.3 | `panic(code, msg)` builtin lowers to `mochi_raise((int)(code), (const char*)(msg))` via `PanicStmt` IR node; registered as builtin in type checker; 1 fixture in `TestPhase7TryCatch` gate | LANDED 2026-05-26 01:07 (GMT+7) | — | — |

## Decisions made

**Phase 7.0: jump-buffer stack in `except.h/c`.** Rather than adding a new runtime module, the exception infrastructure lives in a dedicated `except.h` / `except.c` pair. The stack is a TU-local `static jmp_buf *` array of depth `MOCHI_TRY_MAX_DEPTH=64`. A single-threaded stack is correct for Phase 7; when Phase 9 (streams/agents) adds threads, the storage class will be upgraded to `__thread`.

**Phase 7.0: `mochi_except_code` and `mochi_except_msg` globals.** After a longjmp the catch-block prologue needs the error code and message. These are stored in TU-local globals (`mochi_except_code`, `mochi_except_msg`) before the longjmp. The catch-block emitter reads them to populate the catch variable.

**Phase 7.0: `except.h` included unconditionally in prologue.** `emit.go` adds `#include "mochi/except.h"` to the generated C TU prologue alongside `errors.h`. When the program does not use try/catch the header is included but the functions are never referenced; the linker discards unreferenced symbols at `-O2`.

**Phase 7.2: `mochi_panic_div_zero/index` route through `mochi_raise`.** The only change to `errors.c` is replacing `fputs + exit` with `mochi_raise(MOCHI_ERR_*, msg)`. When no try block is on the stack, `mochi_raise` falls through to `fputs + exit`, producing identical observable behaviour. The full divzero suite (12 fixtures) passes unchanged.

**Phase 7.1: unique jmp_buf names via `tryCounter`.** Each `lowerTryCatch` call increments `l.tryCounter` and generates `__mochi_buf_N` as the C variable name. This prevents collisions when multiple try blocks appear in the same function body (including nested try blocks where the inner and outer buffers must be distinct C locals).

**Phase 7.1: catch variable introduced as `TypeInt` in lowerer scope.** The lowerer creates a fresh child scope for the catch body and seeds it with `{CatchVar: TypeInt}` before lowering catch statements. The verifier mirrors this: `verifyTryCatchStmt` checks the try body, then opens a scope seeded with the catch variable before verifying the catch body.

**Phase 7.1: `mochi_try_pop()` emitted on the normal-exit path inside the `if` branch.** The generated structure is:
```c
{
    jmp_buf __mochi_buf_N;
    mochi_try_push(&__mochi_buf_N);
    if (setjmp(__mochi_buf_N) == 0) {
        /* try body */
        mochi_try_pop();
    } else {
        int64_t e = (int64_t)mochi_except_code;
        /* catch body */
    }
}
```
The `mochi_try_pop()` is NOT emitted on the longjmp path because `mochi_raise` already pops the top buffer before longjmping (invariant: the buffer that caught the raise is gone when the catch body starts). This avoids a double-pop.

**Phase 7.3: `panic` is a builtin call, not a keyword.** The type checker registers `panic` with signature `(int, string) -> unit`. The lowerer recognises `panic` in `lowerExprStmt` alongside `print`, `append`, etc. It does not require a new keyword in the parser. The emitted `PanicStmt` IR node carries typed Code and Msg expressions.

## Deferred work

- Thread-local jump-buffer stack: upgrade `__mochi_try_stack` to `__thread` when Phase 9 lands.
- Itanium-style table-driven unwind via libunwind: v2.
- `defer` / `finally` syntax: deferred to a future MEP sub-phase.

## Closeout notes

Sub-phases 7.0, 7.1, 7.2, and 7.3 are LANDED. Phase 7 is substantially complete. The deferred items (thread-local stack, libunwind) are infrastructure concerns that land with Phase 9.
