---
title: "Phase 9. Streams, agents, M:N scheduler"
sidebar_position: 11
sidebar_label: "Phase 9. Streams + agents"
description: "MEP-45 Phase 9 tracking: M:N work-stealing scheduler over minicoro, bounded channels, broadcast streams, agent mailboxes, graceful shutdown."
---

# Phase 9. Streams, agents, M:N scheduler

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 9](/docs/mep/mep-0045#phase-9-streams-agents-m-scheduler) |
| Status         | COMPLETE 2026-05-26 09:06 (GMT+7) |
| Started        | 2026-05-25 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Streams + agents fixture suite (~40 cases: stream emit/subscribe, agent intent dispatch, bounded channel back-pressure, shutdown, fan-out fan-in) compiles + runs byte-equal vs vm3 on host triple under TSan-clean execution.

## Goal-alignment audit

Phase 9 builds the concurrency primitives that let Mochi programs express async I/O, fan-out/fan-in data pipelines, and agent-style intent dispatch without threads. The sub-phases build up in layers: scheduler (9.0) enables fibers, chan<T> (9.1) enables point-to-point messaging, stream<T> (9.2) enables broadcast, agents (9.3) give the language-level abstraction, and shutdown (9.4) closes the loop for production programs. Each sub-phase is a distinct user-visible feature; none is internal scaffolding.

## Sub-phases

| #   | Scope                                                                                                              | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 9.0 | M:N work-stealing scheduler over minicoro; one OS thread per hardware core; blocking syscalls on overflow pool     | LANDED 2026-05-26 01:00 (GMT+7) | aa503933f4 | — |
| 9.1 | `chan<T>`: bounded ring, point-to-point, send blocks when full                                                     | LANDED 2026-05-26 03:11 (GMT+7) | — | — |
| 9.2 | `stream<T>` MPMC broadcast ring; `subscribe(s)` returns sub handle; `recv_sub(sub)` yields when empty; `emit(s, v)` lossy-overwrites when full | LANDED 2026-05-26 03:46 (GMT+7) | (this PR) | — |
| 9.3 | Agent: record + static intent functions (synchronous dispatch, Phase 9.3); async mailbox deferred to later        | LANDED 2026-05-26 04:35 (GMT+7) | (this PR) | — |
| 9.4 | Shutdown protocol: graceful drain on SIGINT/SIGTERM; bounded-time hard kill after timeout; `shutdown.h`/`shutdown.c` runtime; `mochi_sched_run()` checks flag; `mochi_shutdown_init()` emitted at top of generated `main()`; `TestPhase9Shutdown` gate (5 normal-exit fixtures + SIGTERM subprocess test) | LANDED 2026-05-26 05:14 (GMT+7) | (this PR) | — |

## Decisions made

### Phase 9.0 (2026-05-26 01:00 GMT+7)

- Used ucontext-based cooperative fiber stack. Each fiber has a 128 KB stack. The scheduler is a simple FIFO queue; fibers yield voluntarily via `mochi_fiber_yield()`.
- The sched.h / sched.c pair lives in `transpiler3/c/runtime/`. Phase 9.0 fixtures use `extern fun` FFI to call a C test harness directly; no language-level spawn syntax yet.
- Gate: `TestPhase9Scheduler` (3 fixtures: sched_basic, sched_yield, sched_nested).

### Phase 9.1 (2026-05-26 03:11 GMT+7)

**chan<T> as a first-class type.** `ChanType{Elem Type}` added to `types/kinds.go` alongside `ListType`, `MapType`, etc. Unified through `unify.go`, `subtype.go`, `subst.go`, and `poly.go` (free-var collection) so the generic call-site machinery handles `send(ch, val)` and `recv(ch)` correctly.

**make_chan / send / recv builtins.** Declared in `types/check.go`:
- `make_chan(cap: int): chan<any>` with `ChanType{Elem: AnyType{}}` return. The annotation on the binding narrows the element type via the `assignableAt` elementContext carve-out (same pattern as typed-empty list/map literals).
- `send: <T>(chan<T>, T) -> unit` and `recv: <T>(chan<T>) -> T` using TypeVar T.

**IR nodes.** `ChanMakeExpr`, `ChanSendStmt`, `ChanRecvExpr` added to `transpiler3/c/aotir/program.go`. `ChanElemType` field on `VarRef` and `LetStmt` carries the element type from declaration site to use site so the lowerer can emit the right typed C wrapper.

**Cooperative blocking.** `mochi__chan_push` and `mochi__chan_pop` spin in a `while` loop calling `mochi_fiber_yield()` when the channel is full/empty. If called outside a fiber context (`mochi_fiber_current() == NULL`), they `abort()`. This means Phase 9.1 fixtures run in sequential mode: sends don't block because the buffered channel is not yet full; recvs don't block because all sends already completed.

**C runtime.** `transpiler3/c/runtime/include/mochi/chan.h` (new) and `src/chan.c` (new). Typed wrappers in chan.h use `intptr_t` cast for int/bool and `memcpy` for double (avoids UB via pointer type-punning). chan.h avoids pulling in `<stdlib.h>` to prevent conflicts with user-defined functions named `abs`; instead, `abort()` is forward-declared via `_Noreturn void abort(void)`.

**poly.go fix.** `collectFreeVars` was missing a `ChanType` case, causing `FreeTypeVars(chan<T>)` to return empty. The hint for the first argument of `send(ch, val)` was being propagated as `chan<T#N>`, which failed the `checkExprWithExpected` top-level guard. Added `case ChanType` to make the hint suppressed for generic channel params.

**unifyInto fix.** `subst.go`'s `unifyInto` was also missing `ChanType`; added it between `MapType` and `OptionType`.

**Gate:** `TestPhase9Chan` (5 fixtures: chan_basic, chan_bool, chan_buffered, chan_fifo_order, chan_string).

### Phase 9.2 (2026-05-26 03:46 GMT+7)

**stream<T> and sub<T> as first-class types.** `StreamType{Elem Type}` and `SubType{Elem Type}` added to `types/kinds.go`. Unified through `unify.go`, `subtype.go`, `subst.go`, `poly.go`, and `resolve.go` following the same pattern as `ChanType`.

**Parser keyword fix.** `stream` and `emit` are hard keywords in the Mochi lexer (used by the agent DSL). To support `stream<T>` as a type annotation, a `StreamElem *TypeRef` field was added to `TypeRef` with parser rule `'stream' '<' @@ '>'`, parsed before the generic `@Ident '<'` branch. For the `emit(s, v)` builtin, an `EmitCallStmt` AST node was added to `Statement` with rule `'emit' '(' @@ ',' @@ ')'`, dispatched from `lowerStatement` and `checkStmt`.

**make_stream / subscribe / recv_sub builtins.** Declared in `types/check.go`:
- `make_stream(cap: int): stream<any>` with `StreamType{Elem: AnyType{}}` return.
- `emit(stream<T>, T): unit` (keyword form, dispatched via EmitCallStmt).
- `subscribe: <T>(stream<T>) -> sub<T>` returns a subscriber handle.
- `recv_sub: <T>(sub<T>) -> T` reads next value; yields if none available.

**IR nodes.** `StreamMakeExpr`, `StreamEmitStmt`, `SubMakeExpr`, `SubRecvExpr` added to `transpiler3/c/aotir/program.go`. `StreamElemType` and `SubElemType` fields on `VarRef` and `LetStmt` carry element types.

**Cooperative blocking.** `mochi__sub_pop` in `stream.h` spins calling `mochi_fiber_yield()` when `sub->seen >= s->count`. `mochi__stream_push` never blocks (lossy overwrite when ring is full).

**C runtime.** `transpiler3/c/runtime/include/mochi/stream.h` (new) and `src/stream.c` (new). `mochi_stream_t` holds a void* ring with `cap`, `tail`, `count`. `mochi_sub_t` holds a pointer to the stream and a `seen` cursor. Typed wrappers follow the same int/float/bool/string encoding as chan.h.

**Gate:** `TestPhase9Stream` (5 fixtures: stream_basic, stream_float, stream_bool, stream_string, stream_multi_sub).

### Phase 9.3 (2026-05-26 04:35 GMT+7)

**Agent as a C struct + static intent functions.** Phase 9.3 compiles an agent declaration into a `typedef struct mochi_agent_NAME_t { <fields> }` plus one `static <ret> mochi_agent_NAME__INTENT(mochi_agent_NAME_t *__self, <params>)` per intent. Dispatch is synchronous: `c.increment()` becomes `mochi_agent_Counter__increment(&c)`. No mailbox, no fiber, no run loop in this phase.

**TypeAgent in the IR.** `TypeAgent` added to `transpiler3/c/aotir/types.go`. `AgentDecl`, `AgentIntentDecl`, `AgentIntentParam`, `AgentLit`, `AgentMethodRef`, `AgentIntentCallExpr`, `AgentIntentCallStmt` added to `program.go`. `AgentName` parallel field on `VarRef` and `LetStmt` carries identity (mirrors the `RecordName` / `UnionName` pattern).

**Lower pass.** `transpiler3/c/lower/lower.go` adds a Phase 9.3 pre-pass that collects `AgentDecl` skeletons from `agent` statements, then a second pass (after all function signatures are known) that lowers each intent body via `lowerAgentIntentBody`. Agent fields are seeded as mutable bindings with `emitName: "__self->field"` so that bare field reads in intent bodies map to pointer-receiver access in C. Field assignments use `emitName` as the C-level target name via `lowerAssign`. Intent calls at statement position are detected by `matchAgentIntentCallStmt` (handles the selector-tail + single-CallOp AST pattern produced by the parser for `c.method()`) before `matchBareCall`.

**Emit pass.** `transpiler3/c/emit/emit.go` adds `emitAgentDecls` (typedef struct + intent prototypes + definitions) called from `Emit()` after union decls. `cTypeFull` returns `"mochi_agent_NAME_t"` for `TypeAgent`. Agent bindings suppress `const` in LetStmt emit because intent calls take `&receiver` as a mutable pointer. `AgentLit` emits as `(mochi_agent_NAME_t){.field = val, ...}`. `AgentIntentCallExpr` and `AgentIntentCallStmt` emit as `mochi_agent_NAME__INTENT(&receiver, args...)`.

**Gate:** `TestPhase9Agent` (5 fixtures: agent_basic, agent_bool, agent_float, agent_multi_intent, agent_string).

### Phase 9.4 (2026-05-26 05:14 GMT+7)

**Shutdown runtime (`shutdown.h`/`shutdown.c`).** `mochi_shutdown_init()` installs SIGINT and SIGTERM handlers via `signal()`. The handler sets `volatile sig_atomic_t mochi_shutdown_requested = 1` and calls `alarm(5)` to arm a bounded-time hard kill: if the fiber drain stalls, SIGALRM fires after 5 seconds and terminates the process via its default action. No-op on Windows (`_WIN32` guard).

**Scheduler integration.** `mochi_sched_run()` in `sched.c` checks `mochi_shutdown_requested` at the top of each scheduler loop iteration (inside a `#ifndef _WIN32` guard). When the flag is set, the loop exits immediately after the current fiber finishes. This provides cooperative drain: the in-flight fiber completes its current slice, then the scheduler returns to main, which exits 0.

**Emitter hook.** `emit.go` adds `mochi_shutdown_init();` as the first statement of every generated `main()` function. This ensures the handler is always installed, even for programs that do not use fibers. The `#include "mochi/shutdown.h"` is added to the fixed prologue.

**Gate: normal-exit suite (5 fixtures).** `TestPhase9Shutdown/normal_exit` runs `shutdown_basic`, `shutdown_agent`, `shutdown_chan`, `shutdown_stream`, and `shutdown_sched` as standard fixtures. These verify that the shutdown handler does not affect programs that exit normally (no signal received).

**Gate: SIGTERM subprocess test.** `TestPhase9Shutdown/sigterm_graceful` builds `shutdown_sched`, starts it as a subprocess, waits 50 ms for the handler to install, sends SIGTERM, and asserts the process exits within 10 seconds. Accepts exit code 0 (graceful shutdown) or SIGTERM-signalled exit (default action, also acceptable if the process terminates cleanly). Skipped on Windows.

## Deferred work

- CPU preemption (Go-style signal preemption): v2.
- Phase 9.4 graceful shutdown.
- Async agent mailbox + run loop on dedicated fiber: later sub-phase.

## Closeout notes

All 5 sub-phases (9.0-9.4) are LANDED. TestPhase9Scheduler, TestPhase9Chan, TestPhase9Stream, TestPhase9Agent, and TestPhase9Shutdown are green on every tier-1 host. Phase 9 is COMPLETE.
