---
title: "Phase 11. async/await"
sidebar_position: 13
sidebar_label: "Phase 11. async/await"
description: "MEP-46 Phase 11. async/await — detailed implementation spec."
---

# Phase 11. async/await

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 11. async/await](/docs/mep/mep-0046#phase-11-asyncawait) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's `async`/`await` on the BEAM target using monitored BEAM process spawning and selective receive. Each `async expr` spawns a single BEAM process that computes the expression and sends the result back; `await f` does a selective receive on the unique reference attached to that future. OTP 24+'s recv-marker optimization ensures that awaiting N concurrent futures does not degrade to O(N^2) mailbox scanning.

---

## Gate

See [MEP-46 §Phases · Phase 11. async/await](/docs/mep/mep-0046) for the normative gate. All 15 fixtures must produce byte-equal output to vm3.

---

## Goal-alignment audit

`async`/`await` is a core user-facing concurrency primitive in Mochi. The BEAM target maps it naturally to process spawning, which is the idiomatic BEAM approach to concurrent work. The 15 fixtures cover basic usage, error propagation, combinators (`await_all`, `await_any`), timeout, and interop with streams (Phase 10). All fixtures exercise directly user-visible behaviour.

---

## Sub-phases

### Sub-phase 11.0: async -> monitored spawn

**Syntax and lowering**

`let f = async expr` is a Mochi expression. The lowerer transforms it into:

```erlang
mochi_async:async(fun() -> <lowered_expr> end)
```

The lowerer emits the Core Erlang:

```erlang
c_call(c_atom(mochi_async), c_atom(async), [c_fun([], lowerExpr(expr))])
```

The zero-argument `c_fun` wraps the expression so that `mochi_async:async/1` receives a thunk (a `fun/0`) it can invoke in the spawned process.

**mochi_async:async/1**

```erlang
-spec async(fun(() -> term())) -> mochi_async_ref().
async(Fun) ->
  Ref = make_ref(),
  Parent = self(),
  {Pid, _MonRef} = spawn_monitor(fun() ->
    try
      Result = Fun(),
      Parent ! {mochi_async_done, Ref, {ok, Result}}
    catch
      Class:Reason:Stack ->
        Parent ! {mochi_async_done, Ref, {error, {Class, Reason, Stack}}}
    end
  end),
  {mochi_async_ref, Ref, Pid}.
```

The return type `mochi_async_ref()` is defined as:

```erlang
-type mochi_async_ref() :: {mochi_async_ref, reference(), pid()}.
```

**Crash safety via spawn_monitor**

`spawn_monitor/1` atomically spawns the child process and establishes a monitor in one BIF call. If the spawned process crashes before it can send `{mochi_async_done, ...}` (for example, due to a BEAM-level error like `badarg` inside `Fun()`), the parent receives a `{'DOWN', MonRef, process, Pid, Reason}` message. The `await` implementation handles this DOWN message and converts it to `mochi_err_async_crash`.

**Thunk ownership**

The thunk captures all variables from the enclosing Mochi scope via Erlang closure. Because BEAM closures capture references (heap terms are shared via reference counting), there is no data copying for large values; only the closure environment header is copied when the thunk is sent cross-process.

---

### Sub-phase 11.1: await -> selective receive with recv-marker

**Syntax and lowering**

`await f` lowers to:

```erlang
mochi_async:await(F)
```

The lowerer emits:

```erlang
c_call(c_atom(mochi_async), c_atom(await), [V_f])
```

where `V_f` is the Core Erlang variable holding the `mochi_async_ref()` value.

**mochi_async:await/1**

```erlang
-spec await(mochi_async_ref()) -> term().
await({mochi_async_ref, Ref, Pid}) ->
  receive
    {mochi_async_done, Ref, {ok, Result}} ->
      Result;
    {mochi_async_done, Ref, {error, {_Class, Reason, _Stack}}} ->
      erlang:error({mochi_err_async_crash, Reason});
    {'DOWN', _MonRef, process, Pid, Reason} ->
      erlang:error({mochi_err_async_crash, Reason})
  end.
```

The `receive` pattern matches on `Ref` (the unique reference created in `async/1`). Because each `async` call creates a fresh `make_ref()`, the pattern is unique per future: no two concurrent futures share the same `Ref`.

**OTP 24 recv-marker optimization**

Without recv-markers, `receive {mochi_async_done, Ref, _} end` scans the entire mailbox from position 0 on every iteration. If a process has N concurrent outstanding futures and processes them in order, this is O(N^2) total work.

OTP 24 introduced `erlang:recv_marker_reserve/0` and `erlang:recv_marker_bind/2` to allow the runtime to store a scan-start pointer per reference. The `await` implementation uses this optimization:

```erlang
await({mochi_async_ref, Ref, Pid}) ->
  Marker = erlang:recv_marker_reserve(),
  erlang:recv_marker_bind(Marker, Ref),
  receive
    {mochi_async_done, Ref, {ok, Result}} ->
      erlang:recv_marker_clear(Marker),
      Result;
    {mochi_async_done, Ref, {error, {_Class, Reason, _Stack}}} ->
      erlang:recv_marker_clear(Marker),
      erlang:error({mochi_err_async_crash, Reason});
    {'DOWN', _MonRef, process, Pid, Reason} ->
      erlang:recv_marker_clear(Marker),
      erlang:error({mochi_err_async_crash, Reason})
  end.
```

With the marker bound to `Ref`, BEAM stores the mailbox scan pointer at the position of the message matching `Ref`, so the receive is O(1) regardless of how many other messages are in the mailbox. This is documented in OTP 24 release notes under "Selective receive optimization."

The lowerer wraps `async` + `await` pairs so that the `Marker` is reserved **before** the `async` spawn, ensuring that any message arriving between spawn and await is still found at the correct position.

---

### Sub-phase 11.2: await_all, await_any, await_timeout

**await_all**

`await_all([f1, f2, f3])` waits for all futures in the list to complete and returns a list of their results in order. Lowers to `mochi_async:await_all([F1, F2, F3])`.

Implementation:

```erlang
-spec await_all([mochi_async_ref()]) -> [term()].
await_all(Futures) ->
  lists:map(fun await/1, Futures).
```

This is sequential await: `f1` is awaited first, then `f2`, then `f3`. This is correct because BEAM selective receive allows messages to arrive in any order; awaiting `f1` first will block only until `f1`'s message arrives, regardless of whether `f2` and `f3` have already completed (their messages sit in the mailbox). The recv-marker optimization makes each individual await O(1).

**await_any**

`await_any([f1, f2, f3])` returns the result of whichever future completes first. Lowers to `mochi_async:await_any([F1, F2, F3])`.

Implementation:

```erlang
-spec await_any([mochi_async_ref()]) -> term().
await_any(Futures) ->
  RefSet = maps:from_list([{Ref, true} || {mochi_async_ref, Ref, _} <- Futures]),
  await_any_loop(RefSet, Futures).

await_any_loop(RefSet, Futures) ->
  receive
    {mochi_async_done, Ref, {ok, Result}} when is_map_key(Ref, RefSet) ->
      cancel_remaining(Futures, Ref),
      Result;
    {mochi_async_done, Ref, {error, {_Class, Reason, _Stack}}} when is_map_key(Ref, RefSet) ->
      cancel_remaining(Futures, Ref),
      erlang:error({mochi_err_async_crash, Reason})
  end.

cancel_remaining(Futures, WinnerRef) ->
  [exit(Pid, cancel) || {mochi_async_ref, Ref, Pid} <- Futures, Ref =/= WinnerRef].
```

Cancelled futures receive an `exit(Pid, cancel)` signal. Since the spawned futures are monitored (not linked), the `exit` is an asynchronous kill; the parent is not affected.

**await_timeout**

`await f timeout 5000` lowers to `mochi_async:await_timeout(F, 5000)`.

Implementation:

```erlang
-spec await_timeout(mochi_async_ref(), non_neg_integer()) -> term().
await_timeout({mochi_async_ref, Ref, Pid}, TimeoutMs) ->
  receive
    {mochi_async_done, Ref, {ok, Result}} -> Result;
    {mochi_async_done, Ref, {error, {_Class, Reason, _Stack}}} ->
      erlang:error({mochi_err_async_crash, Reason});
    {'DOWN', _MonRef, process, Pid, Reason} ->
      erlang:error({mochi_err_async_crash, Reason})
  after TimeoutMs ->
    exit(Pid, timeout),
    erlang:error(mochi_err_timeout)
  end.
```

The `after` clause uses BEAM's native receive timeout, which is implemented via a timer wheel (O(1) insert and cancel). On timeout, the spawned process is killed via `exit(Pid, timeout)` to prevent resource leaks.

---

## Test set

15 fixtures under `tests/transpiler3/beam/fixtures/phase11/`:

| # | File | Description |
|---|------|-------------|
| 01 | `async_basic.mochi` | `async` + `await` of a simple integer expression |
| 02 | `async_string.mochi` | `async` + `await` of a string concatenation |
| 03 | `async_computation.mochi` | `async` of a fibonacci computation |
| 04 | `async_multiple.mochi` | Three concurrent `async` calls, each awaited independently |
| 05 | `async_await_all.mochi` | `await_all` of 5 futures |
| 06 | `async_await_any.mochi` | `await_any` of 3 futures with different durations |
| 07 | `async_timeout_ok.mochi` | `await timeout 5000` completes well within timeout |
| 08 | `async_timeout_expire.mochi` | `await timeout 1` expires; error propagated |
| 09 | `async_crash.mochi` | Async expression crashes; `await` surfaces `mochi_err_async_crash` |
| 10 | `async_nested.mochi` | `async` inside `async`; outer awaits inner |
| 11 | `async_capture.mochi` | Async closure captures a large list; verify result correctness |
| 12 | `async_ordering.mochi` | `await_all` returns results in original order, not completion order |
| 13 | `async_stream_interop.mochi` | `async` publish to a stream from Phase 10 |
| 14 | `async_cancel_any.mochi` | `await_any` cancels losers; verify they do not deliver results |
| 15 | `async_recv_marker.mochi` | 100 concurrent futures, awaited in order; verify no O(N^2) degradation |

---

## Decisions made

**Why spawn_monitor instead of plain spawn**

If the spawned process crashes before sending its result, plain `spawn` leaves the awaiting process blocked indefinitely (or until timeout). `spawn_monitor` guarantees a `DOWN` message on any process termination, which `await/1` converts to `mochi_err_async_crash`. This is the standard OTP pattern for safe concurrent calls, as used by `gen_server:call/2` internally.

**Why the recv-marker optimization matters**

Without recv-markers, awaiting N concurrent futures where the first-awaited future is the last to complete requires scanning the entire mailbox (containing N-1 already-delivered messages) on every `receive` iteration. For N=100, this is 100+99+...+1 = 5050 message inspections for the complete await sequence. With recv-markers, each `await` is O(1) regardless of N. This is documented in the OTP 24 release notes under "Selective receive optimization" and is enabled automatically when `make_ref()` is used as the match key.

**Why async/await and not Task/Promise**

Mochi's `async expr` / `await f` syntax maps directly to the BEAM spawned-process model. There is no intermediary abstraction layer (no Task struct, no Promise chain). The BEAM process is the unit of concurrency; each future is exactly one process. This matches how experienced Erlang and Elixir programmers think about concurrent computation and makes the generated Erlang code readable and debuggable without knowledge of Mochi internals. Promise chaining (`.then()`) would require either a callback-based CPS transform or a trampoline, both of which obscure the generated Erlang.

---

## Closeout notes

Sub-phases 11.0, 11.1, and 11.2 landed together as `2a1344880d`. The implementation uses `mochi_async.erl` with `erlang:spawn_monitor` and selective receive. `async expr` lowers to `mochi_async:async(fun() -> Expr end)`; `await f` lowers to `mochi_async:await(F)`. The recv-marker optimization is included for O(1) mailbox scanning. `await_all` and `await_any` combinators were also implemented. All 15 fixtures produce byte-equal output against vm3.
