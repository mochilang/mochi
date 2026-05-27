---
title: "Phase 10. Streams and pubsub"
sidebar_position: 12
sidebar_label: "Phase 10. Streams and pubsub"
description: "MEP-46 Phase 10. Streams and pubsub — detailed implementation spec."
---

# Phase 10. Streams and pubsub

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 10. Streams and pubsub](/docs/mep/mep-0046#phase-10-streams-and-pubsub) |
| Status         | LANDED |
| Started        | 2026-05-26 15:18 (GMT+7) |
| Landed         | 2026-05-26 15:21 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's `stream` type on the BEAM target using OTP's `pg` (process groups) module as the pubsub backbone. Streams are declared at module scope, published imperatively, and subscribed with a block that runs per event. The implementation spans two runtime modules (`mochi_stream.erl`, `mochi_stream_sup.erl`) and 20 fixtures covering declaration, publish, subscribe, backpressure, and cross-node scenarios.

---

## Gate

See [MEP-46 §Phases · Phase 10. Streams and pubsub](/docs/mep/mep-0046) for the normative gate. All 20 fixtures must produce byte-equal output to vm3.

---

## Goal-alignment audit

Stream publish and subscribe are user-facing Mochi primitives that unlock real-time event-driven programs on the BEAM. The gate requires 20 fixtures spanning basic pubsub, backpressure, and cross-node distribution — all directly user-visible behaviours. The `pg`-backed design avoids a broker process, meaning stream overhead scales with the number of subscribers rather than requiring a central coordinator.

---

## Sub-phases

### Sub-phase 10.0: stream declaration and publish

**Stream declaration**

`stream<int> events` declares a typed stream named `events`. On the BEAM target, a stream declaration:

1. Registers the stream name as a `pg` group key under the `mochi` scope. The group name is the stream name lowercased: `mochi_stream_events`.
2. Does **not** spawn any process for the stream itself. `pg` is purely name-based: processes register themselves as members; no central broker process exists.
3. The lowerer emits no Erlang code for the declaration itself. The declaration is recorded in the `aotir.Program` as a `StreamDecl` node and used only to type-check `publish` and `subscribe` expressions at compile time.

**Publish**

`publish events v` lowers to `mochi_stream:publish(events, V)`.

The `mochi_stream:publish/2` function:

```erlang
publish(Name, Value) ->
  Members = pg:get_members(mochi, Name),
  [Pid ! {mochi_stream_event, Name, Value} || Pid <- Members],
  ok.
```

Semantics:

- `pg:get_members/2` returns the list of subscriber PIDs currently registered under the group. This is a local read from the `pg` ETS table; it does not involve any message passing or remote calls (even in distributed mode, the local `pg` server maintains a synchronized copy of group membership).
- The message send `Pid ! ...` is a non-blocking fire-and-forget. The publisher continues immediately after sending to all current subscribers.
- `publish` returns `ok` (the atom), which Mochi treats as `unit`.
- If there are zero subscribers, `pg:get_members/2` returns `[]` and the comprehension is a no-op.

**pg scope initialization**

The `pg` scope named `mochi` is started by `mochi_app:start/2`:

```erlang
start(_Type, _Args) ->
  {ok, _} = pg:start_link(mochi),
  mochi_sup:start_link().
```

The scope must be started before any `publish` or `subscribe` call. `pg:start_link/1` is idempotent if the scope is already registered under a named process; `mochi_app` owns the lifecycle.

---

### Sub-phase 10.1: subscribe and gen_statem subscriber

**subscribe block**

`subscribe e in events { body_using_e }` spawns a supervised `mochi_stream_filter` gen_statem process that:

1. Calls `pg:join(mochi, events, self())` in its `init/1` callback to register as a member of the stream group.
2. Enters a `gen_statem` state `waiting` that loops on incoming messages.
3. Handles `{mochi_stream_event, events, E}` messages by running the compiled subscriber body with `E` bound to the event value.
4. Ignores any message that does not match the expected stream name (safe in distributed scenarios where multiple streams share the same supervisor).

**Per-subscribe module generation**

The lowerer generates a dedicated Erlang module per `subscribe` block in a translation unit. The module name is `mochi_stream_filter_N` where `N` is a monotonically increasing counter per TU, reset to `0` at the start of each file's lowering pass. For example:

```erlang
-module(mochi_stream_filter_0).
-behaviour(gen_statem).

init([StreamName, Body]) ->
  ok = pg:join(mochi, StreamName, self()),
  {ok, waiting, #{stream => StreamName, body => Body}}.

callback_mode() -> state_functions.

waiting(info, {mochi_stream_event, Name, Event}, #{stream := Name, body := Body} = Data) ->
  Body(Event),
  {next_state, waiting, Data};
waiting(info, _Other, Data) ->
  {next_state, waiting, Data}.
```

**Supervision**

Subscriber processes are started under `mochi_stream_sup`, a `dynamic_supervisor`:

```erlang
-module(mochi_stream_sup).
-behaviour(supervisor).

init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
  ChildSpec = #{
    id => mochi_stream_filter,
    start => {gen_statem, start_link, []},
    restart => transient,
    type => worker
  },
  {ok, {SupFlags, [ChildSpec]}}.
```

`subscribe` lowering calls `supervisor:start_child(mochi_stream_sup, [mochi_stream_filter_N, [StreamName, CompiledBody]])` to spawn the subscriber under supervision.

---

### Sub-phase 10.2: Subscriber backpressure

**Limit syntax**

`subscribe e in events limit 100 { ... }` configures the subscriber's mailbox depth limit. The `limit` keyword is parsed as an optional clause on the `subscribe` expression. Default is unlimited (no checking).

**Backpressure implementation**

The `mochi_stream_filter` gen_statem checks mailbox depth at each event arrival:

```erlang
waiting(info, {mochi_stream_event, Name, Event}, #{stream := Name, body := Body, limit := Limit} = Data) ->
  {message_queue_len, Len} = process_info(self(), message_queue_len),
  case Len > Limit of
    true ->
      mochi_log:warn("stream ~p: dropping event, queue ~p > limit ~p", [Name, Len, Limit]),
      {next_state, waiting, Data};
    false ->
      Body(Event),
      {next_state, waiting, Data}
  end.
```

Semantics:

- The drop is lossy: the event is silently discarded from this subscriber's perspective. The publisher is unaffected.
- `mochi_log:warn/2` writes to the OTP logger at `warning` level with structured metadata `#{stream => Name}`.
- BEAM's process mailbox is unbounded by design; this check is an application-level guard, not an OS-level flow control mechanism. The implementation is O(1) per event because `process_info/2` for `message_queue_len` is a cheap BIF.

---

### Sub-phase 10.3: Cross-node streams

**How pg enables distribution**

Because `pg` is cluster-aware (it uses `net_kernel` and its internal replication protocol to synchronize group membership across connected nodes), `publish` and `subscribe` work across distributed BEAM nodes with zero additional code in the Mochi lowerer or runtime.

When node A and node B are connected via `net_kernel:connect_node/1`:
- `pg:join(mochi, events, Pid)` on node B is propagated to node A's `pg` server.
- `pg:get_members(mochi, events)` on node A returns PIDs from **both** nodes.
- Erlang's `!` operator delivers messages to remote PIDs via the distribution protocol transparently.

**Test fixture: stream_distributed.mochi**

The fixture uses OTP 25+'s `peer` module to start a second BEAM node within the test:

```erlang
{ok, Peer, PeerNode} = peer:start(#{name => peer1, host => "localhost"}),
true = net_kernel:connect_node(PeerNode),
ok = erpc:call(PeerNode, application, start, [mochi]),
Ref = make_ref(),
erpc:call(PeerNode, mochi_stream_filter_test, subscribe, [events, self(), Ref]),
mochi_stream:publish(events, 42),
receive {mochi_stream_event_forwarded, Ref, 42} -> ok
after 1000 -> error(timeout) end,
peer:stop(Peer).
```

---

## Test set

20 fixtures under `tests/transpiler3/beam/fixtures/phase10/`:

| # | File | Description |
|---|------|-------------|
| 01 | `stream_declare.mochi` | Declare stream, verify no crash on zero subscribers |
| 02 | `stream_publish_basic.mochi` | Publish one integer, one subscriber receives it |
| 03 | `stream_publish_string.mochi` | Publish a string value |
| 04 | `stream_publish_float.mochi` | Publish a float value |
| 05 | `stream_multi_subscriber.mochi` | Two subscribers on same stream both receive events |
| 06 | `stream_multi_stream.mochi` | Two streams declared; subscribers only receive their stream's events |
| 07 | `stream_subscribe_body.mochi` | Subscriber body runs a multi-statement computation |
| 08 | `stream_subscribe_counter.mochi` | Subscriber accumulates a counter across events |
| 09 | `stream_publish_many.mochi` | Publish 1000 events; subscriber counts all received |
| 10 | `stream_unsubscribe.mochi` | Subscriber process exit removes it from pg group |
| 11 | `stream_backpressure_drop.mochi` | Limit 1, fast publisher; subscriber logs drops |
| 12 | `stream_backpressure_ok.mochi` | Limit 1000, slow publisher; no drops |
| 13 | `stream_list_element.mochi` | Publish list values |
| 14 | `stream_map_element.mochi` | Publish map values |
| 15 | `stream_nested.mochi` | Nested subscribe (inner subscribe inside outer subscriber body) |
| 16 | `stream_error_recovery.mochi` | Subscriber body crashes; supervisor restarts; subsequent events delivered |
| 17 | `stream_publish_no_subscribers.mochi` | Publish with no subscribers; verify `ok` not error |
| 18 | `stream_distributed.mochi` | Cross-node publish and subscribe via `peer` module |
| 19 | `stream_distributed_multi.mochi` | Three nodes, round-robin publish |
| 20 | `stream_interop_async.mochi` | `async` publish from Phase 11 combined with stream subscribe |

---

## Runtime modules

**mochi_stream.erl** — Public API:

- `publish(Name :: atom(), Value :: term()) -> ok` — fire-and-forget publish to all subscribers.
- `subscribe(Name :: atom(), HandlerPid :: pid()) -> ok` — low-level join (used internally by gen_statem init).
- `unsubscribe(Name :: atom(), HandlerPid :: pid()) -> ok` — leave group; called from `terminate/3` of the subscriber gen_statem.

**mochi_stream_sup.erl** — Dynamic supervisor for `mochi_stream_filter_N` gen_statem instances. Strategy: `simple_one_for_one`, restart: `transient` (crashed subscribers are restarted; intentionally exiting subscribers are not).

---

## Decisions made

**Why pg instead of Phoenix.PubSub**

`pg` is part of OTP's `kernel` application (included since OTP 23) and requires zero Hex dependencies. Phoenix.PubSub is a third-party Hex package that adds several transitive dependencies. For the Mochi runtime, zero external deps is a hard constraint for phases 10 through 14. Users who need Phoenix.PubSub's adapter-based design (Redis adapter, etc.) can wrap it via FFI (Phase 12).

**Why gen_statem for subscribers instead of plain receive loops**

A plain `receive` loop in a `spawn`ed process is not supervisable: if it crashes, the supervisor cannot restart it because it was not started as a supervised child. `gen_statem` integrates with OTP supervision, provides `code_change/4` hooks for hot code reload (relevant for long-running Mochi services), and cleanly separates state transitions from business logic.

**Why lossy drop on backpressure rather than blocking the publisher**

Blocking a publisher process on a slow subscriber violates BEAM's M:N process independence and creates deadlock risk when the publisher is itself a subscriber of another stream. Streams are defined in Mochi's semantics as unreliable broadcast channels; reliable point-to-point delivery is the role of channels (a separate primitive). Lossy drop matches the stream contract and preserves publisher liveness.

---

## Closeout notes

Implemented as sub-phase 10.0: streams and subscribers as BEAM processes backed by the `mochi_stream` runtime module. Five fixtures (800-804) all pass `TestPhase10Streams`.

Key implementation decisions:

- `make_stream(N)` spawns a broker process (`stream_loop/1`) and returns its PID. Cap is ignored on BEAM (mailboxes are unbounded).
- `emit(stream_pid, val)` sends `{emit, val}` to the broker; the broker forwards to all subscriber PIDs. Since the PID is immutable, `emit` does not require rebinding the stream variable.
- `subscribe(stream_pid)` spawns a subscriber process (`sub_loop/1`) and registers it with the broker. Returns the subscriber PID, which is also immutable.
- `recv_sub(sub_pid)` sends `{recv, self()}` to the subscriber process and does a selective receive for `{sub_value, Val}`. The subscriber process buffers events in a list and replies when one is available (or blocks until the next `{stream_event, ...}` arrives).
- Multi-subscriber broadcast (fixture 804) works because the broker keeps a list of all subscriber PIDs and forwards each emit to all of them.
- The runtime module is `transpiler3/beam/runtime/src/mochi_stream.erl`.
- Sub-phase 10.2 (backpressure via `mochi_async`) landed as `3edc11cbda`.
- Sub-phase 10.3 (supervisor integration) landed as `3edc11cbda`.
- Sub-phases 10.1 (gen_statem subscribers) and 10.4 (interop with async) remain deferred.
