# MEP-46 research note 09, Agents and streams on OTP

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).

This note covers how Mochi's `agent`, `spawn`, `stream`, `subscribe`, `link`, `monitor` map onto BEAM processes, OTP behaviors, and `pg`. It is the longest "free lunch" of the whole BEAM target: most of Mochi's concurrency surface is already implemented by OTP.

---

## 1. Mochi concurrency surface (recap)

From [[01-language-surface]] §4:

```mochi
agent Counter {
  var n: int = 0
  method inc(): unit { n = n + 1 }
  method value(): int { n }
}

let c = spawn Counter()
c.inc()
let v = c.value()      // synchronous

stream clicks
publish clicks {user: "alice", url: "/home"}
subscribe e in clicks { log(e.url) }

let fut = async fetch("https://x")
let r = await fut
```

Mapping at a glance:

| Mochi              | BEAM                                                  |
|--------------------|-------------------------------------------------------|
| `agent T { ... }`  | `gen_server` callback module                          |
| `spawn T(args)`    | `mochi_agent_sup:start_child(T, args)` (returns ref) |
| `agent.method(x)`  | `gen_server:call(Ref, {method, x})`                   |
| `agent.tell(x)`    | `gen_server:cast(Ref, {tell, x})`                     |
| `stream foo`       | `pg` group named `{?MODULE, foo}` in scope `mochi`   |
| `publish foo m`    | `mochi_stream:publish(foo, m)`                       |
| `subscribe e in s` | `pg:join` plus a `gen_statem` event loop              |
| `async expr`       | `mochi_async:async(fun() -> expr end)` (future)       |
| `await fut`        | `mochi_async:await(fut)` (block until ready)          |
| `link a b`         | `erlang:link(pid(a), pid(b))`                         |
| `monitor a`        | `erlang:monitor(process, pid(a))`                     |

## 2. Agent → gen_server

A Mochi agent compiles to **one Erlang module per agent type** implementing `gen_server`. Example:

```mochi
agent Counter {
  var n: int = 0
  method inc(): unit { n = n + 1 }
  method value(): int { n }
  on_message m: string { log(m) }
}
```

becomes (presented as Erlang for readability; we actually emit Core Erlang):

```erlang
-module(mochi_user_Counter).
-behaviour(gen_server).
-export([start_link/0, inc/1, value/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

inc(V_self) -> gen_server:cast(V_self, {method, inc, []}).
value(V_self) -> gen_server:call(V_self, {method, value, []}).

init([]) -> {ok, #{n => 0}}.

handle_call({method, value, []}, _From, V_state) ->
    {reply, maps:get(n, V_state), V_state}.

handle_cast({method, inc, []}, V_state) ->
    V_n1 = maps:get(n, V_state) + 1,
    {noreply, V_state#{n => V_n1}}.

handle_info(V_msg, V_state) when is_binary(V_msg) ->
    mochi_log:info(V_msg),
    {noreply, V_state};
handle_info(_, V_state) ->
    {noreply, V_state}.

terminate(_, _) -> ok.
code_change(_, V_state, _) -> {ok, V_state}.
```

Conventions:
- **Fields** become map entries in state.
- **Methods with return value** become `gen_server:call`.
- **Methods returning `unit`** become `gen_server:cast` (fire-and-forget). User can force call-style with `agent.method!()` syntax.
- **`on_message T: ...` blocks** become `handle_info/2` clauses with a guard on the message shape.
- **`on_close` blocks** become `terminate/2`.

The pattern is mechanical; the codegen pass `mochi-codegen-agent` emits one module per agent.

## 3. Spawn and supervision

Spawning a Mochi agent:

```mochi
let c = spawn Counter()
```

does not call `gen_server:start_link/3` directly. Instead, it calls a *supervisor*:

```erlang
{ok, V_pid} = mochi_agent_sup:start_child(mochi_user_Counter, [])
```

`mochi_agent_sup` is a `simple_one_for_one` (or modern `dynamic_supervisor`) supervisor with the child spec `{mochi_user_Counter, start_link, []}`. The user's spawn returns an **agent ref** (an opaque struct) wrapping the PID.

This means:
- Agents are **automatically restarted** if they crash (subject to the restart policy).
- Agents are **terminated cleanly** when the application shuts down.
- The user does not see PIDs; everything goes through the agent ref.

The default restart policy is `transient`: restart only on abnormal exit. Users can override with `agent Counter permanent { ... }` syntax (added in v0.2).

## 4. Nested supervision trees

Mochi has no first-class supervisor surface; supervisors are an implementation detail. Users wanting custom supervision write:

```mochi
agent Tree {
  agent A = spawn ChildA()
  agent B = spawn ChildB()
}
```

The codegen sees nested `spawn` calls inside an agent's state and emits a **sub-supervisor** for that agent's children. The agent itself is the supervisor (using `gen_server`-as-supervisor with `set_process_flag(trap_exit, true)` and explicit child management).

For complex trees, users drop down to OTP via FFI:

```mochi
extern "OTP" supervisor MySupervisor {
  strategy: one_for_one
  children: [Counter, Logger]
}
```

This emits a hand-written `supervisor` module with the specified spec.

## 5. async/await futures

`async expr` lowers to spawning a one-shot process that computes `expr` and stores the result in its mailbox:

```erlang
% async expr
V_fut = mochi_async:async(fun() -> Expr end)
% await
mochi_async:await(V_fut)
```

`mochi_async:async/1` spawns a process (under `mochi_agent_sup`), monitors it, and returns a future struct `{mochi_future, Pid, Ref}`. `await/1` does:

```erlang
await({mochi_future, Pid, Ref}) ->
    receive
        {mochi_result, Pid, V_val} ->
            erlang:demonitor(Ref, [flush]),
            V_val;
        {'DOWN', Ref, process, Pid, V_reason} ->
            throw({mochi_async_crashed, V_reason})
    end.
```

The race-free `monitor` setup uses the OTP 24+ alias options. The await receive is a *selective* receive but is safe because the ref is freshly created; the BEAM `recv_marker` optimisation skips over unrelated mailbox messages in O(1).

`mochi_async:await_all/1`, `await_any/1`, `await_timeout/2` provide the obvious combinators.

## 6. Stream → pg

Mochi streams use `pg`:

```mochi
stream clicks
```

does not allocate at declare time. The first `publish clicks m` or `subscribe e in clicks` lazily creates the pg group `{?MODULE, clicks}` in scope `mochi`.

Publish:

```erlang
% publish clicks {user => <<"alice"/utf8>>, url => <<"/home"/utf8>>}
mochi_stream:publish(?MODULE, clicks, #{user => <<"alice"/utf8>>, url => <<"/home"/utf8>>})
```

The `mochi_stream:publish/3` implementation:

```erlang
publish(Module, Stream, Msg) ->
    Group = {Module, Stream},
    [Pid ! {mochi_stream_event, Group, Msg} || Pid <- pg:get_members(mochi, Group)],
    mochi_telemetry:event([mochi, stream, publish], #{count => length(...)}),
    ok.
```

Subscribe:

```mochi
subscribe e in clicks {
  log(e.url)
}
```

becomes a sub-process under `mochi_stream_sup`:

```erlang
mochi_stream_filter:start_link(
    {?MODULE, clicks},
    fun(V_e) -> mochi_log:info(maps:get(url, V_e)) end
)
```

`mochi_stream_filter` is a `gen_statem` that `pg:join`s on init and runs the handler on each `{mochi_stream_event, ...}` message.

## 7. Stream backpressure

`pg`-based pubsub is **fire-and-forget**: the publisher's `!` is async; if a subscriber is slow, its mailbox grows. This is a real concern for high-rate streams.

Mitigations the runtime supports:
- **Subscriber-side queue limit** (`subscribe e in stream limit 1000`): the filter process tracks its mailbox length via `process_info(self(), message_queue_len)` and starts dropping incoming events when over limit.
- **Subscriber-side backpressure via demand** (`subscribe demand 10 from stream`): the filter subscribes only when demanded; the publisher checks demand before sending. Requires both publisher and subscriber to opt in.
- **Bounded-mailbox processes** (OTP 28+): when a mailbox hits a configured limit, the sender's `!` blocks. Not yet standard; experimental in OTP 28.

For most Mochi programs, **drop-when-full** at the subscriber is the right default. The MEP-46 default is `limit 1000`; users override per-subscribe.

## 8. Cross-node streams (free)

Because `pg` is cluster-aware, Mochi streams **work across nodes automatically**. Publishing on node A delivers to subscribers on node B if both have joined the same group with the same scope.

This is one of the highest-value features of the BEAM target: distributed pubsub with no extra code. The MEP-46 user guide should highlight this.

## 9. Link and monitor

`link` and `monitor` are exposed as Mochi operators on agent refs:

```mochi
link a b      // a's death propagates to b
monitor a     // returns a monitor handle
```

The lowering is direct to BEAM's `erlang:link/1` and `erlang:monitor/2`. Mochi's monitor handle wraps the OTP ref + the alias.

Mochi's exception handling system (`try`/`catch`) maps to BEAM's `try`/`catch`. The cross-process error model is BEAM's exit signal model. A crashed agent emits an `'EXIT'` signal to linked processes (which die unless `trap_exit` is set).

`mochi_agent_sup` traps exits (it's a supervisor; trap_exit is set automatically), so agent crashes are caught and reported via telemetry, then restarted per policy.

## 10. Timeouts

Mochi's `await fut for 5s`:

```erlang
case mochi_async:await_timeout(V_fut, 5000) of
    {ok, V_val} -> V_val;
    timeout -> throw(mochi_timeout)
end
```

`gen_server:call` supports a timeout argument (default 5000 ms); Mochi's `agent.method() for 5s` lowers to that.

Stream subscribe timeouts: `subscribe e in s for 30s` ends the subscription after the timeout.

## 11. Selective receive correctness

Mochi user code never writes `receive` directly. All concurrency goes through:
- `agent.method()` (gen_server:call)
- `agent.tell()` (gen_server:cast)
- `subscribe e in s` (gen_statem)
- `await fut` (selective receive on a fresh ref)

The runtime carefully avoids unbounded selective receive patterns. The two specific patterns we use:

1. **gen_server**: `receive Msg -> ...` matches *any* message, so the mailbox is never scanned past the head.
2. **Future await**: `receive {tag, Ref, V} -> V; {'DOWN', Ref, ...} -> throw end` with `Ref` freshly created in the same function. The BEAM `recv_marker_bind` opcode (OTP 24+) tells the runtime to skip past pre-existing messages, making this O(1).

These are both safe; no selective-receive performance footguns.

## 12. Process registry

Mochi agents are addressed by ref, not by name. For globally-named processes, the user opts in:

```mochi
agent counter Counter()
```

(syntax: `agent <name> <Type>(args)`). This registers the process under `{global, <Module>.<name>}`:

```erlang
{ok, V_pid} = mochi_agent_sup:start_child(mochi_user_Counter, []),
global:register_name({?MODULE, counter}, V_pid)
```

`global` is OTP's cluster-aware registry. For local-only names, `mochi_registry` (a `gen_server`-backed local registry) is used instead. The MEP-46 should default to local names.

## 13. Hot reload of agents

When the agent's module is reloaded:
- The supervisor's child spec is unchanged.
- Existing agent processes keep their current code until the next fully-qualified call.
- For state-shape changes, the user must implement Mochi's `migrate(old_state)` method, which lowers to `gen_server`'s `code_change/3`.

The MEP-46 v0.1 spec covers in-place reload; multi-step state migration is a v0.2 feature.

## 14. Sample lowering: full program

Mochi source:

```mochi
agent Logger {
  var lines: list[string] = []
  method log(s: string): unit { lines = lines ++ [s]; print(s) }
  method dump(): list[string] { lines }
}

stream events
let logger = spawn Logger()
subscribe e in events {
  logger.log(e)
}

publish events "hello"
publish events "world"
let all = logger.dump()
```

Lowered modules:
- `mochi_user_Logger` (the agent module)
- `mochi_user_main` (the program entry point)

Generated structure:
- `mochi_user_main:main/1` starts the application, spawns the Logger under `mochi_agent_sup`, registers the subscribe filter under `mochi_stream_sup`, publishes two events, syncs, then calls `Logger.dump`.

The pg-backed delivery is synchronous on the publisher side (no buffering), so the `gen_server:call` to `dump` after the two publishes sees both messages already in the logger's mailbox by the time it runs (because cast/call ordering is preserved per-pair).

## 15. Performance ballpark

| Operation                     | Time                  |
|-------------------------------|-----------------------|
| `spawn Counter()`             | ~3 µs                 |
| `counter.inc()` (cast)        | ~1 µs                 |
| `counter.value()` (call)      | ~5 µs (rtt 2 messages)|
| `publish stream e`            | ~1 µs + N*200ns       |
| Stream subscribe lifecycle    | ~10 µs setup          |
| async/await                   | ~6 µs (spawn + send)  |

These are within a small factor of raw `gen_server`, mostly the cost of the wrapping logic. For high-throughput code, drop to FFI'd raw `gen_server` (an escape hatch we document).

---

## Sources

1. `gen_server` reference. https://www.erlang.org/doc/man/gen_server.html
2. `pg` reference. https://www.erlang.org/doc/man/pg.html
3. `gen_statem` reference. https://www.erlang.org/doc/man/gen_statem.html
4. `supervisor` reference. https://www.erlang.org/doc/man/supervisor.html
5. `global` reference. https://www.erlang.org/doc/man/global.html
6. Process aliases (OTP 24+). https://www.erlang.org/blog/process-alias/
7. Saša Jurić, "Mailbox patterns" Code BEAM 2023.
8. Recv markers, OTP 24 release notes. https://www.erlang.org/news/151
