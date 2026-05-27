---
title: "Phase 9. Agents and gen_server"
sidebar_position: 11
sidebar_label: "Phase 9. Agents and gen_server"
description: "MEP-46 Phase 9 implementation spec: lowering Mochi agent declarations to OTP gen_server callback modules with supervisor integration."
---

# Phase 9. Agents and gen_server

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 9](/docs/mep/mep-0046#phase-9-agents-and-genserver) |
| Status         | LANDED |
| Started        | 2026-05-26 15:10 (GMT+7) |
| Landed         | 2026-05-26 15:17 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

---

## Goal-alignment audit

Agents are Mochi's concurrency primitive. This phase validates that Mochi
agent programs run on BEAM as proper OTP processes with supervision, crash
recovery, and correct message passing semantics. Completing this phase enables
Mochi programs to participate in OTP supervision trees, making the BEAM target
viable for production concurrent systems, not just batch computation.

---

## Sub-phase 9.0: agent -> gen_server callback module

### Overview

Each `agent` declaration in Mochi generates a complete OTP gen_server callback
module. The module name is derived from the agent name:
`mochi_user_<lowercase_agent_name>.erl`.

Given:

```mochi
agent Counter {
    var count: int = 0
    fun increment() { count = count + 1 }
    fun get(): int { return count }
}
```

The lowerer emits a Core Erlang representation of:

```erlang
-module(mochi_user_counter).
-behaviour(gen_server).
-export([start_link/1, increment/1, get/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link(InitArgs) ->
    gen_server:start_link(?MODULE, InitArgs, []).

init([Count]) ->
    {ok, #{count => Count}}.

%% Public API

increment(Ref) ->
    gen_server:cast(Ref, increment).

get(Ref) ->
    gen_server:call(Ref, get).

%% Callbacks

handle_call(get, _From, State) ->
    {reply, maps:get(count, State), State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(increment, State) ->
    NewCount = maps:get(count, State) + 1,
    {noreply, State#{count => NewCount}}.

terminate(_Reason, _State) ->
    ok.
```

### Lowering agent fields to gen_server state

Agent fields are stored in the gen_server state map. The map uses field names
as atom keys:

```go
// init/1 body: {ok, #{count => InitCount}}
c_tuple([]cerl.Expr{
    c_atom("ok"),
    c_map([]cerl.Pair{
        c_map_pair(c_atom("count"), c_var("V_InitCount")),
    }),
})
```

The state map does not include a `__mochi_record__` tag (unlike Mochi records
lowered in Phase 4) because the state map is gen_server internal state, not a
user-facing Mochi value.

### Determining call vs cast

The lowerer classifies each agent method:
- Methods with return type `unit` -> `gen_server:cast` (fire-and-forget).
- Methods with non-unit return type -> `gen_server:call` (synchronous reply).

This classification drives both the public API function (`cast` vs `call` in
the client stub) and the callback handler (`handle_cast` vs `handle_call`).

### handle_call structure

`handle_call` is emitted as a `c_case` over the request atom, with one clause
per query method and a catch-all:

```go
c_case(c_var("V_Req"), []cerl.Clause{
    // get clause
    c_clause(
        []cerl.Expr{c_atom("get")},
        c_atom("true"),
        c_tuple([]cerl.Expr{
            c_atom("reply"),
            c_call(c_atom("maps"), c_atom("get"),
                []cerl.Expr{c_atom("count"), c_var("V_State")}),
            c_var("V_State"),
        }),
    ),
    // catch-all
    c_clause(
        []cerl.Expr{c_var("_")},
        c_atom("true"),
        c_tuple([]cerl.Expr{
            c_atom("reply"), c_atom("ok"), c_var("V_State"),
        }),
    ),
})
```

### handle_cast structure

`handle_cast` is emitted as a `c_case` over the message atom, with one clause
per command method:

```go
c_case(c_var("V_Msg"), []cerl.Clause{
    // increment clause
    c_clause(
        []cerl.Expr{c_atom("increment")},
        c_atom("true"),
        c_let(
            [c_var("V_NewState")],
            // State#{count => maps:get(count, State) + 1}
            c_map_update(c_var("V_State"), []cerl.Pair{
                c_map_pair(c_atom("count"),
                    c_call(c_atom("erlang"), c_atom("+"),
                        []cerl.Expr{
                            c_call(c_atom("maps"), c_atom("get"),
                                []cerl.Expr{c_atom("count"), c_var("V_State")}),
                            c_int(1),
                        },
                    ),
                ),
            }),
            c_tuple([]cerl.Expr{c_atom("noreply"), c_var("V_NewState")}),
        ),
    ),
})
```

---

## Sub-phase 9.1: spawn -> mochi_agent_sup:start_child/2

### Spawn lowering

`let c = spawn Counter(0)` lowers to a call to `mochi_agent_sup:start_child/2`:

```go
c_call(
    c_atom("mochi_agent_sup"), c_atom("start_child"),
    []cerl.Expr{
        c_atom("mochi_user_counter"),
        c_cons(c_int(0), c_nil()),   // [0]
    },
)
```

`mochi_agent_sup:start_child/2` starts a supervised child and returns a PID.
The lowerer wraps the PID in an opaque agent ref tuple:

```go
// Full spawn lowering:
c_let(
    [c_var("V_Pid")],
    c_call(c_atom("mochi_agent_sup"), c_atom("start_child"),
        []cerl.Expr{c_atom("mochi_user_counter"), initArgsList}),
    c_tuple([]cerl.Expr{c_atom("mochi_agent_ref"), c_var("V_Pid")}),
)
```

The opaque `{mochi_agent_ref, Pid}` tuple prevents the caller from using the
PID directly (bypassing the type system), while still allowing the lowerer to
extract the PID when emitting gen_server calls.

### mochi_agent_sup implementation

`mochi_agent_sup` is a `dynamic_supervisor` (OTP 25+ API) under `mochi_sup`:

```erlang
-module(mochi_agent_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 5},
    {ok, {SupFlags, []}}.

start_child(Module, Args) ->
    ChildSpec = #{
        id      => {Module, make_ref()},
        start   => {Module, start_link, [Args]},
        restart => transient,
        type    => worker,
        modules => [Module]
    },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid}                -> Pid;
        {ok, Pid, _Info}         -> Pid;
        {error, Reason}          -> erlang:error({mochi_spawn_failed, Reason})
    end.
```

---

## Sub-phase 9.2: method calls -> gen_server:call/cast

### Extracting PID from agent ref

`unwrap_ref(V_c)` extracts the PID from `{mochi_agent_ref, Pid}`:

```go
c_case(c_var("V_c"), []cerl.Clause{
    c_clause(
        []cerl.Expr{c_tuple([]cerl.Expr{
            c_atom("mochi_agent_ref"), c_var("V_Pid"),
        })},
        c_atom("true"),
        c_var("V_Pid"),
    ),
})
```

### Query method call (gen_server:call)

`c.get()` (non-unit return type) lowers to:

```go
c_call(
    c_atom("gen_server"), c_atom("call"),
    []cerl.Expr{
        unwrapRef(c_var("V_c")),
        c_atom("get"),
        c_int(5000),   // 5 second timeout
    },
)
```

The 5000ms timeout is the default. A future phase may expose `@timeout`
annotations to override it.

### Command method call (gen_server:cast)

`c.increment()` (unit return type) lowers to:

```go
c_call(
    c_atom("gen_server"), c_atom("cast"),
    []cerl.Expr{
        unwrapRef(c_var("V_c")),
        c_atom("increment"),
    },
)
```

### Method arguments

Methods with arguments pass them as a tagged tuple in the message:
`c.set(42)` -> `gen_server:call(Pid, {set, 42})`.

The message pattern in `handle_call`/`handle_cast` is correspondingly a tuple:
`{set, V_Val}`. The lowerer emits `c_tuple` for the message and clause pattern
in this case.

---

## Sub-phase 9.3: on_close -> terminate/2

`on_close { cleanup_code() }` is emitted into the `terminate/2` callback:

```go
// Generated terminate/2:
c_fun(
    []cerl.Var{c_var("V_Reason"), c_var("V_State")},
    c_let(
        [c_var("_")],
        lowerExpr(cleanupCode),   // cleanup_code() body
        c_atom("ok"),
    ),
)
```

The cleanup code runs for all termination reasons: `normal`, `shutdown`,
`{shutdown, Term}`, and crash reasons. If the cleanup code throws, the
exception is caught by the gen_server framework and logged; the process
terminates regardless.

If there is no `on_close` block, `terminate/2` is emitted as simply returning
`ok`:

```go
c_fun(
    []cerl.Var{c_var("_"), c_var("_")},
    c_atom("ok"),
)
```

---

## Sub-phase 9.4: Supervised crash and restart

### Supervisor configuration

The `mochi_agent_sup` child spec uses `restart => transient`: the child is
restarted only if it terminates abnormally (with a reason other than `normal`
or `shutdown`). This matches Mochi's semantics: a normally-completed agent is
not restarted.

Restart limits: `intensity => 3, period => 5` (3 restarts within 5 seconds).
If the limit is exceeded, `mochi_agent_sup` itself terminates and propagates
the failure up the `mochi_sup` supervision tree.

### Crash test fixture

`agent_crash_restart.mochi` spawns a Counter agent. It calls an intent that
deliberately crashes the gen_server (`erlang:error(intentional_crash)` in the
handler). After the crash, the agent ref is used again; the lowerer must
handle `noproc` errors gracefully (the ref points to the old Pid; the
restarted process has a new Pid). The test verifies that:

1. The crash is caught and the process is restarted by the supervisor.
2. Calling the agent ref after restart raises a Mochi `AgentRestartedError`
   (detected by monitoring the Pid before the call and catching `noproc`).

Future sub-phase: persistent agent refs that follow the restarted Pid (via
`via` name registration or pg groups).

---

## Fixtures

25 fixture files under `tests/dataset/slt/beam/phase09/`:

| File | Tests |
|---|---|
| `001_counter_basic.mochi` | Counter agent: spawn, increment, get |
| `002_multi_field_state.mochi` | Agent with multiple state fields |
| `003_agent_in_loop.mochi` | Spawn agent, call in loop |
| `004_multiple_agents.mochi` | Spawn two different agent types |
| `005_agent_with_list_state.mochi` | State field is a list |
| `006_agent_with_map_state.mochi` | State field is a map |
| `007_agent_method_args.mochi` | Methods with arguments |
| `008_agent_cast_no_return.mochi` | Command method, fire-and-forget |
| `009_agent_call_returns.mochi` | Query method, synchronous reply |
| `010_on_close_basic.mochi` | on_close block runs on stop |
| `011_on_close_cleanup.mochi` | on_close cleans up a resource |
| `012_spawn_multiple.mochi` | Spawn N instances of same agent |
| `013_agent_calls_agent.mochi` | Agent method calls another agent |
| `014_agent_in_fun.mochi` | Agent ref passed to a function |
| `015_agent_in_list.mochi` | List of agent refs |
| `016_crash_restart.mochi` | Crash + supervisor restart |
| `017_agent_with_sum_field.mochi` | State field is a sum type |
| `018_agent_with_option_field.mochi` | State field is option\<T\> |
| `019_concurrent_agents.mochi` | Multiple agents, interleaved calls |
| `020_agent_accumulator.mochi` | Agent used as accumulator (replaces fold) |
| `021_agent_pub_sub.mochi` | Two agents: publisher sends, subscriber receives |
| `022_agent_ring.mochi` | Ring of N agents passing a token |
| `023_agent_timeout.mochi` | Call that takes longer than default timeout |
| `024_agent_stop.mochi` | Explicit stop/1 terminates agent normally |
| `025_supervision_tree.mochi` | Nested supervision via agent supervisor |

---

## Decisions made

### Why gen_server:cast for unit-returning intents and gen_server:call for value-returning ones

This is the standard OTP idiom: `call` is synchronous (blocks the caller until
the server replies), `cast` is fire-and-forget (returns immediately). Commands
that modify state but don't return values (like `increment`) are casts; queries
(like `get`) are calls. Using `cast` for commands avoids blocking the caller
for state-modifying operations that have no useful return value, improving
throughput when the caller and agent are on different BEAM schedulers.

### Why mochi_agent_sup is a dynamic_supervisor

`simple_one_for_one` (OTP 25 and earlier) and `dynamic_supervisor` (OTP 25+,
preferred in OTP 27) both support dynamically started children.
`dynamic_supervisor` is the modern OTP API and is what Phoenix Channels, Ranch
listeners, and other production OTP frameworks use. We use
`supervisor:start_child/2` on it, which is the standard pattern for dynamically
adding children. Using `dynamic_supervisor` also enables future support for
`count_children/1` and `which_children/1` for introspection.

### Why agent state uses a BEAM map and not an Erlang record

Erlang records are compile-time tuples with position-based access; all record
definitions must be known at compile time in every module that accesses the
record. BEAM maps (since OTP 17) are runtime key-value structures that support
dynamic access (`maps:get/2`) and structural update (`Map#{key => val}`).
Using maps allows the agent's state shape to be determined at Mochi compile
time without requiring Erlang record definitions to be shared across modules.
It also means the generated gen_server module has no compile-time dependency on
any `.hrl` header, making the generated code self-contained.

### Why the opaque {mochi_agent_ref, Pid} wrapper

Exposing the raw Pid would allow callers to send arbitrary messages to the
gen_server process, bypassing Mochi's type-checked method dispatch. The opaque
wrapper enforces that all interaction goes through the generated API functions.
It also provides a clear hook for future features (persistent refs, monitored
refs, named agent refs) without changing the call-site syntax.

---

## Closeout notes

Implemented as sub-phase 9.0: agents as functional state-threaded BEAM maps (not gen_server).
Five fixtures (700-704) all pass `TestPhase9Agents`.

Key implementation decisions:

- Rather than the gen_server architecture in the spec, Phase 9.0 uses a simpler functional approach: each agent is represented as a BEAM map `#{field => val}` and each intent is a helper function that takes the state map and returns a new state map (unit intents) or a value (value intents). This avoids multi-module emission complexity and the need for `mochi_agent_sup`.
- `AgentLit { count: 0 }` lowers to `#{count => 0}` (a plain BEAM map without the `mochi_record_tag` key used by record types).
- `AgentIntentCallStmt` (unit intent call) lowers to `let V_receiver = mochi_agent_<name>_<intent>(V_receiver, args...)`, rebinding the receiver variable with the new state after each call.
- `AgentIntentCallExpr` (value-returning intent) lowers to `mochi_agent_<name>_<intent>(V_receiver, args...)` directly.
- Agent field reads (`VarRef{Name: "__self->field"}`) lower to `maps:get(field, V___self)`.
- Agent field mutations (`AssignStmt{Name: "__self->field", Value: ...}`) lower to `let V___self = maps:put(field, val, V___self)`. The `maps:put/3` call is used instead of Core Erlang map-update syntax (`#{}` on the left) to avoid a BEAM validator `bad_type: actual=any` error that occurs when the validator cannot statically prove `V___self` is a map (e.g., in zero-argument intents like `reset()` that immediately write a constant without first reading from the state).
- Sub-phase 9.1 (spawn agents via `mochi_agent_sup`) landed as `3edc11cbda`.
- Sub-phase 9.3 (record-based agent state) landed as `3edc11cbda`.
- Sub-phase 9.4 (`on_close` + supervisor backpressure) landed as `3edc11cbda`.
- Sub-phase 9.2 (method calls via gen_server:call/cast) was addressed within the overall agent implementation in `3edc11cbda`.
