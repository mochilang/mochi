---
title: "10. OTP behavior bindings"
sidebar_position: 11
sidebar_label: "10. OTP behaviors"
description: "How the bridge translates stateful OTP gen_server call/cast/info patterns to Mochi's stateless extern fn model via opaque Pid handles, supervisor start/stop/which_children, application start/stop, gen_statem patterns, and the OTP behavior recognition heuristic."
---

# 10. OTP behavior bindings

## The OTP behavior challenge

OTP behaviors (gen_server, gen_statem, supervisor, application) are the core abstraction of the Erlang/OTP platform. They define a server-client model where:

1. A server process is started with `gen_server:start_link/3`.
2. The server runs in an OTP supervision tree and maintains private state.
3. Clients send requests via `gen_server:call/2` (synchronous) or `gen_server:cast/2` (asynchronous).
4. The server replies with a result.

This model is inherently stateful and process-oriented. Mochi's `extern fn` model is stateless and function-oriented: an `extern fn` takes arguments and returns a result with no side channel. Bridging the two requires a strategy.

## Strategy: opaque Pid as server handle

The bridge represents each gen_server process as an opaque `Pid` handle on the Mochi side. Starting a server returns a `Pid`; calling a server takes a `Pid` as the first argument. This makes the stateful server identity explicit in the Mochi type system:

```mochi
extern type Pid
extern type Reference

// Starting a named gen_server (poolboy pool)
extern fn poolboy__start_link(
    pool_args: list<bytes>,
    worker_args: list<bytes>
): result<Pid, string> from erlang "poolboy:start_link/2"

// Checking out a worker from the pool
extern fn poolboy__checkout(pool: Pid): result<Pid, string>
  from erlang "poolboy:checkout/1"

// Checking in a worker
extern fn poolboy__checkin(pool: Pid, worker: Pid): nil
  from erlang "poolboy:checkin/2"
```

The `Pid` handle is passed back to the Erlang shim as an ETF `PID_EXT` term; the Erlang side calls `gen_server:call(Pid, Request)` with the deserialized pid.

## gen_server call/cast patterns

The bridge recognises three gen_server interaction patterns from the module's exported functions and their `-spec` annotations:

### Pattern 1: Named server API (most common)

Many Erlang libraries expose a named gen_server (registered under a module name or an atom) via a public API module that wraps `gen_server:call`:

```erlang
-spec request(Url :: binary(), Opts :: list()) -> 
    {ok, binary()} | {error, atom()}.
request(Url, Opts) ->
    gen_server:call(?MODULE, {request, Url, Opts}).
```

The bridge translates this directly: the exported `request/2` function appears in the shim as a normal `extern fn` that takes `(url: string, opts: list<bytes>): result<bytes, string>`. The gen_server state is hidden behind the module-level named process; the Mochi caller does not need to manage a `Pid`.

### Pattern 2: Explicit server handle

When a module's API takes an explicit server reference (a `pid()` or `gen_server:server_ref()`), the bridge maps the first argument to `Pid`:

```erlang
-spec call(Server :: pid(), Request :: term(), Timeout :: timeout()) ->
    {ok, term()} | {error, term()}.
```

The bridge translates `pid()` arguments to `Pid` and `timeout()` to `int` (milliseconds).

### Pattern 3: Reference-based async

Some libraries use `gen_server:cast/2` followed by a `receive` on a `Reference` for async patterns:

```erlang
-spec async_request(Pid :: pid(), Req :: binary()) -> reference().
async_request(Pid, Req) ->
    Ref = make_ref(),
    gen_server:cast(Pid, {request, Ref, Req, self()}),
    Ref.
```

The `make_ref()` idiom produces an opaque `Reference` handle. The bridge maps `reference()` return types to `extern type Reference`. The subsequent receive loop is out of scope for the static bridge (it would require pattern-matching on messages, which is MEP-66 phase 12).

## supervisor bindings

The `supervisor` module's key API functions are translated:

```mochi
// supervisor:start_link/3 - start a supervision tree
extern fn supervisor__start_link(
    module: string,
    args: bytes
): result<Pid, string> from erlang "supervisor:start_link/3"

// supervisor:which_children/1 - list running children
extern fn supervisor__which_children(
    sup: Pid
): list<[string, Pid, string, list<string>]>
  from erlang "supervisor:which_children/1"

// supervisor:terminate_child/2
extern fn supervisor__terminate_child(sup: Pid, id: string): result<nil, string>
  from erlang "supervisor:terminate_child/2"

// supervisor:restart_child/2
extern fn supervisor__restart_child(sup: Pid, id: string): result<Pid, string>
  from erlang "supervisor:restart_child/2"
```

The `supervisor:which_children/1` return type is `[{Id, Child, Type, Modules}]` where `Child` is a `pid() | restarting | undefined`. The bridge maps this as `list<[string, Pid, string, list<string>]>` (collapsing the `restarting | undefined` cases; a SkipNote documents the approximation).

## application bindings

The `application` module manages OTP application lifecycle:

```mochi
extern fn application__start(app: string): result<nil, string>
  from erlang "application:start/1"

extern fn application__stop(app: string): result<nil, string>
  from erlang "application:stop/1"

extern fn application__get_env(app: string, key: string): string?
  from erlang "application:get_env/2"

extern fn application__set_env(app: string, key: string, value: bytes): nil
  from erlang "application:set_env/3"
```

`application:start/1` and `application:stop/1` take an atom (the application name); the bridge maps this to `string`.

## OTP behavior recognition heuristic

The bridge applies a heuristic to classify modules into behavior categories:

1. If the module's abstract code contains a `-behaviour(gen_server)` attribute, it is classified as a gen_server.
2. If it contains `-behaviour(supervisor)` or `-behaviour(supervisor_bridge)`, it is classified as a supervisor.
3. If it contains `-behaviour(gen_statem)` or `-behaviour(gen_fsm)`, it is classified as a state machine.
4. If it contains `-behaviour(application)`, it is classified as an OTP application callback.

Classified modules get special shim treatment: instead of exporting their internal callback functions (`init/1`, `handle_call/3`, etc.), the bridge exports only their public API functions (those exported with `-spec` annotations that do not match the OTP callback signatures). The internal callbacks are suppressed from the `extern fn` corpus because they are server-side handlers, not client-side calls.

## Phase 11 gate

Phase 11 is LANDED when the following fixture corpus packages produce clean `extern fn` corpora that include the expected gen_server/supervisor bindings:

- `cowboy` (gen_server + supervisor patterns via ranch)
- `poolboy` (gen_server pool; explicit `Pid` handle pattern)
- `gproc` (global process registry; named server pattern)
- `ranch` (supervisor + listener pattern)
- `recon` (application lifecycle query)

## Cross-references

- [[08-port-bridge-protocol]] for the runtime call/response mechanism underlying the gen_server call translation.
- [[05-type-mapping]] for the `pid()` → `Pid` and `reference()` → `Reference` mapping.
- [[12-risks-and-alternatives]] §R4 for the risk of complex OTP callback patterns escaping the heuristic.
