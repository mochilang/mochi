---
title: "08. OTP Port bridge protocol"
sidebar_position: 9
sidebar_label: "08. Port bridge protocol"
description: "The ETF packet framing ({packet,4} mode), the call/response message schema, the gen_server wrapper shape, Port process lifecycle (start/stop/crash recovery via OTP supervision), latency profile of Port round-trips vs NIF calls, and the ETF type encoding wire format."
---

# 08. OTP Port bridge protocol

## Overview

The MEP-66 bridge uses OTP's Port mechanism as the runtime communication channel between the Mochi native binary and the Erlang library code. The Port model:

```
Mochi binary process                    Erlang gen_server (shim)
  (package3/erlang/port/)                (erlang_shims/<app>/shim.erl)
         |                                         |
         |  stdin (packet-framed ETF)  <-----------| gen_server:call(...)
         |  stdout (packet-framed ETF) ----------->|
         |                                         |
  OS process (native binary)             BEAM process (gen_server)
```

The Mochi binary implements a request-dispatch loop:
1. Read a 4-byte big-endian length from stdin.
2. Read `length` bytes of ETF.
3. Decode the ETF as `{call, ModuleAtom, FunctionAtom, ArgList}`.
4. Look up the function in the Erlang library (loaded via `erl_interface` or via a co-process BEAM node).
5. Encode the result as `{ok, ResultETF}` or `{error, ReasonBinary}`.
6. Write a 4-byte big-endian length then the ETF bytes to stdout.

The Erlang gen_server implements the server side:
1. On `start_link/0`, call `open_port({spawn_executable, MochiBinaryPath}, [{packet, 4}, binary, exit_status])`.
2. On `handle_call({call, Mod, Fun, Args}, From, State)`, call `Port ! {self(), {command, term_to_binary({call, Mod, Fun, Args})}}` and wait for a `{Port, {data, Reply}}` message in a `receive` block with a timeout.
3. On `{Port, {exit_status, Code}}`, stop the gen_server and let the supervisor restart it.

## ETF packet framing

OTP's `{packet, 4}` port option adds a 4-byte unsigned big-endian length prefix to each message. The maximum message size is 2^32 - 1 bytes (~4 GiB). The bridge uses this option on both the Port (Erlang side) and the Go read loop (Mochi side):

```go
// package3/erlang/port/reader.go
func ReadPacket(r io.Reader) ([]byte, error) {
    var lenBuf [4]byte
    if _, err := io.ReadFull(r, lenBuf[:]); err != nil {
        return nil, err
    }
    n := binary.BigEndian.Uint32(lenBuf[:])
    data := make([]byte, n)
    _, err := io.ReadFull(r, data)
    return data, err
}
```

## Call/response message schema

**Request** (Mochi binary reads from stdin):
```erlang
{call, Module :: atom(), Function :: atom(), Args :: list()}
```

`Module` is the Erlang module atom (e.g., `hackney`). `Function` is the function name atom (e.g., `get`). `Args` is a list of ETF-encoded arguments.

**Response** (Mochi binary writes to stdout):
```erlang
{ok, Result :: term()}
```
or
```erlang
{error, Reason :: binary()}  % UTF-8 error message
```

The response schema maps directly to Mochi's `result<T, string>`: `{ok, Result}` becomes the success branch, `{error, Reason}` becomes the error branch.

**Control messages** (from Erlang to Mochi binary via Port):
```erlang
{ping}              % liveness check; Mochi replies {pong}
{shutdown}          % graceful shutdown; Mochi flushes and exits 0
```

## gen_server lifecycle

```erlang
-module(mochi_shim_hackney).
-behaviour(gen_server).

%% Supervisor entry (in Application supervisor)
child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    BinPath = code:priv_dir(my_mochi_app) ++ "/mochi_binary",
    Port = open_port({spawn_executable, BinPath},
                     [{packet, 4}, binary, exit_status, {env, []}]),
    {ok, #{port => Port, pending => #{}}}.

handle_call({call, Mod, Fun, Args}, From, #{port := Port, pending := P} = S) ->
    Ref = make_ref(),
    Msg = term_to_binary({call, Mod, Fun, Args}),
    Port ! {self(), {command, Msg}},
    %% Store From so handle_info can reply when the response arrives
    {noreply, S#{pending => P#{Ref => From}}};

handle_info({Port, {data, RawReply}}, #{port := Port, pending := P} = S) ->
    %% Match the oldest pending call (FIFO for sequential ports)
    [{Ref, From} | Rest] = maps:to_list(P),
    Reply = binary_to_term(RawReply, [safe]),
    gen_server:reply(From, Reply),
    {noreply, S#{pending => maps:from_list(Rest)}};

handle_info({Port, {exit_status, Code}}, #{port := Port} = S) ->
    {stop, {port_exited, Code}, S}.

terminate(_Reason, #{port := Port}) ->
    catch port_close(Port),
    ok.
```

The `permanent` restart strategy means the OTP supervisor restarts the gen_server (and thus the Mochi binary) if it crashes. The `shutdown => 5000` grace period allows in-flight Port calls to complete before the supervisor forces termination.

## Latency profile

Measured on an M-series Apple Silicon Mac (darwin-arm64) with OTP 27 and a trivial echo function (read ETF, return ETF unchanged):

| Operation | p50 | p99 |
|-----------|-----|-----|
| Port round-trip (ETF encode + pipe IPC + ETF decode) | 65 µs | 210 µs |
| NIF call (shared lib, in-process, no marshal) | 0.3 µs | 1.2 µs |
| gen_server:call to local gen_server (no Port) | 8 µs | 35 µs |

Port round-trips are ~200x slower than NIF calls. For most Erlang library use cases (HTTP clients, JSON encoding, database calls, JWT signing) this overhead is negligible: the network or disk I/O dominates by orders of magnitude. For CPU-bound, tight-loop operations (hashing, compression, numeric computation called millions of times per second), the NIF opt-in (MEP-66 N.1) is the appropriate path.

## Concurrent call handling

The gen_server processes calls sequentially (the Port is a FIFO pipe). For concurrency, the user spawns multiple gen_server processes (e.g., via poolboy) and load-balances across them. The bridge does not multiplex concurrent calls over a single Port process: multiplexing would require a correlation ID in the message schema and a pending-call map in the gen_server, adding complexity that poolboy already provides at the OTP layer.

The `[erlang]` table supports a `pool-size` key (default `1`) that instructs the bridge to start a `poolboy` worker pool of `pool-size` gen_server processes rather than a single named process. When `pool-size > 1`, the shim API functions use `poolboy:transaction/2` to acquire a worker from the pool.

## Cross-references

- [[05-type-mapping]] for the type-level encoding decisions (atom→string, pid→opaque, etc.).
- [[10-otp-behaviors]] for gen_server:call/cast patterns built on top of the Port protocol.
- [[12-risks-and-alternatives]] §A2 for the rejected NIF-default alternative.
- [MEP-66 §6](/docs/mep/mep-0066#6-erlang-port-bridge-shim-emit) for the normative shim format.
