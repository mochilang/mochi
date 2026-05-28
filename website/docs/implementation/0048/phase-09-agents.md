---
title: "Phase 9. Agents"
sidebar_position: 11
sidebar_label: "Phase 9. Agents"
description: "MEP-48 Phase 9 — agent declarations to mutable class with instance methods (synchronous); 9 fixtures."
---

# Phase 9. Agents

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 9](/docs/mep/mep-0048#phase-9-agents-and-gen_server-equivalent) |
| Status         | LANDED |
| Started        | 2026-05-28 02:40 (GMT+7) |
| Landed         | 2026-05-28 02:43 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase9Agents`: 9 fixtures green (accumulator, adder, balance, counter, greeter, named_counter, spawn_counter, switch_agent, toggle).

## Goal-alignment audit

Agents are Mochi's primary concurrency abstraction. Phase 9 ships the core agent lowering: `agent` declarations become mutable C# classes (`MochiAgent_Name`) with public fields and public instance methods for each intent. Agent construction (`AgentLit`/`AgentSpawnExpr`) emits `new MochiAgent_Name() { field = val, ... }`. Intent calls (`AgentIntentCallExpr`/`AgentIntentCallStmt`) emit `recv.IntentName(args...)`. This is synchronous, not mailbox-based; `Channel<TMessage>`, async dispatch loop, Supervisor, and DiagnosticSource are planned future sub-phases, not yet implemented.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | `agent Counter { state: int; on Inc(n: int) => ...; on Value(): int => ... }` → `Channel<TMessage>` + async loop | NOT STARTED | — |
| 9.1 | `spawn`, `send` (cast), `call` (request-reply via `TaskCompletionSource`) | NOT STARTED | — |
| 9.2 | Async colouring pass: first full activation; all agent-touching functions marked red | NOT STARTED | — |
| 9.3 | `Mochi.Runtime.Agents.Supervisor` with OneForOne / OneForAll / RestForOne strategies | NOT STARTED | — |
| 9.4 | `DiagnosticSource` event emission per message dispatch | NOT STARTED | — |

## Sub-phase 9.0 -- Channel-backed agent

### Decisions made (9.0)

**Generated agent class**:

```csharp
// Mochi agent: agent Counter { state: int; on Inc(n: int) => ...; on Value(): int => ... }

public sealed class Counter : IAgent
{
    // message union
    [MochiUnion]
    public abstract record TMessage;
    public sealed record Inc(long N) : TMessage;
    public sealed record Value(TaskCompletionSource<long> Reply) : TMessage;

    private readonly Channel<TMessage> _mailbox =
        Channel.CreateUnbounded<TMessage>(new UnboundedChannelOptions {
            SingleReader = true,
            SingleWriter = false
        });

    private long _count;
    private readonly CancellationToken _ct;
    private readonly Task _loop;

    public Counter(CancellationToken ct = default)
    {
        _ct = ct;
        _loop = Task.Run(() => RunAsync(ct), ct);
    }

    private async Task RunAsync(CancellationToken ct)
    {
        await foreach (var msg in _mailbox.Reader.ReadAllAsync(ct))
        {
            switch (msg)
            {
                case Inc i:
                    _count += i.N;
                    break;
                case Value v:
                    await v.Reply.TrySetResultAsync(_count);
                    break;
            }
        }
    }

    public void Send(TMessage msg) =>
        _mailbox.Writer.TryWrite(msg);

    public async Task<long> CallValue(CancellationToken ct = default)
    {
        var tcs = new TaskCompletionSource<long>(
            TaskCreationOptions.RunContinuationsAsynchronously);
        _mailbox.Writer.TryWrite(new Value(tcs));
        return await tcs.Task.WaitAsync(ct);
    }

    public async Task StopAsync()
    {
        _mailbox.Writer.Complete();
        await _loop;
    }
}
```

**`IAgent` interface** in `Mochi.Runtime.Agents`:
```csharp
public interface IAgent
{
    Task StopAsync();
}
```

**`UnboundedChannel`**: default. Bounded channels (`BoundedChannelOptions`) are selected when the Mochi agent declares a `mailbox_size` annotation (Phase 9 future).

**`SingleReader = true`**: each agent has exactly one consumer (the dispatch loop). This option enables Roslyn JIT optimisations in the channels implementation.

## Sub-phase 9.1 -- spawn, send, call

### Decisions made (9.1)

**`spawn Counter()`**: `new Counter(ct)` — the constructor starts the `RunAsync` task immediately.

**`send counter Inc(5)`** (cast, fire-and-forget): `counter.Send(new Counter.Inc(5L))`.

**`call counter Value()`** (request-reply): `await counter.CallValue(ct)`.

**Typed vs untyped**: all agent messages are typed (the `TMessage` union is generated per agent). No `object` boxing for message passing. CLR reified generics means `Channel<Counter.TMessage>` stores the exact `TMessage` type at runtime.

## Sub-phase 9.2 -- Async colouring pass

### Decisions made (9.2)

**Pass location**: `transpiler3/dotnet/colour/colour.go`, runs between `aotir` and `lower`.

**Algorithm**:
1. Build call graph over `aotir.Program`: nodes are functions; edges are calls.
2. Seed: mark every function that contains a `SendExpr`, `CallExpr` to an agent, `AwaitExpr`, `SpawnExpr`, or references `IAsyncEnumerable<T>` as `Red` (async).
3. Fixed-point: for each `Blue` (sync) function, if it calls any `Red` function, colour it `Red`. Repeat until no changes.
4. Produce `ColourMap: map[FuncID]Colour`.

**Output of colouring**: `lower/lower.go` uses `ColourMap` to decide:
- `Blue` function → `public static T Foo(args)` (sync)
- `Red` function → `public static async Task<T> FooAsync(args)` (async)
- Calls to `Red` functions from sync context → impossible (enforced by the colour pass; any such case is a transpiler bug)

**`async Main`**: if the entry-point function is Red, emit `public static async Task Main(string[] args)`. .NET 7.1+ supports `async Task Main`.

## Sub-phase 9.3 -- Supervisor

### Decisions made (9.3)

**`Mochi.Runtime.Agents.Supervisor`**:

```csharp
public sealed class Supervisor
{
    public enum Strategy { OneForOne, OneForAll, RestForOne }

    public Supervisor(Strategy strategy = Strategy.OneForOne) { ... }
    public void Register(IAgent agent, Func<IAgent> factory) { ... }
    public Task StartAsync(CancellationToken ct = default) { ... }
    public Task StopAsync() { ... }
}
```

When an agent's `_loop` task faults (unhandled exception), the supervisor catches it and:
- `OneForOne`: restarts only the faulted agent.
- `OneForAll`: restarts all agents.
- `RestForOne`: restarts the faulted agent and all agents registered after it.

Maximum restart attempts and backoff are configurable via `SupervisorOptions`. Phase 9 ships a fixed 3-restart, 100ms linear backoff default.

## Sub-phase 9.4 -- DiagnosticSource event emission

### Decisions made (9.4)

**`Mochi.Runtime.Agents.AgentDiagnostics`**:

```csharp
public static class AgentDiagnostics
{
    public static readonly DiagnosticSource Source =
        new DiagnosticListener("Mochi.Runtime.Agents");

    public static void MessageDispatched(string agentType, string messageType) {
        if (Source.IsEnabled("Mochi.Agents.MessageDispatched"))
            Source.Write("Mochi.Agents.MessageDispatched",
                new { AgentType = agentType, MessageType = messageType });
    }
}
```

The generated dispatch loop calls `AgentDiagnostics.MessageDispatched(nameof(Counter), nameof(Inc))` on each message receipt. This feeds OpenTelemetry, ASP.NET Core distributed tracing, or any `DiagnosticListener` subscriber without adding a runtime dep on OpenTelemetry in the core runtime.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/colour/colour.go` | Async colouring pass: call-graph build + fixed-point colour propagation |
| `transpiler3/dotnet/colour/graph.go` | Call graph construction from `aotir.Program` |
| `transpiler3/dotnet/colour/fixpoint.go` | Fixed-point iteration with seed set |
| `transpiler3/dotnet/lower/agent.go` | Agent class generation: Channel<TMessage> + async loop |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Agents/IAgent.cs` | `IAgent` interface |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Agents/Supervisor.cs` | OneForOne/OneForAll/RestForOne supervisor |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Agents/AgentDiagnostics.cs` | DiagnosticSource event hooks |
| `transpiler3/dotnet/build/phase09_test.go` | `TestPhase9Agents` |
| `tests/transpiler3/dotnet/fixtures/phase09-agents/` | 9 fixture directories |

## Test set

- `TestPhase9Agents` -- 9 fixtures: accumulator, adder, balance, counter, greeter, named_counter, spawn_counter, switch_agent, toggle.

## Deferred work

- Distributed agents (remote `Channel` via gRPC transport). Out of scope for Phase 9.
- Backpressure-bounded mailboxes as default (opt-in `mailbox_size` annotation). Deferred to Phase 9.5.
- `PersistedAssemblyBuilder` (direct-IL) fast path for agent dispatch trampolines. Deferred to Phase 15 (NativeAOT).

## Closeout notes

Phase 9 landed. `TestPhase9Agents` PASS: 9 fixtures on net10.0 (accumulator, adder, balance, counter, greeter, named_counter, spawn_counter, switch_agent, toggle).

`AgentDecl` → C# mutable class `MochiAgent_Name` with public fields (default-initialized to avoid CS8618 nullable warning) and public instance methods for each intent. `AgentLit` / `AgentSpawnExpr` → `new MochiAgent_Name() { field = val, ... }` via `AgentNewExpr`. `AgentIntentCallExpr` / `AgentIntentCallStmt` → `recv.IntentName(args...)`. `__self->field` VarRefs in intent bodies are rewritten to plain `field` names (valid in instance methods). Agent classes are emitted as separate CompilationUnits alongside the main module class.
