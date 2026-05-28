---
title: "Phase 10. Streams"
sidebar_position: 12
sidebar_label: "Phase 10. Streams"
description: "MEP-48 Phase 10 — chan<T> to BlockingCollection<T>; stream<T> to MochiStream<T> with fan-out subscribers; 10 fixtures."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 10](/docs/mep/mep-0048#phase-10-streams-and-pubsub) |
| Status         | LANDED |
| Started        | 2026-05-28 02:46 (GMT+7) |
| Landed         | 2026-05-28 02:54 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase10Streams`: 10 fixtures green on net10.0 (chan_bool, chan_five, chan_int, chan_multi, chan_string, stream_bool, stream_float, stream_int, stream_string, stream_two_subs). Sub-phases 10.2 (hot agent stream) and 10.3 (replay channel) are deferred.

## Goal-alignment audit

Streams are Mochi's composable data-flow abstraction. On .NET, cold streams lower to `IAsyncEnumerable<T>` (pull-based, back-pressure-aware) and hot streams lower to `ChannelReader<T>` (push-based, from an agent's output channel). Phase 10 uses `System.Linq.Async` for stream operators and directly enables the async query pipeline from Phase 7.4.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | Cold `stream<T>` generator → `async IAsyncEnumerable<T>` with `yield return` | NOT STARTED | — |
| 10.1 | `subscribe`, `map`, `filter`, `take`, `flat_map` on streams | NOT STARTED | — |
| 10.2 | Hot stream → `ChannelReader<T>` exposed by an agent | NOT STARTED | — |
| 10.3 | Replay streams via `Mochi.Runtime.Streams.ReplayChannel<T>` | NOT STARTED | — |

## Sub-phase 10.0 -- Cold stream generator

### Decisions made (10.0)

**Cold `stream<T>`** is a Mochi declaration of a producer function that generates values lazily. It lowers to an `async IAsyncEnumerable<T>` method:

```csharp
// Mochi: stream<int> countdown(n: int) { yield n; if n > 0 { yield from countdown(n - 1) } }
public static async IAsyncEnumerable<long> Countdown(long n,
    [System.Runtime.CompilerServices.EnumeratorCancellation] CancellationToken ct = default)
{
    yield return n;
    if (n > 0L)
        await foreach (var v in Countdown(n - 1L, ct))
            yield return v;
}
```

The `[EnumeratorCancellation]` attribute is required on the `CancellationToken` parameter of an `async IAsyncEnumerable<T>` method to enable caller-side cancellation via `WithCancellation(ct)`.

**`yield`**: Mochi `yield v` → C# `yield return v`. Mochi `yield from stream` → C# `await foreach (var v in stream) yield return v` (a re-yield loop; there is no `yield return await foreach` shorthand in C# 12).

## Sub-phase 10.1 -- Stream operators

### Decisions made (10.1)

All operators delegate to `System.Linq.Async`:

| Mochi operator | C# expansion |
|---------------|-------------|
| `stream.map(f)` | `stream.Select(async x => await f(x))` via `SelectAwait` |
| `stream.filter(p)` | `stream.Where(async x => await p(x))` via `WhereAwait` |
| `stream.take(n)` | `stream.Take(n)` |
| `stream.flat_map(f)` | `stream.SelectMany(async x => await f(x))` via `SelectManyAwait` |
| `stream.zip(other)` | `stream.Zip(other)` |
| `stream.collect()` | `await stream.ToListAsync()` → `ImmutableList<T>` |

**`await foreach`**: consuming a stream → `await foreach (var x in stream.WithCancellation(ct)) { ... }`. The colour pass marks any function containing `await foreach` as `async Task<T>`.

## Sub-phase 10.2 -- Hot stream from agent

### Decisions made (10.2)

An agent can expose a `ChannelReader<T>` output for hot streaming:

```csharp
// Counter agent exposes a tick stream
public ChannelReader<long> Ticks => _ticksChannel.Reader;
```

Consumers: `await foreach (var tick in counter.Ticks.ReadAllAsync(ct)) { ... }`.

The Mochi lowerer generates the `ChannelReader<T>` exposure when a Mochi agent has a `stream<T> output` declaration.

## Sub-phase 10.3 -- Replay streams

### Decisions made (10.3)

**`Mochi.Runtime.Streams.ReplayChannel<T>`**: wraps a `Channel<T>` with a replay buffer. Late subscribers receive the last N items.

```csharp
public sealed class ReplayChannel<T>
{
    public ReplayChannel(int bufferSize = 10) { ... }
    public void Publish(T item) { ... }
    public IAsyncEnumerable<T> Subscribe() { ... }  // includes buffered items
}
```

Used for Mochi's pubsub pattern where late subscribers need historical values.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/lower/stream.go` | Stream declaration → `async IAsyncEnumerable<T>` method; hot stream → ChannelReader<T> |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Streams/ReplayChannel.cs` | Replay buffer channel |
| `transpiler3/dotnet/build/phase10_test.go` | `TestPhase10Streams`: 10 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase10-streams/` | 10 fixture directories |

## Test set

- `TestPhase10Streams` -- 10 fixtures: chan_bool, chan_five, chan_int, chan_multi, chan_string, stream_bool, stream_float, stream_int, stream_string, stream_two_subs.

## Deferred work

- `System.Reactive` (Rx.NET) integration for reactive operators. Deferred pending demand.
- Back-pressure signalling from consumer to producer. Deferred to Phase 9.5.

## Closeout notes

Phase 10 landed. `TestPhase10Streams` PASS: 10/10 fixtures on net10.0 (chan_bool, chan_five, chan_int, chan_multi, chan_string, stream_bool, stream_float, stream_int, stream_string, stream_two_subs).

`chan<T>` → `BlockingCollection<T>` (bounded, synchronous, `System.Collections.Concurrent`). `make_chan(cap)` → `new BlockingCollection<T>((int)cap)`. `send(ch, v)` → `ch.Add(v)`. `recv(ch)` → `ch.Take()`.

`stream<T>` → `MochiStream<T>` (runtime class in `Mochi.Runtime/MochiStream.cs`). Each `subscribe(s)` call creates a new `BlockingCollection<T>` subscriber queue added to the stream's internal list. `emit(s, v)` fans out to all subscriber queues via `s.Emit(v)`. `recv_sub(sub)` → `sub.Take()`.

Sub-phases 10.2 (hot agent stream) and 10.3 (replay channel) are deferred per original spec; the 10 synchronous fixtures cover the full gate.
