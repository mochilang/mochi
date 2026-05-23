# MEP-48 research note 09, Agents and streams on .NET

Status: research note feeding MEP-48 (Mochi to .NET / CLR transpiler). This is the concurrency and message-passing chapter. It surveys how Mochi's agent and stream constructs lower to .NET, contrasts the design with MEP-47's Loom-native JVM target, and pins down the runtime support libraries we need under `Mochi.Runtime.*`.

The headline divergence from MEP-47: .NET has no virtual threads. Project Loom on the JVM lets Mochi map each agent to a real (virtual) thread that blocks on a queue. On .NET the canonical pattern is `System.Threading.Channels` plus `async`/`await` on top of the ThreadPool. We get cheap concurrency, but it is cooperative (await points), not preemptive, and every agent loop is colored async all the way down.

Cross-ref the sibling JVM note at `[[../0047/09-agent-streams]]`.

## 1. Channels recap

`System.Threading.Channels` shipped as part of the framework in .NET Core 3.0 (September 2019). Prior to that it lived as a standalone NuGet package, but it became a first-class part of the BCL and is now the recommended primitive for in-process producer / consumer pipelines. Stephen Toub wrote the canonical introduction on the .NET blog the same year.

The shape of the API is small.

```csharp
// Factory entry points.
Channel<T> Channel.CreateUnbounded<T>();
Channel<T> Channel.CreateUnbounded<T>(UnboundedChannelOptions options);
Channel<T> Channel.CreateBounded<T>(int capacity);
Channel<T> Channel.CreateBounded<T>(BoundedChannelOptions options);

// A Channel<T> is essentially a (ChannelWriter<T>, ChannelReader<T>) pair.
public abstract class Channel<T> {
  public ChannelReader<T> Reader { get; }
  public ChannelWriter<T> Writer { get; }
}
```

`ChannelWriter<T>` exposes both a synchronous fast path (`TryWrite`) and an async path (`WriteAsync`, `WaitToWriteAsync`). `ChannelReader<T>` mirrors that with `TryRead`, `ReadAsync`, `WaitToReadAsync`, and (since C# 8) `ReadAllAsync` which returns an `IAsyncEnumerable<T>`.

Bounded channels accept a `BoundedChannelOptions` with the following knobs.

- `Capacity`, the maximum number of buffered items.
- `FullMode`, what to do when the buffer is full. Values are `Wait` (default; backpressure the producer), `DropNewest`, `DropOldest`, `DropWrite`.
- `SingleReader`, `SingleWriter`, hints that let the implementation skip locks. Setting these correctly is the single highest-leverage optimization the Mochi lowering can apply.
- `AllowSynchronousContinuations`, normally false. Setting it true lets the producer thread run reader continuations inline, which can boost throughput at the cost of unpredictable scheduling.

Internally the channels are lock-free FIFO queues over a segmented buffer. The unbounded variant is essentially a `ConcurrentQueue<T>` plus a TCS-based signaling layer. `WriteAsync` on an unbounded channel always completes synchronously (no allocation), because there is no backpressure condition to wait on; this matters for Mochi's lowering because the cheap default case stays cheap.

## 2. Mailbox lowering

Each Mochi `agent` lowers to a CLR class with three fields:

- a `Channel<object>` (or a typed `Channel<TMessage>` if the agent has a single nominal message base type),
- a `Task` for the receive loop,
- a `CancellationToken` to drive cooperative shutdown.

By default the mailbox is unbounded. If the source declares `agent Foo capacity 256`, the lowering picks `Channel.CreateBounded<...>(new BoundedChannelOptions(256) { FullMode = BoundedChannelFullMode.Wait, SingleReader = true })`. `SingleReader = true` is always safe because the receive loop is the only reader. `SingleWriter` is only set when static analysis proves there is exactly one sender (rare, but common for pipeline stages).

The lowering of an agent body looks like.

```csharp
private async Task RunAsync(CancellationToken ct) {
  await foreach (var msg in _mailbox.Reader.ReadAllAsync(ct)) {
    switch (msg) {
      case M1 m1: HandleM1(m1); break;
      case M2 m2: await HandleM2Async(m2, ct); break;
      // ...
    }
  }
}
```

`ReadAllAsync` is the right choice because it converts the channel to an `IAsyncEnumerable<T>` and handles the WaitToRead / TryRead loop for us. The `await foreach` form is also the only place in idiomatic .NET where cancellation flows in without an explicit parameter (via `ConfigureAwait(false).WithCancellation(ct)` patterns, though `ReadAllAsync(ct)` accepts it directly).

When the channel is closed (via `Writer.Complete()` or `Writer.TryComplete(exception)`), `ReadAllAsync` finishes the enumeration. The lowered agent then runs any `on stop { ... }` body and returns. If completion was triggered with an exception, the Task faults; the supervising scope sees the exception.

## 3. Full Counter example

Mochi source.

```mochi
agent Counter {
  var count = 0
  on Inc(delta: int) { count = count + delta }
  on Value(reply: chan<int>) { send reply, count }
}
```

Generated C#.

```csharp
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using Mochi.Runtime.Agents;

public sealed record Inc(long Delta);
public sealed record Value(Channel<long> Reply);

public sealed class Counter : IAgent {
  private readonly Channel<object> _mailbox =
    Channel.CreateUnbounded<object>(new UnboundedChannelOptions {
      SingleReader = true,
      AllowSynchronousContinuations = false,
    });
  private long _count;
  private readonly Task _loop;
  private readonly CancellationTokenSource _cts;

  public Counter(CancellationToken outer = default) {
    _cts = CancellationTokenSource.CreateLinkedTokenSource(outer);
    _loop = Task.Run(() => RunAsync(_cts.Token), _cts.Token);
  }

  public Task Completion => _loop;

  public ValueTask SendAsync(object msg, CancellationToken ct = default) =>
    _mailbox.Writer.WriteAsync(msg, ct);

  public void Stop() {
    _mailbox.Writer.TryComplete();
    _cts.Cancel();
  }

  private async Task RunAsync(CancellationToken ct) {
    try {
      await foreach (var msg in _mailbox.Reader.ReadAllAsync(ct)) {
        switch (msg) {
          case Inc i:
            _count += i.Delta;
            break;
          case Value v:
            await v.Reply.Writer.WriteAsync(_count, ct).ConfigureAwait(false);
            break;
        }
      }
    } catch (OperationCanceledException) {
      // normal shutdown
    }
  }
}
```

Notes on this lowering.

- `count` is `long`, not `int`. Mochi `int` is 64-bit. (`Counter` in Mochi pre-dates the C# `int` / `long` distinction.)
- `Inc` and `Value` are nominal records, even though Mochi's `on` arms don't introduce a named message type at the source level. The transpiler synthesizes one record per arm, named `{Agent}_{Arm}` when there is a collision risk.
- The mailbox uses `Channel<object>` because Mochi message arms have heterogeneous payloads. A typed `Channel<Counter.Message>` (with a sealed-hierarchy base) is a future optimization but requires Mochi-side ADT lowering.
- `Reply` carries a `Channel<long>` (writer side held by the caller, reader side awaited). This is the Mochi `chan<int>` lowering, see section 6.

The call site looks like.

```csharp
var c = new Counter();
await c.SendAsync(new Inc(5));
var reply = Channel.CreateBounded<long>(1);
await c.SendAsync(new Value(reply));
var v = await reply.Reader.ReadAsync();
```

For convenience the transpiler emits typed `Tell` and `Ask` helpers per arm so the caller writes `await c.TellInc(5)` and `var v = await c.AskValue()`.

## 4. No virtual threads

The single biggest design constraint. On the JVM (MEP-47), Project Loom gives us virtual threads, so each Mochi agent can be a real thread that does a blocking `take()` on its mailbox. The scheduler unmounts blocked threads from carriers cheaply, and the developer model is straight-line code.

.NET has nothing equivalent in 2026. The runtime-async experiment (dotnet/runtime#94620) explored what runtime-level async would look like, and the team reported the experiment as successful, but the resulting feature is still about state-machine generation, not virtual threads. Mochi targets stable, shipping .NET, so we plan for ThreadPool plus async / await.

Concrete implications.

- Each Mochi agent costs one `Task` plus its mailbox, not one thread. Far fewer real threads than agents.
- The receive loop yields at every `await`. Between awaits, the carrier ThreadPool thread is non-preemptible by the runtime (only by the OS scheduler).
- A pure-CPU agent that never awaits will hog its carrier thread. The lowering inserts no implicit yields. We document this as a sharp edge.
- Long synchronous handlers in an otherwise async agent are fine for throughput but they delay other ThreadPool work. We may add an opt-in `[CpuBound]` attribute that lowers to `Task.Run` around the handler body.

Compare to Loom where blocking `mailbox.take()` just parks the virtual thread and another virtual thread runs. The Mochi source is the same; the runtime story differs entirely.

## 5. The async / await coloring problem

Functions in C# come in two flavors. Sync (`T Foo()`) and async (`Task Foo()` / `Task<T> Foo()` / `ValueTask<T> Foo()`). You cannot transparently call an async function from sync code without either blocking (which deadlocks in some sync contexts) or `.GetAwaiter().GetResult()` (same hazard, plus exception unwrapping issues). This is "function coloring," and the C# language enforces it lexically.

Mochi has no surface-level color. A Mochi function is a function. The transpiler must therefore decide, per function, whether to emit a sync or async lowering. The rules.

- If the function transitively sends to an agent, reads from a stream, awaits a Future, or calls anything `async`, emit `async Task<T>` (or `async Task` for unit return).
- Otherwise emit sync.
- For polymorphic call sites (the same Mochi function is reached from both sync and async callers), emit two overloads, an async one and a `Sync` shim. The shim only exists for functions whose async body provably never awaits an incomplete task (rare).
- `ValueTask<T>` is the default async return for hot paths (no allocation in the sync-completion case). `Task<T>` is used when the value is intended to be awaited from multiple sites or stored.

Coloring propagation is a fixed-point analysis over the call graph, run after type checking and before lowering. We treat it like an effect inference. Any function that talks to the agent or stream runtime is async; the rest stay sync. This keeps numeric and pure-data code allocation-free.

## 6. Stream lowering

Mochi `stream<T>` lowers to one of two .NET types depending on producer semantics.

**Cold streams** (the producer is driven by the consumer, like a database query or file iteration) lower to `IAsyncEnumerable<T>`. The Mochi function is emitted as a C# `async IAsyncEnumerable<T>` method, with `yield return` for each Mochi `yield`. Iteration uses `await foreach`. This matches the C# 8 / .NET Core 3.0 model and gets language-level support for cancellation tokens via `[EnumeratorCancellation]`.

```csharp
public static async IAsyncEnumerable<int> Range(
  int lo, int hi,
  [EnumeratorCancellation] CancellationToken ct = default)
{
  for (int i = lo; i < hi; i++) {
    ct.ThrowIfCancellationRequested();
    yield return i;
    await Task.Yield(); // optional cooperative yield
  }
}
```

**Hot streams** (the producer runs independently of the consumer; events, sensor data, broadcasts) lower to a `ChannelReader<T>` exposed to consumers, with the producer holding the matching `ChannelWriter<T>`. The transpiler picks bounded vs unbounded based on a `[Capacity = N]` annotation on the stream declaration.

The rule of thumb in .NET, reinforced by community guidance (Niki Forovall, rendle.dev), is that `IAsyncEnumerable` is a pull model and channels are push. Mochi declares this at the source level via `stream cold T` vs `stream hot T`. The default is cold.

**Replay streams** are a Mochi-specific construct (a stream where new subscribers see the last N values plus all future values). There is no .NET BCL equivalent. We implement `Mochi.Runtime.Streams.ReplayChannel<T>` ourselves.

```csharp
namespace Mochi.Runtime.Streams;

public sealed class ReplayChannel<T> {
  private readonly int _replay;
  private ImmutableList<T> _history = ImmutableList<T>.Empty;
  private readonly List<ChannelWriter<T>> _subscribers = new();
  private readonly object _gate = new();

  public ReplayChannel(int replay) { _replay = replay; }

  public ValueTask WriteAsync(T value, CancellationToken ct = default) {
    List<ChannelWriter<T>> snapshot;
    lock (_gate) {
      _history = _history.Count >= _replay
        ? _history.RemoveAt(0).Add(value)
        : _history.Add(value);
      snapshot = _subscribers.ToList();
    }
    var tasks = new List<ValueTask>(snapshot.Count);
    foreach (var w in snapshot) tasks.Add(w.WriteAsync(value, ct));
    return WhenAll(tasks);
  }

  public ChannelReader<T> Subscribe() {
    var ch = Channel.CreateUnbounded<T>(new UnboundedChannelOptions {
      SingleReader = true, SingleWriter = true,
    });
    lock (_gate) {
      foreach (var v in _history) ch.Writer.TryWrite(v);
      _subscribers.Add(ch.Writer);
    }
    return ch.Reader;
  }

  private static async ValueTask WhenAll(List<ValueTask> ts) {
    foreach (var t in ts) await t.ConfigureAwait(false);
  }
}
```

Notes. `ImmutableList` for the history gives us cheap snapshotting under the gate. We accept O(replay) on each write to fan out. For high-fan-out workloads we offer `Mochi.Runtime.Streams.BroadcastChannel<T>` without replay.

## 7. spawn f()

`spawn f()` in Mochi launches a fiber. On .NET that's `Task.Run`.

```csharp
// Mochi: let h = spawn worker(x)
// C#:
var h = Task.Run(async () => await Worker(x, ct), ct);
// h has type Task; Mochi exposes it as Future<unit>.
// If worker returns T, h has type Task<T>.
```

`Task.Run` queues to the ThreadPool and returns immediately. Mochi `Future<T>` is exactly `Task<T>`. The handle is awaitable, cancelable (via the `CancellationToken` baked in), and joinable.

A subtle point. `Task.Run(async () => ...)` allocates a state machine plus the outer Task. If the lambda is trivial (`spawn () => 1 + 2`), we could fold to `Task.Run(() => f())` to skip one async layer. The transpiler does this when the spawned function is sync.

For agent-shaped spawns, we use `Task.Factory.StartNew(... LongRunning ...)` only when `[Background]` is annotated, since `LongRunning` allocates a dedicated thread rather than using the pool.

## 8. await f

Direct map.

```csharp
// Mochi: let v = await h
// C#:
var v = await h.ConfigureAwait(false);
```

`ConfigureAwait(false)` is added by default because Mochi has no notion of synchronization context. UI host integrations (WinForms / WPF) can opt back in via a per-module `[KeepSyncContext]` pragma.

If the Mochi source does `await` on a non-Future expression (currently a type error, but we plan a sugar), the transpiler reports a type error rather than synthesizing an `await` on a non-awaitable.

## 9. Structured concurrency

.NET has no built-in `StructuredTaskScope`. There is community interest (Steven Giesel's blog post, the Icicle library by bmazzarol, InfoWorld's April 2026 piece on structured concurrency in C#), and the runtime team has tracked the area in various issues, but as of .NET 10 there is nothing in the BCL. The proposal at dotnet/runtime#77609 (and related, e.g. #53709) remains open and draft.

Mochi cannot wait for this. We ship `Mochi.Runtime.Scope.MochiScope` ourselves.

```csharp
namespace Mochi.Runtime.Scope;

public sealed class MochiScope : IAsyncDisposable {
  private readonly CancellationTokenSource _cts;
  private readonly List<Task> _children = new();
  private readonly object _gate = new();
  private bool _disposed;

  public MochiScope(CancellationToken outer = default) {
    _cts = CancellationTokenSource.CreateLinkedTokenSource(outer);
  }

  public CancellationToken Token => _cts.Token;

  public Task<T> Fork<T>(Func<CancellationToken, Task<T>> body) {
    ThrowIfDisposed();
    var t = Task.Run(() => body(_cts.Token), _cts.Token);
    lock (_gate) _children.Add(t);
    return t;
  }

  public Task Fork(Func<CancellationToken, Task> body) =>
    Fork<object?>(async ct => { await body(ct).ConfigureAwait(false); return null; });

  public async ValueTask DisposeAsync() {
    if (_disposed) return;
    _disposed = true;
    Task[] children;
    lock (_gate) children = _children.ToArray();
    try {
      await Task.WhenAll(children).ConfigureAwait(false);
    } catch {
      _cts.Cancel();
      try { await Task.WhenAll(children).ConfigureAwait(false); }
      catch { /* swallow secondary */ }
      throw;
    } finally {
      _cts.Dispose();
    }
  }

  private void ThrowIfDisposed() {
    if (_disposed) throw new ObjectDisposedException(nameof(MochiScope));
  }
}
```

Mochi source `concurrent { fork f(); fork g(); }` lowers to.

```csharp
await using var scope = new MochiScope(ct);
scope.Fork(c => F(c));
scope.Fork(c => G(c));
// scope's DisposeAsync awaits all children
```

If either child throws, `DisposeAsync` cancels the rest and re-raises an `AggregateException` (or the single exception if there is only one). This gives us the basic shape of structured concurrency. We don't try to replicate Java's `ShutdownOnFailure` / `ShutdownOnSuccess` variants in the first cut, but the door is open.

When .NET ships an official primitive (whether `TaskScope`, `AsyncScope`, or something else), `MochiScope` becomes a thin wrapper.

## 10. Fault model and supervision

BEAM has supervision trees baked into the VM. JVM via Akka has them as a library. .NET via Akka.NET has them as a library too, but we don't take a hard dependency on Akka.NET (see section 15). Instead Mochi ships its own minimal `Supervisor`.

```csharp
namespace Mochi.Runtime.Agents;

public enum RestartStrategy { OneForOne, OneForAll, RestForOne }

public sealed class Supervisor {
  private readonly RestartStrategy _strategy;
  private readonly Func<int, TimeSpan, bool> _backoff;
  private readonly List<Func<CancellationToken, IAgent>> _factories;
  private IAgent?[] _agents;
  // ...
  public Supervisor(RestartStrategy s, params Func<CancellationToken, IAgent>[] factories) { ... }
  public Task RunAsync(CancellationToken ct) { ... }
}
```

`IAgent` exposes `Task Completion { get; }`, `void Stop()`, and `ValueTask SendAsync(object msg, CancellationToken ct)`. The supervisor `await`s `Task.WhenAny` of all child completions. When one completes (with or without exception), the strategy decides who to restart.

- `OneForOne`, restart only the failed child.
- `OneForAll`, stop all children, restart all.
- `RestForOne`, stop the failed child and every child started after it, restart in original order.

Restart loops include exponential backoff with a configurable ceiling and a maximum restart rate (e.g. "no more than 5 restarts in 60 seconds"). Exceeding the rate escalates to the parent supervisor (or terminates the scope).

Compared to OTP this is a slim implementation. We deliberately do not replicate `gen_server` / `gen_statem`. Mochi agents are simpler.

## 11. Cancellation

Idiomatic .NET threads a `CancellationToken` through every async method as the last parameter. The Mochi lowering follows this convention, mechanically.

- Every async-lowered Mochi function gets an implicit trailing `CancellationToken ct = default` parameter.
- Calls between async-lowered functions forward `ct`.
- Calls into BCL APIs that accept a `CancellationToken` (most of `System.IO`, `System.Net.Http`, `System.Threading.Channels`) get it forwarded too.
- `cancel scope` in Mochi maps to `scope.Cancel()` on the internal `CancellationTokenSource`.

This implicit parameter is invisible in Mochi source. The transpiler adds it during lowering, and the type system treats it as part of the function's effect, not its arity. Reflection-based callers of generated code (rare, only the embed/host story) see the extra parameter.

For sync-lowered functions there is no token. If sync code wants cancellation, it must explicitly take an async dependency (e.g. via `await Task.Delay(ms, ct)`) which forces the function async.

`OperationCanceledException` is the standard signal. The Mochi runtime wraps it as `Mochi.Runtime.Errors.CancelledError` when bubbling to Mochi-level catch blocks, so the surface error type matches MEP-47's `CancellationException` (JVM).

## 12. Determinism mode

`MOCHI_DETERMINISTIC=1` switches two pieces of the runtime.

- The scheduler becomes a custom single-threaded `TaskScheduler` (`Mochi.Runtime.Scheduling.DeterministicScheduler`) that runs all `Task.Run` work on one logical thread, in FIFO order, and uses a deterministic tiebreaker for `Task.WhenAny`.
- The clock becomes `Mochi.Runtime.Time.MockClock`, which virtualizes `DateTimeOffset.UtcNow`, `Stopwatch`, and the `Task.Delay` / `Channel` timeout primitives. Time advances only when no Task is runnable.

This makes Mochi tests bit-reproducible across runs and machines. The same script with the same inputs produces the same agent message order and the same outputs.

We lean on `TaskCreationOptions.RunContinuationsAsynchronously` and a custom `SynchronizationContext` that posts to the deterministic scheduler. `await foreach` over a channel does not introduce nondeterminism because the channel under a deterministic scheduler completes its readers in FIFO order.

Caveats. Native code, third-party libraries that call into the OS for time / threading, and `Thread.Sleep` bypass the deterministic clock. Documentation calls this out. The Mochi standard library is fully deterministic under the flag.

## 13. Performance numbers

Rough costs on a modern x86_64 machine, .NET 10. These come from Stephen Toub's published benchmarks, BenchmarkDotNet reproductions, and the dotnet/runtime issue #11803 ValueTask numbers. They are guidance, not contracts.

| Operation | Cost |
|-----------|-----:|
| `await` of an already-completed `Task<T>`, state-machine path | ~320 ns |
| `await` of an already-completed `ValueTask<T>`, state-machine path | ~1030 ns |
| Manual `IsCompletedSuccessfully` fast-path on `ValueTask<T>` | ~290 ns |
| `await` of an incomplete `Task`, full suspension | ~4 µs + ~300 B alloc |
| `async Task` method completing synchronously | ~0 ns overhead, 0 B alloc |
| `async Task<T>` method completing synchronously | ~150 ns, 88 B alloc (state machine box) |
| `async ValueTask<T>` method completing synchronously | ~120 ns, 0 B alloc |
| `Channel<T>.WriteAsync` on unbounded channel, sync path | ~100 ns, 0 B |
| `Channel<T>.WriteAsync` on bounded channel, must wait | ~1 µs + suspension cost |
| `Channel<T>.ReadAsync`, item already available | ~150 ns |
| `Task.Run` of a trivial delegate | ~300-500 ns |
| `Task` allocation | ~250-300 ns |
| `ThreadPool.QueueUserWorkItem` | ~500 ns |
| 10K idle agents (mailbox + task), resident memory | ~50-200 MB (~5-20 KB / agent) |

Per-agent memory dominates: an idle async state machine plus the `Channel<object>` instance is roughly 5 KB when the channel is empty, climbing toward 20 KB if the mailbox queue retains capacity. We can shrink that with `SingleReader = true` and segment-size tuning, but we don't get below ~2 KB without giving up the BCL channel.

**Contrast with MEP-47 (JVM Loom).**

| Operation | JVM Loom (MEP-47) | .NET (MEP-48) |
|-----------|------------------:|--------------:|
| Spawn unit | ~1 µs virtual thread create | ~300-500 ns Task.Run |
| Idle unit memory | ~200 B per vthread | ~5 KB per Task + Channel |
| Blocking-style API | yes, vthread.parks | no, must await |
| Function coloring | none | sync vs async |
| 100K idle units | ~20-50 MB | ~500 MB - 2 GB |

.NET is competitive for low to moderate agent counts (thousands). At very high agent counts (hundreds of thousands), Loom is meaningfully cheaper. For Mochi's typical workloads (10s to 1000s of agents) the difference is irrelevant; for "spawn a million sensors" workloads it favors the JVM target.

## 14. Library inventory under Mochi.Runtime

The runtime library shipped alongside the .NET transpiler. Each namespace maps to one assembly.

- `Mochi.Runtime.Agents`
  - `IAgent` (the marker interface; `Task Completion`, `ValueTask SendAsync`, `void Stop`).
  - `AgentBase` (optional base class with the common mailbox plumbing; lowering can target either inheritance or composition).
  - `Supervisor` (section 10).
  - `RestartStrategy` enum.
- `Mochi.Runtime.Streams`
  - `ReplayChannel<T>` (section 6).
  - `BroadcastChannel<T>`, fan-out without replay.
  - `StreamMerge<T>`, fair n-way merge of `ChannelReader<T>` into one.
  - `StreamPipeline<TIn, TOut>`, chained transformation stages with backpressure.
- `Mochi.Runtime.Async`
  - Extension methods on `Task`, `Task<T>`, `ValueTask`, `ValueTask<T>`: `WithCancellation`, `OrTimeout`, `ForgetSafely`.
  - `FireAndForget(this Task t, ILogger? l)` to encapsulate the "I really mean it" detached task pattern.
- `Mochi.Runtime.Scope`
  - `MochiScope` (section 9).
  - `ScopeOptions` for tuning backoff and shutdown deadlines.
- `Mochi.Runtime.Context`
  - `AmbientContext` backed by `AsyncLocal<T>`. Mochi `with ctx { ... }` lowers to setting / restoring an AsyncLocal.
  - Carries cancellation, the current `MochiScope`, telemetry tags, and structured logger.
- `Mochi.Runtime.Scheduling`
  - `DeterministicScheduler` (section 12).
  - `LongRunningPool` for `[Background]` agents.
- `Mochi.Runtime.Time`
  - `IClock`, `SystemClock`, `MockClock`.
  - `Mochi.Runtime.Time.Timers` (cancellable delays bound to `IClock`).
- `Mochi.Runtime.Errors`
  - `CancelledError`, `SupervisionError`, `AgentStoppedError`.

Total size goal: < 50 KLOC, no external dependencies beyond what's in the .NET BCL. Optional integration packages (`Mochi.Runtime.OpenTelemetry`, `Mochi.Runtime.Microsoft.Extensions.Logging`) live in separate assemblies.

## 15. Explicit rejections

We considered and rejected each of the following.

- **Akka.NET**. Full actor framework, port of JVM Akka, includes location-transparent remoting, cluster sharding, persistence. Excellent project. Too heavy for Mochi: brings a large dependency surface, opinionated supervision model, and a programming style that doesn't match Mochi's "agents are async functions with a mailbox" semantics. The Etteplan and akka-meta comparisons make the architectural difference clear: Akka actor refs are bound to a host, errors flow to supervisors as exceptions, mailboxes are intrinsic to the actor. We want a thinner model.
- **Proto.Actor**. Lightweight actor library, designed by one of the Akka.NET founders, available in C# and Go. Closer to what we want than Akka.NET but still imposes the actor-framework shape. We get equivalent functionality from `Channel<T>` + 200 lines of `Supervisor`.
- **Microsoft Orleans**. Virtual actor framework, cluster-aware, designed for distributed gaming and services workloads. Excellent for its niche. Out of scope for an in-process language runtime. Mochi may later offer a `Mochi.Runtime.Orleans` interop package, but it is not the default.
- **Reactive Extensions (Rx.NET)**. The classic push-based observable stream library. `IObservable<T>` is a strong alternative to `IAsyncEnumerable<T>` for hot streams. We chose channels instead because they integrate naturally with `await foreach` (zero coloring friction) and have first-party BCL status. Rx remains a fine choice for users who want it, but it's not the default lowering target.
- **TPL Dataflow (System.Threading.Tasks.Dataflow)**. Older (.NET 4.5) async pipeline framework. Block-based: `ActionBlock<T>`, `BufferBlock<T>`, `TransformBlock<TIn, TOut>`. Battle-tested but verbose, with its own scheduling model that overlaps awkwardly with Channels. Channels are the modern successor for in-process producer / consumer; Dataflow stays where it is.

For each rejection: a user can still take a dependency on the rejected library from Mochi via FFI if they want. We just don't ship it by default and we don't lower agent / stream constructs to it.

## 16. Lowering rules summary

Pulled together as a cheat sheet.

| Mochi construct | C# lowering |
|---|---|
| `agent A { ... }` | `sealed class A : IAgent` with `Channel<object>` mailbox and `RunAsync` loop |
| `on M(x) { body }` | `case M m: await BodyM(m, ct); break;` arm in the receive switch |
| `send a, M(x)` | `await a.SendAsync(new M(x), ct)` |
| `let h = spawn f(x)` | `var h = Task.Run(() => F(x, ct), ct)` |
| `await h` | `await h.ConfigureAwait(false)` |
| `concurrent { ... }` | `await using var scope = new MochiScope(ct); ...` |
| `stream cold T = ...` | `async IAsyncEnumerable<T>` method with `yield return` |
| `stream hot T = ...` | `ChannelReader<T>` exposed; producer holds writer |
| `subscribe s` | `await foreach (var v in s.WithCancellation(ct))` |
| `replay 16 stream T` | `ReplayChannel<T>(16)` from `Mochi.Runtime.Streams` |
| `cancel scope` | `scope.Cancel()` via `CancellationTokenSource` |
| `chan<T>` | `Channel<T>` (bounded 1 by default for reply channels) |

## 17. Open questions

- Should we expose `[Inline]` on Mochi handlers so the lowering can elide the `await` for trivially-sync arms? Microbenchmarks suggest 50-100 ns saved per message.
- `ValueTask` everywhere vs `Task` for public surface. We currently use `ValueTask` for internal calls and `Task` for the public boundary. Need to validate against the BCL guidance (Stephen Toub's posts argue for `Task` in public APIs unless the sync-completion case is dominant).
- The runtime-async experiment (dotnet/runtime#94620) may eventually change the cost model for `async`. If state machines move into the runtime, Mochi's coloring decision becomes less important. Track the proposal.
- Determinism mode plus blocking BCL calls (e.g. `File.ReadAllText`). Should we fail loudly? Currently we let them through with a doc warning.

## 18. Cross-references

- `[[01-runtime-overview]]` (Mochi.Runtime layout)
- `[[02-toolchain]]` (how the .NET build emits this code)
- `[[05-types-and-erasure]]` (where coloring inference lives)
- `[[07-stdlib]]` (the std library that uses these primitives)
- `[[10-interop]]` (how Mochi code is consumed from external C# / F#)
- `[[../0047/09-agent-streams]]` (sibling JVM/Loom design; primary contrast)

## Sources

1. [An Introduction to System.Threading.Channels, Stephen Toub, .NET Blog](https://devblogs.microsoft.com/dotnet/an-introduction-to-system-threading-channels/)
2. [Channels, Microsoft Learn .NET docs](https://learn.microsoft.com/en-us/dotnet/core/extensions/channels)
3. [.NET Core 3.0 API diff, System.Threading.Channels (dotnet/core GitHub)](https://github.com/dotnet/core/blob/main/release-notes/3.0/api-diff/.Net/3.0.0_System.Threading.Channels.md)
4. [Channel.CreateUnbounded Method, Microsoft Learn API ref](https://learn.microsoft.com/en-us/dotnet/api/system.threading.channels.channel.createunbounded?view=netcore-3.1)
5. [System.Threading.Channels reference source, dotnet/runtime GitHub](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Threading.Channels/src/System/Threading/Channels/Channel.cs)
6. [The performance characteristics of async methods in C#, Microsoft Developer Support blog](https://devblogs.microsoft.com/premier-developer/the-performance-characteristics-of-async-methods/)
7. [await ValueTask performance, dotnet/runtime issue #11803](https://github.com/dotnet/runtime/issues/11803)
8. [.NET 9 Runtime Async Experiment, dotnet/runtime issue #94620](https://github.com/dotnet/runtime/issues/94620)
9. [Measuring Performance Improvements in .NET Core with BenchmarkDotNet, Andrey Akinshin](https://aakinshin.net/posts/stephen-toub-benchmarks-part1/)
10. [Performance Improvements in .NET 10, Stephen Toub, .NET Blog](https://devblogs.microsoft.com/dotnet/performance-improvements-in-net-10/)
11. [Comparing IAsyncEnumerable and IObservable for event streams, DEV Community](https://dev.to/asik/comparing-iasyncenumerable-and-iobservable-for-event-streams-5g96)
12. [Using Channel Like IAsyncEnumerable, rendle.dev](https://rendle.dev/posts/using-channel-like-iasyncenumerable/)
13. [Building pipelines with IAsyncEnumerable in .NET, Niki Forovall blog](https://nikiforovall.blog/dotnet/2024/08/22/async-enumerable-pipelines.html)
14. [C# Streaming and Pipelines, Steven Stuart Murphy](https://stevenstuartm.com/study-guides/dotnet/c-sharp/async/streaming-and-pipelines.html)
15. [Asynchronous Streams in C#, DEV Community Ciklum](https://dev.to/ciklum_czsk/asynchronous-streams-in-c-3479)
16. [How to use structured concurrency in C#, InfoWorld April 2026](https://www.infoworld.com/article/2335155/how-to-use-structured-concurrency-in-c-sharp.html)
17. [Structured Concurrency in C#, Steven Giesel blog](https://steven-giesel.com/blogPost/59e57336-7c73-472f-a781-b0b79f0d47ad)
18. [Icicle: Structured Concurrency for C# and dotnet, bmazzarol GitHub](https://github.com/bmazzarol/Icicle)
19. [Simplify the concurrency limit with task, dotnet/runtime issue #53709](https://github.com/dotnet/runtime/issues/53709)
20. [Comparing .NET virtual actor frameworks, Etteplan](https://www.etteplan.com/about-us/insights/comparing-net-virtual-actor-frameworks/)
21. [Benchmark .NET virtual actor frameworks, Etteplan](https://www.etteplan.com/about-us/insights/benchmark-net-virtual-actor-frameworks/)
22. [Akka comparison with Orleans, akka/akka-meta GitHub](https://github.com/akka/akka-meta/blob/master/ComparisonWithOrleans.md)
23. [.Net parallel Kafka consumer with Akka.NET, Coding Militia](https://blog.codingmilitia.com/2024/01/08/a-dotnet-parallel-kafka-consumer-proof-of-concept-feat-akkadotnet-and-the-actor-model/)
24. [Java Virtual Threads Benchmark and Performance Analysis, Kloia](https://www.kloia.com/blog/benchmarking-java-virtual-threads-a-comprehensive-analysis)
25. [A simple benchmark for JDK Project Loom's virtual threads, Alexander Zakusylo, Medium](https://medium.com/@zakgof/a-simple-benchmark-for-jdk-project-looms-virtual-threads-4f43ef8aeb1)
26. [Java Virtual Threads, Revisited 2024 April, Borislav Stoilov, Medium](https://medium.com/codex/java-virtual-threads-9fad6c362890)
