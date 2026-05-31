---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "The .NET Task-based async model vs. Mochi's colouring-only async, the SynchronizationContext deadlock hazard, the ThreadPool + ManualResetEventSlim pattern, IAsyncEnumerable deferred path, CancellationToken bridge, and cost comparison with MEP-73's tokio::block_on."
---

# 08. Async bridge

## The impedance mismatch

Mochi's `async` colouring (MEP-48) is a value-level annotation: `async fn f(): T` means `f` returns a computation that will eventually produce `T`. In the MEP-53 Rust target, Mochi `async fn` lowers to a synchronous call on the current thread (there is no Mochi async scheduler; the `async` annotation is carried for type-checking but stripped at codegen time in single-threaded mode).

.NET's `async Task<T>` is a full coroutine system: `await` suspends the current method, returns control to the caller, and resumes when the awaited `Task` completes. The completion may run on any ThreadPool thread (or, if a `SynchronizationContext` is installed, on the context-specific thread).

The bridge must bridge these two models: Mochi makes a synchronous call (from the native Mochi/Rust thread), and the .NET code is async. The wrapper must call the async .NET method and wait for it to complete before returning to the Mochi caller.

## The SynchronizationContext deadlock

The classic pitfall of blocking on a `Task` from a synchronous context:

```csharp
// WRONG: will deadlock in ASP.NET classic / WinForms / WPF
var result = someAsyncMethod().Result;
// WRONG: same deadlock
var result = someAsyncMethod().GetAwaiter().GetResult();
```

The deadlock occurs because:
1. The calling thread has a `SynchronizationContext` (e.g., the ASP.NET request context, or the UI dispatcher thread).
2. `someAsyncMethod()` internally awaits something with `ConfigureAwait(false)` NOT applied, so the continuation is scheduled back on the original `SynchronizationContext`.
3. The calling thread is blocked by `.Result`, so the continuation can never run, and the `.Result` wait never completes.

In MEP-68's NativeAOT context, the calling thread is a native OS thread managed by the Mochi/Rust runtime. NativeAOT does not install a `SynchronizationContext` on native threads. Therefore, calling `.GetAwaiter().GetResult()` directly on the NativeAOT calling thread is technically safe from the classic ASP.NET deadlock.

However, a second risk remains: if the async method's implementation internally dispatches continuation work to the CLR `ThreadPool`, and if the calling thread is the only available thread (ThreadPool exhaustion), the continuation is never scheduled and the `.GetAwaiter().GetResult()` wait never completes (a livelock). While ThreadPool exhaustion is unlikely in typical usage, the bridge adopts the conservative `ThreadPool.QueueUserWorkItem` dispatch to avoid this scenario.

## The ManualResetEventSlim + ThreadPool pattern

For each `async Task<T>` method, the wrapper generates:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_dotnet_<Pkg>_<Type>_<Method>")]
public static unsafe <C_T> <Pkg>_<Type>_<Method>(<C_args>)
{
    var mre = new ManualResetEventSlim(false);
    var result = default(<C_T>);
    Exception? error = null;

    ThreadPool.QueueUserWorkItem(_ =>
    {
        try
        {
            var managed = <unmarshal_args>(<C_args>);
            var task = <Type>.<Method>(managed).AsTask();  // AsTask() for ValueTask<T>
            result = <marshal_result>(task.GetAwaiter().GetResult());
        }
        catch (Exception ex)
        {
            error = ex;
        }
        finally
        {
            mre.Set();
        }
    });

    mre.Wait();

    if (error != null)
    {
        MochiRuntime.ThrowException(error.Message);
        return default;
    }
    return result;
}
```

Why `ManualResetEventSlim` rather than alternatives:

| Alternative | Why rejected |
|-------------|-------------|
| `Task.Wait()` on calling thread | Same ThreadPool starvation risk; calls through managed scheduler |
| `SemaphoreSlim.Wait()` | Managed object; interacts with CLR scheduler; would need `SemaphoreSlim.WaitAsync()` which returns a Task |
| `Monitor.Wait()` | Requires lock acquisition; allocates a lock object |
| `AutoResetEvent` / `ManualResetEvent` (non-slim) | OS kernel objects; 2-4x slower than `ManualResetEventSlim` on the uncontended path |
| `SpinWait` | CPU waste; no benefit over MRE for I/O-bound async |
| `Interlocked` flag + `Thread.SpinWait` | Same as SpinWait |

`ManualResetEventSlim.Wait()` uses an adaptive spin (a few hundred iterations) before falling back to a kernel wait. In NativeAOT, the spin is non-CLR-managed and does not interact with the CLR scheduler. The `Set()` call from the ThreadPool thread is atomic (no lock required after the `QueueUserWorkItem` dispatch).

## Cost analysis

Overhead per async crossing (approximate, Apple M3, NativeAOT release build):

| Component | Time |
|-----------|------|
| `ThreadPool.QueueUserWorkItem` dispatch | ~500 ns |
| `ManualResetEventSlim` spin (fast path, < 200 spins) | ~200 ns |
| `ManualResetEventSlim` kernel wait (slow path, I/O-bound) | ~5 µs (kernel thread wakeup) |
| Marshal args + unmarshal result | ~50–200 ns (depends on types) |
| **Total fast path (CPU-bound async)** | **~750 ns** |
| **Total slow path (I/O-bound async, 1ms I/O)** | **~1ms + 5µs** |

For comparison, MEP-73's `tokio::block_on` overhead:

| Component | Time |
|-----------|------|
| `block_on` dispatch to current-thread runtime | ~300 ns |
| Task wake from I/O completion | ~3 µs |
| **MEP-73 fast path** | **~300 ns** |
| **MEP-73 slow path (1ms I/O)** | **~1ms + 3µs** |

MEP-68's async bridge is about 2x more expensive than MEP-73's on the fast path. The difference is the `ThreadPool.QueueUserWorkItem` dispatch (which posts to a separate thread) vs. `tokio::block_on` (which drives the future on the calling thread directly). The MEP-68 conservative dispatch is required because .NET's async runtime does not offer a "drive this future synchronously on the current thread" primitive equivalent to Tokio's `block_on`.

## CancellationToken bridge

Many .NET async methods accept a `CancellationToken` for cooperative cancellation. The bridge generates a wrapper that accepts an optional `CancellationToken`:

```mochi
extern fn HttpClient_GetStringAsync(client: HttpClient, url: string): async string
extern fn HttpClient_GetStringAsync_Cancellable(client: HttpClient, url: string, ct: CancellationToken): async string
```

The `CancellationToken` is an opaque GC handle (from `CancellationTokenSource.Token`). Mochi code that needs cancellation:

```mochi
let cts = CancellationTokenSource()
let token = cts.Token()
spawn_task(fn() {
    sleep(5s)
    cts.Cancel()
})
let body = await HttpClient_GetStringAsync_Cancellable(client, url, token)
```

`CancellationTokenSource` and `CancellationToken` are translated as opaque GC handle types.

## IAsyncEnumerable deferred path

`IAsyncEnumerable<T>` (introduced in C# 8 / .NET Core 3.0) is used by Entity Framework Core for streaming query results, SignalR for server-sent events, and `System.IO.Pipelines` for byte-stream processing. It cannot be wrapped with the `ManualResetEventSlim` pattern because it is a pull-based async sequence (each element requires a separate `await MoveNextAsync()` call).

Phase 11 will add an `IAsyncEnumerable<T>` bridge using a Go-style buffered channel at the C ABI boundary:

```c
// Proposed phase-11 ABI for IAsyncEnumerable<T>
// The wrapper starts a background thread that drives the async enumerator
// and sends elements into a channel.
typedef struct MochiAsyncSeq MochiAsyncSeq;
MochiAsyncSeq* mochi_dotnet_ef_Users_ToAsyncEnumerable(DbContext* ctx);
int mochi_dotnet_async_seq_next(MochiAsyncSeq* seq, void* out_elem); // 0=has value, 1=done, -1=error
void mochi_dotnet_async_seq_free(MochiAsyncSeq* seq);
```

The v1 bridge generates `SkipReport: IAsyncEnumerable<T> is not supported in v1; add to mochi.toml [dotnet.monomorphise] after phase 11 ships` for any method returning `IAsyncEnumerable<T>`.

## Comparison with MEP-73 async bridge

| Dimension | MEP-68 (.NET) | MEP-73 (Rust/tokio) |
|-----------|--------------|---------------------|
| Runtime | CLR ThreadPool (pre-existing) | tokio::runtime::Runtime (new per-process singleton) |
| Blocking primitive | ManualResetEventSlim | OnceLock<Runtime>.get_or_init().block_on(...) |
| Deadlock risk | Mitigated by ThreadPool dispatch (no sync context on pool thread) | No deadlock risk (tokio block_on has no SynchronizationContext concept) |
| Async stream support | Deferred (IAsyncEnumerable, phase 11) | Deferred (Stream, phase 11) |
| Startup cost | ~0 (ThreadPool is always running in NativeAOT) | ~100µs (tokio runtime init, once per process) |
| Per-call overhead fast path | ~750 ns | ~300 ns |
| CancellationToken analogue | CancellationToken (opaque GC handle) | (no direct Rust analogue in v1; Future cancellation is drop-based) |
