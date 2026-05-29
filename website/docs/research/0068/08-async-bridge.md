---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "The Task<T> synchronous dispatch via .GetAwaiter().GetResult(), CLR thread pool semantics at the [UnmanagedCallersOnly] boundary, deadlock prevention with ConfigureAwait(false), the task-parallel async-mode opt-in for high-throughput cases, and cancellation semantics."
---

# 08. Async bridge

This note documents how the bridge surfaces .NET `async Task<T>` methods into Mochi's synchronous call surface. Mochi v1 does not have a native async surface; a `Task<T>` return type translates to a synchronous Mochi `extern fn` whose C# shim blocks on the CLR thread pool.

## The synchronous dispatch pattern

The C# shim for each `async Task<T>` method uses `.GetAwaiter().GetResult()`:

```csharp
using System.Runtime.InteropServices;
using System.Threading.Tasks;

public static class NewtonsoftJsonShim
{
    [UnmanagedCallersOnly(EntryPoint = "mochi_Serilog_Log_WriteAsync")]
    public static unsafe int WriteAsync(byte* message_ptr, int message_len)
    {
        var message = Marshal.PtrToStringUTF8((IntPtr)message_ptr, message_len);
        Serilog.Log.WriteAsync(message)
            .ConfigureAwait(false)
            .GetAwaiter()
            .GetResult();
        return 0; // success
    }
}
```

The `.ConfigureAwait(false)` is required to prevent the `Task` from capturing a `SynchronizationContext`. Without it, if the calling thread happens to have a synchronisation context (e.g., an ASP.NET Core context), the continuation would try to resume on that context's scheduler, potentially deadlocking if the context's scheduler is blocked waiting for the call to return.

In the CLR hosting context, the calling thread is the Mochi main thread, which has no synchronisation context by default. The `.ConfigureAwait(false)` is defensive: it ensures the pattern is safe even if the thread later gains a sync context.

## CLR thread pool semantics

The `.GetAwaiter().GetResult()` pattern blocks the calling thread while the CLR thread pool runs the async continuation. The thread pool is the same pool used by all CLR-hosted code in the process, including any other `.GetAwaiter().GetResult()` calls from other shim entry points.

A typical `Task<string>` resolution sequence:

1. Calling thread (Mochi main thread) invokes the `[UnmanagedCallersOnly]` entry.
2. The entry calls the managed async method, which schedules the continuation on the CLR thread pool.
3. The calling thread blocks at `.GetAwaiter().GetResult()`.
4. A CLR thread pool thread executes the async continuation.
5. The continuation completes; the calling thread unblocks.
6. The `[UnmanagedCallersOnly]` entry marshals the result and returns to Mochi.

The per-call overhead from the CLR thread pool dispatch is approximately 10-50 microseconds for an IO-bound call that completes quickly. For calls whose async bodies do substantial work (network I/O, database queries), the thread pool overhead is negligible against the body cost.

## Deadlock risk analysis

A deadlock can occur with `.GetAwaiter().GetResult()` in two scenarios:

**Scenario 1**: the calling thread has a `SynchronizationContext` that posts continuations back to a specific thread (e.g., the .NET Framework ASP.NET legacy context), and the calling thread is blocking waiting for the Task.

Mitigation: `.ConfigureAwait(false)` prevents the continuation from targeting the current context's scheduler. The continuation runs on the CLR thread pool instead.

**Scenario 2**: the async method itself calls `.GetAwaiter().GetResult()` on another Task internally, and the inner Task is also waiting for thread pool threads while the outer `.GetAwaiter().GetResult()` holds the calling thread.

Mitigation: This scenario ("blocking on async" anti-pattern inside the package) is a bug in the NuGet package. The bridge cannot prevent this; it is the same issue that affects any .NET consumer of a buggy async library. The `SkipReport` documentation recommends the user check the package's async patterns.

**Scenario 3**: two concurrent Mochi threads (if Mochi supports concurrent calls) both block in `.GetAwaiter().GetResult()`, and both Tasks depend on each other.

Mitigation: Mochi v1 is single-threaded; only one `.GetAwaiter().GetResult()` call can be active at a time.

## `Task<T>` return type variants

The shim handles several variants of async return types:

| CLR return type | Shim pattern | Mochi type |
|-----------------|--------------|------------|
| `Task<string>` | `.GetAwaiter().GetResult()` returns `string` | `string` |
| `Task<int>` | Same, returns `int64` | `int` |
| `Task<List<T>>` | Same, marshals list | `list<T>` |
| `Task` (no result) | `.GetAwaiter().GetResult()` returns void | unit |
| `ValueTask<T>` | `.AsTask().GetAwaiter().GetResult()` | same as Task<T> |
| `ValueTask` | `.AsTask().GetAwaiter().GetResult()` | unit |

`ValueTask<T>` is converted to `Task<T>` via `.AsTask()` before the blocking wait. This is slightly less efficient than awaiting a `ValueTask` directly (ValueTask is optimised for the already-completed case), but it is simpler to implement uniformly.

## The `async-mode = "task-parallel"` opt-in

For high-throughput cases where the synchronous dispatch pattern is too slow, the user can opt into a fully async Mochi bridge via:

```toml
[dotnet.runtime]
async-mode = "task-parallel"
```

In this mode, `Task<T>` methods are exposed as Mochi-level async functions. The bridge generates a different shim that returns a `mochi_task_handle_t` (an opaque integer task ID) instead of blocking:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_HttpClient_GetStringAsync_start")]
public static unsafe long GetStringAsync_start(nint client_handle, byte* uri_ptr, int uri_len)
{
    var client = (HttpClient)GCHandle.FromIntPtr((IntPtr)client_handle).Target!;
    var uri = Marshal.PtrToStringUTF8((IntPtr)uri_ptr, uri_len);
    var task = client.GetStringAsync(uri);
    return TaskRegistry.Register(task);
}

[UnmanagedCallersOnly(EntryPoint = "mochi_HttpClient_GetStringAsync_poll")]
public static unsafe int GetStringAsync_poll(long task_id, byte** result_ptr, int* result_len)
{
    return TaskRegistry.Poll(task_id, result_ptr, result_len);
}
```

The Mochi async colour system (post-v1) can use the `_start` / `_poll` pair to integrate with Mochi's own scheduler. This mode is a post-v1 feature; phase 11 delivers only the synchronous bridge.

## Cancellation semantics

Mochi v1 has no native cancellation primitive. The shim does not expose `CancellationToken` parameters; methods that require a `CancellationToken` are either:

- Refused with `SkipCancellationToken` if the `CancellationToken` is a required (non-optional) parameter.
- Silently passed `CancellationToken.None` if the parameter has a default value of `default(CancellationToken)`.

The user can hand-author a timeout wrapper:

```toml
[[dotnet.extern]]
item = "System.Net.Http.HttpClient.GetStringAsync"
signature = """
extern fn http_get_with_timeout(client: HttpClient, uri: string, timeout_ms: int): string from dotnet "HttpClientShim.GetStringWithTimeout"
"""
```

The custom `HttpClientShim.GetStringWithTimeout` wraps `GetStringAsync` with a `Task.WhenAny` + `Task.Delay(timeout_ms)` pattern to implement a timeout.

## ValueTask and IAsyncEnumerable

`IAsyncEnumerable<T>` (C# 8 async streams) is not supported in v1. A method returning `IAsyncEnumerable<T>` receives `SkipAsyncEnumerable`. The pattern requires a more complex shim (repeated polling via `MoveNextAsync()`) that is deferred to a post-v1 sub-phase.

`ValueTask` without type parameter (bare `ValueTask`) is handled the same as `Task` (no result): `.AsTask().GetAwaiter().GetResult()`.

## Interaction with the CLR GC

While a `.GetAwaiter().GetResult()` call is blocking the calling thread, the CLR GC can run on other threads. The `[UnmanagedCallersOnly]` method pins any parameters passed from native code? No: `[UnmanagedCallersOnly]` does not pin parameters. The shim must copy string parameters into managed heap before the blocking wait:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_SomeApi_FetchAsync")]
public static unsafe IntPtr FetchAsync(byte* url_ptr, int url_len)
{
    // Copy native string into managed memory before blocking.
    // Do NOT pass url_ptr across the GetAwaiter().GetResult() boundary.
    var url = Marshal.PtrToStringUTF8((IntPtr)url_ptr, url_len);
    var result = SomeApi.FetchAsync(url)
        .ConfigureAwait(false)
        .GetAwaiter()
        .GetResult();
    return MochiMarshal.StringToCoTaskMem(result);
}
```

The `Marshal.PtrToStringUTF8` call creates a managed `string` object from the native UTF-8 pointer. This managed object is GC-tracked; the original native pointer (`url_ptr`) must not be dereferenced after the blocking wait because the Mochi GC may have moved or freed the underlying memory.

The shim generator enforces this by always copying all native pointer parameters into managed objects before the first `await` or `.GetAwaiter().GetResult()` call.

## Cross-references

- [[02-design-philosophy]] §4 for why CLR hosting is the default over NativeAOT.
- [[09-abi-stability]] §3 for the GCHandle and opaque handle model.
- [[05-type-mapping]] for the Task<T> type mapping entry.
- [MEP-68 §7](/docs/mep/mep-0068#7-async-bridge-runtime-hook) for the normative async bridge spec.
