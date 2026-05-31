---
title: "10. GC and memory management"
sidebar_position: 11
sidebar_label: "10. GC and memory management"
description: "The .NET tracing GC and object pinning, GCHandle.Alloc(Normal) vs Pinned, the GCHandleTable singleton, the Mochi-side defer _free discipline, ManualResetEventSlim lifetime across the GC boundary, and NativeAOT memory model assumptions."
---

# 10. GC and memory management

The fundamental tension in bridging a tracing garbage-collected runtime (.NET) with a native runtime (Mochi/Rust) is that the GC may move or collect objects at any time unless the native side takes explicit steps to prevent it. This note documents the steps the bridge takes.

## .NET's tracing GC and object mobility

The .NET GC is a generational, compacting, tracing garbage collector. In compacting mode (which is the default for the Server GC flavor and the background GC), the GC moves live objects to compact the heap, updating all managed references to the new addresses. A raw pointer to a managed object obtained before a GC collection is invalid after it.

In NativeAOT, the GC is still present (NativeAOT does not eliminate the GC; it only eliminates the JIT). NativeAOT uses the same CoreCLR GC engine, compiled ahead-of-time. The GC runs on the same OS threads as the application, triggered by allocation pressure.

**Consequence for the bridge**: the bridge must never store a raw pointer to a managed .NET object in a Mochi variable. All references to managed objects must go through `GCHandle`.

## GCHandle.Alloc(Normal) vs GCHandle.Alloc(Pinned)

.NET provides two GC handle types relevant to the bridge:

| GCHandle type | GC can move object? | Raw pointer valid? | Cost |
|---------------|--------------------|--------------------|------|
| `Normal` | Yes (GC moves it; handle remains valid) | No (use `GCHandle.Target` to get current address) | ~50 ns alloc, ~20 ns free |
| `Pinned` | No (GC cannot move pinned objects) | Yes (`GCHandle.AddrOfPinnedObject()` is stable) | ~200 ns alloc (GC must track pinned objects in a separate pinned heap segment) |

The bridge uses `Normal` handles for all reference types. `Pinned` handles are not used because:

1. Pinned objects cause GC fragmentation (the pinned heap segment cannot be compacted around them).
2. Pinned objects are only necessary when the native side needs a stable raw pointer to the object's interior (e.g., writing directly into a `byte[]`'s backing array). The bridge only accesses managed objects through `GCHandle.Target`, not through raw interior pointers.
3. Overusing `Pinned` handles on long-lived objects can prevent the GC from compacting the LOH (Large Object Heap) and Gen2, causing memory pressure.

The only exception: the `Marshal.AllocHGlobal` string buffers are native-heap allocations (not managed objects at all), so they are not subject to GC movement and do not need GC handles.

## The GCHandleTable singleton

`mochi-dotnet-runtime`'s `GCHandleTable` is a process-wide singleton that tracks all `GCHandle.Alloc(Normal)` calls made by the bridge:

```csharp
public static class GCHandleTable {
    // Concurrent dictionary from handle integer to GCHandle
    // Key = GCHandle.ToIntPtr(handle) (stable integer; not the object address)
    private static readonly ConcurrentDictionary<nint, GCHandle> _handles = new();
    private static long _allocCount = 0;
    private static long _freeCount = 0;

    public static nint Alloc(object obj) {
        Interlocked.Increment(ref _allocCount);
        var handle = GCHandle.Alloc(obj, GCHandleType.Normal);
        var key = GCHandle.ToIntPtr(handle);
        _handles[key] = handle;
        return key;
    }

    public static T? Get<T>(nint key) where T : class {
        if (!_handles.TryGetValue(key, out var handle)) return null;
        return handle.Target as T;
    }

    public static void Free(nint key) {
        if (_handles.TryRemove(key, out var handle)) {
            Interlocked.Increment(ref _freeCount);
            handle.Free();
        }
    }

    // Diagnostic: called by mochi_dotnet_gc_report() in debug builds
    public static (long alloc, long free, long live) GetStats() =>
        (_allocCount, _freeCount, _handles.Count);
}
```

The `ConcurrentDictionary` is thread-safe and lock-free for reads (via the CLR's internal CAS-based dictionary implementation). Writes (Alloc/Free) use the internal ConcurrentDictionary locking. Under high concurrent Mochi code that creates many .NET objects, the dictionary can become a contention point; a future optimisation is to shard the dictionary by `key % N` for a configurable `N`.

`GCHandle.ToIntPtr(handle)` returns a stable integer (the index into the GC's handle table, not the object's memory address). This integer is what the Mochi side stores in the `long` backing the `extern type`. The integer does not change when the GC moves the object.

## Mochi-side `defer _free(handle)` discipline

Every `extern type T` backed by a GCHandle has a synthesised `_free` symbol. The Mochi compiler's scope-exit analysis inserts a `defer` call at the earliest safe point:

```mochi
fn get_connection(conn_str: string): NpgsqlConnection {
    let conn = Npgsql.NpgsqlConnection(conn_str)  // allocs GCHandle
    conn.Open()
    return conn
    // defer conn.free() would be WRONG here: we're returning conn
}

fn query_users(conn_str: string): list<User> {
    let conn = get_connection(conn_str)
    defer Npgsql.NpgsqlConnection_free(conn)       // auto-inserted by compiler
    return Dapper.Query(conn, "SELECT * FROM users")
    // conn.free() called here (after return value is captured)
}
```

The compiler tracks `extern type` variables that were assigned from a `extern fn` return (not passed in from a caller) and inserts `defer _free(handle)` at the end of the containing scope. If a variable is returned from the function, the `defer` is suppressed (the caller takes ownership).

This is the same strategy MEP-73 uses for `Box<T>` raw-pointer handles (`box_free` called via `defer`).

**Known limitation**: the compiler cannot insert `defer` for `extern type` values stored in collection types (`list<NpgsqlConnection>`) or in records. The user must call `_free` manually for these cases. A future MEP will add linear type tracking for `extern type` to catch these leaks at compile time.

## ManualResetEventSlim lifetime

`ManualResetEventSlim` (MRES) is a managed object but it is referenced from the async bridge wrapper:

```csharp
var mre = new ManualResetEventSlim(false);
```

The `mre` is stack-allocated (local variable in the `[UnmanagedCallersOnly]` wrapper). In NativeAOT, local variables in `[UnmanagedCallersOnly]` methods are on the native call stack. The GC sees the `mre` reference on the stack (GC roots include native stack frames in NativeAOT) and will not collect it while the method is executing.

After `mre.Set()` and `mre.Wait()` complete (the async work is done), `mre` goes out of scope and becomes eligible for collection in the next GC cycle. There is no manual free required.

**Potential issue**: if the bridge creates many concurrent async calls (e.g., a Mochi program with heavy parallelism calling async .NET methods), each call allocates a `ManualResetEventSlim` on the managed heap. Under very high concurrency, this becomes per-call GC pressure. Mitigation: pool `ManualResetEventSlim` objects via a `ConcurrentBag<ManualResetEventSlim>` in `mochi-dotnet-runtime`. The pooling is not in v1 but is tracked in the risk register.

## NativeAOT GC assumptions

NativeAOT's GC makes several assumptions relevant to the bridge:

1. **All managed object references are either in GC-tracked locations (registers, stack, static fields, GCHandle table) or explicitly protected by `GCHandle`.** The bridge satisfies this by never storing raw object pointers.

2. **The GC can run on any thread that allocates managed memory.** The `ThreadPool.QueueUserWorkItem` dispatch in the async bridge allocates on a pool thread. The GC can trigger mid-wrapper-execution on that thread. The `ManualResetEventSlim` on the calling thread's stack is a GC root and is protected.

3. **`GCHandle.Alloc(Normal)` prevents collection but not movement.** As documented above; this is why raw pointers to managed objects are never exposed across the boundary.

4. **NativeAOT uses a conservative stack scanner by default.** This means any native integer on the native call stack that happens to look like a managed object pointer is treated as a GC root (conservatively). This has no correctness impact on the bridge (it only means some objects live longer than necessary) but can increase memory usage under long-lived native stacks.

## Memory model for the Mochi-side extern type

From the Mochi compiler's perspective, an `extern type T` is:
- A 64-bit opaque integer (`long` at the IR level).
- Semantically, it is a "handle" that the Mochi program owns.
- The associated `_free` symbol invalidates the handle and releases the underlying resource.
- After `_free`, the handle integer is undefined and must not be used.
- Two handles for the same underlying object have the same integer value if they were obtained from the same `GCHandle.Alloc` call; they have different values if they were obtained from separate `Alloc` calls (even for the same object).

This is the same ownership model MEP-73 uses for `Box<T>` raw-pointer handles. The Mochi compiler enforces single-ownership for `extern type` values (the handle is moved, not copied, across assignments) and inserts `defer _free` for locally-owned handles.
