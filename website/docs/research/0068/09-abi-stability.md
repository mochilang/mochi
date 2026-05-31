---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "[UnmanagedCallersOnly] guarantees and limitations, blittable struct pass-by-value, GCHandle opaque handle strategy, Marshal.AllocHGlobal string buffers, nint handle type on 32/64-bit platforms, and cross-RID ABI compatibility."
---

# 09. ABI stability

This note documents the ABI contract between the NativeAOT wrapper static library and the Mochi/Rust binary that links it. The contract must be stable across .NET SDK updates, Mochi version bumps, and the four supported RIDs.

## [UnmanagedCallersOnly] guarantees

`[UnmanagedCallersOnly]` (introduced in .NET 5, stable since .NET 7) makes a `static` method callable from native code via a stable C ABI entry point. Guarantees:

1. **No GC references in the signature.** The compiler enforces that every parameter and return type is blittable (primitive, pointer, or `[StructLayout(Sequential/Explicit)]` struct with only blittable fields). Reference types (`class`, `string`, `T[]`) cannot appear directly; the bridge uses `nint` (pointer-sized integer) for GC handles.

2. **No exception propagation.** Exceptions thrown across an `[UnmanagedCallersOnly]` boundary cause undefined behaviour in the caller. The bridge catches all exceptions inside the wrapper and converts them to a mochi error sentinel (a non-zero `int` return code + a `mochi_dotnet_last_error()` function the Mochi side calls for the message).

3. **Calling convention is platform-default C ABI.** On x86-64 Linux/macOS: System V AMD64 ABI (integer args in rdi/rsi/rdx/rcx/r8/r9, float args in xmm0-7, return in rax). On ARM64: AAPCS64. On x86-64 Windows: Microsoft x64 ABI. The `CallConvs` property of `[UnmanagedCallersOnly]` can override to `CallConvCdecl`, `CallConvStdcall`, or `CallConvFastcall`; the bridge uses the default (platform default) for all four primary RIDs.

4. **No inline caching, no JIT deoptimisation.** NativeAOT compiles the wrapper method to machine code once. There is no JIT, no tiered compilation, no OSR (On-Stack Replacement). The entry point is stable after `dotnet publish`.

5. **Thread safety.** `[UnmanagedCallersOnly]` methods may be called from any thread simultaneously. The bridge generates wrappers that are reentrant (no shared mutable state outside of explicitly thread-safe structures like `GCHandleTable` with its internal lock and `Interlocked` operations).

## Blittable struct pass-by-value

For `[StructLayout(LayoutKind.Sequential)]` structs with all blittable fields (the "value type path"), the bridge passes the struct by value at the ABI. This is possible because `[UnmanagedCallersOnly]` parameters may be value types with blittable fields:

```csharp
[StructLayout(LayoutKind.Sequential)]
public struct Point { public float X; public float Y; }

[UnmanagedCallersOnly(EntryPoint = "mochi_dotnet_MyLib_Geometry_Scale")]
public static Point Geometry_Scale(Point p, float factor) => new Point(p.X * factor, p.Y * factor);
```

On x86-64 SysV ABI, a two-float struct is passed in `xmm0` (two packed 32-bit floats) and returned in `xmm0`. On ARM64 AAPCS64, the same applies (HFA: Homogeneous Floating-point Aggregate). The Mochi-side `extern record Point { X: float, Y: float }` receives and returns the struct by value matching the ABI layout.

For structs larger than 16 bytes (two registers), the ABI uses an implicit pointer argument (the caller allocates stack space, passes a pointer, the callee writes the result into it). This is transparent to the bridge user.

Non-blittable structs (structs with reference-type fields) cannot be passed by value; they use the GCHandle strategy.

## GCHandle opaque handle strategy

Reference types (class instances, arrays, strings) cannot be passed directly across the `[UnmanagedCallersOnly]` boundary because:
- The GC may move them in memory between the wrap call and the native caller's use.
- Their internal layout is not stable (field ordering is up to the GC's optimisation).

The bridge uses `GCHandle.Alloc(obj, GCHandleType.Normal)`:

```csharp
// Create a GCHandle (prevents GC from collecting the object)
var handle = GCHandle.Alloc(myObject);
nint handleValue = GCHandle.ToIntPtr(handle);
// Return handleValue as nint to the Mochi caller

// Later, when the Mochi caller frees:
var handle = GCHandle.FromIntPtr(handleValue);
var obj = (MyType)handle.Target!;
handle.Free();
```

`GCHandle.Alloc(Normal)` pins the object in the GC's tracking table but does not pin its address in memory (the GC can still move it; the handle remains valid). The `nint` value returned is a 4-byte integer on 32-bit platforms and an 8-byte integer on 64-bit platforms, representing the GCHandle table index (not a raw pointer).

The `GCHandleTable` in `mochi-dotnet-runtime` provides a concurrent, GC-pressure-aware wrapper that batches handle allocations and tracks handle lifetimes for leak detection in debug builds:

```csharp
public static class GCHandleTable {
    private static readonly ConcurrentDictionary<nint, GCHandle> _handles = new();

    public static nint Alloc(object obj) {
        var handle = GCHandle.Alloc(obj);
        var key = GCHandle.ToIntPtr(handle);
        _handles[key] = handle;
        return key;
    }

    public static T Get<T>(nint key) => (T)_handles[key].Target!;

    public static void Free(nint key) {
        if (_handles.TryRemove(key, out var handle)) handle.Free();
    }
}
```

The `_handles` dictionary is itself a managed object (not subject to GC under NativeAOT's conservative pinning of static roots).

## Marshal.AllocHGlobal for strings

.NET strings are UTF-16 managed objects. They cannot be returned directly from `[UnmanagedCallersOnly]` methods. The bridge allocates an unmanaged UTF-8 buffer:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_dotnet_<Pkg>_<Method>_ReturnString")]
public static unsafe byte* <Method>_ReturnString(<args>) {
    string managed = <method_call>(<args>);
    if (managed == null) return null;
    byte[] utf8 = System.Text.Encoding.UTF8.GetBytes(managed);
    byte* ptr = (byte*)Marshal.AllocHGlobal(utf8.Length + 1);
    utf8.AsSpan().CopyTo(new Span<byte>(ptr, utf8.Length));
    ptr[utf8.Length] = 0; // null-terminate
    return ptr;
}

// Companion free function:
[UnmanagedCallersOnly(EntryPoint = "mochi_dotnet_string_free")]
public static unsafe void StringFree(byte* ptr) {
    if (ptr != null) Marshal.FreeHGlobal((nint)ptr);
}
```

`Marshal.AllocHGlobal` allocates from the native heap (not the GC heap), producing a stable pointer the Mochi side can hold. `Marshal.FreeHGlobal` frees it. The Mochi type checker inserts a `defer mochi_dotnet_string_free(s)` at the end of every scope that receives a string return value from a .NET method.

## nint handle type on 32/64-bit platforms

`nint` (alias for `System.IntPtr`) is 4 bytes on 32-bit platforms and 8 bytes on 64-bit platforms. All four MEP-68 primary targets (linux-x64, linux-arm64, osx-arm64, win-x64) are 64-bit, so `nint` is uniformly 8 bytes. The Mochi `extern type T` for GC-handle-backed types is emitted as a `long` (64-bit integer) on all four platforms.

If a future MEP adds 32-bit targets (linux-arm, win-x86), the bridge must emit `int` (32-bit) for `nint` on those targets. The `wrapper-sha256` in `mochi.lock` would differ between 32-bit and 64-bit wrapper builds because the symbol signatures differ; the lockfile records the per-RID wrapper hash.

## Static link vs. shared lib vs. CoreCLR hosting ABI comparison

| Strategy | ABI contract | Startup cost | Runtime dep on target |
|----------|-------------|--------------|----------------------|
| NativeAOT static lib (`NativeLib=Static`) | C ABI, `[UnmanagedCallersOnly]`, stable across SDK updates within the same major .NET version | 0 ms | None |
| NativeAOT shared lib (`NativeLib=Shared`) | Same C ABI, but dlopen at runtime | ~1 ms (dlopen) | None (bundled in the Mochi binary distribution) |
| CoreCLR hosting (`coreclr_initialize`) | `coreclr_create_delegate`-acquired function pointers | 100-300 ms | libcoreclr.so on target machine |

MEP-68 uses static lib for the primary path. The shared lib option is noted for future use (e.g., if the Mochi binary size budget cannot accommodate all NativeAOT wrappers linked in).

## Cross-RID ABI compatibility

The same `.mochi` shim file is used for all four RIDs. The `extern fn` declarations in the shim use the same symbol names (`mochi_dotnet_<pkg>_<Type>_<Method>`). The ABI type mapping (int=4 bytes, long=8 bytes, float=4 bytes, double=8 bytes, nint=8 bytes on 64-bit) is stable across all four primary targets.

The only ABI divergence is struct alignment: on x86-64 SysV, structs have natural alignment; on ARM64 AAPCS64, the same. On Windows x64 (MSVC ABI), structs with the same fields may have different padding if `__declspec(align)` or `[StructLayout(Pack=...)]` differ. The bridge always generates `[StructLayout(LayoutKind.Sequential)]` without `Pack` (which uses the platform default alignment), producing consistent layout on all four targets for structs with fields of the same types.
