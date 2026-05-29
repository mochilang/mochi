---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "[UnmanagedCallersOnly] guarantees and constraints, the CLR hosting function-pointer load path, the MochiMarshal type conventions, string and list round-trip encoding, GCHandle-based opaque handles for reference types, drop semantics across the managed-native boundary, and NativeAOT vs CLR hosting ABI differences."
---

# 09. ABI stability

This note documents the ABI the C# shim assembly exposes. The boundary is `[UnmanagedCallersOnly]` entry points called via CLR hosting function pointers; it is what Mochi's runtime calls into.

## `[UnmanagedCallersOnly]` guarantees

The `[UnmanagedCallersOnly]` attribute (introduced in .NET 5, stable in .NET 6+) marks a static method as directly callable from unmanaged (native) code via a function pointer. The constraints:

1. The method must be `static`.
2. The method must not be generic (no open type parameters; monomorphised closed instantiations are permitted via explicit wrapper methods).
3. Parameters and return types must be "blittable": value types that have the same memory representation in managed and unmanaged code (`int`, `long`, `double`, `bool` as `int`, `nint`/`IntPtr`, pointers), or `void`.
4. The method must not throw exceptions that escape the `[UnmanagedCallersOnly]` boundary. The shim catches all managed exceptions and converts them to a `MochiErrorCode` return value.

The entry point name is declared via `EntryPoint`:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_Newtonsoft_Json_JsonConvert_SerializeObject")]
public static unsafe IntPtr SerializeObject(IntPtr value_handle)
```

The calling convention is platform default: `Cdecl` on Linux/macOS (SysV AMD64 ABI on x64, AAPCS64 on ARM64), `Stdcall` on Windows x64 (but effectively `Cdecl` on 64-bit Windows). The bridge specifies `CallConvs = new[] { typeof(CallConvCdecl) }` explicitly to ensure cross-platform consistency:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_pkg_method", CallConvs = new[] { typeof(CallConvCdecl) })]
```

## The CLR hosting function-pointer load path

The bridge loads entry points via the CLR hosting API's `load_assembly_and_get_function_pointer` delegate:

```go
// package3/dotnet/hosting/clr.go

type CLRHost struct {
    loadDelegate  unsafe.Pointer  // load_assembly_and_get_function_pointer
}

func (h *CLRHost) GetFunctionPointer(
    assemblyPath, typeName, methodName string,
) (unsafe.Pointer, error) {
    // Calls load_assembly_and_get_function_pointer via CGO
}
```

Each entry point is loaded once at bridge initialization and cached as a Go function pointer. The loading sequence at process startup:

1. `hostfxr_initialize_for_runtime_config(runtimeConfigPath, ...)`: initialise the CLR with the runtime configuration (`.runtimeconfig.json`) shipped alongside the shim assembly.
2. `hostfxr_get_runtime_delegate(hostContextHandle, hdt_load_assembly_and_get_function_pointer, &delegate)`: obtain the `load_assembly_and_get_function_pointer` delegate.
3. For each shim entry point: `delegate(assemblyPath, typeName, methodName, delegateTypeName, ...)`.

The `delegateTypeName` must match the delegate type signature. For `[UnmanagedCallersOnly]` entry points, the bridge uses `UNMANAGEDCALLERSONLY_METHOD` as the delegate type (a special sentinel value defined in `nethost.h` that bypasses delegate-type checking).

## `MochiMarshal` type conventions

`MochiMarshal` is a static helper class in `dotnet_shim/shared/MochiMarshal.cs` that implements the native↔managed data marshalling conventions:

### String convention

```csharp
// Native to managed: read a UTF-8 byte* + int pair into a C# string
public static string FromNativeString(IntPtr ptr, int len)
    => Marshal.PtrToStringUTF8(ptr, len) ?? string.Empty;

// Managed to native: allocate a CoTaskMem buffer with UTF-8 + return pointer + length
public static (IntPtr ptr, int len) ToNativeString(string s) {
    if (s == null) return (IntPtr.Zero, 0);
    var bytes = Encoding.UTF8.GetBytes(s);
    var ptr = Marshal.AllocCoTaskMem(bytes.Length);
    Marshal.Copy(bytes, 0, ptr, bytes.Length);
    return (ptr, bytes.Length);
}

// Free a native string returned by ToNativeString
public static void FreeNativeString(IntPtr ptr) => Marshal.FreeCoTaskMem(ptr);
```

The convention uses `CoTaskMem` (the COM task memory allocator) for string ownership. `CoTaskMem` memory is allocated by the CLR side and freed by the Mochi side via a matching free call. The Mochi runtime calls `mochi_<pkg>_string_free(ptr)` (a generated shim entry) which delegates to `Marshal.FreeCoTaskMem`.

### List convention

```csharp
public struct MochiSliceI64 {
    public long* Ptr;
    public int Len;
}

public static MochiSliceI64 ToNativeListI64(List<long> list) {
    var arr = list.ToArray();
    var handle = GCHandle.Alloc(arr, GCHandleType.Pinned);
    return new MochiSliceI64 {
        Ptr = (long*)handle.AddrOfPinnedObject(),
        Len = arr.Length
    };
    // NOTE: handle must be freed after Mochi copies the slice
}
```

For scalar lists, the data is pinned in managed memory and the native pointer is valid only for the duration of the call. For lists of strings, each element is marshalled to a `CoTaskMem` buffer.

### Opaque handle convention

For reference types (classes, interfaces), the shim uses `GCHandle` to pin the managed object and passes the `GCHandle.ToIntPtr()` value as an `IntPtr` (Mochi `int` type):

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_HttpClient_new", CallConvs = new[] { typeof(CallConvCdecl) })]
public static IntPtr HttpClient_new()
{
    var client = new HttpClient();
    var handle = GCHandle.Alloc(client, GCHandleType.Normal);
    return GCHandle.ToIntPtr(handle);
}

[UnmanagedCallersOnly(EntryPoint = "mochi_HttpClient_free", CallConvs = new[] { typeof(CallConvCdecl) })]
public static void HttpClient_free(IntPtr handle_ptr)
{
    var handle = GCHandle.FromIntPtr(handle_ptr);
    if (handle.IsAllocated) {
        (handle.Target as IDisposable)?.Dispose();
        handle.Free();
    }
}
```

The Mochi runtime owns the `GCHandle` after the constructor call and calls `mochi_<type>_free` when the Mochi GC determines the handle is unreachable. The `GCHandle.Normal` type keeps the managed object alive as long as the handle is allocated; `GCHandle.Free` releases the CLR's hold on the object, allowing it to be collected.

## Exception handling across the boundary

Exceptions must not escape an `[UnmanagedCallersOnly]` method (an unhandled exception crossing the boundary causes a fatal abort). The shim catches all exceptions and encodes them as a `MochiError` out-parameter:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_Dapper_Query", CallConvs = new[] { typeof(CallConvCdecl) })]
public static unsafe IntPtr Dapper_Query(IntPtr conn_handle, byte* sql_ptr, int sql_len, IntPtr* error_out)
{
    *error_out = IntPtr.Zero;
    try {
        var conn = (IDbConnection)GCHandle.FromIntPtr(conn_handle).Target!;
        var sql = MochiMarshal.FromNativeString((IntPtr)sql_ptr, sql_len);
        var result = conn.Query(sql).AsList();
        return MochiMarshal.ToNativeJsonList(result);
    } catch (Exception ex) {
        *error_out = MochiMarshal.ToNativeString(ex.ToString()).ptr;
        return IntPtr.Zero;
    }
}
```

The Mochi runtime checks `error_out` after each call. If non-null, the Mochi runtime reads the UTF-8 error string, frees it, and raises a Mochi panic with the message.

## NativeAOT vs CLR hosting ABI difference

When `[dotnet] bridge = "nativeaot"` is set, the shim is compiled to a native shared library via `dotnet publish -r <rid> -p:PublishAot=true`. The `[UnmanagedCallersOnly]` entry points are the same; the loading mechanism changes.

With CLR hosting:
- The shim assembly (`.dll`) is loaded into the CLR at runtime via `load_assembly_and_get_function_pointer`.
- Entry points are function pointers obtained from the CLR hosting delegate.

With NativeAOT:
- The shim is a native shared library (`libshim.so` / `libshim.dylib` / `shim.dll`).
- Entry points are standard shared library exports, loadable via `dlopen` / `LoadLibrary`.
- No CLR at runtime; no `hostfxr` invocation.

The Mochi runtime detects the bridge mode from `mochi.lock`'s `[[dotnet-package]] bridge` field and uses the appropriate loading mechanism.

The ABI surface is identical in both modes: the same `[UnmanagedCallersOnly]` entry point names, the same parameter types, the same `MochiMarshal` conventions. This is by design: a NativeAOT shim and a CLR-hosted shim are interchangeable from the Mochi runtime's perspective.

## ABI versioning

Each shim project includes a version sentinel:

```csharp
// MochiShimVersion.cs
public static class MochiShimVersion {
    public const int AbiVersion = 1;
}
```

And a corresponding `[UnmanagedCallersOnly]` accessor:

```csharp
[UnmanagedCallersOnly(EntryPoint = "mochi_shim_abi_version", CallConvs = new[] { typeof(CallConvCdecl) })]
public static int GetAbiVersion() => MochiShimVersion.AbiVersion;
```

The Mochi runtime calls `mochi_shim_abi_version` at load time and refuses to use a shim whose ABI version differs from the runtime's expected version. An ABI version mismatch produces:

```
ERROR: ABI version mismatch
  Shim: dotnet_shim/Serilog/Serilog.dll
  Shim ABI version: 2
  Runtime expected: 1
  Resolution: regenerate the shim with `mochi pkg sync dotnet`
```

## Cross-references

- [[05-type-mapping]] for the CLR-side types that drive the shim surface.
- [[08-async-bridge]] for the `.GetAwaiter().GetResult()` entry into shim functions.
- [[10-generics-and-reification]] for how CLR generics affect the shim encoding.
- [[11-nativeaot-and-trimming]] for the NativeAOT shim path.
- [MEP-68 §6](/docs/mep/mep-0068#6-build-orchestration) for how the shim fits into the build flow.
